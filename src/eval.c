#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "util.h"
#include "symbol.h"
#include "absyn.h"
#include "eval.h"

val voidVal();
val integerVal(int i);
val stringVal(string s);
val arrayVal(int size, val init);
val recordVal(A_efieldList fields, S_table venv);

val evalif(void* iff, S_table venv);
val evalop(void* op, S_table venv);
val evalseq(A_expList seq, S_table venv);
val evalcall(void* call, S_table venv); // or as i often call it apply.
val evalwhile(void* whilee, S_table venv);
val evalfor(void* forr, S_table venv);
val evalarray(void* array, S_table venv);
val evalrecord(void* record, S_table venv);
val evalassign(void* assign, S_table venv);
val evalvar(A_var var, S_table venv);
val evallet(void* let, S_table venv);
val evalexp(A_exp exp, S_table venv);

val stringVal(string s) {
  val v = malloc(sizeof(*v));
  v->kind = V_string;
  v->u.string = s;
  return v;
}

val integerVal(int i) {
  val v = malloc(sizeof(*v));
  v->kind = V_integer;
  v->u.integer = i;
  return v;
}

val voidVal() {
  val v = malloc(sizeof(*v));
  v->kind = V_void;
  return v;
}

val arrayVal(int size, val init) {
  val v = malloc(sizeof(*v));
  v->kind = V_array;
  v->u.array = malloc(size * sizeof(val));
  for (int i=0; i < size; i++)
    v->u.array[i] = init;
  return v;
}

val recordVal(A_efieldList fields, S_table venv) {
  val v = malloc(sizeof(*v));
  v->kind = V_record;
  v->u.record = S_empty();
  for (A_efieldList l=fields; l; l=l->tail)
    S_enter(v->u.record, l->head->name, evalexp(l->head->exp, venv));
  return v;
}

val evalif(void* iff, S_table venv) {
  struct {A_exp test, then, elsee;}* ifptr = iff;
  val testeval = evalexp(ifptr->test, venv);
  if (testeval->u.integer > 0) {
    return evalexp(ifptr->then, venv);
  }
  if (ifptr->elsee) {
    return evalexp(ifptr->elsee, venv);
  }
  return voidVal();
}

val evalop(void* op, S_table venv) {
  struct {A_oper oper; A_exp left; A_exp right;}* opptr = op;
  val lefteval = evalexp(opptr->left, venv);
  val righteval = evalexp(opptr->right, venv);
  switch (opptr->oper) {
    case A_plusOp:
      return integerVal(lefteval->u.integer + righteval->u.integer);
    case A_minusOp:
      return integerVal(lefteval->u.integer - righteval->u.integer);
    case A_timesOp:
      return integerVal(lefteval->u.integer * righteval->u.integer);
    case A_divideOp:
      return integerVal(lefteval->u.integer / righteval->u.integer);
    case A_eqOp:
      return integerVal(lefteval->u.integer == righteval->u.integer);
    case A_neqOp:
      return integerVal(lefteval->u.integer != righteval->u.integer);
    case A_ltOp:
      return integerVal(lefteval->u.integer < righteval->u.integer);
    case A_leOp:
      return integerVal(lefteval->u.integer <= righteval->u.integer);
    case A_gtOp:
      return integerVal(lefteval->u.integer > righteval->u.integer);
    case A_geOp:
      return integerVal(lefteval->u.integer >= righteval->u.integer);
  }
}

val evalseq(A_expList seq, S_table venv) {
  val ret = NULL;
  for (A_expList l = seq; l; l=l->tail)
    ret = evalexp(l->head, venv);
  return ret;
}

val evalarray(void* array, S_table venv) {
  struct {S_symbol typ; A_exp size, init;}* arrayptr = array;
  val s = evalexp(arrayptr->size, venv);
  val i = evalexp(arrayptr->init, venv);
  return arrayVal(s->u.integer,i);
}

val evalrecord(void* record, S_table venv) {
  struct {S_symbol typ; A_efieldList fields;}* recordptr = record;
  return recordVal(recordptr->fields, venv);
}

val evalassign(void* assign, S_table venv) {
  struct {A_var var; A_exp exp;}* assignptr = assign;
  val rval = evalexp(assignptr->exp, venv);
  switch (assignptr->var->kind) {
    case A_simpleVar:
      S_enter(venv, assignptr->var->u.simple, rval);
      break;
    case A_fieldVar: {
      val f = evalvar(assignptr->var->u.field.var, venv);
      S_enter(f->u.record, assignptr->var->u.field.sym, rval);
    } break;
    case A_subscriptVar: {
      val a = evalvar(assignptr->var->u.subscript.var, venv);
      val n = evalexp(assignptr->var->u.subscript.exp, venv);
      a->u.array[n->u.integer] = rval;
    } break;
  }
  return voidVal();
}

val evalvar(A_var var, S_table venv) {
  switch(var->kind) {
    case A_simpleVar:
      return S_look(venv, var->u.simple);
    case A_fieldVar: {
      val f = evalvar(var->u.field.var, venv);
      return S_look(f->u.record, var->u.field.sym);
    }
    case A_subscriptVar: {
      val a = evalvar(var->u.subscript.var, venv);
      val n = evalexp(var->u.subscript.exp, venv);
      return a->u.array[n->u.integer];
    }
  }
  return voidVal();
}

val evalcall(void* call, S_table venv) {
  struct {S_symbol func; A_expList args;}* callptr = call;
  /* built-in stdlib routines. */
  if (strcmp(S_name(callptr->func), "print") == 0) {
    // function print(s : string)
    // Print s on standard output.
    val s = evalexp(callptr->args->head, venv);
    printf("%s", s->u.string);
    return voidVal();
  } else if (strcmp(S_name(callptr->func), "flush") == 0) {
    // function flush()
    // Flush the standard output buffer.
    fflush(stdout);
    return voidVal();
  } else if (strcmp(S_name(callptr->func), "getchar") == 0) {
    // function getchar() : string
    // Read a character from standard input; return empty string on end of file.
    int c = getchar();
    string charstring = malloc(2);
    if (c == EOF)
      charstring[0] = '\0';
    else
      charstring[0] = (char)c;
    charstring[1] = '\0';
    return stringVal(charstring);
  } else if (strcmp(S_name(callptr->func), "ord") == 0) {
    // function ord(s: string) : int
    // Give ASCII value of first character of s; yields -1 if s is empty string.
    val s = evalexp(callptr->args->head, venv);
    if (s->u.string[0] == '\0')
      return integerVal(-1);
    else
      return integerVal((int)s->u.string[0]);
  } else if (strcmp(S_name(callptr->func), "chr") == 0) {
    // function chr(i: int) : string
    // Single-character string from ASCII value i; halt program if i out of range.
    string charstring = malloc(2);
    val i = evalexp(callptr->args->head, venv);
    if (i->u.integer > 127)
      exit(1);
    else
      charstring[0] = (char)i->u.integer;
    charstring[1] = '\0';
    return stringVal(charstring);
  } else if (strcmp(S_name(callptr->func), "size") == 0) {
    // function size(s: string) : int
    // Number of characters in s.
    val s = evalexp(callptr->args->head, venv);
    int i;
    for (i=0; s->u.string[i]!='\0'; i++);
    return integerVal(i);
  } else if (strcmp(S_name(callptr->func), "substring") == 0) {
    // function substring(s:string, first:int, n:int) : string
    // Substring of string s, starting with character first, n characters long. Char- acters are numbered starting at 0.
    val s = evalexp(callptr->args->head, venv);
    val first = evalexp(callptr->args->tail->head, venv);
    val n = evalexp(callptr->args->tail->tail->head, venv);
    string start = s->u.string + first->u.integer;
    string substring = malloc(n->u.integer+1);
    strncpy(substring, start, n->u.integer);
    return stringVal(substring);
  } else if (strcmp(S_name(callptr->func), "concat") == 0) {
    // function concat (s1: string, s2: string) : string
    // Concatenation of s1 and s2.
    val s1 = evalexp(callptr->args->head, venv);
    val s2 = evalexp(callptr->args->tail->head, venv);
    string buffer = malloc(128);
    snprintf(buffer, 128, "%s%s", s1->u.string, s2->u.string);
    return stringVal(buffer);
  } else if (strcmp(S_name(callptr->func), "not") == 0) {
    // function not(i : integer) : integer
    // Return (i=0).
    val i = evalexp(callptr->args->head, venv);
    return integerVal(i->u.integer == 0);
  } else if (strcmp(S_name(callptr->func), "exit") == 0) {
    // function exit(i: int)
    // Terminate execution with code i.
    val i = evalexp(callptr->args->head, venv);
    exit(i->u.integer);
  }
  /* function application */
  A_fundec fundec = S_look(venv, callptr->func);
  S_beginScope(venv);
  A_expList as = callptr->args;
  A_fieldList fs = fundec->params;
  while (as && fs) {
    val a = evalexp(as->head, venv);
    S_enter(venv, fs->head->name, a);
    as=as->tail;
    fs=fs->tail;
  }
  val ret = evalexp(fundec->body, venv);
  S_endScope(venv);
  return ret;
}

val evalwhile(void* whilee, S_table venv) {
  struct {A_exp test, body;}* whileptr = whilee;
  S_beginScope(venv);
  S_enter(venv, S_Symbol("*inloop*"), (void*)1);
  while (evalexp(whileptr->test, venv)->u.integer && S_look(venv, S_Symbol("*inloop*")))
    evalexp(whileptr->body, venv);
  S_endScope(venv);
  return voidVal();
}

val evalfor(void* forr, S_table venv) {
  struct {S_symbol var; A_exp lo,hi,body; bool escape;}* forptr = forr;
  S_beginScope(venv);
  S_enter(venv, S_Symbol("*inloop*"), (void*)1);
  val loval = evalexp(forptr->lo, venv);
  val hival = evalexp(forptr->hi, venv);
  S_enter(venv, forptr->var, loval);
  while (loval->u.integer <= hival->u.integer && S_look(venv, S_Symbol("*inloop*"))) {
    evalexp(forptr->body, venv);
    loval->u.integer++;
  }
  loval->u.integer--;
  S_endScope(venv);
  return voidVal();
}

val evallet(void* let, S_table venv) {
  struct {A_decList decs; A_exp body;}* letptr = let;
  S_beginScope(venv);
  for (A_decList ds=letptr->decs; ds; ds=ds->tail) {
    switch (ds->head->kind) {
      case A_varDec:
        S_enter(venv, ds->head->u.var.var, evalexp(ds->head->u.var.init, venv));
        break;
      case A_typeDec:
        // note! typing information isn't used in this evaluator.
        break;
      case A_functionDec:
        // note! functions aren't first-class in this langauage but they're
        // going in the value environment. as a consequence of this you
        // can't have variables and functions share the same name.
        for (A_fundecList fs=ds->head->u.function; fs; fs=fs->tail)
          S_enter(venv, fs->head->name, fs->head);
        break;
    }
  }
  val ret = evalexp(letptr->body, venv);
  S_endScope(venv);
  return ret;
}

val evalexp(A_exp exp, S_table venv) {
  switch(exp->kind) {
  case A_nilExp:
    return voidVal();
  case A_intExp:
    return integerVal(exp->u.intt);
  case A_stringExp:
    return stringVal(exp->u.stringg);
  case A_breakExp:
     S_enter(venv, S_Symbol("*inloop*"), (void*)0);
    return voidVal();
  case A_varExp:
    return evalvar(exp->u.var, venv);
  case A_assignExp:
    return evalassign(&exp->u.assign, venv);
  case A_arrayExp:
    return evalarray(&exp->u.array, venv);
  case A_recordExp:
    return evalrecord(&exp->u.record, venv);
  case A_ifExp:
    return evalif(&exp->u.iff, venv);
  case A_opExp:
    return evalop(&exp->u.op, venv);
  case A_seqExp:
    return evalseq(exp->u.seq, venv);
  case A_callExp:
    return evalcall(&exp->u.call, venv);
  case A_whileExp:
    return evalwhile(&exp->u.whilee, venv);
  case A_forExp:
    return evalfor(&exp->u.forr, venv);
  case A_letExp:
    return evallet(&exp->u.let, venv);
  }
}

void printval(val v) {
  switch(v->kind) {
    case V_void:
      printf("\n");
      break;
    case V_integer:
      printf("%d\n", v->u.integer);
      break;
    case V_string:
      printf("\"%s\"\n", v->u.string);
      break;
    case V_array:
      printf("#<array %p>\n", v);
      break;
    case V_record:
      printf("#<record %p>\n", v);
      break;
  }
}
