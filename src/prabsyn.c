#include <stdio.h>
#include <stdlib.h>
#include "util.h"
#include "symbol.h"
#include "absyn.h"
#include "prabsyn.h"
// note! might end up blowing the heap doing this...
// note! should call free after every emit.

string emitint(int ifpptr);
string emitstring(string s);
string emitty(A_ty ty);
string emitexp(A_exp exp);
string emitvar(A_var var);
string emitseq(A_expList seq);
// dont try this at home.
string emitlet(void* let);
string emitfor(void* forr);
string emitwhile(void* whilee);
string emitarray(void* array);
string emitrecord(void* record);
string emitcall(void* call);
string emitif(void* iff);
string emitop(void* op);
string emitassign(void* assign);

string emitty(A_ty ty) {
  string tystring = malloc(256);
  string fieldstring = tystring;
  switch (ty->kind) {
    case A_nameTy:
      snprintf(tystring, 256, "%s", S_name(ty->u.name));
      return tystring;
    case A_arrayTy:
      snprintf(tystring, 256, "%s array", S_name(ty->u.array));
      return tystring;
    case A_recordTy:
      snprintf(tystring, 256, "(record");
      for (A_fieldList l = ty->u.record;; l=l->tail) {
        // advance the pointer to the end of the callptr-string.
        for (;*fieldstring != '\0'; ++fieldstring);

        if (l) sprintf(fieldstring, " (%s of %s)", S_name(l->head->name), S_name(l->head->typ));
        else {
          sprintf(fieldstring, ")");
          return tystring;
        }
      }
  }
}

string emitassign(void* assign) {
  struct {A_var var; A_exp exp;}* arrayptr = assign;
  string assignstring = malloc(256);
  snprintf(assignstring, 256, "(set! %s %s)", emitvar(arrayptr->var), emitexp(arrayptr->exp));
  return assignstring;
}

string emitop(void* op) {
  struct {A_oper oper; A_exp left; A_exp right;}* opptr = op;
  string opstring = malloc(256);
  switch (opptr->oper) {
  case A_plusOp:
    snprintf(opstring, 256, "(+ %s %s)", emitexp(opptr->left), emitexp(opptr->right));
    return opstring;
  case A_minusOp:
    snprintf(opstring, 256, "(- %s %s)", emitexp(opptr->left), emitexp(opptr->right));
    return opstring;
  case A_timesOp:
    snprintf(opstring, 256, "(* %s %s)", emitexp(opptr->left), emitexp(opptr->right));
    return opstring;
  case A_divideOp:
    snprintf(opstring, 256, "(/ %s %s)", emitexp(opptr->left), emitexp(opptr->right));
    return opstring;
  case A_eqOp:
    snprintf(opstring, 256, "(= %s %s)", emitexp(opptr->left), emitexp(opptr->right));
    return opstring;
  case A_neqOp:
    snprintf(opstring, 256, "(not (= %s %s))", emitexp(opptr->left), emitexp(opptr->right));
    return opstring;
  case A_ltOp:
    snprintf(opstring, 256, "(< %s %s)", emitexp(opptr->left), emitexp(opptr->right));
    return opstring;
  case A_leOp:
    snprintf(opstring, 256, "(<= %s %s)", emitexp(opptr->left), emitexp(opptr->right));
    return opstring;
  case A_gtOp:
    snprintf(opstring, 256, "(> %s %s)", emitexp(opptr->left), emitexp(opptr->right));
    return opstring;
  case A_geOp:
    snprintf(opstring, 256, "(>= %s %s)", emitexp(opptr->left), emitexp(opptr->right));
    return opstring;
  }
}

string emitif(void* iff) {
  struct {A_exp test, then, elsee;}* ifpptr = iff;
  string ifstring = malloc(256);
  if (ifpptr->elsee)
    snprintf(ifstring, 256, "(if %s %s %s)", emitexp(ifpptr->test), emitexp(ifpptr->then), emitexp(ifpptr->elsee));
  else
    snprintf(ifstring, 256, "(if %s %s)", emitexp(ifpptr->test), emitexp(ifpptr->then));
  return ifstring;
}

string emitcall(void* call) {
  struct {S_symbol func; A_expList args;}* callptr = call;
  string callstring = malloc(256);
  string argsstring = callstring;
  snprintf(callstring, 256, "(%s", S_name(callptr->func));

  for (A_expList l=callptr->args;; l=l->tail) {
    // advance the pointer to the end of the callptr-string.
    for (;*argsstring != '\0'; ++argsstring);

    if (l) sprintf(argsstring, " %s", emitexp(l->head));
    else {
      sprintf(argsstring, ")");
      return callstring;
    }
  }
}

string emitrecord(void* record) {
  struct {S_symbol typ; A_efieldList fields;}* recordptr = record;
  string recordstring = malloc(256);
  string fieldstring = recordstring;
  snprintf(recordstring, 256, "(record %s", S_name(recordptr->typ));

  for (A_efieldList l=recordptr->fields;; l=l->tail) {
    // advance the pointer to the end of the callptr-string.
    for (;*fieldstring != '\0'; ++fieldstring);

    if (l) sprintf(fieldstring, " (def %s %s)", S_name(l->head->name), emitexp(l->head->exp));
    else {
      sprintf(fieldstring, ")");
      return recordstring;
    }
  }
}

string emitarray(void* array) {
  struct {S_symbol typ; A_exp size, init;}* arrayptr = array;
  string arraystring = malloc(256);
  snprintf(arraystring, 256, "(array %s %s %s)", S_name(arrayptr->typ), emitexp(arrayptr->size), emitexp(arrayptr->init));
  return arraystring;
}

string emitwhile(void* whilee) {
  struct {A_exp test, body;}* whileptr = whilee;
  string whilestring = malloc(256);
  snprintf(whilestring, 256, "(while %s %s)", emitexp(whileptr->test), emitexp(whileptr->body));
  return whilestring;
}

string emitfor(void* forr) {
  struct {S_symbol var; A_exp lo,hi,body; bool escape;}* forptr = forr;
  string forstring = malloc(256);
  snprintf(forstring, 256, "(for (%s %s -> %s) %s)", S_name(forptr->var), emitexp(forptr->lo), emitexp(forptr->hi), emitexp(forptr->body));
  return forstring;
}

string emitlet(void* let) {
  struct {A_decList decs; A_exp body;}* letptr = let;
  string syntaxstring = malloc(512);
  string letstring = syntaxstring;
  snprintf(syntaxstring, 512, "(let (");

  for (A_decList l=letptr->decs;; l=l->tail) {
    // advance the pointer to the end of the callptr-string.
    for (;*letstring != '\0'; ++letstring);

    if (l) {
      switch(l->head->kind) {
      case A_varDec:
        if (l->head->u.var.typ)
          sprintf(letstring, "(%s of %s %s)", S_name(l->head->u.var.var), S_name(l->head->u.var.typ), emitexp(l->head->u.var.init));
        else
          sprintf(letstring, "(%s %s)", S_name(l->head->u.var.var), emitexp(l->head->u.var.init));
        break;
      case A_typeDec:
        for (A_nametyList ns = l->head->u.type; ns; ns=ns->tail) {
          for (;*letstring != '\0'; ++letstring);
          sprintf(letstring, "(type %s %s)", S_name(ns->head->name), emitty(ns->head->ty));
        }
        break;
      case A_functionDec:
        for (A_fundecList fs = l->head->u.function; fs; fs=fs->tail) {
          for (;*letstring != '\0'; ++letstring);
          sprintf(letstring, "(function %s (", S_name(fs->head->name));
          // emit parameters of the function declaration.
          for (A_fieldList ps = fs->head->params;; ps=ps->tail) {
            for (;*letstring != '\0'; ++letstring);
            if (ps) {
              if (ps->head->typ)
                sprintf(letstring, " %s of %s", S_name(ps->head->name), S_name(ps->head->typ));
              else
                sprintf(letstring, " %s", S_name(ps->head->name));
            } else {
              sprintf(letstring, ")");
              break;
            }
          }
          // emit function result type if any.
          if (fs->head->result) {
            for (;*letstring != '\0'; ++letstring);
            sprintf(letstring, " of %s", S_name(fs->head->result));
          }
          // emit body of function declaration.
          for (;*letstring != '\0'; ++letstring);
          sprintf(letstring, " %s)", emitexp(fs->head->body));
        }
        break;
      }
    } else {
      sprintf(letstring, ")");
      break;
    }
  }

  for (;*letstring != '\0'; ++letstring);
  sprintf(letstring, "%s)", emitexp(letptr->body));
  return syntaxstring;
}

string emitseq(A_expList seq) {
  string syntaxstring = malloc(256);
  string seqstring = syntaxstring;
  snprintf(syntaxstring, 256, "(begin");

  for (A_expList l=seq;; l=l->tail) {
    // advance the pointer to the end of the callptr-string.
    for (;*seqstring != '\0'; ++seqstring);

    if (l) sprintf(seqstring, " %s", emitexp(l->head));
    else {
      sprintf(seqstring, ")");
      return syntaxstring;
    }
  }
}

string emitvar(A_var var) {
  string varstring = malloc(128);
  switch (var->kind) {
  case A_simpleVar:
    return S_name(var->u.simple);
  case A_fieldVar:
    snprintf(varstring, 128, "%s.%s", emitvar(var->u.field.var), S_name(var->u.field.sym));
    return varstring;
  case A_subscriptVar:
    snprintf(varstring, 128, "(%s %s)", emitvar(var->u.subscript.var), emitexp(var->u.subscript.exp));
    return varstring;
  }
}

string emitint(int ifpptr) {
  string intstring = malloc(64);
  snprintf(intstring, 64, "%d", ifpptr);
  return intstring;
}

string emitstring(string s) {
  string sstring = malloc(128);
  string textptr = sstring;
  *textptr++ = '\"';
  for (int ifpptr =0; s[ifpptr] != '\0'; ++ifpptr) {
    if (s[ifpptr] == '\n') {
      *textptr++ = '\\';
      *textptr++ = 'n';
    } else {
      *textptr++ = s[ifpptr];
    }
  }
  *textptr = '\"';
  return sstring;
}

string emitexp(A_exp exp) {
  switch(exp->kind) {
  case A_nilExp:
    return "nil";
  case A_breakExp:
    return "break";
  case A_intExp:
    return emitint(exp->u.intt);
  case A_stringExp:
    return emitstring(exp->u.stringg);
  case A_varExp:
    return emitvar(exp->u.var);
  case A_arrayExp:
    return emitarray(&exp->u.array);
  case A_recordExp:
    return emitrecord(&exp->u.record);
  case A_callExp:
    return emitcall(&exp->u.call);
  case A_assignExp:
    return emitassign(&exp->u.assign);
  case A_opExp:
    return emitop(&exp->u.op);
  case A_ifExp:
    return emitif(&exp->u.iff);
  case A_seqExp:
    return emitseq(exp->u.seq);
  case A_letExp:
    return emitlet(&exp->u.let);
  case A_whileExp:
    return emitwhile(&exp->u.whilee);
  case A_forExp:
    return emitfor(&exp->u.forr);
  }
}

void printast(A_exp exp) {
  string syntax = emitexp(exp);
  printf("%s\n", syntax);
}
