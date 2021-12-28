#include <stdio.h>
#include <string.h>
#include "util.h"
#include "errormsg.h"
#include "symbol.h"
#include "types.h"
#include "absyn.h"
#include "env.h"
#include "translate.h"
#include "semant.h"

struct expty {
  Tr_exp exp;
  Ty_ty ty;
};

struct expty expTy(Tr_exp exp, Ty_ty ty);
Ty_fieldList transFields(S_table tenv, A_fieldList records);
Ty_tyList transParams(S_table tenv, A_fieldList params);
Ty_ty transTy(S_table tenv, S_symbol name, A_ty a);
void transDec(S_table venv, S_table tenv, A_dec d);
struct expty transExp(S_table venv, S_table tenv, A_exp exp);
struct expty transVar(S_table venv, S_table tenv, A_var var);
struct expty transLet(S_table venv, S_table tenv, void* let);
struct expty transFor(S_table venv, S_table tenv, void* forr);
struct expty transWhile(S_table venv, S_table tenv, void* whilee);
struct expty transSeq(S_table venv, S_table tenv, A_expList seq);
struct expty transIf(S_table venv, S_table tenv, void* iff);
struct expty transCall(S_table venv, S_table tenv, A_exp call);
struct expty transOp(S_table venv, S_table tenv, void* op);
struct expty transAssign(S_table venv, S_table tenv, void* assign);
struct expty transArray(S_table venv, S_table tenv, void* array);
struct expty transRecord(S_table venv, S_table tenv, void* record);

struct expty expTy(Tr_exp exp, Ty_ty ty) {
  struct expty e;
  e.exp=exp;
  e.ty=ty;
  return e;
}

void SEM_transProg(A_exp exp) {
  transExp(E_base_venv(), E_base_tenv(), exp);
}

Ty_ty actual_ty(Ty_ty ty) {
  switch (ty->kind) {
  case Ty_record: return ty;
  case Ty_nil: return ty;
  case Ty_int: return ty;
  case Ty_string: return ty;
  case Ty_array: return ty;
  case Ty_name: return actual_ty(ty->u.name.ty);
  case Ty_void: return ty;
  }
}

Ty_fieldList transFields(S_table tenv, A_fieldList records) {
  Ty_ty typeid = S_look(tenv, records->head->typ);
  if (!typeid)
    EM_error(-1, "Unknown type with irratant: %s", S_name(records->head->typ));
  if (records->tail == NULL)
    return Ty_FieldList(Ty_Field(records->head->name, typeid), NULL);
  else
    return Ty_FieldList(Ty_Field(records->head->name, typeid), transFields(tenv, records->tail));
}

Ty_tyList transParams(S_table tenv, A_fieldList params) {
  Ty_ty typeid = S_look(tenv, params->head->typ);
  if (params->tail == NULL)
    return Ty_TyList(Ty_Name(params->head->name, typeid), NULL);
  else
    return Ty_TyList(Ty_Name(params->head->name, typeid), transParams(tenv, params->tail));
}

Ty_ty transTy(S_table tenv, S_symbol name, A_ty ty) {
  switch (ty->kind) {
  case A_nameTy:
    return Ty_Name(name, S_look(tenv, ty->u.name));
  case A_recordTy:
    S_enter(tenv, name, Ty_Nil());
    return Ty_Record(transFields(tenv, ty->u.record));
  case A_arrayTy:
    return Ty_Array(S_look(tenv, ty->u.array));
  }
}

void transDec(S_table venv, S_table tenv, A_dec dec) {
  switch (dec->kind) {
  case A_varDec:
    if (dec->u.var.typ) {
      Ty_ty ty = S_look(tenv, dec->u.var.typ);
      Ty_ty initty = transExp(venv, tenv, dec->u.var.init).ty;
      if (!ty)
        EM_error(dec->pos, "Unknown type with irratant: %s", S_name(dec->u.var.typ));
      if (actual_ty(ty)->kind != initty->kind)
        EM_error(dec->pos, "Unexpected type: %s", S_name(dec->u.var.typ));
      S_enter(venv, dec->u.var.var, E_VarEntry(initty));
    } else {
      Ty_ty initty = transExp(venv, tenv, dec->u.var.init).ty;
      S_enter(venv, dec->u.var.var, E_VarEntry(initty));
    }
    break;
  case A_typeDec:
    for (A_nametyList ts=dec->u.type; ts; ts=ts->tail)
      S_enter(tenv, ts->head->name, transTy(tenv, ts->head->name, ts->head->ty));
    break;
  case A_functionDec: {
    // Put header information here.
    for (A_fundecList l=dec->u.function; l; l=l->tail) {
      Ty_ty resultty = NULL;
      Ty_tyList paramTys = NULL;
      if (l->head->params)
        paramTys = transParams(tenv, l->head->params);
      if (l->head->result) {
        resultty = S_look(tenv, l->head->result);
        if (!resultty)
          EM_error(l->head->pos, "Unknown type: %s", S_name(l->head->result));
      }
      S_enter(venv, l->head->name, E_FunEntry(paramTys, resultty));
    }
    // Full checking.
    for (A_fundecList l=dec->u.function; l; l=l->tail) {
      Ty_ty resultty = NULL;
      Ty_tyList paramTys = NULL;
      if (l->head->params)
        paramTys = transParams(tenv, l->head->params);
      if (l->head->result) {
        resultty = S_look(tenv, l->head->result);
        if (!resultty)
          EM_error(l->head->pos, "Unknown type: %s", S_name(l->head->result));
      }
      S_beginScope(venv);
      Ty_tyList tys = paramTys;
      A_fieldList args = l->head->params;
      while (tys && args) {
        S_enter(venv, args->head->name, E_VarEntry(tys->head));
        tys=tys->tail;
        args=args->tail;
      }
      Ty_ty bodyty = transExp(venv, tenv, l->head->body).ty;
      if (resultty) {
        if (actual_ty(resultty)->kind != bodyty->kind)
          EM_error(l->head->pos, "Unexpected result type: %s", S_name(l->head->result));
      } else {
        resultty = bodyty;
      }
      S_endScope(venv);
      S_enter(venv, l->head->name, E_FunEntry(paramTys, resultty));
    }
  } break;
  }
}

struct expty transExp(S_table venv, S_table tenv, A_exp exp) {
  switch (exp->kind) {
  case A_nilExp:
    return expTy(NULL, Ty_Nil());
  case A_breakExp:
    return expTy(NULL, Ty_Void());
  case A_intExp:
    return expTy(NULL, Ty_Int());
  case A_stringExp:
    return expTy(NULL, Ty_String());
  case A_varExp:
    return transVar(venv, tenv, exp->u.var);
  case A_callExp:
    return transCall(venv, tenv, exp);
  case A_opExp:
    return transOp(venv, tenv, &exp->u.op);
  case A_recordExp:
    return transRecord(venv, tenv, &exp->u.record);
  case A_seqExp:
    return transSeq(venv, tenv, exp->u.seq);
  case A_assignExp:
    return transAssign(venv, tenv, &exp->u.assign);
  case A_ifExp:
    return transIf(venv, tenv, &exp->u.iff);
  case A_whileExp:
    return transWhile(venv, tenv, &exp->u.whilee);
  case A_forExp:
    return transFor(venv, tenv, &exp->u.forr);
  case A_letExp:
    return transLet(venv, tenv, &exp->u.let);
  case A_arrayExp:
    return transArray(venv, tenv, &exp->u.array);
  }
}

struct expty transVar(S_table venv, S_table tenv, A_var var) {
  switch (var->kind) {
  case A_simpleVar: {
    E_enventry entry = S_look(venv, var->u.simple);
    if (!entry) {
      EM_error(var->pos, "Undefined variable with irratant: %s", S_name(var->u.simple));
      return expTy(NULL, Ty_Void());
    }
    switch (entry->kind) {
    case E_varEntry:
      return expTy(NULL, actual_ty(entry->u.var.ty));
    case E_funEntry:
      EM_error(var->pos, "Functions aren't first class with irratant: %s", S_name(var->u.simple));
      return expTy(NULL, Ty_Void());
    }
  }
  case A_fieldVar: {
    struct expty varty = transVar(venv, tenv, var->u.field.var);
    Ty_fieldList fields = varty.ty->u.record;
    for (Ty_fieldList fs=fields; fs; fs=fs->tail) {
      if (var->u.field.sym == fs->head->name)
        return expTy(NULL, fs->head->ty);
    }
    EM_error(var->pos, "Undefined variable with irratant: %s", S_name(var->u.field.sym));
    return expTy(NULL, Ty_Void());
  }
  case A_subscriptVar: {
    Ty_ty varty = transVar(venv, tenv, var->u.subscript.var).ty;
    Ty_ty expty = transExp(venv, tenv, var->u.subscript.exp).ty;
    if (expty->kind != Ty_int)
      EM_error(var->pos, "Expected an integer.");
    return expTy(NULL, varty);
  }
  }
}

struct expty transOp(S_table venv, S_table tenv, void* op) {
  struct {A_oper oper; A_exp left; A_exp right;}* opptr = op;
  struct expty leftty = transExp(venv, tenv, opptr->left);
  struct expty rightty = transExp(venv, tenv, opptr->right);
  switch (opptr->oper) {
  case A_plusOp:
  case A_minusOp:
  case A_timesOp:
  case A_divideOp:
  case A_neqOp:
  case A_ltOp:
  case A_leOp:
  case A_gtOp:
  case A_geOp:
    if (leftty.ty->kind != Ty_int)
      EM_error(opptr->left->pos, "Expected an integer.");
    if (rightty.ty->kind != Ty_int)
      EM_error(opptr->right->pos, "Expected an integer.");
  case A_eqOp:
    return expTy(NULL, Ty_Int());
  }
}

struct expty transSeq(S_table venv, S_table tenv, A_expList seq) {
    struct expty expty;
    for (A_expList es=seq; es; es=es->tail)
      expty = transExp(venv, tenv, es->head);
    return expty;
}

struct expty transIf(S_table venv, S_table tenv, void* iff) {
  struct {A_exp test, then, elsee;}* ifptr = iff;
  struct expty testty = transExp(venv, tenv, ifptr->test);
  struct expty thenty = transExp(venv, tenv, ifptr->then);
  struct expty elsety = ifptr->elsee ? transExp(venv, tenv, ifptr->elsee) : expTy(NULL,NULL);
  if (testty.ty->kind != Ty_int)
    EM_error(ifptr->test->pos, "Result of conditional must be int.");
  if (ifptr->elsee) {
    if (thenty.ty->kind != elsety.ty->kind)
      EM_error(ifptr->then->pos, "Result of consequent must be the result of the alternative.");
    return thenty;
  } else {
    if (thenty.ty->kind != Ty_void)
      EM_error(ifptr->then->pos, "Result of one armed if must be void.");
    return thenty;
  }
}

struct expty transRecord(S_table venv, S_table tenv, void* record) {
  struct {S_symbol typ; A_efieldList fields;}* recordptr = record;
  Ty_ty typeid = S_look(tenv, recordptr->typ);
  Ty_fieldList tys = typeid->u.record;
  A_efieldList fs = recordptr->fields;
  while (tys && fs) {
    Ty_ty expty = transExp(venv, tenv, fs->head->exp).ty;
    if (strcmp(S_name(fs->head->name), S_name(tys->head->name)) != 0)
      EM_error(fs->head->exp->pos, "Field not of expected name: %s.", S_name(tys->head->name));
    if (actual_ty(tys->head->ty)->kind != expty->kind)
      EM_error(fs->head->exp->pos, "Field %s not of expected type.", S_name(tys->head->name));
    fs=fs->tail;
    tys=tys->tail;
  }
  return expTy(NULL, typeid);
}

struct expty transArray(S_table venv, S_table tenv, void* array) {
  struct {S_symbol typ; A_exp size, init;}* arrayptr = array;
  Ty_ty typeid = S_look(tenv, arrayptr->typ);
  Ty_ty sizety = transExp(venv, tenv, arrayptr->size).ty;
  Ty_ty initty = transExp(venv, tenv, arrayptr->init).ty;
  if (typeid->kind != Ty_array)
    EM_error(arrayptr->size->pos, "Expected an array with irritant, %s.", S_name(arrayptr->typ));
  if (sizety->kind != Ty_int)
    EM_error(arrayptr->size->pos, "Size of array must be an integer.");
  if (actual_ty(typeid->u.array)->kind != initty->kind)
    EM_error(arrayptr->init->pos, "Array was expected to be initialized with: %s.", S_name(arrayptr->typ));
  return expTy(NULL, typeid);
}

struct expty transAssign(S_table venv, S_table tenv, void* assign) {
  struct {A_var var; A_exp exp;}* assignptr = assign;
  Ty_ty varty = transVar(venv, tenv, assignptr->var).ty;
  Ty_ty expty = transExp(venv, tenv, assignptr->exp).ty;
  if (varty->kind == Ty_array) {
    if (actual_ty(varty->u.array)->kind != expty->kind)
      EM_error(assignptr->exp->pos, "Variable set to wrong value.");
  } else {
    if (varty->kind != expty->kind)
      EM_error(assignptr->exp->pos, "Variable set to wrong value.");
  }
  return expTy(NULL, Ty_Void());
}

struct expty transCall(S_table venv, S_table tenv, A_exp call) {
  struct {S_symbol func; A_expList args;}* callptr = (void*)&call->u.call;
  E_enventry funEntry = S_look(venv, callptr->func);
  if (funEntry)
    switch (funEntry->kind) {
    case E_varEntry:
      EM_error(call->pos, "Functions aren't first class with irratant: %s", S_name(callptr->func));
    case E_funEntry: {
      Ty_tyList tys = funEntry->u.fun.formals;
      A_expList args = callptr->args;
      while (tys && args) {
        Ty_ty expty = transExp(venv, tenv, args->head).ty;
        if (actual_ty(tys->head)->kind != expty->kind)
          EM_error(args->head->pos, "Unexpected argument type.");
        tys=tys->tail;
        args=args->tail;
      }
      if (tys != NULL || args != NULL)
        EM_error(call->pos, "Arrity mismatch");
      // funEntry->u.fun.result may be NULL if it's only used for header information.
      if (funEntry->u.fun.result)
        return expTy(NULL, funEntry->u.fun.result);
      else
        return expTy(NULL, Ty_Nil());
    }
    }
  else
    EM_error(call->pos, "Undefined function with irratant: %s", S_name(callptr->func));
  return expTy(NULL, Ty_Void());
}

struct expty transWhile(S_table venv, S_table tenv, void* whilee) {
  struct {A_exp test, body;}* whileptr = whilee;
  Ty_ty testty = transExp(venv, tenv, whileptr->test).ty;
  Ty_ty bodyty = transExp(venv, tenv, whileptr->body).ty;
  if (testty->kind != Ty_int)
    EM_error(whileptr->test->pos, "Result of conditional must be an integer.");
  if (bodyty->kind != Ty_void)
    EM_error(whileptr->body->pos, "Result of while loop must be void.");
  return expTy(NULL, Ty_Void());
}

struct expty transFor(S_table venv, S_table tenv, void* forr) {
  struct {S_symbol var; A_exp lo,hi,body; bool escape;}* forptr = forr;
  Ty_ty loty = transExp(venv, tenv, forptr->lo).ty;
  Ty_ty hity = transExp(venv, tenv, forptr->hi).ty;
  if (loty->kind != Ty_int)
    EM_error(forptr->lo->pos, "For loop initializer must be an integer.");
  if (hity->kind != Ty_int)
    EM_error(forptr->hi->pos, "For loop bound must be an integer.");
  S_beginScope(venv);
  S_beginScope(tenv);
  S_enter(venv, forptr->var, E_VarEntry(loty));
  Ty_ty bodyty = transExp(venv, tenv, forptr->body).ty;
  if (bodyty->kind != Ty_void)
    EM_error(forptr->body->pos, "Result of for loop must be void.");
  S_endScope(tenv);
  S_endScope(venv);
  return expTy(NULL, Ty_Void());
}

struct expty transLet(S_table venv, S_table tenv, void* let) {
  struct {A_decList decs; A_exp body;}* letptr = let;
  struct expty expty;
  S_beginScope(venv);
  S_beginScope(tenv);
  for (A_decList ds=letptr->decs; ds; ds=ds->tail)
    transDec(venv, tenv, ds->head);
  expty = transExp(venv, tenv, letptr->body);
  S_endScope(tenv);
  S_endScope(venv);
  return expty;
}
