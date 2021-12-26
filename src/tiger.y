%{
#include <stdio.h>
#include "util.h"
#include "symbol.h" 
#include "errormsg.h"
#include "absyn.h"

A_exp absyn_root;

void yyerror(char *s) {
  EM_error(EM_tokPos, "%s", s);
}
%}


%union {
    int pos;
    int ival;
    string sval;
    A_oper oper;
    A_field field;
    A_fieldList fieldList;
    A_efield efield;
    A_efieldList efieldList;
    A_ty ty;
    A_namety namety;
    A_nametyList nametyList;
    A_fundec fundec;
    A_fundecList fundecList;
    A_dec dec;
    A_decList decList;
    A_var var;
    A_exp exp;
    A_expList seq;
}

%type <exp> exp program
%type <seq> exps expseq
%type <var> lvalue
%type <field> field
%type <fieldList> fields
%type <efield> efield
%type <efieldList> efields
%type <dec> dec tydec vardec fundec
%type <decList> decs
%type <fundec> fun
%type <fundecList> funs
%type <ty> namety recordty arrayty
%type <namety> ty
%type <nametyList> tys

%token <sval> ID STRING
%token <ival> INT

%token 
  COMMA COLON SEMICOLON LPAREN RPAREN LBRACK RBRACK 
  LBRACE RBRACE DOT 
  PLUS MINUS TIMES DIVIDE EQ NEQ LT LE GT GE
  AND OR ASSIGN
  ARRAY IF THEN ELSE WHILE FOR TO DO LET IN END OF 
  BREAK NIL
  FUNCTION VAR TYPE

%start program
%nonassoc LOW
%nonassoc THEN DO TYPE FUNCTION ID 
%nonassoc ASSIGN LBRACK ELSE OF COMMA
%left OR
%left AND
%nonassoc EQ NEQ LE LT GT GE
%left PLUS MINUS
%left TIMES DIVIDE
%left UMINUS
%%

program:     {absyn_root=NULL;}
       | exp {absyn_root=$1;}

exp : INT                             { $$=A_IntExp(EM_tokPos,$1); }
    | STRING                          { $$=A_StringExp(EM_tokPos,$1); }
    | BREAK                           { $$=A_BreakExp(EM_tokPos); }
    | NIL                             { $$=A_NilExp(EM_tokPos); }
    | lvalue                          { $$=A_VarExp(EM_tokPos,$1); }
    | lvalue ASSIGN exp               { $$=A_AssignExp(EM_tokPos,$1,$3); }
    | LPAREN RPAREN                   { $$=A_SeqExp(EM_tokPos,NULL);}
    | LPAREN expseq RPAREN            { $$=A_SeqExp(EM_tokPos,$2); }
    | MINUS exp %prec UMINUS          { $$=A_OpExp(EM_tokPos,A_minusOp,A_IntExp(EM_tokPos,0),$2); }
		| exp LT exp						          { $$=A_OpExp(EM_tokPos,A_ltOp,$1,$3); }
		| exp GT exp						          { $$=A_OpExp(EM_tokPos,A_gtOp,$1,$3); }
    | exp LE exp						          { $$=A_OpExp(EM_tokPos,A_leOp,$1,$3); }
    | exp GE exp						          { $$=A_OpExp(EM_tokPos,A_geOp,$1,$3); }
    | exp PLUS exp						        { $$=A_OpExp(EM_tokPos,A_plusOp,$1,$3); }
    | exp MINUS exp						        { $$=A_OpExp(EM_tokPos,A_minusOp,$1,$3); }
    | exp TIMES exp						        { $$=A_OpExp(EM_tokPos,A_timesOp,$1,$3); }
    | exp DIVIDE exp			   		      { $$=A_OpExp(EM_tokPos,A_divideOp,$1,$3); }
    | exp EQ exp						          { $$=A_OpExp(EM_tokPos,A_eqOp,$1,$3); }
    | exp NEQ exp						          { $$=A_OpExp(EM_tokPos,A_neqOp,$1,$3); }
    | exp OR exp                      { $$=A_IfExp(EM_tokPos,$1,A_IntExp(EM_tokPos,1),$3); }
    | exp AND exp                     { $$=A_IfExp(EM_tokPos,$1,$3,A_IntExp(EM_tokPos,0)); }
    | IF exp THEN exp                 { $$=A_IfExp(EM_tokPos,$2,$4,NULL); }
    | IF exp THEN exp ELSE exp        { $$=A_IfExp(EM_tokPos,$2,$4,$6); }
    | LET decs IN expseq END          { $$=A_LetExp(EM_tokPos,$2,A_SeqExp(EM_tokPos,$4)); }
    | FOR ID ASSIGN exp TO exp DO exp { $$=A_ForExp(EM_tokPos,S_Symbol($2),$4,$6,$8); }
    | WHILE exp DO exp                { $$=A_WhileExp(EM_tokPos,$2,$4); }
    | ID LBRACK exp RBRACK OF exp     { $$=A_ArrayExp(EM_tokPos,S_Symbol($1),$3,$6); }
    | ID LBRACE RBRACE                { $$=A_RecordExp(EM_tokPos,S_Symbol($1),NULL); }
    | ID LBRACE efields RBRACE        { $$=A_RecordExp(EM_tokPos,S_Symbol($1),$3); }
    | ID LPAREN RPAREN                { $$=A_CallExp(EM_tokPos,S_Symbol($1),NULL); }
    | ID LPAREN exps RPAREN           { $$=A_CallExp(EM_tokPos,S_Symbol($1),$3); }

field : ID COLON ID          { $$=A_Field(EM_tokPos,S_Symbol($1),S_Symbol($3)); }
fields :                     { $$=NULL; }
       | field               { $$=A_FieldList($1,NULL); }
       | field COMMA fields  { $$=A_FieldList($1,$3); }

efield: ID EQ exp { $$=A_Efield(S_Symbol($1),$3); }

efields: efield               { $$=A_EfieldList($1,NULL); }
       | efield COMMA efields { $$=A_EfieldList($1,$3); }

namety   : ID                   { $$=A_NameTy(EM_tokPos,S_Symbol($1)); }
recordty : LBRACE fields RBRACE { $$=A_RecordTy(EM_tokPos,$2); }
arrayty  : ARRAY OF ID          { $$=A_ArrayTy(EM_tokPos,S_Symbol($3)); }

ty : TYPE ID EQ namety   { $$=A_Namety(S_Symbol($2),$4); }
   | TYPE ID EQ recordty { $$=A_Namety(S_Symbol($2),$4); }
   | TYPE ID EQ arrayty  { $$=A_Namety(S_Symbol($2),$4); }

tys : ty %prec LOW { $$=A_NametyList($1,NULL); }
    | ty tys       { $$=A_NametyList($1,$2); }

fun : FUNCTION ID LPAREN fields RPAREN EQ exp          { $$=A_Fundec(EM_tokPos,S_Symbol($2),$4,NULL,$7); }
    | FUNCTION ID LPAREN fields RPAREN COLON ID EQ exp { $$=A_Fundec(EM_tokPos,S_Symbol($2),$4,S_Symbol($7),$9); }

funs : fun %prec LOW { $$=A_FundecList($1,NULL); }
     | fun funs      { $$=A_FundecList($1,$2); }

tydec  : tys                        { $$=A_TypeDec(EM_tokPos,$1); }
vardec : VAR ID ASSIGN exp          { $$=A_VarDec(EM_tokPos,S_Symbol($2),NULL,$4); }
       | VAR ID COLON ID ASSIGN exp { $$=A_VarDec(EM_tokPos,S_Symbol($2),S_Symbol($4),$6); }
fundec : funs                       { $$=A_FunctionDec(EM_tokPos,$1); }

dec : tydec  { $$=$1; }
    | vardec { $$=$1; }
    | fundec { $$=$1; }

decs : dec      { $$=A_DecList($1,NULL); }
     | dec decs { $$=A_DecList($1,$2); }

lvalue : ID %prec LOW             { $$=A_SimpleVar(EM_tokPos,S_Symbol($1)); }
       | lvalue DOT ID            { $$=A_FieldVar(EM_tokPos,$1,S_Symbol($3)); }
       | ID LBRACK exp RBRACK 		{ $$=A_SubscriptVar(EM_tokPos,A_SimpleVar(EM_tokPos,S_Symbol($1)),$3); }
       | lvalue LBRACK exp RBRACK { $$=A_SubscriptVar(EM_tokPos,$1,$3); }

expseq : exp                  { $$=A_ExpList($1,NULL); }
       | exp SEMICOLON        { $$=A_ExpList($1,NULL); }
       | exp SEMICOLON expseq { $$=A_ExpList($1,$3); }

exps : exp            { $$=A_ExpList($1,NULL); }
     | exp COMMA exps { $$=A_ExpList($1,$3); }
