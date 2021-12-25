%{
// lex 2.6.4
#include <string.h>
#include "util.h"
#include "symbol.h" 
#include "errormsg.h"
#include "absyn.h"
#include "y.tab.h"

int charPos = 1;

int yywrap(void) {
  charPos=1;
  return 1;
}

void adjust(void) {
  EM_tokPos = charPos;
  charPos += yyleng;
}
%}

digits [0-9]+
identifier [A-Za-z][A-Za-z0-9_]*
textual \"[#-~\ \\!]*\"

%s COMMENT
%%
<INITIAL>" "	         {adjust(); continue;}
<INITIAL>\t	           {adjust(); continue;}
<INITIAL>\n	           {adjust(); EM_newline(); continue;}
<INITIAL>","           {adjust(); return COMMA;}
<INITIAL>":"           {adjust(); return COLON;}
<INITIAL>";"           {adjust(); return SEMICOLON;}
<INITIAL>"("           {adjust(); return LPAREN;}
<INITIAL>")"           {adjust(); return RPAREN;}
<INITIAL>"["           {adjust(); return LBRACK;}
<INITIAL>"]"           {adjust(); return RBRACK;}
<INITIAL>"{"           {adjust(); return LBRACE;}
<INITIAL>"}"           {adjust(); return RBRACE;}
<INITIAL>"."           {adjust(); return DOT;}
<INITIAL>"+"           {adjust(); return PLUS;}
<INITIAL>"-"           {adjust(); return MINUS;}
<INITIAL>"*"           {adjust(); return TIMES;}
<INITIAL>"/"           {adjust(); return DIVIDE;}
<INITIAL>"="           {adjust(); return EQ;}
<INITIAL>"<>"          {adjust(); return NEQ;}
<INITIAL>"<"           {adjust(); return LT;}
<INITIAL>"<="          {adjust(); return LE;}
<INITIAL>">"           {adjust(); return GT;}
<INITIAL>">="          {adjust(); return GE;}
<INITIAL>"&"           {adjust(); return AND;}
<INITIAL>"|"           {adjust(); return OR;}
<INITIAL>":="          {adjust(); return ASSIGN;}
<INITIAL>while         {adjust(); return WHILE;}
<INITIAL>for           {adjust(); return FOR;}
<INITIAL>to            {adjust(); return TO;}
<INITIAL>break         {adjust(); return BREAK;}
<INITIAL>let           {adjust(); return LET;}
<INITIAL>in            {adjust(); return IN;}
<INITIAL>end           {adjust(); return END;}
<INITIAL>function      {adjust(); return FUNCTION;}
<INITIAL>var           {adjust(); return VAR;}
<INITIAL>type          {adjust(); return TYPE;}
<INITIAL>array         {adjust(); return ARRAY;}
<INITIAL>if            {adjust(); return IF;}
<INITIAL>then          {adjust(); return THEN;}
<INITIAL>else          {adjust(); return ELSE;}
<INITIAL>do            {adjust(); return DO;}
<INITIAL>of            {adjust(); return OF;}
<INITIAL>nil           {adjust(); return NIL;}
<INITIAL>{identifier}  {adjust(); yylval.sval=String(yytext); return ID;}
<INITIAL>{textual} {
  adjust();
  // matches strings including double quotes so, "a"
  // would be yytext[0] = '"', yytext[1] = 'a', yytext[2] = '"'
  // this removes double quotes from string tokens.
  yytext[yyleng-1]='\0';

  // process escape characters. (only handles the case of newline.)
  string strtok = malloc(yyleng);
  string txtptr = yytext+1;
  int i;
  for (i=0; *txtptr != '\0';++i) {
    strtok[i] = *txtptr;
    if (*txtptr == '\\')
      switch (txtptr[1]) {
        case 'n':
          strtok[i] = '\n';
          txtptr++;
          break;
      }
    txtptr++;
  }
  strtok[i] = '\0'; // note! may prevent memory leaks in some malloc-free implmentations.
  yylval.sval = String(strtok);
  return STRING;
}
<INITIAL>{digits}      {adjust(); yylval.ival=atoi(yytext); return INT;}
<INITIAL>"/*"          {adjust(); BEGIN COMMENT;}
<INITIAL>.             {adjust(); EM_error(EM_tokPos,"illegal token");}

<COMMENT>"*/"          {adjust(); BEGIN INITIAL;}
<COMMENT>.             {adjust();}
