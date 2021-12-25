/*
 * parse.c - Parse source file.
 */

#include <stdio.h>
#include "util.h"
#include "symbol.h"
#include "absyn.h"
#include "errormsg.h"
#include "parse.h"

extern FILE *yyin;
extern A_exp absyn_root;
extern int yyparse(void);
extern void* yy_scan_string(const char *);
extern void yy_switch_to_buffer(void*);
extern void yy_delete_buffer(void*);

/* parse source file fname; 
   return abstract syntax data structure */
A_exp parsefile(string fname) {
  EM_reset(fname);

  yyin = fopen(fname, "r");
  if (!yyin)
    fprintf(stderr, "Can't open file: %s\n", fname);

  if (yyparse() != 0)
    fprintf(stderr, "Parsing failed.\n");

  fclose(yyin);
  return absyn_root;
}

A_exp parsestring(string syntax) {
  EM_reset("repl");

  void* buffer = yy_scan_string(syntax);
  yy_switch_to_buffer(buffer);

  if (yyparse() != 0)
    fprintf(stderr, "Parsing failed.\n");

  yy_delete_buffer(buffer);
  return absyn_root;
}
