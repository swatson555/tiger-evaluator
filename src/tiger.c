#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "util.h"
#include "symbol.h"
#include "absyn.h"
#include "prabsyn.h"
#include "semant.h"
#include "parse.h"
#include "eval.h"

void printusage() {
  printf("tiger filename\n");
  printf("tiger --help           - Prints this message.\n");
  printf("tiger --repl           - Creates a tiger repl session.\n");
  printf("tiger --print-ast      - Prints abstract syntax tree to stdout.\n");
  printf("tiger --eval syntax    - Evaluates tiger syntax string.\n");
  printf("tiger --file filename  - Evaluates a file.\n");
}

void evalfromfilename(string filename, int showast, S_table valueenv) {
  A_exp exp = parsefile(filename);
  if (showast)
    printast(exp);
  SEM_transProg(exp);
  val result = evalexp(exp, valueenv);
  printval(result);
}

void evalfromstring(string syntax, int showast, S_table valueenv) {
  A_exp exp = parsestring(syntax);
  if (showast)
    printast(exp);
  SEM_transProg(exp);
  val result = evalexp(exp, valueenv);
  printval(result);
}

void repl(S_table valueenv) {
  char buffer[128];
  printf("tiger>> ");
  while(fgets(buffer, 128, stdin)) {
    A_exp exp = parsestring(buffer);
    SEM_transProg(exp);
    val result = evalexp(exp, valueenv);
    printval(result);
    printf("tiger>> ");
  }
}

int isopt(string arg){
  return arg[0] == '-';
}

string getoptname(string arg) {
  if (arg[0] == '-') {
    if (arg[1] == '-') {
      return arg+2;
    }
    return arg+1;
  }
  fprintf(stderr, "no opt name %s\n", arg);
  return NULL;
}

int main(int argc, char **argv) {
  int withrepl = 0;
  int printast = 0;
  string filename = 0;
  string syntaxstring = 0;

  // The global environment for evaluation.
  S_table valueenv = S_empty();

  /* Special case of no arguments.
   */
  if (argc == 1) {
    printusage();
    return 0;
  }

  /* Special case of one argument.
   */
  if (argc == 2 && !isopt(argv[1])) {
    evalfromfilename(argv[1], 0, valueenv);
    return 0;
  }

  /* General case process all arguments.
   */
  while (--argc) {
    argv++;
    if (isopt(*argv)) {
      string option = getoptname(*argv);
      if (strcmp(option, "help") == 0) {
        printusage();
      }
      if (strcmp(option, "repl") == 0) {
        withrepl = 1;
      }
      if (strcmp(option, "print-ast") == 0) {
        printast = 1;
      }
      if (strcmp(option, "eval") == 0) {
        argv++;
        argc--;
        syntaxstring = *argv;
      }
      if (strcmp(option, "file") == 0) {
        argv++;
        argc--;
        filename = *argv;
      }
    }
  }

  if (filename) {
    evalfromfilename(filename, printast, valueenv);
  }

  if (syntaxstring) {
    evalfromstring(syntaxstring, printast, valueenv);
  }

  if (withrepl) {
    repl(valueenv);
  }
  return 0;
}
