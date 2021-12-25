cmake_minimum_required(VERSION 3.22.1)
project(tiger-evaluator)
find_package(BISON)
find_package(FLEX)

# using the paradigmatic naming of yacc and lex output files.
BISON_TARGET(TigerParser src/tiger.y ${CMAKE_CURRENT_SOURCE_DIR}/src/y.tab.c
  DEFINES_FILE ${CMAKE_CURRENT_SOURCE_DIR}/src/y.tab.h)
FLEX_TARGET(TigerLexer src/tiger.l ${CMAKE_CURRENT_SOURCE_DIR}/src/lex.yy.c
  DEFINES_FILE ${CMAKE_CURRENT_SOURCE_DIR}/src/lex.yy.h)
ADD_FLEX_BISON_DEPENDENCY(TigerLexer TigerParser)

set_source_files_properties(
  ${BISON_TigerParser_OUTPUTS}
  ${FLEX_TigerLexer_OUTPUTS}
  PROPERTIES COMPILE_OPTIONS "-Wno-all"
)

add_executable(tiger)
target_sources(tiger
  PRIVATE
  src/tiger.c
  src/absyn.c
  src/prabsyn.c
  src/env.c
  src/eval.c
  src/errormsg.c
  src/parse.c
  src/semant.c
  src/symbol.c
  src/table.c
  src/types.c
  src/util.c
  ${BISON_TigerParser_OUTPUTS}
  ${FLEX_TigerLexer_OUTPUTS}
)
target_compile_options(tiger
  PRIVATE -Wall
)
