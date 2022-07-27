/* Evaluates a tiger expression.
*/

struct _val {
  enum {V_string, V_integer, V_void, V_array, V_record} kind;
  union {
    int integer;
    string string;
    S_table record;
    struct _val** array;
  } u;
};
typedef struct _val* val;

// evaluate an tiger expresion
val eval(A_exp exp, S_table venv);

// display a datum to stdout
void printval(val v);
