#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>

#include "embeddedRCall.h"

void bar1() ;
void source(const char *name);
/*
  Creates and evaluates a call 
  to a function giving named arguments
   plot(1:10, pch="+")
 */
int
main(int argc, char *argv[])
{
  char *localArgs[] = {"R", "--gui=none", "--silent"};
  init_R(sizeof(localArgs)/sizeof(localArgs[0]), localArgs);
  source("foo.R");
  bar1();

  return(0);
}


/*
 This arranges for the command source("foo.R")
 to be called and this defines the function we will
 call in bar1.
 */
void
source(const char *name)
{
  SEXP e, tmp, fun;

   PROTECT(e = allocVector(LANGSXP, 2));
   PROTECT(fun = Rf_findFun(Rf_install("source"), R_GlobalEnv));
   SETCAR(e, fun);
   SETCAR(CDR(e), tmp = NEW_CHARACTER(1));
   SET_STRING_ELT(tmp, 0, COPY_TO_USER_STRING("foo.R"));

   Test_tryEval(e, NULL);
}

/* 
  Call the function foo() with 3 arguments, 2 of which
  are named.
   foo(pch="+", id = 123, c(T,F))

  Note that Rf_PrintValue() of the expression seg-faults.
  We have to set the print name correctly.
*/

void
bar1() 
{
  SEXP fun, arg, seq, pch, tmp;
  SEXP e, e1;
  int n = 7;

    PROTECT(e = allocVector(LANGSXP, 4));
    fun = Rf_findFun(Rf_install("foo"), R_GlobalEnv);
    if(GET_LENGTH(fun) == 0) {
      fprintf(stderr, "No definition for function foo. Source foo.R and save the session.\n");
      UNPROTECT(1);
      exit(1);
    }
    PROTECT(fun);
    SETCAR(e, fun);

    PROTECT(pch = NEW_CHARACTER(1));
    SET_STRING_ELT(pch, 0, COPY_TO_USER_STRING("+"));
    SETCAR(CDR(e), pch);   

    SET_TAG(CDR(e), Rf_install("pch"));


    PROTECT(pch = NEW_INTEGER(1));
    INTEGER_DATA(pch)[0] = 123;
    SETCAR(CDR(CDR(e)), pch);   

    SET_TAG(CDR(CDR(e)), Rf_install("id"));

    PROTECT(pch = NEW_LOGICAL(2));
     LOGICAL_DATA(pch)[0] = TRUE;
     LOGICAL_DATA(pch)[1] = FALSE;
    SETCAR(CDR(CDR(CDR(e))), pch);   

    Rf_PrintValue(e);
    eval(e, R_GlobalEnv);

    SETCAR(e, Rf_install("foo"));
    Rf_PrintValue(e);
    Test_tryEval(e, NULL);

    UNPROTECT(n);
}

