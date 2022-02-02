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
    char *localArgs[] = {"R", "--silent"};
    init_R(sizeof(localArgs)/sizeof(localArgs[0]), localArgs);
    source("foo.R");
    bar1();

    end_R();
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
    SEXP e;

    PROTECT(e = lang2(install("source"), mkString(name)));
    R_tryEval(e, R_GlobalEnv, NULL);
    UNPROTECT(1);
}

/* 
  Call the function foo() with 3 arguments, 2 of which
  are named.
   foo(pch="+", id = 123, c(T,F))

  Note that PrintValue() of the expression seg-faults.
  We have to set the print name correctly.
*/

void
bar1() 
{
    SEXP fun, pch;
    SEXP e;

    PROTECT(e = allocVector(LANGSXP, 4));
    fun = findFun(install("foo"), R_GlobalEnv);
    if(fun == R_NilValue) {
	fprintf(stderr, "No definition for function foo. Source foo.R and save the session.\n");
	UNPROTECT(1);
	exit(1);
    }
    SETCAR(e, fun);

    SETCADR(e, mkString("+"));
    SET_TAG(CDR(e), install("pch"));

    SETCADDR(e, ScalarInteger(123));   
    SET_TAG(CDR(CDR(e)), install("id"));

    pch = allocVector(LGLSXP, 2);
    LOGICAL(pch)[0] = TRUE;
    LOGICAL(pch)[1] = FALSE;
    SETCADDDR(e, pch);   

    PrintValue(e);
    eval(e, R_GlobalEnv);

    SETCAR(e, install("foo"));
    PrintValue(e);
    R_tryEval(e, R_GlobalEnv, NULL);

    UNPROTECT(1);
}

