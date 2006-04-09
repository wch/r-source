/*
  Tests using the postcript device from within 
  an application that embeds the R interpreter.
  Equivalent of evaluating the expressions:
    postscript()
    plot(1:100)
    q()
 */
#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>

#include "embeddedRCall.h"

int
main(int argc, char *argv[])
{
    SEXP fun, arg;
    SEXP e, e1;
    /* char *localArgs[] = {"R", "--no-save", "--silent"}; */
    init_R(argc, argv);

      /* postscript() */
    PROTECT(fun = Rf_findFun(Rf_install("postscript"),  R_GlobalEnv));  
    PROTECT(e = allocVector(LANGSXP, 1));
    SETCAR(e, fun);
    eval(e, R_GlobalEnv);
    UNPROTECT(2);


    /* expression 1:100 */

    fun = Rf_findFun(Rf_install("plot"),  R_GlobalEnv);
    PROTECT(fun);
         /*  1:100 */
    PROTECT(e1 = allocVector(LANGSXP, 3));
    SETCAR(e1, Rf_install(":"));
    SETCAR(CDR(e1), arg = NEW_INTEGER(1));
    INTEGER_DATA(arg)[0] = 1;
    SETCAR(CDR(CDR(e1)), arg = NEW_INTEGER(1));
    INTEGER_DATA(arg)[0] = 100;
    

    arg = e1;
    e = allocVector(LANGSXP, 3);
    PROTECT(e);
    SETCAR(e, fun);
    SETCAR(CDR(e), arg);

       /* plot( 1:100 )*/
    eval(e, R_GlobalEnv);

    UNPROTECT(2);

    /* q() */
    PROTECT(fun = Rf_findFun(Rf_install("q"),  R_GlobalEnv));  
    PROTECT(e = allocVector(LANGSXP, 2));
    SETCAR(e, fun);
    SETCAR(CDR(e), arg = NEW_CHARACTER(1));
    SET_STRING_ELT(arg, 0, mkChar("no"));
    eval(e, R_GlobalEnv);
    UNPROTECT(2);

    return(0);
}
