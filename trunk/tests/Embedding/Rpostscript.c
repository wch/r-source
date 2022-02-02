/*
  Tests using the postcript device from within 
  an application that embeds the R interpreter.
  Equivalent of evaluating the expressions:
    postscript()
    plot(1:100)
    q()
 */
#include "embeddedRCall.h"

int
main(int argc, char *argv[])
{
    SEXP e, e1;
    /* char *localArgs[] = {"R", "--no-save", "--silent"}; */
    init_R(argc, argv);

    /* postscript() */
    PROTECT(e = lang1(install("postscript")));
    eval(e, R_GlobalEnv);
    UNPROTECT(1);

    /* expression 1:100 */

    /*  1:100 */
    PROTECT(e1 = lang3(install(":"), ScalarInteger(1), ScalarInteger(100)));
    PROTECT(e = lang2(install("plot"), e1));
    /* plot( 1:100 )*/
    eval(e, R_GlobalEnv);
    UNPROTECT(2);

    /* q() */
    PROTECT(e = lang2(install("q"), mkString("no")));
    eval(e, R_GlobalEnv);
    UNPROTECT(1);

    end_R();
    return(0);
}
