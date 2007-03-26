/*
  Tests using the default graphics device from within 
  an application that embeds the R interpreter.
  Equivalent of evaluating the expressions:

     plot(c(1, 2, 3, 4, 5, 6, 7 ,8, 9, 10))
*/
#include "embeddedRCall.h"


int
main(int argc, char *argv[])
{
    /* Evaluates the expression 
       plot(c(1,2,3,4,5,6,7,8,9,10))
    */
    eval_R_command("plot", argc, argv); 
    return(0);
}

