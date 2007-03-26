#include "embeddedRCall.h"

int
main(int argc, char *argv[])
{
    eval_R_command("print", argc, argv);
    return(0);
}
