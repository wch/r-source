#include <tcl.h>

void tcltk_init();

typedef void (* DL3)();
typedef void * (*DL_FUNC)();
DL_FUNC R_FindSymbol(char const *, char const *);
extern void (* R_tcldo)();

void _R_tcldo()
{
    Tcl_ServiceAll();
}

void tcltk_start()
{
    tcltk_init(); /* won't return on error */
    R_tcldo = (DL3) R_FindSymbol("_R_tcldo", "TclTk");
}
