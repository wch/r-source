#include <tcl.h>

void tcltk_init();
#include "R_ext/Rdynload.h"

typedef void (* DL3)();
extern __declspec(dllimport) void (* R_tcldo)();

void _R_tcldo()
{
    Tcl_ServiceAll();
}

static void (* old_R_tcldo)();

void tcltk_start()
{
    tcltk_init(); /* won't return on error */
    old_R_tcldo = R_tcldo;
    R_tcldo = (DL3) R_FindSymbol("_R_tcldo", "TclTk", NULL);
}

void tcltk_end()
{
    R_tcldo = old_R_tcldo;
}
