#include <R_ext/Boolean.h>
#include <tcl.h>
#define WIN32_LEAN_AND_MEAN 1
#include <windows.h>

Rboolean R_ToplevelExec(void (*fun)(void *), void *data);

void tcltk_init();
static int Tcl_lock = 0; /* reentrancy guard */

extern __declspec(dllimport) void (* R_tcldo)();

static void TclSpinLoop(void *data)
{
    Tcl_ServiceAll();
}

static void _R_tcldo(void)
{
    if (!Tcl_lock) {
        Tcl_lock = 1;
        (void) R_ToplevelExec(TclSpinLoop, NULL);
        Tcl_lock = 0;
    }
}

static void (* old_R_tcldo)();

void tcltk_start()
{
    HWND active = GetForegroundWindow(); /* ActiveTCL steals the focus */
    tcltk_init(); /* won't return on error */
    old_R_tcldo = R_tcldo;
    R_tcldo = &_R_tcldo;
    _R_tcldo();  /* one call to trigger the focus stealing bug */
    SetForegroundWindow(active); /* and fix it */
}

void tcltk_end()
{
    R_tcldo = old_R_tcldo;
}
