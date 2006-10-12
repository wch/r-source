#include <tcl.h>
#define WIN32_LEAN_AND_MEAN 1
#include <windows.h>

void tcltk_init();

extern __declspec(dllimport) void (* R_tcldo)();

static void _R_tcldo()
{
    Tcl_ServiceAll();
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
