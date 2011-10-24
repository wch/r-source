#include <windows.h>

/* The mingw-runtime startup code has _argc and _argv as visible
   symbols, as do the MS compilers.

   The mingw-w64-crt is different.
*/

extern void 
GA_startgraphapp(HINSTANCE Instance, HINSTANCE PrevInstance, int CmdShow);

int PASCAL
WinMain (HINSTANCE Instance, HINSTANCE PrevInstance, LPSTR CmdLine,
	 int CmdShow)
{
    extern void AppMain(int argc, char **argv);

#ifdef _W64
    extern int __argc;
    extern char **__argv;

    GA_startgraphapp(Instance, PrevInstance, CmdShow);
    AppMain(__argc, __argv);
#else
    extern int _argc;
    extern char **_argv;

    GA_startgraphapp(Instance, PrevInstance, CmdShow);
    AppMain(_argc, _argv);
#endif
    return 0;
}
