#include <windows.h>
#include <stdio.h>
#include "Platform.h"

#define CharacterMode (*__imp_CharacterMode)
#define UserBreak     (*__imp_UserBreak)

extern void cmdlineoptions(int, char **);
extern void setup_term_ui(void);
extern void mainloop(void);
extern int CharacterMode;
extern int UserBreak;

extern char *getDLLVersion();

static char Rversion[25];
char *getRVersion()
{
    sprintf(Rversion, "%s.%s", R_MAJOR, R_MINOR);
    return(Rversion);
}

static BOOL hCtrlc(DWORD type)
{
    if (type == CTRL_BREAK_EVENT) 
	UserBreak = 1;
    return TRUE;
}


int AppMain (int argc, char **argv)
{
    CharacterMode = 1;
    if(strcmp(getDLLVersion(), getRVersion()) != 0) {
	fprintf(stderr, "Error: R.DLL version does not match\n");
	exit(1);
    }
    if (isatty(0)) 
	FlushConsoleInputBuffer(GetStdHandle(STD_INPUT_HANDLE));
    cmdlineoptions(argc, argv);
    SetConsoleCtrlHandler(hCtrlc, TRUE);
    setup_term_ui();
    mainloop();
}
