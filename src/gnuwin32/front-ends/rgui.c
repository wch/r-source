#include <windows.h>
#include "Platform.h"

#define CharacterMode (*__imp_CharacterMode)

extern void cmdlineoptions(int, char **);
extern int setupui(void);
extern void mainloop(void);
extern  int CharacterMode;

extern char *getDLLVersion();

static char Rversion[25];
char *getRVersion()
{
    sprintf(Rversion, "%s.%s", R_MAJOR, R_MINOR);
    return(Rversion);
}

int AppMain (int argc, char **argv)
{
    CharacterMode = 0;
    if(strcmp(getDLLVersion(), getRVersion()) != 0) {
	MessageBox(0, "R.DLL version does not match", "Terminating",
		   MB_TASKMODAL | MB_ICONSTOP | MB_OK);
	exit(1);
    }
    cmdlineoptions(argc, argv);
    setupui();
    mainloop();
}
