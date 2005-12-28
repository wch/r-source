/*
Can be built with VC++6 by
cl /MT /Ox /c Rchtml.c
link /dll /out:Rchtml.dll Rchtml.obj user32.lib htmlhelp.lib advapi32.lib
*/

#define WIN32_LEAN_AND_MEAN 1
#include <windows.h>
#include <htmlhelp.h>

static char htmlref[256];

__declspec(dllexport) void Rchtml(char **file, char **ptopic, int *error)
{
    char *topic =*ptopic;
    HWND rc;
    
    strcpy(htmlref, *file);
    if(topic && strlen(topic)) {
	strcat(htmlref, "::/");
	strcat(htmlref, topic);
	strcat(htmlref, ".html");
    }
    rc = HtmlHelp(GetDesktopWindow(), htmlref, HH_DISPLAY_TOPIC, 0);
    *error = (rc == (HWND)NULL);
}
