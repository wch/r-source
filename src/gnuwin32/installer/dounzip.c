#include <windows.h>
#include <string.h>
#include "structs.h"
#include "graphapp.h"

extern int WINAPI
Wiz_SingleEntryUnzip(int, char **, int, char **, LPDCL, LPUSERFUNCTIONS);

int hFile;              /* file handle */

LPUSERFUNCTIONS lpUserFunctions;
HANDLE hUF = (HANDLE)NULL;
LPDCL lpDCL = NULL;
HANDLE hDCL = (HANDLE)NULL;
HINSTANCE hUnzipDll;
HANDLE hZCL = (HANDLE)NULL;


extern int WINAPI UnzDisplayBuf(LPSTR, unsigned long);

static void WINAPI
ReceiveDllMessage(unsigned long ucsize, unsigned long csiz,
    unsigned cfactor,
    unsigned mo, unsigned dy, unsigned yr, unsigned hh, unsigned mm,
    char c, LPSTR filename, LPSTR methbuf, unsigned long crc, char fCrypt)
{
}

static int WINAPI password(char *p, int n, const char *m, const char *name)
{
    return 1;
}

static int WINAPI ReplaceYes(char *filename) {return 1;}

static int WINAPI ReplaceNo(char *filename) {return 0;}

static int WINAPI UnzipCallBack(LPCSTR member, unsigned long size)
{
    if(peekevent()) doevent();
    return 0;
}

int do_unzip(char *zipname, char *dest, int nfiles, char **files,
	    int nxfiles, char **xfiles, int over)
{
    int retcode;

    hDCL = GlobalAlloc(GPTR, (DWORD) sizeof(DCL));
    if (!hDCL) return 1;
    lpDCL = (LPDCL) GlobalLock(hDCL);
    if (!lpDCL) { GlobalFree(hDCL); return 1; }

    hUF = GlobalAlloc( GPTR, (DWORD) sizeof(USERFUNCTIONS));
    if (!hUF) { GlobalUnlock(hDCL); GlobalFree(hDCL); return 1;}

    lpUserFunctions = (LPUSERFUNCTIONS) GlobalLock(hUF);
    if (!lpUserFunctions)
    {
	GlobalUnlock(hDCL); GlobalFree(hDCL); GlobalFree(hUF);
	return 1;
    }
    lpUserFunctions->password = password;
    lpUserFunctions->print = UnzDisplayBuf;
    lpUserFunctions->sound = NULL;
    if(over) lpUserFunctions->replace = ReplaceYes;
    else     lpUserFunctions->replace = ReplaceNo;
    lpUserFunctions->SendApplicationMessage = ReceiveDllMessage;
    lpUserFunctions->ServCallBk = UnzipCallBack;


    lpDCL->ncflag = 0; /* Write to stdout if true */
    lpDCL->fQuiet = 0; /* We want all messages.
			  1 = fewer messages,
			  2 = no messages */
    lpDCL->ntflag = 0; /* test zip file if true */
    lpDCL->nvflag = 0; /* give a verbose listing if true */
    lpDCL->nUflag = 0; /* Do not extract only newer */
    lpDCL->nzflag = 0; /* display a zip file comment if true */
    lpDCL->ndflag = 1; /* Recreate directories if true */
    lpDCL->noflag = over > 0; /* Over-write all files if true */
    lpDCL->naflag = 0; /* Do not convert CR to CRLF */
    lpDCL->lpszZipFN = zipname; /* The archive name */
    lpDCL->lpszExtractDir = dest; /* The directory to extract to. This is set
				     to NULL if you are extracting to the
				     current directory.
				  */
    retcode = Wiz_SingleEntryUnzip(nfiles, files, nxfiles, xfiles,
				      lpDCL, lpUserFunctions);
    if (hDCL) { GlobalUnlock(hDCL); GlobalFree(hDCL); }
    if (hUF) { GlobalUnlock(hUF); GlobalFree(hUF); }
    return 0;
}

