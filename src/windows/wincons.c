 /*  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (C) 1995  Robert Gentleman and Ross Ihaka
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include "wincons.h"
#include "shellapi.h"
#include "Fileio.h"
#include "Graphics.h"

#include "IOStuff.h"
#include "Parse.h"

#define STRICT




/*    General Comments

        R For Windows is implemented useing the Multiple Document Interface. The
        basics were taken from Petzold, Programming Windows 95 and in many places
        ideas were borrowed from Luke Tierney's XLisp-Stat. The errors I made up
        myself.
        The MDI has a main frame that manages all of the client (or child windows).
        For R we have a main frame and then the console window, the graphics window,
        the data entry window and a pop-up editor window all created under the graphics
        window.

        Window Variables
        ===============
        RInst - The MDI instance.
        RFrame - The MDI frame.
        RClient - The MDI client window. Handling window for the MDI.
        RConsole - The Console window
        RConsoleFrame - The Frame for the console window.

        RGraphWnd -  The Graphics window.
        REditWnd  -  The editor window.
        RDEWnd    -  The data entry window.

        Both the Console and the EditWnd have frames that are MDI child windows. This lets them
        handle the menu changes that are required when they come to the top. Within that
        frame they have a Windows multiline edit dialogue window running.

        
PASTING TEXT: Text is pasted into the editor one line at a time. I don't see a better
way to do it given that we need to stick '\r' in at the end of each line and the parser
is really dealing with a line at a time. When the console editor runs out of room the top
lines are cut out of the buffer and the remaining lines shifted down.

*/
HINSTANCE RInst;
HWND RFrame, RClient, RConsoleFrame, RConsole;

char RGraphClass[] = "RGraphClass";
char REditClass[] = "REditClass";
char RDEClass[] = "RDEClass";


HMENU RMenuConsole, RMenuConsWin;
HANDLE hAccel;
static HANDLE RPasteText;
static LPSTR RPasteP;
static int RPasteLen;

/*
    InStart locates the position in the text buffer of the end of the
    last statement executed. Anything from here to the end can be modified.

    InFlag is used to see when a carriage return is pressed and hence
    a statement should be sent to the grammar.
*/

static int InStart;
static int InFlag;
static int RClosing;

static char RFrameClass[] = "RFrame";
static char RConsoleClass[] = "RConsole";
char RFName[RBuffLen];

int WINAPI  WinMain(HINSTANCE, HINSTANCE, PSTR, int);

LRESULT CALLBACK MainWndProc(HWND, UINT, WPARAM, LPARAM);
BOOL CALLBACK CloseEnumProc(HWND, LPARAM);
BOOL CALLBACK Menu_SetMem(HWND, UINT, WPARAM, LPARAM);

HWND CreateConsoleWind(HWND, HMENU, HANDLE);
LRESULT CALLBACK _export ConsoleWndProc(HWND,UINT,WPARAM,LPARAM);
LRESULT CALLBACK _export EdWndProc(HWND,UINT,WPARAM,LPARAM);
LRESULT CALLBACK _export GraphWndProc(HWND,UINT,WPARAM,LPARAM);
LRESULT CALLBACK _export REditWndProc(HWND,UINT,WPARAM,LPARAM);
LRESULT CALLBACK _export RDEWndProc(HWND,UINT,WPARAM,LPARAM);

static void menuOpen();
static void menuSave();
static void menuLoad();

static void RSetCursor(void);

/* clipboard functions */

static void RClearSel(void);
static void RPasteFromClip(void);
static void RSelToClip(void);
static void RTrimBuffer(void);

static WNDPROC lpfnOldEd;

/* some DDE stuff */
HWND hWndServerDDE;


#pragma argsused

int mbquery(void) {
    int save;

    save = MessageBox(RFrame, "Do you want to save the image?","R Save",
                        MB_YESNOCANCEL | MB_DEFBUTTON1 | MB_ICONQUESTION | MB_APPLMODAL);
    return (save);
}
    
void R_CleanUp(int ask)
{
    int save=IDYES;
    
        if( R_DirtyImage ) {
             if( ask == 1)  /* query save */
                save = mbquery();
             else if (ask == 3)  /* save without query */
                save = IDYES;
             else /* don't save */
                save = IDNO;
             if( save == IDCANCEL) { /* cancel */
                RClosing = 0;
                jump_to_toplevel();
             }
             if( save == IDYES ) {
                strcpy(RFName, R_ImageName);
                if( !Win_RSaveDlg(RClient) )
                        error("cannot save requested file\n");
                dump_image(RFName,0);
             }
             R_DirtyImage = 0; /*hack to allow WM_CLOSE to check for a dirty image */
        }
        PostMessage(RFrame, WM_CLOSE, 0, 0);
}

int EventLoop()
{
        MSG msg;
        int sstart;
        HWND FrameClient;

        if (RPasteText!=NULL) {
          if(RPasteLen-- > 0)
                SendMessage(RConsole, WM_CHAR, *RPasteP++,0);
          else {
                GlobalUnlock(RPasteText);
                GlobalFree(RPasteText);
                RPasteText=NULL;
                /*set the caret to the end of the pasted text */
                sstart=Edit_GetTextLength(RConsole);
                Edit_SetSel(RConsole, sstart, sstart);
          }
        }
        else {
            FrameClient = GetWindow(RFrame, GW_CHILD);
                if( GetMessage(&msg,NULL,NULL,NULL) ){
                        if( !TranslateMDISysAccel(FrameClient, &msg)
                                &&!TranslateAccelerator(RFrame, hAccel, &msg) )
                        {
                                TranslateMessage(&msg);
                                DispatchMessage(&msg);
                        }
                }
                else {  /* we are exiting */
                  InFlag = 3;
                }  
        }
        
        return msg.wParam;
}

void WinConfig(void)
{
        ATOM atomApp;
        HANDLE hCommand;
        LPSTR lpCommand;

        atomApp=GlobalAddAtom((LPSTR) "PROGMAN");

        SendMessage((HWND) -1,
                        WM_DDE_INITIATE,
                        (WPARAM) RFrame,
                        MAKELONG( atomApp, atomApp));

        GlobalDeleteAtom(atomApp);
        if(  hWndServerDDE==NULL )
                goto errcd;

        if( !(hCommand = GlobalAlloc(GMEM_MOVEABLE | GMEM_DDESHARE,256)) )
                                goto errcd;
        if( !(lpCommand = GlobalLock(hCommand))) {
                GlobalFree(hCommand);
                goto errcd;
        }
        /*[ShowGroup(\"Foo\",1)][AddItem(c:\\r\\proj99.exe, \"R Project\",c:\\r\\proj99.exe,2)]*/
        lstrcpy(lpCommand,"[CreateGroup(\"R Application\")][AddItem(c:\\r\\proj99.exe, \"R Project\"][ExitProgman]");
        GlobalUnlock(hCommand);
        if( !PostMessage(hWndServerDDE, WM_DDE_EXECUTE, (WPARAM) RFrame,
                PackDDElParam(WM_DDE_EXECUTE, 0, (UINT) hCommand)) ) {
                        GlobalFree(hCommand);
                        goto errcd;
                }

        PostMessage(hWndServerDDE, WM_DDE_TERMINATE, PackDDElParam(WM_DDE_TERMINATE,
         (UINT) RFrame, 0),0);

        /* GlobalFree(hCommand);  */
        return;

errcd: MessageBox( RFrame, "Cannot configure for this machine.","R Application",
                MB_ICONEXCLAMATION | MB_OK);
        return;
}
/*
        use a MDI interface for the console; the actual console will be
        a client window under the base MDI window
*/
BOOL InitApplication(HINSTANCE hinstCurrent)
{
        WNDCLASS wc;
		DWORD errno;

        wc.style=CS_HREDRAW | CS_VREDRAW ;
        wc.lpfnWndProc = MainWndProc;
        wc.cbClsExtra = 0;
        wc.cbWndExtra = 0;
        wc.hInstance = hinstCurrent;
        wc.hIcon = LoadIcon(NULL, IDI_APPLICATION);
        wc.hCursor = LoadCursor(NULL,IDC_ARROW);
        wc.hbrBackground = GetStockObject(WHITE_BRUSH);
        wc.lpszMenuName = "Foo";
        wc.lpszClassName = RFrameClass;
        if( !RegisterClass(&wc) ) {
			errno = GetLastError();
			return(FALSE);
		}

        wc.style=CS_HREDRAW | CS_VREDRAW;
        wc.lpfnWndProc= ConsoleWndProc;
        wc.cbClsExtra=0;
        wc.cbWndExtra=0;
        wc.hInstance=hinstCurrent;
        wc.hIcon = LoadIcon(NULL, IDI_APPLICATION);
        wc.hCursor = LoadCursor(NULL,IDC_ARROW);
        wc.hbrBackground = GetStockObject(WHITE_BRUSH);
        wc.lpszMenuName = NULL;
        wc.lpszClassName = RConsoleClass;
        if( !RegisterClass(&wc) ) return(FALSE);

        wc.style=CS_HREDRAW | CS_VREDRAW;
        wc.lpfnWndProc= REditWndProc;
        wc.cbClsExtra=0;
        wc.cbWndExtra=0;
        wc.hInstance=hinstCurrent;
        wc.hIcon = LoadIcon(NULL, IDI_APPLICATION);
        wc.hCursor = LoadCursor(NULL,IDC_ARROW);
        wc.hbrBackground = GetStockObject(WHITE_BRUSH);
        wc.lpszMenuName = NULL;
        wc.lpszClassName = REditClass;
        if( !RegisterClass(&wc) ) return(FALSE);


        wc.style=CS_HREDRAW | CS_VREDRAW | CS_OWNDC;
        wc.lpfnWndProc = GraphWndProc;
        wc.cbClsExtra=0;
        wc.cbWndExtra=sizeof(DevDesc*);
        wc.hInstance=hinstCurrent;
        wc.hIcon=LoadIcon(NULL, IDI_APPLICATION);
        wc.hCursor=LoadCursor(NULL,IDC_CROSS);
        wc.hbrBackground=GetStockObject(WHITE_BRUSH);
        wc.lpszMenuName=NULL;
        wc.lpszClassName = RGraphClass;
        if( !RegisterClass(&wc) ) return(FALSE);

        wc.style=CS_HREDRAW | CS_VREDRAW | CS_OWNDC;
        wc.lpfnWndProc = RDEWndProc;
        wc.cbClsExtra=0;
        wc.cbWndExtra=0;
        wc.hInstance=hinstCurrent;
        wc.hIcon=LoadIcon(NULL, IDI_APPLICATION);
        wc.hCursor=LoadCursor(NULL,IDC_CROSS);
        wc.hbrBackground=GetStockObject(WHITE_BRUSH);
        wc.lpszMenuName=NULL;
        wc.lpszClassName = RDEClass;
        if( !RegisterClass(&wc) ) return(FALSE);


        /* set up the menu handles */
        hAccel=LoadAccelerators(hinstCurrent, "MdiAccel");

        RMenuConsole=LoadMenu(hinstCurrent, "RMenuConsole");
        RMenuConsWin=GetSubMenu(RMenuConsole, 3);
        
        RMenuGraph=LoadMenu(hinstCurrent, "RMenuGraph");
        RMenuGraphWin=GetSubMenu(RMenuGraph, 1);
        RMenuEdit = LoadMenu(hinstCurrent, "RMenuTEd");
        RMenuEditWin = GetSubMenu(RMenuEdit, 1);
        RMenuDE=LoadMenu(hinstCurrent, "RMenuDE");
        RMenuDEWin=GetSubMenu(RMenuDE, 1);
      
        hWndServerDDE=NULL;

        return TRUE;
}

BOOL InitInstance(HINSTANCE hinstCurrent,int nCmdShow)
{
        MDICREATESTRUCT mdicreate;
        RECT r;

        RInst=hinstCurrent;

        RFrame = CreateWindow(
                RFrameClass,
                "R",
                WS_OVERLAPPEDWINDOW | WS_CLIPCHILDREN | WS_MAXIMIZE | WS_CLIPSIBLINGS,
                CW_USEDEFAULT,
                CW_USEDEFAULT,
                CW_USEDEFAULT,
                CW_USEDEFAULT,
                NULL, RMenuConsole, hinstCurrent, NULL);

        if( RFrame == NULL )
                return FALSE;
        /* RClient should be set in the creation of RFrame */
        if( RClient == NULL ) {
                DestroyWindow(RFrame);
                return FALSE;
        }

        /* WinConfig();   */

        ShowWindow(RFrame, SW_SHOWMAXIMIZED );
        UpdateWindow(RFrame);


        
        /* create the console */
        GetClientRect(RClient, (LPRECT) &r);
        mdicreate.szClass = RConsoleClass;
        mdicreate.szTitle = "R Console";
        mdicreate.hOwner = hinstCurrent;
        mdicreate.x = CW_USEDEFAULT;
        mdicreate.y = CW_USEDEFAULT;
        mdicreate.cx = (int)(r.right-r.left)*.8;
        mdicreate.cy = CW_USEDEFAULT;
        mdicreate.style = 0;
        mdicreate.lParam = NULL;
        RConsoleFrame = (HWND) (UINT) SendMessage(RClient, WM_MDICREATE,0,
                (LONG) (LPMDICREATESTRUCT) &mdicreate);

        if( RConsoleFrame == NULL )  {
                DestroyWindow(RClient);
                DestroyWindow(RFrame);
                return FALSE;
        }


        RConsole = CreateConsoleWind(RConsoleFrame, NULL, hinstCurrent);
        DragAcceptFiles(RConsole, TRUE);
        if( RConsole == NULL) {
                DestroyWindow(RConsoleFrame);
                DestroyWindow(RClient);
                DestroyWindow(RFrame);
                return FALSE;
        }

        ShowWindow(RConsoleFrame, SW_SHOW);
        UpdateWindow(RConsoleFrame);
        SendMessage(RClient, WM_MDIACTIVATE, (UINT) RConsoleFrame,0);
        SetFocus(RConsoleFrame);

        return TRUE;
}

HWND CreateConsoleWind(HWND hWndParent, HMENU hMenu, HANDLE hInstance)
{
  RECT Rect;
  HWND hWnd;

  GetClientRect(hWndParent, (LPRECT) &Rect);
  /* the (HMENU) RRR_EDIT is used by ConsoleWndProc to trap EN_**** messages */
  hWnd = CreateWindow("Edit",
                      NULL,
                      WS_CHILD | WS_VISIBLE | WS_VSCROLL | WS_HSCROLL |
                                ES_MULTILINE | ES_AUTOVSCROLL | ES_AUTOHSCROLL ,
                                0,
                                0,
                                (Rect.right - Rect.left),
                                (Rect.bottom - Rect.top),
                                hWndParent,
                                (HMENU) RRR_EDIT,
                                hInstance,
                                NULL);
  lpfnOldEd=(WNDPROC)GetWindowLong(hWnd,GWL_WNDPROC);
  SetWindowLong(hWnd,GWL_WNDPROC,(LONG) EdWndProc);
  /* set the font to be fixed width */
  SendMessage(hWnd, WM_SETFONT, (WPARAM)GetStockObject(OEM_FIXED_FONT),
        MAKELPARAM(TRUE,0L));
  return(hWnd);
}
/* win = 1 ; console
   win = 2 ; editor
   win = 3 ; data.entry
   */
   
void R_ProcessDropFiles(HANDLE dropstruct, int win)
{
    char dfilename[MAXELTSIZE];
    int nFiles, len, status;
    SEXP expr;
    FILE *fp;

    nFiles = DragQueryFile(dropstruct, 0XFFFFFFFF, NULL, 0);
    if( nFiles > 1 )  /* only process one file to be sure we destroy dropstruct */
        warning("drag/drop: only one file will be processed\n");
    len = DragQueryFile(dropstruct, 0, NULL, 0);
    if( len > MAXELTSIZE ) {
          DragFinish(dropstruct);
          error("file name to long to process\n");
     }
    DragQueryFile(dropstruct, 0, dfilename, MAXELTSIZE);
    DragFinish(dropstruct);
    if( !(fp=R_fopen(dfilename, "rt")) )   
        error("couldn't find dropped file\n");
    switch (win) {
        case 1:
        PROTECT(expr = R_ParseFile(fp, -1, &status));
        Rprintf("Parsing %s \n",dfilename);
        if( status == PARSE_ERROR ) 
            error("drag-drop: an error occurred in parsing");
        eval(expr, R_GlobalEnv);
        UNPROTECT(1);
        break;
        case 2:
                strcpy(REdfilename, dfilename);
                break;
        default:
         Rprintf("ohoh\n");
    }
}


LRESULT CALLBACK  EdWndProc(HWND hWnd, UINT message, WPARAM wParam,
        LPARAM lParam)
{
        DWORD curPos;
        char *p;

        switch(message) {
                case WM_DROPFILES:
                        SetFocus(RConsole);
                        R_ProcessDropFiles((HANDLE) wParam, 1);
                        p=(char *) CHAR(STRING(GetOption(install("prompt"), R_NilValue))[0]);
                        R_WriteConsole(p,strlen(p));
                        return 0;
                case WM_PASTE:
                        RPasteFromClip();
                        return 0;
                case WM_CHAR:
                        curPos=Edit_GetSel(RConsole);
                        if( LOWORD(curPos) < InStart ) {
                                RSetCursor(); /*SysBeep();
                                return 0;*/
                        }
                        switch(wParam) {
                                case '\b':
                                        curPos=Edit_GetSel(RConsole);
                                        if( LOWORD(curPos) <= InStart ) {
                                                SysBeep();
                                                return 0;
                                        }
                                        break;
                                case '\n':
                                case '\r':
                                        InFlag=1;
                                        return 0;
                                        break;
                        }
        }
        return CallWindowProc((FARPROC)lpfnOldEd, hWnd, message, wParam, lParam);
}


LRESULT CALLBACK ConsoleWndProc(HWND hWnd,UINT message,WPARAM wParam,
        LPARAM lParam)
{
    char tmp[RBuffLen], *home;
    int save;
    
        switch(message) {
                case WM_CREATE:
                        return(0);
                case WM_SIZE:
                        MoveWindow(RConsole, 0, 0, LOWORD(lParam), HIWORD(lParam), TRUE);
                        if (! IsIconic(hWnd)) { // ### to avoid problems with maximizing
                                DefMDIChildProc(hWnd, message, wParam, lParam);
                                SendMessage(RClient, WM_MDIRESTORE,(WPARAM) hWnd, 0);
                                return(0);
                        }
                        break;
                case WM_SETFOCUS:
                        SetFocus(RConsole);
                        break;
                case WM_CLOSE:   /* disallow it if we aren't coming through quit */
                      if( !RClosing ) {
                        MessageBox (hWnd, "You can't close the console","R Console",
                                        MB_OK | MB_ICONEXCLAMATION );
                        return 0;
                      }
                      break; 
                case WM_COMMAND:
                        switch (GET_WM_COMMAND_ID(wParam,lParam)) {
                                case RRR_NEW:
                                        save=mbquery();
                                        if (save == IDYES) {
                                            if( !Win_RSaveDlg(RClient) )
                                                error("cannot save requested file\n");
                                            dump_image(RFName, 0);
                                        }
                                        R_GlobalEnv=emptyEnv();
                                        jump_to_toplevel();
                                        return 0;
                                case RRR_OPEN:
                                        RFName[0]='\0';
                                        if( Win_ROpenDlg(RClient,1) )
                                                menuOpen();
                                        return 0;
                                case RRR_SAVE:
                                        strcpy(RFName, R_ImageName);
                                        menuSave();
                                        return 0;
                                case RRR_LOAD:
                                        if( Win_ROpenDlg(RClient,2) )
                                                menuLoad();
                                        return 0;
                                case RRR_UNDO:
                                        SysBeep();
                                        MessageBox(hWnd, "Command Not Implemented", "R Console",
                                                MB_OK | MB_ICONEXCLAMATION );
                                        break;
                                case RRR_QBROW:
                                        if( R_BrowseLevel > 0 )
                                                InFlag=2;
                                        else
                                                error("Browser is not active\n");
                                        return 0;
                                case RRR_CUT:
                                        RSelToClip();
                                        RClearSel();
                                        break;
                                case RRR_COPY:
                                        RSelToClip();
                                        return 0;
                                case RRR_PASTE:
                                        RPasteFromClip();
                                        return 0;
                                case RRR_CLEAR:
                                        RClearSel();
                                        return 0;
                                case RRR_CPASTE:
                                        RSelToClip();
                                        RPasteFromClip();
                                        return 0;
                                case RRR_SETMEM:
                                        DialogBox(RInst, "RMemory", RClient, (DLGPROC) Menu_SetMem);
                                        return 0;
                                case RRR_HELP:
                                        if((home = getenv("RHOME")) == NULL)
                                                return 0;
                                        sprintf(tmp,"%s\\html\\index.html",home);
                                        ShellExecute(NULL,"open",tmp,NULL, home, SW_SHOW);
                                        return 0;
                                case RRR_PRINT:
                                        RPrintText(RFrame, RConsole);
                                        return 0;
                                case RRR_QUIT:
                                        PostMessage(RFrame, WM_CLOSE, 0, 0);
                                        return 0;
                                case RRR_EDIT:
                                        switch(GET_WM_COMMAND_CMD(wParam,lParam)) {
                                                case EN_ERRSPACE:
                                                case EN_MAXTEXT:
                                                        RTrimBuffer();
                                                        return 0;
                                        }
                                        break;
                                case RRR_DATA:
                                        RFName[0]='\0';                                       
                                        userdata();
                                        return 0;
                                case RRR_SYSDATA:
                                        sysdata();
                                        return 0;
                        }
                   break;
                case WM_MDIACTIVATE:
                        if((HWND) lParam == hWnd )  {
                          SendMessage(RClient, WM_MDISETMENU, (WPARAM) RMenuConsole,
                                        (LPARAM) RMenuConsWin);
                                DrawMenuBar(RFrame);
                        }
                        return(0);
         }
  return(DefMDIChildProc(hWnd, message, wParam, lParam));
}


/* source a text file */
void menuLoad(void)
{
        FILE *fp;
        SEXP expr;
        int status;

        if(!(fp=R_fopen(RFName,"r")))
                error("load: couldn't open requested file\n");
        Rprintf("\n");
        PROTECT(expr=R_ParseFile(fp, -1, &status));
        if( status == PARSE_ERROR )
                error("load: an error occurred in parsing\n");
        eval(expr, R_GlobalEnv);
        UNPROTECT(1);
        expr = STRING(GetOption(install("prompt"), R_NilValue))[0];
        R_WriteConsole(CHAR(expr), strlen(CHAR(expr)));
}

/*open a saved image to replace the current image */
void menuOpen(void)
{
    FILE *fp;
    char *p;
    
        if(!R_Quiet)
                Rprintf("restore(\"%s\")\n", RFName);
        fp = R_fopen(RFName, "rb");
        if (!fp)
                error("unable to open file\n");
        

        switch(R_ReadMagic(fp)) {
        case R_MAGIC_BINARY:
                FRAME(R_GlobalEnv) = BinaryLoad(fp);
                break;
        case R_MAGIC_ASCII:
                FRAME(R_GlobalEnv) = AsciiLoad(fp);
                break;
        default:
                fclose(fp);
                error("workspace file corrupted -- no data loaded\n");
        }
        fclose(fp);
        p = ((char *) CHAR(STRING(GetOption(install("prompt"), R_NilValue))[0]));
        R_WriteConsole(p, strlen(p));
}


/* save the current image */
void menuSave()
{
        if( !Win_RSaveDlg(RClient) )
                error("cannot save requested file\n");
        dump_image(RFName,0);
}


LRESULT FAR PASCAL MainWndProc(HWND hWnd,UINT message,WPARAM wParam,
        LPARAM lParam)
{
        HWND hWndChild;
        CLIENTCREATESTRUCT cltcr;

        switch (message) {
                case WM_CREATE:
                        cltcr.hWindowMenu = RMenuConsWin;
                        cltcr.idFirstChild = RRR_FIRSTCHILD;
                        RClosing = 0;
                        RClient = CreateWindow("MDICLIENT", NULL,
                                                WS_CHILD | WS_CLIPCHILDREN | WS_VISIBLE |WS_VSCROLL | WS_HSCROLL,
                                                0, 0, 0, 0, hWnd, (HMENU) 1, RInst, (LPSTR) &cltcr);
                        return 0;
                case WM_DDE_ACK:
                        if( hWndServerDDE == NULL )
                                hWndServerDDE= (HWND) wParam;
                        FreeDDElParam(message,lParam);
                        if( IsWindow(RFrame) )
                                ShowWindow(RFrame, SW_SHOWMAXIMIZED);
                        return 0;
                case WM_COMMAND:
                        switch(wParam) {
                                case RRR_ABOUT:
                                        DialogBox(RInst, "AboutBox", hWnd,
                                                (DLGPROC) About);
                                        return(0);
                                case RRR_TILE:
                                        SendMessage(RClient, WM_MDITILE, 0, 0L);
                                        return(0);
                                case RRR_CASC:
                                        SendMessage(RClient, WM_MDICASCADE, 0, 0L);
                                        return(0);
                                case RRR_ARRA:
                                        SendMessage(RClient, WM_MDIICONARRANGE, 0, 0L);
                                        return(0);
                                case RRR_CLOSEALL:
                                        EnumChildWindows(RClient, &CloseEnumProc, 0);
                                        return(0);
                                default:
                                        hWndChild=(HWND) SendMessage(RClient, WM_MDIGETACTIVE, 0,0);
                                        if (IsWindow(hWndChild))
                                                SendMessage(hWndChild,WM_COMMAND,wParam,lParam);
                        }
                        break;
                case WM_CLOSE:
                        if( R_DirtyImage )
                                R_CleanUp(1);
                        RClosing = 1;
                        SendMessage(hWnd, WM_COMMAND, RRR_CLOSEALL, 0);
                        
                        if( NULL != GetWindow( RClient, GW_CHILD) )
                                return 0;
                        break;
                case WM_DESTROY:
                        PostQuitMessage(0);
                        return 0;
        }
        return(DefFrameProc(hWnd, RClient, message, wParam, lParam));
}

BOOL CALLBACK CloseEnumProc (HWND hwnd, LPARAM lparam)
{
    if ( GetWindow(hwnd, GW_OWNER) )
        return 1;
    SendMessage( GetParent(hwnd), WM_MDIRESTORE, (WPARAM) hwnd, 0);
    if( !SendMessage (hwnd, WM_QUERYENDSESSION, 0 , 0))
        return 1;
    SendMessage( GetParent(hwnd), WM_MDIDESTROY, (WPARAM) hwnd, 0);
        return 1;
}

BOOL FAR PASCAL About(hDlg, message, wParam, lParam)
HWND hDlg;
WORD message;
WPARAM wParam;
LPARAM lParam;
{
        switch( message) {
                case WM_INITDIALOG:
                        return TRUE;
                case WM_COMMAND:
                        if( wParam == IDOK || wParam == IDCANCEL ) {
                                EndDialog(hDlg, TRUE);
                                return TRUE;
                        }
                        break;
        }
        return FALSE;
}

        /*************************************************************/
        /* This function print the given prompt at the console       */
        /* and then transfers up to bufsize characters into the      */
        /* buffer pointed to by buf. The last two characters should  */
        /* be set to \n\0. If hist is non-zero then the line is      */
        /* added to any command line history-haha                    */                   

int R_ReadConsole(char *prompt, char *buf, int bufsize, int hist)
{
        int n,j,nchar,lineno,lineind;
        char readbuf[256],*rbufp;

        InFlag=0;
        R_WriteConsole(prompt, strlen(prompt));
        
        do {
                EventLoop();
        }
        while(!InFlag);

        if( InFlag == 3 ) {
            buf[0]=EOF;
            buf[1]='\0';
            return 0;
        }

        if( InFlag == 2) {
                buf[0]='\n';
                buf[1]='\0';
                return(1);
        }

        n= Edit_GetTextLength(RConsole);
        nchar=n-InStart;
        if( nchar > bufsize )
                return -1;
        else  {
                lineno=Edit_LineFromChar(RConsole,InStart);
                lineind=Edit_LineIndex(RConsole,lineno);
                Edit_GetLine(RConsole,lineno,readbuf,256);
                rbufp=&readbuf[(InStart-lineind)];
                for( j=0;j<nchar;j++)
                        *buf++=*rbufp++;
                /* now to make our grammar happy add a carriage return */
                *buf++='\n';
                *buf='\0';
                /* write a new line to the edit window */
                readbuf[0]='\n';
                readbuf[1]='\0';
                R_WriteConsole(&readbuf[0],1);
                InStart= Edit_GetTextLength(RConsole);
                return (nchar+1);
        }
}

void R_WriteConsole(char *buf, int buflen)
{
        int sstart, ssend, slen;
        char IObuf[255],*bufp;

        bufp=IObuf;
        while(*bufp=*buf) {
                if(*buf=='\n') {
                        *bufp++='\r';
                        *bufp++='\n';
                        *bufp='\0';
                        slen=strlen(IObuf);
                        sstart= Edit_GetTextLength(RConsole);
                        ssend= sstart+slen;
                        Edit_SetSel(RConsole,sstart,ssend);
                        Edit_ReplaceSel(RConsole, IObuf);
                        InStart+=slen;
                        bufp=IObuf;
                        buf++;
                }
                else {
                        bufp++;
                        buf++;
                }
        }
        if( bufp != IObuf ) {
                        *bufp='\0';
         slen=strlen(IObuf);
                        sstart= Edit_GetTextLength(RConsole);
                        ssend= sstart+slen;
                        Edit_SetSel(RConsole,sstart,ssend);
                        Edit_ReplaceSel(RConsole, IObuf);
                        InStart+=slen;
        }
        /*RSetCursor();*/
        return;
}

void R_ResetConsole(void)
{
#ifdef OLD
        R_SetInput(R_CONSOLE);
        R_Inputfile = stdin;
#endif
}             
                          
        /* This is stdio support to ensure that console file buffers */
        /* are flushed. */
        
void R_FlushConsole(void)
{
        if (R_CONSOLE == 1)
                fflush(stdin);
}


void R_ClearerrConsole(void)
{
        if (R_CONSOLE == 1)
                clearerr(stdin);
}

        /*--- F i l e    H an d l i n g    C o d e ---*/

FILE *R_OpenLibraryFile(char *file)
{
        char buf[256], buf2[256];
        FILE *fp;

        /*if((home = getenv("RHOME")) == NULL)*/
		GetEnvironmentVariable("RHOME",buf,256);
		if( strlen(buf) == 0 )
                return NULL;
        sprintf(buf2, "%s/library/base/R/%s", buf, file);
        fp = R_fopen(buf2,"rt");
        return fp;
}

FILE *R_OpenSysInitFile(void)
{
        char buf[256],buf2[256];
        FILE *fp;

		GetEnvironmentVariable("RHOME",buf, 256);
		if( strlen(buf) == 0 )
			return NULL;
        sprintf(buf2, "%s/library/base/R/Rprofile",buf);
        fp = R_fopen(buf2, "r");
        return fp;
}

FILE *R_OpenInitFile(void)
{
        char buf[256];
        FILE *fp;

        fp = NULL;

        if(fp = R_fopen(".Rprofile", "r"))
                return fp;

        sprintf(buf, "%s/.Rprofile", getenv("HOME"));
        if(fp = R_fopen(buf, "r"))
                return fp;

        return fp;
}


SEXP do_interactive(SEXP call, SEXP op, SEXP args, SEXP rho)
{
        SEXP rval;

        rval=allocVector(LGLSXP, 1);
        LOGICAL(rval)[0]=1;
        return rval;
}

void RSetCursor(void)
{
        InStart=Edit_GetTextLength(RConsole);
        Edit_SetSel(RConsole,InStart,InStart);
}

void SysBeep(void)
{
        MessageBeep(0);
}

/* copy selection to the clipboard and set the cursor to the end of
        the selection
*/
void RSelToClip(void)
{
        SendMessage(RConsole, WM_COPY, 0, 0);
}

/* check to see if the current caret pos is the current line
        if so then paste there, otherwise paste at the end of the
        text
*/

void RPasteFromClip(void)
{
        long sel;
        int selb, sstart;
        HANDLE cText;
        LPSTR   pText;

        if( !IsClipboardFormatAvailable(CF_TEXT) )
                return;
        OpenClipboard(RConsole);
        if( (cText=GetClipboardData(CF_TEXT)) == NULL )  {
                CloseClipboard();
                return;
        }
        if( (RPasteText=GlobalAlloc(GHND, GlobalSize(cText))) == NULL ) {
                CloseClipboard();
                return;
        }
        if( (RPasteP=GlobalLock(RPasteText)) == NULL ) {
                CloseClipboard();
                return;
        }
        if( (pText=GlobalLock(cText)) == NULL ) {
                CloseClipboard();
                GlobalUnlock(RPasteText);
                return;
        }
        while ( *RPasteP++ = *pText++ );
                /* unlock */
        GlobalUnlock(cText);
        GlobalUnlock(RPasteText);
        CloseClipboard();
        /* set up RPasteP so that it points at the start of the text */
        RPasteP=GlobalLock(RPasteText);
        RPasteLen=strlen(RPasteP);
        /* handle the insertion pt: if we are in the current input line
                don't do anything but if we are elsewhere go to the end of the
                text and start there
        */
        sel=Edit_GetSel(RConsole);
        selb=LOWORD(sel);
        sstart=Edit_GetTextLength(RConsole);
        if( selb < InStart || selb > sstart )
                        Edit_SetSel(RConsole, sstart, sstart);
}

/* check to see if the selection is in the current line */
void RClearSel(void)
{
        long sel;
        int selb;
        char Rbuf[1];

        Rbuf[0]='\0';

        sel=Edit_GetSel(RConsole);
        selb=LOWORD(sel);
        if( selb < InStart )
                SysBeep();
        else
                Edit_ReplaceSel(RConsole, Rbuf);
}

/* Trim the edit buffer so that there are at most savelines lines and
        savechars chars in it
*/

void RTrimBuffer(void)
{
        long sel;
        int dellines, delchars, nlines;
        char Rbuf[1];

        dellines=100;
        delchars=500;
        Rbuf[0]='\0';

        sel=Edit_GetSel(RConsole);
        nlines= Edit_GetLineCount(RConsole);
        if( nlines < 100 )
                dellines= (int) nlines/2;
        delchars=Edit_LineIndex(RConsole,dellines);
        if( delchars<500 )  {
                dellines=max(nlines,(int) 1.5*dellines);
                delchars=Edit_LineIndex(RConsole,dellines);
        }
        Edit_SetSel(RConsole,0,delchars);
        Edit_ReplaceSel(RConsole,Rbuf);
        nlines=LOWORD(sel);
        dellines=HIWORD(sel);
        Edit_SetSel(RConsole,nlines-delchars,dellines-delchars);
}

extern BOOL CALLBACK Menu_SetMem(HWND hDlg, UINT message,
        WPARAM wParam,LPARAM lParam)
{
        char memval[20], *p;
        int nsize, vsize;


        switch (message) {
                case  WM_INITDIALOG:
                    sprintf(memval,"%d", (int) ((R_VSize*sizeof(VECREC))+1048575)/1048576);
                    SetDlgItemText(hDlg, 102, memval);
                    sprintf(memval,"%d", (int) R_NSize);
                    SetDlgItemText(hDlg, 103, memval);
                    SetFocus(GetDlgItem(hDlg, 1));
                    return FALSE;
                case WM_COMMAND:
                    switch (wParam) {
                       case IDOK:
                         GetDlgItemText(hDlg, 102, memval, 20);
                         vsize = strtol(memval, &p, 10);
                         if (*p ) {
                              MessageBox(RConsole, "The  VSize value is not valid",
                                     "R Application", MB_ICONEXCLAMATION | MB_OK);
                              sprintf(memval,"%d", (int)((R_VSize*sizeof(VECREC))+1048575)/1048576);
                              SetDlgItemText(hDlg, 102, memval);
                              return 1;
                         }
						 if (vsize == ((R_VSize*sizeof(VECREC))+1048575)/1048576)
							 vsize = R_VSize*sizeof(VECREC);
						 else
							 vsize = vsize * 1048576;
                         GetDlgItemText(hDlg, 103, memval, 20);
                         nsize = strtol(memval, &p, 10);
                         if (*p ) {
                              MessageBox(RConsole, "The  NSize value is not valid",
                                     "R Application", MB_ICONEXCLAMATION | MB_OK);
                              sprintf(memval,"%d", (int) R_NSize);
                              SetDlgItemText(hDlg, 103, memval);
                              return 1;
                         }
                         EndDialog(hDlg, TRUE);
                         R_SetMemory(nsize, vsize);
                         return TRUE;
                       case IDCANCEL:
                          EndDialog(hDlg, FALSE);
                          return TRUE;
                  }

          }
          return FALSE;
}
