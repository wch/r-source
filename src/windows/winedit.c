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

char REdfilename[MAX_PATH];
static int inFlag=1;

static HWND REditFrame, REditWnd;

HMENU RMenuEdit, RMenuEditWin;

int InitEditor(void) 
{
        MDICREATESTRUCT mdicreate;
        RECT r;

        GetClientRect(RClient, &r);
        mdicreate.szClass = REditClass;
        mdicreate.szTitle = "R Editor";
        mdicreate.hOwner = RInst;
        mdicreate.x = CW_USEDEFAULT;
        mdicreate.y = CW_USEDEFAULT;
        mdicreate.cx = CW_USEDEFAULT;
        mdicreate.cy = CW_USEDEFAULT;
        mdicreate.style = 0;
        mdicreate.lParam = NULL;
        REditFrame = (HWND) (UINT) SendMessage(RClient, WM_MDICREATE, 0,
                (LONG) (LPMDICREATESTRUCT) &mdicreate);
        if( REditFrame == NULL )
                return 0;
        
        GetClientRect(REditFrame, &r);
        REditWnd = CreateWindow("Edit", NULL,
                        WS_CHILD | WS_VISIBLE | WS_VSCROLL | WS_HSCROLL |
                        ES_MULTILINE | ES_AUTOVSCROLL | ES_AUTOHSCROLL ,
                        0, 0, (r.right - r.left), (r.bottom - r.top),
                        REditFrame, (HMENU) RRR_TEDIT, RInst, NULL);
        DragAcceptFiles(REditFrame, TRUE);       
        return 1;
}


void CloseEd(void) {
        SendMessage(RClient, WM_MDIDESTROY, (WPARAM) (HWND) REditFrame, 0L);
        SetFocus(RConsoleFrame);
}
/*Empty the Edit Window */

static void EmptyEd(void) {
        int nchars;
        char Rbuf[1];

        nchars = Edit_GetTextLength(REditWnd);
        Rbuf[0]='\0';
        Edit_SetSel(REditWnd, 0, nchars);
        Edit_ReplaceSel(REditWnd, Rbuf);
}

/* Set the contents back to the original supplied argument */

static void RefreshEd(void) {
    int i;
    char tmp[MAXELTSIZE];
    FILE *fp;

    if( fp = fopen(REdfilename, "rt")) {
        while( fgets(tmp, MAXELTSIZE-1, fp) ) {
                i = strlen(tmp);
                if( tmp[i-1] == '\n' ) {
                    tmp[i-1] = '\r';
                    tmp[i] = '\n';
                    tmp[i+1] = '\0';
                }
                i = Edit_GetTextLength(REditWnd);
                Edit_SetSel(REditWnd, i, i);
                Edit_ReplaceSel(REditWnd, tmp); 
        }
    }
    else {
        tmp[0]='\0';
        i = Edit_GetTextLength(REditWnd);
        Edit_SetSel(REditWnd, 1, i);
        Edit_ReplaceSel(REditWnd, tmp);
    }
   SetFocus(REditWnd);
}  

LRESULT FAR PASCAL REditWndProc(HWND hWnd, UINT message, WPARAM wParam,
        LPARAM lParam)
{
        switch(message) {
                 case WM_MDIACTIVATE:
                        if((HWND) lParam == hWnd ) {
                                SendMessage(RClient, WM_MDISETMENU, (WPARAM) RMenuEdit,
                                 (LPARAM) RMenuEditWin);
                                DrawMenuBar(GetParent(RClient));
                                SetFocus(REditWnd);
                                return 0;
                        }
                        else if( inFlag ) { /* don't let them switch to another window */
                            MessageBox(hWnd, "You must quit the editor before doing something else.",
                                "R Editor", MB_OK | MB_ICONEXCLAMATION);
                            PostMessage(RClient, WM_MDIACTIVATE, (UINT) REditFrame,0);
                        }
                        break;
                 case WM_SYSCOMMAND: /* Disable minimizing */
                        if( (wParam & 0xFFF0) == SC_MINIMIZE ) {
                            MessageBox(hWnd, "You can't iconify the R Editor",
                                "R Data Entry", MB_OK | MB_ICONEXCLAMATION);
                            return 1l;
                        }
                        break;
                case WM_DROPFILES:
                        R_ProcessDropFiles((HANDLE) wParam, 2);
                        EmptyEd();
                        RefreshEd();
                        return 0;
                case WM_COMMAND:
                        switch (GET_WM_COMMAND_ID(wParam,lParam)) {
                                case RRR_QUIT:
                                        inFlag=0;
                                        return 0;
                                case RRR_CLEAR:
                                        EmptyEd();
                                        return 0;
                                case RRR_REFRESH:
                                        EmptyEd();
                                        RefreshEd();
                                        return 0;
                                case RRR_TEDIT:
                                        /* if there isn't enough room empty the buffer */
                                        switch(GET_WM_COMMAND_CMD(wParam,lParam)) {
                                                case EN_ERRSPACE:
                                                case EN_MAXTEXT:
                                                        MessageBox(hWnd,"Text is too large.","R Editor",
                                                                MB_ICONEXCLAMATION | MB_OK);
                                                        EmptyEd();
                                                        return 0;
                                        }
                                        break;
                        }
                        break;
                case WM_SIZE:
                        MoveWindow(REditWnd, 0, 0, LOWORD(lParam), HIWORD(lParam), TRUE);
                        break;
                case WM_CLOSE:
                case WM_DESTROY:
                        inFlag=0;
                        return(0);
        }
        return(DefMDIChildProc(hWnd, message, wParam, lParam));
}

void InitEd()
{
}

SEXP do_edit(SEXP call, SEXP op, SEXP args, SEXP env) 
{
        SEXP x, fn, envir;
        int i, l, n;
        char RTbuf[MAXELTSIZE];
        FILE *fp;

        checkArity(op, args);

        if( !InitEditor())
            error("couldn't start an editor window\n");

        x = CAR(args);
        if( TYPEOF(x) == CLOSXP )
                envir = CLOENV(x);
        else
                envir = R_NilValue;
        PROTECT(envir);

        fn = CADR(args);
        if( !isString(fn))
                error("invalid argument to edit\n");
        if( LENGTH(STRING(fn)[0]) > 0 ) 
               strcpy(REdfilename, CHAR(STRING(fn)[0]));
        else if(x!=R_NilValue || !strlen(REdfilename) ) {/* if x is R_NilValue then we are restoring and want to use the last file */
                GetTempPath(MAXELTSIZE, RTbuf);
                i=GetTempFileName(RTbuf,"NEW",0,(LPTSTR) REdfilename);
        }

        if( x != R_NilValue ) {
            if((fp = fopen(REdfilename, "wt")) == NULL )
                error("unable to open file for writing\n");
            x = deparse1(x,0);
            for(i = 0; i<LENGTH(x); i++ )
                fprintf(fp, "%s\n", CHAR(STRING(x)[i]));
            fclose(fp);
        }
        
 
        RefreshEd();

        inFlag = 1;
        do{
                EventLoop();
        }
        while(inFlag);

        n=Edit_GetLineCount(REditWnd);
                
        if( !(fp = fopen(REdfilename, "wt")) )
                error("unable to open file for writing");
        for(i=0 ; i<n ; i++) {
                l=Edit_GetLine(REditWnd,i,RTbuf,MAXELTSIZE-1);
                RTbuf[l] = '\0';
                fprintf(fp, "%s\n",RTbuf);
        }
        fclose(fp);
        
        CloseEd();

        fp=fopen(REdfilename, "rt");
        R_ParseCnt = 0;
        PROTECT(x = parse(fp, 1));
        fclose(fp);
        if( R_ParseError)
            errorcall(call,"An error occurred on line %d\n use a command like\n x<-edit()\n to recover\n",R_ParseError);
        ResetConsole();
        x = eval(CAR(x), R_GlobalEnv);
        if( TYPEOF(x) == CLOSXP && envir != R_NilValue)
            CLOENV(x) = envir;
        UNPROTECT(2);
        return x;
}
