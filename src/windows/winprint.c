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

/*  TO DO

  Need to adjust RPrintText somewhat so that it only tries to print
  lines of text that are as wide as the printer. Perhaps some sort of
  line breaking procedure is needed.
  The user should be able to set the font somehow (perhaps we should use
  the same font as is current in the display window).
 
*/

BOOL bPrint;
HWND hdlgCancel;

/* sizes of the printer P*** and the window W*** needed to translate the clipping region */
static float PPixelsX, PPixelsY, PMMX, PMMY;
static float WPixelsX, WPixelsY, WMMX, WMMY;


int CALLBACK PrintMetafile(HDC hdc, HANDLETABLE* lpHTable, ENHMETARECORD* lpEMFR, int nObj,
LPVOID lpData)
{
    double x0, x1, y0, y1;
    char *x;
    HRGN hrgn;
    
    if( lpEMFR->iType == EMR_GDICOMMENT ) {
        x = (char *) &(lpEMFR->dParm[1]);
        if( x[0]=='R' && x[1]=='_' && x[2]=='C') {
                sscanf(x+3,"%lf %lf %lf %lf",&x0,&x1,&y0,&y1);
                x0 = x0*PPixelsX*WMMX/(WPixelsX*PMMX);
                x1 = x1*PPixelsX*WMMX/(WPixelsX*PMMX);
                y0 = y0*PPixelsY*WMMY/(WPixelsY*PMMY);
                y1 = y1*PPixelsY*WMMY/(WPixelsY*PMMY);
                hrgn = CreateRectRgn((int) x0, (int) y0, (int) x1, (int) y1);
                SelectClipRgn(hdc, hrgn);
        }
    }
    else
        PlayEnhMetaFileRecord(hdc, lpHTable, lpEMFR, nObj);
    return(1);
}

BOOL RPrintGraph(HWND hwnd, HENHMETAFILE RMhmf)
{
        static PRINTDLG pd;
        static DOCINFO di = {sizeof(DOCINFO), "", NULL};
        RECT rect;
        int nError;
        HDC hdc;
        ENHMETAHEADER emh;

        if( RMhmf == NULL ) /* nothing to print */
                return FALSE;

                
        ZeroMemory(&emh, sizeof(ENHMETAHEADER));
        emh.nSize = sizeof(ENHMETAHEADER);
        if( GetEnhMetaFileHeader( RMhmf, sizeof(ENHMETAHEADER), &emh) == 0 )
                return FALSE;
        
        
        pd.lStructSize = sizeof(PRINTDLG);
        pd.hDevMode = (HANDLE) NULL;
        pd.hDevNames = (HANDLE) NULL;
        pd.Flags = PD_RETURNDC;
        pd.hwndOwner = hwnd;
        pd.hDC = (HDC) NULL;
        pd.nFromPage = 1;
        pd.nToPage = 1;
        pd.nMinPage = 0;
        pd.nMaxPage = 0;
        pd.nCopies = 1;
        pd.hInstance = (HANDLE) NULL;
        pd.lCustData = 0L;
        pd.lpfnPrintHook = (LPPRINTHOOKPROC) NULL;
        pd.lpfnSetupHook = (LPPRINTHOOKPROC) NULL;
        pd.lpPrintTemplateName = (LPSTR) NULL;
        pd.lpSetupTemplateName = (LPSTR) NULL;
        pd.hPrintTemplate = (HANDLE) NULL;
        pd.hSetupTemplate = (HANDLE) NULL;

        PrintDlg(&pd);

        bPrint = TRUE;

        SetAbortProc(pd.hDC, AbortProc);

        hdlgCancel = CreateDialog(RInst, (LPSTR) "PAbortDlg", hwnd,
                        (DLGPROC) AbortPrintJob);

        EnableWindow(hwnd, FALSE);


        

        nError = StartDoc(pd.hDC, &di);
        if( nError == SP_ERROR )
                goto Error;

        nError = StartPage(pd.hDC);
        if( nError <= 0 )
                goto Error;

        PPixelsX = (float) GetDeviceCaps( pd.hDC, HORZRES);
        PPixelsY = (float) GetDeviceCaps(pd.hDC, VERTRES);
        PMMX = (float) GetDeviceCaps(pd.hDC, HORZSIZE);
        PMMY = (float) GetDeviceCaps(pd.hDC, VERTSIZE);
        
        hdc = GetDC(hwnd);
        WPixelsX = (float) GetDeviceCaps(hdc,HORZRES);
        WPixelsY = (float) GetDeviceCaps(hdc, VERTRES);
        WMMX = (float) GetDeviceCaps(hdc, HORZSIZE);
        WMMY = (float) GetDeviceCaps(hdc, VERTSIZE);
        ReleaseDC(hwnd, hdc);
        
        rect.top = (int)((float)(emh.rclFrame.top)*PPixelsY / (PMMY*100.0f));
        rect.left = (int)((float)(emh.rclFrame.left)*PPixelsX / (PMMX*100.0f));
        rect.right = (int)((float)(emh.rclFrame.right)*PPixelsX / (PMMX*100.0f));
        rect.bottom = (int)((float)(emh.rclFrame.bottom)*PPixelsY / (PMMY*100.0f));
        EnumEnhMetaFile(pd.hDC, (HMETAFILE) RMhmf, (ENHMFENUMPROC) PrintMetafile, NULL, &rect);

        nError = EndPage(pd.hDC);
        if(nError <= 0 )
                goto Error;

        EndDoc(pd.hDC);

Error:
        EnableWindow(hwnd, TRUE);
        DestroyWindow(hdlgCancel);
        DeleteDC(pd.hDC);
        return bPrint;
}

BOOL RPrintBitMap(LPBITMAPINFO lpbi, LPBYTE lpbm)
{
        static PRINTDLG pd;
        static DOCINFO di = {sizeof(DOCINFO), "", NULL};
        int nError, rcaps, xmult, ymult, nColorData;
        LPTSTR lpBits;

                
        pd.lStructSize = sizeof(PRINTDLG);
        pd.hDevMode = (HANDLE) NULL;
        pd.hDevNames = (HANDLE) NULL;
        pd.Flags = PD_RETURNDC;
        pd.hwndOwner = RConsoleFrame;
        pd.hDC = (HDC) NULL;
        pd.nFromPage = 1;
        pd.nToPage = 1;
        pd.nMinPage = 0;
        pd.nMaxPage = 0;
        pd.nCopies = 1;
        pd.hInstance = (HANDLE) NULL;
        pd.lCustData = 0L;
        pd.lpfnPrintHook = (LPPRINTHOOKPROC) NULL;
        pd.lpfnSetupHook = (LPPRINTHOOKPROC) NULL;
        pd.lpPrintTemplateName = (LPSTR) NULL;
        pd.lpSetupTemplateName = (LPSTR) NULL;
        pd.hPrintTemplate = (HANDLE) NULL;
        pd.hSetupTemplate = (HANDLE) NULL;

        PrintDlg(&pd);

        bPrint = TRUE;
        
            
        SetAbortProc(pd.hDC, AbortProc);

        hdlgCancel = CreateDialog(RInst, (LPSTR) "PAbortDlg", RConsoleFrame,
                        (DLGPROC) AbortPrintJob);

        EnableWindow(RConsoleFrame, FALSE);

        nError = StartDoc(pd.hDC, &di);

        rcaps = GetDeviceCaps(pd.hDC, RASTERCAPS);
        if( !(rcaps & RC_BITBLT) || !(rcaps & RC_STRETCHBLT) ) {
            MessageBox(RConsoleFrame,
                "Printer cannot display bitmaps",
                "Device Error",
                MB_OK);
            goto Error;
        }
                
        if( nError == SP_ERROR )
                goto Error;

        nError = StartPage(pd.hDC);
        if( nError <= 0 )
                goto Error;


        
        PPixelsX = (float) GetDeviceCaps( pd.hDC, HORZRES);
        PPixelsY = (float) GetDeviceCaps(pd.hDC, VERTRES);
        

        xmult = PPixelsX/lpbi->bmiHeader.biWidth;
        ymult = PPixelsY/lpbi->bmiHeader.biHeight;
        xmult=min(xmult,ymult);

        lpBits = (LPTSTR) lpbi;
        if( lpbi->bmiHeader.biClrUsed != 0 )
                nColorData = lpbi->bmiHeader.biClrUsed;
        else
                switch( lpbi->bmiHeader.biBitCount )
                {
                    case 1: nColorData = 2; break;
                    case 4: nColorData = 16; break;
                    case 8: nColorData = 256; break;
                    case 16: if(lpbi->bmiHeader.biCompression == BI_RGB )
                                nColorData = 0;
                              else if( lpbi->bmiHeader.biCompression == BI_BITFIELDS )
                                nColorData = 3*sizeof(DWORD)/sizeof(RGBQUAD);
                              else
                                nColorData = 0;
                              break;
                    case 24: nColorData = 0; break;
                }


        rcaps=StretchDIBits(pd.hDC, 20,20,lpbi->bmiHeader.biWidth*xmult, lpbi->bmiHeader.biHeight*xmult,0,0,
                       lpbi->bmiHeader.biWidth,  lpbi->bmiHeader.biHeight, lpbm, lpbi,
                       DIB_RGB_COLORS, SRCCOPY );

        
        nError += EndPage(pd.hDC);
        
        if(nError <= 0 )
                goto Error;

        EndDoc(pd.hDC);

Error:
        EnableWindow(RConsoleFrame, TRUE);
        DestroyWindow(hdlgCancel);
        DeleteDC(pd.hDC);
        return bPrint;
}

void RPrintText(HWND hwnd, HWND TextWin)
{
        static PRINTDLG pd;
        static DOCINFO di = {sizeof(DOCINFO), "", NULL};
        TEXTMETRIC tm;
        int nError, nTotalLines, ychar, nLinesPerPage, nTotalPages, nPage;
        int nNonColCopy, nColCopy, nLine, nLineNum, nCharsPerLine, nchars;
        char rbuf[256];

        pd.lStructSize = sizeof(PRINTDLG);
        pd.hDevMode = (HANDLE) NULL;
        pd.hDevNames = (HANDLE) NULL;
        pd.Flags = PD_ALLPAGES | PD_COLLATE | PD_RETURNDC;
        pd.hwndOwner = hwnd;
        pd.hDC = (HDC) NULL;
        pd.nFromPage = 0;
        pd.nToPage = 0;
        pd.nMinPage = 0;
        pd.nMaxPage = 0;
        pd.nCopies = 1;
        pd.hInstance = (HANDLE) NULL;
        pd.lCustData = 0L;
        pd.lpfnPrintHook = (LPPRINTHOOKPROC) NULL;
        pd.lpfnSetupHook = (LPPRINTHOOKPROC) NULL;
        pd.lpPrintTemplateName = (LPSTR) NULL;
        pd.lpSetupTemplateName = (LPSTR) NULL;
        pd.hPrintTemplate = (HANDLE) NULL;
        pd.hSetupTemplate = (HANDLE) NULL;



        if( !PrintDlg(&pd))
                goto Error2;

   SetAbortProc(pd.hDC, AbortProc);

        hdlgCancel = CreateDialog(RInst, (LPSTR) "PAbortDlg", hwnd,
                        (DLGPROC) AbortPrintJob);

        EnableWindow(hwnd, FALSE);
        nTotalLines=Edit_GetLineCount(TextWin);
        if (nTotalLines==0)
                goto Error;

        GetTextMetrics( pd.hDC, &tm);
        ychar=tm.tmHeight+tm.tmExternalLeading;
        nCharsPerLine=GetDeviceCaps(pd.hDC, HORZRES)/tm.tmAveCharWidth;
        nLinesPerPage=GetDeviceCaps(pd.hDC, VERTRES)/ychar;
        nTotalPages=(nTotalLines+nLinesPerPage -1)/nLinesPerPage;

        bPrint = TRUE;


        di.cbSize = sizeof(DOCINFO);
        di.lpszDocName = "Test 1";
        di.lpszOutput = (LPSTR) NULL;

        nError = StartDoc(pd.hDC, &di);
        if( nError == SP_ERROR )
                goto Error;


        for( nColCopy=0; nColCopy< (pd.Flags & PD_COLLATE ? pd.nCopies : 1);
                        nColCopy++) {
                                for( nPage=0 ; nPage < nTotalPages ; nPage++ ) {
                                        for (nNonColCopy=0; nNonColCopy < (pd.Flags & PD_COLLATE ? 1 : pd.nCopies);
                                                nNonColCopy++ )
                                                {
                                                        nError = StartPage(pd.hDC);
                                                        if( nError <= 0 )
                                                                goto Error;
                                                        for(nLine=0;nLine<nLinesPerPage; nLine++) {
                                                                nLineNum=nLinesPerPage*nPage+nLine;
                                                                if( nLineNum >= nTotalLines )
                                                                        break;
                                                                nchars=Edit_GetLine(TextWin, nLineNum, rbuf,256);
                                                                TextOut(pd.hDC, 0, ychar*nLine, rbuf, nchars);
                                                        }
                                                        nError = EndPage(pd.hDC);
                                                        if(nError <= 0 )
                                                                goto Error;
                                        }
                                }
        }
        EndDoc(pd.hDC);

Error:
        EnableWindow(hwnd, TRUE);
        DestroyWindow(hdlgCancel);
        DeleteDC(pd.hDC);
Error2:
        return;
}

BOOL CALLBACK AbortProc(HDC hdcPrn, int nCode)
{
        MSG msg;

        while (bPrint && PeekMessage((LPMSG) &msg, (HWND) NULL, 0,0, PM_REMOVE))
        {
                if(!IsDialogMessage(hdlgCancel, (LPMSG) &msg)) {
                        TranslateMessage((LPMSG) &msg);
                        DispatchMessage((LPMSG) &msg);
                }
        }
        return bPrint;
}

LRESULT CALLBACK AbortPrintJob(HWND hDlg, UINT message, WPARAM wParam,
                        LPARAM lParam)
{
        switch(message) {
                case WM_INITDIALOG:
                        /* SetDlgItemText(hDlg, RDD_FILE, ofn.lpstrFile); */
                        return TRUE;
                case WM_COMMAND:
                        bPrint = FALSE;
                        return TRUE;
                default:
                        return FALSE;
        }
}
