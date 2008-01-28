/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1999, 2000  Guido Masarotto
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
 *  along with this program; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
 */

/* Support for printer
 *  printer newprinter()  - return a printer object - to draw to the
 *                          printer use drawto(...) and the drawXXX
 *                          functions + nextpage(printer)
 *                          printers can be deleted by 'del(printer)'
 */

#include "win-nls.h"
#include "internal.h"
#include "rui.h"


/*
 *  Internal printer deletion function.
 */
static void private_delprinter(printer obj)
{
    HDC h = (HDC) obj->handle;
    if (!obj || !h || (obj->kind != PrinterObject)) return;
    EndPage(h);
    EndDoc(h);
    DeleteDC(h);
    return;
}

/*
 *  Create/return the base printer object.
 */
static object get_printer_base(void)
{
    static object printer_base = NULL;

    if (! printer_base)
	printer_base = new_object(BaseObject, 0, NULL);
    return printer_base;
}

static HDC chooseprinter(void)
{
    PRINTDLG pd;
    HDC dc;
    DWORD rc;
    char cwd[MAX_PATH];

    GetCurrentDirectory(MAX_PATH,cwd);

    pd.lStructSize = sizeof( PRINTDLG );
    pd.hwndOwner = NULL;
    pd.hDevMode = (HANDLE)NULL;
    pd.hDevNames = (HANDLE)NULL;
    pd.Flags = PD_RETURNDC | PD_NOSELECTION | PD_NOPAGENUMS |
	PD_USEDEVMODECOPIES;
    pd.nFromPage = 0;
    pd.nToPage = 0;
    pd.nMinPage = 0;
    pd.nMaxPage = 0;
    pd.nCopies = 1;
    pd.hInstance = (HINSTANCE)NULL;
    pd.lCustData = (LPARAM)0;
    pd.lpfnPrintHook = 0;
    pd.lpfnSetupHook = 0;
    pd.lpPrintTemplateName = (LPCSTR) 0;
    pd.lpSetupTemplateName = (LPCSTR) 0;
    pd.hPrintTemplate = (HGLOBAL)0;
    pd.hSetupTemplate = (HGLOBAL)0;

    dc = PrintDlg( &pd ) ? pd.hDC : NULL;
    SetCurrentDirectory(cwd);
    if (!dc) {
	rc = CommDlgExtendedError(); /* 0 means user cancelled */
	if (rc) R_ShowMessage(_("Unable to choose printer"));
    }
    return dc;
}


printer newprinter(double width, double height, const char *name)
{
    DOCINFO docinfo;
    printer obj;
    HDC hDC;
    double dd,AL;
    int ww,hh,x0,y0;

    if(strlen(name)) {
	OSVERSIONINFO verinfo;
	verinfo.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
	GetVersionEx(&verinfo);
	switch(verinfo.dwPlatformId) {
	case VER_PLATFORM_WIN32_NT:
	    hDC = CreateDC("WINSPOOL", name, NULL, NULL);
	default:
	    hDC = CreateDC(NULL, name, NULL, NULL);
	}
    } else hDC = chooseprinter();
    if ( !hDC ) return NULL;
    obj = new_object(PrinterObject, (HANDLE) hDC, get_printer_base());
    if ( !obj ) {
	R_ShowMessage(_("Insufficient memory for new printer"));
	DeleteDC(hDC);
	return NULL;
    }
    if ((width == 0.0) && (height == 0.0)) {
	ww = GetDeviceCaps(hDC, HORZRES);
	hh = GetDeviceCaps(hDC, VERTRES);
    }
    else {
	if (width < 0.1) width = 0.1;
	if (height < 0.1) height = 0.1;
	dd =  GetDeviceCaps(hDC, HORZSIZE) / width;
	AL = (dd < 1.0) ? dd : 1.0;
	dd = GetDeviceCaps(hDC, VERTSIZE) / height;
	AL = (dd < AL) ? dd : AL;
	ww = (AL * width) * GetDeviceCaps(hDC, LOGPIXELSX) / 25.4;
	hh = (AL * height) * GetDeviceCaps(hDC, LOGPIXELSY) / 25.4;
    }
    x0 = (GetDeviceCaps(hDC, HORZRES) - ww) / 2;
    y0 = (GetDeviceCaps(hDC, VERTRES) - hh) / 2;
    obj->rect = rect(x0, y0, ww, hh);
    obj->depth = GetDeviceCaps(hDC, BITSPIXEL)* GetDeviceCaps(hDC, PLANES);
    obj->die = private_delprinter;
    obj->drawstate = copydrawstate();
    obj->drawstate->dest = obj;

    docinfo.cbSize = sizeof(DOCINFO);	/* set this size... */
    docinfo.lpszDocName = "GraphAppPrintJob";
    docinfo.lpszOutput = 0;		/* no file output... */
    docinfo.lpszDatatype = 0;
    docinfo.fwType = 0;

    if (StartDoc(hDC, &docinfo) <= 0) {
	R_ShowMessage(_("Unable to start the print job"));
	del(obj);
	return NULL;
    }

    StartPage(hDC);
    return obj;
}


void nextpage(printer p)
{
    if (!p || (p->kind != PrinterObject)) return;
    EndPage((HDC) p->handle);
    StartPage((HDC) p->handle);
}
