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
#include <commdlg.h>

static char szFilter[RBuffLen + 2];
static char szImgFilter[] = "R Image File(*.RMG)|*.rmg";
static char szOpenFilter[] = "Text Files (*.TXT)|*.txt|All Files(*.*)|*.*";
static char szDirName[RBuffLen];
extern char RFName[RBuffLen];

OPENFILENAME ofn;

static void ofninit()
{
        ofn.lStructSize = sizeof(OPENFILENAME);
        ofn.hwndOwner = NULL;
        ofn.hInstance = NULL;
        ofn.lpstrFilter = NULL;
        ofn.lpstrCustomFilter = NULL;
        ofn.nMaxCustFilter = 0;
        ofn.nFilterIndex = 0;
        ofn.lpstrFile = NULL;
        ofn.nMaxFile = 0;
        ofn.lpstrFileTitle = 0;
        ofn.nMaxFileTitle = 0;
        ofn.lpstrTitle = NULL;
        ofn.nFileOffset = 0;
        ofn.nFileExtension = 0;
        ofn.lpstrInitialDir = NULL;
        ofn.Flags = 0;
        ofn.nFileOffset = 0;
        ofn.nFileExtension = 0;
        ofn.lpstrDefExt = NULL;
        ofn.lCustData = 0L;
        ofn.lpfnHook = NULL;
        ofn.lpTemplateName = NULL;
}
    

int Win_ROpenDlg(HWND hwnd, char *iname)
{
        int i, n;
        char title[512];

        strcpy(szFilter, szOpenFilter);
        sprintf(title,"R %s File",iname);
        n = strlen(szFilter);
        for (i = 0; i < n; i++)
                if (szFilter[i] == '|')
                        szFilter[i] = '\0';
        szFilter[n] = '\0';
        szFilter[n + 1] = '\0';

        if (! GetCurrentDirectory(sizeof(szDirName), szDirName))
                return 0;

        ofninit();
        
        
        ofn.lpstrFilter = szFilter;
        ofn.nFilterIndex = 2;
        ofn.lpstrFile = RFName;
        ofn.nMaxFile = RBuffLen;
        ofn.lpstrInitialDir = szDirName;
        ofn.Flags = OFN_PATHMUSTEXIST | OFN_FILEMUSTEXIST | OFN_HIDEREADONLY;
        ofn.lpstrTitle = title;

        return (int) GetOpenFileName(&ofn);
}

int Win_RSaveDlg(HWND hwnd)
{
        int i, n;

        strcpy(szFilter, szImgFilter);

        n = strlen(szFilter);
        for (i = 0; i < n; i++)
                if (szFilter[i] == '|')
                        szFilter[i] = '\0';
        szFilter[n] = '\0';
        szFilter[n + 1] = '\0';

        if (! GetCurrentDirectory(sizeof(szDirName), szDirName))
                return 0;

        ofninit();
        
       
        ofn.lpstrFilter = szFilter;
        ofn.nFilterIndex = 1;
        ofn.lpstrFile = RFName;
        ofn.nMaxFile = RBuffLen;
        ofn.lpstrInitialDir = szDirName;
        ofn.Flags = OFN_OVERWRITEPROMPT;
        ofn.lpstrTitle = "R Save Image";

        return (int) GetSaveFileName(&ofn);
}
