/*
 *  R : A Computer Langage for Statistical Data Analysis
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
#include "Graphics.h"
#include "Fileio.h"

static char szDirName[RBuffLen];
static jmp_buf R_Winjbuf;
float R_WinVersion;
        /*--- I n i t i a l i z a t i o n -- C o d e ---*/

static BOOL CheckSystem(void)
{
    OSVERSIONINFO osvi;
    DWORD vinfo;

#ifdef OLD
    osvi.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
    if (!GetVersionEx(&osvi))
        return(FALSE);
    R_WinVersion = (int) osvi.dwMajorVersion;
    R_WinVersion += (int) osvi.dwMinorVersion/10;
#else
        vinfo=GetVersion();
        R_WinVersion = (int) LOBYTE(LOWORD(vinfo));
        R_WinVersion += ((int) HIBYTE(LOWORD(vinfo)))/100.0;        
#endif
    if (R_WinVersion < 3.09 ) {
        sprintf(szDirName,"R requires Windows 3.1 or higher \n You have %f",R_WinVersion);
        MessageBox((HWND) NULL,
               szDirName,
               NULL,
               MB_ICONHAND);
    return(FALSE);
  }
  return(TRUE);
}

static void R_FileAssoc(char *szDirName)
{
    HKEY hkey;
    LONG res;
    DWORD disp;
    char t1[RBuffLen+5];

    res = RegCreateKeyEx( HKEY_CLASSES_ROOT, "R\\shell\\Run\\command",0,"",
        REG_OPTION_NON_VOLATILE, KEY_ALL_ACCESS, NULL, &hkey, &disp);
    if( res == ERROR_SUCCESS && disp != REG_OPENED_EXISTING_KEY ) {
        sprintf(t1,"%s %%1",szDirName);
        res = RegSetValueEx(hkey, NULL, 0, REG_SZ, t1, lstrlen(t1));
        RegCloseKey(hkey);
    }

    res = RegCreateKeyEx( HKEY_CLASSES_ROOT, ".rmg", 0, "", REG_OPTION_NON_VOLATILE,
                        KEY_ALL_ACCESS, NULL, &hkey, &disp);
    if( res == ERROR_SUCCESS && disp != REG_OPENED_EXISTING_KEY ) {
        res = RegSetValueEx(hkey, NULL, 0, REG_SZ, "R",lstrlen("R"));
        RegCloseKey(hkey);
    }
    return;
}    
 
    
            
    

extern HMENU RMenuEdit;
typedef int (*MYPROC)(int);

int WINAPI WinMain(HANDLE hinstCurrent, HANDLE hinstPrevious, 
LPSTR lpszCmdParam, int nCmdShow)
{
        int i;
        char *exe, *ep, tmp[RBuffLen], tm1;
        HINSTANCE hinstLib;
        MYPROC ProcAdd;
        DWORD erno;
          
        if (! CheckSystem()) return(FALSE);

        if( erno = GetLastError() )
                SetLastError(0);
        /* Create the Windows */
        if( !hinstPrevious )
                if( !InitApplication(hinstCurrent) )
                        return FALSE;

        if( !InitInstance(hinstCurrent, nCmdShow) )
                return FALSE;

        /* need to get the working directory */
        i = GetModuleFileName(NULL, szDirName, RBuffLen);
        if (i > RBuffLen || i==0)
                return FALSE;

        /* do the file association thing if need be */
        if( R_WinVersion >= 4.0 )
                R_FileAssoc(szDirName);

        erno = GetLastError();
        
        exe = strrchr(szDirName,'\\');
        *exe = '\0';
        setenv("RHOME",szDirName,1);
        setenv("HOME",szDirName,1);

 
        /* set up the memory sizes */
      R_ImageName[0] = '\0';
      if( strlen(lpszCmdParam) != 0 ) {
            exe = &lpszCmdParam[0];
parsemem:   while( isspace(*exe) )
                exe++;
            if( *exe == '-' ) {
                exe++;
                tm1 = *exe++;
                i=0;
                while( !isspace(*exe) ) 
                    tmp[i++] = *exe++;
                tmp[i]='\0';
                i = strtol(tmp, &ep, 10);
                if( *ep ) {
                        REprintf("invalid argument passed to R\n");
                        return FALSE;
                }
                if(tm1 == 'n')
                        R_NSize = i;
                else if( tm1 == 'v' )
                        R_VSmb = i;
                else
                        REprintf("warning: unkown option\n");
                if(*exe != '\0')
                        goto parsemem;
            }
            else if( *exe != '\0' )
                strcpy(R_ImageName, exe);
        }
        R_VSmb = max(R_VSize/1048576,2);
        
        R_SetMemory(R_NSize, R_VSmb);

        if ( strlen(R_ImageName) == 0 )   {
                strcpy(R_ImageName,szDirName);
                strcat(R_ImageName,"\\.RData.rmg");
        }

        /* test dll's 
        
        hinstLib = LoadLibrary("eda");
        if( hinstLib != NULL) {
                ProcAdd = (MYPROC) GetProcAddress(hinstLib, "tukeyline_");
                if( ProcAdd != NULL)
                        i=3;
        }

        FreeLibrary(hinstLib);
 */              

        erno = GetLastError();
        
        if( 0 == setjmp( R_Winjbuf )  )                                        
                mainloop();
        else
                EventLoop();    /* run the windows event loop so that everything closes down */
        
        DestroyMenu(RMenuDE);
        DestroyMenu(RMenuGraph);
        DestroyMenu(RMenuInit);
        DestroyMenu(RMenuConsole);
        DestroyMenu(RMenuEdit);
        return 0;
}

void R_SetMemory(int nsize, int vsize)
{
        HKEY hkey;
        DWORD disp, l1;
        LONG res;
        char buff[20];
        int rnsize=0, rvsize=0;

        if( nsize > 0 ) {
                res = RegCreateKeyEx( HKEY_USERS, "Sofware\\R\\NSize",0, "", REG_OPTION_NON_VOLATILE,
                        KEY_ALL_ACCESS, NULL, &hkey, &disp);
                if( res == ERROR_SUCCESS ) {
                        if( disp == REG_OPENED_EXISTING_KEY ) {
                                l1 = 20;
                                RegQueryValueEx( hkey, "",0, &disp, buff, &l1);
                                rnsize = atoi(buff);
                        } 
                        if( nsize > 1000000 || nsize < rnsize ) {
                            R_NSize = rnsize;
                            REprintf("warning: invalid language heap size ignored \n");
                        }
                        else {
                            R_NSize = nsize;
                            sprintf(buff,"%d",nsize);
                            res = RegSetValueEx( hkey, NULL, NULL, REG_SZ, buff, lstrlen(buff));
                        }
                        RegCloseKey(hkey);
                }
        }
        if( vsize > 0 ) {
                res = RegCreateKeyEx( HKEY_USERS, "Sofware\\R\\VSize",0, "", REG_OPTION_NON_VOLATILE,
                        KEY_ALL_ACCESS, NULL, &hkey, &disp);
                if( res == ERROR_SUCCESS ) {
                    if( disp == REG_OPENED_EXISTING_KEY ) {
                          l1 = 20;
                          RegQueryValueEx( hkey, "",0, &disp, buff, &l1);
                          rvsize = atoi(buff);
                    }                     
                    if( vsize > 1000 || vsize < rvsize) {
                        R_VSize = rvsize *1048576; /* Mb */
                        R_VSmb = rvsize;
                        REprintf("warning: invalid vector heap size ignored \n");
                    }
                    else {
                        R_VSize = vsize * 1048576;  /* Mb */
                        R_VSmb = vsize;
                        sprintf(buff,"%d",vsize);
                        res = RegSetValueEx( hkey, NULL, NULL, REG_SZ, buff, lstrlen(buff));            
                    }
                    RegCloseKey(hkey);
                }
       }
}


void suicide(char *s)
{
        RCleanUp(2);    /* 2 means don't save anything and it's an unrecoverable abort */
        longjmp(R_Winjbuf, 1);
}


SEXP do_quit(SEXP call, SEXP op, SEXP args, SEXP rho)
{
        char *tmp;
        int ask;

        if(R_BrowseLevel) {
                warning("can't quit from browser\n");
                return R_NilValue;
        }
        if( !isString(CAR(args)) )
                errorcall(call,"one of \"yes\", \"no\" or \"ask\" expected.\n");
        tmp = CHAR(STRING(CAR(args))[0]);
        if( !strcmp(tmp,"ask") )
                ask=1;
        else if( !strcmp(tmp,"no") )
                ask=2;
        else if( !strcmp(tmp,"yes") )
                ask=3;
        else
                errorcall(call,"unrecognized value of ask\n");
        RCleanUp(ask);
        PostMessage(RFrame, WM_CLOSE, 0, 0);
        
        return(R_NilValue);
}
void R_StartUp(void)
{
        R_Init = 1;
}

void R_Busy(int yes)
{
        /* Placeholder */
}

        /*--- I / O --S u p p o r t -- C o d e ---*/


        /*--- P l a t f o r m -- D e p e n d e n t -- F u n c t i o n s ---*/



SEXP do_machine(SEXP call, SEXP op, SEXP args, SEXP env)
{
        return mkString("Win32");
}

SEXP do_system(SEXP call, SEXP op, SEXP args, SEXP rho)
{
        errorcall(call, "\"system\" is only available on Unix");
}

/* this function should set up the file pointer and open the file
   on some systems it should check to make sure that there is room to
   store the image
*/


void dump_image(char* fname, int jump)
{
                FILE *fp;
                long Vsize;
                DWORD Clust, FreeClust, SectPerClust, BytesPerSect;
                char    tstr[2];

        fp = R_fopen(fname, "wb");
        if( !fp )
                error("can't save data -- unable to open file\n");

        /* check to see if another drive was specified; 
           must be a right way to do this one;
           currently (1997) this size stuff is being ignored
        */
        if(fname[1] == ':' )
                strncpy(tstr,fname,2);
        else
                tstr[0]='\0';

        if( strlen(tstr) > 0 )
                GetDiskFreeSpace(tstr,&BytesPerSect, &SectPerClust, &FreeClust, &Clust);
        else
                GetDiskFreeSpace(NULL,&BytesPerSect, &SectPerClust, &FreeClust, &Clust);
        Vsize=BytesPerSect*SectPerClust*FreeClust;


        R_WriteMagic(fp, R_MAGIC_BINARY);
        BinarySave(FRAME(R_GlobalEnv), fp);
        fclose(fp);
        if(jump)
                jump_to_toplevel();
}

void R_InitialData(void)
{
        R_RestoreGlobalEnv();
}

void RBusy(int which)
{
}

void R_SaveGlobalEnv(void)
{
        FILE *fp = R_fopen(R_ImageName, "w");
        if (!fp)
                error("can't save data -- unable to open %s\n",R_ImageName);
        R_WriteMagic(fp, R_MAGIC_BINARY);
        BinarySave(FRAME(R_GlobalEnv), fp);
        fclose(fp);
}               

void R_RestoreGlobalEnv(void)
{                       
        FILE *fp = R_fopen(R_ImageName,"r");
        if (!fp) {      
                /* warning here perhaps */
                return;
        }               
        if(!R_Quiet) 
                Rprintf("[Previously saved workspace restored]\n\n");
                        
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
}

