/*
 *  R : A Computer Language for Statistical Data Analysis
 *  file system.c
 *  Copyright (C) 1998-1999  Ross Ihaka
 *                2000-2002  Stefano M. Iacus and the R core team
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
 *
 *  The R application now handles redirection files "<" and ">" if
 *  the user specifies them. It also accept command line options.
 *  Implemented in R 1.2.2., Stefano M.Iacus, Feb 2001
 */
 
#include <RCarbon.h>

#ifdef __MRC__
#include <CarbonStdCLib.h>
#endif

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include "Defn.h"
#include "Fileio.h"
#include "Graphics.h"
#include "RIntf.h"
#include <Rdevices.h>
#include <CFBundle.h>
#include <Folders.h>

#include "IOStuff.h"		/*-> Defn.h */
#include "Fileio.h"
#include "Parse.h"

extern long start_Time;
extern long last_Time;
char genvString[256];

#ifdef Macintosh 
int pclose(FILE *fp) {
    return(fclose(fp));
}
#endif


/* Experimental code for calling system X functions from Carbon R */
/* The code is adapted from the MiniShell.c file from Apple's     */
/* CarbonLib SDK 1.6.                                             */

/* BSD function prototypes */

int	execv( const char *path, char *const argv[] );
typedef int (*execvFuncPtr)( const char*, char **const );

FILE *	 popen(const char *command, const char *type);
typedef FILE *(*BSDpopenFuncPtr)( const char*, const char* );

int	pclose( FILE *stream );
typedef int (*BSDpcloseFuncPtr)( FILE* );

typedef int (*BSDfreadFuncPtr)( void *, size_t, size_t, FILE * );
typedef int (*BSDfgetsFuncPtr)( char *, int, FILE * );

BSDpopenFuncPtr		BSDpopenFunc;
BSDfreadFuncPtr		BSDfread;
BSDfgetsFuncPtr		BSDfgets;
BSDpcloseFuncPtr	BSDpclose;

void	InvokeTool( char *toolName );
OSStatus LoadFrameworkBundle(CFStringRef framework, CFBundleRef *bundlePtr);

Boolean CanLoadFrameWork(void);
Boolean IsFrameWorkLoaded = false;

OSStatus DoubleClickFile(char *filename);



/* end of code from Apple's CarbonLib SDK 1.6 */
 
void R_Suicide(char *s);
void GetSysVersion(void);
void CleanTempDir(void);



#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <errno.h>

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>

#ifndef __MRC__
#include <unix.h>
#endif

#include <Files.h>
#include <Folders.h>

#ifndef __MRC__
#include <sioux.h>
#endif

#ifndef MIN
#define MIN(a,b)                 ((a) < (b) ? (a) : (b))
#endif

typedef struct _EnviromentPair {
    char *key;
    char *value;
} EnviromentPair;

FILE * FSp_fopen(ConstFSSpecPtr spec, const char * open_mode);
char *load_entry(FILE *file);
EnviromentPair *ParseLine(char *line);

Boolean finished=true;	/* Boolean variable that takes into account the fact we are */
						/* currently editing an R object with the internal editor   */

static char *DefaultFileName;
static int  EdFileUsed = 0;
extern SInt16               Edit_Window;
extern WindowPtr            Edit_Windows[MAX_NUM_E_WIN + 1];
FSSpec tempeditFSS;    	/* This is the temporary edit file FSSpec used by do_edit */

static int DefaultSaveAction = 0;
static int DefaultRestoreAction = 1;

SA_TYPE SaveAction = SA_SAVEASK;
SA_TYPE	RestoreAction = SA_RESTORE;
Rboolean UsingReadline = TRUE;
Rboolean LoadSiteFile = TRUE;
Rboolean LoadInitFile = TRUE;
Rboolean DebugInitFile = FALSE;

extern Rboolean R_Interactive;

long start_Time, last_Time;
SInt16 gAppResFileRefNum;
char testBuf[ALLOW_INPUT_LENGTH];
extern WindowPtr gWindowPtrArray[kMaxWindows + 2];
extern void doGetPreferences(void);

extern char InitFile[256];
extern WindowPtr Console_Window;
extern SInt16 gTextSize;
void  R_doErrorAlert(Str255 labelText);
void  StrToStr255(char* sourceText, Str255 targetText);
void R_ShowMessage(char *);


extern void R_Edit(char** lines, int nlines);
extern void main_1 ( void );
extern Boolean              Have_Console;

char *mac_getenv(const char *name);

void R_setStartTime(void);

#ifdef __MRC__
int mkdir(char *,int );
int rmdir(char *);

extern int chdir(char *);
extern int getcwd(char *,int );

#endif

extern SInt32	systemVersion ;

int R_ReadConsole(char *prompt, unsigned char *buf, int len,int addtohistory)
{
 #ifndef __MRC__
    if(fileno(stdin) > 1) 
		return( FileReadConsole(prompt, buf, len, addtohistory) ); 
    else 
#endif
		R_ReadConsole1(prompt, buf, len, addtohistory); 

    buf[strlen((char *)buf)-1] ='\n';
    buf[strlen((char *)buf)] = '\0';

    return 1;

}

static int
FileReadConsole(char *prompt, char *buf, int len, int addhistory)
{
    int ll;
    if (!R_Slave) {
     RWrite(prompt);
    }
    if (fgets(buf, len, stdin) == NULL)
	return 0;
/* according to system.txt, should be terminated in \n, so check this
   at eof */
    ll = strlen((char *)buf);
    if (feof(stdin) && buf[ll - 1] != '\n' && ll < len) {
	buf[ll++] = '\n'; buf[ll] = '\0';
    }
    if (!R_Interactive && !R_Slave)
	 RWrite(buf);
    return 1;
}



	/* Write a text buffer to the console. */
	/* All system output is filtered through this routine. */


void R_WriteConsole(char *buf, int len)
{
#ifndef __MRC__
    if(fileno(stdout) > 1)
	 fputs(buf,stdout);
    else
#endif
     R_WriteConsole1(buf, len);
}


	/* Indicate that input is coming from the console */
	/* No longer used ??? */

void R_ResetConsole()
{
}

	/* Make sure that pending output is flushed */

void R_FlushConsole()
{
#ifndef __MRC__
    if(fileno(stdin) > 1)
	fflush(stdin);
#endif
}


	/* Clear Console EOF */

void R_ClearerrConsole()
{
#ifndef __MRC__
    if(fileno(stdin) > 1)
	clearerr(stdin);
#endif
}


	/*--- File Handling Code ---*/


	/* Tab induced filename expansion */
	/* Unimplemented for the Macintosh */

char	*R_ExpandFileName(char *s)
{
    /* return the string unmodified */
    return s;
}


FILE *R_OpenSysInitFile(void)
{
    char buf[256];
    FILE *fp;

    sprintf(buf, "%s:library:base:R:Rprofile", R_Home);
    fp = R_fopen(buf, "r");
    return fp;
}


FILE *R_OpenSiteFile(void)
{
    /* This code finds where the R application was invoked */
    /* and descends from there to the folder "etc" and opens */
    /* the file "Rprofile" within that directory.  It returns */
    /* the resulting file pointer. */
    return NULL;
}

    /* This code attempts to open the file ".Rprofile" in the */
    /* current folder and returns the resulting file pointer. */
    /* Does thos make sense on the Mac.  Probably not. */

FILE *R_OpenInitFile(void)
{
    char  buf[256];
    FILE *fp;

    fp = NULL;
    if (LoadInitFile) {
	if ((fp = R_fopen(".Rprofile", "r")))
	    return fp;
	sprintf(buf, "%s:.Rprofile", R_Home);
	if ((fp = R_fopen(buf, "r")))
	    return fp;
    }
    return fp;

}

void GetSysVersion(void){
 Gestalt ( gestaltSystemVersion, & systemVersion ) ;
 systemVersion = ( systemVersion << 16 ) | 0x8000 ;
 
}



/* R_OpenFile
*/
FILE* R_OpenFile1(char *file)
{
    FILE*                fp;
   
    fp = R_fopen(file, "r");

    return fp;
}



FILE *R_OpenLibraryFile(char *file)
{
    char buf[256];
    FILE *fp;
    sprintf(buf, "%s:library:base:R:%s", R_Home, file);
	fp = R_fopen(buf, "r");
    return fp;
}


static char R_HomeLocation[MAC_FILE_SIZE];
static char R_DefHistFile[MAC_FILE_SIZE];

void GetHomeLocation(void);

static char curdir[MAC_FILE_SIZE];

void GetHomeLocation(void)
{
    R_HomeLocation[0] = '\0';
    getcwd(R_HomeLocation,MAC_FILE_SIZE); 
    R_HomeLocation[strlen(R_HomeLocation)-1]='\0';
#ifdef __MRC__
    if( RunningOnCarbonX()) {
     ConvertHFSPathToUnixPath(R_HomeLocation, (char *)&curdir) ;
     bsd_chdir(curdir);
    }
#endif
    
}

char *R_HomeDir()
{
    return R_HomeLocation;
}

Rboolean R_HiddenFile(char *filename)
{
    int len = strlen(filename);
    if (filename[len - 1] == '\r')
	return 1;
    else
  	return 0;
}

	/*--- Initialization Code ---*/

	/* NOTE: The timing code below will have to be adpated */
	/* to use the macintosh specific timing code. */

char *strdup (char *str);

int Mac_initialize_R(int ac, char **av);
#ifndef __MRC__
int main(int ac, char **av)
#else
int ac;
char **av;
int main(void)
#endif
{
    int value;
    char *p;
    SInt16 a;
#ifdef __MRC__
    ac = 1;
    av = (char**) calloc(ac, sizeof(char*));
	av[0] = strdup("R");
#endif
 
    gc_inhibit_torture = 1;
    
#ifndef __MRC__
    SIOUXSettings.standalone = false;  // I only use SIOUX to have command line
    SIOUXSettings.setupmenus = false;  // I'll set up the menus
    SIOUXSettings.initializeTB = false;  // I manage the ToolBox
    SIOUXSettings.asktosaveonclose = false;
    SIOUXSettings.autocloseonquit = true;

    ac = ccommand(&av);  // This must be the first  command after variables initializations !!!
 
#endif
    	
    /* FIXME HERE: record the time at which the program started. */
    /* This is probably zero on the mac as we have direct */
    /* access to the number of ticks since process start */

    /* Set up the file handling defaults. */

    R_Quiet = 0;

    /* ... */
 
/* *** */    
    Mac_initialize_R ( ac, av );
	
/* *** */
    if(R_Interactive)
     { 
     changeSize(Console_Window, gTextSize);
       DoActivate( true,Console_Window );
     }
    /* Call the real R main program (in ../main/main.c) */
			
    mainloop();
   
    return 0;
}

int Mac_initialize_R(int ac, char **av)
{
    int i, ioff = 1, j, value, ierr;
    char *p, msg[1024], **avv;
    structRstart rstart;
    Rstart Rp = &rstart;
    OSErr err;
    
    GetSysVersion();
    GetHomeLocation(); /* should stay here because getenv depends on this */
    if((R_Home = R_HomeDir()) == NULL)
		R_Suicide("R home directory is not defined");
    
    
    if ( (err = Initialize()) == noErr ) {
        
	gAppResFileRefNum = CurResFile();
	
	doGetPreferences();

#ifndef __MRC__
	if((fileno(stdin)==0) || (fileno(stdout)==1)){
#endif
    	DoNew(true);
  		Console_Window = FrontWindow();  
#ifndef __MRC__
    }
#endif
    }
    else
	 return(1);


    IsFrameWorkLoaded = CanLoadFrameWork();

#ifdef _R_HAVE_TIMING_
    R_setStartTime();
#endif
    R_DefParams(Rp);

    /* Store the command line arguments before they are processed
       by the R option handler. These are stored in Rp and then moved
       to the global variable CommandLineArgs in R_SetParams.
    */

    R_set_command_line_arguments(ac, av, Rp);

    R_common_command_line(&ac, av, Rp);


    while (--ac) {
	if (**++av == '-') {
	    if(!strcmp(*av, "--no-readline")) {
		UsingReadline = 0;
	    } else {
		sprintf(msg, "WARNING: unknown option %s\n", *av);
		R_ShowMessage(msg);
	    }
	} else {
	    sprintf(msg, "ARGUMENT '%s' __ignored__\n", *av);
	    R_ShowMessage(msg);
	}
    }
    R_SetParams(Rp);
    
   
    if(!Rp->NoRenviron) process_user_Renviron();
 
 
    /* On Unix the console is a file; we just use stdio to write on it */
#ifndef __MRC__
    if(fileno(stdin) > 1){
	R_Consolefile = stdout;	
	R_Interactive = FALSE;
	Rp->R_Interactive = R_Interactive;
    }
    else{
#endif
    R_Interactive = TRUE;	/* On the Mac we must be interactive */
    Rp->R_Interactive = R_Interactive;
	R_Consolefile = NULL;	/* We get the input from the GUI console*/
#ifndef __MRC__
    }
    
    if(fileno(stdout) > 1)
	R_Outputfile = stdout;	/* We send output to the file specified by the user */
    else
#endif    
	R_Outputfile = NULL;	/* We send the output to the GUI console*/

  //  R_Sinkfile = NULL;		/* We begin writing to the console. */


/*
 *  Since users' expectations for save/no-save will differ, we decided
 *  that they should be forced to specify in the non-interactive case.
 */
    if (!R_Interactive && SaveAction != SA_SAVE && SaveAction != SA_NOSAVE)
	R_Suicide("you must specify `--save', `--no-save' or `--vanilla'");
    
    R_HistoryFile = R_DefHistFile;
    strcpy(R_HistoryFile, ".Rhistory");

    R_HistorySize = 512;
    if ((p = mac_getenv("R_HISTSIZE"))) {
	value = R_Decode2Long(p, &ierr);
	if (ierr != 0 || value < 0)
	    REprintf("WARNING: invalid R_HISTSIZE ignored;");
	else
	    R_HistorySize = value;
    }


    if (R_RestoreHistory)
	mac_loadhistory(R_HistoryFile);

    return(0);
}

void R_InitialData(void)
{
    R_RestoreGlobalEnv();
}

	/* R_CleanUp is invoked at the end of the session to give */
	/* the user the option of saving their data.  If ask=1 the */
	/* user is asked their preference, if ask=2 the answer is */
	/* assumed to be "no" and if ask=3 the answer is assumed to */
	/* be "yes".  When R is being used non-interactively, and */
	/* ask=1, the value is changed to 3.  The philosophy is */
	/* that saving unwanted data is less bad than non saving */
	/* data that is wanted. */
extern void MacFinalCleanup(void);

void R_CleanUp(SA_TYPE saveact, int status, int runLast)
{
    unsigned char buf[128];

    if(saveact == SA_DEFAULT) /* The normal case apart from R_Suicide */
	saveact = SaveAction;

 #ifndef __MRC__
    if(fileno(stdin) > 1) 
	 R_Interactive = false;
#endif
    
    if(saveact == SA_SAVEASK) {
	if(R_Interactive) {
	qask:
	    R_ClearerrConsole();
	    R_FlushConsole();
	    R_ReadConsole("Save workspace image? [y/n/c]: ", buf, 128, 0);
	    switch (buf[0]) {
	    case 'y':
	    case 'Y':
		saveact = SA_SAVE;
		break;
	    case 'n':
	    case 'N':
		saveact = SA_NOSAVE;
		break;
	    case 'c':
	    case 'C':
		jump_to_toplevel();
		break;
	    default:
		goto qask;
	    }
	} else
	    saveact = SaveAction;
    }
    switch (saveact) {
    case SA_SAVE:
	if(runLast) R_dot_Last();
	if(R_DirtyImage) R_SaveGlobalEnv();
	if(R_Interactive) 
	    mac_savehistory(R_HistoryFile);
	break;
    case SA_NOSAVE:
	if(runLast) R_dot_Last();
	break;
    case SA_SUICIDE:
    default:
        break;
    }
    
    
    R_RunExitFinalizers();
    CleanEd();
    CleanTempDir();
    KillAllDevices();
    if(saveact != SA_SUICIDE && R_CollectWarnings)
	PrintWarnings();	/* from device close and .Last */

    MacFinalCleanup();

    exit(status);

}

void R_Busy(int which)
{
    /* This can be used to gray out menus and change the */
    /* cursor (to a watch or equivalent) to indicate that an */
    /* extended computation is taking place. */
}


void R_SaveGlobalEnv(void)
{
    OSErr err;
    Boolean haveCancel;

    err = doRSave(&haveCancel);
//  err = doRSaveAs(&haveCancel);
    if (haveCancel){
	jump_to_toplevel();
    }
}


void R_RestoreGlobalEnv(void)
{
    if(RestoreAction == SA_RESTORE) {
	R_RestoreGlobalEnvFromFile(".RData", R_Quiet);
    }
}


	/*--- Platform Dependent Functions ---*/
#ifdef _R_HAVE_TIMING_
#include <time.h>

static clock_t StartTime;

void R_setStartTime(void)
{
    StartTime = clock(); // ticks from system boot
}

void R_getProcTime(double *data)
{
    double elapsed;
    elapsed = (clock() - StartTime) / (double)CLOCKS_PER_SEC;
    data[0] = R_NaReal; // we have no total user time
    data[1] = clock() / (double)CLOCKS_PER_SEC; // Real total system time
    data[2] = elapsed; // process system time
    data[3] = R_NaReal; // we don't have this under MacOS
    data[4] = R_NaReal; // as above
}

double R_getClockIncrement(void)
{
    return 1.0 / (double) CLOCKS_PER_SEC;
}

SEXP do_proctime(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans = allocVector(REALSXP, 5);
    R_getProcTime(REAL(ans));
    return ans;
}
#endif /* _R_HAVE_TIMING_ */
   
SEXP do_getenv(SEXP call, SEXP op, SEXP args, SEXP env)
{
    int i, j;
    char *s = NULL;
    SEXP ans;
    FILE *fp;
    EnviromentPair *Env1;
    FSSpec spec;
    OSErr err;
    char env_buff[1000];
    char temp_path[NAME_MAX];
    
    checkArity(op, args);

    if (!isString(CAR(args)))
	errorcall(call, "wrong type for argument");

    i = LENGTH(CAR(args));
    if (i == 0) {
    
	sprintf(temp_path,"%s:.Renviron",R_Home);

	err = FSpLocationFromFullPath(strlen(temp_path),temp_path,&spec);
    
	/* try open the file in the R_Home: folder */
	fp = FSp_fopen(&spec,"r");
	if (fp == NULL)
	{ /* Okey, lets try open the file in the preference folder */
	    FSpFindFolder_Name(
		kOnSystemDisk,
		kPreferencesFolderType,
		kDontCreateFolder,
		&spec,
		"\p.Renviron");
    
	    fp = FSp_fopen(&spec,"r");
	    if (fp == NULL)
	    {
		if(Have_Console)
		 errorcall(call,"There is no environment file");
		return R_NilValue; /* there is no enviroment-file */
	    }
	}

	s = load_entry(fp);
	while (s != NULL)
	{   /* parse the file line by line */
	    Env1 = ParseLine(s);
	    if (strlen(Env1->value) > 0) 
		i++;
	    s = load_entry(fp);  /* read next line */
	}
	if(i==0) {
	    fclose(fp);
	    errorcall(call,"The environment file is empty");
	    return R_NilValue;
	}
    
	PROTECT(ans = allocVector(STRSXP, i));
    
	i=0;
	fseek(fp,0,0);
    
	s = load_entry(fp);
    
	while (s != NULL)
	{   /* parse the file line by line */
	    Env1 = ParseLine(s);
	    if (strlen(Env1->value) > 0)
	    {       /* we found a key/value pair */
		sprintf(env_buff,"%s=%s",Env1->key,Env1->value);
		SET_STRING_ELT(ans, i, mkChar(env_buff));
		i++;
	    }
	    s = load_entry(fp);  /* read next line */
	}
	fclose(fp);
    } else {
	PROTECT(ans = allocVector(STRSXP, i));
	for (j = 0; j < i; j++) {
	    s = mac_getenv(CHAR(STRING_ELT(CAR(args), j)));
	    if (s == NULL)
		SET_STRING_ELT(ans, j, mkChar(""));
	    else
		SET_STRING_ELT(ans, j, mkChar(s));
	}
    }
    UNPROTECT(1);
    return (ans);
}



SEXP do_machine(SEXP call, SEXP op, SEXP args, SEXP env)
{
    return mkString("Macintosh");
}

/* Behaves like Unix version.             */
/* Jago March 24th 2002. Stefano M. Iacus */

SEXP do_system(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    char	command[256];
    FILE	*fp;
    SEXP	tlist = R_NilValue, tchar, rval;
    char buf[120];
    int i,j,read;
    
    if(IsFrameWorkLoaded){
     checkArity(op, args);
     if (!isValidStringF(CAR(args)))
	  errorcall(call, "non-empty character argument expected");
     
     if (isLogical(CADR(args)))
	  read = INTEGER(CADR(args))[0];
     
     if (read) {
 	  PROTECT(tlist);
		
	  fp = BSDpopenFunc( CHAR(STRING_ELT(CAR(args), 0)), "r" );							
	  for (i = 0; BSDfgets(buf, 120, fp); i++) {
	    read = strlen(buf);
	    buf[read - 1] = '\0';
	    tchar = mkChar(buf);
	    UNPROTECT(1);
	    PROTECT(tlist = CONS(tchar, tlist));
	  }

	  (void) BSDpclose( fp );							/* Close the FILE stream */

	  rval = allocVector(STRSXP, i);;
	  for (j = (i - 1); j >= 0; j--) {
	    SET_STRING_ELT(rval, j, CAR(tlist));
	    tlist = CDR(tlist);
	  }
	  UNPROTECT(1);
	  return (rval);
     } /* if(read) */
     else {
	  tlist = allocVector(INTSXP, 1);
	  strncpy(command,CHAR(STRING_ELT(CAR(args), 0)),255);    
      INTEGER(tlist)[0] = 0;
      InvokeTool(command);
	  R_Visible = 0;
	  return tlist;
     }
    } /* if(IsFrameWorkLoaded) */
    else
    errorcall(call, "\n The function \"system\" is not implemented on Macintosh\n");

    return R_NilValue;
}

/* 
   	R_tmpnam is a version of the Unix R_tmpnam for Macintosh. 
   	This routine has been rewritten. Now temporary files are
   	written in the default System's Temporary Files directory.
   	This directory is freed as needed by the MasOS from time
   	to time.
   	First version: (Stefano M. Iacus) Jago Nov-00, R pre-alpha 3
	Jago April 2001, Stefano M. Iacus
*/
   
#define MAC_READ_OR_WRITE	0x0 /* fake a UNIX mode */
   


char *R_tmpnam(const char * prefix, const char * tempdir)
{
    char *tmp, tm[PATH_MAX], tmp1[PATH_MAX], *res;
    unsigned int n, done = 0;

    if(!prefix) prefix = "";	/* NULL */
    strcpy(tmp1, tempdir);

    for (n = 0; n < 100; n++) {
	/* try a random number at the end */
        sprintf(tm, "%s:%s%d", tmp1,prefix, rand());
        if (!R_FileExists(tm)) { done = 1; break; }
    }
    
   if(!done)
	error("cannot find unused tempfile name");
    res = (char *) malloc((strlen(tm)+1) * sizeof(char));
    strcpy(res, tm);
    return res;
}




/*
   do_tempfile it is just as under unix. No changes
   (Stefano M. Iacus) Jago Nov-00, implemented pre-alpha 3
   (Changed by DJM Mar-03)
*/


SEXP do_tempfile(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP  ans, pattern, tempdir;
    char *tn, *td, *tm;
    int i, n1, n2, slen;

    checkArity(op, args);
    pattern = CAR(args); n1 = length(pattern);
    tempdir = CADR(args); n2 = length(tempdir);
    if (!isString(pattern))
        errorcall(call, "invalid filename pattern");
    if (!isString(tempdir))
        errorcall(call, "invalid tempdir");
    if (n1 < 1)
	errorcall(call, "no patterns");
    if (n2 < 1)
	errorcall(call, "no tempdir");
    slen = (n1 > n2) ? n1 : n2;
    PROTECT(ans = allocVector(STRSXP, slen));
    for(i = 0; i < slen; i++) {
	tn = CHAR( STRING_ELT( pattern , i%n1 ) );
	td = CHAR( STRING_ELT( tempdir , i%n2 ) );
	/* try to get a new file name */
	tm = R_tmpnam(tn, td);
	SET_STRING_ELT(ans, i, mkChar(tm));
	if(tm) free(tm);
    }
    UNPROTECT(1);
    return (ans);
}

SEXP do_tempdir(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP  ans;

    PROTECT(ans = allocVector(STRSXP, 1));
    SET_STRING_ELT(ans, 0, mkChar(R_TempDir));
    UNPROTECT(1);
    return (ans);
}


/*
   do_dircreate it is just as under Windows. 
   (Stefano M. Iacus) Jago Jan-01, implemented in R 1.2.0 beta 1
*/

SEXP do_dircreate(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP  path, ans;
    char *p, dir[PATH_MAX];
    int res;

    checkArity(op, args);
    path = CAR(args);
    if (!isString(path) || length(path) != 1)
	errorcall(call, "invalid path argument");
    strcpy(dir, CHAR(STRING_ELT(path, 0)));
    for(p = dir; *p != '\0'; p++)
	if(*p == '/') *p = ':';  // Differs from Windows, Jago
    res = mkdir(dir,0);   // Differs from Windows, Jago 
    PROTECT(ans = allocVector(LGLSXP, 1));
    LOGICAL(ans)[0] = (res==0);
    UNPROTECT(1);
    return (ans);
}


static void SelectTargetsToLaunch(void);



SEXP do_truepath(SEXP call, SEXP op, SEXP args, SEXP env);
/* This function returns unix-like path under OSX.
   Useful if you want to use OSX applications as R
   helpers like, for example, an external editor, a
   browser, etc.
   Jago Aug 8 2002, Stefano M. Iacus
*/
   
SEXP do_truepath(SEXP call, SEXP op, SEXP args, SEXP env){
    char buf[300];
    SEXP ans;
    char *file;
                          
    checkArity(op, args);

    if (!isString(CAR(args)))
	 errorcall(call, "invalid topic argument");
    file = CHAR(STRING_ELT(CAR(args), 0));

    if( RunningOnCarbonX()) 
     ConvertHFSPathToUnixPath(file, (char *)&buf);   
    else
     strcpy(buf,file);
    
    PROTECT(ans = allocVector(STRSXP, 1));
	SET_STRING_ELT(ans, 0, mkChar(buf));	
    UNPROTECT(1);
    return (ans);  
}


/* Similar to Windows. We only handle HTML help files
   Stefano M. Iacus (Jago Jan 2001)
*/
   

SEXP do_dataentry(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    errorcall(call, "unimplemented function\n");
}


void InitEd(){
    DefaultFileName = R_tmpnam("REdit", R_TempDir);
}

void CleanEd()
{
    if(EdFileUsed==0) R_unlink(DefaultFileName,1);
}



/*  This routine has been completely rewritten. This is the unix equivalent to
    what is found src/unix/edit.c file and adpted for the Macintosh.
    For the time beeing the internal editor is used, next step is to allow the 
    user to use an external editor.
    Jago April 2001, Stefano M. Iacus
*/
    
SEXP do_edit(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int   i, rc, status;
    SEXP  x, fn, envir, ed, t;
    char *filename, *editcmd, *vmaxsave, *cmd;
    FILE *fp;
    Str255	editname;	
	OSStatus err;

    checkArity(op, args);

    vmaxsave = vmaxget();

    x = CAR(args);
    if (TYPEOF(x) == CLOSXP) envir = CLOENV(x);
    else envir = R_NilValue;
    PROTECT(envir);

    fn = CADR(args);
    if (!isString(fn))
	error("invalid argument to edit()");

    if (LENGTH(STRING_ELT(fn, 0)) > 0) {
	filename = R_alloc(strlen(CHAR(STRING_ELT(fn, 0))), sizeof(char));
	strcpy(filename, CHAR(STRING_ELT(fn, 0)));
    }
    else filename = DefaultFileName;

    if (x != R_NilValue) {

	if((fp=R_fopen(R_ExpandFileName(filename), "w")) == NULL)
	    errorcall(call, "unable to open file");
	if (LENGTH(STRING_ELT(fn, 0)) == 0) EdFileUsed++;
	if (TYPEOF(x) != CLOSXP || isNull(t = getAttrib(x, R_SourceSymbol)))
	    t = deparse1(x, 0);
	for (i = 0; i < LENGTH(t); i++)
	    fprintf(fp, "%s\n", CHAR(STRING_ELT(t, i)));
	fclose(fp);
    }


    ed = CAR(CDDR(args));
    if (!isString(ed)) errorcall(call, "argument `editor' type not valid");
    cmd = CHAR(STRING_ELT(ed, 0));
    if (strlen(cmd) == 0) errorcall(call, "argument `editor' is not set");
    editcmd = R_alloc(strlen(cmd) + strlen(filename) + 6, sizeof(char));


    CopyCStringToPascal(filename,editname);

    err = FSMakeFSSpecFromPath(editname, &tempeditFSS);
    
    DoNew(true);
       
    RemWinMenuItem(Edit_Windows[Edit_Window-1]);
   
    err = ReadTextFile(&tempeditFSS,Edit_Windows[Edit_Window-1]);

   	UniqueWinTitle(Edit_Windows[Edit_Window-1] );

    ShowWindow(Edit_Windows[Edit_Window - 1]);
    finished=false;
    while(!finished)
	{
		ProcessEvent ( );
	}

    DoActivate(true,Console_Window);

    if((fp = R_fopen(R_ExpandFileName(filename), "r")) == NULL)
	errorcall(call, "unable to open file to read");
    R_ParseCnt = 0;
    x = PROTECT(R_ParseFile(fp, -1, &status));
    fclose(fp);
    if (status != PARSE_OK)
	errorcall(call,
		  "An error occurred on line %d\n use a command like\n x <- edit()\n to recover", R_ParseError);
    R_ResetConsole();
    {   /* can't just eval(x) here */
	int j, n;
	SEXP tmp = R_NilValue;

	n = LENGTH(x);
	for (j = 0 ; j < n ; j++)
	    tmp = eval(VECTOR_ELT(x, j), R_GlobalEnv);
	x = tmp;
    }
    if (TYPEOF(x) == CLOSXP && envir != R_NilValue)
	SET_CLOENV(x, envir);
    UNPROTECT(2);
    vmaxset(vmaxsave);
    return (x);
}

/* Adapted from Windows code for Macintosh
   It does not allow wildcards and only files/dirs
   created in the current session can be removed.
   (Stefano M. Iacus) Jago Nov-00
*/

#  ifndef __MRC__
#   include <stat.h>
#  else
#   include <mpw_stat.h>
#  endif

/*
   new functions 
   R_unlink
   R_unlink_one
   added 
   and
   do_unlink modified accordingly

   Jago: Aug 6 2002, Stefano M. Iacus   
*/

static int R_unlink(char *names, int recursive);
static int R_unlink_one(char *dir, char *name, int recursive);

static int R_unlink_one(char *dir, char *name, int recursive)
{
    char tmp[PATH_MAX];

    if(strcmp(name, ":") == 0) return 0;
    if(strcmp(name, "::") == 0) return 0;
    if(strlen(dir)) {
	strcpy(tmp, dir);
	if(*(dir + strlen(dir) - 1) != ':') strcat(tmp, ":");
	strcat(tmp, name);
    } else strcpy(tmp, name);
    return (recursive ? R_unlink(tmp, 1): remove(tmp)) !=0;
}

SEXP do_unlink(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP  fn, ans;
    int i, nfiles, failures = 0, recursive;

    checkArity(op, args);
    fn = CAR(args);
    nfiles = length(fn);
    if (!isString(fn) || nfiles < 1)
	errorcall(call, "invalid file name argument");
    recursive = asLogical(CADR(args));
    if (recursive == NA_LOGICAL)
	errorcall(call, "invalid recursive argument");
    for(i = 0; i < nfiles; i++)
	failures += R_unlink(CHAR(STRING_ELT(fn, i)), recursive);
    PROTECT(ans = allocVector(INTSXP, 1));
    if (!failures)
	INTEGER(ans)[0] = 0;
    else
	INTEGER(ans)[0] = 1;
    UNPROTECT(1);
    return (ans);
}

static int R_unlink(char *names, int recursive)
{
    int failures = 0;
    char *p, tmp[PATH_MAX];
    struct stat sb;

    strcpy(tmp, names);
    for(p = tmp; *p != '\0'; p++) if(*p == '/') *p = ':';
    if(stat(tmp, &sb) == 0) {
	 if(sb.st_mode & S_IFDIR) { /* Is this a directory? */
	    if(recursive) {
			if(rmdir(tmp)) 
			 failures++;
	    } 
	    else 
	     failures++; /* don't try to delete dirs */
	 } 
	 else /* Regular file */
	  remove(tmp);
	}
    
    return failures;
}



void R_Suicide(char *s)
{
    Str255 LabelText;
    int msglen,i;
    /* FIXME HERE: This should pop up a dialog box with the given */
    /* error message displayed, and the quit when the user hits the */
    /* OK button. */
    
  
    msglen = strlen(s);

    for(i = 1; i < msglen; i++)
	if(s[i] == 0x0A)
	    s[i] = 0x0D;

    StrToStr255(s, LabelText);
    R_doErrorAlert(LabelText);

    R_CleanUp(SA_NOSAVE,0,0);
    /* SA_NOSAVE means don't save anything and it's an unrecoverable abort */
}

void StrToStr255(char* sourceText, Str255 targetText)
{
    SInt16 StringLength, Counter;
    StringLength = strlen(sourceText);
    if (StringLength > 254) 
	StringLength = 254;
    targetText[0] = StringLength;
    for (Counter = 1; Counter <=StringLength; Counter ++){
	targetText[Counter] = sourceText[Counter-1];
    }   
}
void  R_doErrorAlert(Str255 labelText)
{
    AlertStdAlertParamRec paramRec;
    //Str255 labelText;
    Str255 narrativeText;
    SInt16 itemHit;
		
    paramRec.movable			= false;
    paramRec.helpButton			= false;
    paramRec.filterProc			= NULL;
    paramRec.defaultText		= (StringPtr) kAlertDefaultOKText;
    paramRec.cancelText			= NULL;
    paramRec.otherText			= NULL;
    paramRec.defaultButton		= kAlertStdAlertOKButton;
    paramRec.cancelButton		= 0;
    paramRec.position			= kWindowAlertPositionMainScreen;

    Do_StandardAlert(labelText);
    //(kAlertStopAlert,labelText,0,&paramRec,&itemHit);
}


	/* Declarations to keep f77 happy */

int MAIN_()  {return 0;}
int MAIN__() {return 0;}
int __main() {return 0;}



/*
 
  What follows is adapted from src/unix/sys-common.c
 
 */

/*
  See ../unix/system.txt for a description of functions
 */


#ifndef HAVE_STRDUP
extern char *strdup();
#endif


/* Permanent copy of the command line arguments and the number
   of them passed to the application.
   These are populated via the routine R_set_command_line_arguments()
   called from R_common_command_line().
*/
int    NumCommandLineArgs = 0;
char **CommandLineArgs = NULL;




/*
 * 5) FILESYSTEM INTERACTION
 */

/*
 * This call provides a simple interface to the "stat" system call.
 */

#ifdef HAVE_STAT
#include <types.h>
#  ifndef __MRC__
#   include <stat.h>
#  else
#   include <mpw_stat.h>
#  endif

Rboolean R_FileExists(char *path)
{
    struct stat sb;
    return stat(R_ExpandFileName(path), &sb) == 0;
}
#else
Rboolean R_FileExists(char *path)
{
    error("file existence is not available on this system");
}
#endif


FILE *R_fopen(const char *filename, const char *mode)
{
#ifdef __MRC__    
    char	unixPath[400];
#endif
    char 	tmpfile[401];
    int		i,slen;
    Boolean isAFolder = FALSE;
    
    if(!filename)
     return(NULL);

    if(filename[0] == ':'){
     strcpy(tmpfile,filename);
     goto openthisfile;
    }
    
    for(i = 1; i<strlen(filename); i++){
     if( filename[i] == ':' ){
     isAFolder = TRUE;
     break;
     }
    }
    
    if(!isAFolder && (filename[0] != ':')){
       tmpfile[0] = ':';
       tmpfile[1] = '\0';
       strcat(tmpfile,filename);
    }else
      strcpy(tmpfile,filename);

openthisfile:    
#ifdef __MRC__    

	if(systemVersion >= 0x10008000){ /* On System X we have to take care of */
	                                 /* Unix <-> HFS path differences       */	
	 ConvertHFSPathToUnixPath(tmpfile, (char *)&unixPath) ;
	 return( fopen((const char *)&unixPath,mode) );
    }
#endif 

    return(tmpfile ? fopen(tmpfile, mode) : NULL );
}



/*
 *  7) PLATFORM DEPENDENT FUNCTIONS
 */



SEXP do_putenv(SEXP call, SEXP op, SEXP args, SEXP env)
{
#ifdef HAVE_PUTENV
    int i, n;
    SEXP ans, vars;

    checkArity(op, args);

    if (!isString(vars =CAR(args)))
	errorcall(call, "wrong type for argument");

    n = LENGTH(vars);
    PROTECT(ans = allocVector(LGLSXP, n));
    for (i = 0; i < n; i++) {
	LOGICAL(ans)[i] = putenv(CHAR(STRING_ELT(vars, i))) == 0;
    }
    UNPROTECT(1);
    return ans;
#else
    error("`putenv' is not available on this system");
    return R_NilValue; /* -Wall */
#endif
}



SEXP do_interactive(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP rval;

    rval=allocVector(LGLSXP, 1);
    LOGICAL(rval)[0]= (R_Interactive) ? 1 : 0;
    return rval;
}

/*
 *  INITIALIZATION HELPER CODE
 */


void R_ShowMessage(char *msg)
{
    Str255 LabelText;
    int msglen,i;
  
    msglen = strlen(msg);  
    for(i = 1; i < msglen; i++)
	if(msg[i] == 0x0A)
	    msg[i] = 0x0D;
      
    StrToStr255(msg, LabelText);
    R_doErrorAlert(LabelText);
}

void R_DefParams(Rstart Rp)
{
    Rp->R_Quiet = FALSE;
    Rp->R_Slave = FALSE;
    Rp->R_Interactive = TRUE;
    Rp->R_Verbose = FALSE;
    Rp->RestoreAction = SA_RESTORE;
    Rp->SaveAction = SA_SAVEASK;
    Rp->LoadSiteFile = TRUE;
    Rp->LoadInitFile = TRUE;
    Rp->DebugInitFile = FALSE;
    Rp->vsize = R_VSIZE;
    Rp->nsize = R_NSIZE;
    Rp->max_vsize = R_SIZE_T_MAX;
    Rp->max_nsize = R_SIZE_T_MAX;
    Rp->NoRenviron = FALSE;
}

#define Max_Nsize 50000000	/* about 1.4Gb 32-bit, 2.8Gb 64-bit */
#define Max_Vsize R_SIZE_T_MAX	/* unlimited */

#define Min_Nsize 220000
#define Min_Vsize (1*Mega)

void R_SizeFromEnv(Rstart Rp)
{
    int ierr;
    R_size_t value;
    char *p;

    if((p = getenv("R_VSIZE"))) {
	value = R_Decode2Long(p, &ierr);
	if(ierr != 0 || value > Max_Vsize || value < Min_Vsize)
	    R_ShowMessage("WARNING: invalid R_VSIZE ignored\n");
	else
	    Rp->vsize = value;
    }
    if((p = getenv("R_NSIZE"))) {
	value = R_Decode2Long(p, &ierr);
	if(ierr != 0 || value > Max_Nsize || value < Min_Nsize)
	    R_ShowMessage("WARNING: invalid R_NSIZE ignored\n");
	else
	    Rp->nsize = value;
    }
}

static void SetSize(R_size_t vsize, R_size_t nsize)
{
    char msg[1024];

    /* vsize >0 to catch long->int overflow */
    if (vsize < 1000 && vsize > 0) {
	R_ShowMessage("WARNING: vsize ridiculously low, Megabytes assumed\n");
	vsize *= Mega;
    }
    if(vsize < Min_Vsize || vsize > Max_Vsize) {
	sprintf(msg, "WARNING: invalid v(ector heap)size `%lu' ignored\n"
		 "using default = %gM\n", vsize, R_VSIZE / Mega);
	R_ShowMessage(msg);
	R_VSize = R_VSIZE;
    } else
	R_VSize = vsize;
    if(nsize < Min_Nsize || nsize > Max_Nsize) {
	sprintf(msg, "WARNING: invalid language heap (n)size `%lu' ignored,"
		 " using default = %ld\n", nsize, R_NSIZE);
	R_ShowMessage(msg);
	R_NSize = R_NSIZE;
    } else
	R_NSize = nsize;
}


void R_SetParams(Rstart Rp)
{
    R_Quiet = Rp->R_Quiet;
    R_Slave = Rp->R_Slave;
    R_Interactive = Rp->R_Interactive;
    R_Verbose = Rp->R_Verbose;
    RestoreAction = Rp->RestoreAction;
    SaveAction = Rp->SaveAction;
    LoadSiteFile = Rp->LoadSiteFile;
    LoadInitFile = Rp->LoadInitFile;
    DebugInitFile = Rp->DebugInitFile;
    SetSize(Rp->vsize, Rp->nsize);
    R_SetMaxNSize(Rp->max_nsize);
    R_SetMaxVSize(Rp->max_vsize);
    CommandLineArgs = Rp->CommandLineArgs;
    NumCommandLineArgs = Rp->NumCommandLineArgs;
}


/* Remove and process common command-line arguments */

/*
  This copies the command line arguments to the Rstart
  structure. The memory is obtained from calloc, etc.
  since these are permanent and it is not intended that
  they be modified. This is why they are copied before
  being processed and removed from the list.

  We might store these as a SEXP. I have no strong opinion
  about this.
 */
void
R_set_command_line_arguments(int argc, char **argv, Rstart Rp)
{
    int i;

    Rp->NumCommandLineArgs = argc;
    Rp->CommandLineArgs = (char**) calloc(argc, sizeof(char*));

    for(i = 0; i < argc; i++) {
	Rp->CommandLineArgs[i] = strdup(argv[i]);
    }
}


/*
  The .Internal which returns the command line arguments that are stored
  in global variables.
 */
SEXP
do_commandArgs(SEXP call, SEXP op, SEXP args, SEXP env)
{
    int i;
    SEXP vals;

    vals = allocVector(STRSXP, NumCommandLineArgs);
    for(i = 0; i < NumCommandLineArgs; i++) {
	SET_STRING_ELT(vals, i, mkChar(CommandLineArgs[i]));
    }

    return(vals);
}

void
R_common_command_line(int *pac, char **argv, Rstart Rp)
{
    int ac = *pac, newac = 1;	/* argv[0] is process name */
    int ierr;
    R_size_t value;
    char *p, **av = argv, msg[1024];

    R_RestoreHistory = 1;
    while(--ac) {
	if(**++av == '-') {
	    if (!strcmp(*av, "--version")) {
		PrintVersion(msg);
		R_ShowMessage(msg);
		exit(0);
	    }
#if 0
	    else if(!strcmp(*av, "--print-nsize")) {
		Rprintf("%d\n", R_NSize);
		exit(0);
	    }
	    else if(!strcmp(*av, "--print-vsize")) {
		Rprintf("%d\n", R_VSize);
		exit(0);
	    }
#endif
	    else if(!strcmp(*av, "--save")) {
		Rp->SaveAction = SA_SAVE;
	    }
	    else if(!strcmp(*av, "--no-save")) {
		Rp->SaveAction = SA_NOSAVE;
	    }
	    else if(!strcmp(*av, "--restore")) {
		Rp->RestoreAction = SA_RESTORE;
	    }
	    else if(!strcmp(*av, "--no-restore")) {
		Rp->RestoreAction = SA_NORESTORE;
		R_RestoreHistory = 0;
	    }
	    else if(!strcmp(*av, "--no-restore-data")) {
		Rp->RestoreAction = SA_NORESTORE;
	    }
	    else if(!strcmp(*av, "--no-restore-history")) {
		R_RestoreHistory = 0;
	    }
	    else if (!strcmp(*av, "--silent") ||
		     !strcmp(*av, "--quiet") ||
		     !strcmp(*av, "-q")) {
		Rp->R_Quiet = TRUE;
	    }
	    else if (!strcmp(*av, "--vanilla")) {
		Rp->SaveAction = SA_NOSAVE; /* --no-save */
		Rp->RestoreAction = SA_NORESTORE; /* --no-restore */
		Rp->LoadSiteFile = FALSE; /* --no-site-file */
		Rp->LoadInitFile = FALSE; /* --no-init-file */
		R_RestoreHistory = 0;     /* --no-restore-history */
		Rp->NoRenviron = TRUE;
	    }
	    else if (!strcmp(*av, "--no-environ")) {
		Rp->NoRenviron = TRUE;
	    }
	    else if (!strcmp(*av, "--verbose")) {
		Rp->R_Verbose = TRUE;
	    }
	    else if (!strcmp(*av, "--slave") ||
		     !strcmp(*av, "-s")) {
		Rp->R_Quiet = TRUE;
		Rp->R_Slave = TRUE;
		Rp->SaveAction = SA_NOSAVE;
	    }
	    else if (!strcmp(*av, "--no-site-file")) {
		Rp->LoadSiteFile = FALSE;
	    }
	    else if (!strcmp(*av, "--no-init-file")) {
		Rp->LoadInitFile = FALSE;
	    }
	    else if (!strcmp(*av, "--debug-init")) {
	        Rp->DebugInitFile = TRUE;
	    }
	    else if (!strcmp(*av, "-save") ||
		     !strcmp(*av, "-nosave") ||
		     !strcmp(*av, "-restore") ||
		     !strcmp(*av, "-norestore") ||
		     !strcmp(*av, "-noreadline") ||
		     !strcmp(*av, "-quiet") ||
		     !strcmp(*av, "-V") ||
		     !strcmp(*av, "-n") ||
		     !strcmp(*av, "-v")) {
		sprintf(msg, "WARNING: option %s no longer supported\n", *av);
		R_ShowMessage(msg);
	    }
            /* mop up --max/min/-n/vsize */
 	    else if(strncmp(*av+7, "size", 4) == 0) {
		if(strlen(*av) < 13) {
		    ac--; av++; p = *av;
		}
		else p = &(*av)[12];
		if (p == NULL) {
		    sprintf(msg, "WARNING: no value given for %s\n", *av);
		    R_ShowMessage(msg);
		    break;
		}
		value = R_Decode2Long(p, &ierr);
		if(ierr) {
		    if(ierr < 0)
			sprintf(msg, "WARNING: %s value is invalid: ignored\n",
				*av);
		    else
			sprintf(msg, "WARNING: %s=%ld`%c': too large and ignored\n",
				*av, value,
				(ierr == 1) ? 'M': ((ierr == 2) ? 'K' : 'k'));
		    R_ShowMessage(msg);

		} else {
		    if(!strncmp(*av, "--min-nsize", 11)) Rp->nsize = value;
		    if(!strncmp(*av, "--max-nsize", 11)) Rp->max_nsize = value;
		    if(!strncmp(*av, "--min-vsize", 11)) Rp->vsize = value;
		    if(!strncmp(*av, "--max-vsize", 11)) Rp->max_vsize = value;
		}
	    }
	    else if(strncmp(*av, "--vsize", 7) == 0) {
		if(strlen(*av) < 9) {
		    ac--; av++; p = *av;
		}
		else
		    p = &(*av)[8];
		if (p == NULL) {
		    R_ShowMessage("WARNING: no vsize given\n");
		    break;
		}
		value = R_Decode2Long(p, &ierr);
		if(ierr) {
		    if(ierr < 0) /* R_common_badargs(); */
			sprintf(msg, "WARNING: --vsize value is invalid: ignored\n");
		    else
			sprintf(msg, "WARNING: --vsize=%ld`%c': too large and ignored\n",
				value,
				(ierr == 1) ? 'M': ((ierr == 2) ? 'K' : 'k'));
		    R_ShowMessage(msg);

		} else
		    Rp->vsize = value;
	    }
	    else if(strncmp(*av, "--nsize", 7) == 0) {
		if(strlen(*av) < 9) {
		    ac--; av++; p = *av;
		}
		else
		    p = &(*av)[8];
		if (p == NULL) {
		    R_ShowMessage("WARNING: no nsize given\n");
		    break;
		}
		value = R_Decode2Long(p, &ierr);
		if(ierr) {
		    if(ierr < 0) /* R_common_badargs(); */
			sprintf(msg, "WARNING: --nsize value is invalid: ignored\n");
		    else
			sprintf(msg, "WARNING: --nsize=%lu`%c': too large and ignored\n",
				value,
				(ierr == 1) ? 'M': ((ierr == 2) ? 'K':'k'));
		    R_ShowMessage(msg);
		} else
		    Rp->nsize = value;
	    }
	    else {
		argv[newac++] = *av;
	    }
	}
	else {
	    argv[newac++] = *av;
	}
    }
    *pac = newac;
    return;
}

/* ------------------- process .Renviron files in C ----------------- */

/* remove leading and trailing space */
static char *rmspace(char *s)
{
    int   i;

    for (i = strlen(s) - 1; i >= 0 && isspace((int)s[i]); i--) s[i] = '\0';
    for (i = 0; isspace((int)s[i]); i++);
    return s + i;
}

/* look for ${FOO:-bar} constructs, recursively */
static char *findterm(char *s)
{
    char *p, *q;

    if(!strlen(s)) return "";
    if(strncmp(s, "${", 2)) return s;
    /* found one, so remove leading ${ and final } */
    if(s[strlen(s) - 1] != '}') return "";
    s[strlen(s) - 1] = '\0';
    s += 2;
    p = strchr(s, '-');
    if(!p) return "";
    q = p + 1; /* start of value */
    if(p - s > 1 && *(p-1) == ':') *(p-1) = '\0'; else *p = '\0';
    s = rmspace(s);
    if(!strlen(s)) return "";
    p = getenv(s);
    if(p && strlen(p)) return p; /* variable was set and non-empty */
    return findterm(q);
}

static void Putenv(char *a, char *b)
{
    char *buf;

    buf = (char *) malloc((strlen(a) + strlen(b) + 2) * sizeof(char));
    if(!buf) R_Suicide("allocation failure in reading Renviron");
    strcpy(buf, a); strcat(buf, "="); strcat(buf, b);
  //  putenv(buf);
    /* no free here: storage remains in use */
}


#define BUF_SIZE 255
#define MSG_SIZE 2000
static int process_Renviron(char *filename)
{
    FILE *fp;
    char *s, *p, sm[BUF_SIZE], *lhs, *rhs, msg[MSG_SIZE+50];
    int errs = 0;

    if (!filename || !(fp = R_fopen(filename, "r"))) return 0;
    sprintf(msg, "\n   File %s contains invalid line(s)", filename);
#ifdef __MRC__
    while(R_fgets(sm, BUF_SIZE, fp)) {
#else
    while(fgets(sm, BUF_SIZE, fp)) {
#endif
	sm[BUF_SIZE] = '\0';
	s = rmspace(sm);
	if(strlen(s) == 0 || s[0] == '#') continue;
	if(!(p = strchr(s, '='))) {
	    errs++;
	    if(strlen(msg) < MSG_SIZE) {
		strcat(msg, "\n      "); strcat(msg, s);
	    }
	    continue;
	}
	*p = '\0';
	lhs = rmspace(s);
	rhs = findterm(rmspace(p+1));
	/* set lhs = rhs */
	if(strlen(lhs) && strlen(rhs)) Putenv(lhs, rhs);
    }
    fclose(fp);
    if (errs) {
	strcat(msg, "\n   They were ignored\n");
	R_ShowMessage(msg);
    }
    return 1;
}


/* try .Renviron, then :.Renviron and finally R_HOME:.Renviron
   They are the same under MacOS but not under MacOSX as for
   CFM-Carbon applications, the current working directory 
   is ~user.
*/

void process_user_Renviron()
{
    char s[300];
    
    
    if(process_Renviron(".Renviron"))  return;
    if(process_Renviron(":.Renviron")) return;
    sprintf(s,"%s:.Renviron",R_Home);
    if(process_Renviron(s)) return;
    
}


SEXP do_syssleep(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    error("Sys.sleep is not implemented on this system");
    return R_NilValue;		/* -Wall */
}

SEXP do_sysinfo(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    warning("Sys.info is not implemented on this system");
    return R_NilValue;		/* -Wall */
}


/* doCopyPString
 */
void doCopyPString(Str255 sourceString,Str255 destinationString)
{
    SInt16   stringLength;

    stringLength = sourceString[0];
    BlockMove(sourceString + 1,destinationString + 1,stringLength);
    destinationString[0] = stringLength;
}


/* doConcatPStrings
 */
void  doConcatPStrings(Str255 targetString, Str255 appendString)
{
    SInt16   appendLength;

    appendLength = MIN(appendString[0],255 - targetString[0]);

    if(appendLength > 0)
    {
	BlockMoveData(appendString+1, targetString+targetString[0]+1,
		      (SInt32) appendLength);
	targetString[0] += appendLength;
    }
}

/* This code is taken from Apple's CarbonLib SDK 1.6 */
/* It consists of part of the MiniShell.c file       */


OSStatus LoadFrameworkBundle(CFStringRef framework, CFBundleRef *bundlePtr)
{
	OSStatus 	err;
	FSRef 		frameworksFolderRef;
	CFURLRef	baseURL;
	CFURLRef	bundleURL;
	
	if ( bundlePtr == nil )	return( -1 );
	
	*bundlePtr = nil;
	
	baseURL = nil;
	bundleURL = nil;
	
	err = FSFindFolder(kOnAppropriateDisk, kFrameworksFolderType, true, &frameworksFolderRef);
	if (err == noErr) {
		baseURL = CFURLCreateFromFSRef(kCFAllocatorSystemDefault, &frameworksFolderRef);
		if (baseURL == nil) {
			err = coreFoundationUnknownErr;
		}
	}
	if (err == noErr) {
		bundleURL = CFURLCreateCopyAppendingPathComponent(kCFAllocatorSystemDefault, 
		 baseURL, framework, false);
		if (bundleURL == nil) {
			err = coreFoundationUnknownErr;
		}
	}
	if (err == noErr) {
		*bundlePtr = CFBundleCreate(kCFAllocatorSystemDefault, bundleURL);
		if (*bundlePtr == nil) {
			err = coreFoundationUnknownErr;
		}
	}
	if (err == noErr) {
	    if ( ! CFBundleLoadExecutable( *bundlePtr ) ) {
			err = coreFoundationUnknownErr;
	    }
	}

	// Clean up.
	if (err != noErr && *bundlePtr != nil) {
		CFRelease(*bundlePtr);
		*bundlePtr = nil;
	}
	if (bundleURL != nil) {
		CFRelease(bundleURL);
	}	
	if (baseURL != nil) {
		CFRelease(baseURL);
	}	
	
	return err;
}

/*	This routine does the interesting stuff, specifically the routine popen() */
/* loaded into the BSDpopenFunc function pointer effectively does a fork()    */
/* and execv() and also opens a stream  for us to read output from.           */

void	InvokeTool( char *toolName )
{
	FILE				*fp;
	char				outputS[1024 * 24];					  /* 24k buffer */
	int					bytesRead;
	
	fp = BSDpopenFunc( toolName, "r" );							
	if ( fp == nil ) return;

	bytesRead	= BSDfread( outputS, sizeof(char), sizeof(outputS), fp );	
	/* stdout gets redirected to this FILE*, stderr still goes to the console */
	
	outputS[bytesRead]	= '\0';
	
	Rprintf( "\"%s\" output: \n%s\n", toolName, outputS );		

	(void) BSDpclose( fp );							/* Close the FILE stream */
}

Boolean CanLoadFrameWork(void)
{
	CFBundleRef 		sysBundle;
	char				command[256];
	OSStatus 			err;
		
	if ( ! RunningOnCarbonX() )
		return(false);
	
/*	Load the "System.framework" bundle.                 */
/*  Most UNIX/BSD routines are in the  System.framework */

	err = LoadFrameworkBundle( CFSTR("System.framework"), &sysBundle );
	if (err != noErr) return(false);
	
	//	Load the Mach-O function pointers for the routines we will be using.
	BSDfread		= (BSDfreadFuncPtr) CFBundleGetFunctionPointerForName( sysBundle, CFSTR("fread") );
	BSDfgets		= (BSDfgetsFuncPtr) CFBundleGetFunctionPointerForName( sysBundle, CFSTR("fgets") );
	BSDpopenFunc	= (BSDpopenFuncPtr) CFBundleGetFunctionPointerForName( sysBundle, CFSTR("popen") );
	BSDpclose		= (BSDpcloseFuncPtr) CFBundleGetFunctionPointerForName( sysBundle, CFSTR("pclose") );
	if ( (BSDpopenFunc == nil) || (BSDpclose == nil) || (BSDfread == nil) || (BSDfgets == nil) )
		return(false);
	
	return(true);	
}

/* end of code from Apple's CarbonLib SDK 1.6 */

#ifdef __MRC__
/* Reference date on Mac is 1904-1-1 on Posix is 1970-1-1 */

time_t POSIXMakeTime(struct tm *tm);

time_t POSIXMakeTime(struct tm *tm)
{
  return mktime(tm) - 2082844800u;
}
#endif



void InitTempDir()
{
    int len, done = false, res;
	char *tmp, tm[PATH_MAX], tmp1[PATH_MAX], *p;
    unsigned int n, pid;
    short 	foundVRefNum,plen;
    long	foundDirID;
    OSStatus	err;
    Str255		string;
    Handle		path = NULL;
    
 
    err = FindFolder(kOnSystemDisk,kTemporaryFolderType, 
    		kCreateFolder, &foundVRefNum, &foundDirID);

    if(err != noErr){
    	done = false;
    	goto cleanup;
    }

    pid = (unsigned int) getpid();
    for (n = 0; n < 100; n++) {
	/* try a random number at the end */
	    sprintf(tm, "%s%d", "Rtmp", rand());
        CopyCStringToPascal(tm,string);
        err = GetFullPath(foundVRefNum,foundDirID,string,&plen,&path);

     	HLock((Handle) path);
        strncpy(tm, *path, plen);
	    tm[plen] = '\0';
	    HUnlock((Handle) path);
 
        if (!R_FileExists(tm)) { done = true; break; }
    }
    
cleanup:

    if(!done)
	 R_Suicide("cannot find unused tempdir name");
    err = DirCreate(foundVRefNum, foundDirID, string, &foundDirID);
    if(err!=noErr) 
	R_Suicide("Can't mkdir R_TempDir");
    len = strlen(tm);
    p = (char *) malloc(len+1);
    if(!p) 
	R_Suicide("Can't allocate R_TempDir");
    else {
	R_TempDir = p;
	strcpy(R_TempDir, tm);
    }
}



void CleanTempDir(void)
{
    R_unlink(R_TempDir, 1);
}



/*
   Newly implemented mkdir and rmdir functions 
   Jago, Aug 6, 2002. Stefano M. Iacus
*/   

#ifdef __MRC__
int mkdir(char *x,int a)
{
	Str31		name;
	short		vRefNum;
    long		parID;
	OSStatus	err;
     long               dirID;
    Str255	   pathname;
    short             realVRefNum;
    long              realParID;
    Str255             realName;
    Boolean           isDirectory;

    CopyCStringToPascal(x,pathname);
   
    GetObjectLocation(vRefNum, dirID, &pathname, &realVRefNum,
     &realParID, &realName, &isDirectory);
    err = DirCreate(realVRefNum, realParID, pathname, &dirID);
  
    return err;
}

int rmdir(char *x)
{
	Str31		name;
	short		vRefNum;
    long		parID;
	OSStatus	err;
     long               dirID;
    Str255	   pathname;
    short             realVRefNum;
    long              realParID;
    Str255             realName;
    Boolean           isDirectory;

    CopyCStringToPascal(x,pathname);
   
    GetObjectLocation(vRefNum, dirID, &pathname, &realVRefNum,
    &realParID, &realName, &isDirectory);
     
    err = DeleteDirectory(realVRefNum, realParID, pathname); 
    return err;
}
#endif
