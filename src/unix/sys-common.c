/*
  R : A Computer Language for Statistical Data Analysis
  Copyright (C) 1995-1996   Robert Gentleman and Ross Ihaka
  Copyright (C) 1997-2000   Robert Gentleman, Ross Ihaka
                            and the R Development Core Team

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at
  your option) any later version.

  This program is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307,
  U.S.A.
 */

/*
  See ../unix/system.txt for a description of functions
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "Defn.h"
#include "Fileio.h"

#include <string.h>
#ifndef HAVE_STRDUP
extern char *strdup();
#endif

extern int SaveAction;
extern int RestoreAction;
extern int LoadSiteFile;
extern int LoadInitFile;
extern int DebugInitFile;

      /* Permanent copy of the command line arguments and the number
         of them passed to the application.
         These are populated via the routine R_set_command_line_arguments()
         called from R_common_command_line().
       */
    int    NumCommandLineArgs = 0;
    char **CommandLineArgs = NULL;



/*
 *  4) INITIALIZATION AND TERMINATION ACTIONS
 */

void R_InitialData(void)
{
    R_RestoreGlobalEnv();
}


FILE *R_OpenLibraryFile(char *file)
{
    char buf[256];
    FILE *fp;

    sprintf(buf, "%s/library/base/R/%s", R_Home, file);
    fp = R_fopen(buf, "r");
    return fp;
}

FILE *R_OpenSysInitFile(void)
{
    char buf[256];
    FILE *fp;

    sprintf(buf, "%s/library/base/R/Rprofile", R_Home);
    fp = R_fopen(buf, "r");
    return fp;
}

FILE *R_OpenSiteFile(void)
{
    char buf[256];
    FILE *fp;

    fp = NULL;
    if (LoadSiteFile) {
	if ((fp = R_fopen(getenv("R_PROFILE"), "r")))
	    return fp;
	if ((fp = R_fopen(getenv("RPROFILE"), "r")))
	    return fp;
	sprintf(buf, "%s/etc/Rprofile", R_Home);
	if ((fp = R_fopen(buf, "r")))
	    return fp;
    }
    return fp;
}

	/* Saving and Restoring the Global Environment */

void R_RestoreGlobalEnv(void)
{
    FILE *fp;
    SEXP img, lst;
    int i;

    if(RestoreAction == SA_RESTORE) {
	if(!(fp = R_fopen(".RData", "rb"))) { /* binary file */
	    /* warning here perhaps */
	    return;
	}
#ifdef OLD
	FRAME(R_GlobalEnv) = R_LoadFromFile(fp, 1);
#else
	PROTECT(img = R_LoadFromFile(fp, 1));
	switch (TYPEOF(img)) {
	case LISTSXP:
	    while (img != R_NilValue) {
		defineVar(TAG(img), CAR(img), R_GlobalEnv);
		img = CDR(img);
	    }
	    break;
	case VECSXP:
	    for (i = 0; i < LENGTH(img); i++) {
		lst = VECTOR(img)[i];
		while (lst != R_NilValue) {
		    defineVar(TAG(lst), CAR(lst), R_GlobalEnv);
		    lst = CDR(lst);
		}
	    }
	    break;
	}
        UNPROTECT(1);
#endif
	if(!R_Quiet)
	    Rprintf("[Previously saved workspace restored]\n\n");
        fclose(fp);
    }
}

void R_SaveGlobalEnv(void)
{
    FILE *fp = R_fopen(".RData", "wb"); /* binary file */
    if (!fp)
	error("can't save data -- unable to open ./.RData");
    if (HASHTAB(R_GlobalEnv) != R_NilValue)
	R_SaveToFile(HASHTAB(R_GlobalEnv), fp, 0, 0);
    else
	R_SaveToFile(FRAME(R_GlobalEnv), fp, 0, 0);
    fclose(fp);
}

/*
 * 5) FILESYSTEM INTERACTION
 */

/*
 * This call provides a simple interface to the "stat" system call.
 */

#include <sys/types.h>
#include <sys/stat.h>

int R_FileExists(char *path)
{
    struct stat sb;
    return stat(R_ExpandFileName(path), &sb) == 0;
}

    /*
     *  Unix file names which begin with "." are invisible.
     */

int R_HiddenFile(char *name)
{
    if (name && name[0] != '.') return 0;
    else return 1;
}


FILE *R_fopen(const char *filename, const char *mode)
{
    return(filename ? fopen(filename, mode) : NULL );
}

/*
 *  6) SYSTEM INFORMATION
 */

          /* The location of the R system files */

char *R_HomeDir()
{
    return getenv("R_HOME");
}

/*
 *  7) PLATFORM DEPENDENT FUNCTIONS
 */

#ifdef Win32
#include <windows.h>
#else
extern char ** environ;
#endif

SEXP do_getenv(SEXP call, SEXP op, SEXP args, SEXP env)
{
    int i, j;
    char *s;
    SEXP ans;

    checkArity(op, args);

    if (!isString(CAR(args)))
	errorcall(call, "wrong type for argument");

    i = LENGTH(CAR(args));
    if (i == 0) {
#ifdef Win32
	char *envir, *e;
	envir = (char *) GetEnvironmentStrings();
	for (i = 0, e = envir; strlen(e) > 0; i++, e += strlen(e)+1);
	PROTECT(ans = allocVector(STRSXP, i));
	for (i = 0, e = envir; strlen(e) > 0; i++, e += strlen(e)+1)
	    STRING(ans)[i] = mkChar(e);
	FreeEnvironmentStrings(envir);
#else
	char **e;
	for (i = 0, e = environ; *e != NULL; i++, e++);
	PROTECT(ans = allocVector(STRSXP, i));
	for (i = 0, e = environ; *e != NULL; i++, e++)
	    STRING(ans)[i] = mkChar(*e);
#endif
    } else {
	PROTECT(ans = allocVector(STRSXP, i));
	for (j = 0; j < i; j++) {
	    s = getenv(CHAR(STRING(CAR(args))[j]));
	    if (s == NULL)
		STRING(ans)[j] = mkChar("");
	    else
		STRING(ans)[j] = mkChar(s);
	}
    }
    UNPROTECT(1);
    return (ans);
}

SEXP do_interactive(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP rval;

    rval=allocVector(LGLSXP, 1);
    if( R_Interactive )
	LOGICAL(rval)[0]=1;
    else
	LOGICAL(rval)[0]=0;
    return rval;
}

/*
 *  INITIALIZATION HELPER CODE
 */

#include "Startup.h"
extern void R_ShowMessage(char *);

void R_DefParams(Rstart Rp)
{
    Rp->R_Quiet = False;
    Rp->R_Slave = False;
    Rp->R_Interactive = True;
    Rp->R_Verbose = False;
    Rp->RestoreAction = SA_RESTORE;
    Rp->SaveAction = SA_SAVEASK;
    Rp->LoadSiteFile = True;
    Rp->LoadInitFile = True;
    Rp->DebugInitFile = False;
    Rp->vsize = R_VSIZE;
    Rp->nsize = R_NSIZE;
#ifdef Win32
    Rp->NoRenviron = False;
#endif
}

#define Max_Nsize 20000000	/* must be < LONG_MAX (= 2^32 - 1 =)
				   2147483647 = 2.1e9 */
#define Max_Vsize (2048*Mega)	/* 2048*Mega = 2^(11+20) must be < LONG_MAX */

#define Min_Nsize 160000
#define Min_Vsize (1*Mega)

void R_SizeFromEnv(Rstart Rp)
{
    int value, ierr;
    char *p;
    if((p = getenv("R_VSIZE"))) {
	value = Decode2Long(p, &ierr);
	if(ierr != 0 || value > Max_Vsize || value < Min_Vsize)
	    R_ShowMessage("WARNING: invalid R_VSIZE ignored\n");
	else
	    Rp->vsize = value;
    }
    if((p = getenv("R_NSIZE"))) {
	value = Decode2Long(p, &ierr);
	if(ierr != 0 || value > Max_Nsize || value < Min_Nsize)
	    R_ShowMessage("WARNING: invalid R_NSIZE ignored\n");
	else
	    Rp->nsize = value;
    }
}

static void SetSize(int vsize, int nsize)
{
    char msg[1024];

    if (vsize < 1000) {
	R_ShowMessage("WARNING: vsize ridiculously low, Megabytes assumed\n");
	vsize *= Mega;
    }
    if(vsize < Min_Vsize || vsize > Max_Vsize) {
	sprintf(msg, "WARNING: invalid v(ector heap)size `%d' ignored\n"
		 "using default = %gM\n", vsize, R_VSIZE / Mega);
	R_ShowMessage(msg);
	R_VSize = R_VSIZE;
    } else
	R_VSize = vsize;
    if(nsize < Min_Nsize || nsize > Max_Nsize) {
	sprintf(msg, "WARNING: invalid language heap (n)size `%d' ignored,"
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
    SetSize(Rp->vsize, Rp-> nsize);
    CommandLineArgs = Rp->CommandLineArgs;
    NumCommandLineArgs = Rp->NumCommandLineArgs;
#ifdef Win32
    R_SetWin32(Rp);
#endif
}


/* Remove and process common command-line arguments */

/* FIXME: not used?
static void R_common_badargs() {
    R_ShowMessage("invalid argument passed to R\n");
    exit(1);
}
*/

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

  for(i=0;i < argc; i++) {
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
    STRING(vals)[i] = mkChar(CommandLineArgs[i]);
  }

 return(vals);
}

void
R_common_command_line(int *pac, char **argv, Rstart Rp)
{
    int ac = *pac, newac = 1;	/* argv[0] is process name */
    int ierr;
    long value;
    char *p, **av = argv, msg[1024];

    while(--ac) {
	if(**++av == '-') {
	    if (!strcmp(*av, "--version")) {
		PrintVersion(msg);
		R_ShowMessage(msg);
		exit(0);
	    }
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
	    }
	    else if (!strcmp(*av, "--silent") ||
		     !strcmp(*av, "--quiet") ||
		     !strcmp(*av, "-q")) {
		Rp->R_Quiet = True;
	    }
	    else if (!strcmp(*av, "--vanilla")) {
		Rp->SaveAction = SA_NOSAVE; /* --no-save */
		Rp->RestoreAction = SA_NORESTORE; /* --no-restore */
		Rp->LoadSiteFile = False; /* --no-site-file */
		Rp->LoadInitFile = False; /* --no-init-file */
	    }
	    else if (!strcmp(*av, "--verbose")) {
		Rp->R_Verbose = True;
	    }
	    else if (!strcmp(*av, "--slave") ||
		     !strcmp(*av, "-s")) {
		Rp->R_Quiet = True;
		Rp->R_Slave = True;
		Rp->SaveAction = SA_NOSAVE;
	    }
	    else if (!strcmp(*av, "--no-site-file")) {
		Rp->LoadSiteFile = False;
	    }
	    else if (!strcmp(*av, "--no-init-file")) {
		Rp->LoadInitFile = False;
	    }
	    else if (!strcmp(*av, "--debug-init")) {
	        Rp->DebugInitFile = True;
	    }
	    else if (!strcmp(*av, "-save") ||
		     !strcmp(*av, "-nosave") ||
		     !strcmp(*av, "-restore") ||
		     !strcmp(*av, "-norestore") ||
		     !strcmp(*av, "-noreadline") ||
		     !strcmp(*av, "-quiet") ||
		     !strcmp(*av, "-V")) {
		sprintf(msg, "WARNING: option %s no longer supported\n", *av);
		R_ShowMessage(msg);
	    }
	    else if((*av)[1] == 'v') {
		R_ShowMessage("ERROR: option `-v' is defunct.  Use `--vsize' instead.\n");
		exit(1);
	    }
	    else if(strncmp(*av, "--vsize", 7) == 0) {
		if(strlen(*av) < 9) {
		    R_ShowMessage("WARNING: use `--vsize=V' rather than `--vsize V'.\n");
		    ac--; av++; p = *av;
		}
		else
		    p = &(*av)[8];
		if (p == NULL) {
		    R_ShowMessage("WARNING: no vsize given\n");
		    break;
		}
		value = Decode2Long(p, &ierr);
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
	    else if((*av)[1] == 'n') {
		R_ShowMessage("ERROR: option `-n' is defunct.  Use `--nsize' instead.\n");
		exit(1);
	    }
	    else if(strncmp(*av, "--nsize", 7) == 0) {
		if(strlen(*av) < 9) {
		    R_ShowMessage("WARNING: use `--nsize=V' rather than `--nsize V'.\n");
		    ac--; av++; p = *av;
		}
		else
		    p = &(*av)[8];		
		if (p == NULL) {
		    R_ShowMessage("WARNING: no nsize given\n");
		    break;
		}
		value = Decode2Long(p, &ierr);
		if(ierr) {
		    if(ierr < 0) /* R_common_badargs(); */
			sprintf(msg, "WARNING: --nsize value is invalid: ignored\n");
		    else
		    sprintf(msg, "WARNING: --nsize=%ld`%c': too large and ignored\n",
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
