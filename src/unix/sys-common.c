/*
  R : A Computer Language for Statistical Data Analysis
  Copyright (C) 1995-1996   Robert Gentleman and Ross Ihaka
  Copyright (C) 1997-2002   Robert Gentleman, Ross Ihaka
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
#include "Startup.h"

#include <string.h>

extern SA_TYPE	SaveAction;
extern SA_TYPE	RestoreAction;
extern Rboolean LoadSiteFile;
extern Rboolean LoadInitFile;
extern Rboolean DebugInitFile;

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
	sprintf(buf, "%s/etc/Rprofile.site", R_Home);
	if ((fp = R_fopen(buf, "r")))
	    return fp;
	sprintf(buf, "%s/etc/Rprofile", R_Home);
	if ((fp = R_fopen(buf, "r")))
	    return fp;
    }
    return fp;
}

	/* Saving and Restoring the Global Environment */

static char workspace_name[100] = ".RData";

void set_workspace_name(char *fn)
{
    strcpy(workspace_name, fn);
}

void R_RestoreGlobalEnv(void)
{
    if(RestoreAction == SA_RESTORE) {
	R_RestoreGlobalEnvFromFile(workspace_name, R_Quiet);
    }
}

void R_SaveGlobalEnv(void)
{
    R_SaveGlobalEnvToFile(".RData");
}

/*
 * 5) FILESYSTEM INTERACTION
 */

/*
 * This call provides a simple interface to the "stat" system call.
 */

#ifdef HAVE_STAT
#include <sys/types.h>
#ifdef HAVE_SYS_STAT_H
# include <sys/stat.h>
#endif

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

    /*
     *  Unix file names which begin with "." are invisible.
     */

Rboolean R_HiddenFile(char *name)
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
# include <windows.h>
#elif defined(__APPLE__)
# include <crt_externs.h>
# define environ (*_NSGetEnviron())
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
	    SET_STRING_ELT(ans, i, mkChar(e));
	FreeEnvironmentStrings(envir);
#else
	char **e;
	for (i = 0, e = environ; *e != NULL; i++, e++);
	PROTECT(ans = allocVector(STRSXP, i));
	for (i = 0, e = environ; *e != NULL; i++, e++)
	    SET_STRING_ELT(ans, i, mkChar(*e));
#endif
    } else {
	PROTECT(ans = allocVector(STRSXP, i));
	for (j = 0; j < i; j++) {
	    s = getenv(CHAR(STRING_ELT(CAR(args), j)));
	    if (s == NULL)
		SET_STRING_ELT(ans, j, mkChar(""));
	    else
		SET_STRING_ELT(ans, j, mkChar(s));
	}
    }
    UNPROTECT(1);
    return (ans);
}

#ifdef HAVE_PUTENV
static int Rputenv(char *str)
{
    char *buf;
    buf = (char *) malloc((strlen(str) + 1) * sizeof(char));
    if(!buf) return 1;
    strcpy(buf, str);
    putenv(buf);
    /* no free here: storage remains in use */
    return 0;
}
#endif


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
	LOGICAL(ans)[i] = Rputenv(CHAR(STRING_ELT(vars, i))) == 0;
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


extern void R_ShowMessage(char *);

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
    Rp->max_vsize = INT_MAX;
    Rp->max_nsize = INT_MAX;
    Rp->NoRenviron = FALSE;
}

#define Max_Nsize 50000000	/* must be < LONG_MAX (= 2^32 - 1 =)
				   2147483647 = 2.1e9 */
                                /* limit was 2e7, changed to 5e7, which gives
                                   nearly 2Gb of cons cells */
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

    /* vsize >0 to catch long->int overflow */
    if (vsize < 1000 && vsize > 0) {
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
    SetSize(Rp->vsize, Rp->nsize);
    R_SetMaxNSize(Rp->max_nsize);
    R_SetMaxVSize(Rp->max_vsize);
    CommandLineArgs = Rp->CommandLineArgs;
    NumCommandLineArgs = Rp->NumCommandLineArgs;
#ifdef Win32
    R_SetWin32(Rp);
#endif
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
    long value;
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
		value = Decode2Long(p, &ierr);
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
    char *buf, *value, *p, *q, quote='\0';
    int inquote = 0;

    buf = (char *) malloc((strlen(a) + strlen(b) + 2) * sizeof(char));
    if(!buf) R_Suicide("allocation failure in reading Renviron");
    strcpy(buf, a); strcat(buf, "="); 
    value = buf+strlen(buf);

    /* now process the value */
    for(p = b, q = value; *p; p++) {
	/* remove quotes around sections, preserve \ inside quotes */
	if(!inquote && (*p == '"' || *p == '\'')) {
	    inquote = 1;
	    quote = *p;
	    continue;
	}
	if(inquote && *p == quote && *(p-1) != '\\') {
	    inquote = 0;
	    continue;
	}
	if(!inquote && *p == '\\') {
	    if(*(p+1) == '\n') p++;
	    else if(*(p+1) == '\\') *q++ = *p;
	    continue;
	}
	if(inquote && *p == '\\' && *(p+1) == quote) continue;
	*q++ = *p;
    }
    *q = '\0';
    putenv(buf);
    /* no free here: storage remains in use */
}


#define BUF_SIZE 255
#define MSG_SIZE 2000
static int process_Renviron(char *filename)
{
    FILE *fp;
    char *s, *p, sm[BUF_SIZE], *lhs, *rhs, msg[MSG_SIZE+50];
    int errs = 0;

    if (!filename || !(fp = fopen(filename, "r"))) return 0;
    sprintf(msg, "\n   File %s contains invalid line(s)", filename);

    while(fgets(sm, BUF_SIZE, fp)) {
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


/* try system Renviron: R_HOME/etc/Renviron.  Unix only. */
void process_system_Renviron()
{
    char buf[PATH_MAX];
    
    if(strlen(R_Home) + strlen("/etc/Renviron") > PATH_MAX - 1) {
	R_ShowMessage("path to system Renviron is too long: skipping");
	return;
    }
    strcpy(buf, R_Home);
    strcat(buf, "/etc/Renviron");
    if(!process_Renviron(buf))
	R_ShowMessage("cannot find system Renviron");
}

/* try site Renviron: R_ENVIRON, then R_HOME/etc/Renviron.site. */
void process_site_Renviron ()
{
    char buf[PATH_MAX];

    if(process_Renviron(getenv("R_ENVIRON"))) return;
    if(strlen(R_Home) + strlen("/etc/Renviron.site") > PATH_MAX - 1) {
	R_ShowMessage("path to Renviron.site is too long: skipping");
	return;
    }
    sprintf(buf, "%s/etc/Renviron.site", R_Home);
    process_Renviron(buf);
}

/* try user Renviron: ./.Renviron, then ~/.Renviron */
void process_user_Renviron()
{
    char *s;
    
    if(process_Renviron(".Renviron")) return;
#ifdef Unix
    s = R_ExpandFileName("~/.Renviron");
#endif
#ifdef Win32
    {
	char buf[1024];
	/* R_USER is not necessarily set yet, so we have to work harder */
	s = getenv("R_USER");
	if(!s) s = getenv("HOME");
	if(!s) return;
	sprintf(buf, "%s/.Renviron", s);
	s = buf;
    }
#endif
    process_Renviron(s);
}
