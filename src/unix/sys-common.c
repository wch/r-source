/*
  R : A Computer Language for Statistical Data Analysis
  Copyright (C) 1995-1996   Robert Gentleman and Ross Ihaka
  Copyright (C) 1997-2004   Robert Gentleman, Ross Ihaka
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

    snprintf(buf, 256, "%s/library/base/R/%s", R_Home, file);
    fp = R_fopen(buf, "r");
    return fp;
}

char *R_LibraryFileName(char *file, char *buf, size_t bsize)
{
    if (snprintf(buf, bsize, "%s/library/base/R/%s", R_Home, file) < 0)
	error("R_LibraryFileName: buffer too small");
    return buf;
}     
     
FILE *R_OpenSysInitFile(void)
{
    char buf[256];
    FILE *fp;

    snprintf(buf, 256, "%s/library/base/R/Rprofile", R_Home);
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
	snprintf(buf, 256, "%s/etc/Rprofile.site", R_Home);
	if ((fp = R_fopen(buf, "r")))
	    return fp;
	snprintf(buf, 256, "%s/etc/Rprofile", R_Home);
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
 *  7) PLATFORM DEPENDENT FUNCTIONS
 */

/*
 *  INITIALIZATION HELPER CODE
 */

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
    Rp->ppsize = R_PPSSIZE;
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
    R_SetPPSize(Rp->ppsize);
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
    long lval;
    R_size_t value;
    char *p, **av = argv, msg[1024];
    Rboolean processing = TRUE;

    R_RestoreHistory = 1;
    while(--ac) {
	if(processing && **++av == '-') {
	    if (!strcmp(*av, "--version")) {
		PrintVersion(msg);
		R_ShowMessage(msg);
		exit(0);
	    }
	    else if(!strcmp(*av, "--args")) {
		/* copy this through for further processing */
		argv[newac++] = *av;
		processing = FALSE;
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
		     !strcmp(*av, "-nsize") ||
		     !strcmp(*av, "-vsize") ||
		     !strcmp(*av, "-V") ||
		     !strcmp(*av, "-n") ||
		     !strcmp(*av, "-v")) {
		snprintf(msg, 1024, 
			 "WARNING: option %s no longer supported\n", *av);
		R_ShowMessage(msg);
	    }
            /* mop up --max/min/-n/vsize */
 	    else if(strncmp(*av+7, "size", 4) == 0) {
		if(strlen(*av) < 13) {
		    ac--; av++; p = *av;
		}
		else p = &(*av)[12];
		if (p == NULL) {
		    snprintf(msg, 1024,
			     "WARNING: no value given for %s\n", *av);
		    R_ShowMessage(msg);
		    break;
		}
		value = R_Decode2Long(p, &ierr);
		if(ierr) {
		    if(ierr < 0)
			snprintf(msg, 1024, 
				 "WARNING: %s value is invalid: ignored\n",
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
	    else if(strncmp(*av, "--max-ppsize", 12) == 0) {
		if(strlen(*av) < 14) {
		    ac--; av++; p = *av;
		} else p = &(*av)[13];
		if (p == NULL) {
		    R_ShowMessage("WARNING: no value given for -max-ppsize given\n");
		    break;
		}
		lval = strtol(p, &p, 10);
		if (lval < 0)
		    R_ShowMessage("WARNING: -max-ppsize value is negative: ignored\n");
		else if (lval < 10000)
		    R_ShowMessage("WARNING: -max-ppsize value is too small: ignored\n");

		else if (lval > 100000)
		    R_ShowMessage("WARNING: -max-ppsize value is too large: ignored\n");
		else Rp->ppsize = lval;
	    }
#if 0
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
#endif
	    else { /* unknown -option */
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

