/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997-1999   Robert Gentleman, Ross Ihaka
 *                            and the R Development Core Team
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
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

         /* See ../unix/system.txt for a description of functions */

#ifdef HAVE_CONFIG_H
#include <Rconfig.h>
#endif

#include "Defn.h"
#include "Fileio.h"

extern int SaveAction;
extern int RestoreAction;
extern int LoadSiteFile;
extern int LoadInitFile;
extern int DebugInitFile;


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
	error("can't save data -- unable to open ./.RData\n");
    if (HASHTAB(R_GlobalEnv) != R_NilValue)
	R_SaveToFile(HASHTAB(R_GlobalEnv), fp, 0);
    else
	R_SaveToFile(FRAME(R_GlobalEnv), fp, 0);
    fclose(fp);
}

/*
 *  5) FILESYSTEM INTERACTION
 */

    /*
     *  This call provides a simple interface to the "stat" system call.
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
    return( fopen(filename, mode) );
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
#define Max_Vsize (2048*Mega)	/* must be < LONG_MAX */

#define Min_Nsize 200000
#define Min_Vsize (2*Mega)

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

void R_common_command_line(int *pac, char **argv, Rstart Rp)
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
	    else if((value = (*av)[1] == 'v') || !strcmp(*av, "--vsize")) {
		if(value)
		    R_ShowMessage("WARNING: option `-v' is deprecated.  Use `--vsize' instead.\n");
		if(!value || (*av)[2] == '\0') {
		    ac--; av++; p = *av;
		}
		else p = &(*av)[2];
		if (p == NULL) {
		    R_ShowMessage("WARNING: no vsize given\n");
		    break;
		}
		value = Decode2Long(p, &ierr);
		if(ierr) {
		    if(ierr < 0) /* R_common_badargs(); */
			sprintf(msg, "WARNING: --vsize value is invalid: ignored\n");
		    else
			sprintf(msg, "WARNING: --vsize %ld`%c': too large and ignored\n", 
				value,
				(ierr == 1) ? 'M': ((ierr == 2) ? 'K' : 'k'));
		    R_ShowMessage(msg);

		} else
		    Rp->vsize = value;
	    }
	    else if((value = (*av)[1] == 'n') || !strcmp(*av, "--nsize")) {
		if(value)
		    R_ShowMessage("WARNING: option `-n' is deprecated.  "
			     "Use `--nsize' instead.\n");
		if(!value || (*av)[2] == '\0') {
		    ac--; av++; p = *av;
		}
		else p = &(*av)[2];
		if (p == NULL) {
		    R_ShowMessage("WARNING: no nsize given\n");
		    break;
		}
		value = Decode2Long(p, &ierr);
		if(ierr) {
		    if(ierr < 0) /* R_common_badargs(); */
			sprintf(msg, "WARNING: --nsize value is invalid: ignored\n");
		    else
		    sprintf(msg, "WARNING: --nsize %ld`%c': too large and ignored\n", 
			    value,
			    (ierr == 1) ? 'M': ((ierr == 2) ? 'K':'k'));
		    R_ShowMessage(msg);
		} else
		    Rp->nsize = value;
	    }
	    else {
		argv[newac++] = *av;
		break;
	    }
	}
	else {
	    argv[newac++] = *av;
	}
    }
    *pac = newac;
    return;
}
