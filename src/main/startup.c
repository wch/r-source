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
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
  USA.
 */

/* <UTF8> char here is handled as a whole string */

/*
  See ../unix/system.txt for a description of some of these functions
  Formally part of ../unix/sys-common.c
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "Defn.h"
#include "Fileio.h" /* for R_fopen */
#include "Startup.h"

/* These are used in ../gnuwin32/system.c, ../unix/sys-std.c */
SA_TYPE SaveAction = SA_SAVEASK;
SA_TYPE	RestoreAction = SA_RESTORE;
static Rboolean LoadSiteFile = TRUE;
attribute_hidden Rboolean LoadInitFile = TRUE;  /* Used in R_OpenInitFile */
static Rboolean DebugInitFile = FALSE;

/*
 *  INITIALIZATION AND TERMINATION ACTIONS
 */

void attribute_hidden R_InitialData(void)
{
    R_RestoreGlobalEnv();
}


attribute_hidden
FILE *R_OpenLibraryFile(const char *file)
{
    char buf[256];
    FILE *fp;

    snprintf(buf, 256, "%s/library/base/R/%s", R_Home, file);
    fp = R_fopen(buf, "r");
    return fp;
}

attribute_hidden
char *R_LibraryFileName(const char *file, char *buf, size_t bsize)
{
    if (snprintf(buf, bsize, "%s/library/base/R/%s", R_Home, file) < 0)
	error(_("R_LibraryFileName: buffer too small"));
    return buf;
}     
     
attribute_hidden
FILE *R_OpenSysInitFile(void)
{
    char buf[256];
    FILE *fp;

    snprintf(buf, 256, "%s/library/base/R/Rprofile", R_Home);
    fp = R_fopen(buf, "r");
    return fp;
}

attribute_hidden
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
	/* snprintf(buf, 256, "%s/etc/Rprofile", R_Home);
	if ((fp = R_fopen(buf, "r")))
	return fp; */
    }
    return fp;
}

	/* Saving and Restoring the Global Environment */

static char workspace_name[100] = ".RData";

#ifdef Win32
void set_workspace_name(char *fn)
{
    strcpy(workspace_name, fn);
}
#endif

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
		 "using default = %gM\n", (unsigned long) vsize, 
		R_VSIZE / Mega);
	R_ShowMessage(msg);
	R_VSize = R_VSIZE;
    } else
	R_VSize = vsize;
    if(nsize < Min_Nsize || nsize > Max_Nsize) {
	sprintf(msg, "WARNING: invalid language heap (n)size `%lu' ignored,"
		 " using default = %ld\n", (unsigned long) nsize, R_NSIZE);
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
#ifdef Win32
    R_SetWin32(Rp);
#endif
}

