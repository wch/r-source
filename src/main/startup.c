/*
  R : A Computer Language for Statistical Data Analysis
  Copyright (C) 1995-1996   Robert Gentleman and Ross Ihaka
  Copyright (C) 1997-2025   The R Core Team

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at
  your option) any later version.

  This program is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, a copy is available at
  https://www.R-project.org/Licenses/
*/

/*
  See ../unix/system.txt for a description of some of these functions
*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h>
#include "Fileio.h" /* for R_fopen */
#include "Startup.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef __APPLE__
# include <sys/types.h>
# include <sys/sysctl.h>
#endif

/* These are used in ../gnuwin32/system.c, ../unix/sys-std.c */
SA_TYPE SaveAction = SA_SAVEASK;
SA_TYPE	RestoreAction = SA_RESTORE;
attribute_hidden bool LoadInitFile = true;  /* Used in R_OpenInitFile */

static bool LoadSiteFile = true;
// static bool DebugInitFile = false; // unused

/*
 *  INITIALIZATION AND TERMINATION ACTIONS
 */

attribute_hidden void R_InitialData(void)
{
    R_RestoreGlobalEnv();
}


attribute_hidden
FILE *R_OpenLibraryFile(const char *file)
{
    char *buf = NULL;
    FILE *fp = NULL;

    Rasprintf_malloc(&buf, "%s/library/base/R/%s", R_Home, file);
    if (buf) {
	fp = R_fopen(buf, "r");
	free(buf);
    }
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
    char *buf = NULL;
    FILE *fp = NULL;

    Rasprintf_malloc(&buf, "%s/library/base/R/Rprofile", R_Home);
    if (buf) {
	fp = R_fopen(buf, "r");
	free(buf);
    }
    return fp;
}

attribute_hidden
FILE *R_OpenSiteFile(void)
{
    char *buf = NULL;
    FILE *fp = NULL;

    fp = NULL;
    if (LoadSiteFile) {
	char *p = getenv("R_PROFILE");
	if (p) {
	    if (*p) return R_fopen(R_ExpandFileName(p), "r");
	    else return NULL;
	}
#ifdef R_ARCH
	Rasprintf_malloc(&buf, "%s/etc/%s/Rprofile.site", R_Home, R_ARCH);
	if (buf) {
	    fp = R_fopen(buf, "r");
	    free(buf);
	    if (fp) return fp;
	    buf = NULL;
	}
#endif
	Rasprintf_malloc(&buf, "%s/etc/Rprofile.site", R_Home);
	if (buf) {
	    fp = R_fopen(buf, "r");
	    free(buf);
	}
    }
    return fp;
}

	/* Saving and Restoring the Global Environment */

#ifndef Win32
static char workspace_name[1000] = ".RData";

/*
  set_workspace_name is in src/gnuwin32/system.c and used to implement
  drag-and-drop on Windows.
 */
#else
static char *workspace_name = ".RData";

attribute_hidden
Rboolean set_workspace_name(const char *fn)
{
    static bool previously_allocated = FALSE;
    size_t needed = strlen(fn) + 1;
    char *new_wsn = (char *)malloc(needed);

    if (!new_wsn)
	return FALSE;
    if (previously_allocated)
	free(workspace_name);
    previously_allocated = true;
    strncpy(new_wsn, fn, needed);
    workspace_name = new_wsn;
    return TRUE;
}
#endif

attribute_hidden
const char* get_workspace_name(void)
{
    return workspace_name;
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
 *  INITIALIZATION HELPER CODE
 */

/* RstartVersion is the version of the passed Rstart structure to fill in.
   Returns 0 on success, a negative number if the version is too old,
   a positive number if it is too new. */
int R_DefParamsEx(Rstart Rp, int RstartVersion)
{
    Rp->RstartVersion = RstartVersion;
    if (RstartVersion < 0) return -1;
    if (RstartVersion > 1) return 1;
    
    Rp->R_Quiet = FALSE;
    Rp->R_NoEcho = FALSE;
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
    Rp->nconnections = 128;
    Rp->NoRenviron = FALSE;
    R_SizeFromEnv(Rp);

    if (RstartVersion > 0) {
#ifdef Win32
	Rp->EmitEmbeddedUTF8 = FALSE;
	R_DefCallbacks(Rp, RstartVersion);
#endif
    }
    return 0;
}

void R_DefParams(Rstart Rp)
{
    R_DefParamsEx(Rp, 0); /* version 0 for now supported */
}

#define Max_Nsize 50000000	/* about 1.4Gb 32-bit, 2.8Gb 64-bit */
#define Max_Vsize R_SIZE_T_MAX	/* unlimited */

// small values ok for R_DEFAULT_PACKAGES=NULL (= 'base' only)
#define Min_Nsize 50000
#define Min_Vsize 262144 // = (Mega/4)

void R_SizeFromEnv(Rstart Rp)
{
    int ierr;
    R_size_t value;
    char *p, msg[256];

    if ((p = getenv("R_MAX_VSIZE"))) {
	value = R_Decode2Long(p, &ierr);
	if(ierr != 0 || value > Max_Vsize)
	    R_ShowMessage("WARNING: invalid R_MAX_VSIZE ignored\n");
	else if(value < Min_Vsize) {
	    snprintf(msg, 256,
		     "WARNING: R_MAX_VSIZE smaller than Min_Vsize = %lu is ignored\n",
		     (unsigned long) Min_Vsize);
	    R_ShowMessage(msg);
	}
	else
	    Rp->max_vsize = value;
    }
#if defined(__APPLE__) && defined(_SC_PHYS_PAGES) && defined(_SC_PAGE_SIZE) \
    && (SIZEOF_SIZE_T > 4)
    /* For now only on macOS place a default limit on the vector heap
       size to avoid having R killed due to memory overcommit.
       Setting the limit at the maximum of 16Gb and available physical
       memory seems reasonable, but there may be better options. LT */
    else {
	R_size_t pages = sysconf(_SC_PHYS_PAGES);
	R_size_t page_size = sysconf(_SC_PAGE_SIZE);
	R_size_t sysmem = pages * page_size;
	R_size_t MinMaxVSize = 17179869184; /* 16 Gb */
	Rp->max_vsize = sysmem > MinMaxVSize ? sysmem : MinMaxVSize;
    }
#elif defined(__APPLE__) && (SIZEOF_SIZE_T > 4)
    else {
	R_size_t sysmem = 0;
	R_size_t len = sizeof(sysmem);
	if (!sysctlbyname("hw.memsize", &sysmem, &len, NULL, 0)
	    && len == sizeof(sysmem)) {

	    R_size_t MinMaxVSize = 17179869184; /* 16 Gb */
	    Rp->max_vsize = sysmem > MinMaxVSize ? sysmem : MinMaxVSize;
	}
    }
#endif
    if((p = getenv("R_VSIZE"))) {
	value = R_Decode2Long(p, &ierr);
	if(ierr != 0 || value > Max_Vsize)
	    R_ShowMessage("WARNING: invalid R_VSIZE ignored\n");
	else if(value < Min_Vsize) {
	    snprintf(msg, 256,
		     "WARNING: R_VSIZE smaller than Min_Vsize = %lu is ignored\n",
		     (unsigned long) Min_Vsize);
	    R_ShowMessage(msg);
	}
	else
	    Rp->vsize = value;
    }
    if((p = getenv("R_NSIZE"))) {
	value = R_Decode2Long(p, &ierr);
	if(ierr != 0 || value > Max_Nsize)
	    R_ShowMessage("WARNING: invalid R_NSIZE ignored\n");
	else if(value < Min_Nsize) {
	    snprintf(msg, 256,
		     "WARNING: R_NSIZE smaller than Min_Nsize = %lu is ignored\n",
		     (unsigned long) Min_Nsize);
	    R_ShowMessage(msg);
	}
	else
	    Rp->nsize = value;
    }
}

static void SetSize(R_size_t vsize, R_size_t nsize)
{
    char msg[1024];
    bool sml;
    /* vsize > 0 to catch long->int overflow */
    if (vsize < 1000 && vsize > 0) {
	R_ShowMessage("WARNING: vsize ridiculously low, Megabytes assumed\n");
	vsize *= (R_size_t) Mega;
    }
    if((sml = vsize < Min_Vsize) || vsize > Max_Vsize) {
	snprintf(msg, 1024,
		 "WARNING: %s v(ector heap)size '%lu' ignored,"
		 " using default = %gM\n",
		 sml ? "too small" : "too large",
		 (unsigned long) vsize, R_VSIZE / Mega);
	R_ShowMessage(msg);
	R_VSize = R_VSIZE;
    } else
	R_VSize = vsize;
    if((sml = nsize < Min_Nsize) || nsize > Max_Nsize) {
	snprintf(msg, 1024,
		 "WARNING: %s language heap (n)size '%lu' ignored,"
		 " using default = %ld\n",
		 sml ? "too small" : "too large", (unsigned long) nsize, R_NSIZE);
	R_ShowMessage(msg);
	R_NSize = R_NSIZE;
    } else
	R_NSize = nsize;
}

static void SetMaxSize(R_size_t vsize, R_size_t nsize)
{
    char msg[1024];

    if (!R_SetMaxVSize(vsize)) {
	/* vsfac is still 1 */
	snprintf(msg, 1024,
		 "WARNING: too small maximum for v(ector heap)size '%lu' ignored,"
		 " the current usage %gM is already larger\n",
		 (unsigned long) vsize, R_VSize / Mega);
	R_ShowMessage(msg);
    }

    if (!R_SetMaxNSize(nsize)) {
	snprintf(msg, 1024,
		 "WARNING: too small maximum for language heap (n)size '%lu' ignored,"
		 " the current usage '%lu' is already larger\n",
		 (unsigned long) nsize, (unsigned long) R_NSize);
	R_ShowMessage(msg);
    }
}

static bool checkBool(int in, const char *name)
{
    if(in != 0 && in != 1) {
	warning("At startup: value %d of Rp->%s taken as true", in, name);
	in = 1;
    }
    return (bool)(in != 0);
}

void R_SetParams(Rstart Rp)
{
    R_Quiet = checkBool(Rp->R_Quiet, "R_Quiet");
    R_NoEcho = (Rboolean) checkBool(Rp->R_NoEcho, "R_NoEcho");
    R_Interactive = (Rboolean)  checkBool(Rp->R_Interactive, "R_Interactive");
    R_Verbose = checkBool(Rp->R_Verbose, "R_Verbose");
    LoadSiteFile = checkBool(Rp->LoadSiteFile, "R_LoadSitefile");
    LoadInitFile = checkBool(Rp->LoadInitFile, "R_LoadInitFile");
//    DebugInitFile = checkBool(Rp->DebugInitFile, "R_DebugInitFile"); // unused
    RestoreAction = Rp->RestoreAction;
    SaveAction = Rp->SaveAction;
    SetSize(Rp->vsize, Rp->nsize);
    SetMaxSize(Rp->max_vsize, Rp->max_nsize);
    R_SetPPSize(Rp->ppsize);
    R_SetNconn(Rp->nconnections);
#ifdef Win32
    R_SetWin32(Rp);
#endif
}
