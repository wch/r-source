/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2012  The R Core Team
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

/* <UTF8> char here is handled as a whole string */


/* See system.txt for a description of functions
 */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#define R_USE_SIGNALS 1
#include <Defn.h>

#include <locale.h>

/* necessary for some (older, i.e., ~ <= 1997) Linuxen, and apparently
   also some AIX systems.  NB, included unconditionally later on.
   */
#ifndef FD_SET
# ifdef HAVE_SYS_TIME_H
#  include <sys/time.h>
# endif
#endif

#ifdef HAVE_UNISTD_H
# include <unistd.h>		/* isatty() */
#endif

#include <errno.h>

#include "Fileio.h"

// This creates the interface pointers in this file
#define __SYSTEM__
#define R_INTERFACE_PTRS 1
#include <Rinterface.h>
#undef __SYSTEM__

#include "Runix.h"

attribute_hidden FILE *ifp = NULL; /* used in sys-std.c */

attribute_hidden
Rboolean UsingReadline = TRUE;  /* used in sys-std.c & ../main/platform.c
				   and also in sys-unix.c for tilde expansion */

/* call pointers to allow interface switching */

void R_Suicide(const char *s) { ptr_R_Suicide(s); }
void R_ShowMessage(const char *s) { ptr_R_ShowMessage(s); }
int R_ReadConsole(const char *prompt, unsigned char *buf, int len, int addtohistory)
{ return ptr_R_ReadConsole(prompt, buf, len, addtohistory); }
void R_WriteConsole(const char *buf, int len) {if (ptr_R_WriteConsole) ptr_R_WriteConsole(buf, len); else ptr_R_WriteConsoleEx(buf, len, 0); }
void R_WriteConsoleEx(const char *buf, int len, int otype) {if (ptr_R_WriteConsole) ptr_R_WriteConsole(buf, len); else ptr_R_WriteConsoleEx(buf, len, otype); }
void R_ResetConsole(void) { ptr_R_ResetConsole(); }
#ifndef HAVE_AQUA
void R_FlushConsole(void) { ptr_R_FlushConsole(); }
#endif
void R_ClearerrConsole(void) { ptr_R_ClearerrConsole(); }
void R_Busy(int which) { ptr_R_Busy(which); }
void R_CleanUp(SA_TYPE saveact, int status, int runLast)
{ ptr_R_CleanUp(saveact, status, runLast); }

attribute_hidden
int R_ShowFiles(int nfile, const char **file, const char **headers,
		const char *wtitle, Rboolean del, const char *pager)
{ return ptr_R_ShowFiles(nfile, file, headers, wtitle, del, pager); }

attribute_hidden
int R_ChooseFile(int _new,  char *buf, int len)
{ return ptr_R_ChooseFile(_new, buf, len); }


void R_setStartTime(void); /* in sys-unix.c */


#ifdef HAVE_AQUA
/*  used here and in main/sysutils.c (for system). */
Rboolean useaqua = FALSE;

// This should have been fixed a long time ago ....
// Finally in Sep 2012 R.app sets ptr_R_FlushConsole
#include <R_ext/Rdynload.h>
DL_FUNC ptr_do_flushconsole;
void R_FlushConsole(void) {
    if (ptr_R_FlushConsole) ptr_R_FlushConsole(); 
    else if (ptr_do_flushconsole) ptr_do_flushconsole();
}
#endif


void R_setupHistory()
{
    int value, ierr;
    char *p;

    if ((R_HistoryFile = getenv("R_HISTFILE")) == NULL)
	R_HistoryFile = ".Rhistory";
    R_HistorySize = 512;
    if ((p = getenv("R_HISTSIZE"))) {
	value = (int) R_Decode2Long(p, &ierr);
	if (ierr != 0 || value < 0)
	    R_ShowMessage("WARNING: invalid R_HISTSIZE ignored;");
	else
	    R_HistorySize = value;
    }
}

#if defined(HAVE_SYS_RESOURCE_H) && defined(HAVE_GETRLIMIT)
/* on MacOS X it seems sys/resource.h needs sys/time.h first */
# ifdef HAVE_SYS_TIME_H
#  include <sys/time.h>
# endif
# include <sys/resource.h>
# ifdef HAVE_LIBC_STACK_END
extern void * __libc_stack_end;
# endif
# ifdef HAVE_KERN_USRSTACK
#  include <unistd.h>
#  include <sys/types.h>
#  include <sys/sysctl.h>
# endif
#endif

int R_running_as_main_program = 0;

extern void BindDomain(char *R_Home);

/* In src/main/main.c, to avoid inlining */
extern uintptr_t dummy_ii(void);

/* Protection against embedded misuse, PR#15420 */
static int num_initialized = 0;

int Rf_initialize_R(int ac, char **av)
{
    int i, ioff = 1, j;
    Rboolean useX11 = TRUE, useTk = FALSE;
    char *p, msg[1024], cmdlines[10000], **avv;
    structRstart rstart;
    Rstart Rp = &rstart;
    Rboolean force_interactive = FALSE;

    if (num_initialized++) {
	fprintf(stderr, "%s", "R is already initialized\n");
	exit(1);
    }


#if defined(HAVE_SYS_RESOURCE_H) && defined(HAVE_GETRLIMIT)
{
    struct rlimit rlim;

    {
	uintptr_t ii = dummy_ii();
	/* 1 is downwards */
	
	R_CStackDir = ((uintptr_t)&i > ii) ? 1 : -1;
    }

    if(getrlimit(RLIMIT_STACK, &rlim) == 0) {
	unsigned long lim1, lim2;
	lim1 = (unsigned long) rlim.rlim_cur;
	lim2 = (unsigned long) rlim.rlim_max; /* Usually unlimited */
	R_CStackLimit = lim1 < lim2 ? lim1 : lim2;
    }
#if defined(HAVE_LIBC_STACK_END)
    R_CStackStart = (uintptr_t) __libc_stack_end;
#elif defined(HAVE_KERN_USRSTACK)
    {
	/* Borrowed from mzscheme/gc/os_dep.c */
	int nm[2] = {CTL_KERN, KERN_USRSTACK};
	void * base;
	size_t len = sizeof(void *);
	(void) sysctl(nm, 2, &base, &len, NULL, 0);
	R_CStackStart = (uintptr_t) base;
    }
#else
    if(R_running_as_main_program) {
	/* This is not the main program, but unless embedded it is
	   near the top, 5540 bytes away when checked. */
	R_CStackStart = (uintptr_t) &i + (6000 * R_CStackDir);
    }
#endif
    if(R_CStackStart == -1) R_CStackLimit = -1; /* never set */

    /* printf("stack limit %ld, start %lx dir %d \n", R_CStackLimit,
	      R_CStackStart, R_CStackDir); */
}
#endif

    ptr_R_Suicide = Rstd_Suicide;
    ptr_R_ShowMessage = Rstd_ShowMessage;
    ptr_R_ReadConsole = Rstd_ReadConsole;
    ptr_R_WriteConsole = Rstd_WriteConsole;
    ptr_R_ResetConsole = Rstd_ResetConsole;
    ptr_R_FlushConsole = Rstd_FlushConsole;
    ptr_R_ClearerrConsole = Rstd_ClearerrConsole;
    ptr_R_Busy = Rstd_Busy;
    ptr_R_CleanUp = Rstd_CleanUp;
    ptr_R_ShowFiles = Rstd_ShowFiles;
    ptr_R_ChooseFile = Rstd_ChooseFile;
    ptr_R_loadhistory = Rstd_loadhistory;
    ptr_R_savehistory = Rstd_savehistory;
    ptr_R_addhistory = Rstd_addhistory;
    ptr_R_EditFile = NULL; /* for future expansion */
    R_timeout_handler = NULL;
    R_timeout_val = 0;

    R_GlobalContext = NULL; /* Make R_Suicide less messy... */

    if((R_Home = R_HomeDir()) == NULL)
	R_Suicide("R home directory is not defined");
    BindDomain(R_Home);

    process_system_Renviron();

    R_setStartTime();
    R_DefParams(Rp);
    /* Store the command line arguments before they are processed
       by the R option handler.
     */
    R_set_command_line_arguments(ac, av);
    cmdlines[0] = '\0';

    /* first task is to select the GUI.
       If run from the shell script, only Tk|tk|X11|x11 are allowed.
     */
    for(i = 0, avv = av; i < ac; i++, avv++) {
	if(!strncmp(*avv, "--gui", 5) || !strncmp(*avv, "-g", 2)) {
	    if(!strncmp(*avv, "--gui", 5) && strlen(*avv) >= 7)
		p = &(*avv)[6];
	    else {
		if(i+1 < ac) {
		    avv++; p = *avv; ioff++;
		} else {
		    snprintf(msg, 1024,
			    _("WARNING: --gui or -g without value ignored"));
		    R_ShowMessage(msg);
		    p = "X11";
		}
	    }
	    if(!strcmp(p, "none"))
		useX11 = FALSE; // not allowed from R.sh
#ifdef HAVE_AQUA
	    else if(!strcmp(p, "aqua"))
		useaqua = TRUE; // not allowed from R.sh but used by R.app
#endif
	    else if(!strcmp(p, "X11") || !strcmp(p, "x11"))
		useX11 = TRUE;
	    else if(!strcmp(p, "Tk") || !strcmp(p, "tk"))
		useTk = TRUE;
	    else {
#ifdef HAVE_X11
		snprintf(msg, 1024,
			 _("WARNING: unknown gui '%s', using X11\n"), p);
#else
		snprintf(msg, 1024,
			 _("WARNING: unknown gui '%s', using none\n"), p);
#endif
		R_ShowMessage(msg);
	    }
	    /* now remove it/them */
	    for(j = i; j < ac - ioff; j++)
		av[j] = av[j + ioff];
	    ac -= ioff;
	    break;
	}
    }

#ifdef HAVE_X11
    if(useX11) R_GUIType = "X11";
#endif /* HAVE_X11 */

#ifdef HAVE_AQUA
    if(useaqua) R_GUIType = "AQUA";
#endif

#ifdef HAVE_TCLTK
    if(useTk) R_GUIType = "Tk";
#endif

    R_common_command_line(&ac, av, Rp);
    while (--ac) {
	if (**++av == '-') {
	    if(!strcmp(*av, "--no-readline")) {
		UsingReadline = FALSE;
	    } else if(!strcmp(*av, "-f")) {
		ac--; av++;
		Rp->R_Interactive = FALSE;
		if(strcmp(*av, "-")) {
		    /* Undo the escaping done in the front end */
		    char path[PATH_MAX], *p = path, *q;
		    for(q = *av; *q; q++) {
			if(*q == '~' && *(q+1) == '+' && *(q+2) == '~') {
			    q += 2;
			    *p++ = ' ';
			} else *p++ = *q;
		    }
		    *p = '\0';
		    ifp = R_fopen(path, "r");
		    if(!ifp) {
			snprintf(msg, 1024,
				 _("cannot open file '%s': %s"),
				 path, strerror(errno));
			R_Suicide(msg);
		    }
		}
	    } else if(!strncmp(*av, "--file=", 7)) {
		Rp->R_Interactive = FALSE;
		if(strcmp((*av)+7, "-")) {
		    /* Undo the escaping done in the front end */
		    char path[PATH_MAX], *p = path, *q;
		    for(q = (*av)+7; *q; q++) {
			if(*q == '~' && *(q+1) == '+' && *(q+2) == '~') {
			    q += 2;
			    *p++ = ' ';
			} else *p++ = *q;
		    }
		    *p = '\0';
		    ifp = R_fopen(path, "r");
		    if(!ifp) {
			snprintf(msg, 1024,
				 _("cannot open file '%s': %s"),
				 path, strerror(errno));
			R_Suicide(msg);
		    }
		}
	    } else if(!strcmp(*av, "-e")) {
		ac--; av++;
		Rp->R_Interactive = FALSE;
		if(strlen(cmdlines) + strlen(*av) + 2 <= 10000) {
		    char *p = cmdlines+strlen(cmdlines), *q;
		    /* Undo the escaping done in the front end */
		    for(q = *av; *q; q++) {
			if(*q == '~' && *(q+1) == '+' && *(q+2) == '~') {
			    q += 2;
			    *p++ = ' ';
			} else *p++ = *q;
		    }
		    *p++ = '\n'; *p = '\0';
		} else {
		    snprintf(msg, 1024, _("WARNING: '-e %s' omitted as input is too long\n"), *av);
		    R_ShowMessage(msg);
		}
	    } else if(!strcmp(*av, "--args")) {
		break;
	    } else if(!strcmp(*av, "--interactive")) {
		force_interactive = TRUE;
		break;
	    } else {
#ifdef HAVE_AQUA
		// r27492: in 2003 launching from 'Finder OSX' passed this
		if(!strncmp(*av, "-psn", 4)) break; else
#endif
		snprintf(msg, 1024, _("WARNING: unknown option '%s'\n"), *av);
		R_ShowMessage(msg);
	    }
	} else {
	    snprintf(msg, 1024, _("ARGUMENT '%s' __ignored__\n"), *av);
	    R_ShowMessage(msg);
	}
    }

    if(strlen(cmdlines)) { /* had at least one -e option */
	size_t res;
	if(ifp) R_Suicide(_("cannot use -e with -f or --file"));
	ifp = tmpfile();
	if(!ifp) R_Suicide(_("creating temporary file for '-e' failed"));
	res = fwrite(cmdlines, strlen(cmdlines)+1, 1, ifp);
	if(res != 1) error("fwrite error in initialize_R");
	fflush(ifp);
	rewind(ifp);
    }
    if (ifp && Rp->SaveAction != SA_SAVE) Rp->SaveAction = SA_NOSAVE;

    R_SetParams(Rp);

    if(!Rp->NoRenviron) {
	process_site_Renviron();
	process_user_Renviron();
    }


    /* On Unix the console is a file; we just use stdio to write on it */

#ifdef HAVE_AQUA
    if(useaqua)
	R_Interactive = useaqua;
    else
#endif
	R_Interactive = R_Interactive && (force_interactive || isatty(0));

#ifdef HAVE_AQUA
    /* for Aqua and non-dumb terminal use callbacks instead of connections
       and pretty-print warnings/errors (ESS = dumb terminal) */
    if(useaqua || 
       (R_Interactive && getenv("TERM") && strcmp(getenv("TERM"), "dumb"))) {
	R_Outputfile = NULL;
	R_Consolefile = NULL;
	ptr_R_WriteConsoleEx = Rstd_WriteConsoleEx;
	ptr_R_WriteConsole = NULL;
    } else {
#endif
    R_Outputfile = stdout;
    R_Consolefile = stderr;
#ifdef HAVE_AQUA
    }
#endif


/*
 *  Since users' expectations for save/no-save will differ, we decided
 *  that they should be forced to specify in the non-interactive case.
 */
    if (!R_Interactive && Rp->SaveAction != SA_SAVE &&
	Rp->SaveAction != SA_NOSAVE)
	R_Suicide(_("you must specify '--save', '--no-save' or '--vanilla'"));

    R_setupHistory();
    if (R_RestoreHistory)
	Rstd_read_history(R_HistoryFile);
    fpu_setup(1);

    return(0);
}

    /*
       This function can be used to open the named files in text
       editors.  If the file does not exist then the editor should be
       opened to create a new file.  On GUI platforms multiple files
       can be opened in separate editor windows, but this currently
       only works on Windows and Aqua.
    */

    /*
     *     nfile   = number of files
     *     file    = array of filenames
     *     editor  = editor to be used.
     */
int R_EditFiles(int nfile, const char **file, const char **title,
		const char *editor)
{
    char  buf[1024];

    if (ptr_R_EditFiles) return(ptr_R_EditFiles(nfile, file, title, editor));

    if (nfile > 0) {
	if (nfile > 1)
	    R_ShowMessage(_("WARNING: Only editing the first in the list of files"));

	if (ptr_R_EditFile) ptr_R_EditFile((char *) file[0]);
	else {
	    /* Quote path if necessary */
	    if (editor[0] != '"' && Rf_strchr(editor, ' '))
		snprintf(buf, 1024, "\"%s\" \"%s\"", editor, file[0]);
	    else
		snprintf(buf, 1024, "%s \"%s\"", editor, file[0]);
	    R_system(buf);
	}
	return 0;
    }
    return 1;
}
