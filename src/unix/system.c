/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2004  Robert Gentleman, Ross Ihaka
 *			      and the R Development Core Team
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

/* See system.txt for a description of functions
 */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

/* necessary for some (older, i.e., ~ <= 1997) Linuxen, and apparently
   also some AIX systems.
   */
#ifndef FD_SET
# ifdef HAVE_SYS_TIME_H
#  include <sys/time.h>
# endif
#endif

#ifdef HAVE_UNISTD_H
# include <unistd.h>		/* isatty() */
#endif

#include "Defn.h"
#include "Fileio.h"
#include "Rdevices.h"		/* KillAllDevices() [nothing else?] */

#define __SYSTEM__
#include "devUI.h"		/* includes Startup.h */
#undef __SYSTEM__

#include "Runix.h"


#ifdef HAVE_AQUA
void R_StartConsole(Rboolean OpenConsole) { ptr_R_StartConsole(); }
#endif

Rboolean UsingReadline = TRUE;  /* used in sys-std.c & ../main/platform.c */
extern SA_TYPE SaveAction;

/* call pointers to allow interface switching */

void R_Suicide(char *s) { ptr_R_Suicide(s); }
void R_ShowMessage(char *s) { ptr_R_ShowMessage(s); }
int R_ReadConsole(char *prompt, unsigned char *buf, int len, int addtohistory)
{ return ptr_R_ReadConsole(prompt, buf, len, addtohistory); }
void R_WriteConsole(char *buf, int len) {ptr_R_WriteConsole(buf, len);}
void R_ResetConsole(void) { ptr_R_ResetConsole(); }
void R_FlushConsole(void) { ptr_R_FlushConsole(); }
void R_ClearerrConsole(void) { ptr_R_ClearerrConsole(); }
void R_Busy(int which) { ptr_R_Busy(which); }
void R_CleanUp(SA_TYPE saveact, int status, int runLast)
{ ptr_R_CleanUp(saveact, status, runLast); }
int R_ShowFiles(int nfile, char **file, char **headers, char *wtitle,
		Rboolean del, char *pager)
{ return ptr_R_ShowFiles(nfile, file, headers, wtitle, del, pager); }
int R_ChooseFile(int new, char *buf, int len)
{ return ptr_R_ChooseFile(new, buf, len); }


void (*ptr_gnome_start)(int ac, char **av, Rstart Rp);

void R_setStartTime(void); /* in sys-unix.c */
void R_load_gnome_shlib(void); /* in dynload.c */


#ifdef HAVE_AQUA
/*  this should be a global variable as it used in unix/devQuartz.c
	and in unix/aqua.c
*/
Rboolean useaqua = FALSE;
Rboolean CocoaGUI = FALSE;
Rboolean useCocoa = FALSE;
#endif


int Rf_initialize_R(int ac, char **av)
{
    int i, ioff = 1, j, value, ierr;
    Rboolean useX11 = TRUE, usegnome = FALSE, useTk = FALSE;
    char *p, msg[1024], **avv;
    structRstart rstart;
    Rstart Rp = &rstart;

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
    R_timeout_handler = NULL;
    R_timeout_val = 0;

    R_GlobalContext = NULL; /* Make R_Suicide less messy... */

    if((R_Home = R_HomeDir()) == NULL)
	R_Suicide("R home directory is not defined");

    process_system_Renviron();

#ifdef _R_HAVE_TIMING_
    R_setStartTime();
#endif
    R_DefParams(Rp);
    /* Store the command line arguments before they are processed
       by the R option handler. 
     */
    R_set_command_line_arguments(ac, av);

    /* first task is to select the GUI */
    for(i = 0, avv = av; i < ac; i++, avv++) {
	if(!strncmp(*avv, "--gui", 5) || !strncmp(*avv, "-g", 2)) {
	    if(!strncmp(*avv, "--gui", 5) && strlen(*avv) >= 7)
		p = &(*avv)[6];
	    else {
		if(i+1 < ac) {
		    avv++; p = *avv; ioff++;
		} else {
		    sprintf(msg, "WARNING: --gui or -g without value ignored");
		    R_ShowMessage(msg);
		    p = "X11";
		}
	    }
	    if(!strcmp(p, "none"))
		useX11 = FALSE;
	    else if(!strcmp(p, "gnome") || !strcmp(p, "GNOME"))
		usegnome = TRUE;
#ifdef HAVE_AQUA
	    else if(!strcmp(p, "aqua") || !strcmp(p, "AQUA"))
		useaqua = TRUE;
	    else if(!strcmp(p, "cocoa") || !strcmp(p, "COCOA"))
		useCocoa = TRUE;
#endif
	    else if(!strcmp(p, "X11") || !strcmp(p, "x11"))
		useX11 = TRUE;
	    else if(!strcmp(p, "Tk") || !strcmp(p, "tk"))
		useTk = TRUE;
	    else {
#ifdef HAVE_X11
		snprintf(msg, 1024,
			 "WARNING: unknown gui `%s', using X11\n", p);
#else
		snprintf(msg, 1024,
			 "WARNING: unknown gui `%s', using none\n", p);
#endif
		R_ShowMessage(msg);
	    }
	    /* now remove it/them */
	    for(j = i; j < ac-ioff; j++) {
		av[j] = av[j + ioff];
	    }
	    ac -= ioff;
	    break;
	}
    }

#ifdef HAVE_X11
    if(useX11) {
	if(!usegnome) {
	    R_GUIType = "X11";
	} else {
#ifndef HAVE_GNOME
	    R_Suicide("GNOME GUI is not available in this version");
#endif
	    R_load_gnome_shlib();
	    R_GUIType = "GNOME";
	    ptr_gnome_start(ac, av, Rp);
	    /* this will never return, but for safety */
	    return 0;
	}
    }
#endif /* HAVE_X11 */
#ifdef HAVE_AQUA
    if(useaqua) {
	R_load_aqua_shlib();
	R_GUIType = "AQUA";
    }
	if(useCocoa)
	R_GUIType = "AQUA";
#endif
#ifdef HAVE_TCLTK
    if(useTk) {
	R_GUIType = "Tk";
    }
#endif
    R_common_command_line(&ac, av, Rp);
    while (--ac) {
	if (**++av == '-') {
	    if(!strcmp(*av, "--no-readline")) {
		UsingReadline = FALSE;
	    } else if(!strcmp(*av, "--args")) {
		break;
	    } else {
#ifdef HAVE_AQUA
		if(!strncmp(*av, "-psn", 4)) 
		    break; 
		else
#endif
		snprintf(msg, 1024, "WARNING: unknown option %s\n", *av);
		R_ShowMessage(msg);
	    }
	} else {
	    snprintf(msg, 1024, "ARGUMENT '%s' __ignored__\n", *av);
	    R_ShowMessage(msg);
	}
    }
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
    R_Interactive = isatty(0);

#ifdef HAVE_AQUA
    if(useaqua){
	R_Outputfile = NULL;
	R_Consolefile = NULL;
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
	R_Suicide("you must specify `--save', `--no-save' or `--vanilla'");

    if ((R_HistoryFile = getenv("R_HISTFILE")) == NULL)
	R_HistoryFile = ".Rhistory";
    R_HistorySize = 512;
    if ((p = getenv("R_HISTSIZE"))) {
	value = R_Decode2Long(p, &ierr);
	if (ierr != 0 || value < 0)
	    R_ShowMessage("WARNING: invalid R_HISTSIZE ignored;");
	else
	    R_HistorySize = value;
    }
    if (R_RestoreHistory)
	Rstd_read_history(R_HistoryFile);
    fpu_setup(1);

#ifdef HAVE_AQUA
    if(useaqua & !CocoaGUI)
		R_StartConsole(TRUE);
#endif

 return(0);
}

    /*
       This function can be used to open the named files in text
       editors.  If the file does not exist then the editor should be
       opened to create a new file.  On GUI platforms multiple files
       can be opened in separate editor windows, but this currently
       only works on Windows, not Aqua.
    */

    /*
     *     nfile   = number of files
     *     file    = array of filenames
     *     editor  = editor to be used.
     */

int R_EditFiles(int nfile, char **file, char **title, char *editor)
{
    char  buf[1024];
#if defined(HAVE_AQUA)
	if (useCocoa){
		return(ptr_R_EditFiles(nfile, file, title, editor));		
	}
#endif

    if (nfile > 0) {
	if (nfile > 1)
	    R_ShowMessage("WARNING: Only editing the first in the list of files");

#if defined(HAVE_AQUA)
	if (!strcmp(R_GUIType, "AQUA"))
	    Raqua_Edit(file[0]);
	else {
#endif
	    /* Quote path if necessary */
	    if (editor[0] != '"' && strchr(editor, ' '))
		snprintf(buf, 1024, "\"%s\" \"%s\"", editor, file[0]);
	    else
		snprintf(buf, 1024, "%s \"%s\"", editor, file[0]);
	    R_system(buf);
#if defined(HAVE_AQUA)
	}
#endif
	return 0;
    }
    return 1;
}


