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
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

         /* See ../unix/system.txt for a description of functions */

#ifdef HAVE_CONFIG_H
#include <Rconfig.h>
#endif

#include "Defn.h"
#include "Fileio.h"
#include "Graphics.h"		/* KillAllDevices() [nothing else?] */
#include "Rversion.h"

#include "devGNOME.h"

/*-- necessary for some (older, i.e., ~ <= 1997) Linuxen:*/
#ifdef linux
#ifndef FD_SET
#include <sys/time.h>
#endif
#endif

#include "terminal.h"
#include "gtkconsole.h"

#include <gnome.h>

	/*--- Initialization Code ---*/

#ifdef __FreeBSD__
#include <floatingpoint.h>
#endif

#ifdef linux
#include <fpu_control.h>
#endif

int UsingReadline = 1;
int SaveAction = SA_SAVEASK;
int RestoreAction = SA_RESTORE;
int LoadSiteFile = 1;
int LoadInitFile = 1;
int DebugInitFile = 0;

/*
 *  1) FATAL MESSAGES AT STARTUP
 */

void suicide_delete_event(GtkWidget *widget, GdkEvent *event, gpointer data)
{
    gtk_main_quit();
}

void R_Suicide(char *s)
{
    GtkWidget *dialog;
    GtkWidget *vbox;
    GtkWidget *errortext;
    GtkWidget *hbox;
    GtkWidget *button;
  
    dialog = gtk_window_new(GTK_WINDOW_DIALOG);

    gtk_window_set_title(GTK_WINDOW(dialog), "R: Fatal error");
    gtk_widget_set_usize(dialog, 300, 150);
    gtk_container_border_width(GTK_CONTAINER(dialog), 5);
    gtk_widget_realize(dialog);

    vbox = gtk_vbox_new(FALSE, 10);
    gtk_container_add(GTK_CONTAINER(dialog), vbox);

    errortext = gtk_text_new(NULL, NULL);
    gtk_text_insert(GTK_TEXT(errortext), NULL, NULL, NULL, s, strlen(s));
    gtk_box_pack_start(GTK_BOX(vbox), errortext, TRUE, FALSE, 0);

    hbox = gtk_hbox_new(FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox), hbox, TRUE, FALSE, 0);

    button = gtk_button_new_with_label("OK");
    gtk_box_pack_start(GTK_BOX(hbox), button, TRUE, FALSE, 0);

    gtk_widget_show_all(dialog);

    gtk_signal_connect(GTK_OBJECT(dialog), "delete_event", GTK_SIGNAL_FUNC(suicide_delete_event), NULL);
    gtk_signal_connect(GTK_OBJECT(dialog), "destroy", GTK_SIGNAL_FUNC(suicide_delete_event), NULL);
    gtk_signal_connect(GTK_OBJECT(button), "clicked", GTK_SIGNAL_FUNC(suicide_delete_event), NULL);
  
    gtk_main();

    R_CleanUp(SA_SUICIDE);
}



/*
 *  3) ACTIONS DURING (LONG) COMPUTATIONS
 */

void R_Busy(int which)
{
}

/*
 *  4) INITIALIZATION AND TERMINATION ACTIONS
 */

/*
   R_CleanUp is invoked at the end of the session to give the user the
   option of saving their data.
   If ask == SA_SAVEASK the user should be asked if possible (and this
   option should not occur in non-interactive use).
   If ask = SA_SAVE or SA_NOSAVE the decision is known.
   If ask = SA_DEFAULT use the SaveAction set at startup.
   In all these cases run .Last() unless quitting is cancelled.
   If ask = SA_SUICIDE, no save, no .Last, possibly other things.
 */

void R_dot_Last(void);		/* in main.c */

void R_CleanUp(int saveact)
{
    GtkWidget *dialog;
    gchar buf[128];
    gint which; /* yes = 0, no = 1, cancel = 2 || -1 */

    GList *curfile = R_gtk_editfiles;
    R_gtk_edititem *edititem;

    if(saveact == SA_DEFAULT) /* The normal case apart from R_Suicide */
	saveact = SaveAction;

    if(saveact == SA_SAVEASK) {
	if(R_Interactive) {
	qask:
	    R_ClearerrConsole();
	    R_FlushConsole();
	    if(R_gtk_gui_quit == TRUE) {
		dialog = gnome_message_box_new("Do you want to save your workspace image?\n\n\
Choose Yes to save an image and exit,\nchoose No to exit without saving,\nor choose Cancel to return to R.",
					       GNOME_MESSAGE_BOX_QUESTION,
					       GNOME_STOCK_BUTTON_YES,
					       GNOME_STOCK_BUTTON_NO,
					       GNOME_STOCK_BUTTON_CANCEL,
					       NULL);
		
		gnome_dialog_set_parent(GNOME_DIALOG(dialog), GTK_WINDOW(R_gtk_main_window));
		gtk_window_set_modal(GTK_WINDOW(dialog), TRUE);
		gnome_dialog_set_default(GNOME_DIALOG(dialog), 0);
		
		which = gnome_dialog_run_and_close(GNOME_DIALOG(dialog));
		switch(which) {
		case 0:
		    saveact = SA_SAVE;
		    break;
		case 1:
		    saveact = SA_NOSAVE;
		    break;
		case 2:
		    jump_to_toplevel();
		    break;
		default:
		    goto qask;
		}
	    } else {
		R_ReadConsole("Save workspace image? [y/n/c]: ", buf, 128, 0);
		switch(buf[0]) {
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
	    }
	}
	else saveact = SaveAction;
    }

    switch (saveact) {
    case SA_SAVE:
	R_dot_Last();
	R_SaveGlobalEnv();
	if(R_Interactive)
	    gtk_console_save_history(GTK_CONSOLE(R_gtk_terminal_text), 
				     R_HistoryFile, R_HistorySize, NULL);
	break;
    case SA_NOSAVE:
	R_dot_Last();
	break;
    case SA_SUICIDE:
    default:
    }

    /* close all the graphics devices */
    KillAllDevices();

    /* unlink all the files we opened for editing */
    while(curfile != NULL) {
      edititem = (R_gtk_edititem *) curfile->data;
      unlink(edititem->filename);
      curfile = g_list_next(curfile);
    }


#ifdef __FreeBSD__
    fpsetmask(~0);
#endif

#ifdef linux
#ifdef HAVE___SETFPUCW
    __setfpucw(_FPU_DEFAULT);
#endif
#endif

    exit(0);
}

void R_ShowMessage(char *s)
{
    fprintf(stderr, s);
}

#include "../unix/Startup.h"

	/*--- Initialization Code ---*/

int arg_no_environ = 0;

static const struct poptOption popt_options[] = {
  { "no-environ", '\0', POPT_ARG_NONE, &arg_no_environ, 0, "", NULL },
  { NULL, '\0', 0, NULL, 0, NULL, NULL }
};

void handle_args()
{
    int value, ierr;
    /* handle command line options */
  
    if(arg_no_environ) {
	/* what does this do?  */
    }
}

void setStartTime(); /* in sys-unix.c */

int main(int ac, char **av)
{
    char *p;
    int value, ierr;
    structRstart rstart;
    Rstart Rp = &rstart;

    gc_inhibit_torture = 1;
#ifdef HAVE_TIMES
    setStartTime();
#endif

    R_DefParams(Rp);
    R_SizeFromEnv(Rp);
    R_common_command_line(&ac, av, Rp);
    R_SetParams(Rp);

    /* Initialise Gnome library and parse command line arguments */
    gnome_init_with_popt_table("R.gnome",
			       g_strdup_printf("%s.%s %s (%s %s, %s)", R_MAJOR, R_MINOR, R_STATUS, R_MONTH, R_DAY, R_YEAR),
			       ac, av,
			       popt_options, 0, NULL);


    if ((R_HistoryFile = getenv("R_HISTFILE")) == NULL)
	R_HistoryFile = ".Rhistory";
    R_HistorySize = 512;
    if ((p = getenv("R_HISTSIZE"))) {
	value = Decode2Long(p, &ierr);
	if (ierr != 0 || value < 0)
	    fprintf(stderr, "WARNING: invalid R_HISTSIZE ignored;");
	else
	    R_HistorySize = value;
    }

    /* Load saved preferences */
    R_gnome_load_prefs();

    /* Act on previously parsed command line arguments */
    handle_args();

    R_Interactive = 1;
    R_Sinkfile = NULL;
    if((R_Home = R_HomeDir()) == NULL) {
        R_Suicide("R home directory is not defined");
    }
/*
 *  Since users' expectations for save/no-save will differ, we decided
 *  that they should be forced to specify in the non-interactive case.
 */
    if (!R_Interactive && SaveAction != SA_SAVE && SaveAction != SA_NOSAVE)
	R_Suicide("you must specify `--save', `--no-save' or `--vanilla'");

#ifdef __FreeBSD__
    fpsetmask(0);
#endif

#ifdef linux
#ifdef HAVE___SETFPUCW
    __setfpucw(_FPU_IEEE);
#endif
#endif

    /* create console */
    R_gtk_terminal_new();

    /* restore command history */
    gtk_console_restore_history(GTK_CONSOLE(R_gtk_terminal_text), R_HistoryFile, R_HistorySize, NULL);

    /* start main loop */
    mainloop();
    /*++++++  in ../main/main.c */

    return 0;
}




	/* Declarations to keep f77 happy */

int MAIN_()  {return 0;}
int MAIN__() {return 0;}
int __main() {return 0;}


