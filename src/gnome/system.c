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
#include "Graphics.h"		/* KillAllDevices() [nothing else?] */
#include "Rversion.h"

#include "devGNOME.h"

#include "Startup.h"

#include "terminal.h"
#include "gtkconsole.h"

#include <gnome.h>

/*-- necessary for some (older, i.e., ~ <= 1997) Linuxen:*/
#ifdef linux
#ifndef FD_SET
#include <sys/time.h>
#endif
#endif

void fpu_setup(int);     /* in sys-unix.c */

	/*--- Initialization Code ---*/

int UsingReadline = 1;
int SaveAction = SA_SAVEASK;
int RestoreAction = SA_RESTORE;
int LoadSiteFile = True;
int LoadInitFile = True;
int DebugInitFile = False;

static gboolean R_gnome_initialised = FALSE; /* true once gnome_init has been called */

static GList *messages_list = NULL;

/*
 *  1) FATAL MESSAGES AT STARTUP
 */

void R_Suicide(char *s)
{
    GtkWidget *dialog;
    gchar *message;

    /* Create the error message */
    message = g_strdup_printf("R: Fatal error\n\n%s", s);

    dialog = gnome_message_box_new(message,
				   GNOME_MESSAGE_BOX_ERROR,
				   GNOME_STOCK_BUTTON_CLOSE,
				   NULL);

    if(R_gtk_main_window != NULL)
	gnome_dialog_set_parent(GNOME_DIALOG(dialog), GTK_WINDOW(R_gtk_main_window));
    gtk_window_set_modal(GTK_WINDOW(dialog), TRUE);
    gnome_dialog_set_default(GNOME_DIALOG(dialog), 0);

    gnome_dialog_run_and_close(GNOME_DIALOG(dialog));

    R_CleanUp(SA_SUICIDE);
}



/*
 *  3) ACTIONS DURING (LONG) COMPUTATIONS
 */

void R_Busy(int which)
{
    if(which == 1) {
	gnome_appbar_set_default(GNOME_APPBAR(GNOME_APP(R_gtk_main_window)->statusbar),
				 "Working...");
	while(gtk_events_pending())
	    gtk_main_iteration();
    }
    else {
	gnome_appbar_set_default(GNOME_APPBAR(GNOME_APP(R_gtk_main_window)->statusbar),
				 "");
    }    
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
    gint which; /* yes = 0, no = 1, cancel = 2 || -1 */

    GList *curfile = R_gtk_editfiles;
    R_gtk_edititem *edititem;

    if(saveact == SA_DEFAULT) /* The normal case apart from R_Suicide */
	saveact = SaveAction;

    if(saveact == SA_SAVEASK) {
	if(R_Interactive) {
	    R_ClearerrConsole();
	    R_FlushConsole();
	    dialog = gnome_message_box_new("Do you want to save your workspace image?\n\nChoose Yes to save an image and exit, choose\nNo to exit without saving, or choose Cancel to\nreturn to R.",
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
	    default:
		jump_to_toplevel();
		break;
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
	break;
    }

    /* unlink all the files we opened for editing */
    while(curfile != NULL) {
      edititem = (R_gtk_edititem *) curfile->data;
      unlink(edititem->filename);
      curfile = g_list_next(curfile);
    }


    /* close all the graphics devices */
    KillAllDevices();
    fpu_setup(0);

    exit(0);
}

void R_ShowMessage(char *s)
{
    GtkWidget *dialog;
    gchar *s_copy;

    if(R_gnome_initialised) {
        dialog = gnome_message_box_new(s,
                                       GNOME_MESSAGE_BOX_WARNING,
                                       GNOME_STOCK_BUTTON_OK,
                                       NULL);
                           
        if(R_gtk_main_window != NULL)
            gnome_dialog_set_parent(GNOME_DIALOG(dialog), GTK_WINDOW(R_gtk_main_window));
        gtk_window_set_modal(GTK_WINDOW(dialog), TRUE);
        gnome_dialog_set_default(GNOME_DIALOG(dialog), 0);

        gnome_dialog_run_and_close(GNOME_DIALOG(dialog));    
    }
    else {
        /* queue the message */
        s_copy = g_strdup(s);

        messages_list = g_list_append(messages_list,
                                      (gpointer) s_copy);
    }
}

void R_ShowQueuedMessages()
{
    GList *l;

    for(l = messages_list; l != NULL; l = l->next) {
        R_ShowMessage((char *) l->data);
        g_free(l->data);
    }

    g_list_free(messages_list);
    messages_list = NULL;
}

	/*--- Initialization Code ---*/


static const struct poptOption popt_options[] = {
  { NULL, '\0', 0, NULL, 0, NULL, NULL }
};

void handle_gnome_args()
{
    /* handle gnome-specific command line options */
    /* from popt_options above */
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

    /* Initialise Gnome library and parse command line arguments */
    gnome_init_with_popt_table("R.gnome",
			       g_strdup_printf("%s.%s %s (%s %s, %s)", R_MAJOR, R_MINOR, R_STATUS, R_MONTH, R_DAY, R_YEAR),
			       ac, av,
			       popt_options, 0, NULL);

    R_gnome_initialised = TRUE;

    R_ShowQueuedMessages();

    /* Load saved preferences */
    R_gnome_load_prefs();

    /* Act on previously parsed command line arguments */
    handle_gnome_args();

    R_SetParams(Rp);

    R_Interactive = isatty(0);
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

    /* create console */
    R_gtk_terminal_new();

    /* restore command history */
    gtk_console_restore_history(GTK_CONSOLE(R_gtk_terminal_text), R_HistoryFile, R_HistorySize, NULL);

    fpu_setup(1);

    /* start main loop */
    mainloop();
    /*++++++  in ../main/main.c */

    return 0;
}




	/* Declarations to keep f77 happy */

int MAIN_()  {return 0;}
int MAIN__() {return 0;}
int __main() {return 0;}


