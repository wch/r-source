/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2000  Robert Gentleman, Ross Ihaka
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
#include <config.h>
#endif

#include <gnome.h>
#include <glade/glade.h>
#include <libgnome/libgnome.h>

#include <sys/types.h>
#include <sys/stat.h>

#include <Rversion.h>

#include "Defn.h"
#include "Fileio.h"
#include "Devices.h"

#include "../Runix.h"

#include "devGNOME.h"

#include "Startup.h"

#include "gtkconsole.h"
#include "terminal.h"
#include "terminal-prefs.h"

	/*--- Initialization Code ---*/

SA_TYPE SaveAction;
SA_TYPE RestoreAction;

static gboolean R_gnome_initialised = FALSE; /* true once gnome_init has been called */

static GList *messages_list = NULL;

/*
 *  1) FATAL MESSAGES AT STARTUP
 */

void Rgnome_Suicide(char *s)
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

    Rgnome_CleanUp(SA_SUICIDE, 2, 0);
}



/*
 *  3) ACTIONS DURING (LONG) COMPUTATIONS
 */

void Rgnome_Busy(int which)
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

void Rgnome_CleanUp(SA_TYPE saveact, int status, int runLast)
{
    GtkWidget *dialog;
    gint which; /* yes = 0, no = 1, cancel = 2 || -1 */

/*
    GList *curfile = R_gtk_editfiles;
    R_gtk_edititem *edititem;
*/
    if(saveact == SA_DEFAULT) /* The normal case apart from R_Suicide */
	saveact = SaveAction;

    if(saveact == SA_SAVEASK) {
	if(R_Interactive) {
	    R_ClearerrConsole();
	    R_FlushConsole();
	    dialog = gnome_message_box_new(
		"Do you want to save your workspace image?\n\n"

		"Choose Yes to save an image and exit, choose\n"
		"No to exit without saving, or choose Cancel to\n"
		"return to R.",
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
	if(runLast) R_dot_Last();
	if(R_DirtyImage) R_SaveGlobalEnv();
	if(R_Interactive)
	    gtk_console_save_history(GTK_CONSOLE(R_gtk_terminal_text), 
				     R_HistoryFile, R_HistorySize, NULL);
	break;
    case SA_NOSAVE:
	if(runLast) R_dot_Last();
	break;
    case SA_SUICIDE:
    default:
	break;
    }

    /* save GUI preferences */
    R_gnome_prefs_save();
/* unlink all the files we opened for editing 
    while(curfile != NULL) {
      edititem = (R_gtk_edititem *) curfile->data;
      unlink(edititem->filename);
      curfile = g_list_next(curfile);
    }
*/


    /* close all the graphics devices */
    KillAllDevices();
    fpu_setup(0);

    exit(status);
}

void Rgnome_ShowMessage(char *s)
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

void R_ShowQueuedMessages(void)
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


void R_set_gnome_prefs(Rstart Rp)
{
    Rp->RestoreAction = prefs_get_restoreact();
    Rp->SaveAction = prefs_get_saveact();
}
static const struct poptOption popt_options[] = {
  { NULL, '\0', 0, NULL, 0, NULL, NULL }
};

void R_set_SaveAction(int sa)
{
    SaveAction = sa;
}

void gnome_start(int ac, char **av, Rstart Rp)
{
    char *p;
    int value, ierr;
    struct stat sb;


    /* Gnome startup preferences */
    gnomelib_init("R",
		  g_strdup_printf("%s.%s %s (%s-%s-%s)", R_MAJOR,
				  R_MINOR, R_STATUS, R_YEAR, R_MONTH,
				  R_DAY));
    R_gnome_prefs_cmd_load(RestoreAction, SaveAction);
    R_set_gnome_prefs(Rp);

    /* command line params */
    R_common_command_line(&ac, av, Rp);

    /* Initialise Gnome library */
    gnome_init("R",
	       g_strdup_printf("%s.%s %s (%s-%s-%s)", R_MAJOR, R_MINOR,
			       R_STATUS, R_YEAR, R_MONTH, R_DAY),
	       ac, av);
    R_gnome_initialised = TRUE;

    /* Initialise libglade */
    glade_gnome_init();

    /* Gnome GUI preferences */
    R_gnome_prefs_gui_load();

    R_ShowQueuedMessages();

    R_SetParams(Rp);
    if(!Rp->NoRenviron) process_users_Renviron();

    R_Interactive = isatty(0);
    R_Sinkfile = NULL;
    if((R_Home = R_HomeDir()) == NULL) {
	R_Suicide("R home directory is not defined");
    }
    glade_interface_file = g_strdup_printf(GLADE_INTERFACE_FILE, R_Home);
    if(stat(glade_interface_file, &sb) == -1) {
	R_Suicide("GNOME interface file not found");
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
    if(R_RestoreHistory)
	gtk_console_restore_history(GTK_CONSOLE(R_gtk_terminal_text), 
				    R_HistoryFile, R_HistorySize, NULL);

    fpu_setup(1);


    /* start main loop */
    mainloop();
    /*++++++  in ../main/main.c */
}

SEXP do_syssleep(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    error("Sys.sleep is not implemented on this system");
}
