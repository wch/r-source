/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--1998  Robert Gentleman, Ross Ihaka and the
 *                            R Development Coreeam
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

/*
 *  SYSTEM DEPENDENT CODE
 *
 *  This source file contains the platform dependent code for
 *  the Unix (reference) port of R.
 *
 *  The first group of functions is concerned with reading and
 *  writing to the system console.
 *
 *    int   R_ReadConsole(char *prompt, char *buf, int buflen, int hist)
 *
 *  This function prints the given prompt at the console and then
 *  does a gets(3)-like operation, transfering up to "buflen" characters
 *  into the buffer "buf".  The last two characters are set to "\n\0"
 *  to preserve sanity.	 If "hist" is non-zero, then the line is added
 *  to any command history which is being maintained.  Note that this
 *  is one natural place from which to run an event loop.
 *
 *    void  R_WriteConsole(char *buf, int buflen)
 *
 *  This function writes the given buffer out to the console.  No
 *  special actions are required.  Under Unix the characters are
 *  just appended to stdout.
 *
 *    void  R_ResetConsole(void)
 *
 *  This function is called when the system is reset after an error.
 *  It probably isn't really needed.
 *
 *    void  R_FlushConsole(void)
 *
 *  This called to flush any output to the system console.  Under Unix
 *  this is just fflush(stdout).  Other systems may not need this.
 *
 *    void  R_ClearerrConsole(void)
 *
 *  This function clears any errors associated with reading from the
 *  console.  In Unix is is used to clear any EOF condition associated
 *  with stdin.
 *
 *    void  R_Suicide(char *msg)
 *
 *  This function displays the given message and the causes R to
 *  die immediately.  It is used for non-recoverable errors such as
 *  not having enough memory to launch etc.  The phrase "dialog box"
 *  springs to mind for non-unix platforms.
 *
 *    void  R_Busy(int which)
 *
 *  This function invokes actions (such as change of cursor) when
 *  R embarks on an extended computation (which=1) and when such a
 *  state terminates (which=0).
 *
 *    void  R_CleanUp(int ask)
 *
 *  This function invokes any actions which occur at system termination.
 *
 *    char* R_ExpandFileName(char *s)
 *
 *  This is a utility function which can be used to expand special
 *  characters in file names.  In Unix it's sole function is to expand
 *  and "~"s which occur in filenames (and then only when the readline
 *  library is available.  The minimal action is to return the argument
 *  unaltered.
 *
 *    void  R_InitialData(void)
 *    FILE* R_OpenInitFile(void)
 *    FILE* R_OpenLibraryFile(char *file)
 *    FILE* R_OpenSysInitFile(void)
 *
 *  The following two functions save and restore the user's global
 *  environment.  The system specific aspect of this what files
 *  are used for this.
 *
 *    void  R_RestoreGlobalEnv(void)
 *    void  R_SaveGlobalEnv(void)
 *
 *  Platform dependent functions.
 *
 *    SEXP  do_interactive(SEXP call, SEXP op, SEXP args, SEXP rho)
 *    SEXP  do_machine(SEXP call, SEXP op, SEXP args, SEXP rho)
 *    SEXP  do_proctime(SEXP call, SEXP op, SEXP args, SEXP rho)
 *    SEXP  do_quit(SEXP call, SEXP op, SEXP args, SEXP rho)
 *    SEXP  do_system(SEXP call, SEXP op, SEXP args, SEXP rho)
 */

#include "Defn.h"
#include "Fileio.h"
#include "Graphics.h"		/* KillAllDevices() [nothing else?] */
#include "Version.h"

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

#ifdef HAVE_TIMES
#include <sys/times.h>

clock_t StartTime;
struct tms timeinfo;
#endif

#ifdef __FreeBSD__
#include <floatingpoint.h>
#endif

#ifdef linux
#include <fpu_control.h>
#endif

int UsingReadline = 1;
int DefaultSaveAction = 0;
int DefaultRestoreAction = 1;
int LoadSiteFile = 1;
int LoadInitFile = 1;
int DebugInitFile = 0;

int arg_save = 0, arg_no_save = 0, arg_restore = 0, arg_no_restore = 0, arg_no_readline = 0, arg_no_site_file = 0;
int arg_no_init_file = 0, arg_no_environ = 0, arg_vanilla = 0, arg_version = 0, arg_verbose = 0, arg_debug_init;
char *arg_vsize, *arg_nsize;
int arg_quiet, arg_slave;

#define Max_Nsize 20000000	/* must be < LONG_MAX (= 2^32 - 1 =) 2147483647 = 2.1e9 */
#define Max_Vsize (2048*Mega)	/* must be < LONG_MAX */

#define Min_Nsize 200000
#define Min_Vsize (2*Mega)

static const struct poptOption popt_options[] = {
  { "version", '\0', POPT_ARG_NONE, &arg_version, 0, "", NULL },
  { "save", '\0', POPT_ARG_NONE, &arg_save, 0, "", NULL },
  { "no-save", '\0', POPT_ARG_NONE, &arg_no_save, 0, "", NULL },
  { "restore", '\0', POPT_ARG_NONE, &arg_restore, 0, "", NULL },
  { "no-restore", '\0', POPT_ARG_NONE, &arg_no_restore, 0, "", NULL },
  { "no-readline", '\0', POPT_ARG_NONE, &arg_no_readline, 0, "", NULL },
  { "no-site-file", '\0', POPT_ARG_NONE, &arg_no_site_file, 0, "", NULL },
  { "no-init-file", '\0', POPT_ARG_NONE, &arg_no_init_file, 0, "", NULL },
  { "debug-init", '\0', POPT_ARG_NONE, &arg_debug_init, 0, "", NULL },
  { "no-environ", '\0', POPT_ARG_NONE, &arg_no_environ, 0, "", NULL },
  { "vanilla", '\0', POPT_ARG_NONE, &arg_vanilla, 0, "", NULL },
  { "verbose", '\0', POPT_ARG_NONE, &arg_verbose, 0, "", NULL },
  { "vsize", 'v', POPT_ARG_STRING, &arg_vsize, 0, "", NULL },
  { "nsize", 'n', POPT_ARG_STRING, &arg_nsize, 0, "", NULL },
  { "quiet", 'q', POPT_ARG_NONE, &arg_quiet, 0, "", NULL },
  { "silent", '\0', POPT_ARG_NONE, &arg_quiet, 0, "", NULL },
  { "slave", 's', POPT_ARG_NONE, &arg_slave, 0, "", NULL },
  { NULL, '\0', 0, NULL, 0, NULL, NULL }
};

void handle_args() {
  int value, ierr;
  /* handle command line options */
  
  if(arg_version) {
    fprintf(stderr, "Version %s.%s %s (%s %s, %s)\n",
	    R_MAJOR, R_MINOR, R_STATUS, R_MONTH, R_DAY, R_YEAR);
    fprintf(stderr, "Copyright (C) %s R Development Core Team\n\n", R_YEAR);
    fprintf(stderr, "R is free software and comes with ABSOLUTELY NO WARRANTY.\n");
    fprintf(stderr, "You are welcome to redistribute it under the terms of the\n");
    fprintf(stderr, "GNU General Public License.  For more information about\n");
    fprintf(stderr, "these matters, see http://www.gnu.org/copyleft/gpl.html.\n");
    exit(0);
  }
  if(arg_save) {
    DefaultSaveAction = 3;
  }
  if(arg_no_save) {
    DefaultSaveAction = 2;
  }
  if(arg_restore) {
    DefaultRestoreAction = 1;
  }
  if(arg_no_restore) {
    DefaultRestoreAction = 0;
  }
  if(arg_quiet) {
    R_Quiet = 1;
  }
  if(arg_vanilla) {
    DefaultSaveAction = 2;/* --no-save */
    DefaultRestoreAction = 0;/* --no-restore */
    LoadSiteFile = 0;/* --no-site-file */
    LoadInitFile = 0;/* --no-init-file */
  }
  if(arg_verbose) {
    R_Verbose = 1;
  }
  if(arg_slave) {
    R_Quiet = 1;
    R_Slave = 1;
    DefaultSaveAction = 2;
  }
  if(arg_no_site_file) {
    LoadSiteFile = 0;
  }
  if(arg_no_init_file) {
    LoadInitFile = 0;
  }
  if(arg_debug_init) {
    DebugInitFile = 1;
  }
  if(arg_vsize) {
    value = Decode2Long(arg_vsize, &ierr);
    if(ierr) {
      if(ierr < 0) {
	fprintf(stderr, "Invalid argument passed to R.\n");
	exit(1);
      }
      fprintf(stderr, "--vsize %ld'%c': too large\n", value,
	      (ierr == 1)?'M':((ierr == 2)?'K':'k'));
    }
    if (value < 1000) {
      fprintf(stderr, "WARNING: vsize ridiculously low, Megabytes assumed\n");
      value *= Mega;
    }
    if(value < Min_Vsize || value > Max_Vsize)
      fprintf(stderr, "WARNING: invalid v(ector heap)size '%d' ignored;"
	      "using default = %gM\n", value, R_VSize / Mega);
    else
      R_VSize = value;
  }
  if(arg_nsize) {
    value = Decode2Long(arg_nsize, &ierr);
    if(ierr) {
      if(ierr < 0) {
	fprintf(stderr, "Invalid argument passed to R.\n");
	exit(1);
      }
      fprintf(stderr, "--nsize %ld'%c': too large", value,
	      (ierr == 1)?'M':((ierr == 2)?'K':'k'));
    }
    if(value < Min_Nsize || value > Max_Nsize)
      fprintf(stderr, "WARNING: invalid language heap (n)size '%d' ignored,"
	      " using default = %d\n", value, R_NSize);
    else
      R_NSize = value;
  }
}

int main(int ac, char **av)
{
    char *p;
    int value, ierr;

    gc_inhibit_torture = 1;
#ifdef HAVE_TIMES
    StartTime = times(&timeinfo);
#endif
    R_Quiet = 0;

    /* Initialise Gnome library and parse command line arguments */
    gnome_init_with_popt_table("R.gnome",
			       g_strdup_printf("%s.%s %s (%s %s, %s)", R_MAJOR, R_MINOR, R_STATUS, R_MONTH, R_DAY, R_YEAR),
			       ac, av,
			       popt_options, 0, NULL);

    /* Environment variables */
    if((p = getenv("R_VSIZE"))) {
	value = Decode2Long(p, &ierr);
	if(ierr != 0 || value > Max_Vsize || value < Min_Vsize) 
	    fprintf(stderr, "WARNING: invalid R_VSIZE ignored;");
	else
	    R_VSize = value;
    }
    if((p = getenv("R_NSIZE"))) {
	value = Decode2Long(p, &ierr);
	if(ierr != 0 || value > Max_Nsize || value < Min_Nsize) 
	    fprintf(stderr, "WARNING: invalid R_NSIZE ignored;");
	else
	    R_NSize = value;
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

    if(!R_Interactive && DefaultSaveAction == 0)
	R_Suicide("you must specify `--save' or `--no-save'");

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
    gtk_console_restore_history(GTK_CONSOLE(R_gtk_terminal_text), R_HistoryFile, R_HistorySize, NULL);

    /* start main loop */
    mainloop();
    /*++++++  in ../main/main.c */

    return 0;
}

	/* R_CleanUp is invoked at the end of the session to give */
	/* the user the option of saving their data.  If ask=1 the */
	/* user is asked their preference, if ask=2 the answer is */
	/* assumed to be "no" and if ask=3 the answer is assumed to */
	/* be "yes".  When R is being used non-interactively, and */
	/* ask=1, the value is changed to 3.  The philosophy is */
	/* that saving unwanted data is less bad than non saving */
	/* data that is wanted. */

void R_CleanUp(int ask)
{
    GtkWidget *dialog;
    gchar buf[128];
    gint which; /* yes = 0, no = 1, cancel = 2 || -1 */

    GList *curfile = R_gtk_editfiles;
    R_gtk_edititem *edititem;

    if( R_DirtyImage ) {
    qask:
	R_ClearerrConsole();
	R_FlushConsole();
	if(!R_Interactive && ask==1)
	    ask = DefaultSaveAction;

	if(ask == 1) {
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
	  }
	  else {
	    R_ReadConsole("Save workspace image? [y/n/c]: ",
			  buf, 128, 0);

	    switch(buf[0]) {
	    case 'y':
	    case 'Y':
	      which = 0;
	      break;

	    case 'n':
	    case 'N':
	      which = 1;
	      break;

	    case 'c':
	    case 'C':
	      which = 2;
	      break;
	    }
	  }
	}
	else if(ask == 2)
	    which = 1;
	else if (ask == 3)
	    which = 0;

	switch (which) {
	case 0:
	    R_SaveGlobalEnv();

	    if(R_Interactive)
	      gtk_console_save_history(GTK_CONSOLE(R_gtk_terminal_text), R_HistoryFile, R_HistorySize, NULL);
	    break;
	case 1:
	    break;
	default:
	    jump_to_toplevel();
	    break;
	}
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

  R_CleanUp(2);
  /*	 2 means don't save anything and it's an unrecoverable abort */
}


	/* Declarations to keep f77 happy */

int MAIN_()  {return 0;}
int MAIN__() {return 0;}
int __main() {return 0;}



