/*
 *  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (C) 1998-1999   Lyndon Drake
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

#ifdef HAVE_CONFIG_H
#include <Rconfig.h>
#endif

#include "Defn.h"
#include "Fileio.h"
#include "Graphics.h"		/* KillAllDevices() [nothing else?] */
#include "Rversion.h"

#include "gnome-callbacks.h"
#include "prefs.h"
#include "rgnome.h"

#ifdef HAVE_LIBREADLINE
#include <readline/readline.h>
#ifdef HAVE_READLINE_HISTORY_H
#include <readline/history.h>
#endif
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <fcntl.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <sys/types.h>

/* necessary for some (older, i.e., ~ <= 1997) Linuxen, and apparently
   also some AIX systems.
   */
#ifndef FD_SET
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#endif

#include <glade/glade.h>
#include <gnome.h>
#include <libgnome/gnome-popt.h>
#include <libgnome/libgnome.h>
#include <libgnomeui/gnome-init.h>
#include <zvt/zvtterm.h>

void fpu_setup (int);     /* in ../unix/sys-unix.c */
void setStartTime (void); /* in ../unix/sys-unix.c */
void R_dot_Last (void);   /* in ../main/main.c */


#define GLADE_INTERFACE_FILE "/etc/gnome-interface.glade"
#define IO_BUF_SIZE 512

struct _r_read_callback_data {
    unsigned char *buf;
    int size, pos;
    gboolean eof;
};

struct _r_choose_file_data {
    GtkFileSelection *filesel;
    char *buf;
    int len;
};

/* FIXME: add assertions to enforce sensible values for these and many
   other variables */
static gboolean r_gnome_initialised = FALSE;
static GList *r_gnome_messages = NULL;
static gchar *r_glade_file = NULL;
static GladeXML *main_xml = NULL;
static GList *messages_list = NULL;
static int commandfd = -1;


int UsingReadline = 1;
int SaveAction = SA_SAVEASK;
int RestoreAction = SA_RESTORE;
int LoadSiteFile = True;
int LoadInitFile = True;
int DebugInitFile = False;


/* Declarations of non-public functions */
static void write_terminal (unsigned char *buf, int size);
static void incoming_keys (gpointer data, gint source,
			   GdkInputCondition condition);
static void output_text (gpointer data, gint source,
			 GdkInputCondition condition);
static gboolean on_term_key_press_event (GtkWidget *widget,
					 GdkEventKey *event,
					 gpointer user_data);
#ifdef HAVE_LIBREADLINE
static void r_gnome_rl_lhandler (char *line);
#endif /* HAVE_LIBREADLINE */
static gboolean read_terminal (unsigned char *buf, int size, int hist);
static void R_ShowQueuedMessages ();
static void showfiles_print (GtkWidget *widget, gpointer data);
static void showfiles_top (GtkWidget *widget, gpointer data);
static void showfiles_pageup (GtkWidget *widget, gpointer data);
static void showfiles_pagedown (GtkWidget *widget, gpointer data);
static void showfiles_bottom (GtkWidget *widget, gpointer data);
static void set_terminal_hints (GtkWidget *widget);
GtkWidget *r_gnome_create_terminal ();
static void load_main_window (void);


/**
 *  1) FATAL MESSAGES AT STARTUP
 **/

/**
 *  This function displays the given message and the causes R to
 *  die immediately.  It is used for non-recoverable errors such as
 *  not having enough memory to launch etc.
 **/
void  R_Suicide (char *msg)
{
    GtkWidget *main_window, *dialog;
    gchar *message;

    if (r_gnome_initialised) {
	if (main_xml)
	    main_window = glade_xml_get_widget (main_xml, "main_window");
	else
	    main_window = NULL;
	message = g_strdup_printf ("R fatal error:\n\n%s", msg);
	dialog = gnome_message_box_new (message,
					GNOME_MESSAGE_BOX_ERROR,
					GNOME_STOCK_BUTTON_CLOSE,
					NULL);
	if (main_window)
	    gnome_dialog_set_parent (GNOME_DIALOG (dialog),
				     GTK_WINDOW (main_window));
	gtk_window_set_modal (GTK_WINDOW (dialog), 0);
	gnome_dialog_run_and_close (GNOME_DIALOG (dialog));
    }
    else {
	fprintf (stderr, "Fatal error: %s\n", msg);
    }
    R_CleanUp (SA_SUICIDE, 2, 0);
}


/**
 *  2) CONSOLE I/O
 **/

static void write_terminal (unsigned char *buf, int size)
{
    GtkWidget *term;
    int n, nsize;
    unsigned char *b, *c;

    term = glade_xml_get_widget (main_xml, "terminal");

    for (n = 0, nsize = 0; n < size; n++, nsize++) {
	if (buf[n] == '\n')
	    nsize++;
    }

    c = b = (unsigned char *) malloc (nsize * sizeof (unsigned char));

    for (n = 0; n < nsize; n++) {
	if (*buf == '\n')
	    *c++ = '\r';
	*c++ = *buf++;
    }

    zvt_term_feed (ZVT_TERM (term), b, nsize);

    free (b);
}

/* Filled in as data is read */
struct _r_read_callback_data *data;

static void incoming_keys (gpointer user_data, gint source,
			   GdkInputCondition condition)
{
    int size, i;
    char buf[IO_BUF_SIZE];
    GtkWidget *term;

    fprintf (stderr, "  incoming_keys: start\n");

#ifdef HAVE_LIBREADLINE
    if (UsingReadline) {
	rl_callback_read_char ();
    }
    else
#endif /* HAVE_LIBREADLINE */
	{
	    term = glade_xml_get_widget (main_xml, "terminal");
	    
	    while ((size = read (source, buf, IO_BUF_SIZE)) > 0) {
		for (i = 0; i < size && data->pos < data->size; i++, data->pos++) {
		    if (buf[i] == '\r' || buf[i] == '\n') {
			data->buf[data->pos] = '\n';
			gtk_main_quit ();
		    }
		    else {
			data->buf[data->pos] = buf[i];
		    }
		}

		write_terminal (data->buf + data->pos - size, size);

		if (data->pos == data->size)
		    gtk_main_quit ();
	    }
	}
    fprintf (stderr, "  incoming_keys: end\n");
}

static void output_text (gpointer data, gint source,
			 GdkInputCondition condition)
{
    int size;
    char buf[IO_BUF_SIZE];
    GtkWidget *term;

    fprintf (stderr, "  output_text: start\n");

#ifdef HAVE_LIBREADLINE
    if (UsingReadline)
#endif /* HAVE_LIBREADLINE */
	{
	    term = glade_xml_get_widget (main_xml, "terminal");
	    
	    while ((size = read (source, buf, IO_BUF_SIZE)) > 0) {
		write_terminal (buf, size);
	    }
	}
    fprintf (stderr, "  output_text: end\n");
}

static gboolean on_term_key_press_event (GtkWidget *widget,
					 GdkEventKey *event,
					 gpointer user_data)
{
    gboolean handled;
    GtkWidget *term;
    char c;

    handled = FALSE;
    term = glade_xml_get_widget (main_xml, "terminal");
    
    zvt_term_hide_pointer (ZVT_TERM (term));

    if (event->state & GDK_CONTROL_MASK) {

	switch (event->keyval) {
	case GDK_c:  /* interrupt */
	case GDK_C:
	    gtk_main_quit ();
	    jump_to_toplevel ();
	    handled = TRUE;
	    break;

	case GDK_d:  /* EOF */
	case GDK_D:
	    if (data->pos == 0) {
		data->eof = TRUE;
		gtk_main_quit ();
	    }
	    else {
		gdk_beep ();
	    }
	    handled = TRUE;
	    break;

	case GDK_s:
	case GDK_S:
	    handled = TRUE;
	    break;

	case GDK_u:  /* kill line (only necessary if not using readline */
	case GDK_U:
#ifdef HAVE_LIBREADLINE
	    if (!UsingReadline)
#endif /* HAVE_LIBREADLINE */
		{
		    if (data->pos == 0) {
			gdk_beep ();
		    }
		    else {
			data->pos = 0;
		    }
		    handled = TRUE;
		}
	    break;
	}
    }

#ifdef HAVE_LIBREADLINE
    if (UsingReadline) {
	if (handled) {
	    if (ZVT_TERM (term)->scroll_on_keystroke) {
		gtk_adjustment_set_value (ZVT_TERM (term)->adjustment,
					  ZVT_TERM (term)->adjustment->upper -
					  ZVT_TERM (term)->adjustment->page_size);
	    }
	    gtk_signal_emit_stop_by_name (GTK_OBJECT (widget), "key_press_event");
	    return TRUE;
	}
	else {
	    return FALSE;
	}
    }
    else
#endif /* HAVE_LIBREADLINE */
	{
	    if (!handled) {
		switch (event->keyval) {
		case GDK_BackSpace:
		case GDK_Delete:
		case GDK_KP_Delete:
		    if (data->pos > 0) {
			data->pos--;
			zvt_term_feed (ZVT_TERM (term), "\010\033[P", 4);
		    }
		    else {
			gdk_beep ();
		    }
		    handled = TRUE;
		    break;
		    
		case GDK_KP_Right: case GDK_Right: case GDK_KP_Left: case GDK_Left:
		case GDK_KP_Up: case GDK_Up: case GDK_KP_Down: case GDK_Down:
		    gdk_beep ();
		    handled = TRUE;
		    break;
		    
		case GDK_KP_Insert: case GDK_Insert:
		case GDK_KP_Home: case GDK_Home: case GDK_KP_End: case GDK_End:
		case GDK_KP_F1: case GDK_F1: case GDK_KP_F2: case GDK_F2:
		case GDK_KP_F3: case GDK_F3: case GDK_KP_F4: case GDK_F4:
		case GDK_F5:  case GDK_F6:  case GDK_F7:  case GDK_F8:
		case GDK_F9:  case GDK_F10:  case GDK_F11:  case GDK_F12:
		case GDK_F13:  case GDK_F14:  case GDK_F15:  case GDK_F16:
		case GDK_F17:  case GDK_F18:  case GDK_F19:  case GDK_F20:
		case GDK_ISO_Left_Tab: case GDK_Tab: case GDK_Escape:
		case GDK_KP_Begin: case GDK_Print: case GDK_Scroll_Lock:
		case GDK_Pause: case GDK_Shift_Lock: case GDK_Num_Lock:
		case GDK_Caps_Lock: case GDK_Control_L: case GDK_Control_R:
		case GDK_Shift_L: case GDK_Shift_R: case GDK_Alt_L: case GDK_Alt_R:
		case GDK_Meta_L: case GDK_Meta_R: case GDK_Mode_switch:
		case GDK_Multi_key:
		    handled = TRUE;
		    break;
		    
		case GDK_KP_Page_Up: case GDK_Page_Up:
		case GDK_KP_Page_Down: case GDK_Page_Down:
		    /* FIXME: do a PageUp/PageDown */
		    handled = TRUE;
		    break;
		    
		default:
		    break;
		}
	    }
	}

    if (handled) {
	if (ZVT_TERM (term)->scroll_on_keystroke) {
	    gtk_adjustment_set_value (ZVT_TERM (term)->adjustment,
				      ZVT_TERM (term)->adjustment->upper -
				      ZVT_TERM (term)->adjustment->page_size);
	}
    }
    else {
	    write (ZVT_TERM (term)->vx->vt.keyfd, event->string, event->length);
    }

    gtk_signal_emit_stop_by_name (GTK_OBJECT (widget), "key_press_event");
    return TRUE;
}

#ifdef HAVE_LIBREADLINE
static void r_gnome_rl_lhandler (char *line)
{
    rl_callback_handler_remove ();

    /* fill in buf from line */
    if (!line) {
	data->eof = TRUE;
	return;
    }

    strncpy (data->buf, line, data->size);
    data->pos = strlen (line);
    if (data->pos > data->size)
	data->pos = data->size;

    gtk_main_quit();
}
#endif /* HAVE_LIBREADLINE */

void r_gnome_exit (void)
{
    data->pos = 0;
    data->eof = TRUE;
    gtk_main_quit ();
}



/**
 *  read_terminal reads up to (size - 2) characters from the terminal
 *  into buf.  The last two characters are set to "\n\0".  If
 *  (!R_Slave) then the characters are echoed.  Returns FALSE if eof.
 **/
static gboolean read_terminal (unsigned char *buf, int size, int hist)
{
    gboolean eof;
    GtkWidget *term;
    guint sigid;

    /* FIXME: make sure that read_terminal only echos if (!R_Slave) */

    fprintf (stderr, " read_terminal: start\n");

    data = (struct _r_read_callback_data *)
	malloc (sizeof (struct _r_read_callback_data));
    g_return_val_if_fail (data != NULL, FALSE);

    data->buf = buf;
    data->size = size - 2;
    data->pos = 0;
    data->eof = FALSE;

    term = glade_xml_get_widget (main_xml, "terminal");
    sigid = gtk_signal_connect (GTK_OBJECT (term), "key_press_event",
				GTK_SIGNAL_FUNC (on_term_key_press_event),
				NULL);

    gtk_main();

    eof = data->eof;
#ifdef HAVE_READLINE_HISTORY_H
    buf [data->pos] = '\0';
    if (!eof && strlen (buf) && hist) {
	add_history (buf);
    }
#endif
    buf [data->pos] = '\n';
    buf [data->pos + 1] = '\0';

    gtk_signal_disconnect (GTK_OBJECT (term), sigid);
    free (data);
    data = NULL;

    fprintf (stderr, " read_terminal: end\n");

    return !eof;
}

/**
 *  This function prints the given prompt at the console and then
 *  does a gets(3)-like operation, transferring up to "buflen" characters
 *  into the buffer "buf".  The last two characters are set to "\n\0"
 *  to preserve sanity.	 If "hist" is non-zero, then the line is added
 *  to any command history which is being maintained.
 **/
int  R_ReadConsole (char *prompt, unsigned char *buf, int buflen, int hist)
{
    char *prelight, *highlight, *postlight, *new_prompt;
    int read_status;

    prelight = "\e[0m";
    highlight = "\e[31m";
    postlight = "\e[0m";

    fprintf (stderr, "R_ReadConsole: start\n");

    R_FlushConsole ();

    new_prompt = (char *) malloc (sizeof (char) *
				  (strlen (prelight) +
				   strlen (prompt) +
				   strlen (highlight)));
    sprintf (new_prompt, "%s%s%s", prelight, prompt, highlight);

    /* FIXME: handle !R_Interactive */
    if (R_Interactive || !R_Slave) {
#ifdef HAVE_LIBREADLINE
	if (UsingReadline) {
	    rl_callback_handler_install (new_prompt, r_gnome_rl_lhandler);
	}
	else
#endif /* HAVE_LIBREADLINE */
	    {
		write_terminal (new_prompt, strlen (new_prompt));
	    }
    }
    free (new_prompt);

    read_status = read_terminal (buf, buflen, hist);

    write_terminal (postlight, strlen (postlight));

    if (!read_status) {
	fprintf (stderr, "R_ReadConsole: end (EOF)\n");
	return 0;
    }

    fprintf (stderr, "R_ReadConsole: end\n");
    return 1;
}

/**
 *  This function writes the given buffer out to the console.  No
 *  special actions are required.
 **/
static char line_buf[IO_BUF_SIZE];
static int line_pos = 0;

void  R_WriteConsole (char *buf, int buflen)
{
    fprintf (stderr, "R_WriteConsole: start\n");
    if (line_pos + buflen >= IO_BUF_SIZE) {
	R_FlushConsole ();
	write_terminal (buf, buflen);
    }
    else {
	strncpy (line_buf + line_pos, buf, buflen);
	line_pos += buflen;
    }
    fprintf (stderr, "R_WriteConsole: end\n");
}

void  R_FlushConsole (void)
{
    fprintf (stderr, "R_FlushConsole: start\n");
    write_terminal (line_buf, line_pos);
    line_pos = 0;
    fprintf (stderr, "R_FlushConsole: end\n");
}

/**
 *  Unused functions.
 **/
void  R_ResetConsole (void)
{
}
void  R_ClearerrConsole (void)
{
}

/**
 *  Display the (multi-line) message in the string s. This might be
 *  printed on an error stream or displayed in a message dialog box:
 *  it should be brought to the user's attention immediately.  If
 *  GNOME has not been initialised, then the message is queued and
 *  displayed later.
 **/
void R_ShowMessage (char *s)
{
    GtkWidget *main_window, *dialog;
    gchar *sc;

    if (r_gnome_initialised) {
	dialog = gnome_message_box_new (s,
					GNOME_MESSAGE_BOX_WARNING,
					GNOME_STOCK_BUTTON_OK,
					NULL);
	if (main_xml) {
	    main_window = glade_xml_get_widget (main_xml, "main_window");
	    gnome_dialog_set_parent (GNOME_DIALOG (dialog),
				     GTK_WINDOW (main_window));
	}
	gtk_window_set_modal (GTK_WINDOW (dialog), TRUE);
	gnome_dialog_set_default (GNOME_DIALOG (dialog), 0);
	gnome_dialog_run_and_close (GNOME_DIALOG (dialog));
    }
    else {
	sc = g_strdup (s);
	messages_list = g_list_append (messages_list,
				       (gpointer) sc);
    }
}

/**
 * Display any messages that had earlier been queued. This function
 * must not be called unless GNOME has been initialised.
 **/
static void R_ShowQueuedMessages ()
{
    GList *l;

    for (l = messages_list; l != NULL; l = l->next) {
	R_ShowMessage ((char *) l->data);
	g_free (l->data);
    }
    g_list_free (messages_list);
    messages_list = NULL;
}

/**
 *  3) ACTIONS DURING (LONG) COMPUTATIONS
 **/

/**
 *  This function invokes actions (such as change of cursor) when
 *  R embarks on an extended computation (which=1) and when such a
 *  state terminates (which=0).
 **/
void  R_Busy (int which)
{
    GtkWidget *main_window, *statusbar;

    main_window = glade_xml_get_widget (main_xml, "main_window");
    statusbar = GNOME_APP (main_window)->statusbar;
    if (which == 1) {
	gnome_appbar_push (GNOME_APPBAR (statusbar), "Working...");
	while (gtk_events_pending ())
	    gtk_main_iteration ();
    }
    else {
	gnome_appbar_pop (GNOME_APPBAR (statusbar));
    }
}

/**
 *  4) INITIALIZATION AND TERMINATION ACTIONS
 **/

/**
 *  This function invokes any actions which occur at system termination.
 **/
void  R_CleanUp (int saveact, int status, int runLast)
{
    GtkWidget *main_window, *dialog;
    GladeXML *dialog_xml;
    gint which; /* yes = 0, no = 1, cancel = 2 || -1 */

    if (saveact == SA_DEFAULT) /* The normal case apart from R_Suicide */
	saveact = SaveAction;

    if (saveact == SA_SAVEASK) {
	if (R_Interactive) {
	    main_window = glade_xml_get_widget (main_xml,
						"main_window");
	    dialog_xml = glade_xml_new (r_get_glade_file (),
					"quit_messagebox");
	    dialog = glade_xml_get_widget (dialog_xml,
					   "quit_messagebox");

	    gnome_dialog_set_parent (GNOME_DIALOG (dialog),
				     GTK_WINDOW (main_window));
	    
	    which = gnome_dialog_run_and_close (GNOME_DIALOG (dialog));
	    switch(which) {
	    case 0:
		saveact = SA_SAVE;
		break;
	    case 1:
		saveact = SA_NOSAVE;
		break;
	    default:
		jump_to_toplevel ();
		break;
	    }
	}
	else saveact = SaveAction;
    }

    /* save GUI preferences */
    r_save_prefs ();

    switch (saveact) {
    case SA_SAVE:
	if (runLast)
	    R_dot_Last ();
	if (R_DirtyImage)
	    R_SaveGlobalEnv ();
#ifdef HAVE_LIBREADLINE
#ifdef HAVE_READLINE_HISTORY_H
	if(R_Interactive && UsingReadline) {
	    stifle_history(R_HistorySize);
	    write_history(R_HistoryFile);
	}
#endif
#endif
	break;
    case SA_NOSAVE:
	if (runLast)
	    R_dot_Last ();
	break;
    case SA_SUICIDE:
    default:
	break;
    }


    KillAllDevices ();
    fpu_setup (0);

    exit (status);
}

/**
 *  5) FILESYSTEM INTERACTION
 **/

/**
 *  This function is used to display the contents of files.  The file
 *  will be displayed in a window with the given title.
 *
 *     nfile   = number of files
 *     file    = array of filenames
 *     headers = the `headers' args of file.show. Printed before each file.
 *     wtitle  = title for window: the `title' arg of file.show
 *     del     = flag for whether files should be deleted after use
 *     pager   = pager to be used.
 **/

static void showfiles_print (GtkWidget *widget, gpointer data)
{
    r_gnome_not_impl ();
}

static void showfiles_top (GtkWidget *widget, gpointer data)
{
    GdkEventKey event;
    gboolean retval;

    event.keyval = GDK_Home;
    event.state = GDK_CONTROL_MASK;

    gtk_signal_emit_by_name (GTK_OBJECT (widget),
			     "key_press_event",
			     &event, &retval);
}

static void showfiles_pageup (GtkWidget *widget, gpointer data)
{
    GdkEventKey event;
    gboolean retval;

    event.keyval = GDK_Page_Up;

    gtk_signal_emit_by_name (GTK_OBJECT (widget),
			     "key_press_event",
			     &event, &retval);
}

static void showfiles_pagedown (GtkWidget *widget, gpointer data)
{
    GdkEventKey event;
    gboolean retval;

    event.keyval = GDK_Page_Down;

    gtk_signal_emit_by_name (GTK_OBJECT (widget),
			     "key_press_event",
			     &event, &retval);
}

static void showfiles_bottom (GtkWidget *widget, gpointer data)
{
    GdkEventKey event;
    gboolean retval;

    event.keyval = GDK_End;
    event.state = GDK_CONTROL_MASK;

    gtk_signal_emit_by_name (GTK_OBJECT (widget),
			     "key_press_event",
			     &event, &retval);
}

int R_ShowFiles (int nfile, char **file, char **title, char *wtitle,
		 int del, char *pager)
{
    GladeXML *show_xml;
    GtkWidget *show_window, *show_text;
    GtkWidget *print, *copy, *top, *pageup, *pagedown, *bottom, *close;

    gchar *realtitle;
    gchar buf[IO_BUF_SIZE];
    gint i, fd, readlen;
    gchar *j, *k;
    gboolean emmode;
    gchar *modestart;

    /* FIXME: set width and height based on font */
    /* FIXME: colours and fonts from preferences */

    if (nfile < 1)
	return 0;

    if ((wtitle != NULL) && (*wtitle != '\0'))
	realtitle = wtitle;
    else
	realtitle = "R pager";

    show_xml = glade_xml_new (r_get_glade_file (), "showfiles_window");
    show_window = glade_xml_get_widget (show_xml, "showfiles_window");
    show_text = glade_xml_get_widget (show_xml, "showfiles_text");

    print = glade_xml_get_widget (show_xml, "print_button");
    copy = glade_xml_get_widget (show_xml, "copy_button");
    top = glade_xml_get_widget (show_xml, "top_button");
    pageup = glade_xml_get_widget (show_xml, "page_up_button");
    pagedown = glade_xml_get_widget (show_xml, "page_down_button");
    bottom = glade_xml_get_widget (show_xml, "bottom_button");
    close = glade_xml_get_widget (show_xml, "close_button");    

    gtk_signal_connect (GTK_OBJECT (show_window), "delete_event",
			(GtkSignalFunc) gtk_widget_destroy, NULL);
    gtk_signal_connect_object (GTK_OBJECT (print), "clicked",
			       (GtkSignalFunc) showfiles_print,
			       GTK_OBJECT (show_text));
    gtk_signal_connect_object (GTK_OBJECT (copy), "clicked",
			       (GtkSignalFunc) gtk_editable_copy_clipboard,
			       GTK_OBJECT (show_text));
    gtk_signal_connect_object (GTK_OBJECT (top), "clicked",
			       (GtkSignalFunc) showfiles_top,
			       GTK_OBJECT (show_text));
    gtk_signal_connect_object (GTK_OBJECT (pageup), "clicked",
			       (GtkSignalFunc) showfiles_pageup,
			       GTK_OBJECT (show_text));
    gtk_signal_connect_object (GTK_OBJECT (pagedown), "clicked",
			       (GtkSignalFunc) showfiles_pagedown,
			       GTK_OBJECT (show_text));
    gtk_signal_connect_object (GTK_OBJECT (bottom), "clicked",
			       (GtkSignalFunc) showfiles_bottom,
			       GTK_OBJECT (show_text));
    gtk_signal_connect_object (GTK_OBJECT (close), "clicked",
			       (GtkSignalFunc) gtk_widget_destroy,
			       GTK_OBJECT (show_window));

    for(i = 0; i < nfile; i++) {
	if((title[i] != NULL) && (*title[i] != '\0')) {
	    g_snprintf(buf, IO_BUF_SIZE, "%s\n\n", title[i]);
	    /*gtk_text_insert(GTK_TEXT(show_text), 
			    titlefont,
			    &textcolor, 
			    &bgcolor,
			    buf, strlen(buf));*/
	    gtk_text_insert(GTK_TEXT(show_text), 
			    NULL,
			    NULL,
			    NULL,
			    buf, strlen(buf));
	}
	if((fd = open(file[i], O_RDONLY, "")) != -1) {
	    do {
		readlen = read(fd, buf, IO_BUF_SIZE);
		
		emmode = FALSE;
		modestart = buf;
		
		/* strip backspaced stuff */
		if(*buf == '\b')
		    *buf = ' ';
		for(j = buf, k = buf; j < buf + readlen; j++) {
		    if(*j == '\b') {
			k--;
			if(k != modestart)
			    gtk_text_insert(GTK_TEXT(show_text), NULL, NULL, NULL,
					    modestart, k - modestart);
			modestart = k;
			emmode = TRUE;
		    }
		    else {
			*k = *j;
			k++;
			if(emmode) {
			    /*gtk_text_insert(GTK_TEXT(show_text),
					    emfont,
					    NULL, 
					    NULL,
					    k - 1, 1);*/
			    gtk_text_insert(GTK_TEXT(show_text),
					    NULL,
					    NULL, 
					    NULL,
					    k - 1, 1);
			    modestart = k;
			    emmode = FALSE;
			}
		    }
		}
		
		gtk_text_insert(GTK_TEXT(show_text), NULL, NULL, NULL,
				modestart, k - modestart);
	    } while(readlen == IO_BUF_SIZE);
	}
	else {
	    g_snprintf(buf, IO_BUF_SIZE, "NO FILE %s\n\n", file[i]);
	    gtk_text_insert(GTK_TEXT(show_text), NULL, NULL, NULL,
			    buf, strlen(buf));
	}
    }

    gtk_widget_show_all (show_window);

    return 0;
}

/**
 *  Choose a file and return its name in buf of length len.
 *
 *  Argument new is designed to choose the style of dialog box
 *  presented to the user: at present only new = 0 is used. (In 
 *  file.choose(new), new is logical.)
 **/

static void get_filename (GtkWidget *widget, gpointer user_data)
{
    struct _r_choose_file_data *data;

    data = (struct _r_choose_file_data *) user_data;

    strncpy (data->buf,
	     gtk_file_selection_get_filename (data->filesel),
	     data->len);
}

int R_ChooseFile (int new, char *buf, int len)
{
    GladeXML *filesel_xml;
    GtkWidget *filesel, *window;
    struct _r_choose_file_data *data;

    filesel_xml = glade_xml_new (r_get_glade_file (), "choose_file_fileselection");
    filesel = glade_xml_get_widget (filesel_xml, "choose_file_fileselection");
    window = glade_xml_get_widget (main_xml, "main_window");

    data = (struct _r_choose_file_data *)
	malloc (sizeof (struct _r_choose_file_data));

    data->filesel = GTK_FILE_SELECTION (filesel);
    data->buf = buf;
    data->buf[0] = '\0';
    data->len = len;

    gtk_signal_connect (GTK_OBJECT (GTK_FILE_SELECTION (filesel)->ok_button),
			"clicked", GTK_SIGNAL_FUNC (get_filename),
			(gpointer) data);
    gtk_signal_connect_after (GTK_OBJECT (GTK_FILE_SELECTION (filesel)->ok_button),
			"clicked", GTK_SIGNAL_FUNC (gtk_main_quit),
			NULL);
    gtk_signal_connect (GTK_OBJECT (GTK_FILE_SELECTION (filesel)->cancel_button),
			"clicked", GTK_SIGNAL_FUNC (gtk_main_quit),
			NULL);

    gtk_widget_show (filesel);

    gtk_main ();

    gtk_widget_destroy (filesel);
    free (data);

    return strlen (buf);
}


/**
 *  8) GNOME SPECIFIC FUNCTIONS
 **/

/* realize signal handler for the terminal widget */
static void set_terminal_hints (GtkWidget *widget)
{
    ZvtTerm *term;
    GdkGeometry hints;
    GtkWidget *app;
    
    g_assert (widget != NULL);
    term = ZVT_TERM (widget);
    
    app = gtk_widget_get_toplevel (widget);
    g_assert (app != NULL);
    
#define PADDING 2
    hints.base_width = (GTK_WIDGET (term)->style->klass->xthickness * 2) + PADDING;
    hints.base_height =  (GTK_WIDGET (term)->style->klass->ythickness * 2);
    
    hints.width_inc = term->charwidth;
    hints.height_inc = term->charheight;
    hints.min_width = hints.base_width + hints.width_inc;
    hints.min_height = hints.base_height + hints.height_inc;
    
    gtk_window_set_geometry_hints (GTK_WINDOW (app),  GTK_WIDGET (term),
				   &hints, GDK_HINT_RESIZE_INC |
				   GDK_HINT_MIN_SIZE | GDK_HINT_BASE_SIZE);
}

gboolean term_configure_event (GtkWidget *widget,
			       GdkEventConfigure *event,
			       gpointer user_data)
{
    gtk_widget_queue_draw (widget);

    return FALSE;
}

/* readline variables */
extern int screenwidth, screenheight, screenchars;

gboolean term_size_allocate (GtkWidget *widget,
			     GtkAllocation *allocation)
{
    int width, height;
    int gridwidth, basewidth, gridheight, baseheight;

    /* FIXME: tell readline about the new size */

    /* FIXME: tell R about the new size */
    /*R_SetOptionWidth (width);*/

    return FALSE;
}

/*  r_gnome_create_terminal is called by libglade to construct the zvt widget */
GtkWidget *r_gnome_create_terminal ()
{
    GtkWidget *term;
    int infd[2], outfd[2];

    term = zvt_term_new ();
    zvt_term_set_size(ZVT_TERM (term), 80, 24);

    /* set window manager hints */
    gtk_signal_connect_after (GTK_OBJECT (term), "realize",
			      GTK_SIGNAL_FUNC (set_terminal_hints),
			      term);

    /* Setup the I/O */
    pipe(infd);
    pipe(outfd);
    ZVT_TERM (term)->vx->vt.keyfd = infd[1];
    ZVT_TERM (term)->vx->vt.childfd = outfd[0];
    fcntl (infd[0], F_SETFL, O_NONBLOCK);
    fcntl (outfd[0], F_SETFL, O_NONBLOCK);
    gtk_input_add_full (infd[0], GDK_INPUT_READ, incoming_keys,
			NULL, NULL, NULL);
    gtk_input_add_full (outfd[0], GDK_INPUT_READ, output_text,
			NULL, NULL, NULL);

    commandfd = infd[1];

#ifdef HAVE_LIBREADLINE
    if (UsingReadline) {
	rl_instream = fdopen (infd[0], "r");
	rl_outstream = fdopen (outfd[1], "w");
    }
#endif /* HAVE_LIBREADLINE */

    return term;
}

/* load_main_window is called by main to load up the main window */
static void load_main_window (void)
{
    GtkWidget *window, *term, *vscrollbar;

    main_xml = glade_xml_new (r_get_glade_file (), "main_window");
    window = glade_xml_get_widget (main_xml, "main_window");
    term = glade_xml_get_widget (main_xml, "terminal");
    vscrollbar = glade_xml_get_widget (main_xml, "terminal_vscrollbar");

    gtk_range_set_adjustment (GTK_RANGE (vscrollbar),
			      GTK_ADJUSTMENT (ZVT_TERM (term)->adjustment));

    /* handle resizes */
    gtk_signal_connect_object (GTK_OBJECT (window), "configure_event",
			       GTK_SIGNAL_FUNC (term_configure_event),
			       GTK_OBJECT (term));
    gtk_signal_connect_after (GTK_OBJECT (term), "size_allocate",
			      GTK_SIGNAL_FUNC (term_size_allocate),
			      NULL);

    gtk_widget_realize (window);

    /* FIXME: set zvt_term stuff from the preferences */
    zvt_term_set_blink (ZVT_TERM (term), FALSE);
    zvt_term_set_scroll_on_keystroke (ZVT_TERM (term), TRUE);
    zvt_term_set_scroll_on_output (ZVT_TERM (term), FALSE);
    zvt_term_set_wordclass (ZVT_TERM (term), "");
    /* FIXME: replace with zvt_term_set_color_scheme from prefs */
    /*    zvt_term_set_default_color_scheme (ZVT_TERM (term));*/
    zvt_term_set_scrollback (ZVT_TERM (term), 1000);
    /* FIXME: add zvt_term_set_fonts */
    zvt_term_set_background (ZVT_TERM (term), NULL, FALSE, 0);
    zvt_term_set_del_key_swap (ZVT_TERM (term), FALSE);
    zvt_term_set_bell (ZVT_TERM (term), TRUE);
    /* FIXME: zvt_term_match_add stuff */

    gtk_widget_show_all (window);

    r_gnome_connect_main_signals (main_xml);

    while (gtk_events_pending ())
	gtk_main_iteration ();
}

/**
 *  9. Exported GNOME functions
 **/

/*  Return the file name for the Glade interfaces */
gchar *r_get_glade_file (void)
{
    return r_glade_file;
}

GladeXML *r_get_main_xml (void)
{
    return main_xml;
}

void r_gnome_not_impl (void)
{
    GladeXML *not_impl_xml;
    GtkWidget *dialog, *main_window;
    
    not_impl_xml = glade_xml_new (r_get_glade_file (),
				  "not_impl_messagebox");
    dialog = glade_xml_get_widget (not_impl_xml,
				   "not_impl_messagebox");
    main_window = glade_xml_get_widget (main_xml, "main_window");
    gnome_dialog_set_parent (GNOME_DIALOG (dialog),
			     GTK_WINDOW (main_window));
    gnome_dialog_run_and_close (GNOME_DIALOG (dialog));
}

void r_send_command (char *command, int len)
{
    write (commandfd, command, len);
}

/*  Extra command line arguments */
static int popt_arg_no_readline = 0;

static const struct poptOption popt_options[] = {
    { "no-readline", '\0', POPT_ARG_NONE, &popt_arg_no_readline, "Don't use readline for command-line editing.", NULL },
    { NULL, '\0', 0, NULL, 0, NULL, NULL }
};

/*  Main function */
int main (int ac, char **av)
{
    int value, ierr;
    char *p, msg[1024];
    structRstart rstart;
    Rstart Rp = &rstart;
    struct stat sb;

#ifdef HAVE_TIMES
    setStartTime ();
#endif

    R_DefParams (Rp);
    R_SizeFromEnv (Rp);

    /* initialise libgnome */
    gnomelib_init ("R",
		   g_strdup_printf ("%s.%s %s (%s %s %s)",
				    R_MAJOR, R_MINOR, R_STATUS,
				    R_MONTH, R_DAY, R_YEAR));
    r_load_initial_prefs (Rp, &UsingReadline);

    /* process R command line options */
    R_set_command_line_arguments (ac, av, Rp);
    R_common_command_line (&ac, av, Rp);

    /* initialise libgnomeui */
    gnome_init_with_popt_table ("R",
				g_strdup_printf ("%s.%s %s (%s %s %s)",
						 R_MAJOR, R_MINOR, R_STATUS,
						 R_MONTH, R_DAY, R_YEAR),
				ac, av, popt_options, 0, NULL);
    if (popt_arg_no_readline)
	UsingReadline = 0;
    glade_gnome_init ();
    r_gnome_initialised = TRUE;

    /* set the parameters */
    R_SetParams (Rp);

    r_load_gui_prefs ();

    R_ShowQueuedMessages ();

    R_Interactive = isatty (0);
    R_Sinkfile = NULL;
    if ((R_Home = R_HomeDir ()) == NULL) {
	R_Suicide ("R home directory is not defined");
    }
    r_glade_file = g_strconcat (R_Home, GLADE_INTERFACE_FILE, NULL);
    if (stat (r_glade_file, &sb) == -1) {
	R_Suicide ("Glade interface file not found");
    }
    if (!R_Interactive && SaveAction != SA_SAVE && SaveAction != SA_NOSAVE) {
	R_Suicide ("You must specify `--save', `--no-save' or `--vanilla'");
    }
    if ((R_HistoryFile = getenv ("R_HISTFILE")) == NULL) {
	R_HistoryFile = ".Rhistory";
    }
    R_HistorySize = 512;
    if ((p = getenv ("R_HISTSIZE"))) {
	value = Decode2Long (p, &ierr);
	if (ierr != 0 || value < 0)
	    R_ShowMessage ("Warning: invalid R_HISTSIZE ignored.");
	else
	    R_HistorySize = value;
    }

#ifdef HAVE_LIBREADLINE
#ifdef HAVE_READLINE_HISTORY_H
    if (R_Interactive && UsingReadline) {
	read_history(R_HistoryFile);
    }
#endif
#endif

    fpu_setup (1);

    /* create GUI */    
    load_main_window ();

    /* R main loop */
    mainloop ();

    return 0;
}

/* Declarations to keep f77 happy */
int MAIN_()  { return 0; }
int MAIN__() { return 0; }
int __main() { return 0; }
