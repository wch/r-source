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

#include <glade/glade.h>
#include <gnome.h>

#include "Defn.h"

#include "prefs.h"
#include "rgnome.h"


#define PREFS_BUF_SIZE 512

typedef struct _r_gnome_prefs_t r_gnome_prefs_t;
struct _r_gnome_prefs_t {
    /** Console **/
    char *console_font_name;
    GdkFont *console_font;
    gboolean console_bold_user_input;
    gboolean console_color_user_input;
    gboolean console_color_syntax; /* unused */
    GdkColor console_text_color;
    GdkColor console_input_color;
    GdkColor console_syntax_color; /* unused */
    GdkColor console_bg_color;

    /** Pager **/
    char *pager_title_font_name;
    GdkFont *pager_title_font;
    char *pager_text_font_name;
    GdkFont *pager_text_font;
    char *pager_emphasis_font_name;
    GdkFont *pager_emphasis_font;
    GdkColor pager_title_color;
    GdkColor pager_text_color;
    GdkColor pager_emphasis_color;

    /** Startup **/
    int startup_restore_workspace;
    gboolean startup_use_readline;

    /** Exit **/
    int exit_save_workspace;
};

static r_gnome_prefs_t r_gnome_prefs;

static void r_gnome_config_set_color (char *key, GdkColor *color);
static void set_widgets_from_prefs (GladeXML *prefs_xml);
static void connect_changed_handlers (GladeXML *prefs_xml,
				      GtkWidget *prefs_propbox);
static void on_prefs_widget_changed (GtkWidget *widget,
				     gpointer user_data);
static void on_color_picker_changed (GtkWidget *colorpicker,
				     guint r, guint g, guint b, guint a,
				     gpointer user_data);
static void on_font_picker_changed (GtkWidget *fontpicker,
				    gchar *font_name,
				    gpointer user_data);
static void on_prefs_propertybox_apply (GnomePropertyBox *propertybox,
					gint pagenum,
					gpointer user_data);
static GdkColor *gnome_config_get_color (char *key);


/**
 *  r_gnome_config_set_color sets key to the string version of color.
 **/
static void r_gnome_config_set_color (char *key, GdkColor *color)
{
    char buf[PREFS_BUF_SIZE];

    snprintf (buf, PREFS_BUF_SIZE, "rgb:%04x/%04x/%04x",
	      color->red, color->green, color->blue);
    buf[PREFS_BUF_SIZE - 1] = '\0';
    gnome_config_set_string (key, buf);
}

/**
 *  r_gnome_config_set_color fills in color with the parsed version
 *  of the color string found in key.
 **/
static void r_gnome_config_get_color (char *key, GdkColor *color)
{
    char *retbuf;

    retbuf = gnome_config_get_string (key);
    gdk_color_parse (retbuf, color);
    g_free (retbuf);
}

/**
 *  r_load_initial_prefs loads the preferences relating to R startup,
 *  consisting of the save and restore workspace settings, and the use
 *  of readline.
 **/
void r_load_initial_prefs (Rstart Rp, int *UseReadline)
{
    /** Startup **/
    gnome_config_push_prefix ("/R/Startup/");

    r_gnome_prefs.startup_restore_workspace =
	gnome_config_get_int ("restore_workspace=-1");

    r_gnome_prefs.startup_use_readline =
	gnome_config_get_bool ("use_readline=true");

    gnome_config_pop_prefix ();

    switch (r_gnome_prefs.startup_restore_workspace) {
    case SA_NORESTORE:
    case SA_RESTORE:
	break;

    default:
	r_gnome_prefs.startup_restore_workspace = SA_RESTORE;
	break;
    }

    Rp->RestoreAction = r_gnome_prefs.startup_restore_workspace;
    *UseReadline = (int) r_gnome_prefs.startup_use_readline;

    /** Exit **/
    gnome_config_push_prefix ("/R/Exit/");

    r_gnome_prefs.exit_save_workspace =
	gnome_config_get_int ("exit_save_workspace=-1");

    gnome_config_pop_prefix ();

    switch (r_gnome_prefs.exit_save_workspace) {
    case SA_NOSAVE:
    case SA_SAVE:
    case SA_SAVEASK:
	break;

    default:
	r_gnome_prefs.exit_save_workspace = SA_SAVEASK;
	break;
    }

    Rp->SaveAction = r_gnome_prefs.exit_save_workspace;
}

/**
 *  r_load_gui_prefs loads the preferences not loaded by
 *  r_load_initial_prefs, which control the GUI.
 **/
void r_load_gui_prefs (void)
{
    /** Console **/
    gnome_config_push_prefix ("/R/Console");

    r_gnome_prefs.console_font_name =
	gnome_config_get_string ("console_font=fixed");
    r_gnome_prefs.console_font =
	gdk_font_load (r_gnome_prefs.console_font_name);

    r_gnome_prefs.console_bold_user_input =
	gnome_config_get_bool ("console_bold_user_input=false");
    r_gnome_prefs.console_color_user_input =
        gnome_config_get_bool ("console_color_user_input=true");

    r_gnome_config_get_color ("console_text_color=rgb:0000/0000/0000",
			      &r_gnome_prefs.console_text_color);
    r_gnome_config_get_color ("console_input_color=rgb:0000/8888/0000",
			      &r_gnome_prefs.console_input_color);
    r_gnome_config_get_color ("console_bg_color=rgb:ffff/ffff/ffff",
			      &r_gnome_prefs.console_bg_color);
    
    gnome_config_pop_prefix ();

    /** Pager **/
    gnome_config_push_prefix ("/R/Pager");

    r_gnome_prefs.pager_title_font_name =
	gnome_config_get_string ("pager_title_font=fixed");
    r_gnome_prefs.pager_title_font =
	gdk_font_load (r_gnome_prefs.pager_title_font_name);

    r_gnome_prefs.pager_text_font_name =
	gnome_config_get_string ("pager_text_font=fixed");
    r_gnome_prefs.pager_text_font =
	gdk_font_load (r_gnome_prefs.pager_text_font_name);

    r_gnome_prefs.pager_emphasis_font_name =
	gnome_config_get_string ("pager_emphasis_font=fixed");
    r_gnome_prefs.pager_emphasis_font =
	gdk_font_load (r_gnome_prefs.pager_emphasis_font_name);

    r_gnome_config_get_color ("pager_title_color=rgb:0000/0000/0000",
			      &r_gnome_prefs.pager_title_color);
    r_gnome_config_get_color ("pager_text_color=rgb:0000/8888/0000",
			      &r_gnome_prefs.pager_text_color);
    r_gnome_config_get_color ("pager_emphasis_color=rgb:ffff/ffff/ffff",
			      &r_gnome_prefs.pager_emphasis_color);
    
    gnome_config_pop_prefix ();
}

/**
 *  r_save_prefs saves all the preferences.
 **/
void r_save_prefs (void)
{
    /** Console **/
    gnome_config_push_prefix ("/R/Console");
    gnome_config_set_string ("console_font",
			     r_gnome_prefs.console_font_name);
    gnome_config_set_bool ("console_bold_user_input",
			   r_gnome_prefs.console_bold_user_input);
    gnome_config_set_bool ("console_color_user_input",
			   r_gnome_prefs.console_color_user_input);
    r_gnome_config_set_color ("console_text_color",
			      &r_gnome_prefs.console_text_color);
    r_gnome_config_set_color ("console_input_color",
			      &r_gnome_prefs.console_input_color);
    r_gnome_config_set_color ("console_bg_color",
			      &r_gnome_prefs.console_bg_color);
    gnome_config_pop_prefix ();

    /** Pager **/
    gnome_config_push_prefix ("/R/Pager");
    gnome_config_set_string ("pager_title_font",
			     r_gnome_prefs.pager_title_font_name);
    gnome_config_set_string ("pager_text_font",
			     r_gnome_prefs.pager_text_font_name);
    gnome_config_set_string ("pager_emphasis_font",
			     r_gnome_prefs.pager_emphasis_font_name);
    r_gnome_config_set_color ("pager_title_color",
			      &r_gnome_prefs.pager_title_color);
    r_gnome_config_set_color ("pager_text_color",
			      &r_gnome_prefs.pager_text_color);
    r_gnome_config_set_color ("pager_emphasis_color",
			      &r_gnome_prefs.pager_emphasis_color);
    gnome_config_pop_prefix ();

    /** Startup **/
    gnome_config_push_prefix ("/R/Startup/");
    gnome_config_set_int ("restore_workspace",
			  r_gnome_prefs.startup_restore_workspace);
    gnome_config_set_bool ("use_readline",
			   r_gnome_prefs.startup_use_readline);
    gnome_config_pop_prefix ();

    /** Exit **/
    gnome_config_push_prefix ("/R/Exit/");
    gnome_config_set_int ("save_workspace",
			  r_gnome_prefs.exit_save_workspace);
    gnome_config_pop_prefix ();
}

/**
 *  set_widgets_from_prefs sets the state of each widget in the
 *  preferences dialog to be consistent with the currently operative
 *  preferences.  Used to initialise the dialog.
 **/
static void set_widgets_from_prefs (GladeXML *prefs_xml)
{
    GtkWidget *widget;

    /** Console **/
    widget = glade_xml_get_widget (prefs_xml,
				   "console_font");
    gnome_font_picker_set_font_name (GNOME_FONT_PICKER (widget),
				     r_gnome_prefs.console_font_name);

    widget = glade_xml_get_widget (prefs_xml,
				   "console_bold_checkbutton");
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (widget),
				  r_gnome_prefs.console_bold_user_input);

    widget = glade_xml_get_widget (prefs_xml,
				   "console_color_checkbutton");
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (widget),
				  r_gnome_prefs.console_color_user_input);

    widget = glade_xml_get_widget (prefs_xml,
				   "console_text_color");
    gnome_color_picker_set_i16 (GNOME_COLOR_PICKER (widget),
				r_gnome_prefs.console_text_color.red,
				r_gnome_prefs.console_text_color.green,
				r_gnome_prefs.console_text_color.blue, 0);

    widget = glade_xml_get_widget (prefs_xml,
				   "console_input_color");
    gnome_color_picker_set_i16 (GNOME_COLOR_PICKER (widget),
				r_gnome_prefs.console_input_color.red,
				r_gnome_prefs.console_input_color.green,
				r_gnome_prefs.console_input_color.blue, 0);

    widget = glade_xml_get_widget (prefs_xml,
				   "console_bg_color");
    gnome_color_picker_set_i16 (GNOME_COLOR_PICKER (widget),
				r_gnome_prefs.console_bg_color.red,
				r_gnome_prefs.console_bg_color.green,
				r_gnome_prefs.console_bg_color.blue, 0);

    /** Pager **/

    /** Startup page **/
    switch (r_gnome_prefs.startup_restore_workspace) {
    case SA_RESTORE:
	widget = glade_xml_get_widget (prefs_xml,
				       "startup_always_restore_radio");
	gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (widget),
				      TRUE);
	break;

    case SA_NORESTORE:
	widget = glade_xml_get_widget (prefs_xml,
				       "startup_never_restore_radio");
	gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (widget),
				      TRUE);
	break;
    }
    
    widget = glade_xml_get_widget (prefs_xml,
				   "startup_readline_checkbutton");
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (widget),
				  r_gnome_prefs.startup_use_readline);

    /** Exit page **/
    switch (r_gnome_prefs.exit_save_workspace) {
    case SA_SAVEASK:
	widget = glade_xml_get_widget (prefs_xml,
				       "exit_prompt_save_radio");
	gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (widget),
				      TRUE);
	break;

    case SA_SAVE:
	widget = glade_xml_get_widget (prefs_xml,
				       "exit_always_save_radio");
	gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (widget),
				      TRUE);
	break;

    case SA_NOSAVE:
	widget = glade_xml_get_widget (prefs_xml,
				       "exit_never_save_radio");
	gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (widget),
				      TRUE);
	break;
    }
}

/**
 *  connect_changed_handlers connects signal handlers for each widget
 *  so that when a preference is changed, the "changed" signal is sent
 *  to the dialog.
 **/
static void connect_changed_handlers (GladeXML *prefs_xml,
				      GtkWidget *prefs_propbox)
{
    GtkWidget *widget;

    /** Console page **/
    widget = glade_xml_get_widget (prefs_xml,
				   "console_font");
    gtk_signal_connect (GTK_OBJECT (widget), "font-set",
			(GtkSignalFunc) on_font_picker_changed,
			(gpointer) prefs_propbox);

    widget = glade_xml_get_widget (prefs_xml,
				   "console_bold_checkbutton");
    gtk_signal_connect (GTK_OBJECT (widget), "clicked",
			(GtkSignalFunc) on_prefs_widget_changed,
			(gpointer) prefs_propbox);

    widget = glade_xml_get_widget (prefs_xml,
				   "console_color_checkbutton");
    gtk_signal_connect (GTK_OBJECT (widget), "clicked",
			(GtkSignalFunc) on_prefs_widget_changed,
			(gpointer) prefs_propbox);

    widget = glade_xml_get_widget (prefs_xml,
				   "console_text_color");
    gtk_signal_connect (GTK_OBJECT (widget), "color-set",
			(GtkSignalFunc) on_color_picker_changed,
			(gpointer) prefs_propbox);

    widget = glade_xml_get_widget (prefs_xml,
				   "console_input_color");
    gtk_signal_connect (GTK_OBJECT (widget), "color-set",
			(GtkSignalFunc) on_color_picker_changed,
			(gpointer) prefs_propbox);

    widget = glade_xml_get_widget (prefs_xml,
				   "console_bg_color");
    gtk_signal_connect (GTK_OBJECT (widget), "color-set",
			(GtkSignalFunc) on_color_picker_changed,
			(gpointer) prefs_propbox);

    /** Pager page **/
    widget = glade_xml_get_widget (prefs_xml,
				   "pager_title_font");
    gtk_signal_connect (GTK_OBJECT (widget), "font-set",
			(GtkSignalFunc) on_font_picker_changed,
			(gpointer) prefs_propbox);

    widget = glade_xml_get_widget (prefs_xml,
				   "pager_text_font");
    gtk_signal_connect (GTK_OBJECT (widget), "font-set",
			(GtkSignalFunc) on_font_picker_changed,
			(gpointer) prefs_propbox);

    widget = glade_xml_get_widget (prefs_xml,
				   "pager_emphasis_font");
    gtk_signal_connect (GTK_OBJECT (widget), "font-set",
			(GtkSignalFunc) on_font_picker_changed,
			(gpointer) prefs_propbox);
    
    widget = glade_xml_get_widget (prefs_xml,
				   "pager_title_color");
    gtk_signal_connect (GTK_OBJECT (widget), "color-set",
			(GtkSignalFunc) on_color_picker_changed,
			(gpointer) prefs_propbox);

    widget = glade_xml_get_widget (prefs_xml,
				   "pager_text_color");
    gtk_signal_connect (GTK_OBJECT (widget), "color-set",
			(GtkSignalFunc) on_color_picker_changed,
			(gpointer) prefs_propbox);

    widget = glade_xml_get_widget (prefs_xml,
				   "pager_bg_color");
    gtk_signal_connect (GTK_OBJECT (widget), "color-set",
			(GtkSignalFunc) on_color_picker_changed,
			(gpointer) prefs_propbox);

    /** Startup page **/
    widget = glade_xml_get_widget (prefs_xml,
				   "startup_always_restore_radio");
    gtk_signal_connect (GTK_OBJECT (widget), "clicked",
			(GtkSignalFunc) on_prefs_widget_changed,
			(gpointer) prefs_propbox);

    widget = glade_xml_get_widget (prefs_xml,
				   "startup_never_restore_radio");
    gtk_signal_connect (GTK_OBJECT (widget), "clicked",
			(GtkSignalFunc) on_prefs_widget_changed,
			(gpointer) prefs_propbox);

    widget = glade_xml_get_widget (prefs_xml,
				   "startup_readline_checkbutton");
    gtk_signal_connect (GTK_OBJECT (widget), "clicked",
			(GtkSignalFunc) on_prefs_widget_changed,
			(gpointer) prefs_propbox);

    /** Exit page **/
    widget = glade_xml_get_widget (prefs_xml,
				   "exit_prompt_save_radio");
    gtk_signal_connect (GTK_OBJECT (widget), "clicked",
			(GtkSignalFunc) on_prefs_widget_changed,
			(gpointer) prefs_propbox);

    widget = glade_xml_get_widget (prefs_xml,
				   "exit_always_save_radio");
    gtk_signal_connect (GTK_OBJECT (widget), "clicked",
			(GtkSignalFunc) on_prefs_widget_changed,
			(gpointer) prefs_propbox);

    widget = glade_xml_get_widget (prefs_xml,
				   "exit_never_save_radio");
    gtk_signal_connect (GTK_OBJECT (widget), "clicked",
			(GtkSignalFunc) on_prefs_widget_changed,
			(gpointer) prefs_propbox);

}

/**
 *  on_prefs_widget_changed is called whenever a widget's state
 *  changes.  It tells the property box that a widget has changed.
 **/
static void on_prefs_widget_changed (GtkWidget *widget,
				     gpointer user_data)
{
    GtkWidget *prefs_propbox;

    prefs_propbox = GTK_WIDGET (user_data);
    gnome_property_box_changed (GNOME_PROPERTY_BOX (prefs_propbox));
}

static void on_color_picker_changed (GtkWidget *colorpicker,
				     guint r, guint g, guint b, guint a,
				     gpointer user_data)
{
    GtkWidget *prefs_propbox;

    prefs_propbox = GTK_WIDGET (user_data);
    gnome_property_box_changed (GNOME_PROPERTY_BOX (prefs_propbox));
}

static void on_font_picker_changed (GtkWidget *fontpicker,
				    gchar *font_name,
				    gpointer user_data)
{
    GtkWidget *prefs_propbox;

    prefs_propbox = GTK_WIDGET (user_data);
    gnome_property_box_changed (GNOME_PROPERTY_BOX (prefs_propbox));
}

/**
 *  on_prefs_propertybox_apply is the signal handler for the "apply"
 *  signal from the property dialog.  This signal is emitted by the
 *  dialog when the user presses the Apply or OK buttons.
 **/
static void on_prefs_propertybox_apply (GnomePropertyBox *propertybox,
					gint pagenum,
					gpointer user_data)
{
    GladeXML *prefs_xml;
    GtkWidget *widget;

    prefs_xml = (GladeXML *) user_data;

    switch (pagenum) {
	/** Console page **/
    case 0:
	break;

	/** Pager page **/
    case 1:
	break;

	/** Startup page **/
    case 2:
	widget = glade_xml_get_widget (prefs_xml,
				       "startup_always_restore_radio");
	if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (widget))) {
	    r_gnome_prefs.startup_restore_workspace = SA_RESTORE;
	}
	widget = glade_xml_get_widget (prefs_xml,
				       "startup_never_restore_radio");
	if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (widget))) {
	    r_gnome_prefs.startup_restore_workspace = SA_NORESTORE;
	}

	widget = glade_xml_get_widget (prefs_xml,
				       "startup_readline_checkbutton");
	if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (widget))) {
	    r_gnome_prefs.startup_use_readline = TRUE;
	}
	else {
	    r_gnome_prefs.startup_use_readline = FALSE;
	}
	break;

	/** Exit page **/
    case 3:
	widget = glade_xml_get_widget (prefs_xml,
				       "exit_prompt_save_radio");
	if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (widget))) {
	    r_gnome_prefs.exit_save_workspace = SA_SAVEASK;
	}
	widget = glade_xml_get_widget (prefs_xml,
				       "exit_always_save_radio");
	if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (widget))) {
	    r_gnome_prefs.exit_save_workspace = SA_SAVE;
	}
	widget = glade_xml_get_widget (prefs_xml,
				       "exit_never_save_radio");
	if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (widget))) {
	    r_gnome_prefs.exit_save_workspace = SA_NOSAVE;
	}
	break;

	/** All pages **/
    default:
	break;
    }
}

/**
 *  r_prefs_dialog creates the preferences dialog and sets up its
 *  widgets.
 **/
void r_prefs_dialog (void)
{
    GladeXML *prefs_xml, *main_xml;
    GtkWidget *prefs_propbox, *main_window;

    prefs_xml = glade_xml_new (r_get_glade_file (),
			       "prefs_propertybox");
    main_xml = r_get_main_xml ();
    prefs_propbox = glade_xml_get_widget (prefs_xml,
					  "prefs_propertybox");
    main_window = glade_xml_get_widget (main_xml,
					"main_window");

    set_widgets_from_prefs (prefs_xml);
    connect_changed_handlers (prefs_xml, prefs_propbox);

    gnome_dialog_set_parent (GNOME_DIALOG (prefs_propbox),
			     GTK_WINDOW (main_window));
    gtk_window_set_title (GTK_WINDOW (prefs_propbox),
			  "R Preferences");
    gtk_signal_connect (GTK_OBJECT (prefs_propbox), "apply",
		        (GtkSignalFunc) on_prefs_propertybox_apply,
			(gpointer) prefs_xml);

    gtk_widget_show (prefs_propbox);
}

