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

#include "gnome-callbacks.h"
#include "prefs.h"
#include "rgnome.h"


#define R_PROJECT_WEB_SITE "http://www.r-project.org/"


/* Template menu callback */
/*static void on_1_activate (GtkWidget *widget, gpointer user_data)
{
    char *buf;

    buf = "\n";
    r_send_command (buf, strlen (buf));
}*/

/* File menu */
static void on_open1_activate (GtkWidget *widget, gpointer user_data)
{
    r_gnome_not_impl ();
}

static void on_save1_activate (GtkWidget *widget, gpointer user_data)
{
    r_gnome_not_impl ();
}

static void on_save_as1_activate (GtkWidget *widget, gpointer user_data)
{
    r_gnome_not_impl ();
}

static void on_exit1_activate (GtkWidget *widget, gpointer user_data)
{
    r_gnome_exit ();
}

/* Edit menu */
static void on_copy1_activate (GtkWidget *widget, gpointer user_data)
{
    r_gnome_not_impl ();
}

static void on_paste1_activate (GtkWidget *widget, gpointer user_data)
{
    r_gnome_not_impl ();
}

static void on_copy_and_paste1_activate (GtkWidget *widget, gpointer user_data)
{
    r_gnome_not_impl ();
}

/* Settings menu */
static void on_preferences1_activate (GtkWidget *widget, gpointer user_data)
{
    r_prefs_dialog ();
}

/* Help menu */
static void on_html_index1_activate (GtkWidget *widget, gpointer user_data)
{
    char *buf;

    buf = "help.start()\n";
    r_send_command (buf, strlen (buf));
}

static void on_text_index1_activate (GtkWidget *widget, gpointer user_data)
{
    char *buf;

    buf = "help()\n";
    r_send_command (buf, strlen (buf));
}

static void on_r_project_web_site1_activate (GtkWidget *widget, gpointer user_data)
{
    gnome_url_show (R_PROJECT_WEB_SITE);
}

static void on_licence1_activate (GtkWidget *widget, gpointer user_data)
{
    char *buf;

    buf = "?licence\n";
    r_send_command (buf, strlen (buf));
}

static void on_contributors1_activate (GtkWidget *widget, gpointer user_data)
{
    char *buf;

    buf = "?contributors\n";
    r_send_command (buf, strlen (buf));
}

static void on_demos_index1_activate (GtkWidget *widget, gpointer user_data)
{
    char *buf;

    buf = "demo()\n";
    r_send_command (buf, strlen (buf));
}

static void on_graphics1_activate (GtkWidget *widget, gpointer user_data)
{
    char *buf;

    buf = "demo(\"graphics\")\n";
    r_send_command (buf, strlen (buf));
}

static void on_image1_activate (GtkWidget *widget, gpointer user_data)
{
    char *buf;

    buf = "demo(\"image\")\n";
    r_send_command (buf, strlen (buf));
}

static void on_lmglm1_activate (GtkWidget *widget, gpointer user_data)
{
    char *buf;

    buf = "demo(\"lm.glm\")\n";
    r_send_command (buf, strlen (buf));
}

static void on_glmvr1_activate (GtkWidget *widget, gpointer user_data)
{
    char *buf;

    buf = "demo(\"glm.vr\")\n";
    r_send_command (buf, strlen (buf));
}

static void on_nlm1_activate (GtkWidget *widget, gpointer user_data)
{
    char *buf;

    buf = "demo(\"nlm\")\n";
    r_send_command (buf, strlen (buf));
}

static void on_recursion1_activate (GtkWidget *widget, gpointer user_data)
{
    char *buf;

    buf = "demo(\"recursion\")\n";
    r_send_command (buf, strlen (buf));
}

static void on_scoping1_activate (GtkWidget *widget, gpointer user_data)
{
    char *buf;

    buf = "demo(\"scoping\")\n";
    r_send_command (buf, strlen (buf));
}

static void on_isthings1_activate (GtkWidget *widget, gpointer user_data)
{
    char *buf;

    buf = "demo(\"is.things\")\n";
    r_send_command (buf, strlen (buf));
}

static void on_dynload1_activate (GtkWidget *widget, gpointer user_data)
{
    char *buf;

    buf = "demo(\"dyn.load\")\n";
    r_send_command (buf, strlen (buf));

}

static void on_about1_activate (GtkWidget *widget, gpointer user_data)
{
    GladeXML *main_xml, *about_xml;
    GtkWidget *main_window, *about_dialog;

    main_xml = r_get_main_xml ();
    about_xml = glade_xml_new (r_get_glade_file (), "about_dialog");
    main_window = glade_xml_get_widget (main_xml, "main_window");
    about_dialog = glade_xml_get_widget (about_xml, "about_dialog");
    gnome_dialog_set_parent (GNOME_DIALOG (about_dialog),
			     GTK_WINDOW (main_window));
    gnome_dialog_run_and_close (GNOME_DIALOG (about_dialog));
}

static gboolean on_main_window_delete_event (GtkWidget *widget,
					     GdkEvent *event,
					     gpointer user_data)
{
    r_gnome_exit ();

    return TRUE;
}


void r_gnome_connect_main_signals (GladeXML *main_xml)
{
    /* template for connecting a signal handler */
    /*glade_xml_signal_connect (main_xml, "on_1_activate",
			      (GtkSignalFunc) on_1_activate);*/

    glade_xml_signal_connect (main_xml, "on_open1_activate",
			      (GtkSignalFunc) on_open1_activate);
    glade_xml_signal_connect (main_xml, "on_save1_activate",
			      (GtkSignalFunc) on_save1_activate);
    glade_xml_signal_connect (main_xml, "on_save_as1_activate",
			      (GtkSignalFunc) on_save_as1_activate);
    glade_xml_signal_connect (main_xml, "on_exit1_activate",
			      (GtkSignalFunc) on_exit1_activate);

    glade_xml_signal_connect (main_xml, "on_copy1_activate",
			      (GtkSignalFunc) on_copy1_activate);
    glade_xml_signal_connect (main_xml, "on_paste1_activate",
			      (GtkSignalFunc) on_paste1_activate);
    glade_xml_signal_connect (main_xml, "on_copy_and_paste1_activate",
			      (GtkSignalFunc) on_copy_and_paste1_activate);

    glade_xml_signal_connect (main_xml, "on_preferences1_activate",
			      (GtkSignalFunc) on_preferences1_activate);

    glade_xml_signal_connect (main_xml, "on_html_index1_activate",
			      (GtkSignalFunc) on_html_index1_activate);
    glade_xml_signal_connect (main_xml, "on_text_index1_activate",
			      (GtkSignalFunc) on_text_index1_activate);
    glade_xml_signal_connect (main_xml, "on_r_project_web_site1_activate",
			      (GtkSignalFunc) on_r_project_web_site1_activate);
    glade_xml_signal_connect (main_xml, "on_licence1_activate",
			      (GtkSignalFunc) on_licence1_activate);
    glade_xml_signal_connect (main_xml, "on_contributors1_activate",
			      (GtkSignalFunc) on_contributors1_activate);
    glade_xml_signal_connect (main_xml, "on_demos_index1_activate",
			      (GtkSignalFunc) on_demos_index1_activate);
    glade_xml_signal_connect (main_xml, "on_graphics1_activate",
			      (GtkSignalFunc) on_graphics1_activate);
    glade_xml_signal_connect (main_xml, "on_image1_activate",
			      (GtkSignalFunc) on_image1_activate);
    glade_xml_signal_connect (main_xml, "on_lmglm1_activate",
			      (GtkSignalFunc) on_lmglm1_activate);
    glade_xml_signal_connect (main_xml, "on_glmvr1_activate",
			      (GtkSignalFunc) on_glmvr1_activate);
    glade_xml_signal_connect (main_xml, "on_nlm1_activate",
			      (GtkSignalFunc) on_nlm1_activate);
    glade_xml_signal_connect (main_xml, "on_recursion1_activate",
			      (GtkSignalFunc) on_recursion1_activate);
    glade_xml_signal_connect (main_xml, "on_scoping1_activate",
			      (GtkSignalFunc) on_scoping1_activate);
    glade_xml_signal_connect (main_xml, "on_isthings1_activate",
			      (GtkSignalFunc) on_isthings1_activate);
    glade_xml_signal_connect (main_xml, "on_dynload1_activate",
			      (GtkSignalFunc) on_dynload1_activate);
    glade_xml_signal_connect (main_xml, "on_about1_activate",
			      (GtkSignalFunc) on_about1_activate);

    glade_xml_signal_connect (main_xml, "on_main_window_delete_event",
			      (GtkSignalFunc) on_main_window_delete_event);
}

