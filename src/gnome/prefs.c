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

#include "rgnome.h"

struct _r_gnome_prefs_t {
};

typedef struct _r_gnome_prefs_t r_gnome_prefs_t;

static r_gnome_prefs_t r_gnome_prefs;

void r_load_initial_prefs (void)
{
}

void r_load_gui_prefs (void)
{
}

void r_save_prefs (void)
{
}

static void set_widgets_from_prefs (GladeXML *prefs_xml)
{
}

static void set_prefs_from_widgets (GladeXML *prefs_xml)
{
}

static void on_prefs_propertybox_apply (GnomePropertyBox *propertybox,
					gint pagenum,
					gpointer user_data)
{
    switch (pagenum) {
    case 0:  /* console */
	break;

    case 1:  /* pager */
	break;

    case 2:  /* startup */
	break;

    case 3:  /* exit */
	break;

    default:
	break;
    }
}

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

    gnome_dialog_set_parent (GNOME_DIALOG (prefs_propbox),
			     GTK_WINDOW (main_window));

    gtk_widget_show (prefs_propbox);
}

