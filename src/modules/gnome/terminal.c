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

#define __GNOME_TERMINAL_C__
#include "terminal.h"
#undef __GNOME_TERMINAL_C__

#include "terminal-find.h"
#include "terminal-functions.h"
#include "terminal-toolbar.h"
#include "terminal-menu.h"
#include "terminal-prefs.h"

/* ************************************************
   GTK+ general stuff
************************************************ */

void terminal_set_style(void) {

    gtk_object_set (GTK_OBJECT(R_gtk_terminal_text),
		    "output_color_gdk", prefs_get_console_outputcolor(),
		    "input_color_gdk",  prefs_get_console_textcolor(),
		    "bg_color_gdk",     prefs_get_console_bgcolor(),
		    "font",             prefs_get_console_font(),
		    NULL);
}

static gint delete_event(GtkWidget *widget, GdkEvent *event, gpointer data)
{
  R_gtk_terminal_quit();
  return TRUE;
}

/*
  Create and display an R terminal window.
  Assumes that there will only be one R terminal per application.
*/
void R_gtk_terminal_new()
{
  GtkWidget *table;
  GtkWidget *vscrollbar;

  gint charw, winw, winh;

  /* Setup main window */
  R_gtk_main_window = gnome_app_new("R.gnome", "R Console");

  /*  gtk_widget_set_usize(R_gtk_main_window, 600, 500);*/

  gtk_window_set_policy(GTK_WINDOW(R_gtk_main_window), TRUE, TRUE, FALSE);
  gtk_widget_realize(R_gtk_main_window);

  /* Add a status bar to the bottom of the window */
  R_gtk_terminal_appbar = gnome_appbar_new(FALSE, TRUE, GNOME_PREFERENCES_USER);
  gnome_app_set_statusbar(GNOME_APP(R_gtk_main_window), R_gtk_terminal_appbar);

  /* Add the menubar */
  R_gtk_terminal_add_menu(R_gtk_main_window);

  /* Add the toolbar */
  R_gtk_terminal_add_toolbar(R_gtk_main_window);

  /* Create a layout table */
  /* This table controls the layout of the text widget and its */
  /* associated scrollbar. */
  table = gtk_table_new(1, 2, FALSE);
  gtk_table_set_col_spacing (GTK_TABLE (table), 0, 2);
  
  /* Create the the console widget */
  R_gtk_terminal_text = gtk_console_new (NULL, NULL);

  terminal_set_style();

  /* tell R how many columns we've got */
  charw = gdk_char_width(R_gtk_terminal_text->style->font, 'w');
  winw = 83 * charw;
  winh = 350;
  gtk_widget_set_usize(R_gtk_terminal_text, winw, winh);

  /*  R_SetOptionWidth(floor((double)winw / (double)charw)); */

  gtk_text_set_editable (GTK_TEXT (R_gtk_terminal_text), TRUE);
  GTK_CONSOLE(R_gtk_terminal_text)->buffer_type = CONSOLE_BUF_BLOCK;
  gtk_table_attach (GTK_TABLE (table), R_gtk_terminal_text, 0, 1, 0, 1,
		    GTK_EXPAND | GTK_SHRINK | GTK_FILL,
		    GTK_EXPAND | GTK_SHRINK | GTK_FILL, 0, 0);

  /* Add a vertical scrollbar to the GtkText widget */
  vscrollbar = gtk_vscrollbar_new (GTK_TEXT (R_gtk_terminal_text)->vadj);
  gtk_table_attach (GTK_TABLE (table), vscrollbar, 1, 2, 0, 1,
		    GTK_FILL, GTK_EXPAND | GTK_SHRINK | GTK_FILL, 0, 0);

  /* Set the table as the contents of the main window */
  gnome_app_set_contents(GNOME_APP(R_gtk_main_window), table);

  /* Add signal handlers */
  gtk_signal_connect(GTK_OBJECT(R_gtk_main_window), "delete_event", GTK_SIGNAL_FUNC(delete_event), NULL);

  gtk_widget_grab_focus(R_gtk_terminal_text);

  /* Show the window */
  gtk_widget_show_all(R_gtk_main_window);

#if 0
  /* initialise list */
  R_gtk_editfiles = NULL;

  R_gtk_gui_quit = FALSE;
#endif

  /* Initialise the find code */
  R_gtk_terminal_find_init();

  /* finish up */
  return;
}
