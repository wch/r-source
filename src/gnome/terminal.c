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

#include "terminal.h"
#include "terminal-find.h"
#include "terminal-functions.h"
#include "terminal-toolbar.h"
#include "terminal-menu.h"

/* ************************************************
   GTK+ general stuff
************************************************ */

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

  GtkStyle *textstyle;

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

  textstyle = gtk_style_copy(gtk_widget_get_style(R_gtk_terminal_text));
  textstyle->font = gdk_font_load(R_gnome_userprefs.font);
  textstyle->text[GTK_STATE_NORMAL] = R_gnome_userprefs.textcolor;
  textstyle->base[GTK_STATE_NORMAL] = R_gnome_userprefs.bgcolor;
  gtk_widget_set_style(R_gtk_terminal_text, textstyle);

  /* tell R how many columns we've got */
  charw = gdk_char_width(R_gtk_terminal_text->style->font, 'w');
  winw = 83 * charw;
  winh = 350;
  gtk_widget_set_usize(R_gtk_terminal_text, winw, winh);

  /*  R_SetOptionWidth(floor((double)winw / (double)charw)); */

  gtk_text_set_editable (GTK_TEXT (R_gtk_terminal_text), TRUE);
  GTK_CONSOLE(R_gtk_terminal_text)->buffer_type = CONSOLE_BUF_LINE;
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

  /* initialise list */
  R_gtk_editfiles = NULL;

  R_gtk_gui_quit = FALSE;

  /* Initialise the find code */
  R_gtk_terminal_find_init();

  /* finish up */
  return;
}

void R_gnome_load_prefs(void)
{
  gchar *tmp;
  GdkColor text, bg;

  /* Text settings */
  gnome_config_push_prefix("/R.gnome/Text/");

  R_gnome_userprefs.font = gnome_config_get_string("font=fixed");

  tmp = gnome_config_get_string("textcolor=black");
  if(gdk_color_parse(tmp, &text) == 0) {
    gdk_color_parse("black", &text);
  }
  R_gnome_userprefs.textcolor = text;

  tmp = gnome_config_get_string("bgcolor=white");
  if(gdk_color_parse(tmp, &bg) == 0) {
    gdk_color_parse("white", &bg);
  }
  R_gnome_userprefs.bgcolor = bg;

  gnome_config_pop_prefix();

  /* Pager settings */
  gnome_config_push_prefix("/R.gnome/Pager/");

  R_gnome_userprefs.pager_title_font = gnome_config_get_string("title_font=-adobe-helvetica-bold-r-normal-*-*-100-*-*-p-*-iso8859-1");

  tmp = gnome_config_get_string("title_textcolor=black");
  if(gdk_color_parse(tmp, &text) == 0) {
    gdk_color_parse("black", &text);
  }
  R_gnome_userprefs.pager_title_textcolor = text;

  tmp = gnome_config_get_string("title_bgcolor=white");
  if(gdk_color_parse(tmp, &bg) == 0) {
    gdk_color_parse("white", &bg);
  }
  R_gnome_userprefs.pager_title_bgcolor = bg;

  R_gnome_userprefs.pager_text_font = gnome_config_get_string("text_font=-misc-fixed-medium-r-normal-*-*-120-*-*-c-*-iso8859-1");
  R_gnome_userprefs.pager_em_font = gnome_config_get_string("em_font=-misc-fixed-bold-r-normal-*-*-120-*-*-c-*-iso8859-1");

  tmp = gnome_config_get_string("text_textcolor=black");
  if(gdk_color_parse(tmp, &text) == 0) {
    gdk_color_parse("black", &text);
  }
  R_gnome_userprefs.pager_text_textcolor = text;

  tmp = gnome_config_get_string("text_bgcolor=white");
  if(gdk_color_parse(tmp, &bg) == 0) {
    gdk_color_parse("white", &bg);
  }
  R_gnome_userprefs.pager_text_bgcolor = bg;

  gnome_config_pop_prefix();
}

void R_gnome_save_prefs(void)
{
  GdkColor tmp;

  /* Text settings */
  gnome_config_push_prefix("/R.gnome/Text/");

  gnome_config_set_string("font", R_gnome_userprefs.font);
  tmp = R_gnome_userprefs.textcolor;
  gnome_config_set_string("textcolor",
			  g_strdup_printf("rgb:%04x/%04x/%04x", tmp.red, tmp.green, tmp.blue));
  tmp = R_gnome_userprefs.bgcolor;
  gnome_config_set_string("bgcolor",
			  g_strdup_printf("rgb:%04x/%04x/%04x", tmp.red, tmp.green, tmp.blue));

  gnome_config_pop_prefix();

  /* Pager settings */
  gnome_config_push_prefix("/R.gnome/Pager/");

  gnome_config_set_string("title_font", R_gnome_userprefs.pager_title_font);
  tmp = R_gnome_userprefs.pager_title_textcolor;
  gnome_config_set_string("title_textcolor",
			  g_strdup_printf("rgb:%04x/%04x/%04x", tmp.red, tmp.green, tmp.blue));
  tmp = R_gnome_userprefs.pager_title_bgcolor;
  gnome_config_set_string("title_bgcolor",
			  g_strdup_printf("rgb:%04x/%04x/%04x", tmp.red, tmp.green, tmp.blue));

  gnome_config_set_string("text_font", R_gnome_userprefs.pager_text_font);
  gnome_config_set_string("em_font", R_gnome_userprefs.pager_em_font);
  tmp = R_gnome_userprefs.pager_text_textcolor;
  gnome_config_set_string("text_textcolor",
			  g_strdup_printf("rgb:%04x/%04x/%04x", tmp.red, tmp.green, tmp.blue));
  tmp = R_gnome_userprefs.pager_text_bgcolor;
  gnome_config_set_string("text_bgcolor",
			  g_strdup_printf("rgb:%04x/%04x/%04x", tmp.red, tmp.green, tmp.blue));

  gnome_config_pop_prefix();

  gnome_config_sync();
}

