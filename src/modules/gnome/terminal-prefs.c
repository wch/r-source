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

#include "Defn.h"
#include "Startup.h"

#include "terminal-prefs.h"
#include "terminal.h"

#include <glade/glade.h>
#include <glib.h>


/* saved user preferences */
typedef struct _gui_preferences gui_preferences;
struct _gui_preferences {
  int restoreact;

  int saveact;

  gchar *console_font;
  GdkColor console_textcolor;
  GdkColor console_bgcolor;

  gchar *pager_title_font;
  GdkColor pager_title_textcolor;
  GdkColor pager_title_bgcolor;
  gchar *pager_text_font;
  gchar *pager_em_font;
  GdkColor pager_text_textcolor;
  GdkColor pager_text_bgcolor;
};


static gui_preferences user_prefs;


void R_gnome_prefs_gui_load(void)
{
  gchar *tmp;
  GdkColor text, bg;

  /* Text settings */
  gnome_config_push_prefix("/R/Console/");
  /*  -misc-fixed-medium-r-semicondensed-*-*-120-*-*-c-*-iso8859-1
      -*-fixed-*-*-*-*-*-*-*-*-*-*-*-*    */
    /*  user_prefs.console_font = gnome_config_get_string("font=fixed");*/
  user_prefs.console_font = gnome_config_get_string("font=-misc-fixed-medium-*-semicondensed-*-*-120-*-*-*-*-iso8859-1");

  tmp = gnome_config_get_string("textcolor=black");
  if(gdk_color_parse(tmp, &text) == 0) {
    gdk_color_parse("black", &text);
  }
  user_prefs.console_textcolor = text;

  tmp = gnome_config_get_string("bgcolor=white");
  if(gdk_color_parse(tmp, &bg) == 0) {
    gdk_color_parse("white", &bg);
  }
  user_prefs.console_bgcolor = bg;

  gnome_config_pop_prefix();

  /* Pager settings */
  gnome_config_push_prefix("/R/Pager/");

  user_prefs.pager_title_font = gnome_config_get_string("title_font=-adobe-helvetica-bold-r-normal-*-*-100-*-*-p-*-iso8859-1");

  tmp = gnome_config_get_string("title_textcolor=black");
  if(gdk_color_parse(tmp, &text) == 0) {
    gdk_color_parse("black", &text);
  }
  g_free(tmp);
  user_prefs.pager_title_textcolor = text;

  tmp = gnome_config_get_string("title_bgcolor=white");
  if(gdk_color_parse(tmp, &bg) == 0) {
    gdk_color_parse("white", &bg);
  }
  g_free(tmp);
  user_prefs.pager_title_bgcolor = bg;

  user_prefs.pager_text_font = gnome_config_get_string("text_font=-misc-fixed-medium-r-normal-*-*-120-*-*-c-*-iso8859-1");
  user_prefs.pager_em_font = gnome_config_get_string("em_font=-misc-fixed-bold-r-normal-*-*-120-*-*-c-*-iso8859-1");

  tmp = gnome_config_get_string("text_textcolor=black");
  if(gdk_color_parse(tmp, &text) == 0) {
    gdk_color_parse("black", &text);
  }
  g_free(tmp);
  user_prefs.pager_text_textcolor = text;

  tmp = gnome_config_get_string("text_bgcolor=white");
  if(gdk_color_parse(tmp, &bg) == 0) {
    gdk_color_parse("white", &bg);
  }
  g_free(tmp);
  user_prefs.pager_text_bgcolor = bg;

  gnome_config_pop_prefix();    
}

void R_gnome_prefs_cmd_load(int defrestoreact, int defsaveact)
{
  gchar *tmp;

  /* Startup settings */
  gnome_config_push_prefix("/R/Startup/");

  tmp = g_strdup_printf("restoreact=%d", defrestoreact);
  user_prefs.restoreact = gnome_config_get_int(tmp);
  g_free(tmp);

  switch(user_prefs.restoreact) {
  case SA_NORESTORE:
  case SA_RESTORE:
      break;

  default:
      user_prefs.restoreact = defrestoreact;
      break;
  }

  gnome_config_pop_prefix();

  /* Exit settings */
  gnome_config_push_prefix("/R/Exit/");

  tmp = g_strdup_printf("saveact=%d", defsaveact);
  user_prefs.saveact = gnome_config_get_int(tmp);
  g_free(tmp);

  switch(user_prefs.saveact) {
  case SA_DEFAULT:
  case SA_NOSAVE:
  case SA_SAVE:
  case SA_SAVEASK:
      break;

  default:
      user_prefs.saveact = defsaveact;
      break;
  }

  gnome_config_pop_prefix();
}

void R_gnome_prefs_save(void)
{
  gchar *tmp;
  GdkColor color;

  /* Text settings */
  gnome_config_push_prefix("/R/Console/");

  gnome_config_set_string("font", user_prefs.console_font);

  color = user_prefs.console_textcolor;
  tmp = g_strdup_printf("rgb:%04x/%04x/%04x", color.red, color.green, color.blue);
  gnome_config_set_string("textcolor", tmp);
  g_free(tmp);
  
  color = user_prefs.console_bgcolor;
  tmp = g_strdup_printf("rgb:%04x/%04x/%04x", color.red, color.green, color.blue);
  gnome_config_set_string("bgcolor", tmp);
  g_free(tmp);

  gnome_config_pop_prefix();

  /* Pager settings */
  gnome_config_push_prefix("/R/Pager/");

  gnome_config_set_string("title_font", user_prefs.pager_title_font);

  color = user_prefs.pager_title_textcolor;
  tmp = g_strdup_printf("rgb:%04x/%04x/%04x", color.red, color.green, color.blue);
  gnome_config_set_string("title_textcolor", tmp);
  g_free(tmp);
  
  color = user_prefs.pager_title_bgcolor;
  tmp = g_strdup_printf("rgb:%04x/%04x/%04x", color.red, color.green, color.blue);
  gnome_config_set_string("title_bgcolor", tmp);
  g_free(tmp);

  gnome_config_set_string("text_font", user_prefs.pager_text_font);

  gnome_config_set_string("em_font", user_prefs.pager_em_font);

  color = user_prefs.pager_text_textcolor;
  tmp = g_strdup_printf("rgb:%04x/%04x/%04x", color.red, color.green, color.blue);
  gnome_config_set_string("text_textcolor", tmp);
  g_free(tmp);
  
  color = user_prefs.pager_text_bgcolor;
  tmp = g_strdup_printf("rgb:%04x/%04x/%04x", color.red, color.green, color.blue);
  gnome_config_set_string("text_bgcolor", tmp);
  g_free(tmp);

  gnome_config_pop_prefix();

  /* Startup settings */
  gnome_config_push_prefix("/R/Startup/");

  gnome_config_set_int("restoreact", user_prefs.restoreact);

  gnome_config_pop_prefix();

  /* Exit settings */
  gnome_config_push_prefix("/R/Exit/");

  gnome_config_set_int("saveact", user_prefs.saveact);

  gnome_config_pop_prefix();
  
  /* Write the config file */
  gnome_config_sync();
}


/* Access functions */

int prefs_get_restoreact(void)
{
    return user_prefs.restoreact;
}

int prefs_get_saveact(void)
{
    return user_prefs.saveact;
}

gchar *prefs_get_console_font(void)
{
    return user_prefs.console_font;
}

GdkColor prefs_get_console_textcolor(void)
{
    return user_prefs.console_textcolor;
}

GdkColor prefs_get_console_bgcolor(void)
{
    return user_prefs.console_bgcolor;
}

gchar *prefs_get_pager_title_font(void)
{
    return user_prefs.pager_title_font;
}

GdkColor prefs_get_pager_title_textcolor(void)
{
    return user_prefs.pager_title_textcolor;
}

GdkColor prefs_get_pager_title_bgcolor(void)
{
    return user_prefs.pager_title_bgcolor;
}

gchar *prefs_get_pager_text_font(void)
{
    return user_prefs.pager_text_font;
}
  GtkWidget *frame, *table;
  GtkWidget *ask;
  GtkWidget *save;
  GtkWidget *savetofile, *dummy, *nameentry;
  GtkWidget *dontsave;

gchar *prefs_get_pager_em_font(void)
{
    return user_prefs.pager_em_font;
}

GdkColor prefs_get_pager_text_textcolor(void)
{
    return user_prefs.pager_text_textcolor;
}

GdkColor prefs_get_pager_text_bgcolor(void)
{
    return user_prefs.pager_text_bgcolor;
}


/* Dialog functions */

static void widget_changed_cb(GtkWidget *widget, gpointer user_data)
{
    g_return_if_fail(user_data != NULL);
    g_return_if_fail(GNOME_IS_PROPERTY_BOX(user_data));

    gnome_property_box_changed(GNOME_PROPERTY_BOX(user_data));
}

static void font_picker_changed_cb(GtkWidget *widget, gchar *font_name, gpointer user_data)
{
    g_return_if_fail(user_data != NULL);
    g_return_if_fail(GNOME_IS_PROPERTY_BOX(user_data));

    gnome_property_box_changed(GNOME_PROPERTY_BOX(user_data));
}

static void color_picker_changed_cb(GtkWidget *widget, guint r, guint g, guint b, guint a, gpointer user_data)
{
    g_return_if_fail(user_data != NULL);
    g_return_if_fail(GNOME_IS_PROPERTY_BOX(user_data));

    gnome_property_box_changed(GNOME_PROPERTY_BOX(user_data));
}

static void console_page_init(GtkWidget *prefs_dialog, GladeXML *prefs_xml)
{
    GtkWidget *console_font, *console_text_color, *console_bg_color;

    console_font = glade_xml_get_widget(prefs_xml, "prefs_console_font");
    console_text_color = glade_xml_get_widget(prefs_xml, "prefs_console_text_color");
    console_bg_color = glade_xml_get_widget(prefs_xml, "prefs_console_bg_color");

    gnome_font_picker_set_font_name(GNOME_FONT_PICKER(console_font),
				    user_prefs.console_font);
    gnome_color_picker_set_i16(GNOME_COLOR_PICKER(console_text_color),
			       user_prefs.console_textcolor.red,
			       user_prefs.console_textcolor.green,
			       user_prefs.console_textcolor.blue,
			       0);
    gnome_color_picker_set_i16(GNOME_COLOR_PICKER(console_bg_color),
			       user_prefs.console_bgcolor.red,
			       user_prefs.console_bgcolor.green,
			       user_prefs.console_bgcolor.blue,
			       0);

    gtk_signal_connect(GTK_OBJECT(console_font), "font-set",
		       font_picker_changed_cb, prefs_dialog);
    gtk_signal_connect(GTK_OBJECT(console_text_color), "color-set",
		       color_picker_changed_cb, prefs_dialog);
    gtk_signal_connect(GTK_OBJECT(console_bg_color), "color-set",
		       color_picker_changed_cb, prefs_dialog);
}

static void console_page_apply(GladeXML *prefs_xml) {
    GtkWidget *console_font, *console_text_color, *console_bg_color;
    gchar *font_name;
    gushort r, g, b, a;
    gboolean change;

    change = FALSE;

    console_font = glade_xml_get_widget(prefs_xml, "prefs_console_font");
    console_text_color = glade_xml_get_widget(prefs_xml, "prefs_console_text_color");
    console_bg_color = glade_xml_get_widget(prefs_xml, "prefs_console_bg_color");

    font_name = gnome_font_picker_get_font_name(GNOME_FONT_PICKER(console_font));
    if(strcmp(user_prefs.console_font, font_name)) {
	g_free(user_prefs.console_font);
	user_prefs.console_font = g_strdup(font_name);
	change = TRUE;
    }

    gnome_color_picker_get_i16(GNOME_COLOR_PICKER(console_text_color), &r, &g, &b, &a);
    if((user_prefs.console_textcolor.red != r) || (user_prefs.console_textcolor.green != g)
       || (user_prefs.console_textcolor.blue != b)) {
	user_prefs.console_textcolor.red = r;
	user_prefs.console_textcolor.green = g;
	user_prefs.console_textcolor.blue = b;
	change = TRUE;
    }

    gnome_color_picker_get_i16(GNOME_COLOR_PICKER(console_bg_color), &r, &g, &b, &a);
    if((user_prefs.console_bgcolor.red != r) || (user_prefs.console_bgcolor.green != g)
       || (user_prefs.console_bgcolor.blue != b)) {
	user_prefs.console_bgcolor.red = r;
	user_prefs.console_bgcolor.green = g;
	user_prefs.console_bgcolor.blue = b;
	change = TRUE;
    }

    if(change == TRUE) {
	terminal_set_style();
    }
}

static void pager_page_init(GtkWidget *prefs_dialog, GladeXML *prefs_xml)
{
    GtkWidget *title_font, *title_text_color, *title_bg_color;
    GtkWidget *body_font, *body_emphasis_font, *body_text_color, *body_bg_color;

    title_font = glade_xml_get_widget(prefs_xml, "prefs_title_font");
    title_text_color = glade_xml_get_widget(prefs_xml, "prefs_title_text_color");
    title_bg_color = glade_xml_get_widget(prefs_xml, "prefs_title_bg_color");

    body_font = glade_xml_get_widget(prefs_xml, "prefs_body_font");
    body_emphasis_font = glade_xml_get_widget(prefs_xml, "prefs_body_emphasis_font");
    body_text_color = glade_xml_get_widget(prefs_xml, "prefs_body_text_color");
    body_bg_color = glade_xml_get_widget(prefs_xml, "prefs_body_bg_color");

    gnome_font_picker_set_font_name(GNOME_FONT_PICKER(title_font),
				    user_prefs.pager_title_font);
    gnome_color_picker_set_i16(GNOME_COLOR_PICKER(title_text_color),
			       user_prefs.pager_title_textcolor.red,
			       user_prefs.pager_title_textcolor.green,
			       user_prefs.pager_title_textcolor.blue,
			       0);
    gnome_color_picker_set_i16(GNOME_COLOR_PICKER(title_bg_color),
			       user_prefs.pager_title_bgcolor.red,
			       user_prefs.pager_title_bgcolor.green,
			       user_prefs.pager_title_bgcolor.blue,
			       0);

    gnome_font_picker_set_font_name(GNOME_FONT_PICKER(body_font),
				    user_prefs.pager_text_font);
    gnome_font_picker_set_font_name(GNOME_FONT_PICKER(body_emphasis_font),
				    user_prefs.pager_em_font);
    gnome_color_picker_set_i16(GNOME_COLOR_PICKER(body_text_color),
			       user_prefs.pager_text_textcolor.red,
			       user_prefs.pager_text_textcolor.green,
			       user_prefs.pager_text_textcolor.blue,
			       0);
    gnome_color_picker_set_i16(GNOME_COLOR_PICKER(body_bg_color),
			       user_prefs.pager_text_bgcolor.red,
			       user_prefs.pager_text_bgcolor.green,
			       user_prefs.pager_text_bgcolor.blue,
			       0);

    gtk_signal_connect(GTK_OBJECT(title_font), "font-set",
		       font_picker_changed_cb, prefs_dialog);
    gtk_signal_connect(GTK_OBJECT(title_text_color), "color-set",
		       color_picker_changed_cb, prefs_dialog);
    gtk_signal_connect(GTK_OBJECT(title_bg_color), "color-set",
		       color_picker_changed_cb, prefs_dialog);
    gtk_signal_connect(GTK_OBJECT(body_font), "font-set",
		       font_picker_changed_cb, prefs_dialog);
    gtk_signal_connect(GTK_OBJECT(body_emphasis_font), "font-set",
		       font_picker_changed_cb, prefs_dialog);
    gtk_signal_connect(GTK_OBJECT(body_text_color), "color-set",
		       color_picker_changed_cb, prefs_dialog);
    gtk_signal_connect(GTK_OBJECT(body_bg_color), "color-set",
		       color_picker_changed_cb, prefs_dialog);
}

static void pager_page_apply(GladeXML *prefs_xml)
{
    GtkWidget *title_font, *title_text_color, *title_bg_color;
    GtkWidget *body_font, *body_emphasis_font, *body_text_color, *body_bg_color;
    gchar *font_name;
    gushort r, g, b, a;
    gboolean change;

    change = FALSE;

    title_font = glade_xml_get_widget(prefs_xml, "prefs_title_font");
    title_text_color = glade_xml_get_widget(prefs_xml, "prefs_title_text_color");
    title_bg_color = glade_xml_get_widget(prefs_xml, "prefs_title_bg_color");

    body_font = glade_xml_get_widget(prefs_xml, "prefs_body_font");
    body_emphasis_font = glade_xml_get_widget(prefs_xml, "prefs_body_emphasis_font");
    body_text_color = glade_xml_get_widget(prefs_xml, "prefs_body_text_color");
    body_bg_color = glade_xml_get_widget(prefs_xml, "prefs_body_bg_color");

    font_name = gnome_font_picker_get_font_name(GNOME_FONT_PICKER(title_font));
    if(strcmp(user_prefs.pager_title_font, font_name)) {
	g_free(user_prefs.pager_title_font);
	user_prefs.pager_title_font = g_strdup(font_name);
	change = TRUE;
    }

    gnome_color_picker_get_i16(GNOME_COLOR_PICKER(title_text_color), &r, &g, &b, &a);
    if((user_prefs.pager_title_textcolor.red != r)
       || (user_prefs.pager_title_textcolor.green != g)
       || (user_prefs.pager_title_textcolor.blue != b)) {
	user_prefs.pager_title_textcolor.red = r;
	user_prefs.pager_title_textcolor.green = g;
	user_prefs.pager_title_textcolor.blue = b;
	change = TRUE;
    }

    gnome_color_picker_get_i16(GNOME_COLOR_PICKER(title_bg_color), &r, &g, &b, &a);
    if((user_prefs.pager_title_bgcolor.red != r)
       || (user_prefs.pager_title_bgcolor.green != g)
       || (user_prefs.pager_title_bgcolor.blue != b)) {
	user_prefs.pager_title_bgcolor.red = r;
	user_prefs.pager_title_bgcolor.green = g;
	user_prefs.pager_title_bgcolor.blue = b;
	change = TRUE;
    }

    font_name = gnome_font_picker_get_font_name(GNOME_FONT_PICKER(body_font));
    if(strcmp(user_prefs.pager_text_font, font_name)) {
	g_free(user_prefs.pager_text_font);
	user_prefs.pager_text_font = g_strdup(font_name);
	change = TRUE;
    }

    font_name = gnome_font_picker_get_font_name(GNOME_FONT_PICKER(body_emphasis_font));
    if(strcmp(user_prefs.pager_em_font, font_name)) {
	g_free(user_prefs.pager_em_font);
	user_prefs.pager_em_font = g_strdup(font_name);
	change = TRUE;
    }

    gnome_color_picker_get_i16(GNOME_COLOR_PICKER(body_text_color), &r, &g, &b, &a);
    if((user_prefs.pager_text_textcolor.red != r)
       || (user_prefs.pager_text_textcolor.green != g)
       || (user_prefs.pager_text_textcolor.blue != b)) {
	user_prefs.pager_text_textcolor.red = r;
	user_prefs.pager_text_textcolor.green = g;
	user_prefs.pager_text_textcolor.blue = b;
	change = TRUE;
    }

    gnome_color_picker_get_i16(GNOME_COLOR_PICKER(body_bg_color), &r, &g, &b, &a);
    if((user_prefs.pager_text_bgcolor.red != r)
       || (user_prefs.pager_text_bgcolor.green != g)
       || (user_prefs.pager_text_bgcolor.blue != b)) {
	user_prefs.pager_text_bgcolor.red = r;
	user_prefs.pager_text_bgcolor.green = g;
	user_prefs.pager_text_bgcolor.blue = b;
	change = TRUE;
    }

    if(change == TRUE) {
	/* FIXME: update existing help windows */
    }
}

static void startup_page_init(GtkWidget *prefs_dialog, GladeXML *prefs_xml)
{
    GtkWidget *always_ws_radio, *never_ws_radio;

    always_ws_radio = glade_xml_get_widget(prefs_xml, "prefs_always_restore_ws_radio");
    never_ws_radio = glade_xml_get_widget(prefs_xml, "prefs_never_restore_ws_radio");

    switch(user_prefs.restoreact) {
    case SA_NORESTORE:
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(never_ws_radio), TRUE);
	break;

    case SA_RESTORE:
    default:
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(always_ws_radio), TRUE);
	break;
    }

    gtk_signal_connect(GTK_OBJECT(always_ws_radio), "toggled",
		       widget_changed_cb, prefs_dialog);
    gtk_signal_connect(GTK_OBJECT(never_ws_radio), "toggled",
		       widget_changed_cb, prefs_dialog);
}

static void startup_page_apply(GladeXML *prefs_xml)
{
    GtkWidget *always_ws_radio, *never_ws_radio;

    always_ws_radio = glade_xml_get_widget(prefs_xml, "prefs_always_restore_ws_radio");
    never_ws_radio = glade_xml_get_widget(prefs_xml, "prefs_never_restore_ws_radio");

    if(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(always_ws_radio)))
	user_prefs.restoreact = SA_RESTORE;
    else if(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(never_ws_radio)))
	user_prefs.restoreact = SA_NORESTORE;
}

static void exit_page_init(GtkWidget *prefs_dialog, GladeXML *prefs_xml)
{
    GtkWidget *prompt_ws_radio, *always_ws_radio, *never_ws_radio;

    prompt_ws_radio = glade_xml_get_widget(prefs_xml,
					   "prefs_prompt_save_ws_radio");
    always_ws_radio = glade_xml_get_widget(prefs_xml,
					   "prefs_always_save_ws_radio");
    never_ws_radio = glade_xml_get_widget(prefs_xml,
					  "prefs_never_save_ws_radio");

    switch(user_prefs.saveact) {
    case SA_SAVE:
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(always_ws_radio), TRUE);
	break;

    case SA_NOSAVE:
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(never_ws_radio), TRUE);
	break;

    case SA_DEFAULT:
    case SA_SAVEASK:
    default:
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(prompt_ws_radio), TRUE);
	break;
    }

    gtk_signal_connect(GTK_OBJECT(prompt_ws_radio), "toggled",
		       widget_changed_cb, prefs_dialog);
    gtk_signal_connect(GTK_OBJECT(always_ws_radio), "toggled",
		       widget_changed_cb, prefs_dialog);
    gtk_signal_connect(GTK_OBJECT(never_ws_radio), "toggled",
		       widget_changed_cb, prefs_dialog);
}

static void exit_page_apply(GladeXML *prefs_xml)
{
    GtkWidget *prompt_ws_radio, *always_ws_radio, *never_ws_radio;

    prompt_ws_radio = glade_xml_get_widget(prefs_xml, "prefs_prompt_save_ws_radio");
    always_ws_radio = glade_xml_get_widget(prefs_xml, "prefs_always_save_ws_radio");
    never_ws_radio = glade_xml_get_widget(prefs_xml, "prefs_never_save_ws_radio");

    if(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(prompt_ws_radio)))
	user_prefs.saveact = SA_SAVEASK;
    else if(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(always_ws_radio)))
	user_prefs.saveact = SA_SAVE;
    else if(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(never_ws_radio)))
	user_prefs.saveact = SA_NOSAVE;

    R_set_SaveAction(user_prefs.saveact);
}


static void prefs_apply_cb(GnomePropertyBox *property_box, gint page_num, gpointer user_data)
{
    GladeXML *prefs_xml;

    prefs_xml = (GladeXML *) user_data;

    if(page_num == -1) {
	console_page_apply(prefs_xml);
	pager_page_apply(prefs_xml);
	startup_page_apply(prefs_xml);
	exit_page_apply(prefs_xml);

	R_gnome_prefs_save();
    }
}

static gboolean prefs_close_cb(GnomeDialog *dialog, gpointer user_data)
{
    /* free the xml object */
    gtk_object_unref(GTK_OBJECT(user_data));

    return FALSE;
}

void settings_prefs_cb(GtkWidget *widget, gpointer data)
{
    GladeXML *prefs_xml;
    GtkWidget *prefs_dialog;

    /* load xml object */
    prefs_xml = glade_xml_new(glade_interface_file, "prefs_propertybox");

    /* create dialog */
    prefs_dialog = glade_xml_get_widget(prefs_xml, "prefs_propertybox");

    /* setup pages */
    console_page_init(prefs_dialog, prefs_xml);
    pager_page_init(prefs_dialog, prefs_xml);
    startup_page_init(prefs_dialog, prefs_xml);
    exit_page_init(prefs_dialog, prefs_xml);

    /* setup dialog */
    gtk_window_set_title(GTK_WINDOW(prefs_dialog), "R preferences");
    gnome_dialog_set_parent(GNOME_DIALOG(prefs_dialog),
			    GTK_WINDOW(R_gtk_main_window));
    gtk_window_set_modal(GTK_WINDOW(prefs_dialog), TRUE);

    gtk_signal_connect(GTK_OBJECT(prefs_dialog), "apply",
		       (GtkSignalFunc)prefs_apply_cb, (gpointer) prefs_xml);
    gtk_signal_connect(GTK_OBJECT(prefs_dialog), "close",
		       (GtkSignalFunc)prefs_close_cb, (gpointer) prefs_xml);

    /* show the dialog box */
    gtk_widget_show_all(GTK_WIDGET(prefs_dialog));
}


