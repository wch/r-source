/*
 *  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (C) 1998-2002   Lyndon Drake
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
# include <config.h>
#endif

/* Avoid braced-groups warning from -Wall */
#define G_STMT_START do
#define G_STMT_END   while(0)

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
  GdkColor console_input_color; /* was console_textcolor */
  GdkColor console_output_color;
  GdkColor console_bg_color;

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
  GdkColor text_color, bg_color;

  /* Text settings */
  gnome_config_push_prefix("/R/Console/");

  user_prefs.console_font = gnome_config_get_string("font=-misc-fixed-medium-r-semicondensed-*-*-120-*-*-*-*-iso8859-1");
  /* FIXME: What to do if default font is not available? */
  
  tmp = gnome_config_get_string("textcolor=red"); 
  if(gdk_color_parse(tmp, &text_color) == 0) {
    gdk_color_parse("black", &text_color);
  }
  g_free(tmp);
  user_prefs.console_input_color = text_color;

  tmp = gnome_config_get_string("outputcolor=black");
  if(gdk_color_parse(tmp, &text_color) == 0) {
    gdk_color_parse("black", &text_color);
  }
  g_free(tmp);
  user_prefs.console_output_color = text_color;


  tmp = gnome_config_get_string("bgcolor=white");
  if(gdk_color_parse(tmp, &bg_color) == 0) {
    gdk_color_parse("white", &bg_color);
  }
  g_free(tmp);
  user_prefs.console_bg_color = bg_color;

  gnome_config_pop_prefix();

  /* Pager settings */
  gnome_config_push_prefix("/R/Pager/");

  user_prefs.pager_title_font = gnome_config_get_string("title_font=-adobe-helvetica-bold-r-normal-*-*-100-*-*-p-*-iso8859-1");

  tmp = gnome_config_get_string("title_textcolor=black");
  if(gdk_color_parse(tmp, &text_color) == 0) {
    gdk_color_parse("black", &text_color);
  }
  g_free(tmp);
  user_prefs.pager_title_textcolor = text_color;

  tmp = gnome_config_get_string("title_bgcolor=white");
  if(gdk_color_parse(tmp, &bg_color) == 0) {
    gdk_color_parse("white", &bg_color);
  }
  g_free(tmp);
  user_prefs.pager_title_bgcolor = bg_color;

  user_prefs.pager_text_font = gnome_config_get_string("text_font=-misc-fixed-medium-r-normal-*-*-120-*-*-c-*-iso8859-1");
  user_prefs.pager_em_font = gnome_config_get_string("em_font=-misc-fixed-bold-r-normal-*-*-120-*-*-c-*-iso8859-1");

  tmp = gnome_config_get_string("text_textcolor=black");
  if(gdk_color_parse(tmp, &text_color) == 0) {
    gdk_color_parse("black", &text_color);
  }
  g_free(tmp);
  user_prefs.pager_text_textcolor = text_color;

  tmp = gnome_config_get_string("text_bgcolor=white");
  if(gdk_color_parse(tmp, &bg_color) == 0) {
    gdk_color_parse("white", &bg_color);
  }
  g_free(tmp);
  user_prefs.pager_text_bgcolor = bg_color;

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

  color = user_prefs.console_input_color;
  tmp = g_strdup_printf("rgb:%04x/%04x/%04x",
			color.red, color.green, color.blue);
  gnome_config_set_string("textcolor", tmp);
  g_free(tmp);
  
  color = user_prefs.console_output_color;
  tmp = g_strdup_printf("rgb:%04x/%04x/%04x",
			color.red, color.green, color.blue);
  gnome_config_set_string("outputcolor", tmp);
  g_free(tmp);

  color = user_prefs.console_bg_color;
  tmp = g_strdup_printf("rgb:%04x/%04x/%04x",
			color.red, color.green, color.blue);
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

const GdkColor * prefs_get_console_textcolor(void)
{
    return &user_prefs.console_input_color;
}

const GdkColor * prefs_get_console_outputcolor(void)
{
  return &user_prefs.console_output_color;
}

const GdkColor * prefs_get_console_bgcolor(void)
{
    return &user_prefs.console_bg_color;
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

static void 
font_picker_changed_cb(GtkWidget *widget, gchar *font_name, gpointer user_data)
{
  g_return_if_fail(user_data != NULL);
  g_return_if_fail(GNOME_IS_PROPERTY_BOX(user_data));
  
  gnome_property_box_changed(GNOME_PROPERTY_BOX(user_data));
}

static void
color_picker_changed_cb(GtkWidget *widget, guint r, guint g, guint b, guint a,
			gpointer user_data)
{
  g_return_if_fail(user_data != NULL);
  g_return_if_fail(GNOME_IS_PROPERTY_BOX(user_data));

  gnome_property_box_changed(GNOME_PROPERTY_BOX(user_data));
}

static void
_set_font (GtkWidget *font_picker, gchar *font_name, GtkWidget *prefs_dialog)
{
  /* 
     Sets the initial value of font_picker to font and sets up the
     callback for when font_picker is changed
  */
  
  gnome_font_picker_set_font_name(GNOME_FONT_PICKER(font_picker), font_name);
  gtk_signal_connect(GTK_OBJECT(font_picker), "font-set",
		     font_picker_changed_cb,  prefs_dialog);
}

static void
_set_color (GtkWidget *color_picker, GdkColor *color, GtkWidget *prefs_dialog)
{
  /* 
     Sets the initial value of color_picker to color and sets up the
     callback for when color_picker is changed
  */
  gnome_color_picker_set_i16(GNOME_COLOR_PICKER(color_picker),
			     color->red,
			     color->green,
			     color->blue,
			     0);
  gtk_signal_connect(GTK_OBJECT(color_picker), "color-set",
		     color_picker_changed_cb, prefs_dialog);
}

static void
console_page_init(GtkWidget *prefs_dialog, GladeXML *prefs_xml)
{
  GtkWidget *font_picker, *color_picker;

  font_picker = glade_xml_get_widget(prefs_xml, "prefs_console_font");
  _set_font(font_picker, user_prefs.console_font, prefs_dialog);

  color_picker = glade_xml_get_widget(prefs_xml, "prefs_console_input_color");
  _set_color(color_picker, &user_prefs.console_input_color, prefs_dialog);

  color_picker = glade_xml_get_widget(prefs_xml, "prefs_console_output_color");
  _set_color(color_picker, &user_prefs.console_output_color, prefs_dialog);

  color_picker = glade_xml_get_widget(prefs_xml, "prefs_console_bg_color");  
  _set_color(color_picker, &user_prefs.console_bg_color, prefs_dialog);
}

static gboolean
_update_color(GtkWidget *color_picker, GdkColor *color)
{
  /* 
     Sets color to the value of color_picker.
     Returns TRUE if color is changed from its previous value
  */
  gushort r, g, b, a;
  
  g_return_val_if_fail(color_picker, FALSE);
  g_return_val_if_fail(color, FALSE);
  g_return_val_if_fail(GNOME_IS_COLOR_PICKER(color_picker), FALSE);

  gnome_color_picker_get_i16(GNOME_COLOR_PICKER(color_picker),
			     &r, &g, &b, &a);
  if((color->red != r) || (color->green != g) || (color->blue != b))
    {
      color->red = r;
      color->green = g;
      color->blue = b;
      return TRUE;
    }
  else
    {
      return FALSE;
    }
}

static gboolean
_update_font (GtkWidget *font_picker, gchar **font_name_ptr)
{
  /* 
     Sets the string pointed to by font_name_ptr to the name
     of the font selected by font_picker.  Returns TRUE if
     the value of the string is changed.
  */
  gchar *new_font_name;
  
  new_font_name = 
    gnome_font_picker_get_font_name(GNOME_FONT_PICKER(font_picker));

  if(strcmp(*font_name_ptr, new_font_name))
    {
      g_free(*font_name_ptr);
      *font_name_ptr = g_strdup(new_font_name);
      return TRUE;
    }
  else
    {
      return FALSE;
    }
}

static void
console_page_apply(GladeXML *prefs_xml)
{
  GtkWidget *font_picker, *color_picker;
  gboolean changed = FALSE;

  font_picker = glade_xml_get_widget(prefs_xml, "prefs_console_font");  
  if (_update_font(font_picker, &(user_prefs.console_font)))
    {
      changed = TRUE;
    }

  color_picker = glade_xml_get_widget(prefs_xml, "prefs_console_output_color");
  if (_update_color(color_picker, &(user_prefs.console_output_color)))
    {
      changed = TRUE;
    }

  color_picker = glade_xml_get_widget(prefs_xml, "prefs_console_bg_color"); 
  if (_update_color(color_picker, &(user_prefs.console_bg_color)))
    {
      changed = TRUE;
    }

  color_picker = glade_xml_get_widget(prefs_xml, "prefs_console_input_color");
  if (_update_color(color_picker, &(user_prefs.console_input_color)))
    {
      changed = TRUE;
    }

  if (changed)
    {
      gtk_object_set(GTK_OBJECT(R_gtk_terminal_text),
		     "input_color_gdk", &user_prefs.console_input_color,
		     "bg_color_gdk",    &user_prefs.console_bg_color,
		     "output_color_gdk",&user_prefs.console_output_color,
		     "font", user_prefs.console_font,
		     NULL);
    }
}

static void pager_page_init(GtkWidget *prefs_dialog, GladeXML *prefs_xml)
{
  GtkWidget *font_picker, *color_picker;

  font_picker = glade_xml_get_widget(prefs_xml, "prefs_title_font");
  _set_font(font_picker, user_prefs.pager_title_font, prefs_dialog);

  color_picker = glade_xml_get_widget(prefs_xml, "prefs_title_text_color");
  _set_color(color_picker, &user_prefs.pager_title_textcolor, prefs_dialog);

  color_picker = glade_xml_get_widget(prefs_xml, "prefs_title_bg_color");
  _set_color (color_picker, &user_prefs.pager_title_bgcolor, prefs_dialog);

  font_picker = glade_xml_get_widget(prefs_xml, "prefs_body_font");
  _set_font (font_picker, user_prefs.pager_text_font, prefs_dialog);

  font_picker = glade_xml_get_widget(prefs_xml, "prefs_body_emphasis_font");
  _set_font (font_picker, user_prefs.pager_em_font, prefs_dialog);

  color_picker = glade_xml_get_widget(prefs_xml, "prefs_body_text_color");
  _set_color (color_picker, &user_prefs.pager_text_textcolor, prefs_dialog);

  color_picker = glade_xml_get_widget(prefs_xml, "prefs_body_bg_color");
  _set_color (color_picker, &user_prefs.pager_text_bgcolor, prefs_dialog);
}

static void pager_page_apply(GladeXML *prefs_xml)
{
  GtkWidget *font_picker, *color_picker;
  gboolean change = FALSE;

  font_picker = glade_xml_get_widget(prefs_xml, "prefs_title_font");
  if (_update_font (font_picker, &(user_prefs.pager_title_font)))
    {
      change = TRUE;
    }
  
  color_picker = glade_xml_get_widget(prefs_xml, "prefs_title_text_color");
  if (_update_color (color_picker, &(user_prefs.pager_title_textcolor)))
    {
      change = TRUE;
    }

  color_picker = glade_xml_get_widget(prefs_xml, "prefs_title_bg_color");
  if (_update_color(color_picker, &user_prefs.pager_title_bgcolor))
    {
      change = TRUE;
    }

  font_picker = glade_xml_get_widget(prefs_xml, "prefs_body_font");
  if (_update_font (font_picker, &user_prefs.pager_text_font))
    {
      change = TRUE;
    }

  color_picker = glade_xml_get_widget(prefs_xml, "prefs_body_bg_color");
  if (_update_color (color_picker, &user_prefs.pager_text_bgcolor))
    {
      change = TRUE;
    }

  if(change == TRUE) {
    /* FIXME: update existing help windows */
  }
}

static void startup_page_init(GtkWidget *prefs_dialog, GladeXML *prefs_xml)
{
  GtkWidget *always_ws_radio, *never_ws_radio;

  always_ws_radio = 
    glade_xml_get_widget(prefs_xml, "prefs_always_restore_ws_radio");
  never_ws_radio = 
    glade_xml_get_widget(prefs_xml, "prefs_never_restore_ws_radio");

  switch(user_prefs.restoreact)
    {
    case SA_NORESTORE:
      gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(never_ws_radio), TRUE);
      break;

    case SA_RESTORE: /* Falling through */
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

  always_ws_radio = 
    glade_xml_get_widget(prefs_xml, "prefs_always_restore_ws_radio");
  never_ws_radio = 
    glade_xml_get_widget(prefs_xml, "prefs_never_restore_ws_radio");

  if(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(always_ws_radio)))
    {
      user_prefs.restoreact = SA_RESTORE;
    }
  else if(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(never_ws_radio)))
    {
      user_prefs.restoreact = SA_NORESTORE;
    }
}

static void
exit_page_init(GtkWidget *prefs_dialog, GladeXML *prefs_xml)
{
  GtkWidget *prompt_ws_radio, *always_ws_radio, *never_ws_radio;

  prompt_ws_radio = glade_xml_get_widget(prefs_xml,
					 "prefs_prompt_save_ws_radio");
  always_ws_radio = glade_xml_get_widget(prefs_xml,
					 "prefs_always_save_ws_radio");
  never_ws_radio = glade_xml_get_widget(prefs_xml,
					"prefs_never_save_ws_radio");

  switch(user_prefs.saveact)
    {
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

static void
exit_page_apply(GladeXML *prefs_xml)
{
  GtkWidget *prompt_ws_radio, *always_ws_radio, *never_ws_radio;

  prompt_ws_radio = glade_xml_get_widget(prefs_xml,
					 "prefs_prompt_save_ws_radio");
  always_ws_radio = glade_xml_get_widget(prefs_xml,
					 "prefs_always_save_ws_radio");
  never_ws_radio = glade_xml_get_widget(prefs_xml,
					"prefs_never_save_ws_radio");

  if(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(prompt_ws_radio)))
    {
      user_prefs.saveact = SA_SAVEASK;
    }
  else if(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(always_ws_radio)))
    {
      user_prefs.saveact = SA_SAVE;
    }
  else if(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(never_ws_radio)))
    {
      user_prefs.saveact = SA_NOSAVE;
    }

  R_set_SaveAction(user_prefs.saveact);
}


static void
prefs_apply_cb(GnomePropertyBox *property_box, gint page_num,
	       gpointer user_data)
{
  GladeXML *prefs_xml;

  prefs_xml = (GladeXML *) user_data;

  if(page_num == -1)
    {
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














