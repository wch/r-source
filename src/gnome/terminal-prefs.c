#include "terminal-prefs.h"
#include "terminal.h"

static void prefs_font_entry_changed(GtkWidget *widget, gpointer data)
{
  GtkWidget *font_picker;
  gchar *font;

  font_picker = (GtkWidget *) data;

  g_return_if_fail(GTK_IS_ENTRY(widget));
  g_return_if_fail(GNOME_IS_FONT_PICKER(font_picker));

  font = gtk_entry_get_text(GTK_ENTRY(widget));
  gnome_font_picker_set_font_name(GNOME_FONT_PICKER(font_picker), font);

  R_gnome_newprefs.font = g_strdup(font);

  gnome_property_box_changed(GNOME_PROPERTY_BOX(prefs_dialog));
}

static void prefs_font_picker_font_set(GtkWidget *widget, gchar *font)
{
  GtkWidget *font_entry;

  font_entry = (GtkWidget *) gtk_object_get_user_data(GTK_OBJECT(widget));

  g_return_if_fail(GNOME_IS_FONT_PICKER(widget));
  g_return_if_fail(GTK_IS_ENTRY(font_entry));

  gtk_entry_set_text(GTK_ENTRY(font_entry), font);

  R_gnome_newprefs.font = g_strdup(font);

  gnome_property_box_changed(GNOME_PROPERTY_BOX(prefs_dialog));
}

static void prefs_text_color_set(GtkWidget *widget, gint r, gint g, gint b, gint a, gpointer data)
{
  R_gnome_newprefs.textcolor.red = r;
  R_gnome_newprefs.textcolor.green = g;
  R_gnome_newprefs.textcolor.blue = b;

  gnome_property_box_changed(GNOME_PROPERTY_BOX(prefs_dialog));
}

static void prefs_bg_color_set(GtkWidget *widget, gint r, gint g, gint b, gint a, gpointer data)
{
  R_gnome_newprefs.bgcolor.red = r;
  R_gnome_newprefs.bgcolor.green = g;
  R_gnome_newprefs.bgcolor.blue = b;

  gnome_property_box_changed(GNOME_PROPERTY_BOX(prefs_dialog));
}

GtkWidget *prefs_text_page(void)
{
  GtkWidget *frame, *table;

  GtkWidget *font_label, *font_entry, *font_picker, *font_picker_label;
  GtkWidget *textcol_label, *textcol_button;
  GtkWidget *bgcol_label, *bgcol_button;

  frame = gtk_frame_new("Text settings");
  table = gtk_table_new(3, 3, FALSE);
  gtk_container_set_border_width(GTK_CONTAINER(frame), GNOME_PAD_SMALL);
  gtk_container_add(GTK_CONTAINER(frame), table);

  /* Font */
  font_label = gtk_label_new("Font: ");
  gtk_misc_set_alignment(GTK_MISC(font_label), 1.0, 0.5);
  font_entry = gtk_entry_new();
  gtk_entry_set_text(GTK_ENTRY(font_entry), R_gnome_userprefs.font);

  font_picker = gnome_font_picker_new();
  gnome_font_picker_set_font_name(GNOME_FONT_PICKER(font_picker),
				  gtk_entry_get_text(GTK_ENTRY (font_entry)));
  gnome_font_picker_set_mode(GNOME_FONT_PICKER(font_picker),
			     GNOME_FONT_PICKER_MODE_USER_WIDGET);
  font_picker_label = gtk_label_new("Browse...");
  gnome_font_picker_uw_set_widget(GNOME_FONT_PICKER(font_picker),
				  font_picker_label);

  gtk_table_attach(GTK_TABLE(table), font_label,
		   0, 1, 0, 1,
		   GTK_FILL, 0, GNOME_PAD_SMALL, GNOME_PAD_SMALL);
  gtk_table_attach(GTK_TABLE(table), font_entry,
		   1, 2, 0, 1,
		   GTK_FILL, 0, 0, GNOME_PAD_SMALL);
  gtk_table_attach(GTK_TABLE(table), font_picker,
		   2, 3, 0, 1,
		   GTK_FILL, 0, GNOME_PAD_SMALL, GNOME_PAD_SMALL);

  gtk_signal_connect(GTK_OBJECT(font_entry), "changed",
		     GTK_SIGNAL_FUNC(prefs_font_entry_changed),
		     (gpointer) font_picker);
  gtk_signal_connect(GTK_OBJECT(font_picker), "font_set",
		     GTK_SIGNAL_FUNC(prefs_font_picker_font_set),
		     NULL);
  gtk_object_set_user_data(GTK_OBJECT(font_picker), GTK_OBJECT(font_entry));

  /* Text colour */
  textcol_label = gtk_label_new("Text colour: ");
  gtk_misc_set_alignment(GTK_MISC(textcol_label), 1.0, 0.5);
  textcol_button = gnome_color_picker_new();
  gnome_color_picker_set_i16 (GNOME_COLOR_PICKER (textcol_button),
			      R_gnome_userprefs.textcolor.red,
			      R_gnome_userprefs.textcolor.green,
			      R_gnome_userprefs.textcolor.blue,
			      0);
  
  gtk_table_attach(GTK_TABLE(table), textcol_label,
		   0, 1, 1, 2,
		   GTK_FILL, 0, GNOME_PAD_SMALL, GNOME_PAD_SMALL);
  gtk_table_attach(GTK_TABLE(table), textcol_button,
		   1, 2, 1, 2,
		   0, 0, 0, GNOME_PAD_SMALL);

  gtk_signal_connect(GTK_OBJECT(textcol_button),
		     "color_set",
		     GTK_SIGNAL_FUNC(prefs_text_color_set),
		     NULL);

  /* Background colour */
  bgcol_label = gtk_label_new("Background colour: ");
  gtk_misc_set_alignment(GTK_MISC(bgcol_label), 1.0, 0.5);
  bgcol_button = gnome_color_picker_new();
  gnome_color_picker_set_i16 (GNOME_COLOR_PICKER (bgcol_button),
			      R_gnome_userprefs.bgcolor.red,
			      R_gnome_userprefs.bgcolor.green,
			      R_gnome_userprefs.bgcolor.blue,
			      0);
  
  gtk_table_attach(GTK_TABLE(table), bgcol_label,
		   0, 1, 2, 3,
		   GTK_FILL, 0, GNOME_PAD_SMALL, GNOME_PAD_SMALL);
  gtk_table_attach(GTK_TABLE(table), bgcol_button,
		   1, 2, 2, 3,
		   0, 0, 0, GNOME_PAD_SMALL);

  gtk_signal_connect(GTK_OBJECT(bgcol_button),
		     "color_set",
		     GTK_SIGNAL_FUNC(prefs_bg_color_set),
		     NULL);

  return frame;
}

GtkWidget *prefs_exit_page(void)
{
  GtkWidget *vbox;
  GtkWidget *workspace_frame, *history_frame;

  vbox = gtk_vbox_new(FALSE, 0);

  workspace_frame = prefs_workspace_frame();
  history_frame = prefs_history_frame();

  gtk_box_pack_start(GTK_BOX(vbox), workspace_frame, FALSE, FALSE, 0);
  gtk_box_pack_start(GTK_BOX(vbox), history_frame, FALSE, FALSE, 0);

  return vbox;
}

GtkWidget *prefs_workspace_frame(void)
{
  GtkWidget *frame, *table;
  GtkWidget *ask;
  GtkWidget *save;
  GtkWidget *savetofile, *dummy, *nameentry, *namebutton;
  GtkWidget *dontsave;

  frame = gtk_frame_new("Workspace");
  
  table = gtk_table_new(2, 5, FALSE);
  gtk_container_set_border_width(GTK_CONTAINER(frame), GNOME_PAD_SMALL);
  gtk_container_add(GTK_CONTAINER(frame), table);

  ask = gtk_radio_button_new_with_label(NULL, "Ask me");
  

  save = gtk_radio_button_new_with_label_from_widget(GTK_RADIO_BUTTON(ask), "Save");
  savetofile = gtk_radio_button_new_with_label_from_widget(GTK_RADIO_BUTTON(ask), "Save in this file:");
  dummy = gtk_event_box_new();
  nameentry = gnome_file_entry_new(NULL, "Workspace file");
  
  dontsave = gtk_radio_button_new_with_label_from_widget(GTK_RADIO_BUTTON(ask), "Don't save");

  gtk_table_attach(GTK_TABLE(table), ask,
		   0, 2, 0, 1,
		   GTK_FILL, 0, GNOME_PAD, 0);
  gtk_table_attach(GTK_TABLE(table), save,
		   0, 2, 1, 2,
		   GTK_FILL, 0, GNOME_PAD, 0);
  gtk_table_attach(GTK_TABLE(table), savetofile,
		   0, 2, 2, 3,
		   GTK_FILL, 0, GNOME_PAD, 0);
  gtk_table_attach(GTK_TABLE(table), dummy,
		   0, 1, 3, 4,
		   GTK_FILL, 0, GNOME_PAD_BIG, 0);
  gtk_table_attach(GTK_TABLE(table), nameentry,
		   1, 2, 3, 4,
		   GTK_FILL, 0, 0, 0);
  gtk_table_attach(GTK_TABLE(table), dontsave,
		   0, 2, 4, 5,
		   GTK_FILL, 0, GNOME_PAD, GNOME_PAD_SMALL);

  return frame;
}

GtkWidget *prefs_history_frame(void)
{
  GtkWidget *frame, *table;
  GtkWidget *save;
  GtkWidget *savetofile, *dummy, *nameentry, *namebutton;
  GtkWidget *dontsave;

  frame = gtk_frame_new("Command history");
  
  table = gtk_table_new(2, 4, FALSE);
  gtk_container_set_border_width(GTK_CONTAINER(frame), GNOME_PAD_SMALL);
  gtk_container_add(GTK_CONTAINER(frame), table);

  save = gtk_radio_button_new_with_label(NULL, "Save");
  savetofile = gtk_radio_button_new_with_label_from_widget(GTK_RADIO_BUTTON(save), "Save in this file:");
  dummy = gtk_event_box_new();
  nameentry = gnome_file_entry_new(NULL, "Command history file");
  dontsave = gtk_radio_button_new_with_label_from_widget(GTK_RADIO_BUTTON(save), "Don't save");

  gtk_table_attach(GTK_TABLE(table), save,
		   0, 2, 0, 1,
		   GTK_FILL, 0, GNOME_PAD, 0);
  gtk_table_attach(GTK_TABLE(table), savetofile,
		   0, 2, 1, 2,
		   GTK_FILL, 0, GNOME_PAD, 0);
  gtk_table_attach(GTK_TABLE(table), dummy,
		   0, 1, 2, 3,
		   GTK_FILL, 0, GNOME_PAD_BIG, 0);
  gtk_table_attach(GTK_TABLE(table), nameentry,
		   1, 2, 2, 3,
		   GTK_FILL, 0, 0, 0);
  gtk_table_attach(GTK_TABLE(table), dontsave,
		   0, 2, 3, 4,
		   GTK_FILL, 0, GNOME_PAD, GNOME_PAD_SMALL);

  return frame;
}

GtkWidget *prefs_apps_page(void)
{
  GtkWidget *frame, *table;

  frame = gtk_frame_new("Helper applications");
  gtk_container_set_border_width(GTK_CONTAINER(frame), GNOME_PAD_SMALL);

  return frame;
}

GtkWidget *prefs_graphics_page(void)
{
  GtkWidget *frame, *table;

  frame = gtk_frame_new("Graphics device options");
  gtk_container_set_border_width(GTK_CONTAINER(frame), GNOME_PAD_SMALL);

  return frame;
}

GtkWidget *prefs_env_page(void)
{
  GtkWidget *frame, *table;

  frame = gtk_frame_new("Internal R environment");
  gtk_container_set_border_width(GTK_CONTAINER(frame), GNOME_PAD_SMALL);

  return frame;
}


