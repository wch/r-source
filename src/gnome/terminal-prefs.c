#include "terminal-prefs.h"
#include "terminal.h"

static void prefs_font_entry_changed(GtkWidget *widget, gpointer data)
{
  GtkWidget *font_picker;
  gchar *font;
  gchar **pref = (gchar **) data;

  if(font_pref_locked == FALSE) {
    font_pref_locked = TRUE;

    font_picker = (GtkWidget *) gtk_object_get_user_data(GTK_OBJECT(widget));
    
    g_return_if_fail(GTK_IS_ENTRY(widget));
    g_return_if_fail(GNOME_IS_FONT_PICKER(font_picker));
    
    font = gtk_entry_get_text(GTK_ENTRY(widget));
    
    *pref = g_strdup(font);
    
    gnome_font_picker_set_font_name(GNOME_FONT_PICKER(font_picker), font);
    
    gnome_property_box_changed(GNOME_PROPERTY_BOX(prefs_dialog));

    font_pref_locked = FALSE;
  }
}

static void prefs_font_picker_font_set(GtkWidget *widget, gchar *font, gpointer data)
{
  GtkWidget *font_entry;
  gchar **pref = (gchar **) data;

  if(font_pref_locked == FALSE) {
    font_pref_locked = TRUE;

    font_entry = (GtkWidget *) gtk_object_get_user_data(GTK_OBJECT(widget));

    g_return_if_fail(GNOME_IS_FONT_PICKER(widget));
    g_return_if_fail(GTK_IS_ENTRY(font_entry));

    *pref = g_strdup(font);

    gtk_entry_set_text(GTK_ENTRY(font_entry), font);

    gnome_property_box_changed(GNOME_PROPERTY_BOX(prefs_dialog));

    font_pref_locked = FALSE;
  }
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
  gtk_entry_set_position(GTK_ENTRY(font_entry), 0);

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
		     (gpointer) &R_gnome_newprefs.font);
  gtk_signal_connect(GTK_OBJECT(font_picker), "font_set",
		     GTK_SIGNAL_FUNC(prefs_font_picker_font_set),
		     (gpointer) &R_gnome_newprefs.font);
  gtk_object_set_user_data(GTK_OBJECT(font_picker), GTK_OBJECT(font_entry));
  gtk_object_set_user_data(GTK_OBJECT(font_entry), GTK_OBJECT(font_picker));

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
  gtk_box_pack_start(GTK_BOX(vbox), history_frame, TRUE, TRUE, 0);

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
  GtkWidget *savewithws;
  GtkWidget *dontsave;

  frame = gtk_frame_new("Command history");
  
  table = gtk_table_new(2, 3, FALSE);
  gtk_container_set_border_width(GTK_CONTAINER(frame), GNOME_PAD_SMALL);
  gtk_container_add(GTK_CONTAINER(frame), table);

  save = gtk_radio_button_new_with_label(NULL, "Always save");
  savewithws = gtk_radio_button_new_with_label_from_widget(GTK_RADIO_BUTTON(save), "Save if workspace saved");
  dontsave = gtk_radio_button_new_with_label_from_widget(GTK_RADIO_BUTTON(save), "Don't save");

  gtk_table_attach(GTK_TABLE(table), save,
		   0, 2, 0, 1,
		   GTK_FILL, 0, GNOME_PAD, 0);
  gtk_table_attach(GTK_TABLE(table), savewithws,
		   0, 2, 1, 2,
		   GTK_FILL, 0, GNOME_PAD, 0);
  gtk_table_attach(GTK_TABLE(table), dontsave,
		   0, 2, 2, 3,
		   GTK_FILL, 0, GNOME_PAD, 0);

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

GtkWidget *prefs_startup_page(void)
{
  GtkWidget *frame, *table;
  GtkWidget *vsize_label, *vsize_entry;
  GtkWidget *nsize_label, *nsize_entry;
  GtkWidget *warning_label;

  frame = gtk_frame_new("R environment");
  table = gtk_table_new(3, 3, FALSE);
  gtk_container_set_border_width(GTK_CONTAINER(frame), GNOME_PAD_SMALL);
  gtk_container_add(GTK_CONTAINER(frame), table);

  /* R_Vsize */
  vsize_label = gtk_label_new("Vector heap size (vsize):");
  gtk_misc_set_alignment(GTK_MISC(vsize_label), 1.0, 0.5);
  vsize_entry = gtk_entry_new();
  /*  gtk_entry_set_text(GTK_ENTRY(nsize_entry), R_gnome_userprefs.nsize); */

  gtk_table_attach(GTK_TABLE(table), vsize_label,
		   0, 1, 0, 1,
		   GTK_FILL, 0, GNOME_PAD_SMALL, GNOME_PAD_SMALL);
  gtk_table_attach(GTK_TABLE(table), vsize_entry,
		   1, 2, 0, 1,
		   GTK_FILL, 0, GNOME_PAD_SMALL, GNOME_PAD_SMALL);

  /* R_Nsize */
  nsize_label = gtk_label_new("Number of cons cells (nsize):");
  gtk_misc_set_alignment(GTK_MISC(nsize_label), 1.0, 0.5);
  nsize_entry = gtk_entry_new();
  /*  gtk_entry_set_text(GTK_ENTRY(nsize_entry), R_gnome_userprefs.nsize); */

  gtk_table_attach(GTK_TABLE(table), nsize_label,
		   0, 1, 1, 2,
		   GTK_FILL, 0, GNOME_PAD_SMALL, GNOME_PAD_SMALL);
  gtk_table_attach(GTK_TABLE(table), nsize_entry,
		   1, 2, 1, 2,
		   GTK_FILL, 0, GNOME_PAD_SMALL, GNOME_PAD_SMALL);

  /* Warning */
  warning_label = gtk_label_new("These settings will take effect next time you start R.");
  gtk_misc_set_alignment(GTK_MISC(warning_label), 0.0, 0.5);

  gtk_table_attach(GTK_TABLE(table), warning_label,
		   0, 2, 2, 3,
		   GTK_FILL, 0, GNOME_PAD_SMALL, GNOME_PAD_SMALL);

  return frame;
}


GtkWidget *prefs_pager_page(void)
{
  GtkWidget *vbox;

  GtkWidget *title_frame, *title_table;

  GtkWidget *tfont_label, *tfont_entry, *tfont_picker, *tfont_picker_label;
  GtkWidget *ttextcol_label, *ttextcol_button;
  GtkWidget *tbgcol_label, *tbgcol_button;

  GtkWidget *text_frame, *text_table;
  
  GtkWidget *nfont_label, *nfont_entry, *nfont_picker, *nfont_picker_label;
  GtkWidget *emfont_label, *emfont_entry, *emfont_picker, *emfont_picker_label;
  GtkWidget *ntextcol_label, *ntextcol_button;
  GtkWidget *nbgcol_label, *nbgcol_button;

  /* Title settings */
  title_frame = gtk_frame_new("Title text settings");
  title_table = gtk_table_new(3, 3, FALSE);
  gtk_container_set_border_width(GTK_CONTAINER(title_frame), GNOME_PAD_SMALL);
  gtk_container_add(GTK_CONTAINER(title_frame), title_table);

  /* Font */
  tfont_label = gtk_label_new("Font: ");
  gtk_misc_set_alignment(GTK_MISC(tfont_label), 1.0, 0.5);
  tfont_entry = gtk_entry_new();
  gtk_entry_set_text(GTK_ENTRY(tfont_entry), R_gnome_userprefs.pager_title_font);
  gtk_entry_set_position(GTK_ENTRY(tfont_entry), 0);

  tfont_picker = gnome_font_picker_new();
  gnome_font_picker_set_font_name(GNOME_FONT_PICKER(tfont_picker),
				  gtk_entry_get_text(GTK_ENTRY (tfont_entry)));
  gnome_font_picker_set_mode(GNOME_FONT_PICKER(tfont_picker),
			     GNOME_FONT_PICKER_MODE_USER_WIDGET);
  tfont_picker_label = gtk_label_new("Browse...");
  gnome_font_picker_uw_set_widget(GNOME_FONT_PICKER(tfont_picker),
				  tfont_picker_label);

  gtk_table_attach(GTK_TABLE(title_table), tfont_label,
		   0, 1, 0, 1,
		   GTK_FILL, 0, GNOME_PAD_SMALL, GNOME_PAD_SMALL);
  gtk_table_attach(GTK_TABLE(title_table), tfont_entry,
		   1, 2, 0, 1,
		   GTK_FILL, 0, 0, GNOME_PAD_SMALL);
  gtk_table_attach(GTK_TABLE(title_table), tfont_picker,
		   2, 3, 0, 1,
		   GTK_FILL, 0, GNOME_PAD_SMALL, GNOME_PAD_SMALL);

  /* Text colour */
  ttextcol_label = gtk_label_new("Text colour: ");
  gtk_misc_set_alignment(GTK_MISC(ttextcol_label), 1.0, 0.5);
  ttextcol_button = gnome_color_picker_new();
  gnome_color_picker_set_i16 (GNOME_COLOR_PICKER (ttextcol_button),
			      R_gnome_userprefs.textcolor.red,
			      R_gnome_userprefs.textcolor.green,
			      R_gnome_userprefs.textcolor.blue,
			      0);
  
  gtk_table_attach(GTK_TABLE(title_table), ttextcol_label,
		   0, 1, 1, 2,
		   GTK_FILL, 0, GNOME_PAD_SMALL, GNOME_PAD_SMALL);
  gtk_table_attach(GTK_TABLE(title_table), ttextcol_button,
		   1, 2, 1, 2,
		   0, 0, 0, GNOME_PAD_SMALL);

  /* Background colour */
  tbgcol_label = gtk_label_new("Background colour: ");
  gtk_misc_set_alignment(GTK_MISC(tbgcol_label), 1.0, 0.5);
  tbgcol_button = gnome_color_picker_new();
  gnome_color_picker_set_i16 (GNOME_COLOR_PICKER (tbgcol_button),
			      R_gnome_userprefs.bgcolor.red,
			      R_gnome_userprefs.bgcolor.green,
			      R_gnome_userprefs.bgcolor.blue,
			      0);
  
  gtk_table_attach(GTK_TABLE(title_table), tbgcol_label,
		   0, 1, 2, 3,
		   GTK_FILL, 0, GNOME_PAD_SMALL, GNOME_PAD_SMALL);
  gtk_table_attach(GTK_TABLE(title_table), tbgcol_button,
		   1, 2, 2, 3,
		   0, 0, 0, GNOME_PAD_SMALL);

  /* Normal text settings */
  text_frame = gtk_frame_new("Body text settings");
  text_table = gtk_table_new(3, 4, FALSE);
  gtk_container_set_border_width(GTK_CONTAINER(text_frame), GNOME_PAD_SMALL);
  gtk_container_add(GTK_CONTAINER(text_frame), text_table);

  /* Font */
  nfont_label = gtk_label_new("Font: ");
  gtk_misc_set_alignment(GTK_MISC(nfont_label), 1.0, 0.5);
  nfont_entry = gtk_entry_new();
  gtk_entry_set_text(GTK_ENTRY(nfont_entry), R_gnome_userprefs.pager_text_font);
  gtk_entry_set_position(GTK_ENTRY(nfont_entry), 0);

  nfont_picker = gnome_font_picker_new();
  gnome_font_picker_set_font_name(GNOME_FONT_PICKER(nfont_picker),
				  gtk_entry_get_text(GTK_ENTRY (nfont_entry)));
  gnome_font_picker_set_mode(GNOME_FONT_PICKER(nfont_picker),
			     GNOME_FONT_PICKER_MODE_USER_WIDGET);
  nfont_picker_label = gtk_label_new("Browse...");
  gnome_font_picker_uw_set_widget(GNOME_FONT_PICKER(nfont_picker),
				  nfont_picker_label);

  gtk_table_attach(GTK_TABLE(text_table), nfont_label,
		   0, 1, 0, 1,
		   GTK_FILL, 0, GNOME_PAD_SMALL, GNOME_PAD_SMALL);
  gtk_table_attach(GTK_TABLE(text_table), nfont_entry,
		   1, 2, 0, 1,
		   GTK_FILL, 0, 0, GNOME_PAD_SMALL);
  gtk_table_attach(GTK_TABLE(text_table), nfont_picker,
		   2, 3, 0, 1,
		   GTK_FILL, 0, GNOME_PAD_SMALL, GNOME_PAD_SMALL);

  /* Emphasis font */
  emfont_label = gtk_label_new("Emphasis font: ");
  gtk_misc_set_alignment(GTK_MISC(emfont_label), 1.0, 0.5);
  emfont_entry = gtk_entry_new();
  gtk_entry_set_text(GTK_ENTRY(emfont_entry), R_gnome_userprefs.pager_em_font);
  gtk_entry_set_position(GTK_ENTRY(emfont_entry), 0);

  emfont_picker = gnome_font_picker_new();
  gnome_font_picker_set_font_name(GNOME_FONT_PICKER(emfont_picker),
				  gtk_entry_get_text(GTK_ENTRY (emfont_entry)));
  gnome_font_picker_set_mode(GNOME_FONT_PICKER(emfont_picker),
			     GNOME_FONT_PICKER_MODE_USER_WIDGET);
  emfont_picker_label = gtk_label_new("Browse...");
  gnome_font_picker_uw_set_widget(GNOME_FONT_PICKER(emfont_picker),
				  emfont_picker_label);

  gtk_table_attach(GTK_TABLE(text_table), emfont_label,
		   0, 1, 1, 2,
		   GTK_FILL, 0, GNOME_PAD_SMALL, GNOME_PAD_SMALL);
  gtk_table_attach(GTK_TABLE(text_table), emfont_entry,
		   1, 2, 1, 2,
		   GTK_FILL, 0, 0, GNOME_PAD_SMALL);
  gtk_table_attach(GTK_TABLE(text_table), emfont_picker,
		   2, 3, 1, 2,
		   GTK_FILL, 0, GNOME_PAD_SMALL, GNOME_PAD_SMALL);

  /* Text colour */
  ntextcol_label = gtk_label_new("Text colour: ");
  gtk_misc_set_alignment(GTK_MISC(ntextcol_label), 1.0, 0.5);
  ntextcol_button = gnome_color_picker_new();
  gnome_color_picker_set_i16 (GNOME_COLOR_PICKER (ntextcol_button),
			      R_gnome_userprefs.textcolor.red,
			      R_gnome_userprefs.textcolor.green,
			      R_gnome_userprefs.textcolor.blue,
			      0);
  
  gtk_table_attach(GTK_TABLE(text_table), ntextcol_label,
		   0, 1, 2, 3,
		   GTK_FILL, 0, GNOME_PAD_SMALL, GNOME_PAD_SMALL);
  gtk_table_attach(GTK_TABLE(text_table), ntextcol_button,
		   1, 2, 2, 3,
		   0, 0, 0, GNOME_PAD_SMALL);

  /* Background colour */
  nbgcol_label = gtk_label_new("Background colour: ");
  gtk_misc_set_alignment(GTK_MISC(nbgcol_label), 1.0, 0.5);
  nbgcol_button = gnome_color_picker_new();
  gnome_color_picker_set_i16 (GNOME_COLOR_PICKER (nbgcol_button),
			      R_gnome_userprefs.bgcolor.red,
			      R_gnome_userprefs.bgcolor.green,
			      R_gnome_userprefs.bgcolor.blue,
			      0);
  
  gtk_table_attach(GTK_TABLE(text_table), nbgcol_label,
		   0, 1, 3, 4,
		   GTK_FILL, 0, GNOME_PAD_SMALL, GNOME_PAD_SMALL);
  gtk_table_attach(GTK_TABLE(text_table), nbgcol_button,
		   1, 2, 3, 4,
		   0, 0, 0, GNOME_PAD_SMALL);


  /* VBox */
  vbox = gtk_vbox_new(FALSE, 0);

  gtk_box_pack_start(GTK_BOX(vbox), title_frame, FALSE, FALSE, 0);
  gtk_box_pack_start(GTK_BOX(vbox), text_frame, TRUE, TRUE, 0);

  return vbox;
}

