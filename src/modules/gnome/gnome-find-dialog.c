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

#include "gnome-find-dialog.h"

#include <gtk/gtk.h>
#include <libgnomeui/gnome-stock.h>
#include <libgnomeui/gnome-uidefs.h>

enum
{
  FIND,
  FIND_AGAIN,
  LAST_SIGNAL
};

static void gnome_find_dialog_class_init   (GnomeFindDialogClass *klass);
static void gnome_find_dialog_init         (GnomeFindDialog *find_dialog);
static void gnome_find_dialog_destroy      (GtkObject *object);

static void find       (GnomeFindDialog *find_dialog);
static void find_again (GnomeFindDialog *find_dialog);
static void close      (GnomeFindDialog *find_dialog);

static void entry_changed_cb     (GtkEditable *editable, gpointer data);

static void forwards_clicked_cb  (GtkButton *button, gpointer data);
static void backwards_clicked_cb (GtkButton *button, gpointer data);
static void top_clicked_cb       (GtkButton *button, gpointer data);
static void cursor_clicked_cb    (GtkButton *button, gpointer data);
static void bottom_clicked_cb    (GtkButton *button, gpointer data);
static void case_toggled_cb      (GtkToggleButton *button, gpointer data);
static void wrap_toggled_cb      (GtkToggleButton *button, gpointer data);
static void regex_toggled_cb   (GtkToggleButton *button, gpointer data);

static void dialog_clicked_cb (GnomeDialog *dialog, gint button,
			       gpointer data);

static GnomeDialogClass *parent_class = NULL;

static guint find_dialog_signals[LAST_SIGNAL] = { 0 };

guint gnome_find_dialog_get_type (void)
{
  static guint find_dialog_type = 0;

  if(!find_dialog_type) {
    GtkTypeInfo find_dialog_info = {
      "GnomeFindDialog",
      sizeof (GnomeFindDialog),
      sizeof (GnomeFindDialogClass),
      (GtkClassInitFunc) gnome_find_dialog_class_init,
      (GtkObjectInitFunc) gnome_find_dialog_init,
      (GtkArgSetFunc) NULL,
      (GtkArgSetFunc) NULL,
    };

    find_dialog_type = gtk_type_unique (gnome_dialog_get_type (),
					&find_dialog_info);
  }

  return find_dialog_type;
}

static void gnome_find_dialog_class_init (GnomeFindDialogClass *klass)
{
  GtkObjectClass *object_class;
  GtkWidgetClass *widget_class;
  GtkWindowClass *window_class;

  object_class = (GtkObjectClass *) klass;
  widget_class = (GtkWidgetClass *) klass;
  window_class = (GtkWindowClass *) klass;

  object_class->destroy = gnome_find_dialog_destroy;

  parent_class = gtk_type_class (gnome_dialog_get_type ());

  find_dialog_signals[FIND] =
    gtk_signal_new ("find",
		    GTK_RUN_LAST,
		    object_class->type,
		    GTK_SIGNAL_OFFSET (GnomeFindDialogClass,
				       find),
		    gtk_marshal_NONE__NONE,
		    GTK_TYPE_NONE, 0);
  find_dialog_signals[FIND_AGAIN] =
    gtk_signal_new ("find_again",
		    GTK_RUN_LAST,
		    object_class->type,
		    GTK_SIGNAL_OFFSET (GnomeFindDialogClass,
				       find_again),
		    gtk_marshal_NONE__NONE,
		    GTK_TYPE_NONE, 0);

  gtk_object_class_add_signals (object_class, find_dialog_signals, 
				LAST_SIGNAL);

  klass->find = NULL;
  klass->find_again = NULL;
}

static void gnome_find_dialog_init (GnomeFindDialog *find_dialog)
{
  GList *button_list;

  GtkWidget *hbox, *find_label;
  GtkWidget *hbox2;
  GtkWidget *pos_frame, *pos_vbox;
  GtkWidget *dir_frame, *dir_vbox;

  gnome_dialog_append_button_with_pixmap(GNOME_DIALOG(find_dialog),
					 "Find",
					 GNOME_STOCK_PIXMAP_SEARCH);
  gnome_dialog_append_button_with_pixmap(GNOME_DIALOG(find_dialog),
					 "Find Again",
					 GNOME_STOCK_PIXMAP_SEARCH);
  gnome_dialog_append_buttons           (GNOME_DIALOG(find_dialog),
					 GNOME_STOCK_BUTTON_CLOSE,
					 NULL);

  button_list = GNOME_DIALOG(find_dialog)->buttons;

  find_dialog->find_button = GTK_WIDGET(button_list->data);
  gnome_dialog_set_sensitive(GNOME_DIALOG(find_dialog), 0, FALSE);
  button_list = button_list->next;

  find_dialog->find_again_button = GTK_WIDGET(button_list->data);
  gnome_dialog_set_sensitive(GNOME_DIALOG(find_dialog), 1, FALSE);
  button_list = button_list->next;

  find_dialog->close_button = GTK_WIDGET(button_list->data);
  button_list = button_list->next;

  gtk_signal_connect(GTK_OBJECT(find_dialog), "clicked",
		     GTK_SIGNAL_FUNC(dialog_clicked_cb),
		     NULL);

  /* text entry */
  hbox = gtk_hbox_new(FALSE, GNOME_PAD_SMALL);
  gtk_container_set_border_width(GTK_CONTAINER(hbox), GNOME_PAD_SMALL);
  find_label = gtk_label_new("Find what:");
  find_dialog->find_entry = gtk_entry_new();

  gtk_box_pack_start(GTK_BOX(hbox), find_label, FALSE, FALSE, 0);
  gtk_box_pack_start(GTK_BOX(hbox), find_dialog->find_entry, TRUE, TRUE, 0);

  gtk_widget_show_all(hbox);

  gtk_box_pack_start(GTK_BOX(GNOME_DIALOG(find_dialog)->vbox), hbox, FALSE, FALSE, 0);

  gtk_signal_connect(GTK_OBJECT(find_dialog->find_entry), "changed",
		     GTK_SIGNAL_FUNC(entry_changed_cb),
		     (gpointer) find_dialog);

  /* hbox2 */
  hbox2 = gtk_hbox_new(TRUE, GNOME_PAD_SMALL);
  gtk_widget_show(hbox2);
  gtk_box_pack_start(GTK_BOX(GNOME_DIALOG(find_dialog)->vbox), hbox2, FALSE, FALSE, 0);

  /* search direction */
  dir_frame = gtk_frame_new("Search direction");
  dir_vbox = gtk_vbox_new(FALSE, GNOME_PAD_SMALL);
  gtk_container_set_border_width(GTK_CONTAINER(dir_frame), GNOME_PAD_SMALL);
  gtk_container_set_border_width(GTK_CONTAINER(dir_vbox), GNOME_PAD_SMALL);
  gtk_container_add(GTK_CONTAINER(dir_frame), dir_vbox);

  find_dialog->forwards_radio = gtk_radio_button_new_with_label(NULL, "Forwards");
  find_dialog->backwards_radio = gtk_radio_button_new_with_label_from_widget(GTK_RADIO_BUTTON(find_dialog->forwards_radio), "Backwards");

  gtk_box_pack_start(GTK_BOX(dir_vbox), find_dialog->forwards_radio, FALSE, FALSE, 0);
  gtk_box_pack_start(GTK_BOX(dir_vbox), find_dialog->backwards_radio, FALSE, FALSE, 0);

  gtk_widget_show_all(dir_frame);

  gtk_box_pack_start(GTK_BOX(hbox2), dir_frame, TRUE, TRUE, 0);

  gtk_signal_connect(GTK_OBJECT(find_dialog->forwards_radio), "clicked",
		     GTK_SIGNAL_FUNC(forwards_clicked_cb),
		     (gpointer) find_dialog);
  gtk_signal_connect(GTK_OBJECT(find_dialog->backwards_radio), "clicked",
		     GTK_SIGNAL_FUNC(backwards_clicked_cb),
		     (gpointer) find_dialog);

  /* start position */
  pos_frame = gtk_frame_new("Start position");
  pos_vbox = gtk_vbox_new(FALSE, GNOME_PAD_SMALL);
  gtk_container_set_border_width(GTK_CONTAINER(pos_frame), GNOME_PAD_SMALL);
  gtk_container_set_border_width(GTK_CONTAINER(pos_vbox), GNOME_PAD_SMALL);
  gtk_container_add(GTK_CONTAINER(pos_frame), pos_vbox);

  find_dialog->top_radio = gtk_radio_button_new_with_label(NULL, "Top");
  find_dialog->cursor_radio = gtk_radio_button_new_with_label_from_widget(GTK_RADIO_BUTTON(find_dialog->top_radio),
									  "Cursor position");
  find_dialog->bottom_radio = gtk_radio_button_new_with_label_from_widget(GTK_RADIO_BUTTON(find_dialog->top_radio),
									  "Bottom");

  gtk_box_pack_start(GTK_BOX(pos_vbox), find_dialog->top_radio, FALSE, FALSE, 0);
  gtk_box_pack_start(GTK_BOX(pos_vbox), find_dialog->cursor_radio, FALSE, FALSE, 0);
  gtk_box_pack_start(GTK_BOX(pos_vbox), find_dialog->bottom_radio, FALSE, FALSE, 0);

  gtk_signal_connect(GTK_OBJECT(find_dialog->top_radio), "clicked",
		     GTK_SIGNAL_FUNC(top_clicked_cb),
		     (gpointer) find_dialog);
  gtk_signal_connect(GTK_OBJECT(find_dialog->cursor_radio), "clicked",
		     GTK_SIGNAL_FUNC(cursor_clicked_cb),
		     (gpointer) find_dialog);
  gtk_signal_connect(GTK_OBJECT(find_dialog->bottom_radio), "clicked",
		     GTK_SIGNAL_FUNC(bottom_clicked_cb),
		     (gpointer) find_dialog);

  /* focus stuff */
  gtk_widget_grab_focus(find_dialog->find_entry);
  gnome_dialog_editable_enters(GNOME_DIALOG(find_dialog), GTK_EDITABLE(find_dialog->find_entry));
  gnome_dialog_set_default(GNOME_DIALOG(find_dialog), GNOME_FIND_BUTTON_FIND);

  /* show it all */
  gtk_widget_show_all(pos_frame);

  gtk_box_pack_start(GTK_BOX(hbox2), pos_frame, TRUE, TRUE, 0);
}

static void gnome_find_dialog_destroy (GtkObject *object)
{
  GnomeFindDialog *find_dialog;

  g_return_if_fail(object != NULL);
  g_return_if_fail(GNOME_IS_FIND_DIALOG(object));

  find_dialog = GNOME_FIND_DIALOG(object);

  if(find_dialog->params.find_text)
    g_free(find_dialog->params.find_text);

  GTK_OBJECT_CLASS (parent_class)->destroy(object);
}

GtkWidget *gnome_find_dialog_new (const gchar *title,
				  const GnomeFindDialogParams *find_params,
				  gboolean show_case_sensitive,
				  gboolean show_wrap_search,
				  gboolean show_regex)
{
  GnomeFindDialog *find_dialog;

  GtkWidget *options_frame, *options_hbox, *case_check, *wrap_check, *regex_check;

  find_dialog = gtk_type_new(gnome_find_dialog_get_type());

  /* set window title */
  if(title)
    gtk_window_set_title(GTK_WINDOW(find_dialog), title);

  /* initialise parameters */
  if(find_params) {
    find_dialog->params = *find_params;
    if(find_params->find_text) {
      gtk_entry_set_text(GTK_ENTRY(find_dialog->find_entry),
			 find_params->find_text);
      gtk_editable_select_region(GTK_EDITABLE(find_dialog->find_entry),
				 0, -1);
    }
  }
  else {
    find_dialog->params.start_pos = GNOME_FIND_TOP;
    find_dialog->params.direction = GNOME_FIND_FORWARDS;
    find_dialog->params.case_sensitive = FALSE;
    find_dialog->params.wrap_search = FALSE;
    find_dialog->params.regex = FALSE;
  }

  find_dialog->params.find_text = NULL;

  switch(find_dialog->params.direction) {
  case GNOME_FIND_FORWARDS:
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(find_dialog->forwards_radio),
				 TRUE);
    break;

  case GNOME_FIND_BACKWARDS:
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(find_dialog->backwards_radio),
				 TRUE);
    break;
  }

  switch(find_dialog->params.start_pos) {
  case GNOME_FIND_TOP:
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(find_dialog->top_radio),
				 TRUE);
    break;

  case GNOME_FIND_CURSOR:
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(find_dialog->cursor_radio),
				 TRUE);
    break;

  case GNOME_FIND_BOTTOM:
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(find_dialog->bottom_radio),
				 TRUE);
    break;
  }

  /* options checkbuttons */
  if(show_case_sensitive || show_wrap_search || show_regex) {
    options_frame = gtk_frame_new("Options");
    options_hbox = gtk_hbox_new(TRUE, GNOME_PAD_SMALL);
    gtk_container_set_border_width(GTK_CONTAINER(options_frame), GNOME_PAD_SMALL);
    gtk_container_set_border_width(GTK_CONTAINER(options_hbox), GNOME_PAD_SMALL);
    gtk_container_add(GTK_CONTAINER(options_frame), options_hbox);
    
    if(show_case_sensitive) {
      case_check = gtk_check_button_new_with_label("Case sensitive");
      gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(case_check),
				   find_dialog->params.case_sensitive);
      gtk_box_pack_start(GTK_BOX(options_hbox), case_check, TRUE, TRUE, 0);
      gtk_signal_connect(GTK_OBJECT(case_check), "toggled",
			 GTK_SIGNAL_FUNC(case_toggled_cb),
			 (gpointer) find_dialog);
    }
    if(show_wrap_search) {
      wrap_check = gtk_check_button_new_with_label("Wrap search");
      gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(wrap_check),
				   find_dialog->params.wrap_search);
      gtk_box_pack_start(GTK_BOX(options_hbox), wrap_check, TRUE, TRUE, 0);
      gtk_signal_connect(GTK_OBJECT(wrap_check), "toggled",
			 GTK_SIGNAL_FUNC(wrap_toggled_cb),
			 (gpointer) find_dialog);
    }
    if(show_regex) {
      regex_check = gtk_check_button_new_with_label("Regular expression");
      gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(regex_check),
				   find_dialog->params.regex);
      gtk_box_pack_start(GTK_BOX(options_hbox), regex_check, TRUE, TRUE, 0);
      gtk_signal_connect(GTK_OBJECT(regex_check), "toggled",
			 GTK_SIGNAL_FUNC(regex_toggled_cb),
			 (gpointer) find_dialog);
    }
    
    gtk_widget_show_all(options_frame);
    
    gtk_box_pack_start(GTK_BOX(GNOME_DIALOG(find_dialog)->vbox), options_frame, FALSE, FALSE, 0);
  }

  return GTK_WIDGET(find_dialog);
}

gchar *gnome_find_dialog_get_find_text(GnomeFindDialog *dialog)
{
  g_return_val_if_fail(dialog != NULL, NULL);
  g_return_val_if_fail(GNOME_IS_FIND_DIALOG(dialog), NULL);

  return gtk_editable_get_chars(GTK_EDITABLE(dialog->find_entry), 0, -1);
}

static void entry_changed_cb(GtkEditable *editable, gpointer data)
{
  gchar *entry_chars;
  GnomeFindDialog *find_dialog;

  g_return_if_fail(editable != NULL);
  g_return_if_fail(GTK_IS_EDITABLE(editable));

  find_dialog = GNOME_FIND_DIALOG(data);

  g_return_if_fail(find_dialog != NULL);
  g_return_if_fail(GNOME_IS_FIND_DIALOG(find_dialog));

  entry_chars = gtk_editable_get_chars(editable, 0, -1);

  if((entry_chars != NULL) && (strlen(entry_chars) > 0)) {
    gnome_dialog_set_sensitive(GNOME_DIALOG(find_dialog), GNOME_FIND_BUTTON_FIND, TRUE);
    gnome_dialog_set_sensitive(GNOME_DIALOG(find_dialog), GNOME_FIND_BUTTON_FIND_AGAIN, TRUE);
  }
  else {
    gnome_dialog_set_sensitive(GNOME_DIALOG(find_dialog), GNOME_FIND_BUTTON_FIND, FALSE);
    gnome_dialog_set_sensitive(GNOME_DIALOG(find_dialog), GNOME_FIND_BUTTON_FIND_AGAIN, FALSE);
  }

  gnome_dialog_set_default(GNOME_DIALOG(find_dialog), GNOME_FIND_BUTTON_FIND);

  g_free(entry_chars);
}

static void forwards_clicked_cb(GtkButton *button, gpointer data)
{
  GnomeFindDialog *find_dialog;

  g_return_if_fail(button != NULL);
  g_return_if_fail(GTK_IS_BUTTON(button));

  find_dialog = GNOME_FIND_DIALOG(data);

  g_return_if_fail(find_dialog != NULL);
  g_return_if_fail(GNOME_IS_FIND_DIALOG(find_dialog));

  find_dialog->params.direction = GNOME_FIND_FORWARDS;
}

static void backwards_clicked_cb (GtkButton *button, gpointer data)
{
  GnomeFindDialog *find_dialog;

  g_return_if_fail(button != NULL);
  g_return_if_fail(GTK_IS_BUTTON(button));

  find_dialog = GNOME_FIND_DIALOG(data);

  g_return_if_fail(find_dialog != NULL);
  g_return_if_fail(GNOME_IS_FIND_DIALOG(find_dialog));

  find_dialog->params.direction = GNOME_FIND_BACKWARDS;
}

static void top_clicked_cb(GtkButton *button, gpointer data)
{
  GnomeFindDialog *find_dialog;

  g_return_if_fail(button != NULL);
  g_return_if_fail(GTK_IS_BUTTON(button));

  find_dialog = GNOME_FIND_DIALOG(data);

  g_return_if_fail(find_dialog != NULL);
  g_return_if_fail(GNOME_IS_FIND_DIALOG(find_dialog));

  find_dialog->params.start_pos = GNOME_FIND_TOP;
}

static void cursor_clicked_cb(GtkButton *button, gpointer data)
{
  GnomeFindDialog *find_dialog;

  g_return_if_fail(button != NULL);
  g_return_if_fail(GTK_IS_BUTTON(button));

  find_dialog = GNOME_FIND_DIALOG(data);

  g_return_if_fail(find_dialog != NULL);
  g_return_if_fail(GNOME_IS_FIND_DIALOG(find_dialog));

  find_dialog->params.start_pos = GNOME_FIND_CURSOR;
}

static void bottom_clicked_cb(GtkButton *button, gpointer data)
{
  GnomeFindDialog *find_dialog;

  g_return_if_fail(button != NULL);
  g_return_if_fail(GTK_IS_BUTTON(button));

  find_dialog = GNOME_FIND_DIALOG(data);

  g_return_if_fail(find_dialog != NULL);
  g_return_if_fail(GNOME_IS_FIND_DIALOG(find_dialog));

  find_dialog->params.start_pos = GNOME_FIND_BOTTOM;
}

static void case_toggled_cb(GtkToggleButton *button, gpointer data)
{
  GnomeFindDialog *find_dialog;

  g_return_if_fail(button != NULL);
  g_return_if_fail(GTK_IS_TOGGLE_BUTTON(button));

  find_dialog = GNOME_FIND_DIALOG(data);

  g_return_if_fail(find_dialog != NULL);
  g_return_if_fail(GNOME_IS_FIND_DIALOG(find_dialog));

  if(button->active) {
    find_dialog->params.case_sensitive = TRUE;
  }
  else {
    find_dialog->params.case_sensitive = FALSE;
  }
}

static void wrap_toggled_cb(GtkToggleButton *button, gpointer data)
{
  GnomeFindDialog *find_dialog;

  g_return_if_fail(button != NULL);
  g_return_if_fail(GTK_IS_TOGGLE_BUTTON(button));

  find_dialog = GNOME_FIND_DIALOG(data);

  g_return_if_fail(find_dialog != NULL);
  g_return_if_fail(GNOME_IS_FIND_DIALOG(find_dialog));

  if(button->active) {
    find_dialog->params.wrap_search = TRUE;
  }
  else {
    find_dialog->params.wrap_search = FALSE;
  }
}

static void regex_toggled_cb(GtkToggleButton *button, gpointer data)
{
  GnomeFindDialog *find_dialog;

  g_return_if_fail(button != NULL);
  g_return_if_fail(GTK_IS_TOGGLE_BUTTON(button));

  find_dialog = GNOME_FIND_DIALOG(data);

  g_return_if_fail(find_dialog != NULL);
  g_return_if_fail(GNOME_IS_FIND_DIALOG(find_dialog));

  if(button->active) {
    find_dialog->params.regex = TRUE;
  }
  else {
    find_dialog->params.regex = FALSE;
  }

  gnome_dialog_set_default(GNOME_DIALOG(find_dialog), GNOME_FIND_BUTTON_FIND);
}

static void dialog_clicked_cb(GnomeDialog *dialog, gint button, gpointer data)
{
  GnomeFindDialog *find_dialog;

  g_return_if_fail(dialog != NULL);
  g_return_if_fail(GNOME_IS_FIND_DIALOG(dialog));

  find_dialog = GNOME_FIND_DIALOG(dialog);

  switch(button) {
    case 0:
      gnome_dialog_set_default(dialog, GNOME_FIND_BUTTON_FIND_AGAIN);
      find(find_dialog);
      break;

    case 1:
      find_again(find_dialog);
      break;

    case 2:
      close(find_dialog);
      break;

    default:
      g_assert_not_reached();
  }
}

static void find(GnomeFindDialog *find_dialog)
{
  gtk_signal_emit(GTK_OBJECT(find_dialog), find_dialog_signals[FIND]);
}

static void find_again(GnomeFindDialog *find_dialog)
{
  gtk_signal_emit(GTK_OBJECT(find_dialog), find_dialog_signals[FIND_AGAIN]);
}

static void close(GnomeFindDialog *find_dialog)
{
  gnome_dialog_close(GNOME_DIALOG(find_dialog));
}

