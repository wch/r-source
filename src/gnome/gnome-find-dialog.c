
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
      (GtkArgSetFunc) NULL
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
  find_dialog_signals[FIND] =
    gtk_signal_new ("find_again",
		    GTK_RUN_LAST,
		    object_class->type,
		    GTK_SIGNAL_OFFSET (GnomeFindDialogClass,
				       find_again),
		    gtk_marshal_NONE__NONE,
		    GTK_TYPE_NONE, 0);

  klass->find = NULL;
  klass->find_again = NULL;
}

static void gnome_find_dialog_init (GnomeFindDialog *find_dialog)
{
  GList *button_list;

  GtkWidget *hbox, *find_label, *find_entry;
  GtkWidget *hbox2;
  GtkWidget *pos_frame, *pos_vbox, *top_radio, *cursor_radio, *bottom_radio;
  GtkWidget *dir_frame, *dir_vbox, *forwards_radio, *backwards_radio;

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
  gtk_widget_set_sensitive(find_dialog->find_button, FALSE);
  button_list = button_list->next;

  find_dialog->find_again_button = GTK_WIDGET(button_list->data);
  gtk_widget_set_sensitive(find_dialog->find_again_button, FALSE);
  button_list = button_list->next;

  find_dialog->close_button = GTK_WIDGET(button_list->data);
  button_list = button_list->next;

  gtk_signal_connect(GTK_OBJECT(find_dialog), "clicked",
		     GTK_SIGNAL_FUNC(dialog_clicked_cb),
		     NULL);

  /* text entry */
  hbox = gtk_hbox_new(FALSE, GNOME_PAD_SMALL);
  gtk_container_set_border_width(GTK_CONTAINER(hbox), GNOME_PAD_SMALL);
  find_label = gtk_label_new("Find text:");
  find_entry = gtk_entry_new();

  gtk_box_pack_start(GTK_BOX(hbox), find_label, FALSE, FALSE, 0);
  gtk_box_pack_start(GTK_BOX(hbox), find_entry, TRUE, TRUE, 0);

  gtk_widget_show_all(hbox);

  gtk_box_pack_start(GTK_BOX(GNOME_DIALOG(find_dialog)->vbox), hbox, FALSE, FALSE, 0);

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

  forwards_radio = gtk_radio_button_new_with_label(NULL, "Forwards");
  backwards_radio = gtk_radio_button_new_with_label_from_widget(GTK_RADIO_BUTTON(forwards_radio), "Backwards");

  gtk_box_pack_start(GTK_BOX(dir_vbox), forwards_radio, FALSE, FALSE, 0);
  gtk_box_pack_start(GTK_BOX(dir_vbox), backwards_radio, FALSE, FALSE, 0);

  gtk_widget_show_all(dir_frame);

  gtk_box_pack_start(GTK_BOX(hbox2), dir_frame, TRUE, TRUE, 0);

  /* start position */
  pos_frame = gtk_frame_new("Start position");
  pos_vbox = gtk_vbox_new(FALSE, GNOME_PAD_SMALL);
  gtk_container_set_border_width(GTK_CONTAINER(pos_frame), GNOME_PAD_SMALL);
  gtk_container_set_border_width(GTK_CONTAINER(pos_vbox), GNOME_PAD_SMALL);
  gtk_container_add(GTK_CONTAINER(pos_frame), pos_vbox);

  top_radio = gtk_radio_button_new_with_label(NULL, "Top");
  cursor_radio = gtk_radio_button_new_with_label_from_widget(GTK_RADIO_BUTTON(top_radio), "Cursor position");
  bottom_radio = gtk_radio_button_new_with_label_from_widget(GTK_RADIO_BUTTON(top_radio), "Bottom");

  gtk_box_pack_start(GTK_BOX(pos_vbox), top_radio, FALSE, FALSE, 0);
  gtk_box_pack_start(GTK_BOX(pos_vbox), cursor_radio, FALSE, FALSE, 0);
  gtk_box_pack_start(GTK_BOX(pos_vbox), bottom_radio, FALSE, FALSE, 0);

  gtk_widget_show_all(pos_frame);

  gtk_box_pack_start(GTK_BOX(hbox2), pos_frame, TRUE, TRUE, 0);
}

static void gnome_find_dialog_destroy (GtkObject *object)
{
  GnomeFindDialog *find_dialog;

  g_return_if_fail(object != NULL);
  g_return_if_fail(GNOME_IS_FIND_DIALOG(object));

  find_dialog = GNOME_FIND_DIALOG(object);

  GTK_OBJECT_CLASS (parent_class)->destroy(object);
}

GtkWidget *gnome_find_dialog_new (const gchar *title,
				  gboolean show_case_sensitive,
				  gboolean show_wrap_search,
				  gboolean show_reg_exp)
{
  GnomeFindDialog *find_dialog;

  GtkWidget *options_frame, *options_hbox, *case_check, *wrap_check, *reg_exp_check;

  find_dialog = gtk_type_new(gnome_find_dialog_get_type());

  /* set window title */
  if(title)
    gtk_window_set_title(GTK_WINDOW(find_dialog), title);

  /* options checkbuttons */
  if(show_case_sensitive || show_wrap_search || show_reg_exp) {
    options_frame = gtk_frame_new("Options");
    options_hbox = gtk_hbox_new(TRUE, GNOME_PAD_SMALL);
    gtk_container_set_border_width(GTK_CONTAINER(options_frame), GNOME_PAD_SMALL);
    gtk_container_set_border_width(GTK_CONTAINER(options_hbox), GNOME_PAD_SMALL);
    gtk_container_add(GTK_CONTAINER(options_frame), options_hbox);
    
    if(show_case_sensitive) {
      case_check = gtk_check_button_new_with_label("Case sensitive");
      gtk_box_pack_start(GTK_BOX(options_hbox), case_check, TRUE, TRUE, 0);
    }
    if(show_wrap_search) {
      wrap_check = gtk_check_button_new_with_label("Wrap search");
      gtk_box_pack_start(GTK_BOX(options_hbox), wrap_check, TRUE, TRUE, 0);
    }
    if(show_reg_exp) {
      reg_exp_check = gtk_check_button_new_with_label("Regular expression");
      gtk_box_pack_start(GTK_BOX(options_hbox), reg_exp_check, TRUE, TRUE, 0);
    }
    
    gtk_widget_show_all(options_frame);
    
    gtk_box_pack_start(GTK_BOX(GNOME_DIALOG(find_dialog)->vbox), options_frame, FALSE, FALSE, 0);
  }

  return GTK_WIDGET(find_dialog);
}

static void dialog_clicked_cb(GnomeDialog *dialog, gint button, gpointer data)
{
  GnomeFindDialog *find_dialog;

  g_return_if_fail(dialog != NULL);
  g_return_if_fail(GNOME_IS_FIND_DIALOG(dialog));

  find_dialog = GNOME_FIND_DIALOG(dialog);

  switch(button) {
    case 0:
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

