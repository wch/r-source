#include "Defn.h"
#include "Fileio.h"

#include "gnome-find-dialog.h"
#include "terminal.h"
#include "terminal-find.h"


static GnomeFindDialogParams find_params;
static gint find_pos, find_pos_max;
static gboolean find_update_cache;
static gchar *find_text_cache;


void R_gtk_terminal_find(GnomeFindDialog *find_dialog)
{
  int find_text_len;
  int find_result;
  int strcmp_result;

  GtkWidget *notfound_dialog;

  if(find_dialog != NULL)
    g_return_if_fail(GNOME_IS_FIND_DIALOG(find_dialog));

  if(find_params.find_text == NULL)
    return;

  find_text_len = strlen(find_params.find_text);

  if((find_update_cache == TRUE) || (find_text_cache == NULL)) {
    find_text_cache = gtk_editable_get_chars(GTK_EDITABLE(R_gtk_terminal_text), 0, -1);
    find_pos_max = gtk_text_get_length(GTK_TEXT(R_gtk_terminal_text));
    find_update_cache = FALSE;
  }

#ifdef HAVE_REGCOMP
  if(find_params.regular_exp == TRUE) {
    /* regular expression search */
  }
  else {
#endif /* HAVE_REGCOMP */
    /* standard (non-regex) search */

    /* FIXME: wrapped searches */

    find_result = GNOME_FIND_NOTFOUND;

    while(find_result == GNOME_FIND_NOTFOUND) {
      switch(find_params.direction) {
      case GNOME_FIND_FORWARDS:
	if(find_pos <= find_pos_max)
	  find_pos++;
	if(find_pos > find_pos_max)
	  find_result = GNOME_FIND_NOMATCH;
	break;
	
      case GNOME_FIND_BACKWARDS:
	if(find_pos >= 0)
	  find_pos--;
	if(find_pos < 0)
	  find_result = GNOME_FIND_NOMATCH;
	break;
      }

      if(find_params.case_sensitive == TRUE)
	strcmp_result = strncmp(find_params.find_text, find_text_cache + find_pos, find_text_len);
      else
	strcmp_result = strncasecmp(find_params.find_text, find_text_cache + find_pos, find_text_len);

      if(strcmp_result == 0)
	find_result = GNOME_FIND_MATCH;
    }

    switch(find_result) {
    case GNOME_FIND_NOMATCH:
      notfound_dialog = gnome_message_box_new("Could not find text in console output.",
					      GNOME_MESSAGE_BOX_WARNING,
					      GNOME_STOCK_BUTTON_OK,
					      NULL);
      gnome_dialog_run_and_close(GNOME_DIALOG(notfound_dialog));
      if(find_dialog != NULL) 
	gnome_dialog_set_default(GNOME_DIALOG(find_dialog), GNOME_FIND_BUTTON_FIND);
      break;

    case GNOME_FIND_MATCH:
      gtk_editable_set_position(GTK_EDITABLE(R_gtk_terminal_text), find_pos + find_text_len);
      gtk_editable_select_region(GTK_EDITABLE(R_gtk_terminal_text), find_pos, find_pos + find_text_len);
      break;
    }

#ifdef HAVE_REGCOMP
  }
#endif /* HAVE_REGCOMP */
}

static void find_console_changed(GtkWidget *widget, gpointer data)
{
  g_free(find_text_cache);
  find_text_cache = NULL;
  find_update_cache = TRUE;
}

static void find_dialog_cb(GtkWidget *widget, gpointer data)
{
  g_return_if_fail(widget != NULL);
  g_return_if_fail(GNOME_IS_FIND_DIALOG(widget));

  if(find_params.find_text != NULL)
    g_free(find_params.find_text);

  find_params = GNOME_FIND_DIALOG(widget)->params;
  find_params.find_text = gnome_find_dialog_get_find_text(GNOME_FIND_DIALOG(widget));

  switch(find_params.start_pos) {
  case GNOME_FIND_TOP:
    find_pos = -1;
    break;

  case GNOME_FIND_CURSOR:
    switch(find_params.direction) {
    case GNOME_FIND_FORWARDS:
      find_pos = gtk_editable_get_position(GTK_EDITABLE(R_gtk_terminal_text)) - 1;
      break;

    case GNOME_FIND_BACKWARDS:
      find_pos = gtk_editable_get_position(GTK_EDITABLE(R_gtk_terminal_text)) + 1;
      break;
    }
    break;

  case GNOME_FIND_BOTTOM:
    find_pos = gtk_text_get_length(GTK_TEXT(R_gtk_terminal_text));
    break;
  }

  R_gtk_terminal_find(GNOME_FIND_DIALOG(widget));
}

static void find_again_dialog_cb(GtkWidget *widget, gpointer data)
{
  g_return_if_fail(widget != NULL);
  g_return_if_fail(GNOME_IS_FIND_DIALOG(widget));

  if(find_params.find_text != NULL)
    g_free(find_params.find_text);

  find_params = GNOME_FIND_DIALOG(widget)->params;
  find_params.find_text = gnome_find_dialog_get_find_text(GNOME_FIND_DIALOG(widget));

  R_gtk_terminal_find(GNOME_FIND_DIALOG(widget));
}

void edit_find_cb(GtkWidget *widget, gpointer data)
{
  GtkWidget *find_dialog;

#ifdef HAVE_REGCOMP
  find_dialog = gnome_find_dialog_new("Find text", NULL, TRUE, TRUE, TRUE);
#else
  find_dialog = gnome_find_dialog_new("Find text", NULL, TRUE, TRUE, FALSE);
#endif /* HAVE_REGCOMP */

  gnome_dialog_set_parent(GNOME_DIALOG(find_dialog), GTK_WINDOW(R_gtk_main_window));

  gtk_signal_connect(GTK_OBJECT(find_dialog),
		     "find",
		     (GtkSignalFunc) find_dialog_cb,
		     NULL);
  gtk_signal_connect(GTK_OBJECT(find_dialog),
		     "find_again",
		     (GtkSignalFunc) find_again_dialog_cb,
		     NULL);

  gtk_widget_show(find_dialog);
}

void edit_find_again_cb(GtkWidget *widget, gpointer data)
{
  R_gtk_terminal_find(NULL);
}

void R_gtk_terminal_find_init()
{
  /* Find functionality */
  find_params.find_text = NULL;
  find_pos = 0;
  find_update_cache = TRUE;
  find_text_cache = NULL;
  
  gtk_signal_connect(GTK_OBJECT(R_gtk_terminal_text), "changed",
		     (GtkSignalFunc) find_console_changed,
		     NULL);
}
