#include "Defn.h"
#include "Fileio.h"

#include "terminal.h"
#include "gtkconsole.h"

#include <gnome.h>

gboolean R_ChooseFile_result;
gboolean R_ChooseFile_closing;

R_ChooseFile_ok(GtkWidget *widget, gpointer data)
{
  R_ChooseFile_result = TRUE;
  R_ChooseFile_closing = TRUE;

  gtk_main_quit();
}

R_ChooseFile_cancel(GtkWidget *widget, gpointer data)
{
  R_ChooseFile_closing = TRUE;

  gtk_main_quit();
}

R_ChooseFile_destroy(GtkWidget *widget, gpointer data)
{
  if(!R_ChooseFile_closing)
    gtk_main_quit();
}

int R_ChooseFile(int new, char *buf, int len)
{
  GtkWidget *fs;
  gchar *fname;

  R_ChooseFile_result = FALSE;
  R_ChooseFile_closing = FALSE;
  *buf = '\0';

  fs = gtk_file_selection_new("Choose file name");

  gtk_window_set_transient_for(GTK_WINDOW(fs), GTK_WINDOW(R_gtk_main_window));
  gtk_window_set_modal(GTK_WINDOW(fs), TRUE);

  gtk_signal_connect(GTK_OBJECT(GTK_FILE_SELECTION(fs)->ok_button),
		     "clicked",
		     (GtkSignalFunc) R_ChooseFile_ok,
		     NULL);
  gtk_signal_connect(GTK_OBJECT(GTK_FILE_SELECTION(fs)->cancel_button),
		     "clicked",
		     (GtkSignalFunc) R_ChooseFile_cancel,
		     NULL);
  gtk_signal_connect(GTK_OBJECT(fs),
		     "delete",
		     (GtkSignalFunc) R_ChooseFile_cancel,
		     NULL);
  gtk_signal_connect(GTK_OBJECT(fs),
		     "destroy",
		     (GtkSignalFunc) R_ChooseFile_cancel,
		     NULL);

  gtk_widget_show(fs);
  gtk_main();

  if(R_ChooseFile_result) {
    fname = gtk_file_selection_get_filename(GTK_FILE_SELECTION(fs));
    strncpy(buf, fname, len);
    buf[len - 1] = '\0';
  }

  gtk_widget_destroy(fs);

  return strlen(buf);
}
