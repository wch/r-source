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

#include "Defn.h"
#include "Fileio.h"

#include "terminal.h"
#include "gtkconsole.h"

#include <gnome.h>

gboolean R_ChooseFile_result;
gboolean R_ChooseFile_closing;

void R_ChooseFile_ok(GtkWidget *widget, gpointer data)
{
  R_ChooseFile_result = TRUE;
  R_ChooseFile_closing = TRUE;

  gtk_main_quit();
}

void R_ChooseFile_cancel(GtkWidget *widget, gpointer data)
{
  R_ChooseFile_closing = TRUE;

  gtk_main_quit();
}

void R_ChooseFile_destroy(GtkWidget *widget, gpointer data)
{
  if(!R_ChooseFile_closing)
    gtk_main_quit();
}

int Rgnome_ChooseFile(int new, char *buf, int len)
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
