/*
 *  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (C) 1998-2004   Lyndon Drake
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
/* 
   GtkConsole is a GTK+ Object derived from GtkText with the following
   additional capabilities:

   * Designed for input/output
     - Separate colours may be assigned to input and output
   * history mechanism for recalling previous commands
   * Buffered output with three options
     - No buffering: output is always flushed
     - Line buffering: output is flushed when a new line is input
     - Block buffering: output is not flushed until buffer is full

*/

#ifndef __GTK_CONSOLE_H__
#define __GTK_CONSOLE_H__

#include <gdk/gdk.h>
#include <gtk/gtktext.h>
#include <gtk/gtksignal.h>

enum
{
    CONSOLE_BUF_NONE,
    CONSOLE_BUF_LINE,
    CONSOLE_BUF_BLOCK
};

#define CONSOLE_MAX_BUF 1024

#define GTK_CONSOLE(obj) \
GTK_CHECK_CAST (obj, gtk_console_get_type (), GtkConsole)
#define GTK_CONSOLE_CLASS(klass)  \
GTK_CHECK_CLASS_CAST (klass, gtk_console_get_type (), GtkConsoleClass)
#define GTK_IS_CONSOLE(obj)  GTK_CHECK_TYPE (obj, gtk_console_get_type ())
  
  typedef struct _GtkConsole GtkConsole;
  typedef struct _GtkConsoleClass GtkConsoleClass;
  
  struct _GtkConsole
  {
    GtkText text;

    GList *history;
    guint history_index;
    GList *history_cur;
    guint history_num_items;

    GdkColor input_color;
    GdkColor output_color;
    GdkColor bg_color;

    gboolean input_enabled;
    guint input_start_index;
    gboolean line_available;

    guint buffer_type;
    guint buffer_index;
    gchar out_buf[CONSOLE_MAX_BUF];
    /*  gchar in_buf[1024]; */

    GdkColormap *cmap;
  };

  struct _GtkConsoleClass
  {
    GtkTextClass parent_class;

     /* Are these ever used? Doesn't look like it.*/
    void (*console_char_ready) (GtkConsole * console);
    void (*console_line_ready) (GtkConsole * console);
    void (*console_line_ready_with_value) (GtkConsole * console, gpointer);
    void (*console_input_enabled) (GtkConsole * console);
    void (*console_input_disabled) (GtkConsole * console);
  };

/* Gtk+ widget functions */
  guint gtk_console_get_type ();
  GtkWidget *gtk_console_new (GtkAdjustment * hadj, GtkAdjustment * vadj);

/* GtkConsole prototypes */

/* Once signalled that there's something available, put it in buf */
  void gtk_console_read (GtkConsole * object, gchar * buf, guint buf_len,
			 gboolean add_to_history);

/* Prompt the user for input */
  void gtk_console_enable_input (GtkConsole * object, gchar * prompt,
				 guint prompt_len);

/* Disable input */
  void gtk_console_disable_input (GtkConsole * object);

/* Write buf out */
  void gtk_console_write (GtkConsole * object, gchar * buf, guint buf_len);
  void gtk_console_flush (GtkConsole * object);

/* History functions */
  void gtk_console_clear_history (GtkConsole * object);
  gboolean gtk_console_save_history (GtkConsole * object, gchar * filename,
				     guint maxitems, gchar * errmsg);
  gboolean gtk_console_restore_history (GtkConsole * object, gchar * filename,
					guint maxitems, gchar * errmsg);

/* misc functions - don't know whether they're any use */
  guint gtk_console_get_input_start_index (GtkConsole * object);
  gboolean gtk_console_get_line_available (GtkConsole * object);
  gboolean gtk_console_get_input_enabled (GtkConsole * object);


#endif /* __GTK_CONSOLE_H__ */
