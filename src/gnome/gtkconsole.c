/* GTK - The GIMP Toolkit
 * Copyright (C) 1995-1997 Peter Mattis, Spencer Kimball and Josh MacDonald
 *
 * GtkConsole is written by Lyndon Drake.  Please send bug reports
 * and/or comments to lyndon@stat.auckland.ac.nz.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#include <gdk/gdkkeysyms.h>
#include <stdio.h>
#include <string.h>

#include "gtkconsole.h"


enum {
  CONSOLE_LINE_READY,
  CONSOLE_CHAR_READY,
  CONSOLE_INPUT_ENABLED,
  CONSOLE_INPUT_DISABLED,
  LAST_SIGNAL
};

/* widget stuff */
static void   gtk_console_class_init          (GtkConsoleClass   *klass);
static void   gtk_console_init                (GtkConsole        *console);
static void   gtk_console_destroy             (GtkObject        *object);


/* events */
static gint gtk_console_key_press      (GtkWidget        *widget,
					GdkEventKey      *event);
static void gtk_console_insert_text    (GtkConsole *console, const gchar *new_text, gint new_text_length, gint *position);
static void gtk_console_delete_text    (GtkConsole *console, gint start_pos, gint end_pos);
static void gtk_console_changed_post   (GtkConsole       *console);



static GtkWidgetClass *parent_class = NULL;

static guint console_signals[LAST_SIGNAL] = { 0 };


/* ********************* *
 * Basic Class Functions *
 * ********************* */

guint
gtk_console_get_type ()
{
  static guint console_type = 0;

  if (!console_type)
    {
      GtkTypeInfo console_info =
      {
	"GtkConsole",
	sizeof (GtkConsole),
	sizeof (GtkConsoleClass),
	(GtkClassInitFunc) gtk_console_class_init,
	(GtkObjectInitFunc) gtk_console_init,
	(GtkArgSetFunc) NULL,
        (GtkArgGetFunc) NULL,
      };

      console_type = gtk_type_unique (gtk_text_get_type (), &console_info);
    }

  return console_type;
}

static void
gtk_console_class_init (GtkConsoleClass *klass)
{
  GtkObjectClass *object_class;

  object_class = (GtkObjectClass*) klass;

  /* init local data */
  parent_class = gtk_type_class (gtk_text_get_type ());

  /* New signals */
  console_signals[CONSOLE_LINE_READY] =
    gtk_signal_new ("console_line_ready",
		    GTK_RUN_LAST,
		    object_class->type,
		    GTK_SIGNAL_OFFSET (GtkConsoleClass, console_char_ready),
		    gtk_marshal_NONE__NONE,
		    GTK_TYPE_NONE, 0);

  console_signals[CONSOLE_CHAR_READY] =
    gtk_signal_new ("console_char_ready",
		    GTK_RUN_LAST,
		    object_class->type,
		    GTK_SIGNAL_OFFSET (GtkConsoleClass, console_line_ready),
		    gtk_marshal_NONE__NONE,
		    GTK_TYPE_NONE, 0);

  console_signals[CONSOLE_INPUT_ENABLED] =
    gtk_signal_new ("console_input_enabled",
		    GTK_RUN_LAST,
		    object_class->type,
		    GTK_SIGNAL_OFFSET (GtkConsoleClass, console_input_enabled),
		    gtk_marshal_NONE__NONE, 
		    GTK_TYPE_NONE, 0);		    

  console_signals[CONSOLE_INPUT_DISABLED] =
    gtk_signal_new ("console_input_disabled",
		    GTK_RUN_LAST,
		    object_class->type,
		    GTK_SIGNAL_OFFSET (GtkConsoleClass, console_input_disabled),
		    gtk_marshal_NONE__NONE,
		    GTK_TYPE_NONE, 0);

		    gtk_object_class_add_signals(object_class, console_signals, LAST_SIGNAL); 

  /* setup signals */
  object_class->destroy = gtk_console_destroy;

  klass->console_char_ready = NULL;
  klass->console_line_ready = NULL;
  klass->console_input_enabled = NULL;
  klass->console_input_disabled = NULL;
}

static void
gtk_console_init (GtkConsole *console)
{
  /* Initialise variables */
  console -> history = NULL;
  console -> history_num_items = 0;
  console -> history_cur = NULL;

  console -> input_enabled = FALSE;
  console -> input_start_index = FALSE;
  console -> line_available = FALSE;

  console -> buffer_type = CONSOLE_BUF_NONE;
  console -> buffer_index = 0;

  /* to keep track of changes*/
  gtk_signal_connect (GTK_OBJECT (console), "insert_text",
		      GTK_SIGNAL_FUNC (gtk_console_insert_text),
		      NULL);
  gtk_signal_connect (GTK_OBJECT (console), "delete_text",
		      GTK_SIGNAL_FUNC (gtk_console_delete_text),
		      NULL);
  gtk_signal_connect (GTK_OBJECT (console), "changed",
		      GTK_SIGNAL_FUNC (gtk_console_changed_post),
		      NULL);
  gtk_signal_connect (GTK_OBJECT (console), "key_press_event",
		      GTK_SIGNAL_FUNC (gtk_console_key_press),
		      NULL);
}

GtkWidget*
gtk_console_new (GtkAdjustment *hadj,
		 GtkAdjustment *vadj)
{
  GtkConsole *console;

  console = gtk_type_new (gtk_console_get_type ());

  gtk_text_set_adjustments (GTK_TEXT (console), hadj, vadj);

  return GTK_WIDGET (console);
}

static void
gtk_console_destroy (GtkObject *object)
{
  g_return_if_fail (object != NULL);
  g_return_if_fail (GTK_IS_CONSOLE (object));


  GTK_OBJECT_CLASS(parent_class)->destroy (object);
}


/* ************************************** *
 * Public functions to access the console *
 * ************************************** */

/* Applications can use this to read the input.  It's
   probably only useful if a signal has been received
   indicating that there's something to read. */
void gtk_console_read(GtkConsole *object, gchar *buf, guint buf_len, gboolean add_to_history)
{
  gchar *text_chars;
  gchar *term_ptr;
  gchar *history_buf;

  g_return_if_fail (buf != NULL);
  /* make buf null-terminated, in case we can't copy */
  *buf = '\0';

  g_return_if_fail (buf_len > 0);
  g_return_if_fail (object != NULL);
  g_return_if_fail (GTK_IS_CONSOLE (object));

  /* copy up to buf_len characters into buf,
   * starting from input_start_index */
  text_chars = gtk_editable_get_chars(GTK_EDITABLE(object), object -> input_start_index, -1);

  g_return_if_fail (text_chars != NULL);

  strncpy(buf, text_chars, buf_len);

  /* add command to history */
  term_ptr = strchr(text_chars, '\n');
  if((add_to_history == TRUE) && (term_ptr != NULL) && (*text_chars != '\n')) {
    history_buf = g_malloc(term_ptr - text_chars + 1);
    *term_ptr = '\0';
    strncpy(history_buf, text_chars, term_ptr - text_chars + 1);

    GTK_CONSOLE(object)->history = g_list_prepend(GTK_CONSOLE(object)->history, history_buf);
    GTK_CONSOLE(object)->history_num_items++;
  }

  g_free(text_chars);
}

/* Prompt the user for input */
void gtk_console_enable_input(GtkConsole *object, gchar *prompt, guint prompt_len)
{
  g_return_if_fail (object != NULL);
  g_return_if_fail (GTK_IS_CONSOLE (object));

  gtk_console_flush(object);

  gtk_text_freeze(GTK_TEXT(object));
  
  /* Enable input */
  object -> input_enabled = TRUE;

  /* Append prompt to end of text box */
  if((prompt != NULL) && (*prompt != '\0') && (prompt_len != 0)) {
    gtk_text_set_point(GTK_TEXT(object), gtk_text_get_length(GTK_TEXT(object)));
    gtk_text_insert(GTK_TEXT(object), NULL, NULL, NULL, prompt, prompt_len);
  }

  /* Update input_start_index */
  object -> input_start_index = gtk_text_get_length(GTK_TEXT(object));

  /* Move point to end of text box */
  gtk_text_set_point(GTK_TEXT(object), gtk_text_get_length(GTK_TEXT(object)));

  gtk_text_thaw(GTK_TEXT(object));

  gtk_editable_set_position(GTK_EDITABLE(object), gtk_text_get_length(GTK_TEXT(object)));

  object->buffer_index = 0;

  /* reset history index */
  object->history_cur = NULL;
}

/* Disable input */
void gtk_console_disable_input(GtkConsole *object)
{
  g_return_if_fail (object != NULL);
  g_return_if_fail (GTK_IS_CONSOLE (object));

  object -> input_enabled = FALSE;
}

/* Write buf out.
 * gtk_console_write also disables input.
 * Buffers output a line at a time, up to 1Kb
 *
 * FIXME: if buf_len is greater than CONSOLE_MAX_BUF, the excess
 * is silently discarded.  We should split such buf's into
 * multiple writes.
 */
void gtk_console_write(GtkConsole *object, gchar *buf, guint buf_len)
{
  g_return_if_fail (object != NULL);
  g_return_if_fail (GTK_IS_CONSOLE (object));

  gtk_console_disable_input(object);

  switch(object->buffer_type) {
  case CONSOLE_BUF_LINE:
    /* run out of buffer space - flush the buffer to free some space */
    if((object->buffer_index > 0) && (object->buffer_index + buf_len > CONSOLE_MAX_BUF - 1)) {
      gtk_console_flush(object);
    }
    
    /* copy */
    strncpy(object->out_buf + object->buffer_index, buf, CONSOLE_MAX_BUF - object->buffer_index - 1);

    object->buffer_index = MIN(CONSOLE_MAX_BUF - 1, object->buffer_index + buf_len);
    object->out_buf[object->buffer_index] = '\0';

    /* check for newline */
    if(strchr(object->out_buf, '\n') != NULL) {
      gtk_console_flush(object);
    }
    break;

  case CONSOLE_BUF_BLOCK:
    /* run out of buffer space - flush the buffer to free some space */
    if((object->buffer_index > 0) && (object->buffer_index + buf_len > CONSOLE_MAX_BUF - 1)) {
      gtk_console_flush(object);
    }
    
    /* copy */
    strncpy(object->out_buf + object->buffer_index, buf, CONSOLE_MAX_BUF - object->buffer_index - 1);

    object->buffer_index = MIN(CONSOLE_MAX_BUF - 1, object->buffer_index + buf_len);
    object->out_buf[object->buffer_index] = '\0';
    break;

  default: /* no buffering */
    strncpy(object->out_buf, buf, CONSOLE_MAX_BUF - 1);
    object->out_buf[CONSOLE_MAX_BUF - 1] = '\0';
    gtk_console_flush(object);
    object->buffer_index = 0;
    object->out_buf[0] = '\0';
    break;
  }
}

void gtk_console_flush(GtkConsole *object)
{
  g_return_if_fail (object != NULL);
  g_return_if_fail (GTK_IS_CONSOLE (object));

  gtk_text_freeze(GTK_TEXT(object));

  /* FIXME: Update text properties */

  /* Append up to buf_len characters from buf to end of text box */
  gtk_text_set_point(GTK_TEXT(object), gtk_text_get_length(GTK_TEXT(object)));
  gtk_text_insert(GTK_TEXT(object), NULL, NULL, NULL, object->out_buf, strlen(object->out_buf));
  /* Move point to end of text box */
  gtk_text_set_point(GTK_TEXT(object), gtk_text_get_length(GTK_TEXT(object)));

  /* FIXME: Update text properties */

  gtk_text_thaw(GTK_TEXT(object));

  gtk_editable_set_position(GTK_EDITABLE(object), gtk_text_get_length(GTK_TEXT(object)));

  object->buffer_index = 0;
  object->out_buf[0] = '\0';
}

/* FIXME: return an error message if things go wrong */
/* FIXME: if errmsg is null, don't return an error message */
/* FIXME: read maxitems from history first, and use them to */
/*        fill up to maxitems after current history is written. */
/*        However, the simplest way to do that is for the app */
/*        to restore the history first. */
gboolean gtk_console_save_history(GtkConsole *object, gchar *filename, guint maxitems, gchar *errmsg)
{
  FILE *hist_file;
  GList *cur_item;
  gint n;

  g_return_val_if_fail (object != NULL, FALSE);
  g_return_val_if_fail (GTK_IS_CONSOLE (object), FALSE);

  hist_file = fopen(filename, "w");

  /* check file OK */
  if(hist_file == NULL)
    return FALSE;

  if(maxitems > object->history_num_items) 
    maxitems = object->history_num_items;

  cur_item = g_list_last(object->history);

  for(n = 0; n < maxitems, cur_item != NULL; n++, cur_item = cur_item->prev) {
    fputs(cur_item->data, hist_file);
    fputs("\n", hist_file);
  }

  return TRUE;
}

gboolean gtk_console_restore_history(GtkConsole *object, gchar *filename, guint maxitems, gchar *errmsg)
{
  FILE *hist_file;
  gchar line_buf[CONSOLE_MAX_BUF];
  gchar *history_buf, *term_ptr;
  gint n, buf_len;

  g_return_val_if_fail (object != NULL, FALSE);
  g_return_val_if_fail (GTK_IS_CONSOLE (object), FALSE);

  hist_file = fopen(filename, "r");

  /* check file ok */
  if(hist_file == NULL)
    return FALSE;

  for(n = 0; n < maxitems, !ferror(hist_file), !feof(hist_file); n++) {
    /* read line */
    line_buf[0] = '\0';
    if(fgets(line_buf, CONSOLE_MAX_BUF - 1, hist_file) != NULL) {
      line_buf[CONSOLE_MAX_BUF - 1] = '\0';
      
      /* remove \n if it's there */
      term_ptr = strchr(line_buf, '\n');
      if(term_ptr != NULL)
	*term_ptr = '\0';
      
      /* copy to real place */
      buf_len = strlen(line_buf) + 1;
      history_buf = g_malloc(buf_len);
      *history_buf = '\0';
      strncpy(history_buf, line_buf, buf_len - 1);
      *(history_buf + buf_len - 1) = '\0';
      
      /* add it to the list */
      object->history = g_list_prepend(object->history, history_buf);
      
      object->history_num_items++;
    }
  }

  object->history_cur = NULL;

  return TRUE;
}

guint gtk_console_get_input_start_index(GtkConsole *object)
{
  g_return_val_if_fail(object != NULL, 0);
  g_return_val_if_fail(GTK_IS_CONSOLE(object), 0);

  return object->input_start_index;
}

gboolean gtk_console_get_line_available(GtkConsole *object)
{
  g_return_val_if_fail(object != NULL, FALSE);
  g_return_val_if_fail(GTK_IS_CONSOLE(object), FALSE);

  return object->line_available;
}

gboolean gtk_console_get_input_enabled(GtkConsole *object)
{
  g_return_val_if_fail(object != NULL, FALSE);
  g_return_val_if_fail(GTK_IS_CONSOLE(object), FALSE);

  return object->input_enabled;
}


/* *************** *
 * Signal handlers *
 * *************** */

static void key_gdk_up(GtkConsole *console)
{
  gint insert_pos;
  gchar *history_buf;
  guint history_idx;
  GList *next = NULL;

  if(console->history_cur == NULL) {
    next = console->history;
  }
  else if(console->history_cur->next != NULL) {
    next = console->history_cur->next;
  }

  if(next != NULL) {
    console->history_cur = next;

    /* delete any existing input */
    if(gtk_text_get_length(GTK_TEXT(console)) > console->input_start_index)
      gtk_editable_delete_text(GTK_EDITABLE(console), console->input_start_index, gtk_text_get_length(GTK_TEXT(console)));

    history_buf = console->history_cur->data;

    if(history_buf != NULL) {
      insert_pos = gtk_text_get_length(GTK_TEXT(console));
      gtk_editable_insert_text(GTK_EDITABLE(console), history_buf, strlen(history_buf), &insert_pos);
    }
  }
}

static void key_gdk_down(GtkConsole *console)
{
  gint insert_pos;
  gchar *history_buf;
  guint history_idx;

  if(console->history_cur != NULL) {
    console->history_cur = console->history_cur->prev;

  /* delete any existing input */
    if(gtk_text_get_length(GTK_TEXT(console)) > console->input_start_index)
      gtk_editable_delete_text(GTK_EDITABLE(console), console->input_start_index, gtk_text_get_length(GTK_TEXT(console)));
    
    if(console->history_cur != NULL) {
      history_buf = console->history_cur->data;

      insert_pos = gtk_text_get_length(GTK_TEXT(console));
      gtk_editable_insert_text(GTK_EDITABLE(console), history_buf, strlen(history_buf), &insert_pos);
    }
  }
}

/* FIXME: Completion will have to be accomplished by a callback.
   The callback should have the same syntax as the readline
   completion callback. */
static gint gtk_console_key_press (GtkWidget *widget,
				   GdkEventKey *event)
{
  g_return_val_if_fail (widget != NULL, FALSE);
  g_return_val_if_fail (event != NULL, FALSE);
  g_return_val_if_fail (GTK_IS_CONSOLE (widget), FALSE);

  /* stuff that we only change if the point is in the input area */
  if(gtk_editable_get_position(GTK_EDITABLE(widget)) >= GTK_CONSOLE(widget)->input_start_index) {
    if(GTK_CONSOLE(widget)->input_enabled == TRUE) {
      switch(event->keyval) {

      case GDK_Tab:
	/* do completion */
	break;

      case GDK_Up:
	/* previous history item */
	key_gdk_up(GTK_CONSOLE(widget));
	gtk_signal_emit_stop_by_name(GTK_OBJECT(widget), "key_press_event");
	return TRUE;
	break;

      case GDK_Down:
	/* next history item */
	key_gdk_down(GTK_CONSOLE(widget));
	gtk_signal_emit_stop_by_name(GTK_OBJECT(widget), "key_press_event");
	return TRUE;
	break;

      case GDK_Left:
	if (!(event->state & GDK_CONTROL_MASK) && !(event->state & GDK_SHIFT_MASK) && (gtk_editable_get_position(GTK_EDITABLE(widget)) == GTK_CONSOLE(widget)->input_start_index)) {
	  gtk_signal_emit_stop_by_name(GTK_OBJECT(widget), "key_press_event");
	  return TRUE;
	}
	break;

      case GDK_Home:
	if (!(event->state & GDK_CONTROL_MASK) && !(event->state & GDK_SHIFT_MASK)) {
	  gtk_editable_set_position(GTK_EDITABLE(widget), GTK_CONSOLE(widget)->input_start_index);
	  gtk_signal_emit_stop_by_name(GTK_OBJECT(widget), "key_press_event");
	  return TRUE;
	}
	break;
      }
    }
  }

  /* things that apply wherever the point is */
  if(GTK_CONSOLE(widget)->input_enabled == TRUE) {
    switch(event->keyval) {

    case GDK_Return:
      gtk_editable_set_position(GTK_EDITABLE(widget), gtk_text_get_length(GTK_TEXT(widget)));
      GTK_CONSOLE(widget)->line_available = TRUE;
      break;

    }
  }
      
  return FALSE;
}

static void gtk_console_insert_text (GtkConsole *console,
				     const gchar *new_text,
				     gint new_text_length,
				     gint *position)
{
  g_return_if_fail(console != NULL);
  g_return_if_fail(GTK_IS_CONSOLE(console));

  if(console->input_enabled == FALSE) {
    gtk_signal_emit_stop_by_name(GTK_OBJECT(console), "insert_text");
  }
  else if(*position < console->input_start_index) {
    *position = console->input_start_index;
  }
}

static void gtk_console_delete_text (GtkConsole *console,
				     gint start_pos,
				     gint end_pos)
{
  g_return_if_fail(console != NULL);
  g_return_if_fail(GTK_IS_CONSOLE(console));

  if((console->input_enabled == FALSE) || (start_pos < console->input_start_index) || (end_pos < console->input_start_index)) {
    gtk_signal_emit_stop_by_name(GTK_OBJECT(console), "delete_text");
    gtk_widget_queue_draw(GTK_WIDGET(console));
  }
}

static void gtk_console_changed_post (GtkConsole *console)
{
  g_return_if_fail(console != NULL);
  g_return_if_fail(GTK_IS_CONSOLE(console));

  if((gtk_text_get_length(GTK_TEXT(console)) - console->input_start_index) > 0) {
    gtk_signal_emit(GTK_OBJECT(console), console_signals[CONSOLE_CHAR_READY]);
  }

  if(console->line_available == TRUE) {
    gtk_signal_emit(GTK_OBJECT(console), console_signals[CONSOLE_LINE_READY]);
    console->line_available = FALSE;
  }

  return;
}


