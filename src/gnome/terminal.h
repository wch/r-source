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

#include <gnome.h>
#include <unistd.h>
#include <sys/stat.h>

#include "gtkconsole.h"

GtkWidget *R_gtk_main_window;
GtkWidget *R_gtk_terminal_text;
GtkWidget *R_gtk_terminal_appbar;
gint R_gtk_terminal_status_cid;
gboolean R_gtk_gui_quit;

/* track files being edited */
GList *R_gtk_editfiles;
typedef struct _R_gtk_edititem R_gtk_edititem;
struct _R_gtk_edititem {
  gchar *filename;
  time_t filetime;
};


/* saved user preferences */
enum save_choices {
  ASK,
  SAVE,
  SAVEAS,
  SAVEIF,
  DONTSAVE
};

typedef struct _R_gnome_pref_t R_gnome_pref_t;
struct _R_gnome_pref_t {
  gchar *font;
  GdkColor textcolor;
  GdkColor bgcolor;

  gchar *vsize;
  gchar *nsize;

  gint workspace_save;
  gint history_save;

  gchar *pager_title_font;
  GdkColor pager_title_textcolor;
  GdkColor pager_title_bgcolor;
  gchar *pager_text_font;
  gchar *pager_em_font;
  GdkColor pager_text_textcolor;
  GdkColor pager_text_bgcolor;
};

R_gnome_pref_t R_gnome_userprefs;
R_gnome_pref_t R_gnome_newprefs;

gboolean font_pref_locked;


/* functions */

void R_gtk_terminal_new();

void R_gnome_load_prefs();
void R_gnome_save_prefs();

