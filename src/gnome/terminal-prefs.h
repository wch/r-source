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

/* dialog widget */
GtkWidget *prefs_dialog;

/* functions */
GtkWidget *prefs_text_page(void);
GtkWidget *prefs_workspace_frame(void);
GtkWidget *prefs_history_frame(void);
GtkWidget *prefs_exit_page(void);
GtkWidget *prefs_apps_page(void);
GtkWidget *prefs_graphics_page(void);
GtkWidget *prefs_startup_page(void);
GtkWidget *prefs_pager_page(void);

void settings_prefs_cb(GtkWidget *widget, gpointer data);
