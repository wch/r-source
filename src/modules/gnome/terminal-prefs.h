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

#include <gnome.h>

/* in system.c */
void R_set_SaveAction(int sa);

/* functions */
void settings_prefs_cb(GtkWidget *widget, gpointer data);

void R_gnome_prefs_gui_load(void);
void R_gnome_prefs_cmd_load(int defrestoreact, int defsaveact);
void R_gnome_prefs_save(void);

int prefs_get_restoreact(void);
int prefs_get_vsize(void);
int prefs_get_nsize(void);
int prefs_get_saveact(void);
gchar *prefs_get_console_font(void);
const GdkColor * prefs_get_console_textcolor(void);
const GdkColor * prefs_get_console_outputcolor(void);
const GdkColor * prefs_get_console_bgcolor(void);
gchar *prefs_get_pager_title_font(void);
GdkColor prefs_get_pager_title_textcolor(void);
GdkColor prefs_get_pager_title_bgcolor(void);
gchar *prefs_get_pager_text_font(void);
gchar *prefs_get_pager_em_font(void);
GdkColor prefs_get_pager_text_textcolor(void);
GdkColor prefs_get_pager_text_bgcolor(void);

GtkWidget *prefs_text_page(void);
GtkWidget *prefs_workspace_frame(void);
GtkWidget *prefs_history_frame(void);
GtkWidget *prefs_exit_page(void);
GtkWidget *prefs_apps_page(void);
GtkWidget *prefs_graphics_page(void);
GtkWidget *prefs_startup_page(void);
GtkWidget *prefs_pager_page(void);

void settings_prefs_cb(GtkWidget *widget, gpointer data);
