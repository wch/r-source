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

#include "terminal.h"
#include "terminal-toolbar.h"
#include "terminal-functions.h"

static void toolbar_generic(GtkWidget *widget, gpointer data)
{
}

static void toolbar_cut(GtkWidget *widget, gpointer data)
{
  gtk_editable_cut_clipboard(GTK_EDITABLE(R_gtk_terminal_text));
}

static void toolbar_copy(GtkWidget *widget, gpointer data)
{
  gtk_editable_copy_clipboard(GTK_EDITABLE(R_gtk_terminal_text));
}

static void toolbar_paste(GtkWidget *widget, gpointer data)
{
  gtk_editable_paste_clipboard(GTK_EDITABLE(R_gtk_terminal_text));
}

static void toolbar_interrupt(GtkWidget *widget, gpointer data)
{
  R_gtk_terminal_interrupt();
}

static GnomeUIInfo main_toolbar[] =
{
  GNOMEUIINFO_ITEM_STOCK("Print", "Print console text", toolbar_generic, GNOME_STOCK_PIXMAP_PRINT),
  GNOMEUIINFO_SEPARATOR,
  GNOMEUIINFO_ITEM_STOCK("Cut", "Cut the selection", toolbar_cut, GNOME_STOCK_PIXMAP_CUT),
  GNOMEUIINFO_ITEM_STOCK("Copy", "Copy the selection", toolbar_copy, GNOME_STOCK_PIXMAP_COPY),
  GNOMEUIINFO_ITEM_STOCK("Paste", "Paste the clipboard", toolbar_paste, GNOME_STOCK_PIXMAP_PASTE),
  GNOMEUIINFO_SEPARATOR,
  GNOMEUIINFO_ITEM_STOCK("Interrupt", "Interrupt R processing", toolbar_interrupt, GNOME_STOCK_PIXMAP_STOP),
  GNOMEUIINFO_END
};

void R_gtk_terminal_add_toolbar(GtkWidget *window)
{
  gnome_app_create_toolbar(GNOME_APP(window), main_toolbar);
}

