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

#ifndef __GNOME_TERMINAL_H__
#define __GNOME_TERMINAL_H__

#include <gnome.h>

#include "gtkconsole.h"

#define GLADE_INTERFACE_FILE "%s/share/glade/gnome-interface.glade"

/* functions */

void terminal_set_style(void);
void R_gtk_terminal_new();

/* variables */

#ifdef __GNOME_TERMINAL_C__
# define extern
#endif

extern GtkWidget *R_gtk_main_window;
extern GtkWidget *R_gtk_terminal_text;
extern GtkWidget *R_gtk_terminal_appbar;

extern gchar *glade_interface_file;

#endif /* __GNOME_TERMINAL_H__ */
