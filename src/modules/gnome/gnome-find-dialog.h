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


#ifndef __GNOME_FIND_DIALOG_H__
#define __GNOME_FIND_DIALOG_H__

#include <libgnomeui/gnome-dialog.h>

BEGIN_GNOME_DECLS

#define GNOME_FIND_DIALOG(Obj)         GTK_CHECK_CAST (Obj, gnome_find_dialog_get_type (), GnomeFindDialog)
#define GNOME_FIND_DIALOG_CLASS(Klass) GTK_CHECK_CLASS_CAST (Klass, gnome_find_dialog_get_type (), GnomeFindDialogClass)
#define GNOME_IS_FIND_DIALOG(Obj)      GTK_CHECK_TYPE (Obj, gnome_find_dialog_get_type ())


typedef enum
{
  GNOME_FIND_TOP,
  GNOME_FIND_CURSOR,
  GNOME_FIND_BOTTOM
} GnomeFindStartPos;

typedef enum
{
  GNOME_FIND_FORWARDS,
  GNOME_FIND_BACKWARDS
} GnomeFindDirection;

typedef enum
{
  GNOME_FIND_NOTFOUND,
  GNOME_FIND_MATCH,
  GNOME_FIND_NOMATCH
} GnomeFindResult;

typedef enum
{
  GNOME_FIND_BUTTON_FIND,
  GNOME_FIND_BUTTON_FIND_AGAIN,
  GNOME_FIND_BUTTON_CLOSE
} GnomeFindButtons;

typedef struct _GnomeFindDialogParams GnomeFindDialogParams;

struct _GnomeFindDialogParams
{
  GnomeFindStartPos start_pos;
  GnomeFindDirection direction;

  gboolean case_sensitive;
  gboolean wrap_search;
  gboolean regex;

  gchar *find_text;
};

typedef struct _GnomeFindDialog      GnomeFindDialog;
typedef struct _GnomeFindDialogClass GnomeFindDialogClass;

struct _GnomeFindDialog
{
  GnomeDialog dialog;

  GnomeFindDialogParams params;

  GtkWidget *find_entry;

  GtkWidget *top_radio, *cursor_radio, *bottom_radio;
  GtkWidget *forwards_radio, *backwards_radio;

  GtkWidget *find_button;
  GtkWidget *find_again_button;
  GtkWidget *close_button;
};

struct _GnomeFindDialogClass
{
  GnomeDialogClass parent_class;

  void (* find)       (GnomeFindDialog *find_dialog);
  void (* find_again) (GnomeFindDialog *find_dialog);
};

guint      gnome_find_dialog_get_type      (void);
GtkWidget *gnome_find_dialog_new           (const gchar *title,
				            const GnomeFindDialogParams *find_params,
				            gboolean show_case_sensitive,
				            gboolean show_wrap_search,
				            gboolean show_regex);
gchar     *gnome_find_dialog_get_find_text (GnomeFindDialog *dialog); /* returns allocated memory */



#endif /* __GNOME_FIND_DIALOG_H__ */

