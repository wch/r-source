
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

typedef struct _GnomeFindDialog      GnomeFindDialog;
typedef struct _GnomeFindDialogClass GnomeFindDialogClass;

struct _GnomeFindDialog
{
  GnomeDialog dialog;

  GtkWidget *exp_entry;

  GnomeFindStartPos start_pos;
  GnomeFindDirection direction;

  gboolean case_sensitive;
  gboolean wrap_search;
  gboolean regular_exp;

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

guint      gnome_find_dialog_get_type (void);
GtkWidget *gnome_find_dialog_new      (const gchar *title,
				       gboolean show_case_sensitive,
				       gboolean show_wrap_search,
				       gboolean show_reg_exp);



#endif /* __GNOME_FIND_DIALOG_H__ */

