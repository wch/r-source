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

#include <limits.h>
#include <stdio.h>

#include "Defn.h"
#include "Graphics.h"
#include "Print.h"
#include "Fileio.h"
#include "IOStuff.h"
#include "Parse.h"
#include "Rversion.h"

#include "terminal.h"
#include "terminal-find.h"
#include "terminal-functions.h"
#include "terminal-menu.h"
#include "terminal-prefs.h"

/* Some menu callbacks are here, others are in terminal-functions.c */
/* Find callbacks are in terminal-find.c */


static void file_exit_cb(GtkWidget *widget,
			 gpointer data)
{
  R_gtk_terminal_quit();
}



static void edit_cut_cb(GtkWidget *widget, gpointer data)
{
  gtk_editable_cut_clipboard(GTK_EDITABLE(R_gtk_terminal_text));
}

static void edit_copy_cb(GtkWidget *widget, gpointer data)
{
  gtk_editable_copy_clipboard(GTK_EDITABLE(R_gtk_terminal_text));
}

static void edit_paste_cb(GtkWidget *widget, gpointer data)
{
  gtk_editable_paste_clipboard(GTK_EDITABLE(R_gtk_terminal_text));
  gtk_editable_select_region(GTK_EDITABLE(R_gtk_terminal_text),
			     gtk_editable_get_position(GTK_EDITABLE(R_gtk_terminal_text)),
			     gtk_editable_get_position(GTK_EDITABLE(R_gtk_terminal_text)));
}

static void edit_copy_paste_cb(GtkWidget *widget, gpointer data)
{
  gtk_editable_copy_clipboard(GTK_EDITABLE(R_gtk_terminal_text));
  gtk_editable_paste_clipboard(GTK_EDITABLE(R_gtk_terminal_text));
  gtk_editable_select_region(GTK_EDITABLE(R_gtk_terminal_text),
			     gtk_editable_get_position(GTK_EDITABLE(R_gtk_terminal_text)),
			     gtk_editable_get_position(GTK_EDITABLE(R_gtk_terminal_text)));
}

static void edit_clear_cb(GtkWidget *widget, gpointer data)
{
  gtk_editable_delete_selection(GTK_EDITABLE(R_gtk_terminal_text));
}



static void data_loadcode_cb(GtkWidget *widget, gpointer data)
{
  R_gtk_edititem *edititem;
  struct stat sb;
  GList *curfile = R_gtk_editfiles;

  while(curfile != NULL) {
    edititem = (R_gtk_edititem *) curfile->data;

    stat(edititem->filename, &sb);

    if(edititem->filetime != sb.st_mtime) {
      /* the file has been modified */
    }

    edititem->filetime = sb.st_mtime;

    curfile = g_list_next(curfile);
  }
}



static void commands_interrupt_cb(GtkWidget *widget, gpointer data)
{
  R_gtk_terminal_interrupt();
}

static void commands_source_ok(GtkWidget *widget, gpointer data)
{
  gchar *fname;

  fname = gtk_file_selection_get_filename(GTK_FILE_SELECTION(data));
  R_gtk_terminal_run_initial();
  R_gtk_terminal_run_partial("source(\"");
  R_gtk_terminal_run_partial(fname);
  R_gtk_terminal_run_final("\")");

  gtk_widget_destroy(GTK_WIDGET(data));
}

static void commands_source_cb(GtkWidget *widget, gpointer data)
{
  GtkWidget *fs;

  fs = gtk_file_selection_new("Source R file");

  gtk_window_set_transient_for(GTK_WINDOW(fs), GTK_WINDOW(R_gtk_main_window));
  gtk_window_set_modal(GTK_WINDOW(fs), TRUE);

  gtk_signal_connect_object(GTK_OBJECT(GTK_FILE_SELECTION(fs)->cancel_button),
			      "clicked",
			      (GtkSignalFunc) gtk_widget_destroy,
			      GTK_OBJECT(fs));
  gtk_signal_connect(GTK_OBJECT(GTK_FILE_SELECTION(fs)->ok_button),
		     "clicked",
		     (GtkSignalFunc) commands_source_ok,
		     GTK_OBJECT(fs));

  gtk_widget_show(fs);
}



static void graphics_new_cb(GtkWidget *widget, gpointer data) 
{
  R_gtk_terminal_run("X11()\n");
}

static void graphics_close_cb(GtkWidget *widget, gpointer data)
{
  R_gtk_terminal_run("dev.off()\n");
}

static void graphics_closeall_cb(GtkWidget *widget, gpointer data)
{
  R_gtk_terminal_run("graphics.off()\n");
}



static void help_html_index_cb(GtkWidget *widget, gpointer data)
{
  R_gtk_terminal_run("help.start()\n");
}

static void help_text_index_cb(GtkWidget *widget, gpointer data)
{
  R_gtk_terminal_run("help()\n");
}

static void help_license_cb(GtkWidget *widget,
			    gpointer data)
{
  R_gtk_terminal_run("?license\n");
}

static void help_contributors_cb(GtkWidget *widget,
				 gpointer data)
{
  R_gtk_terminal_run("?contributors\n");
}

static void help_demos_index_cb(GtkWidget *widget,
				gpointer data)
{
  R_gtk_terminal_run("demo()\n");
}

static void help_demos_run_cb(GtkWidget *widget,
			      gpointer data)
{
  R_gtk_terminal_run_initial();
  R_gtk_terminal_run_partial("demo(\"");
  R_gtk_terminal_run_partial((gchar *) data);
  R_gtk_terminal_run_final("\")\n");
}



static void help_about_cb(GtkWidget *widget,
			  gpointer data)
{
  GtkWidget *about_box, *hbox, *home_href, *FAQ_href;
  gchar *version;
  gchar *copyright;

  const gchar *authors[] = {
    "Douglas Bates",
    "Peter Dalgaard",
    "Robert Gentleman",
    "Kurt Hornik", 
    "Ross Ihaka",
    "Friedrich Leisch",
    "Thomas Lumley",
    "Martin Maechler",
    "Guido Masarotto",
    "Paul Murrell", 
    "Brian Ripley",
    "Heiner Schwarte",
    "Duncan Temple Lang", 
    "Luke Tierney",
    "Lyndon Drake, GNOME interface",
    NULL
  };

  version = g_strdup_printf("%s.%s %s (%s %s, %s)", R_MAJOR, R_MINOR, R_STATUS, R_MONTH, R_DAY, R_YEAR);
  copyright = g_strdup_printf("Copyright (C) %s R Core Team", R_YEAR);
  
  g_assert(version != NULL);
  g_assert(copyright != NULL);

  about_box = gnome_about_new("R", version, copyright, authors,
			      "R is a system for statistical computation and graphics.  It is a dialect of the S programming language from Bell Labs.  R is free software and comes with ABSOLUTELY NO WARRANTY.  You are welcome to redistribute it under certain conditions.  Type	?license or ?licence for distribution details.  R is a collaborative project with many contributors.  Type ?contributors for a list.",
			      "R-logo-sm.xpm");

  hbox = gtk_hbox_new(TRUE, 0);
  home_href = gnome_href_new("http://lib.stat.cmu.edu/R/CRAN/", "R home page");
  FAQ_href = gnome_href_new("http://www.ci.tuwien.ac.at/~hornik/R/R-FAQ.html", "R FAQ");
  gtk_box_pack_start(GTK_BOX(hbox), home_href, FALSE, FALSE, 0);
  gtk_box_pack_start(GTK_BOX(hbox), FAQ_href, FALSE, FALSE, 0);
  gtk_box_pack_start(GTK_BOX(GNOME_DIALOG(about_box)->vbox),
		     hbox, TRUE, FALSE, 0);
  gtk_widget_show_all(hbox);

  gnome_dialog_set_parent(GNOME_DIALOG(about_box), GTK_WINDOW(R_gtk_main_window));

  gnome_dialog_run_and_close(GNOME_DIALOG(about_box));

  g_free(version);
  g_free(copyright);
}




static void generic_cb(GtkWidget *widget, gpointer data)
{
    GtkWidget *dialog;

    dialog = gnome_message_box_new("This menu item is currently unimplemented",
				   GNOME_MESSAGE_BOX_INFO,
				   GNOME_STOCK_BUTTON_CLOSE,
				   NULL);

    gnome_dialog_set_parent(GNOME_DIALOG(dialog), GTK_WINDOW(R_gtk_main_window));
    gtk_window_set_modal(GTK_WINDOW(dialog), TRUE);
    gnome_dialog_set_default(GNOME_DIALOG(dialog), 0);

    gnome_dialog_run_and_close(GNOME_DIALOG(dialog));
}

static GnomeUIInfo file_menu[] =
{
  { GNOME_APP_UI_ITEM, "_Open...", "Open a saved workspace image", R_gtk_terminal_file_open, NULL, NULL, GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_OPEN, GNOME_KEY_NAME_OPEN, GNOME_KEY_MOD_NEW, NULL },
  { GNOME_APP_UI_ITEM, "Save", "Save the workspace image", R_gtk_terminal_file_save, NULL, NULL, GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_SAVE, GNOME_KEY_NAME_SAVE, GNOME_KEY_MOD_SAVE, NULL },
  { GNOME_APP_UI_ITEM, "Save _As...", "Save the workspace image to a file", R_gtk_terminal_file_saveas, NULL, NULL, GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_SAVE_AS, GNOME_KEY_NAME_SAVE_AS, GNOME_KEY_MOD_SAVE_AS, NULL },
  GNOMEUIINFO_SEPARATOR,
  { GNOME_APP_UI_ITEM, "_Print...", "Print the console output", generic_cb, NULL, NULL, GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_PRINT, 0, (GdkModifierType)0, NULL },
  GNOMEUIINFO_MENU_PRINT_SETUP_ITEM(generic_cb, NULL),
  GNOMEUIINFO_SEPARATOR,
  GNOMEUIINFO_MENU_EXIT_ITEM(file_exit_cb, NULL),
  GNOMEUIINFO_END
};

static GnomeUIInfo edit_menu[] = 
{
  GNOMEUIINFO_MENU_CUT_ITEM(edit_cut_cb, NULL),
  GNOMEUIINFO_MENU_COPY_ITEM(edit_copy_cb, NULL),
  GNOMEUIINFO_MENU_PASTE_ITEM(edit_paste_cb, NULL),
  GNOMEUIINFO_ITEM_NONE("Copy and Paste", "Copy the selection and then paste it", edit_copy_paste_cb),
  GNOMEUIINFO_MENU_CLEAR_ITEM(edit_clear_cb, NULL),
  GNOMEUIINFO_SEPARATOR,
  GNOMEUIINFO_MENU_FIND_ITEM(edit_find_cb, NULL),
  GNOMEUIINFO_MENU_FIND_AGAIN_ITEM(edit_find_again_cb, NULL),
  GNOMEUIINFO_END
};

static GnomeUIInfo data_menu[] =
{
  GNOMEUIINFO_ITEM_NONE("Edit Object...", "Use an editor to edit an R object", generic_cb),
  GNOMEUIINFO_ITEM_NONE("Edit Vector...", "Use a spreadsheet to edit an R object", generic_cb),
  GNOMEUIINFO_ITEM_NONE("Reload Files", "Reload objects being edited", generic_cb),
  GNOMEUIINFO_END
};

static GnomeUIInfo graphics_menu[] =
{
  GNOMEUIINFO_ITEM_STOCK("_New Window", "Create a new graphics window", graphics_new_cb, GNOME_STOCK_MENU_NEW),
  GNOMEUIINFO_ITEM_STOCK("_Close Active Device", "Close the active graphics device", graphics_close_cb, GNOME_STOCK_MENU_CLOSE),
  GNOMEUIINFO_ITEM_NONE("Close _All Devices", "Close all graphics devices", graphics_closeall_cb),
  GNOMEUIINFO_END
};

static GnomeUIInfo commands_menu[] =
{
  { GNOME_APP_UI_ITEM, "_Interrupt", "Interrupt R processing (SIGTERM)", commands_interrupt_cb, NULL, NULL, GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_STOP, GDK_Escape, (GdkModifierType)0, NULL },
  GNOMEUIINFO_SEPARATOR,
  GNOMEUIINFO_SUBTREE("_Data", data_menu),
  GNOMEUIINFO_SUBTREE("_Graphics", graphics_menu),
  GNOMEUIINFO_SEPARATOR,
  GNOMEUIINFO_ITEM_NONE("grep...", "Search for matches to a regular expression within a vector of character strings", generic_cb),
  GNOMEUIINFO_ITEM_NONE("setwd...", "Set the working directory", generic_cb),
  GNOMEUIINFO_ITEM_NONE("source...", "Load a file containing R source", commands_source_cb),
  GNOMEUIINFO_END
};

static GnomeUIInfo settings_menu[] =
{
  GNOMEUIINFO_MENU_PREFERENCES_ITEM(settings_prefs_cb, NULL),
  GNOMEUIINFO_END
};

/* FIXME: this should display a list of the other R windows owned by this process */
static GnomeUIInfo windows_menu[] =
{
  GNOMEUIINFO_END
};

static GnomeUIInfo help_demos_menu[] =
{
  GNOMEUIINFO_ITEM_NONE("_Index", NULL, help_demos_index_cb),
  GNOMEUIINFO_SEPARATOR,
  GNOMEUIINFO_ITEM_DATA("_graphics", NULL, help_demos_run_cb, "graphics", NULL),
  GNOMEUIINFO_ITEM_DATA("_image", NULL, help_demos_run_cb, "image", NULL),
  GNOMEUIINFO_ITEM_DATA("_lm.glm", NULL, help_demos_run_cb, "lm.glm", NULL),
  GNOMEUIINFO_ITEM_DATA("glm._vr", NULL, help_demos_run_cb, "glm.vr", NULL),
  GNOMEUIINFO_ITEM_DATA("_nlm", NULL, help_demos_run_cb, "nlm", NULL),
  GNOMEUIINFO_ITEM_DATA("_recursion", NULL, help_demos_run_cb, "recursion", NULL),
  GNOMEUIINFO_ITEM_DATA("_scoping", NULL, help_demos_run_cb, "scoping", NULL),
  GNOMEUIINFO_ITEM_DATA("is._things", NULL, help_demos_run_cb, "is.things", NULL),
  GNOMEUIINFO_END
};

static GnomeUIInfo help_menu[] =
{
  GNOMEUIINFO_ITEM_NONE("HTML _Index", "Display the help index in a browser window", help_html_index_cb),
  GNOMEUIINFO_ITEM_NONE("_Text Index", "Display the help index in a pager", help_text_index_cb),
  GNOMEUIINFO_SEPARATOR,
  GNOMEUIINFO_ITEM_NONE("_License", "Display the software license", help_license_cb),
  GNOMEUIINFO_ITEM_NONE("_Contributors", "Display the list of contributors", help_contributors_cb),
  GNOMEUIINFO_SUBTREE("_Demos", help_demos_menu),
  GNOMEUIINFO_SEPARATOR,
  GNOMEUIINFO_MENU_ABOUT_ITEM(help_about_cb, NULL),
  GNOMEUIINFO_END
};

static GnomeUIInfo main_menu[] =
{
  GNOMEUIINFO_MENU_FILE_TREE(file_menu),
  GNOMEUIINFO_MENU_EDIT_TREE(edit_menu),
  GNOMEUIINFO_SUBTREE("_Commands", commands_menu),
  GNOMEUIINFO_MENU_SETTINGS_TREE(settings_menu),
  GNOMEUIINFO_MENU_WINDOWS_TREE(windows_menu),
  GNOMEUIINFO_MENU_HELP_TREE(help_menu),
  GNOMEUIINFO_END
};

void R_gtk_terminal_add_menu(GtkWidget *window)
{
  gnome_app_create_menus(GNOME_APP(window), main_menu);
  gnome_app_install_menu_hints(GNOME_APP(window), main_menu);
}

