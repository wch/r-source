#include <limits.h>
#include <stdio.h>

#include "Defn.h"
#include "Graphics.h"
#include "Print.h"
#include "Fileio.h"
#include "IOStuff.h"
#include "Parse.h"

#include "terminal.h"
#include "terminal-menu.h"
#include "terminal-functions.h"
#include "terminal-prefs.h"

/* Some menu callbacks are here, others are in terminal-functions.c */




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
}

static void edit_copy_paste_cb(GtkWidget *widget, gpointer data)
{
  gtk_editable_copy_clipboard(GTK_EDITABLE(R_gtk_terminal_text));
  gtk_editable_paste_clipboard(GTK_EDITABLE(R_gtk_terminal_text));
}

static void edit_clear_cb(GtkWidget *widget, gpointer data)
{
  gtk_editable_delete_selection(GTK_EDITABLE(R_gtk_terminal_text));
}

static void edit_find_cb(GtkWidget *widget, gpointer data)
{
}

static void edit_find_again_cb(GtkWidget *widget, gpointer data)
{
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



static void prefs_apply_cb(GtkWidget *widget, int page, gpointer data)
{
  GtkStyle *textstyle;

  /* page = -1 means apply all pages */
  /*  if(page != -1)
      return;*/

  /* font */
  if(g_strcasecmp(R_gnome_userprefs.font, R_gnome_newprefs.font) != 0) {
    textstyle = gtk_style_copy(gtk_widget_get_style(R_gtk_terminal_text));
    textstyle->font = gdk_font_load(R_gnome_userprefs.font);
    gtk_widget_set_style(R_gtk_terminal_text, textstyle);
  }

  /* update prefs */
  R_gnome_userprefs = R_gnome_newprefs;

  R_gnome_save_prefs();
}

static void settings_prefs_cb(GtkWidget *widget, gpointer data)
{
  /* notebook pages */
  GtkWidget *page0, *page1, *page2, *page3, *page4, *page5;
  GtkWidget *label0, *label1, *label2, *label3, *label4, *label5;

  /* copy current prefs */
  R_gnome_newprefs = R_gnome_userprefs;

  /* Page 0: text font and colour options */
  page0 = prefs_text_page();
  label0 = gtk_label_new("Console");

  /* Page 1: actions on exit */
  page1 = prefs_exit_page();
  label1 = gtk_label_new("Exit actions");

  /* Page 2: external applications */
  page2 = prefs_apps_page();
  label2 = gtk_label_new("Applications");

  /* Page 3: graphics options */
  page3 = prefs_graphics_page();
  label3 = gtk_label_new("Graphics");

  /* Page 4: environment options */
  page4 = prefs_env_page();
  label4 = gtk_label_new("Environment");

  /* Create the dialog box */
  prefs_dialog = gnome_property_box_new();
  gtk_window_set_title(GTK_WINDOW(prefs_dialog), "R Preferences");

  /* Append the pages to the notebook */
  gnome_property_box_append_page(GNOME_PROPERTY_BOX(prefs_dialog),
				 page0, label0);
  gnome_property_box_append_page(GNOME_PROPERTY_BOX(prefs_dialog),
				 page1, label1);
  gnome_property_box_append_page(GNOME_PROPERTY_BOX(prefs_dialog),
				 page2, label2);
  gnome_property_box_append_page(GNOME_PROPERTY_BOX(prefs_dialog),
				 page3, label3);
  gnome_property_box_append_page(GNOME_PROPERTY_BOX(prefs_dialog),
				 page4, label4);

  /* Connect to property box signal */
  gtk_signal_connect(GTK_OBJECT(prefs_dialog), "apply",
		     GTK_SIGNAL_FUNC(prefs_apply_cb),
		     NULL);

  /* Setup dialog features */
  gnome_dialog_set_parent(GNOME_DIALOG(prefs_dialog),
			  GTK_WINDOW(R_gtk_main_window));
  gtk_window_set_modal(GTK_WINDOW(prefs_dialog), TRUE);
  
  /* Display the dialog */
  gtk_widget_show_all(prefs_dialog);
}



static void help_index_cb(GtkWidget *widget, gpointer data)
{
  R_gtk_terminal_run("help.start()\n");
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
  GtkWidget *about_box;
  gchar *version;
  gchar *copyright;

  gchar *authors[] = {
    "Ross Ihaka <ihaka@stat.auckland.ac.nz>",
    "Robert Gentleman",
    "The R Core Team (see ?contributors)",
    "Lyndon Drake <lyndon@stat.auckland.ac.nz>",
    NULL
  };

  version = g_strdup_printf("%s.%s %s (%s %s, %s)", R_MAJOR, R_MINOR, R_STATUS, R_MONTH, R_DAY, R_YEAR);
  copyright = g_strdup_printf("Copyright (C) %s R Core Team", R_YEAR);
  
  g_assert(version != NULL);
  g_assert(copyright != NULL);

  about_box = gnome_about_new("R", version, copyright, authors,
			      "R is a system for statistical computation and graphics.  It is a dialect of the S programming language from Bell Labs.  R is free software and comes with ABSOLUTELY NO WARRANTY.",
			      "R-logo-sm.xpm");

  gnome_dialog_set_close(GNOME_DIALOG(about_box), TRUE);
  gnome_dialog_set_parent(GNOME_DIALOG(about_box), GTK_WINDOW(R_gtk_main_window));

  gtk_widget_show(about_box);

  g_free(version);
  g_free(copyright);
}



static void generic_cb(GtkWidget *widget, gpointer data)
{
  g_message("Menu item selected");
}

static GnomeUIInfo file_ws_menu[] =
{
  { GNOME_APP_UI_ITEM, "_Open...", "Open a saved workspace image", R_gtk_terminal_file_open, NULL, NULL, GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_OPEN, GNOME_KEY_NAME_OPEN, GNOME_KEY_MOD_NEW, NULL },
  { GNOME_APP_UI_ITEM, "Save _As...", "Save the workspace image to a file", R_gtk_terminal_file_saveas, NULL, NULL, GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_SAVE_AS, GNOME_KEY_NAME_SAVE, GNOME_KEY_MOD_SAVE, NULL },
  GNOMEUIINFO_END
};

static GnomeUIInfo file_data_menu[] =
{
  GNOMEUIINFO_ITEM_NONE("Source...", "Load a file containing R source", generic_cb),
  GNOMEUIINFO_SEPARATOR,
  GNOMEUIINFO_ITEM_NONE("Edit Code...", "Edit R source in an editor", generic_cb),
  GNOMEUIINFO_ITEM_NONE("Edit Variable...", "Edit a variable in a spreadsheet", generic_cb),
  GNOMEUIINFO_ITEM_NONE("Load Files", "Load the edit files", generic_cb),
  GNOMEUIINFO_END
};

static GnomeUIInfo file_menu[] =
{
  GNOMEUIINFO_SUBTREE("_Workspace", file_ws_menu),
  GNOMEUIINFO_SUBTREE("_Data", file_data_menu),
  GNOMEUIINFO_SEPARATOR,
  { GNOME_APP_UI_ITEM, "_Print...", "Print the console output", generic_cb, NULL, NULL, GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_PRINT, NULL, (GdkModifierType)0, NULL },
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

static GnomeUIInfo commands_menu[] =
{
  { GNOME_APP_UI_ITEM, "_Interrupt", "Interrupt R processing (SIGTERM)", commands_interrupt_cb, NULL, NULL, GNOME_APP_PIXMAP_NONE, NULL, GDK_Escape, (GdkModifierType)0, NULL },
  GNOMEUIINFO_SEPARATOR,
  GNOMEUIINFO_END
};

static GnomeUIInfo graphics_menu[] =
{
  GNOMEUIINFO_ITEM_NONE("_New Window", "Create a new graphics window", graphics_new_cb),
  GNOMEUIINFO_ITEM_NONE("_Close Active Device", "Close the active graphics device", graphics_close_cb),
  GNOMEUIINFO_ITEM_NONE("Close _All Devices", "Close all graphics devices", graphics_closeall_cb),
  GNOMEUIINFO_SEPARATOR,
  GNOMEUIINFO_ITEM_NONE("List of graphics windows", NULL, NULL),
  GNOMEUIINFO_END
};

static GnomeUIInfo settings_menu[] =
{
  GNOMEUIINFO_MENU_PREFERENCES_ITEM(settings_prefs_cb, NULL),
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
  GNOMEUIINFO_ITEM_NONE("HTML _Index", "Display the help index in a browser window", help_index_cb),
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
  GNOMEUIINFO_SUBTREE("_Graphics", graphics_menu),
  GNOMEUIINFO_MENU_SETTINGS_TREE(settings_menu),
  GNOMEUIINFO_MENU_HELP_TREE(help_menu),
  GNOMEUIINFO_END
};

void R_gtk_terminal_add_menu(GtkWidget *window)
{
  gnome_app_create_menus(GNOME_APP(window), main_menu);
  gnome_app_install_menu_hints(GNOME_APP(window), main_menu);
}

