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

