#include <gnome.h>

/* primitives
   code must be terminated with a \n for now
   FIXME: add a \n if none exists */
void R_gtk_terminal_run_initial();
void R_gtk_terminal_run_partial(gchar *code);
void R_gtk_terminal_run_final(gchar *code);
void R_gtk_terminal_run(gchar *code);
void R_gtk_terminal_interrupt();

/* higher level functions */
void R_gtk_terminal_quit();

/* menu/toolbar functions */
void R_gtk_terminal_file_save(GtkWidget *widget, 
			      gpointer data);
void R_gtk_terminal_file_saveas(GtkWidget *widget,
				gpointer data);
void R_gtk_terminal_file_open(GtkWidget *widget,
			      gpointer data);
