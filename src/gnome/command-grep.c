
#include <gnome.h>
#include <glade/glade.h>

#include "command-grep.h"
#include "terminal.h"
#include "terminal-functions.h"

void commands_grep_cb(GtkWidget *widget, gpointer data)
{
    GladeXML *grep_xml;
    GtkWidget *grep_dialog, *grep_pattern_entry, *grep_object_entry;
    GtkWidget *grep_case_cb, *grep_extended_cb, *grep_value_cb;
    gint button;
    gchar *pattern, *object;

    grep_xml = glade_xml_new(glade_interface_file, "grep_dialog");

    grep_dialog = glade_xml_get_widget(grep_xml, "grep_dialog");
    grep_pattern_entry = glade_xml_get_widget(grep_xml, "grep_pattern_entry");
    grep_object_entry = glade_xml_get_widget(grep_xml, "grep_object_entry");
    grep_case_cb = glade_xml_get_widget(grep_xml, "grep_case_cb");
    grep_extended_cb = glade_xml_get_widget(grep_xml, "grep_extended_cb");
    grep_value_cb = glade_xml_get_widget(grep_xml, "grep_value_cb");

    gtk_object_unref(GTK_OBJECT(grep_xml));

    gnome_dialog_set_default(GNOME_DIALOG(grep_dialog), 0);

    gnome_dialog_editable_enters(GNOME_DIALOG(grep_dialog), GTK_EDITABLE(grep_pattern_entry));
    gnome_dialog_editable_enters(GNOME_DIALOG(grep_dialog), GTK_EDITABLE(grep_object_entry));

    button = gnome_dialog_run(GNOME_DIALOG(grep_dialog));

    switch(button) {
	    case 0:
                    pattern = gtk_editable_get_chars(GTK_EDITABLE(grep_pattern_entry), 0, -1);
		    object = gtk_editable_get_chars(GTK_EDITABLE(grep_object_entry), 0, -1);

		    R_gtk_terminal_run_initial();
		    R_gtk_terminal_run_partial("grep(\"");
		    
		    R_gtk_terminal_run_partial(pattern);
		    
		    R_gtk_terminal_run_partial("\", ");
		    
		    R_gtk_terminal_run_partial(object);
		    
		    R_gtk_terminal_run_partial(", ignore.case=");
		    if(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(grep_case_cb)))
			R_gtk_terminal_run_partial("TRUE");
		    else
			R_gtk_terminal_run_partial("FALSE");
		    
		    R_gtk_terminal_run_partial(", extended=");
		    if(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(grep_extended_cb)))
			R_gtk_terminal_run_partial("TRUE");
		    else
			R_gtk_terminal_run_partial("FALSE");
    		    
		    R_gtk_terminal_run_partial(", value=");
		    if(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(grep_value_cb)))
			R_gtk_terminal_run_partial("TRUE");
		    else
			R_gtk_terminal_run_partial("FALSE");
		    R_gtk_terminal_run_final(")\n");
	    	    
		    g_free(pattern);
		    g_free(object);
		    break;
		    
	    default:
		    break;
    }

    gtk_widget_destroy(GTK_WIDGET(grep_dialog));
}



