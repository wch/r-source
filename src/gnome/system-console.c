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

#include "Defn.h"
#include "Fileio.h"

#include "terminal.h"
#include "gtkconsole.h"

#include <gnome.h>

	/*--- I/O Support Code ---*/

	/* These routines provide hooks for supporting console I/O.
	 * Under raw Unix these routines simply provide a
	 * connection to the stdio library.
	 * Under the GNOME interface the routines hook into
	 * the GtkConsole widget.
	 */


/* Catch input in the console window */
void R_gtk_terminal_line_event(GtkWidget *widget)
{
  gtk_main_quit();
}

/* Fill a text buffer with user typed console input. */
int R_ReadConsole(char *prompt, unsigned char *buf, int len, int addtohistory)
{
    if(!R_Interactive) {
	if (!R_Slave)
	    fputs(prompt, stdout);
	if (fgets(buf, len, stdin) == NULL)
	    return 0;
	if (!R_Slave)
	    fputs(buf, stdout);
    }
    else {
      gtk_console_enable_input(GTK_CONSOLE(R_gtk_terminal_text), prompt, strlen(prompt));
      gtk_signal_connect(GTK_OBJECT(R_gtk_terminal_text),
			 "console_line_ready",
			 GTK_SIGNAL_FUNC(R_gtk_terminal_line_event),
			 NULL);

      gtk_main();

      gtk_console_read(GTK_CONSOLE(R_gtk_terminal_text), buf, len, addtohistory);
    }

    return 1;
}

/* Write a text buffer to the console. */
/* All system output is filtered through this routine. */
void R_WriteConsole(char *buf, int len)
{
  gtk_console_write(GTK_CONSOLE(R_gtk_terminal_text), buf, len);
}

/* Indicate that input is coming from the console */
void R_ResetConsole()
{
}

/* Stdio support to ensure the console file buffer is flushed */
void R_FlushConsole()
{
  gtk_console_flush(GTK_CONSOLE(R_gtk_terminal_text));
}


/* Reset stdin if the user types EOF on the console. */
void R_ClearerrConsole()
{
}

