/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1999  Guido Masarotto
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
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

/*
 *  int addstatusbar()  - add a simple status bar to the mdi frame
 *  void setstatus(char *text) - set text
 */

/*
   This file is an add-on  to GraphApp, a cross-platform C graphics library.
 */

#include "internal.h"

static char MDIStatusText[256] = "" ;

int addstatusbar() {
  int a[1]={-1};
  if (!MDIFrame) return 0;
  if (MDIStatus) return 1;
  InitCommonControls();
  MDIStatus = CreateStatusWindow(WS_CHILD|SBARS_SIZEGRIP|WS_VISIBLE,
	"",hwndFrame,121);
  if (!MDIStatus) return 0;
  SendMessage(MDIStatus,SB_SETPARTS,(WPARAM)1,(LPARAM)a);
  return 1;
}



PROTECTED void updatestatus(char *text) {
  if (!MDIStatus) return;
  strncpy(MDIStatusText,text,255);
  SendMessage(MDIStatus,SB_SETTEXT,
            (WPARAM) 0|0,(LPARAM)MDIStatusText);
  SendMessage(MDIStatus,WM_PAINT,(WPARAM)0,(LPARAM)0);
}

void setstatus(char *text) {
  if (!MDIStatus || !current_window) return;
  strncpy(current_window->status,text,255);
  updatestatus(text);
}













