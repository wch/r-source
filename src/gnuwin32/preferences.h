/*
 *  R : A Computer Language for Statistical Data Analysis
 *  file preferences.h
 *  Copyright (C) 2000  Guido Masarotto and Brian Ripley
 *                2004-6  R Core Development Team
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
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA
 */
 
/*                configuration editor                        */

/* current state */

struct structGUI
{
    int MDI;
    int toolbar;
    int statusbar;
    int pagerMultiple;
    char font[50];
    int tt_font;
    int pointsize;
    char style[20];
    int crows, ccols, cx, cy, setWidthOnResize, prows, pcols,
	cbb, cbl, grx, gry;
    rgb bg, fg, user, hlt;
    rect MDIsize;
    char language[20];
    int buffered;
};
typedef struct structGUI *Gui;

int loadRconsole(Gui gui, char *optf);
void getActive(Gui gui);
void applyGUI(Gui newGUI);
