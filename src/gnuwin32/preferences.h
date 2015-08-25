/*
 *  R : A Computer Language for Statistical Data Analysis
 *  file preferences.h
 *  Copyright (C) 2000  Guido Masarotto and Brian Ripley
 *                2004-7  R Core Team
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
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 */
 
#include "guicolors.h"

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
    int cursor_blink; /* 0=none, 1=partial, 2=full */
    rgb guiColors[numGuiColors];
    rect MDIsize;
    char language[20];
    int buffered;
    char warning[256];  /* When loading the Rconsole, the warning system isn't set
                           up, so save one warning here. */
};
typedef struct structGUI *Gui;

int loadRconsole(Gui gui, const char *optf);
void getDefaults(Gui gui);
void getActive(Gui gui);
void applyGUI(Gui newGUI);
