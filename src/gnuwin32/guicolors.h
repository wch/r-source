/*
 *  R : A Computer Language for Statistical Data Analysis
 *  file consolestructs.h
 *  Copyright (C) 2008      The R Foundation
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
 *  http://www.r-project.org/Licenses/
 */


#ifndef _GUICOLORS_H_
#define	_GUICOLORS_H_

typedef enum {
  consolebg, consolefg, consoleuser, 
  pagerbg, pagerfg, pagerhighlight,
  dataeditbg, dataeditfg, dataedituser,
  editorbg, editorfg,
  numGuiColors
} GuiColorIndex;

extern rgb guiColors[numGuiColors];

#endif
