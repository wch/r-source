/*
 *  R : A Computer Language for Statistical Data Analysis
 *  file editor.h
 *  Copyright (C) 1999-2008  The R Core Team
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

void menueditornew(control m);
void menueditoropen(control m);
int editorchecksave(editor c);
void editorsetfont(font f);
int Rgui_Edit(const char *filename, int enc, const char *title, int modal);

#define EDITORMAXTITLE 128
#define MAXNEDITORS 50

struct structEditorData {
    Rboolean file; /* is the editor associated with an existing file */
    char *filename; /* corresponding file, in UTF-8 as from 2.9.0 */
    char *title;    /* window title */
    Rboolean stealconsole;  /* set when using fix() or edit(), so that no events are sent to console until this editor is closed */
    menuitem mcut, mcopy, mdelete, mfind, mreplace,
	mpopcut, mpopcopy, mpopdelete;
    HelpMenuItems hmenu;
    PkgMenuItems pmenu;
};
typedef struct structEditorData *EditorData;
