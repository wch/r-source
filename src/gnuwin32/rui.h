/*
 *  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (C) 1998, 1999  Guido Masarotto and Brian Ripley
 *  Copyright (C) 2004        The R Foundation
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

#define RW_MDI         0x0001
#define RW_TOOLBAR     0x0010
#define RW_STATUSBAR   0x0100
#define RW_LARGEICONS   0x1000

extern int RguiMDI;
extern int MDIset;

#include <R_ext/libextern.h>
LibExtern window RConsole;
#undef LibExtern

typedef struct {
    menuitem m;
    char *name;
    char *action;
}  uitem;
typedef uitem *Uitem;

typedef struct {
  int numItems;
  Uitem *mItems;
} menuItems;

#include <R_ext/Error.h> /* for R_ShowMessage */
int check_doc_file(char *);
void internal_shellexec(char *);
int internal_ShowFile(char *, char *);

int winaddmenu(char * name, char *errmsg);
int winaddmenuitem(char * item, char * menu, char * action, char *errmsg);
int windelmenu(char * menu, char *errmsg); /* delete one menu and its items and submenus */
void windelmenus(char * prefix); /* delete all menus which start with a certain prefix */
int windelmenuitem(char * item, char * menu, char *errmsg);

int numwinmenus(void);
char *getusermenuname(int pos);
menuItems *wingetmenuitems(char *mname, char *errmsg);
void freemenuitems(menuItems *items);

void Rwin_fpset();

void Rgui_configure();
void readconsolecfg();
void breaktodebugger();

#define USE_MDI 1

#ifdef USE_MDI
int RgetMDIwidth();
int RgetMDIheight();
#endif

void menuconfig();
void menuclear(control m);
int RguiPackageMenu();
int RguiCommonHelp();
void closeconsole(control m);
