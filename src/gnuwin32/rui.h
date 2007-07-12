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
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA
 */

#define RW_MDI         0x0001
#define RW_TOOLBAR     0x0010
#define RW_STATUSBAR   0x0100
#define RW_LARGEICONS   0x1000

extern int MDIset;
extern int R_LoadRconsole;

#include <R_ext/Boolean.h>
#include <R_ext/libextern.h>
LibExtern int RguiMDI;
LibExtern window RConsole;
LibExtern window RFrame;
LibExtern int Rwin_graphicsx, Rwin_graphicsy;
LibExtern Rboolean AllDevicesKilled;
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

struct structHelpMenuItems {
  menuitem mhelp, mmanintro, mmanref, mmandata,
    mmanext, mmanint, mmanlang, mmanadmin, mman0, mapropos, mhelpstart, 
    mhelpsearch, msearchRsite, mFAQ, mrwFAQ, mCRAN;
  menu mman;
};
typedef struct structHelpMenuItems *HelpMenuItems;

struct structPkgMenuItems {
  menuitem mpkgl, mpkgm, mpkgi, mpkgil, mpkgu,
    mrepos;
};
typedef struct structPkgMenuItems *PkgMenuItems;

#include <R_ext/Error.h> /* for R_ShowMessage */
int check_doc_file(const char *);
void internal_shellexec(const char *);

int winaddmenu(const char * name, char *errmsg);
int winaddmenuitem(const char * item, const char * menu, const char * action, char *errmsg);
int windelmenu(const char * menu, char *errmsg); /* delete one menu and its items and submenus */
void windelmenus(const char * prefix); /* delete all menus which start with a certain prefix */
int windelmenuitem(const char * item,const  char * menu, char *errmsg);

int numwinmenus(void);
char *getusermenuname(int pos);
menuItems *wingetmenuitems(const char *mname, char *errmsg);
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
void pkgmenuact(PkgMenuItems pmenu);
int RguiCommonHelp();
void helpmenuact(HelpMenuItems hmenu);
void closeconsole(control m);
void showstatusbar();

menu getGraphMenu(const char *); /* from extra.c */
