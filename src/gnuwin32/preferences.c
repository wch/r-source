/*
 *  R : A Computer Language for Statistical Data Analysis
 *  file preferences.c
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
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "win-nls.h"

#ifdef Win32
#define USE_MDI 1
#endif

#define WIN32_LEAN_AND_MEAN 1
#include <windows.h>
#include <ctype.h>  /* isspace */
#include "graphapp/ga.h"
#include "graphapp/graphapp.h"
#include "opt.h"
#include "console.h"
#include "consolestructs.h"
#include "rui.h"
#include "preferences.h"
#include <Fileio.h>


#define gettext GA_gettext

extern char fontname[LF_FACESIZE+1]; /* from console.c */
extern int consolex, consoley; /* from console.c */
extern int pagerMultiple, haveusedapager; /* from pager.c */
void editorsetfont(font f);

static void showDialog(Gui gui);

extern char *ColorName[]; /* from graphapp/rgb.c */

static int cmatch(char *col, char **list)
{
    int i=0;
    char **pos = list;
    while(*pos != NULL) {
	if(strcmpi(*pos, col) == 0) return(i);
	i++; pos++;
    }
    return(-1);
}


static char *StyleList[] = {"normal", "bold", "italic", NULL};
static char *PointsList[] = {"6", "7", "8", "9", "10", "11", "12", "14", "16", "18", "20", "22", "24", "26", "28", "32", "36", NULL};
static char *FontsList[] = {"Courier", "Courier New", "FixedSys", "FixedFont", "Lucida Console", "Terminal", "BatangChe", "DotumChe", "GulimChe", "MingLiU", "MS Gothic", "MS Mincho", "NSimSun", NULL};


static window wconfig;
static button bApply, bSave, bLoad, bOK, bCancel;
static label l_mdi, l_mwin, l_font, l_point, l_style, l_lang, l_crows, l_ccols,
    l_cx, l_cy, l_prows, l_pcols, l_grx, l_gry,
    l_cols, l_bgcol, l_fgcol, l_usercol, l_highlightcol, l_cbb, l_cbl;
static radiogroup g_mwin;
static radiobutton rb_mdi, rb_sdi, rb_mwin, rb_swin;
static listbox f_font, f_style, d_point, bgcol, fgcol, usercol, highlightcol;
static checkbox toolbar, statusbar, tt_font, c_resize, c_buff;
static field f_crows, f_ccols, f_prows, f_pcols, f_cx, f_cy, f_cbb,f_cbl,
    f_grx, f_gry, f_lang;


static void getChoices(Gui p)
{
    p->MDI = ischecked(rb_mdi);
    p->toolbar = ischecked(toolbar);
    p->statusbar = ischecked(statusbar);
    p->pagerMultiple = ischecked(rb_mwin);
    strcpy(p->language, gettext(f_lang));
    strcpy(p->font, gettext(f_font));
    p->tt_font = ischecked(tt_font);
    p->pointsize = atoi(gettext(d_point));
    strcpy(p->style, gettext(f_style));
    p->crows = atoi(gettext(f_crows));
    p->ccols = atoi(gettext(f_ccols));
    p->cx = atoi(gettext(f_cx));
    p->cy = atoi(gettext(f_cy));
    p->setWidthOnResize = ischecked(c_resize);
    p->buffered = ischecked(c_buff);
    p->cbb = atoi(gettext(f_cbb));
    p->cbl = atoi(gettext(f_cbl));
    p->prows = atoi(gettext(f_prows));
    p->pcols = atoi(gettext(f_pcols));
    p->grx = atoi(gettext(f_grx));
    p->gry = atoi(gettext(f_gry));
    p->bg = nametorgb(gettext(bgcol));
    p->fg = nametorgb(gettext(fgcol));
    p->user = nametorgb(gettext(usercol));
    p->hlt = nametorgb(gettext(highlightcol));
    /* MDIsize is not currently a choice in the dialog, only in the Rconsole file */
}

void getActive(Gui gui)
{
    rect r;
    ConsoleData p = (ConsoleData) getdata(RConsole);

    gui->toolbar = ((RguiMDI & RW_TOOLBAR) != 0);
    gui->statusbar = ((RguiMDI & RW_STATUSBAR) != 0);
    gui->MDI = ((RguiMDI & RW_MDI) != 0);
    gui->pagerMultiple = pagerMultiple;
    {
	char *p = getenv("LANGUAGE");
	strcpy(gui->language, p ? p : "");
    }

/* Font, pointsize, style */

    gui->tt_font = FALSE;
    {
	char *pf;
	if ((strlen(fontname) > 1) &&
	    (fontname[0] == 'T') && (fontname[1] == 'T')) {
	    gui->tt_font = TRUE;
	    for (pf = fontname+2; isspace(*pf) ; pf++);
	} else pf = fontname;
	strcpy(gui->font, pf);
    }

    gui->pointsize = pointsize;
    
    if (fontsty & Italic) strcpy(gui->style, "italic");
    else if (fontsty & Bold) strcpy(gui->style, "Bold");
    else strcpy(gui->style, "normal");

/* Console size, set widthonresize */
    gui->crows = ROWS;
    gui->ccols = COLS;
    r = GetCurrentWinPos(RConsole);
    gui->cx = r.x;
    gui->cy = r.y;
    gui->setWidthOnResize = setWidthOnResize;
    gui->cbb = p->lbuf->dim;
    gui->cbl = p->lbuf->ms;
    gui->buffered = consolebuffered;

/* Pager size */
    gui->prows = pagerrow;
    gui->pcols = pagercol;

/* Graphics window */
    gui->grx = Rwin_graphicsx;
    gui->gry = Rwin_graphicsy;

/* Font colours */
    gui->bg = consolebg;
    gui->fg = consolefg;
    gui->user = consoleuser;
    gui->hlt = pagerhighlight;
    
/* MDIsize is not currently a choice in the dialog, only in the Rconsole file, so is not set here */
    
}

static int has_changed(Gui a, Gui b)
{
    return !a || 
        a->MDI != b->MDI ||
	a->toolbar != b->toolbar ||
	a->statusbar != b->statusbar ||
	a->pagerMultiple != b->pagerMultiple ||
	strcmp(a->language, b->language) ||
	strcmp(a->font, b->font) ||
	a->tt_font != b->tt_font ||
	a->pointsize != b->pointsize ||
	strcmp(a->style, b->style) ||
	a->crows != b->crows ||
	a->ccols != b->ccols ||
	a->cx != b->cx ||
	a->cy != b->cy ||
	a->cbb != b->cbb ||
	a->cbl != b->cbl ||
	a->setWidthOnResize != b->setWidthOnResize ||
	a->prows != b->prows ||
	a->pcols != b->pcols ||
	a->grx != b->grx ||
	a->gry != b->gry ||
	a->bg != b->bg ||
	a->fg != b->fg ||
	a->user != b->user ||
	a->hlt != b->hlt;
}


static void cleanup()
{
    hide(wconfig);
    delobj(l_mdi); delobj(rb_mdi); delobj(rb_sdi);
    delobj(toolbar); delobj(statusbar);
    delobj(l_mwin); delobj(g_mwin); delobj(rb_mwin); delobj(rb_swin);
    delobj(l_lang); delobj(f_lang);
    delobj(l_font); delobj(f_font); delobj(tt_font);
    delobj(l_point); delobj(d_point);
    delobj(l_style); delobj(f_style);
    delobj(l_crows); delobj(f_crows); delobj(l_ccols); delobj(f_ccols);
    delobj(c_resize); delobj(c_buff);
    delobj(l_cx); delobj(f_cx); delobj(l_cy); delobj(f_cy);
    delobj(l_cbb); delobj(f_cbb); delobj(l_cbl); delobj(f_cbl);
    delobj(l_prows); delobj(f_prows); delobj(l_pcols); delobj(f_pcols);
    delobj(l_grx); delobj(f_grx); delobj(l_gry); delobj(f_gry);
    delobj(l_cols);
    delobj(l_bgcol); delobj(bgcol);
    delobj(l_fgcol); delobj(fgcol);
    delobj(l_usercol); delobj(usercol);
    delobj(l_highlightcol); delobj(highlightcol);
    delobj(bApply); delobj(bSave); delobj(bOK); delobj(bCancel);
    delobj(wconfig);
}

void applyGUI(Gui newGUI)
{
    rect r = getrect(RConsole);
    ConsoleData p = (ConsoleData) getdata(RConsole);
    int havenewfont = 0;
    struct structGUI curGUI;

    getActive(&curGUI);    
    
    if(!has_changed(&curGUI, newGUI)) return;

    if(newGUI->MDI != curGUI.MDI || newGUI->toolbar != curGUI.toolbar ||
       newGUI->statusbar != curGUI.statusbar)
	askok(G_("The overall console properties cannot be changed\non a running console.\n\nSave the preferences and restart Rgui to apply them.\n"));

    if(strcmp(newGUI->language, curGUI.language)) {
	char *buf = malloc(50);
	askok(G_("The language for menus cannot be changed on a\n running console.\n\nSave the preferences and restart Rgui to apply to menus.\n"));
	sprintf(buf, "LANGUAGE=%s", newGUI->language);
	putenv(buf);	
    }
    

/*  Set a new font? */
    if(strcmp(newGUI->font, curGUI.font) ||
       newGUI->pointsize != curGUI.pointsize ||
       strcmp(newGUI->style, curGUI.style))
    {
	char msg[LF_FACESIZE + 128];
	int sty = Plain;

	if(newGUI->tt_font) strcpy(fontname, "TT "); else strcpy(fontname, "");
	strcat(fontname,  newGUI->font);
	if (!strcmp(newGUI->style, "bold")) sty = Bold;
	if (!strcmp(newGUI->style, "italic")) sty = Italic;
	pointsize = newGUI->pointsize;
	fontsty = sty;

	/* Don't delete font: open pagers may be using it */
	if (strcmp(fontname, "FixedFont"))
	    consolefn = gnewfont(NULL, fontname, fontsty, pointsize, 0.0);
	else consolefn = FixedFont;
	if (!consolefn) {
	    sprintf(msg,
		    G_("Font %s-%d-%d  not found.\nUsing system fixed font"),
		    fontname, fontsty | FixedWidth, pointsize);
	    R_ShowMessage(msg);
	    consolefn = FixedFont;
	}
	/* if (!ghasfixedwidth(consolefn)) {
	    sprintf(msg,
		    G_("Font %s-%d-%d has variable width.\nUsing system fixed font"),
		    fontname, fontsty, pointsize);
	    R_ShowMessage(msg);
	    consolefn = FixedFont;
	    } */
	p->f = consolefn;
	FH = fontheight(p->f);
	FW = fontwidth(p->f);
	havenewfont = 1;
	editorsetfont(consolefn);
    }

/* resize console, possibly with new font */
    if (consoler != newGUI->crows || consolec != newGUI->ccols || havenewfont) {
	char buf[20];
	consoler = newGUI->crows;
	consolec = newGUI->ccols;
	r.width = (consolec + 1) * FW;
	r.height = (consoler + 1) * FH;
	resize(RConsole, r);
	sprintf(buf, "%d", ROWS); settext(f_crows, buf);
	sprintf(buf, "%d", COLS); settext(f_ccols, buf);
    }
    if (p->lbuf->dim != newGUI->cbb || p->lbuf->ms != newGUI->cbl)
	xbufgrow(p->lbuf, newGUI->cbb, newGUI->cbl);

/* Set colours and redraw */
    p->fg = consolefg = newGUI->fg;
    p->ufg = consoleuser = newGUI->user;
    p->bg = consolebg = newGUI->bg;
    drawconsole(RConsole, r);
    pagerhighlight = newGUI->hlt;

    if(haveusedapager && 
       (newGUI->prows != curGUI.prows || newGUI->pcols != curGUI.pcols))
	askok(G_("Changes in pager size will not apply to any open pagers"));
    pagerrow = newGUI->prows;
    pagercol = newGUI->pcols;

    if(newGUI->pagerMultiple != pagerMultiple) {
	if(!haveusedapager ||
	   askokcancel(G_("Do not change pager type if any pager is open\nProceed?"))
	   == YES)
	    pagerMultiple = newGUI->pagerMultiple;
	if(pagerMultiple) {
	    check(rb_mwin); uncheck(rb_swin);
	} else {check(rb_swin); uncheck(rb_mwin);}
    }

    setWidthOnResize = newGUI->setWidthOnResize;
    consolebuffered = newGUI->buffered;
}

static void do_apply()
{
    struct structGUI newGUI;

    getChoices(&newGUI);
    applyGUI(&newGUI);
}

static void save(button b)
{
    char *file, buf[256], *p;
    FILE *fp;

    setuserfilter("All files (*.*)\0*.*\0\0");
    strcpy(buf, getenv("R_USER"));
    file = askfilesavewithdir(G_("Select directory for file 'Rconsole'"),
			      "Rconsole", buf);
    if(!file) return;
    strcpy(buf, file);
    p = buf + strlen(buf) - 2;
    if(!strncmp(p, ".*", 2)) *p = '\0';

    fp = R_fopen(buf, "w");
    if(fp == NULL) {
	MessageBox(0, "Cannot open file to fp",
		   "Configuration Save Error",
		   MB_TASKMODAL | MB_ICONSTOP | MB_OK);
	return;
    }

    fprintf(fp, "%s\n%s\n%s\n\n%s\n%s\n",
	    "# Optional parameters for the console and the pager",
	    "# The system-wide copy is in rwxxxx/etc.",
	    "# A user copy can be installed in `R_USER'.",
	    "## Style",
	    "# This can be `yes' (for MDI) or `no' (for SDI).");
    fprintf(fp, "MDI = %s\n",  ischecked(rb_mdi)?"yes":"no");
    fprintf(fp, "%s\n%s%s\n%s%s\n\n",
	    "# the next two are only relevant for MDI",
	    "toolbar = ", ischecked(toolbar)?"yes":"no",
	    "statusbar = ", ischecked(statusbar)?"yes":"no");

    fprintf(fp, "%s\n%s\n%s\n%s\n%s\n",
	    "## Font.",
	    "# Please use only fixed width font.",
	    "# If font=FixedFont the system fixed font is used; in this case",
	    "# points and style are ignored. If font begins with \"TT \", only",
	    "# True Type fonts are searched for.");
    fprintf(fp, "font = %s%s\npoints = %s\nstyle = %s # Style can be normal, bold, italic\n\n\n",
	    ischecked(tt_font)?"TT ":"",
	    gettext(f_font),
	    gettext(d_point),
	    gettext(f_style));
    fprintf(fp, "# Dimensions (in characters) of the console.\n");
    fprintf(fp, "rows = %s\ncolumns = %s\n",
	    gettext(f_crows), gettext(f_ccols));
    fprintf(fp, "# Dimensions (in characters) of the internal pager.\n");
    fprintf(fp, "pgrows = %s\npgcolumns = %s\n",
	    gettext(f_prows), gettext(f_pcols));
    fprintf(fp, "# should options(width=) be set to the console width?\n");
    fprintf(fp, "setwidthonresize = %s\n\n",
	    ischecked(c_resize) ? "yes" : "no");
    fprintf(fp, "# memory limits for the console scrolling buffer, in bytes and lines\n");
    fprintf(fp, "bufbytes = %s\nbuflines = %s\n\n",
	    gettext(f_cbb), gettext(f_cbl));
    fprintf(fp, "# Initial position of the console (pixels, relative to the workspace for MDI)\n");
    fprintf(fp, "xconsole = %s\nyconsole = %s\n\n",
	    gettext(f_cx), gettext(f_cy));
    fprintf(fp, "%s\n%s\n%s\n%s\n%s\n%s\n\n",
	    "# Dimension of MDI frame in pixels",
	    "# Format (w*h+xorg+yorg) or use -ve w and h for offsets from right bottom",
	    "# This will come up maximized if w==0",
	    "# MDIsize = 0*0+0+0",
	    "# MDIsize = 1000*800+100+0",
	    "# MDIsize = -50*-50+50+50  # 50 pixels space all round");
    fprintf(fp, "%s\n%s\n%s\npagerstyle = %s\n\n\n",
	    "# The internal pager can displays help in a single window",
	    "# or in multiple windows (one for each topic)",
	    "# pagerstyle can be set to `singlewindow' or `multiplewindows'",
	    ischecked(rb_mwin) ? "multiplewindows" : "singlewindow");

    fprintf(fp, "## Colours for console and pager(s)\n# (see rwxxxx/etc/rgb.txt for the known colours).\n");
    fprintf(fp, "background = %s\n", gettext(bgcol));
    fprintf(fp, "normaltext = %s\n", gettext(fgcol));
    fprintf(fp, "usertext = %s\n", gettext(usercol));
    fprintf(fp, "highlight = %s\n", gettext(highlightcol));
    fprintf(fp, "\n\n%s\n%s\nxgraphics = %s\nygraphics = %s\n",
	    "## Initial position of the graphics window",
	    "## (pixels, <0 values from opposite edge)",
	    gettext(f_grx), gettext(f_gry));
    fprintf(fp, "\n\n%s\nlanguage = %s\n",
	    "## Language for messages",
	    gettext(f_lang));
    fprintf(fp, "\n\n## Default setting for console buffering: 'yes' or 'no'\n");
    fprintf(fp, "buffered = %s\n",
	    ischecked(c_buff) ? "yes" : "no");
    fclose(fp);
}

static void load(button b) /* button callback */
{
    char *optf, buf[256];
    struct structGUI newGUI;    

    setuserfilter("All files (*.*)\0*.*\0\0");
    strcpy(buf, getenv("R_USER"));    
    optf = askfilenamewithdir(G_("Select 'Rconsole' file"), "Rconsole", buf);
    if(!optf) return;
    
    getChoices(&newGUI);
    if (loadRconsole(&newGUI, optf)) {
	cleanup();
	showDialog(&newGUI);
    }
}

int loadRconsole(Gui gui, char *optf)
{
    int ok, done, cfgerr;
    char *opt[2];    
    
    if (!optopenfile(optf)) {
	return 0; 	/* this should not happen unless there are permission problems, etc */
    }
    cfgerr = 0;
    while ((ok = optread(opt, '='))) {
	done = 0;
	if (ok == 2) {
	    if (!strcmp(opt[0], "font")) {
		if(strlen(opt[1]) > 127) opt[1][127] = '\0';
		gui->tt_font = FALSE;
		{
		    char *pf;
		    if ((strlen(opt[1]) > 1) &&
			(opt[1][0] == 'T') && (opt[1][1] == 'T')) {
			gui->tt_font = TRUE;
			for (pf = opt[1]+2; isspace(*pf) ; pf++);
		    } else pf = opt[1];
		    strcpy(gui->font, pf);
		}
		done = 1;
	    }
	    if (!strcmp(opt[0], "points")) {
		gui->pointsize = atoi(opt[1]);
		done = 1;
	    }
	    if (!strcmp(opt[0], "style")) {
		strcpy(gui->style, opt[1]); 
 		done = 1;
	    }
	    if (!strcmp(opt[0], "rows")) {
		gui->crows = atoi(opt[1]);
		done = 1;
	    }
	    if (!strcmp(opt[0], "columns")) {
		gui->ccols = atoi(opt[1]);
		done = 1;
	    }
	    if (!strcmp(opt[0], "xconsole")) {
		gui->cx = atoi(opt[1]);
		done = 1;
	    }
	    if (!strcmp(opt[0], "yconsole")) {
		gui->cy = atoi(opt[1]);
		done = 1;
	    }
	    if (!strcmp(opt[0], "xgraphics")) {
		gui->grx = atoi(opt[1]);
		done = 1;
	    }
	    if (!strcmp(opt[0], "ygraphics")) {
		gui->gry = atoi(opt[1]);
		done = 1;
	    }
	    if (!strcmp(opt[0], "pgrows")) {
		gui->prows = atoi(opt[1]);
		done = 1;
	    }
	    if (!strcmp(opt[0], "pgcolumns")) {
		gui->pcols = atoi(opt[1]);
		done = 1;
	    }
	    if (!strcmp(opt[0], "pagerstyle")) {
		if (!strcmp(opt[1], "singlewindow"))
		    gui->pagerMultiple = 0;
		else
		    gui->pagerMultiple = 1;
		done = 1;
	    }
	    if (!strcmp(opt[0], "bufbytes")) {
		gui->cbb = atoi(opt[1]);
		done = 1;
	    }
	    if (!strcmp(opt[0], "buflines")) {
		gui->cbl = atoi(opt[1]);
		done = 1;
	    }
#ifdef USE_MDI
	    if (!strcmp(opt[0], "MDI")) {
		if (!strcmp(opt[1], "yes"))
		    gui->MDI = 1;
		else if (!strcmp(opt[1], "no"))
		    gui->MDI = 0;
		done = 1;
	    }
	    if (!strcmp(opt[0], "toolbar")) {
		if (!strcmp(opt[1], "yes"))
		    gui->toolbar = 1;
		else if (!strcmp(opt[1], "no"))
		    gui->toolbar = 0;
		done = 1;
	    }
	    if (!strcmp(opt[0], "statusbar")) {
		if (!strcmp(opt[1], "yes"))
		    gui->statusbar = 1;
		else if (!strcmp(opt[1], "no"))
		    gui->statusbar = 0;
		done = 1;
	    }
	    if (!strcmp(opt[0], "MDIsize")) { /* wxh+x+y */
		int x=0, y=0, w=0, h=0, sign;
		char *p = opt[1];

		if(*p == '-') {sign = -1; p++;} else sign = +1;
		for(w=0; isdigit(*p); p++) w = 10*w + (*p - '0');
		w *= sign;
		p++;

		if(*p == '-') {sign = -1; p++;} else sign = +1;
		for(h=0; isdigit(*p); p++) h = 10*h + (*p - '0');
		h *= sign;

		if(*p == '-') sign = -1; else sign = +1;
		p++;
		for(x=0; isdigit(*p); p++) x = 10*x + (*p - '0');
		x *= sign;
		if(*p == '-') sign = -1; else sign = +1;
		p++;
		for(y=0; isdigit(*p); p++) y = 10*y + (*p - '0');
		y *= sign;

		gui->MDIsize = rect(x, y, w, h);
		
		done = 1;
	    }
#endif
	    if (!strcmp(opt[0], "background")) {
		if (!strcmpi(opt[1], "Windows"))
		    gui->bg = myGetSysColor(COLOR_WINDOW);
		else gui->bg = nametorgb(opt[1]);
		if (gui->bg != Transparent)
		    done = 1;
	    }
	    if (!strcmp(opt[0], "normaltext")) {
		if (!strcmpi(opt[1], "Windows"))
		    gui->fg = myGetSysColor(COLOR_WINDOWTEXT);
		else gui->fg = nametorgb(opt[1]);
		if (gui->fg != Transparent)
		    done = 1;
	    }
	    if (!strcmp(opt[0], "usertext")) {
		if (!strcmpi(opt[1], "Windows"))
		    gui->user = myGetSysColor(COLOR_ACTIVECAPTION);
		else gui->user = nametorgb(opt[1]);
		if (gui->user != Transparent)
		    done = 1;
	    }
	    if (!strcmp(opt[0], "highlight")) {
		if (!strcmpi(opt[1], "Windows"))
		    gui->hlt = myGetSysColor(COLOR_ACTIVECAPTION);
		else gui->hlt = nametorgb(opt[1]);
		if (gui->hlt != Transparent)
		    done = 1;
	    }
	    if (!strcmp(opt[0], "setwidthonresize")) {
		if (!strcmp(opt[1], "yes"))
		    gui->setWidthOnResize = 1;
		else if (!strcmp(opt[1], "no"))
		    gui->setWidthOnResize = 0;
		done = 1;
	    }
	    if (!strcmp(opt[0], "language")) {
		strcpy(gui->language, opt[1]); 
 		done = 1;
	    }
	    if (!strcmp(opt[0], "buffered")) {
		if (!strcmp(opt[1], "yes"))
		    gui->buffered = 1;
		else if (!strcmp(opt[1], "no"))
		    gui->buffered = 0;		
 		done = 1;
	    }
	} else if (ok == 3) { /* opt[1] == "" */
	    if (!strcmp(opt[0], "language")) {
		strcpy(gui->language, opt[1]); 
 		done = 1;
	    }
	}
	if (!done) {
	    char  buf[128];

	    snprintf(buf, 128, G_("Error at line %d of file %s"),
		     optline(), optfile());
	    askok(buf);
	    cfgerr = 1;
	}
    }
    return !cfgerr;
}

static void apply(button b) /* button callback */
{
    do_apply(); /* to be used outside button callbacks */
}

static void cancel(button b)
{
    cleanup();
    show(RConsole);
}

static void ok(button b)
{
    do_apply(); 
    cleanup();
    show(RConsole);
}

static void cMDI(button b)
{
    enable(toolbar);
    enable(statusbar);
}

static void cSDI(button b)
{
    disable(toolbar);
    disable(statusbar);
}

static void showDialog(Gui gui)
{
    char buf[100];
        
    wconfig = newwindow(G_("Rgui Configuration Editor"), rect(0, 0, 550, 450),
			Titlebar | Centered | Modal);
    setbackground(wconfig, dialog_bg());
    l_mdi = newlabel("Single or multiple windows",
		      rect(10, 10, 140, 20), AlignLeft);
    rb_mdi = newradiobutton("MDI", rect(150, 10 , 70, 20), cMDI);
    rb_sdi = newradiobutton("SDI", rect(220, 10 , 70, 20), cSDI);


    toolbar = newcheckbox("MDI toolbar", rect(300, 10, 100, 20), NULL);
    if(gui->toolbar) check(toolbar);
    statusbar = newcheckbox("MDI statusbar", rect(420, 10, 130, 20), NULL);
    if(gui->statusbar) check(statusbar);
    if(gui->MDI) {
	check(rb_mdi); cMDI(rb_mdi);
    } else {
	check(rb_sdi); cSDI(rb_sdi);
    }

    l_mwin = newlabel("Pager style", rect(10, 40, 90, 20), AlignLeft);
    g_mwin = newradiogroup();
    rb_mwin = newradiobutton("multiple windows", rect(150, 40, 150, 20), NULL);
    rb_swin = newradiobutton("single window", rect(150, 60 , 150, 20), NULL);
    if(gui->pagerMultiple) check(rb_mwin); else check(rb_swin);

    l_lang = newlabel("Language for menus\nand messages", 
		      rect(320, 40, 130, 40), AlignLeft);
    f_lang = newfield(gui->language, rect(450, 45, 60, 20));

/* Font, pointsize, style */

    l_font = newlabel("Font", rect(10, 100, 40, 20), AlignLeft);

    f_font = newdropfield(FontsList, rect(50, 100, 120, 20), NULL);
    tt_font = newcheckbox("TrueType only", rect(180, 100, 110, 20), NULL);
    if (gui->tt_font) check(tt_font);
    settext(f_font, gui->font);

    l_point = newlabel("size", rect(310, 100, 30, 20), AlignLeft);
    d_point = newdropfield(PointsList, rect(345, 100, 50, 20), NULL);
    sprintf(buf, "%d", gui->pointsize);
    settext(d_point, buf);
    l_style = newlabel("style", rect(410, 100, 40, 20), AlignLeft);
    f_style = newdroplist(StyleList, rect(450, 100, 80, 20), NULL);
    setlistitem(f_style, cmatch(gui->style, StyleList));

/* Console size, set widthonresize */
    l_crows = newlabel("Console   rows", rect(10, 150, 100, 20), AlignLeft);
    sprintf(buf, "%d", gui->crows);
    f_crows = newfield(buf, rect(110, 150, 30, 20));
    l_ccols = newlabel("columns", rect(150, 150, 60, 20), AlignLeft);
    sprintf(buf, "%d", gui->ccols);
    f_ccols = newfield(buf, rect(220, 150, 30, 20));
    l_cx = newlabel("Initial left", rect(270, 150, 70, 20), AlignLeft);
    sprintf(buf, "%d", gui->cx);
    f_cx = newfield(buf, rect(350, 150, 40, 20));
    l_cy = newlabel("top", rect(430, 150, 30, 20), AlignLeft);
    sprintf(buf, "%d", gui->cy);
    f_cy = newfield(buf, rect(480, 150, 40, 20));

    c_resize = newcheckbox("set options(width) on resize?",
			   rect(20, 175, 200, 20), NULL);
    if(gui->setWidthOnResize) check(c_resize);

    l_cbb = newlabel("buffer bytes", rect(270, 175, 70, 20), AlignLeft);
    sprintf(buf, "%d", gui->cbb);
    f_cbb = newfield(buf, rect(350, 175, 60, 20));
    l_cbl = newlabel("lines", rect(430, 175, 50, 20), AlignLeft);
    sprintf(buf, "%d", gui->cbl);
    f_cbl = newfield(buf, rect(480, 175, 40, 20));

    c_buff = newcheckbox("buffer console by default?",
			 rect(20, 190, 200, 20), NULL);
    if(gui->buffered) check(c_buff);

/* Pager size */
    l_prows = newlabel("Pager   rows", rect(10, 220, 100, 20), AlignLeft);
    sprintf(buf, "%d", gui->prows);
    f_prows = newfield(buf, rect(110, 220, 30, 20));
    l_pcols = newlabel("columns", rect(150, 220, 60, 20), AlignLeft);
    sprintf(buf, "%d", gui->pcols);
    f_pcols = newfield(buf, rect(220, 220, 30, 20));

/* Graphics window */
    l_grx = newlabel("Graphics windows: initial left",
		    rect(10, 260, 190, 20), AlignLeft);
    sprintf(buf, "%d", gui->grx);
    f_grx = newfield(buf, rect(200, 260, 40, 20));
    l_gry = newlabel("top", rect(270, 260, 30, 20), AlignLeft);
    sprintf(buf, "%d", gui->gry);
    f_gry = newfield(buf, rect(300, 260, 40, 20));

/* Font colours */
    l_cols = newlabel("Console and Pager Colours",
		      rect(10, 300, 520, 20), AlignCenter);
    l_bgcol = newlabel("Background", rect(10, 330, 100, 20), AlignCenter);
    bgcol = newlistbox(ColorName, rect(10, 350, 100, 50), NULL);
    l_fgcol = newlabel("Output text", rect(150, 330, 100, 20), AlignCenter);
    fgcol = newlistbox(ColorName, rect(150, 350, 100, 50), NULL);
    l_usercol = newlabel("User input", rect(290, 330, 100, 20), AlignCenter);
    usercol = newlistbox(ColorName, rect(290, 350, 100, 50), NULL);
    l_highlightcol = newlabel("Titles in pager", rect(430, 330, 100, 20),
			      AlignCenter);
    highlightcol = newlistbox(ColorName, rect(430, 350, 100, 50), NULL);
    setlistitem(bgcol, rgbtonum(gui->bg));
    setlistitem(fgcol, rgbtonum(gui->fg));
    setlistitem(usercol, rgbtonum(gui->user));
    setlistitem(highlightcol, rgbtonum(gui->hlt));

    bApply = newbutton(G_("Apply"), rect(50, 410, 70, 25), apply);
    bSave = newbutton(G_("Save..."), rect(130, 410, 70, 25), save);
    bLoad = newbutton(G_("Load..."), rect(210, 410, 70, 25), load);
    bOK = newbutton(G_("OK"), rect(350, 410, 70, 25), ok);
    bCancel = newbutton(G_("Cancel"), rect(430, 410, 70, 25), cancel);
    show(wconfig);
}

void Rgui_configure()
{
    struct structGUI curGUI;  
    
    getActive(&curGUI);
    showDialog(&curGUI);
}
