/*
 *  GraphApp
 *  --------
 *  Common cross-platform graphics application routines.
 *  Version 2.4 (c) Lachlan Patrick 1996-1998.
 *  This header file is designed to be platform-independent.
 *
 */

/*
 *  Common cross-platform graphics routines library.
 */

#ifndef _GRAPHAPP_H
#define _GRAPHAPP_H	240

/*
 *  Assume C declarations for C++
 */

#ifdef __cplusplus
extern "C" {
#endif /* begin normal C declarations */

/*
 *  Definition of some constants.
 */

#include <stdio.h>
#include <stdlib.h>

#ifndef Pi
#define Pi 3.14159265359
#endif

/*
 *  Types.
 */

typedef unsigned char GAbyte;

#define byte GAbyte

#ifndef objptr
  typedef struct { int kind; } gui_obj;
  typedef gui_obj * objptr;
#endif

typedef unsigned long rgb;    /* red-green-blue colour value */

typedef objptr font;          /* font of certain size and style */
typedef objptr cursor;        /* mouse cursor shape */

typedef objptr drawing;       /* bitmap, window or control */

typedef drawing bitmap;       /* platform-specific bitmap */
typedef drawing window;       /* on-screen window */
typedef drawing control;      /* buttons, text-fields, scrollbars */

typedef control label;        /* text label */
typedef control button;       /* push button */
typedef control checkbox;     /* check-box */
typedef control radiobutton;  /* radio button */
typedef control radiogroup;   /* group of radio buttons */
typedef control field;        /* one-line text field */
typedef control textbox;      /* multi-line text box */
typedef control scrollbar;    /* scroll-bar */
typedef control listbox;      /* list of text */
typedef control progressbar;  /* progress bar */

typedef control menubar;      /* contains menus */
typedef control menu;         /* pull-down menu contains menuitems */
typedef control menuitem;     /* a single item in a pull-down menu */

/*
 *  Structures.
 */

typedef struct point point;
typedef struct rect rect;
typedef struct drawstruct drawstruct;
typedef struct drawstruct *drawstate;
typedef struct imagedata imagedata;
typedef struct imagedata *image;

struct point
{
	int x, y;
};

struct rect
{
	int x, y;		/* top-left point inside rect */
	int width, height;	/* width and height of rect */
};

struct drawstruct
{
	drawing	dest;
	rgb 	hue;
	int 	mode;
	point	p;
	int 	linewidth;
	font	fnt;
	cursor	crsr;
};

struct imagedata {
	int     depth;
	int     width;
	int     height;
	int     cmapsize;
	rgb *   cmap;
	byte *  pixels;
};

/*
 *  Call-backs.
 */

typedef void (*voidfn)(void);
typedef void (*timerfn)(void *data);
typedef void (*actionfn)(control c);
typedef void (*drawfn)(control c, rect r);
typedef void (*mousefn)(control c, int buttons, point xy);
typedef void (*intfn)(control c, int argument);
typedef void (*keyfn)(control c, int key);
typedef void (*menufn)(menuitem m);
typedef void (*scrollfn)(scrollbar s, int position);
typedef void (*dropfn)(control c, char *data);

/*
 *  Mouse buttons state (bit-fields).
 */

#define NoButton    	0x0000
#define LeftButton  	0x0001
#define MiddleButton	0x0002
#define RightButton 	0x0004

/*
 *  ANSI character codes.
 */

#define BELL	0x07
#define BKSP	0x08
#define VTAB	0x0B
#define FF  	0x0C
#define ESC 	0x1B

/*
 *  Edit-key codes.
 */

#define INS 	0x2041
#define DEL 	0x2326
#define HOME	0x21B8
#define END 	0x2198
#define PGUP	0x21DE
#define PGDN	0x21DF
#define ENTER	0x2324

/*
 *  Cursor-key codes.
 */

#define LEFT	0x2190
#define UP  	0x2191
#define RIGHT	0x2192
#define DOWN	0x2193

/*
 *  Function-key codes.
 */

#define F1	0x276C
#define F2	0x276D
#define F3	0x276E
#define F4	0x276F
#define F5	0x2770
#define F6	0x2771
#define F7	0x2772
#define F8	0x2773
#define F9	0x2774
#define F10	0x2775

/*
 *  Redefined functions.
 */
#define REDEFINE_FUNC_NAMES
#define addpt      GA_addpt
#define subpt      GA_subpt
#define equalpt    GA_equalpt
#define newmenu    GA_newmenu
#define newcontrol GA_newcontrol
#define newwindow  GA_newwindow
#define gettext    GA_gettext
#define settext    GA_settext

/*
 *  General functions.
 */

int 	initapp(int argc, char *argv[]);
void	exitapp(void);

void	drawall(void);
int 	peekevent(void);
int 	doevent(void);
void	mainloop(void);

int 	execapp(char *cmd);

/*void	beep(void);*/

/*
 *  Point and rectangle arithmetic.
 */

point	newpoint(int x, int y);
rect	newrect(int left, int top, int width, int height);
rect	rpt(point min, point max);

#define pt(x,y)         newpoint((x),(y))
#define rect(x,y,w,h)   newrect((x),(y),(w),(h))

point	topleft(rect r);
point	bottomright(rect r);
point	topright(rect r);
point	bottomleft(rect r);

point	addpt(point p1, point p2);
point	subpt(point p1, point p2);
point	midpt(point p1, point p2);
point	mulpt(point p1, int i);
point	divpt(point p1, int i);
rect	rmove(rect r, point p);
rect	raddpt(rect r, point p);
rect	rsubpt(rect r, point p);
rect	rmul(rect r, int i);
rect	rdiv(rect r, int i);
rect	growr(rect r, int w, int h);
rect	insetr(rect r, int i);
rect	rcenter(rect r1, rect r2);
int 	ptinr(point p, rect r);
int 	rinr(rect r1, rect r2);
int 	rxr(rect r1, rect r2);
int 	equalpt(point p1, point p2);
int 	equalr(rect r1, rect r2);
rect	clipr(rect r1, rect r2);
rect	rcanon(rect r);

/*
 *  Colour functions and constants.
 */

#define	rgb(r,g,b)    ((((rgb)r)<<16)|(((rgb)g)<<8)|((rgb)b))
#define getalpha(col) (((col)>>24)&0x00FFUL)
#define getred(col)   (((col)>>16)&0x00FFUL)
#define getgreen(col) (((col)>>8)&0x00FFUL)
#define getblue(col)  ((col)&0x00FFUL)

void	setrgb(rgb c);
#define	setcolor(c)  setrgb(c)
#define	setcolour(c) setrgb(c)

/* changed to avoid clashes with w32api 2.0 */
#define gaRed 		0x00FF0000UL
#define gaGreen		0x0000FF00UL
#define gaBlue		0x000000FFUL


#define Transparent     0xFFFFFFFFUL

#define Black		0x00000000UL
#define White		0x00FFFFFFUL
#define Yellow		0x00FFFF00UL
#define Magenta		0x00FF00FFUL
#define Cyan		0x0000FFFFUL

#define Grey		0x00808080UL
#define Gray		0x00808080UL
#define LightGrey	0x00C0C0C0UL
#define LightGray	0x00C0C0C0UL
#define DarkGrey	0x00404040UL
#define DarkGray	0x00404040UL

#define DarkBlue	0x00000080UL
#define DarkGreen	0x00008000UL
#define DarkRed		0x008B0000UL/* changed to match rgb */
#define LightBlue	0x0080C0FFUL
#define LightGreen	0x0080FF80UL
#define LightRed	0x00FFC0FFUL
#define Pink		0x00FFAFAFUL
#define Brown		0x00603000UL
#define Orange		0x00FF8000UL
#define Purple		0x00C000FFUL
#define Lime		0x0080FF00UL

/*
 *  Context functions for bitmaps, windows, controls.
 */

void	addto(control dest);
void	drawto(drawing dest);
void	setlinewidth(int width);

/*
 *  Transfer modes for drawing operations, S=source, D=destination.
 *  The modes are arranged so that, for example, (~D)|S == notDorS.
 */

void	setdrawmode(int mode);

#define Zeros	 0x00
#define DnorS	 0x01
#define DandnotS 0x02
#define notS	 0x03
#define notDandS 0x04
#define notD	 0x05
#define DxorS	 0x06
#define DnandS	 0x07
#define DandS	 0x08
#define DxnorS	 0x09
#define D   	 0x0A
#define DornotS	 0x0B
#define S   	 0x0C
#define notDorS	 0x0D
#define DorS	 0x0E
#define Ones	 0x0F

/*
 *  Drawing functions.
 */

void	bitblt(drawing dest, drawing src, point dp, rect sr, int mode);

void	scrollrect(point dp, rect sr);
void	copyrect(drawing src, point dp, rect sr);
void	texturerect(drawing src, rect r);
void	invertrect(rect r);

rgb 	getpixel(point p);
void	setpixel(point p, rgb c);

/*
 *  Drawing using the current colour.
 */

void	moveto(point p);
void	lineto(point p);

void	drawpoint(point p);
void	drawline(point p1, point p2);
void	drawrect(rect r);
void	fillrect(rect r);
void	drawarc(rect r, int start_angle, int end_angle);
void	fillarc(rect r, int start_angle, int end_angle);
void	drawellipse(rect r);
void	fillellipse(rect r);
void	drawroundrect(rect r);
void	fillroundrect(rect r);
void	drawpolygon(point *p, int n);
void	fillpolygon(point *p, int n);

/*
 *  Drawing text, selecting fonts.
 */

font	newfont(char *name, int style, int size);
void	setfont(font f);

int 	fontwidth(font f);
int 	fontheight(font f);
int 	fontascent(font f);
int 	fontdescent(font f);

#define	getascent(f)	fontascent(f)
#define	getdescent(f)	fontdescent(f)

int	strwidth(font f, char *s);
point	strsize(font f, char *s);
rect	strrect(font f, char *s);

int 	drawstr(point p, char *str);
int	textheight(int width, char *text);
char *	drawtext(rect r, int alignment, char *text);
int 	gprintf(char *fmt, ...);

/*
 *  Text styles and alignments.
 */

#define Plain		0x0000
#define Bold		0x0001
#define Italic		0x0002
#define BoldItalic	0x0003
#define SansSerif	0x0004
#define FixedWidth	0x0008
#define Wide		0x0010
#define Narrow		0x0020

#define AlignTop        0x0000
#define AlignBottom     0x0100
#define VJustify        0x0200
#define VCenter         0x0400
#define VCentre         0x0400
#define AlignLeft       0x0000
#define AlignRight      0x1000
#define Justify	        0x2000
#define Center	        0x4000
#define Centre          0x4000
#define AlignCenter     0x4000
#define AlignCentre     0x4000
#define Underline       0x0800

/*
 *  Find the current state of drawing.
 */

drawing	currentdrawing(void);
rgb 	currentrgb(void);
#define	currentcolor() currentrgb()
#define	currentcolour() currentrgb()
int 	currentmode(void);
point	currentpoint(void);
int 	currentlinewidth(void);
font	currentfont(void);
cursor	currentcursor(void);

/*
 *  Find current keyboard state.
 */

int 	getkeystate(void);

#define AltKey  	0x0001
#define CmdKey  	0x0001
#define CtrlKey		0x0002
#define OptionKey	0x0002
#define ShiftKey	0x0004

/*
 *  Bitmaps.
 */

bitmap	newbitmap(int width, int height, int depth);
bitmap	loadbitmap(char *name);
bitmap	imagetobitmap(image img);
bitmap	createbitmap(int width, int height, int depth, byte *data);
void	setbitmapdata(bitmap b, byte data[]);
void	getbitmapdata(bitmap b, byte data[]);
void	getbitmapdata2(bitmap b, byte **data);

/*
 *  Images.
 */

image	newimage(int width, int height, int depth);
image	copyimage(image img);
void	delimage(image img);

void	setpixels(image img, byte pixels[]);
byte *	getpixels(image img);

void	setpalette(image img, int length, rgb cmap[]);
rgb *	getpalette(image img);
int 	getpalettesize(image img);

image	scaleimage(image src, rect dr, rect sr);
image	convert32to8(image img);
image	convert8to32(image img);
void	sortpalette(image img);

image	loadimage(char *filename);
void	saveimage(image img, char *filename);

void	drawimage(image img, rect dr, rect sr);
void	drawmonochrome(image img, rect dr, rect sr);
void	drawgreyscale(image img, rect dr, rect sr);
#define drawgrayscale drawgreyscale
void	drawdarker(image img, rect dr, rect sr);
void	drawbrighter(image img, rect dr, rect sr);

/*
 *  Windows.
 */

window	newwindow(char *name, rect r, long flags);
void	show(window w);
void	hide(window w);
rect    GetCurrentWinPos(window obj);

/*
 *  Window creation flags.
 */

#define SimpleWindow		0x00000000L

#define Menubar			0x00000010L
#define Titlebar		0x00000020L
#define Closebox		0x00000040L
#define Resize			0x00000080L
#define Maximize		0x00000100L
#define Minimize		0x00000200L
#define HScrollbar      	0x00000400L
#define VScrollbar      	0x00000800L

#define Modal			0x00001000L
#define Floating		0x00002000L
#define Centered		0x00004000L
#define Centred 		0x00004000L

#define Workspace		0x00010000L
#define Document		0x00020000L
#define ChildWindow     	0x00040000L

#define TrackMouse      	0x00080000L

#define UsePalette      	0x00100000L

#define StandardWindow	(Titlebar|Closebox|Resize|Maximize|Minimize)

/*
 *  Functions which work for bitmaps, windows and controls.
 */

int 	objdepth(objptr obj);
rect	objrect(objptr obj);
int 	objwidth(objptr obj);
int 	objheight(objptr obj);
void	delobj(objptr obj);

#define	getdepth(obj)	objdepth((objptr)(obj))
#define	getrect(obj)	objrect((objptr)(obj))
#define	getwidth(obj)	objwidth((objptr)(obj))
#define	getheight(obj)	objheight((objptr)(obj))
#define	del(obj)    	delobj((objptr)(obj))

/*
 *  Setting window and control callback functions.
 */

void	setaction(control c, actionfn fn);
void	sethit(control c, intfn fn);
void	setdel(control c, actionfn fn);
void	setclose(control c, actionfn fn);

void	setredraw(control c, drawfn fn);
void	setresize(control c, drawfn fn);

void	setkeydown(control c, keyfn fn);
void	setkeyaction(control c, keyfn fn);

void	setmousedown(control c, mousefn fn);
void	setmousedrag(control c, mousefn fn);
void	setmouseup(control c, mousefn fn);
void	setmousemove(control c, mousefn fn);
void	setmouserepeat(control c, mousefn fn);

void	setdrop(control c, dropfn fn);

void	setonfocus(control c, actionfn fn);

/*
 *  Using windows and controls.
 */

void	clear(control c);
void	draw(control c);
void	redraw(control c);
void	resize(control c, rect r);

void	show(control c);
void	hide(control c);
int 	isvisible(control c);

void	enable(control c);
void	disable(control c);
int 	isenabled(control c);

void	check(control c);
void	uncheck(control c);
int 	ischecked(control c);

void	highlight(control c);
void	unhighlight(control c);
int 	ishighlighted(control c);

void	flashcontrol(control c);
void	activatecontrol(control c);

/*
 *  Changing the state of a control.
 */

void	settext(control c, char *newtext);
char *	gettext(control c);
#define setname(c,newname) settext(c,newname)
#define getname(c) gettext(c)

void	settextfont(control c, font f);
font	gettextfont(control c);

void	setforeground(control c, rgb fg);
rgb 	getforeground(control c);
void	setbackground(control c, rgb bg);
rgb 	getbackground(control c);

void	setvalue(control c, int value);
int 	getvalue(control c);
void	setdata(control c, void *data);
void *	getdata(control c);

window	parentwindow(control c);

/*
 *  Control states.
 */

#define Visible		0x0001L
#define Enabled 	0x0002L
#define Checked 	0x0004L
#define Highlighted	0x0008L
#define Armed           0x0010L
#define Focus           0x0020L

/*
 *  Create buttons, scrollbars, controls etc on the current window.
 */

control	  newcontrol(char *text, rect r);

drawing	  newdrawing(rect r, drawfn fn);
drawing	  newpicture(image img, rect r);

button	  newbutton(char *text, rect r, actionfn fn);
button	  newimagebutton(image img, rect r, actionfn fn);
void	  setimage(control c, image img);

checkbox  newcheckbox(char *text, rect r, actionfn fn);
checkbox  newimagecheckbox(image img, rect r, actionfn fn);

radiobutton newradiobutton(char *text, rect r, actionfn fn);
radiogroup  newradiogroup(void);

scrollbar newscrollbar(rect r, int max, int pagesize, scrollfn fn);
void	  changescrollbar(scrollbar s, int where, int max, int size);

label	  newlabel(char *text, rect r, int alignment);
field	  newfield(char *text, rect r);
field	  newpassword(char *text, rect r);
textbox	  newtextbox(char *text, rect r);
textbox	  newtextarea(char *text, rect r);
textbox	  newrichtextarea(char *text, rect r);

listbox	  newlistbox(char *list[], rect r, scrollfn fn);
listbox	  newdroplist(char *list[], rect r, scrollfn fn);
listbox	  newdropfield(char *list[], rect r, scrollfn fn);
listbox	  newmultilist(char *list[], rect r, scrollfn fn);
int 	  isselected(listbox b, int index);
void	  setlistitem(listbox b, int index);
int 	  getlistitem(listbox b);
void	  changelistbox(listbox b, char *new_list[]);

progressbar newprogressbar(rect r, int pmin, int pmax, int incr, int smooth);
void setprogressbar(progressbar obj, int n);
void stepprogressbar(progressbar obj, int n);
void setprogressbarrange(progressbar obj, int pbmin, int pbmax);


menubar	  newmenubar(actionfn adjust_menus);
menu	  newsubmenu(menu parent, char *name);
menu	  newmenu(char *name);
menuitem  newmenuitem(char *name, int key, menufn fn);

/*
 *  Text editing functions.
 */

void  undotext(textbox t);
void  cuttext(textbox t);
void  copytext(textbox t);
void  cleartext(textbox t);
void  pastetext(textbox t);
void  inserttext(textbox t, char *text);
void  selecttext(textbox t, long start, long end);
void  textselection(textbox t, long *start, long *end);

/*
 *  Dialogs.
 */

#define YES    1
#define NO    -1
#define CANCEL 0

void	apperror(char *errstr);
void	askok(char *info);
int 	askokcancel(char *question);
int 	askyesno(char *question);
int 	askyesnocancel(char *question);
char *	askstring(char *question, char *default_string);
char *	askpassword(char *question, char *default_string);
char *	askfilename(char *title, char *default_name);
char *	askfilesave(char *title, char *default_name);
char *	askUserPass(char *title);

/*
 *  Time functions.
 */

int 	settimer(unsigned millisec);
void	settimerfn(timerfn timeout, void *data);
int 	setmousetimer(unsigned millisec);
void	delay(unsigned millisec);
long	currenttime(void);

/*
 *  Cursors.
 */

cursor	newcursor(point hotspot, image img);
cursor	createcursor(point offset, byte *white_mask, byte *black_shape);
cursor	loadcursor(char *name);
void	setcursor(cursor c);

/*
 *  Change the drawing state.
 */

drawstate copydrawstate(void);
void	setdrawstate(drawstate saved_state);
void	restoredrawstate(drawstate saved_state);
void	resetdrawstate(void);


/*
 *  Library supplied variables.
 */

extern	font	SystemFont;	/* system font */
extern	font	Times;  	/* times roman font (serif) */
extern	font	Helvetica;	/* helvetica font (sans serif) */
extern	font	Courier;	/* courier font (fixed width) */

#include <R_ext/libextern.h>
LibExtern font		FixedFont;	/* fixed-width font */
LibExtern cursor	ArrowCursor;	/* normal arrow cursor */
LibExtern cursor	BlankCursor;	/* invisible cursor */
LibExtern cursor	WatchCursor;	/* wait for the computer */
LibExtern cursor	CaretCursor;	/* insert text */
LibExtern cursor	TextCursor;	/* insert text */
LibExtern cursor	HandCursor;	/* hand pointer */
LibExtern cursor	CrossCursor;	/* cross pointer */
#undef LibExtern
#undef extern

#ifdef __cplusplus
}
#endif /* end normal C declarations */


#ifdef __cplusplus

/* begin C++ declarations */

/*
 *  Point and rectangle arithmetic.
 */

inline point operator +  (point p, point p2)  {p.x+=p2.x; p.y+=p2.y; return p;}
inline point operator -  (point p, point p2)  {p.x-=p2.x; p.y-=p2.y; return p;}
inline point operator += (point& p, point p2) {p.x+=p2.x; p.y+=p2.y; return p;}
inline point operator -= (point& p, point p2) {p.x-=p2.x; p.y-=p2.y; return p;}

inline rect operator +  (rect r, point p)  {r.x+=p.x; r.y+=p.y; return r;}
inline rect operator -  (rect r, point p)  {r.x-=p.x; r.y-=p.y; return r;}
inline rect operator += (rect& r, point p) {r.x+=p.x; r.y+=p.y; return r;}
inline rect operator -= (rect& r, point p) {r.x-=p.x; r.y-=p.y; return r;}

inline rect operator +  (rect r, int i)    {return insetr(r,-i);}
inline rect operator -  (rect r, int i)    {return insetr(r,i);}
inline rect operator ++ (rect& r)          {return r=insetr(r,-1);}
inline rect operator -- (rect& r)          {return r=insetr(r,1);}
inline rect operator += (rect& r, int i)   {return r=insetr(r,-i);}
inline rect operator -= (rect& r, int i)   {return r=insetr(r,i);}

inline int operator == (point p1, point p2) {return equalpt(p1,p2);}
inline int operator == (rect r1, rect r2)   {return equalr(r1,r2);}
inline int operator != (point p1, point p2) {return !equalpt(p1,p2);}
inline int operator != (rect r1, rect r2)   {return !equalr(r1,r2);}

#endif /* end C++ declarations */

#endif /* Common cross-platform graphics library. */
