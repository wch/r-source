/*
 *  Internal include file
 *  ---------------------
 *  GraphApp internal functions (Windows version).
 *
 *  The type objptr is defined in this file to be an
 *  object pointer. This differs from the normal graphapp.h
 *  definition which makes an objptr a pointer to int.
 *  There are a few reasons for this:
 *  1. The graphapp.h file is platform independent,
 *     so making the basic object type an int pointer
 *     is platform-neutral.
 *  2. Thus the object type does not need to be defined in
 *     graphapp.h since this type includes platform-specific
 *     information.
 *  3. An int pointer facilitates data-hiding. No-one can
 *     poke around in the object data structure.
 *  4. Inside the library code we can treat all fonts, windows
 *     controls, cursors, etc as ordinary objects. There is
 *     no need for typecasting, so code size is reduced.
 */

#ifndef _GRAPH_INT_H
#define _GRAPH_INT_H

/*
 *  Set DEBUG to 1 to produce object debugging, otherwise zero.
 */
#ifndef DEBUG
#define DEBUG 0
#endif

/*
 *  If compiling the whole library from graphapp.c, we
 *  define PROTECTED to be the storage class "static",
 *  hence optimising the library's symbol table by
 *  omitting the internal library function names.
 */
#ifdef _GRAPHAPP_
	#define PROTECTED static
#else
	#define PROTECTED
#endif

/*
 *  Type definitions.
 */

typedef struct callinfo  callinfo;
typedef struct objinfo   objinfo;
typedef objinfo *object;

/* in w32api this needs to be before ga.h */
#ifndef __WINDOWS_H     /* prevent multiple includes */
#include <windows.h>
#endif

#ifndef __GA__VERSION__
#define objptr object
#include "ga.h"
#endif

/* extras */
rect getcliprect(void);
void setcliprect(rect r);
PROTECTED void updatestatus(const char *text);
PROTECTED font new_font_object();

#ifdef __cplusplus
extern "C" {
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>


#include <commdlg.h>
#include <richedit.h>

#ifdef __MWERKS__
	/* Metrowerks Codewarrior Cross-Platform C/C++ Compiler */
	#define COMPILER 32
#endif

#ifdef __MINGW32__
     #define COMPILER 32
     #define WINVER 0x0400
     #define PASS_ARGS 1
#ifndef WIN32
 #define WIN32
#endif
#else
  #define PASS_ARGS	1
#endif


#ifdef _MSC_VER
	/* Microsoft Visual C++ Compiler */
	#ifdef WIN32
		#define COMPILER 32
	#else
		#define COMPILER 16
	#endif
#endif

/*
 *  Set USE_NATIVE_LABELS to 1 for Windows text labels (never necessary).
 *  Set USE_NATIVE_TOGGLES to 1 for Windows checkboxes and radiobuttons.
 *  Set USE_NATIVE_BUTTONS to 1 for Windows buttons.
 */

#define USE_NATIVE_LABELS  0
#define USE_NATIVE_TOGGLES 1
#define USE_NATIVE_BUTTONS 1

#ifdef WINVER
  #if (WINVER <= 0x030a)
	#undef  USE_NATIVE_TOGGLES
	#define USE_NATIVE_TOGGLES 0
	#undef  USE_NATIVE_BUTTONS
	#define USE_NATIVE_BUTTONS 0
  #endif
#endif

#define USE_NATIVE_CONTROLS (USE_NATIVE_LABELS + USE_NATIVE_BUTTONS + USE_NATIVE_TOGGLES)

/*
 *  Check that certain words are defined.
 */
#ifdef COMPILER
   #ifndef __MINGW32__
	#ifndef _export
		#define _export
	#endif
	#ifndef _argc
		#define _argc __argc
	#endif
	#ifndef _argv
		#define _argv __argv
	#endif
  #endif
#endif /* special compiler definitions */

/*
 *  Object types.
 */

	#define BaseObject        0x4000

	#define Image8            0x0008
	#define Image32           0x0020

	#define ControlObject     0x1000
	#define WindowObject      0x1100
	#define BitmapObject      0x0200
	#define CursorObject      0x0400
	#define FontObject        0x0800

	#define UserObject        0x1080
	#define LabelObject       0x1001
	#define ButtonObject      0x1004
	#define CheckboxObject    0x1005
	#define RadioObject       0x1006
	#define ScrollbarObject   0x1008
	#define FieldObject       0x1011
	#define TextboxObject     0x1012
	#define ListboxObject     0x1020
	#define MultilistObject   0x1021
	#define DroplistObject    0x1022
	#define DropfieldObject   0x1023
	#define ProgressbarObject 0x1024

	#define MenubarObject     0x0041
	#define MenuObject        0x0042
	#define MenuitemObject    0x0048

	#define RadiogroupObject  0x2006

        #define PrinterObject     0x0030
        #define MetafileObject    0x0050

/*
 *  Object information structures.
 */

struct objinfo
{
	int 	kind;	/* what kind of object is it? */
	int 	refcount; /* equals zero after del() */
	HANDLE	handle;	/* handle to associated Windows object */
        object  menubar,popup,toolbar;
        char    status[256];
	object	next;	/* next object in the list */
	object	prev;	/* previous object in the list */
	object	parent;	/* object's parent/container */
	object	child;	/* first born */

	actionfn die;	/* private object destructor */

	rect	rect;	/* rectangle size */
	int 	depth;	/* pixel depth */

	drawstate drawstate; /* private drawstate */
	image	img;	/* associated image */

	int 	id;	/* a unique id number */
	long	state;	/* enabled/visible/armed etc */
	long	flags;	/* kind of window/child control */
	void *	data;	/* data supplied by user */
	char *	text;	/* text associated with the object */
	rgb 	fg;	/* foreground colour */
	rgb 	bg;	/* background colour */

	actionfn action; /* button/checkbox action */
	intfn 	hit;	/* menuitem/scrollbar action */
	int 	value;	/* current argument to hit() */
	int 	key;	/* menuitem key equivalent */
	int 	shortcut; /* menu shortcut key */
	int 	max;	/* scrollbar maximum value */
	int 	size;	/* scrollbar page size */
	int 	xmax;	/* scrollbar maximum value */
	int 	xsize;	/* scrollbar page size */

	callinfo *call;	/* window/control call-backs */
	void *	extra;	/* for extra internal data */
	WNDPROC	winproc; /* control's normal event handler */

	#if USE_NATIVE_CONTROLS
	HBRUSH  bgbrush; /* background brush */
	#endif
  };

struct callinfo
{
	actionfn die;		/* user-defined destructor */
	actionfn close;		/* window close function */

	drawfn	redraw;		/* draw the window/control */
	drawfn	resize;		/* window/control was resized */

	keyfn	keydown;	/* normal key pressed */
	keyfn	keyaction;	/* function/arrow key pressed */

	mousefn	mousedown;	/* mouse button was clicked */
	mousefn	mouseup;	/* mouse button was released */
	mousefn	mousemove;	/* mouse was moved */
	mousefn	mousedrag;	/* mouse dragged (button is down) */
	mousefn	mouserepeat;	/* mouse-down timer auto repeat */

	dropfn drop;		/* drag-and-drop function */
    
	actionfn focus;
  };


/*  Useful definitions.  */

#undef min
#undef max
#define min(a,b) (((a) < (b)) ? (a) : (b))
#define max(a,b) (((a) > (b)) ? (a) : (b))

#define MinMenuID  0x0100
#define MinChildID 0x6000
#define MinDocID   0xE000

#define sendmessage(a,b,c,d) SendMessage((HWND)(a),(UINT)(b),(WPARAM)c,(LPARAM)d)

/*
 *  Function prototypes.
 */

/* Array memory management. */

  #define create(type)  ( (type*) memalloc(sizeof(type)) )
  #define array(n,type) ( (type*) memalloc(n*sizeof(type)) )
  #define len(a)        ( memlength((char*)(a))/sizeof((a)[0]) )
  #define element(a,i)  ( (((i)<len(a)) && ((i)>=0)) ? (a)[i] : 0 )
  #define append(a,e)   ( *(char**)&(a)=memexpand((char*)(a),sizeof((a)[0])),\
				(a)[len(a)-1]=(e) )
  #define join(a,b)     ( *(char**)&(a)=memjoin((char*)(a),(char*)(b)) )
  #define discard(a)    ( memfree((char*)(a)), (a)=0 )

  char * memalloc(long size);
  void   memfree(char *a);
  long   memlength(char *a);
  char * memexpand(char *a, long extra);
  char * memjoin(char *a, char *b);

/* Module initialisation methods. */

  PROTECTED void  init_objects(void);
  PROTECTED void  init_events(void);
  PROTECTED void  init_contexts(void);
  PROTECTED void  init_menus(void);
  PROTECTED void  init_fonts(void);
  PROTECTED void  init_cursors(void);

  PROTECTED window  simple_window(void);

/* Module finaliser methods. */

  PROTECTED void  app_cleanup(void);

  PROTECTED void  finish_objects(void);
  PROTECTED void  finish_events(void);
  PROTECTED void  finish_contexts(void);

/* Object management. */

  PROTECTED object  new_object(int kind, HANDLE handle, object parent);
  PROTECTED object  tree_search(object top, HANDLE handle, int id, int key);
  PROTECTED object  find_object(HANDLE handle, int id, int key);
  PROTECTED void    move_to_front(object obj);
  PROTECTED void    apply_to_list(object first, actionfn fn);

  #define find_by_handle(h) find_object(h, 0, 0)
  #define find_by_id(id)    find_object(0, id, 0)
  #define find_by_key(base, key)  tree_search(base, 0, 0, key)

/* Object refcounts and deletion. */

  PROTECTED void  decrease_refcount(object obj);
  PROTECTED void  increase_refcount(object obj);
  PROTECTED void  protect_object(object obj);
  PROTECTED void  deletion_traversal(void);

/* Menu event management. */

  PROTECTED void  adjust_menu(WPARAM wParam);
  PROTECTED void  handle_menu_id(WPARAM wParam);
  PROTECTED int  handle_menu_key(WPARAM wParam);

/* Control event management. */

  PROTECTED void   handle_control(HWND hwnd, UINT message);
  PROTECTED object find_valid_sibling(object obj);

/* Dialog event management */

  PROTECTED void handle_findreplace(HWND hwnd, LPFINDREPLACE pfr);
  PROTECTED HWND get_modeless();

/* Drawing context management. */

  PROTECTED void  add_context(object obj, HDC dc, HGDIOBJ old);
  PROTECTED HDC   get_context(object obj);
  PROTECTED void  remove_context(object obj);
  PROTECTED void  del_context(object obj);
  PROTECTED void  del_all_contexts(void);

  PROTECTED void  fix_brush(HDC dc, object obj, HBRUSH brush);

/* Window private functions. */

  PROTECTED rect  screen_coords(object obj);
  PROTECTED void  show_window(object obj);
  PROTECTED void  hide_window(object obj);

/* Image private functions. */

  PROTECTED image   load_gif(const char *filename);
  PROTECTED void    save_gif(image img, const char *filename);

  PROTECTED rgb  get_image_pixel(image img, int x, int y);
  PROTECTED rgb  get_monochrome_pixel(image img, int x, int y);
  PROTECTED rgb  get_grey_pixel(image img, int x, int y);

  PROTECTED int  has_transparent_pixels(image img);

/* Debugging functions. */

  #if DEBUG
  PROTECTED void    printimage(FILE *file, image img);
  PROTECTED void    print_object_list(void);
  #endif

/* String functions. */

  char * new_string(const char *src);
  void   del_string(const char *str);
  long   string_length(const char *s);
  void   copy_string(char *dest, const char *src);
  int    compare_strings(const char *s1, const char *s2);
  const char * add_strings(const char *s1, const char *s2);
  char * char_to_string(char ch);
  char * int_to_string(long i);
  char * float_to_string(float f);

  PROTECTED int    string_diff(const char *s, const char *t);
  PROTECTED char * to_dos_string(const char *str);
  PROTECTED char * to_c_string(const char *str);

/* New functions yet to be placed in the official header file */


/*
 *  Library internal variables.
 */

  extern int    is_NT;
  extern unsigned int localeCP; /* from Defn.h */
  extern int    app_initialised;
  extern char * app_name;

  extern HANDLE	this_instance;
  extern HANDLE	prev_instance;

  long FAR PASCAL _export app_win_proc (HWND, UINT, UINT, LONG);
  long FAR PASCAL _export app_doc_proc (HWND, UINT, UINT, LONG);
  long FAR PASCAL _export app_work_proc (HWND, UINT, UINT, LONG);
  long FAR PASCAL _export app_control_procedure (HWND, UINT, UINT, LONG);
  UINT FAR PASCAL _export app_timer_procedure(HWND, UINT, UINT, DWORD);
  extern WNDPROC	  app_control_proc;

  extern int 	menus_active;
  extern int 	active_windows;
  extern int 	child_id;

  extern window  current_window;
  extern menubar current_menubar;
  extern menu    current_menu;

  extern HACCEL	hAccel;
  extern HWND	hwndMain;
  extern HWND	hwndClient;
  extern HWND	hwndFrame;
  extern object MDIFrame;
  extern object MDIToolbar;
  extern HWND   MDIStatus;
  extern HDC      dc;
  extern HPEN     the_pen;
  extern HBRUSH   the_brush;
  extern COLORREF win_rgb;

  extern drawstruct app_drawstate;

  extern drawstate current;	/* global colour, font &c */
  extern int 	keystate;	/* state of Shift, Ctrl, Alt */

#ifdef __cplusplus
}
#endif

#endif /* GraphApp internal header file */
