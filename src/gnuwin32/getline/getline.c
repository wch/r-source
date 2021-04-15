#include <R_ext/Boolean.h>
#include <R_ext/Error.h>

/*
 * Copyright (C) 1991, 1992, 1993 by Chris Thewalt (thewalt@ce.berkeley.edu)
 *
 * Permission to use, copy, modify, and distribute this software 
 * for any purpose and without fee is hereby granted, provided
 * that the above copyright notices appear in all copies and that both the
 * copyright notice and this permission notice appear in supporting
 * documentation.  This software is provided "as is" without express or
 * implied warranty.
 *
 * Thanks to the following people who have provided enhancements and fixes:
 *   Ron Ueberschaer, Christoph Keller, Scott Schwartz, Steven List,
 *   DaviD W. Sanderson, Goran Bostrom, Michael Gleason, Glenn Kasten,
 *   Edin Hodzic, Eric J Bivona, Kai Uwe Rommel, Danny Quah, Ulrich Betzler
 */

 /* Copyright (C) 2018-2021 The R Core Team */

#include       "getline.h"

static int      gl_tab();  /* forward reference needed for gl_tab_hook */
int 		(*gl_in_hook)() = 0;
int 		(*gl_out_hook)() = 0;
int 		(*gl_tab_hook)() = gl_tab;

#include <Rconfig.h>
#include <R_ext/Riconv.h>
#include <errno.h>

#include <rlocale.h>
extern Rboolean mbcslocale;
#define mbs_init(x) memset(x, 0, sizeof(mbstate_t))

/* NB:  this define must match the one in src/main/scan.c */
#define CONSOLE_PROMPT_SIZE	256

#include <string.h>
#include <ctype.h>
#include <setjmp.h>
#include <stdlib.h>
#include <stdio.h>
#include <io.h>

/******************** internal interface *********************************/

/* Note for multi-byte support. The original getline only worked on single-byte
   characters all of print width 1, such as ASCII. This R version has been
   extended by R Core to support multi-byte characters of varying print widths,
   with some preparation for edit units composed of multiple (Unicode)
   characters. The present code uses an approximation where an edit unit is
   a sequence of a printable character of width greater than one, followed
   by a sequence of printable characters of width zero.

   Symbols starting with "w_" return/hold offsets in "widths" relative to the
   edit buffer gl_buf. As in the original version, symbols not starting with
   "w_" hold offsets in bytes.
 */
static int      BUF_SIZE;               /* dimension of the buffer received*/
static int      gl_init_done = -1;	/* terminal mode flag  */
static int      gl_w_termw = 80;	/* actual terminal width */
static int      gl_w_width = 0;		/* net size available for input */
static int      gl_extent = 0;		/* how far to redraw, 0 means all */
static int      gl_overwrite = 0;	/* overwrite mode */
static int      gl_pos, gl_cnt = 0;     /* position and size of input */
static int      gl_w_pos;
static int      gl_w_cnt = 0;
static char    *gl_buf;                 /* input buffer */
static char    *gl_killbuf = NULL;      /* killed text */
static const char    *gl_prompt;	/* to save the prompt string */
static int      gl_search_mode = 0;	/* search mode flag */

static jmp_buf  gl_jmp;

static void     gl_init(void);		/* prepare to edit a line */
static void     gl_cleanup(void);	/* to undo gl_init */
static void     gl_char_init(void);	/* get ready for no echo input */
static void     gl_char_cleanup(void);	/* undo gl_char_init */
static size_t   gl_w_strlen(const char *); /* width of a string */
static size_t   gl_e_strlen(const char *); /* edit units in a string */
static size_t 	(*gl_w_promptlen)() = (size_t(*)())gl_w_strlen; 
					/* returns printable prompt width */

static void     gl_addchar(int);	/* install specified char */
static void     gl_del(int);		/* del, either left (-1) or cur (0) */
static void     gl_error(const char *); /* write error msg and die */
static void     gl_fixup(const char *, int, int); /* fixup state variables and screen */
static int      gl_getc(void);		/* read one char from terminal */
static void     gl_kill(int);		/* delete to EOL */
static void     gl_newline(void);	/* handle \n or \r */
static void     gl_putc(int);		/* write one char to terminal */
static void     gl_puts(const char *);	/* write a line to terminal */
static void     gl_redraw(void);	/* issue \n and redraw all */
static void     gl_transpose(void);	/* transpose two chars */
static void     gl_yank(void);		/* yank killed text */
static void     gl_word(int);		/* move a word */
static void     gl_killword(int);

void     gl_hist_init(int, int);	/* initializes hist pointers */
char    *gl_hist_next();		/* return ptr to next item */
char    *gl_hist_prev();		/* return ptr to prev item */
static char    *hist_save();		/* makes copy of a string, without NL */

static void     search_addchar(int);	/* increment search string */
static void     search_term(void);	/* reset with current contents */
static void     search_back(int);	/* look back for current string */
static void     search_forw(int);	/* look forw for current string */
static void     gl_beep(void);          /* try to play a system beep sound */

static size_t   gl_w_from_b(size_t);    /* translate gl_buff offset from bytes to widths */
static size_t   gl_b_from_w(size_t);    /* translate gl_buff offset from widths to bytes */
static size_t   gl_w_align_left(size_t);   /* reduce width offset looking for start of edit unit */
static size_t   gl_w_align_right(size_t);  /* increase width offset looking of start of edit unit */

static void    *gl_nat_to_ucs = NULL;   /* iconv conversion descriptor to UCS-4 */
static void    *gl_nat_to_utf16 = NULL; /* iconv conversion descriptor for UTF-16 */
static void    *gl_ucs_to_nat = NULL;   /* iconv conversion descriptor from UCS-4 */
static void    *gl_oem_to_ucs = NULL;   /* iconv conversion descriptor OEM CP -> UCS-4 */
static size_t  *gl_b2w_map = NULL;      /* map gl_buff offset from bytes to widths */
static size_t  *gl_w2b_map = NULL;      /* map gl_buff offset from widths to bytes */
static size_t  *gl_w2e_map = NULL;      /* map gl_buff offset from widths to edit units */

/************************ nonportable part *********************************/

#define WIN32_LEAN_AND_MEAN 1
#include <windows.h>
static HANDLE Win32OutputStream, Win32InputStream = NULL;
static DWORD OldWin32Mode, AltIsDown;

static void
gl_char_init()			/* turn off input echo */
{
   if (!Win32InputStream) {
       Win32InputStream = GetStdHandle(STD_INPUT_HANDLE);
       Win32OutputStream = GetStdHandle(STD_OUTPUT_HANDLE);	
   }
   GetConsoleMode(Win32InputStream,&OldWin32Mode);
   SetConsoleMode(Win32InputStream, ENABLE_PROCESSED_INPUT); /* So ^C works */
   AltIsDown = 0;
}

static void
gl_char_cleanup(void)		/* undo effects of w_gl_char_init */
{
   SetConsoleMode(Win32InputStream,OldWin32Mode);
   AltIsDown = 0;
}

/* Convert the number (xxx) entered via ALT+xxx to UCS-4. */
static int
gl_alt_to_ucs(int alt)
{
    if (alt <= 0 || alt > 255)
	return 0;

    /* TODO: this is how it worked before, but it would be better to treat
       the input as (decoded) index of the character and to support more
       than a single byte. */
    R_wchar_t uc = 0;
    const char *inbuf = (char *)&alt;
    char *outbuf = (char *)&uc;
    size_t inbytesleft = sizeof(int);  /* the ALT code only uses 1 byte */
    size_t outbytesleft = 4;
    size_t status;

    Riconv(gl_oem_to_ucs, NULL, NULL, NULL, NULL);
    status = Riconv(gl_oem_to_ucs, &inbuf, &inbytesleft, &outbuf, &outbytesleft);
    if (status == (size_t)-1 && errno != E2BIG) {
	gl_putc('\a');
	return 0;
    }
    return uc;
}

/* Get a UCS-4 character without echoing it to screen. */
static int
gl_getc(void)
{
    int             c;

    /* Initial version by Guido Masarotto (3/12/98):
         "get Ansi char code from a Win32 console" */
    DWORD a;
    INPUT_RECORD r;
    DWORD st;
    WORD vk;
    CONSOLE_SCREEN_BUFFER_INFO csb;
    wchar_t high = 0;
    int bbb = 0, nAlt=0, n, hex = 0;
    static int debug_codes = 0;

    c = 0; 
    while (!c) {
      /* Following two lines seem to be needed under Win2k to reshow the
         cursor. */
      GetConsoleScreenBufferInfo(Win32OutputStream, &csb);
      SetConsoleCursorPosition(Win32OutputStream, csb.dwCursorPosition);
      /* Originally uChar.AsciiChar was used and for MBCS characters
         ReadConsoleInput returned as many events as bytes in the character.
         As of Windows 8 this reportedly no longer works, ReadConsoleInput
         would only generate one event with the first byte in AsciiChar.
         The bug still exists in Windows 10, and thus we now call
         GetConsoleInputW to get uchar.UnicodeChar. */
      ReadConsoleInputW(Win32InputStream, &r, 1, &a);
      if (!(r.EventType == KEY_EVENT)) break;
      st = r.Event.KeyEvent.dwControlKeyState;
      vk = r.Event.KeyEvent.wVirtualKeyCode;
      if (debug_codes)
        fprintf(stderr, "st %x vk %x down %x char %x\n",
                        (unsigned int)st, (unsigned int)vk,
                        (unsigned int)r.Event.KeyEvent.bKeyDown,
                        (unsigned int)r.Event.KeyEvent.uChar.UnicodeChar);
      if (r.Event.KeyEvent.bKeyDown) {
        AltIsDown = (st & LEFT_ALT_PRESSED);
	if (vk == VK_MENU && AltIsDown) { /* VK_MENU is Alt or AltGr */
	  nAlt = 0;
	  bbb  = 0;
          hex  = 0;
	}
        else if (AltIsDown && vk == 0x49)  /* Alt+I */
          debug_codes = !debug_codes;
	else if (AltIsDown) { /* Interpret Alt+xxx entries */
	  /* Alt+xxx entries may be given directly by user or may
	     result from pasting a character that does not map to
	     a key on the current keyboard (in that case the numbers
	     are with numlock on at least on Windows 10), which has
	     been observed with tilde on Italian keyboard (PR17679). */
	  switch (vk) {
	  case VK_NUMPAD0: case VK_INSERT: case 0x30: n = 0; break;
	  case VK_NUMPAD1: case VK_END: case 0x31: n = 1; break;
	  case VK_NUMPAD2: case 0x32: n = 2; break;
	  case VK_NUMPAD3: case VK_NEXT: case 0x33: n = 3;break;
	  case VK_NUMPAD4: case 0x34: n = 4; break;
	  case VK_NUMPAD5: case VK_CLEAR: case 0x35: n = 5; break;
	  case VK_NUMPAD6: case 0x36: n = 6; break;
	  case VK_NUMPAD7: case VK_HOME: case 0x37: n = 7; break;
	  case VK_NUMPAD8: case 0x38: n = 8; break;
	  case VK_NUMPAD9: case VK_PRIOR: case 0x39: n = 9; break;
          case VK_ADD: /* + on NumPad */ case VK_OEM_PLUS:
            if (nAlt == 0)
              hex = 1;
            n = -1;
            break;
          case 0x41: case 0x42: case 0x43: case 0x44: case 0x45: case 0x46:
            if (hex)
              n = vk - 0x41 + 10; /* A B C D E F */
            else
              n = -1;
            break;
	  default: n = -1;
	  }
	  if (n >= 0) {
            if (hex)
              bbb = 16 * bbb + n;
            else
              bbb = 10 * bbb + n;
	    nAlt += 1;
            if (debug_codes)
	      fprintf(stderr, "Alt+ [%d] down: %x\n", nAlt, bbb);
          }
	  if (!hex && nAlt==3) {
	    c = gl_alt_to_ucs(bbb);
	    bbb = 0;
	    nAlt = 0;
	  } else if (hex && nAlt==8) {
            c = bbb;
            bbb = 0;
            nAlt = 0;
            hex = 0;
          }
	}
	/* Originally, these (LEFT, RIGHT, HOME, END, UP, DOWN, DELETE) were
	   accepted only with ENHANCED_KEY state and other keys with that state
	   were ignored, but with conPTY on Windows 10, the ENHANCED_KEY state is
	   not set. */
	else if (vk == VK_LEFT)
	    c = '\002';
	else if (vk == VK_RIGHT)
	    c = '\006';
	else if (vk == VK_HOME)
	    c = '\001';
	else if (vk == VK_END)
	    c = '\005';
	else if (vk == VK_UP)
	    c = '\020';
	else if (vk == VK_DOWN)
	    c = '\016';
	else if (vk == VK_DELETE)
	    c = '\004';
	else 
            /* Only characters from BMP obtained this way. */
            c = r.Event.KeyEvent.uChar.UnicodeChar;
      }
      else if (vk == VK_MENU && AltIsDown) { 
       /* Alt key up event: could be AltGr, but let's hope users 
	  only press one of them at a time. */

	wchar_t wc = r.Event.KeyEvent.uChar.UnicodeChar;
	if (IS_HIGH_SURROGATE(wc))
	    high = wc;
	else if (IS_LOW_SURROGATE(wc)) {
	    /* Only supplementary characters obtained this way. */
	    c = 0x10000 + ((int) (high & 0x3FF) << 10 ) + 
                           (int) (wc & 0x3FF);
	    high = 0;
	} else if (hex && wc==0) {
	    c = bbb;
	} else if (wc == 0) {
	    /* Handle Alt+xxx */
	    c = gl_alt_to_ucs(bbb);
	} else 
	    /* E.g. combining diacritical marks appear to arrive this way. */
	    c = wc;
	AltIsDown = 0;
	nAlt = 0;
	bbb = 0;
        hex = 0;
      }
      else if (AltIsDown) {
	/* NumPad cursor keys now come without the key-down event, so
	   handle Alt+xxx for them here (when NumLock is disabled) */
        switch (vk) {
        case VK_DOWN: n = 2; break;
        case VK_LEFT: n = 4; break;
        case VK_RIGHT: n = 6; break;
        case VK_UP: n = 8; break;
        default: n = -1;
        }
        if (n >= 0) {
  	  if (hex)
	    bbb = 16 * bbb + n;
	  else
	    bbb = 10 * bbb + n;
          nAlt += 1;
          if (debug_codes)
	    fprintf(stderr, "Alt+ [%d] up: %x\n", nAlt, bbb);
        }
        if (!hex && nAlt==3) {
	  c = gl_alt_to_ucs(bbb);
	  bbb = 0;
	  nAlt = 0;
        } else if (hex && nAlt==8) {
	  c = bbb;
	  bbb = 0;
	  nAlt = 0;
	  hex = 0;
        }
      } 
    }
    return c;
}

static void
gl_putc(int c)
{
   int ch = c;

    write(1, &ch, 1);
    if (ch == '\n') {
	ch = '\r';
        write(1, &ch, 1);	/* RAW mode needs '\r', does not hurt */
    }
}

/* Print bytes to console (via wchar_t API). On Windows 10 running in a DBCS
   locale (not UTF-8), printing using write() is not reliable, sometimes
   there are extra spaces in the output, depending on timing (probably a race
   condition in the console host). Printing via wchar_t interface seems to be
   more reliable. */
static void gl_write(char *s, int len)
{
    wchar_t buf[len + 1]; /* bigger than needed */
    size_t status, inbytesleft, outbytesleft, wchars;
    const char *inbuf;
    char *outbuf;
    static HANDLE Win32OutputStream;

    if (len == 0)
	return;

    Riconv(gl_nat_to_utf16, NULL, NULL, NULL, NULL);
    inbuf = s;
    inbytesleft = len;
    outbuf = (char *)buf;
    outbytesleft = sizeof(buf);
    status = Riconv(gl_nat_to_utf16, &inbuf, &inbytesleft, &outbuf, &outbytesleft);
    if (status == (size_t)-1) 
	gl_error("\n*** Error: getline(): invalid multi-byte character.\n");

    Win32OutputStream = GetStdHandle(STD_OUTPUT_HANDLE);
    wchars = (sizeof(buf) - outbytesleft)/sizeof(wchar_t);
    WriteConsoleW(Win32OutputStream, buf, wchars, NULL, NULL);
}

/********************* fairly portable part *********************************/

static void
gl_puts(const char *const buf)
{
    int len; 
    
    if (buf) {
        len = strlen(buf);
        write(1, buf, len);
    }
}

void gl_error(const char *const buf)
{
    int len = strlen(buf);

    gl_cleanup();
    write(2, buf, len);
    longjmp(gl_jmp,1);
}

static void
gl_init(void)
/* set up variables and terminal */
{
    char oemname[256];

    if (gl_init_done < 0) {		/* -1 only on startup */
        gl_hist_init(512, 1);
    }
    if (isatty(0) == 0 || isatty(1) == 0)
	gl_error("\n*** Error: getline(): not interactive, use stdio.\n");
    if (!(gl_killbuf=calloc(BUF_SIZE,sizeof(char))))
        gl_error("\n*** Error: getline(): not enough memory.\n");

    gl_nat_to_ucs = Riconv_open("UCS-4LE", "");
    if (gl_nat_to_ucs == (void *)-1) 
	gl_error("\n*** Error: getline(): unable to convert to UCS-4.\n");
    gl_nat_to_utf16 = Riconv_open("UTF-16LE", "");
    if (gl_nat_to_utf16 == (void *)-1) 
	gl_error("\n*** Error: getline(): unable to convert to UTF-16.\n");
    gl_ucs_to_nat = Riconv_open("", "UCS-4LE");
    if (gl_ucs_to_nat == (void *)-1)
	gl_error("\n*** Error: getline(): unable to convert to UCS-4.\n");
    snprintf(oemname, sizeof(oemname), "CP%d", (int)GetOEMCP());
    gl_oem_to_ucs = Riconv_open(oemname, "UCS-4LE");
    if (gl_oem_to_ucs == (void *)-1)
	gl_error("\n*** Error: getline(): unable to convert from OEM CP.\n"); 
    if (!(gl_b2w_map = calloc(BUF_SIZE, sizeof(size_t))))
	gl_error("\n*** Error: getline(): not enough memory.\n");
    if (!(gl_w2b_map = calloc(BUF_SIZE, sizeof(size_t))))
	gl_error("\n*** Error: getline(): not enough memory.\n");
    if (!(gl_w2e_map = calloc(BUF_SIZE, sizeof(size_t))))
	gl_error("\n*** Error: getline(): not enough memory.\n");

    gl_char_init();
    gl_init_done = 1;
}

static void
gl_cleanup(void)
/* undo effects of gl_init, as necessary */
{
    if (gl_init_done > 0)
        gl_char_cleanup();
    if (gl_killbuf)
	free(gl_killbuf);
    if (gl_nat_to_ucs && (gl_nat_to_ucs != (void *)-1))
        Riconv_close(gl_nat_to_ucs);
    if (gl_nat_to_utf16 && (gl_nat_to_utf16 != (void *)-1))
        Riconv_close(gl_nat_to_utf16);
    if (gl_ucs_to_nat && (gl_ucs_to_nat != (void *)-1))
        Riconv_close(gl_ucs_to_nat);
    if (gl_oem_to_ucs && (gl_oem_to_ucs != (void *)-1))
        Riconv_close(gl_oem_to_ucs);
    if (gl_b2w_map)
	free(gl_b2w_map);
    if (gl_w2b_map)
        free(gl_w2b_map);
    if (gl_w2e_map)
        free(gl_w2e_map);
    gl_init_done = 0;
}

void
gl_setwidth(int w)
{
    /* not used in R; should arrange for redraw */
    if (w > 20) 
	gl_w_termw = w;
    else 
	gl_error("\n*** Error: minimum screen width is 21\n");
}

/* Number of bytes of the edit unit left of the cursor (loc = -1) or
   right of the cursor (loc=0). */
static int
gl_edit_unit_size(int loc, int cursor)
{
    size_t w = gl_w_from_b(cursor);

    if (loc == -1) {
	/* left */
	if (w == 0)
	    return 0;
	return gl_b_from_w(w) - gl_b_from_w(gl_w_align_left(w-1));
    } else {
	/* right */
	if (w == gl_w_cnt)
	    return 0;
	return gl_b_from_w(gl_w_align_right(w+1)) - gl_b_from_w(w);
    }
}

static int
gl_edit_unit_size_left()
{
    return gl_edit_unit_size(-1 /* left */, gl_pos);
}

static int
gl_edit_unit_size_right()
{
    return gl_edit_unit_size(0 /* right */, gl_pos);
}

static size_t
gl_w_from_b(size_t b)
{
    if (b >= gl_cnt)
	return gl_w_cnt;
    return gl_b2w_map[b];
}

static size_t
gl_b_from_w(size_t w)
{
    if (w >= gl_w_cnt)
	return gl_cnt;
    return gl_w2b_map[w];
}

static size_t
gl_w_align_left(size_t w)
{
    size_t e;
    
    if (w >= gl_w_cnt)
	return w;
    for(e = gl_w2e_map[w]; (w>0) && (gl_w2e_map[w-1] == e); w--);
    return w;
}

static size_t
gl_w_align_right(size_t w)
{
    if (w == 0)
	return w;

    size_t e;
    for(e = gl_w2e_map[w-1]; (w < gl_w_cnt) && (gl_w2e_map[w] == e); w++);
    return w;
}

/* Update map of characters, widths and edit units to reflect changes in gl_buf,
   given changes in the interval [change, change+gl_extent>. Returns true when
   the (print) width of this interval remains unchanged, false otherwise.

   This also updates gl_cnt and gl_w_cnt. */
static int
update_map(size_t change)
{
    int consider_extent = 1;
    size_t w_old_extent;

    if (gl_extent == 0)
	consider_extent = 0;

    if (gl_extent && consider_extent)
	w_old_extent = gl_w_from_b(change + gl_extent);

    gl_cnt = strlen(gl_buf);
    if (gl_cnt == 0) {
	gl_w_cnt = 0;
	return 0;
    }

    size_t w, b, e, iw, ib, width;
    size_t last_w, last_b, last_e;
    R_wchar_t uc;
    size_t inbytesleft, last_inbytesleft, outbytesleft, status;
    const char *inbuf;
    char *outbuf;

    inbytesleft = gl_cnt - change;
    inbuf = gl_buf + change;
    Riconv(gl_nat_to_ucs, NULL, NULL, NULL, NULL);

    if (change == 0) {
	gl_b2w_map[0] = 0;
	gl_w2b_map[0] = 0;
	gl_w2e_map[0] = 0;
	w = b = e = 0;
    } else {
	b = change;
	w = gl_b2w_map[b];
	e = gl_w2e_map[w];
    }
    ib = b + 1; 
    iw = w + 1;

    while(inbytesleft > 0) {
	outbytesleft = 4;
	outbuf = (char *)&uc;
	last_inbytesleft = inbytesleft;
	status = Riconv(gl_nat_to_ucs, &inbuf, &inbytesleft, &outbuf, &outbytesleft);
	if (status == (size_t)-1 && errno != E2BIG)
	    gl_error("\n*** Error: getline(): invalid multi-byte character.\n");

	width = iswprint(uc) ? Ri18n_wcwidth(uc) : 0;

	last_b = b;
	last_w = w;
	last_e = e;
	/* tab should not appear here */
	w += width;
	b += last_inbytesleft - inbytesleft;
	if (width > 0)
	    /* this is an approximation, ideally use complete grapheme here */
	    e++;

	for(; ib < b; ib++)
	    gl_b2w_map[ib] = last_w;
	gl_b2w_map[ib] = w;
	for(; iw < w; iw++) {
	    gl_w2b_map[iw] = last_b;
	    gl_w2e_map[iw] = last_e;
	}
	gl_w2b_map[iw] = b;
	gl_w2e_map[iw] = e;

	if (consider_extent && (b == change + gl_extent) && (w == w_old_extent))
	    /* gl_w_cnt is unchanged */
	    return 1;
    }
    gl_w_cnt = w;
    return 0;
}

/* Returns 1 on EOF */
int
getline(const char *prompt, char *buf, int buflen)
{
    int c, loc, tmp;

    BUF_SIZE = buflen;
    gl_buf = buf;
    gl_buf[0] = '\0';
    if (setjmp(gl_jmp)) {
	if (gl_init_done > 0) {
	    gl_newline();
	    gl_cleanup();
	    return 0;
	}
	/* predictable error in gl_cleanup() leads to infinite loop when R asks
	   whether the image should be saved */
	gl_cleanup();
	return 1;
    }
    gl_init();	
    gl_pos = 0;
    gl_w_pos = 0;
    gl_prompt = (prompt)? prompt : "";
    if (gl_in_hook)
	gl_in_hook(gl_buf);
    gl_fixup(gl_prompt, -2, BUF_SIZE);
    while ((c = gl_getc()) >= 0) {
	gl_extent = 0;  	/* reset to full extent */
	if (!iswcntrl(c)) {
	    if (gl_search_mode)
	       search_addchar(c);
	    else
	       gl_addchar(c);
	} else {
	    if (gl_search_mode) {
	        if (c == '\033' || c == '\016' || c == '\020') {
	            search_term();
	            c = 0;     		/* ignore the character */
		} else if (c == '\010' || c == '\177') {
		    search_addchar(-1); /* unwind search string */
		    c = 0;
		} else if (c != '\022' && c != '\023') {
		    search_term();	/* terminate and handle char */
		}
	    }
	    switch (c) {
	      case '\n': case '\r': 			/* newline */
		gl_newline();
		gl_cleanup();
		return 0;
	      case '\001': gl_fixup(gl_prompt, -1, 0);		/* ^A, VK_HOME */
		break;
	      case '\002': 	/* ^B, VK_LEFT */
		gl_fixup(gl_prompt, -1, gl_pos - gl_edit_unit_size_left());
		break;
	      case '\003':                                      /* ^C */
		  gl_fixup(gl_prompt, -1, gl_cnt);
		  gl_puts("^C\n");
		  gl_kill(0);
		  gl_fixup(gl_prompt, -2, BUF_SIZE);
		break;
	      case '\004':					/* ^D, VK_DELETE */
		if (gl_cnt == 0) {
		    gl_buf[0] = 0;
		    gl_cleanup();
		    gl_putc('\n');
		    return 0;
		} else {
		    gl_del(0);
		}
		break;
	      case '\005': gl_fixup(gl_prompt, -1, gl_cnt);	/* ^E, VK_END */
		break;
		case '\006': /* ^F */
		gl_fixup(gl_prompt, -1, gl_pos + gl_edit_unit_size_right());
		break;
	      case '\010': case '\177': gl_del(-1);	/* ^H and DEL */
		break;
	      case '\t':        				/* TAB */
                if (gl_tab_hook) {
		    tmp = gl_pos;
	            loc = gl_tab_hook(gl_buf, gl_w_promptlen(gl_prompt), &tmp);
	            if (loc != -1 || tmp != gl_pos)
	                gl_fixup(gl_prompt, loc, tmp);
                }
		break;
	      case '\013': gl_kill(gl_pos);			/* ^K */
		break;
	      case '\014': gl_redraw();				/* ^L */
		break;
	      case '\016': 					/* ^N, VK_DOWN */
		strncpy(gl_buf, gl_hist_next(), BUF_SIZE-2);
		gl_buf[BUF_SIZE-2] = '\0';
                if (gl_in_hook)
	            gl_in_hook(gl_buf);
		gl_fixup(gl_prompt, 0, BUF_SIZE);
		break;
	      case '\017': gl_overwrite = !gl_overwrite;       	/* ^O */
		break;
	      case '\020': 					/* ^P, VK_UP */
		strncpy(gl_buf, gl_hist_prev(),BUF_SIZE-2);
		gl_buf[BUF_SIZE-2] = '\0';
                if (gl_in_hook)
	            gl_in_hook(gl_buf);
		gl_fixup(gl_prompt, 0, BUF_SIZE);
		break;
	      case '\022': search_back(1);			/* ^R */
		break;
	      case '\023': search_forw(1);			/* ^S */
		break;
	      case '\024': gl_transpose();			/* ^T */
		break;
              case '\025': gl_kill(0);				/* ^U */
		break;
              case '\027': gl_killword(-1);			/* ^W */
		break;
	      case '\031': gl_yank();				/* ^Y */
		break;
	      case '\032': 					/* ^Z */
		gl_newline();
		gl_cleanup();
		return 1;
	      case '\033':				/* ansi arrow keys */
		c = gl_getc();
		if (c == '[') {
		    switch(c = gl_getc()) {
		      case 'A':             			/* up */
		        strncpy(gl_buf, gl_hist_prev(), BUF_SIZE-2);
		        gl_buf[BUF_SIZE-2] = '\0';
		        if (gl_in_hook)
	                    gl_in_hook(gl_buf);
		        gl_fixup(gl_prompt, 0, BUF_SIZE);
		        break;
		      case 'B':                         	/* down */
		        strncpy(gl_buf, gl_hist_next(), BUF_SIZE-2);
		        gl_buf[BUF_SIZE-2] = '\0';
                        if (gl_in_hook)
	                    gl_in_hook(gl_buf);
		        gl_fixup(gl_prompt, 0, BUF_SIZE);
		        break;
		    case 'C':                                  /* right */
			gl_fixup(gl_prompt, -1, gl_pos + gl_edit_unit_size_right());
		        break;
		    case 'D':                                  /* left */
			gl_fixup(gl_prompt, -1, gl_pos - gl_edit_unit_size_left());
			break;
		    default: gl_putc('\007');                  /* who knows */
		        break;
		    }
		} else if (c == 'f' || c == 'F') {
		    gl_word(1);
		} else if (c == 'b' || c == 'B') {
		    gl_word(-1);
		} else
		    gl_putc('\007');
		break;
	      default:		/* check for a terminal signal */
                if (c > 0)
		    gl_putc('\007');
		break;
	    }
	}
    }
    gl_newline();
    gl_cleanup();
    return 0;
}

/* Adds bytes from s to the current position of the buffer. The input
   needs to only include complete edit units. */
static void
gl_addbytes(const char *s)
{
    int e, del = 0, size, len, i;

    len = strlen(s);
    if (gl_overwrite == 1) {
	e = gl_e_strlen(s);
	for(i = 0; i < e; i++) {
	    size = gl_edit_unit_size(0 /* right */, gl_pos + del);
	    if (size == 0)
		/* no more edit units */
		break;
	    del += size;
	}
    }
    if (len > del) {
	/* expanding buffer */
	if (gl_cnt + len - del >= BUF_SIZE - 1) 
	    gl_error("\n*** Error: getline(): input buffer overflow\n");
	for (i = gl_cnt; i >= gl_pos + del; i--)
	    gl_buf[i + len - del] = gl_buf[i];
    } else if (len < del) {
	/* reducing buffer */
	for (i = gl_pos + del; i <= gl_cnt; i++)
	    gl_buf[i - (del - len)] = gl_buf[i];
    } else {
	/*  clen == del */
	gl_extent = len;
    }
    for (i=0; i < len; i++)
	gl_buf[gl_pos + i] = s[i];
    gl_fixup(gl_prompt, gl_pos, gl_pos+len);
}

/* Adds character c (UCS-4) to the current position. Normally it would add
   a new edit unit, but eventually this may append to the current edit unit. */
static void
gl_addchar(int c)  
{
    char buf[MB_CUR_MAX + 1];
    size_t status, inbytesleft, outbytesleft, clen, left;
    const char *inbuf;
    char *outbuf;
    int i;

    if (gl_cnt >= BUF_SIZE - 2)
	gl_putc('\a');
    else if (iswprint(c)) {
	Riconv(gl_ucs_to_nat, NULL, NULL, NULL, NULL);
	inbuf = (char *)&c;
	inbytesleft = 4;
	outbuf = buf;
	outbytesleft = MB_CUR_MAX;
	status = Riconv(gl_ucs_to_nat, &inbuf, &inbytesleft, &outbuf, &outbytesleft);
	if (status == (size_t)-1)
	    gl_error("\n*** Error: getline(): invalid multi-byte character.\n");
	clen = MB_CUR_MAX - outbytesleft;
	buf[clen] = '\0';
	
	if (Ri18n_wcwidth(c) > 0) {
	    gl_addbytes(buf);
	    return;
	} else if (GetACP() == 65001 && gl_pos > 0) {
	    /* This is an approximation, ideally we would allow building of
	       arbitrary Unicode sequences (graphemes), including ZWJ. Also,
	       this is experimental and little tested: it seems that currently
	       neither RTerm nor Windows Terminal properly support character
	       composition. */
	    left = gl_edit_unit_size_left();
	  
	    if (left > 0) { 
		if (gl_cnt + clen >= BUF_SIZE - 1)
		    gl_error("\n*** Error: getline(): input buffer overflow\n");

		for (i = gl_cnt; i >= gl_pos; i--)
		    gl_buf[i + clen] = gl_buf[i];
		for (i = 0; i < clen; i++)
		    gl_buf[gl_pos + i] = buf[i];

		gl_fixup(gl_prompt, gl_pos - left, gl_pos + clen);
		return;
	    }
	}
    }
    gl_putc('\a');
}

static void
gl_yank(void)
/* adds the kill buffer to the input buffer at current location */
{
    int  len;

    len = strlen(gl_killbuf);
    if (len > 0)
	gl_addbytes(gl_killbuf);
    else
	gl_beep();
}

static void
gl_transpose(void)
/* switch character under cursor and to left of cursor */
{
    int    c;

    if (gl_pos > 0 && gl_cnt > gl_pos) {
	if(mbcslocale) {
	    int l_len = 0;
	    int r_len = 0;
	    int i = 0;
	    int j = 0;
	    mbstate_t mb_st;

	    mbs_init(&mb_st);
	    for (i = 0; i < gl_pos;) {
		l_len = mbrlen(gl_buf+i, MB_CUR_MAX, &mb_st);
		i += l_len;
	    }
	    mbs_init(&mb_st);
	    r_len = mbrlen(gl_buf+gl_pos, MB_CUR_MAX, &mb_st);
	    for (i = 0; i < r_len; i++) {
		for(j = 0; j < l_len; j++) {
		    c = gl_buf[gl_pos+i-j];
		    gl_buf[gl_pos+i-j] = gl_buf[gl_pos+i-j-1];
		    gl_buf[gl_pos+i-j-1] = (char)c;
		}
	    }
	    gl_extent = l_len + r_len;
	    gl_fixup(gl_prompt, gl_pos - l_len, gl_pos + (r_len - l_len)); 
	} else {
	    c = gl_buf[gl_pos-1];
	    gl_buf[gl_pos-1] = gl_buf[gl_pos];
	    gl_buf[gl_pos] = (char) c;
	    gl_extent = 2;
	    gl_fixup(gl_prompt, gl_pos-1, gl_pos);
	}
    } else
	gl_beep();
}

static void
gl_newline(void)
/*
 * Cleans up entire line before returning to caller. A \n is appended.
 * If line longer than screen, we redraw starting at beginning
 */
{
    int change = gl_cnt;
    int len = gl_cnt;
    /* shifts line back to start position */
    int loc = gl_b_from_w(gl_w_align_left(gl_w_width - 5));

    if (gl_cnt >= BUF_SIZE - 1) { 
        gl_error("\n*** Error: getline(): input buffer overflow\n");
    }
    if (gl_out_hook) {
	change = gl_out_hook(gl_buf);
        len = strlen(gl_buf);
    } 
    if (loc > len)
	loc = len;
    gl_fixup(gl_prompt, change, loc);	/* must do this before appending \n */
    gl_buf[len] = '\n';
    gl_buf[len+1] = '\0';
    gl_putc('\n');
}

static void
gl_del(int loc)
/*
 * Delete a character.  The loc variable can be:
 *    -1 : delete character to left of cursor
 *     0 : delete character under cursor
 */
{
   int i, len;

   if ((loc == -1 && gl_pos > 0) || (loc == 0 && gl_pos < gl_cnt)) {
       len = gl_edit_unit_size(loc, gl_pos);
       for (i = gl_pos+(loc*len); i <= gl_cnt - len; i++)
	   gl_buf[i] = gl_buf[i + len];
       gl_fixup(gl_prompt,gl_pos+(loc * len) , gl_pos+(loc * len));
   } else
       gl_beep();
}

static void
gl_kill(int pos)
        
/* delete from pos to the end of line */
{
    if (pos < gl_cnt) {
	strcpy(gl_killbuf, gl_buf + pos);
	gl_buf[pos] = '\0';
	gl_fixup(gl_prompt, pos, pos);
    } else
	gl_beep();
}

static void
gl_killword(int direction)
{
    int pos = gl_pos;
    int startpos = gl_pos;
    int tmp;
    int i;

    if (direction > 0) {		/* forward */
        while (!isspace(gl_buf[pos]) && pos < gl_cnt) 
	    pos++;
	while (isspace(gl_buf[pos]) && pos < gl_cnt)
	    pos++;
    } else {				/* backward */
	if (pos > 0)
	    pos--;
	while (isspace(gl_buf[pos]) && pos > 0)
	    pos--;
        while (!isspace(gl_buf[pos]) && pos > 0) 
	    pos--;
	if (pos < gl_cnt && isspace(gl_buf[pos]))   /* move onto word */
	    pos++;
    }
    if (pos < startpos) {
    	tmp = pos;
	pos = startpos;
	startpos = tmp;
    }
    memcpy(gl_killbuf, gl_buf + startpos, (size_t) (pos - startpos));
    gl_killbuf[pos - startpos] = '\0';
    if (isspace(gl_killbuf[pos - startpos - 1]))
    	gl_killbuf[pos - startpos - 1] = '\0';
    gl_fixup(gl_prompt, -1, startpos);
    for (i=0, tmp=pos - startpos; i<tmp; i++)
    	gl_del(0);
}	/* gl_killword */

static void
gl_word(int direction)
              
/* move forward or backword one word */
{
    int pos = gl_pos;

    if (direction > 0) {		/* forward */
        while (!isspace(gl_buf[pos]) && pos < gl_cnt) 
	    pos++;
	while (isspace(gl_buf[pos]) && pos < gl_cnt)
	    pos++;
    } else {				/* backword */
	if (pos > 0)
	    pos--;
	while (isspace(gl_buf[pos]) && pos > 0)
	    pos--;
        while (!isspace(gl_buf[pos]) && pos > 0) 
	    pos--;
	if (pos < gl_cnt && isspace(gl_buf[pos]))   /* move onto word */
	    pos++;
    }
    gl_fixup(gl_prompt, -1, pos);
}

static void
gl_redraw(void)
/* emit a newline, reset and redraw prompt and current input line */
{
    if (gl_init_done > 0) {
        gl_putc('\n');
        gl_fixup(gl_prompt, -2, gl_pos);
    }
}

static void
gl_fixup(const char *prompt, int change, int cursor)
              
                      
/*
 * This function is used both for redrawing when input changes or for
 * moving within the input line.  The parameters are:
 *   prompt:  compared to last_prompt[] for changes;
 *   change : the index of the start of changes in the input buffer,
 *            with -1 indicating no changes, -2 indicating we're on
 *            a new line, redraw everything assuming clean line.
 *   cursor : the desired location of the cursor after the call.
 *            A value of BUF_SIZE can be used  to indicate the cursor should
 *            move just past the end of the input line.
 */

/* when change >= 0, change must be aligned to edit units and
                     change+gl_extent must be as well */
/* when cursor != BUF_SIZE, it must be aligned to edit units */

{
    static int   gl_shift;	 /* index of first on screen byte */
    static int   gl_w_shift;
    static int   off_right;	 /* true if more text right of screen */
    static int   off_left;	 /* true if more text left of screen */
    static char  last_prompt[CONSOLE_PROMPT_SIZE] = "";
    int          w_change = -1;
    int          w_cursor;                            
    int          left = 0;     /* index of first byte to print */
    int          right = -1;   /* index of first byte not to print */
    int          w_right;
    int          w_rightmost_printable;
    int          w_pad;        /* how much to erase at end of line */
    int          w_dollar_pad = 0;
    int          w_backup;       /* how far to backup before fixing */
    int          new_shift = 0;  /* value of shift based on cursor */
    int          w_new_shift;
    int          consider_extent = 1;
    int          print_left_dollar = 0;
    int          print_right_dollar = 0;
    int          i;
    int          gl_w_scroll;    /* width of EOL scrolling region */

    gl_w_scroll = gl_w_termw / 3;

    if (change == -2) {   /* reset, initialization, redraw */
	gl_putc('\r');
        gl_pos = gl_cnt = gl_shift = off_right = off_left = 0;
	gl_w_pos = gl_w_cnt = gl_w_shift = 0;

	gl_puts(prompt);
	strncpy(last_prompt, prompt, CONSOLE_PROMPT_SIZE-1);
        gl_w_width = gl_w_termw - gl_w_promptlen(prompt);
	change = 0;
	consider_extent = 0;
    } else if (strcmp(prompt, last_prompt) != 0) {
	gl_putc('\r');
	gl_w_pos = gl_w_shift;
	gl_pos = gl_shift;
	/* temporarily updated gl_w_cnt is only used to include the change in prompt
	   width into calculation of pad, right after that the gl_*cnt values are
	   recomputed (the prompt is never included otherwise into gl_*cnt) */
	gl_w_cnt = gl_w_cnt + gl_w_promptlen(last_prompt) - gl_w_promptlen(prompt);

	gl_puts(prompt);
	strncpy(last_prompt, prompt, CONSOLE_PROMPT_SIZE-1);
        gl_w_width = gl_w_termw - gl_w_promptlen(prompt);
	change = 0;
	consider_extent = 0;
    }

    w_pad = (off_right)? gl_w_width - 1 : gl_w_cnt - gl_w_shift + off_left;   /* old width */
    if (change >= 0) {
	w_change = gl_w_from_b(change);  /* old map or initialization */
	consider_extent = update_map(change) && consider_extent;
	                  /* map_update updates also gl_cnt, gl_w_cnt */
	if (change > gl_cnt) {
	    change = gl_cnt;
	    w_change = gl_w_cnt;
	}
    }
    if (cursor > gl_cnt) {
	if (cursor != BUF_SIZE)		/* BUF_SIZE means end of line */
	    gl_putc('\007');
	cursor = gl_cnt;
    }
    if (cursor < 0) {
	gl_putc('\007');
	cursor = 0;
    }
    w_cursor = gl_w_from_b(cursor);
    w_new_shift = w_cursor - (gl_w_width - 1 - gl_w_scroll);
    if (w_new_shift > 0)
	w_new_shift++; /* adjust if newly off left */
    if (w_new_shift > 0 && gl_w_cnt > w_new_shift  + gl_w_width - 2)
	w_new_shift++; /* adjust if newly off right */
    if (w_new_shift > 0) {
	w_new_shift /= gl_w_scroll;
	w_new_shift *= gl_w_scroll;
	w_new_shift = gl_w_align_right(w_new_shift); 
	new_shift = gl_b_from_w(w_new_shift);
    } else
	w_new_shift = 0;
    w_backup = gl_w_pos - gl_w_shift + off_left;

    if (new_shift != gl_shift) {	/* scroll or redraw/init occurs */
	gl_shift = new_shift;
	gl_w_shift = w_new_shift;
	off_left = print_left_dollar = (gl_shift)? 1 : 0;
        left = gl_shift;
	w_rightmost_printable = gl_w_shift + gl_w_width - 2 - off_left;
	off_right = (gl_w_cnt > w_rightmost_printable + 1)? 1 : 0;

	if (off_right) {
	    /* right needs to account for right-$, but, right is the
	       first byte _not_ to print, while w_rightmost_printable
	       is the last width to print */
	    print_right_dollar = 1;
	    w_right = gl_w_align_left(w_rightmost_printable);
	    right = gl_b_from_w(w_right); 
	    /* there may be something right off the right-$ */
	    w_dollar_pad = w_rightmost_printable - w_right; 
	} else
	    right = gl_cnt;

    } else if (change >= 0) {		/* no scroll, but text changed */
	if (off_left && (change <= gl_shift)) {
	    left = gl_shift;
	    w_backup ++;  /* left-$ present */
	} else {
	    left = change;
	    w_backup = gl_w_pos - w_change;
	}
	w_rightmost_printable = gl_w_shift + gl_w_width - 2 - off_left;
	off_right = (gl_w_cnt > w_rightmost_printable + 1)? 1 : 0;

	if (off_right) {
	    w_right = gl_w_align_left(w_rightmost_printable);
	    right = gl_b_from_w(w_right);
	    if (consider_extent && (left + gl_extent < right))
		right = left + gl_extent;
	    else {
		print_right_dollar = 1;
		/* there may be something right off the right-$ */
		w_dollar_pad = w_rightmost_printable - w_right;
	    }
	} else if (consider_extent && (left + gl_extent < gl_cnt))
	    right = left + gl_extent; 
	else
	    right = gl_cnt;
    }
    w_pad -= (off_right)? gl_w_width - 1 : gl_w_cnt - gl_w_shift + off_left; /* new width */
    w_pad = (w_pad < 0)? 0 : w_pad;

    if (left <= right) {               /* clean up screen */
	for (i=0; i < w_backup; i++) 
	    gl_putc('\b');
	if (print_left_dollar)
	    gl_putc('$');
	if (right > left)
	    gl_write(gl_buf + left, right - left); /* print changed characters */
	gl_pos = right;
	gl_w_pos = gl_w_from_b(gl_pos); 
	if (print_right_dollar)
	    gl_putc('$');
	for(i = 0; i < w_pad + w_dollar_pad; i++)
	    /* erase remains of prev line or right-$ */
	    gl_putc(' ');
	gl_w_pos += print_right_dollar + w_pad + w_dollar_pad;
    }
    i = gl_w_pos - w_cursor;		/* move to final cursor location */
    if (i > 0) {
	while (i--) 
	   gl_putc('\b');
    } else {
	    if (gl_pos < cursor)        /* only to move the cursor on terminal */
		gl_write(gl_buf + gl_pos, cursor - gl_pos);
    }
    gl_w_pos = w_cursor;
    gl_pos = cursor;
}

static int
gl_tab(char *buf, int offset, int *loc)
/* default tab handler, acts like tabstops every 8 cols */
{
    int i, count, len;

    len = gl_w_strlen(buf);
    count = 8 - (offset + *loc) % 8;
    for (i=len; i >= *loc; i--)
        buf[i+count] = buf[i];
    for (i=0; i < count; i++)
        buf[*loc+i] = ' ';
    i = *loc;
    *loc = i + count;
    return i;
}

/******************* strlen stuff **************************************/

/* hook to install a custom gl_w_promptlen, used _only_ for the prompt  */
void gl_strwidth(func)
size_t (*func)();
{
    if (func != 0) {
	gl_w_promptlen = func;
    }
}

/* lenght of string in widths */
static size_t
gl_w_strlen(const char *s)
{
    size_t inbytesleft, outbytesleft, width = 0, status;
    R_wchar_t uc;
    char *outbuf;

    inbytesleft = strlen(s);
    Riconv(gl_nat_to_ucs, NULL, NULL, NULL, NULL);
    while(inbytesleft) {
	outbytesleft = 4;
	outbuf = (char *)&uc;
	status = Riconv(gl_nat_to_ucs, &s, &inbytesleft, &outbuf, &outbytesleft);
	if (status == (size_t)-1 && errno != E2BIG)
	    gl_error("\n*** Error: getline(): invalid multi-byte character.\n");

	if (iswprint(uc))
	    width += Ri18n_wcwidth(uc);
    }
    return width;
}

/* length of string in edit units */
static size_t
gl_e_strlen(const char *s)
{
    size_t inbytesleft, outbytesleft, status, e = 0;
    R_wchar_t uc;
    char *outbuf;

    inbytesleft = strlen(s);
    Riconv(gl_nat_to_ucs, NULL, NULL, NULL, NULL);
    while(inbytesleft) {
	outbytesleft = 4;
	outbuf = (char *)&uc;
	status = Riconv(gl_nat_to_ucs, &s, &inbytesleft, &outbuf, &outbytesleft);
	if (status == (size_t)-1 && errno != E2BIG)
	    gl_error("\n*** Error: getline(): invalid multi-byte character.\n");

	if (iswprint(uc) && Ri18n_wcwidth(uc) > 0)
	    /* this is an approximation, ideally use complete grapheme here */
	    e++;
    }
    return e;
}


/******************* History stuff **************************************/

static int	HIST_SIZE = 512;
static int      hist_pos = 0, hist_last = 0, gl_beep_on = 1;
static char     **hist_buf;

void
gl_hist_init(int size, int beep)
{
    int i;

    HIST_SIZE = size;
    hist_buf = (char **) malloc(size * sizeof(char *));
    if(!hist_buf)
	gl_error("\n*** Error: gl_hist_init() failed on malloc\n");
    hist_buf[0] = "";
    for (i = 1; i < HIST_SIZE; i++)
	hist_buf[i] = (char *)0;
    hist_pos = hist_last = 0;
    gl_init_done = 0;
    gl_beep_on = beep;
}

void
gl_histadd(const char *buf)
{
    const char *p = buf;

    /* in case we call gl_histadd() before we call getline() */
    if (gl_init_done < 0) {		/* -1 only on startup */
        gl_hist_init(512, 1);
        gl_init_done = 0;
    }
    while (*p == ' ' || *p == '\t' || *p == '\n') 
	p++;
    if (*p) {
	hist_buf[hist_last] = hist_save(buf);
	hist_last = hist_last + 1;
	if(hist_last > HIST_SIZE - 1) {
	    int i, size = HIST_SIZE + 512;
	    hist_buf = (char **) realloc(hist_buf, size * sizeof(char *));
	    if(!hist_buf)
		gl_error("\n*** Error: gl_histadd() failed on realloc\n");
	    for(i = HIST_SIZE; i < size; i++)
		hist_buf[i] = (char *)0;
	    HIST_SIZE = size;
	}
	hist_buf[hist_last] = "";
    }
    hist_pos = hist_last;
}

char *
gl_hist_prev(void)
/* loads previous hist entry into input buffer, sticks on first */
{
    char *p = 0;
    int   next = hist_pos - 1;

    if (hist_buf[hist_pos] != 0 && next >= 0) {
        hist_pos = next;
        p = hist_buf[hist_pos];
    } 
    if (p == 0) {
	p = "";
	gl_beep();
    }
    return p;
}

char *
gl_hist_next(void)
/* loads next hist entry into input buffer, clears on last */
{
    char *p = 0;

    if (hist_pos != hist_last) {
        hist_pos = hist_pos+1;
	p = hist_buf[hist_pos];
    } 
    if (p == 0) {
	p = "";
	gl_beep();
    }
    return p;
}

static char *
hist_save(const char *p)
        
/* makes a copy of the string */
{
    char *s = 0;
    int   len = strlen(p);
    char *nl = strchr(p, '\n');

    if (nl) {
        if ((s = (char *) malloc(len)) != 0) {
            memcpy(s, p, len-1);
	    s[len-1] = 0;
	}
    } else {
        if ((s = (char *) malloc(len+1)) != 0) {
            strcpy(s, p);
        }
    }
    if (s == 0) 
	gl_error("\n*** Error: hist_save() failed on malloc\n");
    return s;
}

void gl_savehistory(const char *file, int size)
{
    FILE *fp;
    int i, init;

    if (!file || !hist_last) return;
    fp = fopen(file, "w");
    if (!fp) {
       char msg[256];
       sprintf(msg, "Unable to open %s", file);
       R_ShowMessage(msg);
       return;
    }
    init = hist_last - size;
    init = (init < 0) ? 0 : init;
    for (i = init; i < hist_last; i++)
       fprintf(fp, "%s\n", hist_buf[i]);
    fclose(fp); 
}

void gl_loadhistory(const char *file)
{
    FILE *fp;
    int i;
    char buf[1000];

    if (!file) return;
    fp = fopen(file, "r");
    if (!fp) {
       return;
    }
    for(i = 0;; i++) {
	if(!fgets(buf, 1000, fp)) break;
	gl_histadd(buf);
    }
    fclose(fp); 
}


/******************* Search stuff **************************************/

static char  search_prompt[101];  /* prompt includes search string */
static char  search_string[100];
static int   search_pos = 0;      /* current location in search_string */
static int   search_forw_flg = 0; /* search direction flag */
static int   search_last = 0;	  /* last match found */

static void  
search_update(int c)
{
    if (c == 0) {
	search_pos = 0;
        search_string[0] = 0;
        search_prompt[0] = '?';
        search_prompt[1] = ' ';
        search_prompt[2] = 0;
    } else if (c > 0) {
        search_string[search_pos] = (char) c;
        search_string[search_pos+1] = (char) 0;
        search_prompt[search_pos] = (char) c;
        search_prompt[search_pos+1] = (char) '?';
        search_prompt[search_pos+2] = (char) ' ';
        search_prompt[search_pos+3] = (char) 0;
	search_pos++;
    } else {
	if (search_pos > 0) {
	    search_pos--;
            search_string[search_pos] = (char) 0;
            search_prompt[search_pos] = (char) '?';
            search_prompt[search_pos+1] = (char) ' ';
            search_prompt[search_pos+2] = (char) 0;
	} else {
	    gl_beep();
	    hist_pos = hist_last;
	}
    }
}

static void 
search_addchar(int c)
{
    char *loc;

    search_update(c);
    if (c < 0) {
	if (search_pos > 0) {
	    hist_pos = search_last;
	} else {
	    gl_buf[0] = 0;
	    hist_pos = hist_last;
	}
	strncpy(gl_buf, hist_buf[hist_pos],BUF_SIZE-2);
        gl_buf[BUF_SIZE-2] = '\0' ;
    }
    if ((loc = strstr(gl_buf, search_string)) != 0) {
	gl_fixup(search_prompt, 0, loc - gl_buf);
    } else if (search_pos > 0) {
        if (search_forw_flg) {
	    search_forw(0);
        } else {
	    search_back(0);
        }
    } else {
	gl_fixup(search_prompt, 0, 0);
    }
}

static void     
search_term(void)
{
    gl_search_mode = 0;
    if (gl_buf[0] == 0)		/* not found, reset hist list */
        hist_pos = hist_last;
    if (gl_in_hook)
	gl_in_hook(gl_buf);
    gl_fixup(gl_prompt, 0, gl_pos);
}

static void     
search_back(int new_search)
{
    int    found = 0;
    char  *p, *loc;

    search_forw_flg = 0;
    if (gl_search_mode == 0) {
	search_last = hist_pos = hist_last;	
	search_update(0);	
	gl_search_mode = 1;
        gl_buf[0] = 0;
	gl_fixup(search_prompt, 0, 0);
    } else if (search_pos > 0) {
	while (!found) {
	    p = gl_hist_prev();
	    if (*p == 0) {		/* not found, done looking */
	       gl_buf[0] = 0;
	       gl_fixup(search_prompt, 0, 0);
	       found = 1;
	    } else if ((loc = strstr(p, search_string)) != 0) {
	       strncpy(gl_buf, p, BUF_SIZE-2);
               gl_buf[BUF_SIZE-2] = '\0';
	       gl_fixup(search_prompt, 0, loc - p);
	       if (new_search)
		   search_last = hist_pos;
	       found = 1;
	    } 
	}
    } else {
        gl_beep();
    }
}

static void     
search_forw(int new_search)
{
    int    found = 0;
    char  *p, *loc;

    search_forw_flg = 1;
    if (gl_search_mode == 0) {
	search_last = hist_pos = hist_last;	
	search_update(0);	
	gl_search_mode = 1;
        gl_buf[0] = 0;
	gl_fixup(search_prompt, 0, 0);
    } else if (search_pos > 0) {
	while (!found) {
	    p = gl_hist_next();
	    if (*p == 0) {		/* not found, done looking */
	       gl_buf[0] = 0;
	       gl_fixup(search_prompt, 0, 0);
	       found = 1;
	    } else if ((loc = strstr(p, search_string)) != 0) {
	       strncpy(gl_buf, p, BUF_SIZE-2);
               gl_buf[BUF_SIZE-2] = '\0';
	       gl_fixup(search_prompt, 0, loc - p);
	       if (new_search)
		   search_last = hist_pos;
	       found = 1;
	    } 
	}
    } else {
        gl_beep();
    }
}

static void
gl_beep(void)
{
	if(gl_beep_on) MessageBeep(MB_OK);
}	/* gl_beep */

