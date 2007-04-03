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

#include       "getline.h"
static int      gl_tab();  /* forward reference needed for gl_tab_hook */

/********************* exported interface ********************************/

int             getline();		/* read a line of input */
void            gl_setwidth();		/* specify width of screen */
void            gl_histadd();		/* adds entries to hist */
void		gl_strwidth();		/* to bind gl_strlen */

int 		(*gl_in_hook)() = 0;
int 		(*gl_out_hook)() = 0;
int 		(*gl_tab_hook)() = gl_tab;

#define SUPPORT_MBCS
#include <R_ext/rlocale.h>
#include <wchar.h>
extern Rboolean mbcslocale;
#define mbs_init(x) memset(x, 0, sizeof(mbstate_t))

/******************** imported interface *********************************/

#include <string.h>
#include <ctype.h>
#include <errno.h>
#include <setjmp.h>
#ifdef __unix__
#include <signal.h>
#endif
#include <stdlib.h>
#include <stdio.h>
#include <io.h>

/*extern int     isatty();	
extern void    *malloc();
extern void    *calloc();
extern void     free();*/

/******************** internal interface *********************************/


static int      BUF_SIZE;               /* dimension of the buffer received*/
static int      gl_init_done = -1;	/* terminal mode flag  */
static int      gl_termw = 80;		/* actual terminal width */
static int      gl_scroll = 27;		/* width of EOL scrolling region */
static int      gl_width = 0;		/* net size available for input */
static int      gl_extent = 0;		/* how far to redraw, 0 means all */
static int      gl_overwrite = 0;	/* overwrite mode */
static int      gl_pos, gl_cnt = 0;     /* position and size of input */
static char    *gl_buf;                 /* input buffer */
static char    *gl_killbuf;             /* killed text */
static char    *gl_prompt;		/* to save the prompt string */
#ifdef POSIX
static char     gl_intrc = 0;		/* keyboard SIGINT char */
static char     gl_quitc = 0;		/* keyboard SIGQUIT char */
static char     gl_suspc = 0;		/* keyboard SIGTSTP char */
static char     gl_dsuspc = 0;		/* delayed SIGTSTP char */
#endif
static int      gl_search_mode = 0;	/* search mode flag */

static jmp_buf  gl_jmp;

static void     gl_init(void);		/* prepare to edit a line */
static void     gl_cleanup(void);	/* to undo gl_init */
static void     gl_char_init(void);	/* get ready for no echo input */
static void     gl_char_cleanup(void);	/* undo gl_char_init */
static size_t 	(*gl_strlen)() = (size_t(*)())strlen; 
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

/************************ nonportable part *********************************/

/*extern int      write();
 extern void     exit();*/

#ifdef unix
#ifndef __unix__
#define __unix__
#endif /* not __unix__ */
#endif /* unix */

#ifdef _IBMR2
#ifndef __unix__
#define __unix__
#endif
#endif

#ifdef __GO32__
#include <pc.h>
#undef MSDOS
#undef __unix__
#endif

#ifdef MSDOS
#include <bios.h>
#endif

#ifdef Win32
/* guido masarotto (3/12/98)*/
#define WIN32_LEAN_AND_MEAN 1
#include <windows.h>
static HANDLE Win32OutputStream, Win32InputStream = NULL;
static DWORD OldWin32Mode, AltIsDown;
#endif

#ifdef __unix__
#ifndef __convexc__
extern int      read();
extern int      kill();
extern int      ioctl();
#endif /* not __convexc__ */
#ifdef POSIX		/* use POSIX interface */
#include <termios.h>
struct termios  new_termios, old_termios;
#else /* not POSIX */
#include <sys/ioctl.h>
#ifdef M_XENIX	/* does not really use bsd terminal interface */
#undef TIOCSETN
#endif /* M_XENIX */
#ifdef TIOCSETN		/* use BSD interface */
#include <sgtty.h>
struct sgttyb   new_tty, old_tty;
struct tchars   tch;
struct ltchars  ltch;
#else			/* use SYSV interface */
#include <termio.h>
struct termio   new_termio, old_termio;
#endif /* TIOCSETN */
#endif /* POSIX */
#endif	/* __unix__ */

#ifdef vms
#include <descrip.h>
#include <ttdef.h>
#include <iodef.h>
#include unixio
   
static int   setbuff[2];             /* buffer to set terminal attributes */
static short chan = -1;              /* channel to terminal */
struct dsc$descriptor_s descrip;     /* VMS descriptor */
#endif

static void
gl_char_init()			/* turn off input echo */
{
#ifdef __unix__
#ifdef POSIX
    tcgetattr(0, &old_termios);
    gl_intrc = old_termios.c_cc[VINTR];
    gl_quitc = old_termios.c_cc[VQUIT];
#ifdef VSUSP
    gl_suspc = old_termios.c_cc[VSUSP];
#endif
#ifdef VDSUSP
    gl_dsuspc = old_termios.c_cc[VDSUSP];
#endif
    new_termios = old_termios;
    new_termios.c_iflag &= ~(BRKINT|ISTRIP|IXON|IXOFF);
    new_termios.c_iflag |= (IGNBRK|IGNPAR);
    new_termios.c_lflag &= ~(ICANON|ISIG|IEXTEN|ECHO);
    new_termios.c_cc[VMIN] = 1;
    new_termios.c_cc[VTIME] = 0;
    tcsetattr(0, TCSANOW, &new_termios);
#else				/* not POSIX */
#ifdef TIOCSETN			/* BSD */
    ioctl(0, TIOCGETC, &tch);
    ioctl(0, TIOCGLTC, &ltch);
    gl_intrc = tch.t_intrc;
    gl_quitc = tch.t_quitc;
    gl_suspc = ltch.t_suspc;
    gl_dsuspc = ltch.t_dsuspc;
    ioctl(0, TIOCGETP, &old_tty);
    new_tty = old_tty;
    new_tty.sg_flags |= RAW;
    new_tty.sg_flags &= ~ECHO;
    ioctl(0, TIOCSETN, &new_tty);
#else				/* SYSV */
    ioctl(0, TCGETA, &old_termio);
    gl_intrc = old_termio.c_cc[VINTR];
    gl_quitc = old_termio.c_cc[VQUIT];
    new_termio = old_termio;
    new_termio.c_iflag &= ~(BRKINT|ISTRIP|IXON|IXOFF);
    new_termio.c_iflag |= (IGNBRK|IGNPAR);
    new_termio.c_lflag &= ~(ICANON|ISIG|ECHO);
    new_termio.c_cc[VMIN] = 1;
    new_termio.c_cc[VTIME] = 0;
    ioctl(0, TCSETA, &new_termio);
#endif /* TIOCSETN */
#endif /* POSIX */
#endif /* __unix__ */

#ifdef vms
    descrip.dsc$w_length  = strlen("tt:");
    descrip.dsc$b_dtype   = DSC$K_DTYPE_T;
    descrip.dsc$b_class   = DSC$K_CLASS_S;
    descrip.dsc$a_pointer = "tt:";
    (void)sys$assign(&descrip,&chan,0,0);
    (void)sys$qiow(0,chan,IO$_SENSEMODE,0,0,0,setbuff,8,0,0,0,0);
    setbuff[1] |= TT$M_NOECHO;
    (void)sys$qiow(0,chan,IO$_SETMODE,0,0,0,setbuff,8,0,0,0,0);
#endif /* vms */

#ifdef Win32
/* guido masarotto (3/12/98)*/   
   if (!Win32InputStream) {
       Win32InputStream = GetStdHandle(STD_INPUT_HANDLE);
       Win32OutputStream = GetStdHandle(STD_OUTPUT_HANDLE);	
   }
   GetConsoleMode(Win32InputStream,&OldWin32Mode);
   SetConsoleMode(Win32InputStream, ENABLE_PROCESSED_INPUT); /* So ^C works */
   AltIsDown = 0;
#endif      
   
}

static void
gl_char_cleanup(void)		/* undo effects of gl_char_init */
{
#ifdef __unix__
#ifdef POSIX 
    tcsetattr(0, TCSANOW, &old_termios);
#else 			/* not POSIX */
#ifdef TIOCSETN		/* BSD */
    ioctl(0, TIOCSETN, &old_tty);
#else			/* SYSV */
    ioctl(0, TCSETA, &old_termio);
#endif /* TIOCSETN */
#endif /* POSIX */
#endif /* __unix__ */

#ifdef vms
    setbuff[1] &= ~TT$M_NOECHO;
    (void)sys$qiow(0,chan,IO$_SETMODE,0,0,0,setbuff,8,0,0,0,0);
    sys$dassgn(chan);
    chan = -1;
#endif
   
#ifdef Win32
/* guido masarotto (3/12/98)*/
   SetConsoleMode(Win32InputStream,OldWin32Mode);
   AltIsDown = 0;
#endif         
}

#if MSDOS || __EMX__ || __GO32__
int pc_keymap(c)
int c;
{
    switch (c) {
    case 72: c = 16;   /* up -> ^P */
        break;
    case 80: c = 14;   /* down -> ^N */
        break;
    case 75: c = 2;    /* left -> ^B */
        break;
    case 77: c = 6;    /* right -> ^F */
        break;
    default: c = 0;    /* make it garbage */
    }
    return c;
}
#endif /* MSDOS || __EMX__ || __GO32__ */

static int
gl_getc(void)
/* get a character without echoing it to screen */
{
    int             c;
#ifdef __unix__
    char            ch;
#endif

#ifdef __unix__
    while ((c = read(0, &ch, 1)) == -1) {
	if (errno != EINTR)
	    break;
    }
    c = (ch <= 0)? -1 : ch;
#endif	/* __unix__ */
#ifdef MSDOS
    c = _bios_keybrd(_NKEYBRD_READ);
#endif  /* MSDOS */
#ifdef __GO32__
    c = getkey () ;
    if (c > 255) c = pc_keymap(c & 0377);
#endif /* __GO32__ */
#ifdef __TURBOC__
    while(!bioskey(1))
	;
    c = bioskey(0);
#endif
#if MSDOS || __TURBOC__
    if ((c & 0377) == 224) {
	c = pc_keymap((c >> 8) & 0377);
    } else {
	c &= 0377;
    }
#endif /* MSDOS || __TURBOC__ */
#ifdef __EMX__
    c = _read_kbd(0, 1, 0);
    if (c == 224 || c == 0) {
        c = pc_keymap(_read_kbd(0, 1, 0));
    } else {
        c &= 0377;
    }
#endif
#ifdef vms
    if(chan < 0) {
       c='\0';
    }
    (void)sys$qiow(0,chan,IO$_TTYREADALL,0,0,0,&c,1,0,0,0,0);
    c &= 0177;                        /* get a char */
#endif

#ifdef Win32
/* guido masarotto (3/12/98)
 * get Ansi char code from a Win32 console
*/
{   DWORD a;
    INPUT_RECORD r;
    DWORD st;
    WORD vk;
    CONSOLE_SCREEN_BUFFER_INFO csb;
    int bbb = 0, nAlt=0, n;

    c = 0; 
    while (!c) {
      /* 
	   Following two lines seem to be needed under Win2k 
	   to reshow the cursor 
      */
      GetConsoleScreenBufferInfo(Win32OutputStream, &csb);
      SetConsoleCursorPosition(Win32OutputStream, csb.dwCursorPosition);
      ReadConsoleInput(Win32InputStream, &r, 1, &a);
      if (!(r.EventType == KEY_EVENT)) break;
      st = r.Event.KeyEvent.dwControlKeyState;
      vk = r.Event.KeyEvent.wVirtualKeyCode;
      if (r.Event.KeyEvent.bKeyDown) {
	if (vk == VK_MENU && (st & LEFT_ALT_PRESSED)) { /* VK_MENU is
							   Alt or AltGr */
	  AltIsDown = 1;
	  nAlt = 0;
	  bbb  = 0;
	} 
	else if (st & ENHANCED_KEY) { 
	  switch(vk) {
	  case VK_LEFT: c=2 ;break;
	  case VK_RIGHT: c=6;break;
	  case VK_HOME:  c='\001';break;
	  case VK_END: c='\005';break;
	  case VK_UP:  c=16;break;
	  case VK_DOWN: c=14;break;		
	  case VK_DELETE:  c='\004';break;
	  }
	} 
	else if (AltIsDown) { /* Interpret Alt+xxx entries */
	  switch (vk) {
	  case VK_INSERT: n = 0; break;
	  case VK_END: n = 1; break;
	  case VK_DOWN: n = 2; break;
	  case VK_NEXT: n = 3;break;
	  case VK_LEFT: n = 4; break;
	  case VK_CLEAR:  n = 5; break;
	  case VK_RIGHT: n = 6; break;
	  case VK_HOME: n = 7; break;
	  case VK_UP: n = 8; break;
	  case VK_PRIOR: n = 9; break;	 
	  default: n = -1;
	  }
	  if (n >= 0) bbb = 10 * bbb + n;
	  nAlt += 1;
	  if (nAlt==3) { 
	    c = (bbb < 256) && (bbb > 0) ? bbb : 0;
	    bbb = 0;
	    nAlt = 0;
	  } 
	} 
	else 
	  c = r.Event.KeyEvent.uChar.AsciiChar;
      }
      else if (vk == VK_MENU && AltIsDown) { 
           /* Alt key up event: could be AltGr, but let's hope users 
	      only press one of them at a time. */
	AltIsDown = 0;
	c = (bbb < 256) && (bbb > 0) ? bbb : 0;
	bbb = 0;
	nAlt = 0;
      }
      if ((c < -127) || (c > 255)) c = 0; 
      if (c < 0) c = 256 + c;    
    }
}
#endif         
    return c;
}

static void
gl_putc(int c)
{
#ifdef Win32
/* guido masarotto (3/12/98)*/   
   int ch = c;
#else   
   char   ch = c;
#endif    
    write(1, &ch, 1);
    if (ch == '\n') {
	ch = '\r';
        write(1, &ch, 1);	/* RAW mode needs '\r', does not hurt */
    }
}

/******************** fairly portable part *********************************/

static void
gl_puts(const char *const buf)
{
    int len; 
    
    if (buf) {
        len = strlen(buf);
        write(1, buf, len);
    }
}

static void
gl_error(const char *const buf)
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
    if (gl_init_done < 0) {		/* -1 only on startup */
        gl_hist_init(512, 1);
    }
    if (isatty(0) == 0 || isatty(1) == 0)
	gl_error("\n*** Error: getline(): not interactive, use stdio.\n");
    if (!(gl_killbuf=calloc(BUF_SIZE,sizeof(char))))
        gl_error("\n*** Error: getline(): no enough memory.\n");
    gl_char_init();
    gl_init_done = 1;
}

static void
gl_cleanup(void)
/* undo effects of gl_init, as necessary */
{
    if (gl_init_done > 0)
        gl_char_cleanup();
    free(gl_killbuf);
    gl_init_done = 0;
}

void
gl_setwidth(int w)
{
    if (w > 20) {
	gl_termw = w;
	gl_scroll = w / 3;
    } else {
	gl_error("\n*** Error: minimum screen width is 21\n");
    }
}

int
getline(char *prompt, char *buf, int buflen)
{
    int             c, loc, tmp;
    int mb_len;
    mbstate_t mb_st;
    int i;
    wchar_t wc;
#ifdef __unix__
    int	            sig;
#endif
    BUF_SIZE = buflen;
    gl_buf = buf;
    gl_buf[0] = '\0';
    if (setjmp(gl_jmp)) {
       gl_newline();
       gl_cleanup(); 
       return 0;
    }
    gl_init();	
    gl_pos = 0;
    gl_prompt = (prompt)? prompt : "";
    if (gl_in_hook)
	gl_in_hook(gl_buf);
    gl_fixup(gl_prompt, -2, BUF_SIZE);
    while ((c = gl_getc()) >= 0) {
	gl_extent = 0;  	/* reset to full extent */
	if (!iscntrl(c)) {
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
		/*NOTREACHED*/
		break; 
	      case '\001': gl_fixup(gl_prompt, -1, 0);		/* ^A */
		break;
	      case '\002': 	/* ^B */
		if(mbcslocale) {
		    mb_len = 0;
		    mbs_init(&mb_st);
		    for(i = 0; i< gl_pos ;) {
			mbrtowc(&wc, gl_buf+i, MB_CUR_MAX, &mb_st);
			mb_len = Ri18n_wcwidth(wc);
			i += (wc==0) ? 0 : mb_len;
		    }
		    gl_fixup(gl_prompt, -1, gl_pos - mb_len);
		} else
		    gl_fixup(gl_prompt, -1, gl_pos-1);
		break;
	      case '\003':                                      /* ^C */
		  gl_fixup(gl_prompt, -1, gl_cnt);
		  gl_puts("^C\n");
		  gl_kill(0);
		  gl_fixup(gl_prompt, -2, BUF_SIZE);
		break;
	      case '\004':					/* ^D */
		if (gl_cnt == 0) {
		    gl_buf[0] = 0;
		    gl_cleanup();
		    gl_putc('\n');
		    return 0;
		} else {
		    gl_del(0);
		}
		break;
	      case '\005': gl_fixup(gl_prompt, -1, gl_cnt);	/* ^E */
		break;
		case '\006': /* ^F */
		  if(mbcslocale) { 
		      if(gl_pos >= gl_cnt)break;
		      mb_len = 0;
		      mbs_init(&mb_st);
		      for(i = 0; i<= gl_pos ;){
			  mbrtowc(&wc, gl_buf+i, MB_CUR_MAX, &mb_st);
			  mb_len = Ri18n_wcwidth(wc);
			  i += (wc==0) ? 0 : mb_len;
		      }
		      gl_fixup(gl_prompt, -1, gl_pos + mb_len);
		  }
		else
		  gl_fixup(gl_prompt, -1, gl_pos+1);
		break;
	      case '\010': case '\177': gl_del(-1);	/* ^H and DEL */
		break;
	      case '\t':        				/* TAB */
                if (gl_tab_hook) {
		    tmp = gl_pos;
	            loc = gl_tab_hook(gl_buf, gl_strlen(gl_prompt), &tmp);
	            if (loc != -1 || tmp != gl_pos)
	                gl_fixup(gl_prompt, loc, tmp);
                }
		break;
	      case '\013': gl_kill(gl_pos);			/* ^K */
		break;
	      case '\014': gl_redraw();				/* ^L */
		break;
	      case '\016': 					/* ^N */
		strncpy(gl_buf, gl_hist_next(), BUF_SIZE-2);
		gl_buf[BUF_SIZE-2] = '\0';
                if (gl_in_hook)
	            gl_in_hook(gl_buf);
		gl_fixup(gl_prompt, 0, BUF_SIZE);
		break;
	      case '\017': gl_overwrite = !gl_overwrite;       	/* ^O */
		break;
	      case '\020': 					/* ^P */
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
		/*NOTREACHED*/
		break;
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
		    case 'C': /* right */
			if(mbcslocale) { 
			    mb_len = 0;
			    mbs_init(&mb_st);
			    for(i = 0; i<= gl_pos ;){
				mbrtowc(&wc, gl_buf+i, MB_CUR_MAX, &mb_st);
				mb_len = Ri18n_wcwidth(wc);
				i += (wc==0) ? 0 : mb_len;
			    }
			    gl_fixup(gl_prompt, -1, gl_pos + mb_len);
			} else
			    gl_fixup(gl_prompt, -1, gl_pos+1);
		        break;
		    case 'D': /* left */
		       if(mbcslocale) {
			   mb_len = 0;
			   mbs_init(&mb_st);
			   for(i = 0; i <= gl_pos ;){
			       mbrtowc(&wc, gl_buf+i, MB_CUR_MAX, &mb_st);
			       mb_len = Ri18n_wcwidth(wc);
			       i += (wc==0) ? 0 :mb_len;
			   }
			   gl_fixup(gl_prompt, -1, gl_pos - mb_len);
		       } else
			 gl_fixup(gl_prompt, -1, gl_pos-1);
			break;
		      default: gl_putc('\007');         /* who knows */
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
#ifdef __unix__
	        if (c > 0) {	/* ignore 0 (reset above) */
	            sig = 0;
#ifdef SIGINT
	            if (c == gl_intrc)
	                sig = SIGINT;
#endif
#ifdef SIGQUIT
	            if (c == gl_quitc)
	                sig = SIGQUIT;
#endif
#ifdef SIGTSTP
	            if (c == gl_suspc || c == gl_dsuspc)
	                sig = SIGTSTP;
#endif
                    if (sig != 0) {
	                gl_cleanup();
	                kill(0, sig);
	                gl_init();
	                gl_redraw();
			c = 0;
		    } 
		}
#endif /* __unix__ */
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

static void
gl_addchar(int c)
      
/* adds the character c to the input buffer at current location */
{
    int  i;

    if (gl_cnt >= BUF_SIZE - 2) {
            gl_putc('\a');
            return; 
    }
    if(mbcslocale) {
	mbstate_t mb_st;
	wchar_t wc;
	char s[9];
	int res;
	int clen ;
      
	s[0] = c;
	clen = 1;
	res = 0;
	if((unsigned int)c >= (unsigned int)0x80) {
            while(clen <= MB_CUR_MAX) {
	        mbs_init(&mb_st);
	        res = mbrtowc(&wc, s, clen, &mb_st);
	        if(res >= 0) break;
	        if(res == -1) 
		    gl_error("invalid multibyte character in mbcs_get_next");
  	        /* so res == -2 */
	        c = gl_getc();
	        if(c == EOF) 
		    gl_error("EOF whilst reading MBCS char");
	        s[clen++] = c;
	    } /* we've tried enough, so must be complete or invalid by now */
	}
	if( res >= 0 ) {
	    if (!(gl_overwrite == 0 || gl_pos == gl_cnt))  
		gl_del(0); 
	    for (i=gl_cnt; i >= gl_pos; i--)
                gl_buf[i+clen] = gl_buf[i];
	    for (i=0; i<clen; i++)
                gl_buf[gl_pos + i] = s[i];
	    gl_fixup(gl_prompt, gl_pos, gl_pos+clen);
	}
       
    } else
	if (gl_overwrite == 0 || gl_pos == gl_cnt) {
	    for (i = gl_cnt; i >= gl_pos; i--)
		gl_buf[i+1] = gl_buf[i];
	    gl_buf[gl_pos] = (char) c;
	    gl_fixup(gl_prompt, gl_pos, gl_pos+1);
	} else {
	    gl_buf[gl_pos] = (char) c;
	    gl_extent = 1;
	    gl_fixup(gl_prompt, gl_pos, gl_pos+1);
	}
}

static void
gl_yank(void)
/* adds the kill buffer to the input buffer at current location */
{
    int  i, len;

    len = strlen(gl_killbuf);
    if (len > 0) {
	if (gl_overwrite == 0) {
            if (gl_cnt + len >= BUF_SIZE - 1) 
	        gl_error("\n*** Error: getline(): input buffer overflow\n");
            for (i=gl_cnt; i >= gl_pos; i--)
                gl_buf[i+len] = gl_buf[i];
	    for (i=0; i < len; i++)
                gl_buf[gl_pos+i] = gl_killbuf[i];
            gl_fixup(gl_prompt, gl_pos, gl_pos+len);
	} else {
	    if (gl_pos + len > gl_cnt) {
                if (gl_pos + len >= BUF_SIZE - 1) 
	            gl_error("\n*** Error: getline(): input buffer overflow\n");
		gl_buf[gl_pos + len] = 0;
            }
	    for (i=0; i < len; i++)
                gl_buf[gl_pos+i] = gl_killbuf[i];
	    gl_extent = len;
            gl_fixup(gl_prompt, gl_pos, gl_pos+len);
	}
    } else
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
    int loc = gl_width - 5;	/* shifts line back to start position */
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
   int i;

   if(mbcslocale) {
       int mb_len;
       mbstate_t mb_st;
       wchar_t wc;

       mb_len=0;
       mbs_init(&mb_st);
   
       if ((loc == -1 && gl_pos > 0) || (loc == 0 && gl_pos < gl_cnt)) {
	   for(i = 0; i<= gl_pos + loc;) {
	       mbrtowc(&wc,gl_buf+i, MB_CUR_MAX, &mb_st);
	       mb_len = Ri18n_wcwidth(wc);
	       i += (wc==0) ? 0 : mb_len;
	   }
	   for (i = gl_pos+(loc*mb_len); i <= gl_cnt - mb_len; i++)
	       gl_buf[i] = gl_buf[i + mb_len];
	   gl_fixup(gl_prompt,gl_pos+(loc * mb_len) , gl_pos+(loc * mb_len));
       } else
	   gl_beep();
   } else   
       if ((loc == -1 && gl_pos > 0) || (loc == 0 && gl_pos < gl_cnt)) {
	   for (i = gl_pos+loc; i < gl_cnt; i++)
	       gl_buf[i] = gl_buf[i+1];
	   gl_fixup(gl_prompt, gl_pos+loc, gl_pos+loc);
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
 *            a new line, redraw everything.
 *   cursor : the desired location of the cursor after the call.
 *            A value of BUF_SIZE can be used  to indicate the cursor should
 *            move just past the end of the input line.
 */
{
    static int   gl_shift;	/* index of first on screen character */
    static int   off_right;	/* true if more text right of screen */
    static int   off_left;	/* true if more text left of screen */
    static char  last_prompt[80] = "";
    int          left = 0, right = -1;		/* bounds for redraw */
    int          pad;		/* how much to erase at end of line */
    int          backup;        /* how far to backup before fixing */
    int          new_shift;     /* value of shift based on cursor */
    int          extra;         /* adjusts when shift (scroll) happens */
    int          i;
    int          new_right = -1; /* alternate right bound, using gl_extent */
    int          l1, l2, ll;

    if (change == -2) {   /* reset */
	while (gl_pos--) gl_putc('\b');
        gl_pos = gl_cnt = gl_shift = off_right = off_left = 0;
	gl_puts(prompt);
	strcpy(last_prompt, prompt);
	change = 0;
        gl_width = gl_termw - gl_strlen(prompt);
    } else if (strcmp(prompt, last_prompt) != 0) {
	l1 = gl_strlen(last_prompt);
	l2 = gl_strlen(prompt);
        ll = gl_pos + l1;
	gl_cnt = gl_cnt + l1 - l2;
	strcpy(last_prompt, prompt);
	while (ll--) gl_putc('\b');	
	gl_puts(prompt);
	gl_pos = gl_shift;
        gl_width = gl_termw - l2;
	change = 0;
    }
    pad = (off_right)? gl_width - 1 : gl_cnt - gl_shift;   /* old length */
    backup = gl_pos - gl_shift;
    if (change >= 0) {
        gl_cnt = strlen(gl_buf);
        if (change > gl_cnt)
	    change = gl_cnt;
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
    if (off_right || (off_left && cursor < gl_shift + gl_width - gl_scroll / 2))
	extra = 2;			/* shift the scrolling boundary */
    else 
	extra = 0;
    new_shift = cursor + extra + gl_scroll - gl_width;
    if (new_shift > 0) {
	new_shift /= gl_scroll;
	new_shift *= gl_scroll;
    } else
	new_shift = 0;
    if (new_shift != gl_shift) {	/* scroll occurs */
	gl_shift = new_shift;
	off_left = (gl_shift)? 1 : 0;
	off_right = (gl_cnt > gl_shift + gl_width - 1)? 1 : 0;
        left = gl_shift;
	new_right = right = (off_right)? gl_shift + gl_width - 2 : gl_cnt;
    } else if (change >= 0) {		/* no scroll, but text changed */
	if (change < gl_shift + off_left) {
	    left = gl_shift;
	} else {
	    left = change;
	    backup = gl_pos - change;
	}
	off_right = (gl_cnt > gl_shift + gl_width - 1)? 1 : 0;
	right = (off_right)? gl_shift + gl_width - 2 : gl_cnt;
	new_right = (gl_extent && (right > left + gl_extent))? 
	             left + gl_extent : right;
    }
    pad -= (off_right)? gl_width - 1 : gl_cnt - gl_shift;
    pad = (pad < 0)? 0 : pad;
    if (left <= right) {		/* clean up screen */
	for (i=0; i < backup; i++)
	    gl_putc('\b');
	if (left == gl_shift && off_left) {
	    gl_putc('$');
	    left++;
        }
	for (i=left; i < new_right; i++)
	    gl_putc(gl_buf[i]);
	gl_pos = new_right;
	if (off_right && new_right == right) {
	    gl_putc('$');
	    gl_pos++;
	} else { 
	    for (i=0; i < pad; i++)	/* erase remains of prev line */
		gl_putc(' ');
	    gl_pos += pad;
	}
    }
    i = gl_pos - cursor;		/* move to final cursor location */
    if (i > 0) {
	while (i--)
	   gl_putc('\b');
    } else {
	for (i=gl_pos; i < cursor; i++)
	    gl_putc(gl_buf[i]);
    }
    gl_pos = cursor;
}

static int
gl_tab(char *buf, int offset, int *loc)
/* default tab handler, acts like tabstops every 8 cols */
{
    int i, count, len;

    len = strlen(buf);
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

void gl_strwidth(func)
size_t (*func)();
{
    if (func != 0) {
	gl_strlen = func;
    }
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
gl_histadd(char *buf)
{
    static char *prev = 0;
    char *p = buf;

    /* in case we call gl_histadd() before we call getline() */
    if (gl_init_done < 0) {		/* -1 only on startup */
        gl_hist_init(512, 1);
        gl_init_done = 0;
    }
    while (*p == ' ' || *p == '\t' || *p == '\n') 
	p++;
    if (*p) {
	hist_buf[hist_last] = hist_save(buf);
	prev = hist_buf[hist_last];
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
hist_save(char *p)
        
/* makes a copy of the string */
{
    char *s = 0;
    int   len = strlen(p);
    char *nl = strchr(p, '\n');

    if (nl) {
        if ((s = (char *) malloc(len)) != 0) {
            strncpy(s, p, len-1);
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

void gl_savehistory(char *file, int size)
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

void gl_loadhistory(char *file)
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
#ifdef Win32
	if(gl_beep_on) MessageBeep(MB_OK);
#else
	gl_putc('\007');
#endif
}	/* gl_beep */
