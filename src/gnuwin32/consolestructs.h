/* xbuf */

typedef unsigned int xint;
typedef unsigned long xlong;

struct structXBUF {
    xint  ns, ms, shift;
    xlong dim, av;
    char *b, **s, *free;
    int  *user;
};

typedef struct structXBUF *xbuf;

/* console */

struct structConsoleData {
    int   kind;			/* console or pager */
    int   rows, cols;		/* dimension in char */
    int   w, h;			/* dimensions in pixel */

    font  f;			/* font */
    int   fw, fh;
    int   top, right;           /* borders */
    rgb   bg, fg, ufg;		/* colours */
    int   fv, fc;		/* first line and first char visible */
    int   r, c;			/* cursor position */
    int   sel, mx0, my0, mx1, my1;	/* selection */
    xbuf  lbuf;			/* lines buffer */
    int   firstkey, numkeys;	/* keys buffer */
    char *kbuf;
    int   already;              /* number of keys in buffer to be processed
				   before clipb. */
    char *clp;                  /* data from the clipboard */
    int  pclp;

    int   lazyupdate, needredraw, newfv, newfc;	/* updating and redrawing */
    bitmap bm;

    int   cur_pos, max_pos, prompt_len;	/* editing */
    xbuf  history;

    char  chbrk, modbrk;	/* hook for user's break */
    void  (*fbrk) ();

    menuitem mcopy, mpaste, mpopcopy, mpoppaste;
};

typedef struct structConsoleData *ConsoleData;
#define CONSOLE 1
#define PAGER 2
#define DATAEDITOR 3

#define BM  (p->bm)
#define ROWS (p->rows)
#define COLS (p->cols)
#define WIDTH (p->w)
#define HEIGHT (p->h)
#define BORDERX (p->right)
#define BORDERY (p->top)
#define FW (p->fw)
#define FH (p->fh)
#define FV (p->fv)
#define FC (p->fc)
#define NEWFV (p->newfv)
#define NEWFC (p->newfc)
#define NUMLINES (p->lbuf->ns)
#define LINE(i)  (p->lbuf->s[i])
#define USER(i)  (p->lbuf->user[i])
#define VLINE(i) ((strlen(LINE(i))>FC) ? &LINE(i)[FC] : "")
#define RLINE(i) (rect(0, BORDERY + (i)*FH, WIDTH, FH))
#define RMLINES(i,j) (rect(0, BORDERY + (i)*FH, WIDTH, (j-i+1)*FH))
#define cur_pos (p->cur_pos)
#define max_pos (p->max_pos)
#define prompt_len (p->prompt_len)
#define HISTORY(i) (p->history->s[p->history->ns - i - 1])
#define NHISTORY   (p->history->ns)

#define WRITELINE(i, j) writeline(p, i, j)

#define REDRAW drawconsole(c, getrect(c))

#define FBEGIN { \
                 ConsoleData p; \
                 p = getdata(c); \
               {

#define FEND(result) } return result;}
#define FVOIDEND }}
#define FVOIDRETURN { return; }
#define FRETURN(result) {return result;}

#define PBEGIN

#define PEND

#define RSHOW(r) {gbitblt(c, p->bm, topleft(r), r);}

ConsoleData newconsoledata(font f, int rows, int cols,
    rgb fg, rgb ufg, rgb bg, int kind);

void freeConsoleData(ConsoleData p);
void setfirstvisible(control c, int fv);
void setfirstcol(control c, int newcol);
void console_sbf(control c, int pos);
void console_mousedrag(control c, int button, point pt);
void console_mouserep(control c, int button, point pt);
void console_mousedown(control c, int button, point pt);
void consoleresize(console c, rect r);
void console_ctrlkeyin(control c, int key);
void console_normalkeyin(control c, int k);

font consolefn;
int fontsty, pointsize;
int consoler, consolec;
int pagerrow, pagercol;
rgb consolebg, consolefg, consoleuser, pagerhighlight;

#define DIMLBUF 64*1024         /* console buffer size in chars */
#define MLBUF   8*1024          /* console buffer size in lines */
#define SLBUF   512             /* console buffer shift in lines */
#define DIMHIST 16*1024         /* history buffer size in chars */
#define MHIST   512             /* history buffer size in lines */
#define SHIST   128             /* history buffer shift in lines */
#define NKEYS   512		/* 8Kb paste buffer */
#define TABSIZE 8

xbuf newxbuf(xlong dim, xint ms, xint shift);
void xbufdel(xbuf p);
void xbufaddc(xbuf p, char c);
    


