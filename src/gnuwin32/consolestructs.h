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
