/*
 *  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (C) 1995  Robert Gentleman and Ross Ihaka
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
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include "winde.h"

static int PUPcol, windowWidth, windowHeight;
static int SeenChar; /* determine whether we need to handle a character */
static POINT DEMousePos;
static BOOL  ShiftState, ControlState;
BOOL CALLBACK DEVar(HWND, UINT, WPARAM, LPARAM);
RECT DERect;
HWND RDEWnd;
HMENU RMenuDE, RMenuDEWin;

static void activaterect(int);
/* 
   The spreadsheet function returns a list of vectors. The types of these
   vectors can be specified by the user as can their names. It the names
   are specified they are set during initialization. The user can change
   these via a menu interface, they can also change the type.

   The vectors are created too long and if they need to be increased this
   is done by using the next higher power of 2. They start 100 long. To cut
   them to the correct length for return you need to know the largest row number
   that was assigned to. LEVELS (sxpinfo.gp) is used to keep track of this, 
   separately for each vector. Vectors are initialized to NA when they are
   created so that NA is returned for any cell that was not set by the user.
   So that coercion back and forth maintains values of ssNA_REAL and ssNA_STRING
   I have set ssNA_STRING to be coerceVector(ssNA_REAL), very weird but easy.

        At some point the text drawing, line drawing etc should simply call
        the appropriate graphics functions rather than have all this duplicate
        code.

 */


/* 
   ssNewVector is just an interface to allocVector but it lets us
   set the fields to NA. We need to have a special NA for reals and
   strings so that we can differentiate between uninitialized elements
   in the vectors and user supplied NA's; hence ssNA_REAL and ssNA_STRING
 */

SEXP ssNewVector(SEXPTYPE type, int vlen)
{
        SEXP tvec;
        int j;

        tvec = allocVector(type, vlen);
        for (j = 0; j < vlen; j++)
                if (type == REALSXP)
                        REAL(tvec)[j] = ssNA_REAL;
                else if (type == STRSXP)
                        STRING(tvec)[j] = STRING(ssNA_STRING)[0];
        LEVELS(tvec) = 0;
        return (tvec);
}

SEXP do_dataentry(SEXP call, SEXP op, SEXP args, SEXP rho)
{
        SEXP  tvec2, tvec, colmodes, indata;
        SEXPTYPE type;
        int i, j, len;
        RCNTXT cntxt;

        indata = CAR(args);
        colmodes = CADR(args);

        if (!isList(indata) || !isList(colmodes))
                errorcall(call, "invalid argument\n");

        /* initialize the constants */

        bufp = buf;
        ne = 0;
        currentexp = 0;
        nneg = 0;
        ndecimal = 0;
        clength = 0;
        ccol = 1;
        crow = 1;
        colmin = 1;
        rowmin = 1;
        ssNA_REAL = -NA_REAL;
        tvec = allocVector(REALSXP, 1);
        REAL(tvec)[0] = ssNA_REAL;
        PROTECT(ssNA_STRING = coerceVector(tvec, STRSXP));

        /* program control constants */
        ShiftState=0;
        ControlState=0;
        doneSpread=1;
        SeenChar = 0;

        /* setup inputlist  */

        if (indata != R_NilValue) {
                PROTECT(inputlist = duplicate(indata));
                tvec2 = colmodes;
                for (tvec = inputlist; tvec != R_NilValue; tvec = CDR(tvec)) {
                        type = TYPEOF(CAR(tvec));
                        if (CAR(tvec2) != R_NilValue)
                                type = str2type(CHAR(STRING(CAR(tvec2))[0]));
                        if (type != STRSXP)
                                type = REALSXP;
                        if (CAR(tvec) == R_NilValue) {
                                if (type == NILSXP)
                                        type = REALSXP;
                                CAR(tvec) = ssNewVector(type, 100);
                                TAG(tvec) = install("var1");
                                LEVELS(CAR(tvec)) = 0;
                        }
                        else if (!isVector(CAR(tvec)))
                                errorcall(call, "invalid type for value \n");
                        else {
                                if (TYPEOF(CAR(tvec)) != type)
                                        CAR(tvec) = coerceVector(CAR(tvec), type);
                                LEVELS(CAR(tvec)) = LENGTH(CAR(tvec));
                        }
                        tvec2 = CDR(tvec2);
                }
        }
        else {
                errorcall(call, "invalid parameter \n");
        }


        /* start up the window, more initializing in here */
        if (!DEInit())
                errorcall(call, "wrong device\n");

        /* set up a context which will close the window if there is an error */
        begincontext(&cntxt, 3, R_NilValue, R_NilValue, R_NilValue, R_NilValue);
        cntxt.cend = &CloseDE;

        activaterect(1);

        while (doneSpread)
                EventLoop();

        endcontext(&cntxt);
        CloseDE();

        /* drop out unused columns */
        i = 0;
        for (tvec = inputlist; tvec != R_NilValue; tvec = CDR(tvec))
                if (CAR(tvec) == R_NilValue) {
                        if (i == 0)
                                inputlist = CDR(inputlist);
                        else {
                                tvec2 = nthcdr(inputlist, (i - 1));
                                SETCDR(tvec2, CDR(tvec));
                        }
                }
                else
                        i++;
        UNPROTECT(1);
        PROTECT(inputlist);  /* just in case we got rid of the first column */
        for (tvec = inputlist; tvec != R_NilValue; tvec = CDR(tvec)) {
                len = LEVELS(CAR(tvec));
                if (LENGTH(CAR(tvec)) != len) {
                        tvec2 = ssNewVector(TYPEOF(CAR(tvec)), len);
                        for (j = 0; j < len; j++)
                                if (TYPEOF(CAR(tvec)) == REALSXP)
                                        if (REAL(CAR(tvec))[j] != ssNA_REAL)
                                                REAL(tvec2)[j] = REAL(CAR(tvec))[j];
                                        else
                                                REAL(tvec2)[j] = NA_REAL;
                                else if (TYPEOF(CAR(tvec)) == STRSXP)
                                        if (!streql(CHAR(STRING(CAR(tvec))[j]), CHAR(STRING(ssNA_STRING)[0])))
                                                STRING(tvec2)[j] = STRING(CAR(tvec))[j];
                                        else
                                                STRING(tvec2)[j] = NA_STRING;
                                else
                                        error("spreadsheet: internal memory problem");
                        CAR(tvec) = tvec2;
                }
        }

        UNPROTECT(2);
        return inputlist;
}


static void doDEKey(WPARAM wParam)
{
        switch(wParam) {
                case VK_RETURN:
                case VK_DOWN:
                        advancerect(DOWN);
                        break;
                case VK_LEFT:
                        advancerect(LEFT);
                        break;
                case VK_RIGHT: 
                case VK_TAB:
                        advancerect(RIGHT);
                        break;
                case VK_UP:
                        advancerect(UP);
                        break;
                case VK_BACK: 
                case VK_DELETE: 
                        if (clength > 0) {
                                buf[clength - 1] = ' ';
                                printstring(buf, clength, crow, ccol);
                                clength--;
                                bufp--;
                        }
                        else
                                SysBeep();
                        break;  
                case VK_HOME:
                        ccol=1;
                        crow=1;
                        jumpwin(1, 1);
                        break;
                case VK_SHIFT:
                        ShiftState=1;
                        break;
                case VK_CONTROL:
                        ControlState=1;
                        break;
        }
}


/* Window Drawing Routines */

void drawwindow()
{
        int i;

       closerect();
       clearwindow(); 

       for (i = 1; i <= nwide; i++)
              drawline(i * box_w, 0, i * box_w, windowHeight);
       for (i = 1; i <= nhigh; i++)
              drawline(0, i * box_h, windowWidth, i * box_h);
       colmax = colmin + (nwide - 2);  /* so row 0 and col 0 are reserved for labels */
       rowmax = rowmin + (nhigh - 2);
       printlabs();
       if (inputlist != R_NilValue)
             for (i = 1; i <= nwide; i++)
                   drawcol(i);
       activaterect(1);
}

/* 
   draw the window with the top left box at column wcol and 
   row wrow 
 */

void jumpwin(int wcol, int wrow)
{
        if (wcol < 0 || wrow < 0) {
                SysBeep();
                return;
        }
        closerect();
        colmin = wcol;
        rowmin = wrow;
        drawwindow();
}



void advancerect(int which)
{

        /* if we are in the header, changing a name then only down is allowed */
        if (crow < 1 && which != DOWN) {
                SysBeep();
                return;
        }

        closerect();

        switch (which) {
        case UP:
                if (crow == 1)
                        if (rowmin == 1)
                                SysBeep();
                        else
                                jumppage(UP);
                else
                        crow--;
                break;
        case DOWN:
                if (crow >= (nhigh - 1))
                        jumppage(DOWN);
                else
                        crow++;
                break;
        case RIGHT:
                if (ccol >= (nwide - 1))
                        jumppage(RIGHT);
                else
                        ccol++;
                break;
        case LEFT:
                if (ccol == 1)
                        if (colmin == 1)
                                SysBeep();
                        else
                                jumppage(LEFT);
                else
                        ccol--;
                break;
        default:
                abort();
        }

       activaterect(1);
}


/*
   printelt: print the correct value from vector[vrow] into the
   spread sheet in row ssrow and col sscol
 */
void printelt(SEXP invec, int vrow, int ssrow, int sscol)
{
        char *pp;

        if (TYPEOF(invec) == REALSXP) {
                if (REAL(invec)[vrow] != ssNA_REAL) {
                        pp=EncodeElement(invec, vrow, 0);
                        printstring(pp, strlen(pp), ssrow, sscol);
                }
        }
        else if (TYPEOF(invec) == STRSXP) {
                if (!streql(CHAR(STRING(invec)[vrow]), CHAR(STRING(ssNA_STRING)[0]))) {
                        pp=EncodeElement(invec, vrow, 0);
                        printstring(pp, strlen(pp), ssrow, sscol);
                }
        }
        else
                error("spreadsheet: internal memory error\n");
}

void drawcol(int whichcol)
{
        int i, len;
        SEXP tmp;

        if (length(inputlist) >= whichcol + colmin - 1) {
                tmp = nthcdr(inputlist, whichcol + colmin - 2);
                if (CAR(tmp) != R_NilValue) {
                        len = (LEVELS(CAR(tmp)) > rowmax) ? rowmax : LEVELS(CAR(tmp));
                        for (i = (rowmin - 1); i < len; i++)
                                printelt(CAR(tmp), i, i - rowmin + 2, whichcol);
                }
        }
}

void jumppage(int dir)
{
        switch (dir) {
        case UP:
                if( rowmin <= 1 ) {
                   SysBeep();
                   break;
                }
                rowmin--;
                rowmax--;
                jumpwin(colmin, rowmin);
                break;
        case DOWN:
                rowmin++;
                rowmax++;
                jumpwin(colmin, rowmin);
                break;
        case LEFT:
                if( colmin <= 1 ) {
                        SysBeep();
                        break;
                }
                colmin--;
                colmax--;
                jumpwin(colmin, rowmin);
                break;
        case RIGHT:
                colmin++;
                colmax++;
                jumpwin(colmin, rowmin);
                break;
        }
}

/* draw a rectangle, used to highlight/downlight the current box */
/* windows draws the line outside the rectangle so we need to subtract lwd */

void activaterect(int which)
{

    HDC devHdc;
    int tempr, tempc;

    tempr = crow *box_h;
    tempc = ccol * box_w;
    
    devHdc = GetDC(RDEWnd);
    SelectObject(devHdc, GetStockObject(NULL_BRUSH));
    if( which == 0 )
        SelectObject(devHdc, GetStockObject(WHITE_PEN));
    Rectangle(devHdc, tempc+1, tempr+1, tempc + box_w, tempr + box_h);
    SelectObject(devHdc, GetStockObject(BLACK_PEN));
    ReleaseDC(RDEWnd, devHdc);
}
    
/* 
        when a buttonpress event happens find the square that is being pointed to

 */

int findsquare()
{

        int xw, yw, xr, yr, wcol, wrow;

        closerect();
        querypointer(&xr, &yr, &xw, &yw);

        /* translate to box coordinates */

        wcol = (xw ) / box_w;
        wrow = (yw ) / box_h;

        /* see if it is in the row labels */
        if (wcol == 0) {
                SysBeep();
                activaterect(1);
                return 0;
        }

        /* next check to see if it is in the column labels */

        if (yw <  box_h)
                if (xw >  box_w) {
                        if( ccol != wcol ) {   /* if it is a new col set the focus to it */
                                closerect();
                                ccol=wcol;
                                crow=1;
                                activaterect(1);
                        }
                        popupmenu(wcol);
                }
                else {
                        activaterect(1);
                        SysBeep();
                }
        else if (wcol != ccol || wrow != crow) {
                ccol = wcol;
                crow = wrow;
        }
        activaterect(1);
        return 0;
}

/* return an SEXP containing the data in the current column */
static SEXP getccol()
{
        SEXP tmp, tmp2;
        int i, len, wcol, wrow;
        SEXPTYPE type;
        char cname[10];

        wcol = ccol + colmin - 1;
        wrow = crow + rowmin - 1;
        if (length(inputlist) < wcol) 
                inputlist = listAppend(inputlist,allocList(wcol - length(inputlist)));
        tmp = nthcdr(inputlist, wcol - 1);
        if (CAR(tmp) == R_NilValue) {
                len = (wrow < 100) ? 100 : wrow;
                CAR(tmp) = ssNewVector(REALSXP, len);
                if (TAG(tmp) == R_NilValue) {
                        sprintf(cname, "var%d", wcol);
                        TAG(tmp) = install(cname);
                }
        }
        if (!isVector(CAR(tmp)))
                error("internal type error in spreadsheet\n");
        len = LENGTH(CAR(tmp));
        type = TYPEOF(CAR(tmp));
        if (len < wrow) {
                tmp2 = ssNewVector(type, 2 * len);
                for (i = 0; i < len; i++)
                        if (type == REALSXP)
                                REAL(tmp2)[i] = REAL(CAR(tmp))[i];
                        else if (type == STRSXP)
                                STRING(tmp2)[i] = STRING(CAR(tmp))[i];
                        else
                                error("internal type error in spreadsheet\n");
                LEVELS(tmp2) = LEVELS(CAR(tmp));
                CAR(tmp) = tmp2;
        }
        return (CAR(tmp));
}

/* 
   close up the entry to a square, put the value that has been entered
   into  the correct place and as the correct type
 */

void closerect()
{
        SEXP cvec, tvec;
        
        *bufp = '\0';

        /* first check to see if anything has been entered */
        if ( SeenChar ) {
            SeenChar = 0;
            if (crow == 0) {
                if( clength ) {       /* then we are entering a new column name */
                        if (length(inputlist) < ccol + colmin - 1) {
                                inputlist = listAppend(inputlist, allocList(ccol - colmin - 1 + length(inputlist)));
                                tvec = nthcdr(inputlist, ccol + colmin - 2);
                                TAG(tvec) = install(buf);
                        }
                }
                else {
                        sprintf(buf, "var%d", ccol);
                        printstring(buf, strlen(buf), 0, ccol - colmin + 1);
                }
            }
            else {
                cvec = getccol();
                if( clength ) {
                        if ((crow + rowmin - 1) > LEVELS(cvec))
                                LEVELS(cvec) = (crow + rowmin - 1);
                        if (TYPEOF(cvec) == STRSXP) {
                                tvec = allocString(strlen(buf));
                                strcpy(CHAR(tvec), buf);
                                STRING(cvec)[(rowmin + crow - 2)] = tvec;
                        }
                        else
                                REAL(cvec)[(rowmin + crow - 2)] = atof(buf);
                }
                else  /* we are deleting something */ {
                    if ((crow+rowmin-1) == LEVELS(cvec) ) {
                        LEVELS(cvec)--;
                        if( TYPEOF(cvec) == REALSXP ) 
                            while( REAL(cvec)[LEVELS(cvec)] == ssNA_REAL )
                                LEVELS(cvec)--;
                        else
                            while( STRING(cvec)[LEVELS(cvec)] == ssNA_STRING )
                                LEVELS(cvec)--;
                    }
                    else {
                        if( TYPEOF(cvec) == REALSXP )
                                REAL(cvec)[(rowmin + crow - 2)] = ssNA_REAL;
                        else
                                STRING(cvec)[(rowmin+crow-2)] = ssNA_STRING;
                    }
                }
             }
        }
        else if (crow == 0) {
                sprintf(buf, "var%d", ccol);
                printstring(buf, strlen(buf), 0, ccol - colmin + 1);
        }

        activaterect(0);

        ndecimal = 0;
        nneg = 0;
        ne = 0;
        currentexp = 0;
        clength = 0;
        bufp = buf;
}


void printstring(char *ibuf, int buflen, int row, int col)
{
        int len, x_pos, y_pos;

        x_pos = col*box_w;
        y_pos = row*box_h;
        clearrect(col,row);
        /*(col * box_w + text_offset, row * box_h + text_offset,
                  box_w - 2 * text_offset, box_h - 2 * text_offset);*/
        len = nchars(ibuf, buflen);
        drawtext(x_pos + text_offset, y_pos + text_offset, ibuf, len);
}

int nchars(char *ibuf, int len)
{
        int i;

        for (i = len; i > 1; i--)
                if (textwidth(ibuf, i) < (box_w - text_offset))
                        break;
        return i;
}


/* 
   handlechar has to be able to parse decimal numbers and strings,
   depending on the current column type, only printing characters should get this far
 */

void handlechar(WPARAM c)
{
        SEXP tvec;

        SeenChar = 1;
        if (clength == 0) {
                if (length(inputlist) >= ccol + colmin - 1)
                        tvec = nthcdr(inputlist, ccol + colmin - 2);
                else
                        tvec = R_NilValue;
                if (crow == 0)  /* variable name */
                        currentexp = 3;
                else if (TYPEOF(CAR(tvec)) == STRSXP)   /* character data */
                        currentexp = 2;
                else
                        currentexp = 1;         /* numeric data */
                clearrect(ccol,crow);
                activaterect(1);
        }

        if (currentexp == 1)    /* we are parsing a number */
                switch (c) {
                case '-':
                        if (nneg == 0)
                                nneg++;
                        else
                                goto donehc;
                        break;
                case '.':
                        if (ndecimal == 0)
                                ndecimal++;
                        else
                                goto donehc;
                        break;
                case 'e':
                case 'E':
                        if (ne == 0) {
                                nneg = ndecimal = 0;    /* might have decimal in exponent */
                                ne++;
                        }
                        else
                                goto donehc;
                        break;
                default:
                        if (!isdigit(c))
                                goto donehc;
                        break;
                }
        if (currentexp == 3) {
                if (isspace(c))
                        goto donehc;
                if (clength == 0)
                        if (c != '.' && !isalpha(c))
                                goto donehc;
                        else if (c != '.' && !isalnum(c))
                                goto donehc;
        }

        if (clength++ > 29) {
                warning("spreadsheet: expression too long");
                clength--;
                goto donehc;
        }

        *bufp++ = c;
        printstring(buf, clength, crow, ccol);
        return;

      donehc:SysBeep();
}

void printlabs()
{
        char clab[10];
        int i;
        SEXP tppoint;

        if (length(inputlist) > colmin)
                tppoint = nthcdr(inputlist, colmin - 1);
        else
                tppoint = R_NilValue;

        for (i = colmin; i <= colmax; i++)
                if (TAG(tppoint) != R_NilValue) {
                        printstring(CHAR(PRINTNAME(TAG(tppoint))),
                                    strlen(CHAR(PRINTNAME(TAG(tppoint)))), 0, i - colmin + 1);
                        tppoint = CDR(tppoint);
                }
                else {
                        sprintf(clab, "var%d", i);
                        printstring(clab, strlen(clab), 0, i - colmin + 1);
                }
        for (i = rowmin; i <= rowmax; i++) {
                sprintf(clab, "R %d", i);
                printstring(clab, strlen(clab), i - rowmin + 1, 0);
        }
}

/* Menu Functions */

static void doDEMenu(WPARAM wParam, LPARAM lParam)
{
        switch (GET_WM_COMMAND_ID(wParam,lParam)) {
                case RRR_QUIT:
                        doneSpread=0;
        }
}

static int validName(char *text)
{
        char tmp;


        tmp=*text++;
        if (tmp != '.' && !isalpha(tmp))
                return 0;

        while (*text != '\0') {
                tmp=*text++;
                if ( tmp != '.' && !isalnum(tmp) )
                        return 0;
        }
        return 1;
}



static void drawline(int fromx, int fromy, int tox, int toy)
{
        HDC devHdc;
        POINT lp;

        devHdc=GetDC(RDEWnd);
        if (xlast != fromx || ylast != fromy)
                MoveToEx(devHdc, fromx, fromy, &lp);
        LineTo(devHdc,tox, toy);
        xlast = tox;
        ylast = toy;
        ReleaseDC(RDEWnd, devHdc);
}

static void setattribsfromwindow()
{
    HDC Ihdc;
    TEXTMETRIC Itm;
    int i, j;
    RECT r;
    
    Ihdc=GetDC(RDEWnd);
    GetTextMetrics(Ihdc, &Itm);
    SetBkMode(Ihdc, TRANSPARENT);
    ReleaseDC(RDEWnd, Ihdc);

    box_w = textwidth(digits, strlen(digits)) + 4;
    box_h = Itm.tmAscent + Itm.tmDescent + 8;
    text_offset = 2;
    GetClientRect(RDEWnd, &DERect);
    windowWidth = DERect.right - DERect.left;
    windowHeight = DERect.bottom - DERect.top;
    nwide = windowWidth/box_w;
    nhigh = windowHeight/box_h;
    if( (nwide*box_w != windowWidth) || (nhigh*box_h != windowHeight ) ) {
        GetWindowRect(RDEWnd, &r);
        i=nwide*box_w;
        j=nhigh*box_h;
        MoveWindow(RDEWnd, /*r.left, r.top,*/
                20, 20, (r.right-r.left)-windowWidth+i,
                (r.bottom-r.top)-windowHeight+j,TRUE);
        GetClientRect(RDEWnd, &DERect);
        windowWidth = DERect.right - DERect.left;
        windowHeight = DERect.bottom - DERect.top;
    }
}

/* Text Drawing; */
static void drawtext(int xpos, int ypos, char *text, int len)
{
        HDC devHdc;

        devHdc=GetDC(RDEWnd);
        TextOut(devHdc,xpos, ypos, text, len);
        ReleaseDC(RDEWnd, devHdc);
}

/* cant query the pointer in Windoze */
static void querypointer(int *xglobal, int *yglobal, int *xlocal, int *ylocal)
{
        POINT tp1;

        *xlocal = DEMousePos.x;
        *ylocal = DEMousePos.y;
        tp1=DEMousePos;
        ClientToScreen(RDEWnd, &tp1);
        *xglobal = tp1.x;
        *yglobal = tp1.y;
}

/* find the width of a text string */
static int textwidth(char *text, int nchar)
{
        SIZE lpSize;
        HDC devHdc;

        devHdc=GetDC(RDEWnd);
        GetTextExtentPoint(devHdc,text, nchar, &lpSize);
        ReleaseDC(RDEWnd,devHdc);
        return lpSize.cx;
}


/* Open/Close Windows */
/* to make it the "right" size first set it up and then twiddle the width
   once we can get the text metrics
*/
static int DEInit(void)
{
        MDICREATESTRUCT mdicreate;
        int x, y;
        RECT r;

        GetClientRect(RClient, (LPRECT) &r);
        x = r.left+20;
        y = 20;
        
        mdicreate.szClass = RDEClass;
        mdicreate.szTitle = "R DataEntry";
        mdicreate.hOwner =(HINSTANCE) RInst;
        mdicreate.x = x;
        mdicreate.y = y;
        mdicreate.cx = CW_USEDEFAULT;
        mdicreate.cy = CW_USEDEFAULT;
        mdicreate.style = 0;
        mdicreate.lParam=NULL;
        RDEWnd = (HWND) (UINT) SendMessage(RClient, WM_MDICREATE,0,
                (LONG) (LPMDICREATESTRUCT) &mdicreate);
        if( RDEWnd == NULL )
                return 0;
        ShowWindow(RDEWnd, SW_SHOW);
        setattribsfromwindow();

        return 1;
}

LRESULT FAR PASCAL RDEWndProc(HWND hWnd, UINT message, WPARAM wParam,
        LPARAM lParam)
{
        HDC hdc;
        PAINTSTRUCT ps;
        int i;

        switch(message) {
                case WM_CREATE:
                        break;
                case WM_LBUTTONDOWN:
                case WM_RBUTTONDOWN:
                case WM_MBUTTONDOWN:
                        DEMousePos.x= LOWORD(lParam);
                        DEMousePos.y= HIWORD(lParam);
                        findsquare();
                        break;
                case WM_COMMAND:    
                        doDEMenu(wParam, lParam);
                        return 0;
                case WM_CHAR:
                if( ControlState ) {
                        if( wParam == 'f' ) 
                                jumpwin(colmin, rowmax);
                        if( wParam == 'b' ) {
                                i = (1 > rowmin - nhigh) ? 1 : rowmin - nhigh;
                                jumpwin(colmin, i);
                        }
                }
                else {
                        if( wParam == '\r' || wParam == '\t' || wParam == '\n' )
                         return 0;
                        else
                                handlechar(wParam);
                }
                return 0;
                case WM_KEYUP:
                        if( wParam == VK_SHIFT )
                                ShiftState=0;
                        if( wParam == VK_CONTROL )
                                ControlState=0;
                        break;
                case WM_KEYDOWN:
                        doDEKey( wParam);
                        break;
                case WM_SIZE:
                        if( IsWindow(RDEWnd) )
                                setattribsfromwindow();
                        break;
                case WM_PAINT:
                        hdc=BeginPaint(hWnd, &ps);
                        if( IsWindow(RDEWnd) )
                                drawwindow();
                        EndPaint(hWnd, &ps);
                        return 0;
                case WM_SETFOCUS:
                        SetFocus(hWnd);
                        if (IsWindow(RDEWnd))
                                SendMessage(RDEWnd, WM_MDIACTIVATE, (WPARAM) NULL,
                                        (LPARAM) RDEWnd);
                        break;
                case WM_MDIACTIVATE:
                        if((HWND) lParam == hWnd ) {
                                SendMessage(RClient,WM_MDISETMENU, (WPARAM) RMenuDE, (LPARAM) RMenuDEWin);
                                DrawMenuBar(RFrame);
                        }
                        else if( doneSpread ) {
                            MessageBox(hWnd, "You must quit the data entry window before doing anything else",
                                "R Data Entry", MB_OK | MB_ICONEXCLAMATION);
                            PostMessage(RClient, WM_MDIACTIVATE, (UINT) RDEWnd,0);
                        }
                        return 0;
                        break;
                case WM_SYSCOMMAND: /* Disable minimizing */
                        if( (wParam & 0xFFF0) == SC_MINIMIZE ) {
                            MessageBox(hWnd, "You can't iconify the Data Entry Window",
                                "R Data Entry", MB_OK | MB_ICONEXCLAMATION);
                            return 1l;
                        }
                        break;
                case WM_CLOSE:
                case WM_DESTROY:
                        doneSpread=0;
                        return 0;
        }
        return(DefMDIChildProc(hWnd, message, wParam, lParam));
}



static void CloseDE()
{
        closerect();
        SendMessage(RClient, WM_MDIDESTROY, (WPARAM) (HWND) RDEWnd, 0);
}

/* clear the rectangle with left top corner at xpos,ypos */
/* we use the fact that the pen is width 1 */
static void clearrect(int col, int row)
{
        HDC Nhdc;
        RECT tre;

        tre.left=col*box_w+1;
        tre.top=row*box_h+1;
        tre.right=tre.left+box_w-1;
        tre.bottom=tre.top+box_h-1;
        Nhdc=GetDC(RDEWnd);
        FillRect(Nhdc, &tre, GetStockObject(WHITE_BRUSH));
        ReleaseDC(RDEWnd, Nhdc);
}

static void clearwindow()
{
        HDC Nhdc;

        Nhdc=GetDC(RDEWnd);
        FillRect(Nhdc, &DERect, GetStockObject(WHITE_BRUSH));
        ReleaseDC(RDEWnd, Nhdc);
}

/* Menus  */

void popupmenu(int col)
{
        PUPcol=col;
        DialogBox(RInst, "DEVarBox", RDEWnd, (DLGPROC) DEVar);
        SetFocus(RDEWnd);
}

extern BOOL CALLBACK DEVar(HWND hDlg, UINT message,
        WPARAM wParam,LPARAM lParam)
{
        char name[20];
        static SEXP tvec;
        int type, levs;


        switch (message) {
                case    WM_INITDIALOG:
                        if (length(inputlist) < PUPcol + colmin - 1) 
                                inputlist = listAppend(inputlist, allocList(PUPcol + colmin - 1 - length(inputlist)));
                        tvec = nthcdr(inputlist, PUPcol + colmin - 2);
                        if (TAG(tvec) != R_NilValue)
                                sprintf(name,"%s", CHAR(PRINTNAME(TAG(tvec))));
                        else
                                sprintf(name, "Var%d", PUPcol + colmin - 1);
                        SetDlgItemText(hDlg, RDD_NAME, name);
                        SetWindowText(hDlg, name);
                        if( TYPEOF(CAR(tvec)) == STRSXP )
                                CheckRadioButton( hDlg, RDD_NUM, RDD_CHAR, RDD_CHAR);
                        else
                                CheckRadioButton( hDlg, RDD_NUM, RDD_CHAR, RDD_NUM);
                        return TRUE;
                case WM_COMMAND:
                        switch (wParam) {
                                case IDOK:
                                        GetDlgItemText(hDlg, RDD_NAME, name, 20);
                                        if (!validName(name) ) {
                                                MessageBox(RDEWnd,  "The variable name is not valid",
                                                        "R Application", MB_ICONEXCLAMATION | MB_OK);
                                                name[0]='\0';
                                                SetDlgItemText(hDlg, RDD_NAME, name);
                                                return 1;
                                        }
                                TAG(tvec) = install(name);
                                printstring(name, strlen(name), 0, PUPcol);
                                type = SendDlgItemMessage(hDlg, RDD_NUM, BM_GETCHECK,0,0);
                                if (type == 1) {
                                        if (CAR(tvec) == R_NilValue)
                                                CAR(tvec) = ssNewVector(REALSXP, 100);
                                        else {
                                                levs = LEVELS(CAR(tvec));
                                                CAR(tvec) = coerceVector(CAR(tvec), REALSXP);
                                                LEVELS(CAR(tvec)) = levs;
                                        }
                                }
                                else {
                                        if (CAR(tvec) == R_NilValue)
                                                CAR(tvec) = ssNewVector(STRSXP, 100);
                                        else {
                                                levs = LEVELS(CAR(tvec));
                                                CAR(tvec) = coerceVector(CAR(tvec), STRSXP);
                                                LEVELS(CAR(tvec)) = levs;
                                        }
                                }
                                        EndDialog(hDlg, TRUE);
                                        return TRUE;
                                case IDCANCEL:
                                        EndDialog(hDlg, FALSE);
                                        return TRUE;
                                case RDD_NUM:
                                case RDD_CHAR:
                                        CheckRadioButton(hDlg, RDD_NUM, RDD_CHAR, wParam);
                                        return TRUE;   
                                }

                }
                return FALSE;
}
