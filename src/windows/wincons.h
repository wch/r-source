 /*  R : A Computer Langage for Statistical Data Analysis
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


#include <windows.h>
#include <windowsx.h>
/*#include <env.h>*/

#include "c:\april\include\Defn.h"

#define DllExport __declspec(dllexport)

DllExport void rsort(double*, int);

/* Globals */

extern HINSTANCE RInst;
extern HWND RFrame, RClient, RConsoleFrame, RConsole;
extern HWND hWndServerDDE;

extern char RGraphClass[];
extern char REditClass[];
extern char RDEClass[];

extern char REdfilename[MAX_PATH];

extern float R_WinVersion;

extern HMENU RMenuEdit, RMenuDE, RMenuGraph, RMenuConsole, RMenuInit;
extern HMENU RMenuEditWin, RMenuDEWin, RMenuGraphWin, RMenuConsWin, RMenuInitWin;
/* Function ProtoTypes */

/* R Functions */

extern void     dump_image(char* , int);
extern BOOL FAR PASCAL  About(HWND, WORD, WPARAM, LPARAM);
extern BOOL     RPrintGraph(HWND, HANDLE);
extern BOOL     RPrintBitMap(LPBITMAPINFO, LPBYTE);
extern void     RPrintText(HWND, HWND);

extern SEXP BinaryLoad(FILE*);
extern SEXP AsciiLoad(FILE*);

LRESULT CALLBACK AbortPrintJob(HWND, UINT, WPARAM, LPARAM);
BOOL CALLBACK    AbortProc(HDC, int);
void R_ProcessDropFiles(HANDLE, int);

BOOL             InitApplication(HINSTANCE);
BOOL             InitInstance(HINSTANCE,int);



extern int EventLoop(void);
extern void SysBeep(void);
extern int Win_ROpenDlg(HWND, int);
extern int Win_RSaveDlg(HWND);
extern void R_SetMemory(int, int);

extern void sysdata(void);
extern void userdata(void);

#define R_printf Rprintf

#define ICON_2  2
#define ICON_1  1
#define RRR_FIRSTCHILD 666
#define RBuffLen       300


#define INIT_MENU_POS  0
#define CONS_MENU_POS  1

/* Data Entry constants */

#define RDD_NUM         21
#define RDD_CHAR        22
#define RDD_NAME        23
#define RDD_NO          24


/* 59 is reserved for use with Edit windows */
#define RRR_EDIT                59
#define RRR_TEDIT               60

#define RRR_ABOUT               100
#define RRR_NEW                 101
#define RRR_SAVE                102
#define RRR_QUIT                103
#define RRR_UNDO                104
#define RRR_CUT                 105
#define RRR_COPY                106
#define RRR_PASTE               107
#define RRR_DEL                 108
#define RRR_CPASTE              109
#define RRR_CLEAR               110
#define RRR_GRAPH               111
#define RRR_SETUP               112
#define RRR_PRINT               113
#define RRR_HOTEXT              114
#define RRR_VETEXT              115
#define RRR_REFRESH             116
#define RRR_OPEN                117
#define RRR_LOAD                118
#define RRR_DONE                119
#define RRR_QBROW               120
#define RRR_TILE                121
#define RRR_CASC                122
#define RRR_ARRA                123
#define RRR_CLOSEALL            124
#define RRR_SETMEM              125
#define RRR_HELP                126
#define RRR_DATA                127
#define RRR_SYSDATA             128

#define RDG_FILE                200
#define RDG_DIR                 201
#define RDG_ASISF               202
#define RDG_ASIST               203
#define RDG_HEADF               204
#define RDG_HEADT               205
#define RDG_SEP                 206
#define RDG_NASTRINGS           207
#define RDG_SKIP                208
