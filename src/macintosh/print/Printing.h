/* This program:
  
  o Opens a window in which the contents of the main fields of the
    TPrint, TPrJob, TPrStl and TPrInfo structures are displayed when the
    user dismisses the style dialog box, and in which the first part of
    a document is displayed when the user hits the Print button in the
    job dialog box.
  
  o Adds a pop-up menu button, three radio buttons, a checkbox, and a
    group box to the job dialog box.

  o Allows the user to print a document containing a picture and text,
    with the text being printed in the font and font size, and with
    the fractional widths setting, specified using the items added to
    the job dialog box.

  The program utilises the following resources: 

  o  'MBAR' resource and associated 'MENU' resources (preload, non-purgeable). 
  o  A 'WIND' resource (purgeable). 
  o  A 'TEXT' resource (non-purgeable) used for printing. 
  o  A 'PICT' resource (non-purgeable) used for printing. 
  o  'CNTL' resources (purgeable) for controls added to the job dialog box. 
  o  A 'DITL' resource (purgeable) specifying the items to be appended 
     to the job dialog box.
  o  A 'MENU' resource (preload, non-purgeable) for the pop-up menu button. 

*/


/* includes */

#include <Appearance.h>
#include <Devices.h>
#include <Fonts.h>
#include <Printing.h>
#include <Processes.h>
#include <Resources.h>
#include <TextUtils.h>
#include <ToolUtils.h>

/* defines */

#define mApple			128
#define mFile			129
#define	 iQuit			11
#define	 iPageSetup		8
#define	 iPrint			9
#define rMenubar		128
#define rWindow			128
#define rText			128
#define rPicture		128
#define rJobDialogAppendDITL	128
#define	 iPopupButton		1
#define	 iRadioButton10pt	2
#define	 iRadioButton12pt	3
#define	 iRadioButton14pt	4
#define	 iCheckboxFracWidths	5
#define kMargin			90
#define MAXLONG			0x7FFFFFFF
#define	gFontNumber	4
#define gFontSize	9
#define MIN(a,b)	((a) < (b) ? (a) : (b))

/* function prototypes */

void    doUpdate              (EventRecord*);
void	doPrinting		(void);
OSErr	doCreatePrintRecord	(void);
void	doPrStyleDialog		(void);
SInt16	doCalcNumberOfPages	(Rect);
void	doDrawPage		(Rect,SInt16,SInt16);
SInt16	doGetPageOrientation	(void);
Boolean doIsPrGeneralThere	(void);
void	doPrintRecordsInfo	(void);
void	doDrawRectStrings(Str255,SInt16,SInt16,Str255,SInt16,SInt16,Str255);
void 	doDrawPageOrientation	(void);
void	doErrorAlert	(SInt16,Boolean);
void	doConcatPStrings(Str255,Str255);

pascal TPPrDlg	initialisationFunction	(THPrint);
void doAppendTheDITL			(TPPrDlg);
pascal void itemEvaluationFunction	(TPPrDlg,SInt16);
pascal Boolean  eventFilter(DialogPtr,EventRecord *,SInt16 *);
