/*
 *	WASTE.h
 *
 *	C/C++ interface to the WASTE text engine
 *
 *	version 2.0b2 (August 2000)
 *
 *	Copyright (c) 1993-2000 Marco Piovanelli
 *	All Rights Reserved
 *
 *	<http://www.merzwaren.com/waste/>
 *	<mailto:waste@merzwaren.com>
 *
 */

#ifndef __WASTE__
#define __WASTE__

#ifndef __CONDITIONALMACROS__
#include <ConditionalMacros.h>
#endif

#if !defined(UNIVERSAL_INTERFACES_VERSION) || (UNIVERSAL_INTERFACES_VERSION < 0x320)
#error "You need Universal Headers version 3.2 or newer to use WASTE.h"
#endif

#ifndef __MACTYPES__
#include <MacTypes.h>
#endif

#ifndef __MIXEDMODE__
#include <MixedMode.h>
#endif

#ifndef __QUICKDRAWTEXT__
#include <QuickdrawText.h>
#endif

#ifndef __QUICKDRAW__
#include <Quickdraw.h>
#endif

#ifndef __SCRIPT__
#include <Script.h>
#endif

#ifndef __TEXTUTILS__
#include <TextUtils.h>
#endif

#ifndef __TEXTEDIT__
#include <TextEdit.h>
#endif

#ifndef __DRAG__
#include <Drag.h>
#endif

#ifdef __cplusplus
extern "C" {
#endif

#ifndef __LONGCOORDINATES__
#ifndef _LongCoords_
#define _LongCoords_

typedef struct LongPt
{
	SInt32 v;
	SInt32 h;
} LongPt;

typedef struct LongRect
{
	SInt32 top;
	SInt32 left;
	SInt32 bottom;
	SInt32 right;
} LongRect;

#endif /*_LongCoords_*/
#endif /*__LONGCOORDINATES__*/

#if PRAGMA_STRUCT_ALIGN
#pragma options align=mac68k
#endif

#if PRAGMA_IMPORT
#pragma import on
#endif

/*	The macro WASTE_VERSION expands to the current version of WASTE,	*/
/*	expressed in standard NumVersion format (see <MacTypes.h>)			*/

#define WASTE_VERSION	0x02006002		/* 2.0b2 */

/*	result codes */

enum
{
	weCantUndoErr				=	-10015,	/* undo buffer is clear (= errAECantUndo) */
	weEmptySelectionErr			=	-10013,	/* selection range is empty (= errAENoUserSelection) */
	weUnknownObjectTypeErr		=	-9478,	/* specified object type is not registered */
	weObjectNotFoundErr			=	-9477,	/* no object found at specified offset */
	weReadOnlyErr				=	-9476,	/* instance is read-only */
	weTextNotFoundErr			=	-9474,	/* search string not found */
	weInvalidTextEncodingErr	=	-9473,	/* specified text encoding is invalid or unsupported */
	weDuplicateAttributeErr		=	-9472,	/* one of the attribute selectors was specified more than once */
	weInvalidAttributeSizeErr	=	-9471,	/* attribute value size is invalid */
	weReadOnlyAttributeErr		=	-9470,	/* attribute is read-only */
	weOddByteCountErr			=	-9469,	/* expected an even number of bytes */
	weHandlerNotFoundErr		=	-1717,	/* couldn't find specified handler (= errAEHandlerNotFound) */
	weNotHandledErr				=	-1708,	/* please use default behavior (= errAEEventNotHandled) */
	weNewerVersionErr			=	-1706,	/* version of formatting scrap is too new (= errAENewerVersion) */
	weCorruptDataErr			=	-1702,	/* malformed formatting scrap (= errAECorruptData) */
	weProtocolErr				=	-603,	/* improper call order (= protocolErr) */
	weUndefinedSelectorErr		=	-50		/* unknown selector (= paramErr) */
};

/*	alignment styles */

enum
{
	weFlushLeft 		=	-2,		/* flush left */
	weFlushRight		=	-1,		/* flush right */
	weFlushDefault		=	 0,		/* flush according to system direction */
	weCenter			=	 1,		/* centered */
	weJustify			=	 2		/* fully justified */
};

/*	primary line direction */

enum
{
	weDirDefault		=	 1,		/* according to system direction */
	weDirRightToLeft	=	-1,		/* force right-to-left */
	weDirLeftToRight	=	 0		/* force left-to-right */
};

/*	values for the inMode parameter in WESetStyle and the ioMode parameter in WEContinuousStyle */

enum
{
	weDoFont				=	0x0001,
	weDoFace				=	0x0002,
	weDoSize				=	0x0004,
	weDoColor				=	0x0008,
	weDoAll					=	weDoFont | weDoFace | weDoSize | weDoColor,
	weDoAddSize				=	0x0010,
	weDoToggleFace			=	0x0020,
	weDoReplaceFace			=	0x0040,
	weDoPreserveScript		=	0x0080,
	weDoExtractSubscript	=	0x0100,
	weDoFaceMask			=	0x0200
};

/*	values for the inMode parameter in WESetRuler and the ioMode parameter in WEContinuousRuler */

enum
{
	weDoDirection			=	0x00000001,
	weDoAlignment			=	0x00000002,
	weDoLeftIndent			=	0x00000004,
	weDoRightIndent			=	0x00000008,
	weDoFirstLineIndent		=	0x00000010,
	weDoLineSpacing			=	0x00000020,
	weDoSpaceBefore			=	0x00000040,
	weDoSpaceAfter			=	0x00000080,
	weDoBottomBorderStyle	=	0x00000400
};

/*	values for the outEdge parameter in WEGetOffset etc. */

enum
{
	kLeadingEdge			=	-1,		/* point is on the leading edge of a glyph */
	kTrailingEdge			=	 0,		/* point is on the trailing edge of a glyph */
	kObjectEdge				=	 2		/* point is in the middle of an embedded object */
};

/*	values for the inFeature parameter in WEFeatureFlag */

enum
{
	weFAutoScroll			=	0,		/* automatically scroll the selection range into view */
	weFOutlineHilite		=	2,		/* frame selection when deactivated */
	weFReadOnly				=	5,		/* disallow modifications */
	weFUndo					=	6,		/* support WEUndo() */
	weFIntCutAndPaste		=	7,		/* use intelligent cut-and-paste rules */
	weFDragAndDrop			=	8,		/* support drag-and-drop text editing */
	weFInhibitRecal			=	9,		/* don't recalculate line starts and don't redraw text */
	weFUseTempMem			=	10,		/* use temporary memory for main data structures */
	weFDrawOffscreen		=	11,		/* draw text offscreen for smoother visual results */
	weFInhibitRedraw		=	12,		/* don't redraw text */
	weFMonoStyled			=	13,		/* disallow style changes */
	weFMultipleUndo			=	14,		/* enable multiple undo/redo */
	weFNoKeyboardSync		=	29,		/* disable automatic font/keyboard synchronization */
	weFInhibitICSupport		=	30,		/* don't process command-clicks with Internet Config */
	weFInhibitColor			=	31		/* ignore color information; draw in black & white only */
};

/*	values for the inOptions parameter in WENew */

enum
{
	weDoAutoScroll			=	1UL << weFAutoScroll,
	weDoOutlineHilite		=	1UL << weFOutlineHilite,
	weDoReadOnly			=	1UL << weFReadOnly,
	weDoUndo				=	1UL << weFUndo,
	weDoIntCutAndPaste		=	1UL << weFIntCutAndPaste,
	weDoDragAndDrop			=	1UL << weFDragAndDrop,
	weDoInhibitRecal		=	1UL << weFInhibitRecal,
	weDoUseTempMem			=	1UL << weFUseTempMem,
	weDoDrawOffscreen		=	1UL << weFDrawOffscreen,
	weDoInhibitRedraw		=	1UL << weFInhibitRedraw,
	weDoMonoStyled			=	1UL << weFMonoStyled,
	weDoMultipleUndo		=	1UL << weFMultipleUndo,
	weDoNoKeyboardSync		=	1UL << weFNoKeyboardSync,
	weDoInhibitICSupport	=	1UL << weFInhibitICSupport,
	weDoInhibitColor		=	1UL << weFInhibitColor
};

/*	values for the inAction parameter in WEFeatureFlag */

enum
{
	weBitToggle				=	-2,		/* toggles the specified feature */
	weBitTest				=	-1,		/* returns the current setting of the specified feature */
	weBitClear				=	 0,		/* disables the specified feature */
	weBitSet				=	 1		/* enables the specified feature */
};

/*	values for the inCase parameter in WEChangeCase */

enum
{
	weLowerCase				=	0,		/* lowercase text */
	weUpperCase				=	1		/* uppercase text */
};

/*	values for the inMatchOptions parameter in WEFind */

enum
{
	weFindWholeWords				=	0x00000001,		/* match whole words only */
	weFindCaseInsensitive			=	0x00000002,		/* ignore case differences */
	weFindDiacriticalInsensitive	=	0x00000004		/* ignore diacritical marks */
};

/*	values for the inPutOptions parameter in WEPut */

enum
{
	wePutIntCutAndPaste				=	0x00000001,		/* apply intelligent cut & paste rules */
	wePutAddToTypingSequence		=	0x00000002,		/* don't break current typing sequence */
	wePutDetectUnicodeBOM			=	0x00000200		/* detect Unicode byte-order mark */
};

/*	values for the inStreamOptions parameter in WEStreamRange */

enum
{
	weStreamDestinationKindMask		=	0x000000FF,		/* the low byte in inStreamOptions is passed on to */
														/* the inDestinationKind parameter of object streaming handlers */
														/* (valid option for the kTypeSoup and kTypeStyleScrap flavor types) */
	weStreamIncludeObjects			=	0x00000100		/* include descriptions of embedded objects */
														/* (valid option for the kTypeStyleScrap flavor type) */
};

/*	values for the inGetOptions parameter in WEGetTextRangeAsUnicode */

enum
{
	weGetAddUnicodeBOM				=	0x00000200,		/* prepend a Unicode byte-order mark to the text stream */
	weGetLittleEndian				=	0x00000400		/* use little-endian byte-order */
};

/*	selectors for WESetAttributes, WEGetAttributes, WEMatchAttributes, etc. */

enum
{
/*	character-level attributes */
	weTagFontFamily				=	FOUR_CHAR_CODE('font'),	/* font family number (FMFontFamily) */
	weTagFontSize				=	FOUR_CHAR_CODE('ptsz'),	/* font size (Fixed) */
	weTagPlain					=	FOUR_CHAR_CODE('plan'), /* plain text (Boolean) */
	weTagBold					=	FOUR_CHAR_CODE('bold'), /* bold (Boolean) */
	weTagItalic					=	FOUR_CHAR_CODE('ital'), /* italic (Boolean) */
	weTagUnderline				=	FOUR_CHAR_CODE('undl'),	/* underline (Boolean) */
	weTagOutline				=	FOUR_CHAR_CODE('outl'), /* outline (Boolean) */
	weTagShadow					=	FOUR_CHAR_CODE('shad'), /* shadow (Boolean) */
	weTagCondensed				=	FOUR_CHAR_CODE('cond'), /* condensed (Boolean) */
	weTagExtended				=	FOUR_CHAR_CODE('pexp'), /* extended (Boolean) */
	weTagStrikethrough			=	FOUR_CHAR_CODE('strk'), /* strikethrough (Boolean) */
	weTagTextColor				=	FOUR_CHAR_CODE('colr'),	/* text color (RGBColor) */
	weTagBackgroundColor		=	FOUR_CHAR_CODE('pbcl'), /* background color (RGBColor) */
	weTagTransferMode			=	FOUR_CHAR_CODE('pptm'), /* QuickDraw text transfer mode (SInt16) */
	weTagVerticalShift			=	FOUR_CHAR_CODE('xshf'),	/* vertical shift (Fixed) */

/*	paragraph-level attributes */
	weTagAlignment				=	FOUR_CHAR_CODE('pjst'),	/* alignment (enumeration; can be one of deft/left/cent/rght/full) */
	weTagDirection				=	FOUR_CHAR_CODE('LDIR'), /* primary line direction (enumeration; can be one of deft/L->R/R->L) */
	weTagLineSpacing			=	FOUR_CHAR_CODE('ledg'), /* line spacing (Fixed; 0.0 = normal, 1.0 = double) */
	weTagLeftIndent				=	FOUR_CHAR_CODE('lein'), /* left indent (Fixed) */
	weTagRightIndent			=	FOUR_CHAR_CODE('riin'),	/* right indent (Fixed) */
	weTagFirstLineIndent		=	FOUR_CHAR_CODE('fidt'), /* first line indent (Fixed) */
	weTagSpaceBefore			=	FOUR_CHAR_CODE('spbe'), /* space before (Fixed) */
	weTagSpaceAfter				=	FOUR_CHAR_CODE('spaf'), /* space after (Fixed) */
	weTagBottomBorderStyle		=	FOUR_CHAR_CODE('BBRD'), /* bottom border style (enumeration; can be one of NONE/SLDL/DTDL/THKL) */

/*	the following meta-selectors are only valid in calls to WESetAttributes/WESetOneAttribute */
	weTagForceFontFamily		=	FOUR_CHAR_CODE('ffnt'),	/* like weTagFontFamily, but may change text encoding */
	weTagAddFontSize			=	FOUR_CHAR_CODE('+siz'),	/* like weTagFontSize, but value is added rather than replaced */
	weTagAddVerticalShift		=	FOUR_CHAR_CODE('+shf'),	/* like weTagVerticalShift, but value is added rather than replaced */

/*	selectors for read-only attributes, only valid in calls to WEGetAttributes/WEGetOneAttribute */
	weTagTextEncoding			=	FOUR_CHAR_CODE('ptxe'),	/* text encoding */
	weTagQDStyles				=	FOUR_CHAR_CODE('qdst'),	/* QuickDraw styles (Style) */
	weTagTETextStyle			=	FOUR_CHAR_CODE('tets')	/* TextEdit-compatibile TextStyle record */
};

/*	alignment selectors (use in conjunction with weTagAlignment) */

enum
{
	weTagAlignmentDefault		=	FOUR_CHAR_CODE('deft'),	/* align according to system direction */
	weTagAlignmentLeft			=	FOUR_CHAR_CODE('left'),	/* flush left */
	weTagAlignmentCenter		=	FOUR_CHAR_CODE('cent'), /* center */
	weTagAlignmentRight			=	FOUR_CHAR_CODE('rght'),	/* flush right */
	weTagAlignmentFull			=	FOUR_CHAR_CODE('full')	/* justify */
};

/*	direction selectors (use in conjunction with weTagDirection) */

enum
{
	weTagDirectionDefault		=	FOUR_CHAR_CODE('deft'),	/* arrange bidi text according to system direction */
	weTagDirectionLeftToRight	=	FOUR_CHAR_CODE('L->R'), /* primary line direction is left-to-right */
	weTagDirectionRightToLeft	=	FOUR_CHAR_CODE('R->L')	/* primary line direction is right-to-left */
};

/*	border style selectors (use in conjunction with weTagBottomBorderStyle) */

enum
{
	weTagBorderStyleNone		=	FOUR_CHAR_CODE('NONE'), /* no border */
	weTagBorderStyleThin		=	FOUR_CHAR_CODE('SLDL'), /* thin line */
	weTagBorderStyleDotted		=	FOUR_CHAR_CODE('DTDL'), /* dotted line */
	weTagBorderStyleThick		=	FOUR_CHAR_CODE('THKL')	/* thick line */
};

/*	commonly used values for the line spacing attribute */

enum
{
	weLineSpacingSingle			=	0x00000000,				/* single space */
	weLineSpacingOneAndHalf		=	0x00008000,				/* one and half space */
	weLineSpacingDouble			=	0x00010000				/* double space */
};

/*	selectors for WEGetInfo and WESetInfo */

enum
{
	weCharByteHook				=	FOUR_CHAR_CODE('cbyt'), /* CharByte hook */
	weCharToPixelHook			=	FOUR_CHAR_CODE('c2p '),	/* CharToPixel hook */
	weCharTypeHook				=	FOUR_CHAR_CODE('ctyp'), /* CharType hook */
	weClickLoop					=	FOUR_CHAR_CODE('clik'),	/* click loop callback */
	weCurrentDrag				=	FOUR_CHAR_CODE('drag'),	/* drag currently being tracked from WEClick() */
	weDrawTextHook				=	FOUR_CHAR_CODE('draw'), /* text drawing hook */
	weDrawTSMHiliteHook			=	FOUR_CHAR_CODE('dtsm'),	/* hook for drawing Text Services Manager underlines */
	weEraseHook					=	FOUR_CHAR_CODE('eras'), /* background erasing hook */
	weFontFamilyToNameHook		=	FOUR_CHAR_CODE('ff2n'), /* hook for mapping font family numbers to font names */
	weFontNameToFamilyHook		=	FOUR_CHAR_CODE('fn2f'), /* hook for mapping font names to font family numbers */
	weFluxProc					=	FOUR_CHAR_CODE('flux'), /* flux proc */
	weHiliteDropAreaHook		=	FOUR_CHAR_CODE('hidr'), /* drop area highlighting hook */
	weLineBreakHook				=	FOUR_CHAR_CODE('lbrk'),	/* line breaking hook */
	wePixelToCharHook			=	FOUR_CHAR_CODE('p2c '), /* PixelToChar hook */
	wePort						=	FOUR_CHAR_CODE('port'),	/* graphics port */
	wePreTrackDragHook			=	FOUR_CHAR_CODE('ptrk'), /* pre-TrackDrag hook */
	weRefCon					=	FOUR_CHAR_CODE('refc'),	/* reference constant for use by application */
	weScrollProc				=	FOUR_CHAR_CODE('scrl'),	/* auto-scroll callback */
	weText						=	FOUR_CHAR_CODE('text'),	/* text handle */
	weTranslateDragHook 		=	FOUR_CHAR_CODE('xdrg'), /* drag translation callback */
	weTranslucencyThreshold		=	FOUR_CHAR_CODE('tluc'), /* area threshold for translucent drags */
	weTSMDocumentID				=	FOUR_CHAR_CODE('tsmd'),	/* Text Services Manager document ID */
	weTSMPreUpdate				=	FOUR_CHAR_CODE('pre '),	/* Text Services Manager pre-update callback */
	weTSMPostUpdate				=	FOUR_CHAR_CODE('post'),	/* Text Services Manager post-update callback */
	weURLHint					=	FOUR_CHAR_CODE('urlh'),	/* URL hint string for Internet Config */
	weWordBreakHook				=	FOUR_CHAR_CODE('wbrk')	/* word breaking hook */
};

/*	values for the inHandlerSelector parameter in WEInstallObjectHandler */

enum
{
	weNewHandler				=	FOUR_CHAR_CODE('new '),	/* new handler */
	weDisposeHandler			=	FOUR_CHAR_CODE('free'),	/* dispose handler */
	weDrawHandler				=	FOUR_CHAR_CODE('draw'),	/* draw handler */
	weClickHandler				=	FOUR_CHAR_CODE('clik'),	/* click handler */
	weStreamHandler				=	FOUR_CHAR_CODE('strm'),	/* stream handler */
	weHoverHandler				=	FOUR_CHAR_CODE('hovr')	/* hover handler */
};

/*	values for the inRequestedType parameter in WEStreamRange */

enum
{
	kTypeText					=	FOUR_CHAR_CODE('TEXT'),	/* raw text */
	kTypeStyles 				=	FOUR_CHAR_CODE('styl'),	/* TextEdit-compatible style scrap */
	kTypeSoup 					=	FOUR_CHAR_CODE('SOUP'),	/* "soup" of embedded objects */
	kTypeFontTable				=	FOUR_CHAR_CODE('FISH'),	/* font table */
	kTypeParaFormat				=	FOUR_CHAR_CODE('WEpf'),	/* WASTE 2.0 paragraph formatting */
	kTypeRulerScrap				=	FOUR_CHAR_CODE('WEru'),	/* WASTE 2.0 ruler table */
	kTypeCharFormat				=	FOUR_CHAR_CODE('WEcf'), /* WASTE 2.0 character formatting */
	kTypeStyleScrap				=	FOUR_CHAR_CODE('WEst'),	/* WASTE 2.0 style table */
	kTypeUnicodeText			=	FOUR_CHAR_CODE('utxt'),	/* raw text in UTF-16 Unicode */
	kTypeUTF8Text				=	FOUR_CHAR_CODE('UTF8'),	/* raw text in UTF-8 Unicode */
	kTypeStyledText				=	FOUR_CHAR_CODE('STXT')	/* AppleScript-style styled text */
};

/*	action kinds */

enum
{
	weAKNone			=	0,		/* null action */
	weAKUnspecified		=	1,		/* action of unspecified nature */
	weAKTyping			=	2,		/* some text has been typed in */
	weAKCut				=	3,		/* the selection range has been cut */
	weAKPaste			=	4,		/* something has been pasted */
	weAKClear			=	5,		/* the selection range has been deleted */
	weAKDrag			=	6,		/* drag and drop operation */
	weAKSetStyle		=	7,		/* some style has been applied to a text range */
	weAKSetRuler		=	8,		/* some ruler has been applied to a text range */
	weAKBackspace		=	9,		/* text deleted by backspace */
	weAKFwdDelete		=	10,		/* text deleted by forward delete */
	weAKCaseChange		=	11,		/* case transformation */
	weAKObjectChange	=	12		/* an embedded object was resized */
	/* values above 1023 are free for use by client applications */
};

/*	values for the inDestionationKind parameter passed to object streaming handlers */

enum
{
	weToScrap			=	0,		/* called from WECopy to copy object to the desk scrap */
	weToDrag			=	1,		/* called from WEClick to copy object to a drag */
	weToSoup			=	2		/* called to create a private SOUP for internal use (e.g., for undo/redo) */
	/* values above 127 are free for use by client applications */
};

/*	mouse actions passed to object "hover" handlers */

enum
{
	weMouseEnter		=	0,		/*	mouse has entered object frame */
	weMouseWithin		=	1,		/*	mouse is still within object frame */
	weMouseLeave		=	2		/*	mouse has exited object frame */
};

enum
{
/*	kCurrentSelection is a meta-value that can be passed to some calls, */
/*	like WEStreamRange and WEGetHiliteRgn, to signify "use current selection range" */
	kCurrentSelection	=	-1,

/*	kNullStyle is a meta-value that can be passed to WEGetAttributes / WEGetOneAttribute */
/*	to retrieve the "null" style (the style that would be applied to the next typed character) */
	kNullStyle			=	-2
};

typedef struct OpaqueWEReference *				WEReference;
typedef struct OpaqueWEObjectReference *		WEObjectReference;
typedef struct OpaqueWEPrintSession *			WEPrintSession;
typedef Handle									WESoupHandle;
typedef Handle									WEFontTableHandle;
typedef SInt16									WEActionKind;
typedef SInt8									WEAlignment;
typedef SInt16									WEDirection;
typedef SInt8									WEEdge;
typedef UInt16									WEStyleMode;
typedef UInt32									WERulerMode;
typedef FourCharCode							WESelector;

typedef struct WERunInfo
{
	SInt32 				runStart;		/* byte offset to first character of style run */
	SInt32 				runEnd;			/* byte offset past last character of style run */
	SInt16 				runHeight;		/* line height (ascent + descent + leading) */
	SInt16 				runAscent;		/* font ascent */
	TextStyle 			runStyle;		/* text attributes */
	WEObjectReference	runObject;		/* either nil or reference to embedded object */
} WERunInfo;

typedef struct WERuler
{
	SInt8			alignment;			/* alignment */
	SInt8			direction;			/* primary line direction */
	UInt8			reserved1;			/* reserved for future use */
	UInt8			bottomBorderStyle;	/* bottom border style */
	Fixed			leftIndent;			/* left margin (in fixed points) */
	Fixed			rightIndent;		/* right margin (in fixed points) */
	Fixed			firstLineIndent;	/* first line indent (in fixed points) */
	Fixed			lineSpacing;		/* space between lines (0.0 = normal, 1.0 = double, etc.) */
	Fixed			spaceBefore;		/* space before paragraph (in fixed points) */
	Fixed			spaceAfter;			/* space after paragraph (in fixed points) */
	SInt32			reserved2[57];		/* reserved for future use */
} WERuler;

typedef struct WEParaInfo
{
	SInt32			paraStart;			/* byte offset to first character of paragraph run */
	SInt32			paraEnd;			/* byte offset to last character of paragraph run */
	WERuler			paraRuler;			/* ruler associated with this paragraph run */
} WEParaInfo;

typedef struct WEPrintOptions
{
	Rect			pageRect;			/* destination rectangle for printing */
	SInt32			reserved[14];		/* reserved for future use: set to zero! */
} WEPrintOptions;

/*	WASTE callbacks: prototypes */

typedef CALLBACK_API ( Boolean,				WEClickLoopProcPtr )
	(	WEReference			inWE ) ;

typedef CALLBACK_API ( void,				WEScrollProcPtr )
	(	WEReference			inWE ) ;

typedef CALLBACK_API ( void,				WETSMPreUpdateProcPtr )
	(	WEReference			inWE ) ;

typedef CALLBACK_API ( void,				WETSMPostUpdateProcPtr )
	(	WEReference			inWE,
		SInt32				inFixLength,
		SInt32				inInputAreaStart,
		SInt32				inInputAreaEnd,
		SInt32				inPinRangeStart,
		SInt32				inPinRangeEnd ) ;

typedef CALLBACK_API ( OSErr,				WEPreTrackDragProcPtr )
	(	DragReference		inDrag,
		WEReference			inWE ) ;

typedef CALLBACK_API ( OSErr,				WETranslateDragProcPtr )
	(	DragReference		inDrag,
		ItemReference		inDragItem,
		FlavorType			inRequestedType,
		Handle				outData,
		SInt32				inDropOffset,
		WEReference			inWE ) ;

typedef CALLBACK_API ( OSErr,				WEHiliteDropAreaProcPtr )
	(	DragReference		inDrag,
		Boolean				inHiliteFlag,
		WEReference			inWE ) ;

typedef CALLBACK_API ( OSErr,				WEFontIDToNameProcPtr )
	(	SInt16				inFontID,
		Str255				ioFontName ) ;

typedef CALLBACK_API ( OSErr,				WEFontNameToIDProcPtr )
	(	ConstStr255Param	inFontName,
		SInt16				inOldFontID,
		SInt16 *			outNewFontID ) ;

typedef CALLBACK_API ( void,				WEDrawTextProcPtr )
	(	const char *		inTextPtr,
		SInt32				inTextLength,
		Fixed				inSlop,
		JustStyleCode		inStyleRunPosition,
		WEReference			inWE ) ;

typedef CALLBACK_API ( void,				WEDrawTSMHiliteProcPtr )
	(	const Rect *		inSegmentRect,
		SInt16				inHiliteStyle,
		WEReference			inWE ) ;

typedef CALLBACK_API ( SInt32,				WEPixelToCharProcPtr )
	(	const char *		inTextPtr,
		SInt32				inTextLength,
		Fixed				inSlop,
		Fixed *				ioPixelWidth,
		WEEdge *			outEdge,
		JustStyleCode		inStyleRunPosition,
		Fixed				inHorizontalPosition,
		WEReference			inWE ) ;

typedef CALLBACK_API ( SInt16,				WECharToPixelProcPtr )
	(	const char *		inTextPtr,
		SInt32				inTextLength,
		Fixed				inSlop,
		SInt32				inOffset,
		SInt16				inDirection,
		JustStyleCode		inStyleRunPosition,
		SInt16				inHorizontalOffset,
		WEReference			inWE ) ;

typedef CALLBACK_API ( StyledLineBreakCode,	WELineBreakProcPtr )
	(	const char *		inTextPtr,
		SInt32				inTextLength,
		SInt32				inTextStart,
		SInt32				inTextEnd,
		Fixed *				ioTextWidth,
		SInt32 *			ioTextOffset,
		WEReference			inWE ) ;

typedef CALLBACK_API ( void,				WEWordBreakProcPtr )
	(	const char *		inTextPtr,
		SInt16				inTextLength,
		SInt16				inOffset,
		WEEdge				inEdge,
		OffsetTable			outBreakOffsets,
		ScriptCode			inScript,
		WEReference			inWE ) ;

typedef CALLBACK_API ( SInt16,				WECharByteProcPtr )
	(	const char *		inTextPtr,
		SInt16				inTextOffset,
		ScriptCode			inScript,
		WEReference			inWE ) ;

typedef CALLBACK_API ( SInt16,				WECharTypeProcPtr )
	(	const char *		inTextPtr,
		SInt16				inTextOffset,
		ScriptCode			inScript,
		WEReference			inWE ) ;

typedef CALLBACK_API ( void, 				WEEraseProcPtr )
	(	const Rect *		inDirtyRect,
		WEReference			inWE ) ;

typedef CALLBACK_API ( void, 				WEFluxProcPtr )
	(	SInt32				inOffset,
		SInt32				inDelta,
		WEReference			inWE ) ;

typedef CALLBACK_API ( OSErr,				WENewObjectProcPtr )
	(	Point *				outNaturalObjectSize,
		WEObjectReference	inObject ) ;

typedef CALLBACK_API ( OSErr,				WEDisposeObjectProcPtr )
	(	WEObjectReference	inObject ) ;

typedef CALLBACK_API ( OSErr,				WEDrawObjectProcPtr )
	(	const Rect *		inDestRect,
		WEObjectReference	inObject ) ;

typedef CALLBACK_API ( Boolean,				WEClickObjectProcPtr )
	(	Point				inHitPoint,
		EventModifiers		inModifiers,
		UInt32				inClickTime,
		WEObjectReference	inObject ) ;

typedef CALLBACK_API ( OSErr,				WEStreamObjectProcPtr )
	(	SInt16				inDestinationKind,
		FlavorType *		outStreamedFlavorType,
		Handle				outStreamedData,
		WEObjectReference	inObject ) ;

typedef CALLBACK_API ( OSErr,				WEHoverObjectProcPtr )
	(	SInt16				inMouseAction,
		Point				inMouseLoc,
		RgnHandle			inMouseRgn,
		WEObjectReference	inObject ) ;

/*	WASTE callbacks: UPP types */

typedef STACK_UPP_TYPE(WEClickLoopProcPtr)			WEClickLoopUPP;
typedef STACK_UPP_TYPE(WEScrollProcPtr)				WEScrollUPP;
typedef STACK_UPP_TYPE(WETSMPreUpdateProcPtr)		WETSMPreUpdateUPP;
typedef STACK_UPP_TYPE(WETSMPostUpdateProcPtr)		WETSMPostUpdateUPP;
typedef STACK_UPP_TYPE(WEPreTrackDragProcPtr)		WEPreTrackDragUPP;
typedef STACK_UPP_TYPE(WETranslateDragProcPtr)		WETranslateDragUPP;
typedef STACK_UPP_TYPE(WEHiliteDropAreaProcPtr)		WEHiliteDropAreaUPP;
typedef STACK_UPP_TYPE(WEFontIDToNameProcPtr)		WEFontIDToNameUPP;
typedef STACK_UPP_TYPE(WEFontNameToIDProcPtr)		WEFontNameToIDUPP;
typedef STACK_UPP_TYPE(WEDrawTextProcPtr)			WEDrawTextUPP;
typedef STACK_UPP_TYPE(WEDrawTSMHiliteProcPtr)		WEDrawTSMHiliteUPP;
typedef STACK_UPP_TYPE(WEPixelToCharProcPtr)		WEPixelToCharUPP;
typedef STACK_UPP_TYPE(WECharToPixelProcPtr)		WECharToPixelUPP;
typedef STACK_UPP_TYPE(WELineBreakProcPtr)			WELineBreakUPP;
typedef STACK_UPP_TYPE(WEWordBreakProcPtr)			WEWordBreakUPP;
typedef STACK_UPP_TYPE(WECharByteProcPtr)			WECharByteUPP;
typedef STACK_UPP_TYPE(WECharTypeProcPtr)			WECharTypeUPP;
typedef STACK_UPP_TYPE(WEEraseProcPtr)				WEEraseUPP;
typedef STACK_UPP_TYPE(WEFluxProcPtr)				WEFluxUPP;
typedef STACK_UPP_TYPE(WENewObjectProcPtr)			WENewObjectUPP;
typedef STACK_UPP_TYPE(WEDisposeObjectProcPtr)		WEDisposeObjectUPP;
typedef STACK_UPP_TYPE(WEDrawObjectProcPtr)			WEDrawObjectUPP;
typedef STACK_UPP_TYPE(WEClickObjectProcPtr)		WEClickObjectUPP;
typedef STACK_UPP_TYPE(WEStreamObjectProcPtr)		WEStreamObjectUPP;
typedef STACK_UPP_TYPE(WEHoverObjectProcPtr)		WEHoverObjectUPP;

/*	WASTE callbacks: New*Proc macros */
/*	These macros used to expand to NewRoutineDescriptor in WASTE 1.x */
/*	In WASTE 2.0, they are still available for compatibility with old code, */
/*	but they expand to New*UPP, which are real entry points, not macros */

#define NewWEClickLoopProc							NewWEClickLoopUPP
#define NewWEScrollProc								NewWEScrollUPP
#define NewWETSMPreUpdateProc						NewWETSMPreUpdateUPP
#define NewWETSMPostUpdateProc						NewWETSMPostUpdateUPP
#define NewWEPreTrackDragProc						NewWEPreTrackDragUPP
#define NewWETranslateDragProc						NewWETranslateDragUPP
#define NewWEHiliteDropAreaProc						NewWEHiliteDropAreaUPP
#define NewWEFontIDToNameProc						NewWEFontIDToNameUPP
#define NewWEFontNameToIDProc						NewWEFontNameToIDUPP
#define NewWEDrawTextProc							NewWEDrawTextUPP
#define NewWEDrawTSMHiliteProc						NewWEDrawTSMHiliteUPP
#define NewWEPixelToCharProc						NewWEPixelToCharUPP
#define NewWECharToPixelProc						NewWECharToPixelUPP
#define NewWELineBreakProc							NewWELineBreakUPP
#define NewWEWordBreakProc							NewWEWordBreakUPP
#define NewWECharByteProc							NewWECharByteUPP
#define NewWECharTypeProc							NewWECharTypeUPP
#define NewWEEraseProc								NewWEEraseUPP
#define NewWEFluxProc								NewWEFluxUPP
#define NewWENewObjectProc							NewWENewObjectUPP
#define NewWEDisposeObjectProc						NewWEDisposeObjectUPP
#define NewWEDrawObjectProc							NewWEDrawObjectUPP
#define NewWEClickObjectProc						NewWEClickObjectUPP
#define NewWEStreamObjectProc						NewWEStreamObjectUPP
#define NewWEHoverObjectProc						NewWEHoverObjectUPP

/*	WASTE callbacks: Call*Proc macros */
/*	These macros used to expand to CallUniversalProc in WASTE 1.x */
/*	In WASTE 2.0, they are still available for compatibility with old code, */
/*	but they expand to Invoke*UPP, which are real entry points, not macros */

#define CallWEClickLoopProc							InvokeWEClickLoopUPP
#define CallWEScrollProc							InvokeWEScrollUPP
#define CallWETSMPreUpdateProc						InvokeWETSMPreUpdateUPP
#define CallWETSMPostUpdateProc						InvokeWETSMPostUpdateUPP
#define CallWEPreTrackDragProc						InvokeWEPreTrackDragUPP
#define CallWETranslateDragProc						InvokeWETranslateDragUPP
#define CallWEHiliteDropAreaProc					InvokeWEHiliteDropAreaUPP
#define CallWEFontIDToNameProc						InvokeWEFontIDToNameUPP
#define CallWEFontNameToIDProc						InvokeWEFontNameToIDUPP
#define CallWEDrawTextProc							InvokeWEDrawTextUPP
#define CallWEDrawTSMHiliteProc						InvokeWEDrawTSMHiliteUPP
#define CallWEPixelToCharProc						InvokeWEPixelToCharUPP
#define CallWECharToPixelProc						InvokeWECharToPixelUPP
#define CallWELineBreakProc							InvokeWELineBreakUPP
#define CallWEWordBreakProc							InvokeWEWordBreakUPP
#define CallWECharByteProc							InvokeWECharByteUPP
#define CallWECharTypeProc							InvokeWECharTypeUPP
#define CallWEEraseProc								InvokeWEEraseUPP
#define CallWEFluxProc								InvokeWEFluxUPP
#define CallWENewObjectProc							InvokeWENewObjectUPP
#define CallWEDisposeObjectProc						InvokeWEDisposeObjectUPP
#define CallWEDrawObjectProc						InvokeWEDrawObjectUPP
#define CallWEClickObjectProc						InvokeWEClickObjectUPP
#define CallWEStreamObjectProc						InvokeWEStreamObjectUPP
#define CallWEHoverObjectProc						InvokeWEHoverObjectUPP

/*	WASTE public calls */

/*	getting the shared library version number */

EXTERN_API ( UInt32 )
WEVersion (						void ) ;

/*	creating UPPs for callback functions */

EXTERN_API ( WEClickLoopUPP )
NewWEClickLoopUPP (				WEClickLoopProcPtr		inProcPtr ) ;

EXTERN_API ( WEScrollUPP )
NewWEScrollUPP (				WEScrollProcPtr			inProcPtr ) ;

EXTERN_API ( WETSMPreUpdateUPP )
NewWETSMPreUpdateUPP (			WETSMPreUpdateProcPtr	inProcPtr ) ;

EXTERN_API ( WETSMPostUpdateUPP )
NewWETSMPostUpdateUPP (			WETSMPostUpdateProcPtr	inProcPtr ) ;

EXTERN_API ( WEPreTrackDragUPP )
NewWEPreTrackDragUPP (			WEPreTrackDragProcPtr	inProcPtr ) ;

EXTERN_API ( WETranslateDragUPP )
NewWETranslateDragUPP (			WETranslateDragProcPtr	inProcPtr ) ;

EXTERN_API ( WEHiliteDropAreaUPP )
NewWEHiliteDropAreaUPP (		WEHiliteDropAreaProcPtr	inProcPtr ) ;

EXTERN_API ( WEFontIDToNameUPP )
NewWEFontIDToNameUPP (			WEFontIDToNameProcPtr	inProcPtr ) ;

EXTERN_API ( WEFontNameToIDUPP )
NewWEFontNameToIDUPP (			WEFontNameToIDProcPtr	inProcPtr ) ;

EXTERN_API ( WEDrawTextUPP )
NewWEDrawTextUPP (				WEDrawTextProcPtr		inProcPtr ) ;

EXTERN_API ( WEDrawTSMHiliteUPP )
NewWEDrawTSMHiliteUPP (			WEDrawTSMHiliteProcPtr	inProcPtr ) ;

EXTERN_API ( WEPixelToCharUPP )
NewWEPixelToCharUPP (			WEPixelToCharProcPtr	inProcPtr ) ;

EXTERN_API ( WECharToPixelUPP )
NewWECharToPixelUPP (			WECharToPixelProcPtr	inProcPtr ) ;

EXTERN_API ( WELineBreakUPP )
NewWELineBreakUPP (				WELineBreakProcPtr		inProcPtr ) ;

EXTERN_API ( WEWordBreakUPP )
NewWEWordBreakUPP (				WEWordBreakProcPtr		inProcPtr ) ;

EXTERN_API ( WECharByteUPP )
NewWECharByteUPP (				WECharByteProcPtr		inProcPtr ) ;

EXTERN_API ( WECharTypeUPP )
NewWECharTypeUPP (				WECharTypeProcPtr		inProcPtr ) ;

EXTERN_API ( WEEraseUPP )
NewWEEraseUPP (					WEEraseProcPtr			inProcPtr ) ;

EXTERN_API ( WEFluxUPP )
NewWEFluxUPP (					WEFluxProcPtr			inProcPtr ) ;

EXTERN_API ( WENewObjectUPP )
NewWENewObjectUPP (				WENewObjectProcPtr		inProcPtr ) ;

EXTERN_API ( WEDisposeObjectUPP )
NewWEDisposeObjectUPP (			WEDisposeObjectProcPtr	inProcPtr ) ;

EXTERN_API ( WEDrawObjectUPP )
NewWEDrawObjectUPP (			WEDrawObjectProcPtr		inProcPtr ) ;

EXTERN_API ( WEClickObjectUPP )
NewWEClickObjectUPP (			WEClickObjectProcPtr	inProcPtr ) ;

EXTERN_API ( WEStreamObjectUPP )
NewWEStreamObjectUPP (			WEStreamObjectProcPtr	inProcPtr ) ;

EXTERN_API ( WEHoverObjectUPP )
NewWEHoverObjectUPP (			WEHoverObjectProcPtr	inProcPtr ) ;

/*	destroying UPPs for callback functions */

EXTERN_API ( void )
DisposeWEClickLoopUPP (			WEClickLoopUPP			inUPP ) ;

EXTERN_API ( void )
DisposeWEScrollUPP (			WEScrollUPP				inUPP ) ;

EXTERN_API ( void )
DisposeWETSMPreUpdateUPP (		WETSMPreUpdateUPP		inUPP ) ;

EXTERN_API ( void )
DisposeWETSMPostUpdateUPP (		WETSMPostUpdateUPP		inUPP ) ;

EXTERN_API ( void )
DisposeWEPreTrackDragUPP (		WEPreTrackDragUPP		inUPP ) ;

EXTERN_API ( void )
DisposeWETranslateDragUPP (		WETranslateDragUPP		inUPP ) ;

EXTERN_API ( void )
DisposeWEHiliteDropAreaUPP (	WEHiliteDropAreaUPP		inUPP ) ;

EXTERN_API ( void )
DisposeWEFontIDToNameUPP (		WEFontIDToNameUPP		inUPP ) ;

EXTERN_API ( void )
DisposeWEFontNameToIDUPP (		WEFontNameToIDUPP		inUPP ) ;

EXTERN_API ( void )
DisposeWEDrawTextUPP (			WEDrawTextUPP			inUPP ) ;

EXTERN_API ( void )
DisposeWEDrawTSMHiliteUPP (		WEDrawTSMHiliteUPP		inUPP ) ;

EXTERN_API ( void )
DisposeWEPixelToCharUPP (		WEPixelToCharUPP		inUPP ) ;

EXTERN_API ( void )
DisposeWECharToPixelUPP (		WECharToPixelUPP		inUPP ) ;

EXTERN_API ( void )
DisposeWELineBreakUPP (			WELineBreakUPP			inUPP ) ;

EXTERN_API ( void )
DisposeWEWordBreakUPP (			WEWordBreakUPP			inUPP ) ;

EXTERN_API ( void )
DisposeWECharByteUPP (			WECharByteUPP			inUPP ) ;

EXTERN_API ( void )
DisposeWECharTypeUPP (			WECharTypeUPP			inUPP ) ;

EXTERN_API ( void )
DisposeWEEraseUPP (				WEEraseUPP				inUPP ) ;

EXTERN_API ( void )
DisposeWEFluxUPP (				WEFluxUPP				inUPP ) ;

EXTERN_API ( void )
DisposeWENewObjectUPP (			WENewObjectUPP			inUPP ) ;

EXTERN_API ( void )
DisposeWEDisposeObjectUPP (		WEDisposeObjectUPP		inUPP ) ;

EXTERN_API ( void )
DisposeWEDrawObjectUPP (		WEDrawObjectUPP			inUPP ) ;

EXTERN_API ( void )
DisposeWEClickObjectUPP (		WEClickObjectUPP		inUPP ) ;

EXTERN_API ( void )
DisposeWEStreamObjectUPP (		WEStreamObjectUPP		inUPP ) ;

EXTERN_API ( void )
DisposeWEHoverObjectUPP (		WEHoverObjectUPP		inUPP ) ;

/*	invoking callback functions through UPPs */

EXTERN_API ( Boolean )
InvokeWEClickLoopUPP (			WEReference				inWE,
								WEClickLoopUPP			inUPP ) ;

EXTERN_API ( void )
InvokeWEScrollUPP (				WEReference				inWE,
								WEScrollUPP				inUPP ) ;

EXTERN_API ( void )
InvokeWETSMPreUpdateUPP (		WEReference				inWE,
								WETSMPreUpdateUPP		inUPP ) ;

EXTERN_API ( void )
InvokeWETSMPostUpdateUPP (		WEReference				inWE,
								SInt32					inFixLength,
								SInt32					inInputAreaStart,
								SInt32					inInputAreaEnd,
								SInt32					inPinRangeStart,
								SInt32					inPinRangeEnd,
								WETSMPostUpdateUPP		inUPP ) ;

EXTERN_API ( OSErr )
InvokeWEPreTrackDragUPP (		DragReference			inDrag,
								WEReference				inWE,
								WEPreTrackDragUPP		inUPP ) ;

EXTERN_API ( OSErr )
InvokeWETranslateDragUPP (		DragReference			inDrag,
								ItemReference			inDragItem,
								FlavorType				inRequestedType,
								Handle					outData,
								SInt32					inDropOffset,
								WEReference				inWE,
								WETranslateDragUPP		inUPP ) ;

EXTERN_API ( OSErr )
InvokeWEHiliteDropAreaUPP (		DragReference			inDrag,
								Boolean					inHiliteFlag,
								WEReference				inWE,
								WEHiliteDropAreaUPP		inUPP ) ;

EXTERN_API ( OSErr )
InvokeWEFontIDToNameUPP (		SInt16					inFontID,
								StringPtr				ioFontName,
								WEFontIDToNameUPP		inUPP ) ;

EXTERN_API ( OSErr )
InvokeWEFontNameToIDUPP (		ConstStr255Param		inFontName,
								SInt16					inOldFontID,
								SInt16 *				outNewFontID,
								WEFontNameToIDUPP		inUPP ) ;

EXTERN_API ( void )
InvokeWEDrawTextUPP (			const char *			inTextPtr,
								SInt32					inTextLength,
								Fixed					inSlop,
								JustStyleCode			inStyleRunPosition,
								WEReference				inWE,
								WEDrawTextUPP			inUPP ) ;

EXTERN_API ( void )
InvokeWEDrawTSMHiliteUPP (		const Rect *			inSegmentRect,
								SInt16					inHiliteStyle,
								WEReference				inWE,
								WEDrawTSMHiliteUPP		inUPP ) ;

EXTERN_API ( SInt32 )
InvokeWEPixelToCharUPP (		const char *			inTextPtr,
								SInt32					inTextLength,
								Fixed					inSlop,
								Fixed *					ioPixelWidth,
								WEEdge *				outEdge,
								JustStyleCode			inStyleRunPosition,
								Fixed					inHorizontalPosition,
								WEReference				inWE,
								WEPixelToCharUPP		inUPP ) ;

EXTERN_API ( SInt16 )
InvokeWECharToPixelUPP (		const char *			inTextPtr,
								SInt32					inTextLength,
								Fixed					inSlop,
								SInt32					inOffset,
								SInt16					inDirection,
								JustStyleCode			inStyleRunPosition,
								SInt16					inHorizontalPosition,
								WEReference				inWE,
								WECharToPixelUPP		inUPP ) ;

EXTERN_API ( StyledLineBreakCode )
InvokeWELineBreakUPP (			const char *			inTextPtr,
								SInt32					inTextLength,
								SInt32					inTextStart,
								SInt32					inTextEnd,
								Fixed *					ioTextWidth,
								SInt32 *				ioTextOffset,
								WEReference				inWE,
								WELineBreakUPP			inUPP ) ;

EXTERN_API ( void )
InvokeWEWordBreakUPP (			const char *			inTextPtr,
								SInt16					inTextLength,
								SInt16					inOffset,
								WEEdge					inEdge,
								OffsetTable				outBreakOffsets,
								ScriptCode				inScript,
								WEReference				inWE,
								WEWordBreakUPP			inUPP ) ;

EXTERN_API ( SInt16 )
InvokeWECharByteUPP (			const char *			inTextPtr,
								SInt16					inTextOffset,
								ScriptCode				inScript,
								WEReference				inWE,
								WECharByteUPP			inUPP ) ;

EXTERN_API ( SInt16 )
InvokeWECharTypeUPP (			const char *			inTextPtr,
								SInt16					inTextOffset,
								ScriptCode				inScript,
								WEReference				inWE,
								WECharTypeUPP			inUPP ) ;

EXTERN_API ( void )
InvokeWEEraseUPP (				const Rect *			inDirtyRect,
								WEReference				inWE,
								WEEraseUPP				inUPP ) ;

EXTERN_API ( void )
InvokeWEFluxUPP (				SInt32					inOffset,
								SInt32					inDelta,
								WEReference				inWE,
								WEFluxUPP				inUPP ) ;

EXTERN_API ( OSErr )
InvokeWENewObjectUPP (			SInt32 *				outNaturalObjectSize,
								WEObjectReference		inObject,
								WENewObjectUPP			inUPP ) ;

EXTERN_API ( OSErr )
InvokeWEDisposeObjectUPP (		WEObjectReference		inObject,
								WEDisposeObjectUPP		inUPP ) ;

EXTERN_API ( OSErr )
InvokeWEDrawObjectUPP (			const Rect *			inDestRect,
								WEObjectReference		inObject,
								WEDrawObjectUPP			inUPP ) ;

EXTERN_API ( Boolean )
InvokeWEClickObjectUPP (		Point					inHitPoint,
								EventModifiers			inModifiers,
								UInt32					inClickTime,
								WEObjectReference		inObject,
								WEClickObjectUPP		inUPP ) ;

EXTERN_API ( OSErr )
InvokeWEStreamObjectUPP (		SInt16					inDestinationKind,
								FlavorType *			outStreamedFlavorType,
								Handle					outStreamedData,
								WEObjectReference		inObject,
								WEStreamObjectUPP		inUPP ) ;

EXTERN_API ( OSErr )
InvokeWEHoverObjectUPP (		SInt16					inMouseAction,
								Point					inMouseLoc,
								RgnHandle				inMouseRgn,
								WEObjectReference		inObject,
								WEHoverObjectUPP		inUPP ) ;

/*	creation and destruction */

EXTERN_API ( OSErr )
WENew (							const LongRect *		inDestRect,
								const LongRect *		inViewRect,
								OptionBits				inOptions,
								WEReference *			outWE ) ;

EXTERN_API ( void )
WEDispose (						WEReference				inWE ) ;

/*	getting variables */

EXTERN_API ( Handle )
WEGetText (						WEReference 			inWE ) ;

EXTERN_API ( SInt16 )
WEGetChar ( 					SInt32					inOffset,
								WEReference 			inWE ) ;

EXTERN_API ( SInt32 )
WEGetTextLength ( 				WEReference 			inWE ) ;

EXTERN_API ( void )
WEGetSelection (				SInt32 *				outSelStart,
								SInt32 *				outSelEnd,
								WEReference				inWE ) ;

EXTERN_API ( void )
WEGetDestRect (					LongRect *				outDestRect,
								WEReference 			inWE ) ;

EXTERN_API ( void )
WEGetViewRect (					LongRect *				outViewRect,
								WEReference 			inWE ) ;

EXTERN_API ( Boolean )
WEIsActive (					WEReference 			inWE ) ;

EXTERN_API ( UInt16 )
WEGetClickCount (				WEReference 			inWE ) ;

/*	setting variables */

EXTERN_API ( void )
WESetSelection (				SInt32					inSelStart,
								SInt32					inSelEnd,
								WEReference				inWE ) ;

EXTERN_API ( void )
WESetDestRect (					const LongRect *		inDestRect,
								WEReference 			inWE ) ;

EXTERN_API ( void )
WESetViewRect (					const LongRect *		inViewRect,
								WEReference				inWE ) ;

/*	getting style attributes */

EXTERN_API ( OSErr )
WEGetAttributes (				SInt32					inOffset,
								ItemCount				inAttributeCount,
								const WESelector		inAttributeSelectors [ ],
								void * const			outAttributeValues [ ],
								const ByteCount			inAttributeValueSizes [ ],
								WEReference				inWE ) ;

EXTERN_API ( OSErr )
WEGetOneAttribute (				SInt32					inOffset,
								WESelector				inAttributeSelector,
								void *					outAttributeValue,
								ByteCount				inAttributeValueSize,
								WEReference				inWE ) ;

/*	determining which style attributes are continuous over the selection range */

EXTERN_API ( Boolean )
WEContinuousStyle (				WEStyleMode *			ioMode,
								TextStyle *				outTextStyle,
								WEReference				inWE ) ;

/*	determining which paragraph attributes are continuous over the selection range */

EXTERN_API ( Boolean )
WEContinuousRuler (				WERulerMode *			ioMode,
								WERuler *				outRuler,
								WEReference				inWE ) ;

/*	matching attributes over a text range against an arbitrary set of values */

EXTERN_API ( OSErr )
WEMatchAttributes (				SInt32					inRangeStart,
								SInt32					inRangeEnd,
								WESelector				inAttributeSelector,
								ByteCount				inAttributeValueSize,
								ItemCount				inArraySize,
								const void *			inValueArray,
								Boolean					outWhichValuesArePresent [ ],
								Boolean *				outIsContinuous,
								WEReference				inWE ) ;

/*	low-level access to style run information */

EXTERN_API ( SInt32 )
WECountRuns (					WEReference 			inWE ) ;

EXTERN_API ( SInt32 )
WEOffsetToRun (					SInt32					inOffset,
								WEReference				inWE ) ;

EXTERN_API ( void )
WEGetRunRange (					SInt32					inStyleRunIndex,
								SInt32 *				outStyleRunStart,
								SInt32 *				outStyleRunEnd,
								WEReference				inWE ) ;

EXTERN_API ( void )
WEGetRunInfo (					SInt32					inOffset,
								WERunInfo *				outStyleRunInfo,
								WEReference				inWE ) ;

EXTERN_API ( void )
WEGetIndRunInfo (				SInt32					inStyleRunIndex,
								WERunInfo *				outStyleRunInfo,
								WEReference				inWE ) ;

EXTERN_API ( Boolean )
WEGetRunDirection (				SInt32					inOffset,
								WEReference 			inWE ) ;

/*	low-level access to paragraph run information */

EXTERN_API ( SInt32 )
WECountParaRuns (				WEReference				inWE ) ;

EXTERN_API ( SInt32 )
WEOffsetToParaRun (				SInt32					inOffset,
								WEReference				inWE ) ;

EXTERN_API ( void )
WEGetParaRunRange (				SInt32					inParagraphRunIndex,
								SInt32 *				outParagraphRunStart,
								SInt32 *				outParagraphRunEnd,
								WEReference				inWE ) ;

EXTERN_API ( void )
WEGetParaInfo (					SInt32					inOffset,
								WEParaInfo *			outParagraphRunInfo,
								WEReference				inWE ) ;

EXTERN_API ( void )
WEGetIndParaInfo (				SInt32					inParagraphRunIndex,
								WEParaInfo *			outPararaphRunInfo,
								WEReference				inWE ) ;

/*	access to line layout information */

EXTERN_API ( SInt32 )
WECountLines (					WEReference 			inWE ) ;

EXTERN_API ( SInt32 )
WEOffsetToLine (				SInt32 					inOffset,
								WEReference 			inWE ) ;

EXTERN_API ( void )
WEGetLineRange (				SInt32					inLineIndex,
								SInt32 *				outLineStart,
								SInt32 *				outLineEnd,
								WEReference				inWE ) ;

EXTERN_API ( SInt32 )
WEGetHeight (					SInt32					inStartLineIndex,
								SInt32					inEndLineIndex,
								WEReference 			inWE ) ;

/*	converting byte offsets to screen position and vice versa */

EXTERN_API ( SInt32 )
WEGetOffset (					const LongPt *			inPoint,
								WEEdge *				outEdge,
								WEReference				inWE ) ;

EXTERN_API ( void )
WEGetPoint (					SInt32					inOffset,
								SInt16					inDirection,
								LongPt *				outPoint,
								SInt16 *				outLineHeight,
								WEReference				inWE ) ;

/*	finding words, lines and paragraphs */

EXTERN_API ( void )
WEFindWord (					SInt32					inOffset,
								WEEdge					inEdge,
								SInt32 *				outWordStart,
								SInt32 *				outWordEnd,
								WEReference				inWE ) ;

EXTERN_API ( void )
WEFindLine (					SInt32					inOffset,
								WEEdge					inEdge,
								SInt32 *				outLineStart,
								SInt32 *				outLineEnd,
								WEReference				inWE ) ;

EXTERN_API ( void )
WEFindParagraph (				SInt32					inOffset,
								WEEdge					inEdge,
								SInt32 *				outParagraphStart,
								SInt32 *				outParagraphEnd,
								WEReference				inWE ) ;

/*	matching strings */

EXTERN_API ( OSErr )
WEFind (						const char *			inKey,
								SInt32					inKeyLength,
								TextEncoding			inKeyEncoding,
								OptionBits				inMatchOptions,
								SInt32					inRangeStart,
								SInt32					inRangeEnd,
								SInt32 *				outMatchStart,
								SInt32 *				outMatchEnd,
								WEReference				inWE ) ;

/*	making a copy of a text range */

EXTERN_API ( OSErr )
WEStreamRange (					SInt32					inRangeStart,
								SInt32					inRangeEnd,
								FlavorType				inRequestedType,
								OptionBits				inStreamOptions,
								Handle					outData,
								WEReference				inWE ) ;

EXTERN_API ( OSErr )
WECopyRange (					SInt32					inRangeStart,
								SInt32					inRangeEnd,
								Handle					outText,
								StScrpHandle			outStyles,
								WESoupHandle			outSoup,
								WEReference				inWE ) ;

EXTERN_API ( OSErr )
WEGetTextRangeAsUnicode (		SInt32					inRangeStart,
								SInt32					inRangeEnd,
								Handle					outUnicodeText,
								Handle					ioCharFormat,
								Handle					ioParaFormat,
								TextEncodingVariant		inUnicodeVariant,
								TextEncodingFormat		inTransformationFormat,
								OptionBits				inGetOptions,
								WEReference				inWE ) ;

/*	getting and setting the alignment style */

EXTERN_API ( WEAlignment )
WEGetAlignment (				WEReference				inWE ) ;

EXTERN_API ( void )
WESetAlignment (				WEAlignment				inAlignment,
								WEReference				inWE ) ;

/*	getting and setting the primary line direction */

EXTERN_API ( WEDirection )
WEGetDirection (				WEReference				inWE ) ;

EXTERN_API( void )
WESetDirection (				WEDirection				inDirection,
								WEReference				inWE ) ;

/*	recalculating line breaks, drawing and scrolling */

EXTERN_API ( OSErr )
WECalText (						WEReference 			inWE ) ;

EXTERN_API ( void )
WEUpdate (						RgnHandle				inUpdateRgn,
								WEReference 			inWE ) ;

EXTERN_API ( void )
WEScroll (						SInt32					inHorizontalOffset,
								SInt32					inVerticalOffset,
								WEReference				inWE ) ;

EXTERN_API ( void )
WEPinScroll (					SInt32					inHorizontalOffset,
								SInt32					inVerticalOffset,
								WEReference				inWE ) ;

EXTERN_API ( void )
WESelView (						WEReference				inWE ) ;

/*	handling activate / deactivate events */

EXTERN_API ( void )
WEActivate (					WEReference				inWE ) ;

EXTERN_API ( void )
WEDeactivate (					WEReference 			inWE ) ;

/* 	handling key-down events */

EXTERN_API ( void )
WEKey (							CharParameter			inKey,
								EventModifiers			inModifiers,
								WEReference				inWE ) ;

/*	handling mouse-down events and mouse tracking */

EXTERN_API ( void )
WEClick (						Point					inHitPoint,
								EventModifiers			inModifiers,
								UInt32					inClickTime,
								WEReference				inWE ) ;

/*	adjusting the cursor shape */

EXTERN_API ( Boolean )
WEAdjustCursor (				Point					inMouseLoc,
								RgnHandle				ioMouseRgn,
								WEReference				inWE ) ;

/*	blinking the caret */

EXTERN_API ( void )
WEIdle (						UInt32 *				outMaxSleep,
								WEReference				inWE ) ;

/*	modifying the text and the styles */

EXTERN_API ( OSErr )
WEPut (							SInt32					inRangeStart,
								SInt32					inRangeEnd,
								const void *			inTextPtr,
								SInt32					inTextLength,
								TextEncoding			inTextEncoding,
								OptionBits				inPutOptions,
								ItemCount				inFlavorCount,
								const FlavorType *		inFlavorTypes,
								const Handle *			inFlavorHandles,
								WEReference				inWE ) ;

EXTERN_API ( OSErr )
WEInsert (						const void *			inTextPtr,
								SInt32					inTextLength,
								StScrpHandle			inStyles,
								WESoupHandle			inSoup,
								WEReference				inWE ) ;

EXTERN_API ( OSErr )
WEInsertFormattedText (			const void *			inTextPtr,
								SInt32					inTextLength,
								StScrpHandle			inStyles,
								WESoupHandle			inSoup,
								Handle					inParaFormat,
								Handle					inRulerScrap,
								WEReference				inWE ) ;

EXTERN_API ( OSErr )
WEDelete (						WEReference				inWE ) ;

EXTERN_API ( OSErr )
WEUseText (						Handle					inText,
								WEReference				inWE ) ;

EXTERN_API ( OSErr )
WEChangeCase (					SInt16					inCase,
								WEReference				inWE ) ;

/*	modifying text attributes */

EXTERN_API ( OSErr )
WESetAttributes (				SInt32					inRangeStart,
								SInt32					inRangeEnd,
								ItemCount				inAttributeCount,
								const WESelector		inAttributeSelectors [ ],
								const void * const		inAttributeValues [ ],
								const ByteCount			inAttributeValueSizes [ ],
								WEReference				inWE ) ;

EXTERN_API ( OSErr )
WESetOneAttribute (				SInt32					inRangeStart,
								SInt32					inRangeEnd,
								WESelector				inAttributeSelector,
								const void *			inAttributeValue,
								ByteCount				inAttributeValueSize,
								WEReference				inWE ) ;

EXTERN_API ( OSErr )
WESetStyle (					WEStyleMode				inMode,
								const TextStyle *		inTextStyle,
								WEReference				inWE ) ;

EXTERN_API ( OSErr )
WESetRuler (					WERulerMode				inMode,
								const WERuler *			inRuler,
								WEReference				inWE ) ;

EXTERN_API ( OSErr )
WEUseStyleScrap (				StScrpHandle			inStyles,
								WEReference				inWE ) ;

/*	undo */

EXTERN_API ( OSErr )
WEUndo (						WEReference				inWE ) ;

EXTERN_API ( OSErr )
WERedo (						WEReference				inWE ) ;

EXTERN_API ( void )
WEClearUndo (					WEReference				inWE ) ;

EXTERN_API ( WEActionKind )
WEGetUndoInfo (					Boolean *				outRedoFlag,
								WEReference 			inWE ) ;

EXTERN_API ( WEActionKind )
WEGetIndUndoInfo (				SInt32					inUndoLevel,
								WEReference				inWE ) ;

EXTERN_API ( Boolean )
WEIsTyping (					WEReference				inWE ) ;

EXTERN_API ( OSErr )
WEBeginAction (					WEReference				inWE ) ;

EXTERN_API ( OSErr )
WEEndAction (					WEActionKind			inActionKind,
								WEReference				inWE ) ;

/*	keeping track of changes */

EXTERN_API ( UInt32 )
WEGetModCount (					WEReference				inWE ) ;

EXTERN_API ( void )
WEResetModCount (				WEReference				inWE ) ;

/*	embedded objects */

EXTERN_API ( OSErr )
WEInstallObjectHandler (		FlavorType				inObjectType,
								WESelector				inHandlerSelector,
								UniversalProcPtr		inHandler,
								WEReference				inWE ) ;

EXTERN_API ( OSErr )
WERemoveObjectHandler (			FlavorType				inObjectType,
								WESelector				inHandlerSelector,
								UniversalProcPtr		inHandler,
								WEReference				inWE ) ;

EXTERN_API( OSErr )
WEGetObjectHandler (			FlavorType				inObjecType,
								WESelector				inHandlerSelector,
								UniversalProcPtr *		outHandler,
								WEReference				inWE ) ;

EXTERN_API ( OSErr )
WEInsertObject (				FlavorType				inObjectType,
								Handle					inObjectDataHandle,
								Point					inObjectSize,
								WEReference				inWE ) ;

EXTERN_API ( OSErr )
WEGetSelectedObject (			WEObjectReference *		outObject,
								WEReference				inWE ) ;

EXTERN_API ( OSErr )
WEGetObjectAtOffset (			SInt32					inOffset,
								WEObjectReference *		outObject,
								WEReference				inWE ) ;

EXTERN_API ( SInt32 )
WEFindNextObject (				SInt32					inOffset,
								WEObjectReference *		outObject,
								WEReference				inWE ) ;

EXTERN_API ( OSErr )
WEUseSoup (						WESoupHandle			inSoup,
								WEReference				inWE ) ;

/*	accessing embedded object attributes */

EXTERN_API ( FlavorType )
WEGetObjectType (				WEObjectReference		inObject ) ;

EXTERN_API ( Handle )
WEGetObjectDataHandle (			WEObjectReference		inObject ) ;

EXTERN_API ( WEReference )
WEGetObjectOwner (				WEObjectReference		inObject ) ;

EXTERN_API ( SInt32 )
WEGetObjectOffset (				WEObjectReference		inObject ) ;

EXTERN_API ( Point )
WEGetObjectSize (				WEObjectReference		inObject ) ;

EXTERN_API ( OSErr )
WESetObjectSize (				WEObjectReference		inObject,
								Point					inObjectSize ) ;

EXTERN_API ( OSErr )
WEGetObjectFrame (				WEObjectReference		inObject,
								LongRect *				outObjectFrame ) ;

EXTERN_API ( SInt32 )
WEGetObjectRefCon (				WEObjectReference		inObject ) ;

EXTERN_API ( void )
WESetObjectRefCon (				WEObjectReference		inObject,
								SInt32					inRefCon ) ;

/*	clipboard operations */

EXTERN_API ( OSErr )
WECut (							WEReference				inWE ) ;

EXTERN_API ( OSErr )
WECopy (						WEReference				inWE ) ;

EXTERN_API ( OSErr )
WEPaste (						WEReference				inWE ) ;

EXTERN_API ( Boolean )
WECanPaste (					WEReference				inWE ) ;

/*	Drag Manager support */

EXTERN_API ( RgnHandle )
WEGetHiliteRgn (				SInt32					inRangeStart,
								SInt32					inRangeEnd,
								WEReference				inWE ) ;

EXTERN_API( OSErr )
WETrackDrag (					DragTrackingMessage		inMessage,
								DragReference			inDrag,
								WEReference				inWE ) ;

EXTERN_API ( OSErr )
WEReceiveDrag (					DragReference			inDrag,
								WEReference 			inWE ) ;

EXTERN_API ( Boolean )
WECanAcceptDrag (				DragReference			inDrag,
								WEReference				inWE ) ;

EXTERN_API ( Boolean )
WEDraggedToTrash (				DragReference			inDrag ) ;

/*	font tables */

EXTERN_API ( OSErr )
WEBuildFontTable (				WEFontTableHandle		outFontTable,
								WEFontIDToNameUPP		inFontIDToNameProc,
								WEReference				inWE ) ;

EXTERN_API ( OSErr )
WEUpdateFontTable (				WEFontTableHandle		ioFontTable,
								WEFontNameToIDUPP		inFontNameToIDProc,
								Boolean *				outWasChanged ) ;

EXTERN_API ( OSErr )
WEUpdateStyleScrap (			StScrpHandle			ioStyles,
								WEFontTableHandle		inFontTable ) ;

/*	Script Manager utilities */

EXTERN_API ( SInt16 )
WECharByte (					SInt32					inOffset,
								WEReference				inWE ) ;

EXTERN_API( SInt16 )
WECharType (					SInt32					inOffset,
								WEReference				inWE ) ;

/*	Text Services Manager support */

EXTERN_API ( OSErr )
WEInstallTSMHandlers ( 			void ) ;

EXTERN_API ( OSErr )
WERemoveTSMHandlers (			void ) ;

EXTERN_API ( OSErr )
WEHandleTSMEvent (				const AppleEvent *		inAppleEvent,
								AppleEvent *			ioReply ) ;

EXTERN_API ( void )
WEStopInlineSession (			WEReference				inWE ) ;

/*	printing support */

EXTERN_API ( OSErr )
WENewPrintSession (				const WEPrintOptions *	inPrintOptions,
								WEReference				inWE,
								WEPrintSession *		outPrintSession ) ;

EXTERN_API ( void )
WEDisposePrintSession (			WEPrintSession			inPrintSession ) ;

EXTERN_API ( SInt32 )
WECountPages (					WEPrintSession			inPrintSession ) ;

EXTERN_API ( OSErr )
WEPrintPage (					SInt32					inPageIndex,
								GrafPtr					inPrintPort,
								const Rect *			inPageRect,
								WEPrintSession			inPrintSession ) ;

/*	additional features */

EXTERN_API ( SInt16 )
WEFeatureFlag (					SInt16					inFeature,
								SInt16					inAction,
								WEReference				inWE ) ;

EXTERN_API ( OSErr )
WEGetInfo (						WESelector				inSelector,
								void *					outInfo,
								WEReference				inWE ) ;

EXTERN_API ( OSErr )
WESetInfo (						WESelector				inSelector,
								const void *			inInfo,
								WEReference				inWE ) ;

/*	getting and setting user-defined info */

EXTERN_API ( OSErr )
WEGetUserInfo (					WESelector				inUserTag,
								SInt32 *				outUserInfo,
								WEReference				inWE ) ;

EXTERN_API ( OSErr )
WESetUserInfo (					WESelector				inUserTag,
								SInt32					inUserInfo,
								WEReference				inWE ) ;

EXTERN_API ( OSErr )
WERemoveUserInfo (				WESelector				inUserTag,
								WEReference				inWE ) ;

/*	long coordinate utilities */

EXTERN_API ( void )
WELongPointToPoint (			const LongPt *			inLongPoint,
								Point *					outPoint ) ;

EXTERN_API ( void )
WEPointToLongPoint (			Point					inPoint,
								LongPt *				outLongPoint ) ;

EXTERN_API ( void )
WESetLongRect (					LongRect *				outLongRect,
								SInt32					inLeft,
								SInt32					inTop,
								SInt32					inRight,
								SInt32					inBottom ) ;

EXTERN_API ( void )
WELongRectToRect (				const LongRect *		inLongRect,
								Rect *					outRect ) ;

EXTERN_API ( void )
WERectToLongRect (				const Rect *			inRect,
								LongRect *				outLongRect ) ;

EXTERN_API ( void )
WEOffsetLongRect (				LongRect *				ioLongRect,
								SInt32					inHorizontalOffset,
								SInt32					inVerticalOffset ) ;

EXTERN_API ( Boolean )
WELongPointInLongRect (			const LongPt *			inLongPoint,
								const LongRect *		inLongRect ) ;

#if PRAGMA_IMPORT
#pragma import off
#endif

#if PRAGMA_STRUCT_ALIGN
#pragma options align=reset
#endif

#ifdef __cplusplus
}
#endif

#endif	/*__WASTE__*/
