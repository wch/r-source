/*
 *	File:		SmartScroll.h
 *
 *	Contains:	Smart Scroll Application Programming Interface
 *
 *	Copyright:	© 1996, 1997 by Marc Moini, portions by Marc Menschenfreund,
 *		        Alessandro Levi Montalcini and Mark Shirley (Thanks!),
 *			misc changes for "WASTE Demo" by Marco Piovanelli.
 *			All rights reserved.
 */

#ifndef __SMARTSCROLL__
#define __SMARTSCROLL__

//	MacOS #includes

#ifndef __CONDITIONALMACROS__
#include <ConditionalMacros.h>
#endif

#ifndef __TYPES__
#include <Types.h>
#endif

#ifndef __CONTROLS__
#include <Controls.h>
#endif

//	public prototypes

extern pascal void InitSmartScrollAwareApplication ( void ) ;
extern pascal void CloseSmartScrollAwareApplication ( void ) ;
extern pascal void SetSmartScrollInfo ( ControlHandle inScrollBar, SInt32 inAmountVisible, SInt32 inAmountTotal ) ;
extern pascal void SetSmartScrollProportion ( ControlHandle inScrollBar, Fract inProportion ) ;
extern pascal Fract GetSmartScrollProportion ( ControlHandle inScrollBar ) ;

#endif	/* __SMARTSCROLL */
