/*
 *  R : A Computer Language for Statistical Data Analysis
 *  file LongControls.c
 *  Copyright (C) 1998-1999  Ross Ihaka
 *                2000-2001  Stefano M. Iacus and the R core team
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
 *
 *
 *  This file is adapted from the public demos coming with the Waste library
 *  distribution:  WASTE Text Engine © 1993-2000 Marco Piovanelli.
 *   
 *  This file was originally written by: Wing Kwong (Tiki), WAN 3/2/99
 *  Updated to last version of WasteLib library: Stefano M. Iacus, 2001
 *
 *  Original file was:
 *	
 *  WASTE Demo Project:
 *	Macintosh Controls with Long Values
 *
 *	Copyright © 1993-1998 Marco Piovanelli
 *	All Rights Reserved
 *
 *	C port by John C. Daub
 */

#include <RCarbon.h>

#ifndef __CONTROLS__
#include <Controls.h>
#endif 

#ifndef __FIXMATH__
#include <FixMath.h>
#endif

#ifndef __TOOLUTILS__
#include <ToolUtils.h>
#endif

#ifndef __WEDEMOAPP__
#include "RIntf.h"
#endif


/* long control auxiliary record used for keeping long settings
   a handle to this record is stored in the reference field of the control record
*/
struct LCAuxRec
{
	SInt32	value;	/* long value */
	SInt32	min;	/* long min */
	SInt32	max;	/* long max */
};
typedef struct LCAuxRec LCAuxRec, *LCAuxPtr, **LCAuxHandle;

OSErr LCAttach ( ControlHandle inControl )
{
	LCAuxHandle aux ;

	/*	allocate the auxiliary record that will hold long settings */
	if ( ( aux = ( LCAuxHandle ) NewHandleClear ( sizeof ( LCAuxRec ) ) ) == nil )
	{
		return	MemError ( ) ;
	}

	/*	store a handle to the auxiliary record in the contrlRfCon field */
	SetControlReference ( inControl, ( SInt32 ) aux ) ;

	/*	copy current control settings into the auxiliary record */
	( * aux ) -> value = GetControlValue ( inControl ) ;
	( * aux ) -> min = GetControlMinimum ( inControl ) ;
	( * aux ) -> max = GetControlMaximum ( inControl ) ;

	return noErr ;
}

void LCDetach ( ControlHandle inControl )
{
	Handle aux=NULL ;

	if ( ( aux = ( Handle ) GetControlReference ( inControl ) ) != nil )
	{
		SetControlReference ( inControl, 0L ) ;
		DisposeHandle ( aux ) ;
	}
}

void LCSetValue ( ControlHandle inControl, SInt32 inValue )
{
	LCAuxHandle aux ;
	SInt16 controlMin, controlMax, newControlValue ;

	aux = ( LCAuxHandle ) GetControlReference ( inControl ) ;

	/*	make sure inValue is in the range min...max */
	if ( inValue < ( * aux ) -> min )
	{
		inValue = ( * aux ) -> min ;
	}
	if ( inValue > ( * aux ) -> max )
	{
		inValue = ( * aux ) -> max ;
	}

	/*	save inValue in auxiliary record */
	( * aux ) -> value = inValue ;

	/*	calculate new thumb position */
	controlMin = GetControlMinimum ( inControl ) ;
	controlMax = GetControlMaximum ( inControl ) ;
	newControlValue = controlMin + FixRound ( FixMul ( FixDiv ( inValue - ( * aux ) -> min,
		( * aux ) -> max - ( * aux ) -> min), BSL ( controlMax - controlMin, 16 ) ) ) ;

	/*	do nothing if the thumb position hasn't changed */
	if ( newControlValue != GetControlValue ( inControl ) )
	{
		SetControlValue ( inControl, newControlValue ) ;
	}
}

void LCSetMin ( ControlHandle inControl, SInt32 inMin )
{
	LCAuxHandle aux ;

	aux = ( LCAuxHandle ) GetControlReference ( inControl ) ;

	/*	make sure inMin is less than or equal to max */
	if ( inMin > ( * aux ) -> max )
	{
		inMin = ( * aux ) -> max ;
	}

	/*	save inMin in auxiliary record */
	( * aux ) -> min = inMin ;

	/*	set control minimum to inMin or SHRT_MIN, whichever is greater */
	SetControlMinimum ( inControl, ( inMin >= SHRT_MIN ) ? inMin : SHRT_MIN ) ;

	/*	reset value */
	LCSetValue ( inControl, ( * aux ) -> value ) ;
}

void LCSetMax ( ControlHandle inControl, SInt32 inMax )
{
	LCAuxHandle aux ;

	aux = ( LCAuxHandle ) GetControlReference ( inControl ) ;

	/*	make sure inMax is greater than or equal to min */
	if ( inMax < ( * aux ) -> min )
	{
		inMax = ( * aux ) -> min ;
	}

	/*	save inMax in auxiliary record */
	( * aux ) -> max = inMax ;

	/*	set control maximum to inMax or SHRT_MAX, whichever is less */
	SetControlMaximum ( inControl, ( inMax <= SHRT_MAX ) ? inMax : SHRT_MAX ) ;

	/*	reset value */
	LCSetValue ( inControl, ( * aux ) -> value ) ;
}

SInt32 LCGetValue ( ControlHandle inControl )
{
	return ( * ( LCAuxHandle ) GetControlReference ( inControl ) ) -> value ;
}

SInt32 LCGetMin ( ControlHandle inControl )
{
	return ( * ( LCAuxHandle ) GetControlReference ( inControl ) ) -> min ;
}

SInt32 LCGetMax ( ControlHandle inControl )
{
	return ( * ( LCAuxHandle ) GetControlReference ( inControl ) ) -> max ;
}

void LCSynch ( ControlHandle inControl )
{
	LCAuxHandle aux ;
	SInt16 controlMin, controlMax, controlValue ;

	controlMin = GetControlMinimum ( inControl ) ;
	controlMax = GetControlMaximum ( inControl ) ;
	controlValue = GetControlValue ( inControl ) ;
	aux = ( LCAuxHandle ) GetControlReference ( inControl ) ;

	/*	calculate new long value */
	( * aux ) -> value = ( * aux ) -> min + FixMul ( FixRatio ( controlValue - controlMin,
				  controlMax - controlMin ), ( * aux ) -> max - ( * aux ) -> min ) ;
}
