/*
 *  R : A Computer Language for Statistical Data Analysis
 *  File RVerRes.r
 *  Copyright (C) 2001  Stefano M. Iacus and the R core team
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
 *
 *	Contains:	Resources used to add version info to the R project.
 *
 *	Written by: Stefano M. Iacus	
 *
 */

#include "SysTypes.r"
#include "Types.r"

// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#define kMajorVersNumber	0x01
#define kMinorVersNumber	0x22
#define kReleaseStage		Development
#define kNonFinalRelease	0x03
#define kVersString			"1.2.2"

#define kRegionCode			verUS


resource 'vers' (1) {
	kMajorVersNumber,
	kMinorVersNumber,
	kReleaseStage,
	kNonFinalRelease,
	kRegionCode,
	kVersString,
	"\0xA9 2001 The R Development Core Team,\n" kVersString ", porting by Stefano M. Iacus"
};


resource 'vers' (2) {
	kMajorVersNumber,
	kMinorVersNumber,
	kReleaseStage,
	kNonFinalRelease,
	kRegionCode,
	kVersString,
	"R for MacOS " kVersString
};

#define		kDITLAboutR		137
#define		kRLogoPict		129


resource 'DITL' ( kDITLAboutR, "R about", purgeable )
{
	{
      /* The R Logo */
       { 10, 54, 77, 155 }, Picture { enabled, kRLogoPict },
	
		/*	The R copyright text */
		{ 13, 241, 82, 454 }, StaticText {enabled, 
		"R for MacOS (Developer Version)\n"	/*	Version */
		"\0xA9 2001 R Development Core Team\n"	/*	(c)  */
		"http://www.r-project.org"			/*	HTTP URL */
		 },
		 
		/* GNU License */
		{ 88, 8, 147, 475 }, StaticText {enabled, 
		"R is free software and comes with absolutely no warranty.\n"	
		"You are welcome to redistribute it under conditions which " 
		"may be found in the file named COPYING which accompanies "
		"this distribution."
		 },
		 
        /* Contributors */
		{150, 7, 188, 466 }, StaticText {enabled, 
		"R is a collaborative project with many contributors.\n"	
		"A list of contributors can be obtained by typing ?contributors within R."
		 },
	
	     /* Platfrom specific (c) */
		{199, 8, 216, 470 }, StaticText {enabled, 
		"Macintosh device by Wing Kwong Wan 1999, Stefano M. Iacus 2000-2001."	
		 },
		
		 /* Waste (c) */
		{222, 77, 241, 399 }, StaticText {enabled, 
		"Waste Text Engine \0xA9 1993-2000 Marco Piovanelli."	
		 },
		
		 /* Feedback */
		{249, 79, 271, 396 }, StaticText {enabled, 
		"Feedback is welcome at stefano.iacus@unimi.it"	
		 },
		
		 /* Click to close */
		{282, 112, 301, 351 }, StaticText {enabled, 
		"click on the R logo to close window)"	
		 },
		
		
	}
};




