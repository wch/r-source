/*
 *  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
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

#include "Graphics.h"
#include "Errormsg.h"
#include <string.h>

#define nr	(GP->mfg[2])
#define nc	(GP->mfg[3])
#define byrow	(GP->mfind)

void NewFrameConfirm();

void GNewPlot()
{
	int i, j, n;

	if(!DevInit)
		error("No graphics device is active\n");

	/* Restore Default Parameters */
	DevResize();
	memcpy(GP, DP, sizeof(GPar));

	n = nr * nc;
	if(n > 1) {
		i = GP->mfg[0]-1;
		j = GP->mfg[1]-1;
		if(!GP->new) {
			if(!byrow)  {
				i = i+1;
				if(i >= nr) j=j+1;
			} else {
				j = j+1;
				if(j >= nc) i=i+1;
			}
			i = i%nr;
			j = j%nc;
			if(i==0 && j==0) {
				if(GP->ask) NewFrameConfirm();
				DevNewPlot();
				GP->new =  DP->new = 1;
			}
			GP->mfg[0] = DP->mfg[0] = i+1;
			GP->mfg[1] = DP->mfg[1] = j+1;
		}
	}
	else {
		if(!GP->new) {
			if(GP->ask) NewFrameConfirm();
			DevNewPlot();
		}
		GP->new = DP->new = 1;
	}
	GReset();
	GForceClip();
}
