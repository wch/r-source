/*
 *  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (C) 1995, 1996, 1997  Robert Gentleman and Ross Ihaka
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

#include "Defn.h"
#include "IOSupport.h"

static void InitIoBuffer(IoBuffer *iob)
{
	iob->startbuf = (BufferListItem*)malloc(sizeof(BufferListItem));
	iob->activebuf = iob->startbuf;
	iob->activeoffset = 0;
}

static void NextBuffer(IoBuffer *iob)
{
	if(iob->activebuf->next) {
		iob->activebuf = iob->activebuf->next;
	}
	else {
		BufferListItem *new;
		if(!(new = (BufferListItem*)malloc(sizeof(BufferListItem)))) {
			fprintf(stderr, "Unable to malloc io buffer\n");
			exit(1);
		}
		new->next = NULL;
		iob->activebuf->next = new;
		iob->activebuf = iob->activebuf->next;
	}
	iob->activeoffset = 0;
}

static void AddToBuffer(int c, IoBuffer *iob)
{
	if(iob->activeoffset == IOBSIZE) NextBuffer(iob);
	iob->activebuf->buf[iob->activeoffset++] = c;
}

static void ResetBuffer(IoBuffer *iob)
{
	iob->activebuf = iob->startbuf;
	iob->activeoffset = 0;
}

static void ShowBuffer(IoBuffer *iob)
{
	BufferListItem *b;
	int i;
	for(b = iob->startbuf ; b != iob->activebuf ; b = b->next)
		for(i=0 ; i<IOBSIZE ; i++)
			putchar(b->buf[i]);
	for(i = 0 ; i < iob->activeoffset ; i++)
		putchar(b->buf[i]);
}
