/*
 *  R : A Computer Language for Statistical Data Analysis
 *  file devMacintosh.c
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
 * Files stat.h, emulates stat.h with MPW compiler.
 * This file and stat.c have been originally written by
 * Dan Allen <danallen@nwlink.com>
 * Adapted for R by Stefano M. Iacus
 */


#ifndef	_STAT_MRC_

#define	_STAT_MRC_

#include <files.h>
#include <ioctl.h>
#define DIR DirInfo

struct stat
{
	signed int	st_dev;
	signed int	st_ino;
	signed int  st_mode;
	signed int	st_nlink;
	signed int	st_uid;
	signed int	st_gid;
	signed int	st_rdev;
	signed int	st_size;
	unsigned int st_atime;
	unsigned int st_mtime;
	unsigned int st_ctime;
	signed int	st_blksize;
	signed int	st_blocks;
};

#define	S_IFMT	0170000		/* type of file */
#define	S_IFDIR	0040000		/* directory */
#define	S_IFCHR	0020000		/* character special */
#define	S_IFBLK	0060000		/* block special */
#define	S_IFREG	0100000		/* regular */
#define	S_IFLNK	0120000		/* symbolic link */
#define	S_IFSOCK 0140000	/* socket */
#define	S_ISUID	0004000		/* set user id on execution */
#define	S_ISGID	0002000		/* set group id on execution */
#define	S_ISVTX	0001000		/* save swapped text even after use */
#define	S_IREAD	0000400		/* read permission, owner */
#define	S_IWRITE 0000200	/* write permission, owner */
#define	S_IEXEC	0000100		/* execute/search permission, owner */

int stat(char *fileName,struct stat *statbuf);
int fstat(int fd,struct stat *statbuf);

#endif	_STAT_MRC_

