/*
 *  R : A Computer Language for Statistical Data Analysis
 *  file dounzip.c
 *  first part Copyright (C) 2002-5  the R Development Core Team
 *  second part Copyright (C) 1998 Gilles Vollant
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
 *  Foundation, Inc., 51 Franklin Street Fifth Floor, Boston, MA 02110-1301  USA
 */

/* <UTF8>
   Looks OK as byte-level comparions are all with ASCII chars.
   Has own case-insensitive comparisons which we never use (as from R 2.1.0).
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "Defn.h"
#include "Fileio.h" /* for R_fopen */
#include "unzip.h"
#ifdef HAVE_SYS_STAT_H
# include <sys/stat.h>
#endif

#ifdef Win32
#include <io.h> /* for mkdir */
#endif

/* cf do_dircreate in platform.c */
static int R_mkdir(char *path)
{
#ifdef Win32
    char local[PATH_MAX];
    strcpy(local, path);
    /* need DOS paths on Win 9x */
    R_fixbackslash(local);
    return mkdir(local);
#endif
#ifdef Unix
    return mkdir(path, 0777);
#endif
}


#define BUF_SIZE 4096
static int
extract_one(unzFile uf, const char *const dest, const char * const filename,
	    SEXP names, int *nnames)
{
    int err = UNZ_OK;
    FILE *fout;
    char  outname[PATH_MAX], dirs[PATH_MAX], buf[BUF_SIZE], *p, *pp;
    
    err = unzOpenCurrentFile(uf);
    if (err != UNZ_OK) return err;
    if(strlen(dest) > PATH_MAX - 1) return 1;
    strcpy(outname, dest);
    strcat(outname, FILESEP);
    if(filename) {
	if(strlen(dest) + strlen(filename) > PATH_MAX - 2) return 1;
	strcat(outname, filename);
    } else {
	unz_file_info file_info;
	char filename_inzip[PATH_MAX];
	err = unzGetCurrentFileInfo(uf, &file_info, filename_inzip, 
				    sizeof(filename_inzip), NULL, 0, NULL, 0);
	strcat(outname, filename_inzip);
    }
#ifdef Win32
    R_fixslash(outname);
#endif
    p = outname + strlen(outname) - 1;
    if(*p == '/') { /* Don't know how these are stored in Mac zip files */
	*p = '\0';
	if (!R_FileExists(outname)) err = R_mkdir(outname);
    } else {
	/* make parents as required: have already checked dest exists */
	pp = outname + strlen(dest) + 1;
	while((p = Rf_strrchr(pp, '/'))) {
	    strcpy(dirs, outname);
	    dirs[p - outname] = '\0';
	    /* Rprintf("dirs is %s\n", dirs); */
	    if (!R_FileExists(dirs)) R_mkdir(dirs);
	    pp = p + 1;
	}
	/* Rprintf("extracting %s\n", outname); */
	fout = R_fopen(outname, "wb");
	if (!fout) {
	    unzCloseCurrentFile(uf);
	    error(_("cannot open file '%s'"), outname);
	    return 3;		/* not reached */
	}
	while (1) {
	    err = unzReadCurrentFile(uf, buf, BUF_SIZE);
	    /* Rprintf("read %d bytes\n", err); */
	    if (err <= 0) break;
	    if (fwrite(buf, err, 1, fout) != 1) { err = -200; break; }
	    if (err < BUF_SIZE) { err = 0; break; }
	}
	fclose(fout);
	SET_STRING_ELT(names, (*nnames)++, mkChar(outname));
    }
    unzCloseCurrentFile(uf);
    return err;
}


static int 
do_unzip(const char *zipname, const char *dest, int nfiles, const char **files,
	 SEXP *pnames, int *nnames)
{
    int   i, err = UNZ_OK;
    unzFile uf;
    SEXP names = *pnames;

    uf = unzOpen(zipname);
    if (!uf) return 1;
    if(nfiles == 0) { /* all files */
	unz_global_info gi;
	unzGetGlobalInfo(uf, &gi);
	for (i = 0; i < gi.number_entry; i++) {
	    if (i > 0) if((err = unzGoToNextFile(uf)) != UNZ_OK) break;
	    if(*nnames+1 >= LENGTH(names)) {
		SEXP onames = names;
		names = allocVector(STRSXP, 2*LENGTH(names));
		UNPROTECT(1);
		PROTECT(names);
		copyVector(names, onames);
	    }
	    if ((err = extract_one(uf, dest, NULL, names, nnames)) != UNZ_OK) break;
#ifdef Win32
	    R_ProcessEvents();
#else
	    R_CheckUserInterrupt();
#endif
	}
    } else {
	for (i = 0; i < nfiles; i++) {
	    if ((err = unzLocateFile(uf, files[i], 1)) != UNZ_OK) break;
	    if ((err = extract_one(uf, dest, files[i], names, nnames)) != UNZ_OK) break;
#ifdef Win32
	    R_ProcessEvents();
#else
	    R_CheckUserInterrupt();
#endif
	}
    }
    *pnames = names;
    unzClose(uf);
    return err;
}

SEXP attribute_hidden do_int_unzip(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP  fn, ans, names = R_NilValue;
    char  zipname[PATH_MAX], dest[PATH_MAX];
    const char *p, *topics[500];
    int   i, ntopics, rc, nnames = 0;

    if (!isString(CAR(args)) || LENGTH(CAR(args)) != 1)
	error(_("invalid zip name argument"));
    p = translateChar(STRING_ELT(CAR(args), 0));
    if (strlen(p) > PATH_MAX - 1)
	error(_("zip path is too long"));
    strcpy(zipname, p);
    args = CDR(args);
    fn = CAR(args);
    ntopics = length(fn);
    if (ntopics > 0) {
	if (!isString(fn) || ntopics > 500)
	    error(_("invalid '%s' argument"), "topics");
	for (i = 0; i < ntopics; i++)
	    topics[i] = translateChar(STRING_ELT(fn, i));
    }
    args = CDR(args);
    if (!isString(CAR(args)) || LENGTH(CAR(args)) != 1)
	error(_("invalid '%s' argument"), "destination");
    p = R_ExpandFileName(translateChar(STRING_ELT(CAR(args), 0)));
    if (strlen(p) > PATH_MAX - 1)
	error(_("'destination' is too long"));
    strcpy(dest, p);
    if(!R_FileExists(dest))
	error(_("'destination' does not exist"));
    
    if(ntopics > 0)
	PROTECT(names = allocVector(STRSXP, ntopics));
    else
	PROTECT(names = allocVector(STRSXP, 5000));
    rc = do_unzip(zipname, dest, ntopics, topics, &names, &nnames);
    if(rc != UNZ_OK)
	switch(rc) {
	case UNZ_END_OF_LIST_OF_FILE:
	    warning(_("requested file not found in the zip file"));
	    break;
	case UNZ_BADZIPFILE:
	    warning(_("zip file is corrupt"));
	    break;
	case UNZ_CRCERROR:
	    warning(_("CRC error in zip file"));
	    break;
	case UNZ_PARAMERROR:
	case UNZ_INTERNALERROR:
	    warning(_("internal error in unz code"));
	    break;
	case -200:
	    warning(_("write error in extracting from zip file"));
	    break;
	default:
	    warning(_("error %d in extracting from zip file"), rc);
	}
    PROTECT(ans = ScalarLogical(rc));
    PROTECT(names = lengthgets(names, nnames));
    setAttrib(ans, install("extracted"), names);
    UNPROTECT(3);
    return ans;
}

/* ------------------- unz connections --------------------- */

#include <Rconnections.h>

static Rboolean unz_open(Rconnection con)
{
    unzFile uf;
    char path[2*PATH_MAX], *p;
    const char *tmp;

    if(con->mode[0] != 'r') {
	warning(_("unz connections can only be opened for reading"));
	return FALSE;
    }
    tmp = R_ExpandFileName(con->description);
    if (strlen(tmp) > PATH_MAX - 1) {
	warning(_("zip path is too long"));
	return FALSE;
    }
    strcpy(path, tmp);
    p = Rf_strrchr(path, ':');
    if(!p) {
	warning(_("invalid description of unz connection"));
	return FALSE;
    }
    *p = '\0';
    uf = unzOpen(path);
    if(!uf) {
	warning(_("cannot open zip file '%s'"), path);
	return FALSE;
    }
    if (unzLocateFile(uf, p+1, 1) != UNZ_OK) {
	warning(_("cannot locate file '%s' in zip file '%s'"), p+1, path);
	unzClose(uf);
	return FALSE;
    }
    unzOpenCurrentFile(uf);
    ((Runzconn)(con->private))->uf = uf;
    con->isopen = TRUE;
    con->canwrite = FALSE;
    con->canread = TRUE;
    if(strlen(con->mode) >= 2 && con->mode[1] == 'b') con->text = FALSE;
    else con->text = TRUE;
    /* set_iconv(); not yet */
    con->save = -1000;
    return TRUE;
}

static void unz_close(Rconnection con)
{
    unzFile uf = ((Runzconn)(con->private))->uf;    
    unzCloseCurrentFile(uf);
    unzClose(uf);
    con->isopen = FALSE;
}

static int unz_fgetc_internal(Rconnection con)
{
    unzFile uf = ((Runzconn)(con->private))->uf;
    char buf[1];
    int err, p;

    err = unzReadCurrentFile(uf, buf, 1);
    p = buf[0] % 256;
    return (err < 1) ? R_EOF : p;
}

static size_t unz_read(void *ptr, size_t size, size_t nitems,
		       Rconnection con)
{
    unzFile uf = ((Runzconn)(con->private))->uf;
    return unzReadCurrentFile(uf, ptr, size*nitems)/size;
}

static int null_vfprintf(Rconnection con, const char *format, va_list ap)
{
    error(_("printing not enabled for this connection"));
    return 0; /* -Wall */
}

static size_t null_write(const void *ptr, size_t size, size_t nitems,
			 Rconnection con)
{
    error(_("write not enabled for this connection"));
    return 0; /* -Wall */
}

static double null_seek(Rconnection con, double where, int origin, int rw)
{
    error(_("seek not enabled for this connection"));
    return 0; /* -Wall */
}

static int null_fflush(Rconnection con)
{
    return 0;
}

Rconnection attribute_hidden
R_newunz(const char *description, const char *const mode)
{
    Rconnection new;
    new = (Rconnection) malloc(sizeof(struct Rconn));
    if(!new) error(_("allocation of unz connection failed"));
    new->class = (char *) malloc(strlen("unz") + 1);
    if(!new->class) {
	free(new);
	error(_("allocation of unz connection failed"));
    }
    strcpy(new->class, "unz");
    new->description = (char *) malloc(strlen(description) + 1);
    if(!new->description) {
	free(new->class); free(new);
	error(_("allocation of unz connection failed"));
    }
    init_con(new, description, mode);

    new->canseek = TRUE;
    new->open = &unz_open;
    new->close = &unz_close;
    new->vfprintf = &null_vfprintf;
    new->fgetc_internal = &unz_fgetc_internal;
    new->fgetc = &dummy_fgetc;
    new->seek = &null_seek;
    new->fflush = &null_fflush;
    new->read = &unz_read;
    new->write = &null_write;
    new->private = (void *) malloc(sizeof(struct fileconn));
    if(!new->private) {
	free(new->description); free(new->class); free(new);
	error(_("allocation of unz connection failed"));
    }
    return new;
}

       /* =================== second part ====================== */

/* From minizip contribution to zlib 1.1.3, reformatted by indent,
   unz_copyright is now static.
*/

/* unzip.c -- IO on .zip files using zlib
   Version 0.15 beta, Mar 19th, 1998,

   Read unzip.h for more info
*/


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "zlib.h"
#include "unzip.h"
#ifdef HAVE_ERRNO_H
#include <errno.h>
#else
extern int errno;
#endif


#if !defined(unix) && !defined(CASESENSITIVITYDEFAULT_YES) && \
                      !defined(CASESENSITIVITYDEFAULT_NO)
#define CASESENSITIVITYDEFAULT_NO
#endif


#ifndef UNZ_BUFSIZE
#define UNZ_BUFSIZE (16384)
#endif

#ifndef UNZ_MAXFILENAMEINZIP
#define UNZ_MAXFILENAMEINZIP (256)
#endif

#ifndef ALLOC
#define ALLOC(size) (malloc(size))
#endif
#ifndef TRYFREE
#define TRYFREE(p) {if (p) free(p);}
#endif

#define SIZECENTRALDIRITEM (0x2e)
#define SIZEZIPLOCALHEADER (0x1e)


/* I've found an old Unix (a SunOS 4.1.3_U1) without all SEEK_* defined.... */

#ifndef SEEK_CUR
#define SEEK_CUR    1
#endif

#ifndef SEEK_END
#define SEEK_END    2
#endif

#ifndef SEEK_SET
#define SEEK_SET    0
#endif

static const char unz_copyright[] =
" unzip 0.15 Copyright 1998 Gilles Vollant ";

/* unz_file_info_interntal contain internal info about a file in zipfile*/
typedef struct unz_file_info_internal_s {
    uLong offset_curfile;	/* relative offset of local header 4 bytes */
}     unz_file_info_internal;


/* file_in_zip_read_info_s contain internal information about a file in zipfile,
    when reading and decompress it */
typedef struct {
    char *read_buffer;		/* internal buffer for compressed data */
    z_stream stream;		/* zLib stream structure for inflate */

    uLong pos_in_zipfile;	/* position in byte on the zipfile, for fseek */
    uLong stream_initialised;	/* flag set if stream structure is
				   initialised */

    uLong offset_local_extrafield;	/* offset of the local extra field */
    uInt  size_local_extrafield;/* size of the local extra field */
    uLong pos_local_extrafield;	/* position in the local extra field in read */

    uLong crc32;		/* crc32 of all data uncompressed */
    uLong crc32_wait;		/* crc32 we must obtain after decompress all */
    uLong rest_read_compressed;	/* number of byte to be decompressed */
    uLong rest_read_uncompressed;	/* number of byte to be obtained
					   after decomp */
    FILE *file;			/* io structore of the zipfile */
    uLong compression_method;	/* compression method (0==store) */
    uLong byte_before_the_zipfile;	/* byte before the zipfile, (>0 for
					   sfx) */
}     file_in_zip_read_info_s;


/* unz_s contain internal information about the zipfile
*/
typedef struct {
    FILE *file;			/* io structore of the zipfile */
    unz_global_info gi;		/* public global information */
    uLong byte_before_the_zipfile;	/* byte before the zipfile, (>0 for
					   sfx) */
    uLong num_file;		/* number of the current file in the zipfile */
    uLong pos_in_central_dir;	/* pos of the current file in the central dir */
    uLong current_file_ok;	/* flag about the usability of the current
				   file */
    uLong central_pos;		/* position of the beginning of the central
				   dir */

    uLong size_central_dir;	/* size of the central directory  */
    uLong offset_central_dir;	/* offset of start of central directory with
				   respect to the starting disk number */

    unz_file_info cur_file_info;/* public info about the current file in zip */
    unz_file_info_internal cur_file_info_internal;	/* private info about it */
    file_in_zip_read_info_s *pfile_in_zip_read;	/* structure about the
						   current file if we are
						   decompressing it */
}     unz_s;


/* ===========================================================================
     Read a byte from a gz_stream; update next_in and avail_in. Return EOF
   for end of file.
   IN assertion: the stream s has been sucessfully opened for reading.
*/


static int 
unzlocal_getByte(FILE * fin, int *pi)
{
    unsigned char c;
    int   err = fread(&c, 1, 1, fin);

    if (err == 1) {
	*pi = (int) c;
	return UNZ_OK;
    } else {
	if (ferror(fin))
	    return UNZ_ERRNO;
	else
	    return UNZ_EOF;
    }
}


/* ===========================================================================
   Reads a long in LSB order from the given gz_stream. Sets
*/
static int 
unzlocal_getShort(FILE * fin, uLong * pX)
{
    uLong x;
    int   i = 0 /* -Wall */, err;

    err = unzlocal_getByte(fin, &i);
    x = (uLong) i;
    if (err == UNZ_OK) err = unzlocal_getByte(fin, &i);
    x += ((uLong) i) << 8;
    if (err == UNZ_OK) *pX = x; else *pX = 0;
    return err;
}

static int 
unzlocal_getLong(FILE * fin, uLong * pX)
{
    uLong x;
    int   i = 0 /* -Wall */, err;

    err = unzlocal_getByte(fin, &i);
    x = (uLong) i;

    if (err == UNZ_OK) err = unzlocal_getByte(fin, &i);
    x += ((uLong) i) << 8;
    if (err == UNZ_OK) err = unzlocal_getByte(fin, &i);
    x += ((uLong) i) << 16;
    if (err == UNZ_OK) err = unzlocal_getByte(fin, &i);
    x += ((uLong) i) << 24;
    if (err == UNZ_OK) *pX = x; else *pX = 0;
    return err;
}


/* My own strcmpi / strcasecmp NOT USED in R */
static int 
strcmpcasenosensitive_internal(const char *fileName1, const char *fileName2)
{
    for (;;) {
	char  c1 = *(fileName1++);
	char  c2 = *(fileName2++);

	if ((c1 >= 'a') && (c1 <= 'z')) c1 -= 0x20;
	if ((c2 >= 'a') && (c2 <= 'z')) c2 -= 0x20;
	if (c1 == '\0') return ((c2 == '\0') ? 0 : -1);
	if (c2 == '\0') return 1;
	if (c1 < c2) return -1;
	if (c1 > c2) return 1;
    }
}


#ifdef  CASESENSITIVITYDEFAULT_NO
#define CASESENSITIVITYDEFAULTVALUE 2
#else
#define CASESENSITIVITYDEFAULTVALUE 1
#endif

#ifndef STRCMPCASENOSENTIVEFUNCTION
#define STRCMPCASENOSENTIVEFUNCTION strcmpcasenosensitive_internal
#endif

/*
   Compare two filename (fileName1,fileName2).
   If iCaseSensitivity = 1, comparision is case sensitivity (like strcmp)
   If iCaseSensitivity = 2, comparision is not case sensitivity (like strcmpi
                                                                or strcasecmp)
   If iCaseSensitivity = 0, case sensitivity is defaut of your operating system
        (like 1 on Unix, 2 on Windows)

*/
static int 
unzStringFileNameCompare(const char *fileName1, const char *fileName2, 
			 int iCaseSensitivity)
{
    if (iCaseSensitivity == 0)
	iCaseSensitivity = CASESENSITIVITYDEFAULTVALUE;
    if (iCaseSensitivity == 1)
	return strcmp(fileName1, fileName2);
    return STRCMPCASENOSENTIVEFUNCTION(fileName1, fileName2);
}

#define BUFREADCOMMENT (0x400)

/*
  Locate the Central directory of a zipfile (at the end, just before
    the global comment)
*/
static uLong 
unzlocal_SearchCentralDir(FILE * fin)
{
    unsigned char *buf;
    uLong uSizeFile;
    uLong uBackRead;
    uLong uMaxBack = 0xffff;	/* maximum size of global comment */
    uLong uPosFound = 0;

    if (fseek(fin, 0, SEEK_END) != 0) return 0;
    uSizeFile = ftell(fin);
    if (uMaxBack > uSizeFile) uMaxBack = uSizeFile;
    buf = (unsigned char *) ALLOC(BUFREADCOMMENT + 4);
    if (buf == NULL) return 0;
    uBackRead = 4;
    while (uBackRead < uMaxBack) {
	uLong uReadSize, uReadPos;
	int   i;

	if (uBackRead + BUFREADCOMMENT > uMaxBack) uBackRead = uMaxBack;
	else uBackRead += BUFREADCOMMENT;
	uReadPos = uSizeFile - uBackRead;

	uReadSize = ((BUFREADCOMMENT + 4) < (uSizeFile - uReadPos)) ?
	    (BUFREADCOMMENT + 4) : (uSizeFile - uReadPos);
	if (fseek(fin, uReadPos, SEEK_SET) != 0) break;

	if (fread(buf, (uInt) uReadSize, 1, fin) != 1) break;

	for (i = (int) uReadSize - 3; (i--) > 0;)
	    if (((*(buf + i)) == 0x50) && ((*(buf + i + 1)) == 0x4b) &&
		((*(buf + i + 2)) == 0x05) && ((*(buf + i + 3)) == 0x06)) {
		uPosFound = uReadPos + i;
		break;
	    }
	if (uPosFound != 0) break;
    }
    TRYFREE(buf);
    return uPosFound;
}

/*
  Open a Zip file. path contain the full pathname (by example,
     on a Windows NT computer "c:\\test\\zlib109.zip" or on an Unix computer
	 "zlib/zlib109.zip".
	 If the zipfile cannot be opened (file don't exist or in not valid), the
	   return value is NULL.
     Else, the return value is a unzFile Handle, usable with other function
	   of this unzip package.
*/
static unzFile 
unzOpen(const char *path)
{
    unz_s us;
    unz_s *s;
    uLong central_pos, uL;
    FILE *fin;
    uLong number_disk;		/* number of the current dist, used for
				   spaning ZIP, unsupported, always 0 */
    uLong number_disk_with_CD;	/* number the the disk with central dir, used
				   for spaning ZIP, unsupported, always 0 */
    uLong number_entry_CD;	/* total number of entries in the central dir
				   (same than number_entry on nospan) */
    int   err = UNZ_OK;

    if (unz_copyright[0] != ' ') return NULL;
    fin = R_fopen(path, "rb");
    if (fin == NULL) return NULL;
    central_pos = unzlocal_SearchCentralDir(fin);
    if (central_pos == 0) err = UNZ_ERRNO;
    if (fseek(fin, central_pos, SEEK_SET) != 0) err = UNZ_ERRNO;

    /* the signature, already checked */
    if (unzlocal_getLong(fin, &uL) != UNZ_OK) err = UNZ_ERRNO;

    /* number of this disk */
    if (unzlocal_getShort(fin, &number_disk) != UNZ_OK) err = UNZ_ERRNO;

    /* number of the disk with the start of the central directory */
    if (unzlocal_getShort(fin, &number_disk_with_CD) != UNZ_OK)
	err = UNZ_ERRNO;

    /* total number of entries in the central dir on this disk */
    if (unzlocal_getShort(fin, &us.gi.number_entry) != UNZ_OK) err = UNZ_ERRNO;

    /* total number of entries in the central dir */
    if (unzlocal_getShort(fin, &number_entry_CD) != UNZ_OK) err = UNZ_ERRNO;

    if ((number_entry_CD != us.gi.number_entry) ||
	(number_disk_with_CD != 0) || (number_disk != 0))
	err = UNZ_BADZIPFILE;

    /* size of the central directory */
    if (unzlocal_getLong(fin, &us.size_central_dir) != UNZ_OK) err = UNZ_ERRNO;

    /* offset of start of central directory with respect to the starting disk
       number */
    if (unzlocal_getLong(fin, &us.offset_central_dir) != UNZ_OK)
	err = UNZ_ERRNO;

    /* zipfile comment length */
    if (unzlocal_getShort(fin, &us.gi.size_comment) != UNZ_OK) err = UNZ_ERRNO;

    if ((central_pos < us.offset_central_dir + us.size_central_dir) &&
	(err == UNZ_OK)) err = UNZ_BADZIPFILE;

    if (err != UNZ_OK) {
	fclose(fin);
	return NULL;
    }
    us.file = fin;
    us.byte_before_the_zipfile = central_pos -
	(us.offset_central_dir + us.size_central_dir);
    us.central_pos = central_pos;
    us.pfile_in_zip_read = NULL;

    s = (unz_s *) ALLOC(sizeof(unz_s));
    *s = us;
    unzGoToFirstFile((unzFile) s);
    return (unzFile) s;
}

/*
  Write info about the ZipFile in the *pglobal_info structure.
  No preparation of the structure is needed
  return UNZ_OK if there is no problem. */
static int unzGetGlobalInfo (unzFile file, unz_global_info *pglobal_info)
{
    unz_s* s;
    if (file == NULL) return UNZ_PARAMERROR;
    s = (unz_s*)file;
    *pglobal_info = s->gi;
    return UNZ_OK;
}


/*
  Close a ZipFile opened with unzipOpen.
  If there is files inside the .Zip opened with unzipOpenCurrentFile (see later),
    these files MUST be closed with unzipCloseCurrentFile before call unzipClose.
  return UNZ_OK if there is no problem. */
static int 
unzClose(unzFile file)
{
    unz_s *s;

    if (file == NULL) return UNZ_PARAMERROR;
    s = (unz_s *) file;
    if (s->pfile_in_zip_read != NULL) unzCloseCurrentFile(file);
    fclose(s->file);
    TRYFREE(s);
    return UNZ_OK;
}

/*
   Translate date/time from Dos format to tm_unz (readable more easilty)
*/
static void 
unzlocal_DosDateToTmuDate(uLong ulDosDate, tm_unz * ptm)
{
    uLong uDate;

    uDate = (uLong) (ulDosDate >> 16);
    ptm->tm_mday = (uInt) (uDate & 0x1f);
    ptm->tm_mon = (uInt) ((((uDate) & 0x1E0) / 0x20) - 1);
    ptm->tm_year = (uInt) (((uDate & 0x0FE00) / 0x0200) + 1980);

    ptm->tm_hour = (uInt) ((ulDosDate & 0xF800) / 0x800);
    ptm->tm_min = (uInt) ((ulDosDate & 0x7E0) / 0x20);
    ptm->tm_sec = (uInt) (2 * (ulDosDate & 0x1f));
}

/*
  Get Info about the current file in the zipfile, with internal only info
*/

static int 
unzlocal_GetCurrentFileInfoInternal(unzFile file, 
				    unz_file_info * pfile_info,
				    unz_file_info_internal * pfile_info_internal, 
				    char *szFileName, 
				    uLong fileNameBufferSize, 
				    void *extraField, 
				    uLong extraFieldBufferSize, 
				    char *szComment, uLong commentBufferSize)
{
    unz_s *s;
    unz_file_info file_info;
    unz_file_info_internal file_info_internal;
    int   err = UNZ_OK;
    uLong uMagic;
    long  lSeek = 0;

    if (file == NULL)
	return UNZ_PARAMERROR;
    s = (unz_s *) file;
    if (fseek(s->file, s->pos_in_central_dir + s->byte_before_the_zipfile, 
	      SEEK_SET) != 0)
	err = UNZ_ERRNO;


    /* we check the magic */
    if (err == UNZ_OK) {
	if (unzlocal_getLong(s->file, &uMagic) != UNZ_OK)
	    err = UNZ_ERRNO;
	else if (uMagic != 0x02014b50)
	    err = UNZ_BADZIPFILE;
    }
    if (unzlocal_getShort(s->file, &file_info.version) != UNZ_OK)
	err = UNZ_ERRNO;

    if (unzlocal_getShort(s->file, &file_info.version_needed) != UNZ_OK)
	err = UNZ_ERRNO;

    if (unzlocal_getShort(s->file, &file_info.flag) != UNZ_OK)
	err = UNZ_ERRNO;

    if (unzlocal_getShort(s->file, &file_info.compression_method) != UNZ_OK)
	err = UNZ_ERRNO;

    if (unzlocal_getLong(s->file, &file_info.dosDate) != UNZ_OK)
	err = UNZ_ERRNO;

    unzlocal_DosDateToTmuDate(file_info.dosDate, &file_info.tmu_date);

    if (unzlocal_getLong(s->file, &file_info.crc) != UNZ_OK)
	err = UNZ_ERRNO;

    if (unzlocal_getLong(s->file, &file_info.compressed_size) != UNZ_OK)
	err = UNZ_ERRNO;

    if (unzlocal_getLong(s->file, &file_info.uncompressed_size) != UNZ_OK)
	err = UNZ_ERRNO;

    if (unzlocal_getShort(s->file, &file_info.size_filename) != UNZ_OK)
	err = UNZ_ERRNO;

    if (unzlocal_getShort(s->file, &file_info.size_file_extra) != UNZ_OK)
	err = UNZ_ERRNO;

    if (unzlocal_getShort(s->file, &file_info.size_file_comment) != UNZ_OK)
	err = UNZ_ERRNO;

    if (unzlocal_getShort(s->file, &file_info.disk_num_start) != UNZ_OK)
	err = UNZ_ERRNO;

    if (unzlocal_getShort(s->file, &file_info.internal_fa) != UNZ_OK)
	err = UNZ_ERRNO;

    if (unzlocal_getLong(s->file, &file_info.external_fa) != UNZ_OK)
	err = UNZ_ERRNO;

    if (unzlocal_getLong(s->file, &file_info_internal.offset_curfile) != UNZ_OK)
	err = UNZ_ERRNO;

    lSeek += file_info.size_filename;
    if ((err == UNZ_OK) && (szFileName != NULL)) {
	uLong uSizeRead;

	if (file_info.size_filename < fileNameBufferSize) {
	    *(szFileName + file_info.size_filename) = '\0';
	    uSizeRead = file_info.size_filename;
	} else
	    uSizeRead = fileNameBufferSize;

	if ((file_info.size_filename > 0) && (fileNameBufferSize > 0))
	    if (fread(szFileName, (uInt) uSizeRead, 1, s->file) != 1)
		err = UNZ_ERRNO;
	lSeek -= uSizeRead;
    }
    if ((err == UNZ_OK) && (extraField != NULL)) {
	uLong uSizeRead;

	if (file_info.size_file_extra < extraFieldBufferSize)
	    uSizeRead = file_info.size_file_extra;
	else
	    uSizeRead = extraFieldBufferSize;

	if (lSeek != 0) {
	    if (fseek(s->file, lSeek, SEEK_CUR) == 0)
		lSeek = 0;
	    else
		err = UNZ_ERRNO;
	}
	if ((file_info.size_file_extra > 0) && (extraFieldBufferSize > 0))
	    if (fread(extraField, (uInt) uSizeRead, 1, s->file) != 1)
		err = UNZ_ERRNO;
	lSeek += file_info.size_file_extra - uSizeRead;
    } else
	lSeek += file_info.size_file_extra;


    if ((err == UNZ_OK) && (szComment != NULL)) {
	uLong uSizeRead;

	if (file_info.size_file_comment < commentBufferSize) {
	    *(szComment + file_info.size_file_comment) = '\0';
	    uSizeRead = file_info.size_file_comment;
	} else
	    uSizeRead = commentBufferSize;

	if (lSeek != 0) {
	    if (fseek(s->file, lSeek, SEEK_CUR) == 0)
		lSeek = 0;
	    else
		err = UNZ_ERRNO;
	}
	if ((file_info.size_file_comment > 0) && (commentBufferSize > 0))
	    if (fread(szComment, (uInt) uSizeRead, 1, s->file) != 1)
		err = UNZ_ERRNO;
	lSeek += file_info.size_file_comment - uSizeRead;
    } else
	lSeek += file_info.size_file_comment;

    if ((err == UNZ_OK) && (pfile_info != NULL))
	*pfile_info = file_info;

    if ((err == UNZ_OK) && (pfile_info_internal != NULL))
	*pfile_info_internal = file_info_internal;

    return err;
}



/*
  Write info about the ZipFile in the *pglobal_info structure.
  No preparation of the structure is needed
  return UNZ_OK if there is no problem.
*/
static int 
unzGetCurrentFileInfo(unzFile file,
		      unz_file_info * pfile_info, char *szFileName, 
		      uLong fileNameBufferSize, void *extraField, 
		      uLong extraFieldBufferSize, char *szComment, 
		      uLong commentBufferSize)
{
    return
	unzlocal_GetCurrentFileInfoInternal(file, pfile_info, NULL,
					    szFileName, fileNameBufferSize,
					    extraField, extraFieldBufferSize,
					    szComment, commentBufferSize);
}

/*
  Set the current file of the zipfile to the first file.
  return UNZ_OK if there is no problem
*/
static int 
unzGoToFirstFile(unzFile file)
{
    int   err = UNZ_OK;
    unz_s *s;

    if (file == NULL) return UNZ_PARAMERROR;
    s = (unz_s *) file;
    s->pos_in_central_dir = s->offset_central_dir;
    s->num_file = 0;
    err = unzlocal_GetCurrentFileInfoInternal(file, &s->cur_file_info,
					      &s->cur_file_info_internal,
					      NULL, 0, NULL, 0, NULL, 0);
    s->current_file_ok = (err == UNZ_OK);
    return err;
}


/*
  Set the current file of the zipfile to the next file.
  return UNZ_OK if there is no problem
  return UNZ_END_OF_LIST_OF_FILE if the actual file was the latest.
*/
static int 
unzGoToNextFile(unzFile file)
{
    unz_s *s;
    int   err;

    if (file == NULL) return UNZ_PARAMERROR;
    s = (unz_s *) file;
    if (!s->current_file_ok)
	return UNZ_END_OF_LIST_OF_FILE;
    if (s->num_file + 1 == s->gi.number_entry)
	return UNZ_END_OF_LIST_OF_FILE;

    s->pos_in_central_dir += SIZECENTRALDIRITEM + 
	s->cur_file_info.size_filename +
	s->cur_file_info.size_file_extra + 
	s->cur_file_info.size_file_comment;
    s->num_file++;
    err = unzlocal_GetCurrentFileInfoInternal(file, &s->cur_file_info,
					      &s->cur_file_info_internal,
					      NULL, 0, NULL, 0, NULL, 0);
    s->current_file_ok = (err == UNZ_OK);
    return err;
}


/*
  Try locate the file szFileName in the zipfile.
  For the iCaseSensitivity signification, see unzipStringFileNameCompare

  return value :
  UNZ_OK if the file is found. It becomes the current file.
  UNZ_END_OF_LIST_OF_FILE if the file is not found
*/
static int 
unzLocateFile(unzFile file, const char *szFileName, int iCaseSensitivity)
{
    unz_s *s;
    int   err;

    uLong num_fileSaved;
    uLong pos_in_central_dirSaved;


    if (file == NULL) return UNZ_PARAMERROR;
    if (strlen(szFileName) >= UNZ_MAXFILENAMEINZIP) return UNZ_PARAMERROR;

    s = (unz_s *) file;
    if (!s->current_file_ok) return UNZ_END_OF_LIST_OF_FILE;
    num_fileSaved = s->num_file;
    pos_in_central_dirSaved = s->pos_in_central_dir;
    err = unzGoToFirstFile(file);

    while (err == UNZ_OK) {
	char  szCurrentFileName[UNZ_MAXFILENAMEINZIP + 1];

	unzGetCurrentFileInfo(file, NULL,
			      szCurrentFileName, sizeof(szCurrentFileName) - 1,
			      NULL, 0, NULL, 0);
	if (unzStringFileNameCompare(szCurrentFileName,
				     szFileName, iCaseSensitivity) == 0)
	    return UNZ_OK;
	err = unzGoToNextFile(file);
    }

    s->num_file = num_fileSaved;
    s->pos_in_central_dir = pos_in_central_dirSaved;
    return err;
}


/*
  Read the local header of the current zipfile
  Check the coherency of the local header and info in the end of central
        directory about this file
  store in *piSizeVar the size of extra info in local header
        (filename and size of extra field data)
*/
static int 
unzlocal_CheckCurrentFileCoherencyHeader(unz_s * s, uInt * piSizeVar, 
					 uLong * poffset_local_extrafield, 
					 uInt * psize_local_extrafield)
{
    uLong uMagic, uData, uFlags;
    uLong size_filename;
    uLong size_extra_field;
    int   err = UNZ_OK;

    *piSizeVar = 0;
    *poffset_local_extrafield = 0;
    *psize_local_extrafield = 0;

    if (fseek(s->file, s->cur_file_info_internal.offset_curfile +
	      s->byte_before_the_zipfile, SEEK_SET) != 0)
	return UNZ_ERRNO;

    if (err == UNZ_OK) {
	if (unzlocal_getLong(s->file, &uMagic) != UNZ_OK) err = UNZ_ERRNO;
	else if (uMagic != 0x04034b50) err = UNZ_BADZIPFILE;
    }
    if (unzlocal_getShort(s->file, &uData) != UNZ_OK) err = UNZ_ERRNO;

    if (unzlocal_getShort(s->file, &uFlags) != UNZ_OK) err = UNZ_ERRNO;

    if (unzlocal_getShort(s->file, &uData) != UNZ_OK) err = UNZ_ERRNO;
    else if ((err == UNZ_OK) && 
	     (uData != s->cur_file_info.compression_method))
	err = UNZ_BADZIPFILE;

    if ((err == UNZ_OK) && (s->cur_file_info.compression_method != 0) &&
	(s->cur_file_info.compression_method != Z_DEFLATED))
	err = UNZ_BADZIPFILE;

    if (unzlocal_getLong(s->file, &uData) != UNZ_OK)	/* date/time */
	err = UNZ_ERRNO;

    if (unzlocal_getLong(s->file, &uData) != UNZ_OK)	/* crc */
	err = UNZ_ERRNO;
    else if ((err == UNZ_OK) && (uData != s->cur_file_info.crc) &&
	     ((uFlags & 8) == 0))
	err = UNZ_BADZIPFILE;

    if (unzlocal_getLong(s->file, &uData) != UNZ_OK)	/* size compr */
	err = UNZ_ERRNO;
    else if ((err == UNZ_OK) && 
	     (uData != s->cur_file_info.compressed_size) &&
	     ((uFlags & 8) == 0))
	err = UNZ_BADZIPFILE;

    if (unzlocal_getLong(s->file, &uData) != UNZ_OK)	/* size uncompr */
	err = UNZ_ERRNO;
    else if ((err == UNZ_OK) && 
	     (uData != s->cur_file_info.uncompressed_size) &&
	     ((uFlags & 8) == 0))
	err = UNZ_BADZIPFILE;


    if (unzlocal_getShort(s->file, &size_filename) != UNZ_OK)
	err = UNZ_ERRNO;
    else if ((err == UNZ_OK) && 
	     (size_filename != s->cur_file_info.size_filename))
	err = UNZ_BADZIPFILE;

    *piSizeVar += (uInt) size_filename;

    if (unzlocal_getShort(s->file, &size_extra_field) != UNZ_OK)
	err = UNZ_ERRNO;
    *poffset_local_extrafield = s->cur_file_info_internal.offset_curfile +
	SIZEZIPLOCALHEADER + size_filename;
    *psize_local_extrafield = (uInt) size_extra_field;
    *piSizeVar += (uInt) size_extra_field;
    return err;
}

/*
  Open for reading data the current file in the zipfile.
  If there is no error and the file is opened, the return value is UNZ_OK.
*/
static int 
unzOpenCurrentFile(unzFile file)
{
    int   err = UNZ_OK;
    int   Store;
    uInt  iSizeVar;
    unz_s *s;
    file_in_zip_read_info_s *pfile_in_zip_read_info;
    uLong offset_local_extrafield;	/* offset of the local extra field */
    uInt  size_local_extrafield;/* size of the local extra field */

    if (file == NULL) return UNZ_PARAMERROR;
    s = (unz_s *) file;
    if (!s->current_file_ok) return UNZ_PARAMERROR;

    if (s->pfile_in_zip_read != NULL) unzCloseCurrentFile(file);

    if (unzlocal_CheckCurrentFileCoherencyHeader(s, &iSizeVar,
		&offset_local_extrafield, &size_local_extrafield) != UNZ_OK)
	return UNZ_BADZIPFILE;

    pfile_in_zip_read_info = (file_in_zip_read_info_s *)
	ALLOC(sizeof(file_in_zip_read_info_s));
    if (pfile_in_zip_read_info == NULL) return UNZ_INTERNALERROR;

    pfile_in_zip_read_info->read_buffer = (char *) ALLOC(UNZ_BUFSIZE);
    pfile_in_zip_read_info->offset_local_extrafield = offset_local_extrafield;
    pfile_in_zip_read_info->size_local_extrafield = size_local_extrafield;
    pfile_in_zip_read_info->pos_local_extrafield = 0;

    if (pfile_in_zip_read_info->read_buffer == NULL) {
	TRYFREE(pfile_in_zip_read_info);
	return UNZ_INTERNALERROR;
    }
    pfile_in_zip_read_info->stream_initialised = 0;

    if ((s->cur_file_info.compression_method != 0) &&
	(s->cur_file_info.compression_method != Z_DEFLATED))
	err = UNZ_BADZIPFILE;
    Store = s->cur_file_info.compression_method == 0;

    pfile_in_zip_read_info->crc32_wait = s->cur_file_info.crc;
    pfile_in_zip_read_info->crc32 = 0;
    pfile_in_zip_read_info->compression_method =
	s->cur_file_info.compression_method;
    pfile_in_zip_read_info->file = s->file;
    pfile_in_zip_read_info->byte_before_the_zipfile = 
	s->byte_before_the_zipfile;
    pfile_in_zip_read_info->stream.total_out = 0;

    if (!Store) {
	pfile_in_zip_read_info->stream.zalloc = (alloc_func) 0;
	pfile_in_zip_read_info->stream.zfree = (free_func) 0;
	pfile_in_zip_read_info->stream.opaque = (voidpf) 0;

	err = inflateInit2(&pfile_in_zip_read_info->stream, -MAX_WBITS);
	if (err == Z_OK)
	    pfile_in_zip_read_info->stream_initialised = 1;
	/* windowBits is passed < 0 to tell that there is no zlib header.
	   Note that in this case inflate *requires* an extra "dummy" byte
	   after the compressed stream in order to complete decompression and
	   return Z_STREAM_END. In unzip, i don't wait absolutely
	   Z_STREAM_END because I known the size of both compressed and
	   uncompressed data */
    }
    pfile_in_zip_read_info->rest_read_compressed =
	s->cur_file_info.compressed_size;
    pfile_in_zip_read_info->rest_read_uncompressed =
	s->cur_file_info.uncompressed_size;
    pfile_in_zip_read_info->pos_in_zipfile =
	s->cur_file_info_internal.offset_curfile + SIZEZIPLOCALHEADER +
	iSizeVar;
    pfile_in_zip_read_info->stream.avail_in = (uInt) 0;
    s->pfile_in_zip_read = pfile_in_zip_read_info;
    return UNZ_OK;
}


/*
  Read bytes from the current file.
  buf contain buffer where data must be copied
  len the size of buf.

  return the number of byte copied if somes bytes are copied
  return 0 if the end of file was reached
  return <0 with error code if there is an error
    (UNZ_ERRNO for IO error, or zLib error for uncompress error)
*/
static int 
unzReadCurrentFile(unzFile file, voidp buf, unsigned int len)
{
    int   err = UNZ_OK;
    uInt  iRead = 0;
    unz_s *s;
    file_in_zip_read_info_s *pfile_in_zip_read_info;

    if (file == NULL) return UNZ_PARAMERROR;
    s = (unz_s *) file;
    pfile_in_zip_read_info = s->pfile_in_zip_read;
    if (pfile_in_zip_read_info == NULL) return UNZ_PARAMERROR;
    if ((pfile_in_zip_read_info->read_buffer == NULL))
	return UNZ_END_OF_LIST_OF_FILE;
    if (len == 0) return 0;
    pfile_in_zip_read_info->stream.next_out = (Bytef *) buf;
    pfile_in_zip_read_info->stream.avail_out = (uInt) len;
    if (len > pfile_in_zip_read_info->rest_read_uncompressed)
	pfile_in_zip_read_info->stream.avail_out =
	    (uInt) pfile_in_zip_read_info->rest_read_uncompressed;

    while (pfile_in_zip_read_info->stream.avail_out > 0) {
	if ((pfile_in_zip_read_info->stream.avail_in == 0) &&
	    (pfile_in_zip_read_info->rest_read_compressed > 0)) {
	    uInt  uReadThis = UNZ_BUFSIZE;

	    if (pfile_in_zip_read_info->rest_read_compressed < uReadThis)
		uReadThis = (uInt) pfile_in_zip_read_info->rest_read_compressed;
	    if (uReadThis == 0) return UNZ_EOF;
	    if (fseek(pfile_in_zip_read_info->file,
		      pfile_in_zip_read_info->pos_in_zipfile +
	    pfile_in_zip_read_info->byte_before_the_zipfile, SEEK_SET) != 0)
		return UNZ_ERRNO;
	    if (fread(pfile_in_zip_read_info->read_buffer, uReadThis, 1,
		      pfile_in_zip_read_info->file) != 1)
		return UNZ_ERRNO;
	    pfile_in_zip_read_info->pos_in_zipfile += uReadThis;

	    pfile_in_zip_read_info->rest_read_compressed -= uReadThis;

	    pfile_in_zip_read_info->stream.next_in =
		(Bytef *) pfile_in_zip_read_info->read_buffer;
	    pfile_in_zip_read_info->stream.avail_in = (uInt) uReadThis;
	}
	if (pfile_in_zip_read_info->compression_method == 0) {
	    uInt  uDoCopy, i;

	    if (pfile_in_zip_read_info->stream.avail_out <
		pfile_in_zip_read_info->stream.avail_in)
		uDoCopy = pfile_in_zip_read_info->stream.avail_out;
	    else
		uDoCopy = pfile_in_zip_read_info->stream.avail_in;

	    for (i = 0; i < uDoCopy; i++)
		*(pfile_in_zip_read_info->stream.next_out + i) =
		    *(pfile_in_zip_read_info->stream.next_in + i);

	    pfile_in_zip_read_info->crc32 = crc32(pfile_in_zip_read_info->crc32,
				    pfile_in_zip_read_info->stream.next_out,
						  uDoCopy);
	    pfile_in_zip_read_info->rest_read_uncompressed -= uDoCopy;
	    pfile_in_zip_read_info->stream.avail_in -= uDoCopy;
	    pfile_in_zip_read_info->stream.avail_out -= uDoCopy;
	    pfile_in_zip_read_info->stream.next_out += uDoCopy;
	    pfile_in_zip_read_info->stream.next_in += uDoCopy;
	    pfile_in_zip_read_info->stream.total_out += uDoCopy;
	    iRead += uDoCopy;
	} else {
	    uLong uTotalOutBefore, uTotalOutAfter;
	    const Bytef *bufBefore;
	    uLong uOutThis;
	    int   flush = Z_SYNC_FLUSH;

	    uTotalOutBefore = pfile_in_zip_read_info->stream.total_out;
	    bufBefore = pfile_in_zip_read_info->stream.next_out;

	    /* if ((pfile_in_zip_read_info->rest_read_uncompressed ==
	       pfile_in_zip_read_info->stream.avail_out) &&
	       (pfile_in_zip_read_info->rest_read_compressed == 0)) flush =
	       Z_FINISH; */
	    err = inflate(&pfile_in_zip_read_info->stream, flush);

	    uTotalOutAfter = pfile_in_zip_read_info->stream.total_out;
	    uOutThis = uTotalOutAfter - uTotalOutBefore;

	    pfile_in_zip_read_info->crc32 =
		crc32(pfile_in_zip_read_info->crc32, bufBefore,
		      (uInt) (uOutThis));

	    pfile_in_zip_read_info->rest_read_uncompressed -=
		uOutThis;

	    iRead += (uInt) (uTotalOutAfter - uTotalOutBefore);

	    if (err == Z_STREAM_END) return (iRead == 0) ? UNZ_EOF : iRead;
	    if (err != Z_OK) break;
	}
    }
    if (err == Z_OK) return iRead;
    return err;
}

/*
  Close the file in zip opened with unzipOpenCurrentFile
  Return UNZ_CRCERROR if all the file was read but the CRC is not good
*/
static int 
unzCloseCurrentFile(unzFile file)
{
    int   err = UNZ_OK;

    unz_s *s;
    file_in_zip_read_info_s *pfile_in_zip_read_info;

    if (file == NULL) return UNZ_PARAMERROR;
    s = (unz_s *) file;
    pfile_in_zip_read_info = s->pfile_in_zip_read;
    if (pfile_in_zip_read_info == NULL) return UNZ_PARAMERROR;
    if (pfile_in_zip_read_info->rest_read_uncompressed == 0) {
	if (pfile_in_zip_read_info->crc32 != pfile_in_zip_read_info->crc32_wait)
	    err = UNZ_CRCERROR;
    }
    TRYFREE(pfile_in_zip_read_info->read_buffer);
    pfile_in_zip_read_info->read_buffer = NULL;
    if (pfile_in_zip_read_info->stream_initialised)
	inflateEnd(&pfile_in_zip_read_info->stream);
    pfile_in_zip_read_info->stream_initialised = 0;
    TRYFREE(pfile_in_zip_read_info);
    s->pfile_in_zip_read = NULL;
    return err;
}
