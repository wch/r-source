/*
 *  R : A Computer Language for Statistical Data Analysis
 *  file dounzip.c
 *  first part Copyright (C) 2002-2013  The R Core Team
 *  second part Copyright (C) 1998-2010 Gilles Vollant
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
 *  along with this program; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#define HAVE_BZIP2

#include <Defn.h>
#include <Fileio.h> /* for R_fopen */
#include "unzip.h"
#ifdef HAVE_SYS_STAT_H
# include <sys/stat.h>
#endif
#include <errno.h>

#ifdef Win32
#include <io.h> /* for mkdir */
#endif

/* cf do_dircreate in platform.c */
static int R_mkdir(char *path)
{
#ifdef Win32
    char local[PATH_MAX];
    strcpy(local, path);
    /* needed DOS paths on Win 9x */
    R_fixbackslash(local);
    return mkdir(local);
#endif
#ifdef Unix
    return mkdir(path, 0777);
#endif
}

#ifdef Win32
#include <windows.h>
static void setFileTime(const char *fn, uLong dosdate)
{
    HANDLE hFile;
    FILETIME ftm, ftLocal;

    hFile = CreateFileA(fn, GENERIC_READ | GENERIC_WRITE,
			0, NULL, OPEN_EXISTING, 0, NULL);
    if (hFile == INVALID_HANDLE_VALUE) return;
    DosDateTimeToFileTime((WORD)(dosdate >> 16), (WORD)dosdate, &ftLocal);
    LocalFileTimeToFileTime(&ftLocal, &ftm);
    SetFileTime(hFile, &ftm, NULL, &ftm);
    CloseHandle(hFile);
}
#else
# include <time.h>
# ifdef HAVE_UTIMES
#  include <sys/time.h>
# endif
# ifdef HAVE_UTIME_H
#  include <utime.h>
# endif
static void setFileTime(const char *fn, tm_unz tmu_date)
{
    struct tm dt;
    dt.tm_sec = tmu_date.tm_sec;
    dt.tm_min = tmu_date.tm_min;
    dt.tm_hour = tmu_date.tm_hour;
    dt.tm_mday = tmu_date.tm_mday;
    dt.tm_mon = tmu_date.tm_mon;
    if (tmu_date.tm_year > 1900)
	dt.tm_year = tmu_date.tm_year - 1900;
    else
	dt.tm_year = tmu_date.tm_year;
    dt.tm_isdst = -1;
    time_t ftime = mktime(&dt);
#if defined(HAVE_UTIMES)
    struct timeval times[2];

    times[0].tv_sec = times[1].tv_sec = ftime;
    times[0].tv_usec = times[1].tv_usec = 0;
    utimes(fn, times);
#elif defined(HAVE_UTIME)
    struct utimbuf settime;
    settime.actime = settime.modtime = ftime;
    utime(fn, &settime);
#endif
}
#endif

#define BUF_SIZE 4096
static int
extract_one(unzFile uf, const char *const dest, const char * const filename,
	    SEXP names, int *nnames, int overwrite, int junk, int setTime)
{
    int err = UNZ_OK;
    FILE *fout;
    char  outname[PATH_MAX], dirs[PATH_MAX], buf[BUF_SIZE], *p, *pp;
    char *fn, fn0[PATH_MAX];

    err = unzOpenCurrentFile(uf);
    if (err != UNZ_OK) return err;
    if (strlen(dest) > PATH_MAX - 1) return 1;
    strcpy(outname, dest);
    strcat(outname, FILESEP);
    unz_file_info64 file_info;
    char filename_inzip[PATH_MAX];
    err = unzGetCurrentFileInfo64(uf, &file_info, filename_inzip,
				  sizeof(filename_inzip), NULL, 0, NULL, 0);
    fn = filename_inzip; /* might be UTF-8 ... */
    if (filename) {
	if (strlen(dest) + strlen(filename) > PATH_MAX - 2) return 1;
	strncpy(fn0, filename, PATH_MAX); 
	fn = fn0;
    }
#ifdef Win32
    R_fixslash(fn);
#endif
    if (junk && strlen(fn) >= 2) { /* need / and basename */
	p = Rf_strrchr(fn, '/');
	if (p) fn = p+1;
    }
    strcat(outname, fn);

#ifdef Win32
    R_fixslash(outname); /* ensure path separator is / */
#endif
    p = outname + strlen(outname) - 1;
    if (*p == '/') { /* Directories are stored with trailing slash */
	if (!junk) {
	    *p = '\0';
	    if (!R_FileExists(outname)) {
		/* make parents as required: have already checked dest exists */
		pp = outname + strlen(dest) + 1;
		while((p = Rf_strchr(pp, '/'))) {
		    strcpy(dirs, outname);
		    dirs[p - outname] = '\0';
		    if (!R_FileExists(dirs)) R_mkdir(dirs);
		    pp = p + 1;
		}
		err = R_mkdir(outname);
	    }
	}
    } else {
	/* make parents as required: have already checked dest exists */
	pp = outname + strlen(dest) + 1;
	while((p = Rf_strchr(pp, '/'))) {
	    strcpy(dirs, outname);
	    dirs[p - outname] = '\0';
	    /* Rprintf("dirs is %s\n", dirs); */
	    if (!R_FileExists(dirs)) R_mkdir(dirs);
	    pp = p + 1;
	}
	/* Rprintf("extracting %s\n", outname); */
	if (!overwrite && R_FileExists(outname)) {
	    warning(_(" not overwriting file '%s"), outname);
	}
	fout = R_fopen(outname, "wb");
	int serrno = errno;
	if (!fout) {
	    unzCloseCurrentFile(uf);
	    error(_("cannot open file '%s': %s"), outname, strerror(serrno));
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
#ifdef Win32
    if (setTime) setFileTime(outname, file_info.dosDate);
#else
    if (setTime) setFileTime(outname, file_info.tmu_date);
#endif
    return err;
}


static int
zipunzip(const char *zipname, const char *dest, int nfiles, const char **files,
	 SEXP *pnames, int *nnames, int overwrite, int junk, int setTime)
{
    int   i, err = UNZ_OK;
    unzFile uf;
    SEXP names = *pnames;

    uf = unzOpen64(zipname);
    if (!uf) return 1;
    if (nfiles == 0) { /* all files */
	unz_global_info64 gi;
	unzGetGlobalInfo64(uf, &gi);
	for (i = 0; i < gi.number_entry; i++) {
	    if (i > 0) if ((err = unzGoToNextFile(uf)) != UNZ_OK) break;
	    if (*nnames+1 >= LENGTH(names)) {
		SEXP onames = names;
		names = allocVector(STRSXP, 2*LENGTH(names));
		UNPROTECT(1);
		PROTECT(names);
		copyVector(names, onames);
	    }
	    if ((err = extract_one(uf, dest, NULL, names, nnames, 
				   overwrite, junk, setTime)) != UNZ_OK) break;
#ifdef Win32
	    R_ProcessEvents();
#else
	    R_CheckUserInterrupt();
#endif
	}
    } else {
	for (i = 0; i < nfiles; i++) {
	    if ((err = unzLocateFile(uf, files[i], 1)) != UNZ_OK) break;
	    if ((err = extract_one(uf, dest, files[i], names, nnames, 
				   overwrite, junk, setTime)) != UNZ_OK) break;
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

static SEXP ziplist(const char *zipname)
{
    SEXP ans = R_NilValue, names, lengths, dates;
    unzFile uf;
    uLong i;
    unz_global_info64 gi;
    int err, nfiles;

    uf = unzOpen64(zipname);
    if (!uf) error(_("zip file '%s' cannot be opened"), zipname);

    gi.number_entry = 0; /* =Wall */
    err = unzGetGlobalInfo64 (uf, &gi);
    if (err != UNZ_OK)
        error("error %d with zipfile in unzGetGlobalInfo", err);
    nfiles = (int) gi.number_entry;
    /* name, length, datetime */
    PROTECT(ans = allocVector(VECSXP, 3));
    SET_VECTOR_ELT(ans, 0, names = allocVector(STRSXP, nfiles));
    SET_VECTOR_ELT(ans, 1, lengths = allocVector(REALSXP, nfiles));
    SET_VECTOR_ELT(ans, 2, dates = allocVector(STRSXP, nfiles));

    for (i = 0; i < nfiles; i++) {
        char filename_inzip[PATH_MAX], date[50];
        unz_file_info64 file_info;

        err = unzGetCurrentFileInfo64(uf, &file_info, filename_inzip, 
				      sizeof(filename_inzip), NULL, 0, NULL, 0);
        if (err != UNZ_OK)
            error("error %d with zipfile in unzGetCurrentFileInfo\n", err);
	/* In theory at least bit 11 of the flag tells us that the
	   filename is in UTF-8, so FIXME */
	SET_STRING_ELT(names, i, mkChar(filename_inzip));
	REAL(lengths)[i] = file_info.uncompressed_size;
	snprintf(date, 50, "%d-%02d-%02d %02d:%02d",
		 file_info.tmu_date.tm_year,
		 file_info.tmu_date.tm_mon + 1,
		 file_info.tmu_date.tm_mday,
		 file_info.tmu_date.tm_hour,
		 file_info.tmu_date.tm_min);
	SET_STRING_ELT(dates, i, mkChar(date));

        if (i < nfiles - 1) {
            err = unzGoToNextFile(uf);
            if (err != UNZ_OK)
                error("error %d with zipfile in unzGoToNextFile\n",err);
        }
    }
    unzClose(uf);

    UNPROTECT(1);
    return ans;
}

/* called from a .External in package 'utils', so managing
   the R_alloc stack here is prudence */
SEXP Runzip(SEXP args)
{
    SEXP  fn, ans, names = R_NilValue;
    char  zipname[PATH_MAX], dest[PATH_MAX];
    const char *p, **topics = NULL;
    int   i, ntopics, list, overwrite, junk, setTime, rc, nnames = 0;
    const void *vmax = vmaxget();

    if (!isString(CAR(args)) || LENGTH(CAR(args)) != 1)
	error(_("invalid zip name argument"));
    p = R_ExpandFileName(translateChar(STRING_ELT(CAR(args), 0)));
    if (strlen(p) > PATH_MAX - 1)
	error(_("zip path is too long"));
    strcpy(zipname, p);
    args = CDR(args);
    fn = CAR(args);
    ntopics = length(fn);
    if (ntopics > 0) {
	if (!isString(fn))
	    error(_("invalid '%s' argument"), "files");
	topics = (const char **) R_alloc(ntopics, sizeof(char *));
	for (i = 0; i < ntopics; i++)
	    topics[i] = translateChar(STRING_ELT(fn, i));
    }
    args = CDR(args);
    if (!isString(CAR(args)) || LENGTH(CAR(args)) != 1)
	error(_("invalid '%s' argument"), "exdir");
    p = R_ExpandFileName(translateChar(STRING_ELT(CAR(args), 0)));
    if (strlen(p) > PATH_MAX - 1)
	error(_("'exdir' is too long"));
    strcpy(dest, p);
    if (!R_FileExists(dest))
	error(_("'exdir' does not exist"));
    args = CDR(args);
    list = asLogical(CAR(args));
    if (list == NA_LOGICAL)
	error(_("invalid '%s' argument"), "list");
    if (list) return(ziplist(zipname));
    args = CDR(args);
    overwrite = asLogical(CAR(args));
    if (overwrite == NA_LOGICAL)
	error(_("invalid '%s' argument"), "overwrite");
    args = CDR(args);
    junk = asLogical(CAR(args));
    if (junk == NA_LOGICAL)
	error(_("invalid '%s' argument"), "junkpaths");
    args = CDR(args);
    setTime = asLogical(CAR(args));
    if (setTime == NA_LOGICAL)
	error(_("invalid '%s' argument"), "setTime");

    if (ntopics > 0)
	PROTECT(names = allocVector(STRSXP, ntopics));
    else
	PROTECT(names = allocVector(STRSXP, 5000));
    rc = zipunzip(zipname, dest, ntopics, topics, &names, &nnames, 
		  overwrite, junk, setTime);
    if (rc != UNZ_OK)
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
	    warning("internal error in 'unz' code");
	    break;
	case -200:
	    warning(_("write error in extracting from zip file"));
	    break;
	default:
	    warning(_("error %d in extracting from zip file"), rc);
	}
    PROTECT(ans = ScalarInteger(rc));
    PROTECT(names = lengthgets(names, nnames));
    setAttrib(ans, install("extracted"), names);
    UNPROTECT(3);
    vmaxset(vmax);
    return ans;
}

/* ------------------- unz connections --------------------- */

#include <Rconnections.h>

typedef struct unzconn {
    void *uf;
} *Runzconn;

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
	warning(_("invalid description of 'unz' connection"));
	return FALSE;
    }
    *p = '\0';
    uf = unzOpen64(path);
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
    return unzReadCurrentFile(uf, ptr, (unsigned int)(size*nitems))/size;
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
    if(!new) error(_("allocation of 'unz' connection failed"));
    new->class = (char *) malloc(strlen("unz") + 1);
    if(!new->class) {
	free(new);
	error(_("allocation of 'unz' connection failed"));
    }
    strcpy(new->class, "unz");
    new->description = (char *) malloc(strlen(description) + 1);
    if(!new->description) {
	free(new->class); free(new);
	error(_("allocation of 'unz' connection failed"));
    }
    init_con(new, description, CE_NATIVE, mode);

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
    new->private = (void *) malloc(sizeof(struct unzconn));
    if(!new->private) {
	free(new->description); free(new->class); free(new);
	error(_("allocation of 'unz' connection failed"));
    }
    return new;
}

       /* =================== second part ====================== */

/* From minizip contribution to zlib 1.2.3, updated for 1.2.5 */

/* unzip.c -- IO for uncompress .zip files using zlib
   Version 1.01e, February 12th, 2005

   Copyright (C) 1998-2005 Gilles Vollant

   Read unzip.h for more info
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "zlib.h"
#include "unzip.h"
/*
#ifdef HAVE_ERRNO_H
#include <errno.h>
#else
extern int errno;
#endif
*/

#define local static

#ifndef CASESENSITIVITYDEFAULT_NO
#  if !defined(unix) && !defined(CASESENSITIVITYDEFAULT_YES)
#    define CASESENSITIVITYDEFAULT_NO
#  endif
#endif


#ifndef UNZ_BUFSIZE
#define UNZ_BUFSIZE (16384)
#endif

#ifndef UNZ_MAXFILENAMEINZIP
#define UNZ_MAXFILENAMEINZIP (256)
#endif

#ifndef ALLOC
# define ALLOC(size) (malloc(size))
#endif
#ifndef TRYFREE
# define TRYFREE(p) {if (p) free(p);}
#endif

#define SIZECENTRALDIRITEM (0x2e)
#define SIZEZIPLOCALHEADER (0x1e)


static const char unz_copyright[] =
   " unzip 1.01 Copyright 1998-2004 Gilles Vollant - http://www.winimage.com/zLibDll";

/* unz_file_info_interntal contain internal info about a file in zipfile*/
typedef struct unz_file_info64_internal_s
{
    ZPOS64_T offset_curfile;/* relative offset of local header 8 bytes */
} unz_file_info64_internal;


/* file_in_zip_read_info_s contain internal information about a file in zipfile,
    when reading and decompress it */
typedef struct
{
    char  *read_buffer;         /* internal buffer for compressed data */
    z_stream stream;            /* zLib stream structure for inflate */

#ifdef HAVE_BZIP2
    bz_stream bstream;          /* bzLib stream structure for bziped */
#endif

    ZPOS64_T pos_in_zipfile;       /* position in byte on the zipfile, for fseek*/
    uLong stream_initialised;   /* flag set if stream structure is initialised*/

    ZPOS64_T offset_local_extrafield;/* offset of the local extra field */
    uInt  size_local_extrafield;  /* size of the local extra field */
    ZPOS64_T pos_local_extrafield;   /* position in the local extra field in read*/
    ZPOS64_T total_out_64;

    uLong crc32;                /* crc32 of all data uncompressed */
    uLong crc32_wait;           /* crc32 we must obtain after decompress all */
    ZPOS64_T rest_read_compressed; /* number of byte to be decompressed */
    ZPOS64_T rest_read_uncompressed;/*number of byte to be obtained after decomp*/
    voidpf filestream;        /* io structore of the zipfile */
    uLong compression_method;   /* compression method (0 == store) */
    ZPOS64_T byte_before_the_zipfile;/* byte before the zipfile, (>0 for sfx)*/
    int   raw;
} file_in_zip64_read_info_s;


/* unz64_s contain internal information about the zipfile
*/
typedef struct
{
    int is64bitOpenFunction;
    voidpf filestream;        /* io structore of the zipfile */
    unz_global_info64 gi;       /* public global information */
    ZPOS64_T byte_before_the_zipfile;/* byte before the zipfile, (>0 for sfx)*/
    ZPOS64_T num_file;             /* number of the current file in the zipfile*/
    ZPOS64_T pos_in_central_dir;   /* pos of the current file in the central dir*/
    ZPOS64_T current_file_ok;      /* flag about the usability of the current file*/
    ZPOS64_T central_pos;          /* position of the beginning of the central dir*/

    ZPOS64_T size_central_dir;     /* size of the central directory  */
    ZPOS64_T offset_central_dir;   /* offset of start of central directory with
                                   respect to the starting disk number */

    unz_file_info64 cur_file_info; /* public info about the current file in zip*/
    unz_file_info64_internal cur_file_info_internal; /* private info about it*/
    file_in_zip64_read_info_s* pfile_in_zip_read; /* structure about the current
                                        file if we are decompressing it */
    int encrypted;

    int isZip64;
} unz64_s;


/* ===========================================================================
     Read a byte from a gz_stream; update next_in and avail_in. Return EOF
   for end of file.
   IN assertion: the stream s has been sucessfully opened for reading.
*/

local int unz64local_getByte(voidpf filestream, int *pi)
{
    unsigned char c;
    int err = (int) fread_func(filestream, &c, 1);
    if (err == 1)
    {
        *pi = (int)c;
        return UNZ_OK;
    }
    else
    {
        if (ferror_func(filestream))
            return UNZ_ERRNO;
        else
            return UNZ_EOF;
    }
}


/* ===========================================================================
   Reads a long in LSB order from the given gz_stream. Sets
*/
local int unz64local_getShort (voidpf filestream, uLong *pX)
{
    uLong x ;
    int i = 0;
    int err;

    err = unz64local_getByte(filestream,&i);
    x = (uLong)i;

    if (err == UNZ_OK)
        err = unz64local_getByte(filestream,&i);
    x |= ((uLong)i)<<8;

    if (err == UNZ_OK)
        *pX = x;
    else
        *pX = 0;
    return err;
}

local int unz64local_getLong (voidpf filestream, uLong *pX)
{
    uLong x ;
    int i = 0;
    int err;

    err = unz64local_getByte(filestream, &i);
    x = (uLong)i;

    if (err == UNZ_OK)
        err = unz64local_getByte(filestream, &i);
    x |= ((uLong)i)<<8;

    if (err == UNZ_OK)
        err = unz64local_getByte(filestream, &i);
    x |= ((uLong)i)<<16;

    if (err == UNZ_OK)
        err = unz64local_getByte(filestream, &i);
    x += ((uLong)i)<<24;

    if (err == UNZ_OK)
        *pX = x;
    else
        *pX = 0;
    return err;
}

local int unz64local_getLong64 (voidpf filestream, ZPOS64_T *pX)
{
    ZPOS64_T x ;
    int i = 0;
    int err;

    err = unz64local_getByte(filestream,&i);
    x = (ZPOS64_T)i;

    if (err == UNZ_OK)
        err = unz64local_getByte(filestream,&i);
    x |= ((ZPOS64_T)i) << 8;

    if (err == UNZ_OK)
        err = unz64local_getByte(filestream,&i);
    x |= ((ZPOS64_T)i) << 16;

    if (err == UNZ_OK)
        err = unz64local_getByte(filestream,&i);
    x |= ((ZPOS64_T)i) << 24;

    if (err == UNZ_OK)
        err = unz64local_getByte(filestream,&i);
    x |= ((ZPOS64_T)i) << 32;

    if (err == UNZ_OK)
        err = unz64local_getByte(filestream,&i);
    x |= ((ZPOS64_T)i) << 40;

    if (err == UNZ_OK)
        err = unz64local_getByte(filestream,&i);
    x |= ((ZPOS64_T)i) << 48;

    if (err == UNZ_OK)
        err = unz64local_getByte(filestream,&i);
    x |= ((ZPOS64_T)i) << 56;

    if (err == UNZ_OK)
        *pX = x;
    else
        *pX = 0;
    return err;
}

/* My own strcmpi / strcasecmp */
local int 
strcmpcasenosensitive_internal (const char* fileName1, const char* fileName2)
{
    for (;;)
    {
        char c1 = *(fileName1++);
        char c2 = *(fileName2++);
        if ((c1 >= 'a') && (c1 <= 'z'))
            c1 -= 0x20;
        if ((c2 >= 'a') && (c2 <= 'z'))
            c2 -= 0x20;
        if (c1 == '\0')
            return ((c2 == '\0') ? 0 : -1);
        if (c2 == '\0')
            return 1;
        if (c1 < c2)
            return -1;
        if (c1 > c2)
            return 1;
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
   If iCaseSenisivity = 1, comparision is case sensitivity (like strcmp)
   If iCaseSenisivity = 2, comparision is not case sensitivity (like strcmpi
                                                                or strcasecmp)
   If iCaseSenisivity = 0, case sensitivity is defaut of your operating system
        (like 1 on Unix, 2 on Windows)

*/
extern int ZEXPORT unzStringFileNameCompare (const char*  fileName1,
					     const char*  fileName2,
					     int iCaseSensitivity)

{
    if (iCaseSensitivity == 0)
        iCaseSensitivity = CASESENSITIVITYDEFAULTVALUE;

    if (iCaseSensitivity == 1)
        return strcmp(fileName1, fileName2);

    return STRCMPCASENOSENTIVEFUNCTION(fileName1, fileName2);
}

#ifndef BUFREADCOMMENT
#define BUFREADCOMMENT (0x400)
#endif

/*
  Locate the Central directory of a zipfile (at the end, just before
    the global comment)
*/
local ZPOS64_T unz64local_SearchCentralDir(voidpf filestream)
{
    unsigned char* buf;
    ZPOS64_T uSizeFile;
    ZPOS64_T uBackRead;
    ZPOS64_T uMaxBack = 0xffff; /* maximum size of global comment */
    ZPOS64_T uPosFound = 0;

    if (fseek_func(filestream, 0, ZLIB_FILEFUNC_SEEK_END) != 0)
        return 0;


    uSizeFile = ftell_func(filestream);

    if (uMaxBack>uSizeFile)
        uMaxBack = uSizeFile;

    buf = (unsigned char*)ALLOC(BUFREADCOMMENT+4);
    if (buf == NULL)
        return 0;

    uBackRead = 4;
    while (uBackRead < uMaxBack)
    {
        size_t uReadSize;
        ZPOS64_T uReadPos ;
        int i;
        if (uBackRead+BUFREADCOMMENT > uMaxBack)
            uBackRead = uMaxBack;
        else
            uBackRead += BUFREADCOMMENT;
        uReadPos = uSizeFile - uBackRead ;

        uReadSize = ((BUFREADCOMMENT+4) < (uSizeFile-uReadPos)) ?
                     (BUFREADCOMMENT+4) : (uLong)(uSizeFile-uReadPos);
        if (fseek_func(filestream, uReadPos, ZLIB_FILEFUNC_SEEK_SET) != 0)
            break;

        if (fread_func(filestream, buf, uReadSize) != uReadSize)
            break;

        for (i = (int)uReadSize-3; (i--) > 0;)
            if (((*(buf+i)) == 0x50) && ((*(buf+i+1)) == 0x4b) &&
                ((*(buf+i+2)) == 0x05) && ((*(buf+i+3)) == 0x06))
            {
                uPosFound = uReadPos+i;
                break;
            }

        if (uPosFound != 0)
            break;
    }
    TRYFREE(buf);
    return uPosFound;
}


/*
  Locate the Central directory 64 of a zipfile (at the end, just before
    the global comment)
*/
local ZPOS64_T 
unz64local_SearchCentralDir64(voidpf filestream)
{
    unsigned char* buf;
    ZPOS64_T uSizeFile;
    ZPOS64_T uBackRead;
    ZPOS64_T uMaxBack = 0xffff; /* maximum size of global comment */
    ZPOS64_T uPosFound = 0;
    uLong uL;
    ZPOS64_T relativeOffset;

    if (fseek_func(filestream, 0, ZLIB_FILEFUNC_SEEK_END) != 0)
        return 0;


    uSizeFile = ftell_func(filestream);

    if (uMaxBack > uSizeFile)
        uMaxBack = uSizeFile;

    buf = (unsigned char*)ALLOC(BUFREADCOMMENT+4);
    if (buf == NULL)
        return 0;

    uBackRead = 4;
    while (uBackRead<uMaxBack)
    {
        size_t uReadSize;
        ZPOS64_T uReadPos;
        int i;
        if (uBackRead+BUFREADCOMMENT>uMaxBack)
            uBackRead = uMaxBack;
        else
            uBackRead += BUFREADCOMMENT;
        uReadPos = uSizeFile - uBackRead ;

        uReadSize = ((BUFREADCOMMENT+4) < (uSizeFile-uReadPos)) ?
                     (BUFREADCOMMENT+4) : (uLong)(uSizeFile-uReadPos);
        if (fseek_func(filestream, uReadPos, ZLIB_FILEFUNC_SEEK_SET) != 0)
            break;

        if (fread_func(filestream, buf, uReadSize) != uReadSize)
            break;

        for (i = (int)uReadSize-3; (i--) > 0;)
            if (((*(buf+i)) == 0x50) && ((*(buf+i+1)) == 0x4b) &&
                ((*(buf+i+2)) == 0x06) && ((*(buf+i+3)) == 0x07))
            {
                uPosFound = uReadPos+i;
                break;
            }

        if (uPosFound != 0)
            break;
    }
    TRYFREE(buf);
    if (uPosFound == 0)
        return 0;

    /* Zip64 end of central directory locator */
    if (fseek_func(filestream, uPosFound, ZLIB_FILEFUNC_SEEK_SET) != 0)
        return 0;

    /* the signature, already checked */
    if (unz64local_getLong(filestream, &uL) != UNZ_OK)
        return 0;

    /* number of the disk with the start of the zip64 end of  central directory */
    if (unz64local_getLong(filestream, &uL) != UNZ_OK)
        return 0;
    if (uL != 0)
        return 0;

    /* relative offset of the zip64 end of central directory record */
    if (unz64local_getLong64(filestream, &relativeOffset) != UNZ_OK)
        return 0;

    /* total number of disks */
    if (unz64local_getLong(filestream, &uL) != UNZ_OK)
        return 0;
    if (uL != 1)
        return 0;

    /* Goto end of central directory record */
    if (fseek_func(filestream, relativeOffset, ZLIB_FILEFUNC_SEEK_SET) != 0)
        return 0;

     /* the signature */
    if (unz64local_getLong(filestream, &uL) != UNZ_OK)
        return 0;

    if (uL != 0x06064b50)
        return 0;

    return relativeOffset;
}

/*
  Open a Zip file. path contain the full pathname (by example,
     on a Windows NT computer "c:\\test\\zlib114.zip" or on an Unix computer
     "zlib/zlib114.zip".
     If the zipfile cannot be opened (file doesn't exist or in not valid), the
       return value is NULL.
     Else, the return value is a unzFile Handle, usable with other function
       of this unzip package.
*/
local unzFile unzOpenInternal (const void *path, int is64bitOpenFunction)
{
    unz64_s us;
    unz64_s *s;
    ZPOS64_T central_pos;
    uLong   uL;

    uLong number_disk;          /* number of the current dist, used for
                                   spaning ZIP, unsupported, always 0*/
    uLong number_disk_with_CD;  /* number the the disk with central dir, used
                                   for spaning ZIP, unsupported, always 0*/
    ZPOS64_T number_entry_CD;      /* total number of entries in
                                   the central dir
                                   (same than number_entry on nospan) */

    int err=UNZ_OK;

    if (unz_copyright[0] != ' ')
        return NULL;

    us.is64bitOpenFunction = is64bitOpenFunction;



    us.filestream = fopen_func(path,
			       ZLIB_FILEFUNC_MODE_READ |
			       ZLIB_FILEFUNC_MODE_EXISTING);
    if (us.filestream == NULL)
        return NULL;

    central_pos = unz64local_SearchCentralDir64(us.filestream);
    if (central_pos)
    {
        uLong uS;
        ZPOS64_T uL64;

        us.isZip64 = 1;

        if (fseek_func(us.filestream, central_pos, ZLIB_FILEFUNC_SEEK_SET) != 0)
        err = UNZ_ERRNO;

        /* the signature, already checked */
        if (unz64local_getLong(us.filestream, &uL) != UNZ_OK)
            err = UNZ_ERRNO;

        /* size of zip64 end of central directory record */
        if (unz64local_getLong64(us.filestream, &uL64) != UNZ_OK)
            err = UNZ_ERRNO;

        /* version made by */
        if (unz64local_getShort(us.filestream, &uS) != UNZ_OK)
            err = UNZ_ERRNO;

        /* version needed to extract */
        if (unz64local_getShort(us.filestream, &uS) != UNZ_OK)
            err = UNZ_ERRNO;

        /* number of this disk */
        if (unz64local_getLong(us.filestream, &number_disk) != UNZ_OK)
            err = UNZ_ERRNO;

        /* number of the disk with the start of the central directory */
        if (unz64local_getLong(us.filestream, &number_disk_with_CD) != UNZ_OK)
            err = UNZ_ERRNO;

        /* total number of entries in the central directory on this disk */
        if (unz64local_getLong64(us.filestream, &us.gi.number_entry) != UNZ_OK)
            err = UNZ_ERRNO;

        /* total number of entries in the central directory */
        if (unz64local_getLong64(us.filestream, &number_entry_CD) != UNZ_OK)
            err = UNZ_ERRNO;

        if ((number_entry_CD != us.gi.number_entry) ||
            (number_disk_with_CD != 0) ||
            (number_disk != 0))
            err = UNZ_BADZIPFILE;

        /* size of the central directory */
        if (unz64local_getLong64(us.filestream, &us.size_central_dir) != UNZ_OK)
            err = UNZ_ERRNO;

        /* offset of start of central directory with respect to the
          starting disk number */
        if (unz64local_getLong64(us.filestream, &us.offset_central_dir) != UNZ_OK)
            err = UNZ_ERRNO;

        us.gi.size_comment = 0;
    }
    else
    {
        central_pos = unz64local_SearchCentralDir(us.filestream);
        if (central_pos == 0)
            err = UNZ_ERRNO;

        us.isZip64 = 0;

        if (fseek_func(us.filestream, central_pos, ZLIB_FILEFUNC_SEEK_SET) != 0)
            err = UNZ_ERRNO;

        /* the signature, already checked */
        if (unz64local_getLong(us.filestream, &uL) != UNZ_OK)
            err = UNZ_ERRNO;

        /* number of this disk */
        if (unz64local_getShort(us.filestream, &number_disk) != UNZ_OK)
            err = UNZ_ERRNO;

        /* number of the disk with the start of the central directory */
        if (unz64local_getShort(us.filestream, &number_disk_with_CD) != UNZ_OK)
            err = UNZ_ERRNO;

        /* total number of entries in the central dir on this disk */
        if (unz64local_getShort(us.filestream, &uL) != UNZ_OK)
            err = UNZ_ERRNO;
        us.gi.number_entry = uL;

        /* total number of entries in the central dir */
        if (unz64local_getShort(us.filestream ,&uL) != UNZ_OK)
            err = UNZ_ERRNO;
        number_entry_CD = uL;

        if ((number_entry_CD != us.gi.number_entry) ||
            (number_disk_with_CD != 0) ||
            (number_disk != 0))
            err = UNZ_BADZIPFILE;

        /* size of the central directory */
        if (unz64local_getLong(us.filestream, &uL) != UNZ_OK)
            err = UNZ_ERRNO;
        us.size_central_dir = uL;

        /* offset of start of central directory with respect to the
            starting disk number */
        if (unz64local_getLong(us.filestream, &uL) != UNZ_OK)
            err = UNZ_ERRNO;
        us.offset_central_dir = uL;

        /* zipfile comment length */
        if (unz64local_getShort(us.filestream, &us.gi.size_comment) != UNZ_OK)
            err = UNZ_ERRNO;
    }

    if ((central_pos<us.offset_central_dir+us.size_central_dir) &&
        (err == UNZ_OK))
        err = UNZ_BADZIPFILE;

    if (err != UNZ_OK)
    {
        fclose_func(us.filestream);
        return NULL;
    }

    us.byte_before_the_zipfile = central_pos -
                            (us.offset_central_dir+us.size_central_dir);
    us.central_pos = central_pos;
    us.pfile_in_zip_read = NULL;
    us.encrypted = 0;


    s=(unz64_s*)ALLOC(sizeof(unz64_s));
    if( s != NULL)
    {
        *s = us;
        unzGoToFirstFile((unzFile)s);
    }
    return (unzFile)s;
}


#ifdef UNUSED
extern unzFile ZEXPORT unzOpen (const char *path)
{
    return unzOpenInternal(path, 0);
}
#endif

extern unzFile ZEXPORT unzOpen64 (const void *path)
{
    return unzOpenInternal(path, 1);
}

/*
  Close a ZipFile opened with unzipOpen.
  If there is files inside the .Zip opened with unzipOpenCurrentFile (see later),
    these files MUST be closed with unzipCloseCurrentFile before call unzipClose.
  return UNZ_OK if there is no problem. */
extern int ZEXPORT unzClose (unzFile file)
{
    unz64_s* s;
    if (file == NULL)
        return UNZ_PARAMERROR;
    s = (unz64_s*) file;

    if (s->pfile_in_zip_read != NULL)
        unzCloseCurrentFile(file);

    fclose_func(s->filestream);
    TRYFREE(s);
    return UNZ_OK;
}


/*
  Write info about the ZipFile in the *pglobal_info structure.
  No preparation of the structure is needed
  return UNZ_OK if there is no problem. */
extern int ZEXPORT 
unzGetGlobalInfo64 (unzFile file, unz_global_info64* pglobal_info)
{
    unz64_s* s;
    if (file == NULL)
        return UNZ_PARAMERROR;
    s = (unz64_s*)file;
    *pglobal_info = s->gi;
    return UNZ_OK;
}

/*
   Translate date/time from Dos format to tm_unz (readable more easily)
*/
local void unz64local_DosDateToTmuDate (ZPOS64_T ulDosDate, tm_unz* ptm)
{
    ZPOS64_T uDate;
    uDate = (ZPOS64_T)(ulDosDate>>16);
    ptm->tm_mday = (uInt)(uDate&0x1f) ;
    ptm->tm_mon =  (uInt)((((uDate)&0x1E0)/0x20)-1) ;
    ptm->tm_year = (uInt)(((uDate&0x0FE00)/0x0200)+1980) ;

    ptm->tm_hour = (uInt) ((ulDosDate &0xF800)/0x800);
    ptm->tm_min =  (uInt) ((ulDosDate&0x7E0)/0x20) ;
    ptm->tm_sec =  (uInt) (2*(ulDosDate&0x1f)) ;
}

/*
  Get Info about the current file in the zipfile, with internal only info
*/
local int unz64local_GetCurrentFileInfoInternal (unzFile file,
                                                  unz_file_info64 *pfile_info,
                                                  unz_file_info64_internal
                                                  *pfile_info_internal,
                                                  char *szFileName,
                                                  uLong fileNameBufferSize,
                                                  void *extraField,
                                                  uLong extraFieldBufferSize,
                                                  char *szComment,
                                                  uLong commentBufferSize)
{
    unz64_s* s;
    unz_file_info64 file_info;
    unz_file_info64_internal file_info_internal;
    int err = UNZ_OK;
    uLong uMagic;
    long lSeek = 0;
    uLong uL;

    if (file == NULL)
        return UNZ_PARAMERROR;
    s = (unz64_s*)file;
    if (fseek_func(s->filestream,
		   s->pos_in_central_dir+s->byte_before_the_zipfile,
		   ZLIB_FILEFUNC_SEEK_SET) != 0)
        err = UNZ_ERRNO;


    /* we check the magic */
    if (err == UNZ_OK)
    {
        if (unz64local_getLong(s->filestream,&uMagic) != UNZ_OK)
            err = UNZ_ERRNO;
        else if (uMagic != 0x02014b50)
            err = UNZ_BADZIPFILE;
    }

    if (unz64local_getShort(s->filestream,&file_info.version) != UNZ_OK)
        err = UNZ_ERRNO;

    if (unz64local_getShort(s->filestream,&file_info.version_needed) != UNZ_OK)
        err = UNZ_ERRNO;

    if (unz64local_getShort(s->filestream,&file_info.flag) != UNZ_OK)
        err = UNZ_ERRNO;

    if (unz64local_getShort(s->filestream,&file_info.compression_method) != UNZ_OK)
        err = UNZ_ERRNO;

    if (unz64local_getLong(s->filestream,&file_info.dosDate) != UNZ_OK)
        err = UNZ_ERRNO;

    unz64local_DosDateToTmuDate(file_info.dosDate,&file_info.tmu_date);

    if (unz64local_getLong(s->filestream,&file_info.crc) != UNZ_OK)
        err = UNZ_ERRNO;

    if (unz64local_getLong(s->filestream,&uL) != UNZ_OK)
        err = UNZ_ERRNO;
    file_info.compressed_size = uL;

    if (unz64local_getLong(s->filestream,&uL) != UNZ_OK)
        err = UNZ_ERRNO;
    file_info.uncompressed_size = uL;

    if (unz64local_getShort(s->filestream,&file_info.size_filename) != UNZ_OK)
        err = UNZ_ERRNO;

    if (unz64local_getShort(s->filestream,&file_info.size_file_extra) != UNZ_OK)
        err = UNZ_ERRNO;

    if (unz64local_getShort(s->filestream,&file_info.size_file_comment) != UNZ_OK)
        err = UNZ_ERRNO;

    if (unz64local_getShort(s->filestream,&file_info.disk_num_start) != UNZ_OK)
        err = UNZ_ERRNO;

    if (unz64local_getShort(s->filestream,&file_info.internal_fa) != UNZ_OK)
        err = UNZ_ERRNO;

    if (unz64local_getLong(s->filestream,&file_info.external_fa) != UNZ_OK)
        err = UNZ_ERRNO;

                // relative offset of local header
    if (unz64local_getLong(s->filestream,&uL) != UNZ_OK)
        err = UNZ_ERRNO;
    file_info_internal.offset_curfile = uL;

    lSeek += file_info.size_filename;
    if ((err == UNZ_OK) && (szFileName != NULL))
    {
        size_t uSizeRead ;
        if (file_info.size_filename<fileNameBufferSize)
        {
            *(szFileName+file_info.size_filename)='\0';
            uSizeRead = file_info.size_filename;
        }
        else
            uSizeRead = fileNameBufferSize;

        if ((file_info.size_filename>0) && (fileNameBufferSize>0))
            if (fread_func(s->filestream, szFileName, uSizeRead) != uSizeRead)
                err = UNZ_ERRNO;
        lSeek -= uSizeRead;
    }

    // Read extrafield
    if ((err == UNZ_OK) && (extraField != NULL))
    {
        size_t uSizeRead;
        if (file_info.size_file_extra < extraFieldBufferSize)
            uSizeRead = file_info.size_file_extra;
        else
            uSizeRead = extraFieldBufferSize;

        if (lSeek != 0)
        {
            if (fseek_func(s->filestream, lSeek, ZLIB_FILEFUNC_SEEK_CUR) == 0)
                lSeek = 0;
            else
                err = UNZ_ERRNO;
        }

        if ((file_info.size_file_extra>0) && (extraFieldBufferSize>0))
            if (fread_func(s->filestream, extraField, (size_t) uSizeRead) 
		!= uSizeRead) err = UNZ_ERRNO;

        lSeek += file_info.size_file_extra - (uLong)uSizeRead;
    }
    else
        lSeek += file_info.size_file_extra;


    if ((err == UNZ_OK) && (file_info.size_file_extra != 0))
    {
	uLong acc = 0;

        // since lSeek now points to after the extra field we need to move back
        lSeek -= file_info.size_file_extra;

        if (lSeek != 0)
        {
            if (fseek_func(s->filestream, lSeek, ZLIB_FILEFUNC_SEEK_CUR) == 0)
                lSeek = 0;
            else
                err = UNZ_ERRNO;
        }

        while(acc < file_info.size_file_extra)
        {
            uLong headerId;
	    uLong dataSize;

            if (unz64local_getShort(s->filestream,&headerId) != UNZ_OK)
                err = UNZ_ERRNO;

            if (unz64local_getShort(s->filestream,&dataSize) != UNZ_OK)
                err = UNZ_ERRNO;

            /* ZIP64 extra fields */
            if (headerId == 0x0001)
            {
		uLong uL;
		
		if(file_info.uncompressed_size == (ZPOS64_T)(unsigned long)-1)
		{
		    if (unz64local_getLong64(s->filestream,
					     &file_info.uncompressed_size) != UNZ_OK)
			err = UNZ_ERRNO;
		}
		
		if(file_info.compressed_size == (ZPOS64_T)(unsigned long)-1)
		{
		    if (unz64local_getLong64(s->filestream,
					     &file_info.compressed_size) != UNZ_OK)
			err = UNZ_ERRNO;
		}

		if(file_info_internal.offset_curfile == (ZPOS64_T)(unsigned long)-1)
		{
		    /* Relative Header offset */
		    if (unz64local_getLong64(s->filestream,
					     &file_info_internal.offset_curfile) != UNZ_OK)
			err = UNZ_ERRNO;
		}

		if(file_info.disk_num_start == (unsigned long)-1)
		{
		    /* Disk Start Number */
		    if (unz64local_getLong(s->filestream,&uL) != UNZ_OK)
			err = UNZ_ERRNO;
		}

            }
            else
            {
                if (fseek_func(s->filestream, dataSize, ZLIB_FILEFUNC_SEEK_CUR) != 0)
                    err = UNZ_ERRNO;
            }

            acc += 2 + 2 + dataSize;
        }
    }

    if ((err == UNZ_OK) && (szComment != NULL))
    {
        size_t uSizeRead ;
        if (file_info.size_file_comment<commentBufferSize)
        {
            *(szComment+file_info.size_file_comment) = '\0';
            uSizeRead = file_info.size_file_comment;
        }
        else
            uSizeRead = commentBufferSize;

        if (lSeek != 0)
        {
            if (fseek_func(s->filestream, lSeek, ZLIB_FILEFUNC_SEEK_CUR) ==  0)
                lSeek = 0;
            else
                err = UNZ_ERRNO;
        }

        if ((file_info.size_file_comment>0) && (commentBufferSize>0))
            if (fread_func(s->filestream,szComment,uSizeRead) != uSizeRead)
                err = UNZ_ERRNO;
        lSeek += file_info.size_file_comment - uSizeRead;
    }
    else
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
extern int ZEXPORT 
unzGetCurrentFileInfo64 (unzFile file,
			 unz_file_info64 * pfile_info,
			 char * szFileName, uLong fileNameBufferSize,
			 void *extraField, uLong extraFieldBufferSize,
			 char* szComment,  uLong commentBufferSize)
{
    return unz64local_GetCurrentFileInfoInternal(file,pfile_info,NULL,
                                                szFileName,fileNameBufferSize,
                                                extraField,extraFieldBufferSize,
                                                szComment,commentBufferSize);
}

/*
  Set the current file of the zipfile to the first file.
  return UNZ_OK if there is no problem
*/
extern int ZEXPORT unzGoToFirstFile (unzFile file)
{
    int err = UNZ_OK;
    unz64_s* s;
    if (file == NULL)
        return UNZ_PARAMERROR;
    s = (unz64_s*)file;
    s->pos_in_central_dir = s->offset_central_dir;
    s->num_file = 0;
    err = unz64local_GetCurrentFileInfoInternal(file,&s->cur_file_info,
						&s->cur_file_info_internal,
						NULL,0,NULL,0,NULL,0);
    s->current_file_ok = (err == UNZ_OK);
    return err;
}

/*
  Set the current file of the zipfile to the next file.
  return UNZ_OK if there is no problem
  return UNZ_END_OF_LIST_OF_FILE if the actual file was the latest.
*/
extern int ZEXPORT unzGoToNextFile (unzFile  file)
{
    unz64_s* s;
    int err;

    if (file == NULL)
        return UNZ_PARAMERROR;
    s = (unz64_s*)file;
    if (!s->current_file_ok)
        return UNZ_END_OF_LIST_OF_FILE;
    if (s->gi.number_entry != 0xffff)    /* 2^16 files overflow hack */
      if (s->num_file+1 == s->gi.number_entry)
        return UNZ_END_OF_LIST_OF_FILE;

    s->pos_in_central_dir += 
	SIZECENTRALDIRITEM + s->cur_file_info.size_filename +
	s->cur_file_info.size_file_extra + s->cur_file_info.size_file_comment ;
    s->num_file++;
    err = unz64local_GetCurrentFileInfoInternal(file,&s->cur_file_info,
                                               &s->cur_file_info_internal,
                                               NULL,0,NULL,0,NULL,0);
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
extern int ZEXPORT unzLocateFile (unzFile file, const char *szFileName, int iCaseSensitivity)
{
    unz64_s* s;
    int err;

    /* We remember the 'current' position in the file so that we can jump
     * back there if we fail.
     */
    unz_file_info64 cur_file_infoSaved;
    unz_file_info64_internal cur_file_info_internalSaved;
    ZPOS64_T num_fileSaved;
    ZPOS64_T pos_in_central_dirSaved;


    if (file == NULL)
        return UNZ_PARAMERROR;

    if (strlen(szFileName) >= UNZ_MAXFILENAMEINZIP)
        return UNZ_PARAMERROR;

    s = (unz64_s*)file;
    if (!s->current_file_ok)
        return UNZ_END_OF_LIST_OF_FILE;

    /* Save the current state */
    num_fileSaved = s->num_file;
    pos_in_central_dirSaved = s->pos_in_central_dir;
    cur_file_infoSaved = s->cur_file_info;
    cur_file_info_internalSaved = s->cur_file_info_internal;

    err = unzGoToFirstFile(file);

    while (err == UNZ_OK)
    {
        char szCurrentFileName[UNZ_MAXFILENAMEINZIP+1];
        err = unzGetCurrentFileInfo64(file, NULL,
				      szCurrentFileName,
				      sizeof(szCurrentFileName)-1,
				      NULL, 0, NULL, 0);
        if (err == UNZ_OK)
        {
            if (unzStringFileNameCompare(szCurrentFileName,
					 szFileName,iCaseSensitivity) == 0)
                return UNZ_OK;
            err = unzGoToNextFile(file);
        }
    }

    /* We failed, so restore the state of the 'current file' to where we
     * were.
     */
    s->num_file = num_fileSaved ;
    s->pos_in_central_dir = pos_in_central_dirSaved ;
    s->cur_file_info = cur_file_infoSaved;
    s->cur_file_info_internal = cur_file_info_internalSaved;
    return err;
}


/*
///////////////////////////////////////////
// Contributed by Ryan Haksi (mailto://cryogen@infoserve.net)
// I need random access
//
// Further optimization could be realized by adding an ability
// to cache the directory in memory. The goal being a single
// comprehensive file read to put the file I need in a memory.
*/


/*
// Unzip Helper Functions - should be here?
///////////////////////////////////////////
*/

/*
  Read the local header of the current zipfile
  Check the coherency of the local header and info in the end of central
        directory about this file
  store in *piSizeVar the size of extra info in local header
        (filename and size of extra field data)
*/
local int 
unz64local_CheckCurrentFileCoherencyHeader (unz64_s* s, uInt* piSizeVar,
					    ZPOS64_T * poffset_local_extrafield,
					    uInt  * psize_local_extrafield)
{
    uLong uMagic,uData,uFlags;
    uLong size_filename;
    uLong size_extra_field;
    int err = UNZ_OK;

    *piSizeVar = 0;
    *poffset_local_extrafield = 0;
    *psize_local_extrafield = 0;

    if (fseek_func(s->filestream,s->cur_file_info_internal.offset_curfile +
		   s->byte_before_the_zipfile,ZLIB_FILEFUNC_SEEK_SET) != 0)
        return UNZ_ERRNO;


    if (err == UNZ_OK)
    {
        if (unz64local_getLong(s->filestream,&uMagic) != UNZ_OK)
            err = UNZ_ERRNO;
        else if (uMagic!=0x04034b50)
            err = UNZ_BADZIPFILE;
    }

    if (unz64local_getShort(s->filestream,&uData) != UNZ_OK)
        err = UNZ_ERRNO;
/*
    else if ((err == UNZ_OK) && (uData!=s->cur_file_info.wVersion))
        err=UNZ_BADZIPFILE;
*/
    if (unz64local_getShort(s->filestream,&uFlags)  !=  UNZ_OK)
        err = UNZ_ERRNO;

    if (unz64local_getShort(s->filestream,&uData) != UNZ_OK)
        err = UNZ_ERRNO;
    else if ((err == UNZ_OK) && (uData != s->cur_file_info.compression_method))
        err = UNZ_BADZIPFILE;

    if ((err == UNZ_OK) && (s->cur_file_info.compression_method != 0) &&
/* #ifdef HAVE_BZIP2 */
                         (s->cur_file_info.compression_method != Z_BZIP2ED) &&
/* #endif */
                         (s->cur_file_info.compression_method != Z_DEFLATED))
        err = UNZ_BADZIPFILE;

    if (unz64local_getLong(s->filestream,&uData) != UNZ_OK) /* date/time */
        err = UNZ_ERRNO;

    if (unz64local_getLong(s->filestream,&uData) != UNZ_OK) /* crc */
        err = UNZ_ERRNO;
    else if ((err == UNZ_OK) && (uData != s->cur_file_info.crc) && 
	     ((uFlags & 8) == 0))
        err = UNZ_BADZIPFILE;

    if (unz64local_getLong(s->filestream,&uData) != UNZ_OK) /* size compr */
        err = UNZ_ERRNO;
    else if (uData != 0xFFFFFFFF && (err == UNZ_OK) && 
	     (uData != s->cur_file_info.compressed_size) && ((uFlags & 8) == 0))
        err = UNZ_BADZIPFILE;

    if (unz64local_getLong(s->filestream,&uData) != UNZ_OK) /* size uncompr */
        err = UNZ_ERRNO;
    else if (uData != 0xFFFFFFFF && (err == UNZ_OK) && 
	     (uData != s->cur_file_info.uncompressed_size) && ((uFlags & 8) == 0))
        err = UNZ_BADZIPFILE;

    if (unz64local_getShort(s->filestream,&size_filename) != UNZ_OK)
        err = UNZ_ERRNO;
    else if ((err == UNZ_OK) && (size_filename != s->cur_file_info.size_filename))
        err = UNZ_BADZIPFILE;

    *piSizeVar += (uInt)size_filename;

    if (unz64local_getShort(s->filestream,&size_extra_field) != UNZ_OK)
        err=UNZ_ERRNO;
    *poffset_local_extrafield= s->cur_file_info_internal.offset_curfile +
                                    SIZEZIPLOCALHEADER + size_filename;
    *psize_local_extrafield = (uInt)size_extra_field;

    *piSizeVar += (uInt)size_extra_field;

    return err;
}

/*
  Open for reading data the current file in the zipfile.
  If there is no error and the file is opened, the return value is UNZ_OK.
*/
static int unzOpenCurrentFile3 (unzFile file, int* method,
				int* level, int raw, const char* password)
{
    int err = UNZ_OK;
    uInt iSizeVar;
    unz64_s* s;
    file_in_zip64_read_info_s* pfile_in_zip_read_info;
    ZPOS64_T offset_local_extrafield;  /* offset of the local extra field */
    uInt  size_local_extrafield;    /* size of the local extra field */
    if (password != NULL) return UNZ_PARAMERROR;

    if (file == NULL)
        return UNZ_PARAMERROR;
    s = (unz64_s*)file;
    if (!s->current_file_ok)
        return UNZ_PARAMERROR;

    if (s->pfile_in_zip_read != NULL)
        unzCloseCurrentFile(file);

    if (unz64local_CheckCurrentFileCoherencyHeader(s, &iSizeVar, 
						   &offset_local_extrafield,&size_local_extrafield) != UNZ_OK)
        return UNZ_BADZIPFILE;

    pfile_in_zip_read_info = 
	(file_in_zip64_read_info_s*)ALLOC(sizeof(file_in_zip64_read_info_s));
    if (pfile_in_zip_read_info == NULL)
        return UNZ_INTERNALERROR;

    pfile_in_zip_read_info->read_buffer = (char*)ALLOC(UNZ_BUFSIZE);
    pfile_in_zip_read_info->offset_local_extrafield = offset_local_extrafield;
    pfile_in_zip_read_info->size_local_extrafield = size_local_extrafield;
    pfile_in_zip_read_info->pos_local_extrafield = 0;
    pfile_in_zip_read_info->raw = raw;

    if (pfile_in_zip_read_info->read_buffer == NULL)
    {
        TRYFREE(pfile_in_zip_read_info);
        return UNZ_INTERNALERROR;
    }

    pfile_in_zip_read_info->stream_initialised = 0;

    if (method != NULL)
        *method = (int)s->cur_file_info.compression_method;

    if (level != NULL)
    {
        *level = 6;
        switch (s->cur_file_info.flag & 0x06)
        {
          case 6 : *level = 1; break;
          case 4 : *level = 2; break;
          case 2 : *level = 9; break;
        }
    }

    if ((s->cur_file_info.compression_method != 0) &&
/* #ifdef HAVE_BZIP2 */
        (s->cur_file_info.compression_method != Z_BZIP2ED) &&
/* #endif */
        (s->cur_file_info.compression_method != Z_DEFLATED))

        err=UNZ_BADZIPFILE;

    pfile_in_zip_read_info->crc32_wait = s->cur_file_info.crc;
    pfile_in_zip_read_info->crc32 = 0;
    pfile_in_zip_read_info->total_out_64 = 0;
    pfile_in_zip_read_info->compression_method =
	s->cur_file_info.compression_method;
    pfile_in_zip_read_info->filestream = s->filestream;
    pfile_in_zip_read_info->byte_before_the_zipfile =
	s->byte_before_the_zipfile;

    pfile_in_zip_read_info->stream.total_out = 0;

    if ((s->cur_file_info.compression_method == Z_BZIP2ED) && (!raw))
    {
#ifdef HAVE_BZIP2
      pfile_in_zip_read_info->bstream.bzalloc = (void *(*) (void *, int, int))0;
      pfile_in_zip_read_info->bstream.bzfree = (free_func)0;
      pfile_in_zip_read_info->bstream.opaque = (voidpf)0;
      pfile_in_zip_read_info->bstream.state = (voidpf)0;

      pfile_in_zip_read_info->stream.zalloc = (alloc_func)0;
      pfile_in_zip_read_info->stream.zfree = (free_func)0;
      pfile_in_zip_read_info->stream.opaque = (voidpf)0;
      pfile_in_zip_read_info->stream.next_in = (voidpf)0;
      pfile_in_zip_read_info->stream.avail_in = 0;

      err = BZ2_bzDecompressInit(&pfile_in_zip_read_info->bstream, 0, 0);
      if (err == Z_OK)
        pfile_in_zip_read_info->stream_initialised = Z_BZIP2ED;
      else
      {
        TRYFREE(pfile_in_zip_read_info);
        return err;
      }
#else
      pfile_in_zip_read_info->raw = 1;
#endif
    }
    else if ((s->cur_file_info.compression_method == Z_DEFLATED) && (!raw))
    {
      pfile_in_zip_read_info->stream.zalloc = (alloc_func)0;
      pfile_in_zip_read_info->stream.zfree = (free_func)0;
      pfile_in_zip_read_info->stream.opaque = (voidpf)0;
      pfile_in_zip_read_info->stream.next_in = 0;
      pfile_in_zip_read_info->stream.avail_in = 0;

      err=inflateInit2(&pfile_in_zip_read_info->stream, -MAX_WBITS);
      if (err == Z_OK)
        pfile_in_zip_read_info->stream_initialised = Z_DEFLATED;
      else
      {
        TRYFREE(pfile_in_zip_read_info);
        return err;
      }
        /* windowBits is passed < 0 to tell that there is no zlib header.
         * Note that in this case inflate *requires* an extra "dummy" byte
         * after the compressed stream in order to complete decompression and
         * return Z_STREAM_END.
         * In unzip, i don't wait absolutely Z_STREAM_END because I known the
         * size of both compressed and uncompressed data
         */
    }
    pfile_in_zip_read_info->rest_read_compressed =
            s->cur_file_info.compressed_size ;
    pfile_in_zip_read_info->rest_read_uncompressed =
            s->cur_file_info.uncompressed_size ;


    pfile_in_zip_read_info->pos_in_zipfile =
            s->cur_file_info_internal.offset_curfile + SIZEZIPLOCALHEADER +
              iSizeVar;

    pfile_in_zip_read_info->stream.avail_in = (uInt)0;

    s->pfile_in_zip_read = pfile_in_zip_read_info;
                s->encrypted = 0;

    return UNZ_OK;
}

static int unzOpenCurrentFile (unzFile file)
{
    return unzOpenCurrentFile3(file, NULL, NULL, 0, NULL);
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
static int unzReadCurrentFile  (unzFile file, voidp buf, unsigned len)
{
    int err = UNZ_OK;
    uInt iRead = 0;
    unz64_s* s;
    file_in_zip64_read_info_s* pfile_in_zip_read_info;
    if (file == NULL)
        return UNZ_PARAMERROR;
    s = (unz64_s*)file;
    pfile_in_zip_read_info = s->pfile_in_zip_read;

    if (pfile_in_zip_read_info == NULL)
        return UNZ_PARAMERROR;


    if (pfile_in_zip_read_info->read_buffer == NULL)
        return UNZ_END_OF_LIST_OF_FILE;
    if (len == 0)
        return 0;

    pfile_in_zip_read_info->stream.next_out = (Bytef*)buf;

    pfile_in_zip_read_info->stream.avail_out = (uInt)len;

    if ((len>pfile_in_zip_read_info->rest_read_uncompressed) &&
        (!(pfile_in_zip_read_info->raw)))
        pfile_in_zip_read_info->stream.avail_out =
            (uInt)pfile_in_zip_read_info->rest_read_uncompressed;

    if ((len>pfile_in_zip_read_info->rest_read_compressed+
           pfile_in_zip_read_info->stream.avail_in) &&
         (pfile_in_zip_read_info->raw))
        pfile_in_zip_read_info->stream.avail_out =
            (uInt)pfile_in_zip_read_info->rest_read_compressed+
            pfile_in_zip_read_info->stream.avail_in;

    while (pfile_in_zip_read_info->stream.avail_out>0)
    {
        if ((pfile_in_zip_read_info->stream.avail_in == 0) &&
            (pfile_in_zip_read_info->rest_read_compressed>0))
        {
            size_t uReadThis = UNZ_BUFSIZE;
            if (pfile_in_zip_read_info->rest_read_compressed<uReadThis)
                uReadThis = (uInt)pfile_in_zip_read_info->rest_read_compressed;
            if (uReadThis == 0)
                return UNZ_EOF;
            if (fseek_func(pfile_in_zip_read_info->filestream,
			   pfile_in_zip_read_info->pos_in_zipfile +
			   pfile_in_zip_read_info->byte_before_the_zipfile,
			   ZLIB_FILEFUNC_SEEK_SET) != 0)
                return UNZ_ERRNO;
            if (fread_func(pfile_in_zip_read_info->filestream,
			   pfile_in_zip_read_info->read_buffer,
			   uReadThis) != uReadThis)
                return UNZ_ERRNO;

            pfile_in_zip_read_info->pos_in_zipfile += uReadThis;

            pfile_in_zip_read_info->rest_read_compressed -= uReadThis;

            pfile_in_zip_read_info->stream.next_in =
                (Bytef*)pfile_in_zip_read_info->read_buffer;
            pfile_in_zip_read_info->stream.avail_in = (uInt)uReadThis;
        }

        if ((pfile_in_zip_read_info->compression_method == 0) || 
	    (pfile_in_zip_read_info->raw))
        {
            uInt uDoCopy,i ;

            if ((pfile_in_zip_read_info->stream.avail_in == 0) &&
                (pfile_in_zip_read_info->rest_read_compressed == 0))
                return (iRead == 0) ? UNZ_EOF : iRead;

            if (pfile_in_zip_read_info->stream.avail_out <
                            pfile_in_zip_read_info->stream.avail_in)
                uDoCopy = pfile_in_zip_read_info->stream.avail_out ;
            else
                uDoCopy = pfile_in_zip_read_info->stream.avail_in ;

            for (i = 0; i < uDoCopy; i++)
                *(pfile_in_zip_read_info->stream.next_out+i) =
                        *(pfile_in_zip_read_info->stream.next_in+i);

            pfile_in_zip_read_info->total_out_64 = 
		pfile_in_zip_read_info->total_out_64 + uDoCopy;

            pfile_in_zip_read_info->crc32 = crc32(pfile_in_zip_read_info->crc32,
                                pfile_in_zip_read_info->stream.next_out,
                                uDoCopy);
            pfile_in_zip_read_info->rest_read_uncompressed-=uDoCopy;
            pfile_in_zip_read_info->stream.avail_in -= uDoCopy;
            pfile_in_zip_read_info->stream.avail_out -= uDoCopy;
            pfile_in_zip_read_info->stream.next_out += uDoCopy;
            pfile_in_zip_read_info->stream.next_in += uDoCopy;
            pfile_in_zip_read_info->stream.total_out += uDoCopy;
            iRead += uDoCopy;
        }
        else if (pfile_in_zip_read_info->compression_method == Z_BZIP2ED)
        {
#ifdef HAVE_BZIP2
            uLong uTotalOutBefore,uTotalOutAfter;
            const Bytef *bufBefore;
            uLong uOutThis;

            pfile_in_zip_read_info->bstream.next_in        = 
		(char*)pfile_in_zip_read_info->stream.next_in;
            pfile_in_zip_read_info->bstream.avail_in       = 
		pfile_in_zip_read_info->stream.avail_in;
            pfile_in_zip_read_info->bstream.total_in_lo32  = 
		(unsigned int) pfile_in_zip_read_info->stream.total_in;
            pfile_in_zip_read_info->bstream.total_in_hi32  = 0;
            pfile_in_zip_read_info->bstream.next_out       = 
		(char*)pfile_in_zip_read_info->stream.next_out;
            pfile_in_zip_read_info->bstream.avail_out      = 
		pfile_in_zip_read_info->stream.avail_out;
            pfile_in_zip_read_info->bstream.total_out_lo32 = 
		(unsigned int) pfile_in_zip_read_info->stream.total_out;
            pfile_in_zip_read_info->bstream.total_out_hi32 = 0;

            uTotalOutBefore = pfile_in_zip_read_info->bstream.total_out_lo32;
            bufBefore = (const Bytef *)pfile_in_zip_read_info->bstream.next_out;

            err = BZ2_bzDecompress(&pfile_in_zip_read_info->bstream);

            uTotalOutAfter = pfile_in_zip_read_info->bstream.total_out_lo32;
            uOutThis = uTotalOutAfter-uTotalOutBefore;

            pfile_in_zip_read_info->total_out_64 = 
		pfile_in_zip_read_info->total_out_64 + uOutThis;

            pfile_in_zip_read_info->crc32 = 
		crc32(pfile_in_zip_read_info->crc32,bufBefore,
		      (uInt)(uOutThis));
            pfile_in_zip_read_info->rest_read_uncompressed -= uOutThis;
            iRead += (uInt)(uTotalOutAfter - uTotalOutBefore);

            pfile_in_zip_read_info->stream.next_in   = 
		(Bytef*)pfile_in_zip_read_info->bstream.next_in;
            pfile_in_zip_read_info->stream.avail_in  = 
		pfile_in_zip_read_info->bstream.avail_in;
            pfile_in_zip_read_info->stream.total_in  = 
		pfile_in_zip_read_info->bstream.total_in_lo32;
            pfile_in_zip_read_info->stream.next_out  = 
		(Bytef*)pfile_in_zip_read_info->bstream.next_out;
            pfile_in_zip_read_info->stream.avail_out =
		pfile_in_zip_read_info->bstream.avail_out;
            pfile_in_zip_read_info->stream.total_out = 
		pfile_in_zip_read_info->bstream.total_out_lo32;

            if (err == BZ_STREAM_END)
              return (iRead == 0) ? UNZ_EOF : iRead;
            if (err != BZ_OK)
              break;
#endif
        } // end Z_BZIP2ED
        else
        {
            ZPOS64_T uTotalOutBefore,uTotalOutAfter;
            const Bytef *bufBefore;
            ZPOS64_T uOutThis;
            int flush = Z_SYNC_FLUSH;

            uTotalOutBefore = pfile_in_zip_read_info->stream.total_out;
            bufBefore = pfile_in_zip_read_info->stream.next_out;

            /*
            if ((pfile_in_zip_read_info->rest_read_uncompressed ==
                     pfile_in_zip_read_info->stream.avail_out) &&
                (pfile_in_zip_read_info->rest_read_compressed == 0))
                flush = Z_FINISH;
            */
            err = inflate(&pfile_in_zip_read_info->stream, flush);

            if ((err >= 0) && (pfile_in_zip_read_info->stream.msg != NULL))
              err = Z_DATA_ERROR;

            uTotalOutAfter = pfile_in_zip_read_info->stream.total_out;
            uOutThis = uTotalOutAfter-uTotalOutBefore;

            pfile_in_zip_read_info->total_out_64 = 
		pfile_in_zip_read_info->total_out_64 + uOutThis;

            pfile_in_zip_read_info->crc32 =
                crc32(pfile_in_zip_read_info->crc32,bufBefore,
                        (uInt)(uOutThis));

            pfile_in_zip_read_info->rest_read_uncompressed -=
                uOutThis;

            iRead += (uInt)(uTotalOutAfter - uTotalOutBefore);

            if (err == Z_STREAM_END)
                return (iRead == 0) ? UNZ_EOF : iRead;
            if (err != Z_OK)
                break;
        }
    }

    if (err == Z_OK)
        return iRead;
    return err;
}

/*
  Close the file in zip opened with unzipOpenCurrentFile
  Return UNZ_CRCERROR if all the file was read but the CRC is not good
*/
static int unzCloseCurrentFile (unzFile file)
{
    int err = UNZ_OK;

    unz64_s* s;
    file_in_zip64_read_info_s* pfile_in_zip_read_info;
    if (file == NULL)
        return UNZ_PARAMERROR;
    s = (unz64_s*)file;
    pfile_in_zip_read_info = s->pfile_in_zip_read;

    if (pfile_in_zip_read_info == NULL)
        return UNZ_PARAMERROR;


    if ((pfile_in_zip_read_info->rest_read_uncompressed == 0) &&
        (!pfile_in_zip_read_info->raw))
    {
        if (pfile_in_zip_read_info->crc32 != pfile_in_zip_read_info->crc32_wait)
            err = UNZ_CRCERROR;
    }


    TRYFREE(pfile_in_zip_read_info->read_buffer);
    pfile_in_zip_read_info->read_buffer = NULL;
    if (pfile_in_zip_read_info->stream_initialised == Z_DEFLATED)
        inflateEnd(&pfile_in_zip_read_info->stream);
#ifdef HAVE_BZIP2
    else if (pfile_in_zip_read_info->stream_initialised == Z_BZIP2ED)
        BZ2_bzDecompressEnd(&pfile_in_zip_read_info->bstream);
#endif


    pfile_in_zip_read_info->stream_initialised = 0;
    TRYFREE(pfile_in_zip_read_info);

    s->pfile_in_zip_read = NULL;

    return err;
}


#ifdef Win32
# define f_seek fseeko64
# define f_tell ftello64
#elif defined(HAVE_OFF_T) && defined(HAVE_FSEEKO)
# define f_seek fseeko
# define f_tell ftello
#else
# define f_seek fseek
# define f_tell ftell
#endif

static voidpf fopen_func(const void* filename, int mode)
{
    FILE* file = NULL;
    const char* mode_fopen = NULL;
    if ((mode & ZLIB_FILEFUNC_MODE_READWRITEFILTER) == ZLIB_FILEFUNC_MODE_READ)
        mode_fopen = "rb";
    else if (mode & ZLIB_FILEFUNC_MODE_EXISTING)
        mode_fopen = "r+b";
    else if (mode & ZLIB_FILEFUNC_MODE_CREATE)
        mode_fopen = "wb";

    if ((filename != NULL) && (mode_fopen != NULL))
        file = fopen(filename, mode_fopen);
    return file;
}


static ZPOS64_T ftell_func (voidpf stream)
{
    return f_tell((FILE *)stream);
}

static int fseek_func (voidpf stream, ZPOS64_T offset, int origin)
{
    int fseek_origin = 0;
    int ret;
    switch (origin)
    {
    case ZLIB_FILEFUNC_SEEK_CUR :
        fseek_origin = SEEK_CUR;
        break;
    case ZLIB_FILEFUNC_SEEK_END :
        fseek_origin = SEEK_END;
        break;
    case ZLIB_FILEFUNC_SEEK_SET :
        fseek_origin = SEEK_SET;
        break;
    default: return -1;
    }
    ret = 0;
    if (f_seek((FILE *)stream, offset, fseek_origin) != 0) ret = -1;
    return ret;
}

static size_t fread_func (voidpf stream, void* buf, size_t size)
{
    uLong ret;
    ret = (uLong)fread(buf, 1, size, (FILE *) stream);
    return ret;
}

static int fclose_func (voidpf stream)
{
    int ret;
    ret = fclose((FILE *)stream);
    return ret;
}

static int ferror_func (voidpf stream)
{
    int ret;
    ret = ferror((FILE *)stream);
    return ret;
}
