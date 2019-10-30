/* modified from dos_glob.c to work with wchar_t */

/*
 * Copyright (c) 1989, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Guido van Rossum.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)glob.c	8.3 (Berkeley) 10/13/93";
/* most changes between the version above and the one below have been ported:
static char sscsid[]=  "$OpenBSD: glob.c,v 1.8.10.1 2001/04/10 jason Exp $";
 */
#endif /* LIBC_SCCS and not lint */

/*
 * glob(3) -- a superset of the one defined in POSIX 1003.2.
 *
 * The [!...] convention to negate a range is supported (SysV, Posix, ksh).
 *
 * Optional extra services, controlled by flags not defined by POSIX:
 *
 * GLOB_QUOTE:
 *	Escaping convention: \ inhibits any special meaning the following
 *	character might have (except \ at end of string is retained).
 * GLOB_MAGCHAR:
 *	Set in gl_flags if pattern contained a globbing character.
 * GLOB_NOMAGIC:
 *	Same as GLOB_NOCHECK, but it will only append pattern if it did
 *	not contain any magic characters.  [Used in csh style globbing]
 * GLOB_ALTDIRFUNC:
 *	Use alternately specified directory access functions.
 * GLOB_TILDE:
 *	expand ~user/foo to the /home/dir/of/user/foo
 * GLOB_BRACE:
 *	expand {1,2}{a,b} to 1a 1b 2a 2b
 * gl_matchc:
 *	Number of matches in the current invocation of glob.
 * GLOB_ALPHASORT:
 *	sort alphabetically like csh (case doesn't matter) instead of in ASCII
 *	order
 */

#include <wchar.h>
#include "dos_wglob.h"

//#define GLOB_DEBUG

#ifdef GLOB_DEBUG
void Rprintf(const char *, ...);
#endif

#define	MAXPATHLEN	255
#define DOSISH
#define ARG_MAX		14500


#define	BG_DOLLAR	L'$'
#define	BG_DOT		L'.'
#define	BG_EOS		L'\0'
#define	BG_LBRACKET	L'['
#define	BG_NOT		L'!'
#define	BG_QUESTION	L'?'
#define	BG_QUOTE	L'\\'
#define	BG_RANGE	L'-'
#define	BG_RBRACKET	L']'
#define	BG_SEP		L'/'
#ifdef DOSISH /* true, cannot be set to false anymore */
#define BG_SEP2		L'\\'
#endif
#define	BG_STAR		L'*'
#define	BG_TILDE	L'~'
#define	BG_UNDERSCORE	L'_'
#define	BG_LBRACE	L'{'
#define	BG_RBRACE	L'}'
#define	BG_SLASH	L'/'
#define	BG_COMMA	L','


#include <stdlib.h>
#include <dirent.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <string.h>
#include <wctype.h>


typedef size_t STRLEN;
typedef struct _stat Stat_t;
typedef struct _wdirent Direntry_t;


static int	 compare(const void *, const void *);
static int	 ci_compare(const void *, const void *);
static int	 g_Ctoc(const wchar_t *, wchar_t *, STRLEN);
static int	 g_lstat(wchar_t *, Stat_t *, wglob_t *);
static _WDIR	*g_opendir(wchar_t *, wglob_t *);
static const wchar_t *
		 g_strchr(const wchar_t *, int);
static int	 glob0(const wchar_t *, wglob_t *);
static int	 glob1(wchar_t *, wchar_t *, wglob_t *, size_t *);
static int	 glob2(wchar_t *, wchar_t *, wchar_t *, wchar_t *, wchar_t *, wchar_t *,
		       wglob_t *, size_t *);
static int	 glob3(wchar_t *, wchar_t *, wchar_t *, wchar_t *, wchar_t *, wchar_t *,
		       wchar_t *, wchar_t *, wglob_t *, size_t *);
static int	 globextend(const wchar_t *, wglob_t *, size_t *);
static const wchar_t *
		 globtilde(const wchar_t *, wchar_t *, size_t, wglob_t *);
static int	 globexp1(const wchar_t *, wglob_t *);
static int	 globexp2(const wchar_t *, const wchar_t *, wglob_t *, int *);
static int	 match(wchar_t *, wchar_t *, wchar_t *, int);
#ifdef GLOB_DEBUG
static void	 qprintf(const char *, wchar_t *);
#endif /* GLOB_DEBUG */

/* 
   Protected and meta characters are from Unicode Private Use Area.
   The DOS version set the upper bit of bytes for meta characters.
   An earlier wchar_t version set the top bits of wchar_t.
*/

#define	M_MASK		0xffff /* pointless in current setup */

#define P_LBRACKET  (wchar_t)	0xfdd0
#define P_RBRACKET  (wchar_t)	0xfdd1
#define P_RANGE	    (wchar_t)	0xfdd2
#define P_LBRACE    (wchar_t)	0xfdd3
#define P_RBRACE    (wchar_t)	0xfdd4
#define P_TILDE	    (wchar_t)	0xfdd5
#define P_QUOTE	    (wchar_t)	0xfdd6
#define	M_ALL	    (wchar_t)	0xfdd7
#define	M_END	    (wchar_t)	0xfdd8
#define	M_NOT	    (wchar_t)	0xfdd9
#define	M_ONE	    (wchar_t)	0xfdda
#define	M_RNG	    (wchar_t)	0xfddb
#define	M_SET	    (wchar_t)	0xfddc

static wchar_t WC_PROTECT(wchar_t c)
{
    /* []-{}~\ */

    switch(c) {
	case BG_LBRACKET: return P_LBRACKET;
	case BG_RBRACKET: return P_RBRACKET;
	case BG_RANGE: return P_RANGE;
	case BG_LBRACE: return P_LBRACE;
	case BG_RBRACE: return P_RBRACE;
	case BG_TILDE: return P_TILDE;
	case BG_QUOTE: return P_QUOTE;
	default:
	    /* not reachable */
	    return c;
    }
}

static int ismeta(wchar_t c)
{
    switch(c) {
	case M_ALL:
	case M_END:
	case M_NOT:
	case M_ONE:
	case M_RNG:
	case M_SET:
	    return 1;
	default:
	    return 0;
    }
}

static wchar_t CHAR(wchar_t c)
{
    switch(c) {
	case P_LBRACKET: return BG_LBRACKET;
	case P_RBRACKET: return BG_RBRACKET;
	case P_RANGE: return BG_RANGE;
	case P_LBRACE: return BG_LBRACE;
	case P_RBRACE: return BG_RBRACE;
	case P_TILDE: return BG_TILDE;
	case P_QUOTE: return BG_QUOTE;
	case M_ALL: return BG_STAR;
	case M_END: return BG_RBRACKET;
	case M_NOT: return BG_NOT;
	case M_ONE: return BG_QUESTION;
	case M_RNG: return BG_RANGE;
	case M_SET: return BG_LBRACKET;
	default:
	    return c;
    }
}

int
dos_wglob(const wchar_t *pattern, int flags,
	  int (*errfunc)(const wchar_t *, int), wglob_t *pglob)
{
    const wchar_t *patnext;
    int c;
    wchar_t *bufnext, *bufend, patbuf[MAXPATHLEN];

    patnext = pattern;
#if 1
    if (!(flags & GLOB_APPEND)) {
	pglob->gl_pathc = 0;
	pglob->gl_pathv = NULL;
	if (!(flags & GLOB_DOOFFS))
	    pglob->gl_offs = 0;
    }
#else
    pglob->gl_pathc = 0;
    pglob->gl_pathv = NULL;
    pglob->gl_offs = 0;
#endif
    pglob->gl_flags = flags & ~GLOB_MAGCHAR;
    pglob->gl_errfunc = errfunc;
    pglob->gl_matchc = 0;

    bufnext = patbuf;
    bufend = bufnext + MAXPATHLEN - 1;
#ifdef DOSISH /* true */
    /* Nasty hack to treat patterns like "C:*" correctly. In this
     * case, the * should match any file in the current directory
     * on the C: drive. However, the glob code does not treat the
     * colon specially, so it looks for files beginning "C:" in
     * the current directory. To fix this, change the pattern to
     * add an explicit "./" at the start (just after the drive
     * letter and colon - ie change to "C:./").
     */
    if (iswalpha(pattern[0]) && pattern[1] == L':' &&
	pattern[2] != BG_SEP && pattern[2] != BG_SEP2 &&
	bufend - bufnext > 4) {
	*bufnext++ = pattern[0];
	*bufnext++ = L':';
	*bufnext++ = L'.';
	*bufnext++ = BG_SEP;
	patnext += 2;
    }

    /* Hack from Tony Plate to allow UNC network drive specification:
     * Without this code, '\\' (i.e., literally two backslashes inpattern)
     * at the beginning of a path is not recognized as a network drive,
     * because the GLOB_QUOTE loop below changes the two backslashes to one.
     * So, in the case where there are two but not three backslashes at
     * the beginning of the path, transfer these to the output.
     */
    if (patnext == pattern && bufend - bufnext > 2 &&
	pattern[0] == BG_SEP2 && pattern[1] == BG_SEP2 &&
	pattern[2] != BG_SEP2) {
	*bufnext++ = pattern[0];
	*bufnext++ = pattern[1];
	patnext += 2;
    }
#endif

    if (flags & GLOB_QUOTE) {
	/* Protect the quoted characters. */
	while (bufnext < bufend && (c = *patnext++) != BG_EOS)
	    if (c == BG_QUOTE) {
#ifdef DOSISH /* true */
		/* To avoid backslashitis on Win32,
		 * we only treat \ as a quoting character
		 * if it precedes one of the
		 * metacharacters []-{}~\
		 */
		if ((c = *patnext++) != L'[' && c != L']' &&
		    c != L'-' && c != L'{' && c != L'}' &&
		    c != L'~' && c != L'\\') {
		    /* WC_PROTECT has to support all characters above */
#else
# error DOSISH must be true
		if ((c = *patnext++) == BG_EOS) {
#endif
		    c = BG_QUOTE;
		    --patnext;
		}
		*bufnext++ = WC_PROTECT(c);
	    } else
		*bufnext++ = c;
	} else
	    while (bufnext < bufend && (c = *patnext++) != BG_EOS)
		*bufnext++ = c;
    *bufnext = BG_EOS;

    if (flags & GLOB_BRACE)
	return globexp1(patbuf, pglob);
    else
	return glob0(patbuf, pglob);
}

/*
 * Expand recursively a glob {} pattern. When there is no more expansion
 * invoke the standard globbing routine to glob the rest of the magic
 * characters
 */
static int
globexp1(const wchar_t *pattern, wglob_t *pglob)
{
    const wchar_t* ptr = pattern;
    int rv;

    /* Protect a single {}, for find(1), like csh */
    if (pattern[0] == BG_LBRACE && pattern[1] == BG_RBRACE && pattern[2] == BG_EOS)
	return glob0(pattern, pglob);

    while ((ptr = (const wchar_t *) g_strchr(ptr, BG_LBRACE)) != NULL)
	if (!globexp2(ptr, pattern, pglob, &rv))
	    return rv;

    return glob0(pattern, pglob);
}


/*
 * Recursive brace globbing helper. Tries to expand a single brace.
 * If it succeeds then it invokes globexp1 with the new pattern.
 * If it fails then it tries to glob the rest of the pattern and returns.
 */
static int
globexp2(const wchar_t *ptr, const wchar_t *pattern,
	 wglob_t *pglob, int *rv)
{
    int     i;
    wchar_t   *lm, *ls;
    const wchar_t *pe, *pm, *pl;
    wchar_t    patbuf[MAXPATHLEN];

    /* copy part up to the brace */
    for (lm = patbuf, pm = pattern; pm != ptr; *lm++ = *pm++)
	;
    *lm = BG_EOS;
    ls = lm;

    /* Find the balanced brace */
    for (i = 0, pe = ++ptr; *pe; pe++)
	if (*pe == BG_LBRACKET) {
	    /* Ignore everything between [] */
	    for (pm = pe++; *pe != BG_RBRACKET && *pe != BG_EOS; pe++)
		;
	    if (*pe == BG_EOS) {
		/*
		 * We could not find a matching BG_RBRACKET.
		 * Ignore and just look for BG_RBRACE
		 */
		pe = pm;
	    }
	} else if (*pe == BG_LBRACE)
	    i++;
	else if (*pe == BG_RBRACE) {
	    if (i == 0)
		break;
	    i--;
	}

    /* Non matching braces; just glob the pattern */
    if (i != 0 || *pe == BG_EOS) {
	*rv = glob0(patbuf, pglob);
	return 0;
    }

    for (i = 0, pl = pm = ptr; pm <= pe; pm++) {
	switch (*pm) {
	case BG_LBRACKET:
	    /* Ignore everything between [] */
	    for (pl = pm++; *pm != BG_RBRACKET && *pm != BG_EOS; pm++)
		;
	    if (*pm == BG_EOS) {
		/*
		 * We could not find a matching BG_RBRACKET.
		 * Ignore and just look for BG_RBRACE
		 */
		pm = pl;
	    }
	    break;

	case BG_LBRACE:
	    i++;
	    break;

	case BG_RBRACE:
	    if (i) {
		i--;
		break;
	    }
	    /* FALLTHROUGH */
	case BG_COMMA:
	    if (i && *pm == BG_COMMA)
		break;
	    else {
		/* Append the current string */
		for (lm = ls; (pl < pm); *lm++ = *pl++)
		    ;

		/*
		 * Append the rest of the pattern after the
		 * closing brace
		 */
		for (pl = pe + 1; (*lm++ = *pl++) != BG_EOS; )
		    ;

		/* Expand the current pattern */
#ifdef GLOB_DEBUG
		qprintf("globexp2:", patbuf);
#endif /* GLOB_DEBUG */
		*rv = globexp1(patbuf, pglob);

		/* move after the comma, to the next string */
		pl = pm + 1;
	    }
	    break;

	default:
	    break;
	}
    }
    *rv = 0;
    return 0;
}



/*
 * expand tilde from the passwd file: not supported.
 */
static const wchar_t *
globtilde(const wchar_t *pattern, wchar_t *patbuf, size_t patbuf_len, wglob_t *pglob)
{
    wchar_t *h;
    const wchar_t *p;
    wchar_t *b, *eb;

    if (*pattern != BG_TILDE || !(pglob->gl_flags & GLOB_TILDE))
	return pattern;

    /* Copy up to the end of the string or / */
    eb = &patbuf[patbuf_len - 1];
    for (p = pattern + 1, h = (wchar_t *) patbuf;
	 h < eb && *p && *p != BG_SLASH; *h++ = *p++)
	;

    *h = BG_EOS;

    if (((wchar_t *) patbuf)[0] == BG_EOS) {
	/*
	 * handle a plain ~ or ~/ by expanding $HOME
	 * first and then trying the password file
	 */
	if ((h = _wgetenv(L"R_USER")) == NULL) {
	    return pattern;
	}
    } else {
	/*
	 * Expand a ~user
	 */
	return pattern;
    }

    /* Copy the home directory */
    for (b = patbuf; b < eb && *h; *b++ = *h++)
	;

    /* Append the rest of the pattern */
    while (b < eb && (*b++ = *p++) != BG_EOS)
	;
    *b = BG_EOS;

    return patbuf;
}


/*
 * The main glob() routine: compiles the pattern (optionally processing
 * quotes), calls glob1() to do the real pattern matching, and finally
 * sorts the list (unless unsorted operation is requested).  Returns 0
 * if things went well, nonzero if errors occurred.  It is not an error
 * to find no matches.
 */
static int
glob0(const wchar_t *pattern, wglob_t *pglob)
{
    const wchar_t *qpat, *qpatnext;
    int c, err, oldflags, oldpathc;
    wchar_t *bufnext, patbuf[MAXPATHLEN];
    size_t limit = 0;

    qpat = globtilde(pattern, patbuf, MAXPATHLEN, pglob);
    qpatnext = qpat;
    oldflags = pglob->gl_flags;
    oldpathc = pglob->gl_pathc;
    bufnext = patbuf;

    /* We don't need to check for buffer overflow any more. */
    while ((c = *qpatnext++) != BG_EOS) {
	switch (c) {
	case BG_LBRACKET:
	    c = *qpatnext;
	    if (c == BG_NOT)
		++qpatnext;
	    if (*qpatnext == BG_EOS ||
		g_strchr((wchar_t *) qpatnext+1, BG_RBRACKET) == NULL) {
		*bufnext++ = BG_LBRACKET;
		if (c == BG_NOT)
		    --qpatnext;
		break;
	    }
	    *bufnext++ = M_SET;
	    if (c == BG_NOT)
		*bufnext++ = M_NOT;
	    c = *qpatnext++;
	    do {
		*bufnext++ = CHAR(c);
		if (*qpatnext == BG_RANGE &&
		    (c = qpatnext[1]) != BG_RBRACKET) {
		    *bufnext++ = M_RNG;
		    *bufnext++ = CHAR(c);
		    qpatnext += 2;
		}
	    } while ((c = *qpatnext++) != BG_RBRACKET);
	    pglob->gl_flags |= GLOB_MAGCHAR;
	    *bufnext++ = M_END;
	    break;
	case BG_QUESTION:
	    pglob->gl_flags |= GLOB_MAGCHAR;
	    *bufnext++ = M_ONE;
	    break;
	case BG_STAR:
	    pglob->gl_flags |= GLOB_MAGCHAR;
	    /* collapse adjacent stars to one,
	     * to avoid exponential behavior
	     */
	    if (bufnext == patbuf || bufnext[-1] != M_ALL)
		*bufnext++ = M_ALL;
	    break;
	default:
	    *bufnext++ = CHAR(c);
	    break;
	}
    }
    *bufnext = BG_EOS;
#ifdef GLOB_DEBUG
    qprintf("glob0:", patbuf);
#endif /* GLOB_DEBUG */

    if ((err = glob1(patbuf, patbuf+MAXPATHLEN-1, pglob, &limit)) != 0) {
	pglob->gl_flags = oldflags;
	return(err);
    }

    /*
     * If there was no match we are going to append the pattern
     * if GLOB_NOCHECK was specified or if GLOB_NOMAGIC was specified
     * and the pattern did not contain any magic characters
     * GLOB_NOMAGIC is there just for compatibility with csh.
     */
    if (pglob->gl_pathc == oldpathc &&
	((pglob->gl_flags & GLOB_NOCHECK) ||
	 ((pglob->gl_flags & GLOB_NOMAGIC) &&
	  !(pglob->gl_flags & GLOB_MAGCHAR))))
    {
#ifdef GLOB_DEBUG
	Rprintf("calling globextend from glob0\n");
#endif /* GLOB_DEBUG */
	pglob->gl_flags = oldflags;
	return(globextend(qpat, pglob, &limit));
    }
    else if (!(pglob->gl_flags & GLOB_NOSORT))
	qsort(pglob->gl_pathv + pglob->gl_offs + oldpathc,
	      pglob->gl_pathc - oldpathc, sizeof(wchar_t *),
	      (pglob->gl_flags & (GLOB_ALPHASORT|GLOB_NOCASE))
	      ? ci_compare : compare);
    pglob->gl_flags = oldflags;
    return(0);
}

static int
ci_compare(const void *p, const void *q)
{
    const wchar_t *pp = *(const wchar_t **)p;
    const wchar_t *qq = *(const wchar_t **)q;
    int ci;
    while (*pp && *qq) {
	if (towlower(*pp) != towlower(*qq))
	    break;
	++pp;
	++qq;
    }
    ci = towlower(*pp) - towlower(*qq);
    if (ci == 0)
	return compare(p, q);
    return ci;
}

static int
compare(const void *p, const void *q)
{
    return(wcscmp(*(wchar_t **)p, *(wchar_t **)q));
}

static int
glob1(wchar_t *pattern, wchar_t *pattern_last, wglob_t *pglob, size_t *limitp)
{
    wchar_t pathbuf[MAXPATHLEN];

    /* A null pathname is invalid -- POSIX 1003.1 sect. 2.4. */
    if (*pattern == BG_EOS) return(0);
    return(glob2(pathbuf, pathbuf+MAXPATHLEN-1,
		 pathbuf, pathbuf+MAXPATHLEN-1,
		 pattern, pattern_last, pglob, limitp));
}

/*
 * The functions glob2 and glob3 are mutually recursive; there is one level
 * of recursion for each segment in the pattern that contains one or more
 * meta characters.
 */
static int
glob2(wchar_t *pathbuf, wchar_t *pathbuf_last, wchar_t *pathend, wchar_t *pathend_last,
      wchar_t *pattern, wchar_t *pattern_last, wglob_t *pglob, size_t *limitp)
{
    Stat_t sb;
    wchar_t *p, *q;
    int anymeta;

    /*
     * Loop over pattern segments until end of pattern or until
     * segment with meta character found.
     */
    for (anymeta = 0;;) {
	if (*pattern == BG_EOS) {		/* End of pattern? */
	    *pathend = BG_EOS;
	    if (g_lstat(pathbuf, &sb, pglob)) return(0);

	    if (((pglob->gl_flags & GLOB_MARK) &&
		 pathend[-1] != BG_SEP
#ifdef DOSISH /* true */
		 && pathend[-1] != BG_SEP2
#endif
		    ) && S_ISDIR(sb.st_mode) ) {
		if (pathend+1 > pathend_last)
		    return (1);
		*pathend++ = BG_SEP;
		*pathend = BG_EOS;
	    }
	    ++pglob->gl_matchc;
#ifdef GLOB_DEBUG
	    Rprintf("calling globextend from glob2\n");
#endif /* GLOB_DEBUG */
	    return(globextend(pathbuf, pglob, limitp));
	}

	/* Find end of next segment, copy tentatively to pathend. */
	q = pathend;
	p = pattern;
	while (*p != BG_EOS && *p != BG_SEP
#ifdef DOSISH /* true */
	       && *p != BG_SEP2
#endif
	    ) {
	    if (ismeta(*p)) anymeta = 1;
	    if (q+1 > pathend_last) return (1);
	    *q++ = *p++;
	}

	if (!anymeta) {		/* No expansion, do next segment. */
	    pathend = q;
	    pattern = p;
	    while (*pattern == BG_SEP
#ifdef DOSISH /* true */
		   || *pattern == BG_SEP2
#endif
		) {
		if (pathend+1 > pathend_last) return (1);
		*pathend++ = *pattern++;
	    }
	} else
	    /* Need expansion, recurse. */
	    return(glob3(pathbuf, pathbuf_last, pathend,
			 pathend_last, pattern, pattern_last,
			 p, pattern_last, pglob, limitp));
    }
    /* NOTREACHED */
}

static int
glob3(wchar_t *pathbuf, wchar_t *pathbuf_last, wchar_t *pathend, wchar_t *pathend_last,
      wchar_t *pattern, wchar_t *pattern_last,
      wchar_t *restpattern, wchar_t *restpattern_last, wglob_t *pglob, size_t *limitp)
{
    Direntry_t *dp;
    _WDIR *dirp;
    int err;
    int nocase;
    wchar_t buf[MAXPATHLEN];

    if (pathend > pathend_last)
	return (1);
    *pathend = BG_EOS;
    errno = 0;

    if ((dirp = g_opendir(pathbuf, pglob)) == NULL) {
	/* TODO: don't call for ENOENT or ENOTDIR? */
	if (pglob->gl_errfunc) {
	    if (g_Ctoc(pathbuf, buf, sizeof(buf)))
		return (GLOB_ABEND);
	    if (pglob->gl_errfunc(buf, errno) ||
		(pglob->gl_flags & GLOB_ERR))
		return (GLOB_ABEND);
	}
	return(0);
    }

    err = 0;
    nocase = ((pglob->gl_flags & GLOB_NOCASE) != 0);

    /* Search directory for matching names. */
    while ((dp = _wreaddir(dirp))) {
	wchar_t *sc, *dc;

	/* Initial BG_DOT must be matched literally. */
	if (dp->d_name[0] == BG_DOT && *pattern != BG_DOT)
	    continue;
	dc = pathend;
	sc = dp->d_name;
	while (dc < pathend_last && (*dc++ = *sc++) != BG_EOS)
	    ;
	if (dc >= pathend_last) {
	    *dc = BG_EOS;
	    err = 1;
	    break;
	}

	if (!match(pathend, pattern, restpattern, nocase)) {
	    *pathend = BG_EOS;
	    continue;
	}
	err = glob2(pathbuf, pathbuf_last, --dc, pathend_last,
		    restpattern, restpattern_last, pglob, limitp);
	if (err)
	    break;
    }

    _wclosedir(dirp);
    return(err);
}


#include <R_ext/RS.h> /* for Calloc, Realloc, Free */

/*
 * Extend the gl_pathv member of a glob_t structure to accomodate a new item,
 * add the new item, and update gl_pathc.
 *
 * This assumes the BSD realloc, which only copies the block when its size
 * crosses a power-of-two boundary; for v7 realloc, this would cause quadratic
 * behavior.
 *
 * Return 0 if new item added, error code if memory couldn't be allocated.
 *
 * Invariant of the glob_t structure:
 *	Either gl_pathc is zero and gl_pathv is NULL; or gl_pathc > 0 and
 *	gl_pathv points to (gl_offs + gl_pathc + 1) items.
 */
static int
globextend(const wchar_t *path, wglob_t *pglob, size_t *limitp)
{
    wchar_t **pathv;
    int i;
    STRLEN newsize, len;
    wchar_t *copy;
    const wchar_t *p;

#ifdef GLOB_DEBUG
    Rprintf("Adding ");
    for (p = path; *p; p++)
	(void)Rprintf("%c", CHAR(*p));
    Rprintf("\n");
#endif /* GLOB_DEBUG */

    newsize = sizeof(*pathv) * (2 + pglob->gl_pathc + pglob->gl_offs);
    if (pglob->gl_pathv)
	pathv = Realloc(pglob->gl_pathv, newsize, wchar_t *);
    else
	pathv = Calloc(newsize, wchar_t *);
    if (pathv == NULL) {
	if (pglob->gl_pathv) {
	    Free(pglob->gl_pathv);
	    pglob->gl_pathv = NULL;
	}
	return(GLOB_NOSPACE);
    }

    if (pglob->gl_pathv == NULL && pglob->gl_offs > 0) {
	/* first time around -- clear initial gl_offs items */
	pathv += pglob->gl_offs;
	for (i = pglob->gl_offs; --i >= 0; )
	    *--pathv = NULL;
    }
    pglob->gl_pathv = pathv;

    for (p = path; *p++;)
	;
    len = (STRLEN)(p - path);
    *limitp += len;
    copy = Calloc(p-path, wchar_t);
    if (copy != NULL) {
	if (g_Ctoc(path, copy, len)) {
	    Free(copy);
	    return(GLOB_NOSPACE);
	}
	pathv[pglob->gl_offs + pglob->gl_pathc++] = copy;
    }
    pathv[pglob->gl_offs + pglob->gl_pathc] = NULL;

    if ((pglob->gl_flags & GLOB_LIMIT) &&
	newsize + *limitp >= ARG_MAX) {
	errno = 0;
	return(GLOB_NOSPACE);
    }

    return(copy == NULL ? GLOB_NOSPACE : 0);
}


/*
 * pattern matching function for filenames.  Each occurrence of the *
 * pattern causes a recursion level.
 */
static int
match(wchar_t *name, wchar_t *pat, wchar_t *patend, int nocase)
{
    int ok, negate_range;
    wchar_t c, k;

    while (pat < patend) {
	c = *pat++;
	switch (c & M_MASK) {
	case M_ALL:
	    if (pat == patend)
		return(1);
	    do
		if (match(name, pat, patend, nocase))
		    return(1);
	    while (*name++ != BG_EOS)
		;
	    return(0);
	case M_ONE:
	    if (*name++ == BG_EOS)
		return(0);
	    break;
	case M_SET:
	    ok = 0;
	    if ((k = *name++) == BG_EOS)
		return(0);
	    if ((negate_range = ((*pat & M_MASK) == M_NOT)) != BG_EOS)
		++pat;
	    while (((c = *pat++) & M_MASK) != M_END)
		if ((*pat & M_MASK) == M_RNG) {
		    if (nocase) {
			if (towlower(c) <= towlower(k) && towlower(k) <= towlower(pat[1]))
			    ok = 1;
		    } else {
			if (c <= k && k <= pat[1])
			    ok = 1;
		    }
		    pat += 2;
		} else if (nocase ? (towlower(c) == towlower(k)) : (c == k))
		    ok = 1;
	    if (ok == negate_range)
		return(0);
	    break;
	default:
	    k = *name++;
	    if (nocase ? (towlower(k) != towlower(c)) : (k != c))
		return(0);
	    break;
	}
    }
    return(*name == BG_EOS);
}

/* Free allocated data belonging to a wglob_t structure. */
void
dos_wglobfree(wglob_t *pglob)
{
    int i;
    wchar_t **pp;

    if (pglob->gl_pathv != NULL) {
	pp = pglob->gl_pathv + pglob->gl_offs;
	for (i = pglob->gl_pathc; i--; ++pp)
	    if (*pp)
		Free(*pp);
	Free(pglob->gl_pathv);
	pglob->gl_pathv = NULL;
    }
}

static _WDIR *
g_opendir(wchar_t *str, wglob_t *pglob)
{
    wchar_t buf[MAXPATHLEN];

    if (!*str) wcscpy(buf, L".");
    else
	if (g_Ctoc(str, buf, sizeof(buf))) return(NULL);
    return _wopendir(buf);
}

static int
g_lstat(wchar_t *fn, Stat_t *sb, wglob_t *pglob)
{
    wchar_t buf[MAXPATHLEN];

    if (g_Ctoc(fn, buf, sizeof(buf)))
	return(-1);
    return(_wstat(buf, sb));
}

static const wchar_t *
g_strchr(const wchar_t *str, int ch)
{
    do {
	if (*str == ch)
	    return (str);
    } while (*str++);
    return (NULL);
}

static int
g_Ctoc(const wchar_t *str, wchar_t *buf, STRLEN len)
{
    while (len--)
	if ((*buf++ = *str++) == BG_EOS) return 0;
    return 1;
}

#ifdef GLOB_DEBUG
static void
qprintf(const char *str, wchar_t *s)
{
    wchar_t *p;

    (void)Rprintf("%s:\n", str);
    for (p = s; *p; p++)
	(void)Rprintf("%lc", CHAR(*p));
    (void)Rprintf("\n");
#if 0
    for (p = s; *p; p++)
	(void)Rprintf("%lc", *p & M_PROTECT ? L'"' : L' ');
    (void)Rprintf("\n");
#endif
    for (p = s; *p; p++)
	(void)Rprintf("%lc", ismeta(*p) ? L'_' : L' ');
    (void)Rprintf("\n");
}
#endif /* GLOB_DEBUG */
