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

#include "Defn.h"

/* functions to perform analogues of the standard C string library */
/* most will be vectorized */

SEXP do_nchar(SEXP call, SEXP op, SEXP args, SEXP env)
{
	SEXP d, s, x;
	int i, len;

	checkArity(op, args);
	PROTECT(x = coerceVector(CAR(args), STRSXP));
	if (!isString(x))
		error("invalid type, nchar requires a character vector\n");
	len = LENGTH(x);
	PROTECT(s = allocVector(INTSXP, len));
	for (i = 0; i < len; i++)
		INTEGER(s)[i] = strlen(CHAR(STRING(x)[i]));
	if((d = getAttrib(x, R_DimSymbol)) != R_NilValue)
		setAttrib(s, R_DimSymbol, d);
	if((d = getAttrib(x, R_DimNamesSymbol)) != R_NilValue)
		setAttrib(s, R_DimNamesSymbol, d);
	UNPROTECT(2);
	return s;
}
static void substr(char *buf, char *str, int sa, int so)
{
	int i;

	str += (sa - 1);
	for (i = 0; i <= (so - sa); i++)
		*buf++ = *str++;
	*buf = '\0';
}

SEXP do_substr(SEXP call, SEXP op, SEXP args, SEXP env)
{
	SEXP s, x, sa, so;
	int i, len, start, stop, slen, k, l;
	char buff[MAXELTSIZE];

	checkArity(op, args);
	x = CAR(args);
	sa = CADR(args);
	so = CAR(CDDR(args));

	if (!isString(x) || !isInteger(sa) || !isInteger(so))
		error("invalid type to substr\n");

	len = LENGTH(x);
	k = LENGTH(sa);
	l = LENGTH(so);
	PROTECT(s = allocVector(STRSXP, len));
	for (i = 0; i < len; i++) {
		start = INTEGER(sa)[i % k];
		stop = INTEGER(so)[i % l];
		slen = strlen(CHAR(STRING(x)[i]));
		if (start > stop || start > slen) {
			buff[0]='\0';
			STRING(s)[i] = mkChar(buff);
		}
		else {
			if (stop > slen)
				stop = slen;
			substr(buff, CHAR(STRING(x)[i]), start, stop);
			STRING(s)[i] = mkChar(buff);
		}
	}
	UNPROTECT(1);
	return s;
}

/* strsplit is going to split the strings in the first argument
   into tokens depending on the second argument. The characters
   of the second argument are used to split the first argument.
   A list of vectors is returned of length equal to the input vector x, 
   each element of the list is the collection of splits for the corresponding
   element of x.
 */
SEXP do_strsplit(SEXP call, SEXP op, SEXP args, SEXP env)
{
	SEXP s, t, x, tok, w;
	int i, j, len, tlen, ntok;
	char buff[MAXELTSIZE], *pt;

	checkArity(op, args);
	x = CAR(args);
	tok = CADR(args);

	if (!isString(x) || !isString(tok))
		error("invalid type to strsplit\n");

	len = LENGTH(x);
	tlen = LENGTH(tok);
	PROTECT(s = allocList(len));
	w = s;
	for (i = 0; i < len; i++) {
		/* first find out how many splits there will be */
		strcpy(buff, CHAR(STRING(x)[i]));
		pt = strtok(buff, CHAR(STRING(tok)[i % tlen]));
		if( pt == NULL )
			ntok = 0;
		else
			ntok = 1;
		while ((pt = strtok(NULL, CHAR(STRING(tok)[i % tlen]))) != NULL)
			ntok++;
		PROTECT(t = allocVector(STRSXP, ntok));
		strcpy(buff, CHAR(STRING(x)[i]));
		pt = strtok(buff, CHAR(STRING(tok)[i % tlen]));
		for (j = 0; j < ntok; j++) {
			STRING(t)[j] = mkChar(pt);
			pt = strtok(NULL, CHAR(STRING(tok)[i % tlen]));
		}
		CAR(w) = t;
		UNPROTECT(1);
		w = CDR(w);
	}
	UNPROTECT(1);
	return s;
}
/* abbreviate long names in the S-designated fashion; first spaces then
   lower case vowels; then lower case consonants; then upper case letters
   special characters
   letters are dropped from the end of words and at least one letter is
   retained from each word
   if use.classes is FALSE then the only differentiation is between white
   space and letters
   if unique abbreviations are not produced letters are added until the
   results are unique (duplicated names are removed prior to entry).
   names, minlength, use.classes, dot
*/

static SEXP stripchars(SEXP inchar, int minlen)
{
	int i, j, nspace=0, upper;
	char buff1[MAXELTSIZE];

	strcpy(buff1, CHAR(inchar));
	upper=strlen(buff1)-1;

	/*remove beginning blanks */
	j=0;
	for(i=0 ; i<upper ; i++ )
		if(isspace(buff1[i]))
			j++;
		else 
			break;

	strcpy(buff1,&buff1[j]);
	upper=strlen(buff1)-1;

	if(strlen(buff1)<minlen)
		goto donesc;

	for (i=upper; i>0; i--) { 
		if( isspace(buff1[i]))
			nspace++;
			/*strcpy(buff1[i],buff1[i+1]);*/
		if(strlen(buff1)-nspace <= minlen)
			goto donesc;
	}

	upper=strlen(buff1)-1;

	for (i=upper; i>=0; i--) {
		if( (buff1[i]=='a' || buff1[i]=='e' || buff1[i]=='i' || 
			buff1[i]=='o' || buff1[i]=='u') ) {
			if (i>0) {
				if(!(isspace(buff1[i-1]) && isspace(buff1[i+1])) )
					strcpy(&buff1[i],&buff1[i+1]);
			}
			else if(!isspace(buff1[i+1]))
				strcpy(&buff1[i],&buff1[i+1]);
		}
		if(strlen(buff1)-nspace <= minlen)
			goto donesc;
	}
	
	upper=strlen(buff1)-1;

	for (i=upper; i>=0; i--) {
		if ( islower(buff1[i]) ){
			if(i>0) {
				if(!(isspace(buff1[i-1]) &&isspace(buff1[i+1])) )
					strcpy(&buff1[i],&buff1[i+1]);
			}
			else if(!isspace(buff1[i+1]))
				strcpy(&buff1[i],&buff1[i+1]);
		}
		if(strlen(buff1)-nspace <= minlen)
			goto donesc;
	}

	/* all else has failed so we use brute force */

	upper=strlen(buff1);
	for (i=upper; i>0; i--) {
		if( !(isspace(buff1[i-1]) && isspace(buff1[i+1])) )
			strcpy(&buff1[i],&buff1[i+1]);
		if(strlen(buff1)-nspace <= minlen)
			goto donesc;
	}
	
donesc:
	upper=strlen(buff1);
	if(upper > minlen )
		for (i=upper-1; i>0; i--) 
			if(isspace(buff1[i]))
				strcpy(&buff1[i],&buff1[i+1]);
	
	return(mkChar(buff1));
}

SEXP do_abbrev(SEXP call, SEXP op, SEXP args, SEXP env)
{
	SEXP ans;
	int i, len, minlen, uclass;

	checkArity(op,args);

	if(!isString(CAR(args)))
		errorcall(call,"the first argument must be a string\n");

	len=length(CAR(args));
	PROTECT(ans=allocVector(STRSXP,len));
	minlen=asInteger(CADR(args));
	uclass=asLogical(CAR(CDDR(args)));

	for(i=0 ; i<len ; i++) 
		STRING(ans)[i]=stripchars(STRING(CAR(args))[i],minlen);
	UNPROTECT(1);
	return(ans);
}

#include <sys/types.h>
#include <regex.h>

SEXP do_grep(SEXP call, SEXP op, SEXP args, SEXP env)
{
	SEXP pat, vec, ind, ans;
	regex_t reg;
	int i, j, n, nmatches;
	int igcase_opt, extended_opt, value_opt, eflags;

	checkArity(op, args);
	pat = CAR(args); args = CDR(args);
	vec = CAR(args); args = CDR(args);
	igcase_opt = asLogical(CAR(args)); args = CDR(args);
	extended_opt = asLogical(CAR(args)); args = CDR(args);
	value_opt = asLogical(CAR(args)); args = CDR(args);
	if(igcase_opt == NA_INTEGER) igcase_opt = 0;
	if(extended_opt == NA_INTEGER) extended_opt = 1;
	if(value_opt == NA_INTEGER) value_opt = 0;

	if(!isString(pat) || length(pat) < 1 || !isString(vec))
		errorcall(call, "invalid argument\n");

	eflags = 0;

	if(extended_opt) eflags = eflags | REG_EXTENDED;
	if(igcase_opt) eflags = eflags | REG_ICASE;

	if(regcomp(&reg, CHAR(STRING(pat)[0]), eflags))
		errorcall(call, "invalid regular expression\n");

	n = length(vec);
	ind = allocVector(LGLSXP, n);
	nmatches = 0;
	for(i=0 ; i<n ; i++) {
		if(regexec(&reg, CHAR(STRING(vec)[i]), 0, NULL, 0) == 0) {
			INTEGER(ind)[i] = 1;
			nmatches++;
		}
		else INTEGER(ind)[i] = 0;
	}
	regfree(&reg);
	PROTECT(ind);
	if(value_opt) {
		ans = allocVector(STRSXP, nmatches);
		j = 0;
		for(i=0 ; i<n ; i++)
			if(INTEGER(ind)[i])
				STRING(ans)[j++] = STRING(vec)[i];
	}
	else {
		ans = allocVector(INTSXP, nmatches);
		j = 0;
		for(i=0 ; i<n ; i++)
			if(INTEGER(ind)[i]) INTEGER(ans)[j++] = i+1;
	}
	UNPROTECT(1);
	return ans;
}


	/*  The following R functions do substitution for     */
	/*  regular expressions, either once or globally.     */
	/*  The functions are loosely patterned on the "sub"  */
	/*  and "gsub" in "nawk".			      */

static int length_adj(char *repl, regmatch_t *regmatch, int nsubexpr)
{
	int k, n;
	char *p = repl;
	n = strlen(repl) - (regmatch[0].rm_eo - regmatch[0].rm_so);
	while(*p) {
		if(*p == '\\') {
			if('1' <= p[1] && p[1] <= '9') {
				k = p[1] - '0';
				if(k > nsubexpr)
					error("invalid backreference in regular expression\n");
				n += (regmatch[k].rm_eo - regmatch[k].rm_so) - 2;
				p++;
			}
			else if(p[1] == 0) {
				/* can't escape the final '\0' */
				n -= 1;
			}
			else {
				n -= 1;
				p++;
			}
		}
		p++;
	}
	return n;
}

static char *string_adj(char *target, char *orig, char *repl,
	regmatch_t *regmatch, int nsubexpr)
{
	int i, k;
	char *p = repl, *t = target;
	while(*p) {
		if(*p == '\\') {
			if('1' <= p[1] && p[1] <= '9') {
				k = p[1] - '0';
				for(i=regmatch[k].rm_so ; i<regmatch[k].rm_eo ; i++)
					*t++ = orig[i];
				p += 2;
			}
			else if(p[1] == 0) {
				p += 1;
			}
			else {
				p += 1;
				*t++ = *p++;
			}
		}
		else *t++ = *p++;
	}
	return t;
}

SEXP do_gsub(SEXP call, SEXP op, SEXP args, SEXP env)
{
	SEXP pat, rep, vec, ans;
	regex_t reg;
	regmatch_t regmatch[10];
	int i, j, n, ns, nsubexp, nmatch, offset;
	int global, igcase_opt, extended_opt, value_opt, eflags;
	char *s, *t, *u;

	checkArity(op, args);

	global = PRIMVAL(op);

	pat = CAR(args); args = CDR(args);
	rep = CAR(args); args = CDR(args);
	vec = CAR(args); args = CDR(args);
	igcase_opt = asLogical(CAR(args)); args = CDR(args);
	extended_opt = asLogical(CAR(args)); args = CDR(args);
	if(igcase_opt == NA_INTEGER) igcase_opt = 0;
	if(extended_opt == NA_INTEGER) extended_opt = 1;

	if(!isString(pat) || length(pat) < 1 ||
	   !isString(rep) || length(rep) < 1 ||
	   !isString(vec))
		errorcall(call, "invalid argument\n");

	eflags = 0;
	if(extended_opt) eflags = eflags | REG_EXTENDED;
	if(igcase_opt) eflags = eflags | REG_ICASE;

	if(regcomp(&reg, CHAR(STRING(pat)[0]), eflags))
		errorcall(call, "invalid regular expression\n");

	n = length(vec);
	PROTECT(ans = allocVector(STRSXP, n));

	for(i=0 ; i<n ; i++) {
		offset = 0;
		nmatch = 0;
		s = CHAR(STRING(vec)[i]);
		t = CHAR(STRING(rep)[0]);
		ns = strlen(s);
		while(regexec(&reg, &s[offset], 10, regmatch, 0) == 0) {
			nmatch += 1;
			ns += length_adj(t, regmatch, reg.re_nsub);
			offset += regmatch[0].rm_eo;
			if(s[offset] == '\0' || !global) break;
		}
		if(nmatch = 0) STRING(ans)[i] = STRING(vec)[i];
		else {
			STRING(ans)[i] = allocString(ns);
			offset = 0;
			nmatch = 0;
			s = CHAR(STRING(vec)[i]);
			t = CHAR(STRING(rep)[0]);
			u = CHAR(STRING(ans)[i]);
			ns = strlen(s);
			while(regexec(&reg, &s[offset], 10, regmatch, 0) == 0) {
				for(j=0; j<regmatch[0].rm_so ; j++)
					*u++ = s[offset+j];
				u = string_adj(u, &s[offset], t, regmatch, reg.re_nsub);
				offset += regmatch[0].rm_eo;
				if(s[offset] == '\0' || !global) break;
			}
			for(j=offset ; s[j] ; j++)
				*u++ = s[j];
			*u = '\0';
		}
	}
	regfree(&reg);
	UNPROTECT(1);
	return ans;
}
