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
#include "Print.h"

static SEXP gcall;
static SEXP rlist, rlistp;
static int nr, nc;
static SEXP AsIsSymbol;

static int Handsoff(SEXP x)
{
	return asLogical(getAttrib(x, AsIsSymbol)) == 1;
}

static void BadDimensions()
{
	errorcall(gcall, "incompatible argument dimensions\n");
}

static int CountItems(SEXP sxp, int handsoff)
{
	int lenx;

	if(isNull(sxp))
		return 0;
	if(isMatrix(sxp)) {
		if(nr && (nr != nrows(sxp)))
			BadDimensions();
		nr = nrows(sxp);
		nc += ncols(sxp);
		if(handsoff || Handsoff(sxp))
			return 1;
		else return ncols(sxp);
	}
	if(isVector(sxp)) {
		lenx = length(sxp);
		if(!nr)
			nr = lenx;
		if(lenx != 1 && nr != 1 && lenx != nr )
			BadDimensions();
		if( lenx > nr )
			nr = lenx;
		nc += 1;
		return 1;
	}
	if(isList(sxp) || isFrame(sxp)) {
		return CountItems(CAR(sxp),handsoff)
			+ CountItems(CDR(sxp),handsoff);
	}
	errorcall(gcall, "invalid argument type\n");
}

static void InsertVector(SEXP sxp, int handsoff)
{
	SEXP tmp;
	int i;

	if(NAMED(sxp))
		sxp = duplicate(sxp);
	PROTECT(sxp);

	if( length(sxp) == 1 && nr!= 1 ) {
		tmp=allocVector(TYPEOF(sxp),nr);
		copyVector(tmp,sxp);
		sxp=tmp;
	}
	UNPROTECT(1);
	CAR(rlistp) = sxp;
	NAMED(sxp) = 1;		/* can't re-use */
	rlistp = CDR(rlistp);
}

static void InsertMatrix(SEXP sxp, int handsoff)
{
	SEXP vec;
	int i, j, k;

	if(handsoff || Handsoff(sxp)) {
		if(NAMED(sxp)) sxp = duplicate(sxp);
		CAR(rlistp) = sxp;
		rlistp = CDR(rlistp);
	}
	else {
		k = ncols(sxp);
		for(j=0 ; j<k ; j++) {
			CAR(rlistp) = vec = allocVector(TYPEOF(sxp), nr);
			switch(TYPEOF(sxp)) {
			case LGLSXP:
			case INTSXP:
			case FACTSXP:
			case ORDSXP:
				for(i=0 ; i<nr ; i++)
					INTEGER(vec)[i] = INTEGER(sxp)[i+j*nr];
				break;
			case REALSXP:
				for(i=0 ; i<nr ; i++)
					REAL(vec)[i] = REAL(sxp)[i+j*nr];
				break;
			case STRSXP:
				for(i=0 ; i<nr ; i++)
					STRING(vec)[i] = STRING(sxp)[i+j*nr];
				break;
			}
			if(isFactor(sxp)) {
				LEVELS(vec) = LEVELS(sxp);
				setAttrib(vec, R_LevelsSymbol,
					getAttrib(sxp, R_LevelsSymbol));
			}
			rlistp = CDR(rlistp);
		}
	}
}

static void InsertItems(SEXP sxp, int handsoff)
{
	if(isNull(sxp))
		return;
	if(isMatrix(sxp))
		InsertMatrix(sxp, handsoff);
	else if(isVector(sxp))
		InsertVector(sxp, handsoff);
	else if(isList(sxp) || isFrame(sxp)) {
		InsertItems(CAR(sxp), handsoff);
		InsertItems(CDR(sxp), handsoff);
	}
	else errorcall(gcall, "invalid argument type\n");
}

	/* If tag is a valid name (i.e. it starts with an alphabetic */
	/* character), then return a legal name constructed by replacing */
	/* illegal characters by ".".  Otherwise, make up a name which is */
	/* equal to "X" followed by the offset of this variable within the */
	/* frame.  A separate function called MatrixName takes care of the */
	/* matrix case. */

static int cnt;

static SEXP ValidName(SEXP tag)
{
	if(isNull(tag) || !(isalpha(CHAR(tag)[0]) || (CHAR(tag)[0]) == '.'))  {
		char buf[10];
		sprintf(buf,"X%d", cnt+1);
		return install(buf);
	}
	else {
		SEXP d = duplicate(tag);
		char *p = CHAR(d);
		while(*p) {
			if(!isalnum(*p)) *p = '.';
			p++;
		}
		return install(CHAR(d));
	}
}

static SEXP MatrixName(SEXP tag, int index)
{
	char *p;
	p = Rsprintf("%s.%d", CHAR(tag), index);
	return install(p);
}

static void NameItem(SEXP sxp, int handsoff)
{
	SEXP s;
	int i, p;

	if(isMatrix(CAR(sxp))) {
		if(handsoff || Handsoff(sxp)) {
			TAG(rlistp) = ValidName(PRINTNAME(TAG(sxp)));
			rlistp = CDR(rlistp);
			cnt += 1;
		}
		else {
			p = ncols(CAR(sxp));
			s = CADR(getAttrib(CAR(sxp), R_DimNamesSymbol));
			if(isNull(s)) {
				for(i=0 ; i<p ; i++) {
					TAG(rlistp) = MatrixName(PRINTNAME(TAG(sxp)),i+1);
					rlistp = CDR(rlistp);
					cnt += 1;
				}
			}
			else {
				/* Use Dimnames */
				for(i=0 ; i<p ; i++) {
					TAG(rlistp) = ValidName(STRING(s)[i]);
					rlistp = CDR(rlistp);
					cnt += 1;
				}
			}
		}
	}
	else if(isVector(CAR(sxp))) {
		TAG(rlistp) = ValidName(PRINTNAME(TAG(sxp)));
		rlistp = CDR(rlistp);
		cnt += 1;
	}
	else if(isList(CAR(sxp)) || isFrame(CAR(sxp))) {
		for(s=CAR(sxp) ; s!=R_NilValue ; s=CDR(s)) {
			TAG(rlistp) = ValidName(PRINTNAME(TAG(s)));
			rlistp = CDR(rlistp);
			cnt += 1;
		}
	}
	else errorcall(gcall, "invalid argument type\n");
}

static void NameItems(SEXP sxp, int handsoffarg)
{
	int handsoff;
	SEXP h;
	rlistp = rlist;
	cnt = 0;
	while(sxp != R_NilValue) {
		if(!isNull(h = getAttrib(CAR(sxp), AsIsSymbol))) {
			handsoff = asLogical(h);
			if(handsoff != 1) handsoff = 0;
		}
		else handsoff = handsoffarg;
		NameItem(sxp, handsoff);
		sxp = CDR(sxp);
	}
}

static void NameRows(SEXP sxp, SEXP rn)
{
	SEXP d, s;
	if(!isString(rn) || length(rn) != nr) {
		rn = R_NilValue;
		for(s=sxp; s!=R_NilValue ; s=CDR(s)) {
			if(isMatrix(CAR(s))) {
				d = CADR(getAttrib(CAR(s),R_DimNamesSymbol));
				if(length(d) == nr) {
					rn = d;
					break;
				}
			}
			else if(isVector(CAR(s))) {
				d = getAttrib(CAR(s), R_NamesSymbol);
				if(length(d) == nr) {
					rn = d;
					break;
				}
			}
			else if(isFrame(CAR(s)) || isList(CAR(s))) {
				d = getAttrib(CAR(s), install("row.names"));
				if(isString(d) && length(d) == nr) {
					rn = d;
					break;
				}
			}
		}
	}
	setAttrib(rlist, install("row.names"), rn);
}

	/* FrameClassFix - If a list is not a data frame, */
	/* ensure that it does not have a class "data.frame" */

void RemoveClass(SEXP, char*);

void FrameClassFix(SEXP x)
{
        SEXP xcar, xp;          
        int nr = -1;    
        for(xp=x ; xp!=R_NilValue ; xp=CDR(xp)) {
                xcar = CAR(xp);
                if(isArray(xcar)) {
                        if(isMatrix(xcar)) {
                                if(nr == -1)
                                        nr = nrows(xcar);
                                else if(nr != nrows(xcar))
                                        goto unclass;
                        }
                        else goto unclass;
                }       
                else if(isVector(xcar)) {
                        if(nr == -1)
                                nr = length(xcar);
                        else if(nr != length(xcar))
                                goto unclass;

                }
                else goto unclass;
        }
        return;
unclass:
        PROTECT(x);
        RemoveClass(x, "data.frame");
        UNPROTECT(1);
        return;
}

	/* Make an object be of class data.frame */

void DataFrameClass(SEXP frame)
{
	SEXP class, oclass;
	int i, nclass;
	PROTECT(oclass = getAttrib(frame, R_ClassSymbol));
	nclass = length(oclass);
	PROTECT(class = allocVector(STRSXP, nclass+1));
	STRING(class)[0] = mkChar("data.frame");
	for(i=0 ; i<nclass ; i++)
		STRING(class)[i+1] = STRING(oclass)[i];
	setAttrib(frame, R_ClassSymbol, class);
	UNPROTECT(2);
}

	/* This is called as data.frame(list(...),row.names). */
	/* This builds a dataframe from the given arguments */
	/* The interpreted interface has named the list components */

SEXP do_dataframe(SEXP call, SEXP op, SEXP args, SEXP env)
{
	SEXP a, h;
	int size, handsoff, handsoffarg;

	checkArity(op, args);
	gcall = call;
	AsIsSymbol = install("AsIs");

	/*  We should build a vector of "as.is"-ness and then
	    over-ride any NAs in it by looking at the argument */

	handsoffarg = (asLogical(CADDR(args)) == 1);

	nr = 0;
	nc = 0;
	size = 0;
	for(a=CAR(args) ; a!=R_NilValue ; a=CDR(a)) {
		if(!isNull(h = getAttrib(CAR(a), AsIsSymbol))) {
			handsoff = asLogical(h);
			if(handsoff != 1) handsoff = 0;
		}
		else handsoff = handsoffarg;
		size += CountItems(CAR(a), handsoff);
	}

	PROTECT(rlistp = rlist = allocList(size));

	for(a=CAR(args) ; a!=R_NilValue ; a=CDR(a)) {
		if(!isNull(h = getAttrib(CAR(a), AsIsSymbol))) {
			handsoff = asLogical(h);
			if(handsoff != 1) handsoff = 0;
		}
		else handsoff = handsoffarg;
		InsertItems(CAR(a), handsoff);
	}

	rlistp = rlist;
	NameItems(CAR(args), handsoffarg);
	NameRows(CAR(args), CADR(args));

	DataFrameClass(rlist);
	UNPROTECT(1);
	return rlist;
}

SEXP do_printdf(SEXP call, SEXP op, SEXP args, SEXP env)
{
	printDataFrame(CAR(args));
	R_Visible = 0;
	return CAR(args);
}

SEXP do_asmatrixdf(SEXP call, SEXP op, SEXP args, SEXP env)
{
	int i, ix, k, nr, nc, type;
	SEXP ans, cx, s, x;
	SEXP truestring, falsestring;

	checkArity(op, args);
	if(!isFrame(CAR(args)))
		error("invalid argument in \"as.matrix.data.frame\"\n");
	type = 0;
	nc = 0;
	nr = 0;
	for(x=CAR(args) ; x!=R_NilValue ; x=CDR(x)) {
		switch(TYPEOF(CAR(x))) {
			case LGLSXP:
				type = type | 1;
				break;
			case INTSXP:
				type = type | 2;
				break;
			case FACTSXP:
			case ORDSXP:
				type = type | 4;
				break;
			case REALSXP:
				type = type | 8;
				break;
			case STRSXP:
				type = type | 16;
				break;
			default:
				error("invalid type in as.matrix.data.frame\n");
		}
		nc += ncols(CAR(x));
	}
	if(type & 1 && (type & 4 || type & 16)) {
		PROTECT(truestring = mkChar("T"));
		PROTECT(falsestring = mkChar("F"));
	}
	else {
		PROTECT(R_NilValue);
		PROTECT(R_NilValue);
	}
	if(type & 4 || type & 16)
		type = STRSXP;
	else
		type = REALSXP;
	nr = nrows(CAAR(args));
	if(nr <= 0 || nc <= 0)
		error("invalid matrix extents in \"as.matrix.data.frame\"\n");
	PROTECT(ans = allocMatrix(type, nr,nc));
	k = 0;
	for(x=CAR(args) ; x!=R_NilValue ; x=CDR(x)) {
		cx = CAR(x);
		switch(type) {
		case REALSXP:
			switch(TYPEOF(cx)) {
			case LGLSXP:
			case INTSXP:
				for(i=0 ; i<LENGTH(cx) ; i++) {
					if(INTEGER(cx)[i] == NA_INTEGER)
						REAL(ans)[k++] = NA_REAL;
					else
						REAL(ans)[k++] = INTEGER(cx)[i];
				}
				break;
			case REALSXP:
				for(i=0 ; i<LENGTH(cx) ; i++)
					REAL(ans)[k++] = REAL(cx)[i];
				break;
			}
			break;
		case STRSXP:
			switch(TYPEOF(cx)) {
			case LGLSXP:
				for(i=0 ; i<LENGTH(cx) ; i++) {
					if(INTEGER(cx)[i] == NA_INTEGER)
						STRING(ans)[k++] = NA_STRING;
					else if(INTEGER(cx)[i] != 0)
						STRING(ans)[k++] = truestring;
					else
						STRING(ans)[k++] = falsestring;
				}
				break;
			case INTSXP:
			case REALSXP:
				for(i=0 ; i<LENGTH(cx) ; i++)
					STRING(ans)[k++] = mkChar(EncodeElement(cx,i,0));
				break;
			case FACTSXP:
			case ORDSXP:
				s = getAttrib(cx, R_LevelsSymbol);
				if(s == R_NilValue) {
					for(i=0 ; i<LENGTH(cx) ; i++)
						STRING(ans)[k++] = mkChar(EncodeElement(cx,i,0));
				}
				else {
					for(i=0 ; i<LENGTH(cx) ; i++) {
						ix = FACTOR(cx)[i];
						if(ix == NA_INTEGER)
							STRING(ans)[k++] = NA_STRING;
						else
							STRING(ans)[k++] = STRING(s)[ix-1];
					}
				}
				break;
			case STRSXP:
				for(i=0 ; i<LENGTH(cx) ; i++)
					STRING(ans)[k++] = STRING(cx)[i];
				break;
			}
			break;
		}
	}
	/* attach dimnames here */
	UNPROTECT(3);
	return ans;
}

	/* any.data.frame - this function returns TRUE if any */
	/* of its arguments is a data frame and FALSE otherwise */

SEXP do_anydf(SEXP call, SEXP op, SEXP args, SEXP env)
{
	SEXP ans;

	ans = allocVector(LGLSXP, 1);
	LOGICAL(ans)[0] = 0;
	while(args != R_NilValue) {
		if(isFrame(CAR(args))) {
			LOGICAL(ans)[0] = 1;
			return ans;
		}
		args=CDR(args);
	}
	return ans;
}
