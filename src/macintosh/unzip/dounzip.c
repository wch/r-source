/*
 *  R : A Computer Language for Statistical Data Analysis
 *  file dounzip.c
 *  Copyright (C) 1998--2000  Guido Masarotto and Brian Ripley
 *  Adapted to Macintosh by Stefano M. Iacus, Jan 2001
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
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "Defn.h"
static int do_unzip(char *zipname, char *dest, int nfiles, char **files,
		    int nxfiles, char **xfiles, int over);

SEXP do_int_unzip(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP  fn, ans;
    char zipname[PATH_MAX], *topics[500], dest[PATH_MAX];
    int i, ntopics, rc;

    
    checkArity(op, args);
    
    if (!isString(CAR(args)) || LENGTH(CAR(args)) != 1)
	errorcall(call, "invalid zip name argument");
    strcpy(zipname, CHAR(STRING_ELT(CAR(args), 0)));
    args = CDR(args);
    fn = CAR(args);
    ntopics = length(fn);
    if (ntopics > 0) {
	if (!isString(fn) || ntopics > 500)
	    errorcall(call, "invalid topics argument");
	for(i = 0; i < ntopics; i++)
	    topics[i] = CHAR(STRING_ELT(fn, i));
    }
    args = CDR(args);
    if (!isString(CAR(args)) || LENGTH(CAR(args)) != 1)
	errorcall(call, "invalid destination argument");
    strcpy(dest, CHAR(STRING_ELT(CAR(args), 0)));
   
    rc = do_unzip(zipname, dest, ntopics, topics, 0 , NULL, 1);

    PROTECT(ans = allocVector(INTSXP, 1));
    INTEGER(ans)[0] = rc;
    UNPROTECT(1);
    return ans;
}

int mac_dounzip(int argc,char **argv);

static int do_unzip(char *zipname, char *dest, int nfiles, char **files,
	    int nxfiles, char **xfiles, int over)
{
    int retcode, i;
    int my_argc;
    char *my_argv[100];
    char option[]="-d", app[]="R";
    char option1[]="-oq ";  
    int incr=0;
    
    my_argc = nfiles + 1 + incr;
    
    my_argv[0] = app;
    
    my_argv[incr] = option1;

    my_argv[1+incr] = zipname;
    
    for(i=0;i<nfiles;i++)
      my_argv[i+2+incr] = files[i];

    
    if(dest && dest[0]!='\0')
     {
      my_argv[nfiles+2+incr] = option;
      my_argv[nfiles+3+incr] = dest;
      my_argc += 2;
    }
  
   my_argv[my_argc+1]=NULL;
         
   mac_dounzip(my_argc,my_argv);
     
       
    return 0;
}

