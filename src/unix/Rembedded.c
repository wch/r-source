/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2006  The R Core Team
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
 *  https://www.R-project.org/Licenses/
 */

/* <UTF8> char here is handled as a whole string */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif
#include <Defn.h>

int Rf_initialize_R(int ac, char **av); /* in ../unix/system.c */
void setup_Rmainloop(void); /* in main.c */
void fpu_setup(Rboolean start);  /* in ../unix/sys-std.c */
extern void R_CleanTempDir(void);


/*
 This is the routine that can be called to initialize the R environment
 when it is embedded within another application (by loading libR.so).

 The arguments are the command line arguments that would be passed to
 the regular standalone R, including the first value identifying the
 name of the `application' being run.  This can be used to indicate in
 which application R is embedded and used by R code (e.g. in the
 Rprofile) to determine how to initialize itself. These are accessible
 via the R function commandArgs().

 The return value indicates whether the initialization was successful
 (Currently there is a possibility to do a long jump within the
 initialization code so that will we never return here.)

 Example:
	 0) name of executable
	 1) don't load the X11 module
	 2) don't show the banner at startup.


    char *argv[]= {"REmbeddedPostgres", "--gui=none", "--silent"};
    Rf_initEmbeddedR(sizeof(argv)/sizeof(argv[0]), argv);
*/

int Rf_initEmbeddedR(int argc, char **argv)
{
    Rf_initialize_R(argc, argv);
    R_Interactive = TRUE;  /* Rf_initialize_R set this based on isatty */
    setup_Rmainloop();
    return(1);
}

/* use fatal !=0 for emergency bail out */
void Rf_endEmbeddedR(int fatal)
{
    R_RunExitFinalizers();
    CleanEd();
    if(!fatal) KillAllDevices();
    R_CleanTempDir();
    if(!fatal && R_CollectWarnings)
	PrintWarnings();	/* from device close and .Last */
    fpu_setup(FALSE);
}
