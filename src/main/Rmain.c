/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2004  Robert Gentleman, Ross Ihaka
 *			      and the R Development Core Team
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
# include <config.h>
#endif
#include "Defn.h" /* remap mainloop */

int Rf_initialize_R(int ac, char **av); /* in system.c */


int main(int ac, char **av)
{
    Rf_initialize_R(ac, av);

    mainloop();
    /*++++++  in ../main/main.c */
    return 0;
}

	/* Declarations to keep f77 happy */

int MAIN_(int ac, char **av)  {return 0;}
int MAIN__(int ac, char **av) {return 0;}
int __main(int ac, char **av) {return 0;}
