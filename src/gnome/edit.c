/*
 *  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (C) 1998-1999   Lyndon Drake
 *                            and the R Development Core Team
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

#include "Defn.h"
#include "Print.h"
#include "Fileio.h"
#include "IOStuff.h"
#include "Parse.h"

/*
 * ed, vi etc have 3 parameters. the data, a file and an editor
 *
 * if file is specified then the given file is used (and not removed on exit) if
 * file is not specified then a temporary file is used; since only one
 * temporary file is used for an entire session previous editing is lost
 *
 * if data is specified then it is passed out to be edited; if data is not
 * specified then either file (if specified) or the temporary file is used
 * (thus errors can be re-editied by calling edit a second time with no
 * arguments).
 *
 * if the editor is specified then the specified editor is invoked if possible
 * and an error message reported otherwise
 */

#ifdef HAVE_CONFIG_H
#include <Rconfig.h>
#endif

#include "Defn.h"
#include "Print.h"
#include "Fileio.h"
#include "Parse.h"

#include "rgnome.h"

void InitEd ();
SEXP do_edit (SEXP call, SEXP op, SEXP args, SEXP rho);


void InitEd ()
{
}

SEXP do_edit (SEXP call, SEXP op, SEXP args, SEXP rho)
{
    errorcall (call, "edit() is unavailable");
    return NULL;
}

