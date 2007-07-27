#  File src/library/base/R/license.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

licence <- license <- function() {
    cat("\nThis software is distributed under the terms of the GNU General\n")
    cat("Public License Version 2, June 1991.  The terms of this license\n")
    cat("are in a file called COPYING which you should have received with\n")
    cat("this software and which can be displayed by RShowDoc(\"COPYING\").\n")
    cat("\n")
    cat("If you have not received a copy of this file, you can obtain one\n")
    cat("at http://www.R-project.org/licenses/.\n")
    cat("\n")
    cat("A small number of files (the API header files listed in\n")
    cat("R_DOC_DIR/COPYRIGHTS) are distributed under the\n")
    cat("Lesser GNU General Public LIcense version 2.1.\n")
    cat("This can be displayed by RShowDoc(\"COPYING.LIB\"),\n")
    cat("or obtained at the URI given.\n")
    cat("\n")
    cat("'Share and Enjoy.'\n\n")
}
