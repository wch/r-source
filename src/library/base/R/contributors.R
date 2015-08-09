#  File src/library/base/R/contributors.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2012 The R Core Team
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
#  https://www.R-project.org/Licenses/

contributors <- function()
{
    outFile <- tempfile()
    outConn <- file(outFile, open = "w")
    writeLines(paste0("R is a project which is attempting to provide a ",
                      "modern piece of\nstatistical software for the ",
                      "GNU suite of software.\n\n",
                      "The current R is the result of a collaborative ",
                      "effort with\ncontributions from all over the ",
                      "world.\n\n"), outConn)
    writeLines(readLines(file.path(R.home("doc"), "AUTHORS")), outConn)
    writeLines("", outConn)
    writeLines(readLines(file.path(R.home("doc"), "THANKS")), outConn)
    close(outConn)
    file.show(outFile, delete.file = TRUE)
}
