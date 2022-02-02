#  File src/library/base/R/Scripts.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2019 The R Core Team
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

.Script <-
function(interpreter, script, args, ...)
{
    if(.Platform$OS.type == "windows") {
        cmd <- paste(shQuote(file.path(R.home("bin"), "Rcmd")),
                     file.path("..", "share", interpreter, script),
                     args)
        system(cmd, invisible = TRUE)
    }
    else
        system(paste(shQuote(file.path(R.home("bin"), "Rcmd")),
                     interpreter,
                     shQuote(file.path(R.home("share"),
                                       interpreter, script)),
                     args),
               ...)
}
