#  File src/library/tools/R/zzz.R
#  Part of the R package, http://www.R-project.org
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
#  http://www.r-project.org/Licenses/

.noGenerics <- TRUE

.onUnload <-
function(libpath)
    library.dynam.unload("tools", libpath)

## These are created at install time: the numbers are hard-coded in signals.c
## They happen to be the BSD ones as this started in multicore
## They are created before the C symbols are registered
library.dynam("tools", "tools", .Library)
SIGHUP <- .Call("ps_sigs", 1L, PACKAGE = "tools")
SIGINT <- .Call("ps_sigs", 2L, PACKAGE = "tools")
SIGQUIT <- .Call("ps_sigs", 3L, PACKAGE = "tools")
SIGKILL <- .Call("ps_sigs", 9L, PACKAGE = "tools")
SIGTERM <- .Call("ps_sigs", 15L, PACKAGE = "tools")
SIGSTOP <- .Call("ps_sigs", 17L, PACKAGE = "tools")
SIGTSTP <- .Call("ps_sigs", 18L, PACKAGE = "tools")
SIGCONT <- .Call("ps_sigs", 19L, PACKAGE = "tools")
SIGCHLD <- .Call("ps_sigs", 20L, PACKAGE = "tools")
SIGUSR1 <- .Call("ps_sigs", 30L, PACKAGE = "tools")
SIGUSR2 <- .Call("ps_sigs", 31L, PACKAGE = "tools")



latexArgCount <- integer()              # The next line modifies this
latexTable <- makeLatexTable(utf8table)  # FIXME: Should latexTable be hardcoded instead?
