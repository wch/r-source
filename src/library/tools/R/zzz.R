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

library.dynam("tools", "tools", .Library)
PS_sigs <- getDLLRegisteredRoutines("tools")[[c(".Call", "ps_sigs")]]

## These are created at install time: the numbers are hard-coded in signals.c
## They happen to be the BSD ones as this started in multicore
SIGHUP <- .Call(PS_sigs, 1L)
SIGINT <- .Call(PS_sigs, 2L)
SIGQUIT <- .Call(PS_sigs, 3L)
SIGKILL <- .Call(PS_sigs, 9L)
SIGTERM <- .Call(PS_sigs, 15L)
SIGSTOP <- .Call(PS_sigs, 17L)
SIGTSTP <- .Call(PS_sigs, 18L)
SIGCONT <- .Call(PS_sigs, 19L)
SIGCHLD <- .Call(PS_sigs, 20L)
SIGUSR1 <- .Call(PS_sigs, 30L)
SIGUSR2 <- .Call(PS_sigs, 31L)

## This calls C code in the package too
latexArgCount <- integer()              # The next line modifies this
latexTable <- makeLatexTable(utf8table)  # FIXME: Should latexTable be hardcoded instead?
rm(PS_sigs)
