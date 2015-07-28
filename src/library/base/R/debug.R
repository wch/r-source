#  File src/library/base/R/debug.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2014 The R Core Team
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

debug <- function(fun, text="", condition=NULL)
    .Internal(debug(fun, text, condition))
debugonce <- function(fun, text="", condition=NULL)
    .Internal(debugonce(fun, text, condition))
undebug <- function(fun) .Internal(undebug(fun))
isdebugged <- function(fun) .Internal(isdebugged(fun))

browserText <- function(n=1L) .Internal(browserText(n))
browserCondition <- function(n=1L) .Internal(browserCondition(n))
browserSetDebug <- function(n=1L) .Internal(browserSetDebug(n))

debuggingState <- function(on = NULL) .Internal(debugOnOff(on))
