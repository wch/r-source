#  File src/library/base/R/debug.R
#  Part of the R package, https://www.R-project.org
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
#  https://www.R-project.org/Licenses/

debug <- function(fun, text = "", condition = NULL, signature = NULL) {
    if(is.null(signature))
        .Internal(debug(fun, text, condition))
    else if(requireNamespace("methods"))
        methods::.debugMethod(fun, text, condition, signature, once = FALSE)
    else stop("failed to load the methods package for debugging by signature")
}

debugonce <- function(fun, text = "", condition = NULL, signature = NULL) {
    if(is.null(signature))
        .Internal(debugonce(fun, text, condition))
    else if(requireNamespace("methods"))
        methods::.debugMethod(fun, text, condition, signature, once = TRUE)
    else stop("failed to load the methods package for debugging by signature")
}

undebug <- function(fun, signature = NULL) {
    if(is.null(signature))
        .Internal(undebug(fun))
    else if(requireNamespace("methods"))
        methods::.undebugMethod(fun, signature = signature)
    else stop("failed to load methods package for undebugging by signature")
}

isdebugged <- function(fun, signature = NULL) {
    if(is.null(signature))
        .Internal(isdebugged(fun))
    else if(requireNamespace("methods"))
        methods::.isMethodDebugged(fun, signature)
    else stop("failed to load methods package for handling signature")
}

browserText <- function(n=1L) .Internal(browserText(n))
browserCondition <- function(n=1L) .Internal(browserCondition(n))
browserSetDebug <- function(n=1L) .Internal(browserSetDebug(n))

debuggingState <- function(on = NULL) .Internal(debugOnOff(on))
