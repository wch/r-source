#  File src/library/base/R/bindenv.R
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

lockEnvironment <- function(env, bindings = FALSE)
    .Internal(lockEnvironment(env, bindings))

environmentIsLocked <- function(env)
    .Internal(environmentIsLocked(env))

lockBinding <- function(sym, env) {
    if (is.character(sym)) sym <- as.name(sym)
    .Internal(lockBinding(sym, env))
}

bindingIsLocked <- function(sym, env) {
    if (is.character(sym)) sym <- as.name(sym)
    .Internal(bindingIsLocked(sym, env))
}

makeActiveBinding <- function(sym, fun, env) {
    if (is.character(sym)) sym <- as.name(sym)
    .Internal(makeActiveBinding(sym, fun, env))
}

bindingIsActive <- function(sym, env) {
    if (is.character(sym)) sym <- as.name(sym)
    .Internal(bindingIsActive(sym, env))
}

unlockBinding <- function(sym, env) {
    if (is.character(sym)) sym <- as.name(sym)
    .Internal(unlockBinding(sym, env))
}
