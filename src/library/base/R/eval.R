#  File src/library/base/R/eval.R
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

.GlobalEnv <- environment()
parent.frame <- function(n = 1) .Internal(parent.frame(n))

eval <-
    function(expr, envir = parent.frame(),
	     enclos = if(is.list(envir) || is.pairlist(envir))
                       parent.frame() else baseenv())
    .Internal(eval(expr, envir,enclos))

eval.parent <- function(expr, n = 1){
    p <- parent.frame(n + 1)
    eval(expr , p)
}

evalq <-
    function (expr, envir, enclos)
    eval.parent(substitute(eval(quote(expr), envir, enclos)))

new.env <- function (hash=FALSE, parent=parent.frame(), size=29L)
    .Internal(new.env(hash, parent, size))

parent.env <- function(env)
    .Internal(parent.env(env))

"parent.env<-" <- function(env, value)
    .Internal("parent.env<-"(env, value))

local <-
    function (expr, envir = new.env())
    eval.parent(substitute(eval(quote(expr), envir)))

Recall <- function(...) .Internal(Recall(...))

with <- function(data, expr, ...) UseMethod("with")
within <- function(data, expr, ...) UseMethod("within")

with.default <- function(data, expr, ...)
    eval(substitute(expr), data, enclos=parent.frame())
within.default <- with.default

within.data.frame <- within.list <- function(data, expr, ...) {
    subd <- substitute(data)
    if (is.name(subd))
        subd <- deparse(subd)
    if (!is.character(subd) || length(subd) != 1)
        stop("'within' requires a name for its 'data' argument")
    parent <- parent.frame()
    if (!exists(subd, envir = parent, inherits = TRUE))
        stop("'data' must exist")

    newd <- get(subd, envir=parent)
    e   <- evalq(environment(), newd)
    ret <- eval(substitute(expr), e)
    l   <- as.list(e)
    del <- setdiff(names(newd), names(l))
    newd[names(l)] <- l
    newd[del] <- NULL
    assign(subd, newd, envir=parent, inherits=TRUE)
    invisible(ret)
}





force <- function(x) x
