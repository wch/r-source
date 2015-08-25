#  File src/library/stats/R/nafns.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2013 The R Core Team
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

na.pass <- function(object, ...) object
na.action <- function(object, ...) UseMethod("na.action")
na.action.default <- function(object, ...)
{
    if(is.list(object) && "na.action" %in% names(object)) object[["na.action"]]
    else attr(object, "na.action")
}

na.fail <- function(object, ...) UseMethod("na.fail")
na.fail.default <- function(object, ...)
{
    ok <- complete.cases(object)
    if(all(ok)) object else stop("missing values in object")
}

na.omit <- function(object, ...) UseMethod("na.omit")

na.omit.default <- function(object, ...)
{
    ## only handle vectors and matrices
    if (!is.atomic(object)) return(object)
    d <- dim(object)
    if (length(d) > 2L) return(object)
    omit <- seq_along(object)[is.na(object)]
    if (length(omit) == 0L) return(object)
    if (length(d)){
        omit <- unique(((omit-1) %% d[1L]) + 1L)
        nm <- rownames(object)
        object <- object[-omit, , drop=FALSE]
    } else {
        nm <- names(object)
        object <- object[-omit]
    }
    if (any(omit > 0L)) {
	names(omit) <- nm[omit]
	attr(omit, "class") <- "omit"
	attr(object, "na.action") <- omit
    }
    object
}

na.omit.data.frame <- function(object, ...)
{
    ## Assuming a data.frame like object
    n <- length(object)
    omit <- logical(nrow(object))
    vars <- seq_len(n)
    for(j in vars) {
	x <- object[[j]]
	if(!is.atomic(x)) next
	## variables are assumed to be either some sort of matrix, numeric,...
	x <- is.na(x)
	d <- dim(x)
	if(is.null(d) || length(d) != 2L)
	    omit <- omit | x
	else # matrix
	    for(ii in 1L:d[2L])
		omit <- omit | x[, ii]
    }
    xx <- object[!omit, , drop = FALSE]
    if (any(omit > 0L)) {
	temp <- setNames(seq(omit)[omit],
			 attr(object, "row.names")[omit])
	attr(temp, "class") <- "omit"
	attr(xx, "na.action") <- temp
    }
    xx
}

na.exclude <- function(object, ...) UseMethod("na.exclude")

na.exclude.default <- function(object, ...)
{
    ## only handle vectors and matrices
    if (!is.atomic(object)) return(object)
    d <- dim(object)
    if (length(d) > 2L) return(object)
    omit <- seq_along(object)[is.na(object)]
    if (length(omit) == 0L) return(object)
    if (length(d)){
        omit <- unique(((omit-1) %% d[1L]) + 1L)
        nm <- rownames(object)
        object <- object[-omit, , drop=FALSE]
    } else {
        nm <- names(object)
        object <- object[-omit]
    }
    if (any(omit > 0L)) {
	names(omit) <- nm[omit]
	attr(omit, "class") <- "exclude"
	attr(object, "na.action") <- omit
    }
    object
}

na.exclude.data.frame <- function(object, ...)
{
    ## Assuming a data.frame like object
    n <- length(object)
    omit <- logical(nrow(object))
    vars <- seq_len(n)
    for(j in vars) {
	x <- object[[j]]
	if(!is.atomic(x)) next
	## variables are assumed to be either some sort of matrix, numeric,...
	x <- is.na(x)
	d <- dim(x)
	if(is.null(d) || length(d) != 2L)
	    omit <- omit | x
	else # matrix
	    for(ii in 1L:d[2L])
		omit <- omit | x[, ii]
    }
    xx <- object[!omit, , drop = FALSE]
    if (any(omit > 0L)) {
	temp <- setNames(seq(omit)[omit],
			 attr(object, "row.names")[omit])
	attr(temp, "class") <- "exclude"
	attr(xx, "na.action") <- temp
    }
    xx
}

naresid <- function(omit, x, ...) UseMethod("naresid")
naresid.default <- function(omit, x, ...) x

## naresid.exclude (same as napredict...) *reconstruct* original size values:
naresid.exclude <- function(omit, x, ...)
{
    if (length(omit) == 0 || !is.numeric(omit))
	stop("invalid argument 'omit'")

    ## the next line copes with calls from older versions of weights.default.
    if (is.null(x)) return(x)

    if (is.matrix(x)) {
	n <- nrow(x)
	keep <- rep.int(NA, n+length(omit))
	keep[-omit] <- 1L:n
	x <- x[keep, , drop=FALSE]
	temp <- rownames(x)
	if (length(temp)) {
	    temp[omit] <- names(omit)
	    rownames(x) <- temp
        }
    } else {# vector *or* data.frame !
	n <- length(x)
	keep <- rep.int(NA, n+length(omit))
	keep[-omit] <- 1L:n
	x <- x[keep]
	temp <- names(x)
	if (length(temp)) {
	    temp[omit] <- names(omit)
	    names(x) <- temp
        }
    }
    x
}

naprint <- function(x, ...) UseMethod("naprint")
naprint.default <- function(x, ...) return("")
naprint.exclude <- naprint.omit <- function(x, ...)
    sprintf(ngettext(n <- length(x), "%d observation deleted due to missingness",
                     "%d observations deleted due to missingness"),
            n)

napredict <- function(omit, x, ...) UseMethod("napredict")
napredict.default <- function(omit, x, ...) x
napredict.exclude <- function(omit, x, ...) naresid.exclude(omit, x)
