#  File src/library/base/R/dates.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2018 The R Core Team
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

## First shot at adding a "Date" class to base R.
## Representation is the number of whole days since 1970-01-01.

## The difftime class already covers time differences in days.

## Need to take timezone into account here
Sys.Date <- function() as.Date(as.POSIXlt(Sys.time()))

as.Date <- function(x, ...) UseMethod("as.Date")

as.Date.POSIXct <- function(x, tz = "UTC", ...)
{
    if(tz == "UTC") {
        z <- floor(unclass(x)/86400)
        attr(z, "tzone") <- NULL
        .Date(z)
    } else
        as.Date(as.POSIXlt(x, tz = tz))
}

as.Date.POSIXlt <- function(x, ...) .Internal(POSIXlt2Date(x))

as.Date.factor <- function(x, ...) as.Date(as.character(x), ...)


as.Date.character <- function(x, format,
                              tryFormats = c("%Y-%m-%d", "%Y/%m/%d"),
                              optional = FALSE, ...)
{
    charToDate <- function(x) {
	xx <- x[1L]
        if(is.na(xx)) {
            j <- 1L
            while(is.na(xx) && (j <- j+1L) <= length(x)) xx <- x[j]
            if(is.na(xx)) f <- "%Y-%m-%d" # all NAs
        }
	if(is.na(xx))
            strptime(x, f)
        else {
            for(ff in tryFormats)
                if(!is.na(strptime(xx, ff, tz="GMT")))
                    return(strptime(x, ff))
            ## no success :
            if(optional)
                as.Date.character(rep.int(NA_character_, length(x)), "%Y-%m-%d")
            else stop("character string is not in a standard unambiguous format")
        }
    }
    res <- if(missing(format)) charToDate(x) else strptime(x, format, tz="GMT")
    as.Date(res)
}

as.Date.numeric <- function(x, origin, ...)
{
    if(missing(origin)) stop("'origin' must be supplied")
    as.Date(origin, ...) + x
}

as.Date.default <- function(x, ...)
{
    if(inherits(x, "Date"))
	x
    else if(is.logical(x) && all(is.na(x)))
	.Date(as.numeric(x))
    else
	stop(gettextf("do not know how to convert '%s' to class %s",
		      deparse1(substitute(x)),
		      dQuote("Date")),
	     domain = NA)
}

## ## Moved to package date
## as.Date.date <- function(x, ...)
## {
##     if(inherits(x, "date")) {
##         x <- (x - 3653) # origin 1960-01-01
##         return(structure(x, class = "Date"))
##     } else stop(gettextf("'%s' is not a \"date\" object",
##                          deparse1(substitute(x)) ))
## }

## ## Moved to package chron
## as.Date.dates <- function(x, ...)
## {
##     if(inherits(x, "dates")) {
##         z <- attr(x, "origin")
##         x <- trunc(as.numeric(x))
##         if(length(z) == 3L && is.numeric(z))
##             x  <- x + as.numeric(as.Date(paste(z[3L], z[1L], z[2L], sep="/")))
##         return(structure(x, class = "Date"))
##     } else stop(gettextf("'%s' is not a \"dates\" object",
##                          deparse1(substitute(x)) ))
## }

format.Date <- function(x, ...)
{
    xx <- format(as.POSIXlt(x), ...)
    names(xx) <- names(x)
    xx
}

## keep in sync with  print.POSIX?t()  in ./datetime.R
print.Date <- function(x, max = NULL, ...)
{
    if(is.null(max)) max <- getOption("max.print", 9999L)
    if(max < length(x)) {
	print(format(x[seq_len(max)]), max=max+1, ...)
	cat(" [ reached 'max' / getOption(\"max.print\") -- omitted",
	    length(x) - max, 'entries ]\n')
    } else if(length(x))
	print(format(x), max = max, ...)
    else
	cat(class(x)[1L], "of length 0\n")
    invisible(x)
}

summary.Date <- function(object, digits = 12L, ...)
{
    x <- summary.default(unclass(object), digits = digits, ...)
    if(m <- match("NA's", names(x), 0L)) {
        NAs <- as.integer(x[m])
        x <- x[-m]
        attr(x, "NAs") <- NAs
    }
    .Date(x, c("summaryDefault", "table", oldClass(object)))
}

`+.Date` <- function(e1, e2)
{
    ## need to drop "units" attribute here
    coerceTimeUnit <- function(x)
        as.vector(round(switch(attr(x,"units"),
                               secs = x/86400, mins = x/1440, hours = x/24,
                               days = x, weeks = 7*x)))

    if (nargs() == 1L) return(e1)
    # only valid if one of e1 and e2 is a scalar.
    if(inherits(e1, "Date") && inherits(e2, "Date"))
        stop("binary + is not defined for \"Date\" objects")
    if (inherits(e1, "difftime")) e1 <- coerceTimeUnit(e1)
    if (inherits(e2, "difftime")) e2 <- coerceTimeUnit(e2)
    .Date(unclass(e1) + unclass(e2))
}

`-.Date` <- function(e1, e2)
{
    coerceTimeUnit <- function(x)
        as.vector(round(switch(attr(x,"units"),
                               secs = x/86400, mins = x/1440, hours = x/24,
                               days = x, weeks = 7*x)))
    if(!inherits(e1, "Date"))
        stop("can only subtract from \"Date\" objects")
    if (nargs() == 1L) stop("unary - is not defined for \"Date\" objects")
    if(inherits(e2, "Date")) return(difftime(e1, e2, units="days"))
    if (inherits(e2, "difftime")) e2 <- coerceTimeUnit(e2)
    if(!is.null(attr(e2, "class")))
        stop("can only subtract numbers from \"Date\" objects")
    .Date(unclass(as.Date(e1)) - e2)
}

Ops.Date <- function(e1, e2)
{
    if (nargs() == 1L)
        stop(gettextf("unary %s not defined for \"Date\" objects", .Generic),
             domain = NA)
    boolean <- switch(.Generic, "<" =, ">" =, "==" =,
                      "!=" =, "<=" =, ">=" = TRUE,
                      FALSE)
    if (!boolean)
        stop(gettextf("%s not defined for \"Date\" objects", .Generic),
             domain = NA)
    ## allow character args to be coerced to dates
    if (is.character(e1)) e1 <- as.Date(e1)
    if (is.character(e2)) e2 <- as.Date(e2)
    NextMethod(.Generic)
}

Math.Date <- function (x, ...)
    stop(gettextf("%s not defined for \"Date\" objects", .Generic),
         domain = NA)

Summary.Date <- function (..., na.rm)
{
    ok <- switch(.Generic, max = , min = , range = TRUE, FALSE)
    if (!ok) stop(gettextf("%s not defined for \"Date\" objects", .Generic),
                  domain = NA)
    .Date(NextMethod(.Generic), oldClass(list(...)[[1L]]))
}

`[.Date` <- function(x, ..., drop = TRUE)
{
    .Date(NextMethod("["), oldClass(x))
}

`[[.Date` <- function(x, ..., drop = TRUE)
{
    .Date(NextMethod("[["), oldClass(x))
}

`[<-.Date` <- function(x, ..., value)
{
    if(!length(value)) return(x)
    value <- unclass(as.Date(value))
    .Date(NextMethod(.Generic), oldClass(x))
}

`length<-.Date` <- function(x, value)
    .Date(NextMethod(), oldClass(x))

as.character.Date <- function(x, ...) format(x, ...)

as.data.frame.Date <- as.data.frame.vector

as.list.Date <- function(x, ...)
    lapply(unclass(x), .Date, oldClass(x))

c.Date <- function(..., recursive = FALSE)
    .Date(c(unlist(lapply(list(...), unclass))))# recursive=recursive << FIXME?

mean.Date <- function (x, ...)
    .Date(mean(unclass(x), ...))

seq.Date <- function(from, to, by, length.out = NULL, along.with = NULL, ...)
{
    if (missing(from)) stop("'from' must be specified")
    if (!inherits(from, "Date")) stop("'from' must be a \"Date\" object")
        if(length(as.Date(from)) != 1L) stop("'from' must be of length 1")
    if (!missing(to)) {
        if (!inherits(to, "Date")) stop("'to' must be a \"Date\" object")
        if (length(as.Date(to)) != 1L) stop("'to' must be of length 1")
    }
    if (!missing(along.with)) {
        length.out <- length(along.with)
    }  else if (!is.null(length.out)) {
        if (length(length.out) != 1L) stop("'length.out' must be of length 1")
        length.out <- ceiling(length.out)
    }
    status <- c(!missing(to), !missing(by), !is.null(length.out))
    if(sum(status) != 2L)
        stop("exactly two of 'to', 'by' and 'length.out' / 'along.with' must be specified")
    if (missing(by)) {
        from <- unclass(as.Date(from))
        to <- unclass(as.Date(to))
        res <- seq.int(from, to, length.out = length.out)
        return(.Date(res))
    }

    if (length(by) != 1L) stop("'by' must be of length 1")
    valid <- 0L
    if (inherits(by, "difftime")) {
        by <- switch(attr(by,"units"), secs = 1/86400, mins = 1/1440,
                     hours = 1/24, days = 1, weeks = 7) * unclass(by)
    } else if(is.character(by)) {
        by2 <- strsplit(by, " ", fixed = TRUE)[[1L]]
        if(length(by2) > 2L || length(by2) < 1L)
            stop("invalid 'by' string")
        valid <- pmatch(by2[length(by2)],
                        c("days", "weeks", "months", "quarters", "years"))
        if(is.na(valid)) stop("invalid string for 'by'")
        if(valid <= 2L) {
            by <- c(1, 7)[valid]
            if (length(by2) == 2L) by <- by * as.integer(by2[1L])
        } else
            by <- if(length(by2) == 2L) as.integer(by2[1L]) else 1
    } else if(!is.numeric(by)) stop("invalid mode for 'by'")
    if(is.na(by)) stop("'by' is NA")

    if(valid <= 2L) { # days or weeks
        from <- unclass(as.Date(from))
        if(!is.null(length.out))
            res <- seq.int(from, by = by, length.out = length.out)
        else {
            to0 <- unclass(as.Date(to))
            ## defeat test in seq.default
            res <- seq.int(0, to0 - from, by) + from
        }
        res <- .Date(res)
    } else {  # months or quarters or years
        r1 <- as.POSIXlt(from)
        if(valid == 5L) { # years
            if(missing(to)) {
                yr <- seq.int(r1$year, by = by, length.out = length.out)
            } else {
                to0 <- as.POSIXlt(to)
                yr <- seq.int(r1$year, to0$year, by)
            }
            r1$year <- yr
            res <- as.Date(r1)
        } else { # months or quarters
            if (valid == 4L) by <- by * 3
            if(missing(to)) {
                mon <- seq.int(r1$mon, by = by, length.out = length.out)
            } else {
                to0 <- as.POSIXlt(to)
                mon <- seq.int(r1$mon, 12*(to0$year - r1$year) + to0$mon, by)
            }
            r1$mon <- mon
            res <- as.Date(r1)
        }
    }
    ## can overshoot
    if (!missing(to)) {
        to <- as.Date(to)
        res <- if (by > 0) res[res <= to] else res[res >= to]
    }
    res
}

## *very* similar to cut.POSIXt [ ./datetime.R ] -- keep in sync!
cut.Date <-
    function (x, breaks, labels = NULL, start.on.monday = TRUE,
              right = FALSE, ...)
{
    if(!inherits(x, "Date")) stop("'x' must be a date-time object")
    x <- as.Date(x)

    if (inherits(breaks, "Date")) {
	breaks <- sort(as.Date(breaks))
    } else if(is.numeric(breaks) && length(breaks) == 1L) {
	## specified number of breaks
    } else if(is.character(breaks) && length(breaks) == 1L) {
	by2 <- strsplit(breaks, " ", fixed = TRUE)[[1L]]
	if(length(by2) > 2L || length(by2) < 1L)
	    stop("invalid specification of 'breaks'")
	valid <-
	    pmatch(by2[length(by2)],
		   c("days", "weeks", "months", "years", "quarters"))
	if(is.na(valid)) stop("invalid specification of 'breaks'")
	start <- as.POSIXlt(min(x, na.rm=TRUE))
	if(valid == 1L) incr <- 1L
	if(valid == 2L) {		# weeks
	    start$mday <- start$mday - start$wday
	    if(start.on.monday)
		start$mday <- start$mday + ifelse(start$wday > 0L, 1L, -6L)
            start$isdst <- -1L
	    incr <- 7L
	}
	if(valid == 3L) {		# months
	    start$mday <- 1L
            start$isdst <- -1L
	    end <- as.POSIXlt(max(x, na.rm = TRUE))
	    step <- if(length(by2) == 2L) as.integer(by2[1L]) else 1L
	    end <- as.POSIXlt(end + (31 * step * 86400))
	    end$mday <- 1L
            end$isdst <- -1L
	    breaks <- as.Date(seq(start, end, breaks))
	} else if(valid == 4L) {	# years
	    start$mon <- 0L
	    start$mday <- 1L
            start$isdst <- -1L
	    end <- as.POSIXlt(max(x, na.rm = TRUE))
	    step <- if(length(by2) == 2L) as.integer(by2[1L]) else 1L
	    end <- as.POSIXlt(end + (366 * step * 86400))
	    end$mon <- 0L
	    end$mday <- 1L
            end$isdst <- -1L
	    breaks <- as.Date(seq(start, end, breaks))
	} else if(valid == 5L) {	# quarters
	    qtr <- rep(c(0L, 3L, 6L, 9L), each = 3L)
	    start$mon <- qtr[start$mon + 1L]
	    start$mday <- 1L
            start$isdst <- -1L
	    maxx <- max(x, na.rm = TRUE)
	    end <- as.POSIXlt(maxx)
	    step <- if(length(by2) == 2L) as.integer(by2[1L]) else 1L
	    end <- as.POSIXlt(end + (93 * step * 86400))
	    end$mon <- qtr[end$mon + 1L]
	    end$mday <- 1L
            end$isdst <- -1L
	    breaks <- as.Date(seq(start, end, paste(step * 3L, "months")))
	    ## 93 days ahead could give an empty level, so
	    lb <- length(breaks)
	    if(maxx < breaks[lb-1]) breaks <- breaks[-lb]
	} else {
	    start <- as.Date(start)
	    if (length(by2) == 2L) incr <- incr * as.integer(by2[1L])
	    maxx <- max(x, na.rm = TRUE)
	    breaks <- seq(start, maxx + incr, breaks)
	    breaks <- breaks[seq_len(1L+max(which(breaks <= maxx)))]
	}
    } else stop("invalid specification of 'breaks'")
    res <- cut(unclass(x), unclass(breaks), labels = labels,
	       right = right, ...)
    if(is.null(labels)) {
	levels(res) <-
	    as.character(if (is.numeric(breaks)) x[!duplicated(res)]
			 else breaks[-length(breaks)])
    }
    res
}

julian.Date <- function(x, origin = as.Date("1970-01-01"), ...)
{
    if(length(origin) != 1L) stop("'origin' must be of length one")
    structure(unclass(x) - unclass(origin), "origin" = origin)
}

weekdays.Date <- function(x, abbreviate = FALSE)
    format(x, ifelse(abbreviate, "%a", "%A"))

months.Date <- function(x, abbreviate = FALSE)
    format(x, ifelse(abbreviate, "%b", "%B"))

quarters.Date <- function(x, ...)
{
    x <- (as.POSIXlt(x)$mon) %/% 3L
    paste0("Q", x+1L)
}

## These only make sense for negative digits, but still ...
round.Date <- function(x, ...)
{
    .Date(NextMethod(), oldClass(x))
}

## must avoid truncating forwards dates prior to 1970-01-01.
trunc.Date <- function(x, ...)
    round(x - 0.4999999)

rep.Date <- function(x, ...)
{
    .Date(NextMethod(), oldClass(x))
}

diff.Date <- function (x, lag = 1L, differences = 1L, ...)
{
    ismat <- is.matrix(x)
    xlen <- if (ismat) dim(x)[1L] else length(x)
    if (length(lag) != 1L || length(differences) > 1L || lag < 1L || differences < 1L)
        stop("'lag' and 'differences' must be integers >= 1")
    if (lag * differences >= xlen)
        return(.difftime(numeric(), units="days"))
    r <- x
    i1 <- -seq_len(lag)
    if (ismat)
        for (i in seq_len(differences)) r <- r[i1, , drop = FALSE] -
            r[-nrow(r):-(nrow(r) - lag + 1L), , drop = FALSE]
    else for (i in seq_len(differences))
        r <- r[i1] - r[-length(r):-(length(r) - lag + 1L)]
    r
}

## ---- additions in 2.6.0 -----

is.numeric.Date <- function(x) FALSE

## ---- additions in 2.8.0 -----

split.Date <- function(x, f, drop = FALSE, ...)
{
    lapply(split.default(unclass(x), f, drop = drop, ...),
           .Date, oldClass(x))
}

xtfrm.Date <- function(x) as.numeric(x)

## Added in 3.5.0.

.Date <- function(xx, cl = "Date") `class<-`(xx, cl)

