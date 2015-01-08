#  File src/library/base/R/datetime.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2015 The R Core Team
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

Sys.time <- function() .POSIXct(.Internal(Sys.time()))

## overridden on Windows
Sys.timezone <- function(location = TRUE)
{
    tz <- Sys.getenv("TZ", names = FALSE)
    if(!location || nzchar(tz)) return(Sys.getenv("TZ", unset = NA_character_))
    lt <- normalizePath("/etc/localtime") # Linux, OS X, ...
    if (grepl(pat <- "^/usr/share/zoneinfo/", lt)) sub(pat, "", lt)
    else NA_character_
}

as.POSIXlt <- function(x, tz = "", ...) UseMethod("as.POSIXlt")

as.POSIXlt.Date <- function(x, ...) .Internal(Date2POSIXlt(x))

as.POSIXlt.date <- as.POSIXlt.dates <- function(x, ...)
    as.POSIXlt(as.POSIXct(x), ...)

as.POSIXlt.POSIXct <- function(x, tz = "", ...)
{
    if((missing(tz) || is.null(tz)) &&
       !is.null(tzone <- attr(x, "tzone"))) tz <- tzone[1L]
    .Internal(as.POSIXlt(x, tz))
}

as.POSIXlt.factor <- function(x, ...)
{
    y <- as.POSIXlt(as.character(x), ...)
    names(y$year) <- names(x)
    y
}

as.POSIXlt.character <- function(x, tz = "", format, ...)
{
    x <- unclass(x) # precaution PR7826
    if(!missing(format)) {
        res <- strptime(x, format, tz = tz)
        if(nzchar(tz)) attr(res, "tzone") <- tz
        return(res)
    }
    xx <- x[!is.na(x)]
    if (!length(xx)) {
        res <- strptime(x, "%Y/%m/%d")
        if(nzchar(tz)) attr(res, "tzone") <- tz
        return(res)
    } else if(all(!is.na(strptime(xx, f <- "%Y-%m-%d %H:%M:%OS", tz = tz))) ||
            all(!is.na(strptime(xx, f <- "%Y/%m/%d %H:%M:%OS", tz = tz))) ||
            all(!is.na(strptime(xx, f <- "%Y-%m-%d %H:%M", tz = tz))) ||
            all(!is.na(strptime(xx, f <- "%Y/%m/%d %H:%M", tz = tz))) ||
            all(!is.na(strptime(xx, f <- "%Y-%m-%d", tz = tz))) ||
            all(!is.na(strptime(xx, f <- "%Y/%m/%d", tz = tz)))
            ) {
        res <- strptime(x, f, tz = tz)
        if(nzchar(tz)) attr(res, "tzone") <- tz
        return(res)
    }
    stop("character string is not in a standard unambiguous format")
}

as.POSIXlt.numeric <- function(x, tz = "", origin, ...)
{
    if(missing(origin)) stop("'origin' must be supplied")
    as.POSIXlt(as.POSIXct(origin, tz = "UTC", ...) + x, tz = tz)
}

as.POSIXlt.default <- function(x, tz = "", ...)
{

    if(inherits(x, "POSIXlt")) return(x)
    if(is.logical(x) && all(is.na(x)))
        return(as.POSIXlt(as.POSIXct.default(x), tz = tz))
    stop(gettextf("do not know how to convert '%s' to class %s",
                  deparse(substitute(x)),
                  dQuote("POSIXlt")),
         domain = NA)
}

as.POSIXct <- function(x, tz = "", ...) UseMethod("as.POSIXct")

as.POSIXct.Date <- function(x, ...) .POSIXct(unclass(x)*86400)


## convert from package date
as.POSIXct.date <- function(x, ...)
{
    if(inherits(x, "date")) {
        x <- (x - 3653) * 86400 # origin 1960-01-01
        return(.POSIXct(x))
    } else stop(gettextf("'%s' is not a \"date\" object",
                         deparse(substitute(x)) ))
}

## convert from package chron
as.POSIXct.dates <- function(x, ...)
{
    if(inherits(x, "dates")) {
        z <- attr(x, "origin")
        x <- as.numeric(x) * 86400
        if(length(z) == 3L && is.numeric(z))
            x  <- x + as.numeric(ISOdate(z[3L], z[1L], z[2L], 0))
        return(.POSIXct(x))
    } else stop(gettextf("'%s' is not a \"dates\" object",
                         deparse(substitute(x)) ))
}

as.POSIXct.POSIXlt <- function(x, tz = "", ...)
{
    tzone <- attr(x, "tzone")
    if(missing(tz) && !is.null(tzone)) tz <- tzone[1L]
    ## <FIXME>
    ## Move names handling to C code eventually ...
    y <- .Internal(as.POSIXct(x, tz))
    names(y) <- names(x$year)
    .POSIXct(y, tz)
    ## </FIXME>
}

as.POSIXct.numeric <- function(x, tz = "", origin, ...)
{
    if(missing(origin)) stop("'origin' must be supplied")
    .POSIXct(as.POSIXct(origin, tz = "GMT", ...) + x, tz)
}

as.POSIXct.default <- function(x, tz = "", ...)
{
    if(inherits(x, "POSIXct")) return(x)
    if(is.character(x) || is.factor(x))
	return(as.POSIXct(as.POSIXlt(x, tz, ...), tz, ...))
    if(is.logical(x) && all(is.na(x)))
        return(.POSIXct(as.numeric(x)))
    stop(gettextf("do not know how to convert '%s' to class %s",
                  deparse(substitute(x)),
                  dQuote("POSIXct")),
         domain = NA)
}

as.double.POSIXlt <- function(x, ...) as.double(as.POSIXct(x))

## POSIXlt is not primarily a list, but primarily an abstract vector of
## time stamps:
length.POSIXlt <- function(x) length(x[[1L]])

format.POSIXlt <- function(x, format = "", usetz = FALSE, ...)
{
    if(!inherits(x, "POSIXlt")) stop("wrong class")
    if(format == "") {
        ## need list [ method here.
        times <- unlist(unclass(x)[1L:3L])
        secs <- x$sec; secs <- secs[!is.na(secs)]
        np <- getOption("digits.secs")
        if(is.null(np)) np <- 0L else np <- min(6L, np)
        if(np >= 1L)
            for (i in seq_len(np)- 1L)
                if(all( abs(secs - round(secs, i)) < 1e-6 )) {
                    np <- i
                    break
                }
        format <- if(all(times[!is.na(times)] == 0)) "%Y-%m-%d"
        else if(np == 0L) "%Y-%m-%d %H:%M:%S"
        else paste0("%Y-%m-%d %H:%M:%OS", np)
    }
    ## <FIXME>
    ## Move names handling to C code eventually ...
    y <- .Internal(format.POSIXlt(x, format, usetz))
    names(y) <- names(x$year)
    y
    ## </FIXME>
}

## prior to 2.9.0 the same as format.POSIXlt.
## now more or less the same as format.POSIXct but also works for Dates.
strftime <- function(x, format = "", tz = "", usetz = FALSE, ...)
    format(as.POSIXlt(x, tz = tz), format = format, usetz = usetz, ...)

strptime <- function(x, format, tz = "")
{
    ## <FIXME>
    ## Move names handling to C code eventually ...
    y <- .Internal(strptime(as.character(x), format, tz))
    ## Assuming we can rely on the names of x ...
    names(y$year) <- names(x)
    y
    ## </FIXME>
}

format.POSIXct <- function(x, format = "", tz = "", usetz = FALSE, ...)
{
    if(!inherits(x, "POSIXct")) stop("wrong class")
    if(missing(tz) && !is.null(tzone <- attr(x, "tzone"))) tz <- tzone
    structure(format.POSIXlt(as.POSIXlt(x, tz), format, usetz, ...),
              names = names(x))
}

## could handle arrays for max.print
print.POSIXct <- function(x, ...)
{
    max.print <- getOption("max.print", 9999L)
    if(max.print < length(x)) {
        print(format(x[seq_len(max.print)], usetz = TRUE), ...)
        cat(' [ reached getOption("max.print") -- omitted',
            length(x) - max.print, 'entries ]\n')
    } else print(format(x, usetz = TRUE), ...)
    invisible(x)
}

print.POSIXlt <- function(x, ...)
{
    max.print <- getOption("max.print", 9999L)
    if(max.print < length(x)) {
        print(format(x[seq_len(max.print)], usetz = TRUE), ...)
        cat(' [ reached getOption("max.print") -- omitted',
            length(x) - max.print, 'entries ]\n')
   } else print(format(x, usetz = TRUE), ...)
    invisible(x)
}

summary.POSIXct <- function(object, digits = 15L, ...)
{
    x <- summary.default(unclass(object), digits = digits, ...)
    if(m <- match("NA's", names(x), 0)) {
        NAs <- as.integer(x[m])
        x <- x[-m]
        attr(x, "NAs") <- NAs
    }
    class(x) <- c("summaryDefault", "table", oldClass(object))
    attr(x, "tzone") <- attr(object, "tzone")
    x
}

summary.POSIXlt <- function(object, digits = 15, ...)
    summary(as.POSIXct(object), digits = digits, ...)


`+.POSIXt` <- function(e1, e2)
{
    ## need to drop "units" attribute here
    coerceTimeUnit <- function(x)
        as.vector(switch(attr(x,"units"),
                         secs = x, mins = 60*x, hours = 60*60*x,
                         days = 60*60*24*x, weeks = 60*60*24*7*x))

    if (nargs() == 1) return(e1)
    # only valid if one of e1 and e2 is a scalar/difftime
    if(inherits(e1, "POSIXt") && inherits(e2, "POSIXt"))
        stop("binary '+' is not defined for \"POSIXt\" objects")
    if(inherits(e1, "POSIXlt")) e1 <- as.POSIXct(e1)
    if(inherits(e2, "POSIXlt")) e2 <- as.POSIXct(e2)
    if (inherits(e1, "difftime")) e1 <- coerceTimeUnit(e1)
    if (inherits(e2, "difftime")) e2 <- coerceTimeUnit(e2)
    .POSIXct(unclass(e1) + unclass(e2), check_tzones(e1, e2))
}

`-.POSIXt` <- function(e1, e2)
{
    ## need to drop "units" attribute here
    coerceTimeUnit <- function(x)
        as.vector(switch(attr(x,"units"),
                         secs = x, mins = 60*x, hours = 60*60*x,
                         days = 60*60*24*x, weeks = 60*60*24*7*x))
    if(!inherits(e1, "POSIXt"))
        stop("can only subtract from \"POSIXt\" objects")
    if (nargs() == 1) stop("unary '-' is not defined for \"POSIXt\" objects")
    if(inherits(e2, "POSIXt")) return(difftime(e1, e2))
    if (inherits(e2, "difftime")) e2 <- coerceTimeUnit(e2)
    if(!is.null(attr(e2, "class")))
        stop("can only subtract numbers from \"POSIXt\" objects")
    e1 <- as.POSIXct(e1)
    .POSIXct(unclass(e1) - e2, attr(e1, "tzone"))
}

Ops.POSIXt <- function(e1, e2)
{
    if (nargs() == 1)
        stop(gettextf("unary '%s' not defined for \"POSIXt\" objects",
                      .Generic), domain = NA)
    boolean <- switch(.Generic, "<" = , ">" = , "==" = ,
                      "!=" = , "<=" = , ">=" = TRUE, FALSE)
    if (!boolean)
        stop(gettextf("'%s' not defined for \"POSIXt\" objects", .Generic),
             domain = NA)
    if(inherits(e1, "POSIXlt") || is.character(e1)) e1 <- as.POSIXct(e1)
    if(inherits(e2, "POSIXlt") || is.character(e2)) e2 <- as.POSIXct(e2)
    check_tzones(e1, e2)
    NextMethod(.Generic)
}

Math.POSIXt <- function (x, ...)
{
    stop(gettextf("'%s' not defined for \"POSIXt\" objects", .Generic),
         domain = NA)
}

check_tzones <- function(...)
{
    tzs <- unique(sapply(list(...), function(x) {
        y <- attr(x, "tzone")
        if(is.null(y)) "" else y[1L]
    }))
    tzs <- tzs[tzs != ""]
    if(length(tzs) > 1L)
        warning("'tzone' attributes are inconsistent")
    if(length(tzs)) tzs[1L] else NULL
}

Summary.POSIXct <- function (..., na.rm)
{
    ok <- switch(.Generic, max = , min = , range = TRUE, FALSE)
    if (!ok)
        stop(gettextf("'%s' not defined for \"POSIXt\" objects", .Generic),
             domain = NA)
    args <- list(...)
    tz <- do.call("check_tzones", args)
    val <- NextMethod(.Generic)
    class(val) <- oldClass(args[[1L]])
    attr(val, "tzone") <- tz
    val
}

Summary.POSIXlt <- function (..., na.rm)
{
    ok <- switch(.Generic, max = , min = , range = TRUE, FALSE)
    if (!ok)
        stop(gettextf("'%s' not defined for \"POSIXt\" objects", .Generic),
             domain = NA)
    args <- list(...)
    tz <- do.call("check_tzones", args)
    args <- lapply(args, as.POSIXct)
    val <- do.call(.Generic, c(args, na.rm = na.rm))
    as.POSIXlt(.POSIXct(val, tz))
}

`[.POSIXct` <-
function(x, ..., drop = TRUE)
{
    cl <- oldClass(x)
    ## class(x) <- NULL
    val <- NextMethod("[")
    class(val) <- cl
    attr(val, "tzone") <- attr(x, "tzone")
    val
}

`[[.POSIXct` <-
function(x, ..., drop = TRUE)
{
    cl <- oldClass(x)
    ## class(x) <- NULL
    val <- NextMethod("[[")
    class(val) <- cl
    attr(val, "tzone") <- attr(x, "tzone")
    val
}

`[<-.POSIXct` <-
function(x, ..., value) {
    if(!length(value)) return(x)
    value <- unclass(as.POSIXct(value))
    cl <- oldClass(x)
    tz <- attr(x, "tzone")
    class(x) <- NULL
    x <- NextMethod(.Generic)
    class(x) <- cl
    attr(x, "tzone") <- tz
    x
}

as.character.POSIXt <- function(x, ...) format(x, ...)

as.data.frame.POSIXct <- as.data.frame.vector

as.list.POSIXct <- function(x, ...)
{
    nms <- names(x)
    names(x) <- NULL
    y <- lapply(seq_along(x), function(i) x[i])
    names(y) <- nms
    y
}

is.na.POSIXlt <- function(x) is.na(as.POSIXct(x))
anyNA.POSIXlt <- function(x, recursive = FALSE) anyNA(as.POSIXct(x))

## <FIXME> check the argument validity
## This is documented to remove the timezone
c.POSIXct <- function(..., recursive = FALSE)
    .POSIXct(c(unlist(lapply(list(...), unclass))))

## we need conversion to POSIXct as POSIXlt objects can be in different tz.
c.POSIXlt <- function(..., recursive = FALSE)
    as.POSIXlt(do.call("c", lapply(list(...), as.POSIXct)))



ISOdatetime <- function(year, month, day, hour, min, sec, tz = "")
{
    if(min(vapply(list(year, month, day, hour, min, sec), length, 1, USE.NAMES=FALSE)) == 0L)
        .POSIXct(numeric(), tz = tz)
    else {
        x <- paste(year, month, day, hour, min, sec, sep = "-")
        as.POSIXct(strptime(x, "%Y-%m-%d-%H-%M-%OS", tz = tz), tz = tz)
    }
}

ISOdate <- function(year, month, day, hour = 12, min = 0, sec = 0, tz = "GMT")
    ISOdatetime(year, month, day, hour, min, sec, tz)

as.matrix.POSIXlt <- function(x, ...)
{
    as.matrix(as.data.frame(unclass(x)), ...)
}

mean.POSIXct <- function (x, ...)
    .POSIXct(mean(unclass(x), ...), attr(x, "tzone"))

mean.POSIXlt <- function (x, ...)
    as.POSIXlt(mean(as.POSIXct(x), ...))

## ----- difftime -----

difftime <-
    function(time1, time2, tz,
             units = c("auto", "secs", "mins", "hours", "days", "weeks"))
{
    if (missing(tz)) {
        time1 <- as.POSIXct(time1)
        time2 <- as.POSIXct(time2)
    } else {
        ## Wishlist PR#14182
        time1 <- as.POSIXct(time1, tz = tz)
        time2 <- as.POSIXct(time2, tz = tz)
    }
    z <- unclass(time1) - unclass(time2)
    attr(z, "tzone") <- NULL # it may get copied from args of `-`
    units <- match.arg(units)
    if(units == "auto") {
        if(all(is.na(z))) units <- "secs"
        else {
            zz <- min(abs(z),na.rm = TRUE)
            if(is.na(zz) || zz < 60) units <- "secs"
            else if(zz < 3600) units <- "mins"
            else if(zz < 86400) units <- "hours"
            else units <- "days"
        }
    }
    switch(units,
           "secs" = .difftime(z, units = "secs"),
           "mins" = .difftime(z/60, units = "mins"),
           "hours" = .difftime(z/3600, units = "hours"),
           "days" = .difftime(z/86400, units = "days"),
           "weeks" = .difftime(z/(7*86400), units = "weeks")
           )
}

## "difftime" constructor
## Martin Maechler, Date: 16 Sep 2002
## Numeric input version Peter Dalgaard, December 2006
as.difftime <- function(tim, format = "%X", units = "auto")
{
    if (inherits(tim, "difftime")) return(tim)
    if (is.character(tim)){
        difftime(strptime(tim, format = format),
             strptime("0:0:0", format = "%X"), units = units)
    } else {
        if (!is.numeric(tim)) stop("'tim' is not character or numeric")
	if (units == "auto") stop("need explicit units for numeric conversion")
        if (!(units %in% c("secs", "mins", "hours", "days", "weeks")))
	    stop("invalid units specified")
        structure(tim, units = units, class = "difftime")
    }
}

### For now, these have only difftime methods, but you never know...
units <- function(x) UseMethod("units")

`units<-` <- function(x, value) UseMethod("units<-")

units.difftime <- function(x) attr(x, "units")

`units<-.difftime` <- function(x, value)
{
    from <- units(x)
    if (from == value) return(x)
    if (!(value %in% c("secs", "mins", "hours", "days", "weeks")))
        stop("invalid units specified")
    sc <- cumprod(c(secs = 1, mins = 60, hours = 60, days = 24, weeks = 7))
    newx <- unclass(x) * as.vector(sc[from]/sc[value])
    .difftime(newx, value)
}

as.double.difftime <- function(x, units = "auto", ...)
{
    if (units != "auto") units(x) <- units
    as.vector(x, "double")
}

as.data.frame.difftime <- as.data.frame.vector

format.difftime <- function(x,...) paste(format(unclass(x),...), units(x))



print.difftime <- function(x, digits = getOption("digits"), ...)
{
    if(is.array(x) || length(x) > 1L) {
        cat("Time differences in ", attr(x, "units"), "\n", sep = "")
        y <- unclass(x); attr(y, "units") <- NULL
        print(y)
    }
    else
        cat("Time difference of ", format(unclass(x), digits = digits), " ",
            attr(x, "units"), "\n", sep = "")

    invisible(x)
}

`[.difftime` <- function(x, ..., drop = TRUE)
{
    cl <- oldClass(x)
    class(x) <- NULL
    val <- NextMethod("[")
    class(val) <- cl
    attr(val, "units") <- attr(x, "units")
    val
}

Ops.difftime <- function(e1, e2)
{
    coerceTimeUnit <- function(x)
    {
        switch(attr(x, "units"),
               secs = x, mins = 60*x, hours = 60*60*x,
               days = 60*60*24*x, weeks = 60*60*24*7*x)
    }
    if (nargs() == 1) {
        switch(.Generic, "+" = {}, "-" = {e1[] <- -unclass(e1)},
               stop(gettextf("unary '%s' not defined for \"difftime\" objects",
                             .Generic), domain = NA, call. = FALSE)
               )
        return(e1)
    }
    boolean <- switch(.Generic, "<" = , ">" = , "==" = ,
                      "!=" = , "<=" = , ">=" = TRUE, FALSE)
    if (boolean) {
        ## assume user knows what he/she is doing if not both difftime
        if(inherits(e1, "difftime") && inherits(e2, "difftime")) {
            e1 <- coerceTimeUnit(e1)
            e2 <- coerceTimeUnit(e2)
        }
        NextMethod(.Generic)
    } else if(.Generic == "+" || .Generic == "-") {
        if(inherits(e1, "difftime") && !inherits(e2, "difftime"))
            return(structure(NextMethod(.Generic),
                             units = attr(e1, "units"), class = "difftime"))
        if(!inherits(e1, "difftime") && inherits(e2, "difftime"))
            return(structure(NextMethod(.Generic),
                             units = attr(e2, "units"), class = "difftime"))
        u1 <- attr(e1, "units")
        if(attr(e2, "units") == u1) {
            structure(NextMethod(.Generic), units=u1, class = "difftime")
        } else {
            e1 <- coerceTimeUnit(e1)
            e2 <- coerceTimeUnit(e2)
            structure(NextMethod(.Generic), units = "secs", class = "difftime")
        }
    } else {
        ## '*' is covered by a specific method
        stop(gettextf("'%s' not defined for \"difftime\" objects", .Generic),
             domain = NA)
    }
}

`*.difftime` <- function (e1, e2)
{
    ## need one scalar, one difftime.
    if(inherits(e1, "difftime") && inherits(e2, "difftime"))
        stop("both arguments of * cannot be \"difftime\" objects")
    if(inherits(e2, "difftime")) {tmp <- e1; e1 <- e2; e2 <- tmp}
    .difftime(e2 * unclass(e1), attr(e1, "units"))
}

`/.difftime` <- function (e1, e2)
{
    ## need one scalar, one difftime.
    if(inherits(e2, "difftime"))
        stop("second argument of / cannot be a \"difftime\" object")
    .difftime(unclass(e1) / e2, attr(e1, "units"))
}

## "Math": some methods should work; the other ones are meaningless :
Math.difftime <- function (x, ...)
{
    switch(.Generic,
           "abs" =, "sign" =, "floor" =, "ceiling" =, "trunc" =,
           "round" =, "signif" = {
               units <- attr(x, "units")
               .difftime(NextMethod(), units)
           },
           ### otherwise :
           stop(gettextf("'%s' not defined for \"difftime\" objects", .Generic),
                domain = NA))
}


mean.difftime <- function (x, ...)
    .difftime(mean(unclass(x), ...), attr(x, "units"))

Summary.difftime <- function (..., na.rm)
{
    ## FIXME: this could return in the smallest of the units of the inputs.
    coerceTimeUnit <- function(x)
    {
        as.vector(switch(attr(x,"units"),
                         secs = x, mins = 60*x, hours = 60*60*x,
                         days = 60*60*24*x, weeks = 60*60*24*7*x))
    }
    ok <- switch(.Generic, max = , min = , sum=, range = TRUE, FALSE)
    if (!ok)
        stop(gettextf("'%s' not defined for \"difftime\" objects", .Generic),
             domain = NA)
    x <- list(...)
    Nargs <- length(x)
    if(Nargs == 0) {
        .difftime(do.call(.Generic), "secs")
    } else {
        units <- sapply(x, function(x) attr(x, "units"))
        if(all(units == units[1L])) {
            args <- c(lapply(x, as.vector), na.rm = na.rm)
        } else {
            args <- c(lapply(x, coerceTimeUnit), na.rm = na.rm)
            units <- "secs"
        }
        .difftime(do.call(.Generic, args), units[[1L]])
    }
}


## ----- convenience functions -----

seq.POSIXt <-
    function(from, to, by, length.out = NULL, along.with = NULL, ...)
{
    if (missing(from)) stop("'from' must be specified")
    if (!inherits(from, "POSIXt")) stop("'from' must be a \"POSIXt\" object")
    cfrom <- as.POSIXct(from)
    if(length(cfrom) != 1L) stop("'from' must be of length 1")
    tz <- attr(cfrom , "tzone")
    if (!missing(to)) {
        if (!inherits(to, "POSIXt")) stop("'to' must be a \"POSIXt\" object")
        if (length(as.POSIXct(to)) != 1) stop("'to' must be of length 1")
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
        from <- unclass(cfrom)
        to <- unclass(as.POSIXct(to))
        ## Till (and incl.) 1.6.0 :
        ##- incr <- (to - from)/length.out
        ##- res <- seq.default(from, to, incr)
        res <- seq.int(from, to, length.out = length.out)
        return(.POSIXct(res, tz))
    }

    if (length(by) != 1L) stop("'by' must be of length 1")
    valid <- 0L
    if (inherits(by, "difftime")) {
        by <- switch(attr(by,"units"), secs = 1, mins = 60, hours = 3600,
                     days = 86400, weeks = 7*86400) * unclass(by)
    } else if(is.character(by)) {
        by2 <- strsplit(by, " ", fixed = TRUE)[[1L]]
        if(length(by2) > 2L || length(by2) < 1L)
            stop("invalid 'by' string")
        valid <- pmatch(by2[length(by2)],
                        c("secs", "mins", "hours", "days", "weeks",
                          "months", "years", "DSTdays", "quarters"))
        if(is.na(valid)) stop("invalid string for 'by'")
        if(valid <= 5L) {
            by <- c(1, 60, 3600, 86400, 7*86400)[valid]
            if (length(by2) == 2L) by <- by * as.integer(by2[1L])
        } else
            by <- if(length(by2) == 2L) as.integer(by2[1L]) else 1
    } else if(!is.numeric(by)) stop("invalid mode for 'by'")
    if(is.na(by)) stop("'by' is NA")

    if(valid <= 5L) { # secs, mins, hours, days, weeks
        from <- unclass(as.POSIXct(from))
        if(!is.null(length.out))
            res <- seq.int(from, by = by, length.out = length.out)
        else {
            to0 <- unclass(as.POSIXct(to))
            ## defeat test in seq.default
            res <- seq.int(0, to0 - from, by) + from
        }
        return(.POSIXct(res, tz))
    } else {  # months or years or DSTdays or quarters
        r1 <- as.POSIXlt(from)
        if(valid == 7L) { # years
            if(missing(to)) { # years
                yr <- seq.int(r1$year, by = by, length.out = length.out)
            } else {
                to <- as.POSIXlt(to)
                yr <- seq.int(r1$year, to$year, by)
            }
            r1$year <- yr
        } else if(valid %in% c(6L, 9L)) { # months or quarters
            if (valid == 9L) by <- by * 3
            if(missing(to)) {
                mon <- seq.int(r1$mon, by = by, length.out = length.out)
            } else {
                to0 <- as.POSIXlt(to)
                mon <- seq.int(r1$mon, 12*(to0$year - r1$year) + to0$mon, by)
            }
            r1$mon <- mon
        } else if(valid == 8L) { # DSTdays
            if(!missing(to)) {
                ## We might have a short day, so need to over-estimate.
                length.out <- 2L + floor((unclass(as.POSIXct(to)) -
                                          unclass(as.POSIXct(from)))/86400)
            }
            r1$mday <- seq.int(r1$mday, by = by, length.out = length.out)
        }
	r1$isdst <- -1L
	res <- as.POSIXct(r1)
	## now shorten if necessary.
	if(!missing(to)) {
	    to <- as.POSIXct(to)
	    res <- if(by > 0) res[res <= to] else res[res >= to]
	}
	res
    }
}

## *very* similar to cut.Date [ ./dates.R ] -- keep in sync!
cut.POSIXt <-
    function (x, breaks, labels = NULL, start.on.monday = TRUE,
              right = FALSE, ...)
{
    if(!inherits(x, "POSIXt")) stop("'x' must be a date-time object")
    x <- as.POSIXct(x)

    if (inherits(breaks, "POSIXt")) {
	breaks <- sort(as.POSIXct(breaks))
    } else if(is.numeric(breaks) && length(breaks) == 1L) {
	## specified number of breaks
    } else if(is.character(breaks) && length(breaks) == 1L) {
        by2 <- strsplit(breaks, " ", fixed = TRUE)[[1L]]
        if(length(by2) > 2L || length(by2) < 1L)
            stop("invalid specification of 'breaks'")
	valid <-
	    pmatch(by2[length(by2)],
		   c("secs", "mins", "hours", "days", "weeks",
		     "months", "years", "DSTdays", "quarters"))
	if(is.na(valid)) stop("invalid specification of 'breaks'")
	start <- as.POSIXlt(min(x, na.rm = TRUE))
	incr <- 1
	if(valid > 1L) { start$sec <- 0L; incr <- 60 }
	if(valid > 2L) { start$min <- 0L; incr <- 3600 }
        ## start of day need not be on the same DST, PR#14208
	if(valid > 3L) { start$hour <- 0L; start$isdst <- -1L; incr <- 86400 }
	if(valid == 5L) {               # weeks
	    start$mday <- start$mday - start$wday
	    if(start.on.monday)
		start$mday <- start$mday + ifelse(start$wday > 0L, 1L, -6L)
	    incr <- 7*86400
	}
        if(valid == 8L) incr <- 25*3600 # DSTdays
        if(valid == 6L) {               # months
            start$mday <- 1L
            end <- as.POSIXlt(max(x, na.rm = TRUE))
            step <- ifelse(length(by2) == 2L, as.integer(by2[1L]), 1L)
            end <- as.POSIXlt(end + (31 * step * 86400))
            end$mday <- 1L
            end$isdst <- -1L
            breaks <- seq(start, end, breaks)
        } else if(valid == 7L) {        # years
            start$mon <- 0L
            start$mday <- 1L
            end <- as.POSIXlt(max(x, na.rm = TRUE))
            step <- ifelse(length(by2) == 2L, as.integer(by2[1L]), 1L)
            end <- as.POSIXlt(end + (366 * step* 86400))
            end$mon <- 0L
            end$mday <- 1L
            end$isdst <- -1L
            breaks <- seq(start, end, breaks)
        } else if(valid == 9L) {        # quarters
            qtr <- rep(c(0L, 3L, 6L, 9L), each = 3L)
            start$mon <- qtr[start$mon + 1L]
            start$mday <- 1L
            maxx <- max(x, na.rm = TRUE)
            end <- as.POSIXlt(maxx)
            step <- ifelse(length(by2) == 2L, as.integer(by2[1L]), 1L)
            end <- as.POSIXlt(end + (93 * step * 86400))
            end$mon <- qtr[end$mon + 1L]
            end$mday <- 1L
            end$isdst <- -1L
            breaks <- seq(start, end, paste(step * 3, "months"))
            ## 93 days ahead could give an empty level, so
            lb <- length(breaks)
            if(maxx < breaks[lb-1]) breaks <- breaks[-lb]
        } else {                        # weeks or shorter
            if (length(by2) == 2L) incr <- incr * as.integer(by2[1L])
            maxx <- max(x, na.rm = TRUE)
            breaks <- seq(start, maxx + incr, breaks)
            breaks <- breaks[seq_len(1+max(which(breaks <= maxx)))]
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

julian <- function(x, ...) UseMethod("julian")

julian.POSIXt <- function(x, origin = as.POSIXct("1970-01-01", tz = "GMT"), ...)
{
    origin <- as.POSIXct(origin)
    if(length(origin) != 1L) stop("'origin' must be of length one")
    res <- difftime(as.POSIXct(x), origin, units = "days")
    structure(res, "origin" = origin)
}

weekdays <- function(x, abbreviate) UseMethod("weekdays")
weekdays.POSIXt <- function(x, abbreviate = FALSE)
{
    format(x, ifelse(abbreviate, "%a", "%A"))
}

months <- function(x, abbreviate) UseMethod("months")
months.POSIXt <- function(x, abbreviate = FALSE)
{
    format(x, ifelse(abbreviate, "%b", "%B"))
}

quarters <- function(x, abbreviate) UseMethod("quarters")
quarters.POSIXt <- function(x, ...)
{
    x <- (as.POSIXlt(x)$mon)%/%3
    paste0("Q", x+1)
}

trunc.POSIXt <- function(x, units = c("secs", "mins", "hours", "days"), ...)
{
    units <- match.arg(units)
    x <- as.POSIXlt(x)
    if(length(x$sec))
	switch(units,
	       "secs" = {x$sec <- trunc(x$sec)},
	       "mins" = {x$sec[] <- 0},
	       "hours" = {x$sec[] <- 0; x$min[] <- 0L},
               ## start of day need not be on the same DST.
	       "days" = {x$sec[] <- 0; x$min[] <- 0L; x$hour[] <- 0L; x$isdst[] <- -1L}
	       )
    x
}

round.POSIXt <- function(x, units = c("secs", "mins", "hours", "days"))
{
    ## this gets the default from the generic, as that has two args.
    if(is.numeric(units) && units == 0.0) units <-"secs"
    units <- match.arg(units)
    x <- as.POSIXct(x)
    x <- x + switch(units,
                    "secs" = 0.5, "mins" = 30, "hours" = 1800, "days" = 43200)
    trunc.POSIXt(x, units = units)
}

## ---- additions in 1.5.0 -----

`[.POSIXlt` <- function(x, ..., drop = TRUE)
{
    val <- lapply(X = x, FUN = "[", ..., drop = drop)
    attributes(val) <- attributes(x) # need to preserve timezones
    val
}

`[<-.POSIXlt` <- function(x, i, value)
{
    if(!length(value)) return(x)
    value <- unclass(as.POSIXlt(value))
    cl <- oldClass(x)
    class(x) <- NULL
    for(n in names(x)) x[[n]][i] <- value[[n]]
    class(x) <- cl
    x
}

as.data.frame.POSIXlt <- function(x, row.names = NULL, optional = FALSE, ...)
{
    value <- as.data.frame.POSIXct(as.POSIXct(x), row.names, optional, ...)
    if (!optional)
        names(value) <- deparse(substitute(x))[[1L]]
    value
}

## ---- additions in 1.8.0 -----

rep.POSIXct <- function(x, ...)
{
    y <- NextMethod()
    .POSIXct(y, attr(x, "tzone"))
}

rep.POSIXlt <- function(x, ...)
{
    y <- lapply(X = x, FUN = rep, ...)
    attributes(y) <- attributes(x)
    y
}

diff.POSIXt <- function (x, lag = 1L, differences = 1L, ...)
{
    ismat <- is.matrix(x)
    r <- if(inherits(x, "POSIXlt")) as.POSIXct(x) else x
    xlen <- if (ismat) dim(x)[1L] else length(r)
    if (length(lag) != 1L || length(differences) > 1L || lag < 1L || differences < 1L)
        stop("'lag' and 'differences' must be integers >= 1")
    if (lag * differences >= xlen) return(.difftime(numeric(), "secs"))
    i1 <- -seq_len(lag)
    if (ismat) for (i in seq_len(differences)) r <- r[i1, , drop = FALSE] -
            r[-nrow(r):-(nrow(r) - lag + 1), , drop = FALSE]
    else for (i in seq_len(differences))
        r <- r[i1] - r[-length(r):-(length(r) - lag + 1L)]
    r
}

## ---- additions in 2.2.0 -----

duplicated.POSIXlt <- function(x, incomparables = FALSE, ...)
{
    x <- as.POSIXct(x)
    NextMethod("duplicated", x)
}

unique.POSIXlt <- function(x, incomparables = FALSE, ...)
    x[!duplicated(x, incomparables, ...)]

## ---- additions in 2.4.0 -----

sort.POSIXlt <- function(x, decreasing = FALSE, na.last = NA, ...)
    x[order(as.POSIXct(x), na.last = na.last, decreasing = decreasing)]


## ---- additions in 2.6.0 -----

is.numeric.POSIXt <- function(x) FALSE

## ---- additions in 2.8.0 -----

split.POSIXct <-
function(x, f, drop = FALSE, ...)
    lapply(split.default(as.double(x), f, drop = drop), .POSIXct,
           tz = attr(x, "tzone"))

xtfrm.POSIXct <- function(x) as.numeric(x)
xtfrm.POSIXlt <- function(x) as.double(x)  # has POSIXlt method
xtfrm.difftime <- function(x) as.numeric(x)
is.numeric.difftime <- function(x) FALSE


# class generators added in 2.11.0, class order changed in 2.12.0
.POSIXct <- function(xx, tz = NULL)
    structure(xx, class = c("POSIXct", "POSIXt"), tzone = tz)

.POSIXlt <- function(xx, tz = NULL)
    structure(xx, class = c("POSIXlt", "POSIXt"), tzone = tz)

.difftime <- function(xx, units)
    structure(xx, units = units, class = "difftime")

## ---- additions in 2.13.0 -----

names.POSIXlt <-
function(x)
    names(x$year)

`names<-.POSIXlt` <-
function(x, value)
{
    names(x$year) <- value
    x
}

## 3.1.0

OlsonNames <- function()
{
    if(.Platform$OS.type == "windows")
        tzdir <- Sys.getenv("TZDIR", file.path(R.home("share"), "zoneinfo"))
    else {
        tzdirs <- c(Sys.getenv("TZDIR"),
                    file.path(R.home("share"), "zoneinfo"),
                    "/usr/share/zoneinfo", # Linux, OS X, FreeBSD
                    "/usr/share/lib/zoneinfo", # Solaris, AIX
                    "/usr/lib/zoneinfo",   # early glibc
                    "/usr/local/etc/zoneinfo", # tzcode default
                    "/etc/zoneinfo", "/usr/etc/zoneinfo")
        tzdirs <- tzdirs[file.exists(tzdirs)]
        if (!length(tzdirs)) {
            warning("no Olson database found")
            return(character())
        } else tzdir <- tzdirs[1]
    }
    x <- list.files(tzdir, recursive = TRUE)
    ## all auxiliary files are l/case.
    grep("^[ABCDEFGHIJKLMNOPQRSTUVWXYZ]", x, value = TRUE)
}
