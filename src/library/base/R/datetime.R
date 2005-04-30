Sys.time <- function()
    structure(.Internal(Sys.time()), class = c("POSIXt", "POSIXct"))

Sys.timezone <- function() as.vector(Sys.getenv("TZ"))

as.POSIXlt <- function(x, tz = "")
{
    fromchar <- function(x) {
	xx <- x[1]
        if(is.na(xx)) {
            j <- 1
            while(is.na(xx) && (j <- j+1) <= length(x))
                xx <- x[j]
            if(is.na(xx)) f <- "%Y-%m-%d" # all NAs
        }
	if(is.na(xx) ||
           !is.na(strptime(xx, f <- "%Y-%m-%d %H:%M:%S")) ||
	   !is.na(strptime(xx, f <- "%Y/%m/%d %H:%M:%S")) ||
	   !is.na(strptime(xx, f <- "%Y-%m-%d %H:%M")) ||
	   !is.na(strptime(xx, f <- "%Y/%m/%d %H:%M")) ||
	   !is.na(strptime(xx, f <- "%Y-%m-%d")) ||
	   !is.na(strptime(xx, f <- "%Y/%m/%d")))
        {
	    res <- strptime(x, f)
            if(nchar(tz)) attr(res, "tzone") <- tz
            return(res)
        }
	stop("character string is not in a standard unambiguous format")
    }

    if(inherits(x, "POSIXlt")) return(x)
    if(inherits(x, "Date")) return(.Internal(Date2POSIXlt(x)))
    tzone <- attr(x, "tzone")
    if(inherits(x, "date") || inherits(x, "dates")) x <- as.POSIXct(x)
    if(is.character(x)) return(fromchar(unclass(x))) # precaution PR7826
    if(is.factor(x))	return(fromchar(as.character(x)))
    if(is.logical(x) && all(is.na(x))) x <- as.POSIXct.default(x)
    if(!inherits(x, "POSIXct"))
        stop(gettextf("do not know how to convert '%s' to class \"POSIXlt\"",
                      deparse(substitute(x))))
    if(missing(tz) && !is.null(tzone)) tz <- tzone[1]
    .Internal(as.POSIXlt(x, tz))
}

as.POSIXct <- function(x, tz = "") UseMethod("as.POSIXct")

as.POSIXct.Date <- function(x, ...)
    structure(unclass(x)*86400, class=c("POSIXt", "POSIXct"))


## convert from package date
as.POSIXct.date <- function(x, ...)
{
    if(inherits(x, "date")) {
        x <- (x - 3653) * 86400 # origin 1960-01-01
        return(structure(x, class = c("POSIXt", "POSIXct")))
    } else stop(gettextf("'%s' is not a \"date\" object",
                         deparse(substitute(x)) ))
}

## convert from package chron
as.POSIXct.dates <- function(x, ...)
{
    if(inherits(x, "dates")) {
        z <- attr(x, "origin")
        x <- as.numeric(x) * 86400
        if(length(z) == 3 && is.numeric(z))
            x  <- x + as.numeric(ISOdate(z[3], z[1], z[2], 0))
        return(structure(x, class = c("POSIXt", "POSIXct")))
    } else stop(gettextf("'%s' is not a \"dates\" object",
                         deparse(substitute(x)) ))
}

as.POSIXct.POSIXlt <- function(x, tz = "")
{
    tzone <- attr(x, "tzone")
    if(missing(tz) && !is.null(tzone)) tz <- tzone[1]
    structure(.Internal(as.POSIXct(x, tz)), class = c("POSIXt", "POSIXct"),
              tzone = tz)
}

as.POSIXct.default <- function(x, tz = "")
{
    if(inherits(x, "POSIXct")) return(x)
    if(is.character(x) || is.factor(x))
	return(as.POSIXct(as.POSIXlt(x), tz))
    if(is.logical(x) && all(is.na(x)))
        return(structure(as.numeric(x), class = c("POSIXt", "POSIXct")))
    stop(gettextf("do not know how to convert '%s' to class \"POSIXlt\"",
                  deparse(substitute(x))))
}

format.POSIXlt <- function(x, format = "", usetz = FALSE, ...)
{
    if(!inherits(x, "POSIXlt")) stop("wrong class")
    if(format == "") {
        ## need list [ method here.
        times <- unlist(unclass(x)[1:3])
        format <- if(all(times[!is.na(times)] == 0)) "%Y-%m-%d"
        else "%Y-%m-%d %H:%M:%S"
    }
    .Internal(format.POSIXlt(x, format, usetz))
}

strftime <- format.POSIXlt

strptime <- function(x, format)
    .Internal(strptime(as.character(x), format))


format.POSIXct <- function(x, format = "", tz = "", usetz = FALSE, ...)
{
    if(!inherits(x, "POSIXct")) stop("wrong class")
    if(missing(tz) && !is.null(tzone <- attr(x, "tzone"))) tz <- tzone
    structure(format.POSIXlt(as.POSIXlt(x, tz), format, usetz, ...),
              names=names(x))
}

print.POSIXct <- function(x, ...)
{
    print(format(x, usetz=TRUE, ...), ...)
    invisible(x)
}

print.POSIXlt <- function(x, ...)
{
    print(format(x, usetz=TRUE), ...)
    invisible(x)
}

summary.POSIXct <- function(object, digits=15, ...)
{
    x <- summary.default(unclass(object), digits=digits, ...)[1:6]# no NA's
    class(x) <- oldClass(object)
    attr(x, "tzone") <- attr(object, "tzone")
    x
}

summary.POSIXlt <- function(object, digits = 15, ...)
    summary(as.POSIXct(object), digits = digits, ...)


"+.POSIXt" <- function(e1, e2)
{
    coerceTimeUnit <- function(x)
    {
        switch(attr(x,"units"),
               secs = x, mins = 60*x, hours = 60*60*x,
               days = 60*60*24*x, weeks = 60*60*24*7*x)
    }

    if (nargs() == 1) return(e1)
    # only valid if one of e1 and e2 is a scalar.
    if(inherits(e1, "POSIXt") && inherits(e2, "POSIXt"))
        stop("binary + is not defined for \"POSIXt\" objects")
    if(inherits(e1, "POSIXlt")) e1 <- as.POSIXct(e1)
    if(inherits(e2, "POSIXlt")) e2 <- as.POSIXct(e2)
    if (inherits(e1, "difftime")) e1 <- coerceTimeUnit(e1)
    if (inherits(e2, "difftime")) e2 <- coerceTimeUnit(e2)
    structure(unclass(e1) + unclass(e2), class = c("POSIXt", "POSIXct"))
}

"-.POSIXt" <- function(e1, e2)
{
    coerceTimeUnit <- function(x)
    {
        switch(attr(x,"units"),
               secs = x, mins = 60*x, hours = 60*60*x,
               days = 60*60*24*x, weeks = 60*60*24*7*x)
    }
    if(!inherits(e1, "POSIXt"))
        stop("Can only subtract from POSIXt objects")
    if (nargs() == 1) stop("unary - is not defined for \"POSIXt\" objects")
    if(inherits(e2, "POSIXt")) return(difftime(e1, e2))
    if (inherits(e2, "difftime")) e2 <- unclass(coerceTimeUnit(e2))
    if(!is.null(attr(e2, "class")))
        stop("can only subtract numbers from POSIXt objects")
    structure(unclass(as.POSIXct(e1)) - e2, class = c("POSIXt", "POSIXct"))
}

Ops.POSIXt <- function(e1, e2)
{
    if (nargs() == 1)
        stop("unary", .Generic, " not defined for \"POSIXt\" objects")
    boolean <- switch(.Generic, "<" = , ">" = , "==" = ,
                      "!=" = , "<=" = , ">=" = TRUE, FALSE)
    if (!boolean) stop(.Generic, " not defined for \"POSIXt\" objects")
    if(inherits(e1, "POSIXlt")) e1 <- as.POSIXct(e1)
    if(inherits(e2, "POSIXlt")) e2 <- as.POSIXct(e2)
    NextMethod(.Generic)
}

Math.POSIXt <- function (x, ...)
{
    stop(.Generic, " not defined for POSIXt objects")
}

Summary.POSIXct <- function (x, ...)
{
    ok <- switch(.Generic, max = , min = , range = TRUE, FALSE)
    if (!ok) stop(.Generic, " not defined for \"POSIXct\" objects")
    val <- NextMethod(.Generic)
    class(val) <- oldClass(x)
    val
}

Summary.POSIXlt <- function (x, ...)
{
    ok <- switch(.Generic, max = , min = , range = TRUE, FALSE)
    if (!ok) stop(.Generic, " not defined for \"POSIXlt\" objects")
    x <- as.POSIXct(x)
    val <- NextMethod(.Generic)
    as.POSIXlt(structure(val, class = c("POSIXt", "POSIXct")))
}

"[.POSIXct" <-
function(x, ..., drop = TRUE)
{
    cl <- oldClass(x)
    class(x) <- NULL
    val <- NextMethod("[")
    class(val) <- cl
    val
}

"[[.POSIXct" <-
function(x, ..., drop = TRUE)
{
    cl <- oldClass(x)
    class(x) <- NULL
    val <- NextMethod("[[")
    class(val) <- cl
    val
}

"[<-.POSIXct" <-
function(x, ..., value) {
    if(!as.logical(length(value))) return(x)
    value <- as.POSIXct(value)
    cl <- oldClass(x)
    class(x) <- class(value) <- NULL
    x <- NextMethod(.Generic)
    class(x) <- cl
    x
}

as.character.POSIXt <- function(x, ...) format(x, ...)

str.POSIXt <- function(object, ...) {
    cl <- oldClass(object)
    cat("'", cl[min(2, length(cl))],"', format:", sep = "")
    str(format(object), ...)
}

as.data.frame.POSIXct <- as.data.frame.vector

is.na.POSIXlt <- function(x) is.na(as.POSIXct(x))

## <FIXME> check the argument validity
c.POSIXct <- function(..., recursive=FALSE)
    structure(c(unlist(lapply(list(...), unclass))),
              class=c("POSIXt","POSIXct"))

## we need conversion to POSIXct as POSIXlt objects can be in different tz.
c.POSIXlt <- function(..., recursive=FALSE)
    as.POSIXlt(do.call("c", lapply(list(...), as.POSIXct)))

## force absolute comparisons
all.equal.POSIXct <- function(target, current, ..., scale=1)
    NextMethod("all.equal")



ISOdatetime <- function(year, month, day, hour, min, sec, tz="")
{
    x <- paste(year, month, day, hour, min, sec, sep="-")
    as.POSIXct(strptime(x, "%Y-%m-%d-%H-%M-%S"), tz=tz)
}

ISOdate <- function(year, month, day, hour=12, min=0, sec=0, tz="GMT")
    ISOdatetime(year, month, day, hour, min, sec, tz)

as.matrix.POSIXlt <- function(x)
{
    as.matrix(as.data.frame(unclass(x)))
}

mean.POSIXct <- function (x, ...)
    structure(mean(unclass(x), ...), class = c("POSIXt", "POSIXct"),
              tzone=attr(x, "tzone"))

mean.POSIXlt <- function (x, ...)
    as.POSIXlt(mean(as.POSIXct(x), ...))

## ----- difftime -----

difftime <-
    function(time1, time2, tz = "",
             units = c("auto", "secs", "mins", "hours", "days", "weeks"))
{
    time1 <- as.POSIXct(time1, tz = tz)
    time2 <- as.POSIXct(time2, tz = tz)
    z <- unclass(time1) - unclass(time2)
    units <- match.arg(units)
    if(units == "auto") {
        if(all(is.na(z))) units <- "secs"
        else {
            zz <- min(abs(z),na.rm=TRUE)
            if(is.na(zz) || zz < 60) units <- "secs"
            else if(zz < 3600) units <- "mins"
            else if(zz < 86400) units <- "hours"
            else units <- "days"
        }
    }
    switch(units,
           "secs" = structure(z, units="secs", class="difftime"),
           "mins" = structure(z/60, units="mins", class="difftime"),
           "hours"= structure(z/3600, units="hours", class="difftime"),
           "days" = structure(z/86400, units="days", class="difftime"),
           "weeks" = structure(z/(7*86400), units="weeks", class="difftime")
           )
}

## "difftime" constructor
## Martin Maechler, Date: 16 Sep 2002
as.difftime <- function(tim, format="%X")
{
    difftime(strptime(tim, format=format),
             strptime("0:0:0", format="%X"))
}

print.difftime <- function(x, digits = getOption("digits"), ...)
{
    if(is.array(x)) {
        cat("Time differences in ", attr(x, "units"), "\n", sep="")
        y <- unclass(x); attr(y, "units") <- NULL
        print(y)
    } else if(length(x) > 1)
        cat("Time differences of ",
            paste(format(unclass(x), digits=digits), collapse = ", "), " ",
            attr(x, "units"), "\n", sep="")
    else
        cat("Time difference of ", format(unclass(x), digits=digits), " ",
            attr(x, "units"), "\n", sep="")

    invisible(x)
}

round.difftime <- function (x, digits = 0)
{
   units <- attr(x, "units")
   structure(NextMethod(), units=units, class="difftime")
}

"[.difftime" <- function(x, ..., drop = TRUE)
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
        switch(attr(x,"units"),
               secs = x, mins = 60*x, hours = 60*60*x,
               days = 60*60*24*x, weeks = 60*60*24*7*x)
    }
    if (nargs() == 1)
        stop("unary", .Generic, " not defined for \"difftime\" objects")
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
            structure(NextMethod(.Generic), units=u1, class="difftime")
        } else {
            e1 <- coerceTimeUnit(e1)
            e2 <- coerceTimeUnit(e2)
            structure(NextMethod(.Generic), units="secs", class="difftime")
        }
    } else {
        ## '*' is covered by a specific method
        stop(.Generic, "not defined for \"difftime\" objects")
    }
}

"*.difftime" <- function (e1, e2)
{
    ## need one scalar, one difftime.
    if(inherits(e1, "difftime") && inherits(e2, "difftime"))
        stop("both arguments of * cannot be \"difftime\" objects")
    if(inherits(e2, "difftime")) {tmp <- e1; e1 <- e2; e2 <- tmp}
    structure(e2 * unclass(e1), units = attr(e1, "units"),
              class = "difftime")
}

"/.difftime" <- function (e1, e2)
{
    ## need one scalar, one difftime.
    if(inherits(e2, "difftime"))
        stop("second argument of / cannot be a \"difftime\" object")
    structure(unclass(e1) / e2, units = attr(e1, "units"),
              class = "difftime")
}

Math.difftime <- function (x, ...)
{
    stop(.Generic, "not defined for \"difftime\" objects")
}

mean.difftime <- function (x, ..., na.rm = FALSE)
{
    coerceTimeUnit <- function(x)
    {
        as.vector(switch(attr(x,"units"),
               secs = x, mins = 60*x, hours = 60*60*x,
               days = 60*60*24*x, weeks = 60*60*24*7*x))
    }
    if(length(list(...))) {
        args <- c(lapply(list(x, ...), coerceTimeUnit), na.rm = na.rm)
        structure(do.call("mean", args), units="secs", class="difftime")
    } else {
        structure(mean(as.vector(x), na.rm = na.rm),
                  units=attr(x, "units"), class="difftime")
    }
}

Summary.difftime <- function (x, ..., na.rm = FALSE)
{
    coerceTimeUnit <- function(x)
    {
        as.vector(switch(attr(x,"units"),
                         secs = x, mins = 60*x, hours = 60*60*x,
                         days = 60*60*24*x, weeks = 60*60*24*7*x))
    }
    ok <- switch(.Generic, max = , min = , range = TRUE, FALSE)
    if (!ok) stop(.Generic, " not defined for \"difftime\" objects")
    args <- c(lapply(list(x, ...), coerceTimeUnit), na.rm = na.rm)
    structure(do.call(.Generic, args), units="secs", class="difftime")
}


## ----- convenience functions -----

seq.POSIXt <-
    function(from, to, by, length.out = NULL, along.with = NULL, ...)
{
    if (missing(from)) stop("'from' must be specified")
    if (!inherits(from, "POSIXt")) stop("'from' must be a POSIXt object")
        if(length(as.POSIXct(from)) != 1) stop("'from' must be of length 1")
    if (!missing(to)) {
        if (!inherits(to, "POSIXt")) stop("'to' must be a POSIXt object")
        if (length(as.POSIXct(to)) != 1) stop("'to' must be of length 1")
    }
    if (!missing(along.with)) {
        length.out <- length(along.with)
    }  else if (!missing(length.out)) {
        if (length(length.out) != 1) stop("'length.out' must be of length 1")
        length.out <- ceiling(length.out)
    }
    status <- c(!missing(to), !missing(by), !is.null(length.out))
    if(sum(status) != 2)
        stop("exactly two of 'to', 'by' and 'length.out' / 'along.with' must be specified")
    if (missing(by)) {
        from <- unclass(as.POSIXct(from))
        to <- unclass(as.POSIXct(to))
        ## Till (and incl.) 1.6.0 :
        ##- incr <- (to - from)/length.out
        ##- res <- seq.default(from, to, incr)
        res <- seq.default(from, to, length.out = length.out)
        return(structure(res, class = c("POSIXt", "POSIXct")))
    }

    if (length(by) != 1) stop("'by' must be of length 1")
    valid <- 0
    if (inherits(by, "difftime")) {
        by <- switch(attr(by,"units"), secs = 1, mins = 60, hours = 3600,
                     days = 86400, weeks = 7*86400) * unclass(by)
    } else if(is.character(by)) {
        by2 <- strsplit(by, " ", fixed=TRUE)[[1]]
        if(length(by2) > 2 || length(by2) < 1)
            stop("invalid 'by' string")
        valid <- pmatch(by2[length(by2)],
                        c("secs", "mins", "hours", "days", "weeks",
                          "months", "years", "DSTdays"))
        if(is.na(valid)) stop("invalid string for 'by'")
        if(valid <= 5) {
            by <- c(1, 60, 3600, 86400, 7*86400)[valid]
            if (length(by2) == 2) by <- by * as.integer(by2[1])
        } else
            by <- if(length(by2) == 2) as.integer(by2[1]) else 1
    } else if(!is.numeric(by)) stop("invalid mode for 'by'")
    if(is.na(by)) stop("'by' is NA")

    if(valid <= 5) {
        from <- unclass(as.POSIXct(from))
        if(!is.null(length.out))
            res <- seq.default(from, by=by, length.out=length.out)
        else {
            to <- unclass(as.POSIXct(to))
            ## defeat test in seq.default
            res <- seq.default(0, to - from, by) + from
        }
        return(structure(res, class=c("POSIXt", "POSIXct")))
    } else {  # months or years or DSTdays
        r1 <- as.POSIXlt(from)
        if(valid == 7) {
            if(missing(to)) { # years
                yr <- seq(r1$year, by = by, length = length.out)
            } else {
                to <- as.POSIXlt(to)
                yr <- seq(r1$year, to$year, by)
            }
            r1$year <- yr
            r1$isdst <- -1
            res <- as.POSIXct(r1)
        } else if(valid == 6) { # months
            if(missing(to)) {
                mon <- seq(r1$mon, by = by, length = length.out)
            } else {
                to <- as.POSIXlt(to)
                mon <- seq(r1$mon, 12*(to$year - r1$year) + to$mon, by)
            }
            r1$mon <- mon
            r1$isdst <- -1
            res <- as.POSIXct(r1)
        } else if(valid == 8) { # DSTdays
            if(!missing(to)) {
                ## We might have a short day, so need to over-estimate.
                length.out <- 2 + floor((unclass(as.POSIXct(to)) -
                                         unclass(as.POSIXct(from)))/86400)
            }
            r1$mday <- seq(r1$mday, by = by, length = length.out)
            r1$isdst <- -1
            res <- as.POSIXct(r1)
            ## now correct if necessary.
            if(!missing(to)) res <- res[res <= as.POSIXct(to)]
        }
        return(res)
    }
}

cut.POSIXt <-
    function (x, breaks, labels = NULL, start.on.monday = TRUE,
              right = FALSE, ...)
{
    if(!inherits(x, "POSIXt")) stop("'x' must be a date-time object")
    x <- as.POSIXct(x)

    if (inherits(breaks, "POSIXt")) {
	breaks <- as.POSIXct(breaks)
    } else if(is.numeric(breaks) && length(breaks) == 1) {
	## specified number of breaks
    } else if(is.character(breaks) && length(breaks) == 1) {
        by2 <- strsplit(breaks, " ", fixed=TRUE)[[1]]
        if(length(by2) > 2 || length(by2) < 1)
            stop("invalid specification of 'breaks'")
	valid <-
	    pmatch(by2[length(by2)],
		   c("secs", "mins", "hours", "days", "weeks",
		     "months", "years", "DSTdays"))
	if(is.na(valid)) stop("invalid specification of 'breaks'")
	start <- as.POSIXlt(min(x, na.rm=TRUE))
	incr <- 1
	if(valid > 1) { start$sec <- 0; incr <- 59.99 }
	if(valid > 2) { start$min <- 0; incr <- 3600 - 1 }
	if(valid > 3) { start$hour <- 0; incr <- 86400 - 1 }
	if(valid == 5) {
	    start$mday <- start$mday - start$wday
	    if(start.on.monday)
		start$mday <- start$mday + ifelse(start$wday > 0, 1, -6)
	    incr <- 7*86400
	}
	if(valid == 6) { start$mday <- 1; incr <- 31*86400 }
	if(valid == 7) { start$mon <- 0; incr <- 366*86400 }
        if(valid == 8) incr <- 25*3600
        if (length(by2) == 2) incr <- incr * as.integer(by2[1])
	maxx <- max(x, na.rm = TRUE)
	breaks <- seq(start, maxx + incr, breaks)
	breaks <- breaks[1:(1+max(which(breaks < maxx)))]
    } else stop("invalid specification of 'breaks'")
    res <- cut(unclass(x), unclass(breaks), labels = labels,
               right = right, ...)
    if(is.null(labels)) levels(res) <- as.character(breaks[-length(breaks)])
    res
}

julian <- function(x, ...) UseMethod("julian")

julian.POSIXt <- function(x, origin = as.POSIXct("1970-01-01", tz="GMT"), ...)
{
    if(length(origin) != 1) stop("'origin' must be of length one")
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
    paste("Q", x+1, sep = "")
}

trunc.POSIXt <- function(x, units=c("secs", "mins", "hours", "days"))
{
    units <- match.arg(units)
    x <- as.POSIXlt(x)
    if(length(x$sec) > 0)
	switch(units,
	       "secs" = {x$sec <- trunc(x$sec)},
	       "mins" = {x$sec <- 0},
	       "hours"= {x$sec <- 0; x$min <- 0},
	       "days" = {x$sec <- 0; x$min <- 0; x$hour <- 0; x$isdst <- -1}
	       )
    x
}

round.POSIXt <- function(x, units=c("secs", "mins", "hours", "days"))
{
    ## this gets the default from the generic, as that has two args.
    if(is.numeric(units) && units == 0.0) units <-"secs"
    units <- match.arg(units)
    x <- as.POSIXct(x)
    x <- x + switch(units,
                    "secs" = 0.5, "mins" = 30, "hours"= 1800, "days" = 43200)
    trunc.POSIXt(x, units = units)
}

# ---- additions in 1.5.0 -----

"[.POSIXlt" <- function(x, ..., drop = TRUE)
{
    val <- lapply(x, "[", ..., drop = drop)
    attributes(val) <- attributes(x) # need to preserve timezones
    val
}

"[<-.POSIXlt" <- function(x, i, value)
{
    if(!as.logical(length(value))) return(x)
    value <- as.POSIXlt(value)
    cl <- oldClass(x)
    class(x) <- class(value) <- NULL
    for(n in names(x)) x[[n]][i] <- value[[n]]
    class(x) <- cl
    x
}

as.data.frame.POSIXlt <- function(x, row.names = NULL, optional = FALSE)
{
    value <- as.data.frame.POSIXct(as.POSIXct(x), row.names, optional)
    if (!optional)
        names(value) <- deparse(substitute(x))[[1]]
    value
}

# ---- additions in 1.8.0 -----

rep.POSIXct <- function(x, times,  ...)
{
    y <- NextMethod()
    structure(y, class=c("POSIXt", "POSIXct"), tzone = attr(x, "tzone"))
}

rep.POSIXlt <- function(x, times, ...)
{
    y <- if(missing(times)) lapply(x, rep, ...)
       else lapply(x, rep, times=times, ...)
    attributes(y) <- attributes(x)
    y
}

diff.POSIXt <- function (x, lag = 1, differences = 1, ...)
{
    ismat <- is.matrix(x)
    xlen <- if (ismat) dim(x)[1] else length(x)
    if (length(lag) > 1 || length(differences) > 1 || lag < 1 || differences < 1)
        stop("'lag' and 'differences' must be integers >= 1")
    if (lag * differences >= xlen)
        return(structure(numeric(0), class="difftime", units="secs"))
    r <- x
    i1 <- -1:-lag
    if (ismat) for (i in 1:differences) r <- r[i1, , drop = FALSE] -
            r[-nrow(r):-(nrow(r) - lag + 1), , drop = FALSE]
    else for (i in 1:differences)
        r <- r[i1] - r[-length(r):-(length(r) - lag + 1)]
    r
}
