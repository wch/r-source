## First shot at adding a "Date" class to base R.
## Representation is the number of whole days since 1970-01-01.

## The difftime class already covers time differences in days.

## Need to take timezone into account here
Sys.Date <- function() .Internal(POSIXlt2Date(as.POSIXlt(Sys.time())))

as.Date <- function(x, ...) UseMethod("as.Date")

as.Date.POSIXct <- function(x, ...) {
    z <- trunc(unclass(x)/86400)
    attr(z, "tzone") <- NULL
    structure(z, class="Date")
}

as.Date.POSIXlt <- function(x, ...) .Internal(POSIXlt2Date(x))

as.Date.factor <- function(x, ...) as.Date(as.character(x))


as.Date.character <- function(x, format="", ...)
{
    fromchar <- function(x) {
	xx <- x[1]
        if(is.na(xx)) {
            j <- 1
            while(is.na(xx) && (j <- j+1) <= length(x)) xx <- x[j]
            if(is.na(xx)) f <- "%Y-%m-%d" # all NAs
        }
	if(is.na(xx) ||
	   !is.na(strptime(xx, f <- "%Y-%m-%d")) ||
	   !is.na(strptime(xx, f <- "%Y/%m/%d"))
           ) return(strptime(x, f))
	stop("character string is not in a standard unambiguous format")
    }
    res <- if(missing(format)) fromchar(x) else strptime(x, format)
    .Internal(POSIXlt2Date(res))
}

as.Date.default <- function(x, ...)
{
    if(inherits(x, "Date")) return(x)
    if(is.logical(x) && all(is.na(x)))
        return(structure(as.numeric(x), class = "Date"))
    stop("Don't know how to convert ", sQuote(deparse(substitute(x))),
	       " to class \"Date\"")
}

## convert from package date
as.Date.date <- function(x, ...)
{
    if(inherits(x, "date")) {
        x <- (x - 3653) # origin 1960-01-01
        return(structure(x, class = "Date"))
    } else stop(sQuote(deparse(substitute(x))), " is not a \"dates\" object")
}

## convert from package chron
as.Date.dates <- function(x, ...)
{
    if(inherits(x, "dates")) {
        z <- attr(x, "origin")
        x <- trunc(as.numeric(x))
        if(length(z) == 3 && is.numeric(z))
            x  <- x + as.numeric(as.Date(paste(z[3], z[1], z[2], sep="/")))
        return(structure(x, class = "Date"))
    } else stop(sQuote(deparse(substitute(x))), " is not a \"dates\" object")
}

format.Date <- function(x, ...)
{
    xx <- format(as.POSIXlt(x), ...)
    names(xx) <- names(x)
    xx
}

print.Date <- function(x, ...)
{
    print(format(x), ...)
    invisible(x)
}

summary.Date <- function(object, digits = 12, ...)
{
    x <- summary.default(unclass(object), digits = digits, ...)[1:6]# not NA's
    class(x) <- oldClass(object)
    x
}

"+.Date" <- function(e1, e2)
{
    coerceTimeUnit <- function(x)
    {
        round(switch(attr(x,"units"),
               secs = x/86400, mins = x/1440, hours = x/24,
               days = x, weeks = 7*x))
    }

    if (nargs() == 1) return(e1)
    # only valid if one of e1 and e2 is a scalar.
    if(inherits(e1, "Date") && inherits(e2, "Date"))
        stop("binary + is not defined for Date objects")
    if (inherits(e1, "difftime")) e1 <- coerceTimeUnit(e1)
    if (inherits(e2, "difftime")) e2 <- coerceTimeUnit(e2)
    structure(unclass(e1) + unclass(e2), class = "Date")
}

"-.Date" <- function(e1, e2)
{
    coerceTimeUnit <- function(x)
    {
        round(switch(attr(x,"units"),
               secs = x/86400, mins = x/1440, hours = x/24,
               days = x, weeks = 7*x))
    }
    if(!inherits(e1, "Date"))
        stop("Can only subtract from Date objects")
    if (nargs() == 1) stop("unary - is not defined for Date objects")
    if(inherits(e2, "Date")) return(difftime(e1, e2, units="days"))
    if (inherits(e2, "difftime")) e2 <- unclass(coerceTimeUnit(e2))
    if(!is.null(attr(e2, "class")))
        stop("can only subtract numbers from Date objects")
    structure(unclass(as.Date(e1)) - e2, class = "Date")
}

Ops.Date <- function(e1, e2)
{
    if (nargs() == 1)
        stop("unary ", .Generic, " not defined for Date objects")
    boolean <- switch(.Generic, "<" = , ">" = , "==" = ,
                      "!=" = , "<=" = , ">=" = TRUE, FALSE)
    if (!boolean) stop(.Generic, " not defined for Date objects")
    NextMethod(.Generic)
}

Math.Date <- function (x, ...)
    stop(.Generic, " not defined for Date objects")

Summary.Date <- function (x, ...)
{
    ok <- switch(.Generic, max = , min = , range = TRUE, FALSE)
    if (!ok) stop(.Generic, " not defined for Date objects")
    val <- NextMethod(.Generic)
    class(val) <- oldClass(x)
    val
}

"[.Date" <- function(x, ..., drop = TRUE)
{
    cl <- oldClass(x)
    class(x) <- NULL
    val <- NextMethod("[")
    class(val) <- cl
    val
}

"[[.Date" <- function(x, ..., drop = TRUE)
{
    cl <- oldClass(x)
    class(x) <- NULL
    val <- NextMethod("[[")
    class(val) <- cl
    val
}

"[<-.Date" <- function(x, ..., value)
{
    if(!as.logical(length(value))) return(x)
    value <- as.Date(value)
    cl <- oldClass(x)
    class(x) <- class(value) <- NULL
    x <- NextMethod(.Generic)
    class(x) <- cl
    x
}

as.character.Date <- function(x, ...) format(x, ...)

as.data.frame.Date <- as.data.frame.vector

c.Date <- function(..., recursive=FALSE)
    structure(c(unlist(lapply(list(...), unclass))), class="Date")

mean.Date <- function (x, ...)
    structure(mean(unclass(x), ...), class = "Date")

seq.Date <- function(from, to, by, length.out=NULL, along.with=NULL, ...)
{
    if (missing(from)) stop("'from' must be specified")
    if (!inherits(from, "Date")) stop("'from' must be a Date object")
        if(length(as.Date(from)) != 1) stop("'from' must be of length 1")
    if (!missing(to)) {
        if (!inherits(to, "Date")) stop("'to' must be a Date object")
        if (length(as.Date(to)) != 1) stop("'to' must be of length 1")
        if (to <= from) stop("'to' must be later than 'from'")
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
        from <- unclass(as.Date(from))
        to <- unclass(as.Date(to))
        res <- seq.default(from, to, length.out = length.out)
        return(structure(res, class = "Date"))
    }

    if (length(by) != 1) stop("'by' must be of length 1")
    valid <- 0
    if (inherits(by, "difftime")) {
        by <- switch(attr(by,"units"), secs = 1/86400, mins = 1/1440,
                     hours = 1/24, days = 1, weeks = 7) * unclass(by)
    } else if(is.character(by)) {
        by2 <- strsplit(by, " ", fixed=TRUE)[[1]]
        if(length(by2) > 2 || length(by2) < 1)
            stop("invalid 'by' string")
        valid <- pmatch(by2[length(by2)],
                        c("days", "weeks", "months", "years"))
        if(is.na(valid)) stop("invalid string for 'by'")
        if(valid <= 2) {
            by <- c(1, 7)[valid]
            if (length(by2) == 2) by <- by * as.integer(by2[1])
        } else
            by <- if(length(by2) == 2) as.integer(by2[1]) else 1
    } else if(!is.numeric(by)) stop("invalid mode for 'by'")
    if(is.na(by)) stop("'by' is NA")

    if(valid <= 2) {
        from <- unclass(as.Date(from))
        if(!is.null(length.out))
            res <- seq.default(from, by=by, length.out=length.out)
        else {
            to <- unclass(as.Date(to))
            ## defeat test in seq.default
            res <- seq.default(0, to - from, by) + from
        }
        return(structure(res, class="Date"))
    } else {  # months or years or DSTdays
        r1 <- as.POSIXlt(from)
        if(valid == 4) {
            if(missing(to)) { # years
                yr <- seq(r1$year, by = by, length = length.out)
            } else {
                to <- as.POSIXlt(to)
                yr <- seq(r1$year, to$year, by)
            }
            r1$year <- yr
            res <- .Internal(POSIXlt2Date(r1))
        } else if(valid == 3) { # months
            if(missing(to)) {
                mon <- seq(r1$mon, by = by, length = length.out)
            } else {
                to <- as.POSIXlt(to)
                mon <- seq(r1$mon, 12*(to$year - r1$year) + to$mon, by)
            }
            r1$mon <- mon
            res <- .Internal(POSIXlt2Date(r1))
        }
        return(res)
    }
}

cut.Date <-
    function (x, breaks, labels = NULL, start.on.monday = TRUE,
              right = FALSE, ...)
{
    if(!inherits(x, "Date")) stop("'x' must be a date-time object")
    x <- as.Date(x)

    if (inherits(breaks, "Date")) {
	breaks <- as.Date(breaks)
    } else if(is.numeric(breaks) && length(breaks) == 1) {
	## specified number of breaks
    } else if(is.character(breaks) && length(breaks) == 1) {
        by2 <- strsplit(breaks, " ", fixed=TRUE)[[1]]
        if(length(by2) > 2 || length(by2) < 1)
            stop("invalid specification of 'breaks'")
	valid <-
	    pmatch(by2[length(by2)], c("days", "weeks", "months", "years"))
	if(is.na(valid)) stop("invalid specification of 'breaks'")
	start <- as.POSIXlt(min(x, na.rm=TRUE))
	if(valid == 1) incr <- 1
	if(valid == 2) {
	    start$mday <- start$mday - start$wday
	    if(start.on.monday)
		start$mday <- start$mday + ifelse(start$wday > 0, 1, -6)
	    incr <- 7
	}
	if(valid == 3) { start$mday <- 1; incr <- 31 }
	if(valid == 4) { start$mon <- 0; incr <- 366 }
        start <- .Internal(POSIXlt2Date(start))
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

julian.Date <- function(x, origin = as.Date("1970-01-01"), ...)
{
    if(length(origin) != 1) stop("'origin' must be of length one")
    structure(unclass(x) - unclass(origin), "origin" = origin)
}

weekdays.Date <- function(x, abbreviate = FALSE)
    format(x, ifelse(abbreviate, "%a", "%A"))

months.Date <- function(x, abbreviate = FALSE)
    format(x, ifelse(abbreviate, "%b", "%B"))

quarters.Date <- function(x, ...)
{
    x <- (as.POSIXlt(x)$mon) %/% 3
    paste("Q", x+1, sep = "")
}

## These only make sense for negative digits, but still ...
round.Date <- function(x, ...)
{
    cl <- oldClass(x)
    class(x) <- NULL
    val <- NextMethod()
    class(val) <- cl
    val
}

## must avoid truncating dates prior to 1970-01-01 forwards.
trunc.Date <- function(x) round(x - 0.4999999)

rep.Date <- function(x, times, ...)
{
    y <- NextMethod()
    structure(y, class="Date")
}

diff.Date <- function (x, lag = 1, differences = 1, ...)
{
    ismat <- is.matrix(x)
    xlen <- if (ismat) dim(x)[1] else length(x)
    if (length(lag) > 1 || length(differences) > 1 || lag < 1 || differences < 1)
        stop("'lag' and 'differences' must be integers >= 1")
    if (lag * differences >= xlen)
        return(structure(numeric(0), class="difftime", units="days"))
    r <- x
    i1 <- -1:-lag
    if (ismat) for (i in 1:differences) r <- r[i1, , drop = FALSE] -
            r[-nrow(r):-(nrow(r) - lag + 1), , drop = FALSE]
    else for (i in 1:differences)
        r <- r[i1] - r[-length(r):-(length(r) - lag + 1)]
    r
}
