Sys.time <- function()
    structure(.Internal(Sys.time()), class = c("POSIXt", "POSIXct"))

Sys.timezone <- function() as.vector(Sys.getenv("TZ"))

as.POSIXlt <- function(x, tz = "")
{
    fromchar <- function(x) {
	xx <- x[1]
	if(!is.na(strptime(xx, f <- "%Y-%m-%d %H:%M:%S")) ||
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
    if(inherits(x, "date") || inherits(x, "dates")) x <- as.POSIXct(x)
    if(is.character(x)) return(fromchar(x))
    if(!inherits(x, "POSIXct"))
	stop(paste("Don't know how to convert `", deparse(substitute(x)),
		   "' to class \"POSIXlt\"", sep=""))
    .Internal(as.POSIXlt(x, tz))
}

as.POSIXct <- function(x, tz = "") UseMethod("as.POSIXct")

## convert from package date
as.POSIXct.date <- function(x, ...)
{
    if(inherits(x, "date")) {
        x <- (x - 3653) * 86400 # origin 1960-01-01
        return(structure(x, class = c("POSIXt", "POSIXct")))
    } else stop(paste("`", deparse(substitute(x)),
                      "' is not a \"dates\" object", sep=""))
}

## convert from package chron
as.POSIXct.dates <- function(x, ...)
{
    if(inherits(x, "dates")) {
        z <- attr(x, "origin")
        x <- as.numeric(x) * 86400
        if(length(z) == 3 && is.numeric(z))
            x  <- x - as.numeric(ISOdate(z[3], z[1], z[2], 0))
        return(structure(x, class = c("POSIXt", "POSIXct")))
    } else stop(paste("`", deparse(substitute(x)),
                      "' is not a \"dates\" object", sep=""))
}

as.POSIXct.POSIXlt <- function(x, tz = "")
{
    if(missing(tz) && !is.null(attr(x, "tzone"))) tz <- attr(x, "tzone")[1]
    structure(.Internal(as.POSIXct(x, tz)), class = c("POSIXt", "POSIXct"))
}

as.POSIXct.default <- function(x, tz = "")
{
    if(inherits(x, "POSIXct")) return(x)
    if(is.character(x)) return(as.POSIXct(as.POSIXlt(x), tz))
    stop(paste("Don't know how to convert `", deparse(substitute(x)),
               "' to class \"POSIXct\"", sep=""))
}

format.POSIXlt <- function(x, format = "", usetz = FALSE, ...)
{
    if(!inherits(x, "POSIXlt")) stop("wrong class")
    if(format == "") {
        times <- unlist(x[1:3])
        format <- if(all(times[!is.na(times)] == 0)) "%Y-%m-%d"
        else "%Y-%m-%d %H:%M:%S"
    }
    .Internal(format.POSIXlt(x, format, usetz))
}

strftime <- .Alias(format.POSIXlt)

strptime <- function(x, format)
    .Internal(strptime(x, format))


format.POSIXct <- function(x, format = "", tz = "", usetz = FALSE, ...)
{
    if(!inherits(x, "POSIXct")) stop("wrong class")
    structure(format.POSIXlt(as.POSIXlt(x, tz), format, usetz, ...),
              names=names(x))
}

print.POSIXct <- function(x, ...)
{
    print(format(x, usetz=TRUE), ...)
    invisible(x)
}

print.POSIXlt <- function(x, ...)
{
    print(format(x, usetz=TRUE), ...)
    invisible(x)
}

summary.POSIXct <- function(object, digits=15, ...)
{
    x <- summary.default(unclass(object), digits=digits, ...)
    class(x) <- class(object)
    x
}

summary.POSIXlt <- function(object, digits = 15, ...)
    summary(as.POSIXct(object), digits = digits, ...)


"+.POSIXt" <- function(e1, e2)
{
    if (nargs() == 1) return(e1)
    # only valid if one of e1 and e2 is a scalar.
    if(inherits(e1, "POSIXt") && inherits(e2, "POSIXt"))
        stop("binary + is not defined for POSIXt objects")
    if(inherits(e1, "POSIXlt")) e1 <- as.POSIXct(e1)
    if(inherits(e2, "POSIXlt")) e2 <- as.POSIXct(e2)
    structure(unclass(e1) + unclass(e2), class = c("POSIXt", "POSIXct"))
}

"-.POSIXt" <- function(e1, e2)
{
    if(!inherits(e1, "POSIXt"))
        stop("Can only subtract from POSIXt objects")
    if (nargs() == 1) stop("unary - is not defined for POSIXt objects")
    if(inherits(e2, "POSIXt")) return(difftime(e1, e2))
    if(!is.null(class(e2)))
        stop("can only subtract numbers from POSIXt objects")
    structure(unclass(as.POSIXct(e1)) - e2, class = c("POSIXt", "POSIXct"))
}

Ops.POSIXt <- function(e1, e2)
{
    if (nargs() == 1)
        stop(paste("unary", .Generic, "not defined for POSIXct objects"))
    boolean <- switch(.Generic, "<" = , ">" = , "==" = ,
                 "!=" = , "<=" = , ">=" = TRUE, FALSE)
    if (!boolean) stop(paste(.Generic, "not defined for POSIXct objects"))
    if(inherits(e1, "POSIXlt")) e1 <- as.POSIXct(e1)
    if(inherits(e2, "POSIXlt")) e2 <- as.POSIXct(e2)
    NextMethod(.Generic)
}

Math.POSIXt <- function (x, ...)
{
    stop(paste(.Generic, "not defined for POSIXct objects"))
}

Summary.POSIXct <- function (x, ...)
{
    ok <- switch(.Generic, max = , min = , range = TRUE, FALSE)
    if (!ok) stop(paste(.Generic, "not defined for POSIXct objects"))
    val <- NextMethod(.Generic)
    class(val) <- class(x)
    val
}

Summary.POSIXlt <- function (x, ...)
{
    ok <- switch(.Generic, max = , min = , range = TRUE, FALSE)
    if (!ok) stop(paste(.Generic, "not defined for POSIXlt objects"))
    x <- as.POSIXct(x)
    val <- NextMethod(.Generic)
    as.POSIXlt(structure(val, class = c("POSIXt", "POSIXct")))
}

"[.POSIXct" <-
function(x, ..., drop = TRUE)
{
    cl <- class(x)
    class(x) <- NULL
    val <- NextMethod("[")
    class(val) <- cl
    val
}

"[[.POSIXct" <-
function(x, ..., drop = TRUE)
{
    cl <- class(x)
    class(x) <- NULL
    val <- NextMethod("[[")
    class(val) <- cl
    val
}

"[<-.POSIXct" <-
function(x, ..., value) {
    if(!as.logical(length(value))) return(x)
    value <- as.POSIXct(value)
    cl <- class(x)
    class(x) <- class(value) <- NULL
    x <- NextMethod(.Generic)
    class(x) <- cl
    x
}

as.character.POSIXct <- function(x, ...) format(x, ...)

as.character.POSIXlt <- function(x, ...) format(x, ...)

as.data.frame.POSIXct <- .Alias(as.data.frame.vector)

is.na.POSIXlt <- function(x) is.na(as.POSIXct(x))

c.POSIXct <- function(...)
    structure(c(unlist(lapply(list(...), unclass))), class="POSIXct")

## we need conversion to POSIXct as POSIXlt objects can be in different tz.
c.POSIXlt <- function(...)
    as.POSIXlt(do.call("c", lapply(list(...), as.POSIXct)))

## force absolute comparisons
all.equal.POSIXct <- function(..., scale=1)
    NextMethod("all.equal")


axis.POSIXct <- function(side, x, format, ...)
{
    x <- as.POSIXct(x)
    range <- par("usr")[if(side %%2) 1:2 else 3:4]
    ## find out the scale involved
    d <- range[2] - range[1]
    z <- c(range, x[is.finite(x)])
    if(d < 1.1*60) { # seconds
        sc <- 1
        if(missing(format)) format <- "%S"
    } else if (d < 1.1*60*60) { # minutes
        sc <- 60
        if(missing(format)) format <- "%M:%S"
    } else if (d < 1.1*60*60*24) {# hours
        sc <- 60*24
        if(missing(format)) format <- "%H:%M"
    } else if (d < 2*60*60*24) {
        sc <- 60*24
        if(missing(format)) format <- "%a %H:%M"
    } else if (d < 7*60*60*24) {# days of a week
        sc <- 60*60*24
        if(missing(format)) format <- "%a"
    } else { # days, up to a couple of months
        sc <- 60*60*24
    }
    if(d < 60*60*24*50) {
        zz <- pretty(z/sc)
        z <- zz*sc
        class(z) <- c("POSIXt", "POSIXct")
        if(missing(format)) format <- "%b %d"
    } else if(d < 1.1*60*60*24*365) { # months
        class(z) <- c("POSIXt", "POSIXct")
        zz <- as.POSIXlt(z)
        zz$mday <- 1; zz$isdst <- zz$hour <- zz$min <- zz$sec <- 0
        zz$mon <- pretty(zz$mon)
        m <- length(zz$mon)
        m <- rep(zz$year[1], m)
        zz$year <- c(m, m+1)
        z <- as.POSIXct(zz)
        if(missing(format)) format <- "%b"
    } else { # years
        class(z) <- c("POSIXt", "POSIXct")
        zz <- as.POSIXlt(z)
        zz$mday <- 1; zz$isdst <- zz$mon <- zz$hour <- zz$min <- zz$sec <- 0
        zz$year <- pretty(zz$year)
        z <- as.POSIXct(zz)
        if(missing(format)) format <- "%Y"
    }
    z <- z[z >= range[1] & z <= range[2]]
    labels <- format(z, format = format)
    axis(side, at = z, labels = labels, ...)
}

plot.POSIXct <- function(x, y, xlab = "", ...)
{
    plot.default(x, y, xaxt = "n", xlab = xlab, ...)
    axis.POSIXct(1, x)
}

plot.POSIXlt <- function(x, y, xlab = "", ...)
{
    x <- as.POSIXct(x)
    plot.default(x, y, xaxt = "n", xlab = xlab, ...)
    axis.POSIXct(1, x)
}

ISOdatetime <- function(year, month, day, hour, min, sec, tz="")
{
    x <- paste(year, month, day, hour, min, sec)
    as.POSIXct(strptime(x, "%Y %m %d %H %M %S"), tz=tz)
}

ISOdate <- function(year, month, day, hour=12, min=0, sec=0, tz="GMT")
    ISOdatetime(year, month, day, hour, min, sec, tz)

as.matrix.POSIXlt <- function(x)
{
    as.matrix(as.data.frame(unclass(x)))
}

mean.POSIXct <- function (x, ...)
    structure(mean(unclass(x), ...), class = c("POSIXt", "POSIXct"))

mean.POSIXlt <- function (x, ...)
    as.POSIXlt(mean(as.POSIXct(x), ...))

difftime <-
    function(t1, t2, tz = "",
             units = c("auto", "secs", "mins", "hours", "days"))
{
    t1 <- as.POSIXct(t1, tz = tz)
    t2 <- as.POSIXct(t2, tz = tz)
    z <- unclass(t1) - unclass(t2)
    zz <- min(abs(z))
    if(units == "auto") {
        if(zz < 60) units <- "secs"
        else if(zz < 3600) units <- "mins"
        else if(zz < 86400) units <- "hours"
        else units <- "days"
    }
    switch(units,
           "secs" = structure(z, units="secs", class="difftime"),
           "mins" = structure(z/60, units="mins", class="difftime"),
           "hours"= structure(z/3600, units="hours", class="difftime"),
           "days" = structure(z/86400, units="days", class="difftime")
           )
}

print.difftime <- function(x, digits = getOption("digits"), ...)
{
    if(length(x) > 1)
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

## for back-compatibility

"-.POSIXct" <- function(e1, e2)
{
    if(!inherits(e1, "POSIXct"))
        stop("Can only subtract from POSIXct objects")
    if (nargs() == 1) stop("unary - is not defined for POSIXct objects")
    res<- NextMethod()
    if(inherits(e2, "POSIXct")) unclass(res) else res
}

"-.POSIXlt" <- function(e1, e2)
{
    if (nargs() == 1)
        stop("unary - is not defined for dt objects")
    if(inherits(e1, "POSIXlt")) e1 <- as.POSIXct(e1)
    if(inherits(e2, "POSIXlt")) e2 <- as.POSIXct(e2)
    e1 - e2
}
