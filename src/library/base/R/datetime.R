Sys.time <- function()
    structure(.Internal(Sys.time()), class = "POSIXct")

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
        return(structure(x, class = "POSIXct"))
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
        return(structure(x, class = "POSIXct"))
    } else stop(paste("`", deparse(substitute(x)),
                      "' is not a \"dates\" object", sep=""))
}

as.POSIXct.POSIXlt <- function(x, tz = "")
{
    if(missing(tz) && !is.null(attr(x, "tzone"))) tz <- attr(x, "tzone")[1]
    structure(.Internal(as.POSIXct(x, tz)), class = "POSIXct")
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


"+.POSIXct" <- function(e1, e2)
{
    if (nargs() == 1) return(e1)
    # only valid if one of e1 and e2 is a scalar.
    if(inherits(e1, "POSIXct") && inherits(e2, "POSIXct"))
        stop("binary + is not defined for POSIXct objects")
    structure(NextMethod(), class = "POSIXct")
}

"-.POSIXct" <- function(e1, e2)
{
    if(!inherits(e1, "POSIXct"))
        stop("Can only subtract from POSIXct objects")
    if (nargs() == 1) stop("unary - is not defined for POSIXct objects")
    res<- NextMethod()
    if(inherits(e2, "POSIXct")) unclass(res) else res
}

Ops.POSIXct <- function(e1, e2)
{
    if (nargs() == 1)
        stop(paste("unary", .Generic, "not defined for POSIXct objects"))
    boolean <- switch(.Generic, "<" = , ">" = , "==" = ,
                 "!=" = , "<=" = , ">=" = TRUE, FALSE)
    if (!boolean) stop(paste(.Generic, "not defined for POSIXct objects"))
    NextMethod(.Generic)
}

Math.POSIXct <- function (x, ...)
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

"+.POSIXlt" <- function(e1, e2)
{
    if (nargs() == 1) return(e1)
    # only valid if one of e1 and e2 is a scalar.
    if(inherits(e1, "POSIXlt") && inherits(e2, "POSIXlt"))
        stop("binary + is not defined for POSIXlt objects")
    if(inherits(e1, "POSIXlt")) e1 <- as.POSIXct(e1)
    if(inherits(e2, "POSIXlt")) e2 <- as.POSIXct(e2)
    e1 + e2
}

"-.POSIXlt" <- function(e1, e2)
{
    if (nargs() == 1)
        stop("unary - is not defined for dt objects")
    if(inherits(e1, "POSIXlt")) e1 <- as.POSIXct(e1)
    if(inherits(e2, "POSIXlt")) e2 <- as.POSIXct(e2)
    e1 - e2
}

Ops.POSIXlt <- function(e1, e2)
{
    if (nargs() == 1)
        stop(paste("unary", .Generic, "not defined for POSIXlt objects"))
    boolean <- switch(.Generic, "<" = , ">" = , "==" = ,
                 "!=" = , "<=" = , ">=" = TRUE, FALSE)
    if (!boolean) stop(paste(.Generic, "not defined for POSIXlt objects"))
    e1 <- as.POSIXct(e1)
    e2 <- as.POSIXct(e2)
    NextMethod(.Generic)
}

Math.POSIXlt <- function (x, ...)
{
    stop(paste(.Generic, "not defined for dt objects"))
}

Summary.POSIXlt <- function (x, ...)
{
    ok <- switch(.Generic, max = , min = , range = TRUE, FALSE)
    if (!ok) stop(paste(.Generic, "not defined for POSIXlt objects"))
    x <- as.POSIXct(x)
    val <- NextMethod(.Generic)
    as.POSIXct(val)
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

c.POSIXct <- function(..., recursive=FALSE)
    structure(c(unlist(lapply(list(...), unclass))), class="POSIXct")

## we need conversion to POSIXct as POSIXlt objects can be in different tz.
c.POSIXlt <- function(..., recursive=FALSE)
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
        class(z) <- "POSIXct"
        if(missing(format)) format <- "%b %d"
    } else if(d < 1.1*60*60*24*365) { # months
        class(z) <- "POSIXct"
        zz <- as.POSIXlt(z)
        zz$mday <- 1; zz$isdst <- zz$hour <- zz$min <- zz$sec <- 0
        zz$mon <- pretty(zz$mon)
        m <- length(zz$mon)
        m <- rep(zz$year[1], m)
        zz$year <- c(m, m+1)
        z <- as.POSIXct(zz)
        if(missing(format)) format <- "%b"
    } else { # years
        class(z) <- "POSIXct"
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
    structure(mean(unclass(x), ...), class = "POSIXct")

mean.POSIXlt <- function (x, ...)
    as.POSIXlt(mean(as.POSIXct(x), ...))
