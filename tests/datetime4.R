### more date-time tests where the output is to be checked.

## Expect differences, especially for platforms without tm_zone/tm_gmtoff.

options(warn = 1L)

## tests of "POSIXlt" objects
xU <- strptime("2022-01-01", "%Y-%m-%d", tz = "UTC")
xU
str(unclass(xU))

x0 <- strptime("2022-01-01", "%Y-%m-%d")
x0
str(unclass(x0))

x1 <- strptime("2022-07-01", "%Y-%m-%d", tz = "Europe/London")
x1
str(unclass(x1)) # gmtoff is NA

## give offset value -- makes most sense in UTC
x2 <- strptime("2022-07-01 10:55:03 +0300", "%Y-%m-%d %H:%M:%S %z", tz = "UTC")
x2
str(unclass(x2))
## in another tzone it will report the time in that zone, with its DST ....
x3 <- strptime("2022-07-01 10:55:03 +0300", "%Y-%m-%d %H:%M:%S %z",
               tz = "Europe/Vienna")
x3
str(unclass(x3))

x4 <- strptime("2022-07-01", "%Y-%m-%d", tz ="Pacific/Fiji")
x4
str(unclass(x4)) # abbreviations may be numbers.
# Kiribati does/did not have DST, so second abbreviation may be repeat or empty
x5 <- strptime("2022-07-01", "%Y-%m-%d", tz ="Pacific/Kiritimati")
x5
str(unclass(x5)) # does not have DST, hence no DST abbreviation on some platforms

## edge of range and out of range offsets
strptime("2022-01-01 +1400", "%Y-%m-%d %z", tz = "UTC")
strptime("2022-01-01 -1400", "%Y-%m-%d %z", tz = "UTC")
strptime("2022-01-01 +1500", "%Y-%m-%d %z", tz = "UTC")
strptime("2022-01-01 -1500", "%Y-%m-%d %z", tz = "UTC")


## extreme values for as.Date (negative ones were wrong in R 4.2.2)
as.Date(2^(30:33))
as.Date(-2^(30:33))
## tm$year will overflow ints in less than 800 milion years from present.
as.Date(c(7e11, 8e11, -7e11, -8e11))


## handling of names
# conversion of R objects
x <- seq(as.Date("2022-09-01"), by = "weeks", length = 10)
names(x) <- paste("week", 1:10)
x
(xl <- as.POSIXlt(x))
str(unclass(xl))
xx <- as.POSIXct(x, tz = "Europe/London")
xx
as.POSIXlt(xx)
as.Date(xl)

# character vector -> R objects
y <- format(x)
as.Date(y)
## IGNORE_RDIFF_BEGIN
as.POSIXct(y)
(yy <- as.POSIXlt(y))
unclass(yy)

strptime(y, "%Y-%m-%d")
strftime(y, "%Y-%m-%d")
y2 <- paste(y, "10:01:02"); names(y2) <- names(y)
fmt <- c("%Y-%m-%d", "%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M:%S %Z")
(strptime(y2, fmt[1:2]) -> sy2)
## IGNORE_RDIFF_END
sy2.15 <- strptime(y2, rep(fmt[1:2], length = 15)) # failed to recycle names
stopifnot(suppressWarnings(sy2 == sy2.15))

xl. <- xl[1:9] # length(fmt) == 3 -- fully recycles in xl.
(strftime(xl., fmt) -> sx)
(strftime(xl., rep(fmt, length = 15)) -> sx15)
stopifnot(exprs = { # with warnings  ".. length is not a multiple of shorter .."
    sx == sx15
    names(sx) == names(sx15)
})

x2 <- xl[1:5]
x2$year <- xl$year[1:3]
x2                # correctly has missing names as NA
balancePOSIXlt(x2) # recycles names
strftime(x2, fmt)
strftime(x2, rep(fmt, length = 10))
