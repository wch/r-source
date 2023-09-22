### Tests of often platform-dependent features of the POSIX[cl]t implementation.

### Expect differences, especially with 32-bit time_t and platforms
### without tm_zone/tm_gmtoff.

z <- ISOdate(1890:1912, 1, 10, tz="UTC")
## Rome changed to CET for 1894
as.POSIXlt(z, tz="Europe/Rome")
## Paris changed to PMT for 1892, WET for 1912
(zz <- as.POSIXlt(z, tz="Europe/Paris"))
strftime(zz, "%Y-%m-%d %H:%M:%S %Z")
## The offset was really +00:09:21 until 1911, then +00:00
## Many platforms will give the current offset, +0100
strftime(zz, "%Y-%m-%d %H:%M:%S %z")

## Some platforms give details of the latest conversion.
z <- ISOdate(c(seq(1890, 1940, 5), 1941:1946, 1950), 1, 10, tz="UTC")
as.POSIXlt(z, tz="Europe/Paris")
for(i in seq_along(z)) print(as.POSIXlt(z[i], tz="Europe/Paris"))
## use pf %z needs tm_gmtoff
for(i in seq_along(z))
    print(strftime(as.POSIXlt(z[i], tz="Europe/Paris"), "%Y-%m-%d %H:%M:%S %z"))

strptime("1920-12-27 08:18:23", "%Y-%m-%d %H:%M:%S", tz="Europe/Paris")

## check %V etc

d <- expand.grid(day = 1:7, year = 2000:2010)
z1 <- with(d, ISOdate(year, 1, day))
d <- expand.grid(day = 25:31, year = 2000:2010)
z2 <- with(d, ISOdate(year, 12, day))
z <- sort(c(z1, z2))
strftime(z, "%G %g %W %U %u %V %W %w", tz="Europe/Paris")

## tests of earlier years.  Default format is OS-dependent, so don't test it.
## ISOdate only accepts positive years.
z <- as.Date(ISOdate(c(0, 8, 9, 10, 11, 20, 110, 1010), 1, 10)) - 3630
strftime(z, "%04Y-%m-%d") # with leading zero(s), where supported
strftime(z, "%_4Y-%m-%d") # with leading space(s), where supported
strftime(z, "%0Y-%m-%d") # without


## more test of strftime
x <- ISOdate(2014, 3, 10, c(7, 13))
fmts <- c("%Y-%m-%d %H:%M:%S", "%F", "%A %a %b %h %e %I %j",
          ## locale-dependent ones
          "%X", # but the same in all English locales
          "%c", "%x", "%p", "%r")
for (f in fmts) print(format(x, f))

## Moved from reg-tests-1d.R
## as.POSIXlt(<very large Date>) gave integer overflow
## and needed C-level change for 32-bit time_t.
.Machine$sizeof.time_t
(z <- .Date(2^31 + 10))
as.POSIXlt(z)$year == 5879680L
## year was negative in R <= 4.2.1, even for 64-bit time_t


## ------------- Tests of far-distant dates -----------
Sys.setenv(TZ = "Europe/London")
## the pre-1902 POSIXct values wil be 75s out on platdorm that do not
## know about UK changes prior to 1902 (in fact in 1847-12-01: see below).
as.POSIXct("4000-07-01")
as.Date("4000-07-01")
zz <- z <- as.POSIXlt("2000-07-01")
unclass(z)

years <- c(-1e6, -1e5, -1e4, seq(-1000, 4000, by = 100), 1e4, 1e5, 1e6)
y <- character(length(years))
for(i in seq_along(years)) {
    zz$year = years[i] - 1900
    y[i] <- strftime(zz)
}
## IGNORE_RDIFF_BEGIN
y
## IGNORE_RDIFF_END

y <- double(length(years))
for(i in seq_along(years)) {
    zz$year = years[i] - 1900
    zz$isdst <- -1 # some are DST, some not so let the code decide
    y[i] <- as.POSIXct(zz)
}
print(y, digits=14)
y <- .POSIXct(y)
## IGNORE_RDIFF_BEGIN
(y1 <- strftime(y)) # leading zeros or spaces is platform-dependant
## IGNORE_RDIFF_END
y2 <- strftime(y, "%_4Y-%m-%d") # not all platforms interpret this
if(y2[1] != "4Y-07-01") print(y2) else message('format "%_4Y" unsupported')

y <- double(length(years))
for(i in seq_along(years)) {
    zz$year = years[i] - 1900
    zz$isdst <- -1
    y[i] <- as.Date(zz)
}
y
class(y) <- "Date"
## IGNORE_RDIFF_BEGIN
(y3 <- strftime(y))
## IGNORE_RDIFF_END
y4 <- strftime(y, "%_4Y-%m-%d")
stopifnot(identical(y3, y1))

zz <- as.POSIXlt("1900-07-01")
years <- c(1800, 1847:1848, 1899:1902)
y <- double(length(years))
for(i in seq_along(years)) {
    zz$year = years[i] - 1900
    zz$isdst <- -1 # some are DST, some not so let the code decide
    y[i] <- as.POSIXct(zz)
}
print(y, digits=14)
.POSIXct(y)

## change of 75s in 1847
seq(as.POSIXlt("1847-11-24"), as.POSIXlt("1847-12-07"), by ="day")

## end of ------------- Tests of far-distant dates -----------

## Tests of %z and %Z for output.
## Use pf %z needs tm_gmtoff so offsets will otherwise be +0000
x1 <- strptime("2022-07-01", "%Y-%m-%d", tz = "UTC")
x2 <- strptime("2022-07-01", "%Y-%m-%d", tz = "Europe/Rome")
x1
x2
# RFC5322 format
format(x1, "%a, %d %b %Y %H:%M:%S %z")
# offset may not not determined: +0200 is correct
format(x2, "%a, %d %b %Y %H:%M:%S %z")
format(as.POSIXct(x2), "%a, %d %b %Y %H:%M:%S %z") # usually correct
format(x1, "%a, %d %b %Y %H:%M:%S %Z")
format(x2, "%a, %d %b %Y %H:%M:%S %Z")

## offsets not in whole hours:
x3 <- strptime("2022-01-01", "%Y-%m-%d", tz = "Australia/Adelaide")
format(as.POSIXct(x3), "%a, %d %b %Y %H:%M:%S %z") # +10h30m
# macOS' strftime prints the next two wrong.
# Liberia does/did not have DST, so second abbreviation may be repeat or empty
x4 <- strptime("1971-01-01", "%Y-%m-%d", tz = "Africa/Monrovia")
y4 <- as.POSIXct(x4)
str(unclass(as.POSIXlt(y4))) # correct gmtoff, printed wrong on macOS
format(y4, "%a, %d %b %Y %H:%M:%S %z") # -44m, should be -00:44:30
## timezones in 1900 might not be supported
x5 <- strptime("1900-03-01", "%Y-%m-%d", tz = "Europe/Paris")
y5 <- as.POSIXct(x5)
str(unclass(as.POSIXlt(y5))) # ditto
format(y5, "%a, %d %b %Y %H:%M:%S %z")
