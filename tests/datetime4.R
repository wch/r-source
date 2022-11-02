### more date-time tests where the output is to be checked.

## Expect differences, especially for platforms without tm_zone/tm_gmtoff.

options(warn = 1L)

## tests of "POSIXlt" objects
xU <- strptime("2022-01-01", "%Y-%m-%d", tz = "UTC")
xU
str(unclass(xU))

x0 <- strptime("2022-01-01", "%Y-%m-%d")
x0
str(unclass(x0)) # no tzone attribute

x1 <- strptime("2022-07-01", "%Y-%m-%d", tz = "Europe/London")
x1
str(unclass(x1)) # gmtoff is NA

## nonsense offset  value
x2 <- strptime("2022-01-01 +0300", "%Y-%m-%d %z", tz = "Europe/London")
x2 # no tzone printed
str(unclass(x2))
x3 <- as.POSIXlt(as.POSIXct(x2))
x3
str(unclass(x3)) # uses wrong gmtoff.

x4 <- strptime("2022-07-01", "%Y-%m-%d", tz ="Pacific/Fiji")
x4
str(unclass(x4)) # abbreviations may be numbers.
x5 <- strptime("2022-07-01", "%Y-%m-%d", tz ="Pacific/Kiritimati")
x5
str(unclass(x5)) # does not have DST, hence no DST abbreviation on some platforms

## edge of range and out of range offsets
strptime("2022-01-01 +1400", "%Y-%m-%d %z", tz = "Europe/London")
strptime("2022-01-01 -1400", "%Y-%m-%d %z", tz = "Europe/London")
strptime("2022-01-01 +1500", "%Y-%m-%d %z", tz = "Europe/London")
strptime("2022-01-01 -1500", "%Y-%m-%d %z", tz = "Europe/London")
