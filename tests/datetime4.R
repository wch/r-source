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
x5 <- strptime("2022-07-01", "%Y-%m-%d", tz ="Pacific/Kiritimati")
x5
str(unclass(x5)) # does not have DST, hence no DST abbreviation on some platforms

## edge of range and out of range offsets
strptime("2022-01-01 +1400", "%Y-%m-%d %z", tz = "UTC")
strptime("2022-01-01 -1400", "%Y-%m-%d %z", tz = "UTC")
strptime("2022-01-01 +1500", "%Y-%m-%d %z", tz = "UTC")
strptime("2022-01-01 -1500", "%Y-%m-%d %z", tz = "UTC")
