### Tests of often platform-dependent features of the POSIX[cl]t implementation.

### Expect differences, especially with 32-bit time_t

z <- ISOdate(1890:1912, 1, 10, tz="UTC")
## Rome changed to CET for 1894
as.POSIXlt(z, tz="Europe/Rome")
## Paris changed to PMT for 1892, WET for 1912
(zz <- as.POSIXlt(z, tz="Europe/Paris"))
strftime(zz, "%Y-%m-%d %H:%M:%S %Z")
## The offset was really 00:09:21 until 1911, then 00:00
## Many platforms will give the current offset, +0100
strftime(zz, "%Y-%m-%d %H:%M:%S %z")

## Some platforms give details of the latest conversion.
z <- ISOdate(c(seq(1890, 1940, 5), 1941:1946, 1950), 1, 10, tz="UTC")
as.POSIXlt(z, tz="Europe/Paris")
for(i in seq_along(z)) print(as.POSIXlt(z[i], tz="Europe/Paris"))
for(i in seq_along(z))
    print(strftime(as.POSIXlt(z[i], tz="Europe/Paris"), "%Y-%m-%d %H:%M:%S %z"))

strptime("1920-12-27 08:18:23", "%Y-%m-%d %H:%M:%S", tz="Europe/Paris")
