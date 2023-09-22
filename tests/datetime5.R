### tests of strftime (formatting POSIXlt objects).

Sys.setenv(TZ = "Europe/Rome")

dt <- as.POSIXlt("2022-12-11 09:03;04")

ff <- c(LETTERS, letters)
ff <- setdiff(c(LETTERS, letters),
              c("E", "J", "K", "L", "N", "P", "O", "Q",
                "f", "i", "k", "l",  "o", "q",
                "r", # %r is locale- and implementation-dependent.
                "s", "v")
              )

for (f in ff) {
    f <- paste0("%", f)
    cat(sprintf("%s: %s\n", f, format(dt, f)))
}

## 'not in the standards and less widely implemented'
## %P is a glibc extension which we added to IANA tzcode for R. Not in macOS.
for (f in c("P", "k", "l", "s")) {
    f <- paste0("%", f)
    cat(sprintf("%s: %s\n", f, format(dt, f)))
}

## week numbers
dt2 <- as.POSIXlt(sprintf("%d-01-01 09:03;04", 2015:2018))
cat(format(dt2, "%Y: %U %V %W"), sep = "\n")

