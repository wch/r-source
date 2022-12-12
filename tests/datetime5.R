### tests of strftime (formatting POSIXlt objects).

Sys.setenv(TZ = "Europe/Rome")

dt <- as.POSIXct("2022-12-11 09:03;04")

ff <- c(LETTERS, letters)
ff <- setdiff(c(LETTERS, letters),
              c("E", "J", "K", "L", "N", "O", "Q",
                "f", "i", "k", "l",  "o", "q",
                "r", # %r is locale- and implementation-dependent.
                "s", "v")
              )

for (f in ff) {
    f <- paste0("%", f)
    cat(sprintf("%s: %s\n", f, format(dt, f)))
}

## Natome macOS does not implement %P

## 'not POSISX but widely implemented'
for (f in c("k", "l", "s")) {
    f <- paste0("%", f)
    cat(sprintf("%s: %s\n", f, format(dt, f)))
}

