as.roman <-
function(x)
{
    if(is.numeric(x))
        x <- as.integer(x)
    else if(is.character(x)) {
        ## Let's be nice: either strings that are *all* arabics, or
        ## (hopefully, for the time being) all romans.
        x <- if(all(regexpr("^[[:digit:]]+$", x) > -1))
            as.integer(x)
        else
            .roman2numeric(x)
    }
    else
        stop("cannot coerce 'x' to roman")
    x[(x <= 0 | x >= 3900)] <- NA
    class(x) <- "roman"
    x
}

as.character.roman <-
function(x, ...)
    .numeric2roman(x)
format.roman <-
function(x, ...)
    format(as.character(x))
print.roman <-
function(x, ...)
{
    print(noquote(as.character(x)))
    x
}
"[.roman" <-
function(x, i)
{
    cl <- oldClass(x)
    y <- NextMethod("[")
    oldClass(y) <- cl
    y
}

.numeric2roman <-
function(x) {
    romans <- c("M", "CM", "D", "CD", "C", "XC", "L", "XL", "X", "IX",
                "V", "IV", "I")
    numbers <- c(1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1)
    n2r <- function(z) {
        y <- character()
        for(i in seq_along(romans)) {
            d <- numbers[i]
            while(z >= d) {
                z <- z - d
                y <- c(y, romans[i])
            }
        }
        paste(y, collapse = "")
    }

    out <- character(length(x))
    x <- as.integer(x)
    ind <- is.na(x) | (x <= 0) | (x >= 3900)
    out[ind] <- NA
    if(any(!ind))
        out[!ind] <- sapply(x[!ind], n2r)
    out
}

.roman2numeric <-
function(x)
{
    ## <FIXME>
    ## What if this fails?
    ## Should say something like "Not a valid roman number ..."
    ## </FIXME>
    romans <- c("M", "CM", "D", "CD", "C", "XC", "L", "XL", "X", "IX",
                "V", "IV", "I")
    numbers <- c(1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1)
    out <- integer(length(x))
    ind <- is.na(x)
    out[ind] <- NA
    if(any(!ind)) {
        y <- toupper(x[!ind])
        y <- gsub("CM", "DCCCC", y)
        y <- gsub("CD", "CCCC", y)
        y <- gsub("XC", "LXXXX", y)
        y <- gsub("XL", "XXXX", y)
        y <- gsub("IX", "VIIII", y)
        y <- gsub("IV", "IIII", y)
        ok <- (regexpr("^M{,3}D?C{,4}L?X{,4}V?I{,4}$", y) > -1)
        if(any(!ok)) {
            warning(gettextf("Invalid roman numeral(s): %s",
                             paste(x[!ind][!ok], collapse = " ")))
            out[!ind][!ok] <- NA
        }
        if(any(ok))
            out[!ind][ok] <-
                sapply(strsplit(y[ok], ""),
                       function(z)
                       as.integer(sum(numbers[match(z, romans)])))
    }
    out
}
