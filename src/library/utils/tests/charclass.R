{
    codepointsToString <- function(x)
        parse(keep.source=FALSE, text=dQuote(q="\"\"", paste0(collapse="",
              sprintf("\\u%04x", as.integer(x)))))[[1]]

    testCharClass <- function(codepoints, class, expected = NULL) {
        stopifnot(is.numeric(codepoints))
        codepoints <- as.integer(codepoints)
        stopifnot(!anyNA(codepoints), all(codepoints > 0))
        if (!is.null(expected))
          stopifnot(length(codepoints) == length(expected),
                    is.logical(expected))

        result <- list()
        result$`charClass(int vs char)` <-
            all.equal(charClass(codepoints, class),
                      charClass(codepointsToString(codepoints), class))
        if (!is.null(expected))
            result$`expected` <- all.equal(expected,
                                           charClass(codepoints, class))
        result <- Filter(Negate(isTRUE), result)
        if (length(result)==0) TRUE else result
    }

    charClasses <- c("alnum", "alpha", "blank", "cntrl", "digit", "graph",
                     "lower", "print", "punct", "space", "upper", "xdigit")
    testCodepoints <- list(
        # "\tAB, ab:3", all ASCII
        ASCII = c(0x0009, 0x0041, 0x0042, 0x002c, 0x0020, 0x0061, 0x0062,
                  0x003a, 0x0033),

        # "Ivan IV", with Ivan in Cyrillic
        Cyrillic = c(0x0418, 0x0432, 0x0430, 0x043d, 0x0020, 0x0049, 0x0056),

        # "Shalom", letters are U+05d0 through U+05ea
        # the others (at 2, 3 and 6) are diacritical marks
        Hebrew = c(0x05E9, 0x05C1, 0x05B8, 0x05DC, 0x05D5, 0x05B9, 0x05DD)) 
                                                                             
    # check for consistency between integer and string inputs
    stopifnot(all(unlist((outer(testCodepoints, charClasses,
        function(x,y) lapply(seq_along(x),
                             function(i) testCharClass(x[[i]],y[i])))))))
}

# spot check return values
{
    stopifnot(all.equal(
        c(TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE),
        charClass(testCodepoints[["ASCII"]], "blank")))
}
{
    stopifnot(all.equal(
        c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE),
        charClass(testCodepoints[["ASCII"]], "punct")))
}
{
    stopifnot(all.equal(
        c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE),
        charClass(testCodepoints[["ASCII"]], "digit")))
}
{
    stopifnot(all.equal(
        c(FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, TRUE),
        charClass(testCodepoints[["ASCII"]], "alnum")))
}

# In principle, this can be locale dependent.
# Ubuntu in C locale (without internal iswxxxxx) gives different results. 

if (Sys.getlocale("LC_CTYPE") != "C") {

    stopifnot(all.equal(
        c(TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE),
        charClass(testCodepoints[["Cyrillic"]], "alpha")))

    stopifnot(all.equal(
        c(TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE),
        charClass(testCodepoints[["Cyrillic"]], "upper")))

    stopifnot(all.equal(
        c(FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE),
        charClass(testCodepoints[["Cyrillic"]], "lower")))

    stopifnot(all.equal(
        c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE),
        charClass(testCodepoints[["Cyrillic"]], "space")))

    # Ubuntu & Windows 10 disagree about diacritacals
    stopifnot(all(
        charClass(testCodepoints[["Hebrew"]], "alpha")[-c(2,3,6)]))

    # no cases in Hebrew alphabet
    stopifnot(!any(charClass(testCodepoints[["Hebrew"]], "lower")))

    # no cases in Hebrew alphabet
    stopifnot(!any(charClass(testCodepoints[["Hebrew"]], "upper")))
}
