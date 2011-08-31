#  File src/library/base/R/grep.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

strsplit <-
function(x, split, fixed = FALSE, perl = FALSE, useBytes = FALSE)
    .Internal(strsplit(x, as.character(split), fixed, perl, useBytes))

grep <-
function(pattern, x, ignore.case = FALSE, perl = FALSE,
         value = FALSE, fixed = FALSE, useBytes = FALSE, invert = FALSE)
{
    ## when value = TRUE we return names
    if(!is.character(x)) x <- structure(as.character(x), names=names(x))
    .Internal(grep(as.character(pattern), x, ignore.case, value,
                   perl, fixed, useBytes, invert))
}

grepl <-
function(pattern, x, ignore.case = FALSE, perl = FALSE,
         fixed = FALSE, useBytes = FALSE)
{
    if(!is.character(x)) x <- as.character(x)
    .Internal(grepl(as.character(pattern), x, ignore.case, FALSE,
                    perl, fixed, useBytes, FALSE))
}

sub <-
function(pattern, replacement, x, ignore.case = FALSE,
         perl = FALSE, fixed = FALSE, useBytes = FALSE)
{
    if (!is.character(x)) x <- as.character(x)
     .Internal(sub(as.character(pattern), as.character(replacement), x,
                  ignore.case, perl, fixed, useBytes))
}

gsub <-
function(pattern, replacement, x, ignore.case = FALSE,
         perl = FALSE, fixed = FALSE, useBytes = FALSE)
{
    if (!is.character(x)) x <- as.character(x)
    .Internal(gsub(as.character(pattern), as.character(replacement), x,
                   ignore.case, perl, fixed, useBytes))
}

regexpr <-
function(pattern, text, ignore.case = FALSE, perl = FALSE,
         fixed = FALSE, useBytes = FALSE)
{
    if (!is.character(text)) text <- as.character(text)
    .Internal(regexpr(as.character(pattern), text,
                      ignore.case, perl, fixed, useBytes))
}

gregexpr <-
function(pattern, text, ignore.case = FALSE, perl = FALSE,
         fixed = FALSE, useBytes = FALSE)
{
    if (!is.character(text)) text <- as.character(text)
    .Internal(gregexpr(as.character(pattern), text,
                       ignore.case, perl, fixed, useBytes))
}

grepRaw <-
function(pattern, x, offset = 1L, ignore.case = FALSE, value = FALSE,
         fixed = FALSE, all = FALSE, invert = FALSE)
{
    if (!is.raw(pattern)) pattern <- charToRaw(as.character(pattern))
    if (!is.raw(x)) x <- charToRaw(as.character(x))
    .Internal(grepRaw(pattern, x, offset, ignore.case, fixed, value, all, invert))
}

regexec <-
function(pattern, text, ignore.case = FALSE,
         fixed = FALSE, useBytes = FALSE)
    .Internal(regexec(pattern, text, ignore.case, fixed, useBytes))

agrep <-
function(pattern, x, ignore.case = FALSE, value = FALSE, max.distance = 0.1,
         useBytes = FALSE, fixed = TRUE)
{
    pattern <- as.character(pattern)
    if(!is.character(x)) x <- as.character(x)
    ## behaves like == for NA pattern
    if (is.na(pattern)){
        if (value)
            return(structure(rep.int(NA_character_, length(x)),
                             names = names(x)))
        else
            return(rep.int(NA, length(x)))
    }

    if(!is.character(pattern) || length(pattern) != 1L || !nzchar(pattern))
        stop("'pattern' must be a non-empty character string")

    n <- nchar(pattern, "c")
    if(is.na(n)) stop("invalid multibyte string for 'pattern'")
    if(!is.list(max.distance)) {
        if(!is.numeric(max.distance) || (max.distance < 0))
            stop("'max.distance' must be non-negative")
        if(max.distance < 1)            # transform percentages
            max.distance <- ceiling(n * max.distance)
        max.insertions <- max.deletions <- max.substitutions <-
            max.distance
    } else {
        ## partial matching
        table <- c("all", "deletions", "insertions", "substitutions")
        ind <- pmatch(names(max.distance), table)
        if(any(is.na(ind)))
            warning("unknown match distance components ignored")
        max.distance <- max.distance[!is.na(ind)]
        names(max.distance) <- table[ind]
        ## sanity checks
        comps <- unlist(max.distance)
        if(!all(is.numeric(comps)) || any(comps < 0))
            stop("'max.distance' components must be non-negative")
        ## extract restrictions
        if(is.null(max.distance$all))
            max.distance$all <- 0.1
        max.insertions <- max.deletions <- max.substitutions <-
            max.distance$all
        if(!is.null(max.distance$deletions))
            max.deletions <- max.distance$deletions
        if(!is.null(max.distance$insertions))
            max.insertions <- max.distance$insertions
        if(!is.null(max.distance$substitutions))
            max.substitutions <- max.distance$substitutions
        max.distance <- max.distance$all
        ## transform percentages
        if(max.distance < 1)
            max.distance <- ceiling(n * max.distance)
        if(max.deletions < 1)
            max.deletions <- ceiling(n * max.deletions)
        if(max.insertions < 1)
            max.insertions <- ceiling(n * max.insertions)
        if(max.substitutions < 1)
            max.substitutions <- ceiling(n * max.substitutions)
    }

    .Internal(agrep(pattern, x, ignore.case, value, max.distance,
                    max.deletions, max.insertions, max.substitutions,
                    useBytes, fixed))
}

regmatches <-
function(x, m, invert = FALSE)
{
    if(length(x) != length(m))
        stop(gettextf("%s and %s must have the same length",
                      sQuote("x"), sQuote("m")),
             domain = NA)

    ili <- is.list(m)

    ## Handle useBytes/encoding issues.
    ## For regexpr() and gregexpr(), we get useBytes as TRUE if useBytes
    ## was given as TRUE, or all character string involved were ASCII.
    ## Hence, if useBytes is TRUE, we need to convert non-ASCII strings
    ## to a bytes encoding before computing match substrings.
    useBytes <- if(ili)
        any(unlist(lapply(m, attr, "useBytes")))
    else
        any(attr(m, "useBytes"))
    if(useBytes) {
        ## Cf. tools::showNonASCII():
        asc <- iconv(x, "latin1", "ASCII")
        ind <- is.na(asc) | (asc != x)
        ## Alternatively, could do as in tools:::.is_ASCII().
        if(any(ind))
            Encoding(x[ind]) <- "bytes"
    }

    ## What should we do about NA matches (from matching a non-NA
    ## pattern on an NA string)?  For now, let us always "drop" them so
    ## that extracting direct and inverse matches always gives nothing.

    if(!ili && !invert) {
        so <- m[ind <- (!is.na(m) & (m > -1L))]
        eo <- so + attr(m, "match.length")[ind] - 1L
        return(substring(x[ind], so, eo))
    }

    y <- if(invert) {
        Map(function(u, so, ml) {
            if((n <- length(so)) == 1L) {
                if(is.na(so))
                    return(character())
                else if(so == -1L)
                    return(u)
            }
            beg <- if(n > 1L) {
                ## regexec() could give overlapping matches.
                ## Matches are non-overlapping iff
                ##   eo[i] < so[i + 1], i = 1, ..., n - 1.
                eo <- so + ml - 1L
                if(any(eo[-n] >= so[-1L]))
                    stop(gettextf("need non-overlapping matches for %s",
                                  sQuote("invert = TRUE")),
                         domain = NA)
                c(1L, eo + 1L)
            } else {
                c(1L, so + ml)
            }
            end <- c(so - 1L, nchar(u))
            substring(u, beg, end)
        },
            x, m,
            if(ili)
            lapply(m, attr, "match.length")
            else
            attr(m, "match.length"),
            USE.NAMES = FALSE)
    } else {
        Map(function(u, so, ml) {
            if(length(so) == 1L) {
                if(is.na(so) || (so == -1L))
                    return(character())
            }
            substring(u, so, so + ml - 1L)
        },
            x, m,
            lapply(m, attr, "match.length"),
            USE.NAMES = FALSE)
    }

    names(y) <- names(x)
    y
}
