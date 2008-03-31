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

grep <-
function(pattern, x, ignore.case = FALSE, extended = TRUE, perl = FALSE,
         value = FALSE, fixed = FALSE, useBytes = FALSE)
{
    ## when value = TRUE we return names
    if(!is.character(x)) x <- structure(as.character(x), names=names(x))
    .Internal(grep(as.character(pattern), x, ignore.case, extended, value,
                   perl, fixed, useBytes))
}

sub <-
function(pattern, replacement, x, ignore.case = FALSE, extended = TRUE,
         perl = FALSE, fixed = FALSE, useBytes = FALSE)
{
    if (!is.character(x)) x <- as.character(x)
     .Internal(sub(as.character(pattern), as.character(replacement), x,
                  ignore.case, extended, perl, fixed, useBytes))
}

gsub <-
function(pattern, replacement, x, ignore.case = FALSE, extended = TRUE,
         perl = FALSE, fixed = FALSE, useBytes = FALSE)
{
    if (!is.character(x)) x <- as.character(x)
    .Internal(gsub(as.character(pattern), as.character(replacement), x,
                   ignore.case, extended, perl, fixed, useBytes))
}

regexpr <-
function(pattern, text, ignore.case = FALSE, extended = TRUE, perl = FALSE,
         fixed = FALSE, useBytes = FALSE)
    .Internal(regexpr(as.character(pattern), as.character(text),
                      ignore.case, extended, perl, fixed, useBytes))

gregexpr <-
function(pattern, text, ignore.case = FALSE, extended = TRUE, perl = FALSE,
         fixed = FALSE, useBytes = FALSE)
    .Internal(gregexpr(as.character(pattern), as.character(text),
                       ignore.case, extended, perl, fixed, useBytes))

agrep <-
function(pattern, x, ignore.case = FALSE, value = FALSE, max.distance = 0.1,
         useBytes = FALSE)
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

    if(!is.character(pattern) || length(pattern) != 1 || !nzchar(pattern))
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
                    max.deletions, max.insertions, max.substitutions, useBytes))
}
