#  File src/library/base/R/character.R
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

strsplit <- function(x, split, extended = TRUE, fixed = FALSE, perl = FALSE)
    .Internal(strsplit(x, as.character(split), as.logical(extended),
                       as.logical(fixed), as.logical(perl)))

substr <- function(x, start, stop)
{
    if(!is.character(x)) x <- as.character(x)
    .Internal(substr(x, as.integer(start), as.integer(stop)))
}

substring <- function(text,first,last=1000000)
{
    if(!is.character(text)) x <- as.character(text)
    n <- max(lt <- length(text), length(first), length(last))
    if(lt && lt < n) text <- rep(text, length.out = n)
    substr(text, first, last)
}

`substr<-` <- function(x, start, stop, value)
    .Internal(`substr<-`(x, as.integer(start), as.integer(stop), value))

`substring<-` <- function(text, first, last=1000000, value)
    `substr<-`(text, first, last, value)

abbreviate <-
    function(names.arg, minlength = 4, use.classes = TRUE, dot = FALSE)
{
    ## we just ignore use.classes
    if(minlength <= 0)
	return(rep.int("", length(names.arg)))
    ## need to remove leading/trailing spaces before we check for dups
    ## This is inefficient but easier than modifying do_abbrev (=> FIXME !)
    names.arg <- sub("^ +", "", sub(" +$", "", as.character(names.arg)))
    dups <- duplicated(names.arg)
    old <- names.arg
    if(any(dups))
	names.arg <- names.arg[!dups]
    dup2 <- rep.int(TRUE, length(names.arg))
    x <- these <- names.arg
    repeat {
	ans <- .Internal(abbreviate(these, minlength, use.classes))
	x[dup2] <- ans
	dup2 <- duplicated(x)
	if(!any(dup2))
	    break
	minlength <- minlength+1
	dup2 <- dup2 | match(x, x[dup2], 0)
	these <- names.arg[dup2]
    }
    if(any(dups))
	x <- x[match(old,names.arg)]
    if(dot) { # add "." where we did abbreviate:
        chgd <- x != old
	x[chgd] <- paste(x[chgd],".",sep = "")
    }
    names(x) <- old
    x
}

make.names <- function(names, unique = FALSE, allow_ = TRUE)
{
    names <- .Internal(make.names(as.character(names), allow_))
    if(unique) names <- make.unique(names)
    names
}

make.unique <- function (names, sep = ".") .Internal(make.unique(names, sep))

chartr <- function(old, new, x)
{
    if(!is.character(x)) x <- as.character(x)
    .Internal(chartr(old, new, x))
}
tolower <- function(x) {
    if(!is.character(x)) x <- as.character(x)
    .Internal(tolower(x))
}
toupper <- function(x) {
    if(!is.character(x)) x <- as.character(x)
    .Internal(toupper(x))
}

casefold <- function(x, upper = FALSE)
    if(upper) toupper(x) else tolower(x)

sQuote <- function(x) {
    before <- after <- "'"
    q <- getOption("useFancyQuotes")
    if(!is.null(q)) {
        if(identical(q, TRUE)) {
            li <- l10n_info()
            if(li$"UTF-8") q <- "UTF-8"
            if(!is.null(li$codepage)) {
                if(li$codepage >= 1250 && li$codepage <= 1258) {
                    before <- "\x91"; after <- "\x92"
                } else {
                    z <- iconv(c("\xe2\x80\x98", "\xe2\x80\x99"), "UTF-8", "")
                    before <- z[1]; after <- z[2]
                }
            }
        }
        if(identical(q, "TeX")) {
            before <- "`"; after <- "'"
        }
        if(identical(q, "UTF-8")) {
            before <- "\xe2\x80\x98"; after <- "\xe2\x80\x99"
        }
        if(is.character(q) && length(q) >= 4) {
            before <- q[1]; after <- q[2]
        }
    }
    paste(before, x, after, sep = "")
}

dQuote <- function(x) {
    before <- after <- "\""
    q <- getOption("useFancyQuotes")
    if(!is.null(q)) {
        if(identical(q, TRUE)) {
            li <- l10n_info()
            if(li$"UTF-8") q <- "UTF-8"
            if(!is.null(li$codepage)) {
                if(li$codepage >= 1250 && li$codepage <= 1258) {
                    before <- "\x93"; after <- "\x94"
                } else {
                    z <- iconv(c("\xe2\x80\x9c", "\xe2\x80\x9d"), "UTF-8", "")
                    before <- z[1]; after <- z[2]
                }
            }
        }
        if(identical(q, "TeX")) {
            before <- "``"; after <- "''"
        }
        if(identical(q, "UTF-8")) {
            before <- "\xe2\x80\x9c"; after <- "\xe2\x80\x9d"
        }
        if(is.character(q) && length(q) >= 4) {
            before <- q[3]; after <- q[4]
        }
    }
    paste(before, x, after, sep = "")
}
