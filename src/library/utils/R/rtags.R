#  File src/library/tools/R/rtags.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2012 The R Core Team
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



### These utilities are intended to read R code files and produce tags
### in Emacs' etags format.  Doing so in R allows us to use R's
### parser.  Support for vi-style tags could be useful, but it needs
### the tags file needs to be sorted, making file-by-file processing
### difficult. It may be easier to write a script to convert an etags
### format file (see http://http://en.wikipedia.org/wiki/Ctags).



### * shorten.to.string

## The etags format requires the initial part of a matching line to be
## recorded in the TAGS file, with an optional entry for the `token'
## that is to be matched.  Exact matches to the token are preferred,
## but it seems that subsequent non-exact matches look in the initial
## string as well as other tokens with equal priority.  Such matches
## seem pointless, so an attempt is made to shorten the matching line.
## It is not clear to me whether there are restrictions on what this
## part could be, but a completely blank string doesn't seem to work.
## For now, I'm just keeping the first letter. May change if problems
## arise.

shorten.to.string <-
    function(line, token)
{
    if (FALSE) {
        ans <- regexpr(strsplit("token", ",", fixed = TRUE)[[1L]][1L],
                       line, fixed = TRUE)
        if (ans == -1L) line
        else substr(line, 1L, ans + attr(ans, "match.length") - 1L)
    }
    else {
        ## can we just put essentially nothing? Seems to work
        substr(line, 1L, 1L)
    }
}


### * write.etags

## this function is responsible for formatting the output for a single
## file given the relevant information.  The format was inferred from
## the "Ctags" wikipedia entry and by studying etags output.

write.etags <-
    function(src,
             tokens, startlines, lines, nchars,
             ...,
             shorten.lines = c("token", "simple", "none"))
{
    ## extra 1 for newline
    shorten.lines <- match.arg(shorten.lines)
    offsets <- (cumsum(nchars + 1L) - (nchars + 1L))[startlines]
    lines <-
        switch(shorten.lines,
               none = lines,
               simple = sapply(strsplit(lines, "function", fixed = TRUE), "[", 1),
               token = mapply(shorten.to.string, lines, tokens))
    tag.lines <-
        paste(sprintf("%s\x7f%s\x01%d,%d",
                      lines, tokens, startlines,
                      as.integer(offsets)),
              collapse = "\n")
    ## simpler format: tag.lines <- paste(sprintf("%s\x7f%d,%d", lines, startlines, as.integer(offsets)), collapse = "\n")
    tagsize <- nchar(tag.lines, type = "bytes") + 1L
    cat("\x0c\n", src, ",", tagsize, "\n", tag.lines, "\n", sep = "", ...)
}


### * expr2token

## this computes the tag name from an expression.  Currently, this
## returns the second thing in the expression; so
##
##   foo <- function(x) ...      ==> `<-`,       foo, ...
##   setMethod("foo", "bar" ...  ==> setMethod,  foo, ...
##   setGeneric("foo", "bar" ... ==> setGeneric, foo, ...
##
## which covers the typical uses.  We match against a list to restrict
## types of expressions that are tagged.  To reject things like
##
##   x[i] <- 10
##
## the second component is required to have length 1.  One limitation
## is that things like
##
##   if (require(pkg)) foo <- ... else foo <- ...
##
## will not be handled.

expr2token <-
    function(x,
             ok = c("<-", "=", "<<-", "assign",
                    "setGeneric", "setGroupGeneric", "setMethod",
                    "setClass", "setClassUnion"),
             extended = TRUE)
{
    id <- ""
    value <-
        if ((length(x) > 1L) &&
            (length(token <- as.character(x[[2L]])) == 1L) &&
            (length(id <- as.character(x[[1L]])) == 1L) &&
            (id %in% ok)) token
        else
            character(0L)
    if (extended && identical(id, "setMethod"))
    {
        ## try to add the signature, comma separated
        sig <- tryCatch(eval(x[[3L]]), error = identity)
        if (!inherits(sig, "error") && is.character(sig))
            value <- paste(c(value, sig), collapse=",")
    }
    value
}


### * rtags.file

## Handles a single file

rtags.file <-
    function(src, ofile = "", append = FALSE,
             write.fun = write.etags) ## getOption("writeTags")
{

    ## FIXME: do we need to worry about encoding etc.?
    elist <- parse(src, srcfile = srcfile(src))
    if (length(elist) == 0) return(invisible())
    lines <- readLines(src)
    tokens <- lapply(elist, expr2token)
    startlines <- sapply(attr(elist, "srcref"), "[", 1L)
    if (length(tokens) != length(startlines))
        stop("length mismatch: bug in code!", domain = NA)
    keep <- sapply(tokens, length) == 1L
    if (!any(keep)) return(invisible())
    tokens <- unlist(tokens[keep])
    startlines <- startlines[keep]
    write.fun(src = src,
              tokens = tokens,
              startlines = startlines,
              lines = lines[startlines],
              nchars = nchar(lines, type = "bytes"),
              file = ofile, append = append)
}

### * rtags

## Public interface.  Tags files under a specified directory, using
## regular expressions to filter out inappropriate files.


rtags <-
    function(path = ".", pattern = "\\.[RrSs]$",
             recursive = FALSE,
             src = list.files(path = path,
                              pattern = pattern,
                              full.names = TRUE,
                              recursive = recursive),
             keep.re = NULL,
             ofile = "", append = FALSE,
             verbose = getOption("verbose"))
{
    if (ofile != "" && !append) {
        if (!file.create(ofile, showWarnings = FALSE)) 
            stop(gettextf("Could not create file %s, aborting", ofile),
                 domain = NA)
    }
    if (!missing(keep.re))
        src <- grep(keep.re, src, value = TRUE)
    for (s in src)
    {
        if (verbose) message(gettextf("Processing file %s", s), domain = NA)
        tryCatch(
                 rtags.file(s, ofile = ofile, append = TRUE),
                 error = function(e) NULL)
    }
    invisible()
}




### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "### [*]+" ***
### End: ***


