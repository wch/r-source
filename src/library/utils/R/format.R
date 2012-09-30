#  File src/library/utils/R/format.R
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

formatUL <-
function(x, label = "*", offset = 0,
         width = 0.9 * getOption("width"))
{
    if(!length(x))
        return(character())
    .format_rl_table(label, x, offset, width)
}

formatOL <-
function(x, type = "arabic", offset = 0, start = 1,
         width = 0.9 * getOption("width"))
{
    if(!length(x))
        return(character())
    type_tokens <- c("1", "A", "a", "I", "i")
    type_full_names <- c("arabic", "Alph", "alph", "Roman", "roman")
    type <- match.arg(type, c(type_tokens, type_full_names))
    if(nchar(type, "b") > 1L)
        type <- type_tokens[match(type, type_full_names)]
    len <- length(x)
    labels <- seq.int(start[1L], length.out = len)
    upper <- labels[len]
    if(type %in% c("A", "a")) {
        if(upper > 26L)
            stop(gettextf("too many list items (at most up to %d)", 26L),
                 domain = NA)
        labels <- if(type == "A")
            LETTERS[labels]
        else
            letters[labels]
    }
    else if(type %in% c("I", "i")) {
        if(upper > 3899L)
            stop(gettextf("too many list items (at most up to %d)", 3899L),
                 domain = NA)
        labels <- as.character(as.roman(labels))
        if(type == "i")
            labels <- tolower(labels)
    }
    .format_rl_table(sprintf("%s.", labels), x, offset, width)
}

.format_rl_table <-
function(labels, x, offset = 0, width = 0.9 * getOption("width"),
         sep = " ")
{
    ## Format a 2-column table with right-justified item labels and
    ## left-justified text.  Somewhat tricky because strwrap() eats up
    ## leading whitespace ...

    .make_empty_string <- function(n) {
        paste(rep.int(" ", n), collapse = "")
    }

    labels <- format(labels, justify = "right")
    len <- length(x)
    delta <- nchar(labels[1L], "width") + offset
    x <- strwrap(x, width = width - delta - nchar(sep, "width"),
                 simplify = FALSE)
    nlines <- cumsum(sapply(x, length))
    prefix <- rep.int(.make_empty_string(delta), nlines[len])
    prefix[1L + c(0L, nlines[-len])] <-
        paste0(.make_empty_string(offset), labels)
    paste(prefix, unlist(x), sep = sep)
}
