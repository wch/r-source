#  File src/library/utils/R/glob2rx.R
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

glob2rx <- function(pattern, trim.head = FALSE, trim.tail = TRUE)
{
    ## Purpose: Change 'ls' aka 'wildcard' aka 'globbing' _pattern_ to
    ##	      Regular Expression (as in grep, perl, emacs, ...)
    ## -------------------------------------------------------------------------
    ## Author: Martin Maechler ETH Zurich, ~ 1991
    ##	       New version using [g]sub() : 2004
    p <- gsub("\\.","\\\\.", paste0("^", pattern, "$"))
    p <- gsub("\\?",	 ".",  gsub("\\*",  ".*", p))
    ## 'Escaping hell' : at least for '(', '[' and '{'
    p <- gsub("([^\\])\\(", "\\1\\\\(", p)
    p <- gsub("([^\\])\\[", "\\1\\\\[", p)
    p <- gsub("([^\\])\\{", "\\1\\\\{", p)
    ## these are trimming ".*$" and "^.*" - in most cases only for aesthetics
    if(trim.tail) p <- sub("\\.\\*\\$$", "", p)
    if(trim.head) p <- sub("\\^\\.\\*",  "", p)
    p
}
