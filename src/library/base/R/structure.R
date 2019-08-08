#  File src/library/base/R/structure.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2018 The R Core Team
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
#  https://www.R-project.org/Licenses/

## This remaps special names as they are used by deparsing, but why are they?
##
structure <- function (.Data, ...)
{
    if(is.null(.Data))
        warning("Calling 'structure(NULL, *)' is deprecated, as NULL cannot have attributes.\n  Consider 'structure(list(), *)' instead.")
        ## to become: stop("attempt to set an attribute on NULL")
    attrib <- list(...)
    if(length(attrib)) {
        specials <- c(".Dim", ".Dimnames", ".Names", ".Tsp", ".Label")
        attrnames <- names(attrib)
        m <- match(attrnames, specials)
        ok <- !is.na(m)
        if(any(ok)) {
            replace <- c("dim", "dimnames", "names", "tsp", "levels")
            names(attrib)[ok] <- replace[m[ok]]
        }
        ## prior to 2.5.0 factors would deparse to double codes
        if(any(attrib[["class", exact = TRUE]] == "factor")
           && typeof(.Data) == "double")
            storage.mode(.Data) <- "integer"
        attributes(.Data) <- c(attributes(.Data), attrib)
        .Data
    }
    else .Data
}
