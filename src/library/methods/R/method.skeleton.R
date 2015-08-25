#  File src/library/methods/R/method.skeleton.R
#  Part of the R package, https://www.R-project.org
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
#  https://www.R-project.org/Licenses/

method.skeleton <- function (generic, signature, file, external = FALSE,
			     where = topenv(parent.frame()))
{
    fdef <- getGeneric(generic, where = where)
    if (is.null(fdef)) {
        fdef <- implicitGeneric(generic, where = where)
        if(is.null(fdef))
            stop(gettextf("no function definition found for %s",
                          sQuote(generic)),
                 domain = NA)
    }
    else {
        generic <- fdef@generic
    }
    signature <- matchSignature(signature, fdef)
    if (length(signature) == 0)
        signature <- "ANY"
    sigNames <- fdef@signature
    length(sigNames) <- length (signature)
    method <- function() {
    }
    formals(method) <- formals(fdef)
    body(method) <- quote({
        stop("need a definition for the method here")
    })
    methodName <- paste(c(generic, signature), collapse = "_")
    if (missing(file))
        file <- paste0(methodName, ".R")
    output <- c(paste0("setMethod(\"", generic, '",'),
		paste0("    signature(", paste0(sigNames, ' = "', signature, '"',
						collapse = ", "), "),"))
    method <- deparse(method)
    if (identical(external, FALSE))
        output <- c(output, paste0("    ", method), ")")
    else {
        if(is(external, "character") )
            methodName <- toString(external)
        method[[1L]] <- paste0("`", methodName, "` <- ", method[[1L]])
        output <- c(method, "", output, paste0("  `", methodName, "`)"))
    }
    writeLines(output, file)
    message(gettextf("Skeleton of method written to %s",
                     if (is.character(file)) file else "connection"),
            domain = NA)
    invisible(file)
}
