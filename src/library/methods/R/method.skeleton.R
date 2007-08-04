#  File src/library/methods/R/method.skeleton.R
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

method.skeleton <- function (generic, signature, file, external = FALSE, where = topenv(parent.frame())) 
{
    fdef <- getGeneric(generic, where = where)
    if (is.null(fdef)) {
        fdef <- implicitGeneric(generic, where = where)
        if(is.null(fdef))
          stop(gettextf("No function definition found for \"%s\"", generic))
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
        stop("Need a definition for the method here")
    })
    methodName <- paste(c(generic, signature), collapse = "_")
    if (missing(file)) 
        file <- paste(methodName, ".R", sep = "")
    output <- c(paste("setMethod(\"", generic, "\",", sep = ""), 
        paste("    signature(", paste(sigNames, " = \"", signature, "\"", 
            sep = "", collapse = ", "), "),", sep = ""))
    method <- deparse(method)
    if (identical(external, FALSE)) 
        output <- c(output, paste("    ", method, sep = ""), 
            ")")
    else {
        if(is(external, "character") )
          methodName <- toString(external)
        method[[1]] <- paste("`", methodName, "` <- ", method[[1]], 
            sep = "")
        output <- c(method, "", output, paste("  `", methodName, 
            "`)", sep = ""))
    }
    writeLines(output, file)
    message("Skeleton of method written to ", if (is.character(file)) 
        file
    else "connection")
    invisible(file)
}
