method.skeleton <- function (generic, signature, file, external = FALSE) 
{
    fdef <- getGeneric(generic)
    if (is.null(fdef)) {
        fdef <- getFunction(generic, where = topenv(parent.frame()))
    }
    else {
        generic <- fdef@generic
        signature <- matchSignature(signature, fdef)
    }
    if (length(signature) == 0) 
        signature <- "ANY"
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
        paste("    signature(", paste("\"", signature, "\"", 
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
