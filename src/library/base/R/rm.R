rm <-
    function (..., list = character(0), pos = -1, envir = as.environment(pos),
              inherits = FALSE)
{
    dots <- match.call(expand.dots=FALSE)$...
    if(length(dots) &&
       !all(sapply(dots, function(x) is.symbol(x) || is.character(x))))
       stop("... must contain names or character strings")
    names <- sapply(dots, as.character)
    if (length(names) == 0) names <- character(0)
    list <- .Primitive("c")(list, names)
    .Internal(remove(list, envir, inherits))
}

remove <- rm
