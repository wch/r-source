rm <-
    function (..., list = character(0), pos = -1, envir = as.environment(pos),
              inherits = FALSE)
{
    names <- sapply(match.call(expand.dots=FALSE)$..., as.character)
    list <- .Primitive("c")(list, names)
    .Internal(remove(list, envir, inherits))
}

remove <- rm
