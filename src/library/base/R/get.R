get <-
    function (x, pos = -1, envir = as.environment(pos), mode = "any",
              inherits = TRUE)
    .Internal(get(x, envir, mode, inherits))
