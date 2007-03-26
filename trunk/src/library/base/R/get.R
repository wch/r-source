get <-
    function (x, pos = -1, envir = as.environment(pos), mode = "any",
              inherits = TRUE)
    .Internal(get(x, envir, mode, inherits))

mget <- function(x, envir, mode = "any",
          ifnotfound= list(function(x)
	                stop(paste("value for '", x, "' not found", sep=""),
				call.=FALSE)),
          inherits = FALSE)
     .Internal(mget(x, envir, mode, ifnotfound, inherits))
