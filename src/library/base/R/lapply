lapply <- function(x, FUN, ...) {
  if (is.character(FUN))
    FUN <- get(FUN, mode = "function")
  if (mode(FUN) != "function")
    stop(paste("\"", FUN, "\" is not a function", sep = " "))
  if (!is.list(x))
    x <- as.list(x)
  rval <- vector("list", length(x))
  for(i in seq(along = x))
    rval[i] <- list(FUN(x[[i]], ...))
  names(rval) <- names(x)               # keep `names' !
  return(rval)
}
