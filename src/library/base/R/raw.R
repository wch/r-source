raw <- function(length = 0) vector("raw", length)

as.raw <- function(x) as.vector(x, "raw")

charToRaw <- function(x) .Internal(charToRaw(x))
rawToChar <- function(x, multiple=FALSE) .Internal(rawToChar(x, multiple))
