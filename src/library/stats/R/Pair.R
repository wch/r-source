###
### S3 class for paired data for use in formula interface to t.test
### and wilcox.test
###
Pair <- function(x,y) {
  pp <- cbind(x,y) ## a data frame might be more natural, 
                   ## but model.frame doesn't like it
  class(pp) <- "Pair"
  pp
}
`[.Pair` <- function (x, i, j, drop = FALSE) 
{
    if (missing(j)) {
        x <- unclass(x)[i, , drop = FALSE]
        class(x) <- "Pair"
        x
    }
    else {
        class(x) <- "matrix"
        NextMethod("[")
    }
}

