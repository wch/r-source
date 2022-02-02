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
