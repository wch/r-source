\name{apply}
\title{Apply Functions Over Array Margins}
\usage{
apply(x, MARGIN, FUN, \dots)
}
\alias{apply}
\arguments{
  \item{x}{the array to be used.}
  \item{MARGIN}{a vector giving the subscripts which the function will
    be applied over.
    \code{1} indicates rows, \code{2} indicates columns,
    \code{c(1,2)} indicates rows and columns.}
  \item{FUN}{the function to be applied.
    In the case of functions like \code{+}, \code{\%*\%}, etc., the
    function name must be quoted.}
  \item{\dots}{optional arguments to \code{FUN}.}
}
\value{
  If each call to \code{FUN} returns a vector of length \code{n}, then
  \code{apply} returns an array of dimension \code{c(n,dim(x)[MARGIN])}
  if \code{n > 1}.  If \code{n} equals \code{1}, \code{apply} returns a
  vector if \code{MARGIN} has length 1 and an array of dimension
  \code{dim(x)[MARGIN]} otherwise.

  If the calls to \code{FUN} return vectors of different lengths,
  \code{apply} returns a list of length \code{dim(x)[MARGIN]}.
}
\seealso{
  \code{\link{lapply}}, \code{\link{tapply}}, \code{\link{sweep}}.
}
\examples{
## Compute row and column sums for a matrix:
x <- cbind(x1 = 3, x2 = c(4:1, 2:5))
dimnames(x)[[1]] <- letters[1:8] 
apply(x, 2, mean, trim = .2)
col.sums <- apply(x, 2, sum)
row.sums <- apply(x, 1, sum)
rbind(cbind(x, Rtot = row.sums), Ctot = c(col.sums, sum(col.sums)))

all( apply(x,2, is.vector)) # TRUE [was not in R <= 0.63.2]

## Sort the columns of a matrix
apply(x, 2, sort)

##- function with extra args:
cave <- function(x, c1,c2) c(mean(x[c1]),mean(x[c2]))
apply(x,1, cave,  c1="x1", c2=c("x1","x2"))

ma <- matrix(c(1:4, 1, 6:8), nr = 2)
ma
apply(ma, 1, table)  #--> a list of length 2
apply(ma, 1, quantile)# 5 x n matrix with rownames

all(dim(ma) == dim(apply(ma, 1:2, sum)))## apply BUG [R <= 0.63.0]


\testonly{
 apply(x,  2, summary) # 6 x n matrix
 apply(x,  1, quantile)# 5 x n matrix

 d.arr <- 2:5
 arr <- array(1:prod(d.arr), d.arr,
	  list(NULL,letters[1:d.arr[2]],NULL,paste("V",4+1:d.arr[4],sep="")))
 all(apply(arr, 1:2, sum) == t(apply(arr, 2:1, sum)))

 marg <- list(1:2, 2:3, c(2,4), c(1,3), 2:4, 1:3, 1:4)
 for(m in marg) print(apply(arr, print(m), sum))
 for(m in marg) print(dim(apply(arr, print(m), quantile)) == c(5,d.arr[m]))
}
}
\keyword{iteration}
\keyword{array}
