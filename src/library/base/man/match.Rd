\name{match}
\alias{match}
\alias{\%in\%}
\title{Value Matching}
\description{
  \code{match}: If \code{x[i]} is found to equal \code{table[j]}
  then the value returned in the \code{i}-th position of the return value is
  \code{j}.  If no match is found, the value is \code{nomatch}.
  
  \code{\%in\%}: A utility function, currently defined as \cr
  \code{"\%in\%" <- function(x, table) match(x, table, nomatch = 0) > 0}
  allowing an intuitive usage and returning a logical vector of length
  \code{length(x)}.
}
\usage{
match(x, table, nomatch = NA, incomparables = FALSE)
x \%in\% table
}
\arguments{
  \item{x}{the values to be matched.}
  \item{table}{the values to be matched against.}
  \item{nomatch}{the value to be returned in the case when no match is
    found.}
  \item{incomparables}{a vector of values that cannot be matched.  Any
    value in \code{x} matching a value in this vector is assigned the
    \code{nomatch} value.  Currently, \code{FALSE} is the only possible
    value, meaning that all values can be matched.}
}
\seealso{
\code{\link{pmatch}} and \code{\link{charmatch}} for (\emph{partial})
string matching, \code{\link{match.arg}}, etc for function argument matching.
}
\examples{
## The intersection of two sets :
intersect <- function(x, y) y[match(x, y, nomatch = 0)]
intersect(1:10,7:20)

1:10 \%in\% c(1,3,5,9)
sstr <- c("c","ab","B","bba","c","@","bla","a","Ba","\%")
sstr[sstr \%in\% c(letters,LETTERS)]

"\%w/o\%" <- function(x,y) x[!x \%in\% y] #--  x without y
(1:10) \%w/o\% c(3,7,12)
}
\keyword{manip}
\keyword{logic}
