\name{match}
\title{Value Matching}
\usage{
match(x, table, nomatch=NA)
}
\alias{match}
\arguments{
\item{x}{the values to be matched.}
\item{table}{the values to be matched against.}
\item{nomatch}{the value to be returned in the case when no match is found.}}
\description{
If \code{x[i]} is found to equal \code{table[j]}
then the value returned in the \code{i}-th position of the return value is
\code{j}.
If no match is found, the value is \code{nomatch}.
}
\seealso{
\code{\link{pmatch}} for \emph{partial} string matching.
}
\examples{
"\%in\%" <- function(a, b) !is.na(match(a, b))#- following John Wallace
1:10 \%in\% c(1,3,5,9)
sstr <- c("c","ab","B","bba","c","@","bla","a","Ba","\%")
sstr[sstr \%in\% c(letters,LETTERS)]

## The intersection of two sets :
intersect <- function(x, y) y[match(x, y, nomatch = 0)]
intersect(1:10,7:20)
}
