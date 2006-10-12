\name{match}
\alias{match}
\alias{\%in\%}
\title{Value Matching}
\description{
  \code{match} returns a vector of the positions of (first) matches of
  its first argument in its second.

  \code{\%in\%} is a more intuitive interface as a binary operator,
  which returns a logical vector indicating if there is a match or not
  for its left operand.
}
\usage{
match(x, table, nomatch = NA, incomparables = FALSE)

x \%in\% table
}
\arguments{
  \item{x}{vector or \code{NULL}: the values to be matched.}
  \item{table}{vector or \code{NULL}: the values to be matched against.}
  \item{nomatch}{the value to be returned in the case when no match is
    found.  Note that it is coerced to \code{integer}.}
  \item{incomparables}{a vector of values that cannot be matched.  Any
    value in \code{x} matching a value in this vector is assigned the
    \code{nomatch} value.  Currently, \code{FALSE} is the only possible
    value, meaning that all values can be matched.}
}
\value{
  A vector of the same length as \code{x}.

  \code{match}: An integer vector giving the position in \code{table} of
  the first match if there is a match, otherwise \code{nomatch}.

  If \code{x[i]} is found to equal \code{table[j]} then the value
  returned in the \code{i}-th position of the return value is \code{j},
  for the smallest possible \code{j}.  If no match is found, the value
  is \code{nomatch}.

  \code{\%in\%}: A logical vector, indicating if a match was located for
  each element of \code{x}.
}
\details{
  \code{\%in\%} is currently defined as \cr
  \code{"\%in\%" <- function(x, table) match(x, table, nomatch = 0) > 0}

  Factors, raw vectors and lists are converted to character vectors, and
  then \code{x} and \code{table} are coerced to a common type (the later
  of the two types in R's ordering, logical < integer < numeric <
  complex < character) before matching.

  Matching for lists is potentially very slow and best avoided except in
  simple cases.

  Exactly what matches what is to some extent a matter of definition.
  For all types, \code{NA} matches \code{NA} and no other value.
  For real and complex values, \code{NaN} values are regarded
  as matching any other \code{NaN} value, but not matching \code{NA}.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\seealso{
  \code{\link{pmatch}} and \code{\link{charmatch}} for (\emph{partial})
  string matching, \code{\link{match.arg}}, etc for function argument
  matching.
  \code{\link{findInterval}} similarly returns a vector of positions, but
  finds numbers within intervals, rather than exact matches.

  \code{\link{is.element}} for an S-compatible equivalent of \code{\%in\%}.
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
