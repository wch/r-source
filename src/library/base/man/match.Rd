% File src/library/base/man/match.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2016 R Core Team
% Distributed under GPL 2 or later

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
match(x, table, nomatch = NA_integer_, incomparables = NULL)

x \%in\% table
}
\arguments{
  \item{x}{vector or \code{NULL}: the values to be matched.
    \link{Long vectors} are supported.}
  \item{table}{vector or \code{NULL}: the values to be matched against.
    \link{Long vectors} are not supported.}
  \item{nomatch}{the value to be returned in the case when no match is
    found.  Note that it is coerced to \code{integer}.}
  \item{incomparables}{a vector of values that cannot be matched.  Any
    value in \code{x} matching a value in this vector is assigned the
    \code{nomatch} value.  For historical reasons, \code{FALSE} is
    equivalent to \code{NULL}.}
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
  each element of \code{x}: thus the values are \code{TRUE} or
  \code{FALSE} and never \code{NA}.
}
\details{
  \code{\%in\%} is currently defined as \cr
  \code{"\%in\%" <- function(x, table) match(x, table, nomatch = 0) > 0}

  Factors, raw vectors and lists are converted to character vectors, and
  then \code{x} and \code{table} are coerced to a common type (the later
  of the two types in \R's ordering, logical < integer < numeric <
  complex < character) before matching.  If \code{incomparables} has
  positive length it is coerced to the common type.

  Matching for lists is potentially very slow and best avoided except in
  simple cases.

  Exactly what matches what is to some extent a matter of definition.
  For all types, \code{NA} matches \code{NA} and no other value.
  For real and complex values, \code{NaN} values are regarded
  as matching any other \code{NaN} value, but not matching \code{NA}.

  Character strings will be compared as byte sequences if any input is
  marked as \code{"bytes"}, and otherwise are regarded as equal if they are
  in different encodings but would agree when translated to UTF-8 (see
  \code{\link{Encoding}}.

  That \code{\%in\%} never returns \code{NA} makes it particularly
  useful in \code{if} conditions.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\seealso{
  \code{\link{pmatch}} and \code{\link{charmatch}} for (\emph{partial})
  string matching, \code{\link{match.arg}}, etc for function argument
  matching.
  \code{\link{findInterval}} similarly returns a vector of positions, but
  finds numbers within intervals, rather than exact matches.

  \code{\link{is.element}} for an S-compatible equivalent of \code{\%in\%}.

  \code{\link{unique}} (and \code{\link{duplicated}}) are using the same
  definitions of \dQuote{match} or \dQuote{equality} as \code{match()},
  and these are less strict than \code{\link{==}}, e.g., for
  \code{\link{NA}} and \code{\link{NaN}} in numeric or complex vectors,
  or for strings with different encodings, see also above.
}
\examples{
## The intersection of two sets can be defined via match():
## Simple version:
## intersect <- function(x, y) y[match(x, y, nomatch = 0)]
intersect # the R function in base is slightly more careful
intersect(1:10, 7:20)

1:10 \%in\% c(1,3,5,9)
sstr <- c("c","ab","B","bba","c",NA,"@","bla","a","Ba","\%")
sstr[sstr \%in\% c(letters, LETTERS)]

"\%w/o\%" <- function(x, y) x[!x \%in\% y] #--  x without y
(1:10) \%w/o\% c(3,7,12)
## Note that setdiff() is very similar and typically makes more sense:
        c(1:6,7:2) \%w/o\% c(3,7,12)  # -> keeps duplicates
setdiff(c(1:6,7:2),      c(3,7,12)) # -> unique values

## Illuminating example about NA matching
r <- c(1, NA, NaN)
zN <- c(complex(real = NA , imaginary =  r ), complex(real =  r , imaginary = NA ),
        complex(real =  r , imaginary = NaN), complex(real = NaN, imaginary =  r ))
zM <- cbind(Re=Re(zN), Im=Im(zN), match = match(zN, zN))
rownames(zM) <- format(zN)
zM ##--> only two different matches: NA or non-NA NaN

length(zN) # 12
unique(zN) # only two: the "NA" and the first non-NA NaN:
stopifnot(identical(unique(zN), zN[c(1,7)]))

## very strict equality would have 4 duplicates (of 12):
symnum(outer(zN, zN, Vectorize(identical,c("x","y")),
                     FALSE,FALSE,FALSE,FALSE))
## removing "(very strictly) duplicates",
i <- c(5,8,11,12)  # we get 8 pairwise non-identicals :
Ixy <- outer(zN[-i], zN[-i], Vectorize(identical,c("x","y")),
                     FALSE,FALSE,FALSE,FALSE)
stopifnot(identical(Ixy, diag(8) == 1))
}
\keyword{manip}
\keyword{logic}
