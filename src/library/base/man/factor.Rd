\name{factor}
\title{Factors}
\usage{
factor(x, levels = sort(unique(x), na.last = TRUE), labels,
       exclude = NA, ordered = FALSE)
ordered(x, levels = sort(unique(x), na.last = TRUE), labels,
       exclude = NA, ordered = TRUE)

is.factor(x)
is.ordered(x)
is.unordered(x)

as.factor(x, ordered = FALSE)
as.ordered(x)
}
\alias{factor}
\alias{ordered}
\alias{is.factor}
\alias{is.ordered}
\alias{is.unordered}
\alias{as.factor}
\alias{as.ordered}
\value{
  The function \code{factor} is used to encode a vector as a factor (the
  names category and enumerated type are also used for factors).  If
  \code{ordered} is \code{TRUE}, the factor levels are assumed ordered.
  By default the levels are unordered.  For compatibility purposes,
  there is also a function called \code{ordered} which provides an
  alternative way of creating ordered factors.

  The encoding of the vector happens as follows: if \code{x[i]} equals
  \code{levels[j]}, then the \code{i}-th element of the result is
  \code{j}.  If no match is found for \code{x[i]} in \code{levels}, then
  the \code{i}-th element of the result is set to \code{\link{NA}}.  Any
  values listed in \code{exclude} result in \code{NA}s appearing in the
  factor returned.  If \code{exclude} is set to a zero length vector,
  then any \code{\link{NA}} values in \code{x} are used for form a new
  level for the factor.  \emph{This means that there will be no
  \code{NA} values in the result}.

  \code{labels} is a vector of character strings used to label the
  levels of the factor.  The default is to use the encoded factor levels.

  \code{is.factor} returns \code{TRUE} or \code{FALSE} depending on
  whether its argument is of type factor or not.  Correspondingly,
  \code{is.ordered} (\code{is.unordered}) returns \code{TRUE} when its
  argument is ordered (unordered) and \code{FALSE} otherwise.

  \code{as.factor} coerces its argument to a factor.
  It is an abbreviated form of \code{factor}.

  \code{as.ordered(x)} returns \code{x} if this is ordered, and
  \code{ordered(x)} otherwise.
}
\seealso{
  \code{\link{gl}} for construction of ``balanced'' factors;
  \code{\link{levels}} and \code{\link{nlevels}} for accessing the
  levels,  and \code{\link{codes}} to get integer codes.
}
\keyword{category}
\examples{
ff <- factor(substring("statistics",1:10,1:10), levels=letters)
ff
codes(ff)
}
