\name{codes}
\title{Factor Codes}
\usage{
codes(x, \dots)
codes(x) <- value
}
\alias{codes}
\alias{codes.factor}
\alias{codes.ordered}
\alias{codes<-}

\description{
    This (generic) function returns a numeric coding of a
    factor. It can also be used to assign to a factor using the coded
    form.
}

\value{
    For an ordered factor, it returns the internal coding (1 for
    the lowest group, 2 for the second lowest, etc.).

    For an unordered
    factor, an alphabetical ordering of the levels is assumed, i.e the
    level that is coded 1 is the one whose name is sorted first according
    to the prevailing collating sequence. \bold{Warning:} the sort order
    may well depend on the locale, and should not be assumed to be ASCII.
}
\note{
    Normally \code{codes} is not the appropriate function to use with an
    unordered factor. Use \code{\link{unclass}} or
    \code{\link{as.numeric}} to extract the codes used in the internal
    representation of the factor, as these do not assume that the codes
    are sorted.
}

\seealso{
  \code{\link{factor}}, \code{\link{levels}}, \code{\link{nlevels}}.
}
\examples{
codes(rep(factor(c(20,10)),3))

x <- gl(3,5)
codes(x)[3] <- 2
x

data(esoph)
( ag <- esoph$alcgp[12:1] )
codes(ag)

codes(factor(1:10)) # BEWARE!
}

\note{The behaviour for unordered factors is dubious, but compatible
with S version 3. To get the internal coding of a factor, use
\code{\link{as.integer}}. Note in particular that the codes may not be
the same in different language locales because of collating
differences.}

\keyword{category}
\keyword{classes}


