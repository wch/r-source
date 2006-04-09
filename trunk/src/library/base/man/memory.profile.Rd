\name{memory.profile}
\alias{memory.profile}
\title{Profile the Usage of Cons Cells}
\description{
    Lists the usage of the cons cells by \code{SEXPREC} type.
}
\usage{
memory.profile()
}
\details{
    The current types and their uses are listed in the include file
    \file{Rinternals.h}.
    % There will be blanks in the list corresponding
    % to types that are no longer in use (types 11 and 12 at the time of
    % writing).
    % Also \code{FUNSXP} is not included.
    % Nonsense: that is a mode, not a type.
}
\value{
  A vector of counts, named by the types.  See \code{\link{typeof}} for
  an explanation of types.
}

\seealso{
  \code{\link{gc}} for the overall usage of cons cells.
}

\examples{
memory.profile()
}
\keyword{utilities}
