% File src/library/base/man/sort_by.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2024 R Core Team
% Distributed under GPL 2 or later

\name{sort_by}
\alias{sort_by}
\alias{sort_by.default}
\alias{sort_by.data.frame}
\title{Sorting Vectors or Data Frames by Other Vectors}
\description{
  Generic function to sort an object in the order determined by one or
  more other objects, typically vectors. A method is defined for data
  frames to sort its rows (typically by one or more columns), and the
  default method handles vector-like objects.
}

\usage{
sort_by(x, y, \dots)

\method{sort_by}{default}(x, y, \dots)

\method{sort_by}{data.frame}(x, y, \dots)
}
\arguments{
  \item{x}{An object to be sorted, typically a vector or data frame. }
  \item{y}{Variables to sort by.

    For the default method, this can be a vector, or more generally any
    object that has a \code{\link{xtfrm}} method.

    For the \code{data.frame} method, typically a formula specifying the
    variables to sort by. The formula can take the forms \code{ ~ g} or
    \code{~ list(g)} to sort by the variable \code{g}, or more generally
    the forms \code{ ~ g1 + \dots + gk} or \code{~ list(g1, \dots, gk)}
    to sort by the variables \code{g1}, \dots, \code{gk}, using the
    later ones to resolve ties in the preceding ones.  These variables
    are evaluated in the data frame \code{x} using the usual
    non-standard evaluation rules. If not a formula, \code{y = g} is
    equivalent to \code{y = ~ g} and \code{y = list(g1, \dots, gk)} is
    equivalent to \code{y = ~ list(g1, \dots, gk)}. However,
    non-standard evaluation in \code{x} is not done in this case.
  }
  \item{\dots}{Additional arguments, typically passed on to
    \code{\link{order}}. These may include additional variables to sort
    by, as well as named arguments recognized by \code{order}.
  }
}

\value{
  A sorted version of \code{x}. If \code{x} is a data frame, this means
  that the rows of \code{x} have been reordered to sort the variables
  specified in \code{y}.
}

\seealso{
  \code{\link{sort}}, \code{\link{order}}.}
\examples{
mtcars$am
mtcars$mpg
with(mtcars, sort_by(mpg, am)) # group mpg by am

## data.frame method
sort_by(mtcars, runif(nrow(mtcars))) # random row permutation
sort_by(mtcars, list(mtcars$am, mtcars$mpg))

# formula interface
sort_by(mtcars, ~ am + mpg) |> subset(select = c(am, mpg))
sort_by.data.frame(mtcars, ~ list(am, -mpg)) |> subset(select = c(am, mpg))

}
\keyword{univar}
\keyword{manip}
