% File src/library/stats/man/reorder.factor.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{reorder.default}
\alias{reorder}
\alias{reorder.default}
\title{Reorder Levels of a Factor}
\description{
  \code{reorder} is a generic function.  The \code{"default"} method
  treats its first argument as a categorical variable, and reorders its
  levels based on the values of a second variable, usually numeric.
}

\usage{
reorder(x, \dots)

\method{reorder}{default}(x, X, FUN = mean, \dots,
        order = is.ordered(x))

}
\arguments{
  \item{x}{
    An atomic vector, usually a factor (possibly ordered).  The vector
    is treated as a categorical variable whose levels will be reordered.
    If \code{x} is not a factor, its unique values will be used as
    the implicit levels.
  }
  \item{X}{ a vector of the same length as \code{x}, whose subset
    of values for each unique level of \code{x} determines the
    eventual order of that level.
  }
  \item{FUN}{ a function whose first argument is a vector and
    returns a scalar, to be applied to each subset of \code{X}
    determined by the levels of \code{x}.
  }
  \item{\dots}{ optional: extra arguments supplied to \code{FUN}}
  \item{order}{ logical, whether return value will be an ordered factor
    rather than a factor.
  }
}

\value{
  A factor or an ordered factor (depending on the value of
  \code{order}), with the order of the levels determined by
  \code{FUN} applied to \code{X} grouped by \code{x}.  The
  levels are ordered such that the values returned by \code{FUN}
  are in increasing order.  Empty levels will be dropped.

  Additionally, the values of \code{FUN} applied to the subsets of
  \code{X} (in the original order of the levels of \code{x}) is returned
  as the \code{"scores"} attribute.
}

\author{Deepayan Sarkar \email{deepayan.sarkar@r-project.org}}

\seealso{
  \code{\link{reorder.dendrogram}}, \code{\link{levels}},
  \code{\link{relevel}}.
}

\examples{
require(graphics)

bymedian <- with(InsectSprays, reorder(spray, count, median))
boxplot(count ~ bymedian, data = InsectSprays,
        xlab = "Type of spray", ylab = "Insect count",
        main = "InsectSprays data", varwidth = TRUE,
        col = "lightgray")
}
\keyword{utilities}

