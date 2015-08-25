% File src/library/stats/man/loadings.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{loadings}
\alias{loadings}
\alias{print.loadings}
\alias{print.factanal}
\title{Print Loadings in Factor Analysis}
\description{
  Extract or print loadings in factor analysis (or principal
  components analysis).
}
\usage{
loadings(x, ...)

\method{print}{loadings}(x, digits = 3, cutoff = 0.1, sort = FALSE, \dots)

\method{print}{factanal}(x, digits = 3, \dots)
}
\arguments{
  \item{x}{an object of class \code{"\link{factanal}"} or
    \code{"\link{princomp}"} or the \code{loadings} component of such an
    object.}
  \item{digits}{number of decimal places to use in printing uniquenesses
    and loadings.}
  \item{cutoff}{loadings smaller than this (in absolute value) are suppressed.}
  \item{sort}{logical. If true, the variables are sorted by their
    importance on each factor.  Each variable with any loading larger
    than 0.5 (in modulus) is assigned to the factor with the largest
    loading, and the variables are printed in the order of the factor
    they are assigned to, then those unassigned.}
  \item{\dots}{further arguments for other methods,
    ignored for \code{loadings}.}
}

\details{
  \sQuote{Loadings} is a term from \emph{factor analysis}, but because
  factor analysis and principal component analysis (PCA) are often
  conflated in the social science literature, it was used for PCA by
  SPSS and hence by \code{\link{princomp}} in S-PLUS to help SPSS users.

  Small loadings are conventionally not printed (replaced by spaces), to
  draw the eye to the pattern of the larger loadings.

  The \code{print} method for class \code{"\link{factanal}"} calls the
  \code{"loadings"} method to print the loadings, and so passes down
  arguments such as \code{cutoff} and \code{sort}.
  
  The signs of the loadings vectors are arbitrary for both factor
  analysis and PCA.
}

\note{
  There are other functions called \code{loadings} in contributed
  packages which are S3 or S4 generic: the \code{\dots} argument is to
  make it easier for this one to become a default method.
}

\seealso{
  \code{\link{factanal}}, \code{\link{princomp}}
}

\keyword{multivariate}
\keyword{print}
