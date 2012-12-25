% File src/library/stats/man/offset.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\newcommand{\CRANpkg}{\href{http://CRAN.R-project.org/package=#1}{\pkg{#1}}}

\name{offset}
\title{Include an Offset in a Model Formula}
\usage{
offset(object)
}
\alias{offset}
\arguments{
 \item{object}{An offset to be included in a model frame}
}
\description{
  An offset is a term to be added to a linear predictor, such as in a
  generalised linear model, with known coefficient 1 rather than an
  estimated coefficient.
}
\value{
  The input value.
}
\details{
  There can be more than one offset in a model formula, but \code{-} is
  not supported for \code{offset} terms (and is equivalent to \code{+}).
}

\seealso{
  \code{\link{model.offset}}, \code{\link{model.frame}}.

  For examples see \code{\link{glm}} and
  \code{\link[MASS]{Insurance}} in package \CRANpkg{MASS}.
}
\keyword{models}
