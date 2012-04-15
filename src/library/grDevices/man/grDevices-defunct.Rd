% File src/library/grDevices/man/grDevices-defunct.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{grDevices-defunct}
\alias{postscriptFont}
\title{Defunct Functions in Package \pkg{grDevices}}
\description{
  The functions or variables listed here are no longer part of \R as
  they are not needed (any more).
}
\usage{
postscriptFont(family, metrics, encoding = "default")
}
\arguments{
  \item{family}{a character string giving the name of a Type 1
    font family.}
  \item{metrics}{a vector of four or five strings giving paths
    to the afm (font metric) files for the font.}
  \item{encoding}{the name of an encoding file.}
}
\details{
  The function \code{postscriptFont} has been replaced by
  \code{\link{Type1Font}}, which is just a change of name.
}
\value{
  One or more font mappings.
}
\seealso{
  \code{\link{Defunct}}
}
\keyword{internal}
