\name{grDevices-defunct}
\alias{postscriptFont}
\title{Defunct Functions in Package grDevices}
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
