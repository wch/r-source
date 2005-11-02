\name{grDevices-deprecated}
\alias{postscriptFont}
\title{Deprecated Functions in Package grDevices}
\description{
  These functions are provided for compatibility with older versions of
  \R only, and may be defunct as soon as of the next release.
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
  The function \code{postscriptFont} is deprecated in favour of
  \code{\link{Type1Font}}.
}
\value{
  One or more font mappings.
}
\seealso{
  \code{\link{Deprecated}}, \code{\link{Defunct}}
}
\keyword{misc}
