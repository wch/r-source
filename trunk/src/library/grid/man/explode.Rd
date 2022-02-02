\name{explode}
\alias{explode}
\alias{explode.character}
\alias{explode.path}
\title{
  Explode a path into its components.
}
\description{
  Explode a viewport path or grob path into its components.
}
\usage{
explode(x)
\method{explode}{character}(x)
\method{explode}{path}(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    Typically a viewport path or a grob path, but a character vector
    containing zero or more path separators may also be given.
  }
}
\value{
  A character vector.
}
\seealso{
  \code{\link{vpPath}}, \code{\link{gPath}}.
}
\examples{
explode("vp1::vp2")
explode(vpPath("vp1", "vp2"))
}
\keyword{dplot}
