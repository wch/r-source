\name{save}
\title{Save \R Objects}
\usage{
save(..., list = character(0), file = "", ascii = FALSE)
}
\alias{save}
\arguments{
  \item{\dots}{the names of the objects to be saved.}
  \item{list}{A character vector containing the names of objects to be
    saved.}
  \item{file}{the name of the file where the data will be saved.}
  \item{ascii}{if \code{TRUE}, an ASCII representation of the data is
    written.  This is useful for transporting data between machines of
    different types.  The default value of \code{ascii} is \code{FALSE}
    which leads to a more compact binary file being written.}
}
\description{
  \code{save} writes a external representation of \R objects to the
  specified file.  The objects can be read back from the file at a later
  date by using the function \code{load}.
}
\seealso{
  \code{\link{dput}},
  \code{\link{dump}},
  \code{\link{load}}.
}
\examples{
save(x, y, file = "xy.Rdata")
save(list = ls(), file = "all.Rdata")
}
\keyword{file}
