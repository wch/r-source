\name{save}
\title{Save R Objects}
\usage{
save(\dots, list = character(0), file = "", ascii = FALSE, oldstyle = FALSE)
save.image(file = ".Rdata", oldstyle = FALSE)
}
\alias{save}
\alias{save.image}
\arguments{
  \item{\dots}{the names of the objects to be saved.}
  \item{list}{A character vector containing the names of objects to be
    saved.}
  \item{file}{the name of the file where the data will be saved.}
  \item{ascii}{if \code{TRUE}, an ASCII representation of the data is
    written.  This is useful for transporting data between machines of
    different types.  The default value of \code{ascii} is \code{FALSE}
    which leads to a more compact binary file being written.}
  \item{oldstyle}{logical. Should the old (0.90.1 or earlier) format be used?}
}
\description{
  \code{save} writes a external representation of \R objects to the
  specified file.  The objects can be read back from the file at a later
  date by using the function \code{load}.

  \code{save.image()} is just a short-cut for ``save my current
  environment'',
  i.e., \code{save(list = ls(all=T), file = ".RData")}. It is what also
  happens with \code{\link{q}("yes")}.
}
\seealso{
  \code{\link{dput}}, \code{\link{dump}}, \code{\link{load}}.
}
\examples{
x <- runif(20)
y <- list(a = 1, b = TRUE, c = "oops")
save(x, y, file = "xy.Rdata")
save.image()
unlink("xy.Rdata")
unlink(".RData")
}
\keyword{file}
