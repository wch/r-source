\name{load}
\alias{load}
\title{Reload Saved Datasets}
\description{
  Reload datasets written with the function \code{save}.
}
\usage{
load(file, envir = parent.frame())
}
\arguments{
  \item{file}{a (readable binary) connection or a character string
    giving the name of the file to load.}
  \item{envir}{the environment where the data should be loaded.}
}
\details{
  \code{load} can load \R objects saved in the current or any earlier
  format.  It can read a compressed file (see \code{\link{save}})
  directly from a file or from a suitable connection (including a call
  to \code{\link{url}}).

  Only \R objects saved in the current format (used since \R 1.4.0)
  can be read from a connection.  If no input is available on a
  connection a warning will be given, but any input not in the current
  format will result in a error.
}
\value{
  A character vector of the names of objects created, invisibly.
}
\section{Warning}{
  Saved \R objects are binary files, even those saved with
  \code{ascii = TRUE}, so ensure that they are transferred without
  conversion of end of line markers.  \code{load} tries to detect this
  case and give an informative error message.
}
\seealso{
  \code{\link{save}}, \code{\link{download.file}}.
}
\examples{
## save all data
save(list = ls(all=TRUE), file= "all.Rdata")

## restore the saved values to the current environment
load("all.Rdata")

## restore the saved values to the user's workspace
load("all.Rdata", .GlobalEnv)

\dontrun{
con <- url("http://some.where.net/R/data/kprats.rda")
## print the value to see what objects were created.
print(load(con))
close(con)
}}
\keyword{file}
