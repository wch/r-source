\name{load}
\alias{load}
\alias{loadURL}
\title{Reload Saved Datasets}
\description{
  Reload datasets written with the function \code{save}.
}
\usage{
load(file, envir = parent.frame())
loadURL(url, envir = parent.frame(), quiet = TRUE, \dots)
}
\arguments{
  \item{file}{a connection or a character string giving the name of the
    file to load.}
  \item{envir}{the environment where the data should be loaded.}
  \item{url}{a character string naming a URL.}
  \item{quiet, \dots}{additional arguments to
    \code{\link{download.file}}.}
}
\details{
  \code{load} can load \R objects saved in the current or any earlier
  format.  It can read a compressed file (see \code{\link{save}})
  directly from a file or from a suitable connection (including a call
  to \code{\link{url}}).

  \code{loadURL} is a convenience wrapper which downloads a file, loads
  it and deletes the downloaded copy.

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
  \code{ascii=TRUE}, so ensure that they are transferred without
  conversion of end of line markers.
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
## print the value to see what objects were created.
print(loadURL("http://some.where.net/R/data/kprats.rda"))
# or, avoiding making a local copy,
print(load(url("http://some.where.net/R/data/kprats.rda")))
}}
\keyword{file}
