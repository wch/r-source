\name{tempfile}
\title{Create Unique Names for (Temporary) Files}
\usage{
tempfile(pattern = "file")
}
\alias{tempfile}
\arguments{
  \item{pattern}{a character vector giving the initial part of the name.}
}
\value{
  A character vector giving the names of possible (temporary) files.  The
  names are guaranteed to be unique among calls to \code{tempfile} in an \R
  session, and to not exist as files.  Note that no files are generated
  by \code{tempfile}.
}
\details{
  The filenames will be in the directory given by the first found of
  the environment variables \code{TMP}, \code{TEMP} and \code{R\_USER}
  (see \code{\link{Rconsole}}). If the path to the directory contains
  a space in any of the components, the path returned will use the
  shortnames version of the path. 
}
\seealso{\code{\link{unlink}} for deleting files.}

\keyword{file}
