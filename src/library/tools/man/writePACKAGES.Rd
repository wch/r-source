\name{write_PACKAGES}
\alias{write_PACKAGES}
\title{Generating a PACKAGES file}
\description{
  Generating a \file{PACKAGES} file for a repository of source or
  Windows binary packages.
}
\usage{
write_PACKAGES(dir, fields,
               type = c("source", "mac.binary", "win.binary"),
               verbose = FALSE)
}
\arguments{
  \item{dir}{
    Character vector describing the location of the repository 
    (directory including source or binary packages) to generate
    the \file{PACKAGES} file from and write it to.
  }
  \item{fields}{
    Optional, the fields to be used in the \file{PACKAGES} file.
    The default are those needed by \code{\link{available.packages}}:
    \dQuote{Package}, \dQuote{Bundle}, \dQuote{Priority},
    \dQuote{Version}, \dQuote{Depends}, \dQuote{Suggests},
    \dQuote{Imports} and \dQuote{Contains}.
  }
  \item{type}{
    Type of packages: currently source \code{.tar.gz} archives
    and Windows binary \code{.zip} packages are supported.  Defaults to
    \code{"win.binary"} on Windows and to \code{"source"} otherwise.
  }
  \item{verbose}{logical. Should packages be listed as they are processed?}
}
\details{
  \code{type = "win.binary"} uses \code{\link{unz}} connections to read all
  \file{DESCRIPTION} files contained in the (zipped) binary packages for
  Windows in the given directory \code{dir}, and builds a
  \file{PACKAGES} file from these information.
}
\value{
  Invisibly returns the number of packages described in the resulting
  \file{PACKAGES} file.  If 0, no packages were found and no
  \file{PACKAGES} file has been written.
}
\note{
  Processing \code{.tar.gz} archives to extract the \file{DESCRIPTION}
  files is quite slow.
  
  This function can be useful on other OSes to prepare a repository to
  be accessed by Windows machines, so \code{type = "winBinary"} should
  work on all OSes.
}
\author{
  Uwe Ligges and R-core.
}
\seealso{
  See \code{\link{read.dcf}} and \code{\link{write.dcf}} for reading
  \file{DESCRIPTION} files and writing the \file{PACKAGES} file.
}
\examples{
\dontrun{
write_PACKAGES("c:/myFolder/myRepository")  # on Windows
write_PACKAGES("/pub/RWin/bin/windows/contrib/2.1",
               type="win.binary")  # on Linux
}}
\keyword{file}
\keyword{utilities}

