\name{write_PACKAGES}
\alias{write_PACKAGES}
\title{Generating a PACKAGES file}
\description{
  Generating a PACKAGES file for a repository of packages.
  Currently only implemented for Windows binary repositories.
}
\usage{
write_PACKAGES(dir, fields, type = "winBinary")
}
\arguments{
  \item{dir}{
    Character vector describing the location of the repository 
    (directory including binary packages for Windows) 
    to generate the \file{PACKAGES} file from and write it to.
  }
  \item{fields}{
    Optional, the fields to be used in the \file{PACKAGES} file.
    The default is the same as that one used for building \file{bin/windows}
    area of the CRAN repositories: \dQuote{Package}, 
    \dQuote{Bundle}, \dQuote{Priority}, \dQuote{Version}, 
    \dQuote{Depends}, \dQuote{Suggests}, \dQuote{Imports}, and
    \dQuote{Contains}.
  }
  \item{type}{Type of packages: currently only Windows binary packages
    are supported.
    }
}
\details{
  This function uses \code{\link{unz}} connections to read all
  \file{DESCRIPTION} files contained in the (zipped) binary packages for
  Windows in the given directory \code{dir}, and builds a
  \file{PACKAGES} file from these information.

  \strong{Note:} This function  can be useful on other OSes to prepare a
  repository to be accessed by Windows machines.
}
\value{
  Invisibly returns the number of packages described in the resulting
  \file{PACKAGES} file.  If 0, no packages were found and no
  \file{PACKAGES} file has been written.
}
\author{
  Uwe Ligges
}
\seealso{
  See \code{\link{read.dcf}} and \code{\link{write.dcf}} for reading
  \file{DESCRIPTION} files and writing the \file{PACKAGES} file.
}
\examples{
\dontrun{
write_PACKAGES("c:/myFolder/myRepository")
}}
\keyword{file}
\keyword{utilities}

