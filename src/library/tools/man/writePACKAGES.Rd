% File src/library/tools/man/writePACKAGES.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{write_PACKAGES}
\alias{write_PACKAGES}
\title{Generate PACKAGES files}
\description{
  Generate \file{PACKAGES} and \file{PACKAGES.gz} files for a
  repository of source or Mac/Windows binary packages.
}
\usage{
write_PACKAGES(dir = ".", fields = NULL,
               type = c("source", "mac.binary", "win.binary"),
               verbose = FALSE, unpacked = FALSE, subdirs = FALSE,
               latestOnly = TRUE, addFiles = FALSE)
}
\arguments{
  \item{dir}{Character vector describing the location of the repository
    (directory including source or binary packages) to generate the
    \file{PACKAGES} and \file{PACKAGES.gz} files from and write them to.
  }
  \item{fields}{
    a character vector giving the fields to be used in the
    \file{PACKAGES} and \file{PACKAGES.gz} files in addition to the
    default ones, or \code{NULL} (default).  The default corresponds to
    the fields needed by \code{\link{available.packages}}:
    \code{"Package"}, \code{"Version"}, \code{"Priority"},
    \code{"Depends"}, \code{"Imports"}, \code{"LinkingTo"},
    \code{"Suggests"}, \code{"Enhances"}, \code{"OS_type"},
    \code{"License"} and \code{"Archs"}, and those fields will always
    be included, plus
    the file name in field \code{"File"} if \code{addFile = TRUE}
    and the path to the subdirectory in field \code{"Path"} if subdirectories
    are used.
  }
  \item{type}{
    Type of packages: currently source \file{.tar.\{gz,bz2,xz\}} archives,
    and OS X or Windows binary (\file{.tgz} or \file{.zip},
    respectively) packages are supported.  Defaults to
    \code{"win.binary"} on Windows and to \code{"source"} otherwise.
  }
  \item{verbose}{logical.  Should packages be listed as they are
    processed?}
  \item{unpacked}{a logical indicating whether the package contents are
    available in unpacked form or not (default).}
  \item{subdirs}{either logical (to indicate if subdirectories should be
    included, recursively) or a character vector of name of subdirectories
    to include.}
  \item{latestOnly}{logical: if multiple versions of a package are
    available should only the latest version be included?}
  \item{addFiles}{logical: should the filenames be included as field
    \samp{File} in the \file{PACKAGES} file.}
}
\details{
  \code{write_PACKAGES} scans the named directory for R packages,
  extracts information from each package's \file{DESCRIPTION} file, and
  writes this information into the \file{PACKAGES} and
  \file{PACKAGES.gz} files.

  Including non-latest versions of packages is only useful if they have
  less constraining version requirements, so for example
  \code{latestOnly = FALSE} could be used for a source repository when
  \samp{foo_1.0} depends on \samp{R >= 2.15.0} but \samp{foo_0.9} is
  available which depends on \samp{R >= 2.11.0}.

  Support for repositories with subdirectories and hence for
  \code{subdirs != FALSE} depends on recording a \code{"Path"} field in
  the \file{PACKAGES} file.

  Support for more general file names (e.g., other types of compression)
  \emph{via} a \code{"File"} field in the \file{PACKAGES} file can be
  used by \code{\link{download.packages}}.  If the file names are not of
  the standard form, use \code{addFiles = TRUE}.

  \code{type = "win.binary"} uses \code{\link{unz}} connections to read
  all \file{DESCRIPTION} files contained in the (zipped) binary packages
  for Windows in the given directory \code{dir}, and builds files
  \file{PACKAGES} and \file{PACKAGES.gz} files from this information.
}
\value{
  Invisibly returns the number of packages described in the resulting
  \file{PACKAGES} and \file{PACKAGES.gz} files.  If \code{0}, no
  packages were found and no files were written.
}
\note{
  Processing \file{.tar.gz} archives to extract the \file{DESCRIPTION}
  files is quite slow.

  This function can be useful on other OSes to prepare a repository to
  be accessed by Windows machines, so \code{type = "win.binary"} should
  work on all OSes.
}
\author{
  Uwe Ligges and R-core.
}
\seealso{
  See \code{\link{read.dcf}} and \code{\link{write.dcf}} for reading
  \file{DESCRIPTION} files and writing the \file{PACKAGES} and
  \file{PACKAGES.gz} files.
}
\examples{
\dontrun{
write_PACKAGES("c:/myFolder/myRepository")  # on Windows
write_PACKAGES("/pub/RWin/bin/windows/contrib/2.9",
               type = "win.binary")  # on Linux
}}
\keyword{file}
\keyword{utilities}
