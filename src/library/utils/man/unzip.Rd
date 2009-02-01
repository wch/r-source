% File src/library/utils/man/zip.file.extract.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2009 R Core Development Team
% Distributed under GPL 2 or later

\name{unzip}
\alias{unzip}
\title{
  Extract or List Zip archines
}
\description{
  Extract files from or list a zip archive.
}
\usage{
unzip(zipfile, files = NULL, list = FALSE, exdir = ".")
}
\arguments{
  \item{zipfile}{The pathname of the xip file: tilde expansion (see
    \code{\link{path.expand}} will be performed.}

  \item{files}{A character vector of recorded filepaths to be extracted:
    the default is to extract all files.}
  
  \item{list}{If \code{TRUE}, list the files and extract none.  The
    equivalent of \command{unzip -l}.}
  
  \item{exdir}{The directory to extract files to (the equivalent of
    \code{unzip -d}).  It will be created if necessary.}
}
\details{
  Extraction will overwrite any existing files.
}
\value{
  If \code{list = TRUE}, a data frame with columns \code{Name},
  \code{Length} (the size of the uncompressed file) and \code{Date} (of
  class \code{"\link{POSIXct}"}).

  Otherwise, a character vector of the filepaths extracted to, invisibly.
}
\seealso{
  \code{\link{zip.file.extract}}
}

\keyword{file}
\keyword{utilities}
