% File src/library/utils/man/unzip.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2009 R Core Development Team
% Distributed under GPL 2 or later

\name{unzip}
\alias{unzip}
\title{
  Extract or List Zip Archives
}
\description{
  Extract files from or list a zip archive.
}
\usage{
unzip(zipfile, files = NULL, list = FALSE, overwrite = TRUE,
       junkpaths = FALSE, exdir = ".")
}
\arguments{
  \item{zipfile}{The pathname of the zip file: tilde expansion (see
    \code{\link{path.expand}}) will be performed.}

  \item{files}{A character vector of recorded filepaths to be extracted:
    the default is to extract all files.}
  
  \item{list}{If \code{TRUE}, list the files and extract none.  The
    equivalent of \command{unzip -l}.}
  
  \item{overwrite}{If \code{TRUE}, overwrite existing files, otherwise
    ignore such files.  The equivalent of \command{unzip -o}.}
  
  \item{junkpaths}{If \code{TRUE}, use only the basename of the stored
    filepath when extracting.  The equivalent of \command{unzip -j}.}
  
  \item{exdir}{The directory to extract files to (the equivalent of
    \code{unzip -d}).  It will be created if necessary.}
}

\value{
  If \code{list = TRUE}, a data frame with columns \code{Name},
  \code{Length} (the size of the uncompressed file) and \code{Date} (of
  class \code{"\link{POSIXct}"}).

  Otherwise, a character vector of the filepaths extracted to, invisibly.
}

\source{
  The C code uses \code{zlib} and is in particular based on the
  contributed \samp{minizip} application in the \code{zlib} sources
  (from \url{zlib.net}) by Gilles Vollant.
}

\seealso{
  \code{\link{unz}} and \code{\link{zip.file.extract}} to read a single
  component from a zip file.
}

\keyword{file}
\keyword{utilities}
