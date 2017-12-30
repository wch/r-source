% File src/library/tools/man/checkRdaFiles.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2009-2017 R Core Team
% Distributed under GPL 2 or later

\name{checkRdaFiles}
\alias{checkRdaFiles}
\alias{resaveRdaFiles}
\title{
  Report on Details of Saved Images or Re-saves them
}
\description{
  This reports for each of the files produced by \code{save} the size,
  if it was saved in ASCII or XDR binary format, and if it was
  compressed (and if so in what format).

  Usually such files have extension \file{.rda} or \file{.RData}, hence
  the name of the function.
}
\usage{
checkRdaFiles(paths)
resaveRdaFiles(paths, compress = c("auto", "gzip", "bzip2", "xz"),
               compression_level, version = NULL)
}

\arguments{
  \item{paths}{A character vector of paths to \code{save} files.  If
    this specifies a single directory, it is taken to refer to all
    \file{.rda} and \file{.RData} files in that directory.}

  \item{compress, compression_level}{Type and level of compression: see
    \code{\link{save}}.  Values of \code{compress} can be abbreviated.}
  
  \item{version}{The format to be used when re-saving: see
    \code{\link{save}}.}
}

\details{
  \code{compress = "auto"} asks \R to choose the compression and ignores
  \code{compression_level}.  It will try \code{"gzip"}, \code{"bzip2"}
  and if the \code{"gzip"} compressed size is over 10Kb, \code{"xz"} and
  choose the smallest compressed file (but with a 10\% bias towards
  \code{"gzip"}).  This can be slow.

  For back-compatibility, \code{version = NULL} is interpreted to mean
  version 2: however version-3 files will only be saved as version 3.
}

\value{
  For \code{checkRdaFiles}, a data frame with rows names \code{paths}
  and columns
  \item{size}{numeric: file size in bytes, \code{NA} if the file does
    not exist.}
  \item{ASCII}{logical: true for save(ASCII = TRUE), \code{NA} if the
    format is not that of an \R save file.}
  \item{compress}{character: type of compression.  One of \code{"gzip"},
    \code{"bzip2"}, \code{"xz"}, \code{"none"} or \code{"unknown"} (which
    means that if this is an \R save file it is from a later version of
    \R).}
  \item{version}{integer: the version of the save -- usually \code{2}
    but \code{1} for very old files, and \code{NA} for other files.
    Unfortunately \code{2} means \sQuote{version 2 or later}.}
}
\examples{\dontrun{
## from a package top-level source directory
paths <- sort(Sys.glob(c("data/*.rda", "data/*.RData")))
(res <- checkRdaFiles(paths))
## pick out some that may need attention
bad <- is.na(res$ASCII) | res$ASCII | (res$size > 1e4 & res$compress == "none")
res[bad, ]
}}
\keyword{ utilities }
