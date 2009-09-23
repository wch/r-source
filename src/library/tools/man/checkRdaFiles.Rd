% File src/library/tools/man/checkRdaFiles.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2009 R Core Development Team
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
resaveRdaFiles(paths, compress = c("auto", "gzip", "bzip2", "rda"),
               compression_level)
}

\arguments{
  \item{paths}{A character vector of paths to \code{save} files.}

  \item{compress, compression_level}{type and level of compression: see
    \code{\link{save}}.}
}

\details{  
  \code{compress = "auto"} asks \R to choose the compression and ignores
  \code{compression_level}.  It will try \code{"gzip"}, \code{"bzip2"}
  and if \code{capabilities("xz")} and the \code{"gzip"} compressed size
  is over 10Kb, \code{"xz"} and choose the smallest (but with a 10\%
  bias towards \code{"gzip"}).  This can be slow.
}

\value{
  For \code{checkRdaFiles}, a data frame with rows names \code{paths}
  and columns
  \item{size}{numeric: file size in bytes, \code{NA} if the file does
    not exist.}
  \item{ASCII}{logical: true for save(ASCII=TRUE), \code{NA} is the
    format is not that of a \R save files.}
  \item{compress}{character: type of compression.  One of \code{"gzip"},
    \code{"bzip2"}, \code{"xv"} or \code{"unknown"} (which if this is an
    uncorrupted \R save file means it is uncompressed).}
}
\examples{\dontrun{
## from a package top-level source directory
paths <- sort(c(Sys.glob("data/*.rda"), Sys.glob("data/*.RData")))
(res <- checkRdaFiles(paths))
## pick out some that may need attention
bad <- is.na(res$ASCII) | res$ASCII |
       (res$size > 1e4 & res$compress == "unknown")
res[bad, ]
}}
\keyword{ utilities }
