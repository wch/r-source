% File src/library/utils/man/Rtangle.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2011 R Core Development Team
% Distributed under GPL 2 or later

\name{Rtangle}
\alias{Rtangle}
\alias{RtangleSetup}
\title{R Driver for Stangle}
\description{
  A driver for \code{\link{Stangle}} that extracts \R code chunks.
}
\usage{
Rtangle()
RtangleSetup(file, syntax, output = NULL, annotate = TRUE,
             split = FALSE, prefix = TRUE, quiet = FALSE)
}
\arguments{
  \item{file}{Name of Sweave source file.  See the description of the
    corresponding argument of \code{\link{Sweave}}.}
  \item{syntax}{An object of class \code{SweaveSyntax}.}
  \item{output}{Name of output file used unless \code{split = TRUE}:
    see \sQuote{Details}.}
  \item{annotate}{By default, code chunks are separated by comment
    lines specifying the names and numbers of the code chunks. If
    \code{FALSE}, only the code chunks without any decorating comments
    are extracted.}
  \item{split}{Split output into a file for each code chunk?}
  \item{prefix}{If \code{split = TRUE}, prefix the chunk labels by the
    basename of the input file to get output file names?}
  \item{quiet}{If \code{TRUE} all progress messages are suppressed.}
}
\details{
  Unless \code{split = TRUE}, the default name of the output file is
  \code{basename(file)} with an extension corresponding to the Sweave
  syntax (e.g. \file{Rnw}, \file{Stex}) replaced by \file{R}.

  If splitting is selected (including by the options in the file), each
  chunk is written to a separate file with extension the name of the
  \sQuote{engine} (default \file{.R}).
  
  Note that this does not simple extract the code chunks verbatim
  because code chunks can re-use earlier chunks.
}
\author{Friedrich Leisch and R-core.}
\seealso{
  \sQuote{\href{../doc/Sweave.pdf}{Sweave User Manual}}, a vignette in
  the \pkg{utils} package.

  \code{\link{Sweave}}, \code{\link{RweaveLatex}}
}
\keyword{utilities}

