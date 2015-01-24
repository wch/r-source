% File src/library/utils/man/Rtangle.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2014 R Core Team
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
             split = FALSE, quiet = FALSE, ...)
}
\arguments{
  \item{file}{Name of Sweave source file.  See the description of the
    corresponding argument of \code{\link{Sweave}}.}
  \item{syntax}{An object of class \code{SweaveSyntax}.}
  \item{output}{Name of output file used unless \code{split = TRUE}:
    see \sQuote{Details}.}
  \item{annotate}{By default, code chunks are separated by comment
    lines specifying the names and numbers of the code chunks.  If
    \code{FALSE} the decorating comments are omitted.}
  \item{split}{Split output into a file for each code chunk?}
  \item{quiet}{If \code{TRUE} all progress messages are suppressed.}
  \item{\dots}{Additional named arguments setting defaults for further
    options.}
}
\details{
  Unless \code{split = TRUE}, the default name of the output file is
  \code{basename(file)} with an extension corresponding to the Sweave
  syntax (e.g., \file{Rnw}, \file{Stex}) replaced by \file{R}.  File
  names \code{"stdout"} and \code{"stderr"} are interpreted as the
  output and message connection respectively.

  If splitting is selected (including by the options in the file), each
  chunk is written to a separate file with extension the name of the
  \sQuote{engine} (default \file{.R}).

  The annotation is of one of the forms%
\preformatted{###################################################
### code chunk number 3: viewport
###################################################

###################################################
### code chunk number 18: grid.Rnw:647-648
###################################################

###################################################
### code chunk number 19: trellisdata (eval = FALSE)
###################################################
}
using either the chunk label or the file name and line numbers.

  Note that this driver does not simple extract the code chunks verbatim
  because code chunks can re-use earlier chunks.
}
\section{Supported Options}{
  \code{Rtangle} supports the following options for code chunks (the
  values in parentheses show the default values):
  \describe{
    \item{engine:}{character string (\code{"R"}).  Only chunks with
      \code{engine} equal to \code{"R"} or \code{"S"} are processed.}

    \item{keep.source:}{logical (\code{TRUE}).  If \code{keep.source ==
      TRUE} the original source is copied to the file.  Otherwise,
      deparsed source is output.}

    \item{eval:}{logical (\code{TRUE}).  If \code{FALSE}, the code chunk
      is copied across but commented out.}

    \item{prefix}{Used if \code{split = TRUE}.  See \code{prefix.string}.}

    \item{prefix.string:}{a character string, default is the name of the
      source file (without extension).  Used if \code{split = TRUE} as
      the prefix for the filename if the chunk has no label, or if it
      has a label and \code{prefix = TRUE}.  Note that this is used as
      part of filenames, so needs to be portable.}

    \item{show.line.nos}{logical (\code{FALSE}).  Should the output be
      annotated with comments showing the line number of the first code line
      of the chunk?}
  }
}

\author{Friedrich Leisch and R-core.}
\seealso{
  \sQuote{\href{../doc/Sweave.pdf}{Sweave User Manual}}, a vignette in
  the \pkg{utils} package.

  \code{\link{Sweave}}, \code{\link{RweaveLatex}}
}
\keyword{utilities}

