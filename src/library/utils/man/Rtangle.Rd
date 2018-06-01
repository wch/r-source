% File src/library/utils/man/Rtangle.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2016 R Core Team
% Distributed under GPL 2 or later

\name{Rtangle}
\alias{Rtangle}
\alias{RtangleSetup}
\title{R Driver for Stangle}
\description{
  A driver for \code{\link{Stangle}} that extracts \R code chunks.
  Notably all \code{RtangleSetup()} arguments may be used as arguments
  in the \code{\link{Stangle}()} call.
}
\usage{
Rtangle()
RtangleSetup(file, syntax, output = NULL, annotate = TRUE,
             split = FALSE, quiet = FALSE, drop.evalFALSE = FALSE, ...)
}
\arguments{
  \item{file}{name of Sweave source file.  See the description of the
    corresponding argument of \code{\link{Sweave}}.}
  \item{syntax}{an object of class \code{SweaveSyntax}.}
  \item{output}{name of output file used unless \code{split = TRUE}:
    see \sQuote{Details}.}
  \item{annotate}{a logical or \code{\link{function}}.  When true, as by
    default, code chunks are separated by comment lines specifying the
    names and line numbers of the code chunks.
    If \code{FALSE} the decorating comments are omitted.  Alternatively,
    \code{annotate} may be a function, see section \sQuote{Chunk annotation}.}
  \item{split}{split output into a file for each code chunk?}
  \item{quiet}{logical to suppress all progress messages.}
  \item{drop.evalFALSE}{logical; When false, as by default, all chunks with
    option \code{eval = FALSE} are \emph{commented out} in the output;
    otherwise (\code{drop.evalFALSE = TRUE}) they are omitted entirely.}
  \item{\dots}{additional named arguments setting defaults for further
    options listed in \sQuote{Supported Options}.}
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

  Note that this driver does more than simply extract the code chunks verbatim,
  because chunks may re-use earlier chunks.
}

\section{Chunk annotation (\code{annotate})}{
  By default \code{annotate = TRUE}, the annotation is of one of the forms%
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
  using either the chunk label (if present, i.e., when specified in the
  source) or the file name and line numbers.

  \code{annotate} may be a function with formal arguments
  \code{(options, chunk, output)}, e.g. to produce less dominant chunk
  annotations; see \code{Rtangle()$runcode} how it is called instead of
  the default.
}

\section{Supported Options}{
  \code{Rtangle} supports the following options for code chunks (the
  values in parentheses show the default values):
  \describe{
    \item{engine:}{character string (\code{"R"}).  Only chunks with
      \code{engine} equal to \code{"R"} or \code{"S"} are processed.}

    \item{keep.source:}{logical (\code{TRUE}).  If
      \code{keep.source == TRUE} the original source is copied to the
      file.  Otherwise, deparsed source is output.}

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
  the \pkg{utils} package.%% ../vignettes/Sweave.Rnw

  \code{\link{Sweave}}, \code{\link{RweaveLatex}}
}
\examples{
nmRnw <- "example-1.Rnw"
exfile <- system.file("Sweave", nmRnw, package = "utils")
## Create R source file
Stangle(exfile)
nmR <- sub("Rnw$", "R", nmRnw) # the (default) R output file name
if(interactive()) file.show("example-1.R")
\dontshow{file.rename("example-1.R", "example-1_def.R")}
## Smaller R source file with custom annotation:
my.Ann <- function(options, chunk, output) {
  cat("### chunk #", options$chunknr, ": ",
      if(!is.null(ol <- options$label)) ol else .RtangleCodeLabel(chunk),
      if(!options$eval) " (eval = FALSE)", "\n",
      file = output, sep = "")
}
Stangle(exfile, annotate = my.Ann)
if(interactive()) file.show("example-1.R")
\dontshow{file.rename("example-1.R", "example-1_myA.R")}
Stangle(exfile, annotate = my.Ann, drop.evalFALSE=TRUE)
if(interactive()) file.show("example-1.R")
\dontshow{file.rename("example-1.R", "example-1_myA-noF.R")}
}
\keyword{utilities}
