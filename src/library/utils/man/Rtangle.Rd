% File src/library/utils/man/Rtangle.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2026 R Core Team
% Distributed under GPL 2 or later

\name{Rtangle}
\alias{Rtangle}
\alias{RtangleSetup}
\alias{RtangleRuncode}
\alias{RtangleWritedoc}
\alias{RtangleFinish}
\alias{RtangleOptions}
\title{R Driver for \I{Stangle}}
\description{
  A driver for \code{\link{Stangle}} that extracts \R code chunks.
}
\usage{
Rtangle()

RtangleSetup(file, syntax, output = NULL, annotate = TRUE,
             split = FALSE, quiet = FALSE, drop.evalFALSE = FALSE,
             chunk.sep = "\n\n", \dots)

RtangleRuncode(object, chunk, options)

RtangleWritedoc(object, chunk)

RtangleFinish(object, error = FALSE)

RtangleOptions(options)
}
\arguments{
  \item{file}{name of Sweave source file.  See the description of the
    corresponding argument of \code{\link{Stangle}}.}
  \item{syntax}{an object of class \code{SweaveSyntax}.}
  \item{output}{name of output file unless \code{split = TRUE}; see
    \sQuote{Details}.}
  \item{annotate}{a logical or \code{\link{function}}.  When \code{TRUE}
    code chunks are separated by comment lines specifying the names and
    line numbers of the code chunks. If \code{FALSE} the decorating
    comments are omitted.  Alternatively, \code{annotate} may be a
    function, see section \sQuote{Chunk annotation}.}
  \item{split}{split output into a file for each code chunk?}
  \item{quiet}{logical to suppress all progress messages.}
  \item{drop.evalFALSE}{logical. When \code{FALSE} all chunks with
    option \code{eval = FALSE} are \emph{commented out} in the output;
    otherwise (\code{drop.evalFALSE = TRUE}) they are omitted entirely.}
  \item{chunk.sep}{character string or logical. Text to use as separator
    between code chunks. If \code{TRUE}, the default value is used. If
    \code{FALSE}, no separator is added between chunks. Allowed (and
    specially useful) as a chunk option in the file.}
  \item{\dots}{additional named arguments setting defaults for further
    options listed in \sQuote{Supported Options}.}
  \item{object}{an object created by the \code{setup} function of the
    driver.}
  \item{chunk}{text of a code chunk.}
  \item{options}{named list of character strings.}
  \item{error}{if \code{TRUE}, finish assuming an error occurred in
    \code{\link{Sweave}}.}
}
\details{
  See \code{\link{RweaveLatex}} for details on Sweave drivers.

  \code{Rtangle} is the \I{Stangle} driver. \code{RtangleSetup},
  \code{RtangleRuncode}, \code{RtangleWritedoc}, \code{RtangleFinish}
  and \code{RtangleOptions} are functions for the \code{setup},
  \code{runcode}, \code{writedoc}, \code{finish} and \code{checkopts}
  elements of the driver, respectively.

  All \code{RtangleSetup} arguments may be used as arguments in the
  \code{\link{Stangle}} call.

  Unless \code{split = TRUE}, the default name of the output file is
  \code{basename(file)} with an extension corresponding to the Sweave
  syntax (e.g., \file{Rnw}, \file{Stex}) replaced by \file{R}. File
  names \code{"stdout"} and \code{"stderr"} are interpreted as the
  output and message connection respectively.

  If splitting is selected (including by the options in the file), each
  chunk is written to a separate file with extension as specified by the
  chunk option \code{extension} (by default the \sQuote{engine}, usually
  \file{.R}).

  The chunk separator is added \emph{before} all chunks, except the
  first one. Therefore, no chunk separator follows the last chunk. No
  newline (aka \sQuote{line feed}) is added to the value of the
  argument.

  Note that this driver does more than simply extract the code chunks
  verbatim, because chunks may re-use earlier chunks.
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
  values in parentheses show the default values). Character string
  values should be quoted when passed from \code{\link{Stangle}} through
  \code{\dots} but not when used in the header of a code chunk.
  \describe{
    \item{extension:}{logical (TRUE) or character string. Extension to
      use for the file name, without the leading dot, when \code{split =
	TRUE}. The default extension is the value of \sQuote{engine}. If
      \code{FALSE}, no extension is added to the file name.}

    \item{engine:}{character string (\code{"R"}).  Only chunks with
      \code{engine} equal to \code{"R"} or \code{"S"} are processed.}

    \item{keep.source:}{logical (\code{TRUE}).  If
      \code{keep.source == TRUE} the original source is copied to the
      file.  Otherwise, deparsed source is output.}

    \item{eval:}{logical (\code{TRUE}).  If \code{FALSE}, the code chunk
      is copied across but commented out.}

    \item{ignore.on.tangle:}{logical (FALSE). If \code{TRUE}, the code
      chunk is ignored entirely. Chunks ignored on tangling are not
      written out to script files, but remain processed normally on
      weaving (unless \code{ignore = TRUE}). Therefore, chunks may still
      use \code{eval = TRUE} for weaving purposes, yet not be tangled.}

    \item{ignore:}{logical (FALSE). An alternative way to set both
      \code{ignore.on.weave} of \code{\link{RweaveLatex}} and
      \code{ignore.on.tangle}.}

    \item{tangle:}{An alternative way to specify \code{!ignore.on.tangle}.}

    \item{prefix:}{Used if \code{split = TRUE}.  See \code{prefix.string}.}

    \item{prefix.string:}{a character string, default is the name of the
      source file (without extension).  Used if \code{split = TRUE} as
      the prefix for the filename if the chunk has no label, or if it
      has a label and \code{prefix = TRUE}.  Note that this is used as
      part of filenames, so needs to be portable.}

    \item{show.line.nos}{logical (\code{FALSE}).  Should the output be
      annotated with comments showing the line number of the first code line
      of the chunk?}
  }

  Option \code{extension} is specially useful with \code{ignore.on.weave
  = TRUE} of \code{\link{RweaveLatex}} to include code or text that the
  engine would not be able to parse.

  Note that options may not take their values from objects on tangling,
  since code chunks are never evaluated. For options set this way for
  weaving purposes, the driver will use the default value, with a
  warning.
}

\author{
  Friedrich Leisch and R-core, with contributions by Vincent Goulet.
}
\seealso{
  \sQuote{\href{../doc/Sweave.pdf}{Sweave User Manual}}, a vignette in
  the \pkg{utils} package.%% ../vignettes/Sweave.Rnw

  \code{\link{Sweave}}, \code{\link{RweaveLatex}}
}
\examples{
## Simple example
nmRnw <- "example-1.Rnw"
exfile <- system.file("Sweave", nmRnw, package = "utils")

## Check the contents of the file
if (interactive()) file.show(exfile)

## Tangle the file in the current working directory with the
## default options
Stangle(exfile)

## View the R script
nmR <- sub("Rnw$", "R", nmRnw) # default R output file name
if (interactive()) file.show(nmR)
\dontshow{file.rename(nmR, sub(".R$", "-default.R", nmR))}
## Reduce spacing between code chunks to a single blank line
Stangle(exfile, chunk.sep = "\n")
if (interactive()) file.show(nmR)
\dontshow{file.rename(nmR, sub(".R$", "-single.R", nmR))}
## Repeat with custom annotations
my.Ann <- function(options, chunk, output) {
    cat("### chunk #", options$chunknr, ": ",
        options$label \%||\% .RtangleCodeLabel(chunk),
        if (!options$eval) " (eval = FALSE)", "\n",
        file = output, sep = "")
}
Stangle(exfile, annotate = my.Ann, chunk.sep = "\n")
if (interactive()) file.show(nmR)
\dontshow{file.rename(nmR, sub(".R$", "-myAnn.R", nmR))}
## Options taking their values from objects for weaving
## purposes instead use their default value, with a warning,
## on tangling
nmRnw <- "example-2.Rnw"
exfile <- system.file("Sweave", nmRnw, package = "utils")
if (interactive()) file.show(exfile)
Stangle(exfile)
if (interactive()) file.show(sub("Rnw$", "R", nmRnw))

## More advanced example with finer control of output
nmRnw <- "example-3.Rnw"
exfile <- system.file("Sweave", nmRnw, package = "utils")
if (interactive()) file.show(exfile)
Stangle(exfile)

## Note in the R script: chunk #1 commented out; invalid R
## code in chunks #2, #4 and #5 extracted verbatim; no
## separator between chunks #2 and #3; last chunk left out
## of the script
nmR <- sub("Rnw$", "R", nmRnw)
if (interactive()) file.show(nmR)

## The effect of 'chunk.sep=FALSE' in chunk #3 is more
## interesting without the annotations
Stangle(exfile, annotate = FALSE, chunk.sep = "\n")
if (interactive()) file.show(nmR)
\dontshow{file.rename(nmR, sub(".R$", "-noAnn.R", nmR))}
## With 'drop.evalFALSE = TRUE', unevaluated chunks are
## entirely dropped from the tangled script
Stangle(exfile, annotate = FALSE, drop.evalFALSE = TRUE)
if (interactive()) file.show(nmR)
\dontshow{file.rename(nmR, sub(".R$", "-noAnn-noF.R", nmR))}
## Tangling with 'split=TRUE' extracts the shell script in
## chunk #4 to a file 'hello.sh', and the text file in
## chunk #5 to a file 'README'
Stangle(exfile, split = TRUE, annotate = FALSE)
if (interactive()) file.show("hello.sh")
if (interactive()) file.show("README")
}
\keyword{utilities}
