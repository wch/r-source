% File src/library/tools/man/checkRd.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2008-9 R Core Development Team
% Distributed under GPL 2 or later

\name{checkRd}
\alias{checkRd}
\title{ Check an Rd Object }
\description{
  Check an help file or the output of the \code{\link{parse_Rd}} function.
}
\usage{
checkRd(Rd, defines = .Platform$OS.type, stages = "render",
        unknownOK = TRUE, listOK = TRUE, ..., def_enc = FALSE)
}
\arguments{
  \item{Rd}{ a filename or \code{Rd} object to use as input. }
  \item{defines}{ string(s) to use in \code{"#ifdef"} tests. }
  \item{stages}{ at which stage (\code{"build"}, \code{"install"}, or
    \code{"render"} should \\Sexpr macros be executed? See the notes below.}
  \item{unknownOK}{ unrecognized macros are treated as errors if
    \code{FALSE}, otherwise warnings. }
  \item{listOK}{ unnecessary non-empty braces (e.g. around text, not
    as an argument) are treated as errors if \code{FALSE}, otherwise warnings. }
  \item{\dots}{ additional parameters to pass to \code{\link{parse_Rd}} when
    \code{Rd} is a filename.  One that is often useful is \code{encoding}.}
  \item{def_enc}{logical: has the package declared an encoding, so tests
    for non-ASCII text are suppressed?}
}
\details{
  \code{checkRd} performs consistency checks on an Rd file, confirming that
  required sections are present, etc.

  It accepts a filename for an Rd file, and will use
  \code{\link{parse_Rd}} to parse it before applying the checks.  If so,
  warnings from \code{parse_Rd} are collected, together with those from
  the internal function \code{prepare_Rd}, which does the
  \code{"#ifdef"} and \\Sexpr processing, drops sections that would not
  be rendered or are duplicated (and should not be) and removes empty
  sections.

  An Rd object is passed through \code{prepare_Rd}, but it may already
  have been (and installed Rd objects have).
}
\value{
  This may fail through an \R error, but otherwise warnings are
  collected as returned as an object of class \code{"checkRd"}, a
  character vector of messages.  This class has a \code{print} method
  which only prints unique messages, and has argument \code{minlevel}
  that can be used to select only more serious messages.

  Possible fatal errors are those from running the parser (e.g. a
  non-existent file, unclosed quoted string, non-ASCII input without a
  specified encoding) or from \code{prepare_Rd} (multiple
  \verb{\Rdversion} declarations, invalid \verb{\encoding} or
  \verb{\docType} or \verb{\name} sections, and missing or duplicate
  \verb{\name} or \verb{\title} sections).
}
\author{
  Duncan Murdoch, Brian Ripley
}
\seealso{
  \code{\link{parse_Rd}}, \code{\link{Rd2HTML}}.
}

\keyword{ documentation }
