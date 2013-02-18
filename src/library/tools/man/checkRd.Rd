% File src/library/tools/man/checkRd.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2008-13 R Core Team
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
  \item{defines}{ string(s) to use in \verb{#ifdef} tests. }
  \item{stages}{ at which stage (\code{"build"}, \code{"install"}, or
    \code{"render"}) should \verb{\Sexpr} macros be executed? See the
    notes below.}
  \item{unknownOK}{ unrecognized macros are treated as errors if
    \code{FALSE}, otherwise warnings. }
  \item{listOK}{ unnecessary non-empty braces (e.g., around text, not as
    an argument) are treated as errors if \code{FALSE}, otherwise
    warnings.}
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
  \verb{#ifdef} and \verb{\Sexpr} processing, drops sections that
  would not be rendered or are duplicated (and should not be) and
  removes empty sections.

  An Rd object is passed through \code{prepare_Rd}, but it may already
  have been (and installed Rd objects have).

  Warnings are given a \sQuote{level}: those from \code{prepare_Rd} have
  level 0.  These include
  \tabular{l}{
    All text must be in a section\cr
    Only one \var{tag name} section is allowed: the first will be used\cr
    Section \var{name} is unrecognized and will be dropped\cr
    Dropping empty section \var{name}\cr
  }
  \code{checkRd} itself can show
  \tabular{rl}{
    7 \tab Tag \var{tag name} not recognized\cr
    7 \tab \verb{\tabular} format must be simple text\cr
    7 \tab Unrecognized \verb{\tabular} format: \ldots\cr
    7 \tab Only \var{n} columns allowed in this table\cr
    7 \tab Must have a \var{tag name}\cr
    7 \tab Only one \var{tag name} is allowed\cr
    7 \tab Tag \var{tag name} must not be empty\cr
    7 \tab \verb{\docType} must be plain text\cr
    5 \tab Tag \verb{\method} is only valid in \verb{\usage}\cr
    5 \tab Tag \verb{\dontrun} is only valid in \verb{\examples}\cr
    5 \tab Tag \var{tag name} is invalid in a \var{block name} block\cr
    5 \tab Title of \verb{\section} must be non-empty plain text\cr
    5 \tab \verb{\title} content must be plain text\cr
    3 \tab Empty section \var{tag name}\cr
    3 \tab \verb{\synopsis} will be removed in R 3.1.0\cr
    -1 \tab Non-ASCII contents without declared encoding\cr
    -1 \tab Non-ASCII contents in second part of \verb{\enc}\cr
    -3 \tab Tag \verb{\ldots} is not valid in a code block\cr
    -3 \tab Apparent non-ASCII contents without declared encoding\cr
    -3 \tab Apparent non-ASCII contents in second part of \verb{\enc}\cr
    -3 \tab Unnecessary braces at \ldots\cr
    -3 \tab \verb{\method} not valid outside a code block\cr
  }
  and variations with \verb{\method} replaced by \verb{\S3method} or
  \verb{\S4method}.

  Note that both \code{prepare_Rd} and \code{checkRd} have tests for an
  empty section: that in \code{checkRd} is stricter (essentially that
  nothing is output).
}
\value{
  This may fail through an \R error, but otherwise warnings are
  collected as returned as an object of class \code{"checkRd"}, a
  character vector of messages.  This class has a \code{print} method
  which only prints unique messages, and has argument \code{minlevel}
  that can be used to select only more serious messages.  (This is set
  to \code{-1} in \command{R CMD check}.)

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
