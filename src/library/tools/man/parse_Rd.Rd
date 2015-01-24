% File src/library/tools/man/parse_Rd.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2008-2014 R Core Team
% Distributed under GPL 2 or later

\name{parse_Rd}
\alias{parse_Rd}
\alias{print.Rd}
\alias{as.character.Rd}
\title{ Parse an Rd file }
\description{
  This function reads an R documentation (Rd) file and parses it, for
  processing by other functions.
}
\usage{
parse_Rd(file, srcfile = NULL, encoding = "unknown",
         verbose = FALSE, fragment = FALSE, warningCalls = TRUE,
	 macros = file.path(R.home("share"), "Rd", "macros", "system.Rd"),
         permissive = FALSE)
\method{print}{Rd}(x, deparse = FALSE, \dots)
\method{as.character}{Rd}(x, deparse = FALSE, \dots)
}
\arguments{
  \item{file}{A filename or text-mode connection.  At present filenames
    work best.}
  \item{srcfile}{\code{NULL}, or a \code{"srcfile"} object.  See the
    \sQuote{Details} section.}
  \item{encoding}{Encoding to be assumed for input strings.}
  \item{verbose}{Logical indicating whether detailed parsing
    information should be printed.}
  \item{fragment}{Logical indicating whether file represents a complete
    Rd file, or a fragment.}
  \item{warningCalls}{Logical: should parser warnings include the call?}
  \item{macros}{Filename or environment from which to load additional
    macros, or a logical value.  See the Details below.}
  \item{permissive}{Logical indicating that unrecognized macros
    should be treated as text with no warning.}
  \item{x}{An object of class Rd.}
  \item{deparse}{If \code{TRUE}, attempt to reinstate the escape characters
    so that the resulting characters will parse to the same object.}
  \item{\dots}{Further arguments to be passed to or from other methods.}
}
\details{
  This function parses \file{Rd} files according to the specification given
  in \url{http://developer.r-project.org/parseRd.pdf}.

  It generates a warning for each parse error and attempts to continue
  parsing.  In order to continue, it is generally necessary to drop some
  parts of the file, so such warnings should not be ignored.  
  
  Files without a marked encoding are by default assumed to be in the
  native encoding.  An alternate default can be set using the
  \code{encoding} argument.  All text in files is translated to the
  UTF-8 encoding in the parsed object.
  
  As from \R version 3.2.0, User-defined macros may be given in a
  separate file using \samp{\newcommand} or \samp{\renewcommand}.
  An environment may also be given:  it would be produced by
  \code{\link{loadRdMacros}}, \code{\link{loadPkgRdMacros}}, or
  by a previous call to \code{parse_Rd}.  If a logical value
  is given, only the default built-in macros will be used; 
  \code{FALSE} indicates that no \code{"macros"} attribute
  will be returned with the result.
  
  The \code{permissive} argument allows text to be parsed that is
  not completely in Rd format.  Typically it would be LaTeX code,
  used in an Rd fragment, e.g.\sspace{}in a \code{\link{bibentry}}.  
  With \code{permissive = TRUE}, this will be passed through as plain 
  text.  Since \code{parse_Rd} doesn't know how many arguments
  belong in LaTeX macros, it will guess based on the presence
  of braces after the macro; this is not infallible.
}
\value{
  \code{parse_Rd} returns an object of class \code{"Rd"}.  The
  internal format of this object is subject to change.  The
  \code{as.character()} and \code{print()} methods defined for the
  class return character vectors and print them, respectively.

  Unless \code{macros = FALSE}, the object will have an attribute 
  named \code{"macros"}, which is an environment containing the 
  macros defined in \code{file}, in a format that can be used for 
  further \code{parse_Rd} calls  in the same session.  It is not 
  guaranteed to work if saved to a file and reloaded in a different 
  session.
}
\references{ \url{http://developer.r-project.org/parseRd.pdf} }
\author{ Duncan Murdoch }
\seealso{
  \code{\link{Rd2HTML}} for the converters that use the output of
  \code{parse_Rd()}.
}
\keyword{utilities}
\keyword{documentation}
