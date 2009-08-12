% File src/library/tools/man/Rd2HTML.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2008-9 R Core Development Team
% Distributed under GPL 2 or later

\name{Rd2HTML}
\Rdversion{1.1}
\alias{Rd2txt}
\alias{Rd2HTML}
\alias{Rd2ex}
\alias{Rd2latex}
\alias{checkRd}
\alias{findHTMLlinks}
\title{ Work with an Rd object }
\description{
  These functions take the output of the
  \code{\link{parse_Rd}} function and check it or produce a help
  page from it.  As they are mainly intended for internal
  use, their interfaces are subject to
  change.
}
\usage{
Rd2HTML(Rd, out = "", package = "", defines = .Platform$OS.type,
        Links = NULL, CHM = FALSE,
        stages = "render", outputEncoding = "UTF-8", ...)

Rd2txt(Rd, out = "", package = "", defines = .Platform$OS.type,
       stages = "render", outputEncoding = "", ...)

Rd2latex(Rd, out = "", defines = .Platform$OS.type,
         stages = "render", outputEncoding = "latin1", ...)

Rd2ex(Rd, out = "", defines = .Platform$OS.type,
      stages = "render", outputEncoding = "UTF-8", ...)

checkRd(Rd, defines = .Platform$OS.type, stages = "render",
        unknownOK = TRUE, listOK = TRUE, ...)

findHTMLlinks(pkgDir = "", lib.loc = NULL)
}
\arguments{
  \item{Rd}{ a filename or \code{Rd} object to use as input. }
  \item{out}{ a filename or connection object to which to write the output. }
  \item{package}{ the package to list in the output. }
  \item{defines}{ string(s) to use in \code{"#ifdef"} tests. }
  \item{stages}{ at which stage (\code{"build"}, \code{"install"}, or
    \code{"render"} should \\Sexpr macros be executed? See the notes below.}
  \item{outputEncoding}{ see the \sQuote{Encodings} section below.}
  \item{Links}{\code{NULL} or a named (by topics) character vector of
    links, as returned by \code{findHTMLlinks}.}
  \item{CHM}{logical: prepare HTML suitable for Compiled HTML?}
  \item{unknownOK}{ unrecognized macros are treated as errors if
    \code{FALSE}, otherwise warnings. }
  \item{listOK}{ unnecessary non-empty braces (e.g. around text, not
    as an argument) are treated as errors if \code{FALSE}, otherwise warnings. }
  \item{...}{ additional parameters to pass to \code{\link{parse_Rd}} when
    \code{Rd} is a filename. }
  \item{pkgDir}{the top-level directory of an installed package.}
  \item{lib.loc}{character vector describing the location of \R library trees
    to scan: the default indicates \code{\link{.libPaths}()}.}
}
\details{

  The \code{Rd2*} functions convert man pages:
  \code{Rd2HTML} produces HTML (or compiled HTML, for Windows),
  \code{Rd2txt} produces plain text, \code{Rd2latex} produces
  LaTeX input.  By default, \code{outputEncoding} is set to \code{""}
  and the native character encoding (shown by \code{\link{localeToCharset}})
  is used.

  The converters are still in development, and may still make some errors
  in rendering.

  \code{Rd2ex} extracts the examples in the format used by
  \code{\link{example}} and \R utilities.  it reproduces the previous
  version up to
  \itemize{
    \item The parser does not interpret markup in \R comments: this used
    to be done.
    \item Small improvements in layout.
  }

  \code{checkRd} makes consistency checks on an Rd file, confirming that
  required sections are present, etc.

  Each of the above functions accepts a filename for an Rd file, and
  will use \code{\link{parse_Rd}} to parse it before applying the
  conversions or checks.

  \code{findHTMLlinks} tries to resolve links from one man page to another.  It
  uses in decreasing priority
  \itemize{
    \item The package in \code{pkgDir}: this is used when converting
    HTML help for that package.
    \item The packages that are loaded in a vanilla \R session.
    \item Other packages found in the library trees specified by
    \code{lib.loc} in the order of the trees and alphabetically within a
    library tree.
  }
}
\section{Encodings}{
  Rd files are normally intended to be rendered on a wide variety of
  systems, so care must be taken in the encoding of non-ASCII
  characters.  In general, any such encoding should be declared using
  the \samp{encoding} section for there to be any hope of correct
  rendering.  Avoid exotic encodings unless absolutely necessary: all
  input will be translated to UTF-8, and the quality of such
  translations is poor on some older systems.

  For output, the \code{outputEncoding} variable will be used:
  \code{outputEncoding=""} will choose the native encoding for the
  current system.  The \code{Rd2latex} function is an exception to this
  rule, because LaTeX installations vary considerably in their ability
  to handle encodings.  If an input file contains an \samp{encoding}
  section, \code{Rd2latex} will use that as the output encoding as well.
  Unfortunately, on some older LaTeX installations the conversion to
  UTF-8 and back is not perfect, and some characters may not be rendered
  properly.
}

\note{
  The \\Sexpr macro is a new addition to Rd files.  It includes \R code
  that will be executed at one of three times:  build time (when a
  package's source code is built into a tarball, not yet implemented),
  install time (when the package code is compiled and installed, or
  compiled and built into a binary package), and render time (when the
  man page is converted to a readable format). Depending on the output
  format, render time may be the same as install time.

  Currently only text format man pages render when displayed, all other
  formats render when installed, and in all cases, handling of \\Sexpr
  macros needs to be enabled during the build of R (by defining the
  \code{USE_NEW_HELP} environment variable).  If not enabled, the macro
  will show, rather than the result of evaluating the R code.

For example, this man page was:
\enumerate{
\item built on
\Sexpr[stage=build]{format(Sys.time(), "\%Y-\%m-\%d at \%H:\%M:\%S")},
\item installed on
\Sexpr[stage=install]{format(Sys.time(), "\%Y-\%m-\%d at \%H:\%M:\%S")}, and
\item rendered on
\Sexpr[stage=render]{format(Sys.time(), "\%Y-\%m-\%d at \%H:\%M:\%S")}.
}
Because build-time execution is not yet supported, the first of these will
display as a macro; the other two may or may not,
depending on the build of \R.  (Note that escapes, e.g. for \samp{\%},
may not be identical in this display, but the displayed Rd code should parse 
to the same object.)
}

\value{

The \code{Rd2*} functions are executed mainly for the side effect of
writing the converted help page.  Their value is the name of the
output page.  For \code{Rd2latex}, the output name is given an
attribute \code{"latexEncoding"} giving the encoding of the file in a
form suitable for use with the LaTeX \samp{inputenc} package.

\code{checkRd} returns \code{TRUE} if the file has no errors; it
generates an R error otherwise.

\code{findHTMLlinks} returns a named character vector of relative
file paths, in the same package or in a package in the same library tree.
}
\author{
  Duncan Murdoch, Brian Ripley
}
\references{ \url{http://developer.r-project.org/parseRd.pdf} }
\seealso{
  \code{\link{parse_Rd}}
}
\examples{\donttest{

\dontrun{
## Simulate an install and rendering of this page in HTML and text format:

Rd <- file.path("src/library/tools/Rd2HTML.Rd")

outfile <- paste(tempfile(), ".html", sep="")
browseURL(Rd2HTML(Rd, outfile, package="tools", stages=c("install", "render")))

outfile <- paste(tempfile(), ".txt", sep="")
file.show(Rd2txt(Rd, outfile, package="tools", stages=c("install", "render")))

checkRd(con) # A stricter test than Rd2HTML uses
}}}
\keyword{ documentation }
