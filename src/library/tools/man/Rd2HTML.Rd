% File src/library/tools/man/Rd2HTML.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2008-9 R Core Development Team
% Distributed under GPL 2 or later

\name{Rd2HTML}
\alias{Rd2txt}
\alias{Rd2HTML}
\alias{Rd2ex}
\alias{Rd2latex}
\title{ Rd Converters }
\description{
  These functions take the output of the \code{\link{parse_Rd}} function
  and produce a help page from it.  As they are mainly
  intended for internal use, their interfaces are subject to change.
}
\usage{
Rd2HTML(Rd, out = "", package = "", defines = .Platform$OS.type,
        Links = NULL, Links2 = NULL,
        stages = "render", outputEncoding = "UTF-8", 
        dynamic = FALSE, no_links = FALSE, ...)

Rd2txt(Rd, out = "", package = "", defines = .Platform$OS.type,
       stages = "render", outputEncoding = "",
       width = getOption("help_text_width", 80L), ...)

Rd2latex(Rd, out = "", defines = .Platform$OS.type,
         stages = "render", outputEncoding = "ASCII", ...)

Rd2ex(Rd, out = "", defines = .Platform$OS.type,
      stages = "render", outputEncoding = "UTF-8", ...)
}
\arguments{
  \item{Rd}{ a filename or \code{Rd} object to use as input. }
  \item{out}{ a filename or connection object to which to write the output. }
  \item{package}{ the package to list in the output. }
  \item{defines}{ string(s) to use in \verb{#ifdef} tests. }
  \item{stages}{ at which stage (\code{"build"}, \code{"install"}, or
    \code{"render"}) should \verb{\Sexpr} macros be executed? See the
    notes below.}
  \item{outputEncoding}{ see the \sQuote{Encodings} section below.}
  \item{dynamic}{logical: set links for render-time resolution by
      dynamic help system.}
  \item{no_links}{logical: suppress hyperlinks to other help topics.
      Used by \command{R CMD \link{Rdconv}}.}
  \item{Links, Links2}{\code{NULL} or a named (by topics) character vector of
    links, as returned by \code{\link{findHTMLlinks}}.}
  \item{width}{The intended page width (in characters) for which the
      text rendering should be designed.}
  \item{...}{ additional parameters to pass to \code{\link{parse_Rd}} when
    \code{Rd} is a filename. }
}
\details{
  These functions convert help pages: \code{Rd2HTML} produces HTML (or
  compiled HTML, for Windows), \code{Rd2txt} produces plain text,
  \code{Rd2latex} produces LaTeX input.  By default,
  \code{outputEncoding} is set to \code{""} and the native character
  encoding (shown by \code{\link{localeToCharset}}) is used.

  \code{Rd2ex} extracts the examples in the format used by
  \code{\link{example}} and \R utilities.  it reproduces the previous
  version up to
  \itemize{
    \item The parser does not interpret markup in \R comments: this used
    to be done.
    \item Small improvements in layout.
  }
  Each of the functions accepts a filename for an Rd file, and
  will use \code{\link{parse_Rd}} to parse it before applying the
  conversions or checks.
  
  The difference between arguments \code{Link} and \code{Link2} is that
  links are looked in them in turn, so lazy-evaluation can be used to
  only do a second-level search for links if required.
  
  \code{Rd2txt} will format text paragraphs to a width determined by
  \code{width}, with appropriate margins.  The default is to be close to
  the rendering in versions of \R < 2.10.0.
  
  \code{Rd2txt} will use directional quotes (see \code{\link{sQuote}})
  if option \code{"useFancyQuotes"} is true (the default) and
#ifdef unix
  the current encoding is UTF-8.
#endif
#ifdef windows
  the current locale uses a single-byte encoding (except C).
  (Directional quotes are not attempted in CJK locales as they are
  usually double-width, which looks wrong with English text.)
#endif
}
\section{Encodings}{
  Rd files are normally intended to be rendered on a wide variety of
  systems, so care must be taken in the encoding of non-ASCII
  characters.  In general, any such encoding should be declared using
  the \samp{encoding} section for there to be any hope of correct
  rendering.

  For output, the \code{outputEncoding} argument will be used:
  \code{outputEncoding = ""} will choose the native encoding for the
  current system.  The \code{Rd2latex} function is an exception to this
  rule, because UTF-8 is not a viable universal choice for LaTeX: if an
  input file contains an \samp{encoding} section, \code{Rd2latex} will
  use that for the output encoding.

  If the text cannot be converted to the \code{outputEncoding}, byte
  substitution will be used (see \code{\link{iconv}}): \code{Rd2latex}
  and \code{Rd2ex} give a warning. 
}

\note{
  The \verb{\Sexpr} macro is a new addition to Rd files.  It includes
  \R code that will be executed at one of three times: \emph{build} time
  (when a package's source code is built into a tarball, not yet
  implemented), \emph{install} time (when the package is installed or
  built into a binary package), and \emph{render} time (when the man
  page is converted to a readable format).

  Currently only text format man pages render when displayed; all other
  formats render when installed.

  For example, this man page was:
  \enumerate{
    \item built on
    \Sexpr[stage=build]{format(Sys.time(), "\%Y-\%m-\%d at \%H:\%M:\%S")},
    \item installed on
    \Sexpr[stage=install]{format(Sys.time(), "\%Y-\%m-\%d at \%H:\%M:\%S")}, and
    \item rendered on
    \Sexpr[stage=render]{format(Sys.time(), "\%Y-\%m-\%d at \%H:\%M:\%S")}.
  }
  Because build-time execution is not yet supported, the first of these
  will display as a macro; the other two may or may not, depending on the
  build of \R.  (Note that escapes, e.g. for \samp{\%}, may not be
  identical in this display, but the displayed Rd code should parse to the
  same object.)
}

\value{
  These functions are executed mainly for the side effect of
  writing the converted help page.  Their value is the name of the output
  file (invisibly).  For \code{Rd2latex}, the output name is given an
  attribute \code{"latexEncoding"} giving the encoding of the file in a
  form suitable for use with the LaTeX \samp{inputenc} package.
}
\author{
  Duncan Murdoch, Brian Ripley
}
\references{ \url{http://developer.r-project.org/parseRd.pdf} }
\seealso{
  \code{\link{parse_Rd}}, \code{\link{checkRd}},
  \code{\link{findHTMLlinks}}.
}
\examples{\donttest{

\dontrun{
## Simulate install and rendering of this page in HTML and text format:

Rd <- file.path("src/library/tools/Rd2HTML.Rd")

outfile <- paste(tempfile(), ".html", sep="")
browseURL(Rd2HTML(Rd, outfile, package="tools", stages=c("install", "render")))

outfile <- paste(tempfile(), ".txt", sep="")
file.show(Rd2txt(Rd, outfile, package="tools", stages=c("install", "render")))

checkRd(con) # A stricter test than Rd2HTML uses
}}}
\keyword{ documentation }
