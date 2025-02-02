% File src/library/tools/man/Rd2HTML.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2008-2025 R Core Team
% Distributed under GPL 2 or later

\name{Rd2HTML}
\alias{Rd2txt}
\alias{Rd2HTML}
\alias{Rd2ex}
\alias{Rd2latex}
\title{ Rd Converters }
\description{
  These functions take the output of \code{\link{parse_Rd}()}, an
  \code{Rd} object, and produce a help page from it.  As they are mainly
  intended for internal use, their interfaces are subject to change.
}
\usage{
Rd2HTML(Rd, out = "", package = "", defines = .Platform$OS.type,
        Links = NULL, Links2 = NULL,
        stages = "render", outputEncoding = "UTF-8",
        dynamic = FALSE, no_links = FALSE, fragment = FALSE,
        stylesheet = if (dynamic) "/doc/html/R.css" else "R.css",
        texmath = getOption("help.htmlmath"),
        concordance = FALSE,
        standalone = TRUE,
        hooks = list(),
        toc = isTRUE(getOption("help.htmltoc")),
        Rhtml = FALSE,
        ...)

Rd2txt(Rd, out = "", package = "", defines = .Platform$OS.type,
       stages = "render", outputEncoding = "",
       fragment = FALSE, options, ...)

Rd2latex(Rd, out = "", defines = .Platform$OS.type,
         stages = "render", outputEncoding = "UTF-8",
         fragment = FALSE, ..., writeEncoding = outputEncoding != "UTF-8",
         concordance = FALSE)

Rd2ex(Rd, out = "", defines = .Platform$OS.type,
      stages = "render", outputEncoding = "UTF-8",
      commentDontrun = TRUE, commentDonttest = FALSE, ...)
}
\arguments{
  \item{Rd}{ a filename or \code{Rd} object to use as input. }
  \item{out}{a filename or connection object to which to write the
    output.  The default \code{out = ""} is equivalent to
    \code{out = \link{stdout}()}.}
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
  \item{fragment}{logical:  should fragments of Rd files be accepted?  See the
      notes below.}
  \item{stylesheet}{character: a URL for a stylesheet to be used in the header
      of the HTML output page.}
  \item{texmath}{character: controls how mathematics in \verb{\eqn} and
      \verb{\deqn} commands are typeset in HTML output. Useful values
      are \code{"katex"} (default) and \code{"mathjax"} to use
      \href{https://katex.org/}{\I{KaTeX}} or
      \href{https://www.mathjax.org/}{\I{MathJax}} respectively, otherwise
      basic substitutions are used. May be ignored under certain
      circumstances, e.g., if the help page already uses macros from the
      \code{\link[mathjaxr]{mathjaxr}} package.
    }
  \item{concordance}{Whether concordance data should be embedded in the
    output file and attached to the return value.}
  \item{standalone}{logical: whether the output is intended to be a
    standalone HTML file. If \code{FALSE}, the header and footer are
    omitted, so that the output can be combined with other fragments.
  }
  \item{hooks}{a list of functions controlling details of
    output. Currently the only component used is \code{pkg_href}, which
    is used by \code{pkg2HTML} to determine the output HTML file path
    given a package name as input. }
  \item{toc}{logical: whether the HTML output should include a table
      of contents. Ignored unless \code{standalone = TRUE}. }
  \item{Rhtml}{logical: whether the output is intended to be a \I{Rhtml}
      file that can be processed using \CRANpkg{knitr}. If \code{TRUE}, the
      examples section is wrapped inside a \code{rcode} block. }
  \item{Links, Links2}{\code{NULL} or a named (by topics) character vector of
    links, as returned by \code{\link{findHTMLlinks}}.}
  \item{options}{an optional named list of options to pass to
    \code{\link{Rd2txt_options}}.}
  \item{...}{ additional parameters to pass to \code{\link{parse_Rd}} when
    \code{Rd} is a filename. }
  \item{writeEncoding}{logical: should an \verb{\inputencoding} line be written
      for non-ASCII encodings? \LaTeX release 2018-04-01 and later
      default to UTF-8; the declaration is obsolete in that case.}
  \item{commentDontrun}{logical: should \verb{\dontrun} sections be commented
      out?}
  \item{commentDonttest}{logical: should \verb{\donttest} sections be commented out?}
}
\details{
  These functions convert help documents: \code{Rd2HTML} produces HTML,
  \code{Rd2txt} produces plain text, \code{Rd2latex} produces LaTeX.
  \code{Rd2ex} extracts the examples in the format used by
  \code{\link{example}} and \R utilities.

  Each of the functions accepts a filename for an Rd file, and
  will use \code{\link{parse_Rd}} to parse it before applying the
  conversions or checks.

  The difference between arguments \code{Link} and \code{Link2} is that
  links are looked in them in turn, so lazy-evaluation can be used to
  only do a second-level search for links if required.

  Before \R 3.6.0, the default for \code{Rd2latex} was \code{outputEncoding = "ASCII"},
  including using the second option of \verb{\enc} markup, because \LaTeX
  versions did not provide enough coverage of UTF-8 glyphs for a long time.

  \code{Rd2txt} will format text paragraphs to a width determined by
  \code{width}, with appropriate margins.  The default is to be close to
  the rendering in versions of \R < 2.10.0.

  \code{Rd2txt} will use directional quotes (see \code{\link{sQuote}})
  if option \code{"useFancyQuotes"} is true (usually the default,
  see \code{\link{sQuote}}) and
#ifdef unix
  the current encoding is UTF-8.
#endif
#ifdef windows
  the current encoding is UTF-8 or a single-byte encoding (except C).
  (Directional quotes are not attempted in East Asian locales as they are
  usually double-width, which looks wrong with English text.)
#endif

  Various aspects of formatting by \code{Rd2txt} are controlled by the
  \code{options} argument, documented with the \code{\link{Rd2txt_options}}
  function.  Changes made using \code{options} are temporary, those
  made with \code{\link{Rd2txt_options}} are persistent.

  When \code{fragment = TRUE}, the \code{Rd} file will be rendered
  with no processing of \verb{\Sexpr} elements or conditional defines
  using \verb{#ifdef} or \verb{#ifndef}.  Normally a fragment represents
  text within a section, but if the first element of the fragment
  is a section macro, the whole fragment will be rendered as
  a series of sections, without the usual sorting.
}
\section{Encodings}{
  Rd files are normally intended to be rendered on a wide variety of
  systems, so care must be taken in the encoding of non-ASCII
  characters.  In general, any such encoding should be declared using
  the \samp{encoding} section for there to be any hope of correct
  rendering.

  For output, the \code{outputEncoding} argument will be used:
  \code{outputEncoding = ""} will choose the native encoding for the
  current system.

  If the text cannot be converted to the \code{outputEncoding}, byte
  substitution will be used (see \code{\link{iconv}}): \code{Rd2latex}
  and \code{Rd2ex} give a warning.
}

\note{
  The \verb{\Sexpr} macro includes
  \R code that will be executed at one of three times: \emph{build} time
  (when a package's source code is built into a tarball),
  \emph{install} time (when the package is installed or
  built into a binary package), and \emph{render} time (when the man
  page is converted to a readable format).

  For example, this man page was:
  \enumerate{
    \item built on
    \Sexpr[stage=build]{format(Sys.time(), "\%Y-\%m-\%d at \%H:\%M:\%S")},
    \item installed on
    \Sexpr[stage=install]{format(Sys.time(), "\%Y-\%m-\%d at \%H:\%M:\%S")}, and
    \item rendered on
    \Sexpr[stage=render]{format(Sys.time(), "\%Y-\%m-\%d at \%H:\%M:\%S")}.
  }
}

\value{
  These functions are executed mainly for the side effect of writing the
  converted help page.  Their value is the name of the output file
  (invisibly).  For \code{Rd2latex}, the output name is given an
  attribute \code{"latexEncoding"} giving the encoding of the file in a
  form suitable for use with the LaTeX \samp{inputenc} package. For
  \code{Rd2HTML} with \code{standalone = FALSE}, an attribute
  \code{"info"} gives supplementary information such as the contents of
  the \code{name} and \code{title} fields. This is currently
  experimental, and the details are subject to change.
  
  For \code{Rd2HTML} and \code{Rd2latex} with \code{concordance = TRUE}, a 
  \code{"concordance"} attribute is added, containing an
  \code{\link{Rconcordance}} object.
}
\author{
  Duncan Murdoch, Brian Ripley
}
\references{ \url{https://developer.r-project.org/parseRd.pdf} }
\seealso{
  \code{\link{parse_Rd}}, \code{\link{checkRd}},
  \code{\link{findHTMLlinks}}, \code{\link{Rd2txt_options}},
  \code{\link{matchConcordance}}.
}
\examples{\donttest{% timestamp, browseURL()
## Simulate rendering of this (installed) page in HTML and text format
Rd <- Rd_db("tools")[["Rd2HTML.Rd"]]

outfile <- tempfile(fileext = ".html")
Rd2HTML(Rd, outfile, package = "tools") |> browseURL()

outfile <- tempfile(fileext = ".txt")
Rd2txt(Rd, outfile, package = "tools") |> file.show()
}}
\keyword{ documentation }
