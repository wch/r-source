% File src/library/utils/man/RweaveLatex.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{RweaveLatex}
\alias{RweaveLatex}
\alias{RweaveLatexSetup}
\title{R/LaTeX Driver for Sweave}
\description{
  A driver for \code{\link{Sweave}} that translates R code chunks in
  LaTeX files.
}
\usage{
RweaveLatex()

RweaveLatexSetup(file, syntax, output = NULL, quiet = FALSE,
                 debug = FALSE, stylepath, \dots)
}
\arguments{
  \item{file}{Name of Sweave source file.  See the description of the
    corresponding argument of \code{\link{Sweave}}.}
  \item{syntax}{An object of class \code{SweaveSyntax}.}
  \item{output}{Name of output file.  The default is to remove extension
    \file{.nw}, \file{.Rnw} or \file{.Snw} and to add
    extension \file{.tex}.  Any directory paths in
    \code{file} are also removed such that the output is
    created in the current working directory.}
  \item{quiet}{If \code{TRUE} all progress messages are suppressed.}
  \item{debug}{If \code{TRUE}, input and output of all code
    chunks is copied to the console.}
  \item{stylepath}{See \sQuote{Details}.}
  \item{\dots}{named values for the options listed in \sQuote{Supported
      Options}.}
}
\details{
  The LaTeX file generated needs to contain the line
  \samp{\\usepackage\{Sweave\}}, and if this is not present in the
  Sweave source file (possibly in a comment), it is inserted by the
  \code{RweaveLatex} driver.  If \code{stylepath = TRUE}, a hard-coded
  path to the file \file{Sweave.sty} in the \R installation is set in
  place of \code{Sweave}.  The hard-coded path makes the LaTeX file less
  portable, but avoids the problem of installing the current version of
  \file{Sweave.sty} to some place in your TeX input path.  However, TeX
  may not be able to process the hard-coded path if it contains spaces
  (as it often will under Windows) or TeX special characters.

  The default for \code{stylepath} is now taken from the environment
  variable \env{SWEAVE_STYLEPATH_DEFAULT}, or is \code{FALSE} it that is
  unset or empty.  If set, it should be exactly \code{TRUE} or
  \code{FALSE}: any other values are taken as \code{FALSE}.

  The simplest way for frequent Sweave users to ensure that
  \file{Sweave.sty} is in the TeX input path is to add
  \file{\var{R_HOME}/share/texmf} as a \sQuote{texmf tree} (\sQuote{root
  directory} in the parlance of the \sQuote{MiKTeX settings} utility).

  By default, \file{Sweave.sty} sets the width of all included graphics to:\cr
  \samp{\\setkeys{Gin}{width=0.8\\textwidth}}.

  This setting affects the width size option passed to the
  \samp{\\includegraphics{}} directive for each plot file and in turn
  impacts the scaling of your plot files as they will appear in your
  final document.

  Thus, for example, you may set \code{width=3} in your figure chunk and
  the generated graphics files will be set to 3 inches in
  width.  However, the width of your graphic in your final document will
  be set to \samp{0.8\\textwidth} and the height dimension will be
  scaled accordingly.  Fonts and symbols will be similarly scaled in the
  final document.

  You can adjust the default value by including the
  \samp{\\setkeys{Gin}{width=...}} directive in your \file{.Rnw} file
  after the \samp{\\begin{document}} directive and changing the
  \code{width} option value as you prefer, using standard LaTeX
  measurement values.

  If you wish to override this default behavior entirely, you can add a
  \samp{\\usepackage[nogin]{Sweave}} directive in your preamble.  In this
  case, no size/scaling options will be passed to the
  \samp{\\includegraphics{}} directive and the \code{height} and
  \code{width} options will determine both the runtime generated graphic
  file sizes and the size of the graphics in your final document.

  \file{Sweave.sty} also supports the \samp{[noae]} option, which
  suppresses the use of the \samp{ae} package, the use of which may
  interfere with certain encoding and typeface selections.  If you have
  problems in the rendering of certain character sets, try this option.

  As from \R 3.1.0 it also supports the \samp{[inconsolata]} option, to
  render monospaced text in \code{inconsolata}, the font used by default
  for \R help pages.

  The use of fancy quotes (see \code{\link{sQuote}}) can cause problems
  when setting \R output.  Either set
  \code{\link{options}(useFancyQuotes = FALSE)} or arrange that LaTeX is
  aware of the encoding used (by a \samp{\\usepackage[utf8]\{inputenc\}}
  declaration: Windows users of \code{Sweave} from \command{Rgui.exe}
  will need to replace \samp{utf8} by \samp{cp1252} or similar) and
  ensure that typewriter fonts containing directional quotes are used.

  Some LaTeX graphics drivers do not include \samp{.png} or \samp{.jpg}
  in the list of known extensions.  To enable them, add something like
  \samp{\\DeclareGraphicsExtensions{.png,.pdf,.jpg}} to the preamble of
  your document or check the behavior of your graphics driver.  When
  both \code{pdf} and \code{png} are \code{TRUE} both files will be
  produced by \code{Sweave}, and their order in the
  \samp{DeclareGraphicsExtensions} list determines which will be used by
  \command{pdflatex}.
}

\section{Supported Options}{
  \code{RweaveLatex} supports the following options for code chunks (the
  values in parentheses show the default values).  Character string
  values should be quoted when passed from \code{\link{Sweave}} through
  \code{\dots} but not when use in the header of a code chunk.
  \describe{
    \item{engine:}{character string (\code{"R"}).  Only chunks with
      \code{engine} equal to \code{"R"} or \code{"S"} are processed.}

    \item{echo:}{logical (\code{TRUE}). Include \R code in the output
      file?}

    \item{keep.source:}{logical (\code{TRUE}).  When echoing, if
      \code{keep.source == TRUE} the original source is copied to the
      file.  Otherwise, deparsed source is echoed.}

    \item{eval:}{logical (\code{TRUE}).  If \code{FALSE}, the code chunk
      is not evaluated, and hence no text nor graphical output
      produced.}

    \item{results:}{character string (\code{"verbatim"}).  If
      \code{"verbatim"}, the output of \R commands is included in the
      verbatim-like \samp{Soutput} environment.  If \code{"tex"}, the
      output is taken to be already proper LaTeX markup and included as
      is.  If \code{"hide"} then all output is completely suppressed
      (but the code executed during the weave).  Values can be abbreviated.}

    \item{print:}{logical (\code{FALSE}).  If \code{TRUE}, this forces
      auto-printing of all expressions.}

    \item{term:}{logical (\code{TRUE}).  If \code{TRUE}, visibility of
      values emulates an interactive \R session: values of assignments
      are not printed, values of single objects are printed.  If
      \code{FALSE}, output comes only from explicit \code{\link{print}}
      or similar statements.}

    \item{split:}{logical (\code{FALSE}).  If \code{TRUE}, text output
      is written to separate files for each code chunk.}

    \item{strip.white:}{character string (\code{"true"}).  If
      \code{"true"}, blank lines at the beginning and end of output are
      removed.  If \code{"all"}, then all blank lines are removed from
      the output.  If \code{"false"} then blank lines are retained.

      A \sQuote{blank line} is one that is empty or includes only
      whitespace (spaces and tabs).

      Note that blank lines in a code chunk will usually produce a
      prompt string rather than a blank line on output.
    }

    \item{prefix:}{logical (\code{TRUE}).  If \code{TRUE} generated
      filenames of figures and output all have the common prefix given
      by the \code{prefix.string} option: otherwise only unlabelled
      chunks use the prefix.}

    \item{prefix.string:}{a character string, default is the name of the
      source file (without extension).  Note that this is used as part
      of filenames, so needs to be portable.}

    \item{include:}{logical (\code{TRUE}), indicating whether input
      statements for text output (if \code{split = TRUE}) and
      \samp{\\includegraphics} statements for figures should be
      auto-generated.  Use \code{include = FALSE} if the output should
      appear in a different place than the code chunk (by placing the
      input line manually).}

    \item{fig:}{logical (\code{FALSE}), indicating whether the code
      chunk produces graphical output.  Note that only one figure per
      code chunk can be processed this way.  The labels for figure
      chunks are used as part of the file names, so should preferably be
      alphanumeric.}

    \item{eps:}{logical (\code{FALSE}), indicating whether EPS figures
      should be generated.  Ignored if \code{fig = FALSE}.}

    \item{pdf:}{logical (\code{TRUE}), indicating whether PDF figures
      should be generated.  Ignored if \code{fig = FALSE}.}

    \item{pdf.version, pdf.encoding, pdf.compress:}{passed to
      \code{\link{pdf}} to set the version, encoding and compression (or
      not).  Defaults taken from \code{pdf.options()}.}

    \item{png:}{logical (\code{FALSE}), indicating whether PNG figures
      should be generated.  Ignored if \code{fig = FALSE}.  Only
      available in \R >= 2.13.0.}

    \item{jpeg:}{logical (\code{FALSE}), indicating whether JPEG figures
      should be generated.  Ignored if \code{fig = FALSE}.  Only
      available in \R >= 2.13.0.}

    \item{grdevice:}{character (\code{NULL}): see section \sQuote{Custom
	Graphics Devices}.  Ignored if \code{fig = FALSE}.  Only
      available in \R >= 2.13.0.}

    \item{width:}{numeric (6), width of figures in inches.  See
      \sQuote{Details}.}

    \item{height:}{numeric (6), height of figures in inches.  See
      \sQuote{Details}.}

    \item{resolution:}{numeric (300), resolution in pixels per inch:
      used for PNG and JPEG graphics.  Note that the default is a fairly
      high value, appropriate for high-quality plots.  Something like
      \code{100} is a better choice for package vignettes.}

    \item{concordance:}{logical (\code{FALSE}).  Write a concordance
      file to link the input line numbers to the output line numbers.
      This is an experimental feature; see the source code for the
      output format, which is subject to change in future releases.}

    \item{figs.only:}{logical (\code{FALSE}).
      By default each figure chunk is run once, then re-run for each
      selected type of graphics.  That will open a default graphics
      device for the first figure chunk and use that device for the first
      evaluation of all subsequent chunks.  If this option is true, the
      figure chunk is run only for each selected type of graphics, for
      which a new graphics device is opened and then closed.}
  }

  In addition, users can specify further options, either in the header
  of an individual code section or in a \samp{\\SweaveOpts\{\}} line in
  the document. For unknown options, their type is set at first use.
}

\section{Custom Graphics Devices}{
  If option \code{grdevice} is supplied for a code chunk with both
  \code{fig} and \code{eval} true, the following call is made
  \preformatted{  get(options$grdevice, envir = .GlobalEnv)(name=, width=,
                                              height=, options)
}
  which should open a graphics device.  The chunk's code is then
  evaluated and \code{\link{dev.off}} is called.  Normally a function of
  the name given will have been defined earlier in the Sweave document, e.g.
\preformatted{<<results=hide>>=
my.Swd <- function(name, width, height, ...)
  grDevices::png(filename = paste(name, "png", sep = "."),
                 width = width, height = height, res = 100,
                 units = "in", type = "quartz", bg = "transparent")
@
}
  Currently only one custom device can be used for each chunk, but
  different devices can be used for different chunks.

  A replacement for \code{\link{dev.off}} can be provided as a function
  with suffix \code{.off}, e.g.\sspace{}\code{my.Swd.off()}.
}

\section{Hook Functions}{
  Before each code chunk is evaluated, zero or more hook functions can
  be executed.  If \code{getOption("SweaveHooks")} is set, it is taken
  to be a named list of hook functions.  For each \emph{logical} option of a
  code chunk (\code{echo}, \code{print}, \ldots) a hook can be
  specified, which is executed if and only if the respective option is
  \code{TRUE}.  Hooks must be named elements of the list returned by
  \code{getOption("SweaveHooks")} and be functions taking no arguments.
  E.g., if option \code{"SweaveHooks"} is defined as \code{list(fig =
  foo)}, and \code{foo} is a function, then it would be executed before
  the code in each figure chunk.  This is especially useful to set
  defaults for the graphical parameters in a series of figure chunks.

  Note that the user is free to define new Sweave logical options and
  associate arbitrary hooks with them.  E.g., one could define a hook
  function for a new option called \code{clean} that removes all objects
  in the workspace.  Then all code chunks specified with \code{clean =
    TRUE} would start operating on an empty workspace.
}

\author{Friedrich Leisch and R-core}

\seealso{
  \sQuote{\href{../doc/Sweave.pdf}{Sweave User Manual}}, a vignette in
  the \pkg{utils} package.

  \code{\link{Sweave}}, \code{\link{Rtangle}}
}

\keyword{utilities}
