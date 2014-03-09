% File src/library/utils/man/help.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{help}
\alias{help}
\title{Documentation}
\description{
  \code{help} is the primary interface to the help systems.
}
\usage{
help(topic, package = NULL, lib.loc = NULL,
     verbose = getOption("verbose"),
     try.all.packages = getOption("help.try.all.packages"),
     help_type = getOption("help_type"))
}
\arguments{
  \item{topic}{usually, a \link{name} or character string specifying the
   topic for which help is sought.  A character string (enclosed in
   explicit single or double quotes) is always taken as naming a topic.

   If the value of \code{topic} is a length-one
   character vector the topic is taken to be the value of the only
   element.  Otherwise \code{topic} must be a name or a \link{reserved}
   word (if syntactically valid) or character string.

   See \sQuote{Details} for what happens if this is omitted.
 }
 \item{package}{a name or character vector giving the packages to look
   into for documentation, or \code{NULL}.  By default, all packages in
   the search path are used.  To avoid a name being deparsed use e.g.
   \code{(pkg_ref)} (see the examples).}
 \item{lib.loc}{a character vector of directory names of \R libraries,
   or \code{NULL}.  The default value of \code{NULL} corresponds to all
   libraries currently known.  If the default is used, the loaded
   packages are searched before the libraries.  This is not used for
   HTML help (see \sQuote{Details}.}
 \item{verbose}{logical; if \code{TRUE}, the file name is reported.}
 \item{try.all.packages}{logical; see \code{Note}.}
 \item{help_type}{character string: the type of help required.
   Possible values are \code{"text"}, \code{"html"} and \code{"pdf"}.
   Case is ignored, and partial matching is allowed.}
}
\details{
  The following types of help are available:
  \itemize{
    \item Plain text help
    \item HTML help pages with hyperlinks to other topics, shown in a
    browser by \code{\link{browseURL}}.
#ifdef unix
    (Where possible an existing browser window is re-used: the OS X
    GUI uses its own browser window.)
#endif
    If for some reason HTML help is unavailable (see
    \code{\link{startDynamicHelp}}), plain text help will be used
    instead.
    \item For \code{help} only, typeset as PDF --
    see the section on \sQuote{Offline help}.
  }
#ifdef unix
  The \sQuote{factory-fresh} default is text help except from the Mac OS
  GUI, which uses HTML help displayed in its own browser window.
#endif
#ifdef windows
  The default for the type of help is selected when \R is installed --
  the \sQuote{factory-fresh} default is HTML help.
#endif

  The rendering of text help will use directional quotes in suitable
  locales (UTF-8 and single-byte Windows locales): sometimes the fonts
  used do not support these quotes so this can be turned off by setting
  \code{\link{options}(useFancyQuotes = FALSE)}.

  \code{topic} is not optional: if it is omitted \R will give
  \itemize{
    \item If a package is specified, (text or, in interactive use only,
    HTML) information on the package, including hints/links to suitable
    help topics.
    \item If \code{lib.loc} only is specified, a (text) list of available
    packages.
    \item Help on \code{help} itself if none of the first three
  arguments is specified.
  }

  Some topics need to be quoted (by \link{backtick}s) or given as a
  character string.  These include those which cannot syntactically
  appear on their own such as unary and binary operators,
  \code{function} and control-flow \link{reserved} words (including
  \code{if}, \code{else} \code{for}, \code{in}, \code{repeat},
  \code{while}, \code{break} and \code{next}).  The other \code{reserved}
  words can be used as if they were names, for example \code{TRUE},
  \code{NA} and \code{Inf}.

  If multiple help files matching \code{topic} are found, in interactive
  use a menu is presented for the user to choose one: in batch use the
  first on the search path is used.  (For HTML help the menu will be an
  HTML page, otherwise a graphical menu if possible if
  \code{\link{getOption}("menu.graphics")} is true, the default.)

  Note that HTML help does not make use of \code{lib.loc}: it will
  always look first in the attached packages and then along
  \code{.libPaths()}.
}

\section{Offline help}{
  Typeset documentation is produced by running the LaTeX version of the
  help page through \command{pdflatex}: this will produce a PDF file.

  The appearance of the output can be customized through a file
  \file{Rhelp.cfg} somewhere in your LaTeX search path: this will be
  input as a LaTeX style file after \code{Rd.sty}.  Some
  \link{environment variables} are consulted, notably \env{R_PAPERSIZE}
  (\emph{via} \code{getOption("papersize")}) and \env{R_RD4PDF} (see
  \sQuote{Making manuals} in the \sQuote{R Installation and
  Administration Manual}).

  If there is a function \code{offline_help_helper} in the workspace or
  further down the search path it is used to do the typesetting,
  otherwise the function of that name in the \code{utils} namespace (to
  which the first paragraph applies).  It should accept at least two
  arguments, the name of the LaTeX file to be typeset and the type
  (which is nowadays ignored).  It accepts a third argument,
  \code{texinputs}, which will give the graphics path when the help
  document contains figures, and will otherwise not be supplied.
}

\note{
  Unless \code{lib.loc} is specified explicitly, the loaded packages are
  searched before those in the specified libraries.  This ensures that
  if a library is loaded from a library not in the known library trees,
  then the help from the loaded library is used.  If \code{lib.loc} is
  specified explicitly, the loaded packages are \emph{not} searched.

  If this search fails and argument \code{try.all.packages} is
  \code{TRUE} and neither \code{packages} nor \code{lib.loc} is
  specified, then all the packages in the known library trees are
  searched for help on \code{topic} and a list of (any) packages where
  help may be found is displayed (with hyperlinks for \code{help_type =
  "html"}).  \strong{NB:} searching all packages can be slow, especially
  the first time (caching of files by the OS can expedite subsequent
  searches dramatically).
}

\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\seealso{
  \code{\link[=Question]{?}} for shortcuts to help topics.

  \code{\link{help.search}()} or \code{\link{??}} for finding help pages
  on a vague topic;
  \code{\link{help.start}()} which opens the HTML version of the \R
  help pages;
  \code{\link{library}()} for listing available packages and the
  help objects they contain;
  \code{\link{data}()} for listing available data sets;
  \code{\link{methods}()}.

  Use \code{\link{prompt}()} to get a prototype for writing \code{help}
  pages of your own package.
}
\examples{
help()
help(help)              # the same

help(lapply)

help("for")             # or ?"for", but quotes/backticks are needed

\donttest{try({# requires working TeX installation:
 help(dgamma, help_type = "pdf")
 ## -> nicely formatted pdf -- including math formula -- for help(dgamma):
 system2(getOption("pdfviewer"), "dgamma.pdf", wait = FALSE)
})
}
\donttest{help(package = "splines") # get help even when package is not loaded}

topi <- "women"
help(topi)

try(help("bs", try.all.packages = FALSE)) # reports not found (an error)
help("bs", try.all.packages = TRUE)       # reports can be found
                                          # in package 'splines'

\donttest{## For programmatic use:
topic <- "family"; pkg_ref <- "stats"
help((topic), (pkg_ref))
}}
\keyword{documentation}
