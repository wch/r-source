\name{help}
\alias{help}
\alias{?}
\alias{topicName}		% internal
\title{Documentation}
\description{
  These functions provide access to documentation.
  Documentation on a topic with name \code{name} (typically, an \R
  object or a data set) can be printed with either \code{help(name)} or
  \code{?name}.
}
\usage{
help(topic, offline = FALSE, package = .packages(),
     lib.loc = NULL, verbose = getOption("verbose"),
     try.all.packages = getOption("help.try.all.packages"),
#ifdef unix
     htmlhelp = getOption("htmlhelp"),
#endif
#ifdef mac
     htmlhelp = getOption("htmlhelp"),
#endif
#ifdef windows
     chmhelp = getOption("chmhelp"),
     htmlhelp = getOption("htmlhelp"), winhelp = getOption("winhelp"),
#endif
     pager = getOption("pager"))
?topic
type?topic
}
\arguments{
 \item{topic}{a name or string constant on which documentation is sought
   (but \emph{not} a variable containing a character string!).}
 \item{offline}{a logical indicating whether documentation should be
   displayed on-line to the screen (the default) or hardcopy of it
   should be produced.}
 \item{package}{a name or character vector giving the packages to look
   into for documentation.  By default, all packages in the search path
   are used.}
 \item{lib.loc}{a character vector of directory names of \R libraries,
   or \code{NULL}.  The default value of \code{NULL} corresponds to all
   libraries currently known.  If the default is used, the loaded
   packages are searched before the libraries.}
 \item{verbose}{logical; if \code{TRUE}, the file name is reported.}
 \item{try.all.packages}{logical; see \code{Notes}.}
#ifdef unix
 \item{htmlhelp}{logical (or \code{NULL}).  If \code{TRUE} (which is the
   default after \code{\link{help.start}} has been called), the HTML
   version of the help will be shown in the browser specified by
   \code{options("browser")}.  See \code{\link{browseURL}} for details
   of the browsers that are supported.  Where possible an existing
   browser window is re-used.}
#endif
#ifdef mac
  \item{htmlhelp}{logical (or \code{NULL}).  If \code{TRUE}, the HTML
    version of the help will be shown in a browser.}
#endif
#ifdef windows
  \item{chmhelp}{logical (or \code{NULL}).  If \code{TRUE} the
    Compiled HTML version of the help will be shown in a help viewer.}
  \item{htmlhelp}{logical (or \code{NULL}).  If \code{TRUE}, the HTML
    version of the help will be shown in a browser specified by
    \code{options("browser")} or the file-association mechanism.
  }
  \item{winhelp}{logical (or \code{NULL}).  If \code{TRUE}, a Windows
    \code{.hlp} file will be used if one is available.}
#endif
  \item{pager}{the pager to be used for \code{\link{file.show}}.}
  \item{type}{the special type of documentation to use for this topic;
      for example, if the type is \code{class}, documentation is
      provided for the class with name \code{topic}.  The function
      \code{topicName} returns the actual name used in this case.}
}
\details{
  In the case of unary and binary operators and control-flow special
  forms, the name may need to be quoted.

#ifdef unix
  If \code{offline} is \code{TRUE}, hardcopy of the documentation is
  produced by running the LaTeX version of the help page through
  \code{latex} (note that LaTeX 2e is needed) and \code{dvips}.
  Depending on your \code{dvips} configuration, hardcopy will be sent to
  the printer or saved in a file.  If the programs are in non-standard
  locations and hence were not found at compile time, you can either set
  the options \code{latexcmd} and \code{dvipscmd}, or the environment
  variables \code{R_LATEXCMD} and \code{R_DVIPSCMD} appropriately.
  The appearance of the output can be customized through a file
  \file{Rhelp.cfg} somewhere in your LaTeX search path.
#endif
#ifdef windows
  If \code{offline} is \code{TRUE}, hardcopy of the documentation is
  produced by running the LaTeX version of the help page through
  \code{latex} (note that LaTeX 2e is needed). You need to customize
  the file \file{R\_HOME/bin/helpPRINT.bat} which contains an example.
  The appearance of the output can be customized through a file
  \file{Rhelp.cfg} somewhere in your LaTeX search path.
#endif
#ifdef mac
  Offline printing is not currently supported by the Macintosh port.
#endif

  %% \code{topicName} computes the string used internally to find
  %% documenation of the given type and topic.  Called by the \code{?}
  %% operator and the special prompt functions in the methods package.
  
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
  help may be found is printed (but no help is shown).
  \bold{N.B.} searching all packages can be slow.

  The help files can be many small files.  On some file systems it is
  desirable to save space, and the text files in the \file{help} directory
  of an installed package can be zipped up as a zip archive
  \file{Rhelp.zip}. Ensure that file \file{AnIndex} remains un-zipped.
  Similarly, all the files in the
  \file{latex} directory can be zipped to \file{Rhelp.zip}.
}
\seealso{
  \code{\link{help.search}()} for finding help pages on a ``vague'' topic.
  \code{\link{help.start}()} which opens the HTML version of the \R
  help pages;
  \code{\link{library}()} for listing available packages and the
  user-level objects they contain;
  \code{\link{data}()} for listing available data sets;
  \code{\link{methods}()}.

  See \code{\link{prompt}()} to get a prototype for writing \code{help}
  pages of private packages.
}
\examples{
help()
help(help)              # the same

help(lapply)
?lapply                 # the same

help("for")             # or ?"for", but the quotes are needed
?"+"

help(package = stepfun) # get help even when package is not loaded

data()                  # list all available data sets
?women                  # information about data set "women"

topi <- "women"
\dontrun{help(topi) ##--> Error: No documentation for `topi'}

try(help("bs", try.all.packages=FALSE)) # reports not found (an error)
help("bs", try.all.packages=TRUE) # reports can be found in package `splines'
}
\keyword{documentation}
