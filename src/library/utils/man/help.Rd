\name{help}
\alias{help}
\alias{?}
\alias{topicName}               % internal
\alias{.helpForCall}            % internal
\alias{.tryHelp}                % internal
\title{Documentation}
\description{
  These functions provide access to documentation.
  Documentation on a topic with name \code{name} (typically, an \R
  object or a data set) can be printed with either \code{help(name)} or
  \code{?name}.
}
\usage{
help(topic, offline = FALSE, package = NULL,
     lib.loc = NULL, verbose = getOption("verbose"),
     try.all.packages = getOption("help.try.all.packages"),
     chmhelp = getOption("chmhelp"), htmlhelp = getOption("htmlhelp"),
     pager = getOption("pager"))
?topic
type?topic
}
\arguments{
 \item{topic}{usually, the name on  which documentation is sought.
   The name may be quoted or unquoted (but note that if \code{topic}
   is the name of a
   variable containing a character string  documentation is provided for
   the name, not for the character string).

   The \code{topic} argument may also be a function call, to ask for
   documentation on a corresponding method.  See the section on method
   documentation.
 }
 \item{offline}{a logical indicating whether documentation should be
   displayed on-line to the screen (the default) or hardcopy of it
   should be produced.}
 \item{package}{a name or character vector giving the packages to look
   into for documentation, , or \code{NULL}.  By default, all packages
   in the search path are used.}
 \item{lib.loc}{a character vector of directory names of \R libraries,
   or \code{NULL}.  The default value of \code{NULL} corresponds to all
   libraries currently known.  If the default is used, the loaded
   packages are searched before the libraries.}
 \item{verbose}{logical; if \code{TRUE}, the file name is reported.}
 \item{try.all.packages}{logical; see \code{Notes}.}
 \item{chmhelp}{logical (or \code{NULL}). Only relevant under Windows.
   If \code{TRUE} the Compiled HTML version of the help will be shown
   in a help viewer.}
#ifdef unix
 \item{htmlhelp}{logical (or \code{NULL}).  If \code{TRUE} (which is the
   default after \code{\link{help.start}} has been called), the HTML
   version of the help will be shown in the browser specified by
   \code{options("browser")}.  See \code{\link{browseURL}} for details
   of the browsers that are supported.  Where possible an existing
   browser window is re-used.}
#endif
#ifdef windows
  \item{htmlhelp}{logical (or \code{NULL}).  If \code{TRUE}, the HTML
    version of the help will be shown in a browser specified by
    \code{options("browser")} or the file-association mechanism.
  }
#endif
  \item{pager}{the pager to be used for \code{\link{file.show}}.}
  \item{type}{the special type of documentation to use for this topic;
      for example, if the type is \code{class}, documentation is
      provided for the class with name \code{topic}.  The function
      \code{topicName} returns the actual name used in this case.
    See the section on method documentation for the uses of
      \code{type} to get help on formal methods.}
}
\details{
  In the case of unary and binary operators and control-flow special
  forms (including \code{if}, \code{for} and \code{function}), the topic
  may need to be quoted.

#ifdef unix
  If \code{offline} is \code{TRUE}, hardcopy of the documentation is
  produced by running the LaTeX version of the help page through
  \code{latex} (note that LaTeX 2e is needed) and \code{dvips}.
  Depending on your \code{dvips} configuration, hardcopy will be sent to
  the printer or saved in a file.  If the programs are in non-standard
  locations and hence were not found at compile time, you can either set
  the options \code{latexcmd} and \code{dvipscmd}, or the environment
  variables \env{R\_LATEXCMD} and \env{R\_DVIPSCMD} appropriately.
  The appearance of the output can be customized through a file
  \file{Rhelp.cfg} somewhere in your LaTeX search path.
#endif
#ifdef windows
  If \code{offline} is \code{TRUE}, hardcopy of the documentation is
  produced by running the LaTeX version of the help page through
  \code{latex} (note that LaTeX 2e is needed).  You need to customize
  the file \file{R\_HOME/bin/helpPRINT.bat} which contains an example.
  The appearance of the output can be customized through a file
  \file{Rhelp.cfg} somewhere in your LaTeX search path.
#endif

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
  Similarly, all the files in the \file{latex} directory can be zipped
  to \file{Rhelp.zip}.
#ifdef windows
  This is done on Windows for the larger packages.
#endif
}

\section{Method Documentation.}{
  The authors of formal (\sQuote{S4}) methods can provide documentation
  on specific methods, as well as overall documentation on the methods
  of a particular function.  The \code{"?"} operator allows access to
  this documentation in three ways.

  The expression \code{methods ? f} will look for the overall
  documentation methods for the function \code{f}.  Currently, this
  means the documentation file containing the alias \code{f-methods}.

  There are two different ways to look for documentation on a
  particular method.  The first is to supply the \code{topic} argument
  in the form of a function call, omitting the \code{type} argument.
  The effect is to look for documentation on the method that would be
  used if this function call were actually evaluated. See the examples
  below.  If the function is not a generic (no S4 methods are defined
  for it), the help reverts to documentation on the function name.

  The \code{"?"} operator can also be called with \code{type} supplied
  as \code{"method"}; in this case also, the \code{topic} argument is
  a function call, but the arguments are now interpreted as specifying
  the class of the argument, not the actual expression that will
  appear in a real call to the function.  See the examples below.

  The first approach will be tedious if the actual call involves
  complicated expressions, and may be slow if the arguments take a
  long time to evaluate.  The second approach avoids these
  difficulties, but you do have to know what the classes of the actual
  arguments will be when they are evaluated.

  Both approaches make use of any inherited methods; the signature of
  the method to be looked up is found by using \code{selectMethod}
  (see the documentation for  \code{\link{getMethod}}).


}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\seealso{
  \code{\link{help.search}()} for finding help pages on a \dQuote{vague}
  topic;
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

help(package="splines") # get help even when package is not loaded

data()                  # list all available data sets
?women                  # information about data set "women"

topi <- "women"
\dontrun{help(topi) ##--> Error: No documentation for 'topi'}

try(help("bs", try.all.packages=FALSE)) # reports not found (an error)
help("bs", try.all.packages=TRUE) # reports can be found in package 'splines'

\dontshow{require(methods)}

\dontrun{
## define a generic function and some methods
combo <- function(x, y) c(x, y)
setGeneric("combo")
setMethod("combo", c("numeric", "numeric"),
   function(x, y) x+y)

## assume we have written some documentation for combo, and its methods ....

?combo  ## produces the function documentation

methods?combo  ## looks for the overall methods documentation

method?combo("numeric", "numeric")  ## documentation for the method above

?combo(1:10, rnorm(10))  ## ... the same method, selected according to
                         ## the arguments (one integer, the other numeric)

?combo(1:10, letters)    ## documentation for the default method

}

}

\keyword{documentation}
