\name{help}
\title{Documentation}
\usage{
help(topic, offline = FALSE, package = c(.packages(), .Autoloaded),
     lib.loc = .lib.loc)
?topic
}
\alias{help}
\alias{?}
\arguments{
 \item{topic}{a name or character string on which documentation is
   sought (but \emph{not} a variable containing a character string!).}
 \item{offline}{a logical indicating whether documentation should be
   displayed on-line to the screen (the default) or hardcopy of it
   should be produced.}
 \item{package}{a name or character vector giving the packages to look
  into for documentation.  By default, all packages in the search path
  are used.}
 \item{lib.loc}{A character vector of directory names of \R libraries.
  Defaults to all libraries currently known.}
}
\description{
  These functions provide access to documentation.
  Documentation on a topic with name \code{name} (typically, an \R
  object or a data set) can be printed with either \code{help(name)} or
  \code{?name}.
  In the case of unary and binary operators and control-flow special
  forms, the name may need to be be quoted.

  If \code{offline} is \code{TRUE}, hardcopy of the documentation is
  produced by running the LaTeX version of the help page through
  \code{latex} (note that LaTeX 2e is needed) and \code{dvips}.
  Depending on your \code{dvips} configuration, hardcopy will be sent to
  the printer or saved in a file.  If the programs are in non-standard
  locations and hence were not found at compile time, you need to set
  the environment variables \code{LATEX} and \code{DVIPS} appropriately.
  The appearance of the output can be customized through a file
  \file{Rhelp.cfg} somewhere in your LaTeX search path.
}
\seealso{
  \code{\link{help.start}()} which opens the HTML version of the \R
  Manual;
  \code{\link{library}()} for listing available packages and the
  user-level objects they contain;
  \code{\link{data}()} for listing available data sets;
  \code{\link{methods}()}.

  See \code{\link{prompt}()} to get a prototype for writing \code{help}
  pages of private packages.
}
\examples{
help()
help(help)		# the same

help(lapply)
?lapply			# the same

help("for")		# or ?"for", but the quotes are needed
?"+"

help(package = stepfun)	# get help even when it's not present

data()			# list all available data sets
?women			# information about data set "women"

topi <- "women"
help(topi) ##--> Error: No documentation for `topi'
}
\keyword{documentation}
