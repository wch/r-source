\name{help}
\title{Documentation}
\usage{
help(topic, offline = FALSE, package = c(.packages(), .Autoloaded),
     lib.loc = .lib.loc, verbose = .Options$verbose,
     htmlhelp = .Options$htmlhelp, winhelp = .Options$winhelp)
?topic
}
\alias{help}
\alias{?}
\arguments{
    \item{topic}{a name or character string on which documentation is
	sought (but \emph{not} a variable containing a character string!).}
    \item{offline}{a logical indicating whether documentation should be
	displayed on-line to the screen (the default) or hardcopy of it
	should be produced.  \emph{Ignored in this port of \R}.}
    \item{package}{a name or character vector giving the packages to look
	into for documentation.  By default, all packages in the search path
	are used.}
    \item{lib.loc}{A character vector of directory names of \R libraries.
	Defaults to all libraries currently known.}
    \item{verbose}{logical; if \code{TRUE}, the file name is reported.}
    \item{htmlhelp}{logical (or \code{NULL}).  If \code{TRUE}, the help
	will not be shown on the console, but via the html browser.}
    \item{winlhelp}{logical (or \code{NULL}).  If \code{TRUE}, a Windows
	\code{.hlp} file will be used if one is available.}
}
\description{
    These functions provide access to documentation.
    Documentation on a topic with name \code{name} (typically, an \R
    object or a data set) can be printed with either \code{help(name)} or
    \code{?name}.
    In the case of unary and binary operators and control-flow special
    forms, the name may need to be quoted.

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
\dontrun{help(topi) ##--> Error: No documentation for `topi'}
}
\keyword{documentation}
