\name{help}
\title{Online Documentation}
\usage{
help(topic, package = c(.packages(),.Autoloaded), lib.loc = .lib.loc)

?topic
}
\alias{help}
\alias{?}
\arguments{
 \item{topic}{a name or character string on which documentation is
  sought (but \emph{not} a variable containing a character string!).}
 \item{package}{a name or character vector giving the packages to look
  into for documentation.  By default, all packages in the search path
  are used.}
 \item{lib.loc}{A character vector of directory names of \R libraries.
  Defaults to all libraries currently known.}
}
\description{
These functions provide online access to documentation.
Documentation on a topic with name \code{name} (typically, an \R object or
a data set) can be printed on-screen with either \code{help(name)} or
\code{?name}.
In the case of unary and binary operators and control-flow special
forms, the name may need to be be quoted.
}
\seealso{
\code{\link{help.start}()} which opens the HTML version of the \R Manual;
\code{\link{library}} for listing available packages and the user-level
objects they contain;
\code{\link{data}} for listing available data sets;
\code{\link{methods}}.

See \code{\link{prompt}} to get a prototype for writing \code{help} pages
of private packages.
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
