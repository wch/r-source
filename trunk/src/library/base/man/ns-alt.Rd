\name{ns-alt}
\title{Experimental Alternative Name Specification Support}
\alias{.Export}
\alias{.Import}
\alias{.ImportFrom}
\alias{.S3method}
\description{
  Alternative interface for specifying a name space within the code of a
  package.
}
\usage{
.Export(\dots)
.Import(\dots)
.ImportFrom(name, \dots)
.S3method(generic, class, method)
}
\arguments{
  \item{\dots}{name or literal character string arguments.}
  \item{name}{name or literal character string.}
  \item{generic}{name or literal character string.}
  \item{class}{name or literal character string.}
  \item{method}{optional character or function argument.}
}
\details{
  As an experimental alternative to using a \file{NAMESPACE} file it is
  possible to add a name space to a package by adding a
  \code{Namespace: <package_name>} entry to the \file{DESCRIPTION}
  file and placing directives to specify imports and exports directly in
  package code.  These directives should be viewed as declarations, not
  as function calls.   Except to the optional method argument to
  \code{.S3method} arguments are not evaluated.  These directives should
  only be used at top level of package code except as noted below.

  \code{.Export} is used to declare exports.  Its arguments should be
  literal names or character strings.  \code{.Export} should only be
  used at package top level.

  \code{.Import} is used to declare the import of entire name spaces.
  Its arguments should be literal names or character strings.
  \code{.ImportFrom} is used to declare the import of selected
  variables from a single name space.  The first argument is a literal
  name or character string identifying the source name space; the
  remaining arguments are literal names or character strings identifying
  the variables to import.  As an experimental feature both
  \code{.Import} and \code{.ImportFrom} can be used to import
  variables into a local environment.  The drawback of allowing this
  is that dependencies cannot be determined easily at package load
  time, and as a result this feature may need to be dropped.

  \code{.S3method} is used to declare a method for S3-style
  \code{UseMethod} dispatch.  This is needed since methods in packages
  that are imported but not on the search path might not be visible to
  the standard dispatch mechanism at a call site.  The first argument
  is the name of the generic, the second specifies the class.  The third
  argument is optional and defaults to the usual concatenation of
  generic and class separated by a period.  If supplied, the third
  argument should evaluate to a character string or a function.  If the
  third argument is omitted or a character string is supplied, then a
  function by that name must be defined.  If a function is supplied,
  it is used as the method.  When the method is specified as a name,
  explicitly or implicitly, the function lookup is handled lazily;
  this allows the definition to occur after the \code{.S3method}
  declaration and also integrates with possible data base storage of
  package code.
}
\examples{
\dontrun{
## code for package/name space 'foo'
x <- 1
f <- function(y) c(x,y)
print.foo <- function(x, ...) cat("<a foo>\n")
.Export(f)
S3method(print,foo)

## code for package/name space 'bar'
.Import(foo)
c <- function(...) sum(...)
g <- function(y) f(c(y, 7))
h <- function(y) y+9
.Export(g, h)
}
}
\keyword{internal}
\author{Luke Tierney}
