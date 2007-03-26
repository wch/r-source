\name{example}
\alias{example}
\title{Run an Examples Section from the Online Help}
\description{
  Run all the \R code from the \bold{Examples} part of \R's online help
  topic \code{topic} with two possible exceptions, \code{dontrun} and
  \code{dontshow}, see Details below.
}
\usage{
example(topic, package = NULL, lib.loc = NULL,
        local = FALSE, echo = TRUE, verbose = getOption("verbose"),
        setRNG = FALSE, ask = getOption("example.ask"),
        prompt.prefix = abbreviate(topic, 6))
}
\arguments{
  \item{topic}{name or literal character string: the online
    \code{\link{help}} topic the examples of which should be run.}
  \item{package}{a character vector giving the package names to look
    into for example code, or \code{NULL}.  By default, all packages in
    the search path are used.}
  \item{lib.loc}{a character vector of directory names of \R libraries,
    or \code{NULL}.  The default value of \code{NULL} corresponds to all
    libraries currently known.  If the default is used, the loaded
    packages are searched before the libraries.}
  \item{local}{logical: if \code{TRUE} evaluate locally, if \code{FALSE}
    evaluate in the workspace.}
  \item{echo}{logical;  if \code{TRUE}, show the \R input when sourcing.}
  \item{verbose}{logical;  if \code{TRUE}, show even more when running
    example code.}
  \item{setRNG}{logical or expression;  if not \code{FALSE}, the random
    number generator state is saved, then initialized to a specified state,
    the example is run and the (saved) state is restored.  \code{setRNG
      = TRUE} sets the same state as \code{R CMD \link{check}} does for
    running a package's examples.  This is currently equivalent to
    \code{setRNG = \{RNGkind("default", "default"); set.seed(1)\}}.}
  \item{ask}{logical (or \code{"default"}) indicating if
    \code{\link{par}(ask=TRUE)} should be called before graphical output
    happens from the example code.  The value \code{"default"} (the
    factory-fresh default) means to ask if \code{echo == TRUE} and the
    graphics device appears to be interactive.  This parameter applies
    both to any currently opened device and to any devices opened by the
    example code.}
  \item{prompt.prefix}{character; prefixes the prompt to be used if
    \code{echo = TRUE}.}
}
\value{
  The value of the last evaluated expression.
}
\details{
  If \code{lib.loc} is not specified, the packages are searched for
  amongst those already loaded, then in the specified libraries.
  If \code{lib.loc} is specified, they are searched for only
  in the specified libraries, even if they are already loaded from
  another library.

  An attempt is made to load the package before running the examples,
  but this will not replace a package loaded from another location.

  If \code{local=TRUE} objects are not created in the workspace and so
  not available for examination after \code{example} completes: on the
  other hand they cannot clobber objects of the same name in the
  workspace.

  As detailed in the manual \emph{Writing \R Extensions}, the author of
  the help page can markup parts of the examples for two exception rules
  \describe{
    \item{\code{dontrun}}{encloses code that should not be run.}
    \item{\code{dontshow}}{encloses code that is invisible on help
      pages, but will be run both by the package checking tools,
      and the \code{example()} function.  This was previously
      \code{testonly}, and that form is still accepted.}
  }

  If the examples file contains non-ASCII characters the encoding used
  will matter.  If in a UTF-8 locale \code{example} first tries UTF-8
  and then Latin-1.  (This can be overridden by setting the encoding in
  the \code{.Rd} file.)
}
\note{
  The examples can be many small files.  On some file systems it is
  desirable to save space, and the files in the \file{R-ex} directory
  of an installed package can be zipped up as a zip archive
  \file{Rex.zip}.
}
\author{Martin Maechler and others}
\seealso{\code{\link{demo}}}
\examples{
example(InsectSprays)
## force use of the standard package 'stats':
example("smooth", package="stats", lib.loc=.Library)

## set RNG *before* example as when R CMD check is run:

r1 <- example(quantile, setRNG = TRUE)
x1 <- rnorm(1)
u <- runif(1)
## identical random numbers
r2 <- example(quantile, setRNG = TRUE)
x2 <- rnorm(1)
stopifnot(identical(r1, r2))
## but x1 and x2 differ since the RNG state from before example()
## differs and is restored!
x1; x2
}
\keyword{documentation}
\keyword{utilities}
