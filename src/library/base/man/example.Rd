\name{example}
\alias{example}
\title{Run an Examples Section from the Online Help}
\description{
  Run all the \R code from the \bold{Examples} part of \R's online help
  topic \code{topic} with two possible exceptions, \code{dontrun} and
  \code{testonly}, see Details below.
}
\usage{
example(topic, package = .packages(), lib.loc = NULL,
        local = FALSE, echo = TRUE, verbose = getOption("verbose"),
        prompt.echo = paste(abbreviate(topic, 6),"> ", sep=""))
}
\arguments{
  \item{topic}{name or string constant: the online \code{\link{help}}
    topic the examples of which should be run.}
  \item{package}{a character vector with package names.  By default,
    all packages in the search path are used.}
  \item{lib.loc}{a character vector of directory names of \R libraries,
    or \code{NULL}.  The default value of \code{NULL} corresponds to all
    libraries currently known.  If the default is used, the loaded
    packages are searched before the libraries.}
  \item{local}{logical: if \code{TRUE} evaluate locally, if \code{FALSE}
    evaluate in the workspace.}
  \item{echo}{logical;  if \code{TRUE}, show the \R input when sourcing.}
  \item{verbose}{logical;  if \code{TRUE}, show even more when running
    example code.}
  \item{prompt.echo}{character; gives the prompt to be used if
    \code{echo = TRUE}.}
}
\value{(the value of the last evaluated expression).}
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
    \item{\code{testonly}}{encloses code that is invisible on help
      pages, but will be run both by the package checking tools,
      and the \code{example()} function.}
    }
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
## force use of the standard package eda:
example("smooth", package="eda", lib.loc=.Library)
}
\keyword{documentation}
\keyword{utilities}
