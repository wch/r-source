% File src/library/utils/man/example.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2024 R Core Team
% Distributed under GPL 2 or later

\name{example}
\alias{example}
\title{Run an Examples Section from the Online Help}
\description{
  Run all the \R code from the \bold{Examples} part of \R's online help
  topic \code{topic}, with possible exceptions due to \verb{\dontrun},
  \verb{\dontshow}, and \verb{\donttest} tags, see \sQuote{Details} below.
}
\usage{
example(topic, package = NULL, lib.loc = NULL,
        character.only = FALSE, give.lines = FALSE, local = FALSE,
        type = c("console", "html"), echo = TRUE,
        verbose = getOption("verbose"),
        setRNG = FALSE, ask = getOption("example.ask"),
        prompt.prefix = abbreviate(topic, 6),
        catch.aborts = FALSE,
        run.dontrun = FALSE, run.donttest = interactive())
}
\arguments{
  \item{topic}{name or literal character string: the online
    \code{\link{help}} topic the examples of which should be run.}
  \item{package}{a character vector giving the package names to look
    into for the topic, or \code{NULL} (the default), when all packages on
    the \link{search} path are used.}
  \item{lib.loc}{a character vector of directory names of \R libraries,
    or \code{NULL}.  The default value of \code{NULL} corresponds to all
    libraries currently known.  If the default is used, the loaded
    packages are searched before the libraries.}
  \item{character.only}{a logical indicating whether \code{topic} can be
    assumed to be a character string.}
  \item{give.lines}{logical: if true, the \emph{lines} of the example
    source code are returned as a character vector.}
  \item{local}{logical: if \code{TRUE} evaluate locally, if \code{FALSE}
    evaluate in the workspace.}
  \item{type}{character: whether to show output in the console or a
    browser (using the dynamic help system). The latter is honored only
    in interactive sessions and if the \code{\link[knitr]{knitr}}
    package is installed. Several other arguments are silently ignored
    in that case, including \code{setRNG} and \code{lib.loc}.}
  \item{echo}{logical;  if \code{TRUE}, show the \R input when sourcing.}
  \item{verbose}{logical;  if \code{TRUE}, show even more when running
    example code.}
  \item{setRNG}{logical or expression;  if not \code{FALSE}, the random
    number generator state is saved, then initialized to a specified state,
    the example is run and the (saved) state is restored.
    \code{setRNG = TRUE} sets the same state as
    \command{R CMD \link{check}} does for
    running a package's examples.  This is currently equivalent to
    \code{setRNG = \{RNGkind("default", "default", "default"); set.seed(1)\}}.}
  \item{ask}{logical (or \code{"default"}) indicating if
    \code{\link{devAskNewPage}(ask = TRUE)} should be called
    before graphical output happens from the example code.  The value
    \code{"default"} (the factory-fresh default) means to ask if
    \code{echo} is true and the graphics device appears to be
    interactive.  This parameter applies both to any currently opened
    device and to any devices opened by the example code.}
  \item{prompt.prefix}{character; prefixes the prompt to be used if
    \code{echo} is true (as it is by default).}
  \item{catch.aborts}{logical, passed on to \code{\link{source}()},
    indicating that \I{\dQuote{abort}ing} errors should be caught.}
  \item{run.dontrun}{logical indicating that \verb{\dontrun}
    should be ignored, i.e., do run the enclosed code.}
  \item{run.donttest}{logical indicating that \verb{\donttest}
    should be ignored, i.e., do run the enclosed code.}
}
\value{
  The value of the last evaluated expression, unless \code{give.lines}
  is true, where a \code{\link{character}} vector is returned.
}
\details{
  If \code{lib.loc} is not specified, the packages are searched for
  amongst those already loaded, then in the libraries given by
  \code{\link{.libPaths}()}.  If \code{lib.loc} is specified, packages
  are searched for only in the specified libraries, even if they are
  already loaded from another library.  The search stops at the first
  package found that has help on the topic.

  An attempt is made to load the package before running the examples,
  but this will not replace a package loaded from another location.

  If \code{local = TRUE} objects are not created in the workspace and so
  not available for examination after \code{example} completes: on the
  other hand they cannot overwrite objects of the same name in the
  workspace.

  As detailed in the \sQuote{Writing R Extensions} manual, the author of
  the help page can tag parts of the examples with the following
  exception rules:
  \describe{
    \item{\verb{\dontrun}}{encloses code that should not be run.}
    \item{\verb{\dontshow}}{encloses code that is invisible on help
      pages, but will be run both by the package checking tools,
      and the \code{example()} function.  This was previously
      \verb{\testonly}, and that form is still accepted.}
    \item{\verb{\donttest}}{encloses code that typically should be run,
      but not during package checking.  The default
      \code{run.donttest = \link{interactive}()}
      leads \code{example()} use in other help
      page examples to skip \verb{\donttest} sections appropriately.}
  }
  The additional \verb{\dontdiff} tag (in \R \eqn{\ge} 4.4.0) produces
  special comments in the code run by \code{example} (for
  \code{\link{Rdiff}}-based testing of example output), but does not
  affect which code is run or displayed on the help page.
}
\author{Martin Maechler and others}
\seealso{\code{\link{demo}}}
\examples{
example(InsectSprays)
## force use of the standard package 'stats':
example("smooth", package = "stats", lib.loc = .Library)

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

## Exploring examples code:
## How large are the examples of "lm...()" functions?
lmex <- sapply(apropos("^lm", mode = "function"),
               example, character.only = TRUE, give.lines = TRUE)
lengths(lmex)
}
\keyword{documentation}
\keyword{utilities}
