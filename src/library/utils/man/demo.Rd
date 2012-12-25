% File src/library/utils/man/demo.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2012 R Core Team
% Distributed under GPL 2 or later

\name{demo}
\alias{demo}
\title{Demonstrations of R Functionality}
\description{
  \code{demo} is a user-friendly interface to running some demonstration
  \R scripts.  \code{demo()} gives the list of available topics.
}
\usage{
demo(topic, package = NULL, lib.loc = NULL,
     character.only = FALSE, verbose = getOption("verbose"),
     echo = TRUE, ask = getOption("demo.ask"),
     encoding = getOption("encoding"))
}
\arguments{
  \item{topic}{the topic which should be demonstrated, given as a
    \link{name} or literal character string, or a character string,
    depending on whether \code{character.only} is \code{FALSE} (default)
    or \code{TRUE}.  If omitted, the list of available topics is
    displayed.}
  \item{package}{a character vector giving the packages to look into for
    demos, or \code{NULL}.  By default, all packages in the search path
    are used.}
  \item{lib.loc}{a character vector of directory names of \R libraries,
    or \code{NULL}.  The default value of \code{NULL} corresponds to all
    libraries currently known.  If the default is used, the loaded
    packages are searched before the libraries.}
  \item{character.only}{logical; if \code{TRUE}, use \code{topic} as
    character string.}
  \item{verbose}{a logical.  If \code{TRUE}, additional diagnostics are
    printed.}
  \item{echo}{a logical.  If \code{TRUE}, show the \R input when sourcing.}
  \item{ask}{a logical (or \code{"default"}) indicating if
    \code{\link{devAskNewPage}(ask = TRUE)} should be called before
    graphical output happens from the demo code.  The value
    \code{"default"} (the factory-fresh default) means to ask if
    \code{echo == TRUE} and the graphics device appears to be
    interactive.  This parameter applies both to any currently opened
    device and to any devices opened by the demo code.  If this is
    evaluated to \code{TRUE} and the session is \link{interactive}, the
    user is asked to press RETURN to start.}
  \item{encoding}{See \code{\link{source}}.  If the package has a
    declared encoding, that takes preference.}
}
\details{
  If no topics are given, \code{demo} lists the available demos.  The
  corresponding information is returned in an object of class
  \code{"packageIQR"}.
}
\seealso{
  \code{\link{source}} and \code{\link{devAskNewPage}} which
are called by \code{demo}.
}

\examples{
demo() # for attached packages

## All available demos:
demo(package = .packages(all.available = TRUE))

\donttest{
## Display a demo, pausing between pages
demo(lm.glm, package = "stats", ask = TRUE)

## Display it without pausing
demo(lm.glm, package = "stats", ask = FALSE)
}

\dontrun{
 ch <- "scoping"
 demo(ch, character = TRUE)
}

## Find the location of a demo
system.file("demo", "lm.glm.R", package = "stats")
}
\keyword{documentation}
\keyword{utilities}
