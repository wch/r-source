% File src/library/utils/man/browseEnv.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2015 R Core Team
% Distributed under GPL 2 or later

\name{browseEnv}
\alias{browseEnv}
\alias{wsbrowser}
\title{Browse Objects in Environment}
\description{
  The \code{browseEnv} function opens a browser with list of objects
  currently in \code{sys.frame()} environment.
}
\usage{
browseEnv(envir = .GlobalEnv, pattern,
          excludepatt = "^last\\\\.warning",
          html = .Platform$GUI != "AQUA",
          expanded = TRUE, properties = NULL,
          main = NULL, debugMe = FALSE)
}
\arguments{
  \item{envir}{an \code{\link{environment}} the objects of which are to
    be browsed.}
  \item{pattern}{a \link{regular expression} for object subselection
    is passed to the internal \code{\link{ls}()} call.}
  \item{excludepatt}{a regular expression for \emph{dropping} objects
    with matching names.}
  \item{html}{is used to display the workspace
    on a HTML page in your favorite browser.  The default except when
    running from \command{R.app} on OS X.}
  \item{expanded}{whether to show one level of recursion.  It can be useful
    to switch it to \code{FALSE} if your workspace is large.  This
    option is ignored if \code{html} is set to \code{FALSE}.}
  \item{properties}{a named list of global properties (of the objects chosen)
    to be showed in the browser;  when \code{NULL} (as per default),
    user, date, and machine information is used.}
  \item{main}{a title string to be used in the browser; when \code{NULL}
    (as per default) a title is constructed.}
  \item{debugMe}{logical switch; if true, some diagnostic output is produced.}
}

\details{
  Very experimental code: displays a static HTML page on all platforms
  except \command{R.app} on OS X.

  Only allows one level of recursion into object structures.

  It can be generalized.  See sources for details.
  Most probably, this should rather work through using the \pkg{tkWidget}
  package (from \url{https://www.bioconductor.org}).
}
\seealso{
  \code{\link{str}}, \code{\link{ls}}.
}
\examples{
if(interactive()) {
   ## create some interesting objects :
   ofa <- ordered(4:1)
   ex1 <- expression(1+ 0:9)
   ex3 <- expression(u, v, 1+ 0:9)
   example(factor, echo = FALSE)
   example(table, echo = FALSE)
   example(ftable, echo = FALSE)
   example(lm, echo = FALSE, ask = FALSE)
   example(str, echo = FALSE)

   ## and browse them:
   browseEnv()

   ## a (simple) function's environment:
   af12 <- approxfun(1:2, 1:2, method = "const")
   browseEnv(envir = environment(af12))
 }
}
\keyword{interface}
