\name{browseEnv}
\alias{browseEnv}
\alias{wsbrowser}
\title{Browse Objects in Environment}
\description{
  The \code{browseEnv} function opens a browser with list of objects  currently
  in \code{sys.frame()} environment.
}
\usage{
browseEnv(envir = .GlobalEnv, pattern,
          html = .Platform$OS.type != "mac",
          expanded = TRUE, properties = NULL,
          main = NULL, debugMe = FALSE)
}
\arguments{
  \item{envir}{an \code{\link{environment}} the objects of which are to
    be browsed.}
  \item{pattern}{a regular expression for object subselection is passed
    to the internal \code{\link{ls}()} call.}
  \item{html}{is used on non Macintosh machines to display the workspace on
    a HTML page in your favorite browser.}
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
  Very experimental code.  Only allows one level of recursion into
  object structures. The HTML version is not dynamic.

  It can be generalized.  See sources
  (\file{..../library/base/R/databrowser.R}) for details.

  \code{wsbrowser()} is currently just an internally used function;
  its argument list will certainly change.
}
\seealso{
  \code{\link{str}}, \code{\link{ls}}.
}
\examples{
if(interactive()) {
   ## create some interesting objects :
   ofa <- ordered(4:1)
   ex1 <- expression(1+ 0:9)
   ex3 <- expression(u,v, 1+ 0:9)
   example(factor)
   example(table)
   example(ftable)
   example(lm)

   browseEnv()
 }
}
\keyword{interface}
