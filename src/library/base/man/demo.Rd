\name{demo}
\alias{demo}
\title{Demonstrations of R functionality}
\description{
  \code{demo} is a user-friendly interface to running some demonstration
  \R scripts.  \code{demo()} gives the list of available topics.
}
\usage{
demo(topic, device = getOption("device"),
     package = .packages(), lib.loc = .lib.loc,
     character.only = FALSE)
}
\arguments{
  \item{topic}{the topic which should be demonstrated.  If omitted, the
    list of available topics is displayed.}
  \item{device}{the graphics device to be used.}
  \item{package}{a name or character vector giving the packages to
    look into for data sets.  By default, all packages in the search
    path are used.}
 \item{lib.loc}{a character vector of directory names of \R libraries.
   Defaults to all libraries currently known.  If the default is used,
   the loaded packages are searched before the libraries.}
  \item{character.only}{logical; if \code{TRUE}, use \code{topic} as
    character string instead of name.}
}
\seealso{\code{\link{source}} which is called by \code{demo}.}
\examples{
% Most demos are strictly tested anyway, since R 1.3.0,
% make sure, the others are run here: 
demo() # for attached packages

## All available demos:
demo(package = .packages(all.available = TRUE))

demo(lm.glm)
\testonly{
demo(nlm)
}
\dontrun{
 ch <- "scoping"
 demo(ch, character = TRUE)
}
}
\keyword{documentation}
\keyword{utilities}
