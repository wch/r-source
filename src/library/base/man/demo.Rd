\name{demo}
\title{Demonstrations of R functions}
\usage{
demo(topic, device = getOption("device"),
     character.only = FALSE, interactively = interactive())
}
\alias{demo}
\arguments{
  \item{topic}{the topic which should be demonstrated. If omitted,
    the list of available topics is displayed.}
  \item{device}{the graphics device to be used.}
  \item{character.only}{logical; if true, use \code{topic} as
    \code{character} string instead of name.}
  \item{interactively}{logical; if true, print more and wait for user
    interaction; defaults to \code{\link{interactive}()}.}
}
\description{
  \code{demo} is a user-friendly interface to running some demonstration \R
  scripts.  \code{demo()} gives the list of available topics.
}
\seealso{\code{\link{source}} which is called by \code{demo}.}
\examples{
\dontrun{
% the demos are strictly tested anyway, since R 1.3.0
demo(lm.glm)
ch <- "scoping"
demo(ch, character = TRUE)
}
}
\keyword{documentation}
\keyword{utilities}
