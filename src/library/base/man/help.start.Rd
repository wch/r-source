\name{help.start}
\title{Hypertext Documentation}
\usage{
help.start(gui = "irrelevant", browser = "netscape", remote = NULL)
}
\alias{help.start}
\arguments{
    \item{gui}{just for compatibility}
    \item{browser}{the name of the program to be used as hypertext
	browser.}
    \item{remote}{A character giving a valid URL for the
	\file{\$R\_HOME} directory on a remote location.}
}
\description{
    Start the hypertext (currently HTML) version of \R's online
    documentation.
}
\details{
    The \code{browser} needs to work with a \code{"-remote openURL(..)"}
    call. Then, the hypertext \R help interface is opened in any already
    running browser or a new one if necessary.
}
\seealso{
    \code{\link{help}()} for on- and off-line help in ASCII/Editor or
    PostScript format.
}
\examples{
\dontrun{help.start()}
}
\keyword{documentation}
