\name{load}
\title{Reload Saved Datasets}
\usage{
load(filename)
}
\alias{load}
\arguments{
\item{filename}{a character string giving the name of the file to load.}
}
\description{
This function will reload the datasets written to a file
with the function \code{save}.
}
\seealso{
\code{\link{save}}.
}
\examples{
# save all data
save(list = ls(), file= "all.Rdata")

# restore the saved values
load("all.Rdata")
}
\keyword{file}
