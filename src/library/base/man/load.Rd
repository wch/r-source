\name{load}
\title{Reload Saved Datasets}
\usage{
load(filename, envir = sys.frame(sys.parent())))
}
\alias{load}
\arguments{
  \item{filename}{a character string giving the name of the file to
    load.}
  \item{envir}{the environment where the data should be loaded} 
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

# restore the saved values to the current environment
load("all.Rdata")

# restore the saved values to the global environment
load("all.Rdata",globalenv())
}
\keyword{file}
