\name{savehistory}
\alias{savehistory}
\title{Save Commands History}
\description{
  Save the commands history in a file.
}
\usage{
savehistory(file = ".Rhistory")
}
\arguments{
  \item{file}{The name of the file in which to save the history. The path
    is relative to the current working directory.}
}
\details{
  This works in Rgui and Rterm but not in embedded/DCOM versions.
}
\keyword{utilities}
