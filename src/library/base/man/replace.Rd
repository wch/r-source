\name{replace}
\title{Replace Values in a Vector}
\usage{
replace(x, list, values)
}
\alias{replace}
\description{
\code{replace} replaces the values in \code{x}
with indexes given in \code{list} by those given in \code{values}.
If necessary, the values in \code{values} are recycled.
}
\arguments{
  \item{x}{vector}
  \item{list}{an index vector}
  \item{values}{replacement values}
}
\value{
  A vector with the values replaced.
}
\note{
  \code{x} is unchanged: remember to assign the result.
}
\keyword{manip}
