\name{delete.response}
\title{Modify terms objects}
\usage{
delete.response(termobj)
reformulate(termlabels,response=NULL)
drop.terms(termobj, dropx = NULL, keep.response = FALSE) 
}
\alias{reformulate}
\alias{drop.terms}
\alias{delete.response}
\arguments{
 \item{termobj}{A \code{terms} object}
 \item{termlabels}{vector of strings giving the right-hand side of a model formula}
 \item{response}{string giving the left-hand side of a model formula}
 \item{dropx}{vector of positions of variables to drop from the right-hand side of the model}
 \item{keep.response}{Keep the response in the resulting object?}
}
\description{
\code{delete.response} returns a \code{terms} object for the same model but with no response variable. \code{drop.terms} removes variables from the right-hand side of the model. \code{reformulate} creates a formula from a vector of strings.
}
\value{
\code{delete.response} and \code{drop.terms} return a \code{terms} object; \code{\link{reformulate}} returns a \code{formula}.
}
\seealso{\code{\link{terms}}}

\examples{
ff<-y~z+x+w
tt<-terms(ff)
tt
delete.response(tt)
drop.terms(tt,2:3,keep.response=T)
reformulate(attr(tt,"term.labels"))
}
\keyword{programming}
