\name{model.matrix}
\title{Construct design matrices}
\usage{
model.matrix(formula =, data =)
}
\alias{model.matrix}
\arguments{
  \item{formula}{A model formula or terms object}
  \item{data}{A data frame created with \code{model.frame}}
}
\description{
  Creates a design matrix from the description given in
  \code{terms(formula)}, using the data in \code{data} which must
  contain columns with the same names as would be created by a call to
  \code{model.frame(formula)} or, more precisely, by evaluating
  \code{attr(terms(formula), "variables")}.  There may be other columns
  and the order is not important.
}
\value{
  The design matrix for a regression model with the specified formula
  and data.
}
\references{
  Chambers and Hastie: "Statistical Models in S"
}
\seealso{
  \code{\link{model.frame}}, \code{\link{model.data.frame}},
  \code{\link{terms}}
}
\examples{
data(trees)
ff <- log(Volume) ~ log(Height) + log(Girth)
m <- model.frame(ff, trees)
mat <- model.matrix(ff, m)
}
\keyword{models}
