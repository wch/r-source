\name{model.matrix}
\title{Construct design matrices}
\usage{
model.matrix(formula =, data =,contrasts=NULL)
}
\alias{model.matrix}
\arguments{
  \item{formula}{A model formula or terms object}
  \item{data}{A data frame created with \code{model.frame}}
  \item{contrasts}{A list, whose entries are contrasts suitable for input
to the \code{\link{contrasts}} function and whose names are the names of
columns of \code{data} containing \code{\link{factor}}s}
\description{
  Creates a design matrix from the description given in
  \code{terms(formula)}, using the data in \code{data} which must
  contain columns with the same names as would be created by a call to
  \code{model.frame(formula)} or, more precisely, by evaluating
  \code{attr(terms(formula), "variables")}.  There may be other columns
  and the order is not important. If \code{contrasts} is specified it
overrides the default factor coding for that variable.
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
dd<-data.frame(a=factor(rep(1:3,4)),b=factor(rep(1:4,3)))
model.matrix(~a+b,dd)
model.matrix(~a+b,dd,contrasts=list(a="contr.sum"))
model.matrix(~a+b,dd,contrasts=list(a="contr.sum",b="contr.poly"))
}
\keyword{models}
