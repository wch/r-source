\name{model.matrix}
\title{Construct Design Matrices}
\usage{
model.matrix   (object, \dots)
model.matrix.lm(object, \dots)
model.matrix.default(formula, data, contrasts.arg = NULL, xlev = NULL)
model.weights(x)
}
\alias{model.matrix}
\alias{model.matrix.default}
\alias{model.matrix.lm}
\alias{model.matrix.lm.null}
\alias{model.matrix.glm.null}
\alias{model.weights}
\arguments{
  \item{formula}{a model formula or terms object.}
  \item{data}{a data frame created with \code{\link{model.frame}}.}
  \item{contrasts.arg}{A list, whose entries are contrasts suitable for
    input to the \code{\link{contrasts}} function and whose names are
    the names of columns of \code{data} containing \code{\link{factor}}s.}
  \item{xlev}{to be used as argument of \code{\link{model.frame}} if
    \code{data} has no \code{"terms"} attribute.}
  \item{x}{a modle frame.}
}
\description{
  \code{model.matrix} creates a design matrix.
  \code{model.weights} extracts the weights.
}
\details{
  \code{model.matrix} creates a design matrix from the description given
  in \code{terms(formula)}, using the data in \code{data} which must
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
  Chambers, J. M. and Hastie, T. J. eds (1992)
  \emph{Statistical Models in S.}
  Chapman & Hall, London.
}
\seealso{
  \code{\link{model.frame}},
  \code{\link{terms}}
}
\examples{
data(trees)
ff <- log(Volume) ~ log(Height) + log(Girth)
str(m <- model.frame(ff, trees))
mat <- model.matrix(ff, m)

dd <- data.frame(a = gl(3,4), b = gl(4,1,12))# balanced 2-way
options("contrasts")
model.matrix(~ a + b, dd)
model.matrix(~ a + b, dd, contrasts = list(a="contr.sum"))
model.matrix(~ a + b, dd, contrasts = list(a="contr.sum", b="contr.poly"))
m.orth <- model.matrix(~a+b, dd, contrasts = list(a="contr.helmert"))
crossprod(m.orth)# m.orth is  ALMOST  orthogonal
}
\keyword{models}
