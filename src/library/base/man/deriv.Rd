\name{deriv}
\title{Symbolic and Algorithmic Derivatives of Simple Expressions}
\usage{
D(expr, namevec)
deriv(expr, namevec, function.arg = NULL, tag = ".expr")
}
\alias{D}
\alias{deriv}
\alias{deriv.default}
\alias{deriv.formula}
\arguments{
  \item{expr}{expression which should be derivated}
  \item{namevec}{character vector, giving the variable names with
    respect to which derivatives will be computed.}
  \item{function.arg}{\emph{NOT YET IMPLEMENTED}.  If specified, a
    function `prototype' (with empty \code{\link{body}}) which will be
    used to return a function with the given argument list, instead of
    an expression.}
  \item{tag}{character; the prefix to be used for the locally created
    variables in result..}
}
\description{
  \code{D} is modelled after its S pendant for taking simple symbolic
  derivatives.

  \code{deriv} is a \emph{generic} function with a default and a
  \code{\link{formula}} method.  It returns a \code{\link{call}} for
  computing the \code{expr} and its (partial) derivatives,
  simultaneously.  It uses so-called \emph{``algorithmic
    derivatives''}.

  Currently, \code{deriv.formula} just calls \code{deriv.default} after
  extracting the expression to the right of \code{~}.
}
\value{
  \code{D} returns an expression and therefore can easily be iterated
  for higher derivatives.

  \code{deriv} returns a \code{\link{call}} object which becomes an
  \code{\link{expression}} when evaluated once.  Evaluation of the
  latter expression returns the function values with a
  \code{".gradient"} attribute containing the gradient matrix.
}
\note{
  This help page should be fixed up by one of R&R or someone else who
  fluently speaks the language in \file{\$R\_HOME/src/main/deriv.c}.

  It's author, MM, has only got a vague idea and thinks that a help page
  is better than none.
}
\references{
A. Griewank, G. F. Corliss (1991).
\emph{Automatic Differentiation of Algorithms: Theory, Implementation,
and Application}.
SIAM proceedings, Philadelphia.
}
\seealso{
  \code{\link{nlm}} for numeric minimization which should make use of
  derivatives.
}
\examples{
## formula argument :
dx2x <- deriv(~ x^2, "x") ; dx2x
##- expression({
##-         .value <- x^2
##-         .grad <- array(0, c(length(.value), 1), list(NULL, c("x")))
##-         .grad[, "x"] <- 2 * x
##-         attr(.value, "gradient") <- .grad
##-         .value
##- })
mode(dx2x)
x <- -1:2
eval(dx2x)


## Something `tougher':
trig.exp <- expression(sin(cos(x + y^2)))
( D.sc <- D(trig.exp, c("x", "y")) )

( dxy <- deriv(trig.exp, c("x", "y")) )
y <- 1
eval(dxy)
eval(D.sc)
}
\keyword{math}
\keyword{nonlinear}
