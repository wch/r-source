\name{deriv}
\title{Symbolic and Algorithmic Derivatives of Simple Expressions}
\usage{
   D (expr, name)
deriv(expr, namevec, function.arg = NULL, tag = ".expr")
}
\synopsis{
D(expr, name)
deriv(expr, ...)
deriv.default(expr, namevec, function.arg = NULL, tag = ".expr")
deriv.formula(expr, namevec, function.arg = NULL, tag = ".expr")
}
\alias{D}
\alias{deriv}
\alias{deriv.default}
\alias{deriv.formula}
\arguments{
  \item{expr}{\code{\link{expression}} or \code{\link{call}} which should
      be differentiated.}
  \item{name,namevec}{character vector, giving the variable names (only
    one for \code{D(.)}) with
    respect to which derivatives will be computed.}
  \item{function.arg}{If specified, a character vector of arguments for
    a function return. \bold{Note:} this is incompatible with S.}
  \item{tag}{character; the prefix to be used for the locally created
    variables in result.}
}
\description{
  Compute derivatives of simple expressions, symbolically.
}
\details{
  \code{D} is modelled after its S namesake for taking simple symbolic
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
  \code{D} returns a call and therefore can easily be iterated
  for higher derivatives.

  \code{deriv} normally returns an \code{\link{expression}} object.
  Its evaluation returns the function values with a
  \code{".gradient"} attribute containing the gradient matrix.
  If \code{function.arg} is specified, it returns a function.
}
\note{
  This help page should be fixed up by one of R&R or someone else who
  fluently speaks the language in \file{\$R\_HOME/src/main/deriv.c}.

  Its author, MM, has only got a vague idea and thinks that a help page
  is better than none.
}
\references{
  Griewank, A.  and  Corliss, G. F. (1991)
  \emph{Automatic Differentiation of Algorithms: Theory, Implementation,
    and Application}.
  SIAM proceedings, Philadelphia.
}
\seealso{
  \code{\link{nlm}} and \code{\link{optim}} for numeric minimization
  which could make use of derivatives,
  \code{\link[nls]{nls}} in package \code{nls}.
}
\examples{
## formula argument :
dx2x <- deriv(~ x^2, "x") ; dx2x
\dontrun{expression({
         .value <- x^2
         .grad <- array(0, c(length(.value), 1), list(NULL, c("x")))
         .grad[, "x"] <- 2 * x
         attr(.value, "gradient") <- .grad
         .value
})}
mode(dx2x)
x <- -1:2
eval(dx2x)

## Something `tougher':
trig.exp <- expression(sin(cos(x + y^2)))
( D.sc <- D(trig.exp, "x") )
all.equal(D(trig.exp[[1]], "x"), D.sc)

( dxy <- deriv(trig.exp, c("x", "y")) )
y <- 1
eval(dxy)
eval(D.sc)
stopifnot(eval(D.sc) ==
          attr(eval(dxy),"gradient")[,"x"])

## function returned:
deriv(y ~ b0 + b1 * 2^(-x/th), c("b0", "b1", "th"),
     c("b0", "b1", "th", "x") )


## Higher derivatives:
DD <- function(expr,name, order = 1) {
   if(order < 1) stop("`order' must be >= 1")
   if(order == 1) D(expr,name)
   else DD(D(expr, name), name, order - 1)
}
DD(expression(sin(x^2)), "x", 3)
## showing the limits of the internal "simplify()" :
\dontrun{
-sin(x^2) * (2 * x) * 2 + ((cos(x^2) * (2 * x) * (2 * x) + sin(x^2) *
    2) * (2 * x) + sin(x^2) * (2 * x) * 2)
}
}
\keyword{math}
\keyword{nonlinear}


