\name{plotmath}
\alias{plotmath}
\title{Mathematical Annotation in R}
\description{
  If the \code{text} argument to one of the text-drawing functions
  (\code{\link{text}}, \code{\link{mtext}}, \code{\link{axis}}) in \R
  is an expression, the argument is interpreted as a mathematical
  expression and the output will be formatted according to TeX-like
  rules.
}
\details{
  %% FIXME: use \describe instead when this works in nroff!
  A mathematical expression must obey the normal rules of syntax
  for any \R expression, but it is interpreted according to very
  different rules than for normal \R expressions.
  
  \emph{Binary operators:} addition, subtraction, multiplication, and
  division use the standard \R syntax, although multiplication only
  juxtaposes the arguments.  For example, \code{a+b}, \code{a-b}, and
  \code{a/b}, produce \eqn{a+b}, \eqn{a-b}, and \eqn{a/b}, but
  \code{a*b} produces \eqn{ab}.

  \emph{Unary operators:} positive and negative numbers are specified
  with standard syntax.  For example, \code{+x} produces \eqn{+x} and
  \code{-y} produces \eqn{-y}.

  \emph{Subscripts and superscripts:} a subscript is specified using the
  subsetting syntax and a superscript is specified using the power
  syntax.  For example, \code{x[i]} produces \eqn{x_i} and \code{x^2}
  produces \eqn{x^2}.

  \emph{Accents:} accented expressions are specified using the special
  mathematical functions \code{hat} and \code{bar}.
  % For example, \code{hat(x)} produces \eqn{\hat{x}} and \code{bar(x)}
  % produces \eqn{\bar{x}}.

  \emph{Fractions:} fractions are specified using the special
  mathematical function \code{frac} (or its alias, \code{over}).
  % For example, \code{frac(1,2)} produces \eqn{1\over2}.

  \emph{Relations:} equality or assignment of terms is specified using
  the \code{==} relation.  For example, \code{x == y} produces
  \eqn{x=y}.

  \emph{Visible grouping:} terms are visibly grouped by placing them
  within parentheses.  For example, \code{(x+y)} produces \eqn{(x+y)}.

  \emph{Invisible grouping:} terms are invisibly grouped by placing them
  within curly braces.  For example, \code{x^{2*y}} produces
  \eqn{x^{2y}}, whereas \code{x^2*y} produces \eqn{x^2y}.

  \emph{Big operators:} a sum, product, or integral is specified using
  the special mathematical function of the corresponding name.  Each of
  these functions takes three arguments;  the first indicates what is
  being summed/multiplied/integrated and the second and third specify
  the limits of the summation/product/integral.
  For example, \code{sum(x[i], i==0, n)} produces
  \deqn{\sum\limits_{i=0}^n x_i}{sum_{i=0}^n x_i}.

  \emph{Radicals:} a square root expression is specified using the
  special mathematical functions \code{root} and \code{sqrt}.
  % For example, \code{sqrt(x)} produces \eqn{\sqrt x}.

  \emph{Absolute values:} an absolute term is specified using the
  special mathematical function \code{abs}.  For example, \code{abs(x)}
  produces \eqn{|x|}.

  \emph{Juxtaposition:} multiple terms are juxtaposed using the special
  mathematical function \code{paste}.  For example,
  \code{paste(over(b, 2), y, sum(x))} produces
  \eqn{\frac{b}{2} y \sum x}{b/2 y sum(x)}.

  \emph{Typeface changes:} the default font in mathematical expressions
  is italic (except for terms which are symbols).  A new typeface is
  specified using the special mathematical functions \code{bold},
  \code{italic}, \code{plain}, and \code{bolditalic}.  Note that these
  font specifications do not accumulate (i.e., \code{bold(italic(x)))}
  gives an italic `x', whereas \code{bolditalic(x)} produces a bold,
  italic `x').
  % For example, \code{plain(X)[i]} produces \eqn{\textrm{X}_i}.

  \emph{General expressions:} any functional expression which is not a
  special mathematical function is simply reproduced as a function
  expression.  For example, \code{foo(x)} produces \eqn{foo(x)}.
}
\seealso{
  \code{\link{axis}},
  \code{\link{mtext}},
  \code{\link{text}},
  \code{\link{title}}
}
\examples{
x <- seq(-4, 4, len = 101)
y <- cbind(sin(x), cos(x))
matplot(x, y, type = "l", xaxt = "n",
        main = expression(paste(plain(sin) * phi, "  and  ",
                                plain(cos) * phi)),
        ylab = expression("sin" * phi, "cos" * phi),    # only 1st is taken
        xlab = expression(paste("Phase Angle ", phi)),
        col.main = "blue")
axis(1, at = c(-pi, -pi/2, 0, pi/2, pi),
     lab = expression(-pi, -pi/2, 0, pi/2, pi))

plot(1:10, 1:10)
text(4, 9, expression(hat(beta) == (X^t * X)^{-1} * X^t * y))
text(4, 8.4, "expression(hat(beta) == (X^t * X)^{-1} * X^t * y)", cex = .8)
text(4, 7, expression(bar(x) == sum(frac(x[i], n), i==1, n)))
text(4, 6.4, "expression(bar(x) == sum(frac(x[i], n), i==1, n))", cex = .8)
text(8, 5, expression(paste(frac(1, sigma*sqrt(2*pi)), " ",
                            plain(e)^{frac(-(x-mu)^2, 2*sigma^2)})), cex= 1.2)
}
\keyword{aplot}
