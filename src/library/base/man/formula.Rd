\name{formula}
\title{Model Formulae}
\usage{
y ~ model
formula(object)
formula.default(anything)
formula.formula(formula.obj)
formula.terms(terms.obj)
}
\alias{~}
\alias{formula}
\alias{formula.default}
\alias{formula.formula}
\alias{formula.terms}
\alias{print.formula}
\description{
The models fit by the \code{lm} and \code{glm} functions
are specified in a compact symbolic form.
The \code{~} operator is basic in the formation of such models.
An expression of the form \code{y~model} is interpreted
as a specification that the response \code{y} is modelled
by a linear predictor specified symbolically by \code{model}.
Such a model consists of a series of terms separated
by \code{+} operators.
The terms themselves consist of variable and factor
names separated by \code{:} operators.
Such a term is interpreted as the interaction of
all the variables and factors appearing in the term.

In addition to \code{+} and \code{:}, a number of other
operators are useful in model formulae.
The \code{*} operator denotes factor crossing:
\code{a*b} interpreted as \code{a+b+a:b}.
The \code{^} operator indicates crossing to the
specified degree.  For example \code{(a+b+c)^2}
is identical to \code{(a+b+c)*(a+b+c)} which in turn
expands to a formula containing the main effects
for \code{a}, \code{b} and \code{c} together
with their second-order interactions.
The \code{\%in\%} operator indicates that the terms
on its left are nested within those on the right.
For example \code{a+b\%in\%a} expands to the
formula \code{a+a:b}.

While formulae usually involve just variable and factor
names, they can also involve arithmetic expressions.
The formula \code{log(y)~a+log(x)} is quite legal.
When such arithmetic expressions involve
operators which are also used symbolically
in model formulae, there can be confusion between
arithmetic and symbolic operator use.
To avoid this confusion, the function \code{I()}
can be used to bracket those portions of a model
formula where the operators are used in their
arithmetic sense.  For example, in the formula
\code{y~a+I(b+c)}, the term \code{b+c} is to be
interpreted as the sum of \code{b} and \code{c}.

The generic function \code{formula} and its specific
methods provide a way of extracting formulae
which have been included in other objects.
}
\value{
All the functions above produce an object
of class \code{formula} which contains a symbolic model formula.
}
\seealso{
\code{\link{lm}}, \code{\link{glm}}, \code{\link{terms}}.
}
\examples{
class(fo <- y ~ x1*x2) # "formula"
fo
typeof(fo)# R internal : "language"
terms(fo)
}
\keyword{models}
