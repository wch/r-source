\name{TestRd}% Arithmetic -- Logic -- Extract -- formula -- Comparison}
\title{Arithmetic / Logical Op.s / Extract .. / Formulae / Comparison}
\usage{
x + y
x - y
x * y
x / y
x ^ y
x \%\% y
x \%/\% y
! x
x & y
x && y
x | y
x || y
xor(x, y)
x[i]
x[i,j, ...]
x[i,j, ... , drop=TRUE]
x[[i]]
x[[i,j, ...]]
x$name

y ~ model
formula(object)
formula.default(anything)
formula.formula(formula.obj)
formula.terms(terms.obj)

x < y
x > y
x <= y
x >= y
x == y
x != y
}
\alias{Extract}
\alias{Subscript}
\alias{class<-}
\alias{[}
\alias{[[}
\alias{$}
\alias{+}
\alias{-}
\alias{*}
\alias{/}
\alias{^}
\alias{\%\%}
\alias{\%/\%}
\alias{Arithmetic}
\alias{!}
\alias{&}
\alias{&&}
\alias{|}
\alias{||}
\alias{xor}
\alias{Logic}
\alias{~}
\alias{formula.default}
\alias{formula.formula}
\alias{formula.terms}
\alias{print.formula}
\alias{<}
\alias{<=}
\alias{==}
\alias{!=}
\alias{>=}
\alias{>}
\alias{Comparison}

\description{
  These operators act on logical vectors.

  \code{!} indicates logical negation (NOT).

  \code{&} and \code{&&} indicate logical AND and \code{|} and \code{||}
  indicate logical OR.  The shorter form performs elementwise
  comparisons in much the same way as arithmetic operators.  The longer
  form evaluates left to right examining only the first element of each
  vector.  Evaluation proceeds only until the result is determined.  The
  longer form is appropriate for programming control-flow.

  \code{xor} indicates elementwise exclusive OR.
}
\details{

  These operators are generic. You can write methods to handle subsetting
of specific classes of data.

The \code{[[} operator requires all relavent subscripts be supplied.
With  the \code{[} operator a comma separated blank indicates that all
entries in that dimension are selected.

When operating on a list the  \code{[[} operator gives the specified
element of the list while the \code{[} operator returns a list with
the specified element(s) in it.


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

  An \R ``object'' is a data object which has a \code{class} attribute.
  A class attribute is a vector of character strings giving the names of
  the classes which the object ``inherits'' from.  When a generic
  function \code{fun} is applied to an object with class attribute
  \code{c("first", "second")}, the system searches for a function called
  \code{fun.first} and, if it finds it, applies it to the object.  If no
  such function is found, a function called \code{fun.second} is tried.
  If no class name produces a suitable function, the function
  \code{fun.default} is used.

  The function \code{class} prints the vector of names of classes an
  object inherits from.  Correspondingly, \code{class<-} sets the
  classes an object inherits from.

}
\value{
These binary operators perform arithmetic on vector objects.
They return numeric vectors containing the result of the element
by element operations.  The elements of shorter vectors are recycled
as necessary.  The operators are \code{+} for addition, \code{-} for
subtraction \code{*} for multiplication, \code{/} for division and
\code{^} for exponentiation.
\code{\%\%} indicates \code{x mod y} and \code{\%/\%} indicates integer
division.

Objects such as arrays or time-series can be operated on this
way provided they are conformable.
}
\seealso{
\code{\link{Math}} for miscellaneous and \code{\link{Special}} for special
mathematical functions.

\code{\link{list}}, \code{\link{array}}, \code{\link{matrix}}.
}
\examples{
x <- 1:12; m <- matrix(1:6,nr=2); li <- list(pi=pi, e = exp(1))
x[10]   # the tenth element of x
m[1,]   # the first row of matrix m
m[1, , drop = FALSE]# is a 1-row matrix
li[[1]] # the first element of list li
y<-list(1,2,a=4,5)
y[c(3,4)] # a list containing the third and fourth elements of y
y$a      #the element of y named a
##-------------------------------------------

%%-- The following "\testonly{" is not properly treated [latex/html/nroff]:
%% ===>  ALL the following example text is doubled !!
\testonly{
  cat("x <- c(1,4)\n  x ^ 3 -10 ; outer(1:7,5:9)\n", file="xyz.Rdmped")}
# parse 3 statements from the file "xyz.Rdmped"
parse(file = "xyz.Rdmped", n = 3)


x <- -1:12
x + 1
2 * x + 3
x \%\% 2
x \%/\% 5

y <- 1 + (x <- rpois(50, lambda=1.5) / 4 - 1)
x[(x > 0) & (x < 1)]    # all x values between 0 and 1
if (any(x == 0) || any(y == 0)) "zero encountered"

}
}% this is extraneous! should give an error!
\keyword{array}
\keyword{list}
