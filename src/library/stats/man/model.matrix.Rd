% File src/library/stats/man/model.matrix.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2019 R Core Team
% Distributed under GPL 2 or later

\name{model.matrix}
\alias{model.matrix}
\alias{model.matrix.default}
\alias{model.matrix.lm}
\title{Construct Design Matrices}
\usage{
model.matrix(object, \dots)

\method{model.matrix}{default}(object, data = environment(object),
             contrasts.arg = NULL, xlev = NULL, \dots)
}
\arguments{
  \item{object}{an object of an appropriate class.  For the default
    method, a model \link{formula} or a \code{\link{terms}} object.}
  \item{data}{a data frame created with \code{\link{model.frame}}.  If
    another sort of object, \code{model.frame} is called first.}
  \item{contrasts.arg}{a list, whose entries are values (numeric
    matrices, \code{\link{function}}s or character strings naming
    functions) to be used
    as replacement values for the \code{\link{contrasts}}
    replacement function and whose names are the names of
    columns of \code{data} containing \code{\link{factor}}s.}
  \item{xlev}{to be used as argument of \code{\link{model.frame}} if
    \code{data} is such that \code{model.frame} is called.}
  \item{\dots}{further arguments passed to or from other methods.}
}
\description{
  \code{model.matrix} creates a design (or model) matrix, e.g., by
  expanding factors to a set of dummy variables (depending on the
  contrasts) and expanding interactions similarly.
}
\details{
  \code{model.matrix} creates a design matrix from the description
  given in \code{terms(object)}, using the data in \code{data} which
  must supply variables with the same names as would be created by a
  call to \code{model.frame(object)} or, more precisely, by evaluating
  \code{attr(terms(object), "variables")}.  If \code{data} is a data
  frame, there may be other columns and the order of columns is not
  important.  Any character variables are coerced to factors.  After
  coercion, all the variables used on the right-hand side of the
  formula must be logical, integer, numeric or factor.

  If \code{contrasts.arg} is specified for a factor it overrides the
  default factor coding for that variable and any \code{"contrasts"}
  attribute set by \code{\link{C}} or \code{\link{contrasts}}.
  Whereas invalid \code{contrasts.arg}s have been ignored always, they are
  warned about since \R version 3.6.0.

  In an interaction term, the variable whose levels vary fastest is the
  first one to appear in the formula (and not in the term), so in
  \code{~ a + b + b:a} the interaction will have \code{a} varying
  fastest.

  By convention, if the response variable also appears on the
  right-hand side of the formula it is dropped (with a warning),
  although interactions involving the term are retained.
}
\value{
  The design matrix for a regression-like model with the specified formula
  and data.

  There is an attribute \code{"assign"}, an integer vector with an entry
  for each column in the matrix giving the term in the formula which
  gave rise to the column.  Value \code{0} corresponds to the intercept
  (if any), and positive values to terms in the order given by the
  \code{term.labels} attribute of the \code{terms} structure
  corresponding to \code{object}.

  If there are any factors in terms in the model, there is an attribute
  \code{"contrasts"}, a named list with an entry for each factor.  This
  specifies the contrasts that would be used in terms in which the
  factor is coded by contrasts (in some terms dummy coding may be used),
  either as a character vector naming a function or as a numeric matrix.
}
\references{
  Chambers, J. M. (1992)
  \emph{Data for models.}
  Chapter 3 of \emph{Statistical Models in S}
  eds J. M. Chambers and T. J. Hastie, Wadsworth & Brooks/Cole.
}
\seealso{
  \code{\link{model.frame}}, \code{\link{model.extract}},
  \code{\link{terms}}

  \code{\link[Matrix]{sparse.model.matrix}} from package
  \CRANpkg{Matrix} for creating \emph{sparse} model matrices, which may
  be more efficient in large dimensions.
}
\examples{
ff <- log(Volume) ~ log(Height) + log(Girth)
utils::str(m <- model.frame(ff, trees))
mat <- model.matrix(ff, m)

dd <- data.frame(a = gl(3,4), b = gl(4,1,12)) # balanced 2-way
options("contrasts") # typically 'treatment' (for unordered factors)
model.matrix(~ a + b, dd)
model.matrix(~ a + b, dd, contrasts = list(a = "contr.sum"))
model.matrix(~ a + b, dd, contrasts = list(a = "contr.sum", b = contr.poly))
m.orth <- model.matrix(~a+b, dd, contrasts = list(a = "contr.helmert"))
crossprod(m.orth) # m.orth is  ALMOST  orthogonal
# invalid contrasts.. ignored with a warning:
stopifnot(identical(
   model.matrix(~ a + b, dd),
   model.matrix(~ a + b, dd, contrasts.arg = "contr.FOO")))
}
\keyword{models}
