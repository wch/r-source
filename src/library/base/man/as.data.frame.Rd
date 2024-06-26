% File src/library/base/man/as.data.frame.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2024 R Core Team
% Distributed under GPL 2 or later

\name{as.data.frame}
\title{Coerce to a Data Frame}
\alias{as.data.frame}
\alias{is.data.frame}
\alias{as.data.frame.AsIs}
\alias{as.data.frame.character}
\alias{as.data.frame.data.frame}
\alias{as.data.frame.default}
\alias{as.data.frame.list}
\alias{as.data.frame.matrix}
\alias{as.data.frame.model.matrix}
\alias{as.data.frame.ts}
\alias{as.data.frame.vector}
\alias{as.data.frame.array}

\description{
  Functions to check if an object is a data frame, or coerce it if possible.
}
\usage{
as.data.frame(x, row.names = NULL, optional = FALSE, \dots)

\method{as.data.frame}{character}(x, \dots,
              stringsAsFactors = FALSE)

\method{as.data.frame}{list}(x, row.names = NULL, optional = FALSE, \dots,
              cut.names = FALSE, col.names = names(x), fix.empty.names = TRUE,
              new.names = !missing(col.names),
              check.names = !optional,
              stringsAsFactors = FALSE)

\method{as.data.frame}{matrix}(x, row.names = NULL, optional = FALSE,
              make.names = TRUE, \dots,
              stringsAsFactors = FALSE)

as.data.frame.vector(x, row.names = NULL, optional = FALSE, \dots,
                     nm = deparse1(substitute(x)))

is.data.frame(x)
}
\arguments{
  \item{x}{any \R object.}
  \item{row.names}{\code{NULL} or a character vector giving the row
    names for the data frame.  Missing values are not allowed.}
  \item{optional}{logical. If \code{TRUE}, setting row names and
    converting column names (to syntactic names: see
    \code{\link{make.names}}) is optional.  Note that all of \R's
    \pkg{base} package \code{as.data.frame()} methods use
    \code{optional} only for column names treatment, basically with the
    meaning of \code{\link{data.frame}(*, check.names = !optional)}.
    See also the \code{make.names} argument of the \code{matrix} method.}
  \item{\dots}{additional arguments to be passed to or from methods.}
  \item{stringsAsFactors}{logical: should the character vector be converted
    to a factor?}
  %% list method:
  \item{cut.names}{logical or integer; indicating if column names with
    more than 256 (or \code{cut.names} if that is numeric) characters
    should be shortened (and the last 6 characters replaced by \code{" ..."}).}
  \item{col.names}{(optional) character vector of column names.}
  \item{fix.empty.names}{logical indicating if empty column names, i.e.,
    \code{""} should be fixed up (in \code{\link{data.frame}}) or not.}
  \item{new.names}{logical indicating that \code{col.names} should be used
    (and possibly reset to \code{NA}) explicitly.}
  \item{check.names}{logical; passed to the \code{\link{data.frame}()} call.}
  %% matrix method:
  \item{make.names}{a \code{\link{logical}}, i.e., one of \code{FALSE, NA, TRUE},
    indicating what should happen if the row names (of the matrix
    \code{x}) are invalid.  If they are invalid, the default,
    \code{TRUE}, calls \code{\link{make.names}(*, unique=TRUE)};
    \code{make.names=NA} will use \dQuote{automatic} row names and a
    \code{FALSE} value will signal an error for invalid row names.}
  %% *.vector():
  \item{nm}{a \code{\link{character}} string to be used as column name.}
}
\value{
  \code{as.data.frame} returns a data frame, normally with all row names
  \code{""} if \code{optional = TRUE}.

  \code{is.data.frame} returns \code{TRUE} if its argument is a data
  frame (that is, has \code{"data.frame"} amongst its classes)
  and \code{FALSE} otherwise.
}
\details{
  \code{as.data.frame} is a generic function with many methods, and
  users and packages can supply further methods.  For classes that act
  as vectors, often a copy of \code{as.data.frame.vector} will work
  as the method.

  Since \R 4.3.0, the \emph{default} method will call
  \code{as.data.frame.vector} for atomic (as by \code{\link{is.atomic}}) \code{x}.

  Direct calls of \code{as.data.frame.\var{class}} are still possible (base package!),
  for 12 atomic base classes, but are deprecated
  where calling \code{as.data.frame.vector} instead is recommended.

  If a list is supplied, each element is converted to a column in the
  data frame.  Similarly, each column of a matrix is converted separately.
  This can be overridden if the object has a class which has
  a method for \code{as.data.frame}: two examples are
  matrices of class \code{"\link{model.matrix}"} (which are
  included as a single column) and list objects of class
  \code{"\link{POSIXlt}"} which are coerced to class
  \code{"\link{POSIXct}"}.

  Arrays can be converted to data frames.  One-dimensional arrays are
  treated like vectors and two-dimensional arrays like matrices.  Arrays
  with more than two dimensions are converted to matrices by
  \sQuote{flattening} all dimensions after the first and creating
  suitable column labels.

  Character variables are converted to factor columns unless protected
  by \code{\link{I}}.

  If a data frame is supplied, all classes preceding \code{"data.frame"}
  are stripped, and the row names are changed if that argument is supplied.

  If \code{row.names = NULL}, row names are constructed from the names
  or dimnames of \code{x}, otherwise are the integer sequence
  starting at one.  Few of the methods check for duplicated row names.
  Names are removed from vector columns unless \code{\link{I}}.
}
\references{
  Chambers, J. M. (1992)
  \emph{Data for models.}
  Chapter 3 of \emph{Statistical Models in S}
  eds J. M. Chambers and T. J. Hastie, Wadsworth & Brooks/Cole.
}
\seealso{
  \code{\link{data.frame}} and  \code{\link{make.names}};
  \code{\link{as.data.frame.table}} for the \code{table} method (which has
  additional arguments).
}
\examples{
L0 <- list(LETTERS[1:7], c(4L, 2:3, 5:7, 1L))
L <- L0; names(L) <- nms <- c("nam", "age")
 d0 <- as.data.frame(L0, col.names = nms)
(d1 <- as.data.frame(L))
stopifnot(identical(d0, d1))

## showing possibilities  on how NA names are handled:
L <- list(A = 1:4); names(L) <- NA
names(dL  <- as.data.frame(L))                                          # "NA."
names(dL1 <- as.data.frame(L, col.names = names(L)))                    # "NA."
##
names(dL1.<- as.data.frame(L,                       check.names=FALSE)) # "NA"
##
names(dL2 <- as.data.frame(L, col.names = names(L), check.names=FALSE)) #  NA
names(dLn <- as.data.frame(L, new.names = TRUE,     check.names=FALSE)) #  NA
}
\keyword{classes}
\keyword{methods}
