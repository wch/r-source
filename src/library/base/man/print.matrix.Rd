\name{print.matrix}
\title{Print Matrices}
\usage{
print.matrix(x, rowlab=character(0), collab=character(0),
             quote=TRUE, right=FALSE, na.print = NULL,
             print.gap = NULL, \dots)
}
\alias{print.matrix}
\alias{prmatrix}
\arguments{
  \item{x}{numeric or character matrix.}
  \item{rowlab,collab}{(optional) character vectors giving row or column
    names respectively.  By default, these are taken from
    \code{\link{dimnames}(x)}.}
  \item{quote}{logical; if \code{TRUE} and \code{x} is of mode
    \code{"character"}, \emph{quotes} (\code{".."}) are used.}
  \item{right}{ if \code{TRUE} and \code{x} is of mode
    \code{"character"}, the output columns are \emph{right}-justified.}
  \item{na.print}{how \code{NA}s are printed.  If this is non-null, its
    value is used to represent \code{NA}.}
  \item{print.gap}{not yet used.}
  \item{\dots}{arguments for other methods.}
}
\description{
  Pseudo-method for the \code{\link{print}} generic.  Especially useful
  with the \code{right} argument which does not (yet) exist for
  \code{\link{print.default}}.
}
\details{
  \code{print.matrix} and \code{\link{print.default}} both print
  matrices, and each has at least an optional argument that the other
  lacks.  Also, both directly dispatch into \code{.Internal} code
  directly instead of relying on each other.
  This mainly stems from historic compatibility and similar reasons should
  be changed in the future.
  %%-- ``signed:  Martin Maechler''

  \code{prmatrix} is currently just an \code{.Alias} for
  \code{print.matrix}.
}
\value{Invisibly returns its argument, \code{x}.}}
\seealso{\code{\link{print.default}}, and other \code{\link{print}} methods.}
\examples{
print.matrix(m6 <- diag(6), row = rep("",6), coll=rep("",6))

chm <- matrix(scan(file.path(system.file(pkg = "eda"),
                             "help", "AnIndex"),
                   what = ""), , 2, byrow = TRUE)
chm  #-> print.default(.)  = `same' as  print.matrix(chm)
print.matrix(chm, collab = paste("Column",1:3), right=TRUE, quote=FALSE)
}
\keyword{print}
