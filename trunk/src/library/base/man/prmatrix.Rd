% File src/library/base/man/prmatrix.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{prmatrix}
\alias{prmatrix}
\title{Print Matrices, Old-style}
\usage{
prmatrix(x, rowlab =, collab =,
         quote = TRUE, right = FALSE, na.print = NULL, \dots)
}
\arguments{
  \item{x}{numeric or character matrix.}
  \item{rowlab, collab}{(optional) character vectors giving row or column
    names respectively.  By default, these are taken from
    \code{\link{dimnames}(x)}.}
  \item{quote}{logical; if \code{TRUE} and \code{x} is of mode
    \code{"character"}, \emph{quotes} (\samp{"}) are used.} %"
  \item{right}{ if \code{TRUE} and \code{x} is of mode
    \code{"character"}, the output columns are \emph{right}-justified.}
  \item{na.print}{how \code{NA}s are printed.  If this is non-null, its
    value is used to represent \code{NA}.}
  \item{\dots}{arguments for \code{print} methods.}
}
\description{
  An earlier method for printing matrices, provided for S compatibility.
}
\details{
  \code{prmatrix} is an earlier form of \code{print.matrix}, and
  is very similar to the S function of the same name.
}
\value{
  Invisibly returns its argument, \code{x}.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\seealso{
  \code{\link{print.default}}, and other \code{\link{print}} methods.
}
\examples{
prmatrix(m6 <- diag(6), rowlab = rep("", 6), collab = rep("", 6))

chm <- matrix(scan(system.file("help", "AnIndex", package = "splines"),
                   what = ""), , 2, byrow = TRUE)
chm  # uses print.matrix()
prmatrix(chm, collab = paste("Column", 1:3), right = TRUE, quote = FALSE)
}
\keyword{print}
