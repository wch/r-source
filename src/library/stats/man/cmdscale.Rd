% File src/library/stats/man/cmdscale.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2010 R Core Development Team
% Distributed under GPL 2 or later

\name{cmdscale}
\alias{cmdscale}
\concept{ordination}
\concept{MDS}
\title{Classical (Metric) Multidimensional Scaling}
\usage{
cmdscale(d, k = 2, eig = FALSE, add = FALSE, x.ret = FALSE)
}
\description{
  Classical multidimensional scaling of a data matrix.
  Also known as \emph{principal coordinates analysis} (Gower, 1966).
}
\arguments{
  \item{d}{a distance structure such as that returned by \code{dist}
    or a full symmetric matrix containing the dissimilarities.}
  \item{k}{the dimension of the space which the data are to be
    represented in; must be in \eqn{\{1, 2, \ldots, n-1\}}{{1, 2, \dots, n-1}}.}
  \item{eig}{indicates whether eigenvalues should be returned.}
  \item{add}{logical indicating if an additive constant \eqn{c*} should
    be computed, and added to the non-diagonal dissimilarities such that
    all \eqn{n-1} eigenvalues are non-negative.}
  \item{x.ret}{indicates whether the doubly centred symmetric distance
    matrix should be returned.}
}
\details{
  Multidimensional scaling takes a set of dissimilarities and returns a
  set of points such that the distances between the points are
  approximately equal to the dissimilarities.  (It is a major part of
  what ecologists call \sQuote{ordination}.)

  A set of Euclidean distances on \eqn{n} points can be represented
  exactly in at most \eqn{n-1} dimensions.  However, the approximation
  given by classical scaling for non-Eucliean distances can be poor for
  large \code{k}, and warnings about negative eigenvalues should be
  heeded.
  
  The representation is only determined up to location (\code{cmdscale}
  takes the column means of the configuration to be at the origin),
  rotations and reflections.  The configuration returned is given in
  principal-component axes, so the reflection chosen may differ between
  \R platforms (see \code{\link{prcomp}}).

  When \code{add = TRUE}, an additive constant \eqn{c*} is computed, and
  the dissimilarities \eqn{d_{ij} + c*}{d[i,j] + c*} are used instead of
  the original \eqn{d_{ij}}{d[i,j]}'s.

  Whereas S (Becker \emph{et al.}, 1988) computes this constant using
  an approximation suggested by Torgerson, \R uses the analytical
  solution of Cailliez (1983), see also Cox and Cox (2001).
}
\value{
  If \code{eig = FALSE}, \code{add = FALSE} and \code{x.ret = FALSE}
  (default), a matrix with \code{k} columns whose rows give the
  coordinates of the points chosen to represent the dissimilarities.

  Otherwise, a list containing the following components.
  \item{points}{a matrix with \code{k} columns whose rows give the
    coordinates of the points chosen to represent the dissimilarities.}
  \item{eig}{the \eqn{n} eigenvalues computed during the scaling process if
    \code{eig} is true.  \strong{NB}: versions of \R before 2.12.1
    returned only \code{k} but were documented to return \eqn{n-1}.}
  \item{x}{the doubly centered distance matrix if \code{x.ret} is true.}
  \item{ac}{the additive constant \eqn{c*}, \code{0} if \code{add=FALSE}.}
  \item{GOF}{a numeric vector of length 2, equal to say
    \eqn{(g_1,g_2)}{(g.1,g.2)}, where
    \eqn{g_i = (\sum_{j=1}^k \lambda_j)/ (\sum_{j=1}^n T_i(\lambda_j))}{g.i = (sum{j=1..k} \lambda[j]) / (sum{j=1..n} T.i(\lambda[j]))},
    where \eqn{\lambda_j}{\lambda[j]} are the eigenvalues (sorted in
    decreasing order),
    \eqn{T_1(v) = \left| v \right|}{T.1(v) = abs(v)}, and
    \eqn{T_2(v) = max( v, 0 )}{T.2(v) = max(v, 0)}.
  }
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.

  Cailliez, F. (1983)
  The analytical solution of the additive constant problem.
  \emph{Psychometrika} \bold{48}, 343--349.

  Cox, T. F. and Cox, M. A. A. (2001)
  \emph{Multidimensional Scaling}.  Second edition.
  Chapman and Hall.

  Gower, J. C. (1966)  
  Some distance properties of latent root and vector 
  methods used in multivariate analysis.  
  \emph{Biometrika} \bold{53}, 325--328.

  Krzanowski, W. J. and Marriott, F. H. C. (1994)
  \emph{Multivariate Analysis. Part I. Distributions, Ordination and
  Inference.}  London: Edward Arnold. (Especially pp. 108--111.)

  Mardia, K. V., Kent, J. T. and Bibby, J. M. (1979).  Chapter 14 of
  \emph{Multivariate Analysis}, London: Academic Press.

  Seber, G. A. F. (1984).
  \emph{Multivariate Observations}.
  New York: Wiley.

  Torgerson, W. S. (1958).
  \emph{Theory and Methods of Scaling}.
  New York: Wiley.
}
\seealso{
  \code{\link{dist}}.

  \code{\link[MASS]{isoMDS}} and \code{\link[MASS]{sammon}}
  in package \pkg{MASS} provide alternative methods of multidimensional
  scaling.
}
\examples{
require(graphics)

loc <- cmdscale(eurodist)
x <- loc[, 1]
y <- -loc[, 2] # reflect so North is at the top
## note asp = 1, to ensure Euclidean distances are represented correctly
plot(x, y, type = "n", xlab = "", ylab = "", asp = 1, axes = FALSE,
     main = "cmdscale(eurodist)")
text(x, y, rownames(loc), cex = 0.6)
}
\keyword{multivariate}
