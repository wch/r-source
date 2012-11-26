% File src/library/datasets/man/iris.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{iris}
\docType{data}
\alias{iris}
\alias{iris3}
\title{Edgar Anderson's Iris Data}
\description{
  This famous (Fisher's or Anderson's) iris data set gives the
  measurements in centimeters of the variables sepal length and width
  and petal length and width, respectively, for 50 flowers from each
  of 3 species of iris.  The species are \emph{Iris setosa},
  \emph{versicolor}, and \emph{virginica}.
}
\usage{
iris
iris3
}
\format{
  \code{iris} is a data frame with 150 cases (rows) and 5 variables
  (columns) named \code{Sepal.Length}, \code{Sepal.Width},
  \code{Petal.Length}, \code{Petal.Width}, and \code{Species}.

  \code{iris3} gives the same data arranged as a 3-dimensional array
  of size 50 by 4 by 3, as represented by S-PLUS.  The first dimension
  gives the case number within the species subsample, the second the
  measurements with names \code{Sepal L.}, \code{Sepal W.},
  \code{Petal L.}, and \code{Petal W.}, and the third the species.
}
\source{
  Fisher, R. A. (1936)
  The use of multiple measurements in taxonomic problems.
  \emph{Annals of Eugenics},
  \bold{7}, Part II, 179--188.

  The data were collected by
  Anderson, Edgar (1935).
  The irises of the Gaspe Peninsula,
  \emph{Bulletin of the American Iris Society},
  \bold{59}, 2--5.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole. (has \code{iris3} as \code{iris}.)
}
\seealso{
  \code{\link{matplot}} some examples of which use
  \code{iris}.
}
\examples{
dni3 <- dimnames(iris3)
ii <- data.frame(matrix(aperm(iris3, c(1,3,2)), ncol = 4,
                        dimnames = list(NULL, sub(" L.",".Length",
                                        sub(" W.",".Width", dni3[[2]])))),
    Species = gl(3, 50, labels = sub("S", "s", sub("V", "v", dni3[[3]]))))
all.equal(ii, iris) # TRUE
}
\keyword{datasets}
