\name{iris}
\alias{iris}
\alias{iris3}
\title{
    Edgar Anderson's Iris Data
}
\usage{
data(iris)
data(iris3)
}
\description{
    This famous (Fisher's or Anderson's) iris data set gives the
    measurements in centimeters of the variables sepal length and width
    and petal length and width, respectively, for 50 flowers from each
    of 3 species of iris.  The species are Setosa, Versicolor, and
    Virginica.
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
    R. A. Fisher (1936).
    The Use of Multiple Measurements in Taxonomic Problems.
    \emph{Annals of Eugenics,}
    \bold{7}, Part II, 179--188.

    The data were collected by
    Edgar Anderson (1935).
    The irises of the Gaspe Peninsula,
    \emph{Bulletin of the American Iris Society,}
    \bold{59}, 2--5.
}
\keyword{datasets}
