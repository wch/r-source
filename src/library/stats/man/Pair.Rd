\name{Pair}
\alias{Pair}
\title{
Construct paired-data object
}
\description{
Combines two vectors into an object of class \code{"Pair"}
}
\usage{
Pair(x, y)
}
\arguments{
  \item{x}{
	Vector. 1st element of pair.
  }
  \item{y}{
	Vector. 2nd element of pair. Should be same lenght as \code{x}
  }  
}

\value{
   A 2-column matrix of class \code{"Pair"}

}
\note{
   Mostly designed as part of the formula interface to paired tests.
}


\seealso{
  \code{t.test} and \code{wilcox.test}
}
