% File src/library/stats/man/Pair.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2019 R Core Team
% Distributed under GPL 2 or later

\name{Pair}
\alias{Pair}
\title{
Construct a Paired-Data Object
}
\description{
Combines two vectors into an object of class \code{"Pair"}.
}
\usage{
Pair(x, y)
}
\arguments{
  \item{x}{
    a vector, the 1st element of the pair.
  }
  \item{y}{
    a vector, the 2nd element of the pair.
    Should have the same length as \code{x}.
  }
}

\value{
   A 2-column matrix of class \code{"Pair"}.
}
\note{
   Mostly designed as part of the formula interface to paired tests.
}

\seealso{
  \code{\link{t.test}} and \code{\link{wilcox.test}}
}
