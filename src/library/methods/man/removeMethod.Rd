% File src/library/methods/man/setMethod.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{removeMethod}
\alias{removeMethod}
\title{ Remove a Method }
\description{
  Remove the method for a given function and signature.  Obsolete for
  ordinary applications: Method definitions in a package should never
  need to remove methods and it's very bad practice to remove methods
  that were defined in other packages.
}
\usage{
removeMethod(f, signature, where)
}
\arguments{
  \item{f, signature, where}{  As for \code{\link{setMethod}()}.
}
}
\value{
   \code{TRUE} if a method
  was found to be removed.
}

\references{
 Chambers, John M. (2016)
 \emph{Extending R},
  Chapman & Hall.
(Chapters 9 and 10.)
}


\keyword{programming}
\keyword{classes}
\keyword{methods}
