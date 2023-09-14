% File src/library/utils/man/maintainer.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2012 R Core Team
% Distributed under GPL 2 or later

\name{maintainer}
\alias{maintainer}

\title{
   Show Package Maintainer
}
\description{
  Show the name and email address of the maintainer of an installed package.
}
\usage{
maintainer(pkg)
}
\arguments{
   \item{pkg}{a character string, the name of an installed package.}
}
\details{
   Accesses the \link[=packageDescription]{package description}
   to return the name and email address of the maintainer.

   Questions about contributed packages should often be addressed to
   the package maintainer; questions about base packages should
   usually be addressed to the R-help or R-devel mailing lists.  Bug
   reports should be submitted using the \code{\link{bug.report}}
   function.
}
\value{
   A character string giving the name and email address of the maintainer
   of the package, or \code{NA_character_} if no such package is installed.
}

\author{
 David Scott \email{d.scott@auckland.ac.nz} from code on R-help originally
 due to Charlie Sharpsteen \email{source@sharpsteen.net}; multiple corrections
 by R-core.
}
\references{
  \url{https://stat.ethz.ch/pipermail/r-help/2010-February/230027.html}
}
\seealso{
\code{\link{packageDescription}}, \code{\link{bug.report}}
}
\examples{
maintainer("MASS")
}
\keyword{utilities}

