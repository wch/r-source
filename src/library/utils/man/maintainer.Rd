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
  Show the name and email address of the maintainer of a package.
}
\usage{
maintainer(pkg)
}
\arguments{
   \item{pkg}{Character string.  The name of a single package.}
}
\details{
   Accesses the package description to return the name and email address of
   the maintainer.

   Questions about contributed packages should often be addressed to
   the package maintainer; questions about base packages should
   usually be addressed to the R-help or R-devel mailing lists.  Bug
   reports should be submitted using the \code{\link{bug.report}}
   function.
}
\value{
   A character string giving the name and email address of the maintainer
   of the package, or \code{NA} if no such package is installed.
}

\author{
 David Scott <d.scott@auckland.ac.nz> from code on R-help originally
 due to Charlie Sharpsteen <source@sharpsteen.net>; multiple corrections
 by R-core.
}
\references{
  \url{https://stat.ethz.ch/pipermail/r-help/2010-February/230027.html}
}
% failed 2011-07-07
% \references{
% \url{http://n4.nabble.com/R-help-question-How-can-we-enable-useRs-to-contribute-corrections-to-help-files-faster-tp1572568p1572868.html}
% }
\seealso{
\code{\link{packageDescription}}, \code{\link{bug.report}}
}
\examples{
maintainer("MASS")
}
\keyword{utilities}

