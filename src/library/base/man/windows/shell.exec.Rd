\name{shell.exec}
\alias{shell.exec}
\title{Open a File using Windows File Associations}
\usage{
shell.exec(file)
}
\arguments{
  \item{file}{file to be opened.}
}
\description{
  Opens the specified file using the application specified in the
  Windows file associations.
}
\details{
  It is likely that \code{file} needs to be a complete path, e.g.
  \file{"c:/R/results.html"} or \file{"c:\\R\\results.html"} as most
  applications interpret files relative to \emph{their} working directory.
}
\value{
  No value.
}
\author{B. D. Ripley}

\seealso{\code{\link{system}}, \code{\link{shell}}}

\keyword{utilities}
