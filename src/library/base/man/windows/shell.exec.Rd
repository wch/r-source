\name{shell.exec}
\alias{shell.exec}
\title{Open a File or URL using Windows File Associations}
\usage{
shell.exec(file)
}
\arguments{
  \item{file}{file or URL to be opened.}
}
\description{
  Opens the specified file or URL using the application specified in the
  Windows file associations.
}
\details{
  It is likely that \code{file} needs to be a complete path, e.g.
  \file{"c:/R/results.html"} or \file{"c:\\R\\results.html"} as most
  applications interpret files relative to \emph{their} working
  directory.

  Encoded \code{file://} URLs are not usually accepted.
}
\value{
  No value, but informative error messages will be given if the
  operation fails.
}
\author{B. D. Ripley}

\seealso{\code{\link{system}}, \code{\link{shell}}}

\examples{\dontrun{
## the space should not be encoded here
shell.exec("C:\\Program Files\\BreezeSys\\BreezeBrowser\\Breezebrowser.htm")
shell.exec("C:/Program Files/BreezeSys/BreezeBrowser/Breezebrowser.htm")
shell.exec("file://C:/Program Files/BreezeSys/BreezeBrowser/Breezebrowser.htm")
}}

\keyword{utilities}
