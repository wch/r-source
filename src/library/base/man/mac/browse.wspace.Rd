\name{browse.wspace}
\alias{browse.wspace}
\title{MacOS only Workspace Browser}
\description{
  The \code{browse.wspace} function opens a browser with list of object currently
  in \code{sys.frame()} environment.
}
\usage{
browse.wspace(html=FALSE, expanded=TRUE)
}
\arguments{
  \item{html}{is used on non Macintosh machines to display the workspace on
   a HTML page in your favorite browser.}
  \item{expanded}{whether to show one level of recursion. It can be useful
  to switch it to \code{FALSE} if you workspace has plenty of objects. This
  option is ignored if \code{html} is set to \code{FALSE}.}  
}

\details{
  Experimental code. Only allows one level of recursion into object structures.
  It can be easily generalized. See sources for details.
}
\seealso{
  \code{\link{ls}}.
}
\keyword{interface}

