\name{grid.newpage}
\alias{grid.newpage}
\title{Move to a New Page on a Grid Device}
\description{
  This function erases the current device or moves to a new page.
}
\usage{
grid.newpage(recording = TRUE)
}
\arguments{
  \item{recording}{A logical value to indicate whether the new-page
    operation should be saved onto the Grid display list.}
}
\details{
  There is a hook called \code{"grid..newpage"} (see \code{\link{setHook}})
  which is used in the testing code to annotate the new page.  The hook
  function(s) are called with no argument.  (If the value is a character
  string, \code{get} is called on it from within the \pkg{grid}
  namespace.)
}
\value{
  None.
}
\author{Paul Murrell}
\seealso{
  \link{Grid}
}
\keyword{dplot}
