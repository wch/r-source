\name{file.choose}
\alias{file.choose}
\title{Choose a File Interactively}
\description{
 Choose a file interactively.
}
\usage{
file.choose(new = FALSE)
}
\arguments{
 \item{new}{Logical: choose the style of dialog box
     presented to the user: at present only new = FALSE is used.}
}
\value{
  A character vector of length one giving the file path.
}

\seealso{\code{\link{list.files}} for non-interactive selection.
#ifdef windows    
\code{\link{choose.files}} for selecting multiple files interactively.
#endif
}
\keyword{file}
