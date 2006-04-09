\name{tkpager}
\alias{tkpager}
\title{Page file using Tk text widget}
\description{
  This plugs into \code{file.show}, showing files in separate windows. 
}
\usage{
tkpager(file, header, title, delete.file)
}
\arguments{
  \item{file}{character vector containing the names of the
          files to be displayed}
  \item{header}{headers to use for each file}
  \item{title}{common title to use for the window(s). Pasted together
  with the \code{header} to form actual window title.}
  \item{delete.file}{logical. Should file(s) be deleted after display?}
}
\note{  The \code{"\b_"} string used for underlining is currently
  quietly removed. The font and background colour are currently
  hardcoded to Courier and gray90.
}
\seealso{
  \code{\link{file.show}}
}
\keyword{misc}
