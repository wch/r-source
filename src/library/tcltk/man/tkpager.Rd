\name{tkpager}
\alias{tkpager}
\title{Page file using Tk text widget}
\description{
  This plugs into \code{file.show}, showing files in separate windows. 
}
\usage{
tkpager(file, header, title, delete.file)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{file}{character vector containing the names of the
          files to be displayed}
  \item{header}{headers to use at the beginning of each file}
  \item{title}{title to use for the window}
  \item{delete.file}{logical. Should file(s) be deleted after display}
}
\note{  The \code{"\b_"} string used for underlining is currently
  quietly removed. The font and background colour are currently
  hardcoded to Courier and gray90.
}
\seealso{\code{\link{file.show}}}

\examples{
}
\keyword{ misc }%-- one or more ...
