
\name{savehistory}
\alias{loadhistory}
\alias{savehistory}
\alias{history}
\title{Load or Save or Display the Commands History}
\description{
  Load or save or display the commands history.
}
\usage{
loadhistory(file = ".Rhistory")
savehistory(file = ".Rhistory")
history(max.show = 25, reverse = FALSE)
}
\arguments{
  \item{file}{The name of the file in which to save the history. The path
    is relative to the current working directory.}
  \item{max.show}{The maximum nmber of lines to show. \code{Inf} will
    give all of the currently available history.}
  \item{reverse}{logical. If true, the lines are shown in reverse
    order. Note: this is not useful when there are continuation lines.}
}
\details{
#ifdef windows
  This works in Rgui and Rterm but not in embedded/DCOM versions.
#endif
#ifdef unix
  This works under the \code{readline} and GNOME interfaces, but not if
  \code{readline} is not available.
#endif
}
\keyword{utilities}
