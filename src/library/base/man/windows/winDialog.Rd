\name{winDialog}
\alias{winDialog}
\alias{winDialogString}
\title{Dialog Boxes under Windows}
\description{
  Put up a Windows dialog box to communicate with the user. There are
  various types, either for the user to select from a set of buttons or
  to edit a string.
}
\usage{
winDialog(type = c("ok", "okcancel", "yesno", "yesnocancel"), message)
winDialogString(message, default)
}
\arguments{
  \item{type}{The type of dialog box. It will have the buttons implied
    by its name.}
  \item{message}{The information field of the dialog box.}
  \item{default}{The default string.}
}
}
\value{
  For \code{winDialog} a character string giving the name of the button
  pressed (in capitals) or \code{NULL} (invisibly) if the user had no
  choice.

  For \code{winDialogString} a string giving the contents of the text
  box when \code{Ok} was pressed, or \code{NULL} if code{Cancel} was pressed.
}
\seealso{\code{\link{winMenus}}\cr
  \code{\link{file.choose}} to select a file.}

\examples{
\dontrun{winDialog("yesno", "Is it OK to delete file blah")}
}
\keyword{utilities}
