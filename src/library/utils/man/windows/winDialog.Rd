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
  \item{type}{character. The type of dialog box. It will have the
    buttons implied by its name.}
  \item{message}{character. The information field of the dialog box.}
  \item{default}{character. The default string.}
}
\value{
  For \code{winDialog} a character string giving the name of the button
  pressed (in capitals) or \code{NULL} (invisibly) if the user had no
  choice.

  For \code{winDialogString} a string giving the contents of the text
  box when \code{Ok} was pressed, or \code{NULL} if \code{Cancel} was pressed.
}
\note{
  The standard keyboard accelerators work with these dialog boxes:
  where appropriate \code{Return} accepts the default action,
  \code{Esc} cancels and the underlined initial letter (\code{Y} or
  \code{N}) can be used.
}
\seealso{\code{\link{winMenuAdd}}\cr
  \code{\link{file.choose}} to select a file\cr
  package \code{windlgs} in the package source distribution for ways to
  program dialogs in C in the \code{GraphApp} toolkit.}

\examples{
\dontrun{winDialog("yesno", "Is it OK to delete file blah")}
}
\keyword{utilities}
