\name{menu}
\title{Menu Interaction Function}
\usage{
menu(x, graphics = FALSE, title = "")
}
\alias{menu}
\arguments{
  \item{x}{a character vector of choices}
  \item{graphics}{a logical indicating whether a graphics menu should be
    used.  Currently unused.}
  \item{title}{a character string to be used as the title of the menu}
}
\description{
  \code{menu} presents the user with a menu of choices labelled from 1
  to the number of choices.  To exit without choosing an item one can
  select `0'.
}
\value{
  The number corresponding to the selected item, or 0 if no choice was
  made.
}
\examples{
switch(menu(c("List letters", "List LETTERS")) + 1,
       cat("Nothing done\n"), letters, LETTERS)
}
\keyword{utilities}
\keyword{programming}
