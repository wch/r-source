\name{menu}
\title{Menu Interaction Function}
\usage{
menu(x)
}
\alias{menu}
\arguments{
  \item{x}{a character vector of choices}
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
