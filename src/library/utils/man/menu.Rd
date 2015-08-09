% File src/library/utils/man/menu.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{menu}
\alias{menu}
\title{Menu Interaction Function}
\description{
  \code{menu} presents the user with a menu of choices labelled from 1
  to the number of choices.  To exit without choosing an item one can
  select \samp{0}.
}
\usage{
menu(choices, graphics = FALSE, title = NULL)
}
\arguments{
  \item{choices}{a character vector of choices}
  \item{graphics}{a logical indicating whether a graphics menu should be
    used if available.}
  \item{title}{a character string to be used as the title of the menu.
    \code{NULL} is also accepted.}
}
\details{
  If \code{graphics = TRUE} and a windowing system is available
  (Windows, OS X or X11 \emph{via} Tcl/Tk) a listbox widget is
  used, otherwise a text menu.  It is an error to use \code{menu} in a
  non-interactive session.

  Ten or fewer items will be displayed in a single column, more in
  multiple columns if possible within the current display width.

  No title is displayed if \code{title} is \code{NULL} or \code{""}.
}
\value{
  The number corresponding to the selected item, or 0 if no choice was
  made.
}
\seealso{
  \code{\link{select.list}}, which is used to implement the graphical
  menu, and allows multiple selections.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\examples{
\dontrun{
switch(menu(c("List letters", "List LETTERS")) + 1,
       cat("Nothing done\n"), letters, LETTERS)
}
}
\keyword{utilities}
\keyword{programming}
