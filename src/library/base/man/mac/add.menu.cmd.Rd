\name{add.menu.cmd}
\alias{add.menu.cmd}
\title{Adds a user menu item to the User's menu}
\description{
  This command gives you the opportunity to build a User's Menu in
  R Console menubar on the Macintosh port of R.
  The User's menu is created dynamically only when the user adds a 
  menu to it. The new menu is positioned at the rightend of the
  menu bar (before the Help Menu). A maximum of 99 menu items
  can be added to the User's menu.
}
\usage{
add.menu.cmd(label,command)
}
\arguments{
  \item{label}{the string characterizing the menu entry in the User's menu.
   If the User's menu does not exist, the \R will create it first and then
   add the menu item. If an item with the same label is already present
   in the User's menu, then \R simply substitute the command associated to
   it.}
  \item{command}{is a string containing an \R command. If the this argument
   is not passed to the routine, then \R cannot add the menu item. This
   routine does not verify if the command you pass to this parameter is
   a correct \R command. See the example below.}
}
\seealso{
  \code{\link{get.menu.cmd},\link{del.menu.cmd}}.
}
\examples{
add.menu.cmd("Show Dir","dir()")

# take care of putting '\"' when you need to
# include quotes in the \R command
add.menu.cmd("Show tests dir","dir(\":tests\")")
}
\keyword{interface}

