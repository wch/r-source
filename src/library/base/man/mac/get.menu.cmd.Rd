\name{get.menu.cmd}
\alias{get.menu.cmd}
\title{Returns the command associated to a user menu item from the User's menu}
\description{
  This command returns the \R command associated to an item from the User's Menu.
}
\usage{
get.menu.cmd(label)
}
\arguments{
  \item{label}{the string characterizing the menu entry in the User's menu.
   If \R finds this entry in the User's menu, then it will return the \R
   command associated to it and the corresponding menu number.
  The matching is not case sensitive.}
}
\seealso{
  \code{\link{add.menu.cmd},\link{del.menu.cmd},\link{get.num.cmd}}.
}
\examples{
# we create 3 menus
add.menu.cmd("Menu1","ls()")
add.menu.cmd("Menu2","dir()")
add.menu.cmd("Menu3","library()")

# we get the second menu item and command
get.menu.cmd("Menu2")
# should have returned
# dir() 
#    2 
}
\keyword{interface}

