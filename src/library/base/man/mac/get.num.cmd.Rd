\name{get.num.cmd}
\alias{get.num.cmd}
\title{Returns the command associated to a user menu item from the User's menu}
\description{
  This command returns the \R command associated to an item from the User's Menu.
}
\usage{
get.num.cmd(menunum)
}
\arguments{
  \item{menunum}{the menu number characterizing the menu entry in the User's menu.
   If \R finds this entry in the User's menu, then it will return the \R
   command associated to it and the menu item.}
}
\seealso{
  \code{\link{get.menu.cmd},\link{add.menu.cmd}}.
}
\examples{
# we create 3 menus
add.menu.cmd("Menu1","ls()")
add.menu.cmd("Menu2","dir()")
add.menu.cmd("Menu3","library()")

# we get the second one
get.menu.cmd("Menu2")
# should have returned
# [1] "dir()" "Menu2"
}
\keyword{interface}

