\name{del.menu.cmd}
\alias{del.menu.cmd}
\title{Removes a user menu item from the User's menu}
\description{
  This command remove an entry from the User's Menu.
}
\usage{
del.menu.cmd(label)
}
\arguments{
  \item{label}{the string characterizing the menu entry in the User's menu.
   If \R finds this entry in the User's menu, then it will remove it from the
   menu. The matching is not case sensitive. If the item is the last item in 
   the User's menu, the whole menu will be suppressed.}
}
\seealso{
  \code{\link{add.menu.cmd},\link{del.num.cmd}}.
}
\examples{
# we create 3 menus
add.menu.cmd("Menu1","dir()")
add.menu.cmd("Menu2","dir()")
add.menu.cmd("Menu3","dir()")

# we remove the second one
del.menu.cmd("Menu2")

# take care of the case, the first menu will be also removed
del.menu.cmd("menu1")
}
\keyword{interface}

