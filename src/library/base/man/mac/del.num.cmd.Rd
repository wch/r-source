\name{del.num.cmd}
\alias{del.num.cmd}
\title{Removes a user menu item from the User's menu}
\description{
  This command remove an entry from the User's Menu.
}
\usage{
del.menu.cmd(menunum)
}
\arguments{
  \item{menunum}{the number characterizing the menu entry in the User's menu.
   If \R finds this entry in the User's menu, then it will remove it from the
   menu. If the item is the last item in 
   the User's menu, the whole menu will be suppressed.}
}
\seealso{
  \code{\link{del.menu.cmd},\link{del.usr.cmd}}.
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

