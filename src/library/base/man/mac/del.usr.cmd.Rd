\name{del.usr.cmd}
\alias{del.usr.cmd}
\title{Remove the whole User's menu}
\description{
  This command remove, if any, the User's menu from the \R Console menubar.
}
\usage{
del.usr.cmd()
}

\seealso{
  \code{\link{del.menu.cmd},\link{del.num.cmd}}.
}
\examples{
# we create 3 menus
add.menu.cmd("Menu1","ls()")
add.menu.cmd("Menu2","dir()")
add.menu.cmd("Menu3","library()")

# we remove the second one
del.num.cmd(2)
# we remove the whole menu
del.usr.cmd()
}
\keyword{interface}

