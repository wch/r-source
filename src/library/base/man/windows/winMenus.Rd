\name{winMenus}
\alias{winMenuAdd}
\alias{winMenuAddItem}
\alias{winMenuDel}
\alias{winMenuDelItem}
\title{User Menus under Windows}
\description{
  Enables users to add, delete and program menus under Windows.
}
\usage{
winMenuAdd(menuname)
winMenuAddItem(menuname, itemname, action)
winMenuDel(menuname)
winMenuDelItem(menuname, itemname)
}
\arguments{
  \item{menuname}{a character string naming a menu.}
  \item{itemname}{a character string naming a menu item on an existing menu.}
  \item{action}{a character string describing the action when that menu
    is selected, or \code{"enable"} or \code{"disable"}.}
}
\details{
  User menus are added to the right of existing menus, and items are
  added at the bottom of the menu.

  By default the action character string is treated as \R input, being
  echoed on the command line and parsed and executed as usual.

  Specifying an existing item in \code{winMenuAddItem} enables the
  action to be changed.

  Submenus can be specified by separating the elements in
  \code{menuname} by slashes: as a consequence menu names may not
  contain slashes.

  If the \code{action} is specified as \code{"none"} no action is taken:
  this can be useful to reserve items for future expansion.
}
\value{
  \code{NULL}, invisibly. If an error occurs, an informative error
  message will be given.
}
\seealso{\code{\link{winDialog}}}
\examples{\dontrun{
winMenuAdd("Testit")
winMenuAddItem("Testit", "one", "aaaa")
winMenuAddItem("Testit", "two", "bbbb")
winMenuAdd("Testit/extras")
winMenuAddItem("Testit", "-", "")
winMenuAddItem("Testit", "two", "disable")
winMenuAddItem("Testit", "three", "cccc")
winMenuAddItem("Testit/extras", "one more", "ddd")
winMenuAddItem("Testit/extras", "and another", "eee")
}}
\keyword{utilities}
