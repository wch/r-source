\name{winMenus}
\alias{winMenuAdd}
\alias{winMenuAddItem}
\alias{winMenuDel}
\alias{winMenuDelItem}
\alias{winMenuNames}
\alias{winMenuItems}
\title{User Menus under Windows}
\description{
  Enables users to add, delete and program menus under Windows.
}
\usage{
winMenuAdd(menuname)
winMenuAddItem(menuname, itemname, action)
winMenuDel(menuname)
winMenuDelItem(menuname, itemname)
winMenuNames()
winMenuItems(menuname)
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

  If the \code{menuname} parameter of \code{winMenuAddItem} does not
  already exist, it will be created automatically.
  
  Normally new submenus and menu items are added to the main console
  menu. They may be added elsewhere using the following special names:
  \describe{
      \item{\code{$ConsoleMain}}{The console menu (the default)}
      \item{\code{$ConsolePopup}}{The console popup menu}
      \item{\code{$Graph<n>Main}}{The menu for graphics window \code{<n>}}
      \item{\code{$Graph<n>Popup}}{The popup menu for graphics window 
      \code{<n>}}
  }
  
  Specifying an existing item in \code{winMenuAddItem} enables the
  action to be changed.

  Submenus can be specified by separating the elements in
  \code{menuname} by slashes: as a consequence menu names may not
  contain slashes.

  If the \code{action} is specified as \code{"none"} no action is taken:
  this can be useful to reserve items for future expansion.

  The function \code{winMenuNames} can be used to find out what menus
  have been created by the user and returns a vector of the existing
  menu names.

  The \code{winMenuItems} function will take the name of a menu and
  return the items that exist in that menu.  The return value is a named
  vector where the names correspond to the names of the items and the
  values of the vector are the corresponding actions.
  
  The \code{winMenuDel} function will delete a menu and all of its items
  and submenus.  \code{winMenuDelItem} just deletes one menu item.

  The total path to an item (menu string plus item string) cannot exceed
  1000 bytes, and the menu string cannot exceed 500 bytes.
}
\value{
  \code{NULL}, invisibly.  If an error occurs, an informative error
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
winMenuAdd("$ConsolePopup/Testit")
winMenuAddItem("$ConsolePopup/Testit", "six", "fff")
winMenuNames()
winMenuItems("Testit")
}}
\keyword{utilities}
