\name{dataentry}
\title{Spreadsheet Interface for Entering Data}
\usage{
data.entry(\dots, Modes = NULL, Names = NULL)
dataentry(data, modes)
de(\dots, Modes = NULL, Names = NULL)
}
\alias{data.entry}
\alias{dataentry}
\alias{de}
\alias{de.ncols}
\alias{de.restore}
\alias{de.setup}
\description{
  This is a suite of functions designed to make the interface
  to the spreadsheet painless for users.
}
\details{
  \code{data.entry} has side
  effects, any changes made in the spreadsheet are reflected in the
  variables.  The functions \code{de}, \code{de.ncols}, \code{de.setup}
  and \code{de.restore} are designed to help achieve these side effects.
  If the user passes in a matrix, \code{X} say, then the matrix is
  broken into columns before \code{dataentry} is called. Then on return
  the columns are collected and glued back together and the result
  assigned to the variable \code{X}.  If you don't want this behaviour
  use dataentry directly.

  The primitive function is \code{dataentry}. It takes a list of
  vectors of possibly different lengths and modes (the second argument)
  and opens a spreadsheet with these variables being the columns.
  The columns of the dataentry window are returned as vectors in a
  list when the spreadsheet is closed.

  \code{de.ncols} counts the number of columns which are supplied as arguments
  to \code{data.entry}. It attempts to count columns in lists, matrices
  and vectors.  \code{de.setup} sets things up so that on return the
  columns can be regrouped and reassigned to the correct name. This
  is handled by \code{de.restore}.
}
\note{
  The details of interface to the data grid may differ by platform and
  GUI.  The following description applies to the X11-based
  implementation under Unix.

  You can navigate around the grid using the cursor keys or by clicking
  with the (left) mouse button on any cell.  The active cell is
  highlighted by thickening the surrounding rectangle.  Moving to the
  right or down will scroll the grid as needed: there is no constraint
  to the rows or columns currently in use.

  The are alternative ways to navigate using the keys. Return and
  (keypad) Enter and LineFeed all move down. Tab moves left. Home moves
  to the top left.  Control-F moves down a page, and Control-B up by a
  page.

  Using any other key starts an editing process on the currently
  selected cell: moving away from that cell enters the edited value
  whereas Esc cancels the edit and restores the previous value.  When
  the editing process starts the cell is cleared.  In numerical columns
  (the default) only letters making up a valid number (including
  \code{-.eE}) are accepted, and entering an invalid edited value (such
  as blank) enters \code{NA} in that cell.  The last entered value can
  be deleted using the  BackSpace or Del(ete) key.  Only a limited
  number of characters (currently 29) can be entered in cell, and if
  necessary only the start or end of the string will be displayed, with the
  omissions indicated by \code{>} or \code{<}.

  Entering a value in a cell further down a column than the last used
  cell extends the variable and fills the gap (if any) by \code{NA} (not
  shown on screen).

  The column names can only be selected by clicking in them.  This gives
  a popup-menu to select the column type (currently Real (numeric) or
  Character) or to change the name.  Changing the type converts the
  current contents of the column (and converting from Character to Real
  may generate \code{NA}s.)  If changing the name is selected the
  header cell becomes editable (and is cleared).  As with all cells, the
  value is entered by moving away from the cell by clicking elsewhere or
  by any of the keys for moving down (only).

  New columns are created by entering values in them (and not by just
  assigning a new name).  A new column defaults to numeric with the name
  of the form \code{var7} displayed.  Unused columns are ignored, so
  adding data in \code{var5} to a three-column grid adds one extra
  variable, not two.
}
\seealso{
  \code{\link{vi}}, \code{\link{edit}}.
}
\examples{
# call data entry with variables x and y
\dontrun{data.entry(x,y)}
}
\keyword{utilities}
\keyword{file}
