% File src/library/utils/man/dataentry.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2009 R Core Team
% Distributed under GPL 2 or later

\name{dataentry}
\title{Spreadsheet Interface for Entering Data}
\usage{
data.entry(\dots, Modes = NULL, Names = NULL)
dataentry(data, modes)
de(\dots, Modes = list(), Names = NULL)
}
\alias{data.entry}
\alias{dataentry}
\alias{de}
\alias{de.ncols}
\alias{de.restore}
\alias{de.setup}
\description{
  A spreadsheet-like editor for entering or editing data.
}
\arguments{
  \item{\dots}{A list of variables: currently these should be numeric or
    character vectors or list containing such vectors.}
  \item{Modes}{The modes to be used for the variables.}
  \item{Names}{The names to be used for the variables.}
  \item{data}{A list of numeric and/or character vectors.}
  \item{modes}{A list of length up to that of \code{data} giving the
    modes of (some of) the variables. \code{list()} is allowed.}
}
\details{
  The data entry editor is only available on some platforms and GUIs.
  Where available it provides a means to visually edit a matrix or
  a collection of variables (including a data frame) as described in the
  Notes section.

  \code{data.entry} has side effects, any changes made in the
  spreadsheet are reflected in the variables.  The functions \code{de},
  \code{de.ncols}, \code{de.setup} and \code{de.restore} are designed to
  help achieve these side effects. If the user passes in a matrix,
  \code{X} say, then the matrix is broken into columns before
  \code{dataentry} is called. Then on return the columns are collected
  and glued back together and the result assigned to the variable
  \code{X}.  If you don't want this behaviour use dataentry directly.

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
\value{
  \code{de} and \code{dataentry} return the edited value of their
  arguments. \code{data.entry} invisibly returns a vector of variable
  names but its main value is its side effect of assigning new version
  of those variables in the user's workspace.
}
\note{
  The details of interface to the data grid may differ by platform and
  GUI.  The following description applies to
#ifdef unix
  the X11-based implementation under Unix.
#endif
#ifdef windows
  the GraphApp-based implementation under Windows.
#endif

  You can navigate around the grid using the cursor keys or by clicking
  with the (left) mouse button on any cell.  The active cell is
  highlighted by thickening the surrounding rectangle.  Moving to the
  right or down will scroll the grid as needed: there is no constraint
  to the rows or columns currently in use.

  There are alternative ways to navigate using the keys.  Return and
  (keypad) Enter and LineFeed all move down. Tab moves right and
  Shift-Tab move left.  Home moves to the top left.

  PageDown or Control-F moves down a page, and PageUp or
  Control-B up by a page.  End will show the last used column and the
  last few rows used (in any column).

  Using any other key starts an editing process on the currently
  selected cell: moving away from that cell enters the edited value
  whereas Esc cancels the edit and restores the previous value.  When
  the editing process starts the cell is cleared.
#ifdef windows
  The cursor changes to an I-beam to indicate that the cell is in enter mode.
#endif
  In numerical columns
  (the default) only letters making up a valid number (including
  \code{-.eE}) are accepted, and entering an invalid edited value (such
  as blank) enters \code{NA} in that cell.  The last entered value can
  be deleted using the  BackSpace or Del(ete) key.  Only a limited
  number of characters (currently 29) can be entered in a cell, and if
  necessary only the start or end of the string will be displayed, with the
  omissions indicated by \code{>} or \code{<}.  (The start is shown
  except when editing.)

#ifdef windows
  Double-clicking on a cell selects the cell and makes it into an
  editable field (a cursor will appear at the end of the text and it
  will change to the text highlight colour).  The edited text is
  entered by selecting another cell, for example by hitting Return.
  There is no way to cancel the edits.  The field will be expanded to
  the right if necessary to accommodate existing long strings, so it is
  preferable not to edit in the right-most displayed column.  (The
  editable field is itself scrollable.)
#endif

  Entering a value in a cell further down a column than the last used
  cell extends the variable and fills the gap (if any) by \code{NA}s (not
  shown on screen).

  The column names can only be selected by clicking in them.  This gives
  a popup menu to select the column type (currently Real (numeric) or
  Character) or to change the name.  Changing the type converts the
  current contents of the column (and converting from Character to Real
  may generate \code{NA}s.)
#ifdef unix
  If changing the name is selected the
  header cell becomes editable (and is cleared).  As with all cells, the
  value is entered by moving away from the cell by clicking elsewhere or
  by any of the keys for moving down (only).
#endif
#ifdef windows
  Enter the changes made in the popup window by clicking on its close box.
#endif

  New columns are created by entering values in them (and not by just
  assigning a new name).  The mode of the column is auto-detected from
  the first value entered: if this is a valid number it gives a numeric
  column.  Unused columns are ignored, so
  adding data in \code{var5} to a three-column grid adds one extra
  variable, not two.

#ifdef windows
  There is a popup-menu accessed by right-clicking anywhere in the window
  that refers to the currently selected cell. This can copy the value to
  or paste from the clipboard, or paste in common values in that column.
  Copying and pasting can also be accessed by the usual keyboard shortcuts
  Control-C and Control-V.

  Columns can be resized by selecting and dragging a line (the cursor
  will change) within limits: columns must be between 4 and 50 chars wide.
  The Autosize item on the popup menu will resize the currently selected
  column.
#endif
#ifdef unix
  The \code{Copy} button copies the currently selected cell:
  \code{paste} copies the last copied value to the current cell, and
  right-clicking selects a cell \emph{and} copies in the value.
  Initially the value is blank, and attempts to paste a blank value will
  have no effect.
#endif

  Control-L will refresh the display, recalculating field widths to fit
  the current entries.

  In the default mode the column widths are chosen to fit the contents
  of each column, with a default of 10 characters for empty columns.
  you can specify fixed column widths by setting option
  \code{de.cellwidth} to the required fixed width (in characters).
  (set it to zero to return to variable widths).  The displayed
  width of any field is limited to
#ifdef unix
  600 pixels (and by the window width).
#endif
#ifdef windows
  50 characters (and by the window width).

  The initial size of the data editor window is taken from the default
  dimensions of a pager (see \code{\link{Rconsole}}), but adjusted
  downwards to show a whole number of rows and columns.
#endif
}
#ifdef unix
\section{Resources}{
  The data entry window responds to X resources of class
  \code{R_dataentry}.  Resources \code{foreground}, \code{background} and
  \code{geometry} are utilized.
}
#endif
\seealso{
  \code{\link{vi}}, \code{\link{edit}}: \code{edit} uses
  \code{dataentry} to edit data frames.
}
\examples{
# call data entry with variables x and y
\dontrun{data.entry(x, y)}
}
\keyword{utilities}
\keyword{file}
