\name{options}
\title{Options Settings}
\usage{
options(\dots)
}
\alias{options}
\arguments{
\item{\dots}{any options can be defined, using \code{name = value}.
  However, only the ones below are used in ``base \R''.

  Further, \code{options('name') == options()['name']}, see the example.
}
\item{prompt}{a string, used for \R's prompt; should usually end in a
  blank (\code{" "}).}
\item{continue}{a string setting the prompt used for lines which continue
  over one line.}
\item{width}{controls the number of characters on a line. You may
  want to change this if you resize the window that \R is running in.}
\item{digits}{controls the number of digits to print when printing
  numeric values. It is a suggestion only.}
\item{editor}{sets the default text editor, e.g., for \code{\link{edit}}.}
\item{contrasts}{The default contrasts used in model fitting such as
  \code{\link{lm}}.}
\item{expressions}{sets a limit on the number of nested expressions that
  will be evaluated.   This is especially important on the Macintosh
  since stack overflow is likely if this is set too high.}
\item{na.action}{the name of a function for treating missing values
  (\code{\link{NA}}'s) for certain situations.}
\item{papersize}{the paper format used for graphics printing; currently
  \bold{read-only}, set by environment variable R\_PAPERSIZE, or in
  \file{config.site}.}
\item{printcmd}{the command used for graphics printing; currently
  \bold{read-only}, set by environment variable R\_PRINTCMD, or in
  \file{config.site}.}
\item{ts.eps}{the relative tolerance for certain time series
  (\code{\link{ts}}) computations.}
}
\description{
  This function allows the user to set and examine a variety of global
  ``options'' which affect the way in which \R computes and displays
  its results.

  Invoking \code{options()} with no arguments returns a list with the
  current values of the options.
}
\value{
  A list (in any case) with the previous values of the options changed,
  or all options when no arguments were given.
}
\examples{
options()		 # printing all current options
op <- options(); str(op) # nicer printing

options('width')[[1]] == options()$width # the latter needs more memory
options(digits=20)
pi

# set the editor, and save previous value
old.o <- options(editor="nedit")
old.o

options(op)	# reset (all) initial options
options('digits')
}
\keyword{environment}
%later, when we'll have an error handler : \keyword{error}
\keyword{print}%-- even though it's not working ...
