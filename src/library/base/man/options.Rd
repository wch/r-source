\name{options}
\title{Options Settings}
\usage{
options(\dots)
.Options
}
\alias{options}
\alias{.Options}
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
\item{browser}{default HTML browser used by \code{\link{help.start}()}.}
\item{contrasts}{the default \code{\link{contrasts}} used in model
    fitting such as with \code{\link{aov}} or \code{\link{lm}}.}
\item{expressions}{sets a limit on the number of nested expressions that
  will be evaluated.   This is especially important on the Macintosh
  since stack overflow is likely if this is set too high.}
\item{na.action}{the name of a function for treating missing values
  (\code{\link{NA}}'s) for certain situations.}
\item{pager}{the (standalone) program used for displaying ASCII files on
  \R's console.  Defaults to \file{\$R\_HOME/bin/pager}.}
\item{papersize}{the paper format used for graphics printing; currently
  \bold{read-only}, set by environment variable \code{R_PAPERSIZE}, or in
  \file{config.site}.}
\item{printcmd}{the command used for graphics printing; currently
  \bold{read-only}, set by environment variable \code{R_PRINTCMD}, or in
  \file{config.site}.}
\item{show.signif.stars, show.coef.Pvalues}{logical, affecting P value
    printing, see \code{\link{print.coefmat}}.}
\item{ts.eps}{the relative tolerance for certain time series
  (\code{\link{ts}}) computations.}
\item{warn}{sets the handling of warning messages. If \code{warn}
is negative all warnings are ignored. If \code{warn} is zero (the default)
warnings are stored until the  top--level function returns. If fewer than 10
warnings were signalled they will be printed otherwise a message
saying how many (max 50) were signalled. A top--level variable called
\code{last.warning} is created and can be viewed through the function
\code{\link{warnings}}. If \code{warn} is one, warnings are printed
as they occur. If \code{warn} is two or larger all warnings are turned
into errors.}
}
\description{
  \code{options} allows the user to set and examine a variety of global
  ``options'' which affect the way in which \R computes and displays
  its results.
}
\details{
  Invoking \code{options()} with no arguments returns a list with the
  current values of the options.
  \code{.Options} is another way to access this list \bold{``read only''}.

  Accessing current options, usually will work with \code{.Options$width}
  (e.g.), rather than \code{options("width")} which is a \emph{list} of
  length one.
}
\value{
  A list (in any case) with the previous values of the options changed,
  or all options when no arguments were given.
}
\examples{
options() # printing all current options
op <- options(); str(op) # nicer printing

# .Options is the same:
all(sapply(1:length(op), function(i) all(.Options[[i]] == op[[i]])))

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
\keyword{print}
