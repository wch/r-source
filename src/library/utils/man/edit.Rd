% File src/library/utils/man/edit.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{edit}
\alias{edit}
\alias{edit.default}
\alias{vi}
\alias{emacs}
\alias{pico}
\alias{xemacs}
\alias{xedit}
\title{Invoke a Text Editor}
\description{
  Invoke a text editor on an \R object.
}
\usage{
\method{edit}{default}(name = NULL, file = "", title = NULL,
     editor = getOption("editor"), \dots)

vi(name = NULL, file = "")
emacs(name = NULL, file = "")
pico(name = NULL, file = "")
xemacs(name = NULL, file = "")
xedit(name = NULL, file = "")
}
\arguments{
  \item{name}{a named object that you want to edit. If name is missing
    then the file specified by \code{file} is opened for editing.}
  \item{file}{a string naming the file to write the edited version to.}
  \item{title}{a display name for the object being edited.}
  \item{editor}{a string naming the text editor you want to use.  On
    Unix the default is set from the environment variables \env{EDITOR}
    or \env{VISUAL} if either is set, otherwise \code{vi} is used.  On
    Windows it defaults to \code{"internal"}, the script editor.  On the
    OS X GUI the argument is ignored and the document editor is
    always used.}
  \item{\dots}{further arguments to be passed to or from methods.}
}
\details{
  \code{edit} invokes the text editor specified by \code{editor} with
  the object \code{name} to be edited.  It is a generic function,
  currently with a default method and one for data frames and matrices.

  \code{data.entry} can be used to edit data, and is used by \code{edit}
  to edit matrices and data frames on systems for which
  \code{data.entry} is available.

  It is important to realize that \code{edit} does not change the object
  called \code{name}. Instead, a copy of name is made and it is that
  copy which is changed.  Should you want the changes to apply to the
  object \code{name} you must assign the result of \code{edit} to
  \code{name}.  (Try \code{\link{fix}} if you want to make permanent
  changes to an object.)

  In the form \code{edit(name)},
  \code{edit} deparses \code{name} into a temporary file and invokes the
  editor \code{editor} on this file. Quitting from the editor causes
  \code{file} to be parsed and that value returned.
  Should an error occur in parsing, possibly due to incorrect syntax, no
  value is returned. Calling \code{edit()}, with no arguments, will
  result in the temporary file being reopened for further editing.

  Note that deparsing is not perfect, and the object recreated after
  editing can differ in subtle ways from that deparsed: see
  \code{\link{dput}} and \code{\link{.deparseOpts}}. (The deparse options
  used are the same as the defaults for \code{dump}.)  Editing a
  function will preserve its environment.  See
  \code{\link{edit.data.frame}} for further changes that can occur when
  editing a data frame or matrix.

  Currently only the internal editor in Windows makes use of the
  \code{title} option; it displays the given name in the window
  header.
}
\seealso{
  \code{\link{edit.data.frame}},
  \code{\link{data.entry}},
  \code{\link{fix}}.
}
\note{
  The functions \code{vi}, \code{emacs}, \code{pico}, \code{xemacs},
  \code{xedit} rely on the corresponding editor being available and
  being on the path. This is system-dependent.
}
\examples{
\dontrun{
# use xedit on the function mean and assign the changes
mean <- edit(mean, editor = "xedit")

# use vi on mean and write the result to file mean.out
vi(mean, file = "mean.out")
}
}
\keyword{utilities}
