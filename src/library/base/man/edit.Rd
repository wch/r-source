\name{edit}
\title{Invoke a Text Editor}
\usage{
edit(name=NULL, file="", editor = "vi")
vi(name=NULL, file="")
emacs(name=NULL, file="")
pico(name=NULL, file="")
xemacs(name=NULL, file="")
xedit(name=NULL, file="")
}
\alias{edit}
\alias{vi}
\alias{emacs}
\alias{pico}
\alias{xemacs}
\alias{xedit}
\arguments{
\item{name}{a named object that you want to edit. If name is missing then the
file specified by \code{file} is opened for editing.}
\item{file}{a string naming the file to write the edited version to.}
\item{editor}{a string naming the text editor you want to use.}
}
\description{
\code{edit} invokes the text editor specified by \code{editor} with the object
\code{name} to be edited.
\code{data.entry} can be used to edit data.
It is important to realize that \code{edit} does not change
the object called \code{name}.
Instead, a copy of name is made and it is that copy which is changed.
Should you want the changes to apply to the object \code{name}
you must assign the result of \code{edit} to \code{name}.

In the form \code{edit(name)},
\code{edit} deparses \code{name} into a temporary file and invokes the
editor \code{editor} on this file. Quiting from the editor causes
\code{file} to be parsed and that value returned.
Should an error occur in parsing, possibly due to incorrect syntax, no
value is returned. Calling \code{edit()}, with no arguments, will
result in the temporary file being reopened for further editing.
}
\seealso{
\code{\link{data.entry}}, \code{fix}.
}
\examples{
# use xedit on the function mean and assign the changes
mean <- edit(mean, editor="xedit")

# use vi on mean and write the result to file mean.out
vi(mean, file="mean.out")
}
\keyword{utilities}
