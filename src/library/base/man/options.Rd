\name{options}
\title{Options Settings}
\usage{
options(digits=, expressions=, width=, prompt=,
        continuation=, editor=)
}
\alias{options}
\description{
This function allows user to set and examine a variety of global
``options'' which affect the way in which \R computes and displays
its results.
\code{digits} controls the number of digits to print when printing
numeric values. It is a suggestion only.  \code{expressions} sets
a limit on the number of nested expressions that will be evaluated.
This is especially important on the Macintosh since stack overflow
is likely if this is set too high.

\code{width} controls the number of characters on a line. You may
want to change this if you resize the window that \R is running in.
\code{prompt} must be a quoted string. The expression inside the
quotes will be used for the prompt.  \code{continuation} must be a
quoted string. It sets the prompt used for lines which continue
over one line.  \code{editor} sets the default text editor.  Invoking
options returns a list with the current values of the options stored
in it.
}
\examples{
# print the current options
options()

# set the editor
options(editor="nedit")
}
