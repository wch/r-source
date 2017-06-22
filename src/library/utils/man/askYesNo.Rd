% File src/library/utils/man/askYesNo.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2017 R Core Team
% Distributed under GPL 2 or later

\name{askYesNo}
\alias{askYesNo}
\title{
Ask a Yes/No Question
}
\description{
\code{askYesNo} provides a standard way to ask the user a yes/no question.  
It provides a way for front-ends to substitute their own dialogs.
}
\usage{
askYesNo(msg, default = TRUE, 
         prompts = getOption("askYesNo", gettext(c("Yes", "No", "Cancel"))), 
         ...)
}
\arguments{
  \item{msg}{
The prompt message for the user.
}
  \item{default}{
The default response.
}
  \item{prompts}{
Any of:  a character vector containing 3 prompts corresponding to
return values of \code{TRUE}, \code{FALSE}, or \code{NA}, or
a single character value containing the prompts separated by
\code{/} characters, or a function to call.
}
  \item{\dots}{
Additional parameters, ignored by the default function.
}
}
\details{
\code{askYesNo} will accept case-independent partial matches to the prompts.  
If no response
is given the value of \code{default} will be returned; if a non-empty
string that doesn't match any of the prompts is entered, an error will be 
raised. 

If a function or single character string naming a function
is given for \code{prompts}, it will be called as
\code{fn(msg = msg, default = default, prompts = prompts, ...)}.  On
Windows, the GUI uses the unexported \code{utils:::askYesNoWinDialog}
function for this purpose.

If strings (or a string such as \code{"Y/N/C"}) are given as \code{prompts}, 
the choices will be mapped to lowercase for the non-default choices, and 
left as-is for the default choice.
}
\value{
\code{TRUE} for yes, \code{FALSE} for no, and \code{NA} for cancel.   
}
\seealso{
\code{readline} for more general user input.
}
\examples{
if (interactive())
    askYesNo("Do you want to use askYesNo?")
}
\keyword{utilities}
