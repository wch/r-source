% File src/library/utils/man/windows/winDialog.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2009 R Core Team
% Distributed under GPL 2 or later

\name{tk_messageBox}
\alias{tk_messageBox}

\title{Tk Message Box}

\description{
  An implementation of a generic message box using Tk
}

\usage{
tk_messageBox(type = c("ok", "okcancel", "yesno", "yesnocancel",
                       "retrycancel", "aburtretrycancel"),
              message, caption = "", default = "", ...)
}
\arguments{
  \item{type}{character. The type of dialog box. It will have the
    buttons implied by its name.  Can be abbreviated.}
  \item{message}{character. The information field of the dialog box.}
  \item{caption}{the caption on the widget displayed.}
  \item{default}{character. The name of the button to be used as the
    default.}
  \item{\dots}{additional named arguments to be passed to the Tk
    function of this name.  An example is \code{icon = "warning"}.}
}
\value{
  A character string giving the name of the button pressed.
}

\seealso{
  \code{\link{tkmessageBox}} for a \sQuote{raw} interface.
}

\keyword{utilities}
