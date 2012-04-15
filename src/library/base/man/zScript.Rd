% File src/library/base/man/zScript.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2009 R Core Team
% Distributed under GPL 2 or later

\name{.Script}
\title{Scripting Language Interface}
\alias{.Script}
\description{
  Internal interface to run a script through its interpreter.
}
\usage{
.Script(interpreter, script, args, \dots)
}
\arguments{
  \item{interpreter}{a character string naming the interpreter for the
    script.}
  \item{script}{a character string with the base file name of the
    script, which must be located in the \file{\code{interpreter}}
    subdirectory of
#ifdef unix
    \file{\var{R_SHARE_DIR}} (normally \file{\var{\link{R_HOME}}/share}).
#endif
#ifdef windows
    \file{\var{\link{R_HOME}}/share}.
#endif
  }
  \item{args}{a character string giving the arguments to pass to the
    script.}
  \item{\dots}{further arguments to be passed to \code{\link{system}}
    when invoking the interpreter on the script.}
}
\note{This function is for R internal use only.}
\keyword{interface}
\keyword{internal}
