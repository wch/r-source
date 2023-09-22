% File src/library/utils/man/chooseCRANmirror.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2022 R Core Team
% Distributed under GPL 2 or later

\name{findCRANmirror}
\alias{findCRANmirror}
\title{Find CRAN Mirror Preference}
\description{
  Find out if a CRAN mirror has been selected for the current session.
}
\usage{
findCRANmirror(type = c("src", "web"))
}
\arguments{
  \item{type}{Is the mirror to be used for package sources or web information?}
}
\details{
  Find out if a CRAN mirror has been selected for the current session.
  If so, return its URL else return \samp{"https://CRAN.R-project.org"}.

  The mirror is looked for in several places.
  \itemize{
    \item The value of the environment variable \env{R_CRAN_SRC} or
    \env{R_CRAN_WEB} (depending on \code{type}), if set.
    \item An entry in \code{getOption("repos")} named \samp{CRAN} which
    is not the default \samp{"@CRAN@")}.
    \item The \samp{CRAN} URL entry in the \file{repositories} file (see
    \code{\link{setRepositories}}), if it is not the default \samp{"@CRAN@"}.
  }

  The two types allow for partial local CRAN mirrors, for example those
  mirroring only the package sources where \code{getOption("repos")}
  might point to the partial mirror and \env{R_CRAN_WEB} point to
  a full (remote) mirror.
}
\value{
  A character string.
}
\seealso{
  \code{\link{setRepositories}}, \code{\link{chooseCRANmirror}}
}
\examples{
\dontshow{old <- Sys.getenv("R_CRAN_WEB", NA_character_)}
c(findCRANmirror("src"), findCRANmirror("web"))

Sys.setenv(R_CRAN_WEB = "https://cloud.r-project.org")
c(findCRANmirror("src"), findCRANmirror("web"))
\dontshow{if(is.na(old)) Sys.setenv(R_CRAN_WEB = old) else Sys.unsetenv("R_CRAN_WEB")}
}
\keyword{ utilities }
