% File src/library/datasets/man/Theoph.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2011 R Core Team
% Distributed under GPL 2 or later

\newcommand{\sspace}{\ifelse{latex}{\out{~}}{ }}

\name{Theoph}
\docType{data}
\alias{Theoph}
\title{Pharmacokinetics of Theophylline}
\description{
  The \code{Theoph} data frame has 132 rows and 5 columns of data from
  an experiment on the pharmacokinetics of theophylline.}
\usage{Theoph}
\format{
  An object of class
  \code{c("nfnGroupedData", "nfGroupedData", "groupedData", "data.frame")}
  containing the following columns:
  \describe{
    \item{Subject}{
      an ordered factor with levels \code{1}, \dots, \code{12}
      identifying the subject on whom the observation was made.  The
      ordering is by increasing maximum concentration of theophylline
      observed.
    }
    \item{Wt}{
      weight of the subject (kg).
    }
    \item{Dose}{
      dose of theophylline administered orally to the subject (mg/kg).
    }
    \item{Time}{
      time since drug administration when the sample was drawn (hr).
    }
    \item{conc}{
      theophylline concentration in the sample (mg/L).
    }
  }
}
\details{
  Boeckmann, Sheiner and Beal (1994) report data from a study by
  Dr. Robert Upton of the kinetics of the anti-asthmatic drug
  theophylline.  Twelve subjects were given oral doses of theophylline
  then serum concentrations were measured at 11 time points over the
  next 25 hours.

  These data are analyzed in Davidian and Giltinan (1995) and Pinheiro
  and Bates (2000) using a two-compartment open pharmacokinetic model,
  for which a self-starting model function, \code{SSfol}, is available.

  This dataset was originally part of package \code{nlme}, and that has
  methods (including for \code{[}, \code{as.data.frame}, \code{plot} and
  \code{print}) for its grouped-data classes.
}
\source{
  Boeckmann, A. J., Sheiner, L. B. and Beal, S. L. (1994), \emph{NONMEM
    Users Guide: Part V}, NONMEM Project Group, University of
  California, San Francisco.

  Davidian, M. and Giltinan, D. M. (1995)
  \emph{Nonlinear Models for Repeated Measurement Data},
  Chapman & Hall
  (section 5.5, p.\sspace{}145 and section 6.6, p.\sspace{}176)

  Pinheiro, J. C. and Bates, D. M. (2000) \emph{Mixed-effects Models in
    S and S-PLUS}, Springer (Appendix A.29)
}
\seealso{\code{\link{SSfol}}}
\examples{
require(stats); require(graphics)
\testonly{options(show.nls.convergence=FALSE)}
coplot(conc ~ Time | Subject, data = Theoph, show.given = FALSE)
Theoph.4 <- subset(Theoph, Subject == 4)
fm1 <- nls(conc ~ SSfol(Dose, Time, lKe, lKa, lCl),
           data = Theoph.4)
summary(fm1)
plot(conc ~ Time, data = Theoph.4,
     xlab = "Time since drug administration (hr)",
     ylab = "Theophylline concentration (mg/L)",
     main = "Observed concentrations and fitted model",
     sub  = "Theophylline data - Subject 4 only",
     las = 1, col = 4)
xvals <- seq(0, par("usr")[2], length.out = 55)
lines(xvals, predict(fm1, newdata = list(Time = xvals)),
      col = 4)
}
\keyword{datasets}
