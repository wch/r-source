\name{Theoph}
\alias{Theoph}
\non_function{}
\title{Pharmacokinetics of theophylline}
\description{
    The \code{Theoph} data frame has 132 rows and 5 columns of data from
    an experiment on the pharmacokinetics of theophylline.}
}
\format{
    This data frame contains the following columns:
    \describe{
	\item{Subject}{
	    an ordered factor with levels \code{1}, \dots, \code{12}
	    identifying the subject on whom the observation was made.
	    The ordering is by increasing maximum concentration of
	    theophylline observed.
	}
	\item{Wt}{
	    weight of the subject (kg).
	}
	\item{Dose}{
	    dose of theophylline administered orally to the subject
	    (mg/kg).
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
    Boeckmann, Sheiner and Beal (1994) report data from a study by Dr.
    Robert Upton of the kinetics of the anti-asthmatic drug theophylline.
    Twelve subjects were given oral doses of theophylline then serum
    concentrations were measured at 11 time points over the next 25 hours.

    These data are analyzed in Davidian and Giltinan (1995) and Pinheiro
    and Bates (2000) using a two-compartment open pharmacokinetic model,
    for which a self-starting model function, \code{SSfol}, is
    available.

}
\source{
    Boeckmann, A. J., Sheiner, L. B. and Beal, S. L. (1994), \emph{NONMEM
	Users Guide: Part V}, NONMEM Project Group, University of
    California, San Francisco.
    
    Davidian, M. and Giltinan, D. M. (1995) \emph{Nonlinear Models for
	Repeated Measurement Data}, Chapman & Hall (section 5.5, p. 145
    and section 6.6, p. 176)

    Pinheiro, J. C. and Bates, D. M. (2000) \emph{Mixed-effects Models in
	S and S-PLUS}, Springer (Appendix A.29)
}
\seealso{\code{\link{SSfol}}}
\keyword{datasets}
