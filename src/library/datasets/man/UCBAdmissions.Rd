% File src/library/datasets/man/UCBAdmissions.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2009 R Core Team
% Distributed under GPL 2 or later

\name{UCBAdmissions}
\docType{data}
\alias{UCBAdmissions}
\title{Student Admissions at UC Berkeley}
\description{
  Aggregate data on applicants to graduate school at Berkeley for the
  six largest departments in 1973 classified by admission and sex.
}
\usage{UCBAdmissions}
\format{
  A 3-dimensional array resulting from cross-tabulating 4526
  observations on 3 variables.  The variables and their levels are as
  follows:

  \tabular{rll}{
    No \tab Name \tab Levels\cr
    1 \tab Admit \tab Admitted, Rejected\cr
    2 \tab Gender \tab Male, Female\cr
    3 \tab Dept \tab A, B, C, D, E, F
  }
}
\details{
  This data set is frequently used for illustrating Simpson's paradox,
  see Bickel et al.\ (1975).  At issue is whether the data show evidence
  of sex bias in admission practices.  There were 2691 male applicants,
  of whom 1198 (44.5\%) were admitted, compared with 1835 female
  applicants of whom 557 (30.4\%) were admitted.  This gives a sample
  odds ratio of 1.83, indicating that males were almost twice as likely
  to be admitted.  In fact, graphical methods (as in the example below)
  or log-linear modelling show that the apparent association between
  admission and sex stems from differences in the tendency of males and
  females to apply to the individual departments (females used to apply
  \emph{more} to departments with higher rejection rates).

  This data set can also be used for illustrating methods for graphical
  display of categorical data, such as the general-purpose mosaic plot
  or the fourfold display for 2-by-2-by-\eqn{k} tables.  See the
  home page of Michael Friendly
  (\url{http://www.math.yorku.ca/SCS/friendly.html}) for further
  information.
}
\references{
  Bickel, P. J., Hammel, E. A., and O'Connell, J. W. (1975)
  Sex bias in graduate admissions: Data from Berkeley.
  \emph{Science}, \bold{187}, 398--403.
}
\examples{
require(graphics)
## Data aggregated over departments
apply(UCBAdmissions, c(1, 2), sum)
mosaicplot(apply(UCBAdmissions, c(1, 2), sum),
           main = "Student admissions at UC Berkeley")
## Data for individual departments
opar <- par(mfrow = c(2, 3), oma = c(0, 0, 2, 0))
for(i in 1:6)
  mosaicplot(UCBAdmissions[,,i],
    xlab = "Admit", ylab = "Sex",
    main = paste("Department", LETTERS[i]))
mtext(expression(bold("Student admissions at UC Berkeley")),
      outer = TRUE, cex = 1.5)
par(opar)
}
\keyword{datasets}
