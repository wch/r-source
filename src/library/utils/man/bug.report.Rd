\name{bug.report}
\alias{bug.report}
\title{Send a Bug Report}
\usage{
bug.report(subject = "",
           ccaddress = Sys.getenv("USER"),
           method = getOption("mailer"),
           address = "r-bugs@r-project.org",
#ifdef windows
           file = "R.bug.report", wait = TRUE)
#endif
#ifdef unix
           file = "R.bug.report")
#endif
}
\arguments{
  \item{subject}{Subject of the email. Please do not use single quotes
    (\code{'}) in the subject! File separate bug reports for multiple bugs}
  \item{ccaddress}{Optional email address for copies (default is current
    user).  Use \code{ccaddress = FALSE} for no copies.}
  \item{method}{Submission method, one of \code{"mailx"},
    \code{"gnudoit"}, \code{"none"}, or \code{"ess"}.}
  \item{address}{Recipient's email address.}
  \item{file}{File to use for setting up the email (or storing it when
    method is \code{"none"} or sending mail fails).}
#ifdef windows
  \item{wait}{logical. Should \R wait for the editor to return?}
#endif
}
\description{
  Invokes an editor to write a bug report and optionally mail it to the
  automated r-bugs repository at \email{r-bugs@r-project.org}.  Some standard
  information on the current version and configuration of \R are 
  included automatically.
}
\details{
  Currently direct submission of bug reports works only on Unix systems.
  If the submission method is \code{"mailx"}, then the default editor is
  used to write the bug report. Which editor is used can be controlled
  using \code{\link{options}}, type \code{getOption("editor")} to see what
  editor is currently defined. Please use the help pages of the
  respective editor for details of usage. After saving the bug report
  (in the temporary file opened) and exiting the editor
  the report is mailed using a Unix command line mail utility such as
  \code{mailx}.  A copy of the mail is sent to the current user.

  If method is \code{"gnudoit"}, then an emacs mail buffer is opened
  and used for sending the email.

  If method is \code{"none"} or \code{NULL} (which is the default on
  Windows systems), then only an editor is opened to help writing the
  bug report.  The report can then be copied to your favorite email
  program and be sent to the r-bugs list.

  If method is \code{"ess"} the body of the mail is simply sent to
  stdout.
}
\value{Nothing useful.}
\section{When is there a bug?}{
  If \R executes an illegal instruction, or dies with an operating
  system error message that indicates a problem in the program (as
  opposed to something like \dQuote{disk full}), then it is certainly a
  bug.

  Taking forever to complete a command can be a bug, but you must make
  certain that it was really \R's fault.  Some commands simply take a
  long time.  If the input was such that you KNOW it should have been
  processed quickly, report a bug.  If you don't know whether the
  command should take a long time, find out by looking in the manual or
  by asking for assistance.

  If a command you are familiar with causes an \R error message in a
  case where its usual definition ought to be reasonable, it is probably
  a bug.  If a command does the wrong thing, that is a bug.  But be sure
  you know for certain what it ought to have done.  If you aren't
  familiar with the command, or don't know for certain how the command
  is supposed to work, then it might actually be working right.  Rather
  than jumping to conclusions, show the problem to someone who knows for
  certain.

   Finally, a command's intended definition may not be best for
   statistical analysis.  This is a very important sort of problem, but
   it is also a matter of judgment.  Also, it is easy to come to such a
   conclusion out of ignorance of some of the existing features.  It is
   probably best not to complain about such a problem until you have
   checked the documentation in the usual ways, feel confident that you
   understand it, and know for certain that what you want is not
   available. The mailing list \code{r-devel@r-project.org} is a better
   place for discussions of this sort than the bug list.

   If you are not sure what the command is supposed to do
   after a careful reading of the manual this indicates a bug in the
   manual.  The manual's job is to make everything clear.  It is just as
   important to report documentation bugs as program bugs.

   If the online argument list of a function disagrees with the manual,
   one of them must be wrong, so report the bug.
}
\section{How to report a bug}{
  When you decide that there is a bug, it is important to report it and
  to report it in a way which is useful.  What is most useful is an
  exact description of what commands you type, from when you start \R
  until the problem happens.  Always include the version of \R, machine,
  and operating system that you are using; type \kbd{version} in \R to
  print this.  To help us keep track of which bugs have been fixed and
  which are still open please send a separate report for each bug.

  The most important principle in reporting a bug is to report FACTS,
  not hypotheses or categorizations.  It is always easier to report the
  facts, but people seem to prefer to strain to posit explanations and
  report them instead.  If the explanations are based on guesses about
  how \R is implemented, they will be useless; we will have to try to 
  figure out what the facts must have been to lead to such
  speculations.  Sometimes this is impossible.  But in any case, it is
  unnecessary work for us.

  For example, suppose that on a data set which you know to be quite
  large the command \code{data.frame(x, y, z, monday, tuesday)} never
  returns.  Do not report that \code{data.frame()} fails for large data
  sets.  Perhaps it fails when a variable name is a day of the week.  If
  this is so then when we got your report we would try out the
  \code{data.frame()} command on a large data set, probably with no day
  of the week variable name, and not see any problem. There is no way in
  the world that we could guess that we should try a day of the week
  variable name.

  Or perhaps the command fails because the last command you used was a
  \code{[} method that had a bug causing \R's internal data structures
  to be corrupted and making the \code{data.frame()} command fail from
  then on.  This is why we need to know what other commands you have
  typed (or read from your startup file).

  It is very useful to try and find simple examples that produce
  apparently the same bug, and somewhat useful to find simple examples
  that might be expected to produce the bug but actually do not.  If you
  want to debug the problem and find exactly what caused it, that is
  wonderful.  You should still report the facts as well as any
  explanations or solutions.

  Invoking \R with the \option{--vanilla} option may help in isolating a
  bug.  This ensures that the site profile and saved data files are not
  read.

  A bug report can be generated using the
  \code{bug.report()} function.  This automatically includes the version
  information and sends the bug to the correct address.  Alternatively
  the bug report can be emailed to \email{r-bugs@r-project.org} or
  submitted to the Web page at \url{http://bugs.r-project.org}.

  Bug reports on \strong{contributed packages} should perhaps be sent to the
  package maintainer rather than to r-bugs.
}
\seealso{R FAQ} 
\author{This help page is adapted from the Emacs manual and the R FAQ}
\keyword{utilities}
\keyword{error}
