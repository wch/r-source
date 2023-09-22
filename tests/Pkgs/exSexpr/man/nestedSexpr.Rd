\name{nestedSexpr}
\alias{nestedSexpr}
\title{Nested \verb{\Sexpr} Macros}
% build macro won't be processed after install stage:
\Sexpr[stage=install,results=rd]{"\\\\description{\\\\Sexpr[stage=build]{getRversion()}}"}
% the following should work:
\Sexpr[stage=build,results=rd]{"\\\\details{\\\\Sexpr[stage=install]{getRversion()}}"}

#ifdef windows
\seealso{\link{nestedDefines}}
#endif (to test processRdIfdefs in the presence of \Sexpr macros)
