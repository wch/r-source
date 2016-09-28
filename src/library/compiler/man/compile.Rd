% File src/library/compiler/man/compile.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2011 R Core Team
% Distributed under GPL 2 or later

\name{compile}
\alias{compile}
\alias{cmpfun}
\alias{cmpfile}
\alias{loadcmp}
\alias{disassemble}
\alias{enableJIT}
\alias{compilePKGS}
\alias{getCompilerOption}
\alias{setCompilerOptions}
\title{Byte Code Compiler}
\usage{
cmpfun(f, options = NULL)
compile(e, env = .GlobalEnv, options = NULL, srcref = NULL)
cmpfile(infile, outfile, ascii = FALSE, env = .GlobalEnv,
        verbose = FALSE, options = NULL)
loadcmp(file, envir = .GlobalEnv, chdir = FALSE)
disassemble(code)
enableJIT(level)
compilePKGS(enable)
getCompilerOption(name, options)
setCompilerOptions(...)
}
\arguments{
  \item{f}{a closure.}
  \item{options}{list of named compiler options}
  \item{env}{the top level environment for the compiling.}
  \item{srcref}{initial source reference for the expression.}
  \item{file,infile,outfile}{pathnames; outfile defaults to infile with a .Rc
                             extension in place of any existing extension.}
  \item{ascii}{logical; should the compiled file be saved in ascii format?}
  \item{verbose}{logical; should the compiler show what is being compiled}
  \item{envir}{environment to evaluate loaded expressions in.}
  \item{chdir}{logical; change directory before evaluation?}
  \item{code}{byte code expression or compiled closure}
  \item{e}{expression to compile}
  \item{level}{integer; the JIT level to use}
  \item{enable}{logical; enable compiling packages if \code{TRUE}}
  \item{name}{character string; name of option to return}
  \item{...}{named compiler options to set}
}
\description{
  These functions provide an interface to a byte code compiler for R.
}
\details{
  The function \code{cmpfun} compiles the body of a closure and
  returns a new closure with the same formals and the body replaced by
  the compiled body expression.

  \code{compile} compiles an expression into a byte code object; the
  object can then be evaluated with \code{eval}.

  \code{cmpfile} parses the expression in \code{infile}, compiles
  them, and writes the compiled expressions to \code{outfile}.  If
  \code{outfile} is not provided, it is formed from \code{infile} by
  replacing or appending a \code{.Rc} suffix.

  \code{loadcmp} is used to load compiled files.  It is similar to
  \code{sys.source}, except that its default loading environment is the
  global environment rather than the base environment.

  \code{disassemble} produces a printed representation of the code
  that may be useful to give a hint of what is going on.

  \code{enableJIT} enables or disables just-in-time (JIT)
  compilation. JIT is disabled if the argument is 0. If \code{enable} is
  1 then closures are compiled before their first use.  If \code{enable}
  is 2, then in addition closures are also compiled before they are
  duplicated (useful for some packages, like \code{lattice}, that store
  closures in lists).  If \code{enable} is 3 then in addition all loops
  are compiled before they are executed.  JIT level 3 requires
  optimization level 2 or 3. JIT can also be enabled by starting \R with
  the environment variable \code{R_ENABLE_JIT} set to one of these
  values. Calling \code{enableJIT} with a negative argument returns the
  current JIT level.

  \code{compilePKGS} enables or disables compiling packages when they
  are installed.  This requires that the package use lazy loading as
  compilation occurs as functions are written to the lazy loading data
  base. This can also be enabled by starting \R with the environment
  variable \code{R_COMPILE_PKGS} set to a positive integer value.

  Currently the compiler warns about a variety of things.  It does
  this by using \code{cat} to print messages.  Eventually this should
  use the condition handling mechanism.

  The \code{options} argument can be used to control compiler operation.
  There are currently three options: \code{optimize},
  \code{suppressAll}, and \code{suppressUndefined}. \code{optimize}
  specifies the optimization level, which can be an integer form 0 to 3.
  \code{suppressAll} should be a scalar logical; if \code{TRUE} no
  messages will be shown. \code{suppressUndefined} can be \code{TRUE} to
  suppress all messages about undefined variables, or it can be a
  character vector of the names of variables for which messages should
  not be shown.

  \code{getCompilerOption} returns the value of the specified option.
  The default value is returned unless a value is supplied in the
  \code{options} argument; the \code{options} argument is primarily for
  internal use.  \code{setCompilerOption} sets the default option
  values. Options to set are identified by argument names, e.g.
  \code{setCompilerOptions(suppressAll = TRUE, optimize = 3)}.
  It returns a named list of the previous values.

  Calling the compiler a byte code compiler is actually a bit of a
  misnomer: the external representation of code objects currently uses
  \code{int} operands, and when compiled with \code{gcc} the internal
  representation is actually threaded code rather than byte code.
}
\author{Luke Tierney}

\examples{
# a simple example
f <- function(x) x+1
fc <- cmpfun(f)
fc(2)
disassemble(fc)

# old R version of lapply
la1 <- function(X, FUN, ...) {
    FUN <- match.fun(FUN)
    if (!is.list(X))
	X <- as.list(X)
    rval <- vector("list", length(X))
    for(i in seq(along = X))
	rval[i] <- list(FUN(X[[i]], ...))
    names(rval) <- names(X)		  # keep `names' !
    return(rval)
}
# a small variation
la2 <- function(X, FUN, ...) {
    FUN <- match.fun(FUN)
    if (!is.list(X))
	X <- as.list(X)
    rval <- vector("list", length(X))
    for(i in seq(along = X)) {
        v <- FUN(X[[i]], ...)
        if (is.null(v)) rval[i] <- list(v)
        else rval[[i]] <- v
    }
    names(rval) <- names(X)		  # keep `names' !
    return(rval)
}
# Compiled versions
la1c <- cmpfun(la1)
la2c <- cmpfun(la2)
# some timings
x <- 1:10
y <- 1:100
\donttest{
system.time(for (i in 1:10000) lapply(x, is.null))
system.time(for (i in 1:10000) la1(x, is.null))
system.time(for (i in 1:10000) la1c(x, is.null))
system.time(for (i in 1:10000) la2(x, is.null))
system.time(for (i in 1:10000) la2c(x, is.null))
system.time(for (i in 1:1000) lapply(y, is.null))
system.time(for (i in 1:1000) la1(y, is.null))
system.time(for (i in 1:1000) la1c(y, is.null))
system.time(for (i in 1:1000) la2(y, is.null))
system.time(for (i in 1:1000) la2c(y, is.null))
}
\testonly{
for (i in 1:10000) lapply(x, is.null)
for (i in 1:10000) la1(x, is.null)
for (i in 1:10000) la1c(x, is.null)
for (i in 1:10000) la2(x, is.null)
for (i in 1:10000) la2c(x, is.null)
for (i in 1:1000) lapply(y, is.null)
for (i in 1:1000) la1(y, is.null)
for (i in 1:1000) la1c(y, is.null)
for (i in 1:1000) la2(y, is.null)
for (i in 1:1000) la2c(y, is.null)
}}
\keyword{programming}
