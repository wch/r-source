\name{threads}
\alias{threads}
\alias{Yield}
\alias{Snooze}
\alias{ActiveThreads}
\alias{CurrentThread}
\alias{ThreadName}
\alias{SetThreadName}
\alias{NewThread}
\alias{JoinThread}
\alias{PreemptiveScheduling}
\alias{NewMutex}
\alias{NewCondvar}
\alias{MutexName}
\alias{CondvarName}
\alias{MutexData}
\alias{CondvarData}
\alias{SetMutexData}
\alias{SetCondvarData}
\alias{CondvarSignal}
\alias{CondvarBroadcast}
\alias{MutexLock}
\alias{MutexUnlock}
\alias{Concurrently}
\alias{ReplConsole}
\alias{EventLoop}
\alias{.Main}

\title{Preliminary Low-Level Threds Interface}
\description{
  A preliminary low-level threads interface.
}
\usage{
Yield()
Snooze(time)
ActiveThreads()
CurrentThread()
ThreadName(t)
SetThreadName(t, n)
NewThread(f)
JoinThread(t)
PreemptiveScheduling(val)
NewMutex(name = NULL)
NewCondvar(name = NULL)
MutexName(mx)
CondvarName(cv)
MutexData(mx)
CondvarData(cv)
SetMutexData(mx, data)
SetCondvarData(cv, data)
CondvarSignal(cv)
CondvarBroadcast(cv)
MutexLock(mx, timeout = -1.0, thread = CurrentThread())
MutexUnlock(mx, cv = NULL, timeout = -1.0)

ReplConsole(env = .GlobalEnv)
EventLoop()
.Main()
Concurrently(...)
}
\arguments{
  \item{t}{a thread.}
  \item{name}{character. Name for object.}
  \item{mx}{a mutex.}
  \item{cv}{a condition variable.}
  \item{data}{any R value.}
}
\details{
  This is a very preliminary low-level interface to a threading system.
  The interface is based more or less on the one proposed for Scheme in
  \url{http://srfi.schemers.org/srfi-18/} (one exception: for now
  creating and starting threads has not been
  separated--\code{NewThread} does both).

  The internal implementation is cooperative, which means C code will block
  unless it has been modified to cooperate with threads.  For now, the
  implementation is UNIX(alike)-only.

  In this version, R runs a \code{.Main} function.  This function
  starts a thread that calls \code{EventLoop} to start the X11 event loop
  and then runs \code{ReplConsole}, the read-eval-print loop.

  \code{Concurrently} is one simple example of a higher level function
  built on the threading primitives. It funs each of its function
  arguments in a separate thread and returns a list of the results.
}
\section{Warning}{
  Lazy evaluation and assignment interact in subtle ways.  This gets
  amplified when you add lexical scope, and is apmified even further
  when threads are involved.  If you create functions for
  \code{NewThread} to run as closures, in particular if you do so in
  \code{for} loops, you need to be aware of this. Also be aware that a
  \code{for} loop creates or uses a binding in its caller's
  environment and changes the value of that binding on each iteration.
  Running

  In particular,\code{
    for (i in seq(along=x))
       NewThread(function() do.something(x[[i]]))
    }
  probably won't do what you want it to.
}

\author{Luke Tierney}

\examples{
\dontrun{
# definition of Concurrently
Concurrently <- function(...) {
    if (! ThreadsEnabled()) ThreadsEnabled(TRUE)
    v <- list(...)
    for (i in seq(along = v))
        v[[i]] <- NewThread(v[[i]])
    for (i in seq(along = v))
        v[i] <- list(JoinThread(v[[i]]))
    v
}

# simple examples.  Errors cause termination of thread; value is null.
Concurrently(function() 1+2, function() 3+4)
Concurrently(function() stop("A"), function() 2+3)

# two short threads that each run to completion
f<-function(s, n) for (i in 1:n) cat(paste(s, i, "\n"))
Concurrently(function() f("A",3), function() f("B",3))

# force context switching by calling Yield
g<-function(s, n) for (i in 1:n) { cat(paste(s, i, "\n")); Yield() }
Concurrently(function() g("A",3), function() g("B",3))

# calling Snooze allows context switch
h<-function(s, n) for (i in 1:n) { cat(paste(s, i, "\n")); Snooze(0.5) }
Concurrently(function() h("A",3), function() h("B",3))

# if preemtion is turned on then long threads will switch
k<-function(s, n) for (i in 1:n) if (i \%\% 500 == 0) cat(paste(s, i, "\n"))
PreemptiveScheduling(TRUE)
Concurrently(function() k("A",5000), function() k("B",5000))

# TclTk can have its loop run in a thread.
TclLoop <- function() repeat{ .C("TclHandlerOnly"); Snooze(0.02) }
options(no.tcltk.handler=TRUE)
library(tcltk)
NewThread(TclLoop)
plot(1:10)
demo(tkcanvas)
}
}
\keyword{threads}
