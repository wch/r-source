
Yield <- function() .Internal(Yield())     
Snooze <- function(d) .Internal(Snooze(d))     
ActiveThreads <- function() .Internal(ActiveThreads())
CurrentThread <- function() .Internal(CurrentThread())
ThreadName <- function(t) .Internal(ThreadName(t))
SetThreadName <- function(t, n) .Internal(SetThreadName(t, n))
NewThread <- function(f) .Internal(NewThread(f))
JoinThread <- function(t) .Internal(JoinThread(t))
ThreadsEnabled <-function(val) {
    if (missing(val)) .Internal(ThreadsEnabled())
    else .Internal(ThreadsEnabled(val))
}
PreemptiveScheduling <-function(val) {
    if (missing(val)) .Internal(PreemptiveScheduling())
    else .Internal(PreemptiveScheduling(val))
}

Concurrently<-function(...) .Internal(Concurrently(...))

Concurrently <- function(...) {
    if (! ThreadsEnabled()) ThreadsEnabled(TRUE)
    v <- list(...)
    for (i in seq(along = v))
        v[[i]] <- NewThread(v[[i]])
    for (i in seq(along = v))
        v[i] <- list(JoinThread(v[[i]]))
    v
}

NewMutex <- function(name = NULL) .Internal(NewMutex(name))
NewCondvar <- function(name = NULL) .Internal(NewCondvar(name))
MutexName <- function(mx) .Internal(MutexName(mx))
CondvarName <- function(cv) .Internal(CondvarName(cv))
MutexData <- function(mx) .Internal(MutexData(mx))
CondvarData <- function(cv) .Internal(CondvarData(cv))
SetMutexData <- function(mx, data) .Internal(SetMutexData(mx, data))
SetCondvarData <- function(cv, data) .Internal(SetCondvarData(cv, data))
CondvarSignal <- function(cv) .Internal(CondvarSignal(cv))
CondvarBroadcast <- function(cv) .Internal(CondvarBroadcast(cv))
MutexLock <- function(mx, timeout = -1.0, thread = CurrentThread())
    .Internal(MutexLock(mx, timeout, thread))
MutexUnlock <- function(mx, cv = NULL, timeout = -1.0)
    .Internal(MutexUnock(mx, cv, timeout))

ReplConsole <- function(env = .GlobalEnv) {
    op <- getOption("prompt")
    oc <- getOption("continue")
    on.exit(options(prompt = op, continue = oc))
    if (interactive())
	options(prompt = paste("REPL", op), continue = paste("REPL", oc))
    .Internal(ReplConsole(env))
}
#### need error trapping here
EventLoop <- function() .Internal(EventLoop())

.Main <- function() {
    if (exists("MainThread"))
        stop("can't call .Main twice")
    ThreadsEnabled(TRUE)
    MainThread <<- ActiveThreads()[[1]]  ### need CurrentThread
    SetThreadName(MainThread, "Main Thread")
    EventLoopThread <<- NewThread(EventLoop)
    SetThreadName(EventLoopThread, "Event Loop Thread")
    while(! is.null(try({ ReplConsole(); NULL}))) {}
}
