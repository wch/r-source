Rprof <- function(filename = "Rprof.out", append = FALSE, interval =  0.02, memory.profiling=FALSE)
{
    if(is.null(filename)) filename <- ""
    invisible(.Internal(Rprof(filename, append, interval, memory.profiling)))
}

tracemem <- function(x) {.Internal(memtrace(x))}
untracemem <- function(x) {.Internal(memuntrace(x))}
