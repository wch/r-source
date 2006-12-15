Rprof <- function(filename = "Rprof.out", append = FALSE, interval =  0.02,
                  memory.profiling = FALSE)
{
    if(is.null(filename)) filename <- ""
    invisible(.Internal(Rprof(filename, append, interval, memory.profiling)))
}

Rprofmem <- function(filename = "Rprofmem.out", append = FALSE, threshold=0)
{
    if(is.null(filename)) filename <- ""
    invisible(.Internal(Rprofmem(filename, append, as.double(threshold))))
}

tracemem <- function(x) .Internal(tracemem(x))
untracemem <- function(x) .Internal(untracemem(x))
retracemem <- function(x, previous = NULL) .Internal(retracemem(x, previous))

