mcparallel <- function(expr, name, mc.set.seed = FALSE, silent = FALSE)
{
    f <- mcfork()
    env <- parent.frame()
    if (inherits(f, "masterProcess")) {
        on.exit(exit(1, structure("fatal error in wrapper code",
                                  class = "try-error")))
        if (isTRUE(mc.set.seed)) set.seed(Sys.getpid())
        if (isTRUE(silent)) closeStdout()
        sendMaster(try(eval(expr, env), silent=TRUE))
        mcexit(0L)
    }
    if (!missing(name) && !is.null(name)) f$name <- as.character(name)[1]
    class(f) <- c("parallelJob", class(f))
    f
}

mccollect <- function(jobs, wait = TRUE, timeout = 0, intermediate = FALSE)
{
    if (missing(jobs)) jobs <- children()
    if (!length(jobs)) return (NULL)
    if (isTRUE(intermediate)) intermediate <- str
    if (!wait) {
        s <- selectChildren(jobs, timeout)
        if (is.logical(s) || !length(s)) return(NULL)
        lapply(s, function(x) {
            r <- readChild(x)
            if (is.raw(r)) unserialize(r) else NULL
        })
    } else {
        pids <- if (inherits(jobs, "process") || is.list(jobs)) processID(jobs) else jobs
        if (!length(pids)) return(NULL)
        if (!is.numeric(pids)) stop("invalid jobs argument")
        pids <- as.integer(pids)
        pnames <- as.character(pids)
        if (!inherits(jobs, "process") && is.list(jobs))
            for(i in seq(jobs))
                if (!is.null(jobs[[i]]$name))
                    pnames[i] <- as.character(jobs[[i]]$name)
        res <- lapply(pids, function(x) NULL)
        names(res) <- pnames
        fin <- rep(FALSE, length(jobs))
        while (!all(fin)) {
            s <- selectChildren(pids, 0.5)
            if (is.integer(s)) {
                for (pid in s) {
                    r <- readChild(pid)
                    if (is.integer(r) || is.null(r)) fin[pid == pids] <- TRUE
                    if (is.raw(r)) res[[which(pid == pids)]] <- unserialize(r)
                }
                if (is.function(intermediate)) intermediate(res)
            } else if (all(is.na(match(pids, processID(children()))))) break
        }
        res
    }
}
