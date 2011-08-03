# --- multicore --- low-level functions ---

# selected signals
SIGHUP <- 1L
SIGINT <- 2L
SIGQUIT <- 3L
SIGKILL <- 9L
SIGTERM <- 15L
SIGALRM <- 14L
SIGSTOP <- 17L
SIGCHLD <- 20L
SIGINFO <- 29L
SIGUSR1 <- 30L
SIGUSR2 <- 31L

mcfork <- function() {
    r <- .Call("mc_fork", PACKAGE = "parallel")
    structure(list(pid = r[1L], fd = r[2:3]),
              class = c(if(r[1L]) "childProcess" else "masterProcess", "process"))
}

readChildren <- function(timeout = 0)
    .Call("mc_read_children", as.double(timeout), PACKAGE="parallel")

readChild <- function(child)
{
    if (inherits(child, "process")) child <- processID(child)
    if (!is.numeric(child)) stop("invalid child argument")
    .Call("mc_read_child", as.integer(child), PACKAGE = "parallel")
}

selectChildren <- function(children = NULL, timeout = 0)
{
    if (!length(children)) children <- integer()
    if (inherits(children, "process")) children <- processID(children)
    if (is.list(children))
        children <- unlist(lapply(children, function(x) if (inherits(x, "process")) x$pid
        else stop("children must be a list of processes or a single process")))
    if (!is.numeric(children))
        stop("children must be a list of processes or a single process")
    .Call("mc_select_children", as.double(timeout), as.integer(children))
}

rmChild <- function(child)
{
    if (inherits(child, "process")) child <- processID(child)
    if (!is.numeric(child)) stop("invalid child argument")
    .Call("mc_rm_child", as.integer(child), PACKAGE = "parallel")
}

mckill <- function(process, signal = SIGINT)
{
    process <- processID(process)
    unlist(lapply(process, function(p)
                  .Call("mc_kill", as.integer(p), as.integer(signal))))
}

sendMaster <- function(what)
{
    if (!is.raw(what)) what <- serialize(what, NULL, FALSE)
    .Call("mc_send_master", what, PACKAGE = "parallel")
}

processID <- function(process) {
    if (inherits(process, "process")) process$pid
    else if (is.list(process)) unlist(lapply(process, processID))
    else stop("process must be of the class \"`process\"")
}

sendChildStdin <- function(child, what)
{
    if (inherits(child, "process") || is.list(child)) child <- processID(child)
    if (!is.numeric(child) || !length(child))
        stop("child must be a valid child process")
    child <- as.integer(child)
    if (is.character(what)) what <- charToRaw(paste(what, collapse='\n'))
    if (!is.raw(what)) stop("what must be a character or raw vector")
    unlist(lapply(child, function(p)
                  .Call("mc_send_child_stdin", p, what, PACKAGE = "parallel")))
}

mcexit <- function(exit.code = 0L, send = NULL)
{
    if (!is.null(send)) try(sendMaster(send), silent = TRUE)
    .Call("mc_exit", as.integer(exit.code), PACKAGE = "parallel")
}

children <- function(select)
{
    p <- .Call("mc_children", PACKAGE = "parallel")
    if (!missing(select)) p <- p[p %in% processID(select)]
    lapply(p, function(x)
           structure(list(pid = x), class = c("childProcess", "process")))
}

childrenDescriptors <- function(index = 0L)
    .Call("mc_fds", as.integer(index), PACKAGE = "parallel")

masterDescriptor <- function() .Call("mc_master_fd", PACKAGE = "parallel")

isChild <- function() .Call("mc_is_child", PACKAGE = "parallel")

closeStdout <- function() .Call("mc_close_stdout", PACKAGE = "parallel")
closeStderr <- function() .Call("mc_close_stderr", PACKAGE = "parallel")
closeFD <- function(fds)
    .Call("mc_close_fds", as.integer(fds), PACKAGE = "parallel")

closeAll <- function(includeStd = FALSE)
{
    if (!isChild()) {
        warning("closeAll() is a no-op in the master process")
        return(invisible(FALSE))
    }
    fds <- masterDescriptor()
    if (identical(fds, -1L)) fds <- integer(0)
    if (includeStd) fds <- c(1L, 2L, fds)
    mf <- max(fds) + 16L # take a few more ...
    ## close all but those that we actually use
    closeFD((1:mf)[-fds])
}
