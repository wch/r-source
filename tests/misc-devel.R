#### Regression and other checks for 'make check-devel' but *not* for 'make check'

## PR#18555: dummy_fgetc() returning EOF if the connection has an encoding specified
## --------  Based on small change from https://bugs.r-project.org/show_bug.cgi?id=18555#c4
## FIXME: needs a more thorough analysis and fixes (see 'Comment #2' in PR)
sockChk <- function(enc, timeout = 0.25, verbose = TRUE, port = 27182)
{
    stopifnot(is.character(enc), length(enc) == 1L)
    if(verbose) {
        cat(sprintf("enc = '%s', timeout=%g:\n", enc, timeout))
        pr <- print
    } else
        pr <- identity
    sock <- serverSocket(port)
    outgoing <- socketConnection("localhost", port, encoding=enc)
    incoming <- socketAccept(sock, encoding=enc)
    on.exit({close(incoming); close(outgoing); close(sock) })
    writeLines("hello", outgoing) ; flush(outgoing)
    r1 <- readLines(incoming, 1) # "hello"
    r2 <- readLines(incoming, 1) # character(0) *and*  EOF_signalled gets set
    writeLines("again1", outgoing); flush(outgoing)
    stopifnot(identical(r2, character(0)), ## now *have* incoming again:
              socketSelect(list(incoming)))
    r3 <- readLines(incoming, 1) # got character(0) incorrectly
    ## because of con->EOF_signalled, dummy_fgetc didn't attempt to read more data
    if(socketSelect(list(incoming), timeout=0)) # *was* TRUE wrongly
        stop("incoming is empty")
    writeLines("again2", outgoing)
    writeLines("again3", outgoing); flush(outgoing)
    if(timeout) Sys.sleep(timeout) # flush not sufficient -> need timeout, platform dependently
    r4 <- readLines(incoming, 1) # was character(0), on Mac even after r84624 (timeout=0)
    ## NB isIncomplete(incoming) being FALSE  corresponds to doc
    r5 <- suppressWarnings(# Warning "text connection used with readChar(), .."
        readChar(incoming, 100))
    stopifnot(identical(pr(cbind(r1,r3,r4,r5)[1,]), # print() before possibly reporting ..
                        c(r1 = "hello", r3 = "again1",
                          r4 = "again2",r5 = "again3\n")))
    invisible(TRUE)
}

## All three failed on (arm64) macOS (with 0 timeout); PR#18555, comment c5: even 0.5 is insufficient
timeout <- if(Sys.info()["sysname"] == "Darwin") 1.0 else 0.125
                                        # 0 seemed sufficient for non-Mac
sockChk("ASCII", timeout)
sockChk("UTF-8", timeout)
## The default is  getOption("encoding")  which defaults to "native.enc"
sockChk("native.enc", timeout)
## only the last already worked in R <= 4.3.1 {for non macOS with 0 timeout}

## stress test w/o sleep
lapply(1:50, \(i) try({ cat(i,":\n---\n")
    sockChk("ASCII", 0)
    sockChk("UTF-8", 0)
    sockChk("native.enc", 0)
})) -> R
str(R, give.attr = FALSE)
table(sapply(R, \(e) if(is.logical(e)) e else class(e)))

proc.time()
