#### Regression and other checks for 'make check-devel' but *not* for 'make check'

## PR#18555: dummy_fgetc() returning EOF if the connection has an encoding specified

# try to find an available port
port <- NULL
for(p in round(runif(100, 27000, 28000))) {
  sock <- tryCatch(serverSocket(p), error= function(e) NULL)
  if (!is.null(sock)) {
    port <- p
    break
  }
}
stopifnot(!is.null(port))

outgoing <- socketConnection("localhost", port)
incoming <- socketAccept(sock, encoding="UTF-8")
close(sock)
r <- readLines(incoming, 1) # sets EOF_signalled
stopifnot(identical(r, character(0)))
writeLines("hello", outgoing)
close(outgoing)
r <- readLines(incoming, 1) # due to EOF_signalled, readLines() didn't realize
                            # that more data became available
while(isIncomplete(incoming)) {
  socketSelect(list(incoming))
  r <- readLines(incoming, 1)
}
close(incoming)
stopifnot(identical(r, "hello")) # r was character(0) in error

proc.time()
