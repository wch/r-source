## tests of options in system() and system2.

options(warn = 1)

opts <- list("", NULL, FALSE, TRUE, "o1.txt", "o2.txt")
outs <- c("o1.txt", "o2.txt")
tos <- c(0, 10)

process <- function(res)
{
    unlink(outs)
    if(is.character(res)) {
        cat("value:\n")
        writeLines(res)
    }
    for(f in outs)
        if(file.exists(f)) {
            cat(f, ":\n", sep = "")
            writeLines(readLines(f))
        }
}

for(to in tos)
    for(out in opts)
        for(err in opts) {
            ## skip this for the sake of Unix-alikes
            if(identical(err, TRUE) && !identical(out,TRUE)) next
            cat(sprintf("\ntesting stdout = %s, stderr = %s\n",
                deparse(out), deparse(err)))
            process(system2("test-system2", stdout = out, stderr = err,
                            timeout = to))
        }

for(to in tos) {
    # timeout 0 uses different implementations from timeout 10
    # the outputs should be identical

    process(system("test-system2", timeout = to))
    process(system("test-system2", ignore.stdout = TRUE, timeout = to))
    process(system("test-system2", ignore.stderr = TRUE, timeout = to))
    process(system("test-system2", ignore.stdout = TRUE, ignore.stderr = TRUE,
                   timeout = to))

    process(system("test-system2", TRUE, timeout = to))
    process(system("test-system2", TRUE, ignore.stdout = TRUE, timeout = to))
    process(system("test-system2", TRUE, ignore.stdout = TRUE,
                   ignore.stderr = TRUE, timeout = to))

    process(system2("test-system2", "1", input=letters[1:4], timeout = to))
    process(system2("test-system2", "1", input=letters[1:4], stdout = TRUE,
                    timeout = to))

    process(system("test-system2 1", input=letters[1:4], timeout = to))
    process(system("test-system2 1", input=letters[1:4], intern = TRUE,
                   timeout = to))

    tmp <- tempfile()
    writeLines(letters[5:7], tmp)
    process(system2("test-system2", "1", stdin = tmp, timeout = to))
    process(system2("test-system2", "1", stdin = tmp, stdout = TRUE,
                    timeout = to))
    process(system2("test-system2", "1", stdin = tmp, stdout = TRUE,
                    stderr = TRUE, timeout = to))
    process(system2("test-system2", "1", stdin = tmp, stdout = "o1.txt",
                    stderr = "o1.txt", timeout = to))
    process(system2("test-system2", "1", stdin = tmp, stdout = "o1.txt",
                    stderr = "o2.txt", timeout = to))

    unlink(c(tmp, outs))

    print(system("test-system2 5", timeout = to))
    system("test-system2 6", intern = TRUE, timeout = to)
    print(system2("test-system2", "7", timeout = to))
    system2("test-system2", "8", stdout=TRUE, timeout = to)
}

# tests that time out
#   (each runs for a second)

system("./test-system2 sleep 10", timeout = 1)
system("./test-system2 infinite_loop", timeout = 1)
system("./test-system2 sleep 10", timeout = 1, intern = T)
system("./test-system2 infinite_loop", timeout = 1, intern = T)

## test results with timeout set

stopifnot(identical(system("./test-system2 2", timeout = 1), 2L))
stopifnot(identical(system("./test-system2 2", timeout = 1, intern = T),
                    structure("stdout 1", status = 2L)))
