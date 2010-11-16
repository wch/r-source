## tests of options in system() and system2.

options(warn = 1)

opts <- list("", NULL, FALSE, TRUE, "o1.txt", "o2.txt")
outs <- c("o1.txt", "o2.txt")

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

for(out in opts)
    for(err in opts) {
        ## skip this for the sake of Unix-alikes
        if(identical(err, TRUE) && !identical(out,TRUE)) next
        cat(sprintf("\ntesting stdout = %s, stderr = %s\n",
            deparse(out), deparse(err)))
        process(system2("test-system2", stdout = out, stderr = err))
    }


process(system("test-system2"))
process(system("test-system2", ignore.stdout = TRUE))
process(system("test-system2", ignore.stderr = TRUE))
process(system("test-system2", ignore.stdout = TRUE, ignore.stderr = TRUE))

process(system("test-system2", TRUE))
process(system("test-system2", TRUE, ignore.stdout = TRUE))
process(system("test-system2", TRUE, ignore.stdout = TRUE, ignore.stderr = TRUE))

process(system2("test-system2", "1", input=letters[1:4]))
process(system2("test-system2", "1", input=letters[1:4], stdout = TRUE))

process(system("test-system2 1", input=letters[1:4]))
process(system("test-system2 1", input=letters[1:4], intern = TRUE))

tmp <- tempfile()
writeLines(letters[5:7], tmp)
process(system2("test-system2", "1", stdin = tmp))
process(system2("test-system2", "1", stdin = tmp, stdout = TRUE))
process(system2("test-system2", "1", stdin = tmp, stdout = TRUE, stderr = TRUE))
process(system2("test-system2", "1", stdin = tmp, stdout = "o1.txt", stderr = "o1.txt"))
process(system2("test-system2", "1", stdin = tmp, stdout = "o1.txt", stderr = "o2.txt"))

unlink(c(tmp, outs))

print(system("test-system2 5"))
system("test-system2 6", intern = TRUE)
print(system2("test-system2", "7"))
system2("test-system2", "8", stdout=TRUE)
