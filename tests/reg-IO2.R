## tests of boundary cases in read.table()
# empty file
file.create("foo1")
try(read.table("foo1")) # fails
read.table("foo1", col.names=LETTERS[1:4])
unlink("foo1")

# header only
cat("head\n", file = "foo2")
read.table("foo2")
try(read.table("foo2", header=TRUE)) # fails in 1.2.3
unlink("foo2")
# header detection
cat("head\n", 1:2, "\n", 3:4, "\n", file = "foo3")
read.table("foo3", header=TRUE)
read.table("foo3", header=TRUE, col.names="V1")
read.table("foo3", header=TRUE, row.names=1)
read.table("foo3", header=TRUE, row.names="row.names")
read.table("foo3", header=TRUE, row.names="head") # fails in 1.2.3

# wrong col.names
try(read.table("foo3", header=TRUE, col.names=letters[1:4]))
unlink("foo3")

# incomplete last line
cat("head\n", 1:2, "\n", 3:4, file = "foo4")
read.table("foo4", header=TRUE)
unlink("foo4")

# blank last line
cat("head\n\n", 1:2, "\n", 3:4, "\n\n", file = "foo5")
read.table("foo5", header=TRUE)

# test of fill
read.table("foo5", header=FALSE, fill=TRUE, blank.lines.skip=FALSE) # fails in 1.2.3
unlink("foo5")

cat("head\n", 1:2, "\n", 3:5, "\n", 6:9, "\n", file = "foo6")
try(read.table("foo6", header=TRUE))
try(read.table("foo6", header=TRUE, fill=TRUE))
read.table("foo6", header=FALSE, fill=TRUE)
unlink("foo6")

## end of tests
