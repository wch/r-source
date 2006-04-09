dyn.load(paste("Tests", .Platform$dynlib.ext, sep=""))

cat("[testStrings]\n")
.C("testStrings", 1:3, 3)
.C("testStrings", as.character(1:3), 3)
#
.C("testStrings", as.character(1:3), as.integer(3))

cat("[testSingles]\n")
.C("testSingles", c(1,2,3), as.integer(3))
.C("testSingles", as.numeric(c(1,2,3)), as.integer(3))
.C("testSingles", as.single(c(1,2,3)), 3)
#
.C("testSingles", as.single(c(1,2,3)), as.integer(3))

cat("[testList]\n")
.C("testList", list(1, "a", length(1)), 3)
.C("testList", "a", as.integer(1))

# Would like to have a LENGTH_OF_PREV
.C("testList", "a", 1)

#
.C("testList", list(1, "a", length(1)), as.integer(3))

#
cat("[setNumeric]\n")
print(.C("setNumeric", x=1:3, 3)$x == as.numeric(1:3*17))
