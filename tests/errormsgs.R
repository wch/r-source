options(error=expression())

## arithmetic.c
`+`(1,2,3)
pi + "foo"
matrix(1:6,2,3) + matrix(1:6,3,2)
!"foo"
`/`(1L)
`/`(pi)
sin("foo")
trunc(pi+1i)
atan(2,3)
round(pi, integer(0))
log(pi, integer(0))
log(pi, 10, 1)

# array.c
matrix(1:6,2,3, byrow="foo")
matrix(1:6,NA,3)
matrix(1:6,2,NA)
matrix(1:6,-1,3)
matrix(1:6,2,-1)
matrix(NA_real_, 2^17, 2^16)
row(1)
"foo" %*% pi
aperm(pi)
aperm(matrix(1:6,3.2), 3:1)
aperm(matrix(1:6,3.2), 3:2)
aperm(matrix(1:6,3.2), rep(1,1))
colSums(as.matrix(letters))
colSums(matrix(1:6,3.2), na.rm = NA)

# bind.c
unlist(y ~ x)
c(pi, recursive=TRUE, recursive=FALSE)
c(list(), use.names=FALSE, use.names=TRUE)
cbind(expression(pi), pi)
cbind(matrix(1:6,2,3), matrix(1:6,3,2))
rbind(matrix(1:6,2,3), matrix(1:6,3,2))

# builtin.c
cat(letters, fill = -3)
cat(letters, sep=pi)
cat(letters, fill=3, labels=1:10)
cat(letters, append=NA)
cat(y ~ x)
vector(character(0), 0)
vector("language", 0)
a <- y ~ x
length(a) <- 5
x <- pi
length(x) <- 1:3
length(x) <- NA
switch(1:3)

# character.c
nchar(letters, type="")
nchar(letters, type=pi)
substr("foo", integer(0), 1)
x <- pi
substr(x, integer(0), 1) <- pi
x <- "foo"
substr(x, integer(0), 1) <- pi
substr(x, 1, 1) <- pi
unlist(strsplit("a.b.c", "[.", perl = TRUE))
make.names("pi", allow_ = NA)
grep(character(0), letters)
grep("[.", letters)
grep("[.", letters, perl = TRUE)
sub("ab", "\\1", "abc")
sub("", "aa", "abc", fixed=TRUE)
x <- "MiXeD cAsE 123"
chartr("c-aX", "D-Fw", x)
chartr(NA_character_, "D-Fw", x)
chartr("ab", "c", x)
charToRaw(pi)
charToRaw(letters)
rawToChar(pi)
rawToChar(as.raw(10), multiple=NA)
rawShift(pi, 1)
rawShift(as.raw(10), -20)
rawToBits(pi)
intToBits(pi)
strtrim(paste(letters, collapse="+"), width = -10)
