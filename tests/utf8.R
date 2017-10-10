## This takes the definition of UTF-8 as RFC3629 (2003),
## but not all software does.
## See also https://en.wikipedia.org/wiki/UTF-8

x <- 1L:0x10FFFF
y <- intToUtf8(x, multiple = TRUE)
names(y) <- sprintf("U+%4x", x)
## values in the surrogate range: not handled in R < 3.4.3
sr <- 0xD800:0xDFFF
stopifnot(is.na(y[sr]))
stopifnot(!is.na(y[-sr]))
## too large values: originally handled by UTF-8, but not in RFC3629
## R >= 3.4.3 conforms to RFC3629
stopifnot(is.na(intToUtf8(c(0x200000, 0x10FFFF + 1:10))))

## next command is quite slow.
xx <- sapply(y, function(x) tryCatch(utf8ToInt(x),
                                     error = function(e) NA_character_))
invalid <- sr # previously included 0xFFFE and 0xFFFF
              # but not other 'noncharacters'.
stopifnot(is.na(xx[invalid]), !is.na(xx[!invalid]))
stopifnot(xx[!invalid] == x[!invalid])

## The pre-2003 UTF-8 standard converted larger code-points to 4-6 bytes,
## and was followed by intToUtf8 in earlier versions of R.
## Earlier conversion of 0x111111, 0x200001, 0x10000001)
x <- c("\xf4\x91\x84\x91", "\xf8\x80\x80\x80\x81", "\xfc\x90\x80\x80\x80\x81")
xx <- sapply(x, function(x) tryCatch(utf8ToInt(x),
                                     error = function(e) NA_character_))
stopifnot(is.na(xx)) # first was not in R < 3.4.3

### test surrogate pairs
surrogate_pair <- function(x)
{
    if(any(x < 0x10000 | x > 0x10FFFF))
        stop("Surrogate pairs apply only to supplementary planes")
    x <- x - 0x10000
    as.vector(rbind(0xD800 + x %/% 1024, 0xDC00 + x %% 1024))
}
## check the example:
xx <- surrogate_pair(0x10437)
sprintf("%X", xx)
stopifnot(xx == c(0xD801, 0xDC37))

## there are 2^20 surrogate pairs, but fast enough to check them all
x <- 0x10000:0x10FFFF
x1 <- intToUtf8(x)
x2 <- utf8ToInt(x1)
stopifnot(x2 == x)

z <- surrogate_pair(x)
x1 <- intToUtf8(z, allow_surrogate_pairs = TRUE)
x2 <- utf8ToInt(x1)
stopifnot(x2 == x)
