####  Regression Tests for bugs typically found via "fuzzing", e.g. OSS-Fuzz
####  -------------------------------------------------------
####  Strict tests, but only run from 'make check-all' ("once fixed should remain good")
###                                  ____________________
.pt <- proc.time()
tryCid <- function(expr) tryCatch(expr, error = identity)
assertErrV  <- function(...) tools::assertError  (..., verbose=TRUE)
assertWarnV <- function(...) tools::assertWarning(..., verbose=TRUE)
onWindows <- .Platform$OS.type == "windows"
onUnix    <- .Platform$OS.type == "unix"

### First dozen entries moved from ./tests-reg-tests-1e.R ----------------------

## malformed ALTREP serialized class metadata -- PR#19047
altrep <- rawToChar(serialize(1:3, NULL, ascii = TRUE))
marker <- "\n2\n13\n1\n13\n254\n"
pos <- gregexpr(marker, altrep, fixed = TRUE)[[1L]]
stopifnot(length(pos) == 1L, pos > 0L)
replacements <- c("\n2\n13\n1\n8357\n254\n",
                  "\n2\n14\n1\n13\n254\n",
                  "\n2\n13\n0\n254\n")
errs <- lapply(replacements, function(replacement) {
    serialized <- charToRaw(sub(marker, replacement, altrep, fixed = TRUE))
    tryCid(unserialize(serialized))
})
stopifnot(vapply(errs, inherits, NA, what = "error"),
          vapply(errs, function(e)
              grepl("invalid ALTREP serialized class", conditionMessage(e), fixed = TRUE),
              NA))
## an invalid type value indexed beyond Type2Table and seg.faulted


## x[i] <- v coercing and extending x with check.bounds on -- PR#19111
local({
    op <- options(check.bounds = TRUE)
    on.exit(options(op))
    x <- c(TRUE, FALSE)
    gctorture(TRUE)
    suppressWarnings(x[5] <- 1L)
    gctorture(FALSE)
    stopifnot(identical(x, c(1L, 0L, NA, NA, 1L)))
})
## gave garbage, an error or a segfault in R <= 4.6.2


## readLines() must free its line buffer on error, incl. after growth.
## Also exercise the cleanup under leak checkers.		-- PR#19208
local({
    tf <- tempfile()
    old <- options(warn = 2)
    for (mode in c("", "rt")) {
        ## A corrupt gzip stream fails while reading the first line.
        writeBin(as.raw(c(0x1f, 0x8b, 0x08, 0, 0, 0, 0, 0, 0, 3,
                          rep(0xaf, 40))), tf)
        con <- gzfile(tf, mode)
        stopifnot(inherits(tryCid(readLines(con)), "error"),
                  identical(isOpen(con), nzchar(mode)))
        close(con)
        ## Errors after growth, both during reading and after closing.
        line <- strrep("a", 8000)
        writeBin(c(charToRaw(line), as.raw(0), charToRaw("x\n")), tf)
        con <- file(tf, mode)
        stopifnot(inherits(tryCid(readLines(con)), "error"),
                  identical(isOpen(con), nzchar(mode)))
        close(con)
        writeChar(line, tf, eos = NULL)
        con <- file(tf, mode)
        stopifnot(inherits(tryCid(readLines(con)), "error"),
                  identical(isOpen(con), nzchar(mode)))
        close(con)
    }
    options(old)
    unlink(tf)
})
## leaked the buffer on errors in R <= 4.6.x


## grepRaw() with truncated TRE patterns -- PR#18995
for (p in c("[", "[[:alpha:", "[a-", "(", "(?", "(?#", "\\x", "\\x4", "\\x{4"))
    tryCatch(grepRaw(p, "abc"), error = function(e) NULL)
## read past the end of the pattern in R <= 4.6.2 (visible under ASan)


## integer literals outside the int range -- PR#19050
stopifnot(identical(suppressWarnings(str2lang("4294967296L")), 4294967296),
          identical(suppressWarnings(str2lang("0x80000000L")), 2147483648),
          identical(str2lang("2147483647L"), 2147483647L))
## converted out-of-range doubles to int (undefined behaviour) in R <= 4.6.2


## gsub() when the first replacement is longer than the initial buffer:
## empty input, or a long match repeated by back-references	-- PR#19273
local({
    r <- strrep("b", 2000)
    for(perl in c(FALSE, TRUE))
        stopifnot(identical(gsub("", r, "", perl = perl), r),
                  identical(gsub("(.*)", "\\1\\1\\1", strrep("a", 4000),
                                 perl = perl), strrep("a", 12000)))
    w <- strrep("\u00e9", 2000)
    stopifnot(identical(gsub("", w, ""), w))
})
## overflowed a heap buffer, typically aborting, in R <= 4.6.2
proc.time() - .pt; .pt <- proc.time()


## corrupt .Random.seed must not be used as is -- PR#19276
rk <- RNGkind()
msgs <- character()
saveW <- function(w) { msgs <<- c(msgs, conditionMessage(w))
		       invokeRestart("muffleWarning") }
set.seed(1); s <- .Random.seed
.Random.seed <- integer(0)
withCallingHandlers(runif(1), warning = saveW)
stopifnot(length(.Random.seed) == 626L)
s[2] <- -1000000000L # Mersenne-Twister position before the state array
.Random.seed <- s
stopifnot(runif(1) >= 0, .Random.seed[2] > 0L)
s[1] <- 10303L # user-supplied normal generator, none loaded
.Random.seed <- s
withCallingHandlers(rnorm(1), warning = saveW)
stopifnot(.Random.seed[1] %% 10000L %/% 100L != 3L)
stopifnot(length(msgs) == 2L,
          grepl("length 0",         msgs[1]),
          grepl("normal generator", msgs[2]))
RNGkind("L'Ecuyer-CMRG"); set.seed(1); s <- .Random.seed
s[5:7] <- 0L # second component all zero: the generator degenerates
.Random.seed <- s
runif(1)
stopifnot(any(.Random.seed[5:7] != 0L))
do.call(RNGkind, as.list(rk)) # revert
## read before the state array, called a NULL function pointer, and kept
## a degenerate L'Ecuyer-CMRG state in R <= 4.6.2


## rasterImage() with zero-length 'angle' or 'interpolate' -- PR#19284
local({
    r <- as.raster(matrix(0, 1, 1))
    pdf(NULL); on.exit(dev.off())
    plot.new()
    lisE <- list(tryCid(rasterImage(r, 0, 0, 1, 1, interpolate = NULL)),
                 tryCid(rasterImage(r, 0, 0, 1, 1, interpolate = logical())),
                 tryCid(rasterImage(r, 0, 0, 1, 1, angle = NULL)),
                 tryCid(rasterImage(r, 0, 0, 1, 1, angle = numeric())))
    stopifnot(vapply(lisE, inherits, NA, what = "error"))
    stopifnot(identical(
        vapply(lisE, conditionMessage, ""),
        sprintf(gettext("invalid '%s' argument", domain="graphics"),
                rep(c('interpolate', 'angle'), each=2))))
})
## divided by zero (SIGFPE on x86_64), or read out of bounds, in R <= 4.6.2


## deparse() of a negative constant as the left operand of ^, [, [[ or $ -- PR#19278
x <- -2
e <- bquote(.(x)^2)
stopifnot(identical(deparse(e), "(-2)^2"),
          identical(eval(parse(text = deparse(e))[[1]]), eval(e)),
          identical(deparse(call("^", -2L, 2)), "(-2L)^2"),
          identical(deparse(call("[", -1, 1)), "(-1)[1]"),
          ## unchanged: no parens where none are needed
          identical(deparse(call("^", 2, -2)), "2^-2"),
          identical(deparse(call("*", -1, 2)), "-1 * 2"),
          identical(deparse(call("-", -1)), "--1"),
          identical(deparse(call(":", -1, 2)), "-1:2"),
          identical(deparse(call("^", NA_integer_, 2)), "NA_integer_^2"),
          identical(deparse(call("^", NA_real_, 2)), "NA_real_^2"))
## gave "-2^2", which re-parses as -(2^2), in R <= 4.6.2


## format(<POSIXlt>, usetz = TRUE) appended the zone past the output buffer -- PR#19291
x <- as.POSIXlt("2024-01-15 12:30:00", tz = "Europe/London")
f44 <- strrep("a", 2044)
stopifnot(identical(nchar(format(x, f44, usetz = TRUE)), 2048L))
tools::assertError(format(x, paste0(f44, "a"), usetz = TRUE))
y <- as.POSIXlt("2024-01-15 12:30:00", tz = "UTC")
y$zone <- strrep("Z", 5000)
tools::assertError(format(y, "%Y", usetz = TRUE))
## overflowed a stack buffer, typically aborting, in R <= 4.6.2


## runmed(*, na.action = "na.omit") when the first NA is not x[1] -- PR#19300
x <- c(1, 5, 2, NA, 8, 3, 9)
for(i in 1:11)
  for(alg in c("Stuetzle", "Turlach"))
    stopifnot(exprs = {
        identical(c(runmed(x, 3, endrule = "keep", algorithm = alg, na.action = "na.omit")),
                  c(1, 2, 5, NA, 3, 8, 9))
        identical(c(runmed(c(1, 2, NA, 4), 1, algorithm = alg, na.action = "na.omit")),
                  c(1, 2, NA, 4))
    })
## used an uninitialized value in place of the one before the first NA, in R <= 4.6.2


## plotmath number atoms: the string from asChar() was not protected -- PR#19310
local({
    op <- options(scipen = 999) # 1e300 gets 301 digits, a separately malloc'ed CHARSXP
    pdf(NULL); on.exit({ dev.off(); options(op) })
    x <- as.expression(rep(1e300, 200))
    gctorture(TRUE)
    w <- strwidth(x, "inches")
    gctorture(FALSE)
    stopifnot(length(w) == 200L, w > 0, w == w[1])
})
## read freed memory (visible under ASan or valgrind) in R <= 4.6.2


## gregexpr() with strings that cannot be converted to wide characters -- PR#19301
local({
    n <- 30000 # 2n exceeds the default protect stack size
    x <- c(rep(rawToChar(as.raw(0xff)), n), intToUtf8(252))
    ## invalid UTF-8 fails conversion in any locale; the native "\xff"
    ## converts to U+007F in the C locale on Windows
    Encoding(x) <- "UTF-8"
    r <- suppressWarnings(gregexpr(".", x))
    stopifnot(identical(vapply(r, `[`, 0L, 1L), c(rep(-1L, n), 1L)))
})
## leaked two protect stack entries per such string, so this gave
## "protection stack overflow", in R <= 4.6.2

## readChar() on a raw vector holding 6-byte UTF-8 lead bytes
if (l10n_info()[["UTF-8"]]) {
    s <- readChar(as.raw(rep(0xff, 6 * 64)), 64L)
    invisible(gc())
    stopifnot(identical(nchar(s, "bytes"), 384L))
}
## wrote past its buffer where MB_CUR_MAX is 4 (macOS), seg.faulting, in R <= 4.6.2


## gc() accounting after releasing a long string that carries a hash value -- PR#19195
local({
    used <- function() gc()["Vcells", "used"]
    v0 <- used()
    errs <- lapply(1:8, function(i)
        tryCid(as.name(paste0(strrep("a", 20000), i))))
    stopifnot(vapply(errs, inherits, NA, what = "error"),
              abs(used() - v0) < 1e6)
})
## each rejected name was released as if its length were its hash value, so
## 'used' wrapped around and the vector heap limit stopped applying, in R <= 4.6.2



## 'next' as a call argument in a byte-compiled while()/repeat loop jumps back
## by longjmp, which skipped every check for interrupts and time limits
local({
    f <- compiler::cmpfun(function() {
        i <- 0L
        while((i <- i + 1L) <= 1e8L) invisible(next)
    })
    setTimeLimit(elapsed = 0.5, transient = TRUE)
    E <- tryCid(f())
    stopifnot(inherits(E, "error"),
              identical(conditionMessage(E),
                        gettext("reached elapsed time limit", domain="R")))
})
## ran all 1e8 iterations without checking in R <= 4.6.2


## invalid encodings in serialized data
wide <- if (.Platform$OS.type == "windows") "UTF-16LE" else
    paste0("UCS-4", if (.Platform$endian == "little") "LE" else "BE")
bad <- c(charToRaw(sprintf("A\n3\n6\n8\n%d\n%s16\n1\n9\n1\n",
			   nchar(wide), wide)), as.raw(0xff))
out <- suppressWarnings(unserialize(bad))
stopifnot(identical(charToRaw(out), as.raw(0xff)))
## error for bad ones
bad <- c(charToRaw("A\n3\n6\n8\n14\nUTF-8"), as.raw(0),
	 charToRaw("padding!254\n"))
assertErrV(unserialize(bad))
con <- rawConnection(bad)
assertErrV(infoRDS(con))
close(con)


## malformed serialized state of the base ALTREP classes
hdrlen <- length(serialize(NULL, NULL)) - 4L
item <- function(x) serialize(x, NULL)[-seq_len(hdrlen)]
altrepStream <- function(cls, type, state)
    c(serialize(NULL, NULL)[seq_len(hdrlen)],
      as.raw(c(0, 0, 0, 238)), # ALTREP_SXP
      item(pairlist(as.name(cls), as.name("base"), type)),
      item(state), item(NULL))
states <- list(wrap_integer = 1:5, wrap_integer = c(2L, 7L, 9L),
               wrap_integer = pairlist(1:3, 1L),
               deferred_string = 1:3, deferred_string = pairlist(1.5),
               mmap_integer = 1:3, mmap_integer = pairlist("x", 1, 13L))
types <- c(wrap_integer = 13L, deferred_string = 16L, mmap_integer = 13L)
errs <- Map(function(cls, state)
                tryCid(unserialize(altrepStream(cls, types[[cls]], state))),
            names(states), states)
stopifnot(vapply(errs, function(e) inherits(e, "error") &&
                 grepl("serialized state", conditionMessage(e)), NA))
x <- .Internal(wrap_meta(c(1L, 5L, 9L), 1L, 1L))
stopifnot(identical(unserialize(serialize(x, NULL)), x))
## built garbage objects or seg.faulted in R <= 4.6.2


## duplicated(), unique() and anyDuplicated() with marked encodings
## translated a long latin1 string repeatedly          -- PR#19329
setEnc <- `Encoding<-`
l1 <- setEnc("fa\xE7ile", "latin1") # short
u1 <- enc2utf8(l1)
long <- setEnc(strrep("\xE9", 5000), "latin1")
x <- c(long, rep(c("", "a", "b", l1), 500), u1, enc2utf8(long))
stopifnot(exprs = { # all these were true "forever":
    identical(unique(x), c(long, "", "a", "b", l1))
    identical(which(!duplicated(x)), 1:5)
    identical(which(!duplicated(x, fromLast = TRUE)), c(1998:2000, 2002:2003))
    identical(anyDuplicated(x), 6L)
    identical(anyDuplicated(x, fromLast = TRUE), 2001L)
    identical(duplicated(c(l1, u1, "a", "a"), incomparables = l1),
              c(FALSE, FALSE, FALSE, TRUE))
    identical(anyDuplicated(c(l1, "a", u1, "a"), incomparables = l1), 4L)
})
### Timing from `unique-latin1-translate-repro.R` (PR attachment 4082) --
## 1. A long latin1 string among many short ASCII strings.
p <- 200171L; n <- 2000L
long <- setEnc(paste0("\xE9", strrep("a", p-3), "#v"), "latin1")
short <- c("", vapply(32:126, function(i) rawToChar(as.raw(i)), ""))
n1 <- length(x <- c(long, rep(short, length.out = n)))
system.time(u <- unique(x)) # ~ 2 ms *after* patch; was ~ 1.3 sec
## 2. Strings that differ only in their first character all hash alike
rawtail <- rep(as.raw(0xe9), n)
lat <- vapply(c(letters, LETTERS),
              \(ch) rawToChar(c(charToRaw(ch), rawtail)), "", USE.NAMES = FALSE)
Encoding(lat) <- "latin1"
y <- rep(c(lat[1:26], enc2utf8(lat[27:52])), 200)
system.time(v <- unique(y)) # ~ 65 ms; was ~ 0.9 sec
stopifnot(length(u) == 97L, identical(nchar(u), c(p, 0:1, rep(1L, 94))),
          identical(nchar(v), rep(n1, 52)),
          identical(unique(substring(v, 2L,n1)),
                    setEnc(strrep("\xe9", n), "latin1")))
## times were around 1 sec each, in R <= 4.6.2



## keep at end
rbind(last =  proc.time() - .pt,
      total = proc.time())
