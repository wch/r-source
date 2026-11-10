####  Regression Tests for bugs typically found via "fuzzing", e.g. OSS-Fuzz
####  -------------------------------------------------------
####  Strict tests, but only run from 'make check-all' ("once fixed should remain good")
###                                  ____________________
.pt <- proc.time()
tryCid <- function(expr) tryCatch(expr, error = identity)
assertErrV  <- function(...) tools::assertError  (..., verbose=TRUE)
assertWarnV <- function(...) tools::assertWarning(..., verbose=TRUE)

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




## keep at end
rbind(last =  proc.time() - .pt,
      total = proc.time())
