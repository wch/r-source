## Encoding-related regression tests (initially from reg-tests-1d.R)

str(INFO <- l10n_info())
UTF8 <- INFO[["UTF-8"]]
LATIN1OR9 <- !UTF8 && (
    INFO[["Latin-1"]] ||
    switch(.Platform$OS.type,
           windows = identical(INFO[["codepage"]], 28605L),
           unix = tolower(gsub("-", "", INFO[["codeset"]], fixed = TRUE)) == "iso885915")
)
if(!(UTF8 || LATIN1OR9) ||
   identical(localeToCharset(), "ASCII")) {
    ## checking localeToCharset() because on Windows, in C locale,
    ## l10n_info() would report Latin-1 when that is the code page
    message("SKIPPED: these tests need a UTF-8 or Latin-1 or Latin-9 locale")
    q("no")
}


### BEGIN FROM reg-tests-1d.R

## Overriding encoding in parse()
x8 <- "'\uf6'"
x8.2 <- substr(x8, 2, 2)
stopifnot(identical(Encoding(x8), "UTF-8"))
f8 <- tempfile()
writeLines(x8, f8, useBytes=TRUE) # save in UTF-8
##
chk_x82 <- function(x) stopifnot(identical(Encoding(x), "UTF-8"),
                                 identical(x, x8.2))
## parse(*, encoding = "UTF-8", ..) :
for(FF in c(function(.) parse(text=., encoding="UTF-8", keep.source=TRUE),
            function(.) parse(text=., encoding="UTF-8", keep.source=FALSE)
            )) {
    x <- eval(FF(x8))
    chk_x82(x)
}
for(K.S in c(TRUE, FALSE)) {
    x <- eval(parse(file=f8, encoding="UTF-8", keep.source = K.S))
    chk_x82(x)
}
## latin1 <--> UTF-8
xl <- iconv(x8, from="UTF-8", to="latin1")
stopifnot(identical(Encoding(xl), "latin1"))
stopifnot(identical(x8, iconv(xl, from="latin1", to="UTF-8")))
if (UTF8) {
    for(x in c(eval(parse(text=x8)),
               eval(parse(text=xl, keep.source=TRUE)),
               eval(parse(text=xl, keep.source=FALSE)),
               eval(parse(file=f8)),
               str2lang(x8),
               str2expression(x8)))
        stopifnot(identical(x, x8.2))
}
if (LATIN1OR9) {
    for(x in c(eval(parse(text=xl)),
               eval(parse(text=x8, keep.source=TRUE)),
               eval(parse(text=x8, keep.source=FALSE)),
               str2lang(x8),
               str2expression(x8)))
        stopifnot(identical(x, x8.2))
}
## parse(text=xl) had failed w/ "EOF whilst reading MBCS char at line 2"


## UTF-8 truncation tests
if (UTF8) {
    ## These tests fail on R < 4.0

    ## Use .Internal(seterrmessage(old.err)) to trigger truncation via
    ## Rsnprintf (mbcsTruncateToValid).
    trunc_string <- function(x) {
        old.err <- geterrmessage()
        on.exit(.Internal(seterrmessage(old.err)))
        unname(
            vapply(
                x,
                function(y) {
                    .Internal(seterrmessage(y))
                    geterrmessage()
                },
                ""
            )
        )
    }
    ## limits to detect the internal buffer size for truncation (now 8192)
    buff.min <- 8
    buff.max <- 7e4  # > buff.min
    buff.size <- nchar(
        trunc_string(paste0(rep(0:9, length.out = buff.max), collapse="")),
        type='bytes'
    )
    stopifnot(buff.size >= buff.min + 1)
    if(buff.size == buff.max) {
        ## possibly, the buffer is no longer fixed size?
        warning('BUFSIZE too large for UTF-8 truncation test?')
    } else {
        string.base <- paste0(
            rep(0:9, length.out = buff.size),
            collapse=""
        )
        ## Append UTF-8 sequences at the end of strings that are just
        ## a bit shorter than the buffer, each one byte longer than the
        ## previous.
        string.starts <- substr(
            rep(string.base, 6), 1,
            nchar(string.base) - seq(buff.min, 3, -1)
        )
        ## For each of the increasing length string, append 2, 3, and 4 byte
        ## (valid) UTF-8 characters.
        string.ends <- rep(
            c(
                '\u00A2',            # <C2><A2>           (cent symbol)
                '\u20AC',            # <E2><82><AC>       (euro symbol)
                '\U00010348',        # <F0><90><8D><88>   (circle with dot)
                NULL
            ),
            each=length(string.starts)
        )
        strings <- paste0(
            string.starts,
            '\U0001F600',  # 4 byte grinning face, extra padding char
            string.ends
        )
        output <- trunc_string(strings)
        stopifnot(validUTF8(strings)) # sanity check
        stopifnot(validUTF8(output))
    }

    ## These tests fail on R < 4.1
    ##
    ## Checking that truncation and `...` concatenation are working
    ## correctly in verrorcall_dflt.  Prior to 4.1 truncation detection did
    ## not work with call set, and multibyte characters could be mangled by
    ## the `...`.
    ##
    ## We assume getttext strings are not translated (or are translated
    ## to the same byte-length as the ones in source).

    ## We cannot use `tryCatch` as we're testing the C-level error construction
    ## and that is not invoked when signalled errors are caught, hence:
    capt_err_msg <- function(expr) {
        tmp <- tempfile()
        on.exit(unlink(tmp))
        err.con <- getConnection(sink.number(type='message'))
        sink(file(tmp, 'w'), type='message')
        withRestarts(expr, abort=function() sink(err.con, type='message'))
        ## add back newlines consumed by readlines; we assume a trailing one
        ## exists, if it doesn't readLines will issue a warning
        paste0(c(readLines(tmp), ""), collapse="\n")
    }
    ## Generate errors with long messages (length buff.size + overflow), ending
    ## in `x`, to test truncation.  Will need to be updated if buff.size is
    ## increased.  Function names / etc. are all carefully counted.
    long_error <- function(x, overflow=0, buff.size=8192) {
        overflow <- as.integer(overflow)
        x <- paste0(as.character(x), collapse="")

        ## Compute how many chars needed to fill buffer
        call.len <- 51   # nchar of a_really...(stop(x)) - see below
        extra.len <- 12  # "Error in  : "
        extra.ws <- 3    # +2 spaces +1 \n from `tail`
        chars.left <- buff.size - call.len - extra.len - extra.ws
        chars <- nchar(x, type = 'bytes')
        pad.chars <- chars.left - chars + as.integer(overflow)
        stopifnot(pad.chars >= 0)
        err.msg <- paste0(paste0(rev(rep_len(rev(LETTERS), pad.chars)),
                                 collapse = ""), x)
        ## force truncation despite 8170 warn length limit
        old.opt <- options(warning.length = 8170, warn=2)
        on.exit(options(old.opt))
        a_really_long_function_to_cause_truncation <- function(x) x
        f <- function(x)
            a_really_long_function_to_cause_truncation(stop(x))
        ## trigger error and capture std.err
        capt_err_msg(f(err.msg))
    }
    buff.size.2 <- buff.size + 1     # .Internal(seterrmessage) drops 1 byte

    ## 2 byte and 4 byte utf-8 encoded chars, avoid code points between \u00a0
    ## and \u0100 as some iconv implementations will translate them into char
    ## values in those ranges instead of into "<U+...>" in C locales.
    utf8.test <- '\u0238\U00010348'

    if(buff.size.2 != 8192) {
        warning('These tests assume BUFSIZE = 8192')
    } else {
        ## Mangled multibyte in R < 4.1
        stopifnot(validUTF8(long_error(utf8.test, overflow=-1)))

        ## Truncation detection fails in R < 4.1, so newline isn't appended, so
        ## we get a "incomplete final line" warning (converted to error)
        long_error(utf8.test, overflow=0)

        overflow <- c(
             -6,   # Buffer unambiguosly unfilled for MB_CUR_MAX=6
             -5,   # Buffer maybe filled for MB_CUR_MAX=6
             -4,   # Buffer full with '...\n\0'
             -3,   # Lose 4 byte UTF-8 char
             -2,
             -1,
              0,   # 4 byte UTF-8 char exactly replaced by '...\n', buffer full
              1,   # Lose 2 byte UTF-8 char
              2,
              3,   # Lose first non UTF-8
            # These will need to change if R_ConciseTraceback changes
            -87,   # Room for traceback; options(showErrorCalls=TRUE)
            -86    # No room for traceback.
        )
        le.res <- vapply(overflow, long_error, character(1),
                         buff.size = buff.size.2, x = utf8.test)
        stopifnot(validUTF8(utf8.test))  # sanity check
        stopifnot(validUTF8(le.res))

        ## # For first one, before truncation test, we've used 8186 bytes, so we
        ## # know there was no truncation.  Code adds a trailing newline, which
        ## # is why we get 8187.  For the second, we add one byte to the
        ## # message, which puts us in maybe-truncated state, which adds 3 more
        ## # bytes via with "...", so total of 8187 + 1 + 3 == 8191.
        ## le.res.nc <- nchar(le.res)
        ## data.frame(overflow,
        ##            bytes=nchar(le.res, type='bytes'),
        ##            snippet=substr(le.res, le.res.nc - 5, le.res.nc))
        ##
        ##    overflow bytes snippet
        ## 1        -6  8187 XYZȸ𐍈\n
        ## 2        -5  8191 ȸ𐍈...\n
        ## 3        -4  8192 ȸ𐍈...\n
        ## 4        -3  8189 Zȸ...\n
        ## 5        -2  8190 Zȸ...\n
        ## 6        -1  8191 Zȸ...\n
        ## 7         0  8192 Zȸ...\n
        ## 8         1  8191 YZ...\n
        ## 9         2  8192 YZ...\n
        ## 10        3  8192 XY...\n
        ## 11      -87  8192 ation\n
        ## 12      -86  8107 XYZȸ𐍈\n
        ## test recursive errors in handler, Fails R < 4.0

        handler_error <- function(x, overflow=0, buff.size=8192) {
            overflow <- as.integer(overflow)
            x <- paste0(as.character(x), collapse="")
            pad.chars <- buff.size - nchar(x, type='bytes') + overflow
            err.msg <- paste0(
                paste0(rev(rep_len(rev(LETTERS), pad.chars)), collapse=""), x
            )
            old.opt <- options(
                error=function(...) {
                    options(error=old.opt[['error']])
                    stop(err.msg)
                }
            )
            capt_err_msg(stop('initial error'))
        }
        handler.error.trunc <- vapply(
            c(0, 1, 5), handler_error, x=utf8.test, "", buff.size=buff.size.2
        )
        stopifnot(validUTF8(handler.error.trunc))

        ## Test when warning.length is limiting

        short_error <- function(call.=TRUE) {
            old.opt <- options(warning.length=100)
            on.exit(old.opt)
            f <- function()
                stop(paste0(rep_len(0:9, 110), collapse=""), call.=call.)
            capt_err_msg(f())
        }
        ## trailing newline adds 1
        stopifnot(nchar(short_error(call.=FALSE)) == 101L)
    }

    ## PrintGenericVector truncations
    ##
    ## New printing in r78508 needs to account for UTF-8 truncation
    grin <- "\U0001F600"
    lc1 <- paste0(c(rep(LETTERS, length.out=110), grin), collapse="")
    lc2 <- paste0(c(rep(LETTERS, length.out=111), grin), collapse="")
    list.mats <- list(matrix(list(structure(1:2, class=lc1))),
                      matrix(list(structure(1:2, class=lc2))))

    ## Allowed UTF-8 truncation in R < 4.1
    ls1 <- paste0(c(rep(0:9, length.out=95), "\U0001F600"), collapse="")
    ls2 <- paste0(c(rep(0:9, length.out=96), "\U0001F600"), collapse="")
    long.strings <- list(matrix(list(ls1)), matrix(list(ls2)))

    ## Invalid UTF-8 output as "\xf0\x9f..." so needs to be parsed to un-escape
    capt_parse <- function(x) {
        out <- capture.output(print(x))
        eval(parse(text=paste0(c('c(', sprintf("'%s',", out), 'NULL)'),
                               collapse=""))[[1]])
    }
    capt.parsed <- unlist(lapply(c(list.mats, long.strings), capt_parse))
    stopifnot(validUTF8(capt.parsed))

    ## Allowed MBCS truncation in R < 4.1
    fmt <- paste0(c(rep_len("a", 253), "\U0001f600"), collapse="")
    stopifnot(validUTF8(format(as.POSIXlt('2020-01-01'), fmt)))

    f <- file(paste0(c(rep_len("a", 992), "\U0001F600"), collapse=""))
    suppressWarnings(g <- gzcon(f))
    stopifnot(!grepl("xf0", capture.output(g)[2]))

}


## TRE grep with unflagged UTF-8 regex
if (UTF8) {
    x <- "d\xc3\xa9faut" # "défaut" flagged as native
    stopifnot(grepl("d.faut", x)) # incorrectly FALSE in R < 4.1
}


## in 4.1.0, encodeString() below would return unflagged UTF-8
## representation of the string
if (LATIN1OR9) {
    y <- "\xfc"
    stopifnot(y == encodeString(y))
}

## END FROM reg-tests-1d.R


## str() -- moved from reg-tests-3.R
cc <- "J\xf6reskog" # valid in "latin-1"; invalid multibyte string in UTF-8
.tmp <- capture.output(
    str(cc) # failed in some R-devel versions
)
stopifnot(grepl("chr \"J.*reskog\"", .tmp))


## source() with multiple encodings -- moved from reg-tests-1e.R
writeLines('x <- "fa\xE7ile"', tf <- tempfile(), useBytes = TRUE)
tools::assertError(source(tf, encoding = "UTF-8"))
source(tf, encoding = c("UTF-8", "latin1"))
## in R 4.2.{0,1} gave Warning (that would now be an error):
##   'length(x) = 2 > 1' in coercion to 'logical(1)'
if (UTF8) stopifnot(identical(Encoding(x), "UTF-8"))

## Check that UTF-16 with BOM can be read from a connection.  This tests a
## work-around in R for a bug in libiconv-86 on macOS (at least since
## libiconv-107).
words <- c(0xfeff, 0x30+c(1:9,0,1:9,0), 0x0a) # bom + 12345678901234567890 + newline
hi <- as.raw(words %/% 0x100)
low <- as.raw(words %% 0x100)
be <- c(rbind(hi, low))
befile <- tempfile("be_", fileext=".txt")
writeBin(be, befile)
becon <- file(befile, encoding = "UTF-16", open="r")
stopifnot(identical(readLines(becon), "12345678901234567890"))
close(becon)
le <- c(rbind(low, hi))
lefile <- tempfile("le_", fileext=".txt")
writeBin(le, lefile)
lecon <- file(lefile, encoding = "UTF-16", open="r")
stopifnot(identical(readLines(lecon), "12345678901234567890"))
close(lecon)

## Test that this doesn't crash to test a work-around in R for a bug in
## libiconv-86 on macOS.
r <- charToRaw("Hello world")
r[3] <- as.raw(0xfc)  # invalid
iconv(list(r), "", "", sub = "byte")

## Test substitution of invalid bytes in iconv() with UTF-16 input.  As of R
## 4.5, the input should advance by code unit size (two bytes, not one)
## when an invalid byte is encountered. Also, running into invalid bytes
## should not let libiconv forget about the byte-order specified via BOM.
r8 <- charToRaw("Hello world")
r16 <- c(as.raw(0xff), as.raw(0xfe), rbind(r8, as.raw(0)))  # little-endian
r16[7] <- as.raw(0x00)  # invalid (unpaired surrogate)
r16[8] <- as.raw(0xd8)
stopifnot(identical(iconv(list(r16), "UTF-16", "UTF-8", sub="byte"),
          "He<00><d8>lo world"))


## Using a __unicode__ decimal mark is fine :
op <- options(OutDec = "·", scipen = 1)
x <- pi* 10^(-6:5)
fx <- sapply(x, format)
print(fx, width=88, quote=FALSE) # 3·141593e-06 0·00003141593 0·0003141593 ....
options(OutDec = ".") # back to normal
stopifnot(grepl("·", fx, fixed=TRUE),
          identical(sub("·", ".", fx), sapply(x, format)))
options(op)
