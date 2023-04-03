## This can only be done in a locale that extends Latin-1
(inf <- l10n_info())
if(!(inf$`UTF-8` || inf$`Latin-1`)) {
    warning("this test must be done in a Latin-1 or UTF-8 locale")
    q()
}

inp <- readLines(n = 2)
«Latin-1 accented chars»: éè øØ å<Å æ<Æ é éè
éè

inp
(txt <- iconv(inp[1], "latin1", ""))
(pat <- iconv(inp[2], "latin1", ""))
if(any(is.na(c(txt, pat)))) {
    ## backup test
    warning("this test must be done in a Latin-1 or UTF-8 locale")
    q()
}

testit <- function(x) {print(x); stopifnot(identical(x, 1L))}
testit(grep(pat, txt))
testit(grep(pat, txt, ignore.case = TRUE))
testit(grep(pat, txt, useBytes = TRUE))
testit(grep(pat, txt, ignore.case = TRUE, useBytes = TRUE))
testit(grep(pat, txt, fixed = TRUE))
testit(grep(pat, txt, fixed = TRUE, useBytes = TRUE))
testit(grep(pat, txt, perl = TRUE))
testit(grep(pat, txt, ignore.case = TRUE, perl = TRUE))
testit(grep(pat, txt, perl = TRUE, useBytes = TRUE))
testit(grep(pat, txt, ignore.case = TRUE, perl = TRUE, useBytes = TRUE))
testit(grep(toupper(pat), txt, ignore.case = TRUE))
testit(grep(toupper(pat), txt, ignore.case = TRUE, perl = TRUE))
## matches in Latin-1 but not in UTF-8
grep(toupper(pat), txt, ignore.case = TRUE, perl = TRUE, useBytes = TRUE)

(r1 <- regexpr("en", txt, fixed=TRUE))
(r2 <- regexpr("en", txt, fixed=TRUE, useBytes=TRUE))
stopifnot(identical(r1, regexpr("en", txt)))
stopifnot(identical(r2, regexpr("en", txt, useBytes = TRUE)))
stopifnot(identical(r1, regexpr("en", txt, perl=TRUE)))
stopifnot(identical(r2, regexpr("en", txt, perl=TRUE, useBytes=TRUE)))
stopifnot(identical(r1, regexpr("EN", txt, ignore.case=TRUE)))
stopifnot(identical(r2, regexpr("EN", txt, ignore.case=TRUE, useBytes=TRUE)))
stopifnot(identical(r1, regexpr("EN", txt, ignore.case=TRUE, perl=TRUE)))
stopifnot(identical(r2, regexpr("EN", txt, ignore.case=TRUE, perl=TRUE,
                                useBytes=TRUE)))

(r1 <- regexpr(pat, txt, fixed=TRUE))
(r2 <- regexpr(pat, txt, fixed=TRUE, useBytes=TRUE))
stopifnot(identical(r1, regexpr(pat, txt)))
stopifnot(identical(r2, regexpr(pat, txt, useBytes=TRUE)))
stopifnot(identical(r1, regexpr(pat, txt, perl=TRUE)))
stopifnot(identical(r2, regexpr(pat, txt, perl=TRUE, useBytes=TRUE)))
stopifnot(identical(r1, regexpr(pat, txt, ignore.case=TRUE)))
stopifnot(identical(r2, regexpr(pat, txt, ignore.case=TRUE, useBytes=TRUE)))
stopifnot(identical(r1, regexpr(pat, txt, ignore.case=TRUE, perl=TRUE)))
stopifnot(identical(r2, regexpr(pat, txt, ignore.case=TRUE, perl=TRUE,
                                useBytes=TRUE)))
pat2 <- toupper(pat)
stopifnot(identical(r1, regexpr(pat2, txt, ignore.case=TRUE)))
stopifnot(identical(r1, regexpr(pat2, txt, ignore.case=TRUE, perl=TRUE)))
## will not match in a UTF-8 locale
regexpr(pat2, txt, ignore.case=TRUE, perl=TRUE, useBytes=TRUE)


(r1 <- gregexpr(pat, txt, fixed=TRUE))
(r2 <- gregexpr(pat, txt, fixed=TRUE, useBytes=TRUE))
stopifnot(identical(r1, gregexpr(pat, txt)))
stopifnot(identical(r2, gregexpr(pat, txt, useBytes=TRUE)))
stopifnot(identical(r1, gregexpr(pat, txt, perl=TRUE)))
stopifnot(identical(r2, gregexpr(pat, txt, perl=TRUE, useBytes=TRUE)))
stopifnot(identical(r1, gregexpr(pat, txt, ignore.case=TRUE)))
stopifnot(identical(r2, gregexpr(pat, txt, ignore.case=TRUE, useByte=TRUE)))
stopifnot(identical(r1, gregexpr(pat, txt, ignore.case=TRUE, perl=TRUE)))
stopifnot(identical(r2, gregexpr(pat, txt, ignore.case=TRUE, perl=TRUE,
                                 useBytes=TRUE)))

txt2 <- c("The", "licenses", "for", "most", "software", "are",
  "designed", "to", "take", "away", "your", "freedom",
  "to", "share", "and", "change", "it.",
   "", "By", "contrast,", "the", "GNU", "General", "Public", "License",
   "is", "intended", "to", "guarantee", "your", "freedom", "to",
   "share", "and", "change", "free", "software", "--",
   "to", "make", "sure", "the", "software", "is",
   "free", "for", "all", "its", "users")
( i <- grep("[gu]", txt2, perl = TRUE) )
stopifnot(identical(i, grep("[gu]", txt2)))
## results depend on the locale
(ot <- sub("[b-e]",".", txt2, perl = TRUE))
txt2[ot != sub("[b-e]",".", txt2)]
(ot <- sub("[b-e]",".", txt2, ignore.case = TRUE, perl = TRUE))
txt2[ot != sub("[b-e]",".", txt2, ignore.case = TRUE)]


## These may end up with different encodings: == copes, identical does not

eq <- function(a, b) a == b
(r1 <- gsub(pat, "ef", txt))
stopifnot(eq(r1, gsub(pat, "ef", txt, useBytes = TRUE)))
stopifnot(eq(r1, gsub(pat, "ef", txt, fixed = TRUE)))
stopifnot(eq(r1, gsub(pat, "ef", txt, fixed = TRUE, useBytes = TRUE)))
stopifnot(eq(r1, gsub(pat, "ef", txt, perl = TRUE)))
stopifnot(eq(r1, gsub(pat, "ef", txt, perl = TRUE, useBytes = TRUE)))

pat <- substr(pat, 1, 1)
(r1 <- gsub(pat, "gh", txt))
stopifnot(eq(r1, gsub(pat, "gh", txt, useBytes = TRUE)))
stopifnot(eq(r1, gsub(pat, "gh", txt, fixed = TRUE)))
stopifnot(eq(r1, gsub(pat, "gh", txt, fixed = TRUE, useBytes = TRUE)))
stopifnot(eq(r1, gsub(pat, "gh", txt, perl = TRUE)))
stopifnot(eq(r1, gsub(pat, "gh", txt, perl = TRUE, useBytes = TRUE)))


stopifnot(identical(gsub("a*", "x", "baaac"), "xbxcx"))
stopifnot(identical(gsub("a*", "x", "baaac"), "xbxcx"), perl = TRUE)
stopifnot(identical(gsub("a*", "x", "baaac"), "xbxcx"), useBytes = TRUE)
stopifnot(identical(gsub("a*", "x", "baaac"), "xbxcx"), perl = TRUE, useBytes = TRUE)

## this one seems system-dependent
(x <- gsub("\\b", "|", "The quick brown \ue8\ue9", perl = TRUE))
# stopifnot(identical(x, "|The| |quick| |brown| |\ue8\ue9|"))
(x <- gsub("\\b", "|", "The quick brown fox", perl = TRUE))
stopifnot(identical(x, "|The| |quick| |brown| |fox|"))
## The following is warned against in the help page, but worked in some versions
gsub("\\b", "|", "The quick brown fox")

(z <- strsplit(txt, pat)[[1]])
stopifnot(eq(z, strsplit(txt, pat, useBytes = TRUE)[[1]]))
stopifnot(eq(z, strsplit(txt, pat, fixed = TRUE)[[1]]))
stopifnot(eq(z, strsplit(txt, pat, fixed = TRUE, useBytes = TRUE)[[1]]))
stopifnot(eq(z, strsplit(txt, pat, perl = TRUE)[[1]]))
stopifnot(eq(z, strsplit(txt, pat, perl = TRUE, useBytes = TRUE)[[1]]))

(z <- strsplit(txt, "[a-c]")[[1]])
stopifnot(eq(z, strsplit(txt, "[a-c]", useBytes = TRUE)[[1]]))
stopifnot(eq(z, strsplit(txt, "[a-c]", perl = TRUE)[[1]]))
stopifnot(eq(z, strsplit(txt, "[a-c]", perl = TRUE, useBytes = TRUE)[[1]]))

## strsplit did not useBytes correctly in POSIX mode in R < 4.2 as
## MBCS would still be interpreted
x <- "\xf1\xa1_\xc5\xa2"
split.a <- list(c("\xf1\xa1", "\xc5\xa2"))
split.b <- list(c("\xf1", "_\xc5\xa2"))
stopifnot(identical(strsplit(x, "_", useBytes=TRUE, perl=TRUE), split.a),
          identical(strsplit(x, "\xa1", useBytes=TRUE, perl=TRUE), split.b),
          identical(strsplit(x, "_", useBytes=TRUE), split.a),
          identical(strsplit(x, "\xa1", useBytes=TRUE), split.b))

## strsplit in R < 4.2 marked outputs when using bytes (it shouldn't)
x <- xb <- xu <- "\U1F600"
Encoding(xb) <- "bytes"
Encoding(xu) <- "unknown"
x98 <- "\x98"
Encoding(x98) <- "bytes"
split.a <- c("\xf0", "\x9f", "\x98", "\x80")
Encoding(split.a) <- "unknown"
split.b <- c("\xf0", "\x98")
Encoding(split.b) <- "unknown"
split.c <- c("\xf0\x9f", "\x80")
Encoding(split.c) <- "unknown"
## Are two character vectors truly identical?
identichr <- function(x, y) {
    if (is.character(x) &&
        is.character(y) &&
        identical(Encoding(x), Encoding(y))
    ) {
        Encoding(x) <- "bytes"
        Encoding(y) <- "bytes"
        identical(x, y)
    } else FALSE
}
stopifnot(
    identichr(strsplit(x, "[\x80\x9f]", useBytes=TRUE)[[1]], split.b),
    identichr(strsplit(x, "[\x80\x9f]", useBytes=TRUE, perl=TRUE)[[1]], split.b),
    identichr(strsplit(x, "\x98", useBytes=TRUE, fixed=TRUE)[[1]], split.c))
if(l10n_info()[['Latin-1']]) {
    xl <- x
    Encoding(xl) <- "latin1"
    stopifnot(Encoding(strsplit(xl, "", useBytes=TRUE)[[1]]) == "unknown",
              Encoding(strsplit(xl, "")[[1]]) == "latin1",
              Encoding(strsplit(xl, "\x98")[[1]]) == "latin1",
              Encoding(strsplit(xl, x98)[[1]]) == "bytes")
}

## from strsplit.Rd
z <- strsplit("A text I want to display with spaces", NULL)[[1]]
stopifnot(identical(z,
                    strsplit("A text I want to display with spaces", "")[[1]]))

x <- c(as = "asfef", qu = "qwerty", "yuiop[", "b", "stuff.blah.yech")
(z <- strsplit(x, "e"))
stopifnot(identical(z, strsplit(x, "e", useBytes = TRUE)))
stopifnot(identical(z, strsplit(x, "e", fixed = TRUE)))
stopifnot(identical(z, strsplit(x, "e", fixed = TRUE, useBytes = TRUE)))
stopifnot(identical(z, strsplit(x, "e", perl = TRUE)))
stopifnot(identical(z, strsplit(x, "e", perl = TRUE, useBytes = TRUE)))

## moved from reg-tests-1b.R.
## fails to match on Cygwin, Mar 2011
## regexpr(fixed = TRUE) with a single-byte pattern matching to a MBCS string
x <- iconv("fa\xE7ile a ", "latin1", "UTF-8")
stopifnot(identical(regexpr(" ", x), regexpr(" ", x, fixed=TRUE)))
# fixed=TRUE reported match position in bytes in R <= 2.10.0
stopifnot(identical(regexpr(" a", x), regexpr(" a", x, fixed=TRUE)))
## always worked.

## this broke and segfaulted in 2.13.1 and earlier (PR#14627)
x <- paste(rep("a ", 600), collapse="")
testit(agrep(x, x))
testit(agrep(x, x, max.distance=0.5))

## this is used in QC to check dependencies and was broken intermittently by TRE changes
stopifnot(isTRUE(grepl('^[[:space:]]*(R|[[:alpha:]][[:alnum:].]*[[:alnum:]])([[:space:]]*\\(([^) ]+)[[:space:]]+([^) ]+)\\))?[[:space:]]*$', ' R (>= 2.13.0) ')))

## Bad sub() and gsub() with some regexprs PR#16009
x <- c(NA, "  abc", "a b c    ", "a  b c")
(y <- gsub("\\s{2,}", " ", x))
stopifnot(y[-1] == c(" abc", "a b c ", "a b c"))
x <- c("\ue4", "  abc", "a b c    ", "a  b c")
(y <- gsub("\\s{2,}", " ", x))
stopifnot(y == c(x[1], " abc", "a b c ", "a b c"))
## results were c(x[1], " ", " ", " ") in both cases in R 3.1.1

## Bad mapping of code points to characters with surrogate pairs (in R 4.0)
stopifnot(regexpr("b", "\U0001F937b", perl = TRUE) == 2)

## Mixed MBCS and "bytes" encoded, regression in r73569 (Bugzilla 18021)
x <- rep("\u00e9ab", 2)
Encoding(x[2]) <- "bytes"
res <- c("a", "a")
stopifnot(identical(regmatches(x, regexpr("a", x, perl=TRUE)), res),
          identical(regmatches(x, regexpr("a", x)), res),
          identical(unlist(regmatches(x, regexpr("a", x, perl=TRUE))), res),
          identical(unlist(regmatches(x, regexpr("a", x))), res),
          identical(unlist(regmatches(x, regexec("a", x, perl=TRUE))), res),
          identical(unlist(regmatches(x, regexec("a", x))), res),
          ## Fixed = TRUE
          identical(regmatches(x, regexpr("a", x, fixed=TRUE)), res),
          identical(unlist(regmatches(x, regexpr("a", x, fixed=TRUE))), res),
          identical(unlist(regmatches(x, regexec("a", x, fixed=TRUE))), res))

## Bytes index computation on ASCII used as "character" on non-ASCII
## Identical itself produces error if we end up with byte encoded
## values, which is what we're trying to avoid.

x <- rep("eab", 2)
y <- c("eab", "e\u03b1b")
res <- c("a", "\u03b1")
stopifnot(identical(regmatches(y, regexpr("a", x)), res),
          identical(regmatches(y, regexpr("a", x, perl=TRUE)), res),
          identical(unlist(regmatches(y, gregexpr("a", x))), res),
          identical(unlist(regmatches(y, gregexpr("a", x, perl=TRUE))), res),
          identical(unlist(regmatches(y, regexec("a", x))), res),
          identical(unlist(regmatches(y, regexec("a", x, perl=TRUE))), res),
          ## Fixed = TRUE
          identical(regmatches(y, regexpr("a", x, fixed=TRUE)), res),
          identical(unlist(regmatches(y, gregexpr("a", x, fixed=TRUE))), res),
          identical(unlist(regmatches(y, regexec("a", x, fixed=TRUE))), res))

## This is an adapted `gregexec` implementation from the example of `?grep`.
## We will use it to test `gregexec`.
ex_fn <- function(pattern, text, useBytes = FALSE, perl = FALSE) {
    lapply(
        regmatches(
            text,
            gregexpr(pattern, text, useBytes = useBytes, perl = perl)
        ),
        function(e) {
            pos <- regexec(pattern, e, useBytes = useBytes, perl = perl)
            res <- regmatches(e, pos)
            if(length(res)) do.call(cbind, res) else character()
        }
    )
}

## Captures patterns like LETTERS123 (plus a couple of Unicode chars). 
p.1.raw <- "(?:.* )?(%s[[:alpha:]\u00e9\u00d6]+)(%s[[:digit:]]+)(?: .*)?"
p.1 <- sprintf(p.1.raw, "", "")
p.1n <- sprintf(p.1.raw, "?<a>", "?<b>")   ## named capture groups
s.utf8 <-  "H\u00e9320+W\u00d641"
s.1 <- c(
    "Test: A1-BC23 boo", ## matches and extra
    "DE35",              ## one full match
    "boo",               ## nomatch
    NA,                  ## NA
    s.utf8               ## UTF8 string
)
gr         <- gregexec(p.1, s.1, perl=FALSE)
gr.ub      <- gregexec(p.1, s.1, perl=FALSE, useBytes=TRUE)
gr.perl    <- gregexec(p.1n, s.1, perl=TRUE) 
gr.perl.ub <- gregexec(p.1n, s.1, perl=TRUE, useBytes=TRUE) 

m.gr       <- regmatches(s.1, gr)
m.gr       # inspect visually

m.gr.ub    <- regmatches(s.1, gr.ub)
Encoding(m.gr.ub[[5L]]) <- "UTF-8"
m.gr.ub.ex <- ex_fn(p.1, s.1, perl=FALSE, useBytes=TRUE)
Encoding(m.gr.ub.ex[[5L]]) <- "UTF-8"

## Named captures
m.by.name <- do.call(cbind, regmatches(s.1, gr.perl))
m.by.name.1 <- do.call(cbind, regmatches(s.1, regexec(p.1n, s.1, perl=TRUE)))

stopifnot(
    ## Compare to ?grep example function
    identical(m.gr, ex_fn(p.1, s.1, perl=FALSE)),
    identical(m.gr.ub, m.gr.ub.ex),
    identical(regmatches(s.1, gr.perl), ex_fn(p.1n, s.1, perl=TRUE)),
    identical(regmatches(s.1, gr.perl.ub),
              ex_fn(p.1n, s.1, perl=TRUE, useBytes=TRUE)),
    ## Byte matching increments faster, but matches the same
    all(gr.ub[[5L]] - gr[[5L]] == c(0L, 0L, 1L, 1L, 1L, 2L)),
    identical(m.gr, m.gr.ub),
    ## Perl and non-Perl match the same (in this case)
    identical(m.gr, regmatches(s.1, gregexec(p.1, s.1, perl=TRUE))),
    ## Check perl actually using TRE (no named capture support)
    inherits(try(gregexec(p.1n, s.1), silent=TRUE), "try-error"),
    ## Named groups work
    identical(gr.perl[[1]]["b",], c(8L, 12L)),
    ## Corner cases
    identical(gregexec(p.1, character()), list()),
    identical(gregexec(p.1n, character(), perl=TRUE), list()),
    identical(gregexec(p.1, NULL), list()),
    identical(gregexec(p.1n, NULL, perl=TRUE), list()),
    ## Named capture carry over to matches
    identical(m.by.name["a",], c("A", "BC", "DE", "H\u00e9", "W\u00d6")),
    identical(m.by.name["b",], c("1", "23", "35", "320", "41")),
    identical(m.by.name.1["a",], c("A", "DE", "H\u00e9")),
    identical(m.by.name.1["b",], c("1", "35", "320"))
)

## Invert and `regmatches<-` do not work with overlapping captures,
## but should work if we drop the full match from our data.
drop_first_capt <- function(x) {
    ml <- attr(x, 'match.length')[-1L,]
    x <- x[-1L,]
    attr(x, 'match.length') <- ml
    x
}

## Replace with lower case and multiply nums by 100
s.2 <- s.2a <- s.1[c(1L,5L)]
gr.2 <- lapply(gregexec(p.1, s.2), drop_first_capt)
m.gr.2 <- regmatches(s.2, gr.2)
replacement <- lapply(m.gr.2, tolower)
replacement[[1]][2,] <- as.numeric(replacement[[1]][2,]) * 100
replacement[[2]][2,] <- as.numeric(replacement[[2]][2,]) * 100
s.2a <- s.2
regmatches(s.2a, gr.2) <- replacement

## Replace with `invert=TRUE`
s.2b <- s.2
regmatches(s.2b, gr.2, invert=TRUE) <- 
    replicate(2L, c("~", "#", "~", "@", "~"), simplify=FALSE)

stopifnot(
    identical(regmatches(s.2, gr.2, invert=TRUE), 
              list(c("Test: ", "", "-", "", " boo"), c("", "", "+", "", ""))),
    identical(s.2a, c("Test: a100-bc2300 boo", "h\u00e932000+w\u00f64100")),
    identical(s.2b, c("~A#1~BC@23~", "~H\u00e9#320~W\u00d6@41~")))

## Check that the perl switch is working fully (h/t Michael Chirico)
pat <- "(?<first>\\d+)"
gregexec(pat, "123 456", perl=TRUE)
## TRE does not support name capts
stopifnot(inherits(try(gregexec(pat, "123 456", perl=FALSE)), "try-error"))
local({
    old.warn <- options(warn = 2)
    on.exit(options(old.warn))
    gregexec("123", "123 456", fixed=TRUE) # No warning with perl=FALSE
})
