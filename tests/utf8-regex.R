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

testit <- function(x) {print(x); stopifnot(identical(x, 1L))}
testit(grep(pat, txt))
testit(grep(pat, txt, fixed = TRUE))
testit(grep(pat, txt, fixed = TRUE, useBytes = TRUE))
testit(grep(pat, txt, perl = TRUE))
testit(grep(pat, txt, ignore.case = TRUE, perl = TRUE))
testit(grep(pat, txt, perl = TRUE, useBytes = TRUE))
testit(grep(pat, txt, ignore.case = TRUE, perl = TRUE, useBytes = TRUE))
testit(grep(toupper(pat), txt, ignore.case = TRUE))
## the next two match in Latin-1 but not in UTF-8
grep(toupper(pat), txt, ignore.case = TRUE, perl = TRUE)
grep(toupper(pat), txt, ignore.case = TRUE, perl = TRUE, useBytes = TRUE)

(r1 <- regexpr("en", txt, fixed=TRUE))
(r2 <- regexpr("en", txt, fixed=TRUE, useBytes=TRUE))
stopifnot(identical(r1, regexpr("en", txt)))
stopifnot(identical(r1, regexpr("en", txt, perl=TRUE)))
stopifnot(identical(r2, regexpr("en", txt, perl=TRUE, useBytes=TRUE)))
stopifnot(identical(r1, regexpr("EN", txt, ignore.case=TRUE)))
stopifnot(identical(r1, regexpr("EN", txt, ignore.case=TRUE, perl=TRUE)))
stopifnot(identical(r2, regexpr("EN", txt, ignore.case=TRUE, perl=TRUE,
                                useBytes=TRUE)))

(r1 <- regexpr(pat, txt, fixed=TRUE))
(r2 <- regexpr(pat, txt, fixed=TRUE, useBytes=TRUE))
stopifnot(identical(r1, regexpr(pat, txt)))
stopifnot(identical(r1, regexpr(pat, txt, perl=TRUE)))
stopifnot(identical(r2, regexpr(pat, txt, perl=TRUE, useBytes=TRUE)))
stopifnot(identical(r1, regexpr(pat, txt, ignore.case=TRUE)))
stopifnot(identical(r1, regexpr(pat, txt, ignore.case=TRUE, perl=TRUE)))
stopifnot(identical(r2, regexpr(pat, txt, ignore.case=TRUE, perl=TRUE,
                                useBytes=TRUE)))
pat2 <- toupper(pat)
stopifnot(identical(r1, regexpr(pat2, txt, ignore.case=TRUE)))
stopifnot(identical(r1, regexpr(pat2, txt, ignore.case=TRUE, perl=TRUE)))
regexpr(pat2, txt, ignore.case=TRUE, perl=TRUE, useBytes=TRUE)
## PCRE (as used here) does not have caseless matching for non-ASCII chars
## unless (from R 2.6.0) in UTF-8 mode


(r1 <- gregexpr(pat, txt, fixed=TRUE))
(r2 <- gregexpr(pat, txt, fixed=TRUE, useBytes=TRUE))
stopifnot(identical(r1, gregexpr(pat, txt)))
stopifnot(identical(r1, gregexpr(pat, txt, perl=TRUE)))
stopifnot(identical(r2, gregexpr(pat, txt, perl=TRUE, useBytes=TRUE)))
stopifnot(identical(r1, gregexpr(pat, txt, ignore.case=TRUE)))
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


(r1 <- gsub(pat, "ef", txt))
stopifnot(identical(r1, gsub(pat, "ef", txt, fixed = TRUE)))
stopifnot(identical(r1, gsub(pat, "ef", txt, fixed = TRUE, useBytes = TRUE)))
stopifnot(identical(r1, gsub(pat, "ef", txt, perl = TRUE)))
stopifnot(identical(r1, gsub(pat, "ef", txt, perl = TRUE, useBytes = TRUE)))

pat <- substr(pat, 1, 1)
(r1 <- gsub(pat, "gh", txt))
stopifnot(identical(r1, gsub(pat, "gh", txt, fixed = TRUE)))
stopifnot(identical(r1, gsub(pat, "gh", txt, fixed = TRUE, useBytes = TRUE)))
stopifnot(identical(r1, gsub(pat, "gh", txt, perl = TRUE)))
stopifnot(identical(r1, gsub(pat, "gh", txt, perl = TRUE, useBytes = TRUE)))
