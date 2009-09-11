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


if(FALSE) {
(r1 <- gsub(pat, "ef", txt))
stopifnot(identical(r1, gsub(pat, "ef", txt, useBytes = TRUE)))
stopifnot(identical(r1, gsub(pat, "ef", txt, fixed = TRUE)))
stopifnot(identical(r1, gsub(pat, "ef", txt, fixed = TRUE, useBytes = TRUE)))
stopifnot(identical(r1, gsub(pat, "ef", txt, perl = TRUE)))
stopifnot(identical(r1, gsub(pat, "ef", txt, perl = TRUE, useBytes = TRUE)))

pat <- substr(pat, 1, 1)
(r1 <- gsub(pat, "gh", txt))
stopifnot(identical(r1, gsub(pat, "gh", txt, useBytes = TRUE)))
stopifnot(identical(r1, gsub(pat, "gh", txt, fixed = TRUE)))
stopifnot(identical(r1, gsub(pat, "gh", txt, fixed = TRUE, useBytes = TRUE)))
stopifnot(identical(r1, gsub(pat, "gh", txt, perl = TRUE)))
stopifnot(identical(r1, gsub(pat, "gh", txt, perl = TRUE, useBytes = TRUE)))
}
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
