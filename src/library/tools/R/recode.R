#  File src/library/tools/R/recode.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2012 The R Core Team
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

### Remap a character string from encoded text to LaTeX escapes
encoded_text_to_latex <-
    function(x, encoding = c("latin1", "latin2", "latin9", "UTF-8", "utf8"))
{
    encoding <- match.arg(encoding)
    do_latin1 <- function(x) {
        xx <- charToRaw(x)
        paste(latin1table[as.integer(xx)], collapse="")}
    do_latin2 <- function(x) {
        xx <- charToRaw(x)
        paste(latin2table[as.integer(xx)], collapse="")}
    do_latin9 <- function(x) {
        xx <- charToRaw(x)
        paste(latin9table[as.integer(xx)], collapse="")}
    do_utf8 <- function(x) {
        xx <- utf8ToInt(x)
        y <- rep("?", length(x))
        y[xx < 512] <- utf8table[xx]
        y[xx == 0x02C6] <- "{\\textasciicircum}"
        y[xx == 0x02C7] <- "{\\textasciicaron}"
        y[xx == 0x02CA] <- "{\\textasciitilde}"
        y[xx == 0x02D8] <- "{\\textasciibreve}"
        y[xx == 0x02D9] <- "{\\textperiodcentered}"
        y[xx == 0x02DD] <- "{\\textacutedbl}"
        y[xx == 0x200C] <- "{\\textcompwordmark}"
        y[xx == 0x2018] <- "{\\textquoteleft}"
        y[xx == 0x2019] <- "{\\textquoteright}"
        y[xx == 0x201C] <- "{\\textquotedblleft}"
        y[xx == 0x201D] <- "{\\textquotedblright}"
        y[xx == 0x2020] <- "{\\textdagger}"
        y[xx == 0x2022] <- "{\\textbullet}"
        y[xx == 0x2026] <- "{\\textellipsis}"
        y[xx == 0x20AC] <- "{\\texteuro}"
        paste(y, collapse="")
    }
    as.vector(switch(encoding,
                     "latin1" = sapply(x, do_latin1),
                     "latin2" = sapply(x, do_latin2),
                     "latin9" = sapply(x, do_latin9),
                     "UTF-8"  = sapply(x, do_utf8),
                     "utf8"   = sapply(x, do_utf8),
                     stop("unimplemented encoding")
                        ))
}

latin1table <- c(
     rep("?", 31), ## omit 0x0
     ## 0x20 to %x7F
     rawToChar(as.raw(seq(32, 126)), multiple=TRUE), "?",
     ## 0x80 to 0x9F
     rep("?", 32),
     ## 0xA0 = 160 on
     "{\\nobreakspace}", "{\\textexclamdown}", "{\\textcent}", "{\\textsterling}", "{\\textcurrency}", "{\\textyen}", "{\\textbrokenbar}", "{\\S}",
     '\\"{}', "{\\textcopyright}", "{\\textordfeminine}", "{\\guillemotleft}", "{\\textlnot}", "\\-", "{\\textregistered}", "{\\a={}}",
     "{\\textdegree}", "{\\textpm}", "{\\mathtwosuperior}", "{\\maththreesuperior}", "{\\a'{}}", "{\\textmu}", "{\\P}", "{\\textperiodcentered}",
     "{\\c\\ }", "{\\mathonesuperior}", "{\\textordmasculine}", "{\\guillemotright}", "{\\textonequarter}", "{\\textonehalf}", "{\\textthreequarters}", "{\\textquestiondown}",
     "{\\a`A}", "{\\a'A}", "{\\^A}", "{\\~A}", '{\\"A}', "{\\r A}", "{\\AE}", "{\\c C}",
     "{\\a`E}", "{\\a'E}", "{\\^E}", "{\\a`I}", "{\\a'I}", "{\\^I}", "{\\~I}", '{\\"I}',
     "{\\DH}", "{\\~N}", "{\\a`O}", "{\\a'O}", "{\\^O}", "{\\~O}", '{\\"O}', "{\\texttimes}",
     "{\\O}", "{\\a`U}", "{\\a'U}", "{\\^U}", '{\\"U}', "{\\a`Y}", "{\\TH}", "{\\ss}",
     "{\\a`a}", "{\\a'a}", "{\\^a}", "{\\~a}", '{\\"a}', "{\\r a}", "{\\ae}", "{\\c c}",
     "{\\a`e}", "{\\a'e}", "{\\^e}", '{\\"e}',"{\\a`\\i}", "{\\a'\\i}", "{\\^\\i}", '{\\"\\i}',
     "{\\dh}", "{\\~n}", "{\\a`o}", "{\\a'o}", "{\\^o}", "{\\~o}", '{\\"o}', "{\\textdiv}",
     "{\\o}", "{\\a`u}", "{\\a'u}", "{\\^u}", '{\\"u}', "{\\a`y}", "{\\th}", '{\\"y}'
     )

latin2table <- c(
     rep("?", 31), ## omit 0x0
     ## 0x20 to %x7F
     rawToChar(as.raw(seq(32, 126)), multiple=TRUE), "?",
     ## 0x80 to 0x9F
     rep("?", 32),
     ## 0xA0 = 160 on
     "{\\nobreakspace}", "{\\k A}", "{\\u{}}", "{\\L}", "{\\textcurrency}", "{\\v L}", "{\\a'S}", "{\\S}",
     '\\"{}', "{\\v S}", "{\\c S}", "{\\v T}", "{\\\'Z}", "\\-", "{\\v Z}", "{\\.Z}",
     "{\\textdegree}", "{\\k A}", "{\\k\\ }", "{\\l}", "{\\a'{}}", "{\\v l}", "{\\a's}", "{\\v{}}",
     "{\\c\\ }", "{\\v s}", "{\\c s}", "{\\v t}", "{\\'z}", "{\\H{}}", "{\\v z}", "{\\.z}",
     "{\\a'R}", "{\\a'A}", "{\\^A}", "{\\u A}", '{\\"A}', "{\\'L}", "{\\a'C}", "{\\c C}",
     "{\\v C}", "{\\a'E}", "{\\k E}", '{\\"E}', "{\\v E}", "{\\'I}", "{\\^I}", '{\\v D}',
     "{\\DJ}", "{\\a'N}", "{\\v N}", "{\\a'O}", "{\\^O}", "{\\H O}", '{\\"O}', "{\\texttimes}",
     "{\\v R}", "{\\r U}", "{\\a'U}", "{\\H U}", '{\\"U}', "{\\a`Y}", "{\\c I}", "{\\ss}",
     "{\\a'r}", "{\\a'a}", "{\\^a}", "{\\u a}", '{\\"a}', "{\\'l}", "{\\a'c}", "{\\c c}",
     "{\\v c}", "{\\a'e}", "{\\k e}", '{\\"e}', "{\\v e}", "{\\'\\i}", "{\\^\\i}", '{\\v d}',
     "{\\dj}", "{\\a'n}", "{\\c n}", "{\\a'o}", '{\\"a}', "{\\H o}", '{\\"o}', "{\\textdiv}",
     "{\\v r}", "{\\r u}", "{\\a'u}", "{\\H u}", '{\\"u}', "{\\a`y}", "{\\c t}", '{\\.{}}'
     )

latin9table <- c(
     rep("?}", 31),
     ## 0x20 to %x7F
     rawToChar(as.raw(seq(32, 126)), multiple=TRUE), "?}",
     ## 0x80 to 0x9F
     rep("?}", 32),
     ## 0xA0 = 160 on
     "{\\nobreakspace}", "{\\textexclamdown}", "{\\textcent}", "{\\textsterling}", "{\\texteuro}", "{\\textyen}", "{\\v S}", "{\\S}",
     '{\\v s}', "{\\copyright}", "{\\textordfeminine}", "{\\guillemotleft}", "{\\textlnot}", "\\-", "{\\textregistered}", "{\\a={}}",
     "{\\textdegree}", "{\\textpm}", "{\\mathtwosuperior}", "{\\maththreesuperior}", "{\\v Z}", "{\\textmu}", "{\\P}", "{\\textperiodcentered}",
     "{\\v z}", "{\\mathonesuperior}", "{\\textordmasculine}", "{\\guillemotright}", "{\\OE}", "{\\oe}", '{\\"Y}', "{\\textquestiondown}",
     "{\\a`A}", "{\\a'A}", "{\\^A}", "{\\~A}", '{\\"A}', "{\\r A}", "{\\AE}", "{\\c C}",
     "{\\a`E}", "{\\a'E}", "{\\^E}", "{\\a`I}", "{\\a'I}", "{\\^I}", "{\\~I}", '{\\"I}',
     "{\\DH}", "{\\~N}", "{\\a`O}", "{\\a'O}", "{\\^O}", "{\\~O}", '{\\"O}', "{\\texttimes}",
     "{\\O}", "{\\a`u}", "{\\a'U}", "{\\^U}", '\\"U', "{\\a`Y}", "{\\TH}", "{\\ss}",
     "{\\a`a}", "{\\a'a}", "{\\^a}", "{\\~a}", '{\\"a}', "{\\r a}", "{\\ae}", "{\\c c}",
     "{\\a`e}", "{\\a'e}", "{\\^e}", '{\\"e}',"{\\a`\\i}", "{\\a'\\i}", "{\\^\\i}", '{\\"\\i}',
     "{\\dh}", "{\\~n}", "{\\a`o}", "{\\a'o}", "{\\^o}", "{\\~o}", '{\\"o}', "{\\textdiv}",
     "{\\o}", "{\\a`u}", "{\\a'u}", "{\\^u}", '\\"u', "{\\a`y}", "{\\th}", '{\\"y}'
     )

utf8table <- c(latin1table, rep("?", 256))

utf8table[0x0102:0x107] <-
    c("{\\u A}","{\\u a}", "{\\k A}", "{\\k a}", "{\\a'C}", "{\\a'c}")
utf8table[0x010C:0x111] <-
    c( "{\\v C}","{\\v c}","{\\v D}","{\\v d}","{\\DJ}","{\\dj}")

utf8table[0x0118:0x11B] <- c("{\\k E}","{\\k e}", "{\\v E}","{\\v e}")
utf8table[0x011E:0x11F] <- c("{\\u G}","{\\u g}")
utf8table[0x0130:0x131] <- c("{\\.I}","{\\i}")
utf8table[0x0139:0x13A] <- c("{\\a'L}","{\\a'l}")
utf8table[0x013D:0x13E] <- c("{\\v L}","{\\v l}")
utf8table[0x0141L:0x144] <- c("{\\L}","{\\l}","{\\a'N}","{\\a'n}")
utf8table[0x0147:0x14B] <- c("{\\v N}","{\\v n}","?","{\\NG}","{\\ng}")
utf8table[0x0150:0x155] <- c("{\\H O}","{\\H o}","{\\OE}","{\\oe}","{\\a'R}","{\\a'r}")
utf8table[0x0158:0x15B] <- c("{\\v R}","{\\v r}","{\\a'S}","{\\a's}")
utf8table[0x015E:0x165] <- c("{\\c S}","{\\c s}","{\\v S}","{\\v s}",
                             "{\\c T}","{\\c t}","{\\v T}","{\\v t}")
utf8table[0x016E:0x171] <- c("{\\r U}","{\\r u}","{\\H U}","{\\H u}")
utf8table[0x0178:0x17E] <- c('{\\"Y}',"{\\a'Z}","{\\a'z}","{\\.Z}", "{\\.z}","{\\v Z}","{\\v z}")
utf8table[0x0192] <- "{\\textflorin}"
