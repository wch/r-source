% File src/library/grDevices/man/Hershey.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2012 R Core Team
% Distributed under GPL 2 or later

\name{Hershey}
\alias{Hershey}
\title{Hershey Vector Fonts in R}
\description{
  If the \code{family} graphical parameter (see \code{\link{par}})
  has been set to one of the Hershey fonts (see \sQuote{Details})
  Hershey vector fonts are used to render text.

  When using the \code{\link{text}} and \code{\link{contour}} functions
  Hershey fonts may be selected via the \code{vfont} argument, which is
  a character vector of length 2 (see \sQuote{Details} for valid
  values).  This allows Cyrillic to be selected, which is not available
  via the font families.
}
\usage{
Hershey
}
\details{
  The Hershey fonts have two advantages:\cr
  \enumerate{
    \item vector fonts describe each
    character in terms of a set of points;  \R renders the character by
    joining up the points with straight lines.  This intimate knowledge of
    the outline of each character means that \R can arbitrarily transform
    the characters, which can mean that the vector fonts look better
    for rotated text.
    \item this implementation was adapted from the GNU libplot library
    which provides support for non-ASCII and non-English fonts.  This means
    that it is possible, for example, to produce weird plotting symbols and
    Japanese characters.
  }
  Drawback:\cr
You cannot use mathematical expressions (\code{\link{plotmath}}) with
  Hershey fonts.

  The Hershey characters are organised into a set of fonts.  A
  particular font is selected by specifying one of the following
  font families via \code{par(family)} and specifying the desired font
  face (plain, bold, italic, bold-italic) via \code{par(font)}.

%% The basic "table" is in  ../../../main/engine.c
  \tabular{ll}{
    family \tab faces available \cr
    \code{"HersheySerif"} \tab plain, bold, italic, bold-italic \cr
    \code{"HersheySans"} \tab plain, bold, italic, bold-italic \cr
    \code{"HersheyScript"} \tab plain, bold \cr
    \code{"HersheyGothicEnglish"} \tab plain \cr
    \code{"HersheyGothicGerman"} \tab plain \cr
    \code{"HersheyGothicItalian"} \tab plain \cr
    \code{"HersheySymbol"} \tab plain, bold, italic, bold-italic \cr
    \code{"HersheySansSymbol"} \tab plain, italic \cr
  }

  In the \code{vfont} specification for the \code{text} and
  \code{contour} functions, the Hershey font is specified by a typeface
  (e.g., \code{serif} or \code{sans serif}) and a fontindex or
  \sQuote{style} (e.g., \code{plain} or \code{italic}). The first
  element of \code{vfont} specifies the typeface and the second element
  specifies the fontindex.  The first table produced by
  \code{demo(Hershey)} shows the character \code{a} produced by each of
  the different fonts.

  The available \code{typeface} and \code{fontindex} values are
  available as list components of the variable \code{Hershey}.
  The allowed pairs for \code{(typeface, fontindex)} are:
%% This is checked (via max{#}) in FixupVFont() ../../../main/plot.c
%% The basic "table" really is in  ../../../modules/vfonts/g_fontdb.c

  \tabular{ll}{
    serif \tab plain \cr
    serif \tab italic \cr
    serif \tab bold \cr
    serif \tab bold italic \cr
    serif \tab cyrillic \cr
    serif \tab oblique cyrillic \cr
    serif \tab EUC \cr
    sans serif \tab plain \cr
    sans serif \tab italic \cr
    sans serif \tab bold \cr
    sans serif \tab bold italic \cr
    script \tab plain \cr
    script \tab italic \cr
    script \tab bold \cr
    gothic english \tab plain \cr
    gothic german \tab plain \cr
    gothic italian \tab plain \cr
    serif symbol \tab plain \cr
    serif symbol \tab italic \cr
    serif symbol \tab bold \cr
    serif symbol \tab bold italic \cr
    sans serif symbol \tab plain \cr
    sans serif symbol \tab italic \cr
  }
  and the indices of these are available as \code{Hershey$allowed}.

  \describe{
    \item{Escape sequences:}{The string to be drawn can include escape
      sequences, which all begin with a \samp{\\}.   When \R encounters a
      \samp{\\}, rather than drawing the \samp{\\}, it treats the
      subsequent character(s) as a coded description of what to draw.

      One useful escape sequence (in the current context) is of the
      form: \samp{\\123}.  The three digits following the \samp{\\}
      specify an octal code for a character.  For example, the octal
      code for \code{p} is 160 so the strings \code{"p"} and
      \code{"\\160"} are equivalent. This is useful for producing
      characters when there is not an appropriate key on your keyboard.

      The other useful escape sequences all begin with \samp{\\\\}.
      These are described below.  Remember that backslashes have to be
      doubled in \R character strings, so they need to be entered with
      \emph{four} backslashes.
    }

    \item{Symbols:}{an entire string of Greek symbols can be produced by
      selecting the HersheySymbol or HersheySansSymbol family or the
      Serif Symbol or Sans Serif Symbol typeface.  To allow Greek
      symbols to be embedded in a string which uses a non-symbol
      typeface, there are a set of symbol escape sequences of the form
      \samp{\\\\ab}. For example, the escape sequence \samp{\\\\*a}
      produces a Greek alpha.  The second table in \code{demo(Hershey)}
      shows all of the symbol escape sequences and the symbols that they
      produce.
    }

    \item{ISO Latin-1:}{further escape sequences of the form
      \samp{\\\\ab} are provided for producing ISO Latin-1 characters.
      Another option is to use the appropriate octal code.  The
      (non-ASCII) ISO Latin-1 characters are in the range 241\dots{}377.
      For example, \samp{\\366} produces the character o with an umlaut.
      The third table in \code{demo(Hershey)} shows all of the ISO
      Latin-1 escape sequences.

      These characters can be used directly.  (Characters not in Latin-1
      are replaced by a dot.)

      Several characters are missing, c-cedilla has no cedilla and
      \sQuote{sharp s} (\samp{U+00DF}, also known as \sQuote{esszett})
      is rendered as \code{ss}.
    }

    \item{Special Characters:}{a set of characters are provided which
      do not fall into any standard font.  These can only be accessed by
      escape sequence.  For example, \samp{\\\\LI} produces the zodiac sign for
      Libra, and \samp{\\\\JU} produces the astronomical sign for Jupiter.
      The fourth table in \code{demo(Hershey)} shows all of the
      special character escape sequences.
    }

    \item{Cyrillic Characters:}{cyrillic characters are implemented
      according to the K018-R encoding, and can be used directly in such
      a locale using the Serif typeface and Cyrillic (or Oblique Cyrillic)
      fontindex.  Alternatively they can be specified via an octal code
      in the range 300 to 337 for lower case characters or 340 to 377 for
      upper case characters.  The fifth table in \code{demo(Hershey)} shows the
      octal codes for the available Cyrillic characters.

      Cyrillic has to be selected via a \code{("serif", fontindex)} pair
      rather than via a font family.
    }

    \item{Japanese Characters:}{83 Hiragana, 86 Katakana, and 603 Kanji
      characters are implemented according to the EUC-JP (Extended Unix Code)
      encoding. Each character is identified by a unique hexadecimal code.
      The Hiragana characters
      are in the range 0x2421 to 0x2473, Katakana are in the range 0x2521 to
      0x2576, and Kanji are (scattered about) in the range 0x3021 to 0x6d55.

      When using the Serif typeface and EUC
      fontindex, these characters can be produced by a \emph{pair} of octal
      codes.  Given the hexadecimal code (e.g., 0x2421), take the first two
      digits and add 0x80 and do the same to the second two digits (e.g.,
      0x21 and 0x24 become 0xa4 and 0xa1), then convert both to octal (e.g.,
      0xa4 and 0xa1 become 244 and 241).  For example, the first Hiragana
      character is produced by \samp{\\244\\241}.

      It is also possible to use the hexadecimal code directly.  This works
      for all non-EUC fonts by specifying an escape sequence of the form
      \samp{\\\\#J1234}.  For example, the first Hiragana character is produced
      by \samp{\\\\#J2421}.

      The Kanji characters may be specified in a third way, using the
      so-called "Nelson Index", by specifying an escape sequence of the form
      \samp{\\\\#N1234}.  For example, the (obsolete) Kanji for \sQuote{one} is
      produced by \samp{\\\\#N0001}.

      \code{demo(Japanese)} shows the available Japanese characters.
    }

    \item{Raw Hershey Glyphs:}{all of the characters in the Hershey
      fonts are stored in a large array.  Some characters are not
      accessible in any of the Hershey fonts.  These characters can only
      be accessed via an escape sequence of the form \samp{\\\\#H1234}.
      For example, the fleur-de-lys is produced by \samp{\\\\#H0746}.
      The sixth and seventh tables of \code{demo(Hershey)} shows all of
      the available raw glyphs.
    }
  }
}

\references{
  \url{https://www.gnu.org/software/plotutils/plotutils.html}.
}
\seealso{ \code{\link{demo}(Hershey)}, \code{\link{par}},
  \code{\link{text}}, \code{\link{contour}}.

  \code{\link{Japanese}} for the Japanese characters in the Hershey fonts.
}
\examples{
Hershey

## for tables of examples, see demo(Hershey)
}
\keyword{aplot}
