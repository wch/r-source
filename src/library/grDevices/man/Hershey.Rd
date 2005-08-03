%% NB: Microsoft Help Compiler often crashes on this file
%% Do not alter unless you have checked on Windows!
\name{Hershey}
\alias{Hershey}
\title{Hershey Vector Fonts in R}
\usage{
Hershey
}
\description{
  If the \code{family} graphical parameter (see \code{\link{par}})
  has been set to one of the Hershey fonts (see below)
  Hershey vector fonts are used to render text.
  
  These fonts have two advantages:\cr
  \enumerate{
    \item vector fonts describe each
    character in terms of a set of points;  \R renders the character by
    joining up the points with straight lines.  This intimate knowledge of
    the outline of each character means that \R can arbitrarily transform
    the characters, which can mean that the vector fonts look better
    for rotated and 3d text.
    \item this implementation was adapted from the GNU libplot library
    which provides support for non-ASCII and non-English fonts.  This means
    that it is possible, for example, to produce weird plotting symbols and
    Japanese characters.
  }

  For historical reasons,
  when using the \code{\link{text}} function, Hershey fonts may also be
  selected via the \code{vfont} argument, which is a character vector
  of length 2 (see below for valid values).

  Drawback:\cr
You cannot use mathematical expressions (\code{\link{plotmath}}) with
  Hershey fonts.
}
\details{
  The Hershey characters are organised into a set of fonts.  A
  particular font is selected by specifying one of the following
  font families via \code{par(family)} and specifying the desired font
  face (plain, bold, italic, bold-italic) via \code{par(font)}.

%% The basic "table" is in  ../../../main/engine.c
  \tabular{ll}{
    family \tab faces available \cr
    "HersheySerif" \tab plain, bold, italic, bold-italic \cr
    "HersheySans" \tab plain, bold, italic, bold-italic \cr
    "HersheyScript" \tab plain, bold \cr
    "HersheyGothicEnglish" \tab plain \cr
    "HersheyGothicGerman" \tab plain \cr
    "HersheyGothicItalian" \tab plain \cr
    "HersheySymbol" \tab plain, bold, italic, bold-italic \cr
    "HersheySansSymbol" \tab plain, italic \cr
  }

  In the old, \code{vfont} specification for the \code{text} function,
  the Hershey font is 
  specified by a typeface (e.g., \code{serif} or \code{sans serif})
  and a fontindex or \dQuote{style} (e.g., \code{plain} or \code{italic}).
  The first element of \code{vfont} specifies the typeface and the second
  element specifies the fontindex.  The first table
  produced by \code{demo(Hershey)} shows the character \code{a} produced
  by each of the different fonts.

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
      sequences, which all begin with a \code{\\}.   When \R encounters a
      \code{\\}, rather than drawing the \code{\\}, it treats the
      subsequent character(s) as a coded description of what to draw.

      One useful escape sequence (in the current context) is of the
      form: \code{\\123}.  The three digits following the \code{\\} specify an
      octal code for a character.  For example, the octal code for \code{p} is
      160 so the strings \code{"p"} and \code{"\\160"} are equivalent. This is
      useful for producing characters when there is not an appropriate key on
      your keyboard.

      The other useful escape sequences all begin with \code{\\\\}.  These are
      described below.
      Remember that backslashes have to be doubled in \R character
      strings, so they need to be entered with \emph{four} backslashes.
    }

    \item{Symbols:}{an entire string of Greek symbols can be produced by
      selecting the Serif Symbol or Sans Serif Symbol typeface.  To allow
      Greek symbols to be embedded in a string which uses a non-symbol
      typeface, there are a set of symbol escape sequences of the form
      \code{\\\\ab}. For example, the escape sequence \code{\\\\*a} produces a
      Greek alpha. The second table in \code{demo(Hershey)} shows all of the
      symbol escape sequences and the symbols that they produce.
    }

    \item{ISO Latin-1:}{further escape sequences of the form
      \code{\\\\ab} are provided for producing ISO Latin-1 characters.
      Another option is to use the appropriate octal code.  The
      (non-ASCII) ISO Latin-1 characters are in the range 241\dots377.
      For example, \code{\\366} produces the character \code{o} with an
      umlaut.  The third table in \code{demo(Hershey)} shows all of the
      ISO Latin-1 escape sequences.

      These characters can be used directly in a Latin-1 or UTF-8
      locale. (In the latter, non-Latin-1 characters are replaced by a dot.)
    }

    \item{Special Characters:}{a set of characters are provided which
      do not fall into any standard font.  These can only be accessed by
      escape sequence.  For example, \code{\\\\LI} produces the zodiac sign for
      Libra, and \code{\\\\JU} produces the astronomical sign for Jupiter.
      The fourth table in \code{demo(Hershey)} shows all of the
      special character escape sequences.
    }

    \item{Cyrillic Characters:}{cyrillic characters are implemented
      according to the K018-R encoding.  On a US keyboard, these can be
      produced using the Serif typeface and Cyrillic (or Oblique Cyrillic)
      fontindex and specifying an octal code in the range 300 to 337 for
      lower case characters or 340 to 377 for upper case characters.
      The fifth table in \code{demo(Hershey)} shows the octal codes for
      the available cyrillic characters.
    }

    \item{Japanese Characters:}{83 Hiragana, 86 Katakana, and 603 Kanji
      characters are implemented according to the EUC (Extended Unix Code)
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
      character is produced by \code{\\244\\241}.

      It is also possible to use the hexadecimal code directly.  This works
      for all non-EUC fonts by specifying an escape sequence of the form
      \code{\\\\#J1234}.  For example, the first Hiragana character is produced
      by \code{\\\\#J2421}.

      The Kanji characters may be specified in a third way, using the
      so-called "Nelson Index", by specifying an escape sequence of the form
      \code{\\\\#N1234}.  For example, the Kanji for \dQuote{one} is
      produced by \code{\\\\#N0001}.
    
      \code{demo(Japanese)} shows the available Japanese characters.
    }
      
    \item{Raw Hershey Glyphs:}{all of the characters in the Hershey fonts
      are stored in a large array.  Some characters are not accessible in
      any of the Hershey fonts.  These characters can only be accessed via
      an escape sequence of the form \code{\\\\#H1234}.  For example, the
      fleur-de-lys is produced by \code{\\\\#H0746}.  The sixth and seventh
      tables of \code{demo(Hershey)} shows all of the available raw
      glyphs.
    }
  }
}
\references{
  \url{http://www.gnu.org/software/plotutils/plotutils.html}
}
\seealso{
  \code{demo(Hershey)}, \code{\link{par}}, \code{\link{text}}, 
  \code{\link{contour}}.

  \code{\link{Japanese}} for the Japanese characters in the Hershey fonts.
}
\examples{
Hershey

## for tables of examples, see demo(Hershey)
}
\keyword{aplot}
