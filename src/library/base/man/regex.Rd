% File src/library/base/man/regex.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2008 R Core Development Team
% Distributed under GPL 2 or later

\name{regex}
\Rdversion{1.1}
\alias{regex}
\alias{regexp}
\alias{regular expression}
\concept{regular expression}
\title{Regular Expressions as used in R}
\description{
  This help page documents the regular expression patterns supported by
  \code{\link{grep}} and related functions \code{regexpr},
  \code{gregexpr}, \code{sub} and \code{gsub}, as well as by
  \code{\link{strsplit}}.
}
\details{
  A \sQuote{regular expression} is a pattern that describes a set of
  strings.  Three types of regular expressions are used in \R,
  \emph{extended} regular expressions, used by
  \code{grep(extended = TRUE)} (its default), \emph{basic} regular
  expressions, as used by \code{grep(extended = FALSE)}, and
  \emph{Perl-like} regular expressions used by \code{grep(perl = TRUE)}.

  Other functions which use regular expressions (often via the use of
  \code{grep}) include \code{apropos}, \code{browseEnv},
  \code{help.search}, \code{list.files}, \code{ls} and \code{strsplit}.
  These will all use \emph{extended} regular expressions, unless
  \code{strsplit} is called with argument \code{extended = FALSE} or
  \code{perl = TRUE}.

  Patterns are described here as they would be printed by \code{cat}:
  \emph{do remember that backslashes need to be doubled when entering \R
    character strings}, e.g. from the keyboard.
}
\section{Extended Regular Expressions}{
  This section covers the regular expressions allowed if \code{extended
  = TRUE} in \code{grep}, \code{regexpr}, \code{gregexpr}, \code{sub},
  \code{gsub} and \code{strsplit}.  They use the \code{glibc} 2.7
  implementation of the POSIX 1003.2 standard.

  Regular expressions are constructed analogously to arithmetic
  expressions, by using various operators to combine smaller expressions.

  The fundamental building blocks are the regular expressions that match
  a single character.  Most characters, including all letters and
  digits, are regular expressions that match themselves.  Any
  metacharacter with special meaning may be quoted by preceding it with
  a backslash.  (Escaping other characters with a backslash is
  undefined in POSIX but gives the character in the \R implementation.)
  The metacharacters in EREs are \samp{. \\ | ( ) [ \{ ^ $ * + ?}, but
  note that whether these have a special meaning depends on the context.

  A \emph{character class} is a list of characters enclosed between
  \samp{[} and \samp{]} which matches any single character in that list;
  unless the first character of the list is the caret \samp{^}, when it
  matches any character \emph{not} in the list.  For example, the
  regular expression \samp{[0123456789]} matches any single digit, and
  \samp{[^abc]} matches anything except the characters \samp{a},
  \samp{b} or \samp{c}.  A range of characters may be specified by
  giving the first and last characters, separated by a hyphen.  (Because
  their interpretation is so locale-dependent, they are best avoided.)

  The precise way character ranges are interpreted depends on the values
  of \code{perl} and \code{ignore.case}.  For basic and extended regular
  expressions the collation order is taken from the OS's
  implementation of the setting of the locale category
  \code{LC_COLLATE}, so \samp{[W-Z]} may include \samp{x} and if it
  does may or may not include \samp{w}.  (In most English locales the
  collation order is \samp{wWxXyYzZ}.)  For caseless matching the
  characters in a range are interpreted as if in lower case, so in an
  English locale \samp{[W-z]} matches \samp{WXYZwxyz}.  The only
  portable way to specify all ASCII letters is to list them all as
  character class,
  \samp{[ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz]}.

  For Perl regexps, the ranges are interpreted in the numerical order of
  the characters, either as bytes in a single-byte locale or as Unicode
  points in a UTF-8 locale.  So in either case \samp{[A-Za-z]} specifies
  the set of ASCII letters.

  Certain named classes of characters are predefined.  Their
  interpretation depends on the \emph{locale} (see \link{locales}); the
  interpretation below is that of the POSIX locale.

  \describe{
    \item{\samp{[:alnum:]}}{Alphanumeric characters: \samp{[:alpha:]}
      and \samp{[:digit:]}.}

    \item{\samp{[:alpha:]}}{Alphabetic characters: \samp{[:lower:]} and
      \samp{[:upper:]}.}

    \item{\samp{[:blank:]}}{Blank characters: space and tab.}

    \item{\samp{[:cntrl:]}}{
      Control characters.  In ASCII, these characters have octal codes
      000 through 037, and 177 (\code{DEL}).  In another character set,
      these are the equivalent characters, if any.}

    \item{\samp{[:digit:]}}{Digits: \samp{0 1 2 3 4 5 6 7 8 9}.}

    \item{\samp{[:graph:]}}{Graphical characters: \samp{[:alnum:]} and
      \samp{[:punct:]}.}

    \item{\samp{[:lower:]}}{Lower-case letters in the current locale.}

    \item{\samp{[:print:]}}{
      Printable characters: \samp{[:alnum:]}, \samp{[:punct:]} and space.}

    \item{\samp{[:punct:]}}{Punctuation characters:
      \samp{! " # $ \% & ' ( ) * + , - . / : ; < = > ? @ [ \\ ] ^ _ ` \{ | \} ~}.}
%'"`  keep Emacs Rd mode happy

    \item{\samp{[:space:]}}{
      Space characters: tab, newline, vertical tab, form feed, carriage
      return, and space.}

    \item{\samp{[:upper:]}}{Upper-case letters in the current locale.}

    \item{\samp{[:xdigit:]}}{Hexadecimal digits:
      \samp{0 1 2 3 4 5 6 7 8 9 A B C D E F a b c d e f}.}
  }

  For example, \samp{[[:alnum:]]} means \samp{[0-9A-Za-z]}, except the
  latter depends upon the locale and the character encoding, whereas
  the former is independent of locale and character set.  (Note that the
  brackets in these class names are part of the symbolic names, and must
  be included in addition to the brackets delimiting the bracket list.)
  Most metacharacters lose their special meaning inside lists.  To
  include a literal \samp{]}, place it first in the list.  Similarly, to
  include a literal \samp{^}, place it anywhere but first.  Finally, to
  include a literal \samp{-}, place it first or last (or, for
  \code{perl = TRUE} only, precede it by a backslash.).  (Only these and
  \samp{\\} remain special inside character classes.)

  The period \samp{.} matches any single character.  The symbol
  \samp{\\w} is documented to be synonym for \samp{[[:alnum:]]} and
  \samp{\\W} is its negation.  However, \samp{\\w} also
  matches underscore in the GNU grep code used in \R.

  The caret \samp{^} and the dollar sign \samp{$} are metacharacters
  that respectively match the empty string at the beginning and end of a
  line.  The symbols \samp{\\<} and \samp{\\>} respectively match the
  empty string at the beginning and end of a word.  The symbol \samp{\\b}
  matches the empty string at either edge of a word, and \samp{\\B} matches
  the empty string provided it is not at an edge of a word.

  A regular expression may be followed by one of several repetition
  quantifiers:
  \describe{
    \item{\samp{?}}{The preceding item is optional and will be matched
      at most once.}

    \item{\samp{*}}{The preceding item will be matched zero or more
      times.}

    \item{\samp{+}}{The preceding item will be matched one or more
      times.}

    \item{\samp{{n}}}{The preceding item is matched exactly \code{n}
      times.}

    \item{\samp{{n,}}}{The preceding item is matched \code{n} or more
      times.}

    \item{\samp{{n,m}}}{The preceding item is matched at least \code{n}
      times, but not more than \code{m} times.}
  }
  Repetition is greedy, so the maximal possible number of repeats is used.

  Two regular expressions may be concatenated; the resulting regular
  expression matches any string formed by concatenating two substrings
  that respectively match the concatenated subexpressions.

  Two regular expressions may be joined by the infix operator \samp{|};
  the resulting regular expression matches any string matching either
  subexpression.   For example, \samp{abba|cde} matches either the
  string \code{abba} or the string \code{cde}.  Note that alternation
  does not work inside character classes, where \samp{|} has its literal
  meaning.

  Repetition takes precedence over concatenation, which in turn takes
  precedence over alternation.  A whole subexpression may be enclosed in
  parentheses to override these precedence rules.

  The backreference \samp{\\\var{N}}, where \samp{\var{N}} is a single
  digit, matches the substring previously matched by the Nth
  parenthesized subexpression of the regular expression.
}
\section{Basic Regular Expressions}{
  This section covers the regular expressions allowed if \code{extended
  = FALSE} in \code{grep}, \code{regexpr}, \code{gregexpr}, \code{sub},
  \code{gsub} and \code{strsplit}.

  In basic regular expressions the metacharacters \samp{?}, \samp{+},
  \samp{\{}, \samp{|}, \samp{(}, and \samp{)} lose their special meaning;
  instead use the backslashed versions \samp{\\?}, \samp{\\+},
  \samp{\\ \{}, \samp{\\|}, \samp{\\(}, and \samp{\\)}.  Thus the
  metacharacters are \samp{. \\ [ ^ $ *}.
}
\section{Perl Regular Expressions}{
  The \code{perl = TRUE} argument to \code{grep}, \code{regexpr},
  \code{gregexpr}, \code{sub}, \code{gsub} and \code{strsplit} switches
  to the PCRE library that \sQuote{implements regular expression pattern
  matching using the same syntax and semantics as Perl 5.6 or later,
  with just a few differences}.  It adds some features from Perl 5.10.

  For complete details please consult the man pages for PCRE, especially
  \command{man pcrepattern} and \command{man pcreapi}), on your system or from
  the sources at \url{http://www.pcre.org}. If PCRE support was compiled
  from the sources within \R, the PCRE version is 7.8 as described here
  (version \eqn{\ge}{>=} 7.6 is required if \R is configured to use the
  system's PCRE library).

  Perl regular expressions are computed byte-by-byte rather than
  character-by-character except in UTF-8 locales.  Since the only
  non-UTF-8 multibyte locales in common use are those for CJK languages,
  they should be used with care in non-UTF-8 CJK locales.

  All the regular expressions described for extended regular expressions
  are accepted except \samp{\\<} and \samp{\\>}: in Perl all backslashed
  metacharacters are alphanumeric and backslashed symbols always are
  interpreted as a literal character. \samp{\{} is not special if it would
  be the start of an invalid interval specification.  There can be more than
  9 backreferences.  In a UTF-8 locale the named character classes only
  match ASCII characters: see \samp{\\p} below for an alternative.

  The construct \samp{(?...)} is used for Perl extensions in a variety
  of ways depending on what immediately follows the \samp{?}.

  Perl-like matching can work in several modes, set by the options
  \samp{(?i)} (caseless, equivalent to Perl's \samp{/i}), \samp{(?m)}
  (multiline, equivalent to Perl's \samp{/m}), \samp{(?s)} (single line,
  so a dot matches all characters, even new lines: equivalent to Perl's
  \samp{/s}) and \samp{(?x)} (extended, whitespace data characters are
  ignored unless escaped and comments are allowed: equivalent to Perl's
  \samp{/x}).  These can be concatenated, so for example, \samp{(?im)}
  sets caseless multiline matching.  It is also possible to unset these
  options by preceding the letter with a hyphen, and to combine setting
  and unsetting such as \samp{(?im-sx)}.  These settings can be applied
  within patterns, and then apply to the remainder of the pattern.
  Additional options not in Perl include \samp{(?U)} to set
  \sQuote{ungreedy} mode (so matching is minimal unless \samp{?} is used,
  when it is greedy).  Initially none of these options are set.

  If you want to remove the special meaning from a sequence of
  characters, you can do so by putting them between \samp{\\Q} and
  \samp{\\E}. This is different from Perl in that \samp{$} and \samp{@} are
  handled as literals in \samp{\\Q...\\E} sequences in PCRE, whereas in
  Perl, \samp{$} and \samp{@} cause variable interpolation.

  The escape sequences \samp{\\d}, \samp{\\s} and \samp{\\w} represent any
  decimal digit, space character and \sQuote{word} character
  (letter, digit or underscore in the current locale, except that in a
  UTF-8 locale only ASCII letters are considered) respectively, and
  their upper-case versions represent their negation.  Unlike POSIX and
  earlier versions of Perl and PCRE, vertical tab is not regarded as a
  whitespace character.

  Escape sequence \samp{\\a} is \samp{BEL}, \samp{\\e} is \samp{ESC},
  \samp{\\f} is \samp{FF}, \samp{\\n} is \samp{LF}, \samp{\\r} is
  \samp{CR} and \samp{\\t} is \samp{TAB}.  In addition \samp{\\c\var{x}}
  is \samp{cntrl-\var{x}} for any \samp{\var{x}}, \samp{\\\var{ddd}} is
  the octal character \samp{\var{ddd}} (for up to three digits unless
  interpretable as a backreference, as \samp{\\1} to \samp{\\7} always
  are), and \samp{\\x\var{hh}} specifies a character by two hex digits.
  In a UTF-8 locale, \samp{\\x\{\var{h...}\}} specifies a Unicode point
  by one or more hex digits.

  Outside a character class, \samp{\\b} matches a word boundary,
  \samp{\\B} is its negation, \samp{\\A} matches at start of a subject (even
  in multiline mode, unlike \samp{^}), \samp{\\Z} matches at end of a
  subject or before newline at end, \samp{\\z} matches at end of a
  subject. and \samp{\\G} matches at first matching position in a
  subject (which is subtly different from Perl's end of the previous
  match).  \samp{\\C} matches a single byte. including a newline.
  % pro tem \samp{\\R} matches \R.
  In a UTF-8 locale, \samp{\\R} matches any Unicode newline character
  (not just CR), and \samp{\\X} matches any number of Unicode characters
  that form an extended Unicode sequence.

  In a UTF-8 locale, some Unicode properties are supported via
  \samp{\\p\{\var{xx}\}} and \samp{\\P\{\var{xx}\}} which match
  characters with and without property \samp{\var{xx}} respectively.
  For a list of supported properties see the PCRE documentation, but for
  example \samp{Lu} is \sQuote{upper case letter} and \samp{Sc} is
  \sQuote{currency symbol}.

  The same repetition quantifiers as extended POSIX are supported.
  However, if a quantifier is followed by \samp{?}, the match is
  \sQuote{ungreedy}, that is as short as possible rather than as long as
  possible (unless the meanings are reversed by the \samp{(?U)} option.)

  The sequence \samp{(?#} marks the start of a comment which continues
  up to the next closing parenthesis.  Nested parentheses are not
  permitted.  The characters that make up a comment play no part at all in
  the pattern matching.

  If the extended option is set, an unescaped \samp{#} character outside
  a character class introduces a comment that continues up to the next
  newline character in the pattern.

  The pattern \samp{(?:...)} groups characters just as parentheses do
  but does not make a backreference.

  Patterns \samp{(?=...)} and \samp{(?!...)} are zero-width positive and
  negative lookahead \emph{assertions}: they match if an attempt to
  match the \code{\dots} forward from the current position would succeed
  (or not), but use up no characters in the string being processed.
  Patterns \samp{(?<=...)} and \samp{(?<!...)} are the lookbehind
  equivalents: they do not allow repetition quantifiers nor \samp{\\C}
  in \code{\dots}.

  Named subpatterns, atomic grouping, possessive qualifiers and conditional
  and recursive patterns are not covered here.
}
\author{
  This help page is based on the documentation of GNU grep 2.4.2 and the
  \code{pcrepattern} man page from PCRE 7.4.
}
\seealso{
  \code{\link{grep}}, \code{\link{apropos}}, \code{\link{browseEnv}},
  \code{\link{glob2rx}}, \code{\link{help.search}}, \code{\link{list.files}},
  \code{\link{ls}} and \code{\link{strsplit}}.

  \url{http://www.opengroup.org/onlinepubs/009695399/basedefs/xbd_chap09.html}

  The \code{pcrepattern} can be found as part of
  \url{http://www.pcre.org/pcre.txt}, and details of Perl's own
  implementation at \url{http://perldoc.perl.org/perlre.html}.
}
\keyword{character}
