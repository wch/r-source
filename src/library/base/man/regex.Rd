% File src/library/base/man/regex.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2019 R Core Team
% Distributed under GPL 2 or later

\name{regex}
\alias{regex}
\alias{regexp}
\alias{regular expression}
\concept{regular expression}
\title{Regular Expressions as used in R}
\description{
  This help page documents the regular expression patterns supported by
  \code{\link{grep}} and related functions \code{grepl}, \code{regexpr},
  \code{gregexpr}, \code{sub} and \code{gsub}, as well as by
  \code{\link{strsplit}}.
}
\details{
  A \sQuote{regular expression} is a pattern that describes a set of
  strings.  Two types of regular expressions are used in \R,
  \emph{extended} regular expressions (the default) and
  \emph{Perl-like} regular expressions used by \code{perl = TRUE}.
  There is also \code{fixed = TRUE} which can be considered to use a
  \emph{literal} regular expression.

  Other functions which use regular expressions (often via the use of
  \code{grep}) include \code{apropos}, \code{browseEnv},
  \code{help.search}, \code{list.files} and \code{ls}.
  These will all use \emph{extended} regular expressions.

  Patterns are described here as they would be printed by \code{cat}:
  (\emph{do remember that backslashes need to be doubled when entering \R
    character strings}, e.g.\sspace{}from the keyboard).

  Long regular expression patterns may or may not be accepted: the POSIX
  standard only requires up to 256 \emph{bytes}.
}
\section{Extended Regular Expressions}{
  This section covers the regular expressions allowed in the default
  mode of \code{grep}, \code{grepl}, \code{regexpr}, \code{gregexpr},
  \code{sub}, \code{gsub}, \code{regexec} and \code{strsplit}.  They use
  an implementation of the POSIX 1003.2 standard: that allows some scope
  for interpretation and the interpretations here are those currently
  used by \R.  The implementation supports some extensions to the
  standard.

  Regular expressions are constructed analogously to arithmetic
  expressions, by using various operators to combine smaller
  expressions.  The whole expression matches zero or more characters
  (read \sQuote{character} as \sQuote{byte} if \code{useBytes = TRUE}).

  The fundamental building blocks are the regular expressions that match
  a single character.  Most characters, including all letters and
  digits, are regular expressions that match themselves.  Any
  metacharacter with special meaning may be quoted by preceding it with
  a backslash.  The metacharacters in extended regular expressions are
  \samp{. \\ | ( ) [ \{ ^ $ * + ?}, but note that whether these have a
  special meaning depends on the context.

  Escaping non-metacharacters with a backslash is
  implementation-dependent.  The current implementation interprets
  \samp{\\a} as \samp{BEL}, \samp{\\e} as \samp{ESC}, \samp{\\f} as
  \samp{FF}, \samp{\\n} as \samp{LF}, \samp{\\r} as \samp{CR} and
  \samp{\\t} as \samp{TAB}.  (Note that these will be interpreted by
  \R's parser in literal character strings.)

  A \emph{character class} is a list of characters enclosed between
  \samp{[} and \samp{]} which matches any single character in that list;
  unless the first character of the list is the caret \samp{^}, when it
  matches any character \emph{not} in the list.  For example, the
  regular expression \samp{[0123456789]} matches any single digit, and
  \samp{[^abc]} matches anything except the characters \samp{a},
  \samp{b} or \samp{c}.  A range of characters may be specified by
  giving the first and last characters, separated by a hyphen.  (Because
  their interpretation is locale- and implementation-dependent,
  character ranges are best avoided.)  The only portable way to specify
  all ASCII letters is to list them all as the character class\cr
  \samp{[ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz]}.\cr (The
  current implementation uses numerical order of the encoding.)

  Certain named classes of characters are predefined.  Their
  interpretation depends on the \emph{locale} (see \link{locales}); the
  interpretation below is that of the POSIX locale.

  \describe{
    \item{\samp{[:alnum:]}}{Alphanumeric characters: \samp{[:alpha:]}
      and \samp{[:digit:]}.}

    \item{\samp{[:alpha:]}}{Alphabetic characters: \samp{[:lower:]} and
      \samp{[:upper:]}.}

    \item{\samp{[:blank:]}}{Blank characters: space and tab, and
      possibly other locale-dependent characters such as non-breaking
      space.}

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

    \item{\samp{[:punct:]}}{Punctuation characters:\cr
      \samp{! " # $ \% & ' ( ) * + , - . / : ; < = > ? @ [ \\ ] ^ _ ` \{ | \} ~}.}
%'"`  keep Emacs Rd mode happy

    \item{\samp{[:space:]}}{
      Space characters: tab, newline, vertical tab, form feed, carriage
      return, space and possibly other locale-dependent characters.}

    \item{\samp{[:upper:]}}{Upper-case letters in the current locale.}

    \item{\samp{[:xdigit:]}}{Hexadecimal digits:\cr
      \samp{0 1 2 3 4 5 6 7 8 9 A B C D E F a b c d e f}.}
  }

  For example, \samp{[[:alnum:]]} means \samp{[0-9A-Za-z]}, except the
  latter depends upon the locale and the character encoding, whereas the
  former is independent of locale and character set.  (Note that the
  brackets in these class names are part of the symbolic names, and must
  be included in addition to the brackets delimiting the bracket list.)
  Most metacharacters lose their special meaning inside a character
  class.  To include a literal \samp{]}, place it first in the list.
  Similarly, to include a literal \samp{^}, place it anywhere but first.
  Finally, to include a literal \samp{-}, place it first or last (or,
  for \code{perl = TRUE} only, precede it by a backslash).  (Only
  \samp{^ - \\ ]} are special inside character classes.)

  The period \samp{.} matches any single character.  The symbol
  \samp{\\w} matches a \sQuote{word} character (a synonym for
  \samp{[[:alnum:]_]}, an extension) and \samp{\\W} is its negation
  (\samp{[^[:alnum:]_]}).  Symbols \samp{\\d}, \samp{\\s}, \samp{\\D}
  and \samp{\\S} denote the digit and space classes and their negations
  (these are all extensions).

  The caret \samp{^} and the dollar sign \samp{$} are metacharacters
  that respectively match the empty string at the beginning and end of a
  line.  The symbols \samp{\\<} and \samp{\\>} match the empty string at
  the beginning and end of a word.  The symbol \samp{\\b} matches the
  empty string at either edge of a word, and \samp{\\B} matches the
  empty string provided it is not at an edge of a word.  (The
  interpretation of \sQuote{word} depends on the locale and
  implementation: these are all extensions.)

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
  By default repetition is greedy, so the maximal possible number of
  repeats is used.  This can be changed to \sQuote{minimal} by appending
  \code{?} to the quantifier.  (There are further quantifiers that allow
  approximate matching: see the TRE documentation.)

  Regular expressions may be concatenated; the resulting regular
  expression matches any string formed by concatenating the substrings
  that match the concatenated subexpressions.

  Two regular expressions may be joined by the infix operator \samp{|};
  the resulting regular expression matches any string matching either
  subexpression.   For example, \samp{abba|cde} matches either the
  string \code{abba} or the string \code{cde}.  Note that alternation
  does not work inside character classes, where \samp{|} has its literal
  meaning.

  Repetition takes precedence over concatenation, which in turn takes
  precedence over alternation.  A whole subexpression may be enclosed in
  parentheses to override these precedence rules.

  The backreference \samp{\\N}, where \samp{N = 1 ... 9}, matches
  the substring previously matched by the Nth parenthesized
  subexpression of the regular expression.  (This is an
  extension for extended regular expressions: POSIX defines them only
  for basic ones.)
}
\section{Perl-like Regular Expressions}{
  The \code{perl = TRUE} argument to \code{grep}, \code{regexpr},
  \code{gregexpr}, \code{sub}, \code{gsub} and \code{strsplit} switches
  to the PCRE library that implements regular expression pattern
  matching using the same syntax and semantics as Perl 5.x,
  with just a few differences.

  For complete details please consult the man pages for PCRE, especially
  \command{man pcrepattern} and \command{man pcreapi}, on your system or
  from the sources at \url{http://www.pcre.org}.  (The version in use can be
  found by calling \code{\link{extSoftVersion}}.  It need not be the version
  described in the system's man page.  PCRE1 (reported as version < 10.00 by
  \code{\link{extSoftVersion}}) has been feature-frozen for some time
  (essentially 2012), the man pages at
  \url{http://www.pcre.org/original/doc/html/} should be a good match.
  PCRE2 (PCRE version >= 10.00) has man pages at
  \url{http://www.pcre.org/current/doc/html/}).

  Perl regular expressions can be computed byte-by-byte or
  (UTF-8) character-by-character: the latter is used in all multibyte
  locales and if any of the inputs are marked as UTF-8 (see
  \code{\link{Encoding}}).

  All the regular expressions described for extended regular expressions
  are accepted except \samp{\\<} and \samp{\\>}: in Perl all backslashed
  metacharacters are alphanumeric and backslashed symbols always are
  interpreted as a literal character. \samp{\{} is not special if it
  would be the start of an invalid interval specification.  There can be
  more than 9 backreferences (but the replacement in \code{\link{sub}}
  can only refer to the first 9).

  Character ranges are interpreted in the numerical order of the
  characters, either as bytes in a single-byte locale or as Unicode code
  points in UTF-8 mode.  So in either case \samp{[A-Za-z]} specifies the
  set of ASCII letters.

  In UTF-8 mode the named character classes only match ASCII characters:
  see \samp{\\p} below for an alternative.

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
  \sQuote{ungreedy} mode (so matching is minimal unless \samp{?} is used
  as part of the repetition quantifier, when it is greedy).  Initially
  none of these options are set.

  If you want to remove the special meaning from a sequence of
  characters, you can do so by putting them between \samp{\\Q} and
  \samp{\\E}. This is different from Perl in that \samp{$} and \samp{@} are
  handled as literals in \samp{\\Q...\\E} sequences in PCRE, whereas in
  Perl, \samp{$} and \samp{@} cause variable interpolation.

  The escape sequences \samp{\\d}, \samp{\\s} and \samp{\\w} represent
  any decimal digit, space character and \sQuote{word} character
  (letter, digit or underscore in the current locale: in UTF-8 mode only
  ASCII letters and digits are considered) respectively, and their
  upper-case versions represent their negation.  Vertical tab was not
  regarded as a space character in a \code{C} locale before PCRE 8.34.
  Sequences \samp{\\h}, \samp{\\v}, \samp{\\H} and \samp{\\V} match
  horizontal and vertical space or the negation.  (In UTF-8 mode, these
  do match non-ASCII Unicode code points.)

  There are additional escape sequences: \samp{\\cx} is
  \samp{cntrl-x} for any \samp{x}, \samp{\\ddd} is the
  octal character (for up to three digits unless
  interpretable as a backreference, as \samp{\\1} to \samp{\\7} always
  are), and \samp{\\xhh} specifies a character by two hex digits.
  In a UTF-8 locale, \samp{\\x\{h...\}} specifies a Unicode code point
  by one or more hex digits.  (Note that some of these will be
  interpreted by \R's parser in literal character strings.)

  Outside a character class, \samp{\\A} matches at the start of a
  subject (even in multiline mode, unlike \samp{^}), \samp{\\Z} matches
  at the end of a subject or before a newline at the end, \samp{\\z}
  matches only at end of a subject. and \samp{\\G} matches at first
  matching position in a subject (which is subtly different from Perl's
  end of the previous match).  \samp{\\C} matches a single
  byte, including a newline, but its use is warned against.  In UTF-8
  mode, \samp{\\R} matches any Unicode newline character (not just CR),
  and \samp{\\X} matches any number of Unicode characters that form an
  extended Unicode sequence. \samp{\\X}, \samp{\\R} and \samp{\\B} cannot be
  used inside a character class (with PCRE1, they are treated as characters
  \samp{X}, \samp{R} and \samp{B}; with PCRE2 they cause an error).

  A hyphen (minus) inside a character class is treated as a range, unless it
  is first or last character in the class definition.  It can be quoted to
  represent the hypen literal (\samp{\\-}).  PCRE1 allows an unquoted hyphen
  at some other locations inside a character class where it cannot represent
  a valid range, but PCRE2 reports an error in such cases.

  In UTF-8 mode, some Unicode properties may be supported via
  \samp{\p{xx}} and \samp{\P{xx}} which match characters with and
  without property \samp{xx} respectively.  For a list of supported
  properties see the PCRE documentation, but for example \samp{Lu} is
  \sQuote{upper case letter} and \samp{Sc} is \sQuote{currency symbol}.
  (This support depends on the PCRE library being compiled with
  \sQuote{Unicode property support} which can be checked \emph{via}
  \code{\link{pcre_config}}. PCRE2 when compiled with Unicode support always
  supports also Unicode properies.)

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

  \code{regexpr} and \code{gregexpr} support \sQuote{named capture}.  If
  groups are named, e.g., \code{"(?<first>[A-Z][a-z]+)"} then the
  positions of the matches are also returned by name.  (Named
  backreferences are not supported by \code{sub}.)

  Atomic grouping, possessive qualifiers and conditional
  and recursive patterns are not covered here.
}
\author{
  This help page is based on the TRE documentation and the POSIX
  standard, and the \code{pcrepattern} man page from PCRE 8.36.
}
\seealso{
  \code{\link{grep}}, \code{\link{apropos}}, \code{\link{browseEnv}},
  \code{\link{glob2rx}}, \code{\link{help.search}}, \code{\link{list.files}},
  \code{\link{ls}} and \code{\link{strsplit}}.

  The TRE documentation at
  \url{http://laurikari.net/tre/documentation/regex-syntax/}.

  The POSIX 1003.2 standard at
  \url{http://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap09.html}.

  The \code{pcrepattern} \command{man} page (found as part of
  \url{http://www.pcre.org/original/pcre.txt}), and details of Perl's own
  implementation at \url{http://perldoc.perl.org/perlre.html}.
}
\keyword{character}
