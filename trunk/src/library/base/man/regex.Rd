\name{regex}
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
    character strings from the keyboard}.
}
\section{Extended Regular Expressions}{
  This section covers the regular expressions allowed if \code{extended
  = TRUE} in \code{grep}, \code{regexpr}, \code{gregexpr}, \code{sub},
  \code{gsub} and \code{strsplit}.  They use the \code{glibc 2.3.6}
  implementation of the POSIX 1003.2 standard.

  Regular expressions are constructed analogously to arithmetic
  expressions, by using various operators to combine smaller expressions.

  The fundamental building blocks are the regular expressions that match
  a single character.  Most characters, including all letters and
  digits, are regular expressions that match themselves.  Any
  metacharacter with special meaning may be quoted by preceding it with
  a backslash.  (Escaping other characters with a backslash is
  undefined.)  The metacharacters are in EREs are
  \code{. \\ | ( ) [ \{ ^ $ * + ?}, but note that whether these have a
  special meaning depends on the context.

  A \emph{character class} is a list of characters enclosed by \code{[}
  and \code{]} which matches any single character in that list; if the first
  character of the list is the caret \code{^}, then it matches any
  character \emph{not} in the list.  For example, the regular expression
  \code{[0123456789]} matches any single digit, and \code{[^abc]} matches
  anything except the characters \code{a}, \code{b} or \code{c}.  A range
  of characters may be specified by giving the first and last characters,
  separated by a hyphen.  (Because their interpretation is so
  system-dependent, they are best avoided.)

  The precise way character ranges are interpreted depends on the values
  of \code{perl} and \code{ignore.case}.  For basic and extended regular
  expressions the collation order is taken from the OS's
  implementation of the setting of the locale category
  \code{LC_COLLATE}, so \code{[W-Z]} may include \code{x} and if it
  does may or may not include \code{w}.  (In most English locales the
  collation order is \code{wWxXyYzZ}.)  For caseless matching the
  characters in a range are interpreted as if in lower case, so in an
  English locale \code{[W-z]} matches \code{WXYZwxyz}.

  For Perl regexps, the ranges are interpreted in the numerical order of
  the characters, either as bytes in an 8-bit locale or as Unicode
  points in a UTF-8 locale.

  Certain named classes of characters are predefined.  Their
  interpretation depends on the \emph{locale} (see \link{locales}); the
  interpretation below is that of the POSIX locale.

  \describe{
    \item{\code{[:alnum:]}}{Alphanumeric characters: \code{[:alpha:]}
      and \code{[:digit:]}.}

    \item{\code{[:alpha:]}}{Alphabetic characters: \code{[:lower:]} and
      \code{[:upper:]}.}

    \item{\code{[:blank:]}}{Blank characters: space and tab.}

    \item{\code{[:cntrl:]}}{
      Control characters.  In ASCII, these characters have octal codes
      000 through 037, and 177 (\code{DEL}).  In another character set,
      these are the equivalent characters, if any.}

    \item{\code{[:digit:]}}{Digits: \code{0 1 2 3 4 5 6 7 8 9}.}

    \item{\code{[:graph:]}}{Graphical characters: \code{[:alnum:]} and
      \code{[:punct:]}.}

    \item{\code{[:lower:]}}{Lower-case letters in the current locale.}

    \item{\code{[:print:]}}{
      Printable characters: \code{[:alnum:]}, \code{[:punct:]} and space.}

    \item{\code{[:punct:]}}{Punctuation characters:
      \code{! " # $ \% & ' ( ) * + , - . / : ; < = > ? @ [ \\ ] ^ _ ` \{ | \} ~}.}
%'"`  keep Emacs Rd mode happy

    \item{\code{[:space:]}}{
      Space characters: tab, newline, vertical tab, form feed, carriage
      return, and space.}

    \item{\code{[:upper:]}}{Upper-case letters in the current locale.}

    \item{\code{[:xdigit:]}}{Hexadecimal digits:
      \code{0 1 2 3 4 5 6 7 8 9 A B C D E F a b c d e f}.}
  }

  For example, \code{[[:alnum:]]} means \code{[0-9A-Za-z]}, except the
  latter depends upon the locale and the character encoding, whereas
  the former is independent of locale and character set.  (Note that the
  brackets in these class names are part of the symbolic names, and must
  be included in addition to the brackets delimiting the bracket list.)
  Most metacharacters lose their special meaning inside lists.  To
  include a literal \code{]}, place it first in the list.  Similarly, to
  include a literal \code{^}, place it anywhere but first.  Finally, to
  include a literal \code{-}, place it first or last (or, for
  \code{perl = TRUE} only, precede it by a backslash.).  (Only these and
  \code{\\} remain special inside character classes.)

  The period \code{.} matches any single character.  The symbol
  \code{\\w} is documented to be synonym for \code{[[:alnum:]]} and
  \code{\\W} is its negation.  However, \code{\\w} also
  matches underscore in the GNU grep code used in \R.

  The caret \code{^} and the dollar sign \code{$} are metacharacters
  that respectively match the empty string at the beginning and end of a
  line.  The symbols \code{\\<} and \code{\\>} respectively match the
  empty string at the beginning and end of a word.  The symbol \code{\\b}
  matches the empty string at the edge of a word, and \code{\\B} matches
  the empty string provided it is not at the edge of a word.

  A regular expression may be followed by one of several repetition
  quantifiers:
  \describe{
    \item{\code{?}}{The preceding item is optional and will be matched
      at most once.}

    \item{\code{*}}{The preceding item will be matched zero or more
      times.}

    \item{\code{+}}{The preceding item will be matched one or more
      times.}

    \item{\code{{n}}}{The preceding item is matched exactly \code{n}
      times.}

    \item{\code{{n,}}}{The preceding item is matched \code{n} or more
      times.}

    \item{\code{{n,m}}}{The preceding item is matched at least \code{n}
      times, but not more than \code{m} times.}
  }
  Repetition is greedy, so the maximal possible number of repeats is used.

  Two regular expressions may be concatenated; the resulting regular
  expression matches any string formed by concatenating two substrings
  that respectively match the concatenated subexpressions.

  Two regular expressions may be joined by the infix operator \code{|};
  the resulting regular expression matches any string matching either
  subexpression.   For example, \code{abba|cde} matches either the
  string \code{abba} or the string \code{cde}.  Note that alternation
  does not work inside character classes, where \code{|} has its literal
  meaning.

  Repetition takes precedence over concatenation, which in turn takes
  precedence over alternation.  A whole subexpression may be enclosed in
  parentheses to override these precedence rules.

  The backreference \code{\\N}, where N is a single digit, matches the
  substring previously matched by the Nth parenthesized subexpression of
  the regular expression.
}
\section{Basic Regular Expressions}{
  This section covers the regular expressions allowed if \code{extended
  = FALSE} in \code{grep}, \code{regexpr}, \code{gregexpr}, \code{sub},
  \code{gsub} and \code{strsplit}.

  In basic regular expressions the metacharacters \code{?}, \code{+},
  \code{\{}, \code{|}, \code{(}, and \code{)} lose their special meaning;
  instead use the backslashed versions \code{\\?}, \code{\\+},
  \code{\\ \{}, \code{\\|}, \code{\\(}, and \code{\\)}.  Thus the
  metacharacters are \code{. \\ [ ^ $ *}.
}
\section{Perl Regular Expressions}{
  The \code{perl = TRUE} argument to \code{grep}, \code{regexpr},
  \code{gregexpr}, \code{sub}, \code{gsub} and \code{strsplit} switches
  to the PCRE library that \sQuote{implements regular expression pattern
  matching using the same syntax and semantics as Perl 5.6 or later,
  with just a few differences}.

  For complete details please consult the man pages for PCRE, especially
  \code{man pcrepattern} and \code{man pcreapi}) on your system or from
  the sources at \url{http://www.pcre.org}. If PCRE
  support was compiled from the sources within \R, the PCRE version is 7.0
  as described here (version \eqn{\ge}{>=} 4.0 is required if \R is
  configured to use the system's PCRE library).

  All the regular expressions described for extended regular expressions
  are accepted except \code{\\<} and \code{\\>}: in Perl all backslashed
  metacharacters are alphanumeric and backslashed symbols always are
  interpreted as a literal character. \code{\{} is not special if it would
  be the start of an invalid interval specification.  There can be more than
  9 backreferences.

  The construct \code{(?...)} is used for Perl extensions in a variety
  of ways depending on what immediately follows the \code{?}.

  Perl-like matching can work in several modes, set by the options
  \code{(?i)} (caseless, equivalent to Perl's \code{/i}), \code{(?m)}
  (multiline, equivalent to Perl's \code{/m}), \code{(?s)} (single line,
  so a dot matches all characters, even new lines: equivalent to Perl's
  \code{/s}) and \code{(?x)} (extended, whitespace data characters are
  ignored unless escaped and comments are allowed: equivalent to Perl's
  \code{/x}).  These can be concatenated, so for example, \code{(?im)}
  sets caseless multiline matching.  It is also possible to unset these
  options by preceding the letter with a hyphen, and to combine setting
  and unsetting such as \code{(?im-sx)}.  These settings can be applied
  within patterns, and then apply to the remainder of the pattern.
  Additional options not in Perl include \code{(?U)} to set
  \sQuote{ungreedy} mode (so matching is minimal unless \code{?} is used,
  when it is greedy).  Initially none of these options are set.

  If you want to remove the special meaning from a sequence of
  characters, you can do so by putting them between \code{\\Q} and
  \code{\\E}. This is different from Perl in that \code{$} and \code{@} are
  handled as literals in \code{\\Q...\\E} sequences in PCRE, whereas in
  Perl, \code{$} and \code{@} cause variable interpolation.

  The escape sequences \code{\\d}, \code{\\s} and \code{\\w} represent any
  decimal digit, space character and \sQuote{word} character
  (letter, digit or underscore in the current locale) respectively, and
  their upper-case versions represent their negation.
  Unlike POSIX and earlier versions of Perl and PCRE, vertical tab is
  not regarded as a whitespace character.

  Escape sequence \code{\\a} is \code{BEL}, \code{\\e} is \code{ESC},
  \code{\\f} is \code{FF}, \code{\\n} is \code{LF}, \code{\\r} is
  \code{CR} and \code{\\t} is \code{TAB}.  In addition \code{\\cx} is
  \code{cntrl-x} for any \code{x}, \code{\\ddd} is the octal character
  \code{ddd} (for up to three digits unless interpretable as a
  backreference), and \code{\\xhh} specifies a character in hex.

  Outside a character class, \code{\\b} matches a word boundary,
  \code{\\B} is its negation, \code{\\A} matches at start of a subject (even
  in multiline mode, unlike \code{^}), \code{\\Z} matches at end of a
  subject or before newline at end, \code{\\z} matches at end of a
  subject. and \code{\\G} matches at first matching position in a
  subject. \code{\\C} matches a single byte. including a newline.
  \code{\\R} matches any Unicode newline character (not just CR).
  \code{\\X} matches any number of Unicode characters that form an
  extended Unicode sequence.

  The same repetition quantifiers as extended POSIX are supported.
  However, if a quantifier is followed by \code{?}, the match is
  \sQuote{ungreedy}, that is as short as possible rather than as long as
  possible (unless the meanings are reversed by the \code{(?U)} option.)

  The sequence \code{(?#} marks the start of a comment which continues
  up to the next closing parenthesis.  Nested parentheses are not
  permitted.  The characters that make up a comment play no part at all in
  the pattern matching.

  If the extended option is set, an unescaped \code{#} character outside
  a character class introduces a comment that continues up to the next
  newline character in the pattern.

  The pattern \code{(?:...)} groups characters just as parentheses do
  but does not make a backreference.

  Patterns \code{(?=...)} and \code{(?!...)} are zero-width positive and
  negative lookahead \emph{assertions}: they match if an attempt to
  match the \code{\dots} forward from the current position would succeed
  (or not), but use up no characters in the string being processed.
  Patterns \code{(?<=...)} and \code{(?<!...)} are the lookbehind
  equivalents: they do not allow repetition quantifiers nor \code{\\C}
  in \code{\dots}.

  Named subpatterns, atomic grouping, possessive qualifiers and conditional
  and recursive patterns are not covered here.
}
\author{
  This help page is based on the documentation of GNU grep 2.4.2 and the
  \code{pcrepattern} man page from PCRE 7.0.
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
