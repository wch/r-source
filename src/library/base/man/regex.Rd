\name{regex}
\alias{regex}
\alias{regexp}
\alias{regular expression}
\concept{regular expression}
\title{Regular Expressions as used in R}
\description{
  This help page documents the regular expression patterns supported by
  \code{\link{grep}} and related functions \code{regexpr}, \code{sub}
  and \code{gsub}, as well as by \code{\link{strsplit}}.

  This is preliminary documentation.
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
  \code{strsplit} is called with argument \code{extended = FALSE}.

  Patterns are described here as they would be printed by \code{cat}: do
  remember that backslashes need to be doubled in entering \R character
  strings from the keyboard.
}
\section{Extended Regular Expressions}{
  This section covers the regular expressions allowed
  if \code{extended = TRUE} in \code{grep}, \code{regexpr}, \code{sub},
  \code{gsub} and \code{strsplit}.  They are the GNU implementation of
  the standard POSIX 1003.2.
  
  Regular expressions are constructed analogously to arithmetic
  expressions, by using various operators to combine smaller expressions.
     
  The fundamental building blocks are the regular expressions that
  match a single character.  Most characters, including all letters and
  digits, are regular expressions that match themselves.  Any
  metacharacter with special meaning may be quoted by preceding it with a
  backslash.  A list of characters enclosed by \code{[} and \code{]}
  matches any single character in that list; if the first character of the
  list is the caret \code{^}, then it matches any character
  \emph{not} in the list. For example, the regular expression
  \code{[0123456789]} matches any single digit.
  A range of characters may be specified by giving the first
  and last characters, separated by a hyphen.

  Certain named classes of characters are predefined, as
  follows.  Their interpretation depends on the \emph{locale} (see
  \link{locales}); the interpretation below is that of the POSIX locale.

  \describe{
    \item{\code{[:alnum:]}}{Alphanumeric characters: \code{[:alpha:]}
      and \code{[:digit:]}.}
      
    \item{\code{[:alpha:]}}{Alphabetic characters: \code{[:lower:]} and
      \code{[:upper:]}.}
    
    \item{\code{[:blank:]}}{Blank characters: space and tab.}

    \item{\code{[:cntrl:]}}{
      Control characters.  In ASCII, these characters have octal codes
      000 through 037, and 177 (\code{DEL}).  In other character sets, these
      are the equivalent characters, if any.}

    \item{\code{[:digit:]}}{Digits: \code{0 1 2 3 4 5 6 7 8 9}.}

    \item{\code{[:graph:]}}{Graphical characters: \code{[:alnum:]} and
      \code{[:punct:]}.}
    
    \item{\code{[:lower:]}}{Lower-case letters in the current locale.}

    \item{\code{[:print:]}}{
      Printable characters: \code{[:alnum:]}, \code{[:punct:]} and space.}
    
    \item{\code{[:punct:]}}{Punctuation characters: \code{! " # $ \% & ' ( ) * + , - . / : ; < = > ? @ [ \\ ] ^ _ ` \{ | \} ~}.}
%'"`  keep Emacs Rd mode happy

    \item{\code{[:space:]}}{
      Space characters: tab, newline, vertical tab, form feed, carriage
      return, and space.}

    \item{\code{[:upper:]}}{Upper-case letters in the current locale.}

    \item{\code{[:xdigit:]}}{Hexadecimal digits: \code{0 1 2 3 4 5 6 7 8 9 A
	B C D E F a b c d e f}.}
  }
   
  For example, \code{[[:alnum:]]} means \code{[0-9A-Za-z]}, except the latter
  depends upon the locale and the character encoding, whereas
  the former is independent of locale and character set.  (Note that the
  brackets in these class names are part of the symbolic names, and must
  be included in addition to the brackets delimiting the bracket list.)
  Most metacharacters lose their special meaning inside lists.  To
  include a literal \code{]}, place it first in the list.  Similarly, to
  include a literal \code{^}, place it anywhere but first.  Finally, to
  include a literal \code{-}, place it last.

  The period \code{.} matches any single character.  The symbol
  \code{\w} is a synonym for \code{[[:alnum:]]} and \code{\W} is a synonym
  for \code{[^[:alnum]]}.

  The caret \code{^} and the dollar sign \code{$} are metacharacters that
  respectively match the empty string at the beginning and end of a line.
  The symbols \code{\<} and \code{\>} respectively match the empty
  string at the beginning and end of a word.  The symbol \code{\b}
  matches the empty string at the edge of a word, and \code{\B} matches
  the empty string provided it is not at the edge of a word.

  A regular expression may be followed by one of several repetition
  operators:
  \describe{
    \item{\code{?}}{The preceding item is optional and will be matched
      at most once.}

    \item{\code{*}}{The preceding item will be matched zero or more times.}

    \item{\code{+}}{The preceding item will be matched one or more times.}

    \item{\code{{n}}}{The preceding item is matched exactly \code{n} times.}

    \item{\code{{n,}}}{The preceding item is matched \code{n} or more times.}
    
    \item{\code{{n,m}}}{The preceding item is matched at least \code{n}
      times, but not more than \code{m} times.}
  }
   
  Two regular expressions may be concatenated; the resulting regular
  expression matches any string formed by concatenating two substrings
  that respectively match the concatenated subexpressions.

  Two regular expressions may be joined by the infix operator \code{|};
  the resulting regular expression matches any string matching either
  subexpression.

  Repetition takes precedence over concatenation, which in turn takes
  precedence over alternation.  A whole subexpression may be enclosed in
  parentheses to override these precedence rules.

  The backreference \code{\N}, where N is a single digit, matches the
  substring previously matched by the Nth parenthesized subexpression of
  the regular expression.

  The code attempts to support traditional
  usage by assuming that \code{\{} is not special if it would be the start
  of an invalid interval specification.  (POSIX allows this behavior as
  an extension.)
}
\section{Basic Regular Expressions}{
  This section covers the regular expressions allowed
  if \code{extended = FALSE} in \code{grep}, \code{regexpr}, \code{sub}
  \code{gsub} and \code{strsplit}.

  In basic regular expressions the metacharacters \code{?}, \code{+},
  \code{\{}, \code{|}, \code{(}, and \code{)} lose their special meaning;
  instead use the backslashed versions \code{\?}, \code{\+},
  \code{\\ \{}, \code{\|}, \code{\(}, and \code{\)}.
}
\section{Perl Regular Expressions}{
  A description will be added to this help page soon.

  Meanwhile please consult the man pages for PCRE (especially \code{man
    pcrepattern} or if that does not exist, \code{man pcre}) on your system
  or from the sources at
  \url{ftp://ftp.csx.cam.ac.uk/pub/software/programming/pcre/}.
  If PCRE support was compiled from the sources within \R, the PCRE
  version is 3.9: PCRE >= 4.0 supports more of the Perl regular expressions.
}
\author{
  This help page is based on the documentation of GNU grep 2.4.2, from
  which the C code used by \R has been taken.
}
\seealso{
  \code{\link{grep}}, \code{\link{apropos}}, \code{\link{browseEnv}},
  \code{\link{help.search}}, \code{\link{list.files}},
  \code{\link{ls}} and \code{\link{strsplit}}.
}
\keyword{character}
