package Text::DelimMatch;

use strict;
use vars qw($VERSION @ISA @EXPORT $case_sensitive);

require 5.000;
require Exporter;
require AutoLoader;

@ISA = qw(Exporter AutoLoader);
@EXPORT = qw();
$VERSION = '1.01';

sub new { 
    my $type  = shift;
    my $start = shift;
    my $end   = shift || $start;
    my $esc   = shift;
    my $dblesc= shift;
    my $class = ref($type) || $type;
    my $self  = bless {}, $class;
    local $_  = "no -w warning in evals now";

    eval "/$start/" if defined($start);
    eval "/$end/" if !$@ && defined($end);

    $self->{'STARTREGEXP'} = $start;  # a regexp
    $self->{'ENDREGEXP'}   = $end;    # a regexp
    $self->{'QUOTE'}	   = {};      # a hash of regexp, start => end
    $self->{'ESCAPE'}	   = "";      # a regexp set of chars
    $self->{'DBLESCAPE'}   = "";      # a regexp set of chars

    $self->{'ERROR'}	   = $@;  # false if OK
    $self->{'DEBUG'}	   = 0;	  # boolean
    $self->{'CASESENSE'}   = 0;	  # boolean
    $self->{'FORCESLOW'}   = 0;	  # boolean
    $self->{'KEEP'}	   = 1;	  # boolean

    $self->{'BUFFER'}	   = "";
    $self->{'PRE'}	   = "";
    $self->{'MATCH'}	   = "";
    $self->{'POST'}	   = "";

    $self->escape($esc) if $esc;
    $self->double_escape($dblesc) if $dblesc;
    $self->quote(@_) if @_;

    return $self;
}

sub delim {
    my $self   = shift;
    my $start  = shift;
    my $end    = shift || $start;
    my $curs   = $self->{'STARTREGEXP'};
    my $cure   = $self->{'ENDREGEXP'};
    local $_   = "no -w warning in evals now";

    eval "/$start/" if defined($start);
    eval "/$end/" if !$@ && defined($end);

    $self->{'ERROR'}	   = $@;  # false if OK
    $self->{'STARTREGEXP'} = $start;
    $self->{'ENDREGEXP'}   = $end;

    if ($self->{'DEBUG'}) {
	print "DELIM : $start, $end";
	print ": ", $self->{'ERROR'} if $self->{'ERROR'};
	print "\n";
    }

    return ($curs, $cure);
}

sub quote {
    my $self   = shift;
    my (%oldq) = %{$self->{'QUOTE'}};
    local $_   = "no -w warning in evals now";
    my ($key, $val);

    $key = shift @_;

    if (!defined($key)) {
	$self->{'QUOTE'} = {};
    } else {
	while ($key) {
	    $val = shift @_ || $key;

	    eval "/$key/" if defined($key);
	    eval "/$val/" if !$@ && defined($val);
	    $self->{'ERROR'} = $@ if $@;

	    if ($self->{'DEBUG'}) {
		print "QUOTE : $key = $val";
		print ": ", $self->{'ERROR'} if $self->{'ERROR'};
		print "\n";
	    }

	    $self->{'QUOTE'}->{$key} = $val;
	    $key = shift @_;
	}
    }

    return %oldq;
}

sub escape {
    my $self   = shift;
    my $esc    = shift;
    my $curesc = $self->{'ESCAPE'};
    local $_   = "no -w warning in evals now";

    $esc = '[' . quotemeta($esc) . ']' if defined($esc) && ($esc ne "");

    if (defined($esc) && ($esc ne "")) {
	eval "/$esc/";
	$self->{'ERROR'} = $@ if $@;
    }

    $self->{'ESCAPE'} = $esc;

    if ($self->{'DEBUG'}) {
	print "ESCAPE: $esc";
	print ": ", $self->{'ERROR'} if $self->{'ERROR'};
	print "\n";
    }

    return $curesc;
}

sub double_escape {
    my $self   = shift;
    my $esc    = shift;
    my $curesc = $self->{'DBLESCAPE'};
    local $_   = "no -w warning in evals now";

    $esc = '[' . quotemeta($esc) . ']' if defined($esc) && ($esc ne "");

    if (defined($esc) && ($esc ne "")) {
	eval "/$esc/";
	$self->{'ERROR'} = $@ if $@;
    }

    $self->{'DBLESCAPE'} = $esc;

    if ($self->{'DEBUG'}) {
	print "DBLESC: $esc";
	print ": ", $self->{'ERROR'} if $self->{'ERROR'};
	print "\n";
    }

    return $curesc;
}

sub case_sensitive {
    my $self	 = shift;
    my $setsense = shift;
    my $cursense = $self->{'CASESENSE'};

    $self->{'CASESENSE'} = $setsense || !defined($setsense);

    print "CASE	 : ", $self->{'CASESENSE'}, "\n"
	if $self->{'DEBUG'};

    return $cursense;
}

sub slow {
    my $self	 = shift;
    my $setslow	 = shift;
    my $curslow	 = $self->{'FORCESLOW'};

    $self->{'FORCESLOW'} = $setslow || !defined($setslow);

    print "GOSLOW: ", $self->{'FORCESLOW'}, "\n"
	if $self->{'DEBUG'};

    return $curslow;
}

sub keep {
    my $self	 = shift;
    my $setkeep	 = shift;
    my $curkeep	 = $self->{'KEEP'};

    $self->{'KEEP'} = $setkeep || !defined($setkeep);

    print "KEEP	 : ", $self->{'KEEP'}, "\n"
	if $self->{'DEBUG'};

    return $curkeep;
}

sub debug {
    my $self	 = shift;
    my $setdebug = shift;
    my $curdebug = $self->{'DEBUG'};

    $self->{'DEBUG'} = $setdebug || !defined($setdebug);

    print "DEBUG : ", $self->{'DEBUG'}, "\n"
	if $self->{'DEBUG'};

    return $curdebug;
}

sub error {
    my $self	 = shift;
    my $seterr	 = shift;
    my $curerr	 = $self->{'ERROR'};

    $self->{'ERROR'} = $seterr if defined($seterr);
    return $curerr;
}

sub pre_matched {
    my $self	 = shift;
    $self->{'ERROR'} = "pre_matched requires keep" if !$self->{'KEEP'};
    return $self->{'PRE'};
}

sub matched {
    my $self	 = shift;
    $self->{'ERROR'} = "matched requires keep" if !$self->{'KEEP'};
    return $self->{'MATCH'};
}

sub post_matched {
    my $self	 = shift;
    $self->{'ERROR'} = "post_matched requires keep" if !$self->{'KEEP'};
    return $self->{'POST'};
}

sub dump {
    my $self	 = shift;
    my ($key, $val);

    print "Dump of Text::DelimMatch:\n";

    print "\n\tERROR : ", $self->{'ERROR'}, "\n"
	if $self->{'ERROR'};

    print "\tStart : ", $self->{'STARTREGEXP'}, "\n";
    print "\tEnd   : ", $self->{'ENDREGEXP'}, "\n";
    print "\tEscape: ", $self->{'ESCAPE'}, "\n";
    print "\tDblEsc: ", $self->{'DBLESCAPE'}, "\n";
    print "\tDebug : ", $self->{'DEBUG'}, "\n";
    print "\tCase  : ", $self->{'CASESENSE'}, "\n";
    print "\tSlow  : ", $self->{'FORCESLOW'}, "\n";
    print "\tKeep  : ", $self->{'KEEP'}, "\n";
    print "\tQuote :\n";
    while (($key, $val) = each %{$self->{'QUOTE'}}) {
	print "\t\t$key ... $val\n";
    }
    print "\tBuffer: ", $self->{'BUFFER'}, "\n";
    print "\tPrefix: ", $self->{'PRE'}, "\n";
    print "\tMatch : ", $self->{'MATCH'}, "\n";
    print "\tPost  : ", $self->{'POST'}, "\n\n";
}

sub match {
    my $self   = shift;
    my $string = shift;
    my $state  = 0;
    my $start  = $self->{'STARTREGEXP'};
    my $end    = $self->{'ENDREGEXP'};
    my %quote  = %{$self->{'QUOTE'}};
    my $escape = $self->{'ESCAPE'};
    my $dblesc = $self->{'DBLESCAPE'};
    my $debug  = $self->{'DEBUG'};
    my ($startq, $endq, $specialq) = ("", "", "");
    my ($done) = 0;
    my ($depth) = 0;
    my (@states) = ();
    my ($accum) = "";
    my ($regexp, $match, $pre, $matched, $post);
    my ($scratch);
    local $_   = "no -w warning in evals now";

    return if $self->{'ERROR'};

    if (defined($string)) {
	$self->{'BUFFER'} = $string;
    } else {
	# use post of previous match, if there was a match previously
	$self->{'BUFFER'} = $self->{'POST'} if $self->{'MATCH'} 
    }

    $self->{'PRE'}    = "";
    $self->{'MATCH'}  = "";
    $self->{'POST'}   = "";

    if (!%quote && !$escape && !$dblesc && !$self->{'FORCESLOW'}) {
	return $self->_fast0() if $start eq $end;
	return $self->_fast1();
    }

    # build the regexp that matches the next important thing

    if (%quote) {
	$startq = join (")|(", keys %quote);
	$startq = "($startq)";
    }

    if ($escape || $dblesc) {
	if ($escape && $dblesc) {
	    $specialq = "($escape)|($dblesc)";
	} elsif ($escape) {
	    $specialq = "($escape)";
	} else {
	    $specialq = "($dblesc)";
	}
    }

    $_ = $self->{'BUFFER'};
    $self->{'BUFFER'} = "";
    while ($state != 3) {
	if ($state == 0) {	     # before start tag
	    $regexp = "($start)";
	    $regexp .= "|$startq" if $startq;
	    $regexp .= "|($escape)" if $escape;
	} elsif ($state == 1) {	     # in start tag
	    $regexp = "($start)|($end)";
	    $regexp .= "|$startq" if $startq;
	    $regexp .= "|($escape)" if $escape;
	} elsif ($state == 2) {	     # in quote
	    $regexp = $endq;
	    $regexp .= "|$specialq" if $specialq;
	} else {
	    $self->{'ERROR'} = "BAD STATE!  THIS CAN'T HAPPEN!";
	    return;
	}

	print "STATE: $state: $regexp\n" if $debug;

	($pre, $matched, $post) = $self->_match($regexp, $_);

	print "\tSTR : $_\n" if $debug;
	print "\tPRE : $pre\n" if $debug;
	print "\tMTCH: $matched\n" if $debug;
	print "\tPOST: $post\n" if $debug;

	last if !$matched;

	# First things first, if we've encountered an escaped
	# character, move along
	if ($escape && $self->_match ($escape, $matched)) {
	    $accum .= $pre . $matched;
	    $accum .= substr($post, 0, 1);
	    $_ = substr ($post, 1);
	    next;
	}

	if ($state == 0) {	     # looking for start or startq
	    if ($self->_match($start, $matched)) { # matched start
		$state = 1;
		$depth++;
		print "START: $depth\n" if $debug;

		$self->{'PRE'} = $accum . $pre;
		$accum = $matched;
		$_ = $post;
	    } else {		     # (must have) matched startq
		push (@states, $state);
		$state = 2;
		$accum .= $pre . $matched;
		foreach $scratch (keys %quote) {
		    if ($self->_match ($scratch, $matched)) {
			$endq = $quote{$scratch};
			last;
		    }
		}
		$_ = $post;
	    }
	} elsif ($state == 1) {
	    if ($self->_match($end, $matched)) { # matched end
		$state = 1;
		$depth--;

		print "END : $depth\n" if $debug;
		$accum .= $pre . $matched;
		if ($depth == 0) {
		    $state = 3;
		    $self->{'MATCH'} = $accum;
		    $self->{'POST'} = $post;
		    $_ = "";
		} else {
		    $_ = $post;
		}
	    } elsif ($self->_match($start, $matched)) { # matched start
		$state = 1;
		$depth++;
		print "START: $depth\n" if $debug;

		$accum .= $pre . $matched;
		$_ = $post;
	    } else {		     # (must have) matched startq
		push (@states, $state);
		$state = 2;
		$accum .= $pre . $matched;
		foreach $scratch (keys %quote) {
		    if ($self->_match ($scratch, $matched)) {
			$endq = $quote{$scratch};
			last;
		    }
		}
		$_ = $post;
	    }
	} elsif ($state == 2) {
	    # case 1, matched dblesc and is a doubled char
	    if ($dblesc 
		&& $self->_match ($dblesc, $matched)
		&& ($matched eq substr($post, 0, 1))) { # skip forward
		$accum .= $pre . $matched;
		$accum .= substr($post, 0, 1);
		$_ = substr($post, 1);
		next;
	    } # otherwise check for other things then revisit
		
	    if ($self->_match ($endq, $matched)) { # matched endq
		$state = pop (@states);
		$accum .= $pre . $matched;
		$_ = $post;
	    } else { # (must have) matched a undoubled dblesc
		     # usually this ends a quoted string
		     # (and we'd never get here)
		     # but since it didn't, just skip along
		$accum .= $pre . $matched;
		$_ = $post;
	    }
	} else {
	    $self->{'ERROR'} = "BAD STATE!  THIS CAN'T HAPPEN!";
	    return;
	}
    }

    if ($state == 3) {
	$pre   = $self->{'PRE'};
	$match = $self->{'MATCH'};
	$post  = $self->{'POST'};
    } else {
	$self->{'PRE'}	 = "";
	$self->{'MATCH'} = "";
	$self->{'POST'}	 = "";
	undef $pre;
	undef $match;
	undef $post;
    }
	
    if (!$self->{'KEEP'}) {
	$self->{'PRE'}	 = "";
	$self->{'MATCH'} = "";
	$self->{'POST'}	 = "";
    }

    return wantarray ? ($pre, $match, $post) : $match;
}

sub _fast0 {
    my $self   = shift;
    my $delim  = $self->{'STARTREGEXP'};
    local $_   = $self->{'BUFFER'};
    my ($match, $pre, $match, $post);

    if ($self->{'CASESENSE'}) {
	$match = /^(.*?)($delim.*?$delim)(.*)$/s;
	($pre, $match, $post) = ($1, $2, $3);
    } else {
	$match = /^(.*?)($delim.*?$delim)(.*)$/si;
	($pre, $match, $post) = ($1, $2, $3);
    }

    if ($match) {
	if ($self->{'KEEP'}) {
	    $self->{'PRE'}   = $pre;
	    $self->{'MATCH'} = $match;
	    $self->{'POST'}  = $post;
	}

	return wantarray ? ($pre, $match, $post) : $match;
    } else {
	return wantarray ? (undef, undef, undef) : undef;
    }
}

sub _fast1 {
    my $self   = shift;
    my $string = $self->{'BUFFER'};
    my $start  = $self->{'STARTREGEXP'};
    my $end    = $self->{'ENDREGEXP'};
    my $regexp = "($start)|($end)";
    my $count  = 0;
    my ($match, $realpre, $pre, $post, $matched);

    ($realpre, $match, $post) = $self->_match($start, $string);

    if (defined($match)) {
	$matched = $match;
	$string	 = $post;
	$count++;

	($pre, $match, $post) = $self->_match($regexp, $string);

	while (defined($match)) {
	    $matched .= $pre . $match;

	    if ($self->_match($end, $match)) {
		$count--;
	    } else {
		$count++;
	    }

	    $string = $post;
	    last if $count == 0;

	    ($pre, $match, $post) = $self->_match($regexp, $string);
	}

	if ($count == 0) {
	    if ($self->{'KEEP'}) {
		$self->{'PRE'}	 = $realpre;
		$self->{'MATCH'} = $matched;
		$self->{'POST'}	 = $post;
	    }

	    return wantarray ? ($realpre, $matched, $post) : $matched;
	}
    }

    return wantarray ? (undef, undef, undef) : undef;
}

sub _match {
    my $self   = shift;
    my $regexp = shift;
    local $_   = shift;
    my $match  = 0;
    my ($pre, $matched, $post);

    if ($self->{'CASESENSE'}) {
	$match = /$regexp/s;
	($pre, $matched, $post) = ($`, $&, $');
    } else {
	$match = /$regexp/si;
	($pre, $matched, $post) = ($`, $&, $');
    }

    if ($match) {
	wantarray ? ($pre, $matched, $post) : $matched;
    } else {
	wantarray ? (undef, undef, undef) : undef;
    }
}

sub nested_match {
    my ($search, $start, $end, $three) = @_;
    my $mc = new Text::DelimMatch $start, $end;
    my ($p, $m, $s) = $mc->match($search);

    if (defined($three)) {
	return wantarray ? ($p, $m, $s) : $m;
    } else {
	return wantarray ? ("$p$m", $s) : $m;
    }
}

sub skip_nested_match {
    my ($search, $start, $end, $three) = @_;
    my $mc = new Text::DelimMatch $start, $end;
    my ($p, $m, $s) = $mc->match($search);

    if (defined($three)) {
	return wantarray ? ($p, $m, $s) : $s;
    } else {
	return wantarray ? ("$p$m", $s) : $s;
    }
}

1;
__END__

=head1 NAME

Text::DelimMatch - Perl extension to find regexp delimited strings with proper nesting

=head1 SYNOPSIS

  use Text::DelimMatch;

  $mc = new Text::DelimMatch, $startdelim, $enddelim;

  $mc->quote('"');
  $mc->escape("\\");
  $mc->double_escape('"');
  $mc->case_sensitive(1);

  ($prefix, $match, $remainder) = $mc->match($string);
  ($prefix, $nextmatch, $remainder) = $mc->match();

=head1 DESCRIPTION

These routines allow you to match delimited substrings in a
buffer.  The delimiters can be specified with any regular
expression and the start and end delimiters need not be the
same.  If the delimited text is properly nested, entire nested
groups are returned.

In addition, you may specify quoting and escaping characters that
contribute to the recognition of start and end delimiters.

For example, if you specify the start and end delimiters as '\(' and
'\)', respectively, and the double quote character as a quoting character,
and the backslash as an escaping character, then the delimited substring
in this buffer is "(ma(t)c\)h)":

  'prefix text "(quoted text)" \(escaped \" text) (ma(t)c\)h) postfix text'

In order to support this rather complex interface, the matching context
is encapsulated in an object.  The object, Text::DelimMatch,
has the following public methods:

=over 4

=item new $start, $end, $escape, $dblesc, $qs1, $qe1, ... $qsn, $qen

Creates a new object.  All of the arguments are optional, and can be
set with other methods, but they must be passed in the specified order:
start delimiter, end delimiter, escape characters, double escape characters,
and a set of quote characters.

=item match $string

In an array context, returns ($pre, $match, $post) where $pre is the
text preceding the first match, $match is the matched text (including
the delimiters), and $post is the rest of the text in the buffer.
In a scalar context, returns $match.

If $string is not provided on subsequent calls, the $post from the 
previous match is used, unless keep is false.  If keep is false, the
match always fails.

=item delim $start, $end

Set the start and end delimiters.  Only one set of delimiters can be
in use at any one time.

Returns the delimters in use before this call.

=item quote $startq, $endq

Specifies the start and end quote characters.  Multiple quote
character pairs are supported, so this function is additive.  To
clear the current settings, pass no arguments, e.g.,
$mc->quote().

If only $start is passed, $end is assumed to be the same.

In matching, quotes occur in pairs.  In other words, if (",")
and (',') are both specified as quote pairs and a string
beginning with " is found, it is ended only by another ", not by '.

Returns the quote hash in use before this call.

=item escape $esc

Specifies a set of escaping characters.  This can only be a string
of characters.  $esc can be a regexp set or a simple string.  If it
is a simple string, it will be translated into the regexp set 
"[ quotemeta($esc) ]".

Returns the escape characters in use before this call.

=item double_escape $esc

Specifies a set of double-escaping characters, i.e., characters that
are considered escaped if they occur in pairs.  For example, in some
languages,

  'Don''t you see?'

defines a string containing a single apostrophe.

$esc can only be a string of characters.  $esc can be a regexp
set or a simple string.  If it is a simple string, it will be
translated into the regexp set "[ quotemeta($esc) ]".

Returns the double-escaping characters in use before this call.

=item case_sensitive $bool

Sets case sensitivity to $bool or true if $bool is not specified.

Returns the case sensitivity in use before this call.

=item keep $bool

Sets keep to $bool or true if $bool is not specified.

Keep, which is true by default, specifies whether or not the 
matching context object keeps a local copy of the buffer used in
matching.  Keeping a local copy allows repeated matching on the same
buffer, but might be a bad idea if the buffer is a terabyte long. ;-)

Returns the keep setting in use before this call.

=item error $seterr

Returns the last error that occured.  If $seterr is passed, the error is
set to that value.  Some common kinds of bad input are detected and an
error condition is raised.  If an error condition is raised, all matching
fails until the error is cleared.

The most common error is a bad regular expression, for example specifing
the start delimiter as "(" instead of "\\(".  Remember, these are regexps!

=item pre_matched

Returns the prefix text from the last match if keep is true.  Sets
an error and returns an empty string if keep is false.

=item matched

Returns the matched text from the last match if keep is true.  Sets
an error and returns an empty string if keep is false.

=item post_matched

Returns the postfix text from the last match if keep is true.  Sets
an error and returns an empty string if keep is false.

=item debug $bool

Sets debug to $bool or true if $bool is not specified.

If debug is true, informative and progress messages are printed
to STDOUT by some methods.

Returns the debugging setting in use before this call.

=item dump

For debugging, prints all of the instance variables for a particular
object.

=item slow $bool

For debugging.  Some classes of delimited strings can be located
with much faster algorithms than can be used in the most general
case.  If slow is true, the slower, general algorithm is always
used.

=back

For simplicity, and backward compatibility with the previous
(limited release) incarnation of this module, the following 
functions are also available directly:

=over 4

=item nested_match ($string, $start, $end, $three)

If $three is true, returns ($pre, $match, $post) in an array context
otherwise returns ("$pre$match", $post).  In a scalar context, returns
"$pre$match".


=item skip_nested_match ($string, $start, $end, $three)

If $three is true, returns ($pre, $match, $post) in an array context
otherwise returns ("$pre$match", $post).  In a scalar context, returns
$post.

=back

=head1 EXAMPLES

  $mc = new Text::DelimMatch '"';
  $mc->('pre "match" post') == '"match"';

  $mc->delim("\\(", "\\)");
  $mc->('pre (match) post')   == ('pre ', '(match)', ' post');
  $mc->('pre (ma(t)ch) post') == ('pre ', '(ma(t)ch)', ' post');
 
  $mc->quote('"');
  $mc->escape("\\");
  $mc->('pre (ma")"tch) post') == ('pre ', '(ma")"tch)', ' post');
  $mc->('pre (ma(t)c\)h\") post') == ('pre ', '(ma(t)c\)h\")', ' post');

See also test.pl in the distribution.

=head1 AUTHOR

Norman Walsh, norm@berkshire.net

=head1 COPYRIGHT

Copyright (C) Small Planet Software and Norman Walsh.
All rights reserved.  This program is free software; you can 
redistribute it and/or modify it under the same terms as Perl itself.

=head1 WARRANTY

THIS PACKAGE IS PROVIDED "AS IS" AND WITHOUT ANY EXPRESS OR
IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR
PURPOSE.

=head1 SEE ALSO

perl(1).

=cut

