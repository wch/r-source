package R::Rdtools;

use Carp;
use Exporter;
use FileHandle;
use Text::DelimMatch;

@ISA = qw(Exporter);
@EXPORT = qw(get_section get_usages get_arglist Rdpp);

my $delimcurly = new Text::DelimMatch("\\{", "\\}");
$delimcurly->escape("\\");

my $delimround = new Text::DelimMatch("\\(", "\\)");

sub get_section {

    my ($text, $section) = @_;

    ## remove comments
    $text =~ s/([^\\])%.*\n/$1\n/g;

    my @text = split(/\\$section\{/, " " . $text);
    shift @text;

    my @sections;
    foreach $text (@text) {
	$delimcurly->match("\{" . $text);
	push(@sections, $delimcurly->matched);
    }

    @sections;
}

sub get_usages {

    my ($text, $mode, $verbose) = @_;

    ## remove comments
    $text =~ s/([^\\])%.*\n/$1\n/g;

    ## <FIXME>
    ## This apparently gets quoted args wrong, e.g. in read.table().
    ##   $delimround->quote("\"");
    ##   $delimround->quote("\'");
    ## </FIXME>

    my %usages;
    my @text;
    my $name;
    my $maybe_is_data_set_doc = 0;

    ## Get the \name documented.
    $name = $delimcurly->match(substr($text, index($text, "\\name")));
    $name = substr($name, 1, $#name);

    ## <FIXME>
    ## We need to special-case documentation for data sets, or the
    ## data(FOO) usage for data sets overrides the docs for function
    ## data().  Unless Rd is extended so that there is an Rd type for
    ## data docs, we need to rely on heuristics.  Older versions ignored
    ## all Rd files having 'datasets' as their only keyword.  But this
    ## is a problem: Rd authors might use other keywords to indicate
    ## that a data set is useful for a certain kind of statistical
    ## analysis.  Hence, we do the following: ignore all usages of
    ## data(FOO) in a file with keyword 'datasets' where FOO as only one
    ## argument in the sense that it does not match ','.
    $maybe_is_data_set_doc = 1 if($text =~ "\\keyword\{datasets\}");
    ## </FIXME>

    ## In 'codoc' mode, use \synopsis in case there is one, but warn
    ## about doing so if verbose.
    @text = split(/\\synopsis/, $text) if ($mode eq "codoc");
    if($#text > 0) {
	print "Using synopsis in '$name'\n" if ($verbose);
    } else {
	@text = split(/\\usage/, $text);
    }

    shift @text;

    foreach $text (@text) {

	my $usage = $delimcurly->match($text);

	while($usage) {
	    $usage =~ s/^[\s\n]*//g;

	    ## <FIXME>
	    ## Need to do something smarter about documentation for
	    ## assignment objects.
	    ## </FIXME>

	    ## Try to match the next '(...)' arglist from $usage.
	    my ($prefix, $match, $rest) = $delimround->match($usage);

	    ## Play with $prefix.
	    $prefix =~ s/[\s\n]*$//;
	    $prefix =~ s/^([\s\n\{]*)//;
	    $prefix =~ s/(.*\n)*//g;
	    ## <FIXME>
	    ## Leading semicolons?
	    ##   $prefix =~ s/^;//;
	    ## </FIXME>
	    $prefix =~
		s/\\method\{([a-zA-Z0-9.]+)\}\{([a-zA-Z0-9.]+)\}/$1\.$2/g
		    unless($mode eq "style");

	    ## Play with $match.
	    $match =~ s/=\s*([,\)])/$1/g;
	    $match =~ s/<</\"<</g; # foo = <<see below>> style
	    $match =~ s/>>/>>\"/g; # foo = <<see below>> style
	    $match =~ s/\\%/%/g; # comments

	    if ($rest =~ /^\s*[\n\}]/) {
		if($prefix) {
		    ## $prefix should now be the function name, and
		    ## $match its arg list.
		    ## <FIXME>
		    ## Heuristics for data set documentation once more.
		    return if($maybe_is_data_set_doc
			      && ($prefix eq "data")
			      && ($match !~ /\,/));
		    ## </FIXME>
		    if($usages{$prefix}) {
			## Multiple usages for a function are trouble.
			## We could try to build the full arglist for
			## the function from the usages.  A simple idea
			## is to do
			##   chop($usages{$prefix});
			##   my $foo = ", " . substr($match, 1);
			##   $usages{$prefix} .= $foo;
			## which adds the 'new' args to the 'old' ones.
			## This is not good enough, as it could give
			## duplicate args which is not allowed.  In
			## fact, we would generally need an R parser for
			## the arglist, as the usages could be as bad as
			##   foo(a = c("b", "c"), b = ...)
			##   foo(c = NULL, ...)
			## so that splitting on ',' is not good enough.
			## However, there are really only two functions
			## with justified multiple usage (abline and
			## seq), so we simply warn about multiple usage
			## in case it was not shadowed by a \synopsis
			## unless in mode 'args', where we can cheat.
			if(($mode eq "args") || ($mode eq "style")) {
			    my $save_prefix = $prefix . "0";
			    while($usages{$save_prefix}) {
				$save_prefix .= "0";
			    }
			    $usages{$save_prefix} = $match;
			}
			else {
			    print("Multiple usage for $prefix() " .
				  "in $name\n");
			}
		    } else {
			$usages{$prefix} = $match;
		    }
		}
	    } else {
		$rest =~ s/^.*[\n\}]//g;
	    }
	    $usage = $rest;
	}

    }

    %usages;
}

sub get_arglist {

    ## Get the list of all documented arguments, i.e., the first
    ## arguments from the top-level \item{}{}s in section \arguments,
    ## split on ','.

    my ($text) = @_;
    my @args = ();

    my @keywords = get_section($text, "keyword");
    foreach my $keyword (@keywords) {
    	return ("*internal*") if($keyword =~ /^\{\s*internal\s*\}$/);
    }

    my @chunks = get_section($text, "arguments");
    foreach my $chunk (@chunks) {
	my ($prefix, $match);
	my $rest = substr($chunk, 1, -1);
	while($rest) {
	    ## Try matching top-level \item{}{}.
	    ($prefix, $match, $rest) = $delimcurly->match($rest);
	    if($prefix =~ /\\item$/) {
		## If successful, $match contains the first argument to
		## the \item enclosed by the braces.
		$match =~ s/\\dots/.../g;
		$match =~ s/\n/ /g;
		push(@args, split(/\,\s*/, substr($match, 1, -1)));
	    } else {
		break;
	    }
	    ## Strip off the second argument to \item.
	    ($prefix, $match, $rest) = $delimcurly->match($rest);
	}
    }

    @args;
}

sub Rdpp {

    my ($file, $OS) = @_;
    my $fh = new FileHandle "< $file" or croak "open($file): $!\n";
    my $skip_level;
    my @skip_state;
    my $skip;
    my $text;
    $OS = "unix" unless $OS;    
    while(<$fh>) {
        if (/^#ifdef\s+([A-Za-z0-9]+)/o) {
	    $skip = $1 ne $OS;
	    $skip_level += $skip;
	    push(@skip_state, $skip);
            next;
        }
        if (/^#ifndef\s+([A-Za-z0-9]+)/o) {
	    $skip = $1 eq $OS;
	    $skip_level += $skip;
	    push(@skip_state, $skip);
            next;
        }
        if (/^#endif/o) {
	    $skip_level -= pop(@skip_state);
            next;
        }
        next if $skip_level > 0;
	next if /^\s*%/o;
        $text .= $_;
    }
    close($fh);
    $text;
}

1;
