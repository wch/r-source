package R::Rdtools;

use Text::DelimMatch;
use Exporter;

@ISA = qw(Exporter);
@EXPORT = qw(get_section get_usages);

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

    my ($text) = @_;
    
    ## remove comments
    $text =~ s/([^\\])%.*\n/$1\n/g;

    ## FIXME:
    ## This apparently gets quoted args wrong, e.g. in read.table().
    ##   $delimround->quote("\"");
    ##   $delimround->quote("\'");
    ## </FIXME>

    my %usages;
    my @text;
    my $name;

    ## Get the \name documented.
    $name = $delimcurly->match(substr($text, index($text, "\\name")));
    $name = substr($name, 1, $#name);

    if($text =~ "\\keyword\{datasets\}") {
	## FIXME:
	## Skip documentation for data set.
	## If Rd is extended so that there is an Rd type for data docs,
	## do something smarter.  Currently, the data(FOO) usage for
	## data sets overrides the docs for function data().  Hence, we
	## ignore all files which have `datasets' as their only keyword.
	my @foo = split(/\\keyword\{/, $text);
	if($#foo <= 1) {
	    return;
	}
	## </FIXME>
    }
    ## Use \synopsis in case there is one, but warn about doing so.
    @text = split(/\\synopsis/, $text);
    if($#text > 0) {
	print "Using synopsis in \`$name'\n";	    
    } else {
	@text = split(/\\usage/, $text);
    }

    shift @text;

    foreach $text (@text) {

	my $usage = $delimcurly->match($text);
	
	while($usage) {
	    $usage =~ s/^[\s\n]*//g;

	    ## FIXME:
	    ## Need to do something smarter about documentation for
	    ## assignment objects.

	    ## Try to match the next `(...)' arglist from $usage.
	    my ($prefix, $match, $rest) = $delimround->match($usage);

	    ## Play with $prefix.
	    $prefix =~ s/[\s\n]*$//;
	    $prefix =~ s/^([\s\n\{]*)//;
	    $prefix =~ s/(.*\n)*//g;
	    ## FIXME: Leading semicolons?
	    ##   $prefix =~ s/^;//;
	    ## </FIXME>
	    ## FIXME:
	    ## Hack for method objects documented as `generic[.class]'.
	    ## Eliminate if Rd allows for \method{GENERIC}{CLASS} markup.
	    $prefix =~ s/\[//g;
	    $prefix =~ s/\]//g;
	    ## </FIXME>

	    ## Play with $match.
	    $match =~ s/=\s*([,\)])/$1/g;
	    $match =~ s/<</\"<</g; # foo = <<see below>> style
	    $match =~ s/>>/>>\"/g; # foo = <<see below>> style
	    $match =~ s/\\%/%/g; # comments

	    if ($rest =~ /^\s*[\n\}]/) {
		if($prefix) {
		    ## $prefix should now be the function name, and
		    ## $match its arg list.
		    if($usages{$prefix}) {
			## Multiple usages for a function are trouble.
			## We could try to build the full arglist for
			## the function from the usages.  A simple idea
			## is to do
			##   chop($usages{$prefix});
			##   my $foo = ", " . substr($match, 1);
			##   $usages{$prefix} .= $foo;
			## which adds the `new' args to the `old' ones.
			## This is not good enough, as it could give
			## duplicate args which is not allowed.  In
			## fact, we would generally need an R parser for
			## the arglist, as the usages could be as bad as
			##   foo(a = c("b", "c"), b = ...)
			##   foo(c = NULL, ...)
			## so that splitting on `,' is not good enough.
			## However, there are really only two functions
			## with justified multiple usage (abline and
			## seq), so we simply warn about multiple usage
			## in case it was not shadowed by a \synopsis.
			print("Multiple usage for $prefix() in $name\n");
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

1;
