use Text::DelimMatch;

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

    $delimround->quote("\"");
    $delimround->quote("\'");

    my %usages;

    my @text = split(/\\usage/, $text);

    shift @text;
    foreach $text (@text) {

	my $usage = $delimcurly->match($text);
	
	while($usage){
	    $usage =~ s/^[\s\n]//g;
	    ## FIXME: Need to do something smarter about documentation
	    ## for assignment objects
	    my ($prefix, $match, $rest) = $delimround->match($usage);
	    $prefix =~ s/[\s\n]*$//;
	    $prefix =~ s/^([\s\n\{]*)//;
	    ## FIXME: Leading semicolons?
	    ## $prefix =~ s/^;//;
	    $prefix =~ s/(.*\n)*//g;
	    $prefix =~ s/\[//g;
	    $prefix =~ s/\]//g;
	    $match =~ s/=\s*([,\)])/$1/g;
	    $match =~ s/<</\"<</g;
	    $match =~ s/>>/>>\"/g;
	    $match =~ s/\\%/%/g;
	    if ($rest =~ /^\s*\n/) {
		$usages{$prefix} = $match if $prefix;
	    } else {
		$rest =~ s/^.*\n//g;
	    }
	    $usage = $rest;
	}
	
    }
    
    %usages;
}

1;
