## Perl script to create a TITLE file from the package DESCRIPTION.
##   Usage: [R CMD] perl /path/to/maketitle.pl FILE

require 5.005;  # for Text::Wrap::fill in formatDL
use R::Dcf;
use R::Utils;

my $description = new R::Dcf($ARGV[0]);

print formatDL($description->{"Package"},
	       $description->{"Title"},
	       "table", 72, 16), "\n";

### Local Variables: ***
### mode: perl ***
### perl-indent-level: 4 ***
### End: ***
