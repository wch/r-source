#-*- perl -*-

## Copyright (C) 2000, 2001 R Development Core Team
##
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
##
## This program is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## A copy of the GNU General Public License is available via WWW at
## http://www.gnu.org/copyleft/gpl.html.  You can also obtain it by
## writing to the Free Software Foundation, Inc., 59 Temple Place,
## Suite 330, Boston, MA  02111-1307  USA.
##
## Send any bug reports to r-bugs@r-project.org

use Getopt::Long;
use R::Rdtools;
use R::Utils;

my $revision = ' $Revision: 1.8 $ ';
my $version;
my $name;

$revision =~ / ([\d\.]*) /;
$version = $1;
($name = $0) =~ s|.*/||;

sub usage {
    print STDERR <<END;
Usage: [R CMD] perl /path/to/extract-usage.pl [options] FILE OUT

Extract usage information for use by codoc from the Rd source files
listed in FILE, writing results to OUT.

Options:
  -h, --help            print short help message and exit
  -v, --version         print version info and exit
      --mode=MODE       use operation mode MODE (codoc, args or style)
      --os=NAME         use OS subdir \`NAME\' (unix, mac or windows)
      --OS=NAME         the same as \`--os\'.

Email bug reports to <r-bugs\@r-project.org>.
END
  exit 0;
}

## <FIXME>
## Currently, R_OSTYPE is always set on Unix/Windows.
my $OSdir = R_getenv("R_OSTYPE", "mac");
## </FIXME>
my $opt_mode = "codoc";
my $opt_v;

GetOptions("h|help"    => \&usage,
	   "v|version" => \$opt_v,
	   "os|OS:s"   => \$OSdir,
	   "mode=s"    => \$opt_mode
	  ) or &usage();

&R_version($name, $version) if $opt_v;

open(INFILE, "< $ARGV[0]") or die "Can't open input file";
open(OUTFILE, "> $ARGV[1]") or die "Can't open output file";

while (<INFILE>) {
    chomp;
    open RDFILE, "< $_";
    ## <NOTE>
    ## This is really dangerous ...
    my @chunks = split(/\\name/, &Rdpp($_, $OSdir));
    ## </NOTE>
    foreach my $text (@chunks) {
	next if($text !~ /^\s*{\s*([^}]*[^}\s])\s*}.*/);
	print OUTFILE "# usages in documentation object $1\n";
	print OUTFILE "# arglist: ", join(" ", get_arglist($text)), "\n"
	  unless ($opt_mode eq "codoc");

	## Put \name back in front as get_usages() needs it.  Could also
	## pass on the name we just determined ...
	$text = "\\name" . $text;

	{
	    local $/;		# unset for get_usages()
	    %usages = get_usages($text, $opt_mode);
	}

	foreach my $key (keys(%usages)){
	    $usages{$key} =~ s/ *\\.?dots/ .../g;
	    if ($key !~ /^</) {
		print OUTFILE "$key <- function$usages{$key} NULL\n";
	    }
	}

	print OUTFILE "\n";
    }
}
