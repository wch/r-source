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

## Usage: extract-usage FILE

use Getopt::Long;
use R::Rdtools;
use R::Utils;
use R::Rd;

my $revision = ' $Revision: 1.2 $ ';
my $version;
my $name;

$revision =~ / ([\d\.]*) /;
$version = $1;
($name = $0) =~ s|.*/||;

sub usage {
    print STDERR <<END;
Usage: R CMD perl /path/to/extract-usage.pl [options] FILE

Extract usage information for use by codoc from the Rd source files
listed in FILE.

Options:
  -h, --help            print short help message and exit
  -v, --version         print version info and exit
      --os=NAME         use OS subdir \`NAME\' (unix, mac or windows)
      --OS=NAME         the same as \`--os\'.

Email bug reports to <r-bugs\@r-project.org>.
END
  exit 0;
}

my $OSdir = R_getenv("R_OSTYPE", "unix");

my @knownoptions = ("h|help", "v|version", "os|OS:s");
GetOptions (@knownoptions) || &usage();
&R_version($name, $version) if $opt_v;
&usage() if $opt_h;

$OSdir = $opt_os if $opt_os;

open INFILE, "< $ARGV[0]" || die "Can't open input file";
open OUTFILE, "> $ARGV[1]" || die "Can't open output file";

while (<INFILE>) {
    chomp;
    open RDFILE, "< $_";
    print OUTFILE "# usages in file $_\n";

    my $text = R::Rd::Rdpp($_, $OSdir);

    {
	local $/; # unset for get_usages
	%usages = get_usages($text);
    }
    
    foreach $key (keys(%usages)){
	$usages{$key} =~ s/ *\\.?dots/ .../g;
	if ($key !~ /^</) {
	    print OUTFILE "$key <- function$usages{$key} NULL\n";
	}
    }
    
    print OUTFILE "\n";
}
