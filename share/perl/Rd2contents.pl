# Copyright (C) 1997-2002 R Development Core Team
#
# This document is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# A copy of the GNU General Public License is available via WWW at
# http://www.gnu.org/copyleft/gpl.html.  You can also obtain it by
# writing to the Free Software Foundation, Inc., 59 Temple Place,
# Suite 330, Boston, MA  02111-1307  USA.

use File::Basename;
use Getopt::Long;
use R::Rd;
use R::Rdtools;
use R::Utils;
use R::Vars;

my $OS_type = $R::Vars::OSTYPE;

my @known_options = ("o|output:s", "os|OS:s");
GetOptions (@known_options) || &usage();

$OS_type = $opt_os if $opt_os;

my $out = 0;
$out = $opt_o if(defined $opt_o && length($opt_o));

my $pkg;

$ARGV[0] =~ /([^\/]*)$/;
$pkg = $1;


my $outfile;
if($out) {
    open(OUTFILE, "> $out") or die "Cannot write to '$out'";
    $outfile = OUTFILE;
} else {
    $outfile = STDOUT;
}

my @Rdfiles =
    &list_files_with_type(&file_path($ARGV[0], "man"),
			  "docs",
			  $OS_type);

foreach my $rdfile (@Rdfiles) {
    my $file = basename($rdfile, (".Rd", ".rd"));
    my $rdinfo = R::Rd->info($rdfile, $OS_type);
    print $outfile "Entry: " . $rdinfo->{"name"} . "\n";
    print $outfile "Aliases: " .
	join(" ", @{$rdinfo->{"aliases"}}) . "\n";
    print $outfile "Keywords: " .
	join(" ", @{$rdinfo->{"keywords"}}) . "\n";
    print $outfile "Description: " . $rdinfo->{"title"} . "\n";

    ## <FIXME>
    ## This has extension 'html' hard-wired.
    print $outfile "URL: ../../../library/$pkg/html/$file.html\n\n";
    ## </FIXME>
}


sub usage {
  print STDERR <<END;
Usage: [R CMD] perl /path/to/Rd2contents.pl [options] FILE

Prepare the CONTENTS file for a directory.

Options:
  -o, --output=OUT	use 'OUT' as the output file
      --os=NAME		use OS type 'NAME' (unix, mac or windows)
      --OS=NAME		the same as '--os'.
END
  exit 0;
}
