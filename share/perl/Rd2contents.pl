# Copyright (C) 1997-2001 R Development Core Team
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
use R::Utils;

@knownoptions = ("o|output:s", "os|OS:s");

GetOptions (@knownoptions) || &usage();

sub usage {
  print STDERR <<END;
Usage: perl Rd2contents.pl [options] FILE

Prepare the CONTENTS file for a directory

Options:
  -o, --output=OUT	use \`OUT\' as the output file
      --os=NAME		use OS subdir \`NAME\' (unix, mac or windows)
      --OS=NAME		the same as \`--os\'.
END
  exit 0;
}

my $OSdir = "unix";
$OSdir = $opt_os if $opt_os;

my $out = 0;
if(defined $opt_o && length($opt_o)) { $out = $opt_o; }

if($OSdir eq "mac") {$ARGV[0] =~ /([^\:]*)$/;} else {$ARGV[0] =~ /([^\/]*)$/;}

my $pkg = $1;

my $outfile;
if($out) {
    open(OUTFILE, "> $out") or die "Couldn't open CONTENTS file";
    $outfile = OUTFILE;
} else {
    $outfile = STDOUT;
}

while(glob file_path($ARGV[0], "man", "*.Rd")){ &do_one; }

if(-d  &file_path($ARGV[0], "man", $OSdir)) {
    while(glob file_path($ARGV[0], "man",  $OSdir, "*.Rd")){ &do_one; }
}

sub do_one {
    my $file = basename($_, (".Rd", ".rd"));

    open(rdfile, "<$_");
    undef $text;

    while(<rdfile>){
	$text .= $_;
    }
    close rdfile;

    $text =~ /\\name\{\s*([^\}]+)\s*\}/s;
    my $rdname = $1;
    $rdname =~ s/\n/ /sg;

    $text =~ /\\title\{\s*([^\}]+)\s*\}/s;
    my $rdtitle = $1;
    $rdtitle =~ s/\n/ /sg;

    undef @aliases;
    while($text =~ s/\\alias\{\s*(.*)\s*\}//){
	$alias = $1;
	$alias =~ s/\\%/%/g;
	push @aliases, $alias;
    }

    undef @keywords;
    while($text =~ s/\\keyword\{\s*(.*)\s*\}//){
	$keyword = $1;
	$keyword =~ s/\\%/%/g;
	push @keywords, $keyword;
    }

    $, = " ";
    print $outfile "Entry: $rdname\n";
    print $outfile "Aliases: @aliases\n";
    print $outfile "Keywords: @keywords\n";
    print $outfile "Description: $rdtitle\n";
    print $outfile "URL: ../../../library/$pkg/html/$file.html\n\n";
}

