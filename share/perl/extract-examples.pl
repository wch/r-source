#-*- perl -*-

## Copyright (C) 2002 R Development Core Team
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
use R::Rdconv;
use R::Rdtools;
use R::Utils;
use R::Vars;

my $revision = ' $Revision: 1.1 $ ';
my $version;
my $name;

$revision =~ / ([\d\.]*) /;
$version = $1;
($name = $0) =~ s|.*/||;

sub usage {
    print STDERR <<END;
Usage: [R CMD] perl /path/to/extract-examples.pl [options] FILE OUT

Extract examples from the Rd source file FILE, writing results to OUT.

Options:
  -h, --help            print short help message and exit
  -v, --version         print version info and exit
      --os=NAME         use OS type NAME (unix, mac or windows)
      --OS=NAME         the same as '--os'
  -V, --verbose         print more information about progress

Email bug reports to <r-bugs\@r-project.org>.
END
  exit 0;
}

my $opt_OS = $R::Vars::OSTYPE;
my $opt_version;
my $opt_verbose;

## <NOTE>
## Used in Rdconv.pm and Rdlists.pm ...
my $OSdir = $R::Vars::OSTYPE;
## </NOTE>

GetOptions("h|help"    => \&usage,
	   "v|version" => \$opt_version,
	   "os|OS:s"   => \$opt_OS,
	   "V|verbose" => \$opt_verbose
	  ) or &usage();

&R_version($name, $version) if $opt_version;

Rdconv($ARGV[0], "example", "", $ARGV[1]);

