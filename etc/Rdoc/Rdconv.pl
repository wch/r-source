#! /usr/bin/perl -w
### $Id: Rdconv.pl,v 1.2 1998/04/08 18:21:49 bates Exp $

##          Convert R documentation files to other formats.
## Copyright (C) 1998 Douglas M. Bates <bates@stat.wisc.edu>

## This file is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.

## This file is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.

## A copy of the GNU General Public License is available via WWW at
## http://www.gnu.org/copyleft/gpl.html.  You can also obtain it by
## writing to the Free Software Foundation, Inc., 59 Temple Place, 
## Suite 330, Boston, MA  02111-1307  USA

## Send any bug reports to bates@stat.wisc.edu

use strict;
use Rdoc;
use FileHandle;
use Getopt::Long;

my ($dir, $debug, $type, $help);
my $VERSION = 0.62.0;

GetOptions( "debug|d" => \$debug,
	    "dir=s" => \$dir,
	    "type|t=s" => \$type,
	    "help|h" => \$help
	  );
&usage() if $help;

chdir( $dir ) || die "unable to cd to $dir: $!" if $dir;

if ( $#ARGV >= 0 ) {
  for ( @ARGV ) {
    &doFile ( $_ );
  }
} else {
  opendir DIR, "." or die "can't open cwd - strange: $!";
  for ( sort ( grep /\.Rd$/, readdir DIR ) ) {
    &doFile( $_ );
  }
}

sub doFile {
  my $file = shift;
  print "   ", $file, "\n";
  my $rd = Rdoc->new( $file );
  $rd->SdPrint( $file );
}


sub usage {

    print "Rdconv version $VERSION\n";
    print "Usage: Rdconv [--debug/-d] [--help/-h]";
    print
	" [--type/-t html|nroff|Sd|latex|examp] [--dir=directory] <file>\n\n";

    exit 0;
}

