#! /usr/bin/perl -w
### $Id: Rdconv.pl,v 1.1 1998/04/08 17:58:09 bates Exp $

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

my $dir, $debug, $type, $help;

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
  my $rd = Rdoc->new( $file );
  $rd->SdPrint( $file );
  for ( @{ $rd->{ '.order' } } ) {
    &printSections( $_, $rd->{ 'section' } ), next if /^section:/io;
    &printArray( $_, $rd->{ $_ } ), next if /^(alias|keyword)$/io;
    print $_, ":\n";
    &printIndent( $rd->{$_} );
  }
}


sub printIndent {
  my $lines = shift;
  for ( split( "\n", $lines ) ) {
    print "  ", $_, "\n";
  }
}
    
sub printArray {
  my $name = shift;
  my $arry = shift;
  print $name, ":\n";
  print "  ", join( ', ', @{ $arry } ), "\n";
}

sub printSections {
  my $secname = shift;
  $secname =~ s/^section://io;
  my $sechash = shift;
  print "section - ", $secname, ":\n";
  printIndent( $sechash->{ $secname } );
}

sub usage {

    print "Rdconv version $VERSION\n";
    print "Usage: Rdconv [--debug/-d] [--help/-h]";
    print " [--type/-t html|nroff|Sd|latex|examp] [--dir=directory] <file>\n\n";

    exit 0;
}

