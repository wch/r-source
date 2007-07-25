## Copyright (C) 2002-2004 R Development Core Team
##
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
##
## This program is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
## General Public License for more details.
##
## A copy of the GNU General Public License is available at
## http://www.r-project.org/Licenses/

## Send any bug reports to r-bugs@r-project.org.

package R::Logfile;

use Carp;
use FileHandle;

sub new {
    my ($class, $file) = @_;
    $class = ref($class) || $class;

    ## if we are given a non-empty string we try to open the file for
    ## writing
    my $fh;
    if($file){
	$fh = new FileHandle("> $file") || die "open($file): $!\n";
    }
    my $self = {
	"file" => $file,
	"handle" => $fh,
	"stars" => "*",
	"warnings" => 0};
    
    bless $self, $class;
}

sub close {
    my ($x) = @_;
    $x->{"handle"}->close() if $x->{"handle"};
    $x->{"file"}=$x->{"handle"}="";
}

sub print {
    my ($x, $text) = @_;

    print $text;
    $x->{"handle"}->print($text) if $x->{"handle"};
}
    

## setstars sets the characters at the beginning of the lines of all
## subsequent calls to checking, creating and message. We typically use
## * checking for whatever
## ** this is a subtopic of whatever
## etc.
sub setstars {
    my ($x, $stars) = @_;
    $x->{"stars"} = $stars;
}


sub checking {
    my ($x, $text) = @_;
    $x->print($x->{"stars"} . " checking $text ...");
}

sub creating {
    my ($x, $text) = @_;
    $x->print($x->{"stars"} . " creating $text ...");
}

sub message {
    my ($x, $text) = @_;
    $text =~ s/\n/\n$x->{"stars"} /sg;
    $x->print($x->{"stars"} . " $text\n");
}

sub result {
    my ($x, $text) = @_;
    $x->print(" $text\n");
}

sub error {
    my ($x, $text) = @_;
    $x->result("ERROR");
    $x->message($text) if $text;
}

sub warning {
    my ($x, $text) = @_;
    $x->result("WARNING");
    $x->message($text) if $text;
    $x->{"warnings"}++;
}

sub note {
    my ($x, $text) = @_;
    $x->result("NOTE");
    $x->message($text) if $text;
}

sub summary {
    my ($x) = @_;
    if($x->{'warnings'} > 1) {
	print "WARNING: There were $x->{'warnings'} warnings, see\n" .
	    "  " . $x->{'file'} .
		"\nfor details\n";
    }
    if($x->{'warnings'} == 1) {
	print "WARNING: There was 1 warning, see\n" .
	    "  " . $x->{'file'} .
		"\nfor details\n";
    }

}
	    
    
1;
