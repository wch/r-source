## Copyright (C) 2000-2002 R Development Core Team
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

package R::Dcf;

=head1 NAME
    
  R::Dcf - parse files in DCF format.
    
=head1 SYNOPSIS

  use R::Dcf;

  $rdcf = R::Dcf->new( "foo" );
  @keys = keys( %{ $rdcf } );     # returns an array of the fields in the file
  $field1 = $rdcf->{ $keys[0] };  # returns the first field

  $rdcf = R::Dcf->new( "foo" , "key" );
  @keys = keys( %{ $rdcf } );       # returns an array of the records
  $field1 = %{$rdcf->{ $keys[0] }}; # returns hash of the first record

=head1 DESCRIPTION

    DCF file "foo" is parsed into a hash array. If "key" is missing (or
    empty), then only the first record in the DFC file is read and
    returned as a hash array (with one element per field). If "key" is
    specified, then multi-record DCF files can also be parsed, and a
    hash array of records is returned, each record being itself a hash
    of fields. "key" is used as identifier for the records.
    
=cut
          
use strict;
use Carp;
use FileHandle;


sub new {

    my ($class, $file, $key) = @_;
    $class = ref($class) || $class;

    my $fh = new FileHandle "< $file" or croak "open($file): $!\n";
    my $self = {};
    my %record;
    my $keyval = "";
    my $field = "";
    LINE: while(<$fh>){
	if($field && /^\s*$/){
	    if($key){
		if($keyval){
		    %{$self->{$keyval}} = %record;
		    $keyval = "";
		}
		undef %record;
		next LINE;
	    }
	    else{
		last LINE;
	    }
	}
	
	s/\r/ /g;
	s/\s*$//g;
	if(/^\s+/){
	    if($field){
		s/^\s+/\n/;
		$record{ $field } .= $_;
	    }
	    else{
		die "Malformed DCF file (file $file, line $.)\n";
	    }
	}
	else{
	    if(/^(\S+):(.*)/){
		$field=$1;
		$record{ $field } = $2;
		$record{ $field } =~ s/^\s*(.*)\s*$/$1/;
		$keyval = $record{ $field } if($field eq $key);
	    }
	    else{
		die "Malformed DCF file (file $file, line $.)\n";
	    }
	}
    }
    $fh->close;
    if(!$key){
	%{$self} = %record;
    }
    bless $self, $class;
}

1;
