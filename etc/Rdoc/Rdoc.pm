## $Id: Rdoc.pm,v 1.3 1998/04/16 15:39:47 bates Exp $

## R documentation objects created from a file.

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

package Rdoc;

=head1 NAME

 Rdoc - class for contents of a documentation file for R

=head1 SYNOPSIS

  use Rdoc;
  $rd = Rdoc->new( "foo.Rd" );

  @keys = keys( %{ $rd } );     # returns an array of the sections in the file
  @ordered = @{ $rd->{ '.order' } }; # sections in order seen
  $title = $rd->{ 'title' };         # returns the title section
  @aliases = @{ $rd->{ 'alias' } };  # aliases are an array
  @keywords = @{ $rd->{'keyword'}};  # as are keywords
  %sections = %{ $rd->{'section'}};  # sections are a hash

=head1 DESCRIPTION

The Rdoc class takes a file of R documentation in .Rd format, strips
the comments, and parses the rest of the file into separate sections.
The object itself is a hash.  The different sections are entries in
the hash.

=cut

use strict;
use Carp;
use FileHandle;
use Text::DelimMatch;
use File::Basename;

sub new {
    my ($class, $file) = @_;
    $class = ref($class) || $class;
    my $fh = new FileHandle "< $file" or croak "open($file): $!\n";
    my @lines;
    my $escapedNewline = 0;
    my $lastEscNL = 0;
    while (<$fh>) {
	next if /^\s*\%/o;	# eliminate complete comment lines
	chomp;			# strip the newline
	s/\\\%/\007/og;		# substitute escaped `%' characters 
	$escapedNewline = s/\%$//o;
	s/\s*\%.*$//o;		# strip comment endings
	s/\007/%/og;		# re-insert escaped `%' characters w/o the escape
	s/^\s+//o;		# strip leading white space
	if ( $lastEscNL ) {
	  $lines[ $#lines ] .= $_;
	} else {
	  push( @lines, $_ );
	}
	$lastEscNL = $escapedNewline;
    }
    $fh->close;

    my $mc = new Text::DelimMatch "\\{", "\\}", '', "\\", '';
    my ($prefix, $match) = $mc->match( join( "\n", @lines ) );
    my $self = {};
    $self->{ '.basename' } = basename( $file, ".Rd" );
    $self->{ 'section' } = {};
    $self->{ 'alias' } = [];
    my $seenalias = 0;
    $self->{ 'keyword' } = [];
    my $seenkeyword = 0;
    $self->{ '.order' } = [];
    while ( $match ) {
      croak "Malformed R documentation file $file\n"
	unless ($prefix =~ /\s*\\(\w+)\s*$/o);
      my $key = $1;
      $match =~ s/\s*\}$//o;	# strip trailing white space and delimiter
      $match =~ s/^\{\s*//o;	# strip leading white space and delimiter
      if ($key =~ /section/i) { # swallow the section
	my $secname = $match;
	($prefix, $match) = $mc->match();
	$match =~ s/\s*\}$//o;
	$match =~ s/^\{\s*//o;
	($self->{ 'section' })->{ $secname } = $match;
	push( @{ $self->{'.order'} }, 'section:' . $secname );
      } elsif ($key =~ /alias/io) { # add to the alias array
	push( @{ $self->{'alias'} }, $match );
	push( @{ $self->{'.order'} }, 'alias' ) unless $seenalias++;
      } elsif ($key =~ /keyword/io) { # add to the keyword array
	push( @{ $self->{'keyword'} }, $match );
	push( @{ $self->{'.order'} }, 'keyword' ) unless $seenkeyword++;
      } else {
	$self->{ $key } = $match;
	push( @{ $self->{'.order'} }, $key );
      }
      ($prefix, $match ) = $mc->match();
    }
    delete $self->{'keyword'} unless $seenkeyword;
    delete $self->{'alias'} unless $seenalias;
    delete $self->{'section'} unless scalar( keys( %{ $self->{'section'} } ) ) > 0;
    bless $self, $class;
}

sub arguments {
    my $self = shift;
    my @args;
    my $mc = new Text::DelimMatch "\\{", "\\}", '', "\\", '';
    my ($prefix, $match) = $mc->match( $self->{'arguments'} );
    while ( $match ) {
	croak "Malformed R documentation file.\n"
	    unless ($prefix =~ /\s*\\item\s*$/o);
			      # strip leading and trailing white space
	$match =~ s/(^\{\s*)|(\s*\}$)//og;
	push @args, $match;
	($prefix, $match) = $mc->match();
	$match =~ s/(^\{\s*)|(\s*\}$)//og; 
	push @args, $match;
	($prefix, $match) = $mc->match();
    }
    return( \@args );
}

sub transform {		# transform internal tags to output forms
  my $str = shift;
  return $str unless $str =~ /[^\\]\{/o;
  my $trTpt = shift;
  my $out;
  my $mc = new Text::DelimMatch "\\{", "\\}", '', "\\", '';
  my ( $prefix, $match, $remainder ) = $mc->match( $str );
  while ( $prefix || $match ) {
    $match =~ s/(^\{)|(\}$)//og;
    if ( $prefix =~ s/\\(\w+)\s*$//o ) {
      if ( $trTpt->{ $1 } ) {
	my @subs = @{ $trTpt->{ $1 } };
	$out .= $prefix . $subs[0] . &transform( $match, $trTpt ) . $subs[1];
      } else {
	carp "unknown tag $1 seen\n";
	$out .= $prefix . &transform( $match, $trTpt );
      }
    } else {
      $out .= $prefix . "{" . &transform( $match, $trTpt ) . "}";
    }
    last unless $remainder =~ /[^\\]\{/o;
    ( $prefix, $match, $remainder ) = $mc->match( );
  }
  $out .= $remainder if $remainder;
  return $out;
}

sub SdPrint {			# print in S-PLUS .d format
  my $SdSectRef = {		# table for mapping sections
		   "name"        => [ ".FN  ", "\n" ],
		   "title"       => [ ".TL  ", "\n" ],
		   "usage"       => [ ".CS\n", "\n" ],
		   "alias"       => [ ], # do nothing
		   "arguments"   => [ ], # should cause an error
		   "description" => [ ".DT\n", "\n" ],
		   "value"       => [ ".RT\n", "\n" ],
		   "references"  => [ ".SH REFERENCES\n", "\n" ],
		   "note"        => [ ".SH NOTE\n", "\n" ],
		   "author"      => [ ],
		   "seealso"     => [ ".SA\n", "\n" ],
		   "examples"    => [ ".EX\n", "\n" ],
		   "keyword"     => [ ]
		  };

  my $SdInternal = {		# Table for mapping internal tags
		  "link" => [  "",  "" ], # "\link{foo}" becomes "foo"
		  "code" => [ "`", "'" ], # "\code{foo}" becomes "`foo'"
		  "eqn"  => [  "",  "" ], # "\eqn{x}" becomes "x"
		 };
  my $self = shift;
  my $file = $self->{ '.basename' } . ".d";
  my $fh = new FileHandle "> $file" or croak "open($file): $!\n";
  
  for ( "name", "title", "keyword" ) { # check for required sections
    croak "File $file does not have a \\$_ section" unless $self->{$_};
  }
  my @args;
  for ( @{ $self->{ '.order' } } ) {
    next if /alias/o;
    next if /author/o;
    if ( /arguments/o ) {
      @args = @{ $self->arguments() };
      while ( $#args >= 0 ) {
	print $fh ".AG ", shift( @args ), "\n";
	print $fh &transform( shift( @args ), $SdInternal ), "\n";
      }
      next;
    }
    if ( /keyword/o ) {
      @args = @{ $self->{'keyword'} };
      while ( $#args >= 0 ) {
	print $fh ".KW ", shift( @args ), "\n";
      }
      next;
    }
    if ( $SdSectRef->{$_} ) {
      print $fh $SdSectRef->{$_}[0],
      &transform( $self->{$_}, $SdInternal ) , $SdSectRef->{$_}[1];
    } else {
      print "unknown section name $_\n";
    }
  }
  print $fh ".WR\n";
  $fh->close;
}

1;
