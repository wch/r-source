## $Id: Rdoc.pm,v 1.1 1998/04/08 17:58:09 bates Exp $

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

  $title = $rd->title();
  @aliases = @{ $rd->alias() };
  %sections = %{ $rd->section() };

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
    my $self = {};
    my $fh = new FileHandle "< $file" or croak "open($file): $!\n";
    $self->{ '.basename' } = basename( $file, ".Rd" );
    $self->{ 'section' } = {};
    $self->{ 'alias' } = [];
    my $seenalias = 0;
    $self->{'keyword'} = [];
    my $seenkeyword = 0;
    my $escapedNewline = 0;
    my $lastEscNL = 0;
    $self->{'.order'} = [];
    my @lines;
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

    my $mc = new Text::DelimMatch "\\{", "\\}", '', "\\", '';
    my ($prefix, $match) = $mc->match( join( "\n", @lines ) );
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
      } elsif ($key =~ /alias/i) { # add to the alias array
	push( @{ $self->{'alias'} }, $match );
	push( @{ $self->{'.order'} }, 'alias' ) unless $seenalias++;
      } elsif ($key =~ /keyword/i) { # add to the keyword array
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
	$match =~ s/(^\{\s*)|(\s*\}$)//og; # strip leading and trailing white space
	push @args, $match;
	($prefix, $match) = $mc->match();
	$match =~ s/(^\{\s*)|(\s*\}$)//og; 
	push @args, $match;
    }
    return( \@args );
}

sub value {
    my $self = shift;
    
}

sub link2null {			# transform \link{str} to str
  my $str = shift;
  return $str unless $str =~ /[^\\]\{/;
  my $out;
  my $mc = new Text::DelimMatch "\\{", "\\}", '', "\\", '';
  my ( $prefix, $match, $remainder ) = $mc->match( $str );
  while ( $prefix || $match ) {
    $match =~ s/(^\{)|(\}$)//og;
    if ( $prefix =~ s/\\link\s*$//o ) {
      $out .= $prefix . $match;
    } else {
      $out .= $prefix . "{" . &link2null($match) . "}";
    }
    last unless $remainder =~ /[^\\]\{/;
    ( $prefix, $match, $remainder ) = $mc->match( );
  }
  $out .= $remainder if $remainder;
  return $out;
}

sub code2quote {		# transform \code{str} to `str'
  my $str = shift;
  return $str unless $str =~ /[^\\]\{/o;
  my $out;
  my $mc = new Text::DelimMatch "\\{", "\\}", '', "\\", '';
  my ( $prefix, $match, $remainder ) = $mc->match( $str );
  while ( $prefix || $match ) {
    $match =~ s/(^\{)|(\}$)//og;
    if ( $prefix =~ s/\\code\s*$//o ) {
      $out .= $prefix . "`" . $match . "'";
    } else {
      $out .= $prefix . "{" . &code2quote( $match ) . "}";
    }
    last unless $remainder =~ /[^\\]\{/o;
    ( $prefix, $match, $remainder ) = $mc->match( );
  }
  $out .= $remainder if $remainder;
  return $out;
}

1;
