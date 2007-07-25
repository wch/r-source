## Copyright (C) 2000-2001 R Development Core Team
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

package R::Rd;

=head1 NAME
    
  R::Rd - parse files in Rd format
    
=head1 SYNOPSIS

  use R::Rd;

  ## quick parsing giving only name, title, aliases and keywords
  $rdinfo = R::Rd->info( "foo" );

  $rdinfo->{"name"};           # \name{} 
  $rdinfo->{"title"};          # \title{}
  @{$rdinfo->{"aliases"}};     # array with \alias{}es
  @{$rdinfo->{"keywords"}};    # array with \keyword{}s

=cut
          
use strict;
use Carp;
use FileHandle;
use R::Rdtools;

sub info {

    my ($class, $file, $OS) = @_;
    $class = ref($class) || $class;

    my $self = {};

    $OS = "unix" unless $OS;
    my $text = &Rdpp($file, $OS);

    $text =~ /\\name\{\s*([^\}]+)\s*\}/s;
    $self->{"name"} = $1;
    $self->{"name"} =~ s/\n/ /sg;

    $text =~ /\\title\{\s*([^\}]+)\s*\}/s;
    $self->{"title"} = $1;
    $self->{"title"} =~ s/\n/ /sg;
    $self->{"title"} =~ s/\s+/ /sg;

    while($text =~ s/\\alias\{\s*(.*)\s*\}//) {
        my $alias = $1;
        $alias =~ s/\\%/%/g;
        push @{$self->{"aliases"}}, $alias;
    }

    while($text =~ s/\\keyword\{\s*(.*)\s*\}//) {
        my $keyword = $1;
        $keyword =~ s/\\%/%/g;
        push @{$self->{"keywords"}}, $keyword;
    }

    bless $self, $class;
}

1;
