#! /usr/bin/perl -w

use strict;
use Rdoc;
use FileHandle;
use Getopt::Long;

my $dir;
GetOptions( "dir=s" => \$dir );
if ( $dir ) {
  chdir( $dir ) || die "unable to cd to $dir: $!";
}

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
  print "==================\nFile:\t$file\n=====================\n";
  my $rd = Rdoc->new( $file );
  for ( @{ $rd->{ '.order' } } ) {
    &printSections( $_, $rd->{ 'section' } ), next if /^section:/io;
    &printArray( $_, $rd->{ $_ } ), next if /^(alias|keyword)$/io;
    print $_, ":\n";
    &printIndent( Rdoc::link2null( Rdoc::code2quote( $rd->{$_} ) ) );
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
