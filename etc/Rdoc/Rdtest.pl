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

				# table for mapping internal tags
sub doFile {
  my $SdInternal = {
		    "link" => [  "",  "" ], # "\link{foo}" becomes "foo"
		    "code" => [ "`", "'" ], # "\code{foo}" becomes "`foo'"
		    "eqn"  => [  "",  "" ], # "\eqn{x}" becomes "x"
		   };
  my $file = shift;
  print "==================\nFile:\t$file\n=====================\n";
  my $rd = Rdoc->new( $file );
#   print scalar( @{ $SdInternal->{'code'} } ), "\n";
#   print $SdInternal->{'code'}[0], "\n";
#   print scalar( @{ $SdInternal->{'link'} } ), "\n";
#   print $SdInternal->{'link'}[0], "\n";
#  print scalar( @{ $SdInternal->{'foo'} } ), "\n";
#  exit 0;
  for ( @{ $rd->{ '.order' } } ) {
    &printSections( $_, $rd->{ 'section' } ), next if /^section:/io;
    &printArray( $_, $rd->{ $_ } ), next if /^(alias|keyword)$/io;
    print $_, ":\n";
    &printIndent( Rdoc::transform( $rd->{$_}, $SdInternal ) );
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
