package R::Dcf;

=head1 NAME
    
  R::Dcf - parse files in dcf format (currently handles only files
    with ONE record, if more than one record is contained in the file,
    only the first one is read).
    
=head1 SYNOPSIS

  use R::Dcf;
  $rdcf = R::Dcf->new( "foo" );

  @keys = keys( %{ $rdcf } );     # returns an array of the fields in the file
  $field1 = $rdcf->{ $keys[0] };  # returns the first field

=cut
          
use strict;
use Carp;
use FileHandle;


sub new {

    my ($class, $file) = @_;
    $class = ref($class) || $class;

    my $fh = new FileHandle "< $file" or croak "open($file): $!\n";
    my $self = {};
    my $field = "";
    LINE: while(<$fh>){
	last LINE if($field && /^\s*$/);
	s/\r/ /g;
	s/\s*$//g;
	if(/^\s+/){
	    if($field){
		s/^\s+/\n/;
		$self->{ $field } .= $_;
	    }
	    else{
		die "Malformed DCF file (line $.)\n";
	    }
	}
	else{
	    if(/^(\S+):(.*)/){
		$field=$1;
		$self->{ $field } .= $2;
		$self->{ $field } =~ s/^\s*(.*)\s*$/$1/;
	    }
	    else{
		die "Malformed DCF file (line $.)\n";
	    }
	}
    }	
    $fh->close;
    bless $self, $class;
}

1;




