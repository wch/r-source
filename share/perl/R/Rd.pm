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

sub info {

    my ($class, $file) = @_;
    $class = ref($class) || $class;

    my $self = {};
    $_ = Rdpp($file);
    if(/\\name\{([^\}]*)\}/){
	$self->{"name"} = $1;
    }
    if(/\\title\{\s*([^\}]*)\s*\}/s){
	$self->{"title"} = $1;
	$self->{"title"} =~ s/\s+/ /sg;
    }
    if(/\\alias\{([^\}]*)\}/){
	push @{$self->{"aliases"}}, $1;
    }
    if(/\\keyword\{([^\}]*)\}/){
	push @{$self->{"keywords"}}, $1;
    }
    bless $self, $class;
}

sub Rdpp {

    my $file = $_[0];
    my $OS = $_[1];
    my $fh = new FileHandle "< $file" or croak "open($file): $!\n";
    my $skipping;
    my $text;
    $OS = "unix" unless $OS;    
    while(<$fh>) {
        if (/^#ifdef\s+([A-Za-z0-9]+)/o) {
            if ($1 ne $OS) { $skipping = 1; }
            next;
        }
        if (/^#ifndef\s+([A-Za-z0-9]+)/o) {
            if ($1 eq $OS) { $skipping = 1; }
            next;
        }
        if (/^#endif/o) {
            $skipping = 0;
            next;
        }
        next if $skipping > 0;
	next if /^\s*%/o;
        $text .= $_;
    }
    close($fh);
    $text;
}

1;
