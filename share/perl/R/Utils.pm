package R::Utils;

#use strict;
use Carp;
use FileHandle;
use Exporter;

@ISA = qw(Exporter);
@EXPORT = qw(R_getenv R_version);


#**********************************************************

## return the value of an environment variable; or the default if no
## such environment variable is set or it is empty.
sub R_getenv {

    my ($envvar, $default) = @_;
    if($ENV{$envvar}){
	return($ENV{$envvar});
    }
    else{
	return($default);
    }
}

sub R_version {

    my ($name, $version) = @_;

    print STDERR <<END;
$name $version

Copyright (C) 1997-2000 R Core Development Team.
This is free software; see the GNU General Public Licence version 2
or later for copying conditions.  There is NO warranty.    
END
    exit 0;
}



#**********************************************************


package R::Logfile;


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

sub summary {
    my ($x) = @_;
    print "WARNING: There were $x->{'warnings'} warnings, see\n" .
	"  " . $x->{'file'} .
	    "\nfor details\n";
}
	    
    

#**********************************************************


1;
