package R::Vars;

## return the value of an environment variable; or the default if no
## such environment variable is set or it is empty.
sub getenv {

    my ($envvar, $default) = @_;
    if($ENV{$envvar}){
	return($ENV{$envvar});
    }
    else{
	return($default);
    }
}

sub file_path {
    my @args = @_;
    my $filesep = "/";
    $filesep = ":" if($R::Vars::OSTYPE eq "mac");
    join($filesep, @args);
}

## <FIXME>
## Currently, R_OSTYPE is always set on Unix/Windows when run via Rcmd
$OSTYPE = getenv("R_OSTYPE", "mac");
## </FIXME>


$LATEX = getenv("LATEX", "latex");
$MAKE = getenv("MAKE", "make");

$RHOME = $ENV{'R_HOME'};
#    croak "Error: environment variable R_HOME not found";

$CMD = $ENV{'R_CMD'};
#    croak "Error: environment variable R_CMD not found";

if($OSTYPE eq "windows"){
    $EXE = file_path($R_HOME, "Rterm.exe");
    $TMPDIR = getenv("TMPDIR", "/TEMP");
}
else{
    $EXE = file_path($R_HOME, "bin", "R");
    $TMPDIR = getenv("TMPDIR", "/tmp");
}
#croak "Error: please set TMPDIR to a valid temporary directory\n"
#    unless (-e $R_TMPDIR);

