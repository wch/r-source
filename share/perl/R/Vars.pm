package R::Vars;

=head1 NAME
    
  R::Vars - Platform-Specific Variables
    
=head1 SYNOPSIS

  use R::Vars;

  if($R::Vars::OSTYPE eq "windows"){
      ## do windows-specific things
  }
  else {
      ## do things on mac or unix
  }

  ## stop if no valid command for R itself or the 'R CMD' mechanism is
  ## available 
  R::Vars::error("CMD", "EXE");  

    
=head1 DESCRIPTION

    This package provides variables that help to handle
    platform-specific differences of R, some of these can be set using
    environment variables, these are lsited in square brackates.

    OSTYPE     "mac", "unix" or "windows"
    TMPDIR     name of directory for temporary files [TMPDIR]
    
    RHOME      path to R installation top directory [R_HOME]
    EXE        name of the R executable including path
    CMD        string for 'R CMD' including path

    MAKE       command string for 'make' [MAKE]
    LATEX      command string for 'latex' [LATEX]

    Most have sensible defaults for all platforms.
    
=cut
          
use Carp;

## perl 5.6 uses MSWin32, older versions of perl have win32 
if($^O =~ /^(MS)?Win32$/i){
    $OSTYPE = "windows";
}
elsif($^O =~ /^(MacOS|darwin)$/i){
    $OSTYPE = "mac";
}
else{
    $OSTYPE = "unix";
}

getenv("LATEX", "LATEX", "latex");
getenv("MAKE", "MAKE", "make");
getenv("RHOME", "R_HOME");

if($OSTYPE eq "windows"){
    if($RHOME){
	$EXE = "${RHOME}/bin/Rterm.exe";
	$CMD = "${RHOME}/bin/Rcmd.exe";
    }
    else{
	$EXE = "Rterm.exe";
	$CMD = "Rcmd.exe";
    }
    $TMPDIR = getenv("TMPDIR", "/TEMP");
    $TMPDIR = "" unless (-d $TMPDIR);
}
else{
    if($RHOME){
	$EXE = "${RHOME}/bin/R";
    }
    else{
	$EXE = "R";
    }
    $CMD = "$EXE CMD";
    $TMPDIR = getenv("TMPDIR", "/tmp");
    $TMPDIR = "" unless (-d $TMPDIR);
}


## return the value of an environment variable; or the default if no
## such environment variable is set or it is empty. additionally
## record it in hash envnames (for suitable error messages below.

my %envnames;
sub getenv {

    my ($var, $envvar, $default) = @_;
    if($ENV{$envvar}){
	${$var} = $ENV{$envvar};
    }
    else{
	${$var} = $default;
    }
    $envnames{$var} = $envvar;
}

## check all arguments if they are the name of a variable in this
## package and not empty, issue a warning if not.

sub warning {
    my $v;
    foreach $v (@_) {
	if(! ${"R::Vars::$v"}){
	    if($envnames{$v}){
		carp "Warning: environment variable $envnames{$v} not set " .
		    "and no default available.\n";
	    }
	    else{
		carp "Warning: R::Vars::$v not defined";
	    }

	}
    }
}
	
## check all arguments if they are the name of a variable in this
## package and not empty, issue an error and stop if not.

sub error {
    my $v;
    foreach $v (@_) {
	if(! ${"R::Vars::$v"}){
	    if($envnames{$v}){
		croak "Error: environment variable $envnames{$v} not set " .
		    "and no default available.\n";
	    }
	    else{
		croak "Error: R::Vars::$v not defined";
	    }

	}
    }
}
	
