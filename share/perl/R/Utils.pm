package R::Utils;

use Carp;
use FileHandle;
use Exporter;
use R::Vars;
use Text::Wrap;
use Text::Tabs;

@ISA = qw(Exporter);
@EXPORT = qw(R_cwd R_getenv R_version R_tempfile R_system R_runR R_run_R
	     file_path env_path
	     list_files list_files_with_exts list_files_with_type
	     make_file_exts
	     read_lines
	     shell_quote_file_path
	     sQuote dQuote
	     config_val_to_logical);

### ********************************************************************

### * R_cwd

sub R_cwd {
    my $abspath = Cwd::cwd();
    if($R::Vars::OSTYPE eq "windows") {
	# ensure there are no spaces in the paths.
	Win32::GetShortPathName($abspath) if $abspath =~ / /;
    }
    $abspath;
}

### * R_getenv

sub R_getenv {
    ## Return the value of an environment variable; or the default if no
    ## such environment variable is set or it is empty.
    my ($envvar, $default) = @_;
    if($ENV{$envvar}){
	return($ENV{$envvar});
    }
    else{
	return($default);
    }
}

### * R_version

sub R_version {

    my ($name, $version) = @_;

    print STDERR <<END;
$name $version

Copyright (C) 1997-2003 R Core Development Team.
This is free software; see the GNU General Public Licence version 2
or later for copying conditions.  There is NO warranty.
END
    exit 0;
}

### * R_tempfile

sub R_tempfile {
    my $pat = "Rutils";
    $pat = $_[0] if $_[0];
    R::Vars::error("TMPDIR");
    my $retval = file_path($R::Vars::TMPDIR,
			   $pat . $$ . sprintf("%05d", rand(10**5)));

    my $n=0;
    while(-f $retval){
	$retval = file_path($R::Vars::TMPDIR,
			    $pat . $$ . sprintf("%05d", rand(10**5)));
	croak "Cannot find unused name for temporary file"
	    if($n++ > 1000);
    }
    $retval;
}

### * R_system

sub R_system
{
    my ($cmd, $Renv) = @_;
    my $tmpf = R_tempfile();
    if($R::Vars::OSTYPE eq "windows") {
	open(tmpf, "> $tmpf")
	  or die "Error: cannot write to '$tmpf'\n";
	print tmpf "$cmd $Renv\n";
	close tmpf;
	$res = system("sh $tmpf");
	unlink($tmpf);
	return $res;
    } else {
	return system("$Renv $cmd");
    }
}

### * R_runR

sub R_runR
{
    my ($cmd, $Ropts, $Renv) = @_;
    my $Rin = R_tempfile("Rin");
    my $Rout = R_tempfile("Rout");

    R::Vars::error("R_EXE");
    open RIN, "> $Rin" or die "Error: cannot write to '$Rin'\n";
    print RIN "$cmd\n";
    close RIN;
    R_system(join(" ",
		  (&shell_quote_file_path(${R::Vars::R_EXE}),
		   "${Ropts} < ${Rin} > ${Rout} 2>&1")),
	     $Renv);
    my @out;
    open ROUT, "< $Rout";
    while(<ROUT>) {chomp; push(@out, $_);}
    close ROUT;
    unlink($Rin);
    unlink($Rout);
    return(@out);
}

### * R_run_R

sub R_run_R {
    ## A variant of R_runR (see above) which returns both exit status
    ## from the call to R as well as stdout, and maybe eventually also
    ## stderr separately (currently always redirected to stdout).
    my ($cmd, $Ropts, $Renv) = @_;
    my $Rin = R_tempfile("Rin");
    my $Rout = R_tempfile("Rout");
    my %result;
    my $status;
    my @out;

    R::Vars::error("R_EXE");
    open(RIN, "> $Rin")
	or die "Error: cannot write to '$Rin'\n";
    print RIN "$cmd\n";
    close(RIN);
    $status =
	R_system(join(" ",
		      (&shell_quote_file_path(${R::Vars::R_EXE}),
		       "${Ropts} < ${Rin} > ${Rout} 2>&1")),
		 $Renv);
    @out = &read_lines($Rout);
    unlink($Rin);
    unlink($Rout);
    $result{"status"} = $status;
    @{$result{"out"}} = @out;
    %result;
}

### * file_path

sub file_path {
    my @args = @_;
    my $filesep = "/";
    my $v;

    join($filesep, @args);
}

### * env_path

sub env_path {
    my @args = @_;
    my $envsep = ":";
    $envsep = ";" if($R::Vars::OSTYPE eq "windows");
    join($envsep, @args);
}

### * list_files

sub list_files {
    my $dir = $_[0];
    my @files;
    opendir(DIR, $dir) or die "cannot opendir $dir: $!";
    @files = grep { -f &file_path($dir, $_) } readdir(DIR);
    closedir(DIR);
    my @paths;
    foreach my $file (@files) {
	push(@paths, &file_path($dir, $file));
    }
    @paths;
}

### * list_files_with_exts

sub list_files_with_exts {
    my ($dir, $exts) = @_;
    my @files;
    $exts = ".*" unless $exts;
    opendir(DIR, $dir) or die "cannot opendir $dir: $!";
    @files = grep { /\.$exts$/ && -f "$dir/$_" } readdir(DIR);
    closedir(DIR);
    ## We typically want the paths to the files, see also the R variant
    ## list_files_with_exts() used in some of the QC tools.
    my @paths;
    foreach my $file (@files) {
	push(@paths, &file_path($dir, $file));
    }
    @paths;
}

### * list_files_with_type

sub list_files_with_type {
    my ($dir, $type, $OS) = @_;
    $OS = $R::Vars::OSTYPE unless $OS;
    my $exts = &make_file_exts($type);
    my @files = &list_files_with_exts($dir, $exts);
    if(($type eq "code") || ($type eq "docs")) {
	$dir = &file_path($dir, $OS);
	push(@files, &list_files_with_exts($dir, $exts)) if(-d $dir);
    }
    @files;
}

### * make_file_exts

sub make_file_exts {
    my ($type) = @_;
    my %file_exts =
	("code", "[RrSsq]",
	 "data", "(R|r|RData|rdata|rda|TXT|txt|tab|csv|CSV)",
	 "demo", "[Rr]",
	 "docs", "[Rr]d",
	 "vignette", "[RrSs](nw|tex)");
    my $exts = $file_exts{$type};
    die "Error: unknown type '$type'" unless defined($exts);
    $exts;
}

### * read_lines

sub read_lines {
    my ($file) = @_;
    my @lines;
    open(FILE, "< $file")
	or die "Error: cannot open file '$file' for reading\n";
    chomp(@lines = <FILE>);
    close(FILE);
    @lines;
}

### * shell_quote_file_path

sub shell_quote_file_path {
    ## Quote a file path for passing it to a shell.
    ## Currently only does simple single quoting.
    ## There are much better ways of doing this, such as e.g. using the
    ## CPAN String::ShellQuote module.  The main purpose of the current
    ## version is to isolate the quoting into a separate function rather
    ## than hard-wiring a specific solution.
    return("'" . $_[0] . "'");
}

### * sQuote

sub sQuote {
    ## Single quote text.
    ## Currently does not work for lists.
    return("'" . $_[0] . "'");
}

### * dQuote

sub dQuote {
    ## Double quote text.
    ## Currently does not work for lists.
    return('"' . $_[0] . '"');
}

### * config_val_to_logical

sub config_val_to_logical {
    my ($val) = @_;
    if($val =~ /^(1|yes|true)$/i) {
	return 1;
    }
    elsif($val =~ /^(0|no|false)$/i) {
	return 0;
    }
    carp "Warning: cannot coerce '$val' to logical";
}

### * Non-exported functions

sub get_exclude_patterns {
    ## Return list of file patterns excluded by R CMD build and check.
    ## Kept here so that we ensure that the lists are in sync, but not
    ## exported.
    ## <NOTE>
    ## Has Unix-style '/' path separators hard-coded.
    my @exclude_patterns = ("^.Rbuildignore\$", "(^|/)\\.DS_Store\$",
			    "\~\$", "\\.bak\$", "\\.swp\$",
			    "(^|/)\\.#[^/]*\$", "(^|/)#[^/]*#\$",
			    "^TITLE\$", "^data/00Index\$",
			    "^inst/doc/00Index.dcf\$",
			    "^config\\.(cache|log|status)\$",
			    "^autom4te.cache\$"
			    );
    ## </NOTE>
    @exclude_patterns;
}

sub text2latex {
    s/\\/\\textbackslash{}/g;
    s/([\{\}_\$\^\&\#])/\\$1/g;
    s/>/\\textgreater{}/g;
    s/</\\textless{}/g;
    s/\~/\\textasciitilde{}/g;
    $_;
}

sub text2html {
    s/&/&amp;/g;
    s/>/&gt;/g;
    s/</&lt;/g;
    $_;
}


1;

### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "### [*]+" ***
### End: ***
