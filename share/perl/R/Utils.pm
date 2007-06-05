package R::Utils;

use Carp;
use File::Basename;
use File::Path;
use FileHandle;
use IO::File;
use Exporter;
use R::Dcf;
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
	     config_val_to_logical
	     mime_canonical_encoding latex_canonical_encoding);

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
'$name' SVN revision $version

Copyright (C) 1997-2006 R Core Development Team.
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
    while(-e $retval) { # was -f, but want to be able to create such a file
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
    my ($dir, $dirs_and_files) = @_;
    my @files;
    opendir(DIR, $dir) or die "cannot opendir $dir: $!";
    @files = readdir(DIR);
    @files = grep { -f &file_path($dir, $_) } @files
	unless($dirs_and_files);
    closedir(DIR);
    my @paths;
    foreach my $file (@files) {
	push(@paths, &file_path($dir, $file));
    }
    @paths;
}

### * list_files_with_exts

sub list_files_with_exts {
    my ($dir, $exts, $all_files) = @_;
    my @files;
    $exts = ".*" unless $exts;
    opendir(DIR, $dir) or die "cannot opendir $dir: $!";
    @files = grep { /\.$exts$/ && -f "$dir/$_" } readdir(DIR);
    closedir(DIR);
    @files = grep(!/^\./, @files) unless($all_files);
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
	 "vignette", "[RrSs](nw|tex)",
	 "sources", "([cfmCM]|cc|cpp|f90|f95|mm)",
	 "headers", "h",
	 "src_no_CRLF", "([cfh]|cc|cpp)");
    ## (Note that older Sun compilers objected to CRLF line endings: it
    ## seems that newer Fortran 90/95 or ObjC/C++ compilers do not have
    ## non-CRLF restrictions.)
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

### * canonical_encoding

## use preferred MIME encoding, not IANA registered name
sub mime_canonical_encoding {
    my $encoding = lc($_[0]);
    if(/iso_8859-([0-9]+)/) {$encoding = "iso-8859-$1";}
    $encoding = "iso-8859-1"  if $encoding eq "latin1";
    $encoding = "iso-8859-2"  if $encoding eq "latin2";
    $encoding = "iso-8859-3"  if $encoding eq "latin3";
    $encoding = "iso-8859-4"  if $encoding eq "latin4";
    $encoding = "iso-8859-5"  if $encoding eq "cyrillic";
    $encoding = "iso-8859-6"  if $encoding eq "arabic";
    $encoding = "iso-8859-7"  if $encoding eq "greek";
    $encoding = "iso-8859-8"  if $encoding eq "hebrew";
    $encoding = "iso-8859-9"  if $encoding eq "latin5";
    $encoding = "iso-8859-10" if $encoding eq "latin6";
    $encoding = "iso-8859-14" if $encoding eq "latin8";
    $encoding = "iso-8859-15" if $encoding eq "latin-9";
    $encoding = "iso-8859-16" if $encoding eq "latin10";
    $encoding = "utf-8"       if $encoding eq "utf8";
    return $encoding;
}

sub latex_canonical_encoding {
    my $encoding = lc($_[0]);
    if(/iso_8859-([0-9]+)/) {$encoding = "iso-8859-$1";}
    $encoding = "latin1"  if $encoding eq "iso-8859-1";
    $encoding = "latin2"  if $encoding eq "iso-8859-2";
    $encoding = "latin3"  if $encoding eq "iso-8859-3";
    $encoding = "latin4"  if $encoding eq "iso-8859-4";
    $encoding = "latin5"  if $encoding eq "iso-8859-9";
    $encoding = "latin6"  if $encoding eq "iso-8859-10";
    $encoding = "latin8"  if $encoding eq "iso-8859-14";
    $encoding = "latin9"  if $encoding eq "latin-9";
    $encoding = "latin9"  if $encoding eq "iso-8859-15";
    $encoding = "latin10" if $encoding eq "iso-8859-16";
    $encoding = "utf8"    if $encoding eq "utf-8";
    return $encoding;
}



### * Non-exported functions

sub get_exclude_patterns {
    ## Return list of file patterns excluded by R CMD build and check.
    ## Kept here so that we ensure that the lists are in sync, but not
    ## exported.
    ## <NOTE>
    ## Has Unix-style '/' path separators hard-coded.
    my @exclude_patterns = ("^\\.Rbuildignore\$",
			    "(^|/)\\.DS_Store\$",
			    "\~\$", "\\.bak\$", "\\.swp\$",
			    "(^|/)\\.#[^/]*\$", "(^|/)#[^/]*#\$",
			    ## Outdated ...
			    "^TITLE\$", "^data/00Index\$",
			    "^inst/doc/00Index\\.dcf\$",
			    ## Autoconf
			    "^config\\.(cache|log|status)\$",
			    "^autom4te\\.cache\$",
			    ## Windows dependency files
			    "^src/.*\\.d\$", "^src/Makedeps\$",
			    ## IRIX
			    "^src/so_locations\$"
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

## This is currently shared between build and check.
sub check_package_description {
    
    my ($pkgdir, $pkgname, $log, $in_bundle, $is_base_pkg, $full) = @_;
    
    my ($dfile, $dir, $description);

    if($is_base_pkg) {
	$dfile = "DESCRIPTION.in";
    }
    elsif(!$in_bundle) {
	$dfile = "DESCRIPTION";
    }
    else {
	## Bundles are a bit tricky, as their package (DESCRIPTION)
	## metadata come from merging the bundle DESCRIPTION file
	## with the package DESCRIPTION.in one.  Hence, we
	## concatenate these files to a temporary one.
	$log->checking("for file 'DESCRIPTION.in'");
	if(-r "DESCRIPTION.in") {
	    $log->result("OK");
	}
	else {
	    $log->result("NO");
	    exit(1);
	}
	## Checking metadata currently also includes verifying that
	## the package name and "directory name" are the same.
	$dir = &file_path(${R::Vars::TMPDIR}, "check$$");
	mkdir($dir, 0755)
	    or die ("Error: cannot create directory '$dir'\n");
	$dir = &file_path($dir, $pkgname);
	mkdir($dir, 0755)
	    or die ("Error: cannot create directory '$dir'\n");
	$dfile = &file_path($dir, "DESCRIPTION");
	my $fh = new IO::File($dfile, "w")
	    or die "Error: cannot open file '$dpath' for writing\n";
	my @lines = (&read_lines(&file_path(dirname($pkgdir),
					    "DESCRIPTION")),
		     &read_lines("DESCRIPTION.in"));
	@lines = grep(!/^\s*$/, @lines); # Remove blank lines.
	$fh->print(join("\n", @lines), "\n");
	$fh->close();
    }

    $log->checking("DESCRIPTION meta-information");

    my $description = new R::Dcf($dfile);

    if($full) {
	my $Rcmd = "tools:::.check_package_description(\"$dfile\")";
	my @out = R_runR($Rcmd, "--vanilla --quiet",
			 "R_DEFAULT_PACKAGES=NULL");
	@out = grep(!/^\>/, @out);
	if(scalar(@out) > 0) {
	    rmtree(dirname($dir)) if($in_bundle);
	    $log->error();
	    $log->print(join("\n", @out) . "\n");
	    exit(1);
	}
    }

    ## check the encoding
    {
	my $Rcmd = "tools:::.check_package_description_encoding(\"$dfile\")";
	my @out = R_runR($Rcmd, "--vanilla --quiet",
			 "R_DEFAULT_PACKAGES=NULL");
	@out = grep(!/^\>/, @out);
	if(scalar(@out) > 0) {
	    rmtree(dirname($dir)) if($in_bundle);
	    $log->warning();
	    $log->print(join("\n", @out) . "\n");
	}
	else {
	    $log->result("OK");
	}
    }

    rmtree(dirname($dir)) if($in_bundle);    

#     ## This is no longer needed
#
#     ## Also check whether the package name has two dots, which is not
#     ## portable as it is not guaranteed to work in Windows.  (Do this
#     ## here as R currently turns non-empty package meta data check
#     ## results into installation errors.)
#     if(grep(/\..*\./, $description->{"Package"})) {
# 	$log->warning();
# 	$log->print(wrap("", "",
# 			 ("Package name contains more than one dot.\n",
# 			  "Names should contain at most one dot to",
# 			  "be guaranteed to portably work on all",
# 			  "supported platforms.\n")));
#     }
#     else {
# 	$log->result("OK");
#     }
}


1;

### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "### [*]+" ***
### End: ***
