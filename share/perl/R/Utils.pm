package R::Utils;

#use strict;
use Carp;
use FileHandle;
use Exporter;
use R::Vars;

@ISA = qw(Exporter);
@EXPORT = qw(R_getenv R_version file_path env_path
	     list_files list_files_with_exts
	     R_tempfile R_system R_runR);

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

Copyright (C) 1997-2002 R Core Development Team.
This is free software; see the GNU General Public Licence version 2
or later for copying conditions.  There is NO warranty.
END
    exit 0;
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

sub file_path {
    my @args = @_;
    my $filesep = "/";
    $filesep = ":" if($R::Vars::OSTYPE eq "mac");
    join($filesep, @args);
}

sub env_path {
    my @args = @_;
    my $envsep = ":";
    $envsep = ";" if($R::Vars::OSTYPE eq "windows");
    join($envsep, @args);
}


sub list_files {
    my $dir = $_[0];
    my @files;
    opendir(DIR, $dir) or die "cannot opendir $dir: $!";
    @files = grep { -f &file_path($dir, $_) } readdir(DIR);
    closedir(DIR);
    my @paths;
    foreach my $file (@files) {
	push @paths, &file_path($dir, $file);
    }
    @paths;
}

sub list_files_with_exts {
    my ($dir, $exts) = @_;
    my @files;
    $exts = ".*" unless $exts;
    opendir(DIR, $dir) or die "cannot opendir $dir: $!";
    if($R::Vars::OSTYPE eq "mac"){
	@files = grep { /\.$exts$/ && -f "$dir:$_" } readdir(DIR);
    }
    else{
	@files = grep { /\.$exts$/ && -f "$dir/$_" } readdir(DIR);
    }
    closedir(DIR);
    ## We typically want the paths to the files, see also the R variant
    ## .listFilesWithExts() used in some of the QA tools.
    my @paths;
    foreach my $file (@files) {
	push @paths, &file_path($dir, $file);
    }
    @paths;
}

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

sub R_system
{
    my $cmd = $_[0];
    my $tmpf = R_tempfile();
    if($R::Vars::OSTYPE eq "windows") {
	open(tmpf, "> $tmpf")
	  or die "Error: cannot write to \`$tmpf'\n";
	print tmpf "$cmd\n";
	close tmpf;
	$res = system("sh $tmpf");
	unlink($tmpf);
	return $res;
    } else {
	return system($cmd);
    }
}

sub R_runR
{
    my $cmd = $_[0];
    my $Ropts = $_[1];
    my $Rin = R_tempfile("Rin");
    my $Rout = R_tempfile("Rout");

    R::Vars::error("R_EXE");
    open RIN, "> $Rin" or die "Error: cannot write to \`$Rin'\n";
    print RIN "$cmd\n";
    close RIN;
    R_system("${R::Vars::R_EXE} ${Ropts} < ${Rin} > ${Rout}");
    my @out;
    open ROUT, "< $Rout";
    while(<ROUT>) {chomp; push(@out, $_);}
    close ROUT;
    unlink($Rin);
    unlink($Rout);
    return(@out);
}

1;
