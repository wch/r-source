#-*- mode: perl; perl-indent-level: 4; cperl-indent-level: 4 -*-

# Copyright (C) 1997-2003 R Development Core Team
#
# This document is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# A copy of the GNU General Public License is available via WWW at
# http://www.gnu.org/copyleft/gpl.html.  You can also obtain it by
# writing to the Free Software Foundation, Inc., 59 Temple Place,
# Suite 330, Boston, MA  02111-1307  USA.

use File::Basename;
use Cwd;
use Getopt::Long;
use R::Rdconv;
use R::Rdlists;
use R::Utils;

my $revision = ' $Revision: 1.14 $ ';
my $version;
my $name;

$revision =~ / ([\d\.]*) /;
$version = $1;
($name = $0) =~ s|.*/||;

@knownoptions = ("rhome:s", "html", "txt", "latex", "example", "debug|d",
		 "dosnames", "htmllists", "help|h", "version|v", "os|OS:s", 
		 "index");
GetOptions (@knownoptions) || usage();
&R_version($name, $version) if $opt_version;
&usage() if $opt_help;

$OSdir ="unix";
$OSdir = $opt_os if $opt_os;

$AQUAdir = "aqua" if($ENV{"R_USE_AQUA_SUBDIRS"} eq "yes");

$dir_mod = 0755;#- Permission ('mode') of newly created directories.

if($opt_dosnames){ $HTML=".htm"; } else { $HTML=".html"; }

my $current = cwd();
if($opt_rhome){
    $R_HOME=$opt_rhome;
    print STDERR "R_HOME from --rhome: '$R_HOME'\n" if $opt_debug;
}
elsif($ENV{"R_HOME"}){
    $R_HOME=$ENV{"R_HOME"};
    print STDERR "R_HOME from ENV: '$R_HOME'\n" if $opt_debug;
}
else{
    chdir(dirname($0) . "/..");
    $R_HOME = cwd();
}
chdir($current);
print STDERR "Current directory (cwd): '$current'\n" if $opt_debug;

my $mainlib = file_path($R_HOME, "library");

# if option --htmllists is set we only rebuild some list files and
# exit

if($opt_htmllists){
    build_htmlpkglist($mainlib);

    %anindex = read_anindex($mainlib);
    %htmlindex = read_htmlindex($mainlib);

    exit 0;
}

# default is to build all documentation formats
if(!$opt_html && !$opt_txt && !$opt_latex && !$opt_example){
    $opt_html = 1;
    $opt_txt = 1;
    $opt_latex = 1;
    $opt_example = 1;
}

($pkg, $lib, @mandir) = buildinit();


## !!! Attempting to create the ability to have packages stored in a 
## !!! location other than lib/pkg.  There are obviously better ways
## !!! of doing this, but trying to do it in a manner which will not
## !!! interfere with other things calling this besides INSTALL. JG
$dest = $ARGV[2];
if (!$dest) {
    $dest = file_path($lib, $pkg);
}

print STDERR "Destination dest = '$dest'\n" if $opt_debug;

build_index($lib, $dest, "");
if($opt_index){
    exit 0;
}

if ($opt_latex) {
    $latex_d = file_path($dest, "latex");
    if(! -d $latex_d) {
	mkdir("$latex_d", $dir_mod) or die "Could not create $latex_d: $!\n";
    }
}
if ($opt_example) {
    $Rex_d = file_path($dest, "R-ex");
    if(! -d $Rex_d) {
	mkdir("$Rex_d", $dir_mod) or die "Could not create $Rex_d: $!\n";
    }
}

print " >>> Building/Updating help pages for package '$pkg'\n";
print "     Formats: ";
print "text " if $opt_txt;
print "html " if $opt_html;
print "latex " if $opt_latex;
print "example " if $opt_example;
print "\n";


# get %htmlindex and %anindex

%anindex = read_anindex($lib);
if($opt_html){
    %htmlindex = read_htmlindex($lib);
    if ($lib ne $mainlib) {
	%basehtmlindex = read_htmlindex($mainlib);
	foreach $topic (keys %htmlindex) {
	    $basehtmlindex{$topic} = $htmlindex{$topic};
	}
	%htmlindex = %basehtmlindex;
    }
    # make sure that references are resolved first to this package
    my %thishtmlindex = read_htmlpkgindex($lib, $pkg);
    foreach $topic (keys %thishtmlindex) {
	$htmlindex{$topic} = $thishtmlindex{$topic};
    }
}

format STDOUT =
  @<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< @<<<<<< @<<<<<< @<<<<<< @<<<<<<
  $manfilebase, $textflag, $htmlflag, $latexflag, $exampleflag
.

foreach $manfile (@mandir) {
    if($manfile =~ /\.[Rr]d$/ && !($manfile =~ /^\.#/)) {
	$manfilebase = basename($manfile, (".Rd", ".rd"));
	$manage = (-M $manfile);
	$manfiles{$manfilebase} = $manfile;

	$textflag = $htmlflag = $latexflag = $exampleflag = "";

	if($opt_txt){
	    my $targetfile = $filenm{$manfilebase};
	    $destfile = file_path($dest, "help", $targetfile);
	    if(fileolder($destfile, $manage)) {
		$textflag = "text";
		Rdconv($manfile, "txt", "", "$destfile", $pkg);
	    }
	}

	if($opt_html){
	    my $targetfile = $filenm{$manfilebase};
	    $misslink = "";
	    $destfile = file_path($dest, "html", $targetfile.$HTML);
	    if(fileolder($destfile, $manage)) {
		$htmlflag = "html";
		print "\t$destfile" if $opt_debug;
		Rdconv($manfile, "html", "", "$destfile", $pkg);
	    }
	}

	if($opt_latex){
	    my $targetfile = $filenm{$manfilebase};
	    $destfile = file_path($dest, "latex", $targetfile.".tex");
	    if(fileolder($destfile, $manage)) {
		$latexflag = "latex";
		Rdconv($manfile, "latex", "", "$destfile");
	    }
	}

	if($opt_example){
	    my $targetfile = $filenm{$manfilebase};
	    $destfile = file_path($dest, "R-ex", $targetfile.".R");
	    if(fileolder($destfile, $manage)) {
		if(-f $destfile) {unlink $destfile;}
		Rdconv($manfile, "example", "", "$destfile");
		if(-f $destfile) {$exampleflag = "example";}
	    }
	}

	write if ($textflag || $htmlflag || $latexflag || $exampleflag);
	print "     missing link(s): $misslink\n"
	    if $htmlflag && length($misslink);
    }
}

# remove files not in source directory
if($opt_txt){
    my @destdir;
    opendir dest,  file_path($dest, "help");
    @destdir = sort(readdir(dest));
    closedir dest;
    foreach $destfile (@destdir) {
	if($destfile eq "." || $destfile eq ".." ||
	   $destfile eq "AnIndex") { next; }
	unlink file_path($dest, "help", $destfile)
	    unless defined $manfiles{$destfile};
    }
}
if($opt_html){
    my @destdir;
    opendir dest,  file_path($dest, "html");
    @destdir = sort(readdir(dest));
    closedir dest;
    foreach $destfile (@destdir) {
	$destfilebase = basename($destfile, ".html");
	if($destfile eq "." || $destfile eq ".." ||
	   $destfile eq "00Index.html") { next; }
	unlink file_path($dest, "html", $destfile)
	    unless defined $manfiles{$destfilebase};
    }
}
if($opt_latex){
    my @destdir;
    opendir dest,  file_path($dest, "latex");
    @destdir = sort(readdir(dest));
    closedir dest;
    foreach $destfile (@destdir) {
	$destfilebase = basename($destfile, ".tex");
	if($destfile eq "." || $destfile eq "..") { next; }
	unlink file_path($dest, "latex", $destfile)
	    unless defined $manfiles{$destfilebase};
    }
}
if($opt_example){
    my @destdir;
    opendir dest,  file_path($dest, "R-ex");
    @destdir = sort(readdir(dest));
    closedir dest;
    foreach $destfile (@destdir) {
	$destfilebase = basename($destfile, ".R");
	if($destfile eq "." || $destfile eq "..") { next; }
	unlink file_path($dest, "R-ex", $destfile)
	    unless defined $manfiles{$destfilebase};
    }
}

sub usage {
  print STDERR <<END;
Usage: R CMD $name [options] [pkg] [lib]

Install all help files for package pkg to library lib

Options:
  -h, --help		print short help message and exit
  -v, --version		print version info and exit
  -d, --debug           print debugging information
  --rhome               R home directory, defaults to environment R_HOME
  --html                build HTML files    (default is all)
  --txt                 build text files    (default is all)
  --latex               build LaTeX files   (default is all)
  --example             build example files (default is all)
  --htmllists           build HTML function and package lists
  --dosnames            use 8.3 filenames
  --index               build index file only


Email bug reports to <r-bugs\@r-project.org>.
END
  exit 0;
}
