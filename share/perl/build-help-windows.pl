#-*- mode: perl; perl-indent-level: 4; cperl-indent-level: 4 -*-

# Copyright (C) 1997-2007 R Development Core Team
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
# writing to the Free Software Foundation, Inc., 51 Franklin Street,
# Fifth Floor, Boston, MA 02110-1301  USA.

use File::Basename;
use Cwd;
use Getopt::Long;
use R::Rdconv;
use R::Rdlists;
use R::Utils;
use R::Dcf;

fileparse_set_fstype; # Unix, in case one gets anything else.

@knownoptions = ("rhome:s", "html", "txt", "latex", "example", "debug|d",
		 "dosnames", "chm", "index");
GetOptions (@knownoptions) || usage();

$OSdir = "windows";

$dir_mod = 0755;#- Permission ('mode') of newly created directories.

if($opt_dosnames){ $HTML=".htm"; } else { $HTML=".html"; }

my $current = cwd();
if($opt_rhome){
    $R_HOME=$opt_rhome;
    print STDERR "R_HOME from --rhome: '$R_HOME'\n" if $opt_debug;
} else{
    chdir(dirname($0) . "/../..");
    $R_HOME=cwd();
    print STDERR "R_HOME: '$R_HOME'\n" if $opt_debug;
}
$R_HOME =~ s+\\+/+g; # Unix-style path

chdir($current);
print STDERR "Current directory (cwd): '$current'\n" if $opt_debug;

my $mainlib = file_path($R_HOME, "library");


# default is to build all documentation formats
if(!$opt_html && !$opt_txt && !$opt_latex && !$opt_example && !$opt_chm){
    $opt_html = 1;
    $opt_txt = 1;
    $opt_latex = 1;
    $opt_example = 1;
    $opt_chm = 1 unless $opt_index;
}

($pkg, $version, $lib, @mandir) = buildinit();
$dest = $ARGV[2];
if (!$dest) {$dest = file_path($lib, $pkg);}

print STDERR "Destination dest = '$dest'\n" if $opt_debug;

my $def_encoding = "unknown";
if(-r &file_path($dest, "DESCRIPTION")) {
    my $rdcf = R::Dcf->new(&file_path($dest, "DESCRIPTION"));
    if($rdcf->{"Encoding"}) {
	    $def_encoding = $rdcf->{"Encoding"};
	    chomp $def_encoding;
	    # print "Using $def_encoding as the default encoding\n";
	}
}

if($opt_chm) {
    $chmdir = "../chm";
    if(! -d $chmdir) {
	mkdir($chmdir, $dir_mod) or die "Could not create $chmdir: $!\n";
    }
    open_hhp($pkg);
}

build_index($lib, $dest, $version, $chmdir);
if($opt_index){
    exit 0;
}

if($opt_chm) {
    build_chm_toc();
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
print "chm " if $opt_chm;
print "\n";


# get %htmlindex and %anindex
# as from 1.7.0 we can resolve links to base from other libraries
# by fixing the link in fixup.package.URLs().
# as from 1.9.0 we fix up utils, graphics, stats as well.
# by 2.3.0 grDevices, datasets and methods.

%anindex = read_anindex($lib);
if($opt_html || $opt_chm){
    %htmlindex = read_htmlindex($lib);
    if ($lib ne $mainlib) {
	%basehtmlindex = read_htmlpkgindex($mainlib, "base");
	foreach $pkg ("utils", "graphics", "grDevices", "stats", 
		      "datasets", "methods") {
	    my %pkghtmlindex = read_htmlpkgindex($mainlib, $pkg);
	    foreach $topic (keys %pkghtmlindex) {
		$basehtmlindex{$topic} = $pkghtmlindex{$topic};
	    }
	}
	# current lib will win
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
  @<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< @<<<<<< @<<<<<< @<<<<<< @<<<<<< @<<<<<<
  $manfilebase, $textflag, $htmlflag, $latexflag, $exampleflag, $chmflag
.

foreach $manfile (@mandir) {
    ## Should only process files starting with [A-Za-z0-9] and with
    ## suffix .Rd or .rd, according to `Writing R Extensions'.
    if($manfile =~ /\.[Rr]d$/) {
	$manfilebase = basename($manfile, (".Rd", ".rd"));
	if(! ($manfilebase =~ /^[A-Za-z0-9]/) ) {next;}
	$manage = (-M $manfile);
	$manfiles{$manfilebase} = $manfile;

	$textflag = $htmlflag = $latexflag = $exampleflag = $chmflag = "";
	$types = "";
	undef $do_example;

	if($opt_txt){
	    my $targetfile = $filenm{$manfilebase};
	    $destfile = file_path($dest, "help", $targetfile);
	    if(fileolder($destfile, $manage)) {
		$textflag = "text";
		$types .= "txt,";
	    }
	}

	if($opt_html){
	    my $targetfile = $filenm{$manfilebase};
	    $misslink = "";
	    $destfile = file_path($dest, "html", $targetfile.$HTML);
	    if(fileolder($destfile, $manage)) {
		$htmlflag = "html";
		print "\t$destfile" if $opt_debug;
		$types .= "html,";
	    }
	}

	if($opt_latex){
	    my $targetfile = $filenm{$manfilebase};
	    $destfile = file_path($dest, "latex", $targetfile.".tex");
	    if(fileolder($destfile, $manage)) {
		$latexflag = "latex";
		$types .= "latex,";
	    }
	}

	if($opt_example){
	    my $targetfile = $filenm{$manfilebase};
	    $destfile = file_path($dest, "R-ex", $targetfile.".R");
	    if(fileolder($destfile, $manage)) {
		if(-f $destfile) {unlink $destfile;}
		$types .= "example,";
		$do_example = "yes";
	    }
	}

	Rdconv($manfile, $types, "", "$dest", $pkg, $version, 
	       $def_encoding) if $types ne "";
	if($do_example && -f $destfile) {$exampleflag = "example";}

	if($opt_chm){
	    my $targetfile = $filenm{$manfilebase};
	    $misslink = "";
	    $destfile = "../chm/$targetfile$HTML";
	    print hhpfile "$targetfile$HTML\n";
	    if(fileolder($destfile,$manage)) {
		$chmflag = "chm";
		print "\t$destfile" if $opt_debug;
		Rdconv($manfile, "chm", "", "$destfile", $pkg, $version);
	    }
	}

	write if ($textflag || $htmlflag || $latexflag || 
		  $exampleflag || $chmflag);
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
if($opt_chm){
    my @destdir;
    opendir dest,  "../chm";
    @destdir = sort(readdir(dest));
    closedir dest;
    foreach $destfile (@destdir) {
	$destfilebase = basename($destfile, (".html"));
	next unless $destfile =~ /\.html$/;
	next if $destfile eq "00Index.html";
	unlink "../chm/$destfile" unless defined $manfiles{$destfilebase};
    }
}

sub usage {
    print "Usage:  build-help-windows.pl [--rhome dir] [--html] [--txt]\n" .
      "                   [--latex] [--example] [--chm]\n" .
      "                   [--dosnames] [--debug] [--index]\n" .
      "                   [pkg] [lib]\n";

    exit 0;
}

sub open_hhp {
    my $pkg = $_[0];

    open(hhpfile, ">../chm/$pkg.hhp")
	or die "Couldn't open the chm project file\n";
    print hhpfile "[OPTIONS]\n", "Auto Index=Yes\n",
    "Contents file=$pkg.toc\n",
    "Compatibility=1.1 or later\n",
    "Compiled file=$pkg.chm\n",
    "Default topic=00Index.html\n",
    "Display compile progress=No\n",
    "Full-text search=Yes\n",
    "Full text search stop list file=..\\..\\..\\gnuwin32\\help\\R.stp\n",
    "Title=R Help for package $pkg\n",
    "\n\n[FILES]\n00Index.html\n";
}

sub foldorder {($b =~ /-package$/) cmp ($a =~ /-package$/) or uc($a) cmp uc($b) or $a cmp $b;}

sub build_chm_toc {
    open(tocfile, ">../chm/$pkg.toc")
	or die "Couldn't open the chm toc file";
    print tocfile
	"<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML//EN\">\n",
	"<HEAD></HEAD><HTML><BODY>\n<UL>\n";
    print tocfile
	"<LI> <OBJECT type=\"text/sitemap\">\n",
	"<param name=\"Name\" value=\"Package $pkg:  Contents\">\n",
	"<param name=\"Local\" value=\"00Index.html\">\n",
	"</OBJECT>\n";
    print tocfile
	"<LI> <OBJECT type=\"text/sitemap\">\n",
	"<param name=\"Name\" value=\"Package $pkg:  R objects\">\n",
	"</OBJECT>\n";
    print tocfile "<UL>\n";   # contents of a book
    foreach $alias (sort foldorder keys %aliasnm) {
	print tocfile
	    "<LI> <OBJECT type=\"text/sitemap\">\n",
	    "<param name=\"Name\" value=\"$alias\">\n",
	    "<param name=\"Local\" value=\"$aliasnm{$alias}.html\">\n",
	    "</OBJECT>\n";
    }
    print tocfile "</UL>\n";  # end of a book
    print tocfile
	"<LI> <OBJECT type=\"text/sitemap\">\n",
	"<param name=\"Name\" value=\"Package $pkg:  Titles\">\n",
	"</OBJECT>\n";
    print tocfile "<UL>\n";   # contents of a book
    foreach $title (sort foldorder keys %title2file) {
	print tocfile
	    "<LI> <OBJECT type=\"text/sitemap\">\n",
	    "<param name=\"Name\" value=\"$title\">\n",
	    "<param name=\"Local\" value=\"$title2file{$title}.html\">\n",
	    "</OBJECT>\n";
    }
    print tocfile "</UL>\n";  # end of a book
    print tocfile "</UL>\n</BODY></HTML>\n";
    close tocfile;
}
