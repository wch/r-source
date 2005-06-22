## Subroutines for building R documentation

## Copyright (C) 1997-2005 R Development Core Team
##
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
##
## This program is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
## General Public License for more details.
##
## A copy of the GNU General Public License is available via WWW at
## http://www.gnu.org/copyleft/gpl.html.  You can also obtain it by
## writing to the Free Software Foundation, Inc., 59 Temple Place, Suite
## 330, Boston, MA 02111-1307 USA.

## Send any bug reports to r-bugs@r-project.org.

package R::Rdlists;

require  Exporter;
@ISA     = qw(Exporter);
@EXPORT  = qw(buildinit read_htmlindex read_htmlpkgindex read_anindex
	      build_index fileolder foldorder aliasorder);

use Cwd;
use File::Basename;
use R::Utils;
use R::Vars;
use R::Dcf;

if($main::opt_dosnames) { $HTML = ".htm"; } else { $HTML = ".html"; }

$dir_mod = 0755;#- Permission ('mode') of newly created directories.

### Determine if package (pkg_dir) and lib directories are accessible;
### chdir to package man dir and return package name, full path to lib
### dir and contents of mandir.

sub buildinit {

    my ($pkg_dir, $lib, $dest, $pkg_name) = @ARGV;

    my $currentdir = cwd();

    if($pkg_dir) {
	die("Package directory ${pkg_dir} does not exist\n")
	    unless (-d $pkg_dir);
    }
    else {
	$pkg_dir = file_path($main::R_HOME, "src", "library", "base");
    }

    chdir($currentdir);

    if($lib) {
	if(!(-d $lib)) {
	    mkdir("$lib", $dir_mod) or die "Could not create $lib: $!\n";
	}
	## <NOTE>
	## A version of file_path_as_absolute() would be handy ...
	chdir($lib);
	$lib = cwd();
	chdir($currentdir);
	## </NOTE>
    }
    else{
	$lib = file_path($main::R_HOME, "library");
    }

    chdir($currentdir);

    chdir($pkg_dir) or die("Cannot change to ${pkg_dir}\n");
    $tmp = cwd();
    if($main::OSdir eq "windows") {
	$tmp =~ s+\\+/+g;	# need Unix-style path here
    }
    $pkg_name = basename($tmp) unless($pkg_name);

    my $version;
    if(-r &file_path($lib, $pkg_name, "DESCRIPTION")) {
	$description =
	    new R::Dcf(&file_path($lib, $pkg_name, "DESCRIPTION"));
	if(defined($description->{"Version"})) {
	    $version = $description->{"Version"};
	} else {$version = "";}
    } else {$version = "";}

    chdir "man" or die("There are no man pages in ${pkg_dir}\n");

    ## <FIXME>
    ## Why not simply use
    ##   list_files_with_type(".", "docs", $main::OSdir)
    ## ???
    opendir man, '.';
    @mandir = sort(readdir(man));
    closedir man;
    if(-d $main::OSdir) {
	foreach $file (@mandir) { $Rds{$file} = $file; }
	opendir man, $main::OSdir;
	foreach $file (readdir(man)) {
	    delete $Rds{$file};
	    $RdsOS{$file} = file_path($main::OSdir, $file);
	}
	@mandir = sort(values %Rds);
	push @mandir, sort(values %RdsOS);
    }
    if(-d $main::AQUAdir) {
	foreach $file (@mandir) { $Rds{$file} = $file; }
	opendir man, $main::AQUAdir;
	foreach $file (readdir(man)) {
	    delete $Rds{$file};
	    $RdsOS{$file} = file_path($main::AQUAdir, $file);
	}
	@mandir = sort(values %Rds);
	push @mandir, sort(values %RdsOS);
    }
    ## </FIXME>

    ($pkg_name, $version, $lib, @mandir);
}

### Read all aliases into two hash arrays with basenames and
### (relative) html-paths.


sub read_htmlindex {

    my $lib = $_[0];

    my $pkg, %htmlindex;

    opendir lib, $lib;
    my @libs = readdir(lib);
    closedir lib;

    foreach $pkg (@libs) {
	if(-d file_path($lib, $pkg)){
	    if(! ( ($pkg =~ /^CVS$/) || ($pkg =~ /^\.+$/))){
		if(-r file_path($lib, $pkg, "help", "AnIndex")){
		    open ranindex, "<".file_path($lib, $pkg, "help", "AnIndex");
		    while(<ranindex>){
			/^([^\t]*)\s*\t(.*)/;
			$htmlindex{$1} = file_path($pkg, "html", $2.$HTML);
		    }
		    close ranindex;
		}
	    }
	}
    }
    %htmlindex;
}

sub read_htmlpkgindex {

    my $lib = $_[0];
    my $pkg = $_[1];

    my %htmlindex;

    if(-r file_path($lib, $pkg, "help", "AnIndex")){
	open ranindex, "<".file_path($lib, $pkg, "help", "AnIndex");
	while(<ranindex>){
	    /^([^\t]*)\s*\t(.*)/;
	    $htmlindex{$1} = file_path($pkg, "html", $2.$HTML);
	}
	close ranindex;
    }
    %htmlindex;
}

sub read_anindex {

    my $lib = $_[0];

    my $pkg, %anindex;

    opendir lib, $lib;
    my @libs = readdir(lib);
    closedir lib;

    foreach $pkg (@libs) {
	if(-d file_path($lib, $pkg)){
	    if(! ( ($pkg =~ /^CVS$/) || ($pkg =~ /^\.+$/))){
		if(-r file_path($lib, $pkg, "help", "AnIndex")){
		    open ranindex, "<".file_path($lib, $pkg, "help", "AnIndex");
		    while(<ranindex>){
			/^([^\t]*)\s*\t(.*)/;
			$anindex{$1} = $2;
		    }
		    close ranindex;
		}
	    }
	}
    }
    %anindex;
}

sub striptitle { # text
    my $text = $_[0];
    $text =~ s/\\//go;
    $text =~ s/---/-/go;
    $text =~ s/--/-/go;
    return $text;
}

sub encodealias { # text
    my $alias = $_[0];
    $alias =~ s/&/&amp;/g;
    $alias =~ s/</&lt;/g;
    $alias =~ s/>/&gt;/g;
    return $alias;
}

sub foldorder {uc($a) cmp uc($b) or $a cmp $b;}

## Put -package topic first

sub aliasorder {($b =~ /-package$/) cmp ($a =~ /-package$/) or uc($a) cmp uc($b) or $a cmp $b;}


sub isNonASCII {
    return $_[0] =~ /[^A-Za-z0-9[:punct:][:space:]]/
}

## use preferred MIME encoding, not IANA registered name
sub canonical_encoding {
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
    return $encoding;
}

sub build_index { # lib, dest, version, [chmdir]
    my $lib = $_[0];
    my $dest = $_[1];
    my $version = $_[2];
    my $chmdir = $_[3];

    if(! -d $lib){
        mkdir("$lib", $dir_mod) or die "Could not create directory $lib: $!\n";
    }

    if(! -d "$dest"){
        mkdir("$dest", $dir_mod) or die "Could not create directory $dest: $!\n";
    }

    my $title = "";
    my $pkg_name = "";
    my $pkg_encoding = "unknown";
    ## did not work if builddir ne srcdir
    if(-r &file_path($dest, "DESCRIPTION")) {
	my $rdcf = R::Dcf->new(&file_path($dest, "DESCRIPTION"));
	if($rdcf->{"Package"}) {
	    $pkg_name = $rdcf->{"Package"};
	    chomp $pkg_name;
	}
	if($rdcf->{"Title"}) {
	    $title = $rdcf->{"Title"};
	    chomp $title;
	}
	if($rdcf->{"Encoding"}) {
	    ## we use this even if the pkg title is ASCII
	    $pkg_encoding = $rdcf->{"Encoding"};
	    chomp $pkg_encoding;
	    $pkg_encoding = canonical_encoding($pkg_encoding);
	}
    }

    $pkg_encoding = "iso-8859-1" if lc($pkg_encoding) eq "latin1";
    $pkg_encoding = "iso-8859-2" if lc($pkg_encoding) eq "latin2";

    my $tdir = file_path($dest, "help");
    if(! -d $tdir) {
	mkdir($tdir, $dir_mod) or die "Could not create " . $tdir.": $!\n";
    }
    $tdir = file_path($dest, "html");
    if(! -d $tdir) {
	mkdir($tdir, $dir_mod) or die "Could not create " . $tdir.": $!\n";
    }
    my $anindex = file_path($dest, "help", "AnIndex");

    my %alltitles;
    my $naliases;
    my $nmanfiles;
    my %firstlettersfound;
    my %internal;
    my $tfile;

    foreach $manfile (@mandir) {
	if($manfile =~ /\.Rd$/i){

	    my $rdname = basename($manfile, (".Rd", ".rd"));
	    my $internal = 0;
	    my $encoding = "unknown";

	    if($main::opt_dosnames){
		$manfilebase = "x" . $nmanfiles++;
	    }
	    else{
		$manfilebase = $rdname;
	    }

	    open(rdfile, "< $manfile");
	    undef $text;
	    while(<rdfile>){  # skip comment lines
		if(!/^%/) { $text .= $_; }
	    }
	    close rdfile;
	    $text =~ /\\title\{\s*([^\}]+)\s*\}/s;
	    my $rdtitle = $1;
	    $rdtitle =~ s/\n/ /sg;
	    $rdtitle =~ s/\\R/R/g; # don't use \R in titles
	    $internal = 1 if $text =~ /\\keyword\{\s*internal\s*\}/;
	    if($text =~ /\\encoding\{\s*([^\}]+)\s*\}/s) {
		$encoding = canonical_encoding($1);
		if(isNonASCII($rdtitle)) {
		    if($pkg_encoding eq "unknown") {
			$pkg_encoding = $encoding;
		    } elsif($encoding ne $pkg_encoding) {
			warn "Warning: " .
			    "encoding of Rd title in '$encoding'".
			    " is inconsistent with ".
			    "earlier encoding '$pkg_encoding'\n";
		    }
		}
	    }
	    $main::filenm{$rdname} = $manfilebase;
	    if($main::opt_chm) {
		$main::title2file{$rdtitle} = $manfilebase;
	    }

	    while($text =~ s/\\alias\{\s*(.*)\s*\}//){
		$alias = $1;
		$alias =~ s/\\%/%/g;
		if ($internal){
		    $internal{$alias} = 1;
		}
		my $an = $main::aliasnm{$alias};
		if ($an) {
		    if($an ne $manfilebase) {
			warn "Warning: " .
			    "\\alias\{$alias\} already in $an.Rd -- " .
			    "skipping the one in $manfilebase.Rd\n";
		    }
		} else {
		    $main::alltitles{$alias} = $rdtitle;
		    $main::aliasnm{$alias} = $manfilebase;
		    if(!$internal){
			my $flc = firstLetterCategory($alias);
			$firstlettersfound{$flc}++;
		    }
		    $naliases++;
		}
	    }
	}
    }

    open(anindex, "> ${anindex}") or die "Could not open ${anindex}";
    foreach $alias (sort aliasorder keys %main::aliasnm) {
	print anindex "$alias\t$main::aliasnm{$alias}\n";
    }
    close anindex;


    open(anindex, "< $anindex");
    $tfile = file_path($dest, "html", "00Index".$HTML);
    open(htmlfile, "> $tfile") or die "Could not open $tfile";
    if($main::opt_chm) { # Windows only
	open(chmfile, "> $chmdir/00Index$HTML") or
	    die "Could not open $chmdir/00Index$HTML";
    }
    $pkg_encoding = canonical_encoding($pkg_encoding);

    $pkg_encoding = "iso-8859-1" if $pkg_encoding eq "unknown";

    print htmlfile html_pagehead("$title", "../../../doc/html",
				 "../../../doc/html/index$HTML", "Top",
				 "../../../doc/html/packages$HTML",
				 "Package List", "", "", "../../R.css",
				 $pkg_encoding);

    if($main::opt_chm) {
	print chmfile chm_pagehead("$title");
	print chmfile "<h2>Help pages for package `", $pkg_name, "'"; 
	print chmfile " version ", $version if $version != "";
	print chmfile "</h2>\n\n";
    }

    print htmlfile "<h2>Documentation for package `", $pkg_name, "'"; 
    print htmlfile " version ", $version if $version != "";
    print htmlfile "</h2>\n\n";

    if(-d file_path($dest, "doc")){
	print htmlfile "<h2>User Guides and Package Vignettes</h2>\n"
	    . "Read <a href=\"../doc/index.html\">overview</a> or "
	    . "browse <a href=\"../doc\">directory</a>.\n\n";
    }
    print htmlfile "<h2>Help Pages</h2>\n\n";
	
    if($naliases>100){
	print htmlfile html_alphabet(keys(%firstlettersfound));
	if($main::opt_chm) {
	    print chmfile html_alphabet(keys(%firstlettersfound));
	}
   }

    print htmlfile "\n<table width=\"100%\">\n";
    if($main::opt_chm) {print chmfile "\n<table width=\"100%\">\n";}

    my $firstletter = "";
    my $current = "", $currentfile = "", $file, $generic;
    while(<anindex>){
        chomp;  ($alias, $file) = split /\t/;
	if(!$internal{$alias}){
	    $aliasfirst = firstLetterCategory($alias);
	    if( ($naliases > 100) && ($aliasfirst ne $firstletter) ) {
		print htmlfile "</table>\n";
		print htmlfile html_title2("<a name=\"$aliasfirst\">-- $aliasfirst --</a>");
		print htmlfile "<table width=\"100%\">\n";
		if($main::opt_chm) {
		    print chmfile "</table>\n";
		    print chmfile html_title2("<a name=\"$aliasfirst\">-- $aliasfirst --</a>");
		    print chmfile "<table width=\"100%\">\n";
		}
		$firstletter = $aliasfirst;
	    }
            ## skip method aliases.
	    $generic = $alias;  
	    $generic =~ s/\.data\.frame$/.dataframe/o;
	    $generic =~ s/\.model\.matrix$/.modelmatrix/o;
	    $generic =~ s/\.[^.]+$//o;

	    next if $alias =~ /<-$/o || $generic =~ /<-$/o;
	    if ($generic ne "" && $generic eq $current && 
		$file eq $currentfile && $generic ne "ar") { 

		next; 
	    } else { $current = $alias; $currentfile = $file;}

	    my $title = striptitle($main::alltitles{$alias});
	    print htmlfile "<tr><td width=\"25%\"><a href=\"$file$HTML\">" .
		encodealias($alias) . "</a></td>\n<td>$title</td></tr>\n";
	    if($main::opt_chm) {
		print chmfile "<tr><td width=\"25%\"><a href=\"$file$HTML\">" .
		    encodealias($alias) . "</a></td>\n<td>$title</td></tr>\n";
	    }
	}
    }

    print htmlfile "</table>\n";
    print htmlfile "</body></html>\n";
    if($main::opt_chm) {print chmfile "</table>\n</body></html>\n";}

    close htmlfile;
    if($main::opt_chm) {close chmfile;}
    close anindex;
}




## return ``true'' if file exists and is older than $age
sub fileolder {
    my($file, $age) = @_;
    (! ((-f $file) && ((-M $file) < $age)));
}


## Return the first letter in uppercase, empty string for <=A and
## or "*-package" and "misc" for >=Z 
## used for indexing various HTML lists.
sub firstLetterCategory {
    my ($x) = @_;
    
    if ($x =~ /-package$/) { $x = " "; }
    else {
    	$x = uc substr($x, 0, 1);
    	if($x lt "A") { $x = ""; }
    	if($x gt "Z") { $x = "misc"; }
    }
    $x;
}

## produce alphabet for head of pages
## optional argument gives array of letters to use
sub html_alphabet
{
    my @letters = @_;

    @letters = (A..Z) if $#letters<0;
    my $retval = "<p align=\"center\">\n";
    foreach $letter (sort(@letters)){
	$retval .= "<a href=\"#${letter}\">${letter}</a>\n";
    }
    $retval . "</p>";
}

sub html_pagehead
{
    my ($title, $top, $up, $uptext, $prev, $prevtext, $next, $nextext, 
	$cssloc, $enc) = @_;

    my $retval = "<html><head><title>R: $title</title>\n" .
	"<meta http-equiv=\"Content-Type\" content=\"text/html; charset=$enc\">\n" .
	"<link rel=\"stylesheet\" type=\"text/css\" href=\"$cssloc\">\n" .
	"</head><body>\n" .
	"<h1>$title " .
	"<img class=\"toplogo\" src=\"$top/logo.jpg\" alt=\"[R logo]\"></h1>\n\n" .
        "<hr>\n\n" .
        "<div align=\"center\">\n";

    $retval .= "<a href=\"$prev\"><img src=\"$top/left.jpg\"\n" .
	"alt=\"[$prevtext]\" width=\"30\" height=\"30\" border=\"0\"></a>\n"
	    if $prev;

    $retval .=
	"<a href=\"$up\"><img src=\"$top/up.jpg\"\n" .
        "alt=\"[$uptext]\" width=\"30\" height=\"30\" border=\"0\"></a>\n"
	    if $up;

    $retval .= "<a href=\"$next\"><img src=\"$top/right.jpg\"\n" .
	"alt=\"[$nextext]\" width=\"30\" height=\"30\" border=\"0\"></a>\n"
	if $next;  # always so in current usage

    $retval .= "</div>\n\n";

    $retval;
}

sub chm_pagehead
{
    my ($title) = @_;

    my $retval = "<html><head><title>$title</title>\n" .
	"<link rel=\"stylesheet\" type=\"text/css\" href=\"Rchm.css\">\n" .
	"</head><body>\n" .
	"<h1>$title\n" .
        "<img class=\"toplogo\" src=\"logo.jpg\" alt=\"[R logo]\"></h1>\n\n" .
        "<hr>\n\n";
    $retval .= "<object type=\"application/x-oleobject\" classid=\"clsid:1e2a7bd0-dab9-11d0-b93a-00c04fc99f9e\">\n";
    $retval .= "<param name=\"keyword\" value=\".. contents\">\n" .
	"</object>\n\n";

    $retval;
}

sub html_title2
{
    my $title = $_[0];

    "\n<h2>$title</h2>\n\n";
}


1;
# Local variables: **
# perl-indent-level: 4 **
# cperl-indent-level: 4 **
# End: **
