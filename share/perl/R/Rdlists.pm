# Subroutines for building R documentation

# Copyright (C) 1997-2000 R Development Core Team
#
# This program is free software; you can redistribute it and/or modify
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

# Send any bug reports to r-bugs@r-project.org

package R::Rdlists;

require  Exporter;
@ISA     = qw(Exporter);
@EXPORT  = qw(buildinit read_titles read_functiontitles read_htmlindex read_anindex build_htmlpkglist build_index build_htmlfctlist fileolder foldorder);

use Cwd;
use File::Basename;

if($main::opt_dosnames){
    $HTML="htm";
}
else{
    $HTML="html";
}

$dir_mod = 0755;#- Permission ('mode') of newly created directories.

# determine if pkg and lib directory are accessible; chdir to pkg man dir
# and return pkg name, full path to lib dir and contents of mandir

sub buildinit {

    my $pkg = $ARGV[0];
    my $lib = $ARGV[1];

    my $currentdir = getcwd();

    if($pkg){
	die("Package $pkg does not exist\n") unless (-d $pkg);
    }
    else{
	$pkg="$main::R_HOME/src/library/base";
    }

    chdir $currentdir;

    if($lib){
        mkdir "$lib", $dir_mod || die "Could not create $lib: $!\n";
	chdir $lib;
	$lib=getcwd();
	chdir $currentdir;
    }
    else{
	$lib="$main::R_HOME/library";
    }

    chdir $currentdir;

    chdir($pkg) or die("Cannot change to $pkg\n");
    $tmp = getcwd();
    if($main::OSdir eq "windows") {
	$tmp =~ s+\\+/+g; # need Unix-style path here
    }
    $pkg = basename($tmp);
#    $pkg = basename(getcwd());

    chdir "man" or die("There are no man pages in $pkg\n");
    opendir man, '.';
    @mandir = sort(readdir(man));
    closedir man;

    if(-d $main::OSdir) {
	foreach $file (@mandir) { $Rds{$file} = $file; }
	opendir man, $main::OSdir;
	foreach $file (readdir(man)) {
	    delete $Rds{$file};
	    $RdsOS{$file} = $main::OSdir."/".$file;
	}
	@mandir = sort(values %Rds);
	push @mandir, sort(values %RdsOS);
    }

    ($pkg, $lib, @mandir);
}


### Read the titles of all installed packages into an hash array

sub read_titles {

    my $lib = $_[0];

    my %tit;
    my $pkg;

    opendir lib, $lib;
    my @libs = readdir(lib);
    closedir lib;

    foreach $pkg (@libs) {
	if(-d "$lib/$pkg"){
	    if(! ( ($pkg =~ /^CVS$/) || ($pkg =~ /^\.+$/))){
		if(-r "$lib/$pkg/TITLE"){
		    open rtitle, "< $lib/$pkg/TITLE";
		    $_ = <rtitle>;
		    /^(\S*)\s*(.*)/;
		    my $pkgname = $1;
		    $tit{$pkgname} = $2;
		    while(<rtitle>){
			/\s*(.*)/;
			$tit{$pkgname} = $tit{$pkgname} . "\n" .$1;
		    }
		    close rtitle;
		}
	    }
	}
    }

    close titles;
    %tit;
}

### Read the titles of all installed functions into an hash array

sub read_functiontitles {

    my $lib = $_[0];

    my %tit;
    my $pkg;

    opendir lib, $lib;
    my @libs = readdir(lib);
    closedir lib;

    foreach $pkg (@libs) {
	if(-d "$lib/$pkg"){
	    if(! ( ($pkg =~ /^CVS$/) || ($pkg =~ /^\.+$/))){
		if(-r "$lib/$pkg/TITLE"){
		    open rtitle, "< $lib/$pkg/help/00Titles";
		    while(<rtitle>){
			/^([^\t]*)\s*(.*)/;
			my $alias = $1;
			$tit{$alias} = $2 . " ($pkg)";
		    }
		    close rtitle;
		}
	    }
	}
    }

    close titles;
    %tit;
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
	if(-d "$lib/$pkg"){
	    if(! ( ($pkg =~ /^CVS$/) || ($pkg =~ /^\.+$/))){
		if(-r "$lib/$pkg/help/AnIndex"){
		    open ranindex, "< $lib/$pkg/help/AnIndex";
		    while(<ranindex>){
			/^([^\t]*)\s*\t(.*)/;
			$htmlindex{$1} = "$pkg/html/$2.$HTML";
		    }
		    close ranindex;
		}
	    }
	}
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
	if(-d "$lib/$pkg"){
	    if(! ( ($pkg =~ /^CVS$/) || ($pkg =~ /^\.+$/))){
		if(-r "$lib/$pkg/help/AnIndex"){
		    open ranindex, "< $lib/$pkg/help/AnIndex";
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



### Build $R_HOME/doc/html/packages.html from the $pkg/TITLE files

sub build_htmlpkglist {

    my $lib = $_[0];

    my %htmltitles = read_titles($lib);
    my $key;

    open(htmlfile, "> $main::R_HOME/doc/html/packages.$HTML") ||
	die "Could not open $main::R_HOME/doc/html/packages.$HTML";

    print htmlfile html_pagehead("Package Index", ".",
				 "index.$HTML", "Top",
				 "", "",
				 "function.$HTML", "Functions");

    print htmlfile "<table align=\"center\" summary=\"R Package list\">\n";

    foreach $key (sort(keys %htmltitles)) {
	print htmlfile "<tr align=\"left\" valign=\"top\">\n";
	print htmlfile "<td><a href=\"../../library/$key/html/00Index.$HTML\">";
	print htmlfile encodealias($key), "</a></td><td>";
	print htmlfile $htmltitles{$key}, "</td></tr>\n";
    }

    print htmlfile "</table>\n";
    print htmlfile "</body></html>\n";

    close htmlfile;
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

sub build_index { # lib, dest
    my $lib = $_[0];
    my $dest = $_[1];
    my $chmdir = $_[2];

    if(! -d $lib){
        mkdir "$lib", $dir_mod || die "Could not create directory $lib: $!\n";
    }

    if(! -d "$dest"){
        mkdir "$dest", $dir_mod || die "Could not create directory $dest: $!\n";
    }

    open title, "<../TITLE";
    my $title = <title>;
    close title;
    chomp $title;
    $title =~ s/^\S*\s*(.*)/$1/;

    mkdir "$dest/help", $dir_mod || die "Could not create $dest/help: $!\n";
    mkdir "$dest/html", $dir_mod || die "Could not create $dest/html: $!\n";
    my $anindex = "$dest/help/AnIndex";

    my %alltitles;
    my $naliases;
    my $nmanfiles;
    my %firstlettersfound;
                           
    foreach $manfile (@mandir) {
	if($manfile =~ /\.Rd$/i){

	    my $rdname = basename($manfile, (".Rd", ".rd"));

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

	    $main::filenm{$rdname} = $manfilebase;
	    if($main::opt_chm) {
		$main::title2file{$rdtitle} = $manfilebase;
	    }

	    while($text =~ s/\\(alias|name)\{\s*(.*)\s*\}//){
		$alias = $2;
		$alias =~ s/\\%/%/g;
		my $an = $main::aliasnm{$alias};
		if ($an) {
		    if($an ne $manfilebase) {
			warn "\\$1\{$alias\} already in $an.Rd -- " .
			    "skipping the one in $manfilebase.Rd\n";
		    }
		} else {
		    $main::alltitles{$alias} = $rdtitle;
		    $main::aliasnm{$alias} = $manfilebase;
		    my $flc = firstLetterCategory($alias);
		    $firstlettersfound{$flc}++;
		    $naliases++;
		}
	    }
	}
    }

    open anindex, "> ${anindex}" || die "Could not open ${anindex}";
    foreach $alias (sort foldorder keys %main::aliasnm) {
	print anindex "$alias\t$main::aliasnm{$alias}\n";
    }
    close anindex;


    open(anindex, "< $anindex");
    open(titleindex, "> $dest/help/00Titles") 
	|| die "Could not open $dest/help/00Titles";
    open(htmlfile, "> $dest/html/00Index.$HTML")
	|| die "Could not open $dest/help/00Index.$HTML";
    if($main::opt_chm) {
	open(chmfile, "> $chmdir/00Index.$HTML") ||
	    die "Could not open $chmdir/00Index.$HTML";
    }

    print htmlfile html_pagehead("$title", "../../../doc/html",
				 "../../../doc/html/index.$HTML", "Top",
				 "../../../doc/html/packages.$HTML",
				 "Package List");

    if($main::opt_chm) {
	print chmfile chm_pagehead("$title");
    }


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
# skip method aliases.
	$generic = $alias;  
	$generic =~ s/\.data\.frame$/.dataframe/o;
	$generic =~ s/\.model\.matrix$/.modelmatrix/o;
	$generic =~ s/\.[^.]+$//o;
#	print "   $alias, $generic, $file, $currentfile\n";
	next if $alias =~ /<-$/o || $generic =~ /<-$/o;
	if ($generic ne "" && $generic eq $current && 
	    $file eq $currentfile && $generic ne "ar") { 
#	    print "skipping $alias\n";
	    next; 
	} else { $current = $alias; $currentfile = $file;}

	print titleindex "$alias\t$main::alltitles{$alias}\n";
	my $title = striptitle($main::alltitles{$alias});
	print htmlfile "<tr><td width=\"25%\"><a href=\"$file.$HTML\">" .
	    encodealias($alias) . "</a></td>\n<td>$title</td></tr>\n";
	if($main::opt_chm) {
	    print chmfile "<tr><td width=\"25%\"><a href=\"$file.$HTML\">" .
		encodealias($alias) . "</a></td>\n<td>$title</td></tr>\n";
	}
    }

    print htmlfile "</table>\n";
    print htmlfile "</body></html>\n";
    if($main::opt_chm) {print chmfile "</table>\n</body></html>\n";}

    close titleindex;
    close htmlfile;
    if($main::opt_chm) {close chmfile;}
    close anindex;

#    build_htmlpkglist($lib);
}


sub build_htmlfctlist {

    my $lib = $_[0];

    my %htmltitles = read_functiontitles($lib);
    my $key;

    open(htmlfile, "> $main::R_HOME/doc/html/function.$HTML") ||
	die "Could not open $main::R_HOME/doc/html/function.$HTML";

    print htmlfile html_pagehead("Functions installed in R_HOME", ".",
				 "index.$HTML", "Top",
				 "packages.$HTML", "Packages");

    print htmlfile html_alphabet();

    print htmlfile html_title2("-- Operators, Global Variables, ... --");
    print htmlfile "\n<table width=\"100%\">\n";
    foreach $alias (sort foldorder keys %htmltitles) {
	print htmlfile "<tr><td width=\"25%\">" .
	    "<a href=\"../../library/$htmlindex{$alias}\">" .
		encodealias($alias) . 
		"</a></td>\n<td>$htmltitles{$alias}</td></tr>\n"
		unless $alias =~ /^[a-zA-Z]/;
    }
    print htmlfile "\n</table>\n<table width=\"100%\">\n";

    my $firstletter = "";
    my $current = "", $currentfile = "", $file, $generic;
    foreach $alias (sort foldorder keys %htmltitles) {
	$aliasfirst = uc substr($alias, 0, 1);
	if($aliasfirst =~ /[A-Z]/){
	    if($aliasfirst ne $firstletter){
		print htmlfile "</table>\n";
		print htmlfile "<a name=\"" . uc $aliasfirst . "\"></a>";
		print htmlfile html_title2("-- " . uc $aliasfirst . " --");
		print htmlfile "<table width=\"100%\">\n";
		$firstletter = $aliasfirst;
	    }
# skip method aliases.
	    $file = $htmlindex{$alias};
	    $generic = $alias;  
	    $generic =~ s/\.data\.frame$/.dataframe/o;
	    $generic =~ s/\.model\.matrix$/.modelmatrix/o;
	    $generic =~ s/\.[^.]+$//o;
# omit all replacement functions and all plot and print methods
	    next if $alias =~ /<-$/o || $generic =~ /<-$/o;
	    next if $alias =~ /plot\./o;
	    next if $alias =~ /print\./o;
	    if ($generic ne "" && $generic eq $current && 
		$file eq $currentfile && $generic ne "ar") { 
		next;
	    } else { $current  = $alias; $currentfile = $file;}
	    my $title = striptitle($htmltitles{$alias});
	    print htmlfile "<tr><td width=\"25%\">" .
		"<a href=\"../../library/$file\">" .
		    encodealias($alias) .
			"</a></td>\n<td>$title</td></tr>\n";
	}
    }

    print htmlfile "</table>\n";
    print htmlfile "</body>\n";

    close htmlfile;
}


## return ``true'' if file exists and is older than $age
sub fileolder {
    my($file, $age) = @_;
    (! ((-f $file) && ((-M $file) < $age)));
}


## Return the first letter in uppercase, empty string for <=A and
## "misc" for >=Z 
## used for indexing various HTML lists.
sub firstLetterCategory {
    my ($x) = @_;
    
    $x = uc substr($x, 0, 1);
    if($x lt "A") { $x = ""; }
    if($x gt "Z") { $x = "misc"; }
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
    my ($title, $top, $up, $uptext, $prev, $prevtext, $next, $nextext) = @_;

    my $retval = "<html><head><title>R: $title</title>\n" .
	"<link rel=\"stylesheet\" type=\"text/css\" href=\"$top/R.css\">\n" .
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
	if $next;

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
