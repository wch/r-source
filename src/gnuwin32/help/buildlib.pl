# Subroutines for building R documentation

# Copyright (C) 1997 Friedrich Leisch
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

# Send any bug reports to Friedrich.Leisch@ci.tuwien.ac.at


use Cwd;
use File::Basename;

require "$R_HOME/src/gnuwin32/help/html-layout.pl";


if($opt_dosnames){
    $HTML="htm";
}
else{
    $HTML="html";
}

$dir_mod = 0755;#- Permission ('mode') of newly created directories.

# determine of pkg and lib directory are accessible; chdir to pkg man dir
# and return pkg name, full path to lib dir and contents of mandir

sub buildinit {

    my $pkg = $ARGV[0];
    my $lib = $ARGV[1];

    my $currentdir = getcwd();

    print STDERR "pkg:$pkg, lib:$lib\n" if $opt_debug;
    if($pkg){
	$pkg0 = $pkg;
	$pkg = "$R_HOME/src/library/" . "$pkg" unless (-d $pkg);
	die("Package $pkg0 does not exist\n") unless (-d $pkg);
    }
    else{
	$pkg="$R_HOME/src/library/base";
    }

    chdir $currentdir;

    if($lib){
        mkdir "$lib", $dir_mod || die "Could not create $lib: $!\n";
	chdir $lib;
	$lib=getcwd();
	chdir $currentdir;
    }
    else{
	$lib="$R_HOME/library";
    }

    chdir $currentdir;

    chdir($pkg) or die("Cannot change to $pkg\n");
    $tmp = getcwd();
    $tmp =~ s+\\+/+g; # need Unix-style path here
    $pkg = basename($tmp);
#    $pkg = basename(getcwd());

    chdir "man" or die("There are no man pages in $pkg\n");
    opendir man, '.';
    @mandir = sort(readdir(man));
    closedir man;

    if(-d $OSdir) {
	foreach $file (@mandir) { $Rds{$file} = $file; }
	opendir man, $OSdir;
	foreach $file (readdir(man)) {
	    delete $Rds{$file};
	    $RdsOS{$file} = $OSdir."/".$file; 
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

    open(htmlfile, ">$R_HOME/doc/html/packages.$HTML");

    print htmlfile html_pagehead("Package Index", ".",
				 "index.$HTML", "Top",
				 "", "",
				 "function.$HTML", "Functions");

    print htmlfile "<P><TABLE align=center>\n";

    foreach $key (sort(keys %htmltitles)) {
	print htmlfile "<TR ALIGN=LEFT VALIGN=TOP>\n";
	print htmlfile "<TD><A HREF=\"../../library/$key/html/00Index.$HTML\">";
	print htmlfile "$key</A><TD>";
	print htmlfile $htmltitles{$key};
    }

    print htmlfile "</TABLE>\n";
    print htmlfile "</BODY></HTML>\n";

    close htmlfile;
}



sub build_index {

    if(! -d $lib){
        mkdir "$lib", $dir_mod || die "Could not create directory $lib: $!\n";
    }

    if(! -d "$dest"){
        mkdir "$dest", $dir_mod || die "Could not create directory $dest: $!\n";
    }

    open title, "<../TITLE";
#    open out, ">$dest/TITLE";
    $title = <title>;
#    print out "$title";
    close title;
#    close out;
    $title =~ s/^\S*\s*(.*)/$1/;

    mkdir "$dest/help", $dir_mod || die "Could not create $dest/help: $!\n";
    mkdir "$dest/html", $dir_mod || die "Could not create $dest/html: $!\n";

    $anindex = "$lib/$pkg/help/AnIndex";

    my %alltitles;
    my $naliases;
    my $nmanfiles;

    foreach $manfile (@mandir) {
	if($manfile =~ /\.Rd$/i){

	    my $rdname = basename($manfile, (".Rd", ".rd"));
	    
	    if($opt_dosnames){
		    $manfilebase = "x" . (1000 + $nmanfiles++);
	    }else{
		$manfilebase = $rdname;
	    }

	    open(rdfile, "<$manfile");
	    undef $text;
	    while(<rdfile>){  # skip comment lines
		if(!/^%/) { $text .= $_; }
	    }
	    close rdfile;
	    $text =~ /\\title\{\s*([^\}]+)\s*\}/s;
	    my $rdtitle = $1;
	    $rdtitle =~ s/\n/ /sg;
	    $rdtitle =~ s/\\R/R/g; # don't use \R in titles

	    $filenm{$rdname} = $manfilebase;
	    $title2file{$rdtitle} = $manfilebase;

	    while($text =~ s/\\(alias|name)\{\s*([^\}]+)\s*\}//){
		$alias = $2;
		$alias =~ s/\\%/%/g;
		$alltitles{$alias} = $rdtitle;
		$aliasnm{$alias} = $manfilebase;
		$naliases++;
	    }
	}
    }

    sub foldorder {uc($a) cmp uc($b) or $a cmp $b;}

    open(anindex, ">${anindex}");
    foreach $alias (sort foldorder keys %aliasnm) {
	print anindex "$alias\t$aliasnm{$alias}\n";
    }
    close anindex;


    open(anindex, "<$anindex");
    open(titleindex, ">$lib/$pkg/help/00Titles");
    open(htmlfile, ">$lib/$pkg/html/00Index.$HTML");
    if($opt_chm) {open(chmfile, ">$chmdir/00Index.$HTML");}

    print htmlfile html_pagehead("$title", "../../../doc/html",
				 "../../../doc/html/index.$HTML", "Top",
				 "../../../doc/html/packages.$HTML",
				 "Package List");

    if($opt_chm) {print chmfile chm_pagehead("$title");}


    if($naliases>100){
	print htmlfile html_alphabet();
	if($opt_chm) {print chmfile html_alphabet();}
   }

    print htmlfile "\n<p>\n<table width=\"100%\">\n";
    if($opt_chm) {print chmfile "\n<p>\n<table width=\"100%\">\n";}

    my $firstletter = "";
    while(<anindex>){ 
        chomp;  ($alias, $file) = split /\t/; 
        $aliasfirst = uc substr($alias, 0, 1);
	if($aliasfirst lt "A") { $aliasfirst = ""; }
	if($aliasfirst gt "Z") { $aliasfirst = "misc"; }
	if( ($naliases > 100) && ($aliasfirst ne $firstletter) ) {
	    print htmlfile "</table>\n";
	    print htmlfile html_title2("<a name=\"$aliasfirst\">-- $aliasfirst --</a>");
	    print htmlfile "<table width=\"100%\">\n";
	    if($opt_chm) {
		print chmfile "</table>\n";
		print chmfile html_title2("<a name=\"$aliasfirst\">-- $aliasfirst --</a>");
		print chmfile "<table width=\"100%\">\n";
	    }
	    $firstletter = $aliasfirst;
	}
	print titleindex "$alias\t$alltitles{$alias}\n";
	$htmlalias = $alias;
	$htmlalias =~ s/</&lt;/go;
	$htmlalias =~ s/>/&gt;/go;
	print htmlfile "<TR><TD width=\"25%\"><A HREF=\"$file.$HTML\">" .
	    "$htmlalias</A></TD>\n<TD>$alltitles{$alias}</TD></TR>\n";
	if($opt_chm) {
	    print chmfile "<TR><TD width=\"25%\"><A HREF=\"$file.$HTML\">" .
		"$htmlalias</A></TD>\n<TD>$alltitles{$alias}</TD></TR>\n";}
    }

    print htmlfile "</TABLE>\n";
    print htmlfile "</BODY></HTML>\n";
    if($opt_chm) {print chmfile "</table>\n</body></HTML>\n";}

    close titleindex;
    close htmlfile;
    if($opt_chm) {close chmfile;}
    close anindex;

    build_htmlpkglist($lib);
}


sub build_htmlfctlist {

    my $lib = $_[0];

    my %htmltitles = read_functiontitles($lib);
    my $key;

    open(htmlfile, ">$R_HOME/doc/html/function.$HTML");

    print htmlfile html_pagehead("Functions installed in R_HOME", ".",
				 "index.$HTML", "Top",
				 "packages.$HTML", "Packages");

    print htmlfile html_alphabet();

    print htmlfile html_title2("-- Operators, Global Variables, ... --");
    print htmlfile "\n<p>\n<table width=\"100%\">\n";
    foreach $alias (sort foldorder keys %htmltitles) {
	print htmlfile "<TR><TD width=\"25%\">" .
	    "<A HREF=\"../../library/$htmlindex{$alias}\">" .
	    "$alias</A></TD>\n<TD>$htmltitles{$alias}</TD></TR>\n"
		unless $alias =~ /^[a-zA-Z]/;
    }
    print htmlfile "\n</table>\n<p>\n<table width=\"100%\">\n";

    my $firstletter = "";
    foreach $alias (sort foldorder keys %htmltitles) {
	$aliasfirst = uc substr($alias, 0, 1);
	if($aliasfirst =~ /[A-Z]/){
	    if($aliasfirst ne $firstletter){
		print htmlfile "</table>\n";
		print htmlfile "<a name=\"" . uc $aliasfirst . "\">\n";
		print htmlfile html_title2("-- " . uc $aliasfirst . " --");
		print htmlfile "<table width=\"100%\">\n";
		$firstletter = $aliasfirst;
	    }
	    print htmlfile "<TR><TD width=\"25%\">" .
		"<A HREF=\"../../library/$htmlindex{$alias}\">" .
		    "$alias</A></TD>\n<TD>$htmltitles{$alias}</TD></TR>\n";
	}
    }

    print htmlfile "</TABLE>\n";
    print htmlfile "</BODY>\n";

    close htmlfile;
}

sub fileolder { #(filename, age)
    my($file, $age) = @_;
    #- return ``true'' if file exists and is older than $age
    (! ((-f $file) && ((-M $file) < $age)))
}

sub build_chm_toc {
    open tocfile, ">../chm/$pkg.toc" 
	|| die "Couldn't open the chm toc file";
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

1;
# Local variables: **
# perl-indent-level: 4 **
# cperl-indent-level: 4 **
# End: **
