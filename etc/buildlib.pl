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
# writing to the Free Software Foundation, Inc., 675 Mass Ave,
# Cambridge, MA 02139, USA.

# Send any bug reports to Friedrich.Leisch@ci.tuwien.ac.at


use Cwd;
use File::Basename;

require "$RHOME/etc/html-layout.pl";


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

    if($pkg){
	die("Package $pkg does not exist\n") unless (-d $pkg);
    }
    else{
	$pkg="$RHOME/src/library/base";
    }

    chdir $currentdir;

    if($lib){
        mkdir "$lib", $dir_mod || die "Could not create $lib: $!\n";
	chdir $lib;
	$lib=getcwd();
	chdir $currentdir;
    }
    else{
	$lib="$RHOME/library";
    }

    chdir $currentdir;

    chdir($pkg) or die("Cannot change to $pkg\n");
    $pkg = basename(getcwd());

    chdir "man" or die("There are no man pages in $pkg\n");
    opendir man, '.';
    @mandir = sort(readdir(man));
    closedir man;

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



### Build $RHOME/doc/html/packages.html from the $pkg/TITLE files

sub build_htmlpkglist {

    my $lib = $_[0];

    my %htmltitles = read_titles($lib);
    my $key;

    open(htmlfile, ">$RHOME/doc/html/packages.$HTML");

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
    print htmlfile "</BODY>\n";

    close htmlfile;
}



sub build_index {

    if(! -d $lib){
        mkdir "$lib", $dir_mod || die "Could not create directory $lib: $!\n";
    }

    if(! -d "$dest"){
        mkdir "$dest", $dir_mod || die "Could not create directory $dest: $!\n";
    }

    system("/bin/cp ../TITLE $dest/TITLE");
    open title, "<../TITLE";
    $title = <title>;
    close title;
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
		$manfilebase = "x" . $nmanfiles++;
	    }
	    else{
		$manfilebase = $rdname;
	    }

	    open(rdfile, "<$manfile");
	    undef $text;
	    while(<rdfile>){ $text .= $_;}
	    close rdfile;
	    $text =~ /\\title\{\s*([^\}]+)\s*\}/s;
	    my $rdtitle = $1;
	    $rdtitle =~ s/\n/ /sg;

	    $filenm{$rdname} = $manfilebase;

	    while($text =~ s/\\(alias|name)\{\s*(.*)\s*\}//){
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

    print htmlfile html_pagehead("$title", "../../../doc/html",
				 "../../../doc/html/index.$HTML", "Top",
				 "../../../doc/html/packages.$HTML",
				 "Package List");


    if($naliases>100){
       print htmlfile html_alphabet();
   }

    print htmlfile "\n<p>\n<table width=100%>\n";

    my $firstletter = "";
    while(<anindex>){ 
        chomp;  ($alias, $file) = split /\t/; 
        $aliasfirst = uc substr($alias, 0, 1); 
        if(($aliasfirst ne $firstletter) && 
	   ($aliasfirst =~ /[A-Z]/) &&
	   ($naliases>100)){
	    print htmlfile "</table>\n";
	    print htmlfile "<a name=\"$aliasfirst\">\n";
	    print htmlfile html_title2("-- $aliasfirst --");
	    print htmlfile "<table width=100%>\n";
	    $firstletter = $aliasfirst;
	}
	print titleindex "$alias\t$alltitles{$alias}\n";
	print htmlfile "<TR><TD width=25%><A HREF=\"$file.$HTML\">" .
	    "$alias</A></TD>\n<TD>$alltitles{$alias}</TD></TR>\n";
    }

    print htmlfile "</TABLE>\n";
    print htmlfile "</BODY>\n";

    close titleindex;
    close htmlfile;
    close anindex;

#    build_htmlpkglist($lib);
}


sub build_htmlfctlist {

    my $lib = $_[0];

    my %htmltitles = read_functiontitles($lib);
    my $key;

    open(htmlfile, ">$RHOME/doc/html/function.$HTML");

    print htmlfile html_pagehead("Functions installed in RHOME", ".",
				 "index.$HTML", "Top",
				 "packages.$HTML", "Packages");

    print htmlfile html_alphabet();

    print htmlfile html_title2("-- Operators, Global Variables, ... --");
    print htmlfile "\n<p>\n<table width=100%>\n";
    foreach $alias (sort foldorder keys %htmltitles) {
	print htmlfile "<TR><TD width=25%>" .
	    "<A HREF=\"../../library/$htmlindex{$alias}\">" .
	    "$alias</A></TD>\n<TD>$htmltitles{$alias}</TD></TR>\n"
		unless $alias =~ /^[a-zA-Z]/;
    }
    print htmlfile "\n</table>\n<p>\n<table width=100%>\n";

    my $firstletter = "";
    foreach $alias (sort foldorder keys %htmltitles) {
	$aliasfirst = uc substr($alias, 0, 1);
	if($aliasfirst =~ /[A-Z]/){
	    if($aliasfirst ne $firstletter){
		print htmlfile "</table>\n";
		print htmlfile "<a name=\"" . uc $aliasfirst . "\">\n";
		print htmlfile html_title2("-- " . uc $aliasfirst . " --");
		print htmlfile "<table width=100%>\n";
		$firstletter = $aliasfirst;
	    }
	    print htmlfile "<TR><TD width=25%>" .
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

1;
# Local variables: **
# perl-indent-level: 4 **
# cperl-indent-level: 4 **
# End: **
