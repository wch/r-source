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

$dir_mod = 0755;#- Permission ('mode') of newly created directories.

# determine of pkg and lib directory are accessible; chdir to pkg man dir
# and return pkg name, full path to lib dir and contents of mandir

sub buildinit {

    my $pkg = $ARGV[0];
    my $lib = $ARGV[1];

    my $currentdir = getcwd();

    if($pkg){
	die("Package $pkg does not exit\n") unless (-d $pkg);
    }
    else{
	$pkg="$RHOME/src/library/base";
    }

    chdir $currentdir;

    if($lib){
        mkdir "$lib", $dir_mod || die "Could not create $lib: $!\n";
	$lib=getcwd();
    }
    else{
	$lib="$RHOME/library";
    }

    chdir $currentdir;

    chdir($pkg) or die("Cannot change to $pkg\n");
    $pkg = basename(getcwd());

    chdir "man" or die("There are no man pages in $pkg\n");
    opendir man, '.';
    @mandir = readdir(man);
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

### Read all aliases into an hash array with the (relative) paths to
### the corresponding html files

sub read_htmlindex {

    my $lib = $_[0];

    my %hi;
    my $pkg;

    opendir lib, $lib;
    my @libs = readdir(lib);
    closedir lib;

    foreach $pkg (@libs) {
	if(-d "$lib/$pkg"){
	    if(! ( ($pkg =~ /^CVS$/) || ($pkg =~ /^\.+$/))){
		if(-r "$lib/$pkg/help/AnIndex"){
		    open ranindex, "< $lib/$pkg/help/AnIndex";
		    while(<ranindex>){
			/^(\S*)\s*(.*)/;
			$hi{$1} = "../../$pkg/html/$2.html";
		    }
		    close ranindex;
		}
	    }
	}
    }

    %hi;
}


### Build $RHOME/library/index.html from the $pkg/TITLE files

sub build_htmlpkglist {

    my $lib = $_[0];

    my %htmltitles = read_titles($lib);
    my $key;

    open(htmlfile, ">$lib/index.html");

    print htmlfile "<HEAD><TITLE>R Function Index</TITLE></HEAD>\n";
    print htmlfile "<BODY LINK=#0000EF VLINK=#0000EF>\n";
    print htmlfile "<A HREF =\"../html/index.html\">[top]</A>\n";
    print htmlfile "<HR ALIGN=middle>\n";
    print htmlfile "<H2><CENTER>";
    print htmlfile "R Function Index";
    print htmlfile "</CENTER></H2>\n";
    print htmlfile "<HR ALIGN=middle>\n";
    print htmlfile "<P>";
    print htmlfile "The following packages are currently installed:\n";
    print htmlfile "<P><TABLE>\n";

    foreach $key (sort(keys %htmltitles)) {
	print htmlfile "<TR ALIGN=LEFT VALIGN=TOP>\n";
	print htmlfile "<TD><A HREF=\"$key/html/00Index.html\">";
	print htmlfile "$key</A><TD>";
	print htmlfile $htmltitles{$key};

    }

    print htmlfile "</TABLE>\n";
    print htmlfile "</BODY>\n";

    close htmlfile;
}


1;



