## Subroutines for building R documentation

## Copyright (C) 1997-2009 R Development Core Team
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
## A copy of the GNU General Public License is available at
## http://www.r-project.org/Licenses/

## Send any bug reports to r-bugs@r-project.org.

package R::Rdlists;

require  Exporter;
@ISA     = qw(Exporter);
@EXPORT  = qw(buildinit read_htmlindex read_htmlpkgindex read_anindex
	     fileolder foldorder aliasorder);

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
    $text =~ s/&/&amp;/go;
    $text =~ s/---/-/go;
    $text =~ s/--/-/go;
    $text =~ s/\`\`/&ldquo;/g;
    $text =~ s/\'\'/&rdquo;/g;
    $text =~ s/\`([^']+)'/&lsquo;$1&rsquo;/g;
    $text =~ s/\`/\'/g;		# @samp{'} could be an apostrophe ...
    $text =~ s/</&lt;/g;
    $text =~ s/>/&gt;/g;
    $text =~ s/ +/ /go;
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

## return true if file exists and is older than $age
sub fileolder {
    my($file, $age) = @_;
    (! ((-f $file) && ((-M $file) < $age)));
}

sub html_pagehead
{
    my ($title, $top, $up, $uptext, $prev, $prevtext, $next, $nextext, 
	$cssloc, $enc) = @_;

    my $retval = 
	"<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">\n" .
	"<html><head><title>R: $title</title>\n" .
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

1;
# Local variables: **
# perl-indent-level: 4 **
# cperl-indent-level: 4 **
# End: **
