#-*- perl -*-
# Copyright (C) 2001-10 R Development Core Team
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	See the GNU
# General Public License for more details.
#
# A copy of the GNU General Public License is available at
# http://www.r-project.org/Licenses/

# Send any bug reports to r-bugs@r-project.org


use Cwd;
use File::Find;

my $fn, $component, $path;
my $startdir=cwd();
my $RVER, $RVER0, $SVN;
my $RW=$ARGV[0];
my $SRCDIR=$ARGV[1];
my $MDISDI=$ARGV[2];
my $HelpStyle=$ARGV[3];
my $Internet=$ARGV[4];
my $Producer = $ARGV[5];

my $have32bit = 0;
my $have64bit = 0;

$have32bit = 1 if -d "$SRCDIR\\bin\\i386";
$have64bit = 1 if -d "$SRCDIR\\bin\\x64";


$SRCDIR =~ s+/+\\+g; # need DOS-style paths

## add to the target command line in the CmdParms function below

open ver, "< ../../../VERSION";
$RVER = <ver>;
close ver;
$RVER =~ s/\n.*$//;
$RVER =~ s/Under .*$/Pre-release/;
$RVER0 = $RVER;
$RVER0 =~ s/ .*$//;
## now add SVN revision
open ver, "< ../../../SVN-REVISION";
$SVN = <ver>;
close ver;
$SVN =~s/Revision: //;
$RVER0 .= "." . $SVN;

open insfile, "> R.iss" || die "Cannot open R.iss\n";
print insfile <<END;
[Setup]
END

if ($have32bit && $have64bit) {
    $suffix = "win";
    $QUAL = "";
    $SUFF = "";
    # bindir and RK are not used
    print insfile "ArchitecturesInstallIn64BitMode=x64\n";
} elsif ($have64bit) {
    $suffix = "win64";
    $QUAL = " x64"; # used for AppName
    $SUFF = "-x64"; # used for default install dir
    $bindir = "bin/x64"; # used for shortcuts
    $RK = "R64"; # arch-specific key
    print insfile "ArchitecturesInstallIn64BitMode=x64\nArchitecturesAllowed=x64\n";
} else { # 32-bit only
    $suffix = "win32";
    $QUAL = "";
    $SUFF = "";
    $bindir = "bin/i386";
    $RK = "R32";
}

print insfile <<END;
OutputBaseFilename=${RW}-${suffix}
AppName=R for Windows$QUAL $RVER
AppVerName=R for Windows$QUAL $RVER
AppVersion=${RVER}
VersionInfoVersion=$RVER0
DefaultDirName={code:UserPF}\\R\\${RW}${SUFF}
InfoBeforeFile=${SRCDIR}\\COPYING
END

if($Producer eq "R-core") {
    print insfile "AppPublisher=R Development Core Team\n";
} else {
    print insfile "AppPublisher=$Producer\n";
}

open IN,  "< header1.iss" or die "can't open header1.iss";
while (<IN>) {print insfile $_;} 
close IN;

my $regfile;
if ($have32bit && $have64bit) {
    $regfile = "reg3264.iss";
} else {
    $regfile = "reg.iss";
}

open (IN,  "< $regfile") or die "can't open $regfile";
while (<IN>) {
    s/\@RVER\@/${RVER}/g;
    s/\@Producer\@/${Producer}/g;
    s/\@bindir\@/${bindir}/g;
    s/\@RK\@/${RK}/g;
    s/\@QUAL\@/${QUAL}/g;
    print insfile $_;
}
close IN;


my $types;
if ($have32bit && $have64bit) {
    $types = "types3264.iss";
} elsif ($have64bit) {
    $types = "types64.iss";
} else {
    $types = "types32.iss";
}

open (IN,  "< $types") or die "can't open $types";
while (<IN>) {print insfile $_;} 
close IN;


open (IN,  "< code.iss") or die "can't open code.iss";
while (<IN>) {
    s/\@MDISDI\@/${MDISDI}/g;
    s/\@HelpStyle\@/${HelpStyle}/g;
    s/\@Internet\@/${Internet}/g;
    print insfile $_;
} 
close IN;

print insfile "\n\n[Files]\n"; 

$path="${SRCDIR}"; chdir($path);
find(\&listFiles, ".");

close insfile;

sub listFiles {
    $fn = $File::Find::name;
    $fn =~ s+^./++;
    if (!(-d $_)) {
	$fn =~ s+/+\\+g;
	$dir = $fn;
	$dir =~ s/[^\\]+$//;
	$dir = "\\".$dir;
	$dir =~ s/\\$//;
	$_ = $fn;
	
	## These manuals are on the Rgui menu, so should always be installed
	if ($_ eq "doc\\manual\\R-FAQ.html"
		 || $_ eq "doc\\html\\rw-FAQ.html"
		 || $_ eq "share\\texmf\\Sweave.sty") {
	    $component = "main";
	} elsif (m/^doc\\html/
		 || m/^library\\[^\\]*\\html/
		 || $_ eq "library\\R.css") {
	    $component = "main";
	} elsif (m/^doc\\manual\\[^\\]*\.html/ ) {
	    $component = "html";
	} elsif ($_ eq "doc\\manual\\R-data.pdf"
		 || $_ eq "doc\\manual\\R-intro.pdf") {
	    $component = "manuals/basic";
	} elsif ($_ eq "doc\\manual\\R-admin.pdf" 
		 || $_ eq "doc\\manual\\R-exts.pdf"
		 || $_ eq "doc\\manual\\R-ints.pdf"
		 || $_ eq "doc\\manual\\R-lang.pdf") {
	    $component = "manuals/technical";
	} elsif ($_ eq "doc\\manual\\refman.pdf") {
	    $component = "manuals/refman";
	} elsif (m/^doc\\manual/ && $_ ne "doc\\manual\\R-FAQ.pdf") {
	    $component = "manuals";
	} elsif (m/^library\\[^\\]*\\tests/) {
	    	$component = "tests";
	} elsif (m/^tests/) {
	    	$component = "tests";
	} elsif (m/^Tcl\\(bin|lib)64/) {
	    $component = "tcl/64";
	} elsif ($have32bit && m/^Tcl\\bin/) {
	    $component = "tcl/32";
	} elsif ($have32bit && m/^Tcl\\lib\\(dde1.3|reg1.2|Tktable)/) {
	    $component = "tcl/32";
	} elsif (m/^Tcl\\doc\\.*chm$/) {
	    $component = "tcl/chm";
	} elsif (m/^Tcl\\lib\\tcl8.5\\tzdata/) {
	    $component = "tcl/tzdata";
	} elsif (m/^Tcl/) {
	    $component = "tcl/noarch";
	} elsif (m/^library\\grid\\doc/ || m/^library\\Matrix\\doc/) {
	    $component = "manuals/libdocs";
	} elsif (m/^share\\locale/ 
		 || m/^library\\[^\\]*\\po/) {
	    $component = "trans";
	} elsif (m/\\i386\\/) {
	    $component = "i386";
	} elsif (m/\\x64\\/) {
	    $component = "x64";
	} else {
	    $component = "main";
	}

	$lines="Source: \"$path\\$fn\"; DestDir: \"{app}$dir\"; Flags: ignoreversion; Components: $component";
	$lines="$lines; AfterInstall: EditOptions()" 
	    if $_ eq "etc\\Rprofile.site" || $_ eq "etc\\Rconsole";
	$lines="$lines\n";

	print insfile $lines;
    }
}
