#-*- perl -*-
# Copyright (C) 2001-4 R Development Core Team
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
# A copy of the GNU General Public License is available via WWW at
# http://www.gnu.org/copyleft/gpl.html.	 You can also obtain it by
# writing to the Free Software Foundation, Inc., 59 Temple Place,
# Suite 330, Boston, MA  02111-1307  USA.

# Send any bug reports to r-bugs@r-project.org

use Cwd;
use File::Find;

my $fn, $component, $mini, $path;
my $startdir=cwd();
my $RVER;
my $RW=$ARGV[0];
my $SRCDIR=$ARGV[1];
$SRCDIR =~ s+/+\\+g; # need DOS-style paths
my $iconpars="WorkingDir: \"{app}\"" ;
## add to the target command line as in the next example
# my $iconpars="Parameters: \"--sdi\"; WorkingDir: \"{app}\"" ;

open ver, "< ../../../VERSION";
$RVER = <ver>;
close ver;
$RVER =~ s/\n.*$//;
$RVER =~ s/Under .*$/Pre-release/;

open insfile, "> R.iss" || die "Cannot open R.iss\n";
open minifile,"> Rsmall.iss" || die "Cannot open Rsmall.iss\n";

print minifile <<END;
[Setup]
OutputBaseFilename=miniR
DiskSpanning=yes
END

print insfile <<END;
[Setup]
OutputBaseFilename=${RW}
END

my $lines=<<END;
AppName=R for Windows
AppVerName=R for Windows $RVER
AppPublisher=R Development Core Team
AppPublisherURL=http://www.r-project.org
AppSupportURL=http://www.r-project.org
AppUpdatesURL=http://www.r-project.org
AppVersion=${RVER}
DefaultDirName={pf}\\R\\${RW}
DefaultGroupName=R
AllowNoIcons=yes
LicenseFile=${SRCDIR}\\COPYING
DisableReadyPage=yes
DisableStartupPrompt=yes
OutputDir=.
WizardSmallImageFile=R.bmp
UsePreviousAppDir=no
ChangesAssociations=yes
Compression=lzma
END

my $lines2=<<END;

[Tasks]
Name: "desktopicon"; Description: "Create a &desktop icon"; GroupDescription: "Additional icons:"; MinVersion: 4,4
Name: "quicklaunchicon"; Description: "Create a &Quick Launch icon"; GroupDescription: "Additional icons:"; MinVersion: 4,4; Flags: unchecked 
Name: "associate"; Description: "&Associate R with .RData files"; GroupDescription: "Registry entries:"; MinVersion: 4,4
Name: "DCOM"; Description: "&Register R path for use by the (D)COM server"; GroupDescription: "Registry entries:"; MinVersion: 4,4

[Icons]
Name: "{group}\\R $RVER"; Filename: "{app}\\bin\\Rgui.exe"; $iconpars
Name: "{group}\\Uninstall R $RVER"; Filename: "{uninstallexe}"
Name: "{userdesktop}\\R $RVER"; Filename: "{app}\\bin\\Rgui.exe"; MinVersion: 4,4; Tasks: desktopicon; $iconpars

[Registry] 
Root: HKLM; Subkey: "Software\\R-core"; Flags: uninsdeletekeyifempty; Tasks: DCOM
Root: HKLM; Subkey: "Software\\R-core\\R"; Flags: uninsdeletekey; Tasks: DCOM
Root: HKLM; Subkey: "Software\\R-core\\R"; ValueType: string; ValueName: "InstallPath"; ValueData: "{app}"; Tasks: DCOM
Root: HKLM; Subkey: "Software\\R-core\\R"; ValueType: string; ValueName: "Current Version"; ValueData: "${RVER}"; Tasks: DCOM

Root: HKCR; Subkey: ".RData"; ValueType: string; ValueName: ""; ValueData: "RWorkspace"; Flags: uninsdeletevalue; Tasks: associate
Root: HKCR; Subkey: "RWorkspace"; ValueType: string; ValueName: ""; ValueData: "R Workspace"; Flags: uninsdeletekey; Tasks: associate 
Root: HKCR; Subkey: "RWorkspace\\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\\bin\\RGui.exe,0"; Tasks: associate 
Root: HKCR; Subkey: "RWorkspace\\shell\\open\\command"; ValueType: string; ValueName: ""; ValueData: """{app}\\bin\\RGui.exe"" ""%1"""; Tasks: associate 
END

print insfile $lines;
print insfile "SolidCompression=yes\n";
print insfile $lines2;
print insfile <<END;

[Icons]
Name: "{group}\\R $RVER Help"; Filename: "{app}\\doc\\html\\Rwin.html"; Components: html

[Types]
Name: "user"; Description: "User installation"
Name: "compact"; Description: "Minimal user installation"
Name: "full"; Description: "Full installation"
Name: "CJK"; Description: "Chinese/Japanese/Korean installation";
Name: "custom"; Description: "Custom installation"; Flags: iscustom

[Components]
Name: "main"; Description: "Main Files"; Types: user compact full CJK custom; Flags: fixed
Name: "chtml"; Description: "Compiled HTML Help Files"; Types: user full CJK custom
Name: "html"; Description: "HTML Help Files"; Types: user full CJK custom
Name: "latex"; Description: "Latex Help Files"; Types: full custom
Name: "manuals"; Description: "On-line (PDF) Manuals"; Types: user full CJK custom
Name: "refman"; Description: "PDF Reference Manual"; Types: full custom
Name: "libdocs"; Description: "Docs for Packages grid and survival"; Types: user full CJK custom
Name: "devel"; Description: "Source Package Installation Files"; Types: user full CJK custom
Name: "tcl"; Description: "Support Files for Package tcltk"; Types: user full CJK custom
Name: "mbcs"; Description: "Version for East Asian languages"; Types: CJK custom
Name: "Rd"; Description: "Source Files for Help Pages"; Types: full custom

[Files]
END

print minifile $lines;
print minifile $lines2;
print minifile <<END;

[Types]
Name: "compact"; Description: "Minimal user installation"
Name: "custom"; Description: "Custom installation"; Flags: iscustom

[Components]
Name: "main"; Description: "Main Files"; Types: compact custom; Flags: fixed
Name: "manuals"; Description: "On-line (PDF) Manuals"; Types: custom

[Files]
END

my %develfiles=("doc\\html\\logo.jpg" => 1,
		"README.packages" => 1,
		"COPYING.LIB" => 1,
		"bin\\INSTALL" => 1,
		"bin\\REMOVE" => 1,
		"bin\\SHLIB" => 1,
		"bin\\build" => 1,
		"bin\\check" => 1,
		"bin\\massage-Examples" => 1,
		"bin\\Rd2dvi.sh" => 1,
		"bin\\Rd2txt" => 1,
		"bin\\Rdconv" => 1,
		"bin\\Rdiff.sh" => 1,
		"bin\\Sd2Rd" => 1);
		
$path="${SRCDIR}";chdir($path);
find(\&listFiles, ".");

close insfile;
close minifile;

sub listFiles {
    $fn = $File::Find::name;
    $fn =~ s+^./++;
    my $mini = 1;
    my $newname = "";
    if (!(-d $_)) {
	$fn =~ s+/+\\+g;
	$dir = $fn;
	$dir =~ s/[^\\]+$//;
	$dir = "\\".$dir;
	$dir =~ s/\\$//;
	$_ = $fn;
	
	if (m/^library\\tcltk/ || m/^MD5/ || m/^bin\\md5check.exe/) {
	    $mini = 0;
	}
	if ($_ eq "bin\\Rchtml.dll" 
	    || m/^library\\[^\\]*\\chtml/) {
	    $component = "chtml";
	    $mini = 0;
	} elsif ($_ eq "doc\\html\\logo.jpg") {
	    $component = "html devel";
	    $mini = 0;
	} elsif ($_ eq "doc\\manual\\R-FAQ.html"
		 || $_ eq "doc\\html\\rw-FAQ.html"
		 || $_ eq "share\\texmf\\Sweave.sty") {
	    $component = "main";
	} elsif (m/^doc\\html/
		 || m/^doc\\manual\\[^\\]*\.html/
		 || m/^library\\[^\\]*\\html/
		 || m/^library\\[^\\]*\\CONTENTS/
		 || $_ eq "library\\R.css") {
	    $component = "html";
	    $mini = 0;
	} elsif ($_ eq "doc\\manual\\refman.pdf") {
	    $component = "refman";
	    $mini = 0;
	} elsif (m/^doc\\manual/ && $_ ne "doc\\manual\\R-FAQ.pdf") {
	    $component = "manuals";
	    if (m/R-admin.pdf/ || m/R-exts.pdf/ || m/R-lang.pdf/) {
		$mini = 0;
	    }
	} elsif (m/^library\\[^\\]*\\latex/) {
	    	$component = "latex";
	    	$mini = 0;
	} elsif (m/^library\\[^\\]*\\man/) {
	    	$component = "Rd";
	    	$mini = 0;
	} elsif (m/^Tcl/) {
	    $component = "tcl";
	    $mini = 0;
	} elsif (exists($develfiles{$_})
		 || m/^doc\\KEYWORDS/
		 || m/^src\\gnuwin32/
		 || m/^include/
		 || m/^src\\library\\windlgs/
		 || m/^share\\make/
		 || m/^share\\perl/
		 || m/^share\\R/
		 || m/^share\\texmf/
		 || m/^bin\\build/
		 || m/^bin\\check/
		 || m/^bin\\INSTALL/
		 || m/^bin\\massage-Examples/
		 || m/^bin\\Rd2dvi.sh/
		 || m/^bin\\Rd2txt/
		 || m/^bin\\Rdconv/
		 || m/^bin\\Rdiff.sh/
		 || m/^bin\\REMOVE/
		 || m/^bin\\Rprof/
		 || m/^bin\\Sd2Rd/
		 || m/^bin\\SHLIB/
		 || m/^lib\\/) {
	    $component = "devel";
	    $mini = 0;
	} elsif (m/^library\\grid\\doc/
		 || $_ eq "library\\survival\\survival.ps.gz") {
	    $component = "libdocs";
	    $mini = 0;
	} elsif ($_ eq "modules\\iconv.dll") {
	    $component = "main";
	    $mini = 0;
	} elsif ($_ eq "bin\\Rmbcs.dll") {
	    $component = "mbcs";
	    $mini = 0;
	    $newname = "R.dll";
	} else {
	    $component = "main";
	}

	$lines="Source: \"$path\\$fn\"; DestDir: \"{app}$dir\"; Flags: ignoreversion; Components: $component\n";
	$lines="Source: \"$path\\$fn\"; DestDir: \"{app}$dir\"; DestName: \"$newname\"; Flags: ignoreversion; Components: $component\n" if $newname ne "";

	print insfile $lines;
	if ($mini) {
	    print minifile $lines;
	}
    }
}
