#-*- perl -*-
# Copyright (C) 2001-5 R Development Core Team
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
# writing to the Free Software Foundation, Inc., 51 Franklin Street,
# Fifth Floor, Boston, MA 02110-1301  USA.

# Send any bug reports to r-bugs@r-project.org

use Cwd;
use File::Find;

my $fn, $component, $path;
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
print insfile <<END;
[Setup]
OutputBaseFilename=${RW}-win32
END

my $lines=<<END;
AppName=R for Windows
AppVerName=R for Windows $RVER
AppPublisher=R Development Core Team
AppPublisherURL=http://www.r-project.org
AppSupportURL=http://www.r-project.org
AppUpdatesURL=http://www.r-project.org
AppVersion=${RVER}
DefaultDirName={code:UserPF}\\R\\${RW}
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

[Languages]
Name: en; MessagesFile: "compiler:Default.isl"
Name: br; MessagesFile: "compiler:Languages\\BrazilianPortuguese.isl"
Name: ca; MessagesFile: "compiler:Languages\\Catalan.isl"
Name: cz; MessagesFile: "compiler:Languages\\Czech.isl"
Name: dk; MessagesFile: "compiler:Languages\\Danish.isl"
Name: nl; MessagesFile: "compiler:Languages\\Dutch.isl"
Name: fr; MessagesFile: "compiler:Languages\\French.isl"
Name: fi; MessagesFile: "compiler:Languages\\Finnish.isl"
Name: de; MessagesFile: "compiler:Languages\\German.isl"
Name: hu; MessagesFile: "compiler:Languages\\Hungarian.isl"
Name: it; MessagesFile: "compiler:Languages\\Italian.isl"
Name: no; MessagesFile: "compiler:Languages\\Norwegian.isl"
Name: po; MessagesFile: "compiler:Languages\\Polish.isl"
Name: pt; MessagesFile: "compiler:Languages\\Portuguese.isl"
Name: ru; MessagesFile: "compiler:Languages\\Russian.isl"
Name: sl; MessagesFile: "compiler:Languages\\Slovenian.isl"
Name: chs; MessagesFile: "ChineseSimp.isl"
Name: cht; MessagesFile: "ChineseTrad.isl"
Name: ja; MessagesFile: "Japanese.isl"
Name: ko; MessagesFile: "Korean.isl"

#include "CustomMsg.txt"

[Tasks]
Name: "desktopicon"; Description: {cm:CreateDesktopIcon}; GroupDescription: {cm:AdditionalIcons}; MinVersion: 4,4
Name: "quicklaunchicon"; Description: {cm:CreateQuickLaunchIcon}; GroupDescription: {cm:AdditionalIcons}; MinVersion: 4,4; Flags: unchecked 
Name: "recordversion"; Description: {cm:recordversion}; GroupDescription: {cm:regentries}; MinVersion: 4,4
Name: "associate"; Description: {cm:associate}; GroupDescription: {cm:regentries}; MinVersion: 4,4; Check: IsAdmin


[Icons]
Name: "{group}\\R $RVER"; Filename: "{app}\\bin\\Rgui.exe"; $iconpars
Name: "{group}\\Uninstall R $RVER"; Filename: "{uninstallexe}"
Name: "{userdesktop}\\R $RVER"; Filename: "{app}\\bin\\Rgui.exe"; MinVersion: 4,4; Tasks: desktopicon; $iconpars
Name: "{userappdata}\\Microsoft\\Internet Explorer\\Quick Launch\\R $RVER"; Filename: "{app}\\bin\\Rgui.exe"; Tasks: quicklaunchicon; $iconpars


[Registry] 
Root: HKLM; Subkey: "Software\\R-core"; Flags: uninsdeletekeyifempty; Tasks: recordversion; Check: IsAdmin
Root: HKLM; Subkey: "Software\\R-core\\R"; Flags: uninsdeletekeyifempty; Tasks: recordversion; Check: IsAdmin
Root: HKLM; Subkey: "Software\\R-core\\R"; Flags: uninsdeletevalue; ValueType: string; ValueName: "InstallPath"; ValueData: "{app}"; Check: IsAdmin
Root: HKLM; Subkey: "Software\\R-core\\R"; Flags: uninsdeletevalue; ValueType: string; ValueName: "Current Version"; ValueData: "${RVER}"; Check: IsAdmin

Root: HKLM; Subkey: "Software\\R-core\\R\\${RVER}"; Flags: uninsdeletekey; Tasks: recordversion; Check: IsAdmin
Root: HKLM; Subkey: "Software\\R-core\\R\\${RVER}"; ValueType: string; ValueName: "InstallPath"; ValueData: "{app}"; Check: IsAdmin

Root: HKCU; Subkey: "Software\\R-core"; Flags: uninsdeletekeyifempty; Tasks: recordversion; Check: NonAdmin
Root: HKCU; Subkey: "Software\\R-core\\R"; Flags: uninsdeletekeyifempty; Tasks: recordversion; Check: NonAdmin
Root: HKCU; Subkey: "Software\\R-core\\R"; Flags: uninsdeletevalue; ValueType: string; ValueName: "InstallPath"; ValueData: "{app}"; Check: NonAdmin
Root: HKCU; Subkey: "Software\\R-core\\R"; Flags: uninsdeletevalue; ValueType: string; ValueName: "Current Version"; ValueData: "${RVER}"; Check: NonAdmin

Root: HKCU; Subkey: "Software\\R-core\\R\\${RVER}"; Flags: uninsdeletekey; Tasks: recordversion; Check: NonAdmin
Root: HKCU; Subkey: "Software\\R-core\\R\\${RVER}"; ValueType: string; ValueName: "InstallPath"; ValueData: "{app}"; Check: NonAdmin

Root: HKCR; Subkey: ".RData"; ValueType: string; ValueName: ""; ValueData: "RWorkspace"; Flags: uninsdeletevalue; Tasks: associate; Check: IsAdmin
Root: HKCR; Subkey: "RWorkspace"; ValueType: string; ValueName: ""; ValueData: "R Workspace"; Flags: uninsdeletekey; Tasks: associate; Check: IsAdmin
Root: HKCR; Subkey: "RWorkspace\\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\\bin\\RGui.exe,0"; Tasks: associate; Check: IsAdmin 
Root: HKCR; Subkey: "RWorkspace\\shell\\open\\command"; ValueType: string; ValueName: ""; ValueData: """{app}\\bin\\RGui.exe"" ""%1"""; Tasks: associate; Check: IsAdmin
END

print insfile $lines;
print insfile "SolidCompression=yes\n";
print insfile $lines2;
print insfile <<END;

[Icons]
Name: "{group}\\R $RVER Help"; Filename: "{app}\\doc\\html\\Rwin.html"; Components: html

[Types]
Name: "user"; Description: {cm:user}
Name: "compact"; Description: {cm:compact}
Name: "full"; Description: {cm:full}
Name: "CJK"; Description: {cm:CJK}
Name: "custom"; Description: {cm:custom}; Flags: iscustom

[Components]
Name: "main"; Description: "Main Files"; Types: user compact full CJK custom; Flags: fixed
Name: "chtml"; Description: "Compiled HTML Help Files"; Types: user full CJK custom
Name: "html"; Description: "HTML Help Files"; Types: user full CJK custom
Name: "manuals"; Description: "On-line (PDF) Manuals"; Types: user full CJK custom
Name: "devel"; Description: "Source Package Installation Files"; Types: user full CJK custom
Name: "tcl"; Description: "Support Files for Package tcltk"; Types: user full CJK custom
Name: "libdocs"; Description: "Docs for Packages grid and survival"; Types: user full CJK custom
Name: "trans"; Description: "Message Translations"; Types: user full CJK custom
Name: "mbcs"; Description: "Version for East Asian languages"; Types: CJK custom
Name: "latex"; Description: "Latex Help Files"; Types: full custom
Name: "refman"; Description: "PDF Reference Manual"; Types: full custom
Name: "Rd"; Description: "Source Files for Help Pages"; Types: full custom

[Code]

var
  NoAdminPage: TOutputMsgWizardPage;

function IsAdmin: boolean;
begin
  Result := IsAdminLoggedOn or IsPowerUserLoggedOn;
end;

function NonAdmin: boolean;
begin
  Result := not IsAdmin;
end;

procedure InitializeWizard;
begin
  NoAdminPage := CreateOutputMsgPage(wpWelcome, SetupMessage(msgInformationTitle), 
    CustomMessage(\'adminprivilegesrequired\'), CustomMessage(\'adminexplanation\'));
end;

function ShouldSkipPage(PageID: Integer): boolean;
begin
  if PageID = NoAdminPage.ID then Result := IsAdmin
  else Result := false;
end;

function UserPF(Param:String): String;
begin
  Result := ExpandConstant(\'{pf}\');
  if (not IsAdmin) then 
  begin
    try
      Result := ExpandConstant('\{userdocs}\');
    except
    // Do nothing, user doesn't have a My Documents folder
    end;
  end;
end;

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

sub listFiles {
    $fn = $File::Find::name;
    $fn =~ s+^./++;
    my $newname = "";
    if (!(-d $_)) {
	$fn =~ s+/+\\+g;
	$dir = $fn;
	$dir =~ s/[^\\]+$//;
	$dir = "\\".$dir;
	$dir =~ s/\\$//;
	$_ = $fn;
	
	if ($_ eq "bin\\Rchtml.dll" 
	    || m/^library\\[^\\]*\\chtml/) {
	    $component = "chtml";
	} elsif ($_ eq "doc\\html\\logo.jpg") {
	    $component = "html devel";
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
	} elsif ($_ eq "doc\\manual\\refman.pdf") {
	    $component = "refman";
	} elsif (m/^doc\\manual/ && $_ ne "doc\\manual\\R-FAQ.pdf") {
	    $component = "manuals";
	} elsif (m/^library\\[^\\]*\\latex/) {
	    	$component = "latex";
	} elsif (m/^library\\[^\\]*\\man/) {
	    	$component = "Rd";
	} elsif (m/^Tcl/) {
	    $component = "tcl";
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
	} elsif (m/^library\\grid\\doc/
		 || $_ eq "library\\survival\\survival.ps.gz") {
	    $component = "libdocs";
	} elsif ($_ eq "modules\\iconv.dll") {
	    $component = "main";
	} elsif (m/^share\\locale/ 
		 || m/^library\\[^\\]*\\po/) { # needs iconv
	    $component = "trans";
	} elsif ($_ eq "bin\\Rmbcs.dll") {
	    $component = "mbcs";
	    $newname = "R.dll";
	} elsif (m/^library\\grDevices\\CID/) {
	    $component = "mbcs";
	} else {
	    $component = "main";
	}

	$lines="Source: \"$path\\$fn\"; DestDir: \"{app}$dir\"; Flags: ignoreversion; Components: $component\n";
	$lines="Source: \"$path\\$fn\"; DestDir: \"{app}$dir\"; DestName: \"$newname\"; Flags: ignoreversion; Components: $component\n" if $newname ne "";

	print insfile $lines;
    }
}
