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
my $RVER, $RVER0;
my $RW=$ARGV[0];
my $SRCDIR=$ARGV[1];
my $MDISDI=$ARGV[2];
my $HelpStyle=$ARGV[3];
my $Internet=$ARGV[4];

$SRCDIR =~ s+/+\\+g; # need DOS-style paths

## add to the target command line in the CmdParms function below

open ver, "< ../../../VERSION";
$RVER = <ver>;
close ver;
$RVER =~ s/\n.*$//;
$RVER =~ s/Under .*$/Pre-release/;
$RVER0 = $RVER;
$RVER0 =~ s/ .*$//;

open insfile, "> R.iss" || die "Cannot open R.iss\n";
print insfile <<END;
[Setup]
OutputBaseFilename=${RW}-win32
PrivilegesRequired=none
END

my $lines=<<END;
AppName=R for Windows
AppVerName=R for Windows $RVER
AppPublisher=R Development Core Team
AppPublisherURL=http://www.r-project.org
AppSupportURL=http://www.r-project.org
AppUpdatesURL=http://www.r-project.org
AppVersion=${RVER}
VersionInfoVersion=$RVER0
DefaultDirName={code:UserPF}\\R\\${RW}
DefaultGroupName=R
AllowNoIcons=yes
InfoBeforeFile=${SRCDIR}\\COPYING
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
Name: es; MessagesFile: "compiler:Languages\\Spanish.isl"
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
Name: "{group}\\R $RVER"; Filename: "{app}\\bin\\Rgui.exe"; WorkingDir: "{app}"; Parameters: {code:CmdParms}
Name: "{group}\\Uninstall R $RVER"; Filename: "{uninstallexe}"
Name: "{commondesktop}\\R $RVER"; Filename: "{app}\\bin\\Rgui.exe"; MinVersion: 4,4; Tasks: desktopicon; WorkingDir: "{app}"; Parameters: {code:CmdParms}
Name: "{userappdata}\\Microsoft\\Internet Explorer\\Quick Launch\\R $RVER"; Filename: "{app}\\bin\\Rgui.exe"; Tasks: quicklaunchicon; WorkingDir: "{app}"; Parameters: {code:CmdParms}


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
Name: "{group}\\R $RVER Help"; Filename: "{app}\\doc\\html\\index.html"; Components: html

[Types]
Name: "user"; Description: {cm:user}
Name: "compact"; Description: {cm:compact}
Name: "full"; Description: {cm:full}
Name: "custom"; Description: {cm:custom}; Flags: iscustom

[Components]
Name: "main"; Description: "Main Files"; Types: user compact full custom; Flags: fixed
Name: "chtml"; Description: "Compiled HTML Help Files"; Types: user full custom
Name: "html"; Description: "HTML Help Files"; Types: user full custom
Name: "manuals"; Description: "On-line (PDF) Manuals"; Types: user full custom
Name: "devel"; Description: "Source Package Installation Files"; Types: user full custom
Name: "tcl"; Description: "Support Files for Package tcltk"; Types: user full custom
Name: "libdocs"; Description: "Docs for Packages grid and survival"; Types: user full custom
Name: "trans"; Description: "Message Translations"; Types: user full custom
Name: "latex"; Description: "Latex Help Files"; Types: full custom
Name: "refman"; Description: "PDF Reference Manual"; Types: full custom
Name: "Rd"; Description: "Source Files for Help Pages"; Types: full custom

[Code]

var
  NoAdminPage: TOutputMsgWizardPage;
  SelectOptionsPage: TInputOptionWizardPage;
  MDISDIPage: TInputOptionWizardPage;
  HelpStylePage: TInputOptionWizardPage;
  InternetPage: TInputOptionWizardPage;
  
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
  
  SelectOptionsPage := CreateInputOptionPage(wpSelectComponents,
    CustomMessage(\'startupt'\), CustomMessage(\'startupq\'),
    CustomMessage(\'startupi\'), True, False);
  SelectOptionsPage.Add(CustomMessage(\'startup0\'));
  SelectOptionsPage.Add(CustomMessage(\'startup1\'));
  SelectOptionsPage.SelectedValueIndex := 1;
  
  MDISDIPage := CreateInputOptionPage(SelectOptionsPage.ID,
    CustomMessage(\'MDIt'\), CustomMessage(\'MDIq\'),
    CustomMessage(\'MDIi\'), True, False);
  MDISDIPage.Add(CustomMessage(\'MDI0\'));
  MDISDIPage.Add(CustomMessage(\'MDI1\'));
  
  HelpStylePage := CreateInputOptionPage(MDISDIPage.ID,
    CustomMessage(\'HelpStylet'\), CustomMessage(\'HelpStyleq\'),
    CustomMessage(\'HelpStylei\'), True, False);
  HelpStylePage.Add(CustomMessage(\'HelpStyle0\'));
  HelpStylePage.Add(CustomMessage(\'HelpStyle1\'));
  HelpStylePage.Add(CustomMessage(\'HelpStyle2\'));
   
  InternetPage := CreateInputOptionPage(HelpStylePage.ID,
    CustomMessage(\'Internett'\), CustomMessage(\'Internetq\'),
    CustomMessage(\'Interneti\'), True, False);
  InternetPage.Add(CustomMessage(\'Internet0\'));
  InternetPage.Add(CustomMessage(\'Internet1\'));    

  case GetPreviousData(\'MDISDI\', \'\') of
    \'MDI\': MDISDIPage.SelectedValueIndex := 0;
    \'SDI\': MDISDIPage.SelectedValueIndex := 1;
  else
    MDISDIPage.SelectedValueIndex := ${MDISDI};
  end;

  case GetPreviousData(\'HelpStyle\', \'\') of
    \'plain\': HelpStylePage.SelectedValueIndex := 0;
    \'CHM\':   HelpStylePage.SelectedValueIndex := 1;
    \'HTML\':  HelpStylePage.SelectedValueIndex := 2;
  else
    HelpStylePage.SelectedValueIndex := ${HelpStyle};
  end;
  
  case GetPreviousData(\'Internet\', \'\') of
    \'Standard\': InternetPage.SelectedValueIndex := 0;
    \'Internet2\': InternetPage.SelectedValueIndex := 1;
  else
    InternetPage.SelectedValueIndex := ${Internet};
  end;  
  
end;

procedure RegisterPreviousData(PreviousDataKey: Integer);
var
  MDISDI: String;
  HelpStyle: String;
  Internet: String;
begin
  { Store the settings so we can restore them next time }
  case MDISDIPage.SelectedValueIndex of
    0: MDISDI := \'MDI\';
    1: MDISDI := \'SDI\';
  end;
  SetPreviousData(PreviousDataKey, \'MDISDI\', MDISDI);
  case HelpStylePage.SelectedValueIndex of
    0: HelpStyle := \'plain\';
    1: HelpStyle := \'CHM\';
    2: HelpStyle := \'HTML\';
  end;
  SetPreviousData(PreviousDataKey, \'HelpStyle\', HelpStyle);  
  case InternetPage.SelectedValueIndex of
    0: Internet := \'Standard\';
    1: Internet := \'Internet2\';
  end;
  SetPreviousData(PreviousDataKey, \'Internet\', Internet);
end;

procedure SetCommentMarker(var lines: TArrayOfString; option: String; active: boolean);
var
  i : integer;
begin
  for i := 0 to pred(GetArrayLength(lines)) do
    if pos(option, lines[i]) > 0 then 
    begin
      if active then
        lines[i][1] := \' \'
      else
        lines[i][1] := \'#\';
      exit;
    end;
end;
  
procedure EditOptions();
var
  lines : TArrayOfString;
  filename : String;
begin
  filename := ExpandConstant(CurrentFilename);
  LoadStringsFromFile(filename, lines);
  
  SetCommentMarker(lines, \'MDI = yes\', MDISDIPage.SelectedValueIndex = 0);
  SetCommentMarker(lines, \'MDI = no\', MDISDIPage.SelectedValueIndex = 1);
  
  SetCommentMarker(lines, \'options(chmhelp\', HelpStylePage.SelectedValueIndex = 1);
  SetCommentMarker(lines, \'options(htmlhelp\', HelpStylePage.SelectedValueIndex = 2);
  
  SaveStringsToFile(filename, lines, False);
end;

function CmdParms(Param:String): String;
begin
  Result := \'\';
  if InternetPage.SelectedValueIndex = 1 then
    Result := \'--internet2\';
end;

function ShouldSkipPage(PageID: Integer): boolean;
begin
  if PageID = NoAdminPage.ID then Result := IsAdmin
  else if (PageID = MDISDIPage.ID) or (PageID = HelpStylePage.ID) or (PageID = InternetPage.ID) then 
    Result := SelectOptionsPage.SelectedValueIndex = 1
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
		"bin\\Sd2Rd" => 1,
		"etc\\Makeconf" => 1);
		
$path="${SRCDIR}";chdir($path);
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
	} else {
	    $component = "main";
	}

	$lines="Source: \"$path\\$fn\"; DestDir: \"{app}$dir\"; Flags: ignoreversion; Components: $component";
	$lines="$lines; AfterInstall: EditOptions()" if $_ eq "etc\\Rprofile.site"
	                                             || $_ eq "etc\\Rconsole";
	$lines="$lines\n";

	print insfile $lines;
    }
}
