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
my $mode64bit = $ARGV[6];

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

if($mode64bit) {
    $suffix = "win64";
    $PF = "pf64"; # "pf" should do
    $QUAL = " x64"; # used for AppName
    $SUFF = "-x64"; # used for default install dir
    $RK = "R64"; # arch-specific key
} else {
    $suffix = "win32";
    $PF = "pf32"; # "pf" should do
    $QUAL = "";
    $SUFF = "";
    $RK = "R32";
}
open insfile, "> R.iss" || die "Cannot open R.iss\n";
print insfile <<END;
[Setup]
OutputBaseFilename=${RW}-${suffix}
PrivilegesRequired=none
MinVersion=0,5.0
END
print insfile "ArchitecturesInstallIn64BitMode=x64\nArchitecturesAllowed=x64\n" if $mode64bit;

my $lines=<<END;
AppName=R for Windows$QUAL $RVER
AppVerName=R for Windows$QUAL $RVER
AppPublisherURL=http://www.r-project.org
AppSupportURL=http://www.r-project.org
AppUpdatesURL=http://www.r-project.org
AppVersion=${RVER}
VersionInfoVersion=$RVER0
DefaultDirName={code:UserPF}\\R\\${RW}${SUFF}
DefaultGroupName=R
AllowNoIcons=yes
InfoBeforeFile=${SRCDIR}\\COPYING
DisableReadyPage=yes
DisableStartupPrompt=yes
OutputDir=.
WizardSmallImageFile=R.bmp
UsePreviousAppDir=no
ChangesAssociations=yes
Compression=lzma/ultra
SolidCompression=yes
END

my $lines2=<<END;

[Languages]
Name: en; MessagesFile: "compiler:Default.isl"
Name: br; MessagesFile: "compiler:Languages\\BrazilianPortuguese.isl"
Name: ca; MessagesFile: "compiler:Languages\\Catalan.isl"
Name: cz; MessagesFile: "compiler:Languages\\Czech.isl"
Name: dk; MessagesFile: "compiler:Languages\\Danish.isl"
Name: nl; MessagesFile: "compiler:Languages\\Dutch.isl"
Name: fi; MessagesFile: "compiler:Languages\\Finnish.isl"
Name: fr; MessagesFile: "compiler:Languages\\French.isl"
Name: de; MessagesFile: "compiler:Languages\\German.isl"
Name: he; MessagesFile: "compiler:Languages\\Hebrew.isl"
Name: hu; MessagesFile: "compiler:Languages\\Hungarian.isl"
Name: it; MessagesFile: "compiler:Languages\\Italian.isl"
Name: ja; MessagesFile: "compiler:Languages\\Japanese.isl"
Name: no; MessagesFile: "compiler:Languages\\Norwegian.isl"
Name: po; MessagesFile: "compiler:Languages\\Polish.isl"
Name: pt; MessagesFile: "compiler:Languages\\Portuguese.isl"
Name: ru; MessagesFile: "compiler:Languages\\Russian.isl"
Name: sk; MessagesFile: "compiler:Languages\\Slovak.isl"
Name: sl; MessagesFile: "compiler:Languages\\Slovenian.isl"
Name: es; MessagesFile: "compiler:Languages\\Spanish.isl"
Name: chs; MessagesFile: "ChineseSimp.isl"
Name: cht; MessagesFile: "ChineseTrad.isl"
Name: ko; MessagesFile: "Korean.isl"

#include "CustomMsg.txt"

[Tasks]
Name: "desktopicon"; Description: {cm:CreateDesktopIcon}; GroupDescription: {cm:AdditionalIcons}; MinVersion: 0,5.0
Name: "quicklaunchicon"; Description: {cm:CreateQuickLaunchIcon}; GroupDescription: {cm:AdditionalIcons}; MinVersion: 0,5.0; Flags: unchecked 
Name: "recordversion"; Description: {cm:recordversion}; GroupDescription: {cm:regentries}; MinVersion: 0,5.0
Name: "associate"; Description: {cm:associate}; GroupDescription: {cm:regentries}; MinVersion: 0,5.0; Check: IsAdmin


[Icons]
Name: "{group}\\R$QUAL $RVER"; Filename: "{app}\\bin\\Rgui.exe"; WorkingDir: "{userdocs}"; Parameters: {code:CmdParms}
Name: "{group}\\Uninstall R$QUAL $RVER"; Filename: "{uninstallexe}"
Name: "{commondesktop}\\R$QUAL $RVER"; Filename: "{app}\\bin\\Rgui.exe"; MinVersion: 0,5.0; Tasks: desktopicon; WorkingDir: "{userdocs}"; Parameters: {code:CmdParms}
Name: "{userappdata}\\Microsoft\\Internet Explorer\\Quick Launch\\R$QUAL $RVER"; Filename: "{app}\\bin\\Rgui.exe"; Tasks: quicklaunchicon; WorkingDir: "{userdocs}"; Parameters: {code:CmdParms}


[Registry] 
Root: HKLM; Subkey: "Software\\$Producer"; Flags: uninsdeletekeyifempty; Tasks: recordversion; Check: IsAdmin
Root: HKLM; Subkey: "Software\\$Producer\\R"; Flags: uninsdeletekeyifempty; Tasks: recordversion; Check: IsAdmin
Root: HKLM; Subkey: "Software\\$Producer\\R"; Flags: uninsdeletevalue; ValueType: string; ValueName: "InstallPath"; ValueData: "{app}"; Tasks: recordversion; Check: IsAdmin
Root: HKLM; Subkey: "Software\\$Producer\\R"; Flags: uninsdeletevalue; ValueType: string; ValueName: "Current Version"; ValueData: "${RVER}"; Tasks: recordversion; Check: IsAdmin
Root: HKLM; Subkey: "Software\\$Producer\\R\\${RVER}"; Flags: uninsdeletekey; Tasks: recordversion; Check: IsAdmin
Root: HKLM; Subkey: "Software\\$Producer\\R\\${RVER}"; ValueType: string; ValueName: "InstallPath"; ValueData: "{app}"; Tasks: recordversion; Check: IsAdmin

Root: HKLM; Subkey: "Software\\$Producer\\${RK}"; Flags: uninsdeletekeyifempty; Tasks: recordversion; Check: IsAdmin
Root: HKLM; Subkey: "Software\\$Producer\\${RK}"; Flags: uninsdeletevalue; ValueType: string; ValueName: "InstallPath"; ValueData: "{app}"; Tasks: recordversion; Check: IsAdmin
Root: HKLM; Subkey: "Software\\$Producer\\${RK}"; Flags: uninsdeletevalue; ValueType: string; ValueName: "Current Version"; ValueData: "${RVER}"; Tasks: recordversion; Check: IsAdmin
Root: HKLM; Subkey: "Software\\$Producer\\${RK}\\${RVER}"; Flags: uninsdeletekey; Tasks: recordversion; Check: IsAdmin
Root: HKLM; Subkey: "Software\\$Producer\\${RK}\\${RVER}"; ValueType: string; ValueName: "InstallPath"; ValueData: "{app}"; Tasks: recordversion; Check: IsAdmin

Root: HKCU; Subkey: "Software\\$Producer"; Flags: uninsdeletekeyifempty; Tasks: recordversion; Check: NonAdmin
Root: HKCU; Subkey: "Software\\$Producer\\R"; Flags: uninsdeletekeyifempty; Tasks: recordversion; Check: NonAdmin
Root: HKCU; Subkey: "Software\\$Producer\\R"; Flags: uninsdeletevalue; ValueType: string; ValueName: "InstallPath"; ValueData: "{app}"; Tasks: recordversion; Check: NonAdmin
Root: HKCU; Subkey: "Software\\$Producer\\R"; Flags: uninsdeletevalue; ValueType: string; ValueName: "Current Version"; ValueData: "${RVER}"; Tasks: recordversion; Check: NonAdmin
Root: HKCU; Subkey: "Software\\$Producer\\R\\${RVER}"; Flags: uninsdeletekey; Tasks: recordversion; Check: NonAdmin
Root: HKCU; Subkey: "Software\\$Producer\\R\\${RVER}"; ValueType: string; ValueName: "InstallPath"; ValueData: "{app}"; Tasks: recordversion; Check: NonAdmin

Root: HKCU; Subkey: "Software\\$Producer\\${RK}"; Flags: uninsdeletekeyifempty; Tasks: recordversion; Check: NonAdmin
Root: HKCU; Subkey: "Software\\$Producer\\${RK}"; Flags: uninsdeletevalue; ValueType: string; ValueName: "InstallPath"; ValueData: "{app}"; Tasks: recordversion; Check: NonAdmin
Root: HKCU; Subkey: "Software\\$Producer\\${RK}"; Flags: uninsdeletevalue; ValueType: string; ValueName: "Current Version"; ValueData: "${RVER}"; Tasks: recordversion; Check: NonAdmin
Root: HKCU; Subkey: "Software\\$Producer\\${RK}\\${RVER}"; Flags: uninsdeletekey; Tasks: recordversion; Check: NonAdmin
Root: HKCU; Subkey: "Software\\$Producer\\${RK}\\${RVER}"; ValueType: string; ValueName: "InstallPath"; ValueData: "{app}"; Tasks: recordversion; Check: NonAdmin

Root: HKCR; Subkey: ".RData"; ValueType: string; ValueName: ""; ValueData: "RWorkspace"; Flags: uninsdeletevalue; Tasks: associate; Check: IsAdmin
Root: HKCR; Subkey: "RWorkspace"; ValueType: string; ValueName: ""; ValueData: "R Workspace"; Flags: uninsdeletekey; Tasks: associate; Check: IsAdmin
Root: HKCR; Subkey: "RWorkspace\\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\\bin\\RGui.exe,0"; Tasks: associate; Check: IsAdmin 
Root: HKCR; Subkey: "RWorkspace\\shell\\open\\command"; ValueType: string; ValueName: ""; ValueData: """{app}\\bin\\RGui.exe"" ""%1"""; Tasks: associate; Check: IsAdmin
END

## It is OK to use the same keys in HKLM for 32- and 64-bit versions as the 
## view of the Registry depends on the arch.
## But not for HLCU (and file associations are necessarily for both).

print insfile $lines;
if($Producer eq "R-core") {
    print insfile "AppPublisher=R Development Core Team\n";
} else {
    print insfile "AppPublisher=$Producer\n";
}
print insfile $lines2;
print insfile <<END;

[Icons]
Name: "{group}\\R$QUAL $RVER Help"; Filename: "{app}\\doc\\html\\index.html"; Components: html

[Types]
Name: "user"; Description: {cm:user}
Name: "compact"; Description: {cm:compact}
Name: "full"; Description: {cm:full}
Name: "custom"; Description: {cm:custom}; Flags: iscustom

[Components]
Name: "main"; Description: "Main Files"; Types: user compact full custom; Flags: fixed
Name: "html"; Description: "HTML Manuals"; Types: user full custom
Name: "manuals"; Description: "On-line PDF Manuals"; Types: user full custom
Name: "manuals/basic"; Description: "Basic Manuals"; Types: user full custom
Name: "manuals/technical"; Description: "Technical Manuals"; Types: full custom
Name: "manuals/refman"; Description: "PDF help pages (reference manual)"; Types: full custom
Name: "manuals/libdocs"; Description: "Docs for Packages grid and Matrix"; Types: full custom
Name: "tcl"; Description: "Support Files for Package tcltk"; Types: user full custom; Flags: checkablealone
Name: "tcl/tzdata"; Description: "Timezone files for Tcl"; Types: full custom
Name: "tcl/chm"; Description: "Tcl/Tk Help (Compiled HTML)"; Types: full custom
Name: "trans"; Description: "Message Translations"; Types: user full custom
Name: "tests"; Description: "Test files"; Types: full custom


[Code]

var
  NoAdminPage: TOutputMsgWizardPage;
  SelectOptionsPage: TInputOptionWizardPage;
  MDISDIPage: TInputOptionWizardPage;
  HelpStylePage: TInputOptionWizardPage;
  InternetPage: TInputOptionWizardPage;
  INIFilename: String;
  
function IsAdmin: boolean;
begin
  Result := IsAdminLoggedOn or IsPowerUserLoggedOn;
end;

function NonAdmin: boolean;
begin
  Result := not IsAdmin;
end;

procedure InitializeWizard;
var
  option : String;
  index : Integer;
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
   
  InternetPage := CreateInputOptionPage(HelpStylePage.ID,
    CustomMessage(\'Internett'\), CustomMessage(\'Internetq\'),
    CustomMessage(\'Interneti\'), True, False);
  InternetPage.Add(CustomMessage(\'Internet0\'));
  InternetPage.Add(CustomMessage(\'Internet1\'));    

  INIFilename := ExpandConstant(\'{param:LOADINF}\');
  if INIFilename <> \'\' then INIFilename := ExpandFilename(INIFilename);
  
  { From highest to lowest, priority is:
    LOADINF value
    PreviousData value
    Default from build }
  
  option := GetPreviousData(\'MDISDI\', \'\');
  if INIFilename <> '' then
    option := GetIniString(\'R\', \'MDISDI\', option, INIFilename);
  case option of
    \'MDI\': index := 0;
    \'SDI\': index := 1;
  else
    index := ${MDISDI};
  end;  
  MDISDIPage.SelectedValueIndex := index;

  option := GetPreviousData(\'HelpStyle\', \'\');
  if INIFilename <> \'\' then
    option := GetIniString(\'R\', \'HelpStyle\', option, INIFilename);  
  case option of
    \'plain\': index := 0;
    \'CHM\':   index := 1;
    \'HTML\':  index := 1;
  else
    index := ${HelpStyle};
  end;
  HelpStylePage.SelectedValueIndex := index;
  
  option := GetPreviousData(\'Internet\', \'\');
  if INIFilename <> \'\' then
    option := GetIniString(\'R\', \'Internet\', option, INIFilename);
  case option of
    \'Standard\':  index := 0;
    \'Internet2\': index := 1;
  else
    index := ${Internet};
  end;
  InternetPage.SelectedValueIndex := index;
    
  { Get the save name now, because the current dir might change }
  INIFilename := ExpandConstant(\'{param:SAVEINF}\');
  if INIFilename <> \'\' then INIFilename := ExpandFilename(INIFilename);    
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
  if INIFilename <> \'\' then
    SetIniString(\'R\', \'MDISDI\', MDISDI, INIFilename);
    
  case HelpStylePage.SelectedValueIndex of
    0: HelpStyle := \'plain\';
    1: HelpStyle := \'HTML\';
  end;
  SetPreviousData(PreviousDataKey, \'HelpStyle\', HelpStyle);  
  if INIFilename <> \'\' then
    SetIniString(\'R\', \'HelpStyle\', HelpStyle, INIFilename);
  
  case InternetPage.SelectedValueIndex of
    0: Internet := \'Standard\';
    1: Internet := \'Internet2\';
  end;
  SetPreviousData(PreviousDataKey, \'Internet\', Internet);
  if INIFilename <> \'\' then
    SetIniString(\'R\', \'Internet\', Internet, INIFilename);
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
  
  SetCommentMarker(lines, \'options(help_type="text"\', HelpStylePage.SelectedValueIndex = 0);
  SetCommentMarker(lines, \'options(help_type="html"\', HelpStylePage.SelectedValueIndex = 1);
  
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
  Result := ExpandConstant(\'{${PF}}\');
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
	} elsif (m/^Tcl\\doc\\.*chm$/) {
	    $component = "tcl/chm";
	} elsif (m/^Tcl\\lib\\tcl8.5\\tzdata/) {
	    $component = "tcl/tzdata";
	} elsif (m/^Tcl/) {
	    $component = "tcl";
	} elsif (m/^library\\grid\\doc/ || m/^library\\Matrix\\doc/) {
	    $component = "manuals/libdocs";
	} elsif (m/^share\\locale/ 
		 || m/^library\\[^\\]*\\po/) {
	    $component = "trans";
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
