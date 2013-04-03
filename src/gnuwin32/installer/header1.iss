PrivilegesRequired=none
MinVersion=0,5.0
DefaultGroupName=R
AllowNoIcons=yes
DisableReadyPage=yes
DisableStartupPrompt=yes
OutputDir=.
WizardSmallImageFile=R.bmp
UsePreviousAppDir=no
ChangesAssociations=yes
Compression=lzma/ultra
SolidCompression=yes
AppPublisherURL=http://www.r-project.org
AppSupportURL=http://www.r-project.org
AppUpdatesURL=http://www.r-project.org

[Languages]
Name: en; MessagesFile: "compiler:Default.isl"
Name: br; MessagesFile: "compiler:Languages\BrazilianPortuguese.isl"
Name: ca; MessagesFile: "compiler:Languages\Catalan.isl"
Name: cz; MessagesFile: "compiler:Languages\Czech.isl"
Name: dk; MessagesFile: "compiler:Languages\Danish.isl"
Name: nl; MessagesFile: "compiler:Languages\Dutch.isl"
Name: fi; MessagesFile: "compiler:Languages\Finnish.isl"
Name: fr; MessagesFile: "compiler:Languages\French.isl"
Name: de; MessagesFile: "compiler:Languages\German.isl"
Name: gr; MessagesFile: "compiler:Languages\Greek.isl"
Name: he; MessagesFile: "compiler:Languages\Hebrew.isl"
Name: hu; MessagesFile: "compiler:Languages\Hungarian.isl"
Name: it; MessagesFile: "compiler:Languages\Italian.isl"
Name: ja; MessagesFile: "compiler:Languages\Japanese.isl"
Name: no; MessagesFile: "compiler:Languages\Norwegian.isl"
Name: po; MessagesFile: "compiler:Languages\Polish.isl"
Name: pt; MessagesFile: "compiler:Languages\Portuguese.isl"
Name: ru; MessagesFile: "compiler:Languages\Russian.isl"
Name: sk; MessagesFile: "compiler:Languages\Slovakian.isl"
Name: sl; MessagesFile: "compiler:Languages\Slovenian.isl"
Name: es; MessagesFile: "compiler:Languages\Spanish.isl"
Name: uk; MessagesFile: "compiler:Languages\Ukrainian.isl"
Name: chs; MessagesFile: "ChineseSimplified.isl"
Name: cht; MessagesFile: "ChineseTraditional.isl"
Name: ko; MessagesFile: "Korean.isl"

#include "CustomMsg.iss"

[Tasks]
Name: "desktopicon"; Description: {cm:CreateDesktopIcon}; GroupDescription: {cm:AdditionalIcons}; MinVersion: 0,5.0
Name: "quicklaunchicon"; Description: {cm:CreateQuickLaunchIcon}; GroupDescription: {cm:AdditionalIcons}; MinVersion: 0,5.0; Flags: unchecked 
Name: "recordversion"; Description: {cm:recordversion}; GroupDescription: {cm:regentries}; MinVersion: 0,5.0
Name: "associate"; Description: {cm:associate}; GroupDescription: {cm:regentries}; MinVersion: 0,5.0; Check: IsAdmin
