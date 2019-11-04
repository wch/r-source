[Icons]
Name: "{group}\R @RVER@"; Filename: "{app}\bin\i386\Rgui.exe"; WorkingDir: "{app}"; Parameters: "--cd-to-userdocs"
Name: "{commondesktop}\R @RVER@"; Filename: "{app}\bin\i386\Rgui.exe"; MinVersion: 0,5.0; Tasks: desktopicon; WorkingDir: "{app}"; Parameters: "--cd-to-userdocs"
Name: "{userappdata}\Microsoft\Internet Explorer\Quick Launch\R @RVER@"; Filename: "{app}\bin\i386\Rgui.exe"; Tasks: quicklaunchicon; WorkingDir: "{app}"; Parameters: "--cd-to-userdocs"

[Registry] 
Root: HKLM; Subkey: "Software\@Producer@"; Flags: uninsdeletekeyifempty; Tasks: recordversion; Check: IsAdmin
Root: HKLM; Subkey: "Software\@Producer@\R"; Flags: uninsdeletekeyifempty; Tasks: recordversion; Check: IsAdmin
Root: HKLM; Subkey: "Software\@Producer@\R"; Flags: uninsdeletevalue; ValueType: string; ValueName: "InstallPath"; ValueData: "{app}"; Tasks: recordversion; Check: IsAdmin
Root: HKLM; Subkey: "Software\@Producer@\R"; Flags: uninsdeletevalue; ValueType: string; ValueName: "Current Version"; ValueData: "@RVER@"; Tasks: recordversion; Check: IsAdmin
Root: HKLM; Subkey: "Software\@Producer@\R\@RVER@"; Flags: uninsdeletekey; Tasks: recordversion; Check: IsAdmin
Root: HKLM; Subkey: "Software\@Producer@\R\@RVER@"; ValueType: string; ValueName: "InstallPath"; ValueData: "{app}"; Tasks: recordversion; Check: IsAdmin

Root: HKLM; Subkey: "Software\@Producer@\R32"; Flags: uninsdeletekeyifempty; Tasks: recordversion; Check: IsAdmin
Root: HKLM; Subkey: "Software\@Producer@\R32"; Flags: uninsdeletevalue; ValueType: string; ValueName: "InstallPath"; ValueData: "{app}"; Tasks: recordversion; Check: IsAdmin
Root: HKLM; Subkey: "Software\@Producer@\R32"; Flags: uninsdeletevalue; ValueType: string; ValueName: "Current Version"; ValueData: "@RVER@"; Tasks: recordversion; Check: IsAdmin
Root: HKLM; Subkey: "Software\@Producer@\R32\@RVER@"; Flags: uninsdeletekey; Tasks: recordversion; Check: IsAdmin
Root: HKLM; Subkey: "Software\@Producer@\R32\@RVER@"; ValueType: string; ValueName: "InstallPath"; ValueData: "{app}"; Tasks: recordversion; Check: IsAdmin

Root: HKCU; Subkey: "Software\@Producer@"; Flags: uninsdeletekeyifempty; Tasks: recordversion; Check: NonAdmin
Root: HKCU; Subkey: "Software\@Producer@\R"; Flags: uninsdeletekeyifempty; Tasks: recordversion; Check: NonAdmin
Root: HKCU; Subkey: "Software\@Producer@\R"; Flags: uninsdeletevalue; ValueType: string; ValueName: "InstallPath"; ValueData: "{app}"; Tasks: recordversion; Check: NonAdmin
Root: HKCU; Subkey: "Software\@Producer@\R"; Flags: uninsdeletevalue; ValueType: string; ValueName: "Current Version"; ValueData: "@RVER@"; Tasks: recordversion; Check: NonAdmin
Root: HKCU; Subkey: "Software\@Producer@\R\@RVER@"; Flags: uninsdeletekey; Tasks: recordversion; Check: NonAdmin
Root: HKCU; Subkey: "Software\@Producer@\R\@RVER@"; ValueType: string; ValueName: "InstallPath"; ValueData: "{app}"; Tasks: recordversion; Check: NonAdmin

Root: HKCU; Subkey: "Software\@Producer@\R32"; Flags: uninsdeletekeyifempty; Tasks: recordversion; Check: NonAdmin
Root: HKCU; Subkey: "Software\@Producer@\R32"; Flags: uninsdeletevalue; ValueType: string; ValueName: "InstallPath"; ValueData: "{app}"; Tasks: recordversion; Check: NonAdmin
Root: HKCU; Subkey: "Software\@Producer@\R32"; Flags: uninsdeletevalue; ValueType: string; ValueName: "Current Version"; ValueData: "@RVER@"; Tasks: recordversion; Check: NonAdmin
Root: HKCU; Subkey: "Software\@Producer@\R32\@RVER@"; Flags: uninsdeletekey; Tasks: recordversion; Check: NonAdmin
Root: HKCU; Subkey: "Software\@Producer@\R32\@RVER@"; ValueType: string; ValueName: "InstallPath"; ValueData: "{app}"; Tasks: recordversion; Check: NonAdmin

Root: HKCR; Subkey: ".RData"; ValueType: string; ValueName: ""; ValueData: "RWorkspace"; Flags: uninsdeletevalue; Tasks: associate; Check: IsAdmin
Root: HKCR; Subkey: "RWorkspace"; ValueType: string; ValueName: ""; ValueData: "R Workspace"; Flags: uninsdeletekey; Tasks: associate; Check: IsAdmin
Root: HKCR; Subkey: "RWorkspace\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\bin\i386\RGui.exe,0"; Tasks: associate; Check: IsAdmin 
Root: HKCR; Subkey: "RWorkspace\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\bin\i386\RGui.exe"" --workspace=""%1"""; Tasks: associate; Check: IsAdmin
