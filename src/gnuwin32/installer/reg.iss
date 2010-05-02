[Icons]
Name: "{group}\R@QUAL@ @RVER@"; Filename: "{app}\@bindir@\Rgui.exe"; WorkingDir: "{userdocs}"; Parameters: {code:CmdParms}
Name: "{group}\Uninstall R@QUAL@ @RVER@"; Filename: "{uninstallexe}"
Name: "{commondesktop}\R@QUAL@ @RVER@"; Filename: "{app}\@bindir@\Rgui.exe"; MinVersion: 0,5.0; Tasks: desktopicon; WorkingDir: "{userdocs}"; Parameters: {code:CmdParms}
Name: "{userappdata}\Microsoft\Internet Explorer\Quick Launch\R@QUAL@ @RVER@"; Filename: "{app}\@bindir@\Rgui.exe"; Tasks: quicklaunchicon; WorkingDir: "{userdocs}"; Parameters: {code:CmdParms}
Name: "{group}\R@QUAL@ @RVER@ Help"; Filename: "{app}\doc\html\index.html"; Components: html

[Registry] 
Root: HKLM; Subkey: "Software\@Producer@"; Flags: uninsdeletekeyifempty; Tasks: recordversion; Check: IsAdmin
Root: HKLM; Subkey: "Software\@Producer@\R"; Flags: uninsdeletekeyifempty; Tasks: recordversion; Check: IsAdmin
Root: HKLM; Subkey: "Software\@Producer@\R"; Flags: uninsdeletevalue; ValueType: string; ValueName: "InstallPath"; ValueData: "{app}"; Tasks: recordversion; Check: IsAdmin
Root: HKLM; Subkey: "Software\@Producer@\R"; Flags: uninsdeletevalue; ValueType: string; ValueName: "Current Version"; ValueData: "@RVER@"; Tasks: recordversion; Check: IsAdmin
Root: HKLM; Subkey: "Software\@Producer@\R\@RVER@"; Flags: uninsdeletekey; Tasks: recordversion; Check: IsAdmin
Root: HKLM; Subkey: "Software\@Producer@\R\@RVER@"; ValueType: string; ValueName: "InstallPath"; ValueData: "{app}"; Tasks: recordversion; Check: IsAdmin

Root: HKLM; Subkey: "Software\@Producer@\@RK@"; Flags: uninsdeletekeyifempty; Tasks: recordversion; Check: IsAdmin
Root: HKLM; Subkey: "Software\@Producer@\@RK@"; Flags: uninsdeletevalue; ValueType: string; ValueName: "InstallPath"; ValueData: "{app}"; Tasks: recordversion; Check: IsAdmin
Root: HKLM; Subkey: "Software\@Producer@\@RK@"; Flags: uninsdeletevalue; ValueType: string; ValueName: "Current Version"; ValueData: "@RVER@"; Tasks: recordversion; Check: IsAdmin
Root: HKLM; Subkey: "Software\@Producer@\@RK@\@RVER@"; Flags: uninsdeletekey; Tasks: recordversion; Check: IsAdmin
Root: HKLM; Subkey: "Software\@Producer@\@RK@\@RVER@"; ValueType: string; ValueName: "InstallPath"; ValueData: "{app}"; Tasks: recordversion; Check: IsAdmin

Root: HKCU; Subkey: "Software\@Producer@"; Flags: uninsdeletekeyifempty; Tasks: recordversion; Check: NonAdmin
Root: HKCU; Subkey: "Software\@Producer@\R"; Flags: uninsdeletekeyifempty; Tasks: recordversion; Check: NonAdmin
Root: HKCU; Subkey: "Software\@Producer@\R"; Flags: uninsdeletevalue; ValueType: string; ValueName: "InstallPath"; ValueData: "{app}"; Tasks: recordversion; Check: NonAdmin
Root: HKCU; Subkey: "Software\@Producer@\R"; Flags: uninsdeletevalue; ValueType: string; ValueName: "Current Version"; ValueData: "@RVER@"; Tasks: recordversion; Check: NonAdmin
Root: HKCU; Subkey: "Software\@Producer@\R\@RVER@"; Flags: uninsdeletekey; Tasks: recordversion; Check: NonAdmin
Root: HKCU; Subkey: "Software\@Producer@\R\@RVER@"; ValueType: string; ValueName: "InstallPath"; ValueData: "{app}"; Tasks: recordversion; Check: NonAdmin

Root: HKCU; Subkey: "Software\@Producer@\@RK@"; Flags: uninsdeletekeyifempty; Tasks: recordversion; Check: NonAdmin
Root: HKCU; Subkey: "Software\@Producer@\@RK@"; Flags: uninsdeletevalue; ValueType: string; ValueName: "InstallPath"; ValueData: "{app}"; Tasks: recordversion; Check: NonAdmin
Root: HKCU; Subkey: "Software\@Producer@\@RK@"; Flags: uninsdeletevalue; ValueType: string; ValueName: "Current Version"; ValueData: "@RVER@"; Tasks: recordversion; Check: NonAdmin
Root: HKCU; Subkey: "Software\@Producer@\@RK@\@RVER@"; Flags: uninsdeletekey; Tasks: recordversion; Check: NonAdmin
Root: HKCU; Subkey: "Software\@Producer@\@RK@\@RVER@"; ValueType: string; ValueName: "InstallPath"; ValueData: "{app}"; Tasks: recordversion; Check: NonAdmin

Root: HKCR; Subkey: ".RData"; ValueType: string; ValueName: ""; ValueData: "RWorkspace"; Flags: uninsdeletevalue; Tasks: associate; Check: IsAdmin
Root: HKCR; Subkey: "RWorkspace"; ValueType: string; ValueName: ""; ValueData: "R Workspace"; Flags: uninsdeletekey; Tasks: associate; Check: IsAdmin
Root: HKCR; Subkey: "RWorkspace\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\@bindir@\RGui.exe,0"; Tasks: associate; Check: IsAdmin 
Root: HKCR; Subkey: "RWorkspace\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\@bindir@\RGui.exe"" ""%1"""; Tasks: associate; Check: IsAdmin
