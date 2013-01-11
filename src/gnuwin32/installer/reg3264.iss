[Icons]
Name: "{group}\R i386 @RVER@"; Filename: "{app}\bin\i386\Rgui.exe"; WorkingDir: "{userdocs}"; Parameters: {code:CmdParms}; Check: isComponentSelected('i386')
Name: "{group}\R x64 @RVER@"; Filename: "{app}\bin\x64\Rgui.exe"; WorkingDir: "{userdocs}"; Parameters: {code:CmdParms}; Check: isComponentSelected('x64') and Is64BitInstallMode

Name: "{commondesktop}\R i386 @RVER@"; Filename: "{app}\bin\i386\Rgui.exe"; MinVersion: 0,5.0; Tasks: desktopicon; WorkingDir: "{userdocs}"; Parameters: {code:CmdParms}; Check: isComponentSelected('i386')
Name: "{commondesktop}\R x64 @RVER@"; Filename: "{app}\bin\x64\Rgui.exe"; MinVersion: 0,5.0; Tasks: desktopicon; WorkingDir: "{userdocs}"; Parameters: {code:CmdParms}; Check: isComponentSelected('x64') and Is64BitInstallMode

Name: "{userappdata}\Microsoft\Internet Explorer\Quick Launch\R i386 @RVER@"; Filename: "{app}\bin\i386\Rgui.exe"; Tasks: quicklaunchicon; WorkingDir: "{userdocs}"; Parameters: {code:CmdParms}; Check: isComponentSelected('i386')
Name: "{userappdata}\Microsoft\Internet Explorer\Quick Launch\R x64 @RVER@"; Filename: "{app}\bin\x64\Rgui.exe"; Tasks: quicklaunchicon; WorkingDir: "{userdocs}"; Parameters: {code:CmdParms}; Check: isComponentSelected('x64') and Is64BitInstallMode 

[Registry] 
Root: HKLM; Subkey: "Software\@Producer@"; Flags: uninsdeletekeyifempty; Tasks: recordversion; Check: IsAdmin and isComponentSelected('x64') and Is64BitInstallMode
Root: HKLM; Subkey: "Software\@Producer@\R"; Flags: uninsdeletekeyifempty; Tasks: recordversion; Check: IsAdmin and isComponentSelected('x64') and Is64BitInstallMode
Root: HKLM; Subkey: "Software\@Producer@\R"; Flags: uninsdeletevalue; ValueType: string; ValueName: "InstallPath"; ValueData: "{app}"; Tasks: recordversion; Check: IsAdmin and isComponentSelected('x64') and Is64BitInstallMode
Root: HKLM; Subkey: "Software\@Producer@\R"; Flags: uninsdeletevalue; ValueType: string; ValueName: "Current Version"; ValueData: "@RVER@"; Tasks: recordversion; Check: IsAdmin and isComponentSelected('x64') and Is64BitInstallMode
Root: HKLM; Subkey: "Software\@Producer@\R\@RVER@"; Flags: uninsdeletekey; Tasks: recordversion; Check: IsAdmin and isComponentSelected('x64') and Is64BitInstallMode
Root: HKLM; Subkey: "Software\@Producer@\R\@RVER@"; ValueType: string; ValueName: "InstallPath"; ValueData: "{app}"; Tasks: recordversion; Check: IsAdmin and isComponentSelected('x64') and Is64BitInstallMode

Root: HKLM; Subkey: "Software\@Producer@\R64"; Flags: uninsdeletekeyifempty; Tasks: recordversion; Check: IsAdmin and isComponentSelected('x64') and Is64BitInstallMode
Root: HKLM; Subkey: "Software\@Producer@\R64"; Flags: uninsdeletevalue; ValueType: string; ValueName: "InstallPath"; ValueData: "{app}"; Tasks: recordversion; Check: IsAdmin and isComponentSelected('x64') and Is64BitInstallMode
Root: HKLM; Subkey: "Software\@Producer@\R64"; Flags: uninsdeletevalue; ValueType: string; ValueName: "Current Version"; ValueData: "@RVER@"; Tasks: recordversion; Check: IsAdmin and isComponentSelected('x64') and Is64BitInstallMode
Root: HKLM; Subkey: "Software\@Producer@\R64\@RVER@"; Flags: uninsdeletekey; Tasks: recordversion; Check: IsAdmin and isComponentSelected('x64') and Is64BitInstallMode 
Root: HKLM; Subkey: "Software\@Producer@\R64\@RVER@"; ValueType: string; ValueName: "InstallPath"; ValueData: "{app}"; Tasks: recordversion; Check: IsAdmin and isComponentSelected('x64') and Is64BitInstallMode

Root: HKLM32; Subkey: "Software\@Producer@"; Flags: uninsdeletekeyifempty; Tasks: recordversion; Check: IsAdmin and isComponentSelected('i386')
Root: HKLM32; Subkey: "Software\@Producer@\R"; Flags: uninsdeletekeyifempty; Tasks: recordversion; Check: IsAdmin and isComponentSelected('i386')
Root: HKLM32; Subkey: "Software\@Producer@\R"; Flags: uninsdeletevalue; ValueType: string; ValueName: "InstallPath"; ValueData: "{app}"; Tasks: recordversion; Check: IsAdmin and isComponentSelected('i386')
Root: HKLM32; Subkey: "Software\@Producer@\R"; Flags: uninsdeletevalue; ValueType: string; ValueName: "Current Version"; ValueData: "@RVER@"; Tasks: recordversion; Check: IsAdmin and isComponentSelected('i386')
Root: HKLM32; Subkey: "Software\@Producer@\R\@RVER@"; Flags: uninsdeletekey; Tasks: recordversion; Check: IsAdmin and isComponentSelected('i386')
Root: HKLM32; Subkey: "Software\@Producer@\R\@RVER@"; ValueType: string; ValueName: "InstallPath"; ValueData: "{app}"; Tasks: recordversion; Check: IsAdmin and isComponentSelected('i386')

Root: HKLM32; Subkey: "Software\@Producer@\R32"; Flags: uninsdeletekeyifempty; Tasks: recordversion; Check: IsAdmin and isComponentSelected('i386')
Root: HKLM32; Subkey: "Software\@Producer@\R32"; Flags: uninsdeletevalue; ValueType: string; ValueName: "InstallPath"; ValueData: "{app}"; Tasks: recordversion; Check: IsAdmin and isComponentSelected('i386')
Root: HKLM32; Subkey: "Software\@Producer@\R32"; Flags: uninsdeletevalue; ValueType: string; ValueName: "Current Version"; ValueData: "@RVER@"; Tasks: recordversion; Check: IsAdmin and isComponentSelected('i386')
Root: HKLM32; Subkey: "Software\@Producer@\R32\@RVER@"; Flags: uninsdeletekey; Tasks: recordversion; Check: IsAdmin and isComponentSelected('i386')
Root: HKLM32; Subkey: "Software\@Producer@\R32\@RVER@"; ValueType: string; ValueName: "InstallPath"; ValueData: "{app}"; Tasks: recordversion; Check: IsAdmin and isComponentSelected('i386')

Root: HKCU; Subkey: "Software\@Producer@"; Flags: uninsdeletekeyifempty; Tasks: recordversion; Check: NonAdmin
Root: HKCU; Subkey: "Software\@Producer@\R"; Flags: uninsdeletekeyifempty; Tasks: recordversion; Check: NonAdmin
Root: HKCU; Subkey: "Software\@Producer@\R"; Flags: uninsdeletevalue; ValueType: string; ValueName: "InstallPath"; ValueData: "{app}"; Tasks: recordversion; Check: NonAdmin
Root: HKCU; Subkey: "Software\@Producer@\R"; Flags: uninsdeletevalue; ValueType: string; ValueName: "Current Version"; ValueData: "@RVER@"; Tasks: recordversion; Check: NonAdmin
Root: HKCU; Subkey: "Software\@Producer@\R\@RVER@"; Flags: uninsdeletekey; Tasks: recordversion; Check: NonAdmin
Root: HKCU; Subkey: "Software\@Producer@\R\@RVER@"; ValueType: string; ValueName: "InstallPath"; ValueData: "{app}"; Tasks: recordversion; Check: NonAdmin


Root: HKCU; Subkey: "Software\@Producer@\R32"; Flags: uninsdeletekeyifempty; Tasks: recordversion; Check: NonAdmin and isComponentSelected('i386')
Root: HKCU; Subkey: "Software\@Producer@\R32"; Flags: uninsdeletevalue; ValueType: string; ValueName: "InstallPath"; ValueData: "{app}"; Tasks: recordversion; Check: NonAdmin and isComponentSelected('i386')
Root: HKCU; Subkey: "Software\@Producer@\R32"; Flags: uninsdeletevalue; ValueType: string; ValueName: "Current Version"; ValueData: "@RVER@"; Tasks: recordversion; Check: NonAdmin and isComponentSelected('i386')
Root: HKCU; Subkey: "Software\@Producer@\R32\@RVER@"; Flags: uninsdeletekey; Tasks: recordversion; Check: NonAdmin and isComponentSelected('i386')
Root: HKCU; Subkey: "Software\@Producer@\R32\@RVER@"; ValueType: string; ValueName: "InstallPath"; ValueData: "{app}"; Tasks: recordversion; Check: NonAdmin and isComponentSelected('i386')

Root: HKCU; Subkey: "Software\@Producer@\R64"; Flags: uninsdeletekeyifempty; Tasks: recordversion; Check: NonAdmin and isComponentSelected('x64') and Is64BitInstallMode
Root: HKCU; Subkey: "Software\@Producer@\R64"; Flags: uninsdeletevalue; ValueType: string; ValueName: "InstallPath"; ValueData: "{app}"; Tasks: recordversion; Check: NonAdmin and isComponentSelected('x64') and Is64BitInstallMode
Root: HKCU; Subkey: "Software\@Producer@\R64"; Flags: uninsdeletevalue; ValueType: string; ValueName: "Current Version"; ValueData: "@RVER@"; Tasks: recordversion; Check: NonAdmin and isComponentSelected('x64') and Is64BitInstallMode
Root: HKCU; Subkey: "Software\@Producer@\R64\@RVER@"; Flags: uninsdeletekey; Tasks: recordversion; Check: NonAdmin and isComponentSelected('x64') and Is64BitInstallMode
Root: HKCU; Subkey: "Software\@Producer@\R64\@RVER@"; ValueType: string; ValueName: "InstallPath"; ValueData: "{app}"; Tasks: recordversion; Check: NonAdmin and isComponentSelected('x64') and Is64BitInstallMode

Root: HKCR; Subkey: ".RData"; ValueType: string; ValueName: ""; ValueData: "RWorkspace"; Flags: uninsdeletevalue; Tasks: associate; Check: IsAdmin
Root: HKCR; Subkey: "RWorkspace"; ValueType: string; ValueName: ""; ValueData: "R Workspace"; Flags: uninsdeletekey; Tasks: associate; Check: IsAdmin
Root: HKCR; Subkey: "RWorkspace\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\bin\i386\RGui.exe,0"; Tasks: associate; Check: IsAdmin and isComponentSelected('i386')
Root: HKCR; Subkey: "RWorkspace\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\bin\i386\RGui.exe"" ""%1"""; Tasks: associate; Check: IsAdmin and isComponentSelected('i386')
Root: HKCR; Subkey: "RWorkspace\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\bin\x64\RGui.exe,0"; Tasks: associate; Check: IsAdmin and isComponentSelected('x64') and Is64BitInstallMode
Root: HKCR; Subkey: "RWorkspace\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\bin\x64\RGui.exe"" ""%1"""; Tasks: associate; Check: IsAdmin and isComponentSelected('x64') and Is64BitInstallMode
