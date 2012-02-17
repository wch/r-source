
[Types]
Name: "user"; Description: "User installation"; Check: Is64BitInstallMode
Name: "user32"; Description: "32-bit user installation"
Name: "user64"; Description: "64-bit user installation"; Check: Is64BitInstallMode
Name: "custom"; Description: "Custom installation"; Flags: iscustom

[Components]
Name: "main"; Description: "Core Files"; Types: user user32 user64  custom
Name: "i386"; Description: "32-bit Files"; Types: user user32 custom
Name: "x64"; Description: "64-bit Files"; Types: user user64 custom
