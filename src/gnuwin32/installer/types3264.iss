
[Types]
Name: "user"; Description: {cm:user}; Check: Is64BitInstallMode
Name: "user32"; Description: {cm:user32}
Name: "user64"; Description: {cm:user64}; Check: Is64BitInstallMode
Name: "custom"; Description: {cm:custom}; Flags: iscustom

[Components]
Name: "main"; Description: "Core Files"; Types: user user32 user64  custom
Name: "i386"; Description: "32-bit Files"; Types: user user32 custom
Name: "x64"; Description: "64-bit Files"; Types: user user64 custom
