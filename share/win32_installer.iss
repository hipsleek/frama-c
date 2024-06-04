; Script generated by the Inno Setup Script Wizard.
; SEE THE DOCUMENTATION FOR DETAILS ON CREATING INNO SETUP SCRIPT FILES!

#define MyAppName "Frama-C"
#define MyAppVerName "Frama-C Boron 20100401"
#define MyAppPublisher "CEA LIST"
#define MyAppURL "http://frama-c.com"
#define MyAppExeName "frama-c-gui.exe"

[Setup]
; NOTE: The value of AppId uniquely identifies this application.
; Do not use the same AppId value in installers for other applications.
; (To generate a new GUID, click Tools | Generate GUID inside the IDE.)
AppId={{0726456B-CCA1-4836-BEBB-4C5D0DD832E2}
AppName={#MyAppName}
AppVerName={#MyAppVerName}
AppPublisher={#MyAppPublisher}
AppPublisherURL={#MyAppURL}
AppSupportURL={#MyAppURL}
AppUpdatesURL={#MyAppURL}
DefaultDirName=C:\{#MyAppName}
DefaultGroupName={#MyAppName}
AllowNoIcons=yes
OutputBaseFilename=frama-c-Boron-20100401
SetupIconFile=C:\Frama-C\share\frama-c\frama-c.ico
Compression=lzma
SolidCompression=yes
ChangesEnvironment=yes
InfoAfterFile=win32_manual_installation_step.txt
[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked
Name: "quicklaunchicon"; Description: "{cm:CreateQuickLaunchIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked

[Files]
Source: "C:\Frama-C\bin\frama-c-gui.exe"; DestDir: "{app}\bin"; Flags: ignoreversion
Source: "C:\Frama-C\*"; DestDir: "{app}"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "C:\DEV_ROOT\GTK\*"; DestDir: "{app}"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "C:\DEV_ROOT\ocamlmgw\*"; DestDir: "{app}"; Flags: ignoreversion recursesubdirs createallsubdirs
; NOTE: Don't use "Flags: ignoreversion" on any shared system files

[Icons]
Name: "{group}\{#MyAppName}"; Filename: "{app}\bin\{#MyAppExeName}"; WorkingDir: "{app}\bin"; IconFileName:"{app}\share\frama-c\frama-c.ico"
Name: "{group}\manuals"; Filename: "{app}\share\frama-c\manuals"; WorkingDir: "{app}\bin"; IconFileName:"{app}\share\frama-c\frama-c.ico"
Name: "{commondesktop}\{#MyAppName}"; Filename: "{app}\bin\{#MyAppExeName}"; Tasks: desktopicon; WorkingDir: "{app}\bin"; IconFileName:"{app}\share\frama-c\frama-c.ico"
Name: "{userappdata}\Microsoft\Internet Explorer\Quick Launch\{#MyAppName}"; Filename: "{app}\bin\{#MyAppExeName}"; Tasks: quicklaunchicon; WorkingDir: "{app}\bin"; IconFileName:"{app}\share\frama-c\frama-c.ico"

[Registry]
Root: HKLM; Subkey: "SYSTEM\CurrentControlSet\Control\Session Manager\Environment"; ValueType: string; ValueName: "FRAMAC_SHARE"; ValueData: "{app}\share\frama-c"; Flags: uninsdeletevalue
Root: HKLM; Subkey: "SYSTEM\CurrentControlSet\Control\Session Manager\Environment"; ValueType: string; ValueName: "FRAMAC_LIB"; ValueData: "{app}\lib\frama-c"; Flags: uninsdeletevalue
Root: HKLM; Subkey: "SYSTEM\CurrentControlSet\Control\Session Manager\Environment"; ValueType: string; ValueName: "FRAMAC_PLUGIN"; ValueData: "{app}\lib\frama-c\plugins"; Flags: uninsdeletevalue
Root: HKLM; Subkey: "SYSTEM\CurrentControlSet\Control\Session Manager\Environment"; ValueType: string; ValueName: "OCAMLLIB"; ValueData: "{app}\lib"; Flags: uninsdeletevalue
Root: HKLM; Subkey: "SYSTEM\CurrentControlSet\Control\Session Manager\Environment"; ValueType: string; ValueName: "WHYLIB"; ValueData: "'{app}\lib\why'"; Flags: uninsdeletevalue
Root: HKLM; Subkey: "SYSTEM\CurrentControlSet\Control\Session Manager\Environment"; ValueType: string; ValueName: "ERGOLIB"; ValueData: "{app}\lib\alt-ergo"; Flags: uninsdeletevalue



