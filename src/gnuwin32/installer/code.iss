
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
    CustomMessage('adminprivilegesrequired'), CustomMessage('adminexplanation'));
  
  SelectOptionsPage := CreateInputOptionPage(wpSelectComponents,
    CustomMessage('startupt'), CustomMessage('startupq'),
    CustomMessage('startupi'), True, False);
  SelectOptionsPage.Add(CustomMessage('startup0'));
  SelectOptionsPage.Add(CustomMessage('startup1'));
  SelectOptionsPage.SelectedValueIndex := 1;
  
  MDISDIPage := CreateInputOptionPage(SelectOptionsPage.ID,
    CustomMessage('MDIt'), CustomMessage('MDIq'),
    CustomMessage('MDIi'), True, False);
  MDISDIPage.Add(CustomMessage('MDI0'));
  MDISDIPage.Add(CustomMessage('MDI1'));
  
  HelpStylePage := CreateInputOptionPage(MDISDIPage.ID,
    CustomMessage('HelpStylet'), CustomMessage('HelpStyleq'),
    CustomMessage('HelpStylei'), True, False);
  HelpStylePage.Add(CustomMessage('HelpStyle0'));
  HelpStylePage.Add(CustomMessage('HelpStyle1'));
   
  InternetPage := CreateInputOptionPage(HelpStylePage.ID,
    CustomMessage('Internett'), CustomMessage('Internetq'),
    CustomMessage('Interneti'), True, False);
  InternetPage.Add(CustomMessage('Internet0'));
  InternetPage.Add(CustomMessage('Internet1'));    

  INIFilename := ExpandConstant('{param:LOADINF}');
  if INIFilename <> '' then INIFilename := ExpandFilename(INIFilename);
  
  { From highest to lowest, priority is:
    LOADINF value
    PreviousData value
    Default from build }
  
  option := GetPreviousData('MDISDI', '');
  if INIFilename <> '' then
    option := GetIniString('R', 'MDISDI', option, INIFilename);
  case option of
    'MDI': index := 0;
    'SDI': index := 1;
  else
    index := @MDISDI@;
  end;  
  MDISDIPage.SelectedValueIndex := index;

  option := GetPreviousData('HelpStyle', '');
  if INIFilename <> '' then
    option := GetIniString('R', 'HelpStyle', option, INIFilename);  
  case option of
    'plain': index := 0;
    'CHM':   index := 1;
    'HTML':  index := 1;
  else
    index := @HelpStyle@;
  end;
  HelpStylePage.SelectedValueIndex := index;
  
  option := GetPreviousData('Internet', '');
  if INIFilename <> '' then
    option := GetIniString('R', 'Internet', option, INIFilename);
  case option of
    'Standard':  index := 0;
    'Internet2': index := 1;
  else
    index := @Internet@;
  end;
  InternetPage.SelectedValueIndex := index;
    
  { Get the save name now, because the current dir might change }
  INIFilename := ExpandConstant('{param:SAVEINF}');
  if INIFilename <> '' then INIFilename := ExpandFilename(INIFilename);    
end;

procedure RegisterPreviousData(PreviousDataKey: Integer);
var
  MDISDI: String;
  HelpStyle: String;
  Internet: String;
begin

  
  { Store the settings so we can restore them next time }
  case MDISDIPage.SelectedValueIndex of
    0: MDISDI := 'MDI';
    1: MDISDI := 'SDI';
  end;
  SetPreviousData(PreviousDataKey, 'MDISDI', MDISDI);
  if INIFilename <> '' then
    SetIniString('R', 'MDISDI', MDISDI, INIFilename);
    
  case HelpStylePage.SelectedValueIndex of
    0: HelpStyle := 'plain';
    1: HelpStyle := 'HTML';
  end;
  SetPreviousData(PreviousDataKey, 'HelpStyle', HelpStyle);  
  if INIFilename <> '' then
    SetIniString('R', 'HelpStyle', HelpStyle, INIFilename);
  
  case InternetPage.SelectedValueIndex of
    0: Internet := 'Standard';
    1: Internet := 'Internet2';
  end;
  SetPreviousData(PreviousDataKey, 'Internet', Internet);
  if INIFilename <> '' then
    SetIniString('R', 'Internet', Internet, INIFilename);
end;

function SetCommentMarker(var lines: TArrayOfString; option: String; active: boolean) : boolean;
var
  i : integer;
  old : string;
begin
  Result := false;
  for i := 0 to pred(GetArrayLength(lines)) do
    if pos(option, lines[i]) > 0 then 
    begin
      old := lines[i];
      if active then
        lines[i][1] := ' '
      else
        lines[i][1] := '#';
      if old <> lines[i] then
        Result := true;  
      exit;
    end;
end;
  
procedure EditOptions();
var
  lines : TArrayOfString;
  filename : String;
  changed : boolean;
begin
  changed := false;
  filename := ExpandConstant(CurrentFilename);
  LoadStringsFromFile(filename, lines);
  
  changed := changed or SetCommentMarker(lines, 'MDI = yes', MDISDIPage.SelectedValueIndex = 0);
  changed := changed or SetCommentMarker(lines, 'MDI = no', MDISDIPage.SelectedValueIndex = 1);
  
  changed := changed or SetCommentMarker(lines, 'options(help_type="text"', HelpStylePage.SelectedValueIndex = 0);
  changed := changed or SetCommentMarker(lines, 'options(help_type="html"', HelpStylePage.SelectedValueIndex = 1);
  
  if changed then
    SaveStringsToFile(filename, lines, False);
end;

function CmdParms(Param:String): String;
begin
  Result := '';
  if InternetPage.SelectedValueIndex = 1 then
    Result := '--internet2';
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
  Result := ExpandConstant('{pf}');
  if (not IsAdmin) then 
  begin
    try
      Result := ExpandConstant('{userdocs}');
    except
    // Do nothing, user doesn't have a My Documents folder
    end;
  end;
end;
