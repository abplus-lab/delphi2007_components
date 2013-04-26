unit ABIconUpDown;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ExtCtrls, SHLObj, ActiveX;

type
  TABIconUpDown = class(TCustomUpDown)
  private
    { Private éŒ¾ }
    FIconFile: string;
    FIconList: TImageList;
    FRelImage: TImage;
    FSeeAlsoLink:  boolean;
    FSeeAlsoAssoc: boolean;
    function getCount: SmallInt;
    function SeeLink(var filename: string): integer;
    function LoadAssocIcon(var filename: string): integer;
  protected
    { Protected éŒ¾ }
    procedure Click(Button: TUDBtnType); override;
  public
    { Public éŒ¾ }
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure LoadIcon(filename: string);
    procedure GetIcon(icon: TIcon);
    property IconFile: string read FIconFile;
    property IconCount: SmallInt read getCount;
  published
    { Published éŒ¾ }
    property Position;
    //property Min;
    //property Max;
    property AlignButton;
    property Associate;
    property ArrowKeys;
    property Enabled;
    property Hint;
    property Increment;
    property Orientation;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Thousands;
    property Visible;
    property Wrap;
    property OnChanging;
    property OnClick;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property RelImage: TImage read FRelImage write FRelImage;
    property SeeAlsoLink: boolean read FSeeAlsoLink write FSeeAlsoLink;
    property SeeAlsoAssoc: boolean read FSeeAlsoAssoc write FSeeAlsoAssoc;
  end;

procedure Register;

implementation

uses
  ShellAPI;

constructor TABIconUpDown.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIconFile := '';
  FIconList := nil;
  FRelImage := nil;
  Min := 0;
  Max := 0;
  CoInitialize(nil);
end;

destructor  TABIconUpDown.Destroy;
begin
  CoUninitialize;
  FIconList.Free;
  Inherited Destroy;
end;

function TABIconUpDown.getCount: SmallInt;
begin
  if Assigned(FIconList)
  then Result := FIconList.Count
  else Result := 0;
end;

procedure TABIconUpDown.Click(Button: TUDBtnType);
begin
  if Assigned(FRelImage) and Assigned(FIconList) then
  begin
    if Position >= 0 then
    begin
      FIconList.GetIcon(Position, FRelImage.Picture.Icon);
      FRelImage.Refresh;
    end;
  end;
  inherited Click(Button);
end;

function TABIconUpDown.SeeLink(var filename: string): integer;
const
  bufSize = 1024;
var
  shellLink: IShellLink;
  persistFile: IPersistFile;
  wideName: POleStr;
  s: PChar;
  wfd: TWIN32FINDDATA;
  hr: HResult;
  i: integer;
begin
  Result := 0;
  if FSeeAlsoLink = false then exit;
  if CompareText(ExtractFileExt(fileName), '.lnk') <> 0 then exit;

  GetMem(wideName, SizeOf(WideChar) * bufSize);
  StringToWideChar(FileName, wideName, bufSize);

  s := StrAlloc(bufSize);

  try
    hr := CoCreateInstance(
      CLSID_ShellLink,
      nil,
      CLSCTX_INPROC_SERVER,
      IShellLink,
      shellLink);
    if Failed(hr)
    then raise Exception.Create('CreateInstance Error');

    hr := ShellLink.QueryInterFace(
      IPersistFile,
      persistFile);
    if Failed(hr)
    then raise Exception.Create('PersistFile Error1');

    hr := PersistFile.Load(
      wideName,
      STGM_READ);
    if Failed(hr)
    then raise Exception.Create('PersistFile Error2');

    hr := shellLink.Resolve(
      Handle,
      SLR_ANY_MATCH);
    if Failed(hr)
    then raise Exception.Create('Resolve Error');

    hr := ShellLink.GetIconLocation(s, bufSize, i);
    if succeeded(hr)
    then Result := ExtractIcon(Handle, s, UINT(-1));

    if Result > 0 then
    begin
      filename := StrPas(s);
    end
    else
    begin
      hr := ShellLink.GetPath(s, bufSize, wfd, 0);
      if succeeded(hr)
      then Result := ExtractIcon(Handle, s, UINT(-1));
      filename := StrPas(s);
    end;

  except
  end;

  StrDispose(s);
  FreeMem(wideName);

end;

function TABIconUpDown.LoadAssocIcon(var filename: string): integer;
var
  i: word;
  icon: TIcon;
  path: array [0..MAX_PATH] of char;
begin
  Result := 0;
  if FSeeAlsoAssoc = false then exit;
  StrCopy(path, PChar(filename));
  icon := TIcon.Create;
  icon.Handle := ExtractAssociatedIcon(Application.Handle, path, i);
  if icon.Handle <> 0 then
  begin
    FIconList.AddIcon(icon);
    filename := StrPas(path);
    Result := 1;
  end;
  icon.Free;
end;

procedure TABIconUpDown.LoadIcon(filename: string);
var
  n: integer;
  i: integer;
  icon: TIcon;
begin
  n := ExtractIcon(Application.Handle, PChar(filename), UINT(-1));
  if n = 0 then n := SeeLink(filename);
  if not Assigned(FIconList) then
  begin
    try
      FIconList := TImageList.Create(Self);
      FIconList.Height := 32;
      FIconList.Width  := 32;
    except
      FIconList.Free;
      FIconList := nil;
    end;
  end;
  if Assigned(FIconList) then
  begin
    FIconList.Clear;
    for i := 0 to n - 1 do
    begin
      icon := TIcon.Create;
      icon.Handle := ExtractIcon(Application.Handle, PChar(filename), i);
      FIconList.AddIcon(icon);
      icon.Free;
    end;
    if n = 0 then n := LoadAssocIcon(filename);
    Position := 0;
    if Assigned(FRelImage) then
    begin
      FIconList.GetIcon(Position, FRelImage.Picture.Icon);
      FRelImage.Refresh;
    end;
    Max := n - 1;
  end;
  FIconFile := filename;
  Refresh;
end;

procedure TABIconUpDown.GetIcon(icon: TIcon);
begin
  if (0 <= Position) and (Position < FIconList.Count) then
    FIconList.GetIcon(Position, icon);
end;

procedure Register;
begin
  RegisterComponents('ABplus', [TABIconUpDown]);
end;

end.
