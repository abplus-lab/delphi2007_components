//  Copyright (C) 2000-2005 ABplus kazhida.
//  All rights reserved.
//  $Id: ABUtils.pas 1284 2009-09-04 01:41:01Z kazhida $
//  $Author: kazhida $
//
unit ABUtils;

interface

{:Note::

  ABplus特製ユーティリティ

  TABFileSearchクラス
    --- ファイル抽出用のクラス
        FindFirst, FindNext, FindCloseを使って、ファイルのリストを作ります

  ABNextToken関数
    --- 文字列を区切る関数

  ABBrowseFolder関数
    --- フォルダ選択ダイアログを表示して、フォルダ選択をする関数

  ABFileVersion関数
    --- バージョン番号を返す関数


::Note:}

uses
  Classes, Contnrs;

type

  TAbFileInfo = class(TObject)
    FTime: Integer;
    FSize: Int64;
    FAttr: Integer;
    FName: String;
  public
    constructor Create(time: Integer; size: Int64;
                       attr: Integer; const name: String);
    property  Time: Integer read FTime;
    property  Size: Int64   read FSize;
    property  Attr: Integer read FAttr;
    property  Name: String  read FName;
  end;

  TAbFileSearch = class(TObject)
  private
    FFiles: TObjectList;
    function  GetCount: Integer;
    function  GetFileInfo(idx: Integer): TABFileInfo;
    function  GetFileName(idx: Integer): String;
    function  GetFileAttr(idx: Integer): Integer;
    function  GetFileSize(idx: Integer): Int64;
    function  GetFileTime(idx: Integer): TDateTime;
    procedure Search(path: String; attr: Integer; recursive: Boolean);
    class function IsBitOn(attr, bits: Integer): Boolean;
  public
    constructor Create(const path: String;
                       recursive: Boolean = True;
                       attr: Integer = 0);
    destructor  Destroy;  override;
    class function  IsReadOnly(attr: Integer): Boolean;
    class function  IsHidden(attr: Integer): Boolean;
    class function  IsSysFile(attr: Integer): Boolean;
{$IFDEF SUPPORT_VOLUME_ID}
    class function  IsVolumeID(attr: Integer): Boolean;
{$ENDIF}
    class function  IsDirectory(attr: Integer): Boolean;
    class function  IsArchive(attr: Integer): Boolean;
    function  IsAttrReadOnly(idx: Integer): Boolean;
    function  IsAttrHidden(idx: Integer): Boolean;
    function  IsAttrSysFile(idx: Integer): Boolean;
{$IFDEF SUPPORT_VOLUME_ID}
    function  IsAttrVolumeID(idx: Integer): Boolean;
{$ENDIF}
    function  IsAttrDirectory(idx: Integer): Boolean;
    function  IsAttrArchive(idx: Integer): Boolean;
    property  Count: Integer read GetCount;
    property  FileNames[idx: Integer]: String     read GetFileName;
    property  FileAttrs[idx: Integer]: Integer    read GetFileAttr;
    property  FileSizes[idx: Integer]: Int64      read GetFileSize;
    property  FileTimes[idx: Integer]: TDateTime  read GetFileTime;
  end;

function  AbNextToken(var str: String;
                     const dlm: String = #9;
                     triming: Boolean = True): String;

function AbBrowseFolderDialog(var path: String): Boolean;

function AbAssignExtToApp(const path, ext, name: String;
                          const desc: String = '';
                          iconIdx: Integer = 0): Boolean;

function AbAssignedApp(const ext: String): String;

type
  TABVersion = (verMajor, verMinor, verRelease, verBuild);

function ABFileVersion(const filename: String; ver: TABVersion): Integer;

////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////
implementation

uses
  SysUtils, Windows, ShlObj, ActiveX, Registry;

resourcestring
  MSG_SELECT_FOLDER = 'フォルダを選択して下さい。';

////////////////////////////////////////////////////////////////////
{ TABFileInfo }

constructor TABFileInfo.Create(time: Integer; size: Int64;
                       attr: Integer; const name: String);
begin
  inherited Create;
  FTime := time;
  FSize := size;
  FAttr := attr;
  FName := name;
end;

////////////////////////////////////////////////////////////////////
{ TABFileSearch }

constructor TABFileSearch.Create(const path: String;
  recursive: Boolean;
  attr: Integer);
begin
  inherited Create;
  FFiles := TObjectList.Create;

  if recursive then attr := attr or faDirectory;
  Search(path, attr, recursive);
end;

destructor  TABFileSearch.Destroy;
begin
  FFiles.Free;
  inherited Destroy;
end;

procedure TABFileSearch.Search(path: String; attr: Integer;
  recursive: Boolean);
var
  sr: TSearchRec;
  fn: String;
  patn: String;
begin
  if (Pos('?', path) = 0) and (Pos('*', path) = 0) then
    patn := path + '\*.*'
  else
  begin
    patn := path;
    path := ExtractFileDir(path);
  end;
  if FindFirst(patn, attr, sr) = 0 then
  begin
    try
      repeat
        if (sr.Name <> '.') and (sr.Name <> '..') then
        begin
          fn := path + '\' + sr.Name;
          FFiles.Add(TABFileInfo.Create(sr.Time, sr.Size, sr.Attr, fn));
          if recursive and ((sr.Attr and faDirectory) = faDirectory) then
            Search(fn, attr, recursive);  //再帰
        end;
      until FindNext(sr) <> 0;
    finally
      SysUtils.FindClose(sr); //名前がバッティングした。
    end;
  end;
end;

function  TABFileSearch.GetCount: Integer;
begin
  Result := FFiles.Count;
end;

function  TABFileSearch.GetFileInfo(idx: Integer): TABFileInfo;
begin
  Result := FFiles.Items[idx] as TABFileInfo;
end;

function  TABFileSearch.GetFileName(idx: Integer): String;
begin
  Result := GetFileInfo(idx).Name;
end;

function  TABFileSearch.GetFileAttr(idx: Integer): Integer;
begin
  Result := GetFileInfo(idx).Attr;
end;

function  TABFileSearch.GetFileSize(idx: Integer): Int64;
begin
  Result := GetFileInfo(idx).Size;
end;

function  TABFileSearch.GetFileTime(idx: Integer): TDateTime;
begin
  Result := FileDateToDateTime(GetFileInfo(idx).Time);
end;

function TABFileSearch.IsAttrReadOnly(idx: Integer): Boolean;
begin
  Result := IsBitOn(GetFileInfo(idx).Attr, faReadOnly);
end;

function TABFileSearch.IsAttrHidden(idx: Integer): Boolean;
begin
  Result := IsBitOn(GetFileInfo(idx).Attr, faHidden);
end;

function TABFileSearch.IsAttrSysFile(idx: Integer): Boolean;
begin
  Result := IsBitOn(GetFileInfo(idx).Attr, faSysFile);
end;

{$IFDEF SUPPORT_VOLUME_ID}
function TABFileSearch.IsAttrVolumeID(idx: Integer): Boolean;
begin
  Result := IsBitOn(GetFileInfo(idx).Attr, faVolumeID);
end;
{$ENDIF}

function TABFileSearch.IsAttrDirectory(idx: Integer): Boolean;
begin
  Result := IsBitOn(GetFileInfo(idx).Attr, faDirectory);
end;

function TABFileSearch.IsAttrArchive(idx: Integer): Boolean;
begin
  Result := IsBitOn(GetFileInfo(idx).Attr, faArchive);
end;

class function  TABFileSearch.IsBitOn(attr, bits: Integer): Boolean;
begin
  Result := ((attr and bits) = bits);
end;

class function  TABFileSearch.IsReadOnly(attr: Integer): Boolean;
begin
  Result := IsBitOn(attr, faReadOnly);
end;

class function  TABFileSearch.IsHidden(attr: Integer): Boolean;
begin
  Result := IsBitOn(attr, faHidden);
end;

class function  TABFileSearch.IsSysFile(attr: Integer): Boolean;
begin
  Result := IsBitOn(attr, faSysFile);
end;

{$IFDEF SUPPORT_VOLUME_ID}
class function  TABFileSearch.IsVolumeID(attr: Integer): Boolean;
begin
  Result := IsBitOn(attr, faVolumeID);
end;
{$ENDIF}

class function  TABFileSearch.IsDirectory(attr: Integer): Boolean;
begin
  Result := IsBitOn(attr, faDirectory);
end;

class function  TABFileSearch.IsArchive(attr: Integer): Boolean;
begin
  Result := IsBitOn(attr, faArchive);
end;


////////////////////////////////////////////////////////////////////

function ABNextToken(var str: String; const dlm: String; triming: Boolean): String;
var
  p:  Integer;

  function Trim2(s: String): String;
  begin
    if dlm = #9 then
    begin
      while (Length(s) > 0) and (s[1] = ' ') do
        Delete(s, 1, 1);
      Result := TrimRight(s);
    end
    else
      Result := Trim(s);
  end;

begin
  p := Pos(dlm, str) - 1;
  if p < 0 then
  begin
    Result := str;
    str := '';
  end
  else
  begin
    Result := Copy(str, 1, p);
    Delete(str, 1, p + Length(dlm));
  end;
  if triming then
  begin
    Result := Trim2(Result);
    str := Trim2(str);
  end;
end;

////////////////////////////////////////////////////////////////////

function BrowseCallback(hWnd: HWND; msg: UINT;
                                    lparam: LPARAM;
                                    wparam: WPARAM): Integer; stdcall; export;
var
  PathName: array[0..MAX_PATH] of Char;
begin
  Result := 0;
  case msg of
    BFFM_INITIALIZED:
      begin
        SendMessage(hWnd, BFFM_SETSELECTION, 1, LongInt(wparam));
      end;
    BFFM_SELCHANGED:
      begin
      SHGetPathFromIDList(PItemIDList(lParam), PathName);
      SendMessage(hWnd, BFFM_SETSTATUSTEXT, 0, LongInt(@PathName));
    end;
  end;
end;


function ABBrowseFolderDialog(var path: String): Boolean;
const
{$IFDEF VER140}     //delphi6
  CB_FLAGS = BIF_RETURNONLYFSDIRS or BIF_STATUSTEXT;
{$ELSE}
  CB_FLAGS = BIF_RETURNONLYFSDIRS or BIF_STATUSTEXT or BIF_NEWDIALOGSTYLE;
{$ENDIF}
var
  malloc: IMalloc;
  binfo:  TBrowseInfo;
  bpath:  array[0..MAX_PATH] of Char;
  ids:    PItemIdList;
  buf:    PChar;
  ppath:  PChar;
begin
  Result := False;

  if Succeeded(SHGetMalloc(malloc)) then
  begin
    ppath := PChar(path);

    with binfo do
    begin
      hwndOwner := GetForegroundWindow();
      pidlRoot := nil;
      pszDisplayName := bpath;
      lpszTitle := PChar(MSG_SELECT_FOLDER);
      ulFlags := CB_FLAGS;
      lpfn := @BrowseCallback; //コールバック関数指定
      lParam := LongInt(ppath); //初期フォルダ指定
      iImage := 0;
    end;

    ids := SHBrowseForFolder(binfo);
    if Assigned(ids) then
    begin
      buf := Malloc.Alloc(MAX_PATH); //フォルダパス取得用バッファ
      try
        SHGetPathFromIDList(ids, buf);//フォルダパスを取得
        path := String(buf);
      finally
        Malloc.Free(buf);
      end;
      Malloc.Free(ids);
      Result := True;
    end;
  end;
end;

function ABAssignExtToApp(const path, ext, name, desc: String;
                          iconIdx: Integer): Boolean;
var
  reg:  TRegistry;
  key:  String;
begin
  Result := False;
  if (Trim(ext)  = '') then Exit;
  if (Trim(name) = '') then Exit;
  if (Trim(path) = '') then Exit;
  Result := True;
  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_CLASSES_ROOT;
    //  拡張子とオブジェクト名の関連づけ
    key := Trim(ext);
    if key[1] <> '.' then key := '.' + key;
    if reg.OpenKey('\' + key, True) then
    begin
      reg.WriteString('', Trim(name));
      reg.CloseKey;
      //  オブジェクト名の説明
      key := Trim(name);
      if reg.OpenKey('\' + key, True) then
      begin
        if Trim(desc) <> '' then
          reg.WriteString('', Trim(desc))
        else
          reg.WriteString('', Trim(ext) + 'ファイル');
        reg.CloseKey;
        //  アイコンの設定
        if Result and reg.OpenKey('\' + key + '\DefaultIcon', True) then
        begin
          reg.WriteString('', Trim(path) + ',' + IntToStr(iconIdx));
          reg.CloseKey;
        end
        else
          Result := False;
        //  以降、シェルの設定
        if Result and reg.OpenKey('\' + key + '\Shell', True) then
        begin
          reg.WriteString('', 'open');
          reg.CloseKey;
        end
        else
          Result := False;
        if Result and reg.OpenKey('\' + key + '\Shell\open', True) then
        begin
          reg.CloseKey;
        end
        else
          Result := False;
        if Result and reg.OpenKey('\' + key + '\Shell\open\command', True) then
        begin
          reg.WriteString('', Trim(path) + ' "%1"');
          reg.CloseKey;
        end
        else
          Result := False;
      end
      else
        Result := False;
    end
    else
      Result := False;
  finally
    reg.Free;
  end;
end;

function  ABAssignedApp(const ext: String): String;
var
  reg:  TRegistry;
  key:  String;
  p:    Integer;
begin
  Result := '';
  if (Trim(ext)  = '') then Exit;
  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_CLASSES_ROOT;
    //  拡張子とオブジェクト名の関連づけを調べる
    key := Trim(ext);
    if key[1] <> '.' then key := '.' + key;
    if reg.OpenKeyReadOnly('\' + key) then
    begin
      key := reg.ReadString('');
      reg.CloseKey;
      if reg.OpenKeyReadOnly('\' + key + '\Shell\open\command') then
      begin
        Result := reg.ReadString('');
        reg.CloseKey;
      end;
    end;
  finally
    reg.Free;
  end;
  //  引数が指定されていたりするので、実行ファイル名だけを取り出す
  if Result <> '' then
  begin
    if Result[1] = '"' then
    begin
      Delete(Result, 1, 1);
      Result := ABNextToken(Result, '"');
    end;
    p := Pos('.exe', LowerCase(Result));
    if p > 0 then
      Delete(Result, p + 4, Length(Result));
  end;
end;

function ABFileVersion(const filename: String; ver: TABVersion): Integer;
const
  qfvs = '\StringFileInfo\041103A4\FileVersion';
var
  hwnd: DWORD;
  nfvi: DWORD;
  nvqv: DWORD;
  pbuf: Pointer;
  pval: Pointer;
  vers: String;
  vmaj: Integer;
  vmin: Integer;
  vrel: Integer;
  vbld: Integer;
begin
  Result := 0;
  nfvi := GetFileVersionInfoSize(PChar(filename), hwnd);
  if nfvi > 0 then
  begin
    pbuf := AllocMem(nfvi);
    try
      GetFileVersionInfo(PChar(filename), 0, nfvi, pbuf);
      VerQueryValue(pbuf, PChar(qfvs), pval, nvqv);
      if nvqv > 0 then
      begin
        SetLength(vers, nvqv);
        StrLCopy(PChar(vers), pval, nvqv);
        vmaj := StrToIntDef(AbNextToken(vers, '.'), 0);   //  major
        vmin := StrToIntDef(AbNextToken(vers, '.'), 0);   //  minor
        vrel := StrToIntDef(AbNextToken(vers, '.'), 0);   //  release
        vbld := StrToIntDef(Trim(vers), 0);               //  build
        case ver of
          verMajor:   Result := vmaj;
          verMinor:   Result := vmin;
          verRelease: Result := vrel;
          verBuild:   Result := vbld;
        end;
      end;
    finally
      FreeMem(pbuf, nfvi);
    end;
  end;
end;

end.
