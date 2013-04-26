//  Copyright (C) 2000-2005 ABplus kazhida.
//  All rights reserved.
//  $Id: ABEzProtect.pas 770 2008-05-01 08:02:30Z kazhida $
//  $Author: kazhida $
//
unit ABEzProtect;

interface

{:Note::
  ���C�Z���X�E�t�@�C���𗘗p�����A
  �J�W���A���E�R�s�[��h�~���邽�߂̃v���e�N�g
::Note:}

uses
  Classes;

type

  TAbEzpLimiUnit = (ezpDay, ezpHour, ezpMinute);

  TAbEzProtect = class(TComponent)
  private
    FFileName:      String;
    FLimit:         Integer;
    FLimitUnit:     TABEzpLimiUnit;
    FKeyword1:      String;
    FKeyword2:      String;
    FIllegalUseMsg: String;
    FOutofDateMsg:  String;
  public
    constructor Create(AOwner: TComponent); override;
    procedure LicenseCheck;
  published
    property FileName:      String  read FFileName      write FFileName;
    property Limit:         Integer read FLimit         write FLimit default 30;
    property LimitUnit: TABEzpLimiUnit  read FLimitUnit write FLimitUnit default ezpDay;
    property Keyword1:      String  read FKeyword1      write FKeyword1;
    property Keyword2:      String  read FKeyword2      write FKeyword2;
    property IllegalUseMsg: String  read FIllegalUseMsg write FIllegalUseMsg;
    property OutofDateMsg:  String  read FOutofDateMsg  write FOutofDateMsg;
  end;

procedure Register;

////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////
implementation

uses
  Windows, SysUtils, ShlObj, DateUtils, Forms;

const
  EZPROTECT_MSG_ILLEGAL_USE = '�s���g�p�̋^��������̂ŏI�����܂��B';
  EZPROTECT_MSG_OUT_OF_DATE = '���C�Z���X�������؂�ł��B�V�K���C�Z���X���擾���Ă��������B';


////////////////////////////////////////////////////////////////////
{ utility }

function LicenseFolder(handle: HWND): String;
var
  path: String;
begin
  SetLength(path, MAX_PATH);
  SHGetSpecialFolderPath(handle, PChar(path), CSIDL_PERSONAL, False);
  Result := PChar(path);
end;

function TooOld(fn: String; ds: Integer; ut: TABEzpLimiUnit): Boolean;
{$IFDEF VER140} //delphi6
  function FileAge(const fn: String; var limit: TDateTime): Boolean;
  var
    fdt:  Integer;
  begin
    fdt := SysUtils.FileAge(fn);
    if fdt < 0 then
      Result := False
    else
    begin
      limit  := FileDateToDateTime(fdt);
      Result := True;
    end;
  end;
{$ENDIF}
var
  limit: TDateTime;
begin
  if FileAge(fn, limit) then
  begin
    case ut of
      ezpMinute:  limit  := IncMinute(limit, ds);
      ezpHour:    limit  := IncHour(limit, ds);
    else          limit  := IncDay(limit, ds);
    end;
    Result := (Now > Limit);
  end
    else Result := True;
end;

////////////////////////////////////////////////////////////////////
{ TABEzProtect }

constructor TABEzProtect.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFileName      := '';
  FLimit         := 30;
  FLimitUnit     := ezpDay;
  FKeyword1      := '';
  FKeyword2      := '';
  FIllegalUseMsg := EZPROTECT_MSG_ILLEGAL_USE;
  FOutofDateMsg  := EZPROTECT_MSG_OUT_OF_DATE;
end;

procedure TABEzProtect.LicenseCheck;
var
  fn: String;
  st: TStringList;
begin
  //  �����炪�󔒂̏ꍇ�́A�`�F�b�N���Ȃ��B
  if (Trim(FFileName) = '') or
     (Trim(FKeyword1) = '') or
     (Trim(FKeyword2) = '') then
    Exit;

  fn := LicenseFolder(Application.Handle) + '\' + FFileName;
  st := TStringList.Create;

  try
    try
      st.LoadFromFile(fn);
    except
      //  ���C�Z���X�E�t�@�C�����Ȃ�
      raise Exception.Create(FIllegalUseMsg);
    end;
      //  ���C�Z���X�E�t�@�C�����U��
      if st.Count < 2 then
        raise Exception.Create(FIllegalUseMsg);
      if Pos(Trim(FKeyword1), Trim(st.Strings[0])) <> 1 then
        raise Exception.Create(FIllegalUseMsg);
      //  ���C�Z���X�������؂�
      if (Pos(Trim(FKeyword2), Trim(st.Strings[1])) <> 1) and
         TooOld(fn, FLimit, FLimitUnit) then
        raise Exception.Create(FOutofDateMsg);
  finally
    st.Free;
  end;
end;

/////////////////////////////////////////////////////////////////

procedure Register;
begin
  RegisterComponents('ABplus', [TABEzProtect]);
end;

end.
