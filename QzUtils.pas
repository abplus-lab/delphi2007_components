unit QzUtils;

interface

uses
  Dialogs;

function  NextToken(var str: String;
                    const dlm: String = #9;
                    triming: Boolean = True): String;

function  IntToDec(val: Integer; digit: Integer): String;   overload;
function  IntToDec(val: Int64; digit: Integer): String;     overload;

procedure ErrorDlg(const msg: String);
procedure WarningDlg(const msg: String);
function  ConfirmDlg(const msg: String;
                      btns: TMsgDlgButtons = [mbYes, mbNo, mbCancel];
                      def: TMsgDlgBtn = mbCancel): Integer;


//////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////
implementation

uses
  SysUtils;

function NextToken(var str: String; const dlm: String; triming: Boolean): String;
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

function  ZeroSupress(const s: String; digit: Integer): String;
begin
  Result := s;
  while Length(Result) < digit do
    Result := '0' + Result;
end;

function  IntToDec(val: Integer; digit: Integer): String;
begin
  Result := ZeroSupress(IntToStr(val), digit);
end;

function  IntToDec(val: Int64; digit: Integer): String;
begin
  Result := ZeroSupress(IntToStr(val), digit);
end;

procedure ErrorDlg(const msg: String);
begin
  MessageDlg(msg, mtError, [mbOk], 0);
end;

procedure WarningDlg(const msg: String);
begin
  MessageDlg(msg, mtWarning, [mbOk], 0);
end;

function  ConfirmDlg(const msg: String; btns: TMsgDlgButtons;
  def: TMsgDlgBtn): Integer;
begin
  Result := MessageDlg(msg, mtConfirmation, btns, 0, def);
end;


end.
