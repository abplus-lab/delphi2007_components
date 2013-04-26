unit QzCtrls;

interface

uses
  Types, Controls;

type

  TMouseButtonState = (msDown, msMove, msUp);

  TDownedInfo = record
    Downed:   Boolean;
    Button:   TMouseButton;
    Position: array [TMouseButtonState] of TPoint;
    procedure Init;
    function  Down(x, y: Integer; btn: TMouseButton): Boolean;
    function  Move(x, y: Integer): Boolean;
    function  Up(x, y: Integer): Boolean;
    function  RelPos(ms: TMouseButtonState): TPoint;
    property  DownedPos: TPoint read Position[msDown];
  end;

/////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////
implementation

/////////////////////////////////////////////////////////////////////////
{ TDownedInfo }

procedure TDownedInfo.Init;
begin
  Downed := False;
  Button := mbLeft;

  Position[msDown] := Point(0, 0);
  Position[msMove] := Point(0, 0);
  Position[msUp]   := Point(0, 0);
end;

function  TDownedInfo.Down(x, y: Integer; btn: TMouseButton): Boolean;
begin
  Downed := True;
  Button := btn;

  Position[msDown] := Point(x, y);
  Position[msMove] := Point(x, y);
  Position[msUp]   := Point(x, y);

  Result := Downed;
end;

function  TDownedInfo.Move(x, y: Integer): Boolean;
begin
  Position[msMove] := Point(x, y);

  Result := Downed;
end;

function  TDownedInfo.Up(x, y: Integer): Boolean;
begin
  Downed := False;

  Position[msUp] := Point(x, y);

  Result := Downed;
end;

function  TDownedInfo.RelPos(ms: TMouseButtonState): TPoint;
begin
  Result.X := Position[ms].X - Position[msDown].X;
  Result.Y := Position[ms].Y - Position[msDown].Y;
end;


end.
