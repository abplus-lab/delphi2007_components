unit BmpUtils;

interface

uses
  Types, Classes, Graphics;

type

  TRGBQ = packed record
    function  GetColor: TColor;
    procedure SetColor(col: TColor);
    property  Color: TColor read GetColor write SetColor;
  case Integer of
    0:  (C: Cardinal);
    1:  (B, G, R, reserve: Byte);
    2:  (Bytes: array [0..3] of Byte);
  end;

  TColorHLS = packed record
    H:  Word;
    L:  Word;
    S:  Word;
  end;

  TDepthType = (st1bit, st4bit, st8bit);

  TIndexMap = class
  private
    FBuf:     array of TRGBQ;
    FWidth:   Integer;
    FHeight:  Integer;
    FSubType: TDepthType;
    function  GetPixel1(x, y: Integer): Cardinal;
    procedure SetPixel1(x, y: Integer; c: Cardinal);
    function  GetPixel4(x, y: Integer): Cardinal;
    procedure SetPixel4(x, y: Integer; c: Cardinal);
    function  GetPixel8(x, y: Integer): Cardinal;
    procedure SetPixel8(x, y: Integer; c: Cardinal);
    function  GetPixel(x, y: Integer): Cardinal;
    procedure SetPixel(x, y: Integer; c: Cardinal);
    function  GetBitCnt: Integer;
    function  LineLength: Integer;
    function  GetByte(x, y, b:  Integer): Byte;
    procedure SetByte(x, y, b: Integer; v: Byte);
    function  GetMapSize: Integer;
  public
    constructor Create(w, h: Integer; t: TDepthType);
    procedure ReadFromStream(stream: TStream);
    procedure WriteToStream(stream: TStream);
    property  Width:  Integer                 read FWidth;
    property  Height: Integer                 read FHeight;
    property  BitCount: Integer               read GetBitCnt;
    property  MapSize:  Integer               read GetMapSize;
    property  Pixels[x, y: Integer]: Cardinal read GetPixel write SetPixel;
  end;

  TMaskMap = class(TIndexMap)
  private
    function  GetMask(x, y: Integer): Boolean;
    procedure SetMask(x, y: Integer; m: Boolean);
  public
    constructor Create(w, h: Integer);
    property  Masks[x, y: Integer]: Boolean read GetMask write SetMask;
  end;

  TMaskedIndexMap = class(TIndexMap)
  private
    FMask:  TMaskMap;
    function  GetMask(x, y: Integer): Boolean;
    procedure SetMask(x, y: Integer; m: Boolean);
  public
    constructor Create(w, h: Integer; t: TDepthType);
    destructor  Destroy;  override;
    property  MaskMap: TMaskMap             read FMask;
    property  Masks[x, y: Integer]: Boolean read GetMask write SetMask;
  end;

function  ColorToRGBQ(c: TColor): TRGBQ;
function  RGBQToColor(c: TRGBQ): TColor;

function  ColorDistance(c1, c2: TColor): Double;      overload;
function  ColorDistance(c1, c2: TColorHLS): Double;   overload;

function  StdPaletteColor16(idx: Integer): TColor;
function  StdPaletteColor256(idx: Integer): TColor;

procedure EzFillRect(cvs: TCanvas; w, h: Integer; c: TColor);   overload;
procedure EzFillRect(cvs: TCanvas; r: TRect; c: TColor);        overload;


//////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////
implementation

uses
  SysUtils, Windows, GraphUtil;

type

  TRGBA = packed record
  case Integer of
    0:  (C: TColor);
    1:  (R, G, B, A: Byte);
  end;


//////////////////////////////////////////////////////////////////////////

function  ColorToRGBQ(c: TColor): TRGBQ;
var
  d:  TRGBA;
begin
  d.C := ColorToRGB(c);
  Result.R := d.R;
  Result.G := d.G;
  Result.B := d.B;
  Result.reserve := 0;
end;

function  RGBQToColor(c: TRGBQ): TColor;
var
  d:  TRGBA;
begin
  d.R := c.R;
  d.G := c.G;
  d.B := c.B;
  d.A := 0;
  Result := d.C;
end;

function  ColorDistance(c1, c2: TColor): Double;
var
  d1, d2: TColorHLS;
begin
  ColorRGBToHLS(c1, d1.H, d1.L, d1.S);
  ColorRGBToHLS(c2, d2.H, d2.L, d2.S);
  Result := ColorDistance(d1, d2);
end;

function  ColorDistance(c1, c2: TColorHLS): Double;
var
  t:  Double;
  x:  Double;
  y:  Double;
  z:  Double;
begin
  t := c2.H - c1.H;
  t := t / 240 * 2 * Pi;

  x := c2.S * Cos(t) - c1.S;
  y := c2.S * Sin(t);
  z := c2.L - c1.L;

  Result := Sqrt(x * x + y * y + z * z);
end;

function  StdPaletteColor16(idx: Integer): TColor;
begin
  case idx of
    0:  Result := $000000;
    1:  Result := $800000;
    2:  Result := $008000;
    3:  Result := $808000;
    4:  Result := $000080;
    5:  Result := $800080;
    6:  Result := $008080;
    7:  Result := $808080;
    8:  Result := $C0C0C0;
    9:  Result := $FF0000;
    10: Result := $00FF00;
    11: Result := $FFFF00;
    12: Result := $0000FF;
    13: Result := $FF00FF;
    14: Result := $00FFFF;
    15: Result := $FFFFFF;
  else  Result := clNone;
  end;
end;

function  StdPaletteColor256(idx: Integer): TColor;
var
  r:  Integer;
  g:  Integer;
  b:  Integer;
begin
  if idx < 16 then
    Result := $111111 * (idx mod 16)  //  Gray scale
  else if idx = 16 then
    Result := $FFFF00
  else if idx < 32 then
    Result := $000011 * (idx mod 16)  //  Red scale
  else if idx = 32 then
    Result := $FF00FF
  else if idx < 48 then
    Result := $001100 * (idx mod 16)  //  Green scale
  else if idx = 48 then
    Result := $00FFFF
  else if idx < 64 then
    Result := $110000 * (idx mod 16)  //  Blue scale
  else
  begin
    //  WebSafeColor‚ÅA‚±‚ê‚Ü‚Å‚Éo‚Ä‚«‚Ä‚È‚¢‚à‚Ì
    Dec(idx, 64);
    Result := clNone;
    for r := 0 to 5 do for g := 0 to 5 do for b := 0 to 5 do
    begin
      if (r = g) and (g = b) then
        Continue
      else if (r = 0) and (g = 0) then
        Continue
      else if (g = 0) and (b = 0) then
        Continue
      else if (b = 0) and (r = 0) then
        Continue
      else if (r = 5) and (g = 5) and (b = 0) then
        Continue
      else if (g = 5) and (b = 5) and (r = 0) then
        Continue
      else if (b = 5) and (r = 5) and (g = 0) then
        Continue
      else if idx > 0 then
        Dec(idx)
      else
      begin
        Result := $000033 * r + $003300 * g + $330000 * b;
        Exit;
      end;
    end;
  end;
end;

procedure EzFillRect(cvs: TCanvas; w, h: Integer; c: TColor);
begin
  EzFillRect(cvs, Rect(0, 0, w - 1, h - 1), c);
end;

procedure EzFillRect(cvs: TCanvas; r: TRect; c: TColor);
begin
  cvs.Pen.Color   := c;
  cvs.Pen.Style   := psSolid;
  cvs.Brush.Color := c;
  cvs.Brush.Style := bsSolid;
  cvs.Rectangle(r);
end;

//////////////////////////////////////////////////////////////////////////
{ TRGBQ }

function  TRGBQ.GetColor: TColor;
begin
  Result := RGB(R, G, B);
end;

procedure TRGBQ.SetColor(col: TColor);
begin
  R := GetRValue(col);
  G := GetGValue(col);
  B := GetBValue(col);
end;

//////////////////////////////////////////////////////////////////////////
{ TIndexMap }

constructor TIndexMap.Create(w, h: Integer; t: TDepthType);
begin
  inherited Create;

  FWidth    := w;
  FHeight   := h;
  FSubType  := t;

  SetLength(FBuf, LineLength * FHeight);
end;

procedure TIndexMap.ReadFromStream(stream: TStream);
var
  i:  Integer;
  n:  Integer;
begin
  n := Length(FBuf);
  for i := 0 to n - 1 do
    stream.ReadBuffer(FBuf[i mod n], 4);
end;

procedure TIndexMap.WriteToStream(stream: TStream);
var
  i:  Integer;
  n:  Integer;
begin
  n := Length(FBuf);
  for i := 0 to n - 1 do
    stream.WriteBuffer(FBuf[i mod n], 4);
end;

function  TIndexMap.LineLength: Integer;
var
  cnt:  Integer;
begin
  case FSubType of
    st1bit:  cnt := 32 div 1;
    st4bit:  cnt := 32 div 4;
  else       cnt := 32 div 8;
  end;

  Result := (FWidth + (cnt - 1)) div cnt;
end;

function  TIndexMap.GetBitCnt: Integer;
begin
  case FSubType of
    st1bit:  Result := 1;
    st4bit:  Result := 4;
  else       Result := 8;
  end;
end;

function  TIndexMap.GetPixel(x, y: Integer): Cardinal;
begin
  case FSubType of
    st1bit:  Result := GetPixel1(x, FHeight - 1 - y);
    st4bit:  Result := GetPixel4(x, FHeight - 1 - y);
  else       Result := GetPixel8(x, FHeight - 1 - y);
  end;
end;

procedure TIndexMap.SetPixel(x, y: Integer; c: Cardinal);
begin
  case FSubType of
    st1bit:  SetPixel1(x, FHeight - 1 - y, c);
    st4bit:  SetPixel4(x, FHeight - 1 - y, c);
  else       SetPixel8(x, FHeight - 1 - y, c);
  end;
end;

function  TIndexMap.GetByte(x, y, b:  Integer): Byte;
var
  n:  Integer;
  m:  Integer;
  i:  Integer;
  j:  Integer;
begin
  n := (32 div b);
  m := (8 div b);
  i := y * LineLength + x div n;
  j := (x mod n) div m;

  Result := FBuf[i].Bytes[j];
end;

procedure TIndexMap.SetByte(x, y, b: Integer; v: Byte);
var
  n:  Integer;
  m:  Integer;
  i:  Integer;
  j:  Integer;
begin
  n := (32 div b);
  m := (8 div b);
  i := y * LineLength + x div n;
  j := (x mod n) div m;

  FBuf[i].Bytes[j] := v;
end;


function  TIndexMap.GetPixel1(x, y: Integer): Cardinal;
var
  b:  Byte;
  m:  Byte;
  i:  Integer;
begin
  b := GetByte(x, y, 1);
  i := 7 - (x mod 8);
  m := 1 shl i;
  b := b and m;
  Result := b shr i;
end;

procedure TIndexMap.SetPixel1(x, y: Integer; c: Cardinal);
var
  b:  Byte;
  m:  Byte;
  i:  Integer;
begin
  b := GetByte(x, y, 1);
  i := 7 - (x mod 8);
  m := not (1 shl i);
  c := c and 1;
  c := c shl i;

  SetByte(x, y, 1, b and m or c);
end;

function  TIndexMap.GetPixel4(x, y: Integer): Cardinal;
var
  b:  Byte;
  m:  Byte;
  i:  Integer;
begin
  b := GetByte(x, y, 4);
  i := 1 - (x mod 2);
  i := i * 4;
  m := $F shl i;
  b := b and m;
  Result := b shr i;
end;

procedure TIndexMap.SetPixel4(x, y: Integer; c: Cardinal);
var
  b:  Byte;
  m:  Byte;
  i:  Integer;
begin
  b := GetByte(x, y, 4);
  i := 1 - (x mod 2);
  i := i * 4;
  m := not ($F shl i);
  c := c and $F;
  c := c shl i;

  SetByte(x, y, 4, b and m or c);
end;

function  TIndexMap.GetPixel8(x, y: Integer): Cardinal;
begin
  Result := GetByte(x, y, 8);
end;

procedure TIndexMap.SetPixel8(x, y: Integer; c: Cardinal);
begin
  SetByte(x, y, 8, c and $FF);
end;

function  TIndexMap.GetMapSize;
begin
  Result := Length(FBuf) * 4;
end;

//////////////////////////////////////////////////////////////////////////
{ TMaskMap }

constructor TMaskMap.Create(w, h: Integer);
var
  x, y: Integer;
begin
  inherited Create(w, h, st1bit);

  for y := 0 to Height - 1 do
    for x := 0 to Width - 1 do
      Masks[x, y] := True;
end;

function  TMaskMap.GetMask(x, y: Integer): Boolean;
begin
  Result := Pixels[x, y] <> 0;
end;

procedure TMaskMap.SetMask(x, y: Integer; m: Boolean);
begin
  if m then
    Pixels[x, y] := 1
  else
    Pixels[x, y] := 0;
end;

//////////////////////////////////////////////////////////////////////////
{ TMaskedIndexMap }

constructor TMaskedIndexMap.Create(w, h: Integer; t: TDepthType);
begin
  inherited Create(w, h, t);
  FMask := TMaskMap.Create(w, h);
end;

destructor  TMaskedIndexMap.Destroy;
begin
  FreeAndNil(FMask);
  inherited Destroy;
end;

function  TMaskedIndexMap.GetMask(x, y: Integer): Boolean;
begin
  Result := FMask.GetMask(x, y);
end;

procedure TMaskedIndexMap.SetMask(x, y: Integer; m: Boolean);
begin
  FMask.SetMask(x, y, m);
end;

end.
