unit QzGraphics;

interface

uses
  Windows, Classes, Graphics;

//  色を扱うためのいろいろなデータ型

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
  function  RGBQ(r, g, b: Byte): TRGBQ;
  function  RGBQToColor(r, g, b: Byte): TColor;
  procedure ColorToRGBQ(col: TColor; var r, g, b: Byte);

type

  TColorHLS = record
    H:  Word;
    L:  Word;
    S:  Word;
    function  GetColor: TColor;
    procedure SetColor(col: TColor);
    property  Color: TColor read GetColor write SetColor;
  end;
  function  ColorHLS(h, l, s: Word): TColorHLS;   overload;
  function  ColorHLS(col: TColor): TColorHLS;     overload;

type

  TColorYUV = record
    Y:  Byte;
    U:  Byte;
    V:  Byte;
    function  GetColor: TColor;
    procedure SetColor(col: TColor);
    property  Color: TColor read GetColor write SetColor;
  end;
  function  ColorYUV(y, u, v: Byte): TColorYUV;   overload;
  function  ColorYUV(col: TColor): TColorYUV;     overload;

type

  TColorRGBA = packed record
    function  GetColor: TColor;
    procedure SetColor(col: TColor);
    function  GetAlpha: Byte;
    function  BrendTo(back: TColorRGBA): TColorRGBA;
    procedure SetAlpha(alp: Byte);
    property  Color: TColor read GetColor write SetColor;
    property  Alpha:  Byte  read GetAlpha write SetAlpha;
  case Integer of
    0:  (C: TColor);
    1:  (R, G, B, A: Byte);
  end;
  function  ColorRGBA(col: TColor; alp: Byte = 255): TColorRGBA;


//  色を扱うためのユーティリティ関数

function  ColorDistance(c1, c2: TColor): Double;      overload;
function  ColorDistance(c1, c2: TColorHLS): Double;   overload;
function  Brightness(c: TColor): Byte;

//  モノクロームに特化したグラフィックフォーマット

type

  TMograImage = class(TGraphic)
  private
    FWidth:   Integer;
    FHeight:  Integer;
    FOldSize: TPoint;
    FBuf:     array of Byte;
    FIgnore:  Boolean;
  protected
    procedure Changed(Sender: TObject);                   override;
    procedure Draw(cvs: TCanvas; const rct: TRect);       override;
    function  GetEmpty: Boolean;                          override;
    function  GetHeight: Integer;                         override;
    function  GetWidth: Integer;                          override;
    function  GetTransparent: Boolean;                    override;
    procedure SetHeight(h: Integer);                      override;
    procedure SetWidth(w: Integer);                       override;
    function  GetBright(x, y: Integer): Byte;             virtual;
    procedure SetBright(x, y: Integer; b: Byte);          virtual;
    function  GetPixel(x, y: Integer): TColor;            virtual;
    procedure SetPixel(x, y: Integer; c: TColor);         virtual;
  public
    constructor Create;                                   override;
    procedure SetSize(w, h: Integer);                     override;
    procedure LoadFromStream(st: TStream);                override;
    procedure SaveToStream(St: TStream);                  override;
    procedure LoadFromClipboardFormat(fmt: Word;
                                      dat: THandle;
                                      pal: HPALETTE);     override;
    procedure SaveToClipboardFormat(var fmt: Word;
                                    var dat: THandle;
                                    var pal: HPALETTE);   override;
    procedure Assign(src: TPersistent);                   override;
    property  Brights[x, y: Integer]: Byte    read GetBright  write SetBright;
    property  Pixels[x, y: Integer]: TColor   read GetPixel   write SetPixel;
  end;

  TMograHeader = packed record
    MarkW:  Cardinal;
    Width:  Integer;
    MarkH:  Cardinal;
    Height: Integer;
  end;
  PMograHeader = ^TMograHeader;


/////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////
implementation

uses
  Clipbrd, GraphUtil;

//////////////////////////////////////////////////////////////////////////

function  ColorDistance(c1, c2: TColor): Double;
begin
  Result := ColorDistance(ColorHLS(c1), ColorHLS(c2));
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

function  Brightness(c: TColor): Byte;
var
  r:  Byte;
  g:  Byte;
  b:  Byte;
  v:  Integer;
begin
  ColorToRGBQ(c, r, g, b);
{:Note::
  RGB to YUV
  Y =  0.299R + 0.587G + 0.114B
  U = -0.169R - 0.331G + 0.500B
  V =  0.500R - 0.419G - 0.081B
::Note:}
  v := 299 * r + 587 * g + 114 * b;
  v := (v + 500) div 1000;
  if v < 0    then v := 0;
  if v > 255  then v := 255;
  Result := v;
end;

//////////////////////////////////////////////////////////////////////////
{ TRGBQ }

function  RGBQ(r, g, b: Byte): TRGBQ;
begin
  Result.R := r;
  Result.G := g;
  Result.B := b;
  Result.reserve := 0;
end;


function  TRGBQ.GetColor: TColor;
begin
  Result := C;
end;

procedure TRGBQ.SetColor(col: TColor);
begin
  C := ColorToRGB(col);
end;

function  RGBQToColor(r, g, b: Byte): TColor;
begin
  Result := RGBQ(r, g, b).C;
end;

procedure ColorToRGBQ(col: TColor; var r, g, b: Byte);
var
  c:  TRGBQ;
begin
  c.C := ColorToRGB(col);
  r := c.R;
  g := c.G;
  b := c.B;
end;


//////////////////////////////////////////////////////////////////////////
{ TColorHLS }

function  ColorHLS(h, l, s: Word): TColorHLS;
begin
  Result.H := h;
  Result.L := l;
  Result.S := s;
end;

function  ColorHLS(col: TColor): TColorHLS;
begin
  Result.Color := col;
end;

function  TColorHLS.GetColor: TColor;
begin
  Result := ColorHLSToRGB(H, L, S);
end;

procedure TColorHLS.SetColor(col: TColor);
begin
  ColorRGBToHLS(col, H, L, S);
end;

//////////////////////////////////////////////////////////////////////////
{ TColorHLS }

function  ColorYUV(y, u, v: Byte): TColorYUV;
begin
  Result.Y := y;
  Result.U := u;
  Result.V := v;
end;

function  ColorYUV(col: TColor): TColorYUV;
begin
  Result.Color := col;
end;

function calcByte(k0, k1, k2, k3, b0, b1, b2, b3: Integer): Byte;
var
  c:  Integer;
begin
  c := k1 * b1 + k2 * b2 + k3 * b3;
  c := (c + k0) div (2 * k0) + b0;
  if c <   0 then c :=   0;
  if c > 255 then c := 255;
  Result := c;
end;


function  TColorYUV.GetColor: TColor;
var
  c:  TRGBQ;
begin
{:Note::
  YUV to RGB
  R = 1.000Y          + 1.402V
  G = 1.000Y - 0.344U - 0.714V
  B = 1.000Y + 1.772U
::Note:}
  c.R := calcByte(1000,  1000,    0, 1402, 0, Y, U - 128, V - 128);
  c.G := calcByte(1000,  1000, -344, -714, 0, Y, U - 128, V - 128);
  c.B := calcByte(1000,  1000, 1772,    0, 0, Y, U - 128, V - 128);
  c.reserve := 0;
  Result := c.C;
end;

procedure TColorYUV.SetColor(col: TColor);
var
  c:  TRGBQ;
begin
{:Note::
  RGB to YUV
  Y =  0.299R + 0.587G + 0.114B
  U = -0.169R - 0.331G + 0.500B
  V =  0.500R - 0.419G - 0.081B
::Note:}
  c.Color := col;
  Y := calcByte(1000,  299,  587, 114,   0, c.R, c.G, c.B);
  U := calcByte(1000, -169, -331, 500, 128, c.R, c.G, c.B);
  V := calcByte(1000,  500, -419, -81, 128, c.R, c.G, c.B);
end;

//////////////////////////////////////////////////////////////////////////
{ TColorRGBA }

function  ColorRGBA(col: TColor; alp: Byte): TColorRGBA;
begin
  Result.C := ColorToRGB(col);
  Result.A := alp;
end;

function  TColorRGBA.GetColor: TColor;
begin
  Result := C and $FFFFFF;
end;

procedure TColorRGBA.SetColor(col: TColor);
begin
  C := C and $FF000000;
  C := C + ColorToRGB(col);
end;

function  TColorRGBA.GetAlpha: Byte;
begin
  Result := A;
end;

procedure TColorRGBA.SetAlpha(alp: Byte);
begin
  A := alp;
end;

function  TColorRGBA.BrendTo(back: TColorRGBA): TColorRGBA;

  function  BrendByte(bc, fc: Word; a: Byte): Byte;
  begin
    Result := ((255 - a) * bc + a * fc + 128) div 255;
  end;

begin
  Result.R := BrendByte(back.R, R, A);
  Result.G := BrendByte(back.G, G, A);
  Result.B := BrendByte(back.B, B, A);
  Result.A := back.A;
end;

//////////////////////////////////////////////////////////////////////////
{ TMograImage }

const
  mogW = $57676F6D;  //mogW
  mogH = $48676F6D;  //mogH

var
  MograFormatID: Cardinal;

constructor TMograImage.Create;
var
  i:  Integer;
begin
  inherited Create;

  SetSize(32, 32);
  for i := 0 to Length(FBuf) - 1 do FBuf[i] := $FF;
end;

procedure TMograImage.Draw(cvs: TCanvas; const rct: TRect);
var
  x:  Integer;
  y:  Integer;
begin
  for y := rct.Top to rct.Bottom do
    if (0 <= y) and (y < FHeight) then
      for x := rct.Left to rct.Right do
        if (0 <= x) and (x < FWidth) then
          cvs.Pixels[x, y] := Pixels[x, y];
end;

function  TMograImage.GetEmpty: Boolean;
begin
  Result := False;
end;

function  TMograImage.GetTransparent: Boolean;
begin
  Result := True;
end;

function  TMograImage.GetBright(x, y: Integer): Byte;
begin
  Result := FBuf[y * FWidth + x];
end;

procedure TMograImage.SetBright(x, y: Integer; b: Byte);
begin
  FBuf[y * FWidth + x] := b;
end;

function  TMograImage.GetPixel(x, y: Integer): TColor;
begin
  Result := FBuf[y * FWidth + x] * $010101;
end;

procedure TMograImage.SetPixel(x, y: Integer; c: TColor);
begin
  FBuf[y * FWidth + x] := Brightness(c);
end;

function  TMograImage.GetHeight: Integer;
begin
  Result := FHeight;
end;

function  TMograImage.GetWidth: Integer;
begin
  Result := FWidth;
end;

procedure TMograImage.Changed(Sender: TObject);
begin
  if not FIgnore then
  begin
    if (FOldSize.X <> FWidth) or (FOldSize.Y <> FHeight) then
      SetLength(FBuf, FWidth * FHeight);
    inherited Changed(Sender);
  end;
end;

procedure TMograImage.SetSize(w, h: Integer);
begin
  FOldSize.X := FWidth;
  FOldSize.Y := FHeight;
  inherited SetSize(w, h);
end;

procedure TMograImage.SetHeight(h: Integer);
begin
  if FHeight <> h then
  begin
    FHeight := h;
    Changed(Self);
  end;
end;

procedure TMograImage.SetWidth(w: Integer);
begin
  if FWidth <> w then
  begin
    FWidth := w;
    Changed(Self);
  end;
end;

procedure TMograImage.Assign(src: TPersistent);

  procedure CopyImage(src: TMograImage);
  var
    x:  Integer;
    y:  Integer;
  begin
    FIgnore := True;
    try
      SetSize(src.Width, src.Height);
      for y := 0 to FHeight - 1 do
        for x := 0 to FWidth - 1 do
          Brights[x, y] := src.Pixels[x, y];
    finally
      FIgnore := False;
    end;
    Changed(Self);
  end;

  procedure CopyClip(clip: TClipboard);
  var
    h:  THandle;
  begin
    clip.Open;
    try
      // クリップボードから ClipboardFormat 型のデータを取得
      h := clip.GetAsHandle(MograFormatID);
      // データを押し込む
      LoadFromClipboardFormat(MograFormatID, h, 0);
    finally
      Clip.Close;
    end;
  end;

begin
  if src is TMograImage then
    CopyImage(src as TMograImage)
  else if src is TClipBoard then
    CopyClip(src as TClipBoard)
  else if src = nil then
    SetSize(0, 0)
  else
    inherited Assign(src);
end;

procedure TMograImage.LoadFromStream(st: TStream);
var
  header: TMograHeader;
  i:      Integer;
begin
  header.MarkW  := mogW;
  header.MarkH  := mogH;
  header.Width  := FWidth;
  header.Height := FHeight;
  st.WriteBuffer(header, SizeOf(TMograHeader));
  for i := 0 to Length(FBuf) - 1 do
    st.WriteBuffer(FBuf[i], 1);
end;

procedure TMograImage.SaveToStream(St: TStream);
var
  header: TMograHeader;
  i:      Integer;
begin
  st.ReadBuffer(header, SizeOf(TMograHeader));
  if (header.MarkW <> mogW) or (header.MarkH <> mogH) then
    raise EInvalidGraphic.Create('Invalid MograHeader');

  FIgnore := True;
  try
    SetSize(header.Width, header.Height);
  finally
    FIgnore := False;
  end;

  for i := 0 to Length(FBuf) - 1 do
    st.ReadBuffer(FBuf[i], 1);
end;

function  MograImgPtr(p:  PMograHeader): PByte;
begin
  Result := PByte(Integer(p) + SizeOf(TMograHeader));
end;

procedure TMograImage.LoadFromClipboardFormat(fmt: Word;
                                              dat: THandle; pal: HPALETTE);
var
  p:  PMograHeader;
  q:  PByte;
  i:  Integer;
begin
  if fmt <> MograFormatID then
    raise EInvalidGraphic.Create('Invalid ClipboardFormat for MograImage');
  if fmt = 0 then
    raise EInvalidGraphic.Create('ClipboardFormat not found for MograImage');

  p := GlobalLock(dat);
  try
    if (p^.MarkW <> mogW) or (p^.MarkH <> mogH) then
      raise EInvalidGraphic.Create('Invalid MograHeader');

    FIgnore := True;
    try
      SetSize(p^.Width, p^.Height);
    finally
      FIgnore := False;
    end;

    q := MograImgPtr(p);
    for i := 0 to Length(FBuf) - 1 do
    begin
      FBuf[i] := q^;
      Inc(q);
    end;
  finally
    GlobalUnlock(dat);
  end;
  Changed(Self);
end;

procedure TMograImage.SaveToClipboardFormat(var fmt: Word;
                                            var dat: THandle;
                                            var pal: HPALETTE);
var
  h: THandle;
  p: PMograHeader;
  q:  PByte;
  i:  Integer;
begin
  h := GlobalAlloc(GHND, SizeOf(TMograHeader) + Length(FBuf));
  p := GlobalLock(h);
  try
    p^.MarkW  := mogW;
    p^.MarkH  := mogH;
    p^.Width  := FWidth;
    p^.Height := FHeight;
    q := MograImgPtr(p);
    for i := 0 to Length(FBuf) - 1 do
    begin
      q^ := FBuf[i];
      Inc(q);
    end;
  finally
    GlobalUnlock(h);
  end;
  fmt := MograFormatID;
  dat := h;
  pal := 0;
end;

Initialization

  TPicture.RegisterFileFormat('mog', 'MograImage', TMograImage);

  MograFormatID := Windows.RegisterClipboardFormat(PChar('CF_MOGRAIMG'));
  TPicture.RegisterClipBoardFormat(MograFormatID, TMograImage);

finalization
  TPicture.UnRegisterGraphicClass(TMograImage);

end.
