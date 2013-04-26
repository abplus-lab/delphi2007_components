unit PixelMaps;

interface

uses
  Types, Graphics, Classes, BmpUtils;

type

  TColorRGBA = packed record
    function  GetColor: TColor;
    procedure SetColor(col: TColor);
    property  Color: TColor read GetColor write SetColor;
  case Integer of
    0:  (C: TColor);
    1:  (R, G, B, A: Byte);
  end;

  TColorIndex = packed record
    Alpha:  Byte;
    Index:  Byte;
  end;

  TColorPalette = class
  private
    FCols:  array of TColor;
  protected
    function  GetCount: Integer;                    virtual;
    function  GetColor(idx: Integer): TColor;       virtual;
    procedure SetColor(idx: Integer; col: TColor);  virtual;
  public
    constructor Create(n: Integer);
    procedure ResetToStandard;
    function  IndexOf(c: TColor): Integer;
    property  Count: Integer                read GetCount;
    property  Colors[idx: Integer]: TColor  read GetColor   write SetColor;
  end;

  TPixelMaps = class;
  
  TPixelMap = class
  private
    FWidth:     Integer;
    FHeight:    Integer;
    FName:      String;
    FDefAlpha:  Byte;
    FVisible:   Boolean;
    FData:      TObject;
  protected
    function  GetPixel(x, y: Integer): TColorRGBA;     virtual;  abstract;
    procedure SetPixel(x, y: Integer; c: TColorRGBA);  virtual;  abstract;
    procedure DoAssign(src: TPixelMap);                virtual;
  public
    constructor Create(owner: TPixelMaps);
    procedure Assign(src: TPixelMap);
    procedure Clear(c: TColor = clNone);              virtual;  abstract;
    procedure RotateLeft;                             virtual;  abstract;
    procedure RotateRight;                            virtual;  abstract;
    procedure RotateUp;                               virtual;  abstract;
    procedure RotateDown;                             virtual;  abstract;
    property  Width: Integer                    read FWidth;
    property  Height: Integer                   read FHeight;
    property  Name: String                      read FName      write FName;
    property  Pixels[x, y: Integer]: TColorRGBA  read GetPixel   write SetPixel;
    property  DefaultAlpha: Byte                read FDefAlpha  write FDefAlpha;
    property  Visible:  Boolean                 read FVisible   write FVisible;
    property  Data: TObject                     read FData      write FData;
  end;

  TIndexPixMap = class(TPixelMap)
  private
    FIdxBuf:  array of TColorIndex;
    FPalette: TColorPalette;
    function  AlphaIndexOf(c: TColor): TColorIndex;
  protected
    function  GetPixel(x, y: Integer): TColorRGBA;     override;
    procedure SetPixel(x, y: Integer; c: TColorRGBA);  override;
    function  GetIdx(x, y: Integer): TColorIndex;
    procedure SetIdx(x, y: Integer; i: TColorIndex);        
    procedure DoAssign(src: TPixelMap);                override;
  public
    constructor Create(owner: TPixelMaps);
    procedure Clear(c: TColor = clNone);              override;
    procedure RotateLeft;                             override;
    procedure RotateRight;                            override;
    procedure RotateUp;                               override;
    procedure RotateDown;                             override;
    property  Palette: TColorPalette              read FPalette write FPalette;
    property  Indexes[x, y: Integer]: TColorIndex read GetIdx   write SetIdx;
  end;

  TFullColorPixMap = class(TPixelMap)
  private
    FColBuf:  array of TColorRGBA;
  protected
    function  GetPixel(x, y: Integer): TColorRGBA;     override;
    procedure SetPixel(x, y: Integer; c: TColorRGBA);  override;
  public
    constructor Create(owner: TPixelMaps);
    procedure Clear(c: TColor = clNone);              override;
    procedure RotateLeft;                             override;
    procedure RotateRight;                            override;
    procedure RotateUp;                               override;
    procedure RotateDown;                             override;
  end;

  TPixelMaps = class
  private
    FPixMaps: TList;
    FPalette: TColorPalette;
    FWidth:   Integer;
    FHeight:  Integer;
    FDepth:   Integer;
    function  GetCount: Integer;
    function  GetItem(idx: Integer): TPixelMap;
    function  GetPixel(i, x, y: Integer): TColorRGBA;
    procedure SetPixel(i, x, y: Integer; c: TColorRGBA);
  protected
    procedure ReplacePalette(pal: TColorPalette);
  public
    constructor Create(w, h: Integer; d: Integer = 0);
    destructor  Destroy;                                      override;
    procedure LoadFromStream(stream: TStream);                virtual;
    procedure SaveToStream(stream: TStream);                  virtual;
    function  Add(const name: String = ''): Integer;          virtual;
    procedure Delete(idx: Integer);                           virtual;
    procedure Move(fidx, tidx: Integer);
    procedure Erase(i, x, y: Integer);
    procedure Display(i, x, y: Integer);
    function  IndexOf(layer: TPixelMap): Integer;
    function  Brended(i, x, y: Integer; c: TColor): TColor;   overload;
    function  Brended(x, y: Integer; c: TColor): TColor;      overload;
    procedure DrawTo(cvs: TCanvas; dstx: Integer = 0;
                                   dsty: Integer = 0);        overload;
    procedure DrawTo(cvs: TCanvas; r: TRect;
                                   dstx: Integer = 0;
                                   dsty: Integer = 0);        overload;
    property  Width: Integer                      read FWidth;
    property  Height: Integer                     read FHeight;
    property  Depth:  Integer                     read FDepth;
    property  Palette: TColorPalette              read FPalette;
    property  Count:  Integer                     read GetCount;
    property  Items[idx: Integer]: TPixelMap      read GetItem;
    property  Pixels[i, x, y: Integer]: TColorRGBA read GetPixel write SetPixel;
  end;

  TByteMap = class
  private
    FBuf:     array of Byte;
    FWidth:   Integer;
    FHeight:  Integer;
    function  GetCount: Integer;
    function  GetByte(i: Integer): Byte;
    procedure SetByte(i: Integer; b: Byte);
    function  GetCell(x, y: Integer): Byte;
    procedure SetCell(x, y: Integer; b: Byte);
  public
    constructor Create(w, h: Integer);
    procedure Clear(flgs: Byte = 0);
    procedure BitsOn(p: TPoint; m: Byte = $FF);               overload;
    procedure BitsOff(p: TPoint; m: Byte = $FF);              overload;
    function  Flags(p: TPoint; m: Byte = $FF): Boolean;       overload;
    procedure BitsOn(x, y: integer; m: Byte = $FF);           overload;
    procedure BitsOff(x, y: integer; m: Byte = $FF);          overload;
    function  Flags(x, y: integer; m: Byte = $FF): Boolean;   overload;
    procedure BitsOn(i: Integer; m: Byte = $FF);              overload;
    procedure BitsOff(i: Integer; m: Byte = $FF);             overload;
    function  Flags(i: Integer; m: Byte = $FF): Boolean;      overload;
    procedure Shift(bits: Integer);
    property  Width: Integer              read FWidth;
    property  Height: Integer             read FHeight;
    property  Count:  Integer             read GetCount;
    property  Bytes[i: Integer]: Byte     read GetByte  write SetByte;
    property  Cells[x, y: Integer]: Byte  read GetCell  write SetCell;
  end;

function  ColorIndex(alp, idx: Byte): TColorIndex;
function  ColorRGBA(col: TColor; alp: Byte = 255): TColorRGBA;
function  IntToRGBA(val: Integer): TColorRGBA;

function  BrendColorRGBA(fc, bc: TColorRGBA): TColorRGBA;

//////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////
implementation

uses
  SysUtils, Windows, GraphUtil;

//////////////////////////////////////////////////////////////////////////

function  ColorIndex(alp, idx: Byte): TColorIndex;
begin
  Result.Alpha := alp;
  Result.Index := idx;
end;


function  ColorRGBA(col: TColor; alp: Byte): TColorRGBA;
begin
  Result.C := ColorToRGB(col);
  Result.A := alp;
end;

function  IntToRGBA(val: Integer): TColorRGBA;
begin
  Result.C := val;
end;

function  IsDarkColor(c: TColor): Boolean;
begin
  Result := (GetRValue(c) < 128) and
            (GetGValue(c) < 128) and
            (GetBValue(c) < 128);
end;

function  Ceil16(val, ofs: Integer): Integer;
var
  tmp:  Integer;
begin
  tmp := (val + ofs) div 16;
  if (val + ofs) mod 16 > 0 then Inc(tmp);
  Result := tmp * 16 - ofs;
end;

function  BrendColorRGBA(fc, bc: TColorRGBA): TColorRGBA;

  function  BrendByte(bc, fc: Word; a: Byte): Byte;
  begin
    Result := ((255 - a) * bc + a * fc + 128) div 255;
  end;

begin
  Result.R := BrendByte(bc.R, fc.R, fc.A);
  Result.G := BrendByte(bc.G, fc.G, fc.A);
  Result.B := BrendByte(bc.B, fc.B, fc.A);
  Result.A := 255;
end;


//////////////////////////////////////////////////////////////////////////
{ TColorRGBA }

function  TColorRGBA.GetColor: TColor;
begin
  Result := C and $FFFFFF;
end;

procedure TColorRGBA.SetColor(col: TColor);
begin
  C := C and $FF000000;
  C := C + ColorToRGB(col);
end;

//////////////////////////////////////////////////////////////////////////
{ TColorPalette }

constructor TColorPalette.Create(n: Integer);
begin
  inherited Create;

  SetLength(FCols, n);
  ResetToStandard;
end;

procedure TColorPalette.ResetToStandard;
var
  n:  Integer;
  i:  Integer;
  m:  Integer;
begin
  n := Length(FCols);

  if n < 256 then
    m := 16
  else
    m := 256;

  for i := 0 to n - 1 do
  begin
    if i >= m then
      FCols[i] := clBlack
    else if n < 256 then
      FCols[i] := StdPaletteColor16(i)
    else
      FCols[i] := StdPaletteColor256(i);
  end;
end;

function  TColorPalette.GetCount: Integer;
begin
  Result := Length(FCols);
end;

function  TColorPalette.GetColor(idx: Integer): TColor;
begin
  Result := FCols[idx];
end;

procedure TColorPalette.SetColor(idx: Integer; col: TColor);
begin
  FCols[idx] := ColorToRGB(col);
end;

function  TColorPalette.IndexOf(c: TColor): Integer;
var
  i:  Integer;
  d:  Double;
  m:  Double;
  c1: TColorHLS;
  c2: TColorHLS;
begin
  Result := -1;

  m := 1000000.0;   //  圧倒的に大きな値

  ColorRGBToHLS(c, c1.H, c1.L, c1.S);

  for i := 0 to Length(FCols) - 1 do
  begin
    if FCols[i] = c then
    begin
      //  bingo!
      Result := i;
      Exit
    end;
    //  距離を測る
    ColorRGBToHLS(FCols[i], c2.H, c2.L, c2.S);
    d := ColorDistance(c1, c2);
    if m > d then
    begin
      Result := i;
      m := d;
    end;
  end;
end;

//////////////////////////////////////////////////////////////////////////
{ TPixelMap }

constructor TPixelMap.Create(owner: TPixelMaps);
begin
  inherited Create;

  FWidth    := 16;
  FHeight   := 16;
  FDefAlpha := 255;
  FVisible  := True;

  if Assigned(owner) then
  begin
    FWidth  := owner.Width;
    FHeight := owner.Height;
  end
end;

procedure TPixelMap.Assign(src: TPixelMap);
begin
  if Assigned(src) then
  begin
    if (Width <> src.Width) or (Height <> src.Height) then
      raise Exception.Create('大きさの異なるPixelMapは、Assignできません')
    else
      DoAssign(src);
  end;
end;

procedure TPixelMap.DoAssign(src: TPixelMap);
var
  x:  Integer;
  y:  Integer;
begin
  for y := 0 to Height - 1 do for x := 0 to Width - 1 do
    Self.Pixels[x, y] := src.Pixels[x, y];
end;

//////////////////////////////////////////////////////////////////////////
{ TIndexPixMap }

constructor TIndexPixMap.Create(owner: TPixelMaps);
begin
  inherited Create(owner);

  SetLength(FIdxBuf, Width * Height);

  if Assigned(owner) then
    FPalette := owner.Palette
  else
    FPalette := nil;

  Clear;
end;

procedure TIndexPixMap.DoAssign(src: TPixelMap);
var
  m:  TIndexPixMap;
  i:  Integer;
begin
  if src is TIndexPixMap then
  begin
    m := src as TIndexPixMap;
    for i := 0 to Length(FIdxBuf) - 1 do
      FIdxBuf[i] := m.FIdxBuf[i];
  end
  else
    inherited DoAssign(src);
end;

function  TIndexPixMap.AlphaIndexOf(c: TColor): TColorIndex;
begin
  if Assigned(FPalette) and (c <> clNone) then
  begin
    Result.Index := FPalette.IndexOf(c);
    Result.Alpha := 255;
  end
  else
  begin
    Result.Index := 0;
    Result.Alpha := 0;
  end;
end;

procedure TIndexPixMap.Clear(c: TColor);
var
  i:  Integer;
  ai: TColorIndex;
begin
  ai := AlphaIndexOf(c);
  for i := 0 to Length(FIdxBuf) - 1 do
  begin
    FIdxBuf[i].Alpha := ai.Alpha;
    FIdxBuf[i].Index := ai.Index;
  end;
end;

function  TIndexPixMap.GetIdx(x, y: Integer): TColorIndex;
begin
  Result := FIdxBuf[y * Width + x];
end;

procedure TIndexPixMap.SetIdx(x, y: Integer; i: TColorIndex);
begin
  FIdxBuf[y * Width + x] := i;
end;

function  TIndexPixMap.GetPixel(x, y: Integer): TColorRGBA;
var
  i:  Integer;
begin
  i := y * Width + x;

  if Assigned(FPalette) then
  begin
    Result.Color := FPalette.Colors[FIdxBuf[i].Index];
    Result.A     := FIdxBuf[i].Alpha;
  end
  else
    Result.C := clNone;
end;

procedure TIndexPixMap.SetPixel(x, y: Integer; c: TColorRGBA);
var
  i:  Integer;
begin
  i := y * Width + x;
  FIdxBuf[i] := AlphaIndexOf(c.Color);
  FIdxBuf[i].Alpha := c.A;
end;

procedure TIndexPixMap.RotateLeft;
var
  y:  Integer;
  x:  Integer;
  p:  TColorIndex;
begin
  for y := 0 to Height - 1 do
  begin
    p := FIdxBuf[y * Width];
    for x := 1 to Width - 1 do
      FIdxBuf[y * Width + x - 1] := FIdxBuf[y * Width + x];
    FIdxBuf[y * Width + Width - 1] := p;
  end;
end;

procedure TIndexPixMap.RotateRight;
var
  y:  Integer;
  x:  Integer;
  p:  TColorIndex;
begin
  for y := 0 to Height - 1 do
  begin
    p := FIdxBuf[y * Width + Width - 1];
    for x := Width - 1 downto 1 do
      FIdxBuf[y * Width + x] := FIdxBuf[y * Width + x - 1];
    FIdxBuf[y * Width] := p;
  end;
end;

procedure TIndexPixMap.RotateUp;
var
  b:  array of TColorIndex;
  y:  Integer;
  x:  Integer;
begin
  SetLength(b, Width);
  for x := 0 to Width - 1 do
    b[x] := FIdxBuf[x];

  for y := 1 to Height - 1 do
    for x := 0 to Width - 1 do
      FIdxBuf[(y - 1) * Width + x] := FIdxBuf[y * Width + x];

  for x := 0 to Width - 1 do
    FIdxBuf[(Height - 1) * Width + x] := b[x];
end;

procedure TIndexPixMap.RotateDown;
var
  b:  array of TColorIndex;
  y:  Integer;
  x:  Integer;
begin
  SetLength(b, Width);
  for x := 0 to Width - 1 do
    b[x] := FIdxBuf[(Height - 1) * Width + x];

  for y := Height - 1 downto 1 do
    for x := 0 to Width - 1 do
      FIdxBuf[y * Width + x] := FIdxBuf[(y - 1) * Width + x];

  for x := 0 to Width - 1 do
    FIdxBuf[x] := b[x];
end;

//////////////////////////////////////////////////////////////////////////
{ TFullColorPixMap }

constructor TFullColorPixMap.Create(owner: TPixelMaps);
begin
  inherited Create(owner);
  
  SetLength(FColBuf, Width * Height);
  Clear;
end;

procedure TFullColorPixMap.Clear(c: TColor);
var
  i:  Integer;
begin
  if c = clNone then
    c := 0
  else
    c := ColorToRGB(c) and $FF000000;

  for i := 0 to Length(FColBuf) - 1 do
    FColBuf[i].C := c;
end;

function  TFullColorPixMap.GetPixel(x, y: Integer): TColorRGBA;
begin
  Result := FColBuf[y * Width + x];
end;

procedure TFullColorPixMap.SetPixel(x, y: Integer; c: TColorRGBA);
begin
  FColBuf[y * Width + x] := c;
end;

procedure TFullColorPixMap.RotateLeft;
var
  y:  Integer;
  x:  Integer;
  p:  TColorRGBA;
begin
  for y := 0 to Height - 1 do
  begin
    p := FColBuf[y * Width];
    for x := 1 to Width - 1 do
      FColBuf[y * Width + x - 1] := FColBuf[y * Width + x];
    FColBuf[y * Width + Width - 1] := p;
  end;
end;

procedure TFullColorPixMap.RotateRight;
var
  y:  Integer;
  x:  Integer;
  p:  TColorRGBA;
begin
  for y := 0 to Height - 1 do
  begin
    p := FColBuf[y * Width + Width - 1];
    for x := Width - 1 downto 1 do
      FColBuf[y * Width + x] := FColBuf[y * Width + x - 1];
    FColBuf[y * Width] := p;
  end;
end;

procedure TFullColorPixMap.RotateUp;
var
  b:  array of TColorRGBA;
  y:  Integer;
  x:  Integer;
begin
  SetLength(b, Width);
  for x := 0 to Width - 1 do
    b[x] := FColBuf[x];

  for y := 1 to Height - 1 do
    for x := 0 to Width - 1 do
      FColBuf[(y - 1) * Width + x] := FColBuf[y * Width + x];

  for x := 0 to Width - 1 do
    FColBuf[(Height - 1) * Width + x] := b[x];
end;

procedure TFullColorPixMap.RotateDown;
var
  b:  array of TColorRGBA;
  y:  Integer;
  x:  Integer;
begin
  SetLength(b, Width);
  for x := 0 to Width - 1 do
    b[x] := FColBuf[(Height - 1) * Width + x];

  for y := Height - 1 downto 1 do
    for x := 0 to Width - 1 do
      FColBuf[y * Width + x] := FColBuf[(y - 1) * Width + x];

  for x := 0 to Width - 1 do
    FColBuf[x] := b[x];
end;

//////////////////////////////////////////////////////////////////////////
{ TPixelMaps }

constructor TPixelMaps.Create(w, h, d: Integer);
begin
  inherited Create;

  if (d > 256) or (d < 1) then
    FPalette := nil
  else if d > 16 then
    FPalette := TColorPalette.Create(256)
  else
    FPalette := TColorPalette.Create(16);

  FPixMaps  := TList.Create;
  FWidth    := w;
  FHeight   := h;
  if Assigned(FPalette) then
    FDepth  := FPalette.Count
  else
    FDepth  := 0;
end;

destructor  TPixelMaps.Destroy;
begin
  while Count > 0 do
    Delete(0);
  FreeAndNil(FPixMaps);
  FreeAndNil(FPalette);
  inherited Destroy;
end;

procedure TPixelMaps.LoadFromStream(stream: TStream);

  function  ReadByte: Byte;
  begin
    stream.ReadBuffer(Result, 1);
  end;

  function  ReadLong: Integer;
  begin
    stream.ReadBuffer(Result, 4);
  end;

var
  i:  Integer;
  x:  Integer;
  y:  Integer;
  c:  Byte;
  a:  Byte;
begin
  //  パレット
  if Assigned(FPalette) then
    for i := 0 to FPalette.Count - 1 do
      FPalette.Colors[i] := ReadLong;
  //  イメージ
  try
  for i := 0 to Count - 1 do
  begin
    for y := 0 to FHeight - 1 do
    begin
      for x := 0 to FWidth - 1 do
      begin
        if Assigned(FPalette) then
        begin
          a := ReadByte;
          c := ReadByte;
          (Items[i] as TIndexPixMap).Indexes[x, y] := ColorIndex(a, c);
        end
        else
          Items[i].Pixels[x, y] := IntToRGBA(ReadLong);
      end;
    end;
  end;
  except
  end;
end;

procedure TPixelMaps.SaveToStream(stream: TStream);

  procedure WriteByte(b: Byte);     overload;
  begin
    stream.WriteBuffer(b, 1);
  end;

  procedure WriteByte(c: AnsiChar); overload;
  begin
    stream.WriteBuffer(c, 1);
  end;

  procedure WriteLong(c: Integer);
  begin
    stream.WriteBuffer(c, 4);
  end;

  procedure WriteText(const s: String);
  var
    n:  Integer;
    i:  Integer;
  begin
    n := Ceil16(Length(s), 4);
    //  文字数
    WriteLong(Length(s));
    //  文字
    for i := 0 to n - 1 do
    begin
      if i < Length(s) then
        WriteByte(s[i + 1])
      else
        WriteByte(0);
    end;
  end;

var
  i:  Integer;
  x:  Integer;
  y:  Integer;
  ci: TColorIndex;
begin
  //  パレット
  if Assigned(FPalette) then
    for i := 0 to FPalette.Count - 1 do
      WriteLong(FPalette.Colors[i]);
  //  イメージ
  for i := 0 to Count - 1 do
  begin
    for y := 0 to FHeight - 1 do
    begin
      for x := 0 to FWidth - 1 do
      begin
        if Assigned(FPalette) then
        begin
          ci := (Items[i] as TIndexPixMap).Indexes[x, y];
          WriteByte(ci.Alpha);
          WriteByte(ci.Index);
        end
        else
          WriteLong(Items[i].Pixels[x, y].C);
      end;
    end;
  end;
end;

procedure TPixelMaps.Erase(i, x, y: Integer);
begin
  Items[i].Pixels[x, y] := ColorRGBA(Items[i].Pixels[x, y].Color, 0);
end;

procedure TPixelMaps.Display(i, x, y: Integer);
begin
  Items[i].Pixels[x, y] := ColorRGBA(Items[i].Pixels[x, y].Color, Items[i].DefaultAlpha);
end;

procedure TPixelMaps.ReplacePalette(pal: TColorPalette);
begin
  if Assigned(FPalette) then
  begin
    FreeAndNil(FPalette);
    FPalette := pal;
  end
  else
    pal.Free;
end;

function  TPixelMaps.GetItem(idx: Integer): TPixelMap;
begin
  Result := FPixMaps.Items[idx];
end;

function  TPixelMaps.GetPixel(i, x, y: Integer): TColorRGBA;
begin
  Result := Items[i].Pixels[x, y];
end;

procedure TPixelMaps.SetPixel(i, x, y: Integer; c: TColorRGBA);
begin
  Items[i].Pixels[x, y] := c;
end;

function  TPixelMaps.GetCount: Integer;
begin
  Result := FPixMaps.Count;
end;

procedure TPixelMaps.Delete(idx: Integer);
begin
  TPixelMap(FPixMaps.Items[idx]).Free;
  FPixMaps.Delete(idx);
end;

function  TPixelMaps.Add(const name: String): Integer;
var
  itm:  TPixelMap;
begin
  if Assigned(FPalette) then
    itm := TIndexPixMap.Create(Self)
  else
    itm := TFullColorPixMap.Create(Self);
  itm.Name := name;
  Result := FPixMaps.Add(itm);
end;

procedure TPixelMaps.Move(fidx, tidx: Integer);
var
  itm:  TPixelMap;
begin
  if fidx <> tidx then
  begin
  itm := FPixMaps.Items[fidx];
    FPixMaps.Remove(itm);
    FPixMaps.Insert(tidx, itm);
  end;
end;

function  TPixelMaps.IndexOf(layer: TPixelMap): Integer;
var
  i:  Integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
  begin
    if Items[i] = layer then
    begin
      Result := i;
      Break;
    end;
  end;
end;

function  TPixelMaps.Brended(i, x, y: Integer; c: TColor): TColor;
begin
  if c <> clNone then
    Result := BrendColorRGBA(Pixels[i, x, y], ColorRGBA(c, 255)).Color
  else if Pixels[i, x, y].A > 0 then
    Result := Pixels[i, x, y].Color
  else
    Result := c;
end;

function  TPixelMaps.Brended(x, y: Integer; c: TColor): TColor;
var
  i:    Integer;
begin
  Result := c;

  for i := 0 to Count - 1 do
    if Items[i].Visible then
      Result := Brended(i, x, y, Result);
end;

procedure TPixelMaps.DrawTo(cvs: TCanvas; dstx, dsty: Integer);
begin
  DrawTo(cvs, Rect(0, 0, Width - 1, Height - 1), dstx, dsty);
end;

procedure TPixelMaps.DrawTo(cvs: TCanvas; r: TRect; dstx, dsty: Integer);

  procedure DrawPixel(x, y: Integer);
  var
    dx: Integer;
    dy: Integer;
  begin
    dx := dstx + x;
    dy := dsty + y;
    cvs.Pixels[dx, dy] := Brended(x, y, cvs.Pixels[dx, dy]);
  end;

var
  x:  Integer;
  y:  Integer;
begin
  if r.Right  >= Width  then r.Right  := Width  - 1;
  if r.Bottom >= Height then r.Bottom := Height - 1;

  for y := r.Top to r.Bottom do
    for x := r.Left to r.Right do
      cvs.Pixels[dstx + x, dsty + y] := Brended(x, y, cvs.Pixels[dstx + x, dsty + y]);
end;

//////////////////////////////////////////////////////////////////////////
{ TByteMap }

constructor TByteMap.Create(w, h: Integer);
begin
  inherited Create;
  FWidth  := w;
  FHeight := h;

  SetLength(FBuf, w * h);
  Clear;
end;

procedure TByteMap.Clear(flgs: Byte = 0);
var
  i:  Integer;
begin
  for i := 0 to Length(FBuf) - 1 do
    FBuf[i] := flgs;
end;

function  TByteMap.GetCount: Integer;
begin
  Result := Length(FBuf);
end;

function  TByteMap.GetByte(i: Integer): Byte;
begin
  if (0 <= i) and (i < Length(Fbuf)) then
    Result := FBuf[i]
  else
    Result := 0;
end;

procedure TByteMap.SetByte(i: Integer; b: Byte);
begin
  if (0 <= i) and (i < Length(Fbuf)) then
    FBuf[i] := b;
end;

function  TByteMap.GetCell(x, y: Integer): Byte;
begin
  if (0 <= x) and (x < FWidth) and
     (0 <= y) and (y < FHeight) then
    Result := FBuf[y * FWidth + x]
  else
    Result := 0;
end;

procedure TByteMap.SetCell(x, y: Integer; b: Byte);
begin
  if (0 <= x) and (x < FWidth) and
     (0 <= y) and (y < FHeight) then
    FBuf[y * FWidth + x] := b;
end;


procedure TByteMap.BitsOn(p: TPoint; m: Byte);
begin
  BitsOn(p.X, p.Y, m);
end;

procedure TByteMap.BitsOff(p: TPoint; m: Byte);
begin
  BitsOff(p.X, p.Y, m);
end;

function  TByteMap.Flags(p: TPoint; m: Byte): Boolean;
begin
  Result := Flags(p.X, p.Y, m);
end;

procedure TByteMap.BitsOn(x, y: integer; m: Byte);
begin
  if (0 <= x) and (x < FWidth) and
     (0 <= y) and (y < FHeight) then
    FBuf[y * FWidth + x] := FBuf[y * FWidth + x] or m;
end;

procedure TByteMap.BitsOff(x, y: integer; m: Byte);
begin
  if (0 <= x) and (x < FWidth) and
     (0 <= y) and (y < FHeight) then
    FBuf[y * FWidth + x] := FBuf[y * FWidth + x] and (not m);
end;

function  TByteMap.Flags(x, y: integer; m: Byte): Boolean;
begin
  if (0 <= x) and (x < FWidth) and
     (0 <= y) and (y < FHeight) then
    Result := (FBuf[y * FWidth + x] and m) <> 0
  else
    Result := False;
end;

procedure TByteMap.BitsOn(i: integer; m: Byte);
begin
  if (0 <= i) and (i < Length(FBuf)) then
    FBuf[i] := FBuf[i] or m;
end;

procedure TByteMap.BitsOff(i: integer; m: Byte);
begin
  if (0 <= i) and (i < Length(FBuf)) then
    FBuf[i] := FBuf[i] and (not m);
end;

function  TByteMap.Flags(i: integer; m: Byte): Boolean;
begin
  if (0 <= i) and (i < Length(FBuf)) then
    Result := (FBuf[i] and m) <> 0
  else
    Result := False;
end;

procedure TByteMap.Shift(bits: Integer);
var
  i:  integer;
begin
  for i := 0 to Length(FBuf) - 1 do
  begin
    if bits > 0 then
      FBuf[i] := FBuf[i] shl  bits
    else if bits < 0 then
      FBuf[i] := FBuf[i] shr -bits;
  end;
end;


end.
