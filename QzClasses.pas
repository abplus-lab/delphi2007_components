unit QzClasses;

interface

uses
  Classes;

type

  TBitStream = class(TMemoryStream)
  private
    FBuf: Byte;
    FMsk: Byte;
    FLSB: Boolean;
    FLst: Byte;
  protected
    procedure IncBit;
    procedure InitMask;
  public
    constructor Create(fromLSB: Boolean = False);
    constructor CreateFrom(src: TStream; len: Int64 = 0; fromLSB: Boolean = False);
    constructor CreateFromFile(const filename: String; fromLSB: Boolean = False);
    function  Eof: Boolean;
    procedure Rewind;
    function  ReadBit: Integer;
    function  ReadBool: Boolean;
    procedure UngetBit;
    function  WriteBit(b: Integer): Boolean;
    function  WriteBool(b: Boolean): Boolean;
    procedure Flush;
    procedure ResetMask;
    property  FromLSB: Boolean  read FLSB write FLSB;
    property  LastByte: Byte    read FLst;
    property  ByteBuffer: Byte  read FBuf;
    property  ByteMask: Byte    read FMsk;
  end;

/////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////
implementation


/////////////////////////////////////////////////////////////////////////
{ TBitStream }

constructor TBitStream.Create(fromLSB: Boolean);
begin
  inherited Create;
  FBuf := 0;
  FMsk := $FF;
  FLSB := fromLSB;
end;

constructor TBitStream.CreateFrom(src: TStream; len: Int64; fromLSB: Boolean);
begin
  inherited Create;
  FBuf := 0;
  FMsk := $FF;
  FLSB := fromLSB;

  LoadFromStream(src);
  Seek(0, soBeginning);
  if len > 0 then
    Size := len;
end;

constructor TBitStream.CreateFromFile(const filename: String; fromLSB: Boolean);
begin
  inherited Create;
  FBuf := 0;
  FMsk := 0;
  FLSB := fromLSB;

  LoadFromFile(filename);
  Seek(0, soBeginning);
end;

function  TBitStream.Eof: Boolean;
begin
  Result := (Position = Size) and (FMsk in [0, $FF]);
end;

procedure TBitStream.Rewind;
begin
  Seek(0, soBeginning);
  FMsk := $FF;
  FBuf := 0;
end;

procedure TBitStream.IncBit;
begin
  if FLSB then
    FMsk := FMsk shl 1
  else
    FMsk := FMsk shr 1;
end;

procedure TBitStream.InitMask;
begin
  if FLSB then
    FMsk := $01
  else
    FMsk := $80;
end;

function  TBitStream.ReadBit: Integer;
begin
  if ReadBool then
    Result := 1
  else
    Result := 0;
end;

function  TBitStream.ReadBool: Boolean;
begin
  FLst := FBuf;
  if FMsk in [0, $FF] then
  begin
    if Read(FBuf, 1) = 0 then
    begin
      FBuf := 0;
      FMsk := 0;
    end
    else
      InitMask;
  end;
  Result := ((FBuf and FMsk) <> 0);
  IncBit;
end;

procedure TBitStream.UngetBit;
begin
  //  マスクを逆にシフト
  FLSB := not FLSB;
  try
    if FMsk = 0then
      InitMask
    else
      IncBit;
  finally
    FLSB := not FLSB;
  end;
  //  バッファも元に戻す
  FBuf := FLst;
end;

function  TBitStream.WriteBit(b: Integer): Boolean;
begin
  if b = 0 then
    Result := WriteBool(False)
  else
    Result := WriteBool(True);
end;

function  TBitStream.WriteBool(b: Boolean): Boolean;
begin
  if FMsk = 0 then
  begin
    Flush;
    Result := True;
  end
  else if FMsk = $FF then
  begin
    InitMask;
    Result := True;
  end
  else
    Result := False;

  if b then
    FBuf := FBuf or FMsk
  else
    FBuf := FBuf and (not FMsk);

  IncBit;
end;

procedure TBitStream.Flush;
begin
  FLst := FBuf;
  WriteBuffer(FBuf, 1);
  InitMask;
  FBuf := 0;
end;

procedure TBitStream.ResetMask;
begin
  FMsk := 0;
end;

end.
