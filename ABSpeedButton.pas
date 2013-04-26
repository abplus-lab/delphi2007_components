unit ABSpeedButton;

interface

uses
  //Windows,
  Types, Graphics, Messages, SysUtils, Classes, Controls, Buttons;

type

  TABSpeedButton = class(TSpeedButton)
  private
    { Private 宣言 }
    FBackDrops:   TImageList;
    FFontImage:   TBitmap;
    FPressed:     Boolean;
    FLastCaption: String;
    FInside:      Boolean;
  protected
    { Protected 宣言 }
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
                        X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
                      X, Y: Integer);   override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure FontChanged(Sender: TObject);
    procedure DrawGlyph;
    procedure DrawCaption;
  public
    { Public 宣言 }
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Paint; override;
  published
    { Published 宣言 }
    property BackDrops: TImageList read FBackDrops write FBackDrops;
  end;

procedure Register;

implementation

constructor TABSpeedButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  //FBackDrops := TImageList.Create(nil);
  FFontImage := nil;
  FInside    := False;
  FPressed   := False;
  FLastCaption  := '';
  Font.OnChange := FontChanged;
  Flat        := True;
  Transparent := True;
end;

destructor TABSpeedButton.Destroy;
begin
  //FBackDrops.Free;
  FFontImage.Free;
  inherited Destroy;
end;

procedure TABSpeedButton.FontChanged(Sender: TObject);
begin
  FreeAndNil(FFontImage);
end;

procedure TABSpeedButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
                                    X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  FPressed := True;
end;

procedure TABSpeedButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
                                  X,Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  FPressed := False;
end;

procedure TABSpeedButton.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  FInside := (0 <= X) and (X < Width) and (0 <= Y) and (Y < Height)
end;

procedure TABSpeedButton.DrawGlyph;
var
  p: TPoint;
  idx: Integer;
  offs: Integer;
begin
  p.X := (Width  - FBackDrops.Width)  div 2;
  p.Y := (Height - FBackDrops.Height) div 2;
  case FState of
    bsDisabled:   idx := 1;
    bsDown:       idx := 2;
    bsExclusive:  idx := 3;
  else
    if MouseInControl
    then idx := 4
    else idx := 0;
  end;

  if idx >= FBackDrops.Count then idx := 0;

  offs := 0;
  if (not Flat) and (FState in [bsDown, bsExclusive]) then offs := 1;

  FBackDrops.Draw(Canvas, p.X + offs, p.Y + offs, idx);
end;

procedure TABSpeedButton.DrawCaption;
var
  p: TPoint;
  offs: Integer;
begin
  if Caption <> FLastCaption then
  begin
    FreeAndNil(FFontImage);
    FLastCaption := Caption;
  end;
  if not Assigned(FFontImage) then
  begin
    FFontImage := TBitmap.Create;
    with FFontImage.Canvas do
    begin
      Font := Self.Font;
      Brush.Color := $FFFFFF xor ColorToRGB(Font.Color);
      p.X := TextWidth(Caption);
      p.Y := TextHeight(Caption);
      FFontImage.Width  := p.X + 2;
      FFontImage.Height := p.Y + 2;
      FillRect(Rect(0, 0, Width, Height));
      TextOut(1, 1, Caption);
      FLastCaption := Caption;
    end;
    FFontImage.TransparentMode := tmAuto;
    FFontImage.Transparent     := True;
  end;

  if Assigned(FFontImage) and (Caption <> '')  then
  begin
    offs := 0;
    if (not Flat) and (FState in [bsDown, bsExclusive]) then offs := 1;
    p.X := (Width  - FFontImage.Width)  div 2 + offs;
    p.Y := (Height - FFontImage.Height) div 2 + offs;
    Canvas.Draw(p.X, p.Y, FFontImage);
  end;
end;

procedure TABSpeedButton.Paint;
begin
  if Assigned(FBackDrops) and (FBackDrops.Count > 0) then
  begin
    //  FStateの更新
    if not Enabled then
    begin
      if FPressed then MouseUp(mbLeft, [], -1, -1);
      FState := bsDisabled;
    end
    else if FState = bsDisabled then
    begin
      if Down and (GroupIndex <> 0)
      then FState := bsExclusive
      else FState := bsUp;
    end;
    Canvas.Font := Self.Font;
    //  塗りつぶし
    if not Transparent then
    begin
      Canvas.FillRect(Rect(0, 0, Width, Height));
    end;
    //  グリフの描画
    DrawGlyph;
    //  キャプションの描画
    DrawCaption;
  end
  else inherited Paint;
end;


procedure Register;
begin
  RegisterComponents('ABplus', [TABSpeedButton]);
end;

end.
