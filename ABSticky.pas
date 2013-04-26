//  Copyright (C) 2000-2005 ABplus kazhida.
//  All rights reserved.
//  $Id: ABSticky.pas 770 2008-05-01 08:02:30Z kazhida $
//  $Author: kazhida $
//
unit ABSticky;

interface

uses
  Types, Classes, Controls, StdCtrls, ExtCtrls, Graphics;

{$IFDEF VER140} //delphi6

  //  2Kスタイルでは使わない

{$ELSE}

type

  TAbStickyPos = (absNone, absExit,
                  absColor, absColor0, absColor1, absColor2, absColor3,
                  absTop, absLeft, absRight, absBottom);

  TAbStickyPanel = class(TPanel)
  private
    FDowned:  TAbStickyPos;
    FOrigin:  TRect;
    FMemo:    TMemo;
    FCSelect: Boolean;
    function  PositionOf(X, Y: Integer): TAbStickyPos;
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Paint; override;
  public
    property  Memo: TMemo write FMemo;
  end;

  TAbStickyItem = class(TCollectionItem)
  private
    FPanel:   TAbStickyPanel;
    FMemo:    TMemo;
    function  GetTop:     Integer;
    function  GetLeft:    Integer;
    function  GetWidth:   Integer;
    function  GetHeight:  Integer;
    function  GetColor:   TColor;
    function  GetVisible: Boolean;
    function  GetPadding: TPadding;
    function  GetParent:  TWinControl;
    procedure SetTop(Value:     Integer);
    procedure SetLeft(Value:    Integer);
    procedure SetWidth(Value:   Integer);
    procedure SetHeight(Value:  Integer);
    procedure SetColor(Value:   TColor);
    procedure SetVisible(Value: Boolean);
    procedure SetParent(Value:  TWinControl);
    procedure BuildPanel(AOwner: TComponent; AParent: TWinControl);
  public
    constructor Create(ACollection: TCollection);     override;
    destructor  Destroy;                              override;
    procedure Assign(ASource: TPersistent);           override;
    procedure Show;
    procedure Hide;
    procedure BringToFront;
    procedure SendToBack;
    property  Parent: TWinControl read GetParent write SetParent;
  published
    property  Top:    Integer read GetTop     write SetTop;
    property  Left:   Integer read GetLeft    write SetLeft;
    property  Width:  Integer read GetWidth   write SetWidth;
    property  Height: Integer read GetHeight  write SetHeight;
    property  Color:    TColor    read GetColor   write SetColor;
    property  Padding:  TPadding  read GetPadding;
    property  Memo:     TMemo     read FMemo;
    property  Visible:  Boolean   read GetVisible write SetVisible;
  end;

  TAbStickieCollection = class(TCollection)
  private
    FOwner:     TComponent;
    FDefParent: TWinControl;
    function  GetItem(Index: Integer): TAbStickyItem;
    procedure SetItem(Index: Integer; Value: TAbStickyItem);
  protected
    function  GetOwner: TPersistent;          override;
    procedure Update(AItem: TCollectionItem); override;
    procedure AssignTo(ADest: TPersistent);   override;
  public
    constructor Create(AOwner: TComponent; ADefParent: TWinControl);
    function  Add: TAbStickyItem;
    property  Items[Index: Integer]: TAbStickyItem read GetItem write SetItem;
  end;

  TAbStickey = class(TComponent)
  private
    FStickies:  TAbStickieCollection;
    FDefParent: TWinControl;
  public
    constructor Create(AOwner: TComponent); override;
    function  AddItemAt(X, Y: Integer; Parent: TWinControl = nil): TAbStickyItem;
    procedure Clear;
    procedure ShowAll(BringToFront: Boolean = True);
    procedure HideAll;
  published
    property  Stickies: TAbStickieCollection read FStickies write FStickies;
    property  DefaultParent:  TWinControl read FDefParent write FDefParent;
  end;

procedure Register;

{$ENDIF}

////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////
implementation

uses
  Forms, Windows, SysUtils;

{$IFDEF VER140} //delphi6

  //  2Kスタイルでは使わない

{$ELSE}

const
  Colors: array [0..3] of TColor = ($00FFFFCC, $00CCFFFF, $00CCFFCC, $00FFCCFF);

procedure Register;
begin
  RegisterComponents('ABplus', [TAbStickey]);
end;

////////////////////////////////////////////////////////////////////
{ TAbStickey }

constructor TAbStickey.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if AOwner is TWinControl then
    FDefParent := AOwner as TWinControl;
  FStickies := TAbStickieCollection.Create(Self, FDefParent);
end;

procedure TAbStickey.Clear;
begin
  FStickies.Clear;
end;

procedure TAbStickey.ShowAll(BringToFront: Boolean);
var
  i:  Integer;
begin
  for i := 0 to FStickies.Count - 1 do
  begin
    FStickies.Items[i].Show;
    if BringToFront then
      FStickies.Items[i].BringToFront;
  end;
end;

procedure TAbStickey.HideAll;
var
  i:  Integer;
begin
  for i := 0 to FStickies.Count - 1 do
    FStickies.Items[i].Hide;
end;

function  TAbStickey.AddItemAt(X, Y: Integer; Parent: TWinControl): TAbStickyItem;
begin
  Result := FStickies.Add;
  if Assigned(Result) then
  begin
    if Assigned(Parent) then
      Result.Parent := Parent;
    Result.Left := X;
    Result.Top  := Y;
    Result.Show;
    Result.BringToFront;
  end;
end;

////////////////////////////////////////////////////////////////////
{ TAbStickieCollection }

constructor TAbStickieCollection.Create(AOwner: TComponent; ADefParent: TWinControl);
begin
  inherited Create(TAbStickyItem);
  FOwner     := AOwner;
  FDefParent := ADefParent;
end;

function  TAbStickieCollection.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TAbStickieCollection.Update(AItem: TCollectionItem);
begin
  inherited;
end;

procedure TAbStickieCollection.AssignTo(ADest: TPersistent);
begin
  if ADest is TAbStickieCollection then
    (ADest as TAbStickieCollection).Assign(Self)
  else
    inherited AssignTo(ADest);
end;

function  TAbStickieCollection.Add: TAbStickyItem;
begin
  Result := (inherited Add) as TAbStickyItem;
end;

function  TAbStickieCollection.GetItem(Index: Integer): TAbStickyItem;
begin
  Result := (inherited GetItem(Index)) as TAbStickyItem;
end;

procedure TAbStickieCollection.SetItem(Index: Integer; Value: TAbStickyItem);
begin
  inherited SetItem(Index, Value);
end;

////////////////////////////////////////////////////////////////////
{ TAbStickyItem }


constructor TAbStickyItem.Create(ACollection: TCollection);
var
  c:  Integer;
begin
  inherited Create(ACollection);
  FPanel := nil;
  FMemo  := nil;
  if ACollection is TAbStickieCollection then
    with ACollection as TAbStickieCollection do
    begin
      c := Count mod 4;
      BuildPanel(FOwner, FDefParent);
      FPanel.Color := Colors[c];
    end;
end;

destructor TAbStickyItem.Destroy;
begin
  FPanel.Free;
  inherited Destroy;
end;

procedure TAbStickyItem.BuildPanel(AOwner: TComponent; AParent: TWinControl);
begin
  if not Assigned(FPanel) then
  begin
    FPanel := TAbStickyPanel.Create(AOwner);
    FPanel.Parent := AParent;
    FPanel.Width  := 153;
    FPanel.Height := 121;
    FPanel.Padding.Top    := 12;
    FPanel.Padding.Left   := 4;
    FPanel.Padding.Right  := 4;
    FPanel.Padding.Bottom := 4;
  end;
  if not Assigned(FMemo) then
  begin
    FMemo := TMemo.Create(AOwner);
    FMemo.Parent      := FPanel;
    FMemo.Ctl3D       := False;
    FMemo.BorderStyle := bsNone;
    FMemo.ParentColor := True;
    FMemo.Align       := alClient;
    FMemo.Lines.Clear;
    FPanel.Memo := FMemo;
  end;
end;

procedure TAbStickyItem.Assign(ASource: TPersistent);
var
  src: TAbStickyItem;
begin
  inherited Assign(ASource);

  if ASource is TAbStickyItem then
  begin
    src := ASource as TAbStickyItem;
    if Assigned(FPanel) then
      FPanel.Assign(src.FPanel);
    if Assigned(FMemo) then
      FMemo.Assign(src.FMemo);
  end;
end;


procedure TAbStickyItem.Show;
begin
  if Assigned(FPanel) then
    FPanel.Show;
end;

procedure TAbStickyItem.Hide;
begin
  if Assigned(FPanel) then
    FPanel.Hide;
end;

procedure TAbStickyItem.BringToFront;
begin
  if Assigned(FPanel) then
    FPanel.BringToFront;
end;

procedure TAbStickyItem.SendToBack;
begin
  if Assigned(FPanel) then
    FPanel.SendToBack;
end;


function  TAbStickyItem.GetTop:     Integer;
begin
  if Assigned(FPanel) then
    Result := FPanel.Top
  else
    Result := 0;
end;

function  TAbStickyItem.GetLeft:    Integer;
begin
  if Assigned(FPanel) then
    Result := FPanel.Left
  else
    Result := 0;
end;

function  TAbStickyItem.GetWidth:   Integer;
begin
  if Assigned(FPanel) then
    Result := FPanel.Width
  else
    Result := 0;
end;

function  TAbStickyItem.GetHeight:  Integer;
begin
  if Assigned(FPanel) then
    Result := FPanel.Height
  else
    Result := 0;
end;

function  TAbStickyItem.GetColor:   TColor;
begin
  if Assigned(FPanel) then
    Result := FPanel.Color
  else
    Result := 0;
end;

function  TAbStickyItem.GetVisible: Boolean;
begin
  if Assigned(FPanel) then
    Result := FPanel.Visible
  else
    Result := False;
end;

function  TAbStickyItem.GetParent: TWinControl;
begin
  if Assigned(FPanel) then
    Result := FPanel.Parent
  else
    Result := nil;
end;

function  TAbStickyItem.GetPadding: TPadding;
begin
  if Assigned(FPanel) then
    Result := FPanel.Padding
  else
    Result := nil;
end;

procedure TAbStickyItem.SetTop(Value:     Integer);
begin
  if Assigned(FPanel) then
    FPanel.Top := Value;
end;

procedure TAbStickyItem.SetLeft(Value:    Integer);
begin
  if Assigned(FPanel) then
    FPanel.Left := Value;
end;

procedure TAbStickyItem.SetWidth(Value:   Integer);
begin
  if Assigned(FPanel) then
    FPanel.Width := Value;
end;

procedure TAbStickyItem.SetHeight(Value:  Integer);
begin
  if Assigned(FPanel) then
    FPanel.Height := Value;
end;

procedure TAbStickyItem.SetColor(Value:   TColor);
begin
  if Assigned(FPanel) then
    FPanel.Color := Value;
end;

procedure TAbStickyItem.SetVisible(Value: Boolean);
begin
  if Assigned(FPanel) then
    FPanel.Visible := Value;
end;

procedure TAbStickyItem.SetParent(Value: TWinControl);
begin
  if Assigned(FPanel) then
    FPanel.Parent := Value;
end;

////////////////////////////////////////////////////////////////////
{ TAbStickyItem }

function TAbStickyPanel.PositionOf(X, Y: Integer): TAbStickyPos;
begin
  if (2 < Y) and (Y < 10) and
     (Width - 10 < X) and (X < Width - 2) then
    Result := absExit
  else if (3 < Y) and (Y < 9) and
     (Width - 18 < X) and (X < Width - 12) then
    Result := absColor
  else if Y < Padding.Top then
    Result := absTop
  else if Y > Self.Height - Padding.Bottom then
    Result := absBottom
  else if X < Padding.Left then
    Result := absLeft
  else if X > Self.Width - Padding.Right then
    Result := absRight
  else
    Result := absNone;

  if FCSelect and (3 < Y) and (Y < 9) then
  begin
    if Width - 18 <= X then
      Result := Result
    else if Width - 18 - 8 * 1 <= X then
      Result := absColor0
    else if Width - 18 - 8 * 2 <= X then
      Result := absColor1
    else if Width - 18 - 8 * 3 <= X then
      Result := absColor2
    else if Width - 18 - 8 * 4 <= X then
      Result := absColor3
  end;
end;

procedure TAbStickyPanel.Paint;
var
  X, Y: Integer;
  i:    Integer;
begin
  inherited;
  Canvas.Pen.Color := clGray;
  Canvas.Pen.Style := psSolid;

  X := Self.Width - 10;
  Y := 2;
  Canvas.MoveTo(X + 1, Y + 0);
  Canvas.LineTo(X + 8, Y + 0);
  Canvas.MoveTo(X + 1, Y + 8);
  Canvas.LineTo(X + 8, Y + 8);
  Canvas.MoveTo(X,     Y + 1);
  Canvas.LineTo(X,     Y + 8);
  Canvas.MoveTo(X + 8, Y + 1);
  Canvas.LineTo(X + 8, Y + 8);
  Canvas.MoveTo(X + 3, Y + 3);
  Canvas.LineTo(X + 6, Y + 6);
  Canvas.MoveTo(X + 5, Y + 3);
  Canvas.LineTo(X + 2, Y + 6);

  X := Self.Width - 18;
  Y := 2;
  Canvas.MoveTo(X + 0, Y + 1);
  Canvas.LineTo(X + 5, Y + 1);
  Canvas.LineTo(X + 5, Y + 7);
  Canvas.LineTo(X + 0, Y + 7);
  Canvas.LineTo(X + 0, Y + 1);
  Canvas.MoveTo(X + 2, Y + 4);
  Canvas.LineTo(X + 4, Y + 2);
  Canvas.MoveTo(X + 2, Y + 4);
  Canvas.LineTo(X + 4, Y + 6);
  if FCSelect then
  begin
    for i := 0 to 3 do
    begin
      Dec(X, 8);
      Canvas.Pen.Color := Colors[i];
      Canvas.Brush.Color := Colors[i];
      Canvas.FillRect(Rect(X, Y + 1, X + 8, Y + 8));
    end;
  end;
end;


procedure TAbStickyPanel.MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
begin
  if (FDowned = absNone) and (Button = mbLeft) then
  begin
    FDowned   := PositionOf(X, Y);
    if FDowned = absExit then
    begin
      //  閉じる
      Self.Hide;
      FDowned := absNone;
    end
    else if FDowned = absColor then
    begin
      //  色選択
      FCSelect := True;
      Invalidate;
    end
    else if (absColor0 <= FDowned) and (FDowned <= absColor3) then
    begin
      //  色選択
      FCSelect := False;
      Color := Colors[Ord(FDowned) - Ord(absColor0)];
      Invalidate;
      FDowned  := absNone;
    end
    else
    begin
      //  移動、変形
      FOrigin.Top    := Y;
      FOrigin.Left   := X;
      FOrigin.Right  := Left + Width;
      FOrigin.Bottom := Top + Height;
      FMemo.Visible := False;
    end;
  end;
  inherited;
end;

procedure TAbStickyPanel.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  r:  TRect;
begin
  if FDowned = absTop then
  begin
    r.Top    := Self.Top  + Y - FOrigin.Top;
    r.Left   := Self.Left + X - FOrigin.Left;
    r.Right  := r.Left + Self.Width;
    r.Bottom := r.Top  + Self.Height;
    Self.BoundsRect := r;
  end
  else if FDowned = absLeft then
  begin
    r.Top    := Self.Top;
    r.Left   := Self.Left + X - FOrigin.Left;
    r.Right  := FOrigin.Right;
    r.Bottom := FOrigin.Bottom;
    Self.BoundsRect := r;
  end
  else if FDowned = absRight then
  begin
    r.Top    := Self.Top;
    r.Left   := Self.Left;
    r.Right  := FOrigin.Right + X - FOrigin.Left;
    r.Bottom := FOrigin.Bottom;
    Self.BoundsRect := r;
  end
  else if FDowned = absBottom then
  begin
    r.Top    := Self.Top;
    r.Left   := Self.Left;
    r.Right  := FOrigin.Right;
    r.Bottom := FOrigin.Bottom + Y - FOrigin.Top;
    Self.BoundsRect := r;
  end
  else
  begin
    case PositionOf(X, Y) of
      absExit:    Cursor := crArrow;
      absColor..absColor3:   Cursor := crArrow;
      absTop:     Cursor := crHandPoint;
      absLeft:    Cursor := crSizeWE;
      absRight:   Cursor := crSizeWE;
      absBottom:  Cursor := crSizeNS;
    else
      Cursor := crDefault;
    end;
  end;

  inherited;
end;

procedure TAbStickyPanel.MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
begin
  FDowned := absNone;
  FMemo.Visible := True;
  inherited;
end;

{$ENDIF}

end.
