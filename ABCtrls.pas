//  Copyright (C) 2000-2005 ABplus kazhida.
//  $Id: ABCtrls.pas 1278 2009-09-01 05:11:44Z kazhida $
//  $Author: kazhida $
//
unit ABCtrls;

interface

{:Note::

  ABplus特製コントロール

  TABBeveledLabel
    --- Bevel付のラベル
                                           
  TABLabeledEdit
    --- TextのAlignmentを変更できるLabeledEdit

  TABEdit
    --- TextのAlignmentを変更できるEdit

  TABSplitter
    --- Caption付のスプリッタ
        Snap,Unsnap時にイベントが発生するようにしてある

  TABLampButton
    --- 背景色が変えられたりするButton

  TABFlickerTimer
    --- TABLampButtonをFlickerさせるTimer

  TABListView
    --- デフォルトの設定が違うListView

  TABLockedTimer
    --- CriticalSectionを持ったタイマー

  TABSidePanel
    --- サイドに置くことに特化したPanel

  TABTopPanel
    --- トップに置くことに特化したPanel

  TABBottomPanel
    --- ボトムに置くことに特化したPanel

::Note:}

uses
  Windows, Messages, Classes, Controls, ExtCtrls, Graphics, 
  Forms, StdCtrls, SyncObjs, ComCtrls;

type

  { TABBeveledLabel }
  TABBeveledLabel = class(TLabel)
  private
    FBevelInner:  TBevelCut;
    FBevelOuter:  TBevelCut;
    FBevelKind:   TBevelKind;
    FBevelWidth:  TBevelWidth;
    FBorderWidth: TBorderWidth;
    procedure SetBevelInner(Value: TBevelCut);
    procedure SetBevelOuter(Value: TBevelCut);
    procedure SetBevelKind(Value: TBevelKind);
    procedure SetBevelWidth(Value: TBevelWidth);
    procedure SetBorderWidth(Value: TBorderWidth);
  protected
    procedure Paint;  override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property BevelInner: TBevelCut     read FBevelInner  write SetBevelInner  default bvNone;
    property BevelOuter: TBevelCut     read FBevelOuter  write SetBevelOuter  default bvNone;
    property BevelKind:  TBevelKind    read FBevelKind   write SetBevelKind   default bkNone;
    property BevelWidth: TBevelWidth   read FBevelWidth  write SetBevelWidth  default 1;
    property BorderWidth: TBorderWidth read FBorderWidth write SetBorderWidth default 0;
    property Alignment default taCenter;
    property Layout    default tlCenter;
  end;

  { TABEdit }

  TABEdit = class(TCustomEdit)
  private
    FAlignment: TAlignment;
    FEditColor: TColor;
    FOrigColor: TColor;
    FEntered:   Boolean;
    FSelectAllAtClick:  Boolean;
    FExitAtCR: Boolean;
    procedure SetAlignment(a: TAlignment);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DoEnter;  override;
    procedure DoExit;   override;
    procedure Click;    override;
    procedure KeyPress(var Key: Char); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Alignment: TAlignment read FAlignment write SetAlignment
      default taLeftJustify;
    property EditColor: TColor read FEditColor write FEditColor default clWindow;
    property SelectAllAtClick: Boolean read FSelectAllAtClick
                                      write FSelectAllAtClick default False;
    property Align;
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    property BevelWidth;
    property BiDiMode;
    property BorderStyle;
    property CharCase;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property OEMConvert;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    property ExitProcAtCR: Boolean read FExitAtCR write FExitAtCR default False;
  end;

  { TABLabeledEdit }

  TABLabeledEdit = class(TCustomLabeledEdit)
  private
    FAlignment: TAlignment;
    FEditColor: TColor;
    FOrigColor: TColor;
    FEntered:   Boolean;
    FSelectAllAtClick:  Boolean;
    FExitAtCR: Boolean;
    procedure SetAlignment(a: TAlignment);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DoEnter;  override;
    procedure DoExit;   override;
    procedure Click;    override;
    procedure KeyPress(var Key: Char); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Alignment: TAlignment read FAlignment write SetAlignment
      default taLeftJustify;
    property EditColor: TColor read FEditColor write FEditColor default clWindow;
    property SelectAllAtClick: Boolean read FSelectAllAtClick
                                      write FSelectAllAtClick default False;
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BevelEdges;
    property BevelInner;
    property BevelKind;
    property BevelOuter;
    property BiDiMode;
    property BorderStyle;
    property CharCase;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property EditLabel;
    property Enabled;
    property Font;
    property HideSelection;
    property ImeMode;
    property ImeName;
    property LabelPosition;
    property LabelSpacing;
    property MaxLength;
    property OEMConvert;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    property ExitProcAtCR: Boolean read FExitAtCR write FExitAtCR default False;
  end;

  { TABSplitter }

  TABSplitter = class(TSplitter)
  private
    FLastSize:    Integer;
    FOnSnaped:    TNotifyEvent;
    FOnUnsnaped:  TNotifyEvent;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure SetFontOrientation;
  protected
    procedure RequestAlign; override;
    function  DoCanResize(var NewSize: Integer): Boolean;  override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint;  override;
  published
    property  Caption;
    property  Font;
    property  OnDblClick;
    property  OnSnaped:   TNotifyEvent  read FOnSnaped    write FOnSnaped;
    property  OnUnsnaped: TNotifyEvent  read FOnUnsnaped  write FOnUnsnaped;
  end;

  { TABLampButton }

  TABFlickerTimer = class;

  TABLampButton = class(TButton)
  private
    FOnColor:     TColor;
    FOffColor:    TColor;
    FFlickColor:  TColor;
    FOnCaption:   String;
    FOffCaption:  String;
    FPressed:     Boolean;
    FWordWrap:    Boolean;
    FIsFocues:    Boolean;
    FCanvas:      TCanvas;
    FTimer:       TABFlickerTimer;
    FFlickering:  Boolean;    //  TFlickerTimerからアクセスされる
    FNotLinkYet:  Boolean;    //  TFlickerTimerとのリンク用
    FGroupIndex:  Integer;
    FBevelWidth:  Integer;
    function  GetColor: TColor;
    procedure SetOnColor(Value: TColor);
    procedure SetOffColor(Value: TColor);
    procedure SetOnCaption(Value: String);
    procedure SetOffCaption(Value: String);
    procedure SetPressed(Value: Boolean);
    procedure SetWordWrap(Value: Boolean);
    procedure SetTimer(Timer: TABFlickerTimer);
    procedure SetBevelWidth(w: Integer);
    procedure CNMeasureItem(var Msg: TWMMeasureItem); message CN_MEASUREITEM;
    procedure CNDrawItem(var Msg: TWMDrawItem);       message CN_DRAWITEM;
    procedure CMEnabledChanged(var Msg: TMessage);    message CM_ENABLEDCHANGED;
    procedure CMFontChanged(var Msg: TMessage);       message CM_FONTCHANGED;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DrawButton(Rect: TRect; ItemState: UInt); virtual;
    procedure SetButtonStyle(ADefault: Boolean);  override;
    procedure Loaded; override;
    procedure SetColor(Value: TColor);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy;  override;
    procedure Click;  override;
    property  Canvas: TCanvas read FCanvas;
  published
    property  Color:        TColor read GetColor    write SetColor    default clBtnFace;
    property  OnColor:      TColor read FOnColor    write SetOnColor  default clBtnFace;
    property  OffColor:     TColor read FOffColor   write SetOffColor default clBtnFace;
    property  FlickerColor: TColor read FFlickColor write FFlickColor default clBtnFace;
    property  OnCaption:  String read FOnCaption  write SetOnCaption;
    property  OffCaption: String read FOffCaption write SetOffCaption;
    property  Pressed:  Boolean read FPressed  write SetPressed  default False;
    property  WordWrap: Boolean read FWordWrap write SetWordWrap default False;
    property  FlickerTimer:  TABFlickerTimer read FTimer write SetTimer;
    property  GroupIndex: Integer read FGroupIndex write FGroupIndex default 0;
    property  BevelWidth: Integer read FBevelWidth write SetBevelWidth default 2;
  end;

  { TABFlickerTimer }

  TABFlickerTimer = class(TTimer)
  private
    FButtons: TList;
    FLock:    TCriticalSection;
    FFlicker: Boolean;
    function  GetCount: Integer;
    function  GetButton(i: Integer): TABLampButton;
  protected
    procedure Timer;  override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy;  override;
    procedure Add(Button: TABLampButton);
    procedure Remove(Button: TABLampButton);
    property  Count: Integer read GetCount;
  end;

  { TABListView }

  TABListView = class(TListView)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TABLockedTimer }

  TABLockedTimer = class(TTimer)
  private
    FLock:  TCriticalSection;
    FLMode: Boolean;
  protected
    procedure Timer;  override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy;  override;
    procedure Enter;
    procedure Leave;
  published
    property  LockingMode: Boolean read FLMode write FLMode default False;
  end;

  { TABSidePanel   }
  { TABBottomPanel }

  TABSideAlign = (saLeft, saRight);

  TABSnapPanel = class(TCustomPanel)
  private
    FAlignment: TAlignment;
    FAutoSnap: Boolean;
    FBrush: TBrush;
    FDownPos: TPoint;
    FSplit: Integer;
    FLineDC: HDC;
    FLineVisible: Boolean;
    FMinSize: NaturalNumber;
    FMaxSize: Integer;
    FOldSize: Integer;
    FNewSize: Integer;
    FPrevBrush: HBrush;
    FBackCap: String;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure SetAlignment(AValue: TAlignment);
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Paint;  override;
    procedure ReleaseLineDC;
    procedure AllocateLineDC;
    procedure PaintCaption;                   virtual;  abstract;
    procedure DrawLine;                       virtual;  abstract;
    function  IsSnapped: Boolean;             virtual;  abstract;
    procedure DoSnap(var NewSize: Integer);   virtual;  abstract;
    procedure CalcSplitSize(X, Y: Integer;
      var NewSize, Split: Integer);           virtual;  abstract;
    function  CalcSplitRect: TRect;           virtual;  abstract;
    function  CalcMaxSize:  Integer;          virtual;  abstract;
    procedure UpdateControlSize;              virtual;  abstract;
  public
    constructor Create(AOwner: TComponent);   override;
    destructor  Destroy;                      override;
    procedure   Snap;                         virtual;  abstract;
    procedure   Unsnap;                       virtual;  abstract;
    property    Snapped: Boolean read IsSnapped;
  published
    property  Alignment: TAlignment read FAlignment write SetAlignment default taCenter;
    property  BevelEdges;
    property  BevelInner;
    property  BevelKind;
    property  BevelOuter;
    property  BevelWidth;
    property  BiDiMode;
    property  BorderWidth;
    property  BorderStyle;
    property  Caption;
    property  Color;
    property  Constraints;
    property  Ctl3D;
    property  Enabled;
    property  FullRepaint;
    property  Font;
    property  Locked;
    property  Padding;
    property  ParentBiDiMode;
    property  ParentBackground;
    property  ParentColor;
    property  ParentCtl3D;
    property  ParentFont;
    property  ParentShowHint;
    property  PopupMenu;
    property  ShowHint;
    property  TabOrder;
    property  TabStop;
    property  Visible;
    property  OnAlignInsertBefore;
    property  OnAlignPosition;
    property  OnCanResize;
    property  OnClick;
    property  OnConstrainedResize;
    property  OnContextPopup;
    property  OnDblClick;
    property  OnDragDrop;
    property  OnDragOver;
    property  OnEndDrag;
    property  OnEnter;
    property  OnExit;
    property  OnGetSiteInfo;
    property  OnMouseActivate;
    property  OnMouseDown;
    property  OnMouseEnter;
    property  OnMouseLeave;
    property  OnMouseMove;
    property  OnMouseUp;
    property  OnResize;
    property  OnStartDrag;
    property  AutoSnap: Boolean read FAutoSnap write FAutoSnap default True;
    property  MinSize: NaturalNumber read FMinSize write FMinSize default 30;
  end;

  TABSidePanel = class(TABSnapPanel)
  private
    FSideWidth: Integer;
    procedure SetSideAlign(AValue: TABSideAlign);
    function  GetSideAlign: TABSideAlign;
    procedure SetSideWidth(AVAlue: Integer);
    procedure SetFontOrientation(fnt: TFont);
  protected
    procedure CalcSplitSize(X, Y: Integer;
      var NewSize, Split: Integer);           override;
    function  IsSnapped: Boolean;             override;
    procedure DoSnap(var NewSize: Integer);   override;
    procedure DrawLine;                       override;
    procedure UpdateControlSize;              override;
    procedure PaintCaption;                   override;
    function  CalcSplitRect: TRect;           override;
    function  CalcMaxSize:  Integer;          override;
  published
    constructor Create(AOwner: TComponent);   override;
    procedure Snap;                           override;
    procedure Unsnap;                         override;
    property  SideWidth:  Integer read FSideWidth write SetSideWidth default 15;
    property  SideAlign:  TABSideAlign read GetSideAlign write SetSideAlign;
  end;

  TABTopPanel = class(TABSnapPanel)
  private
    FBtmHeight: Integer;
    FBtmEdge:   Boolean;
    procedure SetBtmHeight(AValue: Integer);
    procedure SetBtmEdge(AValue: Boolean);
  protected
    procedure CalcSplitSize(X, Y: Integer;
      var NewSize, Split: Integer);           override;
    function  IsSnapped: Boolean;             override;
    procedure DoSnap(var NewSize: Integer);   override;
    procedure DrawLine;                       override;
    procedure UpdateControlSize;              override;
    procedure PaintCaption;                   override;
    function  CalcSplitRect: TRect;           override;
    function  CalcMaxSize:  Integer;          override;
  published
    constructor Create(AOwner: TComponent);   override;
    procedure Snap;                           override;
    procedure Unsnap;                         override;
    property  BottomHeight: Integer read FBtmHeight write SetBtmHeight  default 15;
    property  BottomEdge:  Boolean  read FBtmEdge   write SetBtmEdge    default False;
  end;

  TABBottomPanel = class(TABSnapPanel)
  private
    FTopHeight: Integer;
    FTopEdge:   Boolean;
    procedure SetTopHeight(AValue: Integer);
    procedure SetTopEdge(AValue: Boolean);
  protected
    procedure CalcSplitSize(X, Y: Integer;
      var NewSize, Split: Integer);           override;
    function  IsSnapped: Boolean;             override;
    procedure DoSnap(var NewSize: Integer);   override;
    procedure DrawLine;                       override;
    procedure UpdateControlSize;              override;
    procedure PaintCaption;                   override;
    function  CalcSplitRect: TRect;           override;
    function  CalcMaxSize:  Integer;          override;
  published
    constructor Create(AOwner: TComponent);   override;
    procedure Snap;                           override;
    procedure Unsnap;                         override;
    property  TopHeight: Integer read FTopHeight write SetTopHeight default 15;
    property  TopEdge:  Boolean read FTopEdge write SetTopEdge default False;
  end;

procedure Register;

////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////
implementation

uses
  SysUtils;

function Min255(x: Integer): Integer;
begin
  if x < 255 then
    Result := x
  else
    Result := 255;
end;

function Max0(x: Integer): Integer;
begin
  if x > 0 then
    Result := x
  else
    Result := 0;
end;

function LightColor(c: TColor; offs: Integer = 128): TColor;
var
  crgb:  DWORD;
begin
  crgb := DWORD(ColorToRGB(c));
  Result := TColor(RGB(Min255(GetRValue(crgb) + offs),
                       Min255(GetGValue(crgb) + offs),
                       Min255(GetBValue(crgb) + offs)));
end;

function DarkColor(c: TColor; offs: Integer = 128): TColor;
var
  crgb:  DWORD;
begin
  crgb := DWORD(ColorToRGB(c));
  Result := TColor(RGB(Max0(GetRValue(crgb) - offs),
                       Max0(GetGValue(crgb) - offs),
                       Max0(GetBValue(crgb) - offs)));
end;


////////////////////////////////////////////////////////////////////
{ TABBeveledLabel }

constructor TABBeveledLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBevelInner  := bvNone;
  FBevelOuter  := bvNone;
  FBevelKind   := bkNone;
  FBevelWidth  := 1;
  FBorderWidth := 0;
  //  Captionは中央
  Alignment := taCenter;
  Layout    := tlCenter;
end;

procedure TABBeveledLabel.SetBevelInner(Value: TBevelCut);
begin
  if FBevelInner <> Value then
  begin
    FBevelInner := Value;
    Invalidate;
  end;
end;

procedure TABBeveledLabel.SetBevelOuter(Value: TBevelCut);
begin
  if FBevelOuter <> Value then
  begin
    FBevelOuter:= Value;
    Invalidate;
  end;
end;

procedure TABBeveledLabel.SetBevelKind(Value: TBevelKind);
begin
  if FBevelKind <> Value then
  begin
    FBevelKind := Value;
    Invalidate;
  end;
end;

procedure TABBeveledLabel.SetBevelWidth(Value: TBevelWidth);
begin
  if FBevelWidth <> Value then
  begin
    FBevelWidth := Value;
    Invalidate;
  end;
end;

procedure TABBeveledLabel.SetBorderWidth(Value: TBorderWidth);
begin
  if FBorderWidth <> Value then
  begin
    FBorderWidth := Value;
    Invalidate;
  end;
end;

procedure TABBeveledLabel.Paint;

  procedure DrawRaized(var r: TRect);
  begin
    Frame3D(Canvas, r, LightColor(Color), DarkColor(Color), FBevelWidth);
  end;

  procedure DrawLowered(var r: TRect);
  begin
    Frame3D(Canvas, r, DarkColor(Color), LightColor(Color), FBevelWidth);
  end;

var
  r:  TRect;
begin
  inherited Paint;

  r := ClientRect;

  case FBevelOuter of
    bvLowered: DrawLowered(r);
    bvRaised:  DrawRaized(r);
    bvSpace:   DrawRaized(r);
  end;

  InflateRect(r, -FBorderWidth, -FBorderWidth);

  case FBevelInner of
    bvLowered: DrawLowered(r);
    bvRaised:  DrawRaized(r);
    bvSpace:   DrawRaized(r);
  end;
end;


////////////////////////////////////////////////////////////////////
{ TABEdit }

constructor TABEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAlignment := taLeftJustify;
  FEditColor := clWindow;
  FSelectAllAtClick := False;
end;

procedure TABEdit.SetAlignment(a: TAlignment);
begin
  if FAlignment <> a then
  begin
    FAlignment := a;
    RecreateWnd;
  end;
end;

procedure TABEdit.CreateParams(var Params: TCreateParams);
const
  Alignments: array[Boolean, TAlignment] of DWORD = (
    (ES_LEFT, ES_RIGHT, ES_CENTER),
    (ES_RIGHT, ES_LEFT, ES_CENTER)
  );
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or Alignments[UseRightToLeftAlignment, FAlignment];
  end;
end;

procedure TABEdit.DoEnter;
begin
  inherited;
  if not ReadOnly then
  begin
    FOrigColor := Color;
    Color := FEditColor;
    FEntered := True;
  end;
end;

procedure TABEdit.DoExit;
begin
  if not ReadOnly then
  begin
    Color := FOrigColor;
  end;
  inherited;
end;

procedure TABEdit.KeyPress(var Key: Char);
begin
  if FExitAtCR and Assigned(OnExit) and (Key = #13) then
  begin
    OnExit(Self);
    Key := #0;
  end;
  inherited KeyPress(Key);
end;


procedure TABEdit.Click;
begin
  inherited;
  if FSelectAllAtClick and FEntered then SelectAll;
  FEntered := False;
end;

////////////////////////////////////////////////////////////////////
{ TABLabeledEdit }

constructor TABLabeledEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  LabelPosition := lpLeft;
  FAlignment := taLeftJustify;
  FEditColor := clWindow;
  FSelectAllAtClick := False;
end;

procedure TABLabeledEdit.SetAlignment(a: TAlignment);
begin
  if FAlignment <> a then
  begin
    FAlignment := a;
    RecreateWnd;
  end;
end;

procedure TABLabeledEdit.CreateParams(var Params: TCreateParams);
const
  Alignments: array[Boolean, TAlignment] of DWORD = (
    (ES_LEFT, ES_RIGHT, ES_CENTER),
    (ES_RIGHT, ES_LEFT, ES_CENTER)
  );
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or Alignments[UseRightToLeftAlignment, FAlignment];
  end;
end;

procedure TABLabeledEdit.DoEnter;
begin
  inherited DoEnter;
  if not ReadOnly then
  begin
    FOrigColor := Color;
    Color := FEditColor;
    FEntered := True;
  end;
end;

procedure TABLabeledEdit.DoExit;
begin
  if not ReadOnly then
  begin
    Color := FOrigColor;
  end;
  inherited DoExit;
end;

procedure TABLabeledEdit.KeyPress(var Key: Char);
begin
  if FExitAtCR and Assigned(OnExit) and (Key = #13) then
  begin
    OnExit(Self);
    Key := #0;
  end;
  inherited KeyPress(Key);
end;

procedure TABLabeledEdit.Click;
begin
  inherited;
  if FSelectAllAtClick and FEntered then SelectAll;
  FEntered := False;
end;

////////////////////////////////////////////////////////////////////
{ TABSplitter }

constructor TABSplitter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLastSize := MinSize;
end;

procedure TABSplitter.Loaded;
begin
  inherited Loaded;
  FLastSize := MinSize;
end;

procedure TABSplitter.SetFontOrientation;
var
  LF: TLogFont;
  TF: TFont;
begin
  with Canvas do
  begin
    TF := TFont.Create;
    try
      TF.Assign(Font);
      GetObject(TF.Handle,SizeOf(LF),@LF);
      if Align in [alLeft, alRight] then
        LF.lfEscapement := 900
      else
        LF.lfEscapement := 0;
      LF.lfOrientation := LF.lfEscapement;
      TF.Handle := CreateFontIndirect(LF);
      Font.Assign(TF);
    finally
      TF.Free;
    end;
  end;
end;

procedure TABSplitter.RequestAlign;
begin
  inherited RequestAlign;
  SetFontOrientation;
end;

procedure TABSplitter.Paint;
var
  TmpOnPaint: TNotifyEvent;
  R:    TRect;
  W, H: Integer;
  X, Y: Integer;
begin
  TmpOnPaint := OnPaint;
  OnPaint := nil;
  inherited Paint;
  OnPaint := TmpOnPaint;
  if Assigned(OnPaint) then
    OnPaint(Self)
  else  with Canvas do
  begin
    R := ClientRect;
    if Align in [alLeft, alRIght] then
    begin
      H := TextWidth(Caption);
      W := TextHeight(Caption);
      X := R.Left    + (R.Right - R.Left - W) div 2;
      Y := R.Bottom  - (R.Bottom - R.Top - H) div 2;
    end
    else
    begin
      W := TextWidth(Caption);
      H := TextHeight(Caption);
      X := R.Left + (R.Right - R.Left - W) div 2;
      Y := R.Top  + (R.Bottom - R.Top - H) div 2;
    end;
    //Brush.Style := bsClear;
    TextRect(R, X, Y, Caption);
  end;
end;

function TABSplitter.DoCanResize(var NewSize: Integer): Boolean;
begin
  Result := inherited DoCanResize(NewSize);
  if Result then
  begin
    if (NewSize = 0) and (FLastSize > 0) and Assigned(FOnSnaped) then
      FOnSnaped(Self);

    if (NewSize > 0) and (FLastSize = 0) and Assigned(FOnUnsnaped) then
      FOnUnsnaped(Self);

    FLastSize := NewSize;
  end;
end;

procedure TABSplitter.CMTextChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TABSplitter.CMFontChanged(var Message: TMessage);
begin
  inherited;
  Canvas.Font.Assign(Self.Font);
  SetFontOrientation;
  Invalidate;
end;

/////////////////////////////////////////
{ TABLampButton }

var
  GLampButtons: TList;

procedure LampGroupAutoOff(Sender: TABLampButton);
var
  i:    Integer;
  idx:  Integer;
  btn:  TABLampButton;
begin
  idx := Sender.GroupIndex;
  if idx < 1 then Exit;

  for i := 0 to GLampButtons.Count - 1 do
  begin
    btn := TABLampButton(GLampButtons.Items[i]);
    if (btn <> Sender) and (btn.GroupIndex = idx) then
      btn.Pressed := False;
  end;
end;

constructor TABLampButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FOnColor    := clBtnFace;
  FOffColor   := clBtnFace;
  FFlickColor := clBtnFace;
  FOnCaption  := '';
  FOffCaption := '';
  FPressed    := False;
  FWordWrap   := False;
  FIsFocues   := False;
  FFlickering := False;
  FNotLinkYet := False;
  FGroupIndex := 0;
  FBevelWidth := 2;

  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;

  GLampButtons.Add(Self);
end;

destructor  TABLampButton.Destroy;
begin
  GLampButtons.Remove(Self);

  SetTimer(nil);
  FCanvas.Free;
  inherited Destroy;
end;

procedure TABLampButton.Click;
begin
  if (FOnColor <> FOffColor) or (FOnCaption <> FOffCaption) then
  begin
    if FGroupIndex <> 0 then
      SetPressed(True)
    else
      SetPressed(not FPressed);
    Update;
  end;
  inherited Click;
end;

procedure TABLampButton.SetTimer(Timer: TABFlickerTimer);
begin
  if FTimer <> Timer then
  begin
    if Assigned(FTimer) then
      FTimer.Remove(Self);
    FTimer := Timer;
    if Assigned(FTimer) then
    begin
      if csFixups	in ComponentState then
        FNotLinkYet := True
      else
        FTimer.Add(Self);
    end;
  end;
end;

procedure TABLampButton.Loaded;
begin
  inherited Loaded;
  if Assigned(FTimer) and FNotLinkYet then
    FTimer.Add(Self);
  FNotLinkYet := False;
  SetColor(Color);
end;

function  TABLampButton.GetColor: TColor;
begin
  Result := inherited Color;
end;

procedure TABLampButton.SetColor(Value: TColor);
begin
  if (not (csLoading in ComponentState)) and (FOnColor <> FOffColor) then
  begin
    if FFlickering then
      Value := FFlickColor
    else if FPressed then
      Value := FOnColor
    else
      Value := FOffColor;
    if (FOnCaption <> '') or (FOffCaption <> '') then
    begin
      if FPressed then
        Caption := FOnCaption
      else
        Caption := FOffCaption;
    end;
  end;
  inherited Color := Value;   //  必要があれば、ここで再描画されるはず。
end;

procedure TABLampButton.SetOnColor(Value: TColor);
begin
  if FOnColor <> Value then
  begin
    FOnColor := Value;
    SetColor(Color);
  end;
end;

procedure TABLampButton.SetOffColor(Value: TColor);
begin
  if FOffColor <> Value then
  begin
    FOffColor := Value;
    SetColor(Color);
  end;
end;

procedure TABLampButton.SetOnCaption(Value: String);
begin
  if FOnCaption <> Value then
  begin
    FOnCaption := Value;
    SetColor(Color);
  end;
end;

procedure TABLampButton.SetOffCaption(Value: String);
begin
  if FOffCaption <> Value then
  begin
    FOffCaption := Value;
    SetColor(Color);
  end;
end;

procedure TABLampButton.SetPressed(Value: Boolean);
begin
  if FPressed <> Value then
  begin
    FPressed := Value;
    SetColor(Color);
    //  自分以外はOFF
    if FPressed then
      LampGroupAutoOff(Self);
  end;
end;

procedure TABLampButton.SetWordWrap(Value: Boolean);
begin
  if FWordWrap <> Value then
  begin
    FWordWrap := Value;
    Invalidate;
  end;
end;

procedure TABLampButton.SetButtonStyle(ADefault: Boolean);
begin
  if FIsFocues <> ADefault then
  begin
    FIsFocues := ADefault;
    Refresh;
  end;
end;

procedure TABLampButton.SetBevelWidth(w: Integer);
begin
  if FBevelWidth <> w then
  begin
    FBevelWidth := w;
    Invalidate;
  end;
end;


procedure TABLampButton.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or BS_OWNERDRAW;
end;

procedure TABLampButton.CNMeasureItem(var Msg: TWMmeasureItem);
begin
  msg.MeasureItemStruct.itemWidth  := Width;
  msg.MeasureItemStruct.itemHeight := Height;
end;

procedure TABLampButton.CNDrawItem(var Msg: TWMDrawItem);
var
  dc: Integer;
begin
  with Msg.DrawItemStruct^ do
  begin
    dc := SaveDC(hDC);
    FCanvas.Handle := hDC;
    DrawButton(rcItem, itemState);
    FCanvas.Handle := 0;
    RestoreDC(hDC, dc);
  end;
  Msg.Result := 1;
end;

procedure TABLampButton.CMEnabledChanged(var Msg: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TABLampButton.CMFontChanged(var Msg: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TABLampButton.DrawButton(Rect: TRect; ItemState: UInt);
var
  isDown:   Boolean;
  isFocus:  Boolean;

  procedure DrawCaption(rect: TRect; offs: Integer);
  var
    s:  String;
    t:  UInt;
    r:  TRect;
    h:  THandle;
  begin
    s := Caption;
    t := DT_EXPANDTABS or DT_CENTER;
    if FWordWrap then
      t := t or DT_WORDBREAK;
    r := rect;
    h := FCanvas.Handle;
    //  描画範囲を計算
    r.Right  := r.Right  - 2;
    r.Bottom := r.Bottom - 2;
    DrawText(h, PChar(s), -1, r, t or DT_CALCRECT);
    InflateRect(rect, -1, (r.Bottom - rect.Bottom) div 2);
    OffsetRect(rect, offs, offs);
    //  描画
    DrawText(h, PChar(s), -1, rect, t);
  end;

  procedure DrawFocusRect2(rect: TRect);
  begin
    InflateRect(rect, -2, -2);
    FCanvas.DrawFocusRect(rect);
  end;

begin
  SetColor(Color);    //  背景色の決定

  isDown  := (ItemState and ODS_SELECTED) <> 0;
  isFocus := (ItemState and ODS_FOCUS)    <> 0;

  with FCanvas do
  begin
    Font.Assign(Self.Font);
    Brush.Color := Color;
    Brush.Style := bsSolid;
    FillRect(rect);
    //  DefaultButtonのとき
    if FIsFocues or isFocus then
    begin
      Brush.Color := DarkColor(Color, 192);
      FrameRect(rect);
      InflateRect(rect, -1, -1);
    end;
    //  へこみ、出っ張り
    if FBevelWidth <> 0 then
    begin
      if isDown then
        Frame3D(FCanvas, rect, DarkColor(Color), LightColor(Color), FBevelWidth)
      else
        Frame3D(FCanvas, rect, LightColor(Color), DarkColor(Color), FBevelWidth);
    end;
    //  FocusRectを描く
    if FIsFocues and isFocus then                                     
    begin
      Pen.Color   := clWindowFrame;
      Brush.Color := Color;
      DrawFocusRect2(rect);
    end;
    //  Captionを描く
    Brush.Color := Color;
    if isDown then
      OffsetRect(rect, 1, 1);
    if Enabled then
      DrawCaption(rect, 0)
    else
    begin
      //  Disabledなとき
      Brush.Style := bsClear;
      Font.Color := LightColor(Color);
      DrawCaption(rect, 1);
      Font.Color := DarkColor(Color, 64);
      DrawCaption(rect, 0);
    end;
  end;
end;


/////////////////////////////////////////
{ TABFlickerTimer }

constructor TABFlickerTimer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FButtons := TList.Create;
  FLock    := TCriticalSection.Create;
  FFlicker := False;
end;

destructor  TABFlickerTimer.Destroy;
var
  i:  Integer;
begin
  FLock.Enter;
  try
    for i := 0 to FButtons.Count - 1 do
      GetButton(i).FlickerTimer := nil;
  finally
    FLock.Leave;
  end;
  inherited Destroy;
end;

function  TABFlickerTimer.GetCount: Integer;
begin
  FLock.Enter;
  try
    Result := FButtons.Count;
  finally
    FLock.Leave;
  end;
end;

function  TABFlickerTimer.GetButton(i: Integer): TABLampButton;
begin
  Result := TABLampButton(FButtons.Items[i]);
end;

procedure TABFlickerTimer.Add(Button: TABLampButton);
begin
  FLock.Enter;
  try
    FButtons.Add(Button);
  finally
    FLock.Leave;
  end;
end;

procedure TABFlickerTimer.Remove(Button: TABLampButton);
begin
  FLock.Enter;
  try
    FButtons.Remove(Button);
  finally
    FLock.Leave;
  end;
end;

procedure TABFlickerTimer.Timer;
var
  i:  Integer;
begin
  for i := 0 to FButtons.Count - 1 do
  begin
    FFlicker := not FFlicker;
    GetButton(i).FFlickering := FFlicker;
    GetButton(i).Invalidate;
  end;
  inherited Timer;
end;

/////////////////////////////////////////
{ TABListView }

constructor TABListView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ReadOnly  := True;
  RowSelect := True;
  ViewStyle := vsReport;
  Columns.Add;    //  カラムを１こ作っておく
end;

/////////////////////////////////////////
{ TABLockedTimer }

constructor TABLockedTimer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLock  := TCriticalSection.Create;
  FLMode := False;
end;

destructor  TABLockedTimer.Destroy;
begin
  FLock.Free;
  inherited Destroy;
end;

procedure TABLockedTimer.Timer;
var
  lk: Boolean;
begin
  lk := FLMode;
  if lk then FLock.Enter;
  try
    inherited Timer;
  finally
    if lk then FLock.Leave;
  end;
end;

procedure TABLockedTimer.Enter;
begin
  FLock.Enter;
end;

procedure TABLockedTimer.Leave;
begin
  FLock.Leave;
end;

////////////////////////////////////////////////////////////////////
{ TABSnapPanel }

constructor TABSnapPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAutoSnap := True;
  Height := 100;
  Margins.Top := 0;
  Margins.Left := 0;
  Margins.Right := 0;
  Margins.Bottom := 0;
  Padding.Top := 0;
  Padding.Left := 0;
  Padding.Right := 0;
  Padding.Bottom := 0;

  FOldSize   := -1;
  FMinSize   := 30;
  FAutoSnap  := True;
  FAlignment := taCenter;
end;

destructor  TABSnapPanel.Destroy;
begin
  FBrush.Free;
  inherited Destroy;
end;

procedure TABSnapPanel.AllocateLineDC;
begin
  FLineDC := GetDCEx(Parent.Handle, 0, DCX_CACHE or DCX_CLIPSIBLINGS
    or DCX_LOCKWINDOWUPDATE);
  if FBrush = nil then
  begin
    FBrush := TBrush.Create;
    FBrush.Bitmap := AllocPatternBitmap(clBlack, clWhite);
  end;
  FPrevBrush := SelectObject(FLineDC, FBrush.Handle);
end;

procedure TABSnapPanel.ReleaseLineDC;
begin
  if FPrevBrush <> 0 then
    SelectObject(FLineDC, FPrevBrush);
  ReleaseDC(Parent.Handle, FLineDC);
  if FBrush <> nil then
  begin
    FBrush.Free;
    FBrush := nil;
  end;
end;

procedure TABSnapPanel.Paint;
const
  XorColor = $00FFD8CE;
begin
  FBackCap := Caption;
  try
    Caption  := '';
    inherited Paint;
  finally
    Caption  := FBackCap;
    FBackCap := '';
  end;

  Canvas.Pen.Color    := Self.Color;
  Canvas.Brush.Color  := Self.Color;
  Canvas.Brush.Style  := bsSolid;
  Canvas.FillRect(CalcSplitRect);

  PaintCaption;

  if csDesigning in ComponentState then
  begin
    Canvas.Pen.Style    := psDot;
    Canvas.Pen.Mode     := pmXor;
    Canvas.Pen.Color    := XorColor;
    Canvas.Brush.Style  := bsClear;
    Canvas.Rectangle(CalcSplitRect);
  end;
end;

procedure TABSnapPanel.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);

begin
  inherited MouseDown(Button, Shift, X, Y);

  if Button = mbLeft then
  begin
    FDownPos := Point(X, Y);
    FMaxSize := CalcMaxSize;
    CalcSplitSize(X, Y, FNewSize, FSplit);
    AllocateLineDC;
    DrawLine;
  end;
end;

procedure TABSnapPanel.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  NewSize:    Integer;
  Split:      Integer;
  NewHeight:  Integer;    //dummy
begin
  inherited;

  if (ssLeft in Shift) then
  begin
    CalcSplitSize(X, Y, NewSize, Split);
    if CanResize(NewSize, NewHeight) then
    begin
      if (NewSize <= MinSize) and FAutoSnap then
        DoSnap(NewSize);
      DrawLine;
      FNewSize := NewSize;
      FSplit := Split;
      DrawLine;
    end;
  end;
end;

procedure TABSnapPanel.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  DrawLine;
  UpdateControlSize;
  if FLineVisible then DrawLine;
  ReleaseLineDC;
  if Snapped then
    Realign;
end;

procedure TABSnapPanel.CMTextChanged(var Message: TMessage);
begin
  if FBackCap = '' then
    inherited;
end;

procedure TABSnapPanel.CMFontChanged(var Message: TMessage);
begin
  inherited;
  Canvas.Font.Assign(Self.Font);
  Invalidate;
end;

procedure TABSnapPanel.SetAlignment(AValue: TAlignment);
begin
  if FAlignment <> AValue then
  begin
    FAlignment := AValue;
    Invalidate;
  end;
end;


////////////////////////////////////////////////////////////////////
{ TABSidePanel }

constructor TABSidePanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Align := alRight;
  Cursor := crHSplit;
  FSideWidth    := 15;
  Padding.Left  := FSideWidth;
  Padding.Right := 0;
end;

function  TABSidePanel.GetSideAlign: TABSideAlign;
begin
  if Align = alLeft then
    Result := saLeft
  else
    Result := saRight;
end;

procedure TABSidePanel.SetSideAlign(AValue: TABSideAlign);
var
  needRealign: Boolean;
begin
  needRealign := SideAlign <> AValue;
  if AValue = saLeft then
  begin
    Align := alLeft;
    Padding.Left := 0;
    Padding.Right := FSideWidth;
  end
  else
  begin
    Align := alRight;
    Padding.Left := FSideWidth;
    Padding.Right := 0;
  end;
  if needRealign then
    Realign;
  Invalidate;
end;

procedure TABSidePanel.SetSideWidth(AVAlue: Integer);
begin
  if FSideWidth <> AVAlue then
  begin
    FSideWidth := AVAlue;
    Constraints.MinWidth := FSideWidth;
    if Align = alLeft then
    begin
      Padding.Left  := 0;
      Padding.Right := FSideWidth;
    end
    else
    begin
      Padding.Left  := FSideWidth;
      Padding.Right := 0;
    end;
    Invalidate;
  end;
end;

procedure TABSidePanel.SetFontOrientation(fnt: TFont);
var
  LF: TLogFont;
  TF: TFont;
begin
  TF := TFont.Create;
  try
    TF.Assign(fnt);
    GetObject(TF.Handle,SizeOf(LF),@LF);
    if LF.lfEscapement <> 900 then
    begin
      LF.lfEscapement := 900;
      LF.lfOrientation := LF.lfEscapement;
      TF.Handle := CreateFontIndirect(LF);
      fnt.Assign(TF);
    end;
  finally
    TF.Free;
  end;
end;

procedure TABSidePanel.DrawLine;
var
  P: TPoint;
begin
  FLineVisible := not FLineVisible;
  P.X := Left + FSplit;
  p.Y := Top;
  if SideAlign = saLeft then
    Inc(p.X, Width - FSideWidth);
  PatBlt(FLineDC, p.X, p.Y, FSideWidth, Height, PATINVERT);
end;


function  TABSidePanel.IsSnapped: Boolean;
begin
  Result := Width = FSideWidth;
end;

procedure   TABSidePanel.Snap;
begin
  if FLineVisible then DrawLine;
  Width := FSideWidth;
end;

procedure   TABSidePanel.Unsnap;
begin
  if FLineVisible then DrawLine;
  if Width < FMinSize then
    Width := FMinSize;
end;

procedure TABSidePanel.CalcSplitSize(X, Y: Integer; var NewSize, Split: Integer);
var
  S: Integer;
begin
  Split := X - FDownPos.X;
  if SideAlign = saLeft then
    S := Width + Split
  else
    S := Width - Split;
  NewSize := S;
  if S < FMinSize then
    NewSize := FMinSize
  else if S > FMaxSize then
    NewSize := FMaxSize;
  if S <> NewSize then
  begin
    if SideAlign = saLeft then
      S := NewSize - S
    else
      S := S - NewSize;
    Inc(Split, S);
  end;
end;

procedure TABSidePanel.DoSnap(var NewSize: Integer);
begin
  if SideAlign = saLeft then
    NewSize := 0
  else
    NewSize := FSideWidth;
end;

function  TABSidePanel.CalcSplitRect: TRect;
begin
  if SideAlign = saLeft then
    Result := Rect(Width - FSideWidth, 0, Width, ClientHeight)
  else
    Result := Rect(0, 0, FSideWidth, ClientHeight);
end;

procedure TABSidePanel.PaintCaption;
var
  p:  TPoint;
begin
  SetFontOrientation(Canvas.Font);

  p.X := FSideWidth div 2;
  if SideAlign = saLeft then
    p.X := ClientWidth - p.X;
  p.X := p.X - Canvas.TextHeight(Caption) div 2;

  if FAlignment = taLeftJustify then
    p.Y := ClientHeight - 4
  else if FAlignment = taRightJustify then
    p.Y := Canvas.TextWidth(Caption) + 4
  else
    p.Y := (ClientHeight + Canvas.TextWidth(Caption)) div 2;

  Canvas.TextOut(p.X, p.Y, Caption);
end;

procedure TABSidePanel.UpdateControlSize;
begin
  if FNewSize <> FOldSize then
  begin
    if SideAlign = saLeft then
    begin
      if FNewSize < FSideWidth then
        Width := FSideWidth
      else
        Width := FNewSize;
    end
    else
    begin
      Parent.DisableAlign;
      try
        Left := Left + (Width - FNewSize);
        Width := FNewSize;
      finally
        Parent.EnableAlign;
      end;
    end;
    Update;
    FOldSize := FNewSize;
  end;
end;

function  TABSidePanel.CalcMaxSize:  Integer;
var
  i: Integer;
begin
  Result := Parent.ClientWidth;
  for i := 0 to Parent.ControlCount - 1 do
    if Parent.Controls[i] <> Self then
      if Parent.Controls[i].Visible then
        if Parent.Controls[i].Align in [alLeft, alRight] then
          Dec(Result, Parent.Controls[i].Width);
end;

////////////////////////////////////////////////////////////////////
{ TABBottomPanel }

constructor TABBottomPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Cursor := crVSplit;

  FTopHeight     := 15;
  Padding.Top    := FTopHeight;
  Padding.Bottom := 0;
  Align := alBottom;
end;

procedure TABBottomPanel.SetTopHeight(AValue: Integer);
begin
  if FTopHeight <> AValue then
  begin
    FTopHeight := AValue;
    Constraints.MinHeight := FTopHeight;
    Padding.Top    := FTopHeight;
    Padding.Bottom := 0;
    Invalidate;
  end;
end;

procedure TABBottomPanel.SetTopEdge(AValue: Boolean);
begin
  if FTopEdge <> AValue then
  begin
    FTopEdge := AValue;
    Invalidate;
  end;
end;

procedure TABBottomPanel.DrawLine;
var
  P: TPoint;
begin
  FLineVisible := not FLineVisible;
  P.X := Left;
  p.Y := Top + FSplit;
  PatBlt(FLineDC, p.X, p.Y, Width, FTopHeight, PATINVERT);
end;


function  TABBottomPanel.IsSnapped: Boolean;
begin
  Result := Height = FTopHeight;
end;

procedure   TABBottomPanel.Snap;
begin
  if FLineVisible then DrawLine;
  Height := FTopHeight;
end;

procedure   TABBottomPanel.Unsnap;
begin
  if FLineVisible then DrawLine;
  if Height < FMinSize then
    Height := FMinSize;
end;

procedure TABBottomPanel.DoSnap(var NewSize: Integer);
begin
  NewSize := FTopHeight;
end;

procedure TABBottomPanel.CalcSplitSize(X, Y: Integer; var NewSize, Split: Integer);
var
  S: Integer;
begin
  Split := Y - FDownPos.Y;
  S := Height - Split;
  NewSize := S;
  if S < FMinSize then
    NewSize := FMinSize
  else if S > FMaxSize then
    NewSize := FMaxSize;
  if S <> NewSize then
  begin
    S := S - NewSize;
    Inc(Split, S);
  end;
end;

function  TABBottomPanel.CalcSplitRect: TRect;
begin
  Result := Rect(0, 0, ClientWidth, FTopHeight);

  if FTopEdge then
  begin
    Result.Top    := Result.Top    + BevelWidth;
    Result.Bottom := Result.Bottom - BevelWidth;
  end;
end;

procedure TABBottomPanel.PaintCaption;
var
  p:  TPoint;
begin
  p.Y := (FTopHeight - Canvas.TextHeight(Caption)) div 2;

  if FAlignment = taLeftJustify then
    p.X := 4
  else if FAlignment = taRightJustify then
    p.X := ClientWidth - Canvas.TextWidth(Caption) - 4
  else
    p.X := (ClientWidth - Canvas.TextWidth(Caption)) div 2;

  Canvas.TextOut(p.X, p.Y, Caption);
end;

procedure TABBottomPanel.UpdateControlSize;
begin
  if FNewSize <> FOldSize then
  begin
    Parent.DisableAlign;
    try
      Top := Top + (Height - FNewSize);
      Height := FNewSize;
    finally
      Parent.EnableAlign;
    end;
    Update;
    FOldSize := FNewSize;
  end;
end;

function  TABBottomPanel.CalcMaxSize:  Integer;
var
  i: Integer;
begin
  Result := Parent.ClientHeight;
  for i := 0 to Parent.ControlCount - 1 do
    if Parent.Controls[i] <> Self then
      if Parent.Controls[i].Visible then
        if Parent.Controls[i].Align in [alTop, alBottom] then
          Dec(Result, Parent.Controls[i].Height);
end;

////////////////////////////////////////////////////////////////////
{ TABTopPanel }

constructor TABTopPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Cursor := crVSplit;

  FBtmHeight     := 15;
  Padding.Bottom := FBtmHeight;
  Padding.Top    := 0;
  Align := alTop;
end;

procedure TABTopPanel.SetBtmHeight(AValue: Integer);
begin
  if FBtmHeight <> AValue then
  begin
    FBtmHeight := AValue;
    Constraints.MinHeight := FBtmHeight;
    Padding.Bottom := FBtmHeight;
    Padding.Top    := 0;
    Invalidate;
  end;
end;

procedure TABTopPanel.SetBtmEdge(AValue: Boolean);
begin
  if FBtmEdge <> AValue then
  begin
    FBtmEdge := AValue;
    Invalidate;
  end;
end;

procedure TABTopPanel.DrawLine;
var
  P: TPoint;
begin
  FLineVisible := not FLineVisible;
  P.X := Left;
  p.Y := Top + Height - FBtmHeight + FSplit;
  PatBlt(FLineDC, p.X, p.Y, Width, FBtmHeight, PATINVERT);
end;


function  TABTopPanel.IsSnapped: Boolean;
begin
  Result := Height = FBtmHeight;
end;

procedure   TABTopPanel.Snap;
begin
  if FLineVisible then DrawLine;
  Height := FBtmHeight;
end;

procedure   TABTopPanel.Unsnap;
begin
  if FLineVisible then DrawLine;
  if Height < FMinSize then
    Height := FMinSize;
end;

procedure TABTopPanel.DoSnap(var NewSize: Integer);
begin
  NewSize := FBtmHeight;
end;

procedure TABTopPanel.CalcSplitSize(X, Y: Integer; var NewSize, Split: Integer);
var
  S: Integer;
begin
  Split := Y - FDownPos.Y;
  S := Height + Split;
  NewSize := S;
  if S < FMinSize then
    NewSize := FMinSize
  else if S > FMaxSize then
    NewSize := FMaxSize;
  if S <> NewSize then
  begin
    S := NewSize - S;
    Inc(Split, S);
  end;
end;

function  TABTopPanel.CalcSplitRect: TRect;
begin
  Result := Rect(0, Height - FBtmHeight, ClientWidth, Height);

  if FBtmEdge then
  begin
    Result.Top    := Result.Top    + BevelWidth;
    Result.Bottom := Result.Bottom - BevelWidth;
  end;
end;

procedure TABTopPanel.PaintCaption;
var
  p:  TPoint;
begin
  p.Y := Height - FBtmHeight + (FBtmHeight - Canvas.TextHeight(Caption)) div 2;

  if FAlignment = taLeftJustify then
    p.X := 4
  else if FAlignment = taRightJustify then
    p.X := ClientWidth - Canvas.TextWidth(Caption) - 4
  else
    p.X := (ClientWidth - Canvas.TextWidth(Caption)) div 2;

  Canvas.TextOut(p.X, p.Y, Caption);
end;

procedure TABTopPanel.UpdateControlSize;
begin
  if FNewSize <> FOldSize then
  begin
    if FNewSize < FBtmHeight then
      Height := FBtmHeight
    else
      Height := FNewSize;
    Update;
    FOldSize := FNewSize;
  end;
end;

function  TABTopPanel.CalcMaxSize:  Integer;
var
  i: Integer;
begin
  Result := Parent.ClientHeight;
  for i := 0 to Parent.ControlCount - 1 do
    if Parent.Controls[i] <> Self then
      if Parent.Controls[i].Visible then
        if Parent.Controls[i].Align in [alTop, alBottom] then
          Dec(Result, Parent.Controls[i].Height);
end;



////////////////////////////////////////////////////////////////////

procedure Register;
begin
  RegisterComponents('ABplus', [TABBeveledLabel]);
  RegisterComponents('ABplus', [TABEdit]);
  RegisterComponents('ABplus', [TABLabeledEdit]);
  RegisterComponents('ABplus', [TABSplitter]);
  RegisterComponents('ABplus', [TABLampButton]);
  RegisterComponents('ABplus', [TABFlickerTimer]);
  RegisterComponents('ABplus', [TABListView]);
  RegisterComponents('ABplus', [TABLockedTimer]);
  RegisterComponents('ABplus', [TABSidePanel]);
  RegisterComponents('ABplus', [TABTopPanel]);
  RegisterComponents('ABplus', [TABBottomPanel]);
end;



initialization

  GLampButtons := TList.Create;

finalization

  GLampButtons.Free;


end.
