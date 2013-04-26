//  Copyright (C) 2000-2005 ABplus kazhida.
//  All rights reserved.
//  $Id: ABGrids.pas 1304 2009-11-17 07:14:05Z kazhida $
//  $Author: kazhida $
//
unit ABGrids;

interface

{:Note::

  ABplus特製グリッド・コントロール

  TABSmartGrid
    --- いろんな便利操作を加えたStringGrid
        リスト選択入力もできるようになっている
        背景やテキストのAlignを変更可能
        Ctrl＋矢印でExcel風にジャンプする
        Paint(から呼んでいるDoPaint)内で、
        DefaultDrawingプロパティを挿げ替えているので、
        DrawCellをoverrideする場合は、DoPaintにも注意
        あるいは、DrawCellをoverrideせずに、
        DrawCellAtをoverrideすること

::Note:}

uses
  Windows, Messages, Classes, Controls, StdCtrls, SysUtils, Graphics, Menus,
  Grids;

type

  EABCtrlException = class(Exception)
    FCtrl:  TWinControl;
    FRow:   Integer;
    FCol:   Integer;
  public
    constructor CreateAt(const msg: String; ctrl: TWinControl;
                         col: Integer = 0; row: Integer = 0);
    property  Ctrl: TWinControl read FCtrl;
    property  Row:  Integer read FRow;
    property  Col:  Integer read FCol;
    procedure BackToOccurred;
  end;

  TABSmartGrid = class;

  TABMoveAtExit =  (maeStay,    //  編集後そのまま
                    maeUp,      //  編集後、上に移動
                    maeDown,    //  編集後、下に移動
                    maeLeft,    //  編集後、左に移動
                    maeRight);  //  編集後、右に移動

  TABFixedSelectable = (fsRow,  //  行選択可能
                        fsCol); //  列選択可能

  TABFixedSelectables = set of TABFixedSelectable;

  TABGridShortCut = (kscDelete,    //  Deleteキーで、選択領域の削除
                   kscCut,       //  ^Cで、選択領域のカット
                   kscCopy,      //  ^Xで、選択領域のコピー
                   kscPaste,     //  ^Vで、ペースト
                   kscUndo);     //  ^Zで、アンドゥ

  TABGridShortCuts = set of TABGridShortCut;

  TABDrawCellProperty = record
    Alignment:  TAlignment;
    Layout:     TTextLayout;
    FontColor:  TColor;
    CellColor:  TColor;
    Vertical:   Boolean;
    Visible:    Boolean;
    TopBorder:  Boolean;
    LeftBorder: Boolean;
    JointRows:  Integer;
    JointCols:  Integer;
  end;

  TABCellJoint = class(TCollectionItem)
  private
    FCol:       Integer;
    FRow:       Integer;
    FColCount:  Integer;
    FRowCount:  Integer;
    procedure SetCol(AValue: Integer);
    procedure SetRow(AValue: Integer);
    procedure SetColCount(AValue: Integer);
    procedure SetRowCount(AValue: Integer);
  protected
    FOwner:     TABSmartGrid;
    FName:      String;
    FTag:       Integer;
  public
    constructor Create(ACollection: TCollection);     override;
    procedure Assign(ASource: TPersistent);           override;
    function  IsLeftTop(ACol, ARow: Integer): Boolean;
    function  Includes(ACol, ARow: Integer): Boolean;
    function  JointCols: Integer;
    function  JointRows: Integer;
  published
    property  Col: Integer      read FCol       write SetCol      default 0;
    property  Row: Integer      read FRow       write SetRow      default 0;
    property  ColCount: Integer read FColCount  write SetColCount default 1;
    property  RowCount: Integer read FRowCount  write SetRowCount default 1;
    property  Name: String      read FName      write FName;
    property  Tag:  Integer     read FTag       write FTag        default 0;
  end;

  TABCellJoints = class(TCollection)
  private
    FOwner:   TABSmartGrid;
    function  GetItem(Index: Integer): TABCellJoint;
    procedure SetItem(Index: Integer; Value: TABCellJoint);
  protected
    function  GetOwner: TPersistent;          override;
    procedure Update(AItem: TCollectionItem); override;
    procedure AssignTo(ADest: TPersistent);   override;
  public
    constructor Create(AOwner: TABSmartGrid);
    function Add: TABCellJoint;
    property OwnerGrid: TABSmartGrid              read FOwner;
    property Items[Index: Integer]: TABCellJoint  read GetItem write SetItem;
  end;

  TABNumericOption = (nuEnabled, nuIntegerOnly, nuDenyNegative);
  TABNumericOptions = set of TABNumericOption;

  TABCellDecorator = class(TABCellJoint)
  private
    FAlignment:   TAlignment;
    FLayout:      TTextLayout;
    FFontColor:   TColor;
    FCellColor:   TColor;
    FVirtical:    Boolean;
    FVisible:     Boolean;
    FTopBorder:   Boolean;
    FLeftBorder:  Boolean;
    FImeMode:     TImeMode;
    FEditMask:    String;
    FMaxLength:   Integer;
    FStrings:     TStrings;
    FReadOnly:    Boolean;
    FHideAtSame:  Boolean;
    FBehindRCCol: Boolean;
    FNumOptions:  TABNumericOptions;
    FSelectList:  TStrings;
    FPrecision:   Integer;
    FOnDrawCell:  TDrawCellEvent;
    procedure SetAlignment(AValue: TAlignment);
    procedure SetLayout(AValue: TTextLayout);
    procedure SetFontColor(AValue: TColor);
    procedure SetCellColor(AValue: TColor);
    procedure SetVirtical(AValue: Boolean);
    procedure SetVisible(AValue: Boolean);
    procedure SetTopBorder(AValue: Boolean);
    procedure SetLeftBorder(AValue: Boolean);
    procedure SetStrings(AValue: TStrings);
    procedure SetSelectList(AValue: TStrings);
  public
    constructor Create(ACollection: TCollection); override;
    destructor  Destroy;                          override;
    procedure Assign(ASource: TPersistent);       override;
  published
    property Alignment: TAlignment  read FAlignment   write SetAlignment  default taLeftJustify;
    property Layout: TTextLayout    read FLayout      write SetLayout     default tlCenter;
    property FontColor: TColor      read FFontColor   write SetFontColor  default clNone;
    property CellColor: TColor      read FCellColor   write SetCellColor  default clNone;
    property Virtical: Boolean      read FVirtical    write SetVirtical   default False;
    property Visible: Boolean       read FVisible     write SetVisible    default True;
    property TopBorder: Boolean     read FTopBorder   write SetTopBorder  default False;
    property LeftBorder: Boolean    read FLeftBorder  write SetLeftBorder default False;
    property ImeMode: TImeMode      read FImeMode     write FImeMode      default imDontCare;
    property EditMask: String       read FEditMask    write FEditMask;
    property MaxLength: Integer     read FMaxLength   write FMaxLength    default 0;
    property Strings: TStrings      read FStrings     write SetStrings;
    property ReadOnly:  Boolean     read FReadOnly    write FReadOnly     default False;
    property HideAtSame:  Boolean   read FHideAtSame  write FHideAtSame   default False;
    property BehindRCColor: Boolean read FBehindRCCol write FBehindRCCol  default True;
    property SelectList: TStrings   read FSelectList  write SetSelectList;
    property Precision: Integer     read FPrecision   write FPrecision    default 0;
    property NumOptions:  TABNumericOptions read FNumOptions write FNumOptions default [];
    property OnDrawCell: TDrawCellEvent read FOnDrawCell write FOnDrawCell;
  end;

  TABCellDecorators = class(TCollection)
  private
    FOwner:   TABSmartGrid;
    function  GetItem(Index: Integer): TABCellDecorator;
    procedure SetItem(Index: Integer; Value: TABCellDecorator);
  protected
    function  GetOwner: TPersistent;          override;
    procedure Update(AItem: TCollectionItem); override;
    procedure AssignTo(ADest: TPersistent);   override;
  public
    constructor Create(AOwner: TABSmartGrid);
    function Add: TABCellDecorator;
    property OwnerGrid: TABSmartGrid                  read FOwner;
    property Items[Index: Integer]: TABCellDecorator  read GetItem
                                                      write SetItem;
  end;

  TABPreDrawEvent = procedure  (Sender: TObject;
                                ACol, ARow: Integer;
                                AState: TGridDrawState;
                                var dcp: TABDrawCellProperty) of Object;

  TABGetEditStyleEvent = procedure (Sender: TObject;
                                    ACol, ARow: Integer;
                                    var EditStyle: TEditStyle) of Object;

  TABModifiedEvent = procedure (Sender: TObject;
                                ACol, ARow: Integer;
                                const OldText, NewText: String;
                                var Cancel: Boolean) of Object;

  TABPreModifiedEvent = procedure (Sender: TObject;
                                ACol, ARow: Integer;
                                const OldText: String;
                                var   NewText: String;
                                var Cancel: Boolean) of Object;

  TABColAdjustEvent = procedure (Sender: TObject; ACol: Integer;
                                var Ignore: Boolean) of Object;

  TABMouseDownedAt = (mdNone,       //  初期状態
                      mdFixedAll,   //  行列とも固定
                      mdFixedCol,   //  固定列
                      mdFixedRow,   //  固定行
                      mdEditCell);  //  通常のセル

  TABModifiedMode = (mmNone, mmEdited, mmPasted, mmDeleted, mmBacking, mmPicked);

  TABGridUndo = record
    ARow: Integer;
    ACol: Integer;
    Text: String;
    ID:   Cardinal;
  end;
  PABGridUndo = ^TABGridUndo;

  TABGridUndoBuffer = class
  private
    FBuf:     TList;
    FUndoing: Boolean;
    function  GetCount: Integer;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure Push(const undo: TABGridUndo);
    function  Pop(var undo: TABGridUndo): Boolean;
    procedure Clear;
    function  NextID: Cardinal;
    function  DumpForDebug: String;
    property  Count: Integer    read GetCount;
    property  Undoing: Boolean  read FUndoing write FUndoing;
  end;


  TABInplaceEditList = class(TInplaceEditList)
  protected
    procedure CloseUp(Accept: Boolean);                   override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char);                    override;
    procedure DoGetPickListItems;                         override;
    procedure WndProc(var msg: TMessage);                 override;
  public
  published
    constructor Create(AOwner: TComponent); override;
    property  Color;
    property  ImeMode;
    property  OnEditButtonClick;
  end;

  TABEditPopupMenu = class(TPopupMenu)
  protected
    FEdit:        TABInplaceEditList;
    FCutMenu:     TMenuItem;
    FCopyMenu:    TMenuItem;
    FPasteMenu:   TMenuItem;
    FDeleteMenu:  TMenuItem;
    procedure CutClick(Sender: TObject);
    procedure CopyClick(Sender: TObject);
    procedure PasteClick(Sender: TObject);
    procedure DeleteClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TABGridPaintedList = class(TObject)
  private
    FList:      array of TGridCoord;
    FCapacity:  Integer;
    FCount:     Integer;
  public
    constructor Create;
    destructor  Destroy;  override;
    procedure Add(ACol, ARow: Integer);
    procedure Clear;
    function  Include(ACol, ARow: Integer): Boolean;
  end;

  TABGridInternalFlags = record
    Editing:        Boolean;
    Jumping:        Boolean;
    LastErrMsg:     String;
    OriginalText:   String;
    GotOriginal:    Boolean;
    MouseDowned:    TABMouseDownedAt;
    ModifiedMode:   TABModifiedMode;
    LastPressed:    DWord;
    ForceInvalid:   Boolean;
    PickedIndex:    Integer;
  end;

  TABColors = array of TColor;

  TABSmartGrid = class(TStringGrid)
  private
    FMoveAtExit:        TABMoveAtExit;
    FOnPreDraw:         TABPreDrawEvent;
    FOnGetEditStyle:    TABGetEditStyleEvent;
    FOnModified:        TABModifiedEvent;
    FOnPreModified:     TABPreModifiedEvent;
    FOnColAdjust:       TABColAdjustEvent;
    FOnEditEnter:       TNotifyEvent;
    FOnEditExit:        TNotifyEvent;
    FOnEditButtonClick: TNotifyEvent;
    FOnStartPaste:      TNotifyEvent;
    FOnEndPaste:        TNotifyEvent;
    FOnStartDelete:     TNotifyEvent;
    FOnEndDelete:       TNotifyEvent;
    FFixedSelect:       TABFixedSelectables;
    FKBShortcut:        TABGridShortCuts;
    FUndoBuffer:        TABGridUndoBuffer;
    FList:              TStrings;
    FDropDownRows:      Integer;
    FFontV:             TFont;
    FFontH:             TFont;
    FChangeFixedColor:  Boolean;
    FRowSelectAtExit:   Boolean;
    FEditColor:         TColor;
    FExpandRow:         Boolean;
    FExpandCol:         Boolean;
    FColNames:          TStrings;
    FRowNames:          TStrings;
    FHideSelection:     Boolean;
    FAutoWidthCols:     Integer;
    FMinColWidth:       Integer;
    FColAdjusting:      Boolean;
    FCellJoints:        TABCellJoints;
    FCellDecorators:    TABCellDecorators;
    FPainted:           TABGridPaintedList;
    Flags:              TABGridInternalFlags;
    FRowColors:         TABColors;
    FColColors:         TABColors;
    FRestCells:         Integer;
    FForceRangeSel:     Boolean;
    FModifyAtCloseup:   Boolean;
    //FSnached:   TDateTime;    //for debug!
    //FRestored:  TDateTime;    //for debug!
    FRestMsg:   String;       //for debug!
    function  GetUndoable: Boolean;
    procedure CopySelectedCells;
    procedure ClearSelectedCells;
    procedure SetSelectList(List: TStrings);
    procedure SetColNames(List: TStrings);
    procedure SetRowNames(List: TStrings);
    procedure SetHVFont;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMExit(var Message: TMessage);        message CM_EXIT;
    procedure CMShowingChanged(var Message: TMessage); message CM_SHOWINGCHANGED;    procedure GridRectToScreenRect(GridRect: TGridRect; var ScreenRect: TRect);
    procedure SetRowSelectAtExit(AValue: Boolean);
    procedure SetHideSelection(AValue: Boolean);
    procedure SetAutoWidthCols(AValue: Integer);
    function  DoModified: Boolean;
    procedure DoSetColNames;
    procedure DoSetRowNames;
    procedure SetCellStrings;
    function  ReadOnlyCell(c, r: Integer): Boolean;
    function  SnachOptEditing: Boolean;
    procedure RestoreOptEditing(const msg: String);
  protected
    FDefaultDrawing:    Boolean;
    FDrawInfo:          TGridDrawInfo;
    FFrameFlags1:       DWORD;
    FFrameFlags2:       DWORD;
    //  描画に関するメソッド
    procedure DoPaint;                                          virtual;
    procedure DrawCellAt( ACol, ARow: Longint; var ARect: TRect;
                          AState: TGridDrawState;
                          const dcp: TABDrawCellProperty;
                          var FrameFlags1, FrameFlags2: DWORD); virtual;
    procedure DrawCell( ACol, ARow: Longint; ARect: TRect;
                        AState: TGridDrawState);                override;
    procedure DoPreDraw(ACol, ARow: Integer; AState: TGridDrawState;
                        var dcp: TABDrawCellProperty);          virtual;
    //  入力チェック関連のメソッド
    procedure DoOnModified(ACol, ARow: Integer;
                            const OldText: String;
                            var NewText: String;
                            var Cancel: Boolean); virtual;
    procedure DoOnPreModified(ACol, ARow: Integer;
                              const OldText: String;
                              var NewText: String;
                              var Cancel: Boolean); virtual;
    //  入力チェックとの関連で、貼り付け、削除(切り取り含む)を通知するメソッド
    procedure DoStartPaste;   virtual;
    procedure DoEndPaste;     virtual;
    procedure DoStartDelete;  virtual;
    procedure DoEndDelete;    virtual;
    //  オーバーライド
    procedure KeyDown(var Key: Word; Shift: TShiftState);     override;
    procedure KeyPress(var Key: Char);                        override;
    procedure KeyUp(var Key: Word; Shift: TShiftState);       override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
                        X, Y: Integer);                       override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer);   override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
                      X, Y: Integer);                         override;
    function  GetEditText(ACol, ARow: Longint): string;       override;
    function  GetEditMask(ACol, ARow: Longint): string;       override;
    function  GetEditLimit: Integer;                          override;
    function  IsSelected(ACol, ARow: Longint): Boolean;
    function  CreateEditor: TInplaceEdit;                     override;
    function  GetEditStyle(ACol, ARow: Longint): TEditStyle;  override;
    procedure DoEnter;                                        override;
    procedure DoExit;                                         override;
    procedure Click;                                          override;
    procedure TopLeftChanged;                                 override;
    procedure SetEnabled(Value: Boolean);                     override;
    function  SelectCell(ACol, ARow: Longint): Boolean;       override;
    procedure Loaded;                                         override;
    procedure SetEditText(ACol, ARow: Longint;
                          const Value: string);               override;
    procedure WndProc(var msg: TMessage);                     override;
    procedure PostErr(const msg: String; col, row: Integer);  virtual;
    procedure Resize;                                         override;
    procedure ColWidthsChanged;                               override;
    procedure SizeChanged(OldColCount, OldRowCount: Longint); override;

    procedure OnEditButtonClicked(Sender: TObject);           virtual;
    procedure OnEditorExit(Sender: TObject);                  virtual;
    //  表示関係
    procedure DrawFocus(FocRect: TRect);                      virtual;
    procedure DoIngoreAdjust(col: Integer; var ign: Boolean); virtual;
    procedure SetColColor(i: Integer; c: TColor);             virtual;
    procedure SetRowColor(i: Integer; c: TColor);             virtual;
    function  GetColColor(i: Integer): TColor;                virtual;
    function  GetRowColor(i: Integer): TColor;                virtual;
    function  IgnoreAdjust(col: Integer): Boolean;            virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy;  override;
    procedure Update;     override;
    procedure Invalidate; override;
    procedure Paint;      override;
    procedure InsertRows(Index: Integer = -1; Count: Integer = -1);  virtual;
    procedure InsertCols(Index: Integer = -1; Count: Integer = -1);  virtual;
    procedure DeleteRows(Index: Integer = -1; Count: Integer = -1);  virtual;
    procedure DeleteCols(Index: Integer = -1; Count: Integer = -1);  virtual;
    procedure ReleaseSelect;
    procedure LineSelect(ACol, ARow: Longint; Count: Integer = 1);
    procedure RectSelect(const sr: TGridRect);
    procedure ClearCells; virtual;
    procedure CutCells;   virtual;
    procedure CopyCells;  virtual;
    procedure Undo;       virtual;
    function  PasteCells(ACol: Integer = -1;
                         ARow: Integer = -1;
                         expandRow: Boolean = True;
                         expandCol: Boolean = False): String; virtual;
    procedure ClearAll(fixedcol: Boolean = True;
                       fixedrow: Boolean = False);   virtual;
    procedure AdjustColWidths;                                virtual;
    property  Undoable: Boolean read GetUndoable;
    property  Selected[ACol, ARow: Integer]: Boolean read IsSelected;
    property  ModifiedMode: TABModifiedMode read Flags.ModifiedMode;
    property  PickedIndex: Integer read Flags.PickedIndex;
    property  ColColors[col: Integer]: TColor read GetColColor write SetColColor;
    property  RowColors[col: Integer]: TColor read GetRowColor write SetRowColor;
    procedure ShowCurrentCell;
    procedure ClearUndo;
    function  GetUndoDebugInfo: String;
    procedure MoveCellTo(ACol, ARow: Integer; MoveAnchor: Boolean = True);  virtual;
    procedure FixUp;
    property  RestCells: Integer read FRestCells;
    //procedure GetDebugState(var ro, rs: Boolean; var gs: TGridState; var st, rt: TDateTime; var rm: String);
    procedure GetDebugState(var ro, rs: Boolean; var rm: String);
  published
    property  Options default [goFixedVertLine, goFixedHorzLine,
                               goVertLine, goHorzLine, goRangeSelect,
                               goEditing, goColSizing];
    property  FixedSelectable: TABFixedSelectables  read    FFixedSelect
                                                    write   FFixedSelect;
    property  KeyboardShortCut: TABGridShortCuts    read    FKBShortcut
                                                    write   FKBShortcut;
    property  InplaceEditor;                        //  TStringListでは
                                                    //  Protected
    property  SelectList: TStrings                  read    FList
                                                    write   SetSelectList;
    property  ChangeFixedColor: Boolean             read    FChangeFixedColor
                                                    write   FChangeFixedColor
                                                    default False;
    property  MoveAtExit: TABMoveAtExit             read    FMoveAtExit
                                                    write   FMoveAtExit
                                                    default maeStay;
    property  RowSelectAtExit: Boolean              read    FRowSelectAtExit
                                                    write   SetRowSelectAtExit
                                                    default False;
    property  DropDownCount: Integer                read    FDropDownRows
                                                    write   FDropDownRows
                                                    default 8;
    property  EditColor: TColor                     read    FEditColor
                                                    write   FEditColor
                                                    default $CCFFFF;
    property  ExpandRowAtPaste: Boolean             read    FExpandRow
                                                    write   FExpandRow
                                                    default True;
    property  ExpandColAtPaste: Boolean             read    FExpandCol
                                                    write   FExpandCol
                                                    default False;
    property  ColNames: TStrings                    read    FColNames
                                                    write   SetColNames;
    property  RowNames: TStrings                    read    FRowNames
                                                    write   SetRowNames;
    property  HideSelection: Boolean                read    FHideSelection
                                                    write   SetHideSelection
                                                    default False;
    property  AutoColWidth: Integer                 read    FAutoWidthCols
                                                    write   SetAutoWidthCols
                                                    default 0;
    property  MinColWidth: Integer                  read    FMinColWidth
                                                    write   FMinColWidth
                                                    default 4;
    property  ForceRangeSelect: Boolean             read    FForceRangeSel
                                                    write   FForceRangeSel
                                                    default False;
    property  ModifyAtCloseup:   Boolean            read    FModifyAtCloseup
                                                    write   FModifyAtCloseup
                                                    default False;
    //  飾り関係
    property  CellJoints: TABCellJoints             read    FCellJoints
                                                    write   FCellJoints;
    property  CellDecorators: TABCellDecorators     read    FCellDecorators
                                                    write   FCellDecorators;
    //  イベント・プロシージャ
    property  OnPreDraw: TABPreDrawEvent            read    FOnPreDraw
                                                    write   FOnPreDraw;
    property  OnGetEditStyle: TABGetEditStyleEvent  read    FOnGetEditStyle
                                                    write   FOnGetEditStyle;
    property  OnModified: TABModifiedEvent          read    FOnModified
                                                    write   FOnModified;
    property  OnPreModified: TABPreModifiedEvent    read    FOnPreModified
                                                    write   FOnPreModified;
    property  OnColAdjust:  TABColAdjustEvent       read    FOnColAdjust
                                                    write   FOnColAdjust;              
    property  OnEditEnter: TNotifyEvent             read    FOnEditEnter
                                                    write   FOnEditEnter;
    property  OnEditExit:  TNotifyEvent             read    FOnEditExit
                                                    write   FOnEditExit;
    property  OnEditButtonClick: TNotifyEvent       read    FOnEditButtonClick
                                                    write   FOnEditButtonClick;
    property  OnStartPaste: TNotifyEvent            read    FOnStartPaste
                                                    write   FOnStartPaste;
    property  OnEndPaste:   TNotifyEvent            read    FOnEndPaste
                                                    write   FOnEndPaste;
    property  OnStartDelete: TNotifyEvent           read    FOnStartDelete
                                                    write   FOnStartDelete;
    property  OnEndDelete:   TNotifyEvent           read    FOnEndDelete
                                                    write   FOnEndDelete;
  end;

procedure Register;

////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////
implementation

uses
  Dialogs, Forms, Clipbrd, Imm;

const
  BS   = #8;
  TAB  = #9;
  CRLF = #13#10;

  WM_ERROR_OCURRED = WM_USER + $1001;


/////////////////////////////////////////
{ TABSmartGrid }

var
  GUndoCount: Cardinal;

constructor TABSmartGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Options := Options + [goEditing, goColSizing];
  DefaultRowHeight := 18;

  FMoveAtExit   := maeStay;
  FFixedSelect  := [fsRow, fsCol];
  FKBShortcut   := [kscDelete, kscCut, kscCopy, kscPaste];
  FDropDownRows := 8;
  FList         := TStringList.Create;
  FFontV        := TFont.Create;
  FFontH        := TFont.Create;
  SetHVFont;

  Flags.Editing       := False;
  Flags.Jumping       := False;
  Flags.LastErrMsg    := '';
  Flags.OriginalText  := '';
  Flags.GotOriginal   := False;
  Flags.MouseDowned   := mdNone;
  Flags.ModifiedMode  := mmNone;
  Flags.LastPressed   := 0;
  Flags.PickedIndex   := -1;

  FRowSelectAtExit  := False;
  FChangeFixedColor := False;
  FEditColor        := $CCFFFF;
  FExpandRow        := True;
  FExpandCol        := False;
  FColNames         := TStringList.Create;
  FRowNames         := TStringList.Create;
  FHideSelection    := False;
  FUndoBuffer       := TABGridUndoBuffer.Create;
  FCellJoints       := TABCellJoints.Create(Self);
  FCellDecorators   := TABCellDecorators.Create(Self);
  FMinColWidth      := 4;

  FPainted  := TABGridPaintedList.Create;

  ClearUndo;
end;

destructor  TABSmartGrid.Destroy;
begin
  ClearUndo;
  FUndoBuffer.Free;
  FFontV.Free;
  FFontH.Free;
  FList.Free;
  FColNames.Free;
  FRowNames.Free;
  FCellJoints.Free;
  FCellDecorators.Free;
  FPainted.Free;
  inherited Destroy;
end;

//  overrided methods

procedure TABSmartGrid.Loaded;
begin
  inherited Loaded;
  DoSetColNames;
  DoSetRowNames;
  SetCellStrings;
end;

procedure TABSmartGrid.Click;
begin
  //  ジャンプ中は保留
  if not Flags.Jumping then inherited Click;
end;

procedure TABSmartGrid.Update;
begin
  //  ジャンプ中は保留
  if not Flags.Jumping then inherited Update;
end;

procedure TABSmartGrid.Invalidate;
begin
  //  ジャンプ中は保留
  if not Flags.Jumping then inherited Invalidate;
end;

function  TABSmartGrid.ReadOnlyCell(c, r: Integer): Boolean;
var
  i:  Integer;
begin
  Result := False;
  with FCellDecorators do
    for i := 0 to Count - 1 do
      if Items[i].Includes(c, r) and Items[i].ReadOnly then
        Result := True;
end;

procedure TABSmartGrid.KeyDown(var Key: Word; Shift: TShiftState);
var
  done:   Boolean;
  empty:  Boolean;
  jshift: TShiftState;
  r, c:   Integer;

  function jumpKey: Boolean;
  begin
    Result := (ssCtrl in Shift);
    if Result then
    begin
      case Key of
        VK_UP:    ;
        VK_DOWN:  ;
        VK_LEFT:  ;
        VK_RIGHT: ;
      else
        Result := False;
      end;
    end;
  end;

  function doneUp(var c, r: Integer): Boolean;
  begin
    if r = FixedRows then Result := True
    else if empty    then Result := (Cells[c, r    ] <> '')
    else                  Result := (Cells[c, r - 1] =  '');
    if not Result then r := r - 1;
  end;

  function doneDown(var c, r: Integer): Boolean;
  begin
    if r = RowCount - 1  then Result := True
    else if empty        then Result := (Cells[c, r    ] <> '')
    else                      Result := (Cells[c, r + 1] =  '');
    if not Result then r := r + 1;
  end;

  function doneLeft(var c, r: Integer): Boolean;
  begin
    if c = FixedCols then Result := True
    else if empty    then Result := (Cells[c,     r] <> '')
    else                  Result := (Cells[c - 1, r] =  '');
    if not Result then c := c - 1;
  end;

  function doneRight(var c, r: Integer): Boolean;
  begin
    if c = ColCount - 1  then Result := True
    else if empty        then Result := (Cells[c,     r] <> '')
    else                      Result := (Cells[c + 1, r] =  '');
    if not Result then c := c + 1;
  end;

begin

  done := False;

  //  Ctrl+矢印キーで、ジャンプ
  if jumpKey then
  begin
    jshift := Shift - [ssCtrl];
    Flags.Jumping := True;
    empty  := (Cells[Col, Row] = '');
    if not empty then
    begin
      //  とりあえず一回動かしてみて、その時にセルが空欄かどうかを調べる
      inherited KeyDown(Key, jshift);
      empty  := (Cells[Col, Row] = '');
    end;
    r := Row;
    c := Col;
    repeat
      case Key of
        VK_UP:    done := doneUp(c, r);
        VK_DOWN:  done := doneDown(c, r);
        VK_LEFT:  done := doneLeft(c, r);
        VK_RIGHT: done := doneRight(c, r);
      end;
    until done;
    FocusCell(c, r, not (ssShift in Shift));

    Flags.Jumping := False;
    //  保留にしていた処理を行う
    Invalidate;
    Update;
    Click;
  end;
  if (not done) then
  begin
    if Key in [VK_UP, VK_DOWN, VK_LEFT, VK_RIGHT, VK_TAB, VK_NEXT, VK_PRIOR] then
    begin
      inherited KeyDown(Key, Shift);
      Invalidate;   //  キーによる移動時に再描画
      done := True;
    end;
  end;

  //  Deleteキーで選択範囲の削除
  if (Key = VK_DELETE) and (Shift = []) and (kscDelete in FKBShortcut) then
  begin
    ClearCells;
    done := True;
  end;

  //  ReadOnlyならF2キーを無視
  if not (Key in [$9, $10, $13]) and ReadOnlyCell(Col, Row) then
    SnachOptEditing;

  //  ここで処理していなければ、もともとの操作
  if not done then
  begin
    inherited KeyDown(Key, Shift);
    if (not (goAlwaysShowEditor in Options)) and
      (goEditing in Options) and
      (EditorMode = False) and
      (Shift = []) then
    begin
      EditorMode := True;
      InplaceEditor.SelStart  := 0;
      InplaceEditor.SelLength := Length(Cells[Col, Row]);
    end;
  end;
end;

procedure TABSmartGrid.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited KeyUp(Key, Shift);
  RestoreOptEditing('KeyUp');
end;

procedure TABSmartGrid.KeyPress(var Key: Char);
var
  edited:   Boolean;
  movedir:  TABMoveAtExit;
begin
  if (Key = #13) and EditorMode then
  begin
    edited  := True;
    movedir := FMoveAtExit;
  end
  else
  begin
    edited  := False;
    movedir := maeStay;
  end;

  inherited;
  case Key of
    #3:   if (kscCopy  in FKBShortcut) then CopyCells;
    #22:  if (kscPaste in FKBShortcut) then
            PasteCells(Col, Row, FExpandRow, FExpandCol);
    #24:  if (kscCut   in FKBShortcut) then CutCells;
    #26:  if (kscUndo  in FKBShortcut) then Undo;
  end;

  if edited then
  begin
    if not DoModified then
    begin
      case movedir of
        maeUp:    if Row > FixedRows    then Row := Row - 1;
        maeDown:  if Row < RowCount - 1 then Row := Row + 1;
        maeLeft:  if Col > FixedCols    then Col := Col - 1;
        maeRight: if Col < ColCount - 1 then Col := Col + 1;
      end;
    end;
  end;
end;

function  TABSmartGrid.SnachOptEditing: Boolean;
begin
  //  強制的に編集なしにする
  Result := goEditing in Options;
  if Result then
  begin
    Flags.Editing := Result;
    Options := Options - [goEditing];

    //FSnached := Now;
  end;
end;

procedure TABSmartGrid.RestoreOptEditing(const msg: String);
begin
  //  編集可に戻す
  if Flags.Editing then
  begin
    Options  := Options + [goEditing];
    Flags.Editing := False;
    //FRestored := Now;
    FRestMsg  := msg;
  end;
end;

procedure TABSmartGrid.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  r,  c:  Longint;
  nowtick: DWord;
begin

  nowtick := GetTickCount;
  MouseToCell(X, Y, c, r);
  Flags.MouseDowned := mdEditCell;

  if (InplaceEditor <> nil) and InplaceEditor.Modified then
    DoModified;

  if (goRangeSelect in Options) and ForceRangeSelect then
  begin
    if (c <> Col) or (r <> Row) or
       (Flags.LastPressed + 500 > nowtick) or
       ReadOnlyCell(c, r) then
    begin
      SnachOptEditing;
    end;
  end;
  Flags.LastPressed := nowtick;

  //  本来の動作
  inherited;
  if not (FGridState in [gsNormal, gsSelecting]) then Exit;

  if c < FixedCols then
  begin
    if r < FixedRows then
    begin
      Flags.MouseDowned := mdFixedAll;
    end
    else
    begin
      //  行選択
      if (fsRow in FFixedSelect) and (Button = mbLeft) then
      begin
        MoveColRow(ColCount - 1, r, True,  False);
        MoveColRow(FixedCols,    r, False, False);
        FGridState := gsSelecting;
        Invalidate;
      end;
      Flags.MouseDowned := mdFixedCol;
    end
  end
  else
  begin
    if r < FixedRows then
    begin
      //  列選択
      if (fsCol in FFixedSelect) and (Button = mbLeft) then
      begin
        MoveColRow(c, RowCount - 1, True,  False);
        MoveColRow(c, FixedRows ,   False, False);
        FGridState := gsSelecting;
        Invalidate;
      end;
      Flags.MouseDowned := mdFixedRow;
    end
    else
    begin
      //  編集可能なセル
      Flags.MouseDowned := mdEditCell;
    end;
  end;
end;

procedure TABSmartGrid.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  c, r: Integer;
begin
  inherited;

  if Flags.MouseDowned in [mdFixedCol, mdFixedRow] then
  begin
    MouseToCell(X, Y, c, r);
    if c < FixedCols then c := FixedCols;
    if r < FixedRows then r := FixedRows;
    MoveColRow(c, r, False, False);
  end;
end;

procedure TABSmartGrid.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Flags.MouseDowned := mdNone;
  RestoreOptEditing('MouseUp');
  inherited;
end;

procedure TABSmartGrid.TopLeftChanged;
begin
  inherited;
  Invalidate;
end;


//  表示関係


procedure TABSmartGrid.DoPaint;
begin
  //  DefaultDrawingをごまかす
  DefaultDrawing := False;
  FPainted.Clear;
  inherited Paint;
end;

procedure TABSmartGrid.Paint;
var
  FocRect:  TRect;
begin
  //  DrawCellで必要なものを計算しておく
  CalcDrawInfo(FDrawInfo);
  FFrameFlags1 := 0;
  FFrameFlags2 := 0;
  if goFixedVertLine in Options then
  begin
    FFrameFlags1 := BF_RIGHT;
    FFrameFlags2 := BF_LEFT;
  end;
  if goFixedHorzLine in Options then
  begin
    FFrameFlags1 := FFrameFlags1 or BF_BOTTOM;
    FFrameFlags2 := FFrameFlags2 or BF_TOP;
  end;

  FDefaultDrawing := DefaultDrawing;
  DoPaint;
  DefaultDrawing := FDefaultDrawing;

  //  focusの描画
  if not (csDesigning in ComponentState) and
    (goRowSelect in Options) and DefaultDrawing and Focused then
  begin
    GridRectToScreenRect(Selection, FocRect);
    DrawFocus(FocRect);
  end;

end;

procedure TABSmartGrid.DrawFocus(FocRect: TRect);
var
  AFocRect: TRect;
begin
  if not UseRightToLeftAlignment then
    Canvas.DrawFocusRect(FocRect)
  else
  begin
    AFocRect := FocRect;
    AFocRect.Left := FocRect.Right;
    AFocRect.Right := FocRect.Left;
    DrawFocusRect(Canvas.Handle, AFocRect);
  end;
end;

procedure TABSmartGrid.DoPreDraw(ACol, ARow: Integer; AState: TGridDrawState;
  var dcp: TABDrawCellProperty);
begin
  if Assigned(FOnPreDraw) then
    FOnPreDraw(Self, ACol, ARow, AState, dcp);
end;

procedure TABSmartGrid.GridRectToScreenRect(GridRect: TGridRect;
  var ScreenRect: TRect);

  function LinePos(const AxisInfo: TGridAxisDrawInfo; Line: Integer): Integer;
  var
    Start, I: Longint;
  begin
    with AxisInfo do
    begin
      Result := 0;
      if Line < FixedCellCount then
        Start := 0
      else
      begin
        if Line >= FirstGridCell then
          Result := FixedBoundary;
        Start := FirstGridCell;
      end;
      for I := Start to Line - 1 do
      begin
        Inc(Result, GetExtent(I) + EffectiveLineWidth);
        if Result > GridExtent then
        begin
          Result := 0;
          Exit;
        end;
      end;
    end;
  end;

  function CalcAxis(const AxisInfo: TGridAxisDrawInfo;
    GridRectMin, GridRectMax: Integer;
    var ScreenRectMin, ScreenRectMax: Integer): Boolean;
  begin
    Result := False;
    with AxisInfo do
    begin
      if (GridRectMin >= FixedCellCount) and (GridRectMin < FirstGridCell) then
        if GridRectMax < FirstGridCell then
        begin
          FillChar(ScreenRect, SizeOf(ScreenRect), 0); { erase partial results }
          Exit;
        end
        else
          GridRectMin := FirstGridCell;
      if GridRectMax > LastFullVisibleCell then
      begin
        GridRectMax := LastFullVisibleCell;
        if GridRectMax < GridCellCount - 1 then Inc(GridRectMax);
        if LinePos(AxisInfo, GridRectMax) = 0 then
          Dec(GridRectMax);
      end;

      ScreenRectMin := LinePos(AxisInfo, GridRectMin);
      ScreenRectMax := LinePos(AxisInfo, GridRectMax);
      if ScreenRectMax = 0 then
        ScreenRectMax := ScreenRectMin + GetExtent(GridRectMin)
      else
        Inc(ScreenRectMax, GetExtent(GridRectMax));
      if ScreenRectMax > GridExtent then
        ScreenRectMax := GridExtent;
    end;
    Result := True;
  end;

var
  DrawInfo: TGridDrawInfo;
  Hold: Integer;
begin
  FillChar(ScreenRect, SizeOf(ScreenRect), 0);
  if (GridRect.Left > GridRect.Right) or (GridRect.Top > GridRect.Bottom) then
    Exit;
  CalcDrawInfo(DrawInfo);
  with DrawInfo do
  begin
    if GridRect.Left > Horz.LastFullVisibleCell + 1 then Exit;
    if GridRect.Top  > Vert.LastFullVisibleCell + 1 then Exit;

    if CalcAxis(Horz, GridRect.Left, GridRect.Right, ScreenRect.Left,
      ScreenRect.Right) then
    begin
      CalcAxis(Vert, GridRect.Top, GridRect.Bottom, ScreenRect.Top,
        ScreenRect.Bottom);
    end;
  end;
  if UseRightToLeftAlignment and (Canvas.CanvasOrientation = coLeftToRight) then
  begin
    Hold := ScreenRect.Left;
    ScreenRect.Left := ClientWidth - ScreenRect.Right;
    ScreenRect.Right := ClientWidth - Hold;
  end;
end;

function  AddTailSpace(const s: String; d: TABCellDecorator): String;
var
  i:  Integer;
begin
  if d.Alignment <> taRightJustify then
    Result := s
  else if d.Precision <= 0 then
    Result := s
  else
  begin
    for i := 0 to Length(s) - 1 do
    begin
      if s[Length(s) - i] = '.' then
      begin
        if i < d.Precision then
          Result := s + StringOfChar(' ', d.Precision - i)
        else
          Result := s;
        Exit;
      end;
    end;
    Result := s + StringOfChar(' ', d.Precision + 1);
  end;
end;

procedure TABSmartGrid.DrawCellAt(ACol, ARow: Longint; var ARect: TRect;
  AState: TGridDrawState; const dcp: TABDrawCellProperty;
  var FrameFlags1, FrameFlags2: DWORD);
var
  s:    String;
  w, h: Integer;
  f:    TColor;
  Hold: Integer;
  Left: Integer;
  Top:  Integer;
  r, c: Integer;
  i:    Integer;
begin
  if DefaultDrawing and (dcp.JointRows > 0) and (dcp.JointCols > 0) then
  begin
    //  ARect を広げる
    for r := 1 to dcp.JointRows - 1 do
      ARect.Bottom := ARect.Bottom +
                      FDrawInfo.Vert.EffectiveLineWidth + RowHeights[ARow + r];
    for c := 1 to dcp.JointCols - 1 do
      ARect.Right := ARect.Right +
                      FDrawInfo.Horz.EffectiveLineWidth + ColWidths[ACol + c];

    s := Cells[ACol, ARow];
    for i := 0 to FCellDecorators.Count - 1 do
      if FCellDecorators.Items[i].Includes(ACol, ARow) then
        s := AddTailSpace(s, FCellDecorators.Items[i]);

    w := Canvas.TextWidth(s);
    h := Canvas.TextHeight(s);
    f := Canvas.Font.Color;
    if dcp.Vertical then
    begin
      Canvas.Font.Assign(FFontV);
      Canvas.Font.Color := f;
      Top  := ARect.Bottom;
      Left := (ARect.Left + ARect.Right - h) div 2;
      case dcp.Alignment of
        taLeftJustify:  Top := ARect.Bottom - 2;
        taRightJustify: Top := ARect.Top + w + 2;
        taCenter:       Top := (ARect.Top + ARect.Bottom + w) div 2;
      end;
      case dcp.Layout of
        tlTop:    Left := ARect.Left + 2;
        tlCenter: Left := (ARect.Left + ARect.Right - h) div 2;
        tlBottom: Left := ARect.Right - h - 2;
      end;
    end
    else
    begin
      Canvas.Font.Assign(FFontH);
      Canvas.Font.Color := f;
      Top  := (ARect.Top + ARect.Bottom - h) div 2;
      Left := ARect.Left + 2;
      case dcp.Alignment of
        taLeftJustify:  Left := ARect.Left + 2;
        taRightJustify: Left := ARect.Right - w - 2;
        taCenter:       Left := (ARect.Left + ARect.Right - w) div 2;
      end;
      case dcp.Layout of
        tlTop:    Top := ARect.Top + 2;
        tlCenter: Top := (ARect.Top + ARect.Bottom - h) div 2;
        tlBottom: Top := ARect.Bottom - h - 2;
      end;
    end;
    Canvas.FillRect(ARect);
    if dcp.Visible then Canvas.TextRect(ARect, Left, Top, s);
    for i := 0 to FCellDecorators.Count - 1 do
    begin
      with FCellDecorators.Items[i] do
      begin
        if FCellDecorators.Items[i].Includes(ACol, ARow) and
           Assigned(FCellDecorators.Items[i].OnDrawCell) then
        begin
          if UseRightToLeftAlignment then
          begin
            ARect.Left := ClientWidth - ARect.Left;
            ARect.Right := ClientWidth - ARect.Right;
            Hold := ARect.Left;
            ARect.Left := ARect.Right;
            ARect.Right := Hold;
            ChangeGridOrientation(False);
          end;
          FCellDecorators.Items[i].OnDrawCell(Self, ACol, ARow, ARect, AState);
          if UseRightToLeftAlignment then ChangeGridOrientation(True);
        end;
      end;
    end;
  end
  else
  begin
    FrameFlags1 := 0;
    FrameFlags2 := 0;
  end;

  //inherited DrawCell(ACol, ARow, ARect, AState);
  //  継承元で描画してしまうので、継承元のさらに継承元(TCustomGrid)のDrawCell
  //  と同じ処理をする

  if Assigned(OnDrawCell) then
  begin
    if UseRightToLeftAlignment then
    begin
      ARect.Left := ClientWidth - ARect.Left;
      ARect.Right := ClientWidth - ARect.Right;
      Hold := ARect.Left;
      ARect.Left := ARect.Right;
      ARect.Right := Hold;
      ChangeGridOrientation(False);
    end;
    OnDrawCell(Self, ACol, ARow, ARect, AState);
    if UseRightToLeftAlignment then ChangeGridOrientation(True);
  end;

  for r := 0 to dcp.JointRows - 1 do
    for c := 0 to dcp.JointCols - 1 do
      FPainted.Add(ACol + c, ARow + r);
end;

procedure TABSmartGrid.DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState);

  function Highlited: Boolean;
  begin
    Result := gdSelected in AState;
    if gdFocused in AState then
    begin
      if [goDrawFocusSelected, goRowSelect] * Options <> [] then
        Result := True
      else
        Result := False;
    end
    else if not Focused then
    begin
      if FHideSelection then
        Result := False
      else if FRowSelectAtExit and (ARow = Row) and
              (not (gdFixed in AState)) then
        Result := True;
    end;
  end;
var
  TempRect:     TRect;
  OrigRect:     TRect;
  FrameFlags1:  DWORD;
  FrameFlags2:  DWORD;
  dcp:  TABDrawCellProperty;
  fc: TColor;
  pc: TColor;
  bc: TColor;
  i:  Integer;
  j:  TABCellJoint;
  d:  TABCellDecorator;
begin
  //  PreDrawの準備
  if gdFixed in AState then
    dcp.Alignment := taCenter
  else
    dcp.Alignment := taLeftJustify;
  dcp.Layout := tlCenter;

  fc := Canvas.Font.Color;
  pc := Canvas.Pen.Color;
  bc := Canvas.Brush.Color;

  dcp.FontColor   := fc;
  dcp.CellColor   := bc;
  dcp.Vertical    := False;
  dcp.JointCols   := 1;
  dcp.JointRows   := 1;
  dcp.Visible     := True;
  dcp.TopBorder   := False;
  dcp.LeftBorder  := False;

  //  ここで、本来のDefaultDrawingに戻す
  DefaultDrawing := FDefaultDrawing;

  //  本来は、Paintの中で行われている前処理
  if gdFixed in AState then
    dcp.CellColor := Self.FixedColor
  else
    dcp.CellColor := Self.Color;

  //  セルの連結
  for i := 0 to FCellJoints.Count - 1 do
  begin
    j := FCellJoints.Items[i];
    if (j.Col = ACol) and (j.Row = ARow) then
    begin
      dcp.JointCols := j.JointCols;
      dcp.JointRows := j.JointRows;
    end
    else if j.Includes(ACol, ARow) then
    begin
      dcp.JointCols := 0;
      dcp.JointRows := 0;
    end;
  end;

  //  セルの修飾(Row/ColColorより弱い)
  for i := 0 to FCellDecorators.Count - 1 do
  begin
    d := FCellDecorators.Items[i];
    if d.Includes(ACol, ARow) then
    begin
      if d.BehindRCColor then
      begin
        if d.FontColor <> clNone then dcp.FontColor := d.FontColor;
        if d.CellColor <> clNone then dcp.CellColor := d.CellColor;
      end;
    end;
  end;

  //  セルの色(行優先)
  if ColColors[ACol] <> clNone then dcp.CellColor := ColColors[ACol];
  if RowColors[ARow] <> clNone then dcp.CellColor := RowColors[ARow];

  //  セルの修飾
  for i := 0 to FCellDecorators.Count - 1 do
  begin
    d := FCellDecorators.Items[i];
    if d.Includes(ACol, ARow) then
    begin
      if not d.BehindRCColor then
      begin
        if d.FontColor <> clNone then dcp.FontColor := d.FontColor;
        if d.CellColor <> clNone then dcp.CellColor := d.CellColor;
      end;
      dcp.Alignment   := d.Alignment;
      dcp.Layout      := d.Layout;
      dcp.Vertical    := d.Virtical;
      dcp.Visible     := d.Visible;
      dcp.TopBorder   := d.TopBorder;
      dcp.LeftBorder  := d.LeftBorder;
    end;
  end;

  //  PreDraw処理
  DoPreDraw(ACol, ARow, AState, dcp);

  if (not FChangeFixedColor) and (gdFixed in AState) then
    dcp.CellColor := Self.FixedColor;

  if not Enabled then
    dcp.FontColor := clGrayText;

  //  本来は、Paintの中で行われている前処理
  if DefaultDrawing or (csDesigning in ComponentState) then
  begin
    with Canvas do
    begin
      Font := Self.Font;
      if Highlited then
      begin
        Brush.Color := clHighlight;
        Font.Color  := clHighlightText;
      end
      else
        Brush.Color := dcp.CellColor;
      //FillRect(ARect);  //DrawCellAtの中で行う
    end;
  end;

  FrameFlags1 := FFrameFlags1;
  FrameFlags2 := FFrameFlags2;
  if Highlited then
  begin
    Canvas.Brush.Color := clHighlight;
    Canvas.Font.Color  := clHighlightText;
  end
  else
  begin
    Canvas.Brush.Color := dcp.CellColor;
    Canvas.Font.Color  := dcp.FontColor;
  end;

  OrigRect := ARect;
  DrawCellAt(ACol, ARow, ARect, AState, dcp, FrameFlags1, FrameFlags2);

  //  罫線
  if dcp.TopBorder then
  begin
    Canvas.Pen.Color := clWindowText;
    Canvas.MoveTo(ARect.Left,  ARect.Top);
    Canvas.LineTo(ARect.Right, ARect.Top);
  end;
  if dcp.LeftBorder then
  begin
    Canvas.Pen.Color := clWindowText;
    Canvas.MoveTo(ARect.Left, ARect.Top);
    Canvas.LineTo(ARect.Left, ARect.Bottom);
  end;

  Canvas.Pen.Color   := pc;
  Canvas.Font.Color  := fc;
  Canvas.Brush.Color := bc;

  //  本来は、Paintの中で行われている後処理
  if DefaultDrawing and (gdFixed in AState) and Ctl3D and
    ((FrameFlags1 or FrameFlags2) <> 0) then
  begin
    TempRect := ARect;
    if (FrameFlags1 and BF_RIGHT) = 0 then
      Inc(TempRect.Right, FDrawInfo.Horz.EffectiveLineWidth)
    else if (FrameFlags1 and BF_BOTTOM) = 0 then
      Inc(TempRect.Bottom, FDrawInfo.Vert.EffectiveLineWidth);
    DrawEdge(Canvas.Handle, TempRect, BDR_RAISEDINNER, FrameFlags1);
    DrawEdge(Canvas.Handle, TempRect, BDR_RAISEDINNER, FrameFlags2);
  end;
  if DefaultDrawing and not (csDesigning in ComponentState) and
    (gdFocused in AState) and
    ([goEditing, goAlwaysShowEditor] * Options <>
    [goEditing, goAlwaysShowEditor])
    and not (goRowSelect in Options) then
  begin
    DrawFocus(OrigRect);
  end;

{
  if DefaultDrawing then
  begin
    if (dcp.JointRows <= 0) and (ARow > 0) then
    begin
      InvalidateCell(ACol, ARow - 1);
      if not FPainted.Include(ACol, ARow) then
        InvalidateCell(ACol, ARow);
    end;
    if (dcp.JointCols <= 0) and (ACol > 0) then
    begin
      InvalidateCell(ACol - 1, ARow);
      if not FPainted.Include(ACol, ARow) then
        InvalidateCell(ACol, ARow);
    end;
  end;
}
  DefaultDrawing := False;
end;

function  TABSmartGrid.GetEditText(ACol, ARow: Longint): string;
var
  i:  Integer;
  d:  TABCellDecorator;
begin
  Result := inherited GetEditText(ACol, ARow);
  for i := 0 to FCellDecorators.Count - 1 do
  begin
    d := FCellDecorators.Items[i];
    if d.Includes(ACol, ARow) and (d.ImeMode <> imDontCare) then
      (InplaceEditor as TABInplaceEditList).ImeMode := d.ImeMode;
  end;
  if Flags.ModifiedMode = mmNone then
  begin
    Flags.OriginalText := Result;
    Flags.GotOriginal  := True;
  end;
  if Flags.ForceInvalid then
  begin
    Flags.ForceInvalid := False;
    Update;
  end;
end;

function  TABSmartGrid.GetEditMask(ACol, ARow: Longint): string;
var
  i:  Integer;
begin
  Result := inherited GetEditMask(ACol, ARow);
  for i := 0 to FCellDecorators.Count - 1 do
  begin
    if FCellDecorators.Items[i].Includes(ACol, ARow) then
      Result := FCellDecorators.Items[i].EditMask;
  end;
end;

function  TABSmartGrid.GetEditLimit: Integer;
var
  i:  Integer;
begin
  Result := inherited GetEditLimit;
  for i := 0 to FCellDecorators.Count - 1 do
  begin
    if FCellDecorators.Items[i].Includes(Col, Row) then
      Result := FCellDecorators.Items[i].MaxLength;
  end;
end;

procedure TABSmartGrid.SetEditText(ACol, ARow: Longint; const Value: string);
begin
  inherited;
end;

procedure TABSmartGrid.DoEnter;
begin
  RestoreOptEditing('DoEnter');
  inherited;

  if ReadOnlyCell(Col, Row) then
    SnachOptEditing;

  if FRowSelectAtExit then
    Repaint;
end;


procedure TABSmartGrid.DoExit;
begin
  RestoreOptEditing('DoExit');
  inherited;

  if FRowSelectAtExit then
  begin
    Repaint;
    Flags.ForceInvalid := True;
  end;
  Invalidate;
end;

procedure TABSmartGrid.WndProc(var msg: TMessage);
begin
  inherited WndProc(msg);
  case msg.Msg of
    WM_ERROR_OCURRED:
      begin
        RestoreOptEditing('ERROR_OCURRED');
        Flags.ModifiedMode := mmBacking;
        MoveCellTo(msg.WParam, msg.LParam);
        MessageDlg(Flags.LastErrMsg, mtError, [mbOK], 0);
        Flags.ModifiedMode := mmNone;
        Flags.LastErrMsg := '';
      end;
  end;
end;

procedure TABSmartGrid.PostErr(const msg: String; col, row: Integer);
begin
  Flags.LastErrMsg := msg;
  PostMessage(Self.Handle, WM_ERROR_OCURRED, Col, Row);
end;

procedure TABSmartGrid.SetRowSelectAtExit(AValue: Boolean);
var
  form:     TCustomForm;
begin
  if AValue <> FRowSelectAtExit then
  begin
    form := GetParentForm(Self);
    if Assigned(form) and (form.ActiveControl = Self) then
      Invalidate;

    FRowSelectAtExit := AValue;
  end;
end;

procedure TABSmartGrid.SetHideSelection(AValue: Boolean);
begin
  if AValue <> FHideSelection then
  begin
    FHideSelection := AValue;
    Invalidate;
  end;
end;

procedure TABSmartGrid.SetAutoWidthCols(AValue: Integer);
begin
  if AValue <> FAutoWidthCols then
  begin
    FAutoWidthCols := AValue;
    AdjustColWidths;
  end;
end;

procedure TABSmartGrid.DoIngoreAdjust(col: Integer; var ign: Boolean);
begin
  if Assigned(FOnColAdjust) then
    FOnColAdjust(Self, col, ign);
end;

function  TABSmartGrid.IgnoreAdjust(col: Integer): Boolean;
begin
  Result := False;
  DoIngoreAdjust(col, Result);
end;


procedure TABSmartGrid.AdjustColWidths;
var
  wx: Integer;
  wf: Integer;
  w:  Integer;
  i:  Integer;
  num:  Integer;
  last: Integer;
begin
  if not FColAdjusting and HandleAllocated and Showing and (FAutoWidthCols > 0) then
  begin
    FColAdjusting := True;
    try
      wx := 0;
      wf := 0;
      num := 0;
      for i := 0 to ColCount - 1 do
      begin
        if i < ColCount - FAutoWidthCols then
          Inc(wx, ColWidths[i] + GridLineWidth)
        else if IgnoreAdjust(i) then
          Inc(wx, ColWidths[i] + GridLineWidth)
        else
        begin
          Inc(wf, ColWidths[i] + GridLineWidth);
          Inc(num);
        end;
      end;
      if wx + wf <> ClientWidth then
      begin
        w := (ClientWidth - wx) div num - GridLineWidth;
        if w < FMinColWidth then w := FMinColWidth;
        last := ColCount - FAutoWidthCols;
        for i := ColCount - FAutoWidthCols to ColCount - 1 do
          if not IgnoreAdjust(i) then
            last := i;
        for i := ColCount - FAutoWidthCols to ColCount - 1 do
        begin
          if IgnoreAdjust(i) then Continue;
          if i < last then
            ColWidths[i] := w
          else
          begin
            w := (w + GridLineWidth) * (num - 1);
            w := ClientWidth - wx - w - 1;
            if w < FMinColWidth then w := FMinColWidth;
            ColWidths[i] := w;
          end;
        end;
      end;
    finally
      FColAdjusting := False;
    end;
  end;
end;

procedure TABSmartGrid.Resize;
begin
  inherited;
  AdjustColWidths;
end;

procedure TABSmartGrid.ColWidthsChanged;
begin
  AdjustColWidths;
  inherited;
end;

procedure TABSmartGrid.SizeChanged(OldColCount, OldRowCount: Longint);
begin
  AdjustColWidths;
  DoSetColNames;
  DoSetRowNames;  
end;

procedure TABSmartGrid.SetEnabled(Value: Boolean);
var
  OldValue: Boolean;
begin
  OldValue := Enabled;
  inherited SetEnabled(Value);

  if OldValue <> Value then
    Invalidate;
end;

//  internal utility

function  TABSmartGrid.IsSelected(ACol, ARow: Longint): Boolean;
begin
  Result := False;
  if ACol < Selection.Left   then Exit;
  if ACol > Selection.Right  then Exit;
  if ARow < Selection.Top    then Exit;
  if ARow > Selection.Bottom then Exit;
  Result := True;
end;

//  for properties

function  TABSmartGrid.GetUndoable: Boolean;
begin
  Result := (FUndoBuffer.Count > 0);
end;

//  external utility

procedure TABSmartGrid.ReleaseSelect;
var
  sr: TGridRect;
begin
  sr.Left   := Col;
  sr.Right  := Col;
  sr.Top    := Row;
  sr.Bottom := Row;
  Selection := sr;
end;

procedure TABSmartGrid.RectSelect(const sr: TGridRect);
begin
  //  4隅のどれかが、カレントセルなら、それを維持する
  if (sr.Left = Col) and (sr.Top = Row) then
  begin
    MoveColRow(sr.Right, sr.Bottom, True, False);
    MoveColRow(sr.Left, sr.Top,     False, False);
  end
  else if (sr.Left = Col) and (sr.Bottom = Row) then
  begin
    MoveColRow(sr.Right, sr.Top,    True, False);
    MoveColRow(sr.Left, sr.Bottom,  False, False);
  end
  else if (sr.Right = Col) and (sr.Top = Row) then
  begin
    MoveColRow(sr.Left, sr.Bottom,  True, False);
    MoveColRow(sr.Right, sr.Top,    False, False);
  end
  else if (sr.Right = Col) and (sr.Bottom = Row) then
  begin
    MoveColRow(sr.Left, sr.Top,     True, False);
    MoveColRow(sr.Right, sr.Bottom, False, False);
  end
  else Selection := sr;
end;

procedure TABSmartGrid.LineSelect(ACol, ARow: Longint; Count: Integer);
var
  sr: TGridRect;
begin
  sr.Left   := FixedCols;
  sr.Right  := ColCount - 1;
  sr.Top    := FixedRows;
  sr.Bottom := RowCount - 1;
  if (ACol < FixedCols) and (ARow < FixedRows) then
  begin
    //  nop
  end
  else if ACol < FixedCols then
  begin
    sr.Top    := ARow;
    sr.Bottom := ARow + Count - 1;
  end
  else if ARow < FixedRows then
  begin
    sr.Left  := ACol;
    sr.Right := ACol + Count - 1;
  end
  else
  begin
    Exit;
  end;

  //  範囲選択
  RectSelect(sr);
end;

procedure TABSmartGrid.ShowCurrentCell;
var
  r: Integer;
  c: Integer;
begin
  r := TopRow;
  c := LeftCol;

  if r > Row then r := Row;
  if c > Col then c := Col;

  if r + VisibleRowCount >= Row then r := Row - VisibleRowCount + 1;
  if c + VisibleColCount >= Col then c := Col - VisibleColCount + 1;

  if r < FixedRows then r := FixedRows;
  if c < FixedCols then c := FixedCols;

  if (r <> TopRow) or (c <> LeftCol) then
  begin
    TopRow  := r;
    LeftCol := c;
    TopLeftChanged;
  end;
end;

function  TABSmartGrid.GetUndoDebugInfo: String;
begin
  Result := FUndoBuffer.DumpForDebug;
end;

procedure TABSmartGrid.Undo;
var
  buf:  TABGridUndo;
  id:   Cardinal;
begin
  if Undoable and (not FUndoBuffer.Undoing) then
  begin
    FUndoBuffer.Undoing := True;
    id := FUndoBuffer.NextID;
    while (FUndoBuffer.NextID = id) and FUndoBuffer.Pop(buf) do
    begin
      Row := buf.ARow;
      Col := buf.ACol;
      Flags.OriginalText  := Cells[Col, Row];
      Flags.GotOriginal   := True;
      Cells[Col, Row]     := buf.Text;
      DoModified;
    end;
    ShowCurrentCell;
    FUndoBuffer.Undoing := False;
  end;
end;

procedure TABSmartGrid.ClearUndo;
begin
  FUndoBuffer.Clear;
end;

procedure TABSmartGrid.InsertRows(Index: Integer; Count: Integer);
var
  r, c: Integer;
begin
  if Index < 0 then Index := Selection.Top;
  if Count < 0 then Count := Selection.Bottom - Selection.Top + 1;
  if Count < 1 then Exit;

  //  増やして、ずらす
  RowCount := RowCount + Count;
  r := RowCount - Count - 1;
  while r >= Index do
  begin
    for c := 0 to ColCount - 1 do Cells[c, r + Count] := Cells[c, r];
    Dec(r);
  end;

  //  ふやした部分は空欄
  for r := Index to Index + Count - 1 do for c := 0 to ColCount - 1 do
  begin
    Cells[c, r] := '';
  end;

  //  ふやした部分を選択状態にする
  LineSelect(0, Index, Count);
end;

procedure TABSmartGrid.InsertCols(Index: Integer; Count: Integer);
var
  r, c: Integer;
begin
  if Index < 0 then Index := Selection.Left;
  if Count < 0 then Count := Selection.Right - Selection.Left + 1;
  if Count < 1 then Exit;

  //  増やして、ずらす
  ColCount := ColCount + Count;
  c := ColCount - Count - 1;
  while c >= Index do
  begin
    for r := 0 to RowCount - 1 do Cells[c + Count, r] := Cells[c, r];
    Dec(c);
  end;

  //  ふやした部分は空欄
  for c := Index to Index + Count - 1 do for r := 0 to RowCount - 1 do
  begin
    Cells[c, r] := '';
  end;

  //  ふやした部分を選択状態にする
  LineSelect(Index, 0, Count);
end;

procedure TABSmartGrid.DeleteRows(Index: Integer; Count: Integer);
var
  r, c: Integer;
begin
  if Index < 0 then Index := Selection.Top;
  if Count < 0 then Count := Selection.Bottom - Selection.Top + 1;
  if Count < 1 then Exit;

  if RowCount - Count <= FixedRows then Count := RowCount - FixedRows - 1;
  //  Gridの仕様上、Fixedでない行を1つ以上残さなければいけない。

  for r := Index to RowCount - Count - 1 do for c := 0 to ColCount - 1 do
  begin
    Cells[c, r] := Cells[c, r + Count];
  end;
  RowCount := RowCount - Count;
  if Index < RowCount
  then Row := Index
  else Row := RowCount - 1;
  ReleaseSelect;
end;

procedure TABSmartGrid.DeleteCols(Index: Integer; Count: Integer);
var
  r, c: Integer;
begin
  if Index < 0 then Index := Selection.Left;
  if Count < 0 then Count := Selection.Right - Selection.Left + 1;
  if Count < 1 then Exit;
  if Index < 0 then Exit;

  if ColCount - Count <= FixedCols then Count := ColCount - FixedCols - 1;
  //  Gridの仕様上、Fixedでない列を1つ以上残さなければいけない。

  for c := Index to ColCount - Count - 1 do for r := 0 to RowCount - 1 do
  begin
    Cells[c, r] := Cells[c + Count, r];
  end;
  ColCount := ColCount - Count;
  if Index < ColCount
  then Col := Index
  else Col := ColCount - 1;
  ReleaseSelect;
end;

procedure TABSmartGrid.CutCells;
begin
  CopySelectedCells;
  ClearSelectedCells;
  //ReleaseSelect;
end;

procedure TABSmartGrid.CopyCells;
begin
  CopySelectedCells;
  //ReleaseSelect;
end;

procedure TABSmartGrid.ClearCells;
begin
  ClearSelectedCells;
  //ReleaseSelect;
end;


procedure TABSmartGrid.CopySelectedCells;
var
  r: Longint;
  c: Longint;
  s: String;
  t: String;
begin
  s := '';
  for r := Selection.Top to Selection.Bottom do
  begin
    t := '';
    for c := Selection.Left to Selection.Right do
    begin
      t := t + TAB + Cells[c, r];
    end;
    Delete(t, 1, Length(TAB));
    s := s + t + CRLF;
  end;
  Clipboard.SetTextBuf(PChar(s));
end;

procedure TABSmartGrid.DoStartDelete;
begin
  if Assigned(FOnStartDelete) then FOnStartDelete(Self);
end;

procedure TABSmartGrid.DoEndDelete;
begin
  if Assigned(FOnEndDelete) then FOnEndDelete(Self);
end;

procedure TABSmartGrid.ClearSelectedCells;
var
  r:  Longint;
  c:  Longint;
  o:  String;
  cancel: Boolean;
  undo:   TABGridUndo;
  s:  String;
begin
  FRestCells := (Selection.Bottom - Selection.Top + 1) * (Selection.Right - Selection.Left + 1);
  DoStartDelete;
  try
    for r := Selection.Top to Selection.Bottom do
    for c := Selection.Left to Selection.Right do
    begin
      cancel := False;
      o := Cells[c, r];
      Flags.ModifiedMode := mmDeleted;
      try
        undo.ARow := r;
        undo.ACol := c;
        undo.Text := Cells[c, r];
        undo.ID   := GUndoCount;
        s := '';
        Dec(FRestCells);
        DoOnPreModified(c, r, o, s, cancel);
        Cells[c, r] := s;
        DoOnModified(c, r, o, s, cancel);
        if cancel then
          Cells[c, r] := o
        else
          FUndoBuffer.Push(undo);
      except
        Flags.ModifiedMode := mmNone;
        raise;
      end;
      Flags.ModifiedMode := mmNone;
    end;
    Inc(GUndoCount);
  finally
    FRestCells := 0;
    DoEndDelete;
  end;
end;

function NextToken(var s: String; const dlm: String): String;
var
  p: Integer;
begin
  p := Pos(dlm, s) - 1;
  if p < 0 then
  begin
    Result := s;
    s := '';
  end
  else
  begin
    Result := Copy(s, 1, p);
    Delete(s, 1, p + Length(dlm));
  end;
end;

procedure GetRowCol(src: String; var r: Integer; var c: Integer);
var
  n: Integer;
  p: Integer;
  s: String;
begin
  r := 0;
  c := 0;
  while src <> '' do
  begin
    Inc(r);
    s := NextToken(src, CRLF);
    n := 0;
    repeat
      Inc(n);
      p := Pos(TAB, s);
      Delete(s, 1, p);
    until p = 0;
    if c < n then c := n;
  end;
end;

procedure TABSmartGrid.DoStartPaste;
begin
  if Assigned(FOnStartPaste) then FOnStartPaste(Self);
end;

procedure TABSmartGrid.DoEndPaste;
begin
  if Assigned(FOnEndPaste) then FOnEndPaste(Self);
end;

function  TABSmartGrid.PasteCells(ACol: Integer; ARow: Integer;
                                   expandRow: Boolean;
                                   expandCol: Boolean): String;
var
  nr: Integer;
  nc: Integer;

  function  GetClipText: String;
  var
    h:  THandle;
    p:  PChar;
  begin
    //  クリップボードから取り出し
    Clipboard.Open;
    try
      h := Clipboard.GetAsHandle(CF_TEXT);
      p := GlobalLock(h);
      Result := p;
      GlobalUnlock(h);
    finally
      Clipboard.Close;
    end;
  end;

  procedure Expand(const s: String);
  begin
    //  Pasteする位置と大きさを調べる
    if ARow < 0 then ARow := Row;  //Selection.Top;
    if ACol < 0 then ACol := Col;  //Selection.Left;
    GetRowCol(s, nr, nc);
    if ARow + nr > RowCount then
    begin
      if expandRow
      then RowCount := ARow + nr
      else nr := RowCount - ARow;
    end;
    if ACol + nc > ColCount then
    begin
      if expandCol
      then ColCount := ACol + nc
      else nc := ColCount - ACol;
    end;
  end;

  procedure PasteIt(s: String);
  var
    r:  Integer;
    c:  Integer;
    t:  String;
    n:  String;
    o:  String;
    cancel: Boolean;
    undo:   TABGridUndo;
  begin
    //  貼り付ける
    r := 0;
    while r < nr do
    begin
      c := 0;
      t := NextToken(s, CRLF);
      while c < nc do
      begin
        //  いったん新しい内容にしてからイベントを呼び出している
        o := Cells[c + ACol, r + ARow];
        cancel := False;
        n := NextToken(t, TAB);
        Flags.ModifiedMode := mmPasted;
        try
          undo.ARow := r + ARow;
          undo.ACol := c + ACol;
          undo.Text := o;
          undo.ID   := GUndoCount;
          Dec(FRestCells);
          DoOnPreModified(c + ACol, r + ARow, o, n, cancel);
          Cells[c + ACol, r + ARow] := n;
          DoOnModified(c + ACol, r + ARow, o, n, cancel);
          if cancel then
            Cells[c + ACol, r + ARow] := o
          else
            FUndoBuffer.Push(undo);
        except
          Flags.ModifiedMode := mmNone;
          raise;
        end;
        Flags.ModifiedMode := mmNone;
        Inc(c);
      end;
      Inc(r);
    end;
    Inc(GUndoCount);
  end;

  procedure SelectAt;
  var
    sr: TGridRect;
    om: TABModifiedEvent;
    op: TABPreModifiedEvent;
  begin
    //  貼り付けた部分を選択状態にする
    sr.Top    := ARow;
    sr.Left   := ACol;
    sr.Bottom := ARow + nr - 1;
    sr.Right  := ACol + nc - 1;
    om := FOnModified;
    FOnModified := nil;
    op := FOnPreModified;
    FOnPreModified := nil;
    try
      RectSelect(sr);
    finally
      FOnModified    := om;
      FOnPreModified := op;
    end;
  end;

var
  s:  String;
begin
  s := GetClipText;

  if s <> '' then
  begin
    DoStartPaste;
    try
      Expand(s);
      FRestCells := nr * nc;
      PasteIt(s);
      Flags.ModifiedMode := mmPasted;   //範囲選択まで貼り付けの続き
      SelectAt;
      Invalidate;
    finally
      FRestCells := 0;
      DoEndPaste;
      Flags.ModifiedMode := mmNone;
    end;
    //  素の状態でセル選択を反映させる
    inherited SelectCell(Col, Row);
  end;
  //  残ったのが返り値
  Result := s;
end;

procedure TABSmartGrid.ClearAll(fixedcol, fixedrow: Boolean);
var
  r0: Integer;
  c0: Integer;
  r:  Integer;
  c:  Integer;
begin
  if fixedrow then
    r0 := 0
  else
    r0 := FixedRows;
  if fixedcol then
    c0 := 0
  else
    c0 := FixedCols;

  for r := r0 to RowCount - 1 do
    for c := c0 to ColCount - 1 do
      Cells[c, r] := '';

  for r := r0 to RowCount - 1 do
    RowColors[r] := clNone;
  for c := c0 to ColCount - 1 do
    ColColors[c] := clNone;
end;

function TABSmartGrid.CreateEditor: TInplaceEdit;
begin
  Result := TABInplaceEditList.Create(Self);
  with Result as TABInplaceEditList do
  begin
    DropDownRows      := FDropDownRows;
    OnEditButtonClick := OnEditButtonClicked;
    OnExit            := OnEditExit;
    Color             := FEditColor;
  end;
end;

function  TABSmartGrid.GetEditStyle(ACol, ARow: Longint): TEditStyle;
var
  i:  Integer;
  j:  Integer;
  di: TABCellDecorator;
begin
  Result := esSimple;
  if Assigned(FOnGetEditStyle) then
    FOnGetEditStyle(Self, ACol, ARow, Result)
  else
  begin
    for i := 0 to FCellDecorators.Count - 1 do
    begin
      di := FCellDecorators.Items[i];
      if di.Includes(ACol, ARow) and (di.SelectList.Count > 0) then
      begin
        SelectList.Clear;
        for j := 0 to di.SelectList.Count - 1 do
          SelectList.Add(di.SelectList.Strings[j]);
        Result := esPickList;
      end;
    end;
  end;
end;

procedure TABSmartGrid.SetSelectList(List: TStrings);
begin
  if Assigned(Flist) then
    Flist.Assign(List)
  else
    Flist := List;
end;

procedure TABSmartGrid.DoSetColNames;
var
  i:  Integer;
begin
  if (Self.RowCount > 0) and (FColNames.Count > 0) then
  begin
    for i := 0 to Self.ColCount - 1 do
    begin
      if i < FColNames.Count then
        Self.Cells[i, 0] := FColNames.Strings[i]
      else
        Self.Cells[i, 0] := '';
    end;
  end;
end;

procedure TABSmartGrid.DoSetRowNames;
var
  i:  Integer;
begin
  if (Self.ColCount > 0) and (FRowNames.Count > 0) then
  begin
    for i := 0 to Self.RowCount - 1 do
    begin
      if i < FRowNames.Count then
        Self.Cells[0, i] := FRowNames.Strings[i]
      else
        Self.Cells[0, i] := '';
    end;
  end;
end;

procedure TABSmartGrid.SetCellStrings;
var
  i:  Integer;
  r:  Integer;
  c:  Integer;
  d:  TABCellDecorator;
  s:  String;
begin
  for i := 0 to FCellDecorators.Count - 1 do
  begin
    d := FCellDecorators.Items[i];
    if d.Strings.Count > 0 then
    begin
      for r := 0 to d.RowCount - 1 do
      begin
        if r < d.Strings.Count then
          s := d.Strings.Strings[r]
        else
          s := '';
        for c := 0 to d.ColCount - 1 do
        begin
          Cells[d.Col + c, d.Row + r] := NextToken(s, #9);
        end;
      end;
    end;
  end;
end;

procedure TABSmartGrid.SetColNames(List: TStrings);
begin
  if Assigned(FColNames)
  then FColNames.Assign(List)
  else FColNames := List;
  DoSetColNames;
end;

procedure TABSmartGrid.SetRowNames(List: TStrings);
begin
  if Assigned(FRowNames)
  then FRowNames.Assign(List)
  else FRowNames := List;
  DoSetRowNames;
end;

procedure TABSmartGrid.SetColColor(i: Integer; c: TColor);
var
  n:  Integer;
  j:  Integer;
begin
  if (0 <= i) and (i < ColCount) then
  begin
    n := Length(FColColors);
    if i >= n then
    begin
      SetLength(FColColors, ColCount);
      for j := n to ColCount - 1 do
        FColColors[j] := clNone;
    end;
    FColColors[i] := c;
  end;
end;

procedure TABSmartGrid.SetRowColor(i: Integer; c: TColor);
var
  n:  Integer;
  j:  Integer;
begin
  if (0 <= i) and (i < RowCount) then
  begin
    n := Length(FRowColors);
    if i >= n then
    begin
      SetLength(FRowColors, RowCount);
      for j := n to RowCount - 1 do
        FRowColors[j] := clNone;
    end;
    FRowColors[i] := c;
  end;
end;

function  TABSmartGrid.GetColColor(i: Integer): TColor;
begin
  if (0 <= i) and (i < ColCount) and (i < Length(FColColors)) then
    Result := FColColors[i]
  else
    Result := clNone;
end;

function  TABSmartGrid.GetRowColor(i: Integer): TColor;
begin
  if (0 <= i) and (i < RowCount) and (i < Length(FRowColors)) then
    Result := FRowColors[i]
  else
    Result := clNone;
end;

procedure TABSmartGrid.CMFontChanged(var Message: TMessage);
begin
  inherited;
  SetHVFont;
end;

procedure TABSmartGrid.SetHVFont;
var
  LF: TLogFont;
  TF: TFont;
begin
  FFontH.Assign(Font);
  TF := TFont.Create;
  try
    TF.Assign(Font);
    GetObject(TF.Handle,SizeOf(LF),@LF);
    LF.lfEscapement  := 900;
    LF.lfOrientation := 900;
    TF.Handle := CreateFontIndirect(LF);
    FFontV.Assign(TF);
  finally
    TF.Free;
  end;
end;

function TABSmartGrid.DoModified: Boolean;
var
  undo:   TABGridUndo;
  s:      String;
begin
  Result := False;

  if Flags.GotOriginal then
  begin
    Result := ReadOnlyCell(Col, Row);
    s := Cells[Col, Row];
    Flags.ModifiedMode := mmEdited;
    try
      undo.ARow := Row;
      undo.ACol := Col;
      undo.Text := Flags.OriginalText;
      undo.ID   := GUndoCount;
      try
        DoOnPreModified(Col, Row, Flags.OriginalText, s, Result);
      except
        on e: Exception do
        begin
          PostErr(e.Message, Col, Row);
          Result := True;
        end;
      end;
      if not Result then
      begin
        Cells[Col, Row] := s;
        try
          DoOnModified(Col, Row, Flags.OriginalText, s, Result);
        except
          on e: Exception do
          begin
            PostErr(e.Message, Col, Row);
            Result := True;
          end;
        end;
      end;
      if Result then
        Cells[Col, Row] := Flags.OriginalText
      else if Flags.OriginalText <> s then
      begin
        FUndoBuffer.Push(undo);
        Inc(GUndoCount);
      end;
    except
      Flags.GotOriginal  := False;
      Flags.ModifiedMode := mmNone;
      raise;
    end;
    Flags.GotOriginal  := False;
    Flags.ModifiedMode := mmNone;
  end;
end;

procedure TABSmartGrid.DoOnModified(ACol, ARow: Integer;
  const OldText: String; var NewText: String; var Cancel: Boolean);
begin
  if Assigned(FOnModified) then
    FOnModified(Self, ACol, ARow, OldText, NewText, Cancel);
end;

procedure TABSmartGrid.DoOnPreModified(ACol, ARow: Integer;
      const OldText: String; var NewText: String; var Cancel: Boolean);
begin
  if Assigned(FOnPreModified) then
    FOnPreModified(Self, ACol, ARow, OldText, NewText, Cancel);
end;


function  TABSmartGrid.SelectCell(ACol, ARow: Longint): Boolean;
var
  i:  Integer;
begin
  Result := True;
  if Flags.MouseDowned = mdNone then
    RestoreOptEditing('SelectCell');

  if (Flags.ModifiedMode = mmBacking) then Exit;
  try
    Result := inherited SelectCell(ACol, ARow);
    if Result then
      Result := not DoModified;
  except
    on e: Exception do
    begin
      Result := False;
      PostErr(e.Message,Col, Row);
    end;
  end;

  if Result then
    for i := 0 to CellDecorators.Count - 1 do
      if CellDecorators.Items[i].Includes(ACol, ARow) then
        if CellDecorators.Items[i].ReadOnly then
          SnachOptEditing;

  if FRowSelectAtExit and (not Focused) then
    Invalidate;
end;

procedure TABSmartGrid.MoveCellTo(ACol, ARow: Integer; MoveAnchor: Boolean);
begin
  if SelectCell(ACol, ARow) then
    FocusCell(ACol, ARow, MoveAnchor);
end;

procedure TABSmartGrid.CMExit(var Message: TMessage);
begin
  if (Flags.ModifiedMode <> mmBacking) then
    DoModified;
  inherited;
end;

procedure TABSmartGrid.CMShowingChanged(var Message: TMessage);
begin
  inherited;
  if Showing then
    AdjustColWidths;
end;

procedure TABSmartGrid.OnEditButtonClicked(Sender: TObject);
begin
  if Assigned(FOnEditButtonClick) then
    FOnEditButtonClick(Self);
end;

procedure TABSmartGrid.OnEditorExit(Sender: TObject);
begin
  DoModified;
end;

procedure TABSmartGrid.FixUp;
begin
  EditorMode := False;
  DoModified;
end;

//procedure TABSmartGrid.GetDebugState(var ro, rs: Boolean; var gs: TGridState;
//  var st, rt: TDateTime; var rm: String);
procedure TABSmartGrid.GetDebugState(var ro, rs: Boolean; var rm: String);
begin
  ro := not (goEditing in Options);
  rs := goRangeSelect in Options;
  //gs := FGridState;
  //st := FSnached;
  //rt := FRestored;
  rm := FRestMsg;
end;

///////////////////////////////////////////////
{ TABGridUndoBuffer }

constructor TABGridUndoBuffer.Create;
begin
  inherited Create;
  FBuf      := TList.Create;
  FUndoing  := False;
end;

destructor  TABGridUndoBuffer.Destroy;
begin
  Clear;
  FBuf.Free;
  inherited Destroy;
end;

procedure TABGridUndoBuffer.Clear;
var
  undo: TABGridUndo;
begin
  while Pop(undo) do;
end;

function  TABGridUndoBuffer.GetCount: Integer;
begin
  Result := FBuf.Count;
end;

procedure TABGridUndoBuffer.Push(const undo: TABGridUndo);
var
  p:  PABGridUndo;
begin
  New(p);
  p^.ARow := undo.ARow;
  p^.ACol := undo.ACol;
  p^.Text := undo.Text;
  p^.ID   := undo.ID;
  FBuf.Add(p);
end;

function TABGridUndoBuffer.Pop(var undo: TABGridUndo): Boolean;
var
  p:  PABGridUndo;
begin
  if FBuf.Count > 0 then
  begin
    p := FBuf.Last;
    FBuf.Remove(p);
    undo.ARow := p^.ARow;
    undo.ACol := p^.ACol;
    undo.Text := p^.Text;
    undo.ID   := p^.ID;
    Dispose(p);
    Result := True;
  end
  else
    Result := False;
end;

function  TABGridUndoBuffer.NextID: Cardinal;
var
  p:  PABGridUndo;
begin
  if FBuf.Count > 0 then
  begin
    p := FBuf.Last;
    Result := p^.ID;
  end
  else
    Result := 0;
end;

function  TABGridUndoBuffer.DumpForDebug: String;
var
  i:  Integer;
  p:  PABGridUndo;
begin
  Result := IntToStr(FBuf.Count);
  if FUndoing then
    Result := Result + #9 + 'Undoing'
  else
    Result := Result + #9 + 'not Undoing';
  Result := Result + #13#10;

  for i := 0 to FBuf.Count - 1 do
  begin
    p := FBuf.Items[i];
    Result := Result + IntToStr(p^.ID);
    Result := Result + #9 + IntToStr(p^.ARow);
    Result := Result + #9 + IntToStr(p^.ACol);
    Result := Result + #9 + p^.Text + #13#10;
  end;
end;

///////////////////////////////////////////////
{ TABCellJoint }

constructor TABCellJoint.Create(ACollection: TCollection);
begin
  inherited;
  FCol      := 0;
  FRow      := 0;
  FColCount := 1;
  FRowCount := 1;
  FName     := 'ABCellJoint' + IntToStr(ACollection.Count);
  FTag      := 0;
  if ACollection is TABCellJoints then
    FOwner := (ACollection as TABCellJoints).OwnerGrid;
end;

procedure TABCellJoint.Assign(ASource: TPersistent);
var
  src: TABCellJoint;
begin
  inherited Assign(ASource);

  if ASource is TABCellJoint then
  begin
    src := ASource as TABCellJoint;
    FCol      := src.Col;
    FRow      := src.Row;
    FColCount := src.ColCount;
    FRowCount := src.RowCount;
    FName     := src.Name;
    FTag      := src.Tag;
    FOwner    := src.FOwner;
  end;
end;


procedure TABCellJoint.SetCol(AValue: Integer);
begin
  if FCol <> AValue then
  begin
    FCol := AValue;
    if Assigned(FOwner) then FOwner.Invalidate;
  end;
end;

procedure TABCellJoint.SetRow(AValue: Integer);
begin
  if FRow <> AValue then
  begin
    FRow := AValue;
    if Assigned(FOwner) then FOwner.Invalidate;
  end;
end;

procedure TABCellJoint.SetColCount(AValue: Integer);
begin
  if FColCount <> AValue then
  begin
    FColCount := AValue;
    if Assigned(FOwner) then FOwner.Invalidate;
  end;
end;

procedure TABCellJoint.SetRowCount(AValue: Integer);
begin
  if FRowCount <> AValue then
  begin
    FRowCount := AValue;
    if Assigned(FOwner) then FOwner.Invalidate;
  end;
end;

function  TABCellJoint.IsLeftTop(ACol, ARow: Integer): Boolean;
begin
  Result := (ACol = FCol) and (ARow = FRow);
end;

function  TABCellJoint.Includes(ACol, ARow: Integer): Boolean;
begin
  Result := False;
  if ACol < FCol then Exit;
  if ARow < FRow then Exit;

  if (FColCount < 0) and Assigned(FOwner) then
  begin
    //  負数の場合は、最後からのオフセット
    if ACol > FOwner.ColCount + FColCount then Exit;
  end
  else if ACol >= FCol + FColCount then Exit;

  if (FRowCount < 0) and Assigned(FOwner) then
  begin
    //  負数の場合は、最後からのオフセット
    if ARow > FOwner.RowCount + FRowCount then Exit;
  end
  else if ARow >= FRow + FRowCount then Exit;

  Result := True;
end;

function  TABCellJoint.JointCols: Integer;
begin
  if (FColCount < 0) and Assigned(FOwner) then
    Result := FOwner.ColCount - FCol + FColCount + 1
  else
    Result := FColCount;
end;

function  TABCellJoint.JointRows: Integer;
begin
  if (FRowCount < 0) and Assigned(FOwner) then
    Result := FOwner.RowCount - FRow + FRowCount + 1
  else
    Result := FRowCount;
end;

///////////////////////////////////////////////
{ TABCellDecorator }

constructor TABCellDecorator.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FName := 'ABCellDecorator' + IntToStr(ACollection.Count);
  FAlignment  := taLeftJustify;
  FLayout     := tlCenter;
  FFontColor  := clNone;
  FCellColor  := clNone;
  FVirtical   := False;
  FVisible    := True;
  FTopBorder  := False;
  FLeftBorder := False;
  FImeMode    := imDontCare;
  FEditMask   := '';
  FMaxLength  := 0;
  FPrecision  := 0;
  FBehindRCCol := True;
  FStrings    := TStringList.Create;
  FSelectList := TStringList.Create;
  if ACollection is TABCellDecorators then
    FOwner := (ACollection as TABCellDecorators).OwnerGrid;
end;

destructor  TABCellDecorator.Destroy;
begin
  FStrings.Free;
  inherited Destroy;
end;

procedure TABCellDecorator.Assign(ASource: TPersistent);
var
  src: TABCellDecorator;
begin
  inherited Assign(ASource);

  if ASource is TABCellDecorator then
  begin
    src := ASource as TABCellDecorator;
    FAlignment  := src.Alignment;
    FLayout     := src.Layout;
    FFontColor  := src.FontColor;
    FCellColor  := src.CellColor;
    FVirtical   := src.Virtical;
    FVisible    := src.Visible;
    FTopBorder  := src.TopBorder;
    FLeftBorder := src.LeftBorder;
    FImeMode    := src.ImeMode;
    FEditMask   := src.EditMask;
    FMaxLength  := src.MaxLength;
    Strings     := src.Strings;
  end;
end;

procedure TABCellDecorator.SetAlignment(AValue: TAlignment);
begin
  if FAlignment <> AValue then
  begin
    FAlignment := AValue;
    if Assigned(FOwner) then FOwner.Invalidate;
  end;
end;

procedure TABCellDecorator.SetLayout(AValue: TTextLayout);
begin
  if FLayout <> AValue then
  begin
    FLayout := AValue;
    if Assigned(FOwner) then FOwner.Invalidate;
  end;
end;

procedure TABCellDecorator.SetFontColor(AValue: TColor);
begin
  if FFontColor <> AValue then
  begin
    FFontColor := AValue;
    if Assigned(FOwner) then FOwner.Invalidate;
  end;
end;

procedure TABCellDecorator.SetCellColor(AValue: TColor);
begin
  if FCellColor <> AValue then
  begin
    FCellColor := AValue;
    if Assigned(FOwner) then FOwner.Invalidate;
  end;
end;

procedure TABCellDecorator.SetVirtical(AValue: Boolean);
begin
  if FVirtical <> AValue then
  begin
    FVirtical := AValue;
    if Assigned(FOwner) then FOwner.Invalidate;
  end;
end;

procedure TABCellDecorator.SetVisible(AValue: Boolean);
begin
  if FVisible <> AValue then
  begin
    FVisible := AValue;
    if Assigned(FOwner) then FOwner.Invalidate;
  end;
end;

procedure TABCellDecorator.SetTopBorder(AValue: Boolean);
begin
  if FTopBorder <> AValue then
  begin
    FTopBorder := AValue;
    if Assigned(FOwner) then FOwner.Invalidate;
  end;
end;

procedure TABCellDecorator.SetLeftBorder(AValue: Boolean);
begin
  if FLeftBorder <> AValue then
  begin
    FLeftBorder := AValue;
    if Assigned(FOwner) then FOwner.Invalidate;
  end;
end;

procedure TABCellDecorator.SetStrings(AValue: TStrings);
begin
  if Assigned(FStrings) then
    FStrings.Assign(AValue)
  else
    FStrings := AValue;
  if Assigned(FOwner) then
    FOwner.SetCellStrings;
end;

procedure TABCellDecorator.SetSelectList(AValue: TStrings);
begin
  if Assigned(FSelectList) then
    FSelectList.Assign(AValue)
  else
    FSelectList := AValue;
end;

///////////////////////////////////////////////
{ TABCellJoints }

constructor TABCellJoints.Create(AOwner: TABSmartGrid);
begin
  inherited Create(TABCellJoint);
  FOwner := AOwner;
end;

procedure TABCellJoints.Update(AItem: TCollectionItem);
begin
  inherited;
  if csDesigning in FOwner.ComponentState then FOwner.Repaint;
end;

function  TABCellJoints.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TABCellJoints.AssignTo(ADest: TPersistent);
begin
  if ADest is TABCellJoints then
    (ADest as TABCellJoints).Assign(Self)
  else
    inherited AssignTo(ADest);
end;

function TABCellJoints.Add: TABCellJoint;
begin
  Result := (inherited Add) as TABCellJoint;
end;

function  TABCellJoints.GetItem(Index: integer): TABCellJoint;
begin
  Result := (inherited GetItem(Index)) as TABCellJoint;
end;

procedure TABCellJoints.SetItem(Index: integer; Value: TABCellJoint);
begin
  inherited SetItem(Index, Value);
end;

///////////////////////////////////////////////
{ TABCellDecorators }

constructor TABCellDecorators.Create(AOwner: TABSmartGrid);
begin
  inherited Create(TABCellDecorator);
  FOwner := AOwner;
end;

procedure TABCellDecorators.Update(AItem: TCollectionItem);
begin
  inherited;
  if csDesigning in FOwner.ComponentState then FOwner.Repaint;
end;

function  TABCellDecorators.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TABCellDecorators.AssignTo(ADest: TPersistent);
begin
  if ADest is TABCellDecorators then
    (ADest as TABCellDecorators).Assign(Self)
  else
    inherited AssignTo(ADest);
end;

function TABCellDecorators.Add: TABCellDecorator;
begin
  Result := (inherited Add) as TABCellDecorator;
end;

function  TABCellDecorators.GetItem(Index: Integer): TABCellDecorator;
begin
  Result := (inherited GetItem(Index)) as TABCellDecorator;
end;

procedure TABCellDecorators.SetItem(Index: Integer; Value: TABCellDecorator);
begin
  inherited SetItem(Index, Value);
end;

//////////////////////////////////////////////////////////////////
{ EABCtrlException }

constructor EABCtrlException.CreateAt(const msg: String; ctrl: TWinControl;
                                   col, row: Integer);
begin
  inherited Create(msg);
  FCtrl := ctrl;
  FRow  := row;
  FCol  := col;
end;

procedure EABCtrlException.BackToOccurred;
begin
  if Assigned(FCtrl) then
  begin
    if FCtrl is TABSmartGrid then
    begin
      (FCtrl as TABSmartGrid).Flags.ModifiedMode := mmBacking;
      try
        (FCtrl as TABSmartGrid).SetFocus;
        if FCol >= 0 then
          (FCtrl as TABSmartGrid).MoveCellTo(FCol, FRow)
        else
          (FCtrl as TABSmartGrid).LineSelect(FCol, FRow);
      finally
        (FCtrl as TABSmartGrid).Flags.ModifiedMode := mmNone;
      end;
    end
    else if FCtrl is TCustomDrawGrid then
    begin
      (FCtrl as TCustomDrawGrid).SetFocus;
      (FCtrl as TCustomDrawGrid).Row := FRow;
      (FCtrl as TCustomDrawGrid).Col := FCol;
    end
    else
      FCtrl.SetFocus;
  end;
end;




///////////////////////////////////////////////
{ TABInplaceEditList }

constructor TABInplaceEditList.Create(AOwner: TComponent);
var
  mnu:  TABEditPopupMenu;
begin
  inherited Create(AOwner);

  if not (csDesigning in ComponentState) then
  begin
    mnu := TABEditPopupMenu.Create(Self);
    mnu.FEdit := Self;
    PopupMenu := mnu;
  end;
end;

procedure TABInplaceEditList.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
  begin
    Text := (Grid as TABSmartGrid).Flags.OriginalText;
    //SetSel(Length(Text), Length(Text));
    Modified := False;
    (Grid as TABSmartGrid).EditorMode := False;
  end;
  inherited KeyDown(Key, Shift);
end;

procedure TABInplaceEditList.KeyPress(var Key: Char);

  procedure CheckPeriod(var Key: Char; deny: Boolean);
  var
    i:  Integer;
    s:  String;
  begin
    if deny then
      Key := #0
    else
    begin
      s := Text;
      for i := 1 to Length(s) do
      begin
        if s[i] = '.' then
        begin
          Key := #0;
          Break;
        end;
      end;
    end;
  end;

  procedure CheckMinus(var Key: Char; deny: Boolean);
  var
    s:  String;
  begin
    if deny then
      Key := #0
    else
    begin
      s := Text;
      if SelStart > 0 then  //先頭じゃなきゃだめ
        Key := #0
      else if Length(s) > 0 then
      begin
        //  その後が数字じゃなきゃだめ
        if s[1] < '0' then Key := #0;
        if s[1] > '9' then Key := #0;
      end;
    end;
  end;

  function  IsImmOpen(h: THandle): Boolean;
  var
    imc:  HIMC;
  begin
    Result := False;
    imc := ImmGetContext(h);
    if imc <> 0 then
    begin
      try
        Result := ImmGetOpenStatus(imc);
      finally
        ImmReleaseContext(h, imc);
      end;
    end;
  end;

var
  opts: TABNumericOptions;
  i:    Integer;
begin
  opts := [];
  if Grid is TABSmartGrid then
    with Grid as TABSmartGrid do
      for i := 0 to CellDecorators.Count - 1 do
        if CellDecorators.Items[i].Includes(Col, Row) then
          opts := CellDecorators.Items[i].NumOptions;

  if nuEnabled in opts then
  begin
    //  数値入力オンリーの場合
    case Key of
      '-':  CheckMinus(Key, nuDenyNegative in opts);
      '.':  CheckPeriod(Key, nuIntegerOnly in opts);
    else if not (Key in ['0'..'9', #8, #10, #13]) then
      Key := #0;
    end;
  end;

  if (Key <> #0) and (not ListVisible) then
  begin
    with Grid as TABSmartGrid do
    begin
      if GetEditStyle(Col, Row) = esPickList then
      begin
        DropDown;
        if IsImmOpen(Self.Handle) and Assigned(ActiveList) then
          SendMessage(ActiveList.Handle, WM_CHAR, Word(BS), 0);
        PostMessage(ActiveList.Handle, WM_CHAR, Word(Key), 0);
      end;
    end;
  end;
  inherited KeyPress(Key);
end;

procedure TABInplaceEditList.DoGetPickListItems;
begin
  PickList.Items.Text := (Grid as TABSmartGrid).SelectList.Text;
end;

procedure TABInplaceEditList.WndProc(var msg: TMessage);

  procedure DoEnter;
  begin
    with Grid as TABSmartGrid do
    begin
      if Assigned(OnEditEnter) then
        OnEditEnter(Grid);
    end;
  end;

  procedure DoExit;
  begin
    with Grid as TABSmartGrid do
    begin
      if Assigned(OnEditExit) then
        OnEditExit(Grid);
    end;
  end;

begin
  inherited WndProc(msg);
  if msg.Msg = WM_CHAR then
  begin
    if ListVisible and (ActiveList = PickList) and
                        (PickList.ItemIndex >= 0) and
                        PickList.Selected[PickList.ItemIndex] then
      Text := PickList.Items.Strings[PickList.ItemIndex];
  end
  else if (msg.Msg = WM_COMMAND) and (THandle(msg.LParam) = Self.Handle) then
  begin
    case msg.WParamHi of
      EN_SETFOCUS:  DoEnter;
      EN_KILLFOCUS: DoExit;
    end;
  end;
end;

procedure TABInplaceEditList.CloseUp(Accept: Boolean);
var
  lst:  TCustomListbox;
  grd:  TABSmartGrid;
  cancel:  Boolean;
begin
  if ListVisible and (ActiveList = PickList) then
    lst := PickList
  else
    lst := nil;

  grd := Grid as TABSmartGrid;
  if Assigned(lst) and Accept then
    grd.Flags.PickedIndex := lst.ItemIndex;

  inherited CloseUp(Accept);

  if Assigned(lst) and (lst.Items.IndexOf(Text) < 0) then
  begin
    Text := '';
    Modified := True;
    with Grid As TABSmartGrid do
    begin
      SetEditText(Col, Row, '');
      Invalidate;
    end;
  end;

  if Assigned(lst) and Accept and Assigned(grd.OnModified) and grd.ModifyAtCloseup then
  begin
    grd.Flags.ModifiedMode := mmPicked;
    grd.OnModified(grd, grd.Col, grd.Row, grd.Flags.OriginalText, Text, cancel);
    if cancel then
      Text := grd.Flags.OriginalText
    else
      grd.Flags.OriginalText := Text;
  end;
end;


////////////////////////////////////////////////////////////////////
{ TABEditPopupMenu }

constructor TABEditPopupMenu.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCutMenu := TMenuItem.Create(Self);
  FCutMenu.Caption    := '切り取り(&T)';
  FCutMenu.ShortCut   := TextToShortCut('Ctrl+X');
  FCutMenu.OnClick    := CutClick;
  Items.Add(FCutMenu);

  FCopyMenu := TMenuItem.Create(Self);
  FCopyMenu.Caption   := 'コピー(&C)';
  FCopyMenu.ShortCut  := TextToShortCut('Ctrl+C');
  FCopyMenu.OnClick   := CopyClick;
  Items.Add(FCopyMenu);

  FPasteMenu := TMenuItem.Create(Self);
  FPasteMenu.Caption  := '貼り付け(&P)';
  FPasteMenu.ShortCut := TextToShortCut('Ctrl+V');
  FPasteMenu.OnClick  := PasteClick;
  Items.Add(FPasteMenu);

  FDeleteMenu := TMenuItem.Create(Self);
  FDeleteMenu.Caption := '削除(&D)';
  FDeleteMenu.OnClick := DeleteClick;
  Items.Add(FDeleteMenu);
end;

procedure TABEditPopupMenu.CutClick(Sender: TObject);
begin
  if Assigned(FEdit) then
    FEdit.CutToClipboard;
end;

procedure TABEditPopupMenu.CopyClick(Sender: TObject);
begin
  if Assigned(FEdit) then
    FEdit.CopyToClipboard;
end;

procedure TABEditPopupMenu.PasteClick(Sender: TObject);
begin
  if Assigned(FEdit) then
    FEdit.PasteFromClipboard;
end;

procedure TABEditPopupMenu.DeleteClick(Sender: TObject);
begin
  if Assigned(FEdit) then
    FEdit.ClearSelection;
end;


////////////////////////////////////////////////////////////////////
{ TABGridPaintedList }

constructor TAbGridPaintedList.Create;
begin
  inherited;
  FCount    := 0;
  FCapacity := 256;
  SetLength(FList, FCapacity);
end;

destructor  TAbGridPaintedList.Destroy;
begin
  FCount    := 0;
  FCapacity := 0;
  SetLength(FList, FCapacity);
  inherited;
end;

{
function TABInplaceEditList.GetPickList: TCustomListbox;
var
  PopupListbox: TABPopupListbox;
begin
  if not Assigned(FPickList) then
  begin
    PopupListbox := TABPopupListbox.Create(Self);
    PopupListbox.Visible := False;
    PopupListbox.Parent := Self;
    PopupListbox.OnMouseUp := ListMouseUp;
    PopupListbox.IntegralHeight := True;
    PopupListbox.ItemHeight := 11;
    FPickList := PopupListBox;
  end;
  Result := FPickList;
end;
}

procedure TAbGridPaintedList.Add(ACol, ARow: Integer);
begin
  while FCount >= FCapacity do
  begin
    Inc(FCapacity, 256);
    SetLength(FList, FCapacity);
  end;
  FList[FCount].X := ACol;
  FList[FCount].Y := ARow;
  Inc(FCount);
end;

procedure TAbGridPaintedList.Clear;
begin
  FCount := 0;
end;

function  TAbGridPaintedList.Include(ACol, ARow: Integer): Boolean;
var
  i:  Integer;
begin
  Result := False;
  for i := 0 to FCount - 1 do
  begin
    if (ACol = FList[i].X) and (ARow = FList[i].Y) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;




////////////////////////////////////////////////////////////////////

procedure Register;
begin
  RegisterComponents('ABplus', [TABSmartGrid]);
end;

initialization

  GUndoCount := 1;

finalization

end.
