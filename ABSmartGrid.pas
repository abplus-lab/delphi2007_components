unit ABSmartGrid;

interface

{:ToDo::
  �E�J�����g�Z���������Ă���Z���̐F���f�t�H���g�̂܂܂Ȃ̂��_�T��
  �E�s�^��I���ŃJ�����g�Z������ԉ��^�E�ɂȂ��Ă��܂��̂��_�T��
  �EUndo��������
  �EgoEditing��False�̂Ƃ��Ƃ��AgoAlwaysShowEditor��True�̂Ƃ��ɁA
    ForceRowSelect�̋��������܂����s����
::ToDo:}

uses
  Classes, Types, Graphics, Controls, StdCtrls, Grids, Forms, Menus;

type

  TABSmartGridMoveAtExit = (maeStay,    //  �ҏW�セ�̂܂�
                            maeUp,      //  �ҏW��A��Ɉړ�
                            maeDown,    //  �ҏW��A���Ɉړ�
                            maeLeft,    //  �ҏW��A���Ɉړ�
                            maeRight);  //  �ҏW��A�E�Ɉړ�
  TABSmartGridPreDrawEvent = procedure (Sender: TObject; ACol, ARow: Integer;
                                      AState: TGridDrawState;
                                      var Alignment: TAlignment;
                                      var FColor, BColor: TColor) of Object;
  TABSmartGridGetEditStyleEvent = procedure (Sender: TObject;
                                      ACol, ARow: Integer;
                                      var EditStyle: TEditStyle) of Object;
  TABSmartGridFixedSelectable =  (fsRow,  //  �s�I���\
                                  fsCol); //  ��I���\
  TABSmartGridFixedSelectables = set of TABSmartGridFixedSelectable;
  TABSmartGridKBShortCut = (kscDelete,    //  Delete�L�[�ŁA�I��̈�̍폜
                            kscCut,       //  ^C�ŁA�I��̈�̃J�b�g
                            kscCopy,      //  ^X�ŁA�I��̈�̃R�s�[
                            kscPaste,     //  ^V�ŁA�y�[�X�g
                            kscUndo);     //  ^X�ŁA�A���h�D
  TABSmartGridKBShortCuts = set of TABSmartGridKBShortCut;
  TABSmartGridMouseDowned =  (mdNone,       //  �������
                              mdFixedAll,   //  �s��Ƃ��Œ�
                              mdFixedCol,   //  �Œ��
                              mdFixedRow,   //  �Œ�s
                              mdEditCell);  //  �ʏ�̃Z��

  TABSmartGridUndo = record
    ARow: Integer;
    ACol: Integer;
    Text: String;
  end;

  TABSmartGridEdit = class(TInplaceEditList)
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  end;

  TABSmartGrid = class(TStringGrid)
  private
    { Private �錾 }
    FFixedRowMenu:      TPopupMenu;
    FFixedColMenu:      TPopupMenu;
    FEditCellMenu:      TPopupMenu;
    FMoveAtExit:        TABSmartGridMoveAtExit;
    FOnPreDraw:         TABSmartGridPreDrawEvent;
    FOnGetEditStyle:    TABSmartGridGetEditStyleEvent;
    FFixedSelect:       TABSmartGridFixedSelectables;
    FKBShortcut:        TABSmartGridKBShortCuts;
    //FUndoBuffer:        TABSmartGridUndo;
    FEditing:           Boolean;
    FJumping:           Boolean;
    FMouseDowned:       TABSmartGridMouseDowned;
    FLastPressed:       DWord;
    FList:              TStrings;
    FDropDownRows:      Integer;
    FEditList:          TInplaceEditList;
    FOnEditButtonClick: TNotifyEvent;
    procedure SetEditCellMenu(AMenu: TPopupMenu);
    procedure SetFixedRowMenu(AMenu: TPopupMenu);
    procedure SetFixedColMenu(AMenu: TPopupMenu);
    procedure SetMoveAtExit(where: TABSmartGridMoveAtExit);
    function  GetUndoable: Boolean;
    procedure CopySelectedCells;
    procedure ClearSelectedCells;
    procedure SetSelectList(List: TStrings);
    procedure SetDropDownCount(Value: Integer);
    procedure SetOnEditButtonClick(const Value: TNotifyEvent);
    procedure EditListGetItems(ACol, ARow: Integer; Items: TStrings);
  protected
    { Protected �錾 }
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    function  GetEditText(ACol, ARow: Longint): string; override;
    function  IsSelected(ACol, ARow: Longint): Boolean;
    function  CreateEditor: TInplaceEdit; override;
    function  GetEditStyle(ACol, ARow: Longint): TEditStyle; override;
    procedure DoEnter; override;
    procedure Click; override;
  public
    { Public �錾 }
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy;  override;
    procedure Update;     override;
    procedure Invalidate; override;
	  procedure InsertRows(Index: Integer = -1; Count: Integer = -1);  virtual;
	  procedure InsertCols(Index: Integer = -1; Count: Integer = -1);  virtual;
	  procedure DeleteRows(Index: Integer = -1; Count: Integer = -1);  virtual;
	  procedure DeleteCols(Index: Integer = -1; Count: Integer = -1);  virtual;
    procedure ReleaseSelect;
    procedure LineSelect(ACol, ARow: Longint; Count: Integer);
    procedure RectSelect(const sr: TGridRect);
    procedure ClearCells; virtual;
    procedure CutCells;   virtual;
    procedure CopyCells;  virtual;
    procedure Undo;       virtual;
    function  PasteCells(ACol: Integer = -1;
                         ARow: Integer = -1;
                         expandRow: Boolean = True;
                         expandCol: Boolean = False): String; virtual;
    property  Undoable: Boolean read GetUndoable;
    property  Selected[ACol, ARow: Integer]: Boolean read IsSelected;
  published
    { Published �錾 }
    property MoveAtExit: TABSmartGridMoveAtExit read  FMoveAtExit
                                              write SetMoveAtExit
                                              default maeStay;
    property EditCellMenu: TPopupMenu read  FEditCellMenu
                                      write SetEditCellMenu;
    property FixedRowMenu: TPopupMenu read  FFixedRowMenu
                                      write SetFixedRowMenu;
    property FixedColMenu: TPopupMenu read  FFixedColMenu
                                      write SetFixedColMenu;
    property OnPreDraw: TABSmartGridPreDrawEvent read  FOnPreDraw
                                                 write FOnPreDraw;
    property OnGetEditStyle: TABSmartGridGetEditStyleEvent read  FOnGetEditStyle
                                                           write FOnGetEditStyle;
    property FixedSelectable: TABSmartGridFixedSelectables read  FFixedSelect
                                                           write FFixedSelect;
    property KeyboardShortCut: TABSmartGridKBShortCuts read  FKBShortcut
                                                       write FKBShortcut;
    property SelectList: TStrings read FList write SetSelectList;
    property EditList: TInplaceEditList read FEditList;
    property DropDownCount: Integer read  FDropDownRows
                                    write SetDropDownCount default 8;
    property OnEditButtonClick: TNotifyEvent  read  FOnEditButtonClick
                                              write SetOnEditButtonClick;
  end;

procedure Register;



/////////////////////////////////////////
/////////////////////////////////////////
/////////////////////////////////////////
implementation

uses
  Windows, Clipbrd;

const
  TAB  = #9;
  CRLF = #13#10;

/////////////////////////////////////////
{ TABSmartGrid }

constructor TABSmartGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Options := Options + [goEditing, goColSizing];
  DefaultRowHeight := 18;
  FMoveAtExit   := maeStay;
  FFixedSelect  := [fsRow, fsCol];
  FKBShortcut   := [kscDelete, kscCut, kscCopy, kscPaste];
  FLastPressed  := 0;
  FDropDownRows := 8;
  FMouseDowned  := mdNone;
  FJumping      := False;
  FList := TStringList.Create;
end;

destructor  TABSmartGrid.Destroy;
begin
  FList.Free;
  inherited;
end;

//  overrided methods

procedure TABSmartGrid.Click;
begin
  //  �W�����v���͕ۗ�
  if not FJumping then inherited Click;
end;

procedure TABSmartGrid.Update;
begin
  //  �W�����v���͕ۗ�
  if not FJumping then inherited Update;
end;

procedure TABSmartGrid.Invalidate;
begin
  //  �W�����v���͕ۗ�
  if not FJumping then inherited Invalidate;
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

  function doneUp: Boolean;
  begin
    if Row = FixedRows
    then Result := True
    else if empty
    then Result := (Cells[Col, Row    ] <> '')
    else Result := (Cells[Col, Row - 1] =  '');
  end;

  function doneDown: Boolean;
  begin
    if Row = RowCount - 1
    then Result := True
    else if empty
    then Result := (Cells[Col, Row    ] <> '')
    else Result := (Cells[Col, Row + 1] =  '');
  end;

  function doneLeft: Boolean;
  begin
    if Col = FixedCols
    then Result := True
    else if empty
    then Result := (Cells[Col,     Row] <> '')
    else Result := (Cells[Col - 1, Row] =  '');
  end;

  function doneRight: Boolean;
  begin
    if Col = ColCount - 1
    then Result := True
    else if empty
    then Result := (Cells[Col,     Row] <> '')
    else Result := (Cells[Col + 1, Row] =  '');
  end;

begin

  done := False;

  //  Ctrl+���L�[�ŁA�W�����v
  if jumpKey then
  begin
    jshift := Shift - [ssCtrl];
    FJumping := True;
    empty  := (Cells[Col, Row] = '');
    if not empty then
    begin
      //  �Ƃ肠������񓮂����Ă݂āA���̎��ɃZ�����󗓂��ǂ����𒲂ׂ�
      inherited KeyDown(Key, jshift);
      empty  := (Cells[Col, Row] = '');
    end;
    //  ���ꂼ��̕����ɃW�����v
    repeat
      r := Row;
      c := Col;
      case Key of
        VK_UP:    done := doneUp;
        VK_DOWN:  done := doneDown;
        VK_LEFT:  done := doneLeft;
        VK_RIGHT: done := doneRight;
      end;
      if not done then inherited KeyDown(Key, jshift);
      if (r = Row) and (c = Col) then
      begin
        //  �I���ł��Ȃ��Z���ɍs�������Ă��܂���
        done := True;
      end;
    until done;
    FJumping := False;
    //  �ۗ��ɂ��Ă����������s��
    Invalidate;
    Update;
    Click;
  end;

  //  Delete�L�[�őI��͈͂̍폜
  if (Key = VK_DELETE) and (Shift = []) and (kscDelete in FKBShortcut) then
  begin
    ClearCells;
    done := True;
  end;

  //  �����ŏ������Ă��Ȃ���΁A���Ƃ��Ƃ̑���
  if not done then inherited KeyDown(Key, Shift);
end;

procedure TABSmartGrid.KeyPress(var Key: Char);
var
  movedir: TABSmartGridMoveAtExit;
begin
  if (Key = #13) and EditorMode
  then movedir := FMoveAtExit
  else movedir := maeStay;

  inherited;
  case Key of
    #3:   if (kscCopy  in FKBShortcut) then CopyCells;
    #22:  if (kscPaste in FKBShortcut) then PasteCells;
    #24:  if (kscCut   in FKBShortcut) then CutCells;
    #26:  if (kscUndo  in FKBShortcut) then Undo;
  end;

  case movedir of
    maeUp:    if Row > FixedRows    then Row := Row - 1;
    maeDown:  if Row < RowCount - 1 then Row := Row + 1;
    maeLeft:  if Col > FixedCols    then Col := Col - 1;
    maeRight: if Col < ColCount - 1 then Col := Col + 1;
  end;
end;

procedure TABSmartGrid.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  r,  c:  Longint;
  nowtick: DWord;
begin

  nowtick := GetTickCount;
  MouseToCell(X, Y, c, r);
  FMouseDowned := mdEditCell;

  if (c = Col) and (r = Row)
      and (nowtick < FLastPressed + 1000) and (Button = mbLeft) then
  begin
    //  �I������Ă���Z����������x�N���b�N�����Ƃ��́A���ʂ̓���
    inherited;
    Exit;
  end;
  if not (goRangeSelect in Options) and (Button = mbLeft)
      and (c >= FixedCols) and (r >= FixedRows) then
  begin
    //  �͈͑I�����Ȃ��ꍇ�́A���ʂ̓���
    inherited;
    Exit;
  end;
  FLastPressed := nowtick;

  //  �����I�ɕҏW�Ȃ��ɂ���
  FEditing := goEditing in Options;
  Options := Options - [goEditing];
  //  ���j���[�̕ύX
  if c < FixedCols then
  begin
    if r < FixedRows
    then PopupMenu := nil
    else PopupMenu := FFixedColMenu;
  end
  else
  begin
    if r < FixedRows
    then PopupMenu := FFixedRowMenu
    else PopupMenu := FEditCellMenu;
  end;
  //  �{���̓���
  inherited;
  if not (FGridState in [gsNormal, gsSelecting]) then Exit;

  if c < FixedCols then
  begin
    if r < FixedRows then
    begin
      FMouseDowned := mdFixedAll;
    end
    else
    begin
      //  �s�I��
      if (fsRow in FFixedSelect) and (Button = mbLeft) then
      begin
        MoveColRow(ColCount - 1, r, True,  False);
        MoveColRow(FixedCols,    r, False, False);
        FGridState := gsSelecting;
        Invalidate;
      end;
      FMouseDowned := mdFixedCol;
    end
  end
  else
  begin
    if r < FixedRows then
    begin
      //  ��I��
      if (fsCol in FFixedSelect) and (Button = mbLeft) then
      begin
        MoveColRow(c, RowCount - 1, True,  False);
        MoveColRow(c, FixedRows ,   False, False);
        FGridState := gsSelecting;
        Invalidate;
      end;
      FMouseDowned := mdFixedRow;
    end
    else
    begin
      //  �ҏW�\�ȃZ��
      FMouseDowned := mdEditCell;
    end;
  end;
end;

procedure TABSmartGrid.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  c, r: Integer;
begin
  inherited;
  if FMouseDowned in [mdFixedCol, mdFixedRow] then
  begin
    MouseToCell(X, Y, c, r);
    if c < FixedCols then c := FixedCols;
    if r < FixedRows then r := FixedRows;
    MoveColRow(c, r, False, False);
  end;
end;

procedure TABSmartGrid.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if FEditing then Options := Options + [goEditing];
  FMouseDowned := mdNone;
end;

procedure TABSmartGrid.DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState);
var
  Hold: Integer;
  Alignment: TAlignment;
  FColor: TColor;
  BColor: TColor;
  Left, Top: Integer;
  s: String;
  w: Integer;
  fc: TColor;
  pc: TColor;
  bc: TColor;
begin
  if gdFixed in AState then
    Alignment := taCenter
  else
    Alignment := taLeftJustify;
  fc := Canvas.Font.Color;
  pc := Canvas.Pen.Color;
  bc := Canvas.Brush.Color;
  FColor := fc;
  BColor := bc;
  if Assigned(FOnPreDraw)
  then FOnPreDraw(Self, ACol, ARow, AState, Alignment, FColor, BColor);
  if DefaultDrawing then
  begin
    s := Cells[ACol, ARow];
    w := Canvas.TextWidth(s);
    Top  := (ARect.Top + ARect.Bottom - Canvas.TextHeight(s)) div 2;
    Left := ARect.Left;
    case Alignment of
      taLeftJustify:  Left := ARect.Left + 2;
      taRightJustify: Left := ARect.Right - w - 2;
      taCenter:       Left := (ARect.Left + ARect.Right - w) div 2;
    end;
    if not (gdFixed in AState) then
    begin
      Canvas.Pen.Color   := FColor;
      Canvas.Font.Color  := FColor;
      Canvas.Brush.Color := BColor;
    end;
    Canvas.FillRect(ARect);
    Canvas.TextRect(ARect, Left, Top, s);
    Canvas.Pen.Color   := pc;
    Canvas.Font.Color  := fc;
    Canvas.Brush.Color := bc;
  end;
  //inherited DrawCell(ACol, ARow, ARect, AState);
  //  �p�����ŕ`�悵�Ă��܂��̂ŁA�p�����̂���Ɍp����(TCustomGrid)��DrawCell
  //  �Ɠ�������������
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
end;

function  TABSmartGrid.GetEditText(ACol, ARow: Longint): string;
begin
  Result := inherited GetEditText(ACol, ARow);
  PopupMenu := nil;
end;

procedure TABSmartGrid.DoEnter;
begin
  PopupMenu := FEditCellMenu;
  inherited;
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

procedure TABSmartGrid.SetEditCellMenu(AMenu: TPopupMenu);
begin
  FEditCellMenu := AMenu;
  inherited PopupMenu := AMenu;
end;

procedure TABSmartGrid.SetFixedRowMenu(AMenu: TPopupMenu);
begin
  FFixedRowMenu := AMenu;
end;

procedure TABSmartGrid.SetFixedColMenu(AMenu: TPopupMenu);
begin
  FFixedColMenu := AMenu;
end;


procedure TABSmartGrid.SetMoveAtExit(where: TABSmartGridMoveAtExit);
begin
  FMoveAtExit := where;
end;

function  TABSmartGrid.GetUndoable: Boolean;
begin
  Result := False;
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
  //  4���̂ǂꂩ���A�J�����g�Z���Ȃ�A������ێ�����
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

  //  �͈͑I��
  RectSelect(sr);
end;

procedure TABSmartGrid.Undo;
begin
  //  ������
end;

procedure TABSmartGrid.InsertRows(Index: Integer; Count: Integer);
var
  r, c: Integer;
begin
  if Index < 0 then Index := Selection.Top;
  if Count < 0 then Count := Selection.Bottom - Selection.Top + 1;
  if Count < 1 then Exit;

  //  ���₵�āA���炷
  RowCount := RowCount + Count;
  r := RowCount - Count - 1;
  while r >= Index do
  begin
    for c := 0 to ColCount - 1 do Cells[c, r + Count] := Cells[c, r];
    Dec(r);
  end;

  //  �ӂ₵�������͋�
  for r := Index to Index + Count - 1 do for c := 0 to ColCount - 1 do
  begin
    Cells[c, r] := '';
  end;

  //  �ӂ₵��������I����Ԃɂ���
  LineSelect(0, Index, Count);
end;

procedure TABSmartGrid.InsertCols(Index: Integer; Count: Integer);
var
  r, c: Integer;
begin
  if Index < 0 then Index := Selection.Left;
  if Count < 0 then Count := Selection.Right - Selection.Left + 1;
  if Count < 1 then Exit;

  //  ���₵�āA���炷
  ColCount := ColCount + Count;
  c := ColCount - Count - 1;
  while c >= Index do
  begin
    for r := 0 to RowCount - 1 do Cells[c + Count, r] := Cells[c, r];
    Dec(c);
  end;

  //  �ӂ₵�������͋�
  for c := Index to Index + Count - 1 do for r := 0 to RowCount - 1 do
  begin
    Cells[c, r] := '';
  end;

  //  �ӂ₵��������I����Ԃɂ���
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
  //  Grid�̎d�l��AFixed�łȂ��s��1�ȏ�c���Ȃ���΂����Ȃ��B

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
  //  Grid�̎d�l��AFixed�łȂ����1�ȏ�c���Ȃ���΂����Ȃ��B

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


procedure TABSmartGrid.ClearSelectedCells;
var
  r: Longint;
  c: Longint;
begin
  for r := Selection.Top to Selection.Bottom do
  for c := Selection.Left to Selection.Right do Cells[c, r] := '';
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

function  TABSmartGrid.PasteCells(ACol: Integer; ARow: Integer;
                                   expandRow: Boolean;
                                   expandCol: Boolean): String;
var
  r:  Integer;
  c:  Integer;
  p:  PChar;
  s:  String;
  t:  String;
  h:  THandle;
  nr: Integer;
  nc: Integer;
  sr: TGridRect;
begin

  //  �N���b�v�{�[�h������o��
  Clipboard.Open;
  try
    h := Clipboard.GetAsHandle(CF_TEXT);
    p := GlobalLock(h);
    s := p;
    GlobalUnlock(h);
  finally
    Clipboard.Close;
  end;

  //  Paste����ʒu�Ƒ傫���𒲂ׂ�
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

  //  �\��t����
  r := 0;
  while r < nr do
  begin
    c := 0;
    t := NextToken(s, CRLF);
    while c < nc do
    begin
      Cells[c + ACol, r + ARow] := NextToken(t, TAB);
      Inc(c);
    end;
    Inc(r);
  end;

  //  �\��t����������I����Ԃɂ���
  sr.Top    := ARow;
  sr.Left   := ACol;
  sr.Bottom := ARow + nr - 1;
  sr.Right  := ACol + nc - 1;
  RectSelect(sr);

  //  �c�����̂��Ԃ�l
  Result := s;
end;


procedure TABSmartGrid.SetDropDownCount(Value: Integer);
begin
  FDropDownRows := Value;
  if Assigned(FEditList) then FEditList.DropDownRows := FDropDownRows;
end;

procedure TABSmartGrid.SetOnEditButtonClick(const Value: TNotifyEvent);
begin
  FOnEditButtonClick := Value;
  if Assigned(FEditList)
  then FEditList.OnEditButtonClick := FOnEditButtonClick;
end;

function TABSmartGrid.CreateEditor: TInplaceEdit;
begin
  FEditList := TABSmartGridEdit.Create(Self);
  FEditList.DropDownRows := FDropDownRows;
  FEditList.OnEditButtonClick := FOnEditButtonClick;
  FEditList.OnGetPickListitems := EditListGetItems;
  Result := FEditList;
end;

function  TABSmartGrid.GetEditStyle(ACol, ARow: Longint): TEditStyle;
begin
  Result := esSimple;
  if Assigned(FOnGetEditStyle)
  then FOnGetEditStyle(Self, ACol, ARow, Result);
end;

procedure TABSmartGrid.SetSelectList(List: TStrings);
begin
  if Assigned(Flist)
  then Flist.Assign(List)
  else Flist := List;
end;

procedure TABSmartGrid.EditListGetItems(ACol, ARow: Integer; Items: TStrings);
begin
  Items.Text := Flist.Text
end;


///////////////////////////////////////////////
{ TABSmartGridEdit }

procedure TABSmartGridEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if (Key <> 0) and  (EditStyle = esPickList) then
  begin
    Key := 0;
    DropDown;
  end;
end;

procedure Register;
begin
  RegisterComponents('ABplus', [TABSmartGrid]);
end;

end.
