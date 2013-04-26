unit ABLabeledEdit;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, StdCtrls, ExtCtrls;

type
  TABLabeledEdit = class(TCustomLabeledEdit)
  private
    { Private éŒ¾ }
    FAlignment: TAlignment;
    procedure SetAlignment(a: TAlignment);
  protected
    { Protected éŒ¾ }
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(AOwner: TComponent); override;
    { Public éŒ¾ }
  published
    { Published éŒ¾ }
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
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
  end;

procedure Register;

implementation

constructor TABLabeledEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  LabelPosition := lpLeft;
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

procedure Register;
begin
  RegisterComponents('ABplus', [TABLabeledEdit]);
end;

end.
