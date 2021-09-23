unit CatSynEdit;

{
  Catarinka TCatSynEdit - Enhanced SynEdit with popup menu
  Copyright (c) 2013-2021 Felipe Daragon
  Based on uSynEditPopupEdit.pas by Rodrigo Ruz V

  License: MIT (http://opensource.org/licenses/mit-license.php)
  Same license as the original code.

  The LoadFromFile() method is also a better alternative to Lines.LoadFromFile()
}

interface

{$I Catarinka.inc}

uses
{$IFDEF DXE2_OR_UP}
  Winapi.Messages, System.Classes, System.Types, System.SysUtils,
  Winapi.Windows, Vcl.Menus, Vcl.ActnList,
{$ELSE}
  Messages, Classes, Types, SysUtils, Windows, Menus, ActnList,
{$ENDIF}
{$IF DXE3_OR_UP}
  System.Actions,
{$IFEND}
  SynEdit, CatStringList;

{$DEFINE OVMOUSEWHEEL}

type
  TCatSynEdit = class(SynEdit.TSynEdit)
  private
    fActnList: TActionList;
    fFilename: string;
    fPopupMenu: TPopupMenu;
    procedure CreateActns;
    procedure FillPopupMenu(APopupMenu: TPopupMenu);
    procedure CutExecute(Sender: TObject);
    procedure CutUpdate(Sender: TObject);
    procedure CopyExecute(Sender: TObject);
    procedure CopyUpdate(Sender: TObject);
    procedure PasteExecute(Sender: TObject);
    procedure PasteUpdate(Sender: TObject);
    procedure DeleteExecute(Sender: TObject);
    procedure DeleteUpdate(Sender: TObject);
    procedure SelectAllExecute(Sender: TObject);
    procedure SelectAllUpdate(Sender: TObject);
    procedure RedoExecute(Sender: TObject);
    procedure RedoUpdate(Sender: TObject);
    procedure UndoExecute(Sender: TObject);
    procedure UndoUpdate(Sender: TObject);
    procedure SetPopupMenu_(const Value: TPopupMenu);
    function GetPopupMenu_: TPopupMenu;
  protected
{$IFDEF OVMOUSEWHEEL}
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override;
{$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadFromFile(const filename:string);
    procedure SaveToFile;
  published
    property PopupMenu: TPopupMenu read GetPopupMenu_ write SetPopupMenu_;
    property Filename: string read fFilename write fFilename;
  end;

implementation

const
  MenuName = 'uSynEditPopupMenu';

{$IFDEF OVMOUSEWHEEL}

function TCatSynEdit.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
var
  I: Integer;
begin
  Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
  if WheelDelta < 0 then
  begin
    for I := 1 to 10 do
      Perform(WM_VSCROLL, MAKELONG(SB_LINEDOWN, 0), 0);
  end
  else if WheelDelta > 0 then
  begin
    for I := 1 to 10 do
      Perform(WM_VSCROLL, MAKELONG(SB_LINEUP, 0), 0);
  end;
end;
{$ENDIF}

procedure TCatSynEdit.CopyExecute(Sender: TObject);
begin
  Self.CopyToClipboard;
end;

procedure TCatSynEdit.CopyUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Self.SelAvail;
end;

procedure TCatSynEdit.CutExecute(Sender: TObject);
begin
  Self.CutToClipboard;
end;

procedure TCatSynEdit.CutUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Self.SelAvail and not Self.ReadOnly;
end;

procedure TCatSynEdit.DeleteExecute(Sender: TObject);
begin
  Self.SelText := '';
end;

procedure TCatSynEdit.DeleteUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Self.SelAvail and not Self.ReadOnly;
end;

procedure TCatSynEdit.PasteExecute(Sender: TObject);
begin
  Self.PasteFromClipboard;
end;

procedure TCatSynEdit.PasteUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Self.CanPaste;
end;

procedure TCatSynEdit.RedoExecute(Sender: TObject);
begin
  Self.Redo;
end;

procedure TCatSynEdit.RedoUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Self.CanRedo;
end;

procedure TCatSynEdit.SelectAllExecute(Sender: TObject);
begin
  Self.SelectAll;
end;

procedure TCatSynEdit.SelectAllUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Self.Lines.Text <> '';
end;

procedure TCatSynEdit.UndoExecute(Sender: TObject);
begin
  Self.Undo;
end;

procedure TCatSynEdit.UndoUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Self.CanUndo;
end;

constructor TCatSynEdit.Create(AOwner: TComponent);
begin
  inherited;
  FActnList := TActionList.Create(Self);
  FPopupMenu := TPopupMenu.Create(Self);
  FPopupMenu.Name := MenuName;
  CreateActns;
  FillPopupMenu(FPopupMenu);
  PopupMenu := FPopupMenu;
  FFilename := EmptyStr;
end;

procedure TCatSynEdit.CreateActns;

  procedure AddActItem(const AText: string; AShortCut: TShortCut;
    AEnabled: Boolean; OnExecute, OnUpdate: TNotifyEvent);
  Var
    ActionItem: TAction;
  begin
    ActionItem := TAction.Create(FActnList);
    ActionItem.ActionList := FActnList;
    ActionItem.Caption := AText;
    ActionItem.ShortCut := AShortCut;
    ActionItem.Enabled := AEnabled;
    ActionItem.OnExecute := OnExecute;
    ActionItem.OnUpdate := OnUpdate;
  end;
  function ShortCut(key: word; Shift: TShiftState): word;
  begin
{$IFDEF DXE2_OR_UP}
    Result := Vcl.Menus.ShortCut(key, Shift);
{$ELSE}
    Result := Menus.ShortCut(key, Shift);
{$ENDIF}
  end;

begin
  AddActItem('&Undo', ShortCut(word('Z'), [ssCtrl]), False, UndoExecute,
    UndoUpdate);
  AddActItem('&Redo', ShortCut(word('Z'), [ssCtrl, ssShift]), False,
    RedoExecute, RedoUpdate);
  AddActItem('-', 0, False, nil, nil);
  AddActItem('Cu&t', ShortCut(word('X'), [ssCtrl]), False, CutExecute,
    CutUpdate);
  AddActItem('&Copy', ShortCut(word('C'), [ssCtrl]), False, CopyExecute,
    CopyUpdate);
  AddActItem('&Paste', ShortCut(word('V'), [ssCtrl]), False, PasteExecute,
    PasteUpdate);
  AddActItem('De&lete', 0, False, DeleteExecute, DeleteUpdate);
  AddActItem('-', 0, False, nil, nil);
  AddActItem('Select &All', ShortCut(word('A'), [ssCtrl]), False,
    SelectAllExecute, SelectAllUpdate);
end;

procedure TCatSynEdit.SetPopupMenu_(const Value: TPopupMenu);
Var
  MenuItem: TMenuItem;
begin
  SynEdit.TSynEdit(Self).PopupMenu := Value;
  if CompareText(MenuName, Value.Name) <> 0 then
  begin
    MenuItem := TMenuItem.Create(Value);
    MenuItem.Caption := '-';
    Value.Items.Add(MenuItem);
    FillPopupMenu(Value);
  end;
end;

function TCatSynEdit.GetPopupMenu_: TPopupMenu;
begin
  Result := SynEdit.TSynEdit(Self).PopupMenu;
end;

destructor TCatSynEdit.Destroy;
begin
  FPopupMenu.Free;
  FActnList.Free;
  inherited;
end;

procedure TCatSynEdit.FillPopupMenu(APopupMenu: TPopupMenu);
var
  I: Integer;
  MenuItem: TMenuItem;
begin
  if Assigned(FActnList) then
    for I := 0 to FActnList.ActionCount - 1 do
    begin
      MenuItem := TMenuItem.Create(APopupMenu);
      MenuItem.Action := FActnList.Actions[I];
      APopupMenu.Items.Add(MenuItem);
    end;
end;

// Alternative to lines.loadfromfile() which uses improved file loading from
// TCatStringList that deals with rare UTF8 encoding problems when loading a file
procedure TCatSynEdit.LoadFromFile(const filename:string);
var sl:TCatStringList;
begin
  fFilename := filename;
  sl := TCatStringList.Create;
  sl.LoadFromFile(filename);
  Lines.Text := sl.Text;
  sl.Free;
end;

procedure TCatSynEdit.SaveToFile;
begin
  if fFilename <> emptystr then
   Lines.SaveToFile(fFilename);
end;

end.
