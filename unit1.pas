unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  Menus, StdCtrls, ComCtrls, ExtCtrls, Buttons, Unit2, _Bnote,
  XMLConf, types, LCLType;

type

  { TFormMain }

  TFormMain = class(TForm)
    ButtonRename: TButton;
    ButtonHelp: TButton;
    ButtonSetname: TButton;
    ButtonDone: TButton;
    ButtonDelete: TButton;
    ButtonNew: TButton;
    ButtonSettings: TButton;
    ButtonCreate: TButton;
    EditName: TEdit;
    EditRename: TEdit;
    LabelName: TLabel;
    ListViewNotes: TListBox;
    MemoNote: TMemo;
    PanelEdit: TPanel;
    PanelList: TPanel;
    PanelMemo: TPanel;
    PanelRename: TPanel;
    PanelMain: TPanel;
    PanelCreate: TPanel;
    XMLConfig: TXMLConfig;
    procedure ButtonDeleteClick(Sender: TObject);
    procedure ButtonDoneClick(Sender: TObject);
    procedure ButtonHelpClick(Sender: TObject);
    procedure ButtonNewClick(Sender: TObject);
    procedure ButtonRenameClick(Sender: TObject);
    procedure ButtonSetnameClick(Sender: TObject);
    procedure ButtonSettingsClick(Sender: TObject);
    procedure ButtonExitClick(Sender: TObject);
    procedure ButtonCreateClick(Sender: TObject);
    procedure EditNameKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditRenameKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);
    procedure ListViewNotesDblClick(Sender: TObject);
    procedure ListViewNotesDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure ListViewNotesKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ListViewNotesMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure MemoNoteChange(Sender: TObject);
    procedure MemoNoteClick(Sender: TObject);
    procedure MemoNoteEditingDone(Sender: TObject);
    procedure MemoNoteKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure MemoNoteMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  FormMain: TFormMain;
  ListViewNote: Integer;
  MemoNoteText: String;
  FormMainAlphaRestore: Integer;

const clHighlight = clRed;
const clActiveBackground = clGreen;

implementation

{ TFormMain }

procedure PanelShow (Name: String);
begin
  if (Name=FormMain.PanelCreate.Name) then FormMain.PanelCreate.Show else FormMain.PanelCreate.Hide;
  if (Name=FormMain.PanelEdit.Name) then FormMain.PanelEdit.Show else FormMain.PanelEdit.Hide;
  if (Name=FormMain.PanelMain.Name) then FormMain.PanelMain.Show else FormMain.PanelMain.Hide;
  if (Name=FormMain.PanelRename.Name) then FormMain.PanelRename.Show else FormMain.PanelRename.Hide;
end;

procedure MemoEnable (var Memo: TMemo);
begin
  FormMain.PanelMemo.Show;
  Memo.Enabled := True;
  Memo.Visible := True;
  Memo.SetFocus;
end;

procedure MemoDisable (var Memo: TMemo);
begin
  Memo.Visible := False;
  Memo.Enabled := False;
  FormMain.PanelMemo.Hide
end;

procedure TFormMain.ButtonCreateClick(Sender: TObject);
begin
  MemoNoteText := 'new';
  BnoteFilesRefresh (ListViewNotes);
  if EditName.Text<>'' then
  begin
    BnoteFileCreate(EditName.Text, ListViewNotes);
    ListViewNotes.SetFocus;
    LabelName.Caption := EditName.Text;
    EditName.Text := '';
    PanelShow ('PanelEdit');
    MemoEnable (MemoNote);
  end
  else
  begin
    PanelShow ('PanelMain');
    ListViewNotes.SetFocus;
  end;
end;

procedure TFormMain.EditNameKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
  );
begin
  if Key=13 then ButtonCreate.Click;
end;

procedure TFormMain.EditRenameKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key=13) then ButtonSetname.Click;
end;

procedure TFormMain.FormActivate(Sender: TObject);
begin
  FormMain.AlphaBlendValue := FormMain.AlphaBlendValue + FormMainAlphaRestore;
  FormMainAlphaRestore := 0;
end;

procedure TFormMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  XMLConfig.SetValue('/Form/Width', FormMain.Width);
  XMLConfig.SetValue('/Form/Height', FormMain.Height);
  XMLConfig.SetValue('/Form/Top', FormMain.Top);
  XMLConfig.SetValue('/Form/Left', FormMain.Left);
  XMLConfig.SetValue('/Form/Alpha', FormMain.AlphaBlendValue+FormMainAlphaRestore);
  XMLConfig.Flush;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  {$IFDEF UNIX}
  FormMain.Font.Name := 'Monospace';
  FormMain.Font.Size := 7;
  {$ENDIF}
  FormMainAlphaRestore := 0;
  BnoteFilesRefresh (ListViewNotes);
  XMLConfig.Filename := BnoteConfigPath;
  FormMain.Top := XMLConfig.GetValue('/Form/Top', FormMain.Top);
  FormMain.Left := XMLConfig.GetValue('/Form/Left', FormMain.Left);
  FormMain.Width := XMLConfig.GetValue('/Form/Width', FormMain.Width);
  FormMain.Height := XMLConfig.GetValue('/Form/Height', FormMain.Height);
  FormMain.AlphaBlendValue := XMLConfig.GetValue('/Form/Alpha', FormMain.AlphaBlendValue);
  FormMain.Caption:=BnoteDirectory;
end;

procedure TFormMain.FormDeactivate(Sender: TObject);
begin
  if (FormMain.AlphaBlendValue>100) then
  begin
    FormMainAlphaRestore := FormMain.AlphaBlendValue - 50;
    FormMain.AlphaBlendValue := 50;
  end;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  MemoNote.EditingDone;
end;

procedure TFormMain.FormShow(Sender: TObject);
begin
  ListViewNotes.SetFocus;
end;

procedure TFormMain.FormWindowStateChange(Sender: TObject);
begin

end;

procedure TFormMain.ListViewNotesDblClick(Sender: TObject);
begin
  if ListViewNotes.ItemIndex<>-1 then
  begin
    LabelName.Caption := ListViewNotes.Items[ListViewNotes.ItemIndex];
    BnoteFileLoad (LabelName.Caption, MemoNote);
    MemoNoteText := MemoNote.Lines.Text;
    PanelShow ('PanelEdit');
    ListViewNote := ListViewNotes.ItemIndex;
    MemoEnable (MemoNote);
    MemoNote.SelStart := 0;
    MemoNote.SetFocus;
  end;
end;

procedure TFormMain.ListViewNotesDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
begin
  with (Control as TListBox).Canvas do
  begin
    if odSelected in State then
    begin
      Brush.Color := $000F559D;
      Font.Color := clWhite;
    end else
    begin
      Brush.Color := $00222827;
      Font.Color := clWhite;
    end;
    FillRect(ARect);
    TextOut(ARect.Left + 2, ARect.Top+2, ListViewNotes.Items[index]);
    if (odFocused in State) then
    begin
       DrawFocusRect(ARect);
    end;
  end;
end;

procedure TFormMain.ListViewNotesKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
     if (Shift=[ssCtrl]) and ((chr(Key)='n') or (chr(Key)='N')) then ButtonNew.Click;
     if (Shift=[ssCtrl]) and ((chr(Key)='d') or (chr(Key)='D')) then ButtonDelete.Click;
     if Key=13 then
     begin
          FormMain.ListViewNotesDblClick(nil);
     end;
end;

procedure TFormMain.ListViewNotesMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
begin
     if ((WheelDelta<0) and (FormMain.AlphaBlendValue-5>30)) or ((WheelDelta>0) and (FormMain.AlphaBlendValue+5<255)) then
     begin
       if (WheelDelta<0) then FormMain.AlphaBlendValue := FormMain.AlphaBlendValue - 5
       else FormMain.AlphaBlendValue := FormMain.AlphaBlendValue + 5
     end;
end;

procedure TFormMain.MemoNoteChange(Sender: TObject);
var Previous_, Current, Replace: String;
begin
  if MemoNote.SelStart>1 then
  begin
  MemoNote.SelStart:=MemoNote.SelStart-2;
  MemoNote.SelLength:=1;
  Previous_ := MemoNote.SelText;
  MemoNote.SelStart := MemoNote.SelStart + 1;
  MemoNote.SelLength:=1;
  Current := MemoNote.SelText;
  MemoNote.SelStart := MemoNote.SelStart + 1;
  end
  else if MemoNote.SelStart=1 then
  begin
    MemoNote.SelStart := MemoNote.SelStart - 1;
    MemoNote.SelLength:=1;
    Current := MemoNote.SelText;
    MemoNote.SelStart := MemoNote.SelStart + 1;
  end;
  //FormMain.Caption:= IntToStr(Ord(Previous_[2]));
  if (Previous_=#10) or (MemoNote.SelStart=1) then
  begin
    Replace := '';
    if Current='.' then Replace := '□';
    if Current='+' then Replace := '■';
    if Current='-' then Replace := '▬';
    if (Replace<>'') then
    begin
      MemoNote.SelStart:=MemoNote.SelStart-1;
      MemoNote.SelLength:=1;
      MemoNote.SelText:=Replace;
    end;
  end;
end;

procedure TFormMain.MemoNoteClick(Sender: TObject);
begin

end;

procedure TFormMain.MemoNoteEditingDone(Sender: TObject);
begin
     if (PanelMemo.Visible) and (Trim(MemoNoteText)<>Trim(MemoNote.Lines.Text)) then
     begin
       BnoteFileSave (LabelName.Caption, MemoNote);
       BnoteFilesRefresh (ListViewNotes);
       MemoNoteText := '';
       ListViewNotes.ItemIndex := 0;
     end;
end;

procedure TFormMain.MemoNoteKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
     if (Shift=[ssCtrl]) and ((chr(Key)='s') or (chr(Key)='S')) then ButtonDone.Click;
     //if Key=13 then FormMain.Caption := FormMain.Caption + ' E';
end;

procedure TFormMain.MemoNoteMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
     if ((WheelDelta<0) and (FormMain.AlphaBlendValue-5>30)) or ((WheelDelta>0) and (FormMain.AlphaBlendValue+5<255)) then
     begin
       if (WheelDelta<0) then FormMain.AlphaBlendValue := FormMain.AlphaBlendValue - 5
       else FormMain.AlphaBlendValue := FormMain.AlphaBlendValue + 5
     end;
end;

procedure TFormMain.ButtonNewClick(Sender: TObject);
begin
  PanelShow('PanelCreate');
  EditName.SetFocus;
end;

procedure TFormMain.ButtonRenameClick(Sender: TObject);
begin
  if ListViewNotes.ItemIndex<>-1 then
  begin
    PanelShow ('PanelRename');
    EditRename.Caption := ListViewNotes.Items[ListViewNotes.ItemIndex];
    EditRename.SetFocus;
  end;
end;

procedure TFormMain.ButtonSetnameClick(Sender: TObject);
begin
  if (EditRename.Text<>'') and (not Notes.Exists(BnoteFileName(EditRename.Text))) then
  begin
    BnoteFileRename (ListViewNotes.Items[ListViewNotes.ItemIndex], EditRename.Text);
    BnoteFilesRefresh (ListViewNotes);
    ListViewNotes.ItemIndex := 0;
  end;
  PanelShow ('PanelMain');
  ListViewNotes.SetFocus;
end;

procedure TFormMain.ButtonDeleteClick(Sender: TObject);
begin
  BnoteFileDelete (ListViewNotes);
end;

procedure TFormMain.ButtonDoneClick(Sender: TObject);
begin
  PanelShow ('PanelMain');
  MemoDisable (MemoNote);
  if Trim(MemoNoteText)<>Trim(MemoNote.Lines.Text) then
  begin
    BnoteFileSave (LabelName.Caption, MemoNote);
    BnoteFilesRefresh(ListViewNotes);
    ListViewNotes.ItemIndex := 0;
  end
  else
  begin
    ListViewNotes.ItemIndex := ListViewNote;
  end;
  MemoNote.Lines.Text := '';
  LabelName.Caption := '';
  ListViewNotes.SetFocus;
end;

procedure TFormMain.ButtonHelpClick(Sender: TObject);
begin
    PanelShow('PanelCreate');
    EditName.Text := 'Bnote Help';
    ButtonCreate.Click;
    MemoNote.Text:= 'Press Ctrl+N to create note'+#13#10+
    'Press Ctrl+S to complete editing'+#13#10+
    'Press Ctrl+R to rename note'+#13#10+
    'Press Ctrl+D to delete note'+#13#10+
    'Press Enter to enter note or complete dialog'+#13#10+#13#10+
    '□ While editing type . on new line for uncomplete task sign'+#13#10+
    '■ While editing type + on new line for complete task sign'+#13#10+
    '▬ While editing type - on new line for current task sign'+#13#10+#13#10+
    'Scroll mouse up and down on listbox or on text for window to fade in fade out'+#13#10+#13#10+
    'Works on windows linux and mac';
end;

procedure TFormMain.ButtonSettingsClick(Sender: TObject);
begin
  FormSettings.Show;
end;

procedure TFormMain.ButtonExitClick(Sender: TObject);
begin
  Application.Terminate;
end;

initialization
  {$I unit1.lrs}

end.

