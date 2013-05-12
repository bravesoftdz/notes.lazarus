unit Unit2; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, Buttons, _Bnote;

type

  { TFormSettings }

  TFormSettings = class(TForm)
    Button1: TButton;
    ButtonCancel: TButton;
    ButtonSave: TButton;
    EditHost: TEdit;
    EditPassword: TEdit;
    EditPassword1: TEdit;
    EditPath: TEdit;
    EditUsername: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    PageControl1: TPageControl;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    TabSheetRemote: TTabSheet;
    TabSheetSecurity: TTabSheet;
    TabSheetDisplay: TTabSheet;
    TabSheetStorage: TTabSheet;
    procedure Button1Click(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
    procedure ButtonSaveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TabControl1Change(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  FormSettings: TFormSettings;

implementation

{ TFormSettings }

procedure TFormSettings.Button1Click(Sender: TObject);
begin
     if SelectDirectoryDialog1.Execute then
     begin
          EditPath.Text := SelectDirectoryDialog1.FileName;
     end;
end;

procedure TFormSettings.ButtonCancelClick(Sender: TObject);
begin
  FormSettings.Close;
end;

procedure TFormSettings.ButtonSaveClick(Sender: TObject);
begin
  FormSettings.Close;
end;

procedure TFormSettings.FormCreate(Sender: TObject);
begin
  if (EditPath.Text='') then
  begin
    EditPath.Text := BnoteDirectory();
  end;
end;

procedure TFormSettings.TabControl1Change(Sender: TObject);
begin

end;

initialization
  {$I unit2.lrs}

end.

