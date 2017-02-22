unit LibBnote;

{$mode objfpc}

interface

uses
  Classes, SysUtils, ComCtrls, StdCtrls, FileUtil, LibString, LibArray, LibFile;

function BnoteDirectory: String;
function BnoteFilesLoad: Boolean;
procedure BnoteFilesRefresh (var ListView: TListBox);
procedure BnoteFileCreate (Caption: String; var ListView: TListBox);
function BnoteFilePath (Name: String): String;
procedure BnoteFileDelete (var ListView: TListBox);
function BnoteFileCaption (Caption: String): String;
function BnoteFileName (Caption: String): String;
procedure BnoteViewRefresh (var ListView: TListBox);
function BnoteFileLoad (Caption:String; var Memo: TMemo): Boolean;
function BnoteFileSave (Caption:String; var Memo: TMemo): Boolean;
procedure BnoteFileRename (CaptionFrom, CaptionTo: String);
function BnoteConfigPath: String;

var
   //Notes: TArrayStringInteger;
   Notes: TArrayIntegerString;
   const BnoteExtension = ExtensionSeparator + 'note';

implementation


function BnoteDirectory: String;
begin
  //Result := GetUserDir() + '.notes' + DirectorySeparator;
  Result := GetCurrentDirUTF8 + DirectorySeparator + 'notes' + DirectorySeparator;
  if not DirectoryExistsUTF8(Result) then CreateDirUTF8(Result);
end;

function BnoteFilesLoad: Boolean;
var Files: TStringList;
   Position: Integer;
   Name: String;
   Count: Integer;
begin
     Result := False;
     //Notes := TArrayStringInteger.Create;
     Notes := TArrayIntegerString.Create;
     Files := FindAllFiles (BnoteDirectory, '*'+BnoteExtension, False);
     if Files.Count>0 then
     begin
       for Position:=0 to Files.Count-1 do
       begin
         Name := ExtractFileNameOnly(Files.Strings[Position]);
         //Notes[ExtractFileNameOnly(Files.Strings[Position])] := FileAgeUTF8(Files.Strings[Position]);
         Notes[FileAgeUTF8(Files.Strings[Position])] := ExtractFileNameOnly(Files.Strings[Position]);
         Count := Notes.Count;
       end;
       Notes.Rsort;
       Result := True;
     end;
end;

procedure BnoteFilesRefresh (var ListView: TListBox);
begin
  BnoteFilesLoad;
  BnoteViewRefresh (ListView);
end;

procedure BnoteViewRefresh (var ListView: TListBox);
var Current,Count,Position: Integer;
begin
  Position:= 0;
  Current:=-1;
  if ListView.ItemIndex<>-1 then
  begin
    Current := ListView.ItemIndex;
  end;
  ListView.Clear;
  Notes.Reset;
  Count := Notes.Count;
  while Notes.Foreach do
  begin
    //ListView.Items.Insert(0, BnoteFileCaption(Notes.Index));
    //ListView.Items.Insert(Position, '['+IntToStr(Notes.Index)+']'+BnoteFileCaption(Notes.Value));
    ListView.Items.Insert(Position, BnoteFileCaption(Notes.Value));
    Position := Position + 1;
  end;

  if Current<>-1 then
  begin
      if ListView.Items.Count>0 then
      begin
        if ListView.Items.Count=1 then
        begin
          ListView.ItemIndex := 0;
        end
        else
        begin
          if ListView.Items.Count=Current
          then
          begin
            ListView.ItemIndex:=Current-1;
          end
          else
          begin
             ListView.ItemIndex:=Current;
          end;
        end;
      end;
  end;

end;

function BnoteFilePath (Name: String): String;
begin
     Result := BnoteDirectory+Name+BnoteExtension;
end;

procedure BnoteFileCreate (Caption: String; var ListView: TListBox);
var
   Note: TFile;
   Name: String;
begin
  Name := BnoteFileName(Caption);
  if not FileExistsUTF8(BnoteFilePath(Name)) then
  begin
     Caption := BnoteFileCaption (Name);
     Note := TFile.Assign(BnoteFilePath(Name));
     Note.Create;
     Note.Close;
     BnoteFilesRefresh(ListView);
  end;
end;

procedure BnoteFileDelete (var ListView: TListBox);
var
   Current: Integer;
   Name: String;
begin
  if ListView.ItemIndex<>-1 then
  begin
    Name := BnoteFileName (ListView.Items[ListView.ItemIndex]);
    if DeleteFileUTF8 (BnoteFilePath(Name)) then
    begin
      BnoteFilesRefresh (ListView);
    end;
  end;
end;

function BnoteFileName (Caption: String): String;
begin
  Result := StrReplace ([' ','/','\'], '_', Caption);
end;

function BnoteFileCaption (Caption: String): String;
begin
  Result := StrReplace ('_', ' ', Caption);
end;

function BnoteFileSave (Caption:String; var Memo: TMemo): Boolean;
var Note: TFile;
   Name: String;
begin
  Name := BnoteFileName (Caption);
  Note := TFile.Assign (BnoteFilePath(Name));
  Note.Overwrite;
  //Note.Edit;
  Note.Write (Trim(Memo.Lines.Text));
  Note.Close;
end;

function BnoteFileLoad (Caption:String; var Memo: TMemo): Boolean;
var Note: TFile;
   Name: String;
begin
  Name := BnoteFileName (Caption);
  Note := TFile.Assign (BnoteFilePath(Name));
  Note.Open;
  Memo.Clear;
  Memo.Lines.Text := Note.Read;
  Note.Close;
  //FileSetDateUTF8(BnoteFilePath(Name), DateTimeToFileDate(Time));
end;

procedure BnoteFileRename (CaptionFrom, CaptionTo: String);
begin
     if RenameFileUTF8(BnoteFilePath(BnoteFileName(CaptionFrom)), BnoteFilePath(BnoteFileName(CaptionTo))) then
     begin
       FileSetDateUTF8(BnoteFilePath(BnoteFileName(CaptionTo)), DateTimeToFileDate(Time));
     end;
end;

function BnoteConfigPath: String;
begin
  if not DirectoryExistsUTF8(GetAppConfigDirUTF8(False)) then CreateDirUTF8(GetAppConfigDirUTF8(False));
  Result := GetAppConfigFileUTF8(False);
end;

end.

