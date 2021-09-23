unit CatConsole;

{
  Catarinka Console Component
  Copyright (c) 2012-2014 Syhunt Informatica
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details
}

interface

{$I Catarinka.inc}

uses
{$IFDEF DXE2_OR_UP}
  Vcl.Forms, Vcl.Controls, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Menus, Vcl.Clipbrd, Vcl.Dialogs,
{$ELSE}
  Forms, Controls, SysUtils, Classes, Graphics, Menus, Clipbrd, Dialogs,
{$ENDIF}
  CatConsoleCore, CatStringLoop, CatPrefs;

type
  TCatConsoleOnScriptCommand = procedure(const Code: string) of object;

type
  TCatConsole = class(TCustomControl)
  private
    fConsole: TConsole;
    fCustomCommand: boolean;
    fCustomCommandState: Integer;
    fCustomHandler: string;
    fHelpParser: TStringLoop;
    fOnScriptCommand: TCatConsoleOnScriptCommand;
    fPopupMenu: TPopupMenu;
    fProgDir: string;
    fPromptText: string;
    function GetLastCommand: string;
    procedure PopupMenuitemClick(Sender: TObject);
    procedure SetCustomHandler(s: string);
    procedure ConsoleBoot(Sender: TCustomConsole; var ABootFinished: boolean);
    procedure ConsoleCommandExecute(Sender: TCustomConsole; ACommand: String;
      var ACommandFinished: boolean);
    procedure ConsoleCommandKeyPress(Sender: TCustomConsole; var AKey: Char;
      var ATerminateCommand: boolean);
    procedure ConsoleGetPrompt(Sender: TCustomConsole;
      var APrompt, ADefaultText: string; var ADefaultCaretPos: Integer);
  protected
  public
    function PrintAvailableCommands(Sender: TCustomConsole): boolean;
    procedure Boot;
    procedure Clear;
    procedure ConsoleOutput(Enabled: boolean);
    procedure LoadSettings(prefs: TCatPreferences);
    procedure WriteLn(ALine: string = '');
    procedure Write(ALine: string = '');
    procedure WriteVersion;
    procedure ResetPrompt;
    procedure ResetFull;
    procedure SetCurrentLine(s: string);
    procedure SetPrompt(s: string);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // properties
    property Console: TConsole read fConsole;
    property CustomHandler: string read fCustomHandler write SetCustomHandler;
    property LastCommand: string read GetLastCommand;
    property PromptText: string read fPromptText write fPromptText;
    property OnScriptCommand: TCatConsoleOnScriptCommand read fOnScriptCommand
      write fOnScriptCommand;
  end;

const
  cCommandsSubDir = 'Scripts\Commands\';

var
  TabCommands, TabCommandsDesc: TStringList;

procedure AddConsoleCommand(cmd, luacode, description: string);
procedure ReadDiskCommands;

implementation

uses CatStrings, CatHTTP, CatFiles, CatJSON;

procedure AddConsoleCommand(cmd, luacode, description: string);
var
  cmdname, cmdparams: string;
begin
  cmdname := cmd;
  if pos(' ', cmd) <> 0 then
  begin
    cmdname := before(cmd, ' ');
    cmdparams := after(cmd, ' ');
    if cmdparams <> emptystr then
      cmdparams := ' ' + cmdparams;
  end;
  if TabCommands.Values[cmdname] = emptystr then
  begin
    TabCommandsDesc.Add('<tr><td><b>' + cmdname + '</b>' + cmdparams +
      '</td><td>&nbsp;&nbsp;' + description + '</td><tr>');
    TabCommandsDesc.Sorted := true;
  end;
  TabCommands.Values[cmdname] := luacode;
end;

procedure ReadDiskCommands;
var
  slp: TStringLoop;
  script: TStringList;
  progdir, cmd, desc: string;
const
  cLuaCommentPrefix = '--';
begin
  progdir := extractfilepath(paramstr(0));
  script := TStringList.Create;
  slp := TStringLoop.Create;
  GetFiles(progdir + cCommandsSubDir + '\*.lua', slp.List);
  while slp.Found do
  begin
    script.Clear;
    script.LoadFromFile(progdir + cCommandsSubDir + '\' + slp.Current);
    if script.Count >= 2 then
    begin
      cmd := trim(script.Strings[0]);
      desc := trim(script.Strings[1]);
      if beginswith(cmd, cLuaCommentPrefix) and
        beginswith(desc, cLuaCommentPrefix) then
      begin
        cmd := trim(after(cmd, cLuaCommentPrefix));
        desc := trim(after(desc, cLuaCommentPrefix));
        AddConsoleCommand(cmd, script.Text, desc);
      end;
    end;
  end;
  slp.free;
  script.free;
end;

procedure TCatConsole.LoadSettings(prefs: TCatPreferences);
const
  SCO_CONSOLE_FONT_COLOR = 'sandcat.console.font.color';
  SCO_CONSOLE_BGCOLOR = 'sandcat.console.bgcolor';
begin
  fConsole.Font.Color := HtmlColorToColor
    (prefs.getvalue(SCO_CONSOLE_FONT_COLOR));
  fConsole.Color := HtmlColorToColor(prefs.getvalue(SCO_CONSOLE_BGCOLOR));
end;

procedure TCatConsole.SetCustomHandler(s: string);
begin
  if s = emptystr then
    fCustomHandler := emptystr
  else
    fCustomHandler := s + ' ';
end;

procedure TCatConsole.Boot;
begin
  fConsole.Boot;
end;

procedure TCatConsole.ConsoleCommandExecute(Sender: TCustomConsole;
  ACommand: String; var ACommandFinished: boolean);
var
  p: TCommandParser;
  cmdscriptfile, params: string;
  sl: TStringList;
  procedure List;
  begin
    ReadDiskCommands;
    fHelpParser.Load(TabCommandsDesc);
    ACommandFinished := False;
    fCustomCommand := true;
    fCustomCommandState := 0;
    Sender.Writeln('Available commands:');
    PrintAvailableCommands(Sender);
  end;

begin
  p := TCommandParser.Create(fCustomHandler + ACommand);
  params := TabCommands.Values[p.Command];
  if ACommand <> emptystr then
  begin
    if p.Command = 'list' then
      List
    else
    begin
      cmdscriptfile := fProgDir + '\' + cCommandsSubDir + p.Command + '.lua';
      if params <> emptystr then
      begin
        if assigned(OnScriptCommand) then
          OnScriptCommand(params);
        ACommandFinished := False;
      end
      else if fileexists(cmdscriptfile) then
      begin
        sl := TStringList.Create;
        sl.LoadFromFile(cmdscriptfile);
        if assigned(OnScriptCommand) then
          OnScriptCommand(sl.Text);
        ACommandFinished := False;
        sl.free;
      end
      else
        Sender.Writeln('"' + p.Command + '" command not recognized.');
    end;
  end;
  p.free;
end;

procedure TCatConsole.ConsoleCommandKeyPress(Sender: TCustomConsole;
  var AKey: Char; var ATerminateCommand: boolean);
begin
  if (fCustomCommand) then
  Begin
    if fCustomCommandState = 0 then
    begin // help command
      AKey := #0;
      if PrintAvailableCommands(Sender) = False then
      begin
        fCustomCommand := False;
        ATerminateCommand := true;
      end;
    end;
  end;
end;

function TCatConsole.PrintAvailableCommands(Sender: TCustomConsole): boolean;
var
  c: Integer;
const
  max = 20;
  function RemoveHTML(s: string): string;
  begin
    s := replacestr(s, '&nbsp;&nbsp;', ' - ');
    s := striphtml(s);
    result := s;
  end;

begin
  result := False;
  c := 0;
  while (fHelpParser.Found) do
  begin
    Inc(c);
    Sender.Writeln('   ' + RemoveHTML(fHelpParser.Current));
    result := true;
    if c = max then
      Sender.Writeln('Press any key to continue. . .');
    if c = max then
      exit;
  end;
  if c < max then
    result := False;
end;

procedure TCatConsole.ConsoleGetPrompt(Sender: TCustomConsole;
  var APrompt, ADefaultText: string; var ADefaultCaretPos: Integer);
begin
  APrompt := fPromptText + '>';
end;

procedure TCatConsole.SetCurrentLine(s: string);
begin
  fConsole.CurrLine.Text := s;
  fConsole.CaretX := Length(s) + 1;
  fConsole.Invalidate;
end;

procedure TCatConsole.SetPrompt(s: string);
begin
  fPromptText := s;
  if s + '>' <> fConsole.LastPrompt then
    ResetPrompt;
end;

procedure TCatConsole.ResetPrompt;
begin
  fConsole.BeginExternalOutput;
  fConsole.EndExternalOutput;
end;

procedure TCatConsole.ResetFull;
begin
  fCustomHandler := emptystr;
  SetPrompt(emptystr);
  Clear;
  WriteVersion;
end;

procedure TCatConsole.ConsoleOutput(Enabled: boolean);
begin
  if Enabled = False then
  begin
    if fConsole.prompt = False then
      fConsole.EndExternalOutput;
  end;
end;

function TCatConsole.GetLastCommand: string;
begin
  result := fCustomHandler + fConsole.LastCommand;
end;

procedure TCatConsole.Clear;
begin
  fConsole.BeginExternalOutput;
  fConsole.Clear;
  fConsole.EndExternalOutput;
end;

procedure TCatConsole.PopupMenuitemClick(Sender: TObject);
begin
  case tmenuitem(Sender).Tag of
    1:
      Clear;
    2:
      fConsole.PasteFromClipboard;
  end;
end;

procedure TCatConsole.Write(ALine: string = '');
begin
  ALine := replacestr(ALine, #10, emptystr);
  if fConsole.prompt = true then
    fConsole.BeginExternalOutput;
  fConsole.Write(ALine);
  fConsole.Repaint;
  application.ProcessMessages;
end;

procedure TCatConsole.Writeln(ALine: string = '');
var
  slp: TStringLoop;
begin
  if fConsole.Lines.Count >= 1000 then
    fConsole.Clear;
  if fConsole.prompt = true then
    fConsole.BeginExternalOutput;
  if pos(crlf, ALine) <> 0 then
  begin
    slp := TStringLoop.Create(ALine);
    while slp.Found do
      fConsole.Writeln(slp.Current);
    slp.free;
  end
  else
    fConsole.Writeln(ALine);
  application.ProcessMessages;
end;

procedure TCatConsole.WriteVersion;
begin
  if fConsole.prompt = true then
    fConsole.BeginExternalOutput;
  fConsole.Writeln('Sandcat' + '/' + GetFileVersion(paramstr(0)));
  fConsole.Writeln('Type help for a list of commands.');
  fConsole.Writeln;
end;

procedure TCatConsole.ConsoleBoot(Sender: TCustomConsole;
  var ABootFinished: boolean);
begin
  WriteVersion;
end;

constructor TCatConsole.Create(AOwner: TComponent);
var
  mi: tmenuitem;
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csAcceptsControls];
  fProgDir := extractfilepath(paramstr(0));
  fHelpParser := TStringLoop.Create;
  fConsole := TConsole.Create(self);
  fConsole.Parent := self;
  fConsole.Align := alClient;
  fConsole.Font.Name := 'Fixedsys';
  fConsole.Color := $002D2D2D;
  fConsole.Font.Color := clWhite;
  fConsole.Font.Style := [];
  fConsole.OnBoot := ConsoleBoot;
  fConsole.OnCommandExecute := ConsoleCommandExecute;
  fConsole.OnCommandKeyPress := ConsoleCommandKeyPress;
  fConsole.OnGetPrompt := ConsoleGetPrompt;
  fPopupMenu := TPopupMenu.Create(self);
  fConsole.PopupMenu := fPopupMenu;
  mi := tmenuitem.Create(self);
  fPopupMenu.Items.Add(mi);
  mi.Caption := '&Clear';
  mi.Tag := 1;
  mi.OnClick := PopupMenuitemClick;
  mi := tmenuitem.Create(self);
  fPopupMenu.Items.Add(mi);
  mi.Caption := '-';
  mi.OnClick := PopupMenuitemClick;
  mi := tmenuitem.Create(self);
  fPopupMenu.Items.Add(mi);
  mi.Caption := '&Paste';
  mi.Tag := 2;
  mi.OnClick := PopupMenuitemClick;
end;

destructor TCatConsole.Destroy;
begin
  fPopupMenu.free;
  fHelpParser.free;
  fConsole.free;
  inherited Destroy;
end;

initialization

TabCommands := TStringList.Create;
TabCommandsDesc := TStringList.Create;
AddConsoleCommand('list', emptystr, 'Displays this list');

finalization

TabCommands.free;
TabCommandsDesc.free;

end.
