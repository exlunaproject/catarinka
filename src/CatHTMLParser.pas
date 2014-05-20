unit CatHTMLParser;
{$B-}

{
 Catarinka HTML Parser
 Copyright (c) 2003-2014 Felipe Daragon
 
 Based on HTMLParser.pas (THtmlParser),
 Copyright (c) 1999-2000 Przemyslaw Jankowski (pjank@home.pl)
 
 License: MIT (http://opensource.org/licenses/mit-license.php)
 
 You can do whatever you want with this code as long as you include the
 original copyright and license.
 
 Changes:
 - Added Pos, TagPos, TagLine and TextBetweenPos properties
}

(***********************************************************************************)
(*                                                                                 *)
(*   Classes defined in this unit allow you to parse (and update!) any HTML data   *)
(*                                                                                 *)
(* To use this unit you must first:                                                *)
(*  - create a THtmlParser object                                                  *)
(*  - set its >Text< property to the HTML text you want to parse                   *)
(* Then you can "move around" this text with two methods:                          *)
(*  - NextTag - moves you to the next tag from current position                    *)
(*              (after setting Text current position is the beginning of the text) *)
(*  - PrevTag - moves to the previous tag ("goes back")                            *)
(* The current tag (the tag at current position) is returned by Tag property       *)
(* You have also access to the text between two tags - it's in TextBetween prop.   *)
(* There are also some useful methods:                                             *)
(*  - LoadFromFile  - loads Text from the specified file from disk                 *)
(*  - SaveToFile    - saves the Text to disk                                       *)
(*  - GotoBeginning - sets current position at the beginning of the text           *)
(*                    (note: Tag and TextBetween are set to nothing)               *)
(*  - GotoEnd       - sets current position at the end of the text                 *)
(*                    (same note as above)                                         *)
(*  - RemoveTag     - deletes the current tag                                      *)
(*  - InsertTag     - inserts a new tag before the current one                     *)
(*                    (the current position "moves" behind the new tag)            *)
(*  - InsertText    - inserts some text in the current position                    *)
(*                                                                                 *)
(*                                                                                 *)
(* The TTag class provides you access to everything between two brackets: < and >  *)
(*  - Name - this is the tag's name (e.g. 'TABLE', 'IMG' or '/BODY')               *)
(*           (when you read it, it always returns uppercase)                       *)
(*  - Params - this is a TStringList with all parameters                           *)
(*             (each line is something like: 'width=100' or 'ALT="my image"')      *)
(*             hint: you may use the TStringList's Names, Values properties        *)
(*                                                                                 *)
(*                                                                                 *)
(* Take a look at the Demo1.pas (Button1Click) to see an example.                  *)
(*                                                                                 *)
(***********************************************************************************)
(*                                                                                 *)
(*  version 1.0 -  18.03.2000                                                      *)
(*   - fixed adding empty lines in Tag.Params                                      *)
(*     (thanks to: JulianWEB <julian@clubdelphi.com>)                              *)
(*   - changed the name TParser to THtmlParser because of a conflict               *)
(*     with Classes.pas unit  (thanks: Michael Belmont)                            *)
(*   - a little improved demo project - now shows, what's inside all TTag objects  *)
(*                                                                                 *)
(*  version 0.9  -  30.12.1999                                                     *)
(*   - first released version                                                      *)
(*                                                                                 *)
(***********************************************************************************)

interface

uses
{$IF CompilerVersion >= 23} // XE2 or higher
  System.SysUtils, System.Classes;
{$ELSE}
  Classes, SysUtils;
{$IFEND}


type
  TSimpleEvent = procedure of object;

  TSandTag = class
    constructor Create;
    destructor Destroy; override;
  private
    fName: string;
    fParams: TStrings;
    fOnChanged: TSimpleEvent;
    procedure Changed;
    function GetName:string;
    function GetText:string;
    procedure SetName(const NewName:string);
    procedure SetText(const text:string);
  public
    property Text:string read GetText write SetText;  // this is all the stuff
                                                      // between "<" and ">"
    property Name:string read GetName write SetName;  // tag name (returns uppercase)
    property NameOriginal:string read fName; // FD
    property Params:TStrings read fParams;            // parameters list
  private
    // used only by TCatHTMLParser - updates TCatHTMLParser.Text
    property OnChanged:TSimpleEvent read fOnChanged write fOnChanged;
  end;

  TCatHTMLParser = class
    constructor Create;
    destructor Destroy; override;
  private
    fText: string;
    fTextBetween: string;
    fTag: TSandTag;
    fPos: Integer;               // current position in Text
    fTagPos,fTagLen: Integer;    // Tag position and length (including brackets)
    fTBPos: Integer;             // TextBetween position
    function GetTag:TSandTag;
    procedure SetText(const NewText:string);
    procedure SetTextBetween(const text:string);
    procedure TagChanged;
    procedure ClearTag;
    procedure ClearTB;
    procedure CheckPos;
    procedure SetTagText(const text:string);
    function FindTag(next:Boolean):Boolean;
    function GetTagLine:integer;// FD
  public
    property Text:string read fText write SetText;     // here is all the HTML file
    property Tag:TSandTag read GetTag;     // current tag
    procedure RemoveTag;               // remove the current tag
    procedure InsertTag(NewTag:TSandTag);  // insert a new tag BEFORE the current one
    procedure InsertText(text:string); // insert some text before the current tag
    function NextTag:Boolean;          // find next tag from current pos.
    function PrevTag:Boolean;          // find previous tag from current pos.
    procedure GotoBeginning;
    procedure GotoEnd;
    procedure LoadFromFile(filename:string);
    procedure SaveToFile(filename:string);
  public
    property Pos:integer read fPos; // FD
    property TagPos:integer read fTagPos; // FD
    property TagLine:integer read GetTagLine; // FD
    property TextBetweenPos:integer read fTBPOS; // FD
    property TextBetween:string        // this is the text between two tags:
             read fTextBetween         // - the last one - before calling NextTag/PrevTag
             write SetTextBetween;     // - and the new (current) one
  end;



implementation

function GetLineByPos(const s: string; const Position: Integer): Integer;
var
  i, ln: Integer;
begin
  result := -1;
  if (Position = -1) then
    Exit;

  i := 1;
  ln := 0;
  while i < Position do
  begin
    if (s[i] = #13) then
      ln := ln + 1;
    i := i + 1;
  end;
  result := ln;
end;

{ TParams }

type
  TParams = class (TStringList)
    fTag: TSandTag;
    procedure Changed; override;
  end;

procedure TParams.Changed;
begin
  inherited;
  if Assigned(fTag) then fTag.Changed;
end;



{ TSandTag }

constructor TSandTag.Create;
begin
  fName:= '';
  fParams:= TParams.Create;
  TParams(fParams).fTag:= Self;
  fOnChanged:= nil;
end;

destructor TSandTag.Destroy;
begin
  fParams.Free;
  inherited Destroy;
end;

procedure TSandTag.Changed;
begin
  if Assigned(fOnChanged) then fOnChanged;
end;

function TSandTag.GetName: string;
begin
  Result:= UpperCase(fName);
end;

procedure TSandTag.SetName(const NewName: string);
begin
  if NewName<>fName then begin
    fName:= NewName;
    Changed;
  end;
end;

function TSandTag.GetText: string;
var i: Integer;
begin
  Result:= fName;
  for i:= 0 to fParams.Count-1 do
    Result:= Result + ' ' + fParams[i];
end;

procedure TSandTag.SetText(const text: string);
var i,k: Integer;
    len: Integer;
    q1,q2: Boolean;
  procedure AddParam;
  var s: string;
  begin
    s:= Trim(Copy(text,k,i-k+1));
    if s<>'' then fParams.Add(s);
    k:= i+1;
  end;
begin
  q1:= False;
  q2:= False;
  len:= Length(text);

  // getting name
  i:= 1;
  while not ((i>len) or (text[i]=' ')) do Inc(i);
  fName:= Copy(text, 1, i-1);

  k:= i+1;  i:= k;
  fParams.Clear;
  // getting parameters
  while not (i>len) do begin
    if (text[i] in ['''', '"']) then begin
      if (text[i]='"')
       then begin if not q1 then q2:= not q2 end
       else begin if not q2 then q1:= not q1 end;
      if not (q1 or q2) then AddParam;
    end else
    if (text[i]=' ') and not (q1 or q2) then AddParam;
    Inc(i);
  end;
  if k<i then AddParam;
end;



{ TCatHTMLParser }

function TCatHTMLParser.GetTagLine:integer;
begin
 result:=GetLineByPos(ftext,ftagpos);
end;

constructor TCatHTMLParser.Create;
begin
  fTag:= TSandTag.Create;
  SetText('');
end;

procedure TCatHTMLParser.SetTagText(const text:string);
begin
  fTag.OnChanged:= nil;
  fTag.Text:= text;
  fTag.OnChanged:= TagChanged;
end;

destructor TCatHTMLParser.Destroy;
begin
  fTag.Free;
  inherited Destroy;
end;

function TCatHTMLParser.GetTag:TSandTag;
begin
  if fTagPos=0
   then Result:= nil
   else Result:= fTag;
end;

procedure TCatHTMLParser.ClearTag;
begin
  SetTagText('');
  fTagPos:= 0;
  fTagLen:= 0;
end;

procedure TCatHTMLParser.ClearTB;
begin
  fTextBetween:= '';
  fTBPos:= 0;
end;

procedure TCatHTMLParser.CheckPos;
begin
  if fPos<1 then fPos:= 1  else
  if fPos>Length(fText) then fPos:= Length(fText);
end;

procedure TCatHTMLParser.InsertTag(NewTag: TSandTag);
begin
  CheckPos;
  Insert('<'+NewTag.Text+'>', fText, fPos);
  NextTag;
end;

procedure TCatHTMLParser.InsertText(text: string);
begin
  CheckPos;
  ClearTB;
  Insert(text, fText, fPos);
  Inc(fPos, Length(text));
end;

procedure TCatHTMLParser.RemoveTag;
begin
  if fTagPos=0 then Exit;
  Delete(fText, fTagPos, fTagLen);
  ClearTag;
  ClearTB;
end;

procedure TCatHTMLParser.SetText(const NewText: string);
begin
  fText:= NewText;
  GotoBeginning;
end;

procedure TCatHTMLParser.SetTextBetween(const text: string);
begin
  if fTBPos=0 then Exit;
  if text<>fTextBetween then begin
    if (fTBPos<>0) and (fTagPos>fTBPos) then
      Inc(fTagPos, Length(text)-Length(fTextBetween));
    Delete(fText, fTBPos, Length(fTextBetween));
    Insert(text, fText, fTBPos);
  end;
end;

procedure TCatHTMLParser.TagChanged;
var s: string;
begin
  if fTagPos=0 then Exit;
  Delete(fText, fTagPos+1, fTagLen-2);
  s:= fTag.Text;
  if (fTBPos>fTagPos) then Inc(fTBPos, Length(s)+2-fTagLen);
  fTagLen:= Length(s)+2;
  Insert(s, fText, fTagPos+1);
end;


function TCatHTMLParser.NextTag: Boolean;
begin
  Result:= FindTag(True);
end;


function TCatHTMLParser.PrevTag: Boolean;
begin
  Result:= FindTag(False);
end;



function FindNext(const text:string; ch:char; startfrom:Integer; var pos:Integer):Boolean;
begin
  pos:= startfrom;
  while (pos<=Length(text)) and (text[pos]<>ch) do Inc(pos);
  Result:= (text[pos]=ch);
end;

function FindPrev(const text:string; ch:char; startfrom:Integer; var pos:Integer):Boolean;
begin
  pos:= startfrom;
  while (pos>0) and (text[pos]<>ch) do Dec(pos);
  Result:= (text[pos]=ch);
end;


function TCatHTMLParser.FindTag(next: Boolean): Boolean;
var tag1, tag2,         // first/last char of the new tag
    tb1, tb2: Integer;  // first/last char of new TextBetween
begin

  if Length(fText)=0 then begin
    Result:= False;
    Exit;
  end;

  if fTagPos<>0 then
    if next then Inc(fPos) else Dec(fPos);

  CheckPos;

  if next then begin
    // find next tag
    Result:= FindNext(fText, '<', fPos, tag1) and FindNext(fText, '>', tag1, tag2);
    // find end of current tag
    if FindNext(fText, '>', fPos, tb1) and (tb1<tag1)
     then tb1:= tb1+1
     else tb1:= fPos;
    tb2:= 0; //this is just to get rid of a stupid warning
  end
  else begin
    tb2:= fPos;
    // find previous tag
    Result:= FindPrev(fText, '>', tb2, tag2) and FindPrev(fText, '<', tag2, tag1);
  end;

  if Result then begin
    fPos:= tag1;
    if next
     then tb2:= tag1-1
     else tb1:= tag2+1;
  end
  else begin
    if next then begin
      fPos:= Length(fText);
      tb2:= Length(fText);
    end
    else begin
      fPos:= 1;
      tb1:= 1;
    end;
    tag1:= 0;
    tag2:= 0;
  end;

  fTagPos:= tag1;
  fTagLen:= tag2-tag1+1;
  SetTagText(Copy(fText, fTagPos+1, fTagLen-2));
  fTBPos:= tb1;
  fTextBetween:= Copy(fText, fTBPos, tb2-tb1+1);
end;

procedure TCatHTMLParser.GotoBeginning;
begin
  fPos:= 0;
  ClearTag;
  ClearTB;
end;

procedure TCatHTMLParser.GotoEnd;
begin
  fPos:= Length(fText);
  ClearTag;
  ClearTB;
end;

procedure TCatHTMLParser.LoadFromFile(filename: string);
var l: TStringList;
begin
  l:= TStringList.Create;
  try
    l.LoadFromFile(filename);
    Text:= l.Text;
  finally
    l.Free;
  end;
end;

procedure TCatHTMLParser.SaveToFile(filename: string);
var l: TStringList;
begin
  l:= TStringList.Create;
  try
    l.Text:= Text;
    l.SaveToFile(filename);
  finally
    l.Free;
  end;
end;

end.
