unit CatHighlighters;

{
  Catarinka - Multiple Code Highlighters
  Copyright (c) 2011-2018 Syhunt Informatica
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details

  Color scheme adapted from the CodeRay project (MIT-licensed)
  https://github.com/rubychan/coderay
  Copyright (C) 2005-2012 Kornelius Kalnbach <murphy@rubychan.de> (@murphy_karasu)
}

interface

{$I Catarinka.inc}

uses
{$IFDEF DXE2_OR_UP}
  System.Classes, System.SysUtils, System.TypInfo, Vcl.Graphics,
{$ELSE}
  Classes, SysUtils, TypInfo, Graphics,
{$ENDIF}
  SynUnicode,
  SynExportHTML,
  SynEditHighlighter,
  SynHighlighterJava,
  SynHighlighterRuby,
  SynHighlighterPerl,
  SynHighlighterPython,
  SynHighlighterPas,
  SynHighlighterVBScript,
  SynHighlighterSQL,
  SynHighlighterWeb,
  SynHighlighterWebData,
  SynHighlighterWebMisc;

type
  TCatHighlighters = class
  private
    fSynWebEngine: TSynWebEngine;
    fWebJS: TSynWebESSyn;
    fWebCSS: TSynWebCSSSyn;
    fWebHTML: TSynWebHtmlSyn;
    fWebXML: TSynWebXMLSyn;
    fWebPHP: TSynWebPHPPlainSyn;
    fJava: TSynJavaSyn;
    fRuby: TSynRubySyn;
    fPascal: TSynPasSyn;
    fPerl: TSynPerlSyn;
    fPython: TSynPythonSyn;
    fSQLSyn: TSynSQLSyn;
    fVBScript: TSynVBScriptSyn;
    function GetSynExport(HL: TSynCustomHighlighter; Source: string): string;
  public
    function GetByFileExtension(const fileext: string): TSynCustomHighlighter;
    function GetByContentType(const contenttype: string): TSynCustomHighlighter;
    function GetByResponseText(const s: string): TSynCustomHighlighter;
    function HighlightSourceByFileExt(const Source, fileext: string): string;
    procedure SetCodeRayColors(const e: TSynWebEngine);
    constructor Create(AOwner: TObject);
    destructor Destroy; override;
    property WebHTML: TSynWebHtmlSyn read fWebHTML;
  end;

implementation

uses
  CatStrings;

function TCatHighlighters.GetByContentType(const contenttype: string)
  : TSynCustomHighlighter;
var
  ct: string;
begin
  result := nil;
  ct := lowercase(contenttype);
  if pos('html', ct) <> 0 then
    result := fWebHTML;
  if pos('javascript', ct) <> 0 then
    result := fWebJS;
  if pos('json', ct) <> 0 then
    result := fWebJS;
  if pos('xml', ct) <> 0 then
    result := fWebXML;
  if pos('css', ct) <> 0 then
    result := fWebCSS;
end;

function TCatHighlighters.GetByResponseText(const s: string): TSynCustomHighlighter;
begin
  if pos('<html', s) <> 0 then
    result := fWebHTML;
end;

type
  TWebExts = (css, dpr, htm, html, java, js, json, jsie, lua, lp, pas, pasrem, php,
    pl, py, rb, sql, tis, vbs, xml);

function TCatHighlighters.GetByFileExtension(const fileext: string)
  : TSynCustomHighlighter;
var
  ext: string;
begin
  result := fWebHTML;
  ext := lowercase(fileext);
  if beginswith(ext, '.') then
    ext := after(ext, '.');
  if ext = emptystr then
    exit;
  case TWebExts(GetEnumValue(TypeInfo(TWebExts), ext)) of
    htm, html:
      result := fWebHTML;
    lua, lp:
      result := nil;
    java:
      result := fJava;
    js, json, jsie, tis:
      result := fWebJS;
    css:
      result := fWebCSS;
    php:
      result := fWebPHP;
    rb:
      result := fRuby;
    pas, dpr, pasrem:
      result := fPascal;
    pl:
      result := fPerl;
    py:
      result := fPython;
    sql:
      result := fSQLSyn;
    vbs:
      result := fVBScript;
    xml:
      result := fWebXML;
  end;
end;

function TCatHighlighters.GetSynExport(HL: TSynCustomHighlighter;
  Source: string): string;
var
  codestream: TMemoryStream;
  Code: TUnicodeStringList;
  SynExp: TSynExporterHTML;
begin
  result := emptystr;
  if Source = emptystr then
    exit;
  codestream := TMemoryStream.Create;
  Code := TUnicodeStringList.Create;
  Code.Text := Source;
  SynExp := TSynExporterHTML.Create(nil);
  SynExp.Highlighter := HL;
  SynExp.ExportAsText := TRUE;
  SynExp.ExportAll(Code);
  SynExp.SaveToStream(codestream);
  SynExp.free;
  codestream.Position := 0;
  Code.LoadFromStream(codestream);
  // code.SaveToFile('D:\debug.txt');
  result := Code.Text;
  Code.free;
  codestream.free;
end;

function TCatHighlighters.HighlightSourceByFileExt(const Source,
  fileext: string): string;
var
  HL: TSynCustomHighlighter;
begin
  result := emptystr;
  if Source = emptystr then
    exit;
  HL := GetByFileExtension(fileext);
  if HL <> nil then
    result := GetSynExport(HL, Source);
end;

procedure TCatHighlighters.SetCodeRayColors(const e: TSynWebEngine);
begin
  e.mltagnameattri.foreground := $00007700;
  e.mltagnameundefattri.foreground := $00007700;
  e.mltagattri.foreground := $00007700;
  e.mltagkeyattri.foreground := $008844BB;
  e.mltagkeyundefattri.foreground := $008844BB;
  e.mltagkeyundefattri.Style := e.mltagkeyundefattri.Style - [fsUnderline];
  e.mltagkeyvalueattri.foreground := $000022DD;
  e.mltagkeyvaluequotedattri.foreground := $000022DD;
  e.mlerrorattri.Style := e.mlerrorattri.Style - [fsUnderline];
  e.mlerrorattri.foreground := $00007700;
  e.eskeyattri.foreground := $00008800;
  e.esidentifierattri.foreground := clBlack;
  e.esnumberattri.foreground := $00DD0000;
  e.escommentattri.foreground := $00777777;
  e.eswhitespaceattri.background := $00E0E0E0;
  e.cssselectorattri.foreground := $00993333;
  e.csspropattri.foreground := $00660066;
  e.csspropundefattri.foreground := $00660066;
  e.cssselectorclassattri.foreground := $006600BB;
  e.cssselectoridattri.foreground := $0000AA00;
  e.cssrulesetwhitespaceattri.background := clNone;
  e.csswhitespaceattri.background := clNone;
  e.csscommentattri.foreground := $00777777;
  e.cssvalattri.foreground := $00888800;
  e.cssvalundefattri.foreground := $00888800;
  e.cssvalundefattri.Style := e.cssvalundefattri.Style - [fsUnderline];
  e.csserrorattri.foreground := $000000FF;
  e.csserrorattri.Style := e.csserrorattri.Style - [fsUnderline];
  e.cssvalnumberattri.foreground := $0000AA00;
  e.CssValStringAttri.foreground := $000022DD;
  e.phpstringattri.foreground := $000022DD;
  e.phpstringspecialattri.foreground := $000022DD;
  e.phpvariableattri.foreground := $00008800;
  e.phpfunctionattri.foreground := $00996633;
  e.phpfunctionattri.Style := e.phpfunctionattri.Style + [fsBold];
  e.phpkeyattri.foreground := $00008800;
  e.specialattri.phpmarker.foreground := $00666666;
  e.phpcommentattri.foreground := $00777777;
  e.phpdoccommentattri.foreground := $00777777;
  e.phpidentifierattri.foreground := $00BB6600;
  e.phpidentifierattri.Style := e.phpidentifierattri.Style + [fsBold];
  e.PhpNumberAttri.foreground := $00DD0000;
end;

constructor TCatHighlighters.Create(AOwner: TObject);
begin
  inherited Create;
  fJava := TSynJavaSyn.Create(nil);
  fRuby := TSynRubySyn.Create(nil);
  fPascal := TSynPasSyn.Create(nil);
  fPerl := TSynPerlSyn.Create(nil);
  fPython := TSynPythonSyn.Create(nil);
  fVBScript := TSynVBScriptSyn.Create(nil);
  fSQLSyn := TSynSQLSyn.Create(nil);
  fSynWebEngine := TSynWebEngine.Create(nil);
  fSynWebEngine.Options.CssVersion := scvCSS3;
  fWebHTML := TSynWebHtmlSyn.Create(nil);
  fWebHTML.Engine := fSynWebEngine;
  fWebHTML.Options.PhpEmbeded := false;
  fWebPHP := TSynWebPHPPlainSyn.Create(nil);
  fWebPHP.Engine := fSynWebEngine;
  fWebJS := TSynWebESSyn.Create(nil);
  fWebJS.Engine := fSynWebEngine;
  fWebCSS := TSynWebCSSSyn.Create(nil);
  fWebCSS.Engine := fSynWebEngine;
  fWebCSS.Options.CssVersion := scvCSS3;
  fWebXML := TSynWebXMLSyn.Create(nil);
  fWebXML.Engine := fSynWebEngine;
  SetCodeRayColors(fSynWebEngine);
end;

destructor TCatHighlighters.Destroy;
begin
  fSynWebEngine.Free;
  fWebHTML.Free;
  fWebPHP.Free;
  fWebJS.Free;
  fWebCSS.Free;
  fWebXML.Free;
  fSQLSyn.Free;
  fVBScript.Free;
  fPython.Free;
  fPerl.Free;
  fPascal.Free;
  fRuby.Free;
  fJava.Free;
  inherited;
end;

end.
