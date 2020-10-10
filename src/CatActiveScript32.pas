unit CatActiveScript32;
{
  This is an ugly hack to allow two IActiveScriptParse, one linked to
  CLSID for 64-bit and this one linked to the CLSID for 32-bit
  In case you want to try both without having to recompile your binary
}

{*******************************************************}
{                ActiveScript library                }
{                     version 1.1                       }
{ Author:                                               }
{ Serhiy Perevoznyk                                     }
{ serge_perevoznyk@hotmail.com                          }
{                                                       }
{*******************************************************}

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is ActiveScript Library
 *
 * The Initial Developer of the Original Code is
 * Serhiy Perevoznyk
 *
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$I Catarinka.inc}

uses
 {$IFDEF DXE2_OR_UP}
   Winapi.Windows, System.SysUtils, Winapi.ActiveX, System.Win.ComObj, System.Contnrs,
   System.Classes, Vcl.Dialogs, System.Variants, Vcl.Forms;
 {$ELSE}
  Windows, SysUtils, ActiveX, ComObj, Contnrs, Classes, Dialogs,
  {$IFNDEF VER130}
  Variants,
  {$ENDIF}
  Forms;
 {$ENDIF}

const
  xSCATID_ActiveScript =               '{F0B7A1A1-9847-11cf-8F20-00805F2CD064}';
  xSCATID_ActiveScriptParse =          '{F0B7A1A2-9847-11cf-8F20-00805F2CD064}';
  xSID_IActiveScript =                 '{BB1A2AE1-A4F9-11cf-8F20-00805F2CD064}';
  xSID_IActiveScriptParse =            '{BB1A2AE2-A4F9-11cf-8F20-00805F2CD064}';
  xSID_IActiveScriptParseProcedureOld ='{1CFF0050-6FDD-11d0-9328-00A0C90DCAA9}';
  xSID_IActiveScriptParseProcedure =   '{AA5B6A80-B834-11d0-932F-00A0C90DCAA9}';
  xSID_IActiveScriptError =            '{EAE1BA61-A4ED-11cf-8F20-00805F2CD064}';

  xSID_IActiveScriptSite =             '{DB01A1E3-A42B-11cf-8F20-00805F2CD064}';
  xSID_IActiveScriptSiteWindow =       '{D10F6761-83E9-11cf-8F20-00805F2CD064}';
  xSID_IActiveScriptSiteInterruptPoll ='{539698A0-CDCA-11CF-A5EB-00AA0047A063}';
  xSID_IBindEventHandler =             '{63CDBCB0-C1B1-11d0-9336-00A0C90DCAA9}';
  xSID_IActiveScriptStats =            '{B8DA6310-E19B-11d0-933C-00A0C90DCAA9}';

  xCATID_ActiveScript:                 TGUID = xSCATID_ActiveScript;
  xCATID_ActiveScriptParse:            TGUID = xSCATID_ActiveScriptParse;
  xIID_IActiveScript:                  TGUID = xSID_IActiveScript;
  xIID_IActiveScriptParse:             TGUID = xSID_IActiveScriptParse;
  xIID_IActiveScriptParseProcedureOld: TGUID = xSID_IActiveScriptParseProcedureOld;
  xIID_IActiveScriptParseProcedure:    TGUID = xSID_IActiveScriptParseProcedure;
  xIID_IActiveScriptSite:              TGUID = xSID_IActiveScriptSite;
  xIID_IActiveScriptSiteWindow:        TGUID = xSID_IActiveScriptSiteWindow;
  xIID_IActiveScriptSiteInterruptPoll: TGUID = xSID_IActiveScriptSiteInterruptPoll;
  xIID_IActiveScriptError:             TGUID = xSID_IActiveScriptError;
  xIID_IBindEventHandler:              TGUID = xSID_IBindEventHandler;
  xIID_IActiveScriptStats:             TGUID = xSID_IActiveScriptStats;

// Constants used by ActiveX Scripting:
//

(* IActiveScript::AddNamedItem() input flags *)

  xSCRIPTITEM_ISVISIBLE     = $00000002;
  xSCRIPTITEM_ISSOURCE      = $00000004;
  xSCRIPTITEM_GLOBALMEMBERS = $00000008;
  xSCRIPTITEM_ISPERSISTENT  = $00000040;
  xSCRIPTITEM_CODEONLY      = $00000200;
  xSCRIPTITEM_NOCODE        = $00000400;
  xSCRIPTITEM_ALL_FLAGS     =(xSCRIPTITEM_ISSOURCE or
                             xSCRIPTITEM_ISVISIBLE or
                             xSCRIPTITEM_ISPERSISTENT or
                             xSCRIPTITEM_GLOBALMEMBERS or
                             xSCRIPTITEM_NOCODE or
                             xSCRIPTITEM_CODEONLY);

(* IActiveScript::AddTypeLib() input flags *)

  xSCRIPTTYPELIB_ISCONTROL    = $00000010;
  xSCRIPTTYPELIB_ISPERSISTENT = $00000040;
  xSCRIPTTYPELIB_ALL_FLAGS    = (xSCRIPTTYPELIB_ISCONTROL or
                                xSCRIPTTYPELIB_ISPERSISTENT);

(* IActiveScriptParse::AddScriptlet() and
   IActiveScriptParse::ParseScriptText() input flags *)

  xSCRIPTTEXT_DELAYEXECUTION    = $00000001;
  xSCRIPTTEXT_ISVISIBLE         = $00000002;
  xSCRIPTTEXT_ISEXPRESSION      = $00000020;
  xSCRIPTTEXT_ISPERSISTENT      = $00000040;
  xSCRIPTTEXT_HOSTMANAGESSOURCE = $00000080;
  xSCRIPTTEXT_ALL_FLAGS         = (xSCRIPTTEXT_DELAYEXECUTION or
                                  xSCRIPTTEXT_ISVISIBLE or
                                  xSCRIPTTEXT_ISEXPRESSION or
                                  xSCRIPTTEXT_ISPERSISTENT or
                                  xSCRIPTTEXT_HOSTMANAGESSOURCE);

(* IActiveScriptParseProcedure::ParseProcedureText() input flags *)

  xSCRIPTPROC_HOSTMANAGESSOURCE = $00000080;
  xSCRIPTPROC_IMPLICIT_THIS     = $00000100;
  xSCRIPTPROC_IMPLICIT_PARENTS  = $00000200;
  xSCRIPTPROC_ALL_FLAGS         = (xSCRIPTPROC_HOSTMANAGESSOURCE or
                                  xSCRIPTPROC_IMPLICIT_THIS or
                                  xSCRIPTPROC_IMPLICIT_PARENTS);

(* IActiveScriptSite::GetItemInfo() input flags *)

  xSCRIPTINFO_IUNKNOWN  = $00000001;
  xSCRIPTINFO_ITYPEINFO = $00000002;
  xSCRIPTINFO_ALL_FLAGS = (xSCRIPTINFO_IUNKNOWN or
                          xSCRIPTINFO_ITYPEINFO);

(* IActiveScript::Interrupt() Flags *)

  xSCRIPTINTERRUPT_DEBUG          = $00000001;
  xSCRIPTINTERRUPT_RAISEEXCEPTION = $00000002;
  xSCRIPTINTERRUPT_ALL_FLAGS      = (xSCRIPTINTERRUPT_DEBUG or
                                    xSCRIPTINTERRUPT_RAISEEXCEPTION);

(* IActiveScriptStats::GetStat() values *)

  xSCRIPTSTAT_STATEMENT_COUNT   = 1;
  xSCRIPTSTAT_INSTRUCTION_COUNT = 2;
  xSCRIPTSTAT_INTSTRUCTION_TIME = 3;
  xSCRIPTSTAT_TOTAL_TIME        = 4;

(* script state values *)

type
  xtagSCRIPTSTATE = integer;
  xSCRIPTSTATE = xtagSCRIPTSTATE;
const
  xSCRIPTSTATE_UNINITIALIZED = $00000000;
  xSCRIPTSTATE_INITIALIZED   = $00000005;
  xSCRIPTSTATE_STARTED       = $00000001;
  xSCRIPTSTATE_CONNECTED     = $00000002;
  xSCRIPTSTATE_DISCONNECTED  = $00000003;
  xSCRIPTSTATE_CLOSED        = $00000004;

(* script thread state values *)

type
  xtagSCRIPTTHREADSTATE = integer;
  xSCRIPTTHREADSTATE = xtagSCRIPTTHREADSTATE;
const
  xSCRIPTTHREADSTATE_NOTINSCRIPT = $00000000;
  xSCRIPTTHREADSTATE_RUNNING     = $00000001;

(* Thread IDs *)

type
  xSCRIPTTHREADID = DWORD;
const
  xSCRIPTTHREADID_CURRENT = xSCRIPTTHREADID(-1);
  xSCRIPTTHREADID_BASE    = xSCRIPTTHREADID(-2);
  xSCRIPTTHREADID_ALL     = xSCRIPTTHREADID(-3);

type
  xIActiveScriptSite =           interface;
  xIActiveScriptSiteWindow =     interface;
  xIActiveScript =               interface;
  xIActiveScriptParse =          interface;
  xIActiveScriptParseProcedure = interface;
  xIActiveScriptError =          interface;
  LPCOLESTR = PWideChar;

  xIActiveScriptSite = interface(IUnknown)
    [xSID_IActiveScript]
    function GetLCID(out plcid: LCID): HResult; stdcall;
    function GetItemInfo(
      pstrName: LPCOLESTR;
      dwReturnMask: DWORD;
      out ppiunkItem: IUnknown;
      out ppti: ITypeInfo): HResult; stdcall;
    function GetDocVersionString(out pbstrVersion: WideString): HResult; stdcall;
    function OnScriptTerminate(
      var pvarResult: OleVariant;
      var pexcepinfo: EXCEPINFO): HResult; stdcall;
    function OnStateChange(ssScriptState: xSCRIPTSTATE): HResult; stdcall;
    function OnScriptError(
      const pscripterror: xIActiveScriptError): HResult; stdcall;
    function OnEnterScript: HResult; stdcall;
    function OnLeaveScript: HResult; stdcall;
  end;

  xIActiveScriptError = interface(IUnknown)
    [xSID_IActiveScriptError]
    function GetExceptionInfo(out pexcepinfo: EXCEPINFO): HResult; stdcall;
    function GetSourcePosition(
      out pdwSourceContext: DWORD;
      out pulLineNumber: ULONG;
      out plCharacterPosition: Integer): HResult; stdcall;
    function GetSourceLineText(out pbstrSourceLine: WideString): HResult; stdcall;
  end;

  xIActiveScriptSiteWindow = interface(IUnknown)
    [xSID_IActiveScriptSiteWindow]
    function GetWindow(out phwnd: HWND): HResult; stdcall;
    function EnableModeless(fEnable: BOOL): HResult; stdcall;
  end;

  xIActiveScriptSiteInterruptPoll = interface(IUnknown)
    [xSID_IActiveScriptSiteInterruptPoll]
    function QueryContinue: HResult; stdcall;
  end;

  xIActiveScript = interface(IUnknown)
    [xSID_IActiveScript]
    function SetScriptSite(const pass: xIActiveScriptSite): HResult; stdcall;
    function GetScriptSite(
      const riid: TGUID;
      out ppvObject: Pointer): HResult; stdcall;
    function SetScriptState(ss: xSCRIPTSTATE): HResult; stdcall;
    function GetScriptState(out pssState: xSCRIPTSTATE): HResult; stdcall;
    function Close: HResult; stdcall;
    function AddNamedItem(
      pstrName: LPCOLESTR;
      dwFlags: DWORD): HResult; stdcall;
    function AddTypeLib(
      const rguidTypeLib: TGUID;
      dwMajor: DWORD;
      dwMinor: DWORD;
      dwFlags: DWORD): HResult; stdcall;
    function GetScriptDispatch(
      pstrItemName: LPCOLESTR;
      out ppdisp: IDispatch): HResult; stdcall;
    function GetCurrentScriptThreadID(
      out pstidThread: xSCRIPTTHREADID): HResult; stdcall;
    function GetScriptThreadID(dwWin32ThreadId: DWORD;
      out pstidThread: xSCRIPTTHREADID): HResult; stdcall;
    function GetScriptThreadState(
      stidThread: xSCRIPTTHREADID;
      out pstsState: xSCRIPTTHREADSTATE): HResult; stdcall;
    function InterruptScriptThread(
      stidThread: xSCRIPTTHREADID;
      var pexcepinfo: EXCEPINFO;
      dwFlags: DWORD): HResult; stdcall;
    function Clone(out ppscript: xIActiveScript): HResult; stdcall;
  end;

  xIActiveScriptParse = interface(IUnknown)
    [xSID_IActiveScriptParse]
    function InitNew: HResult; stdcall;
    function AddScriptlet(
      pstrDefaultName: LPCOLESTR;
      pstrCode: LPCOLESTR;
      pstrItemName: LPCOLESTR;
      pstrSubItemName: LPCOLESTR;
      pstrEventName: LPCOLESTR;
      pstrDelimiter: LPCOLESTR;
      dwSourceContextCookie: DWORD;
      ulStartingLineNumber: ULONG;
      dwFlags: DWORD;
      out pbstrName: WideString;
      out pexcepinfo: EXCEPINFO): HResult; stdcall;
    function ParseScriptText(
      pstrCode: LPCOLESTR;
      pstrItemName: LPCOLESTR;
      const punkContext: IUnknown;
      pstrDelimiter: LPCOLESTR;
      dwSourceContextCookie: DWORD;
      ulStartingLineNumber: ULONG;
      dwFlags: DWORD;
      out pvarResult: OleVariant;
      out pexcepinfo: EXCEPINFO): HResult; stdcall;
  end;

  xIActiveScriptParseProcedureOld = interface(IUnknown)
    [xSID_IActiveScriptParseProcedureOld]
    function ParseProcedureText(
      pstrCode: LPCOLESTR;
      pstrFormalParams: LPCOLESTR;
      pstrItemName: LPCOLESTR;
      const punkContext: IUnknown;
      pstrDelimiter: LPCOLESTR;
      dwSourceContextCookie: DWORD;
      ulStartingLineNumber: ULONG;
      dwFlags: DWORD;
      out ppdisp: IDispatch): HResult; stdcall;
  end;

  xIActiveScriptParseProcedure = interface(IUnknown)
    [xSID_IActiveScriptParseProcedure]
    function ParseProcedureText(
      pstrCode: LPCOLESTR;
      pstrFormalParams: LPCOLESTR;
      pstrProcedureName: LPCOLESTR;
      pstrItemName: LPCOLESTR;
      const punkContext: IUnknown;
      pstrDelimiter: LPCOLESTR;
      dwSourceContextCookie: DWORD;
      ulStartingLineNumber: ULONG;
      dwFlags: DWORD;
      out ppdisp: IDispatch): HResult; stdcall;
  end;

  xIBindEventHandler = interface(IUnknown)
    [xSID_IBindEventHandler]
    function BindHandler(
      pstrEvent: LPCOLESTR;
      const pdisp: IDispatch): HResult; stdcall;
  end;

  xIActiveScriptStats = interface(IUnknown)
    [xSID_IActiveScriptStats]
    function GetStat(
      stid: DWORD;
      out pluHi: ULONG;
      out pluLo: ULONG): HResult; stdcall;
    function GetStatEx(
      const guid: TGUID;
      out pluHi: ULONG;
      out pluLo: ULONG): HResult; stdcall;
    function ResetStats: HResult; stdcall;
  end;

type
 TOnActiveScriptError = procedure(Sender : TObject; Line, Pos : integer; ASrc : string; ADescription : string) of object;

  TXSyScriptGlobalObjects = class(TObject)
  private
    FIntfList: IInterfaceList;
    FNamedList: TStrings;
    FGlobalMembers: TStrings;
  public
    constructor Create;
    function GetNamedItemCount: Integer;
    function GetNamedItemName(I: Integer): string;
    procedure AddNamedIntf(const AName: string; AIntf: IUnknown);
    function FindNamedItemIntf(const AName: string): IUnknown;
    destructor Destroy; override;
    property NamedItemCount: Integer read GetNamedItemCount;
    property NamedItemName[I: Integer]: string read GetNamedItemName;
  end;

  TXSyActiveScriptSite = class(TComponent, xIActiveScriptSite)
  private
    FUseSafeSubset : boolean;
    FDisp: OleVariant;
    FGlobalObjects : TXSyScriptGlobalObjects;
    FOnError : TOnActiveScriptError;
    FParser: xIActiveScriptParse;
    FScriptLanguage : string;
    procedure CreateScriptEngine(Language: string);
    procedure CloseScriptEngine;
  protected
    { IActiveScriptSite }
    function  GetLCID(out plcid: LongWord): HResult; stdcall;
    function GetItemInfo(
      pstrName: LPCOLESTR;
      dwReturnMask: DWORD;
      out ppiunkItem: IUnknown;
      out ppti: ITypeInfo): HResult; stdcall;
    function  GetDocVersionString(out pbstrVersion: WideString): HResult; stdcall;
    function  OnScriptTerminate(var pvarResult: OleVariant; var pexcepinfo: EXCEPINFO): HResult; stdcall;
    function  OnStateChange(ssScriptState: xtagSCRIPTSTATE): HResult; stdcall;
    function  OnScriptError(const pscripterror: xIActiveScriptError): HResult; stdcall;
    function  OnEnterScript: HResult; stdcall;
    function  OnLeaveScript: HResult; stdcall;
  public
    FEngine: xIActiveScript; // FD
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    function RunExpression(ACode : Widestring) : string;
    procedure  Execute(ACode : WideString);
    procedure AddNamedItem(AName : string; AIntf : IUnknown);
    procedure AddGlobalMember(AName:string); //FD
    property ScriptInterface : OleVariant read FDisp;
  published
    property ScriptLanguage : string read FScriptLanguage write FScriptLanguage;
    property OnError : TOnActiveScriptError read FOnError write FOnError;
    property UseSafeSubset : boolean read FUseSafeSubset write FUseSafeSubset default false;
  end;

  TXSyActiveScriptWindow = class(TXSyActiveScriptSite, xIActiveScriptSiteWindow)
  protected
    {IActiveSriptSiteWindow}
    function GetWindow(out phwnd: HWND): HResult; stdcall;
    function EnableModeless(fEnable: BOOL): HResult; stdcall;
  end;

procedure GetActiveScriptParseX(List: TStrings);


implementation

const
  INTERFACESAFE_FOR_UNTRUSTED_CALLER = $00000001  // Caller of interface may be untrusted
  ;
  INTERFACESAFE_FOR_UNTRUSTED_DATA = $00000002  // Data passed into interface may be untrusted
  ;
  INTERFACE_USES_DISPEX = $00000004  // Object knows to use IDispatchEx
  ;
  INTERFACE_USES_SECURITY_MANAGER = $00000008  // Object knows to use IInternetHostSecurityManager
  ;


procedure GetActiveScriptParseX(List: TStrings);
var
  ProgID: string;

  function ValidProgID: Boolean;
  var
    PID: string;
  begin
     if Length(ProgID) > 7 then
       Result := AnsiCompareStr('.Encode', Copy(ProgID, Length(ProgID)-6, 7)) <> 0
     else
       Result := True;
     // Exclude XML script engine
     if CompareText(Copy(ProgID, 1, 3), 'XML') = 0 then
       Result := False;
     // Exclude "signed" script engines
     PID := UpperCase(ProgID);
     if Pos('SIGNED', PID) <> 0 then
       Result := False;
  end;
var
  EnumGUID: IEnumGUID;
  Fetched: Cardinal;
  Guid: TGUID;
  Rslt: HResult;
  CatInfo: ICatInformation;
  I, BufSize: Integer;
  ClassIDKey: HKey;
  S: string;
  Buffer: array[0..255] of Char;
begin
  List.Clear;
  Rslt := CoCreateInstance(CLSID_StdComponentCategoryMgr, nil,
    CLSCTX_INPROC_SERVER, ICatInformation, CatInfo);
  if Succeeded(Rslt) then
  begin
    OleCheck(CatInfo.EnumClassesOfCategories(1, @xCATID_ActiveScriptParse, 0, nil, EnumGUID));
    while EnumGUID.Next(1, Guid, Fetched) = S_OK do
    begin
      try
        ProgID := ClassIDToProgID(Guid);
        if ValidProgID then
          List.Add(ProgID);
      except
        ProgID := ClassIDToProgID(StringToGUID(Buffer));
        List.Add('Invalid Entry In Categories');
      end;
    end;
  end else
  begin
    if RegOpenKey(HKEY_CLASSES_ROOT, 'CLSID', ClassIDKey) <> 0 then
      try
        I := 0;
        while RegEnumKey(ClassIDKey, I, Buffer, SizeOf(Buffer)) = 0 do
        begin
          S := Format('%s\Implemented Categories\%s',[Buffer,  { do not localize }
            GUIDToString(xCATID_ActiveScriptParse)]);
          // FD: add XE2 or up compatibility
          {$IFDEF DXE2_OR_UP}
          if RegQueryValue(ClassIDKey, PWideChar(WideString(S)), nil, BufSize) = 0 then
          {$ELSE}
          if RegQueryValue(ClassIDKey, PChar(S), nil, BufSize) = 0 then
          {$ENDIF}
          begin
            ProgID := ClassIDToProgID(StringToGUID(Buffer));
            if ValidProgID then
              List.Add(ProgID);
          end;
          Inc(I);
        end;
      finally
        RegCloseKey(ClassIDKey);
      end;
  end;
end;

{ TXSyActiveScriptSite }


constructor TXSyActiveScriptSite.Create(AOwner : TComponent);
begin
  inherited;
  FScriptLanguage := 'VBScript';
  FGlobalObjects := TXSyScriptGlobalObjects.Create;
  FUseSafeSubset := false;
end;

destructor TXSyActiveScriptSite.Destroy;
begin
  CloseScriptEngine;
  FGlobalObjects.Free;
  inherited;
end;

procedure TXSyActiveScriptSite.AddGlobalMember(AName:string); //FD
begin
 FGlobalObjects.FGlobalMembers.Add(aname)
end;

procedure TXSyActiveScriptSite.AddNamedItem(AName: string;
  AIntf: IUnknown);
begin
  FGlobalObjects.AddNamedIntf(AName, AIntf);
end;



procedure TXSyActiveScriptSite.CreateScriptEngine(
  Language: string);
const
  NULL_GUID: TGUID = '{00000000-0000-0000-0000-000000000000}';
var
  ScriptCLSID : TGUID;
  LanguageW : WideString;
  hr : HRESULT;
  i : integer;
  Disp: IDispatch;
  pOs : IObjectSafety;
  dwSupported : DWORD;
  dwEnabled : DWORD;
begin
  CloseScriptEngine;
  LanguageW := Language;
  if CLSIDFromProgID(PWideChar(LanguageW), ScriptCLSID) <> S_OK then
    ScriptCLSID := NULL_GUID;
  FEngine := CreateComObject(ScriptCLSID) as xIActiveScript;
  if FUseSafeSubset then
   begin
     dwSupported := 0;
     dwEnabled := 0;
     FEngine.QueryInterface(IObjectSafety, pOS);
     if Assigned(pOS) then
      begin
        pOS.GetInterfaceSafetyOptions(IDispatch, @dwSupported, @dwEnabled);
          if (INTERFACE_USES_SECURITY_MANAGER and dwSupported) = INTERFACE_USES_SECURITY_MANAGER then
           begin
             dwEnabled := dwEnabled or INTERFACE_USES_SECURITY_MANAGER;
           end;
         pOS.SetInterfaceSafetyOptions(IDispatch, INTERFACE_USES_SECURITY_MANAGER, dwEnabled);
      end;
    end;
  //writeln(GUIDToString(IActiveScriptParse));
  hr := FEngine.QueryInterface(xIActiveScriptParse, FParser);
  OLECHECK(hr);
  hr := FEngine.SetScriptSite(Self);
  OLECHECK(hr);

  hr := FParser.InitNew();
  OLECHECK(hr);
  for I := 0 to FGlobalObjects.NamedItemCount - 1 do begin
    if FGlobalObjects.FGlobalMembers.IndexOf(FGlobalObjects.NamedItemName[I]) = -1 then begin
      // FD
      FEngine.AddNamedItem(PWideChar(WideString(FGlobalObjects.NamedItemName[I])), xSCRIPTITEM_ISVISIBLE);
    end else begin
      FEngine.AddNamedItem(PWideChar(WideString(FGlobalObjects.NamedItemName[I])), xSCRIPTITEM_ISVISIBLE or xSCRIPTITEM_GLOBALMEMBERS);
    end;
  end;
  FEngine.GetScriptDispatch(nil, Disp);
  FDisp := Disp;
end;


procedure TXSyActiveScriptSite.CloseScriptEngine;
begin
  FParser := nil;
  if FEngine <> nil then FEngine.Close;
    FEngine := nil;
  FDisp := Unassigned;
end;

function TXSyActiveScriptSite.RunExpression(ACode: WideString): string;
var
  AResult: OleVariant;
  ExcepInfo: TEXCEPINFO;
begin
  //CreateScriptEngine(FScriptLanguage); //FD: commented out to allow SCRIPTTEXT_ISPERSISTENT
  try
  if FParser.ParseScriptText(PWideChar(ACode), nil, nil, nil, 0, 0,
    xSCRIPTTEXT_ISEXPRESSION, AResult, ExcepInfo) = S_OK
    then
      Result := AResult
        else
          Result := '';
  except end; // FD
end;


procedure TXSyActiveScriptSite.Execute(ACode: Widestring);
var
  Result: OleVariant;
  ExcepInfo: TEXCEPINFO;
begin
  CreateScriptEngine(FScriptLanguage);
  FParser.ParseScriptText(PWideChar(ACode), nil, nil, nil, 0, 0, xSCRIPTTEXT_ISPERSISTENT, Result, ExcepInfo); // FD
  FEngine.SetScriptState(xSCRIPTSTATE_CONNECTED);
end;

function TXSyActiveScriptSite.GetDocVersionString(
  out pbstrVersion: WideString): HResult;
begin
  Result := E_NOTIMPL;
end;

function TXSyActiveScriptSite.GetItemInfo(pstrName: LPCOLESTR;
      dwReturnMask: DWORD;
  out ppiunkItem: IUnknown;
  out ppti: ITypeInfo): HResult; stdcall;
begin
  if @ppiunkItem <> nil then Pointer(ppiunkItem) := nil;
  if @ppti <> nil then Pointer(ppti) := nil;
  if (dwReturnMask and xSCRIPTINFO_IUNKNOWN) <> 0
    then ppiunkItem := FGlobalObjects.FindNamedItemIntf(pstrName);
  Result := S_OK;
end;

function TXSyActiveScriptSite.GetLCID(out plcid: LongWord): HResult;
begin
  plcid := GetSystemDefaultLCID;
  Result := S_OK;
end;

function TXSyActiveScriptSite.OnEnterScript: HResult;
begin
  result := S_OK;
end;

function TXSyActiveScriptSite.OnLeaveScript: HResult;
begin
  result := S_OK;
end;

function TXSyActiveScriptSite.OnScriptError(
  const pscripterror: xIActiveScriptError): HResult;
var
  wCookie   : Dword;
  ExcepInfo : TExcepInfo;
  CharNo    : integer;
  LineNo    : DWORD;
  SourceLineW : WideString;
  SourceLine : string;
  Desc : string;
begin
  Result := S_OK;
  wCookie := 0;
  LineNo  := 0;
  CharNo  := 0;
  if Assigned(pscripterror) then
    begin
      pscripterror.GetExceptionInfo(ExcepInfo);
      Desc := ExcepInfo.bstrDescription;
      pscripterror.GetSourcePosition(wCookie, LineNo, CharNo);
      pscripterror.GetSourceLineText(SourceLineW);
      SourceLine := SourceLineW;
      if Assigned(FOnError) then
        FOnError(Self, LineNo, CharNo, SourceLine, Desc);
    end;
end;

function TXSyActiveScriptSite.OnScriptTerminate(var pvarResult: OleVariant;
  var pexcepinfo: EXCEPINFO): HResult;
begin
  Result := S_OK;
end;

function TXSyActiveScriptSite.OnStateChange(
  ssScriptState: xtagSCRIPTSTATE): HResult;
begin
   case ssScriptState of
     xSCRIPTSTATE_UNINITIALIZED:;
     xSCRIPTSTATE_INITIALIZED:;
     xSCRIPTSTATE_STARTED:;
     xSCRIPTSTATE_CONNECTED:;
     xSCRIPTSTATE_DISCONNECTED:;
     xSCRIPTSTATE_CLOSED:;
   end;

   Result := S_OK;

end;


{ TXSyActiveScriptWindow }

function TXSyActiveScriptWindow.EnableModeless(fEnable: BOOL): HResult;
begin
  Result := S_OK;
end;

function TXSyActiveScriptWindow.GetWindow(out phwnd: HWND): HResult;
begin
  if (Owner is TCustomForm) then
   begin
    phwnd := (Owner as TCustomForm).Handle;
    Result := S_OK;
   end
    else
      begin
        phwnd := 0;
        Result := S_FALSE;
      end;
end;


{ TXSyScriptGlobalObjects }

procedure TXSyScriptGlobalObjects.AddNamedIntf(const AName: string; AIntf: IUnknown);
begin
  FNamedList.Add(AName);
  FIntfList.Add(AIntf);
end;

constructor TXSyScriptGlobalObjects.Create;
begin
  inherited Create;
  FNamedList := TStringList.Create;
  FIntfList := TInterfaceList.Create;
  FGlobalMembers:=TStringList.create;// FD
end;

destructor TXSyScriptGlobalObjects.Destroy;
begin
  inherited;
  FNamedList.Free;
  FGlobalMembers.free; // FD
end;

function TXSyScriptGlobalObjects.FindNamedItemIntf(const AName: string): IUnknown;
var
  I: Integer;
begin
  I := FNamedList.IndexOf(AName);
  if I >= 0 then
    Result := FIntfList[I]
  else
    Result := nil;
end;

function TXSyScriptGlobalObjects.GetNamedItemCount: Integer;
begin
  Result := FNamedList.Count;
end;

function TXSyScriptGlobalObjects.GetNamedItemName(I: Integer): string;
begin
  Result := FNamedList[I];
end;

end.
