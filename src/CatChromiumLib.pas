unit CatChromiumLib;

{
  Catarinka Browser Component
  Copyright (c) 2011-2017 Syhunt Informatica
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details
}

interface

{$I Catarinka.inc}

uses
{$IFDEF DXE2_OR_UP}
  System.Classes, Winapi.Windows, Winapi.Messages, Vcl.Controls, Vcl.Graphics,
  Vcl.Forms, System.SysUtils, System.SyncObjs, Vcl.Dialogs, Vcl.Clipbrd,
  System.TypInfo, Vcl.StdCtrls, Vcl.ExtCtrls, System.UITypes,
{$ELSE}
  Classes, Windows, Messages, Controls, Graphics, Forms, SysUtils, SyncObjs,
  Dialogs, Clipbrd, TypInfo, StdCtrls, ExtCtrls,
{$ENDIF}
{$IFDEF USEWACEF}
  WACefComponent, WACefInterfaces, WACefTypes, WACefOwns, WACefCExports,
  WACefLib, WACefRefs,
{$ELSE}
  cefgui, cefvcl, ceflib,
{$ENDIF}
  superobject, CatJSON, CatMsg, CatPanels;

{$IFDEF USEWACEF}

type
  TChromium = TWAChromium;
  TCustomChromiumOSR = TWAChromiumOSR;
{$ENDIF}

type
  TCatChromiumOnBrowserMessage = procedure(const msg: integer;
    const str: string) of object;
  TCatChromiumOnAfterSetSource = procedure(const s: string) of object;
  TCatChromiumOnTitleChange = procedure(Sender: TObject; const title: string)
    of object;
  TCatChromiumOnLoadEnd = procedure(Sender: TObject; httpStatusCode: integer)
    of object;
  TCatChromiumOnLoadStart = procedure(Sender: TObject) of object;
  TCatChromiumOnAddressChange = procedure(Sender: TObject; const url: string)
    of object;
  TCatChromiumOnStatusMessage = procedure(Sender: TObject; const value: string)
    of object;
  // TCatChromiumOnRequestComplete = procedure(const s:string) of object;
  TCatChromiumOnBeforePopup = procedure(Sender: TObject; var url: string;
    out Result: Boolean) of object;
  TCatChromiumOnConsoleMessage = procedure(Sender: TObject;
    const message, source: string; line: integer) of object;
  TCatChromiumOnBeforeResourceLoad = procedure(Sender: TObject;
    const request: ICefRequest; out Result: Boolean) of object;
  TCatChromiumOnBeforeDownload = procedure(Sender: TObject; const id: integer;
    const suggestedName: string) of object;
  TCatChromiumOnDownloadUpdated = procedure(Sender: TObject;
    var cancel: Boolean; const id, state, percentcomplete: integer;
    const fullPath: string) of object;
  TCatChromiumOnLoadingStateChange = procedure(Sender: TObject;
    const isLoading, canGoBack, canGoForward: Boolean) of object;
  TCatChromiumOnLoadError = procedure(Sender: TObject; const errorCode: integer;
    const errorText, failedUrl: string) of object;
  TCatChromiumOnCertificateError = procedure(Sender: TObject;
    aCertError: TCefErrorCode; const aRequestUrl: ustring;
    const aSslInfo: ICefSslinfo; const aCallback: ICefRequestCallback;
    out Result: Boolean) of object;

type
  TCatCustomJSCall = record
   Code: string;
   URL: string;
   StartLine: integer;
   Silent: Boolean;
  end;

type
  TCatRequestHeaders = record
    StatusCode: string;
    SentHead: string;
    RcvdHead: string;
  end;

type
  TCatChromiumRequest = record
    Method: string;
    url: string;
    PostData: string;
    Headers: string;
    IgnoreCache: Boolean;
    UseCachedCredentials: Boolean;
    UseCookies: Boolean;
    Details: string;
  end;

type
  TSpecialCEFReq = class(TCefUrlRequestClientOwn)
  private
    fr: ISuperObject;
    fCriticalSection: TCriticalSection;
    fLogged: Boolean;
    fResponseStream: TMemoryStream;
    function CEF_GetPostData(request: ICefRequest): string;
    function CEF_GetSentHeader(request: ICefRequest;
      IncludePostData: Boolean = true): string;
    function CEF_GetRcvdHeader(Response: ICefResponse): string;
  protected
    procedure OnRequestComplete(const request: ICefUrlRequest); override;
    procedure OnDownloadData(const request: ICefUrlRequest;
{$IFDEF USEWACEF}const
{$ENDIF} data: Pointer; dataLength: NativeUInt); override;
  public
    MsgHandle: HWND;
    Details: string;
    constructor Create; override;
    destructor Destroy; override;
  end;

type
  TCatDevTools = class(TCustomControl)
  private
{$IFNDEF USEWACEF}
    fDevTools: TChromiumDevTools;
{$ENDIF}
    fTitlePanel: TBarTitlePanel;
    fSplitter: TSplitter;
    procedure CloseBtnClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure View(browser: ICefBrowser);
    property Splitter: TSplitter read fSplitter write fSplitter;
  end;

const
  cURL_ABOUTBLANK = 'about:blank';
  cURL_HOME = 'sandcat:home';
  cOptions = 'chrome.options.';

const // Messages from the Chromium renderer to the Sandcat Tab object
  CRM_LOG_REQUEST_JSON = 1;
  CRM_RENDER_PROCESSTERMINATED = 3;
  CRM_NEWTAB = 4;
  CRM_JS_ALERT = 10;
  CRM_NEWPAGERESOURCE = 11;
  CRM_SAVECACHEDRESOURCE = 12;
  CRM_SEARCHWITHENGINE = 13;
  CRM_SEARCHWITHENGINE_INNEWTAB = 14;
  CRM_NEWTAB_INBACKGROUND = 15;
  CRM_SAVECLOUDRESOURCE = 16;
  CRM_BOOKMARKURL = 17;

const // Messages from the Sandcat tab to the Chromium renderer object
  SCTM_SET_V8_MSGHANDLE = 1;
  SCTM_V8_REGISTEREXTENSION = 2;

const // Download related
  SCD_UNKNOWN = 0;
  SCD_INPROGRESS = 1;
  SCD_COMPLETE = 2;
  SCD_CANCELED = 3;

const // Shutdown modes
  SHTD_STANDARD = 10;
  SHTD_FORCED = 11;
  SHTD_MANUAL = 12;

const // Context menu options
  CRMMENU_ID_USER_FIRST = integer(MENU_ID_USER_FIRST);
  CRMMENU_ID_OPENIMAGE = CRMMENU_ID_USER_FIRST;
  CRMMENU_ID_OPENIMAGE_INNEWTAB = CRMMENU_ID_USER_FIRST + 1;
  CRMMENU_ID_COPYIMAGEADDRESS = CRMMENU_ID_USER_FIRST + 2;
  CRMMENU_ID_SAVEIMAGEAS = CRMMENU_ID_USER_FIRST + 3;
  CRMMENU_ID_OPENLINK = CRMMENU_ID_USER_FIRST + 4;
  CRMMENU_ID_OPENLINK_INNEWTAB = CRMMENU_ID_USER_FIRST + 5;
  CRMMENU_ID_COPYADDRESS = CRMMENU_ID_USER_FIRST + 6;
  CRMMENU_ID_SEARCH = CRMMENU_ID_USER_FIRST + 7;
  CRMMENU_ID_SEARCH_INNEWTAB = CRMMENU_ID_USER_FIRST + 8;
  CRMMENU_ID_LINK_COPYADDRESS = CRMMENU_ID_USER_FIRST + 9;
  CRMMENU_ID_OPENLINK_INBGTAB = CRMMENU_ID_USER_FIRST + 10;
  CRMMENU_ID_FRAMEMENU = CRMMENU_ID_USER_FIRST + 11;
  CRMMENU_ID_FRAMEMENU_OPEN = CRMMENU_ID_USER_FIRST + 12;
  CRMMENU_ID_FRAMEMENU_OPEN_INNEWTAB = CRMMENU_ID_USER_FIRST + 13;
  CRMMENU_ID_FRAMEMENU_OPEN_INBGTAB = CRMMENU_ID_USER_FIRST + 14;
  CRMMENU_ID_FRAMEMENU_COPYADDRESS = CRMMENU_ID_USER_FIRST + 15;
  CRMMENU_ID_PAGE_SAVEAS = CRMMENU_ID_USER_FIRST + 16;
  CRMMENU_ID_LINK_SAVEAS = CRMMENU_ID_USER_FIRST + 17;
  CRMMENU_ID_PAGE_BOOKMARK = CRMMENU_ID_USER_FIRST + 18;
  CRMMENU_ID_LINK_BOOKMARK = CRMMENU_ID_USER_FIRST + 19;

function CertErrorCodeToErrorName(c: TCefErrorCode): string;
function CreateCEFPOSTField(const str: String): ICefPostDataElement;
function GetCEFUserAgent: string;
function GetCEFDefaults(settings: TCatJSON): string;
function CEFStateToBool(s: TCefState): Boolean;
function CEFV8ValueToStr(v: ICefv8Value): string;
function DownloadStateToStr(i: integer): string;
function StrToCefString(s: ustring): TCefString;
function StrToCEFV8Value(const s: string): ICefv8Value;
function BuildRequest(Method, url: string; PostData: string = '')
  : TCatChromiumRequest;
function CatCEFLoadLib: Boolean;
procedure CatCEFShutdown(mode: integer);
function SaveResponseToFile(s: string): string;

implementation

uses CatTasks, CatStrings, CatHTTP, CatTime, CatMsgCromis;

var
  TempFileCount: integer = 0;

function CatCEFLoadLib: Boolean;
begin
{$IFDEF USEWACEF}
  TWACef.Initialize;
{$ENDIF}
  Result := {$IFDEF USEWACEF}TWACef.LoadLib{$ELSE}CefLoadLibDefault{$ENDIF};
end;

function BuildRequest(Method, url: string; PostData: string = '')
  : TCatChromiumRequest;
begin
  Result.Method := Method;
  Result.url := url;
  Result.PostData := PostData;
  Result.Headers := emptystr;
  Result.IgnoreCache := true;
  Result.UseCookies := true;
  Result.UseCachedCredentials := true;
  Result.Details := emptystr;
end;

procedure CatCEFShutdown(mode: integer);
begin
  case mode of
    SHTD_STANDARD:
      ; // do nothing
    SHTD_FORCED:
      KillProcessbyPID(GetCurrentProcessId);
    SHTD_MANUAL:
      begin
{$IFDEF USEWACEF}
        cef_shutdown;
{$ELSE}
        ceflib.CefShutDown;
{$ENDIF}
        ExitProcess(0);
      end;
  end;
end;

function CertErrorCodeToErrorName(c: TCefErrorCode): string;
begin
  case c of
    ERR_CERT_COMMON_NAME_INVALID:
      Result := 'Certificate common name invalid.';
    ERR_CERT_DATE_INVALID:
      Result := 'Certificate date invalid.';
    ERR_CERT_AUTHORITY_INVALID:
      Result := 'Certificate authority invalid.';
    ERR_CERT_CONTAINS_ERRORS:
      Result := 'Certificate contains errors.';
    ERR_CERT_NO_REVOCATION_MECHANISM:
      Result := 'No certificate revocation mechanism.';
    ERR_CERT_UNABLE_TO_CHECK_REVOCATION:
      Result := 'Unable to check certificate revocation.';
    ERR_CERT_REVOKED:
      Result := 'Certificate revoked.';
    ERR_CERT_INVALID:
      Result := 'Invalid certificate.';
    ERR_CERT_END:
      Result := 'Certificate end.';
  end;
end;

function CreateCEFPOSTField(const str: String): ICefPostDataElement;
begin
  Result := TCefPostDataElementRef.New;
  Result.SetToBytes(Length(AnsiString(str)), PAnsiChar(AnsiString(str)));
end;

function StrToCefString(s: ustring): TCefString;
begin
  Result := {$IFDEF USEWACEF}TWACef.ToCefString{$ELSE}CefString{$ENDIF}(s);
end;

function StrToCEFV8Value(const s: string): ICefv8Value;
begin
  Result := TCefv8ValueRef.{$IFDEF USEWACEF}CreateString{$ELSE}NewString{$ENDIF}(s);
end;

function CEFV8ValueToStr(v: ICefv8Value): string;
begin
  Result := emptystr;
  if v.IsString then
    Result := v.GetStringValue
  else
  begin
    if v.IsUndefined then
      Result := 'undefined'
    else if v.IsNull then
      Result := 'null'
    else if v.IsBool then
    begin
      if v.GetBoolValue = true then
        Result := 'true'
      else
        Result := 'false';
    end
    else if v.IsInt then
      Result := inttostr(v.GetIntValue)
    else if v.IsUInt then
      Result := inttostr(v.GetUIntValue)
    else if v.IsDouble then
      Result := floattostr(v.GetDoubleValue)
      // else if v.IsDate then
      // Result := datetimetostr(v.GetDateValue)
    else if v.IsObject then
      Result := '[object]'
    else if v.IsArray then
      Result := '[array]'
    else if v.IsFunction then
      Result := '[function ' + v.GetFunctionName + ']';
  end;
end;

function CEFStateToStr(s: TCefState): string;
begin
  Result := emptystr;
  case s of
    STATE_ENABLED:
      Result := 'Enabled';
    STATE_DISABLED:
      Result := 'Disabled';
    STATE_DEFAULT:
      Result := 'Default';
  end;
end;

function CEFStateToBool(s: TCefState): Boolean;
begin
  Result := true;
  case s of
    STATE_ENABLED:
      Result := true;
    STATE_DISABLED:
      Result := false;
    STATE_DEFAULT:
      Result := true; // currently all Chromium options are true by default
  end;
end;

function DownloadStateToStr(i: integer): string;
begin
  case i of
    SCD_INPROGRESS:
      Result := 'inprogress';
    SCD_CANCELED:
      Result := 'canceled';
    SCD_COMPLETE:
      Result := 'complete';
  end;
end;

function GetCEFDefaults(settings: TCatJSON): string;
var
  sl: TStringList;
  Count, Size, i: integer;
  List: PPropList;
  PropInfo: PPropInfo;
  CID: string;
  opt: TChromiumOptions;
begin
  sl := TStringList.Create;
  opt := TChromiumOptions.Create;
  Count := GetPropList(opt.ClassInfo, tkAny, nil);
  Size := Count * SizeOf(Pointer);
  GetMem(List, Size);
  try
    Count := GetPropList(opt.ClassInfo, tkAny, List);
    for i := 0 to Count - 1 do
    begin
      PropInfo := List^[i];
      if PropInfo^.PropType^.Name = 'TCefState' then
      begin
        CID := cOptions + lowercase(string(PropInfo^.Name));
        // currently all Chromium options are true by default
        settings[CID] := true;
        sl.Add(CID);
      end;
    end;
  finally
    FreeMem(List);
  end;
  opt.Free;
  Result := sl.text;
  sl.Free;
end;

function GetCEFCacheDir: string;
begin
{$IFDEF USEWACEF}
  Result := CefCachePath;
{$ELSE}
  Result := CefCache;
{$ENDIF}
end;

function GetCEFUserAgent: string;
begin
  Result := CefUserAgent;
end;

function GetTempFile: string;
var
  f: string;
begin
  TempFileCount := TempFileCount + 1;
  f := inttostr(GetCurrentProcessId) + ' - ' + inttostr(DateTimeToUnix(now)) +
    '-' + inttostr(TempFileCount) + '.tmp';
  Result := GetCEFCacheDir + 'Headers\' + f;
end;

function SaveResponseToFile(s: string): string;
var
  sl: TStringList;
begin
  Result := GetTempFile;
  if GetCEFCacheDir = emptystr then
    exit;
  sl := TStringList.Create;
  sl.text := s;
  sl.SaveToFile(Result);
  sl.Free;
end;

// ------------------------------------------------------------------------//
// TSpecialCEFReq                                                          //
// ------------------------------------------------------------------------//

function TSpecialCEFReq.CEF_GetRcvdHeader(Response: ICefResponse): string;
var
  i: integer;
  s, kv, lastkv: string;
  Map: ICefStringMultimap;
  procedure Add(key, value: string);
  begin
    kv := key + ': ' + value;
    if kv <> lastkv then
      s := s + crlf + kv; // Workaround: CEF3 sometimes returns repeated headers
    lastkv := kv;
  end;

begin
  Map := TCefStringMultiMapOwn.Create;
  Response.GetHeaderMap(Map);
  s := 'HTTP/1.1 ' + inttostr(Response.getStatus) + ' ' +
    Response.GetStatusText;
  with Map do
  begin
    for i := 0 to GetSize do
    begin
      if i < GetSize then
        Add(GetKey(i), GetValue(i));
    end;
  end;
  Result := s;
end;

function TSpecialCEFReq.CEF_GetSentHeader(request: ICefRequest;
  IncludePostData: Boolean = true): string;
var
  i: integer;
  s, PostData, kv, lastkv: string;
  Map: ICefStringMultimap;
  procedure Add(key, value: string);
  begin
    kv := key + ': ' + value;
    if kv <> lastkv then
      s := s + crlf + kv; // Workaround: CEF3 sometimes returns repeated headers
    lastkv := kv;
  end;

begin
  Map := TCefStringMultiMapOwn.Create;
  request.GetHeaderMap(Map);
  s := request.getMethod + ' /' + ExtractUrlPath(request.GetURL) + ' HTTP/1.1';
  with Map do
  begin
    if FindCount('Host') = 0 then
      Add('Host', ExtractURLHost(request.GetURL));
    for i := 0 to GetSize do
    begin
      if i < GetSize then
        Add(GetKey(i), GetValue(i));
    end;
  end;
  if (IncludePostData) and (request.getMethod = 'POST') then
  begin
    PostData := CEF_GetPostData(request);
    if PostData <> emptystr then
    begin
      s := s + crlf;
      s := s + crlf + PostData;
    end;
  end;
  Result := s + crlf;
end;

function TSpecialCEFReq.CEF_GetPostData(request: ICefRequest): string;
var
  i: integer;
  ansi, datastr: AnsiString;
  postElement: ICefPostDataElement;
  PostData: ICefPostData;
  List: IInterfaceList;
  elcount: NativeUInt;
begin
  ansi := '';
  PostData := request.getPostData;
  if PostData <> nil then
  begin
    elcount := PostData.{$IFDEF USEWACEF}GetElementCount{$ELSE}GetCount{$ENDIF};
{$IFDEF USEWACEF}
    // FIXME: WACEF branch 2357 is crashing when trying to get POST elements
    try
      List := PostData.GetElements(elcount);
    except
    end;
{$ELSE}
    List := PostData.GetElements(elcount);
{$ENDIF}
    for i := 0 to List.Count - 1 do
    begin
      postElement := List[i] as ICefPostDataElement;
      case postElement.GetType of
        PDE_TYPE_BYTES: // ToDo: handle PDE_TYPE_FILE and PDE_TYPE_EMPTY
          begin
            SetLength(datastr, postElement.GetBytesCount);
            postElement.GetBytes(postElement.GetBytesCount, PAnsiChar(datastr));
            ansi := ansi + datastr;
          end;
      end;
    end;
  end;
  Result := string(ansi);
end;

constructor TSpecialCEFReq.Create;
begin
  inherited Create;
  fCriticalSection := TCriticalSection.Create;
  fCriticalSection.Enter;
  fResponseStream := TMemoryStream.Create;
  fLogged := false;
end;

destructor TSpecialCEFReq.Destroy;
begin
  fResponseStream.Free;
  fCriticalSection.Free;
  inherited;
end;

procedure TSpecialCEFReq.OnDownloadData(const request: ICefUrlRequest;
{$IFDEF USEWACEF}const {$ENDIF} data: Pointer; dataLength: NativeUInt);
begin
{$IFDEF DXE3_OR_UP}
  fResponseStream.WriteData(data, dataLength);
{$ELSE}
  fResponseStream.Write(data, dataLength);
{$ENDIF}
  inherited;
end;

procedure TSpecialCEFReq.OnRequestComplete(const request: ICefUrlRequest);
var
  req: ICefRequest;
  resp: ICefResponse;
var
  SentHead, RcvdHead, referrer, respfilename: string;
begin
  inherited;
  fCriticalSection.Enter;
  try
    fr := TSuperObject.Create(stObject);
    req := request.getrequest;
    resp := request.getresponse;
    SentHead := CEF_GetSentHeader(req);
    RcvdHead := CEF_GetRcvdHeader(resp);
    fr.s['method'] := req.getMethod;
    fr.s['url'] := req.GetURL;
    if pos('Referer', SentHead) <> 0 then
      referrer := trim(getfield('Referer', SentHead));
    if req.getMethod = 'POST' then
      fr.s['postdata'] := CEF_GetPostData(req)
    else
      fr.s['postdata'] := emptystr;
    fr.s['status'] := inttostr(resp.getStatus);
    fr.s['mimetype'] := resp.GetMimeType;
    if Details <> emptystr then // user specified
      fr.s['details'] := Details
    else
    begin
      if lowercase(extracturlfileext(referrer)) = '.swf' then
        fr.s['details'] := 'Flash Plugin Request'
      else
        fr.s['details'] := 'Browser Request';
    end;
    fr.s['reqid'] := emptystr;
    fr.s['response'] := emptystr;
    respfilename := GetTempFile;
    fr.s['responsefilename'] := respfilename;
    fResponseStream.SaveToFile(respfilename);
    fr.s['length'] := inttostr(fResponseStream.Size);
    fr.b['isredir'] := false;
    fr.b['islow'] := false;
    fr.s['headers'] := SentHead;
    fr.s['responseheaders'] := RcvdHead;
    if MsgHandle <> 0 then
      SendCromisMessage(MsgHandle, CRM_LOG_REQUEST_JSON, fr.AsJson(true));
    fr := nil;
  finally
    fCriticalSection.Leave;
  end;
end;

// ------------------------------------------------------------------------//
// TCatDevTools                                                            //
// ------------------------------------------------------------------------//

procedure TCatDevTools.View(browser: ICefBrowser);
begin
{$IFNDEF USEWACEF}
  if fDevTools = nil then
  begin
    fDevTools := TChromiumDevTools.Create(self);
    fDevTools.Parent := self;
    fDevTools.Align := AlClient;
  end;
  fDevTools.ShowDevTools(browser);
{$ENDIF}
end;

procedure TCatDevTools.CloseBtnClick(Sender: TObject);
begin
  height := 0;
  if fSplitter <> nil then
    fSplitter.Visible := false;
end;

constructor TCatDevTools.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csAcceptsControls];
  Color := clWindow;
  fTitlePanel := TBarTitlePanel.Create(self);
  fTitlePanel.Parent := self;
  fTitlePanel.Align := alTop;
  fTitlePanel.Caption := 'DevTools';
  fTitlePanel.CloseButton.OnClick := CloseBtnClick;
end;

destructor TCatDevTools.Destroy;
begin
{$IFNDEF USEWACEF}
  if fDevTools <> nil then
    fDevTools.Free;
{$ENDIF}
  fTitlePanel.Free;
  inherited Destroy;
end;

initialization

CefRemoteDebuggingPort := 8000;

end.
