unit CatChromium;

{
  Catarinka Browser Component
  Copyright (c) 2011-2015 Syhunt Informatica
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details
}

interface

{$I Catarinka.inc}

uses
{$IFDEF DXE2_OR_UP}
  System.Classes, Winapi.Windows, Winapi.Messages, Vcl.Controls, Vcl.Graphics,
  Vcl.Forms, System.SysUtils, System.SyncObjs, Vcl.Dialogs, Vcl.Clipbrd,
  System.TypInfo,
{$ELSE}
  Classes, Windows, Messages, Controls, Graphics, Forms, SysUtils, SyncObjs,
  Dialogs, Clipbrd, TypInfo,
{$ENDIF}
{$IFDEF USEWACEF}
  WACefComponent, WACefInterfaces, WACefTypes, WACefOwns, WACefCExports,
  WACefLib, WACefRefs,
{$ELSE}
  cefgui, cefvcl, ceflib,
{$ENDIF}
  superobject, CatJSON, CatMsg;

{$IFDEF USEWACEF}

type
  TChromium = TWAChromium;
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
  TCatChromiumOnLoadError = procedure(Sender: TObject; const errorCode:
{$IFDEF USEWACEF}TCefErrorCode{$ELSE}integer{$ENDIF};
    const errorText, failedUrl: string) of object;
  TCatChromiumOnCertificateError = procedure(Sender: TObject;
    aCertError: TCefErrorCode; const aRequestUrl: ustring;
    const aSslInfo: ICefSslinfo; const aCallback: ICefRequestCallback;
    out Result: Boolean) of object;

type
  TCatSourceVisitorOwn = class(TCefStringVisitorOwn)
  private
    fCriticalSection: TCriticalSection;
  protected
    procedure Visit(const str: ustring); override;
  public
    Browser: TCustomControl;
    constructor Create; override;
    destructor Destroy; override;
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
  TCatChromium = class(TCustomControl)
  private
{$IFNDEF USEWACEF}
    fDevTools: TChromiumDevTools;
{$ENDIF}
    fAdjustSourceDisplayMethod: Boolean;
    fAutoGetSource: Boolean;
    fCriticalSection: TCriticalSection;
    fCrm: TChromium;
    fEnableDownloads: Boolean;
    fHeaders: TCatRequestHeaders;
    fInterceptRequests: Boolean;
    fLastStatusCode: integer;
    fLastTitle: string;
    fLogJavaScriptErrors: Boolean;
    fLogURLs: Boolean;
    fMsg: TCatMsg;
    fNeedRecreate: Boolean;
    fOnBrowserMessage: TCatChromiumOnBrowserMessage;
    fOnAfterSetSource: TCatChromiumOnAfterSetSource;
    fOnTitleChange: TCatChromiumOnTitleChange;
    fOnLoadEnd: TCatChromiumOnLoadEnd;
    fOnLoadStart: TCatChromiumOnLoadStart;
    fOnAddressChange: TCatChromiumOnAddressChange;
    fOnStatusMessage: TCatChromiumOnStatusMessage;
    // fOnRequestComplete:TCatChromiumOnRequestComplete;
    fOnBeforePopup: TCatChromiumOnBeforePopup;
    fOnConsoleMessage: TCatChromiumOnConsoleMessage;
    fOnBeforeResourceLoad: TCatChromiumOnBeforeResourceLoad;
    fOnBeforeDownload: TCatChromiumOnBeforeDownload;
    fOnCertificateError: TCatChromiumOnCertificateError;
    fOnDownloadUpdated: TCatChromiumOnDownloadUpdated;
    fOnLoadingStateChange: TCatChromiumOnLoadingStateChange;
    fOnLoadError: TCatChromiumOnLoadError;
    fPreventPopup: Boolean;
    fResourceList: TStringList;
    fSentRequests: integer;
    fSource: string;
    fSourceVisitor: TCatSourceVisitorOwn;
    fURLLog: TStringList;
    procedure ClearRequestData;
    procedure crmTitleChange(Sender: TObject; const Browser: ICefBrowser;
      const title: ustring);
    procedure crmLoadEnd(Sender: TObject; const Browser: ICefBrowser;
      const frame: ICefFrame; httpStatusCode: integer);
    procedure crmLoadStart(Sender: TObject; const Browser: ICefBrowser;
      const frame: ICefFrame);
    procedure crmAddressChange(Sender: TObject; const Browser: ICefBrowser;
      const frame: ICefFrame; const url: ustring);
    procedure crmStatusMessage(Sender: TObject; const Browser: ICefBrowser;
      const value: ustring);
    procedure crmBeforeContextMenu(Sender: TObject; const Browser: ICefBrowser;
      const frame: ICefFrame; const params: ICefContextMenuParams;
      const model: ICefMenuModel);
    procedure crmContextMenuCommand(Sender: TObject; const Browser: ICefBrowser;
      const frame: ICefFrame; const params: ICefContextMenuParams;
      commandId: integer; eventFlags: TCefEventFlags; out Result: Boolean);
    procedure crmConsoleMessage(Sender: TObject; const Browser: ICefBrowser;
      const message, source: ustring; line: integer; out Result: Boolean);
    procedure crmProcessMessageReceived(Sender: TObject;
      const Browser: ICefBrowser; sourceProcess: TCefProcessId;
      const message: ICefProcessMessage; out Result: Boolean);
    procedure crmBeforeDownload(Sender: TObject; const Browser: ICefBrowser;
      const downloadItem: ICefDownloadItem; const suggestedName: ustring;
      const Callback: ICefBeforeDownloadCallback);
    procedure crmDownloadUpdated(Sender: TObject; const Browser: ICefBrowser;
      const downloadItem: ICefDownloadItem;
      const Callback: ICefDownloadItemCallback);
    procedure crmGetResourceHandler(Sender: TObject; const Browser: ICefBrowser;
      const frame: ICefFrame; const request: ICefRequest;
      out Result: ICefResourceHandler);
    procedure crmLoadError(Sender: TObject; const Browser: ICefBrowser;
      const frame: ICefFrame; errorCode:
{$IFDEF USEWACEF}TCefErrorCode{$ELSE}integer{$ENDIF};
      const errorText, failedUrl: ustring);
    procedure crmLoadingStateChange(Sender: TObject; const Browser: ICefBrowser;
      isLoading, canGoBack, canGoForward: Boolean);
    procedure crmPluginCrashed(Sender: TObject; const Browser: ICefBrowser;
      const pluginPath: ustring);
    procedure crmGetAuthCredentials(Sender: TObject; const Browser: ICefBrowser;
      const frame: ICefFrame; isProxy: Boolean; const host: ustring;
      port: integer; const realm, scheme: ustring;
      const Callback: ICefAuthCallback; out Result: Boolean);
    procedure crmRenderProcessTerminated(Sender: TObject;
      const Browser: ICefBrowser; status: TCefTerminationStatus);
    procedure LogURL(const url: string);
    procedure SendMessageToTab(const id: integer; const s: string);
    procedure SetOptionState(settings, DefaultSettings: TCatJSON;
      const curstate: TCefState; const propname: string);
    procedure SetZoomLevel(const zl: double);
    function GetZoomLevel: double;
    function GetURLShort: string;
    procedure ReCreateBrowser(const aURL: string);
    procedure StopLoadBlank;
    procedure WMCopyData(const msgid: integer; const str: string);
{$IFDEF USEWACEF}
    procedure crmJsdialog(Sender: TObject; const aBrowser: ICefBrowser;
      const aOriginUrl: ustring; const aAcceptLang: ustring;
      aDialogType: TCefJsdialogType; const aMessageText: ustring;
      const aDefaultPromptText: ustring; const aCallback: ICefJsdialogCallback;
      var aSuppressMessage: Boolean; out Result: Boolean);
{$ELSE}
    procedure crmJsdialog(Sender: TObject; const aBrowser: ICefBrowser;
      const aOriginUrl, aAcceptLang: ustring; aDialogType: TCefJsdialogType;
      const aMessageText, aDefaultPromptText: ustring;
      aCallback: ICefJsdialogCallback; out aSuppressMessage: Boolean;
      out Result: Boolean);
{$ENDIF}
    procedure crmBeforeResourceLoad(Sender: TObject; const Browser: ICefBrowser;
      const frame: ICefFrame; const request: ICefRequest;
      const Callback: ICefRequestCallback; out Result: TCefReturnValue);
    procedure crmBeforePopup(Sender: TObject; const aBrowser: ICefBrowser;
      const aFrame: ICefFrame;
{$IFDEF USEWACEF}
      var aTargetUrl: ustring; const aTargetFrameName: ustring;
{$ELSE}
      const aTargetUrl, aTargetFrameName: ustring;
{$ENDIF}
      aTargetDisposition: TCefWindowOpenDisposition; aUserGesture: Boolean;
      var aPopupFeatures: TCefPopupFeatures; var aWindowInfo: TCefWindowInfo;
      var aClient: ICefClient; var aSettings: TCefBrowserSettings;
      var aNoJavascriptAccess: Boolean; out Result: Boolean);
    procedure crmCertificateError(Sender: TObject; const aBrowser: ICefBrowser;
      aCertError: TCefErrorCode; const aRequestUrl: ustring;
      const aSslInfo: ICefSslinfo; const aCallback: ICefRequestCallback;
      out Result: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function EvalJavaScript(const Script: string): variant;
    function GetURL: string;
    function IsMain(const b: ICefBrowser; const f: ICefFrame = nil): Boolean;
    function IsFrameNil: Boolean;
    procedure AddToResourceList(const url: string);
    procedure GoBack;
    procedure GoForward;
    procedure Load(const url: string);
    procedure LoadBlank(const WaitLoad: Boolean = false);
    procedure LoadFromString(const s, url: string);
    procedure LoadSettings(settings, DefaultSettings: TCatJSON);
    procedure RunJavaScript(const Script: string); overload;
    procedure RunJavaScript(const Script: string; const ScriptURL: string;
      const StartLine: integer; const ReportErrors: Boolean = false); overload;
    procedure RegisterNewV8Extension(const v8js: string);
    procedure Reload(const IgnoreCache: Boolean = false);
    procedure SendMessage(const msg: integer; const msgstr: string);
    procedure SendRequest(const req: TCatChromiumRequest;
      const Load: Boolean = false);
    procedure SetV8MsgHandle(const handle: integer);
    procedure Stop(const waitstop: Boolean = false);
    procedure GetSource; // callback
    procedure GetSourceAsText; // callback
    procedure SetSource(const s: string);
    function isLoading: Boolean;
    procedure ShowAuthDialog(const Username: string = '';
      const Password: string = '');
    procedure ViewDevTools;
    procedure ViewSourceExternalEditor;
    // properties
    property AdjustSourceDisplayMethod: Boolean read fAdjustSourceDisplayMethod
      write fAdjustSourceDisplayMethod;
    property Crm: TChromium read fCrm;
    property EnableDownloads: Boolean read fEnableDownloads
      write fEnableDownloads;
    property Headers: TCatRequestHeaders read fHeaders;
    property InterceptRequests: Boolean read fInterceptRequests
      write fInterceptRequests;
    property LogURLs: Boolean read fLogURLs write fLogURLs;
    property LogJavaScriptErrors: Boolean read fLogJavaScriptErrors
      write fLogJavaScriptErrors;
    property ResourceList: TStringList read fResourceList;
    property title: string read fLastTitle;
    property URLLog: TStringList read fURLLog;
    property URLShort: string read GetURLShort;
    property ZoomLevel: double read GetZoomLevel write SetZoomLevel;
  published
    property OnAfterSetSource: TCatChromiumOnAfterSetSource
      read fOnAfterSetSource write fOnAfterSetSource;
    property OnBrowserMessage: TCatChromiumOnBrowserMessage
      read fOnBrowserMessage write fOnBrowserMessage;
    property OnCertificateError: TCatChromiumOnCertificateError
      read fOnCertificateError write fOnCertificateError;
    property OnLoadEnd: TCatChromiumOnLoadEnd read fOnLoadEnd write fOnLoadEnd;
    property OnLoadStart: TCatChromiumOnLoadStart read fOnLoadStart
      write fOnLoadStart;
    property OnTitleChange: TCatChromiumOnTitleChange read fOnTitleChange
      write fOnTitleChange;
    property OnAddressChange: TCatChromiumOnAddressChange read fOnAddressChange
      write fOnAddressChange;
    property OnStatusMessage: TCatChromiumOnStatusMessage read fOnStatusMessage
      write fOnStatusMessage;
    // property OnRequestComplete:TCatChromiumOnRequestComplete read FOnRequestComplete write FOnRequestComplete;
    property OnBeforePopup: TCatChromiumOnBeforePopup read fOnBeforePopup
      write fOnBeforePopup;
    property OnConsoleMessage: TCatChromiumOnConsoleMessage
      read fOnConsoleMessage write fOnConsoleMessage;
    property OnBeforeResourceLoad: TCatChromiumOnBeforeResourceLoad
      read fOnBeforeResourceLoad write fOnBeforeResourceLoad;
    property OnBeforeDownload: TCatChromiumOnBeforeDownload
      read fOnBeforeDownload write fOnBeforeDownload;
    property OnDownloadUpdated: TCatChromiumOnDownloadUpdated
      read fOnDownloadUpdated write fOnDownloadUpdated;
    property OnLoadingStateChange: TCatChromiumOnLoadingStateChange
      read fOnLoadingStateChange write fOnLoadingStateChange;
    property OnLoadError: TCatChromiumOnLoadError read fOnLoadError
      write fOnLoadError;
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

function GetCEFUserAgent: string;
function GetCEFDefaults(settings: TCatJSON): string;
function CEFV8ValueToStr(v: ICefv8Value): string;
function StrToCEFV8Value(const s: string): ICefv8Value;
function BuildRequest(Method, url: string; PostData: string = '')
  : TCatChromiumRequest;
procedure CatCEFShutdown(mode: integer);
function SaveResponseToFile(s: string): string;

implementation

uses uAuthentication, CatJINI, CatStringLoop, CatTasks, CatUI, CatFiles,
  CatStrings, CatHTTP, CatUtils, CatTime, CatPointer;

var
  TempFileCount: integer = 0;

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

function GetCEFDefaults(settings: TCatJSON): string;
var
  sl: TStringList;
  Count, Size, I: integer;
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
    for I := 0 to Count - 1 do
    begin
      PropInfo := List^[I];
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
// TCatChromium                                                            //
// ------------------------------------------------------------------------//

procedure TCatChromium.SendMessage(const msg: integer; const msgstr: string);
var
  m: ICefProcessMessage;
begin
  if fCrm.Browser = nil then
    exit;
  m := TCefProcessMessageRef.New('msg');
  m.getArgumentList.SetInt(0, msg);
  m.getArgumentList.SetString(1, msgstr);
  fCrm.Browser.SendProcessMessage(PID_RENDERER, m);
end;

procedure TCatChromium.RegisterNewV8Extension(const v8js: string);
var
  m: ICefProcessMessage;
begin
  if fCrm.Browser = nil then
    exit;
  m := TCefProcessMessageRef.New('msg');
  m.getArgumentList.SetInt(0, SCTM_V8_REGISTEREXTENSION);
  m.getArgumentList.SetString(1, v8js);
  fCrm.Browser.SendProcessMessage(PID_RENDERER, m);
end;

procedure TCatChromium.SetV8MsgHandle(const handle: integer);
var
  m: ICefProcessMessage;
begin
  if fCrm.Browser = nil then
    exit;
  m := TCefProcessMessageRef.New('msg');
  m.getArgumentList.SetInt(0, SCTM_SET_V8_MSGHANDLE);
  m.getArgumentList.SetInt(1, handle);
  fCrm.Browser.SendProcessMessage(PID_RENDERER, m);
end;

procedure TCatChromium.crmProcessMessageReceived(Sender: TObject;
  const Browser: ICefBrowser; sourceProcess: TCefProcessId;
  const message: ICefProcessMessage; out Result: Boolean);
begin
  // test code
  // if sourceprocess <> PID_RENDERER then exit;
  { if message.getName = 'msg' then begin
    showmessage(message.getName);
    Result := True;
    end else result:=false; }
end;

function TCatChromium.IsMain(const b: ICefBrowser; const f: ICefFrame): Boolean;
begin
  Result := (b <> nil) and (b.GetIdentifier = fCrm.BrowserId) and
    ((f = nil) or (f.IsMain));
end;

function TCatChromium.GetZoomLevel: double;
begin
  Result := 0;
  if fCrm.Browser = nil then
    exit;
  Result := fCrm.Browser.GetHost.GetZoomLevel;
end;

procedure TCatChromium.SetZoomLevel(const zl: double);
begin
  if fCrm.Browser = nil then
    exit;
  fCrm.Browser.GetHost.SetZoomLevel(zl);
end;

procedure TCatChromium.GoBack;
begin
  if fCrm.Browser <> nil then
    fCrm.Browser.GoBack;
end;

procedure TCatChromium.GoForward;
begin
  if fCrm.Browser <> nil then
    fCrm.Browser.GoForward;
end;

procedure TCatChromium.SetSource(const s: string);
begin
  fSource := s;
  if assigned(OnAfterSetSource) then
    OnAfterSetSource(s);
end;

function TCatChromium.IsFrameNil: Boolean;
begin
  Result := false;
  if fCrm.Browser = nil then
    Result := true;
  if fCrm.Browser = nil then
    exit;
  if fCrm.Browser.GetMainFrame = nil then
    Result := true;
end;

procedure TCatChromium.GetSourceAsText;
begin
  if IsFrameNil then
    exit;
  fSourceVisitor := TCatSourceVisitorOwn.Create;
  fSourceVisitor.Browser := self;
  fCrm.Browser.GetMainFrame.GetText(fSourceVisitor)
end;

procedure TCatChromium.GetSource;
var
  ext: string;
  showtext: Boolean;
begin
  if IsFrameNil then
    exit;
  ext := lowercase(extracturlfileext(GetURL));
  showtext := false;
  if fAdjustSourceDisplayMethod then
  begin
    if ext = '.js' then
      showtext := true;
    if ext = '.css' then
      showtext := true;
    if ext = '.xml' then
      showtext := true;
  end;
  fSourceVisitor := TCatSourceVisitorOwn.Create;
  fSourceVisitor.Browser := self;
  if showtext then
    fCrm.Browser.GetMainFrame.GetText(fSourceVisitor)
  else
    fCrm.Browser.GetMainFrame.GetSource(fSourceVisitor);
  // There is no need to free the source visitor own according to the DCEF author
  // fsourcevisitor.free; // causes AV
end;

function TCatChromium.GetURL: string;
begin
  if fCrm.Browser = nil then
    exit;
  if fCrm.Browser.GetMainFrame <> nil then
    Result := fCrm.Browser.GetMainFrame.GetURL;
end;

function TCatChromium.GetURLShort: string;
var
  u: string;
begin
  u := GetURL;
  if u = cURL_HOME then
  begin
    Result := emptystr;
  end
  else
  begin
    u := CatHTTP.ExtractURLHost(u);
    if beginswith(u, 'www.') then
    begin
      u := after(u, '.');
      u := before(u, '.');
    end
    else
    begin
      u := before(u, '.');
    end;
    Result := u;
  end;
end;

function TCatChromium.EvalJavaScript(const Script: string): variant;
var
  ret: ICefv8Value;
  expt: ICefV8Exception;
  ctx: ICefv8Context;
begin
  // test with console.write('eval:'..tab:evaljs('"v8" + " rocks" '))
  if IsFrameNil then
    exit;
  ctx := fCrm.Browser.GetMainFrame.GetV8Context;
  // if ctx = nil then showmessage('nil');
  if ctx <> nil then
  begin
    ctx.Enter;
    try
      if ctx.Eval(Script, ret, expt) then
        Result := ret.GetStringValue
      else
        Result := expt.message;
    finally
      ctx.exit;
      ctx := nil;
    end;
  end;
end;

procedure TCatChromium.RunJavaScript(const Script: string);
begin
  RunJavaScript(Script, emptystr, 0, false);
end;

procedure TCatChromium.RunJavaScript(const Script: string;
  const ScriptURL: string; const StartLine: integer;
  const ReportErrors: Boolean = false);
begin
  // CEF will not execute the JS if no URL is loaded,
  // so we load a blank URL before
  if ReportErrors then
    fLogJavaScriptErrors := true;
  if GetURL = emptystr then
    LoadBlank;
  if fCrm.Browser = nil then
    exit;
  if fCrm.Browser.GetMainFrame = nil then
    exit;
  fCrm.Browser.GetMainFrame.ExecuteJavaScript(Script, ScriptURL, StartLine);
end;

procedure TCatChromium.crmGetAuthCredentials(Sender: TObject;
  const Browser: ICefBrowser; const frame: ICefFrame; isProxy: Boolean;
  const host: ustring; port: integer; const realm, scheme: ustring;
  const Callback: ICefAuthCallback; out Result: Boolean);
var
  u, p: ustring;
  r: Boolean;
begin
  TThread.Synchronize(nil,
    procedure
    begin
      with TPasswordDlg.Create(nil) do
        try
          if ShowModal = mrOk then
          begin
            u := edtusername.text;
            p := edtPassword.text;
            r := true;
          end
          else
            r := false;
        finally
          Free;
        end
    end);

  Result := r;
  if r = true then
  begin
    Callback.Cont(u, p);
  end;
end;

procedure TCatChromium.ShowAuthDialog(const Username: string = '';
const Password: string = '');
var
  u, p: string;
  r: Boolean;
var
  req: ICefRequest;
  Map: ICefStringMultimap;
begin
  if IsFrameNil then
    exit;
  with TPasswordDlg.Create(nil) do
    try
      edtusername.text := Username;
      edtPassword.text := Password;
      if ShowModal = mrOk then
      begin
        u := edtusername.text;
        p := edtPassword.text;
        r := true;
      end
      else
        r := false;
    finally
      Free;
    end;
  if r = true then
  begin
    req := TCefRequestRef.New;
    req.url := GetURL;
    req.Method := 'GET';
    Map := TCefStringMultiMapOwn.Create;
    req.GetHeaderMap(Map);
    Map.Append('Authorization', 'Basic ' + base64encode(u + ':' + p));
    req.SetHeaderMap(Map);
    fCrm.Browser.MainFrame.LoadRequest(req);
  end;
end;

procedure TCatChromium.crmLoadEnd(Sender: TObject; const Browser: ICefBrowser;
const frame: ICefFrame; httpStatusCode: integer);
begin
  if IsMain(Browser, frame) = false then
    exit;
  fLastStatusCode := httpStatusCode;
  if assigned(OnLoadEnd) then
    OnLoadEnd(Sender, httpStatusCode);
  if fAutoGetSource then
    GetSource;
end;

procedure TCatChromium.crmLoadStart(Sender: TObject; const Browser: ICefBrowser;
const frame: ICefFrame);
begin
  if IsMain(Browser, frame) = false then
    exit;
  fResourceList.clear;
  if assigned(OnLoadStart) then
    OnLoadStart(Sender);
end;

procedure TCatChromium.crmLoadError(Sender: TObject; const Browser: ICefBrowser;
const frame: ICefFrame; errorCode:
{$IFDEF USEWACEF}TCefErrorCode{$ELSE}integer{$ENDIF};
const errorText, failedUrl: ustring);
begin
  if IsMain(Browser, frame) = false then
    exit;
  if assigned(OnLoadError) then
    OnLoadError(Sender, errorCode, errorText, failedUrl);
end;

procedure TCatChromium.crmLoadingStateChange(Sender: TObject;
const Browser: ICefBrowser; isLoading, canGoBack, canGoForward: Boolean);
begin
  if IsMain(Browser) = false then
    exit;
  if assigned(OnLoadingStateChange) then
    OnLoadingStateChange(Sender, isLoading, canGoBack, canGoForward);
end;

procedure TCatChromium.crmPluginCrashed(Sender: TObject;
const Browser: ICefBrowser; const pluginPath: ustring);
begin
  // TODO
end;

procedure TCatChromium.crmTitleChange(Sender: TObject;
const Browser: ICefBrowser; const title: ustring);
begin
  if IsMain(Browser) = false then
    exit;
  fLastTitle := title;
  if assigned(OnTitleChange) then
    OnTitleChange(Sender, title);
end;

procedure TCatChromium.crmAddressChange(Sender: TObject;
const Browser: ICefBrowser; const frame: ICefFrame; const url: ustring);
begin
  if IsMain(Browser, frame) = false then
    exit;
  if assigned(OnAddressChange) then
    OnAddressChange(Sender, url);
end;

procedure TCatChromium.crmStatusMessage(Sender: TObject;
const Browser: ICefBrowser; const value: ustring);
begin
  if assigned(OnStatusMessage) then
    OnStatusMessage(Sender, value);
end;

const
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

procedure TCatChromium.crmBeforeContextMenu(Sender: TObject;
const Browser: ICefBrowser; const frame: ICefFrame;
const params: ICefContextMenuParams; const model: ICefMenuModel);
var
  fn: string;
  addsep, canclear: Boolean;
  framemodel: ICefMenuModel;
begin
  addsep := false;
  canclear := true;
  if params.IsEditable = false then
  begin
    if not(CM_TYPEFLAG_SELECTION in params.TypeFlags) then
    begin
      model.InsertSeparatorAt(2);
      model.InsertItemAt(3, integer(MENU_ID_RELOAD), 'Reload');
      model.InsertItemAt(4, integer(MENU_ID_RELOAD_NOCACHE),
        'Reload (Ignore Cache)');
      model.InsertSeparatorAt(5);
      model.InsertItemAt(6, integer(CRMMENU_ID_PAGE_BOOKMARK), 'Bookmark Page');
      model.InsertItemAt(7, integer(CRMMENU_ID_PAGE_SAVEAS), 'Save Page As...');
      model.InsertItemAt(8, CRMMENU_ID_COPYADDRESS, 'Copy Location');
      model.InsertSeparatorAt(9);
      model.InsertItemAt(10, integer(MENU_ID_SELECT_ALL), 'Select All');
      if CM_TYPEFLAG_FRAME in params.TypeFlags then
      begin
        model.AddSeparator;
        framemodel := model.AddSubMenu(CRMMENU_ID_FRAMEMENU, 'Frame');
        framemodel.AddItem(CRMMENU_ID_FRAMEMENU_OPEN, 'Open');
        framemodel.AddItem(CRMMENU_ID_FRAMEMENU_OPEN_INNEWTAB,
          'Open in New Tab');
        framemodel.AddItem(CRMMENU_ID_FRAMEMENU_OPEN_INBGTAB,
          'Open in Background Tab');
        framemodel.AddSeparator;
        framemodel.AddItem(CRMMENU_ID_FRAMEMENU_COPYADDRESS, 'Copy Location')
      end;
    end
    else
    begin
      model.AddSeparator;
      model.AddItem(CRMMENU_ID_SEARCH, 'Search');
      model.AddItem(CRMMENU_ID_SEARCH_INNEWTAB, 'Search in New Tab');
    end;
  end;
  if CM_TYPEFLAG_LINK in params.TypeFlags then
  begin
    if canclear then
      model.clear;
    canclear := false;
    model.AddItem(CRMMENU_ID_OPENLINK, 'Open Link Location');
    model.AddItem(CRMMENU_ID_OPENLINK_INNEWTAB, 'Open Link in New Tab');
    model.AddItem(CRMMENU_ID_OPENLINK_INBGTAB, 'Open Link in Background Tab');
    model.AddSeparator;
    model.AddItem(CRMMENU_ID_LINK_BOOKMARK, 'Bookmark Link');
    model.AddItem(CRMMENU_ID_LINK_COPYADDRESS, 'Copy Link');
    model.AddItem(CRMMENU_ID_LINK_SAVEAS, 'Save Link As...');
    addsep := true;
  end;
  // CM_TYPEFLAG_FRAME
  // CM_TYPEFLAG_SELECTION
  // CM_TYPEFLAG_EDITABLE
  if CM_TYPEFLAG_MEDIA in params.TypeFlags then
  begin
    if CM_MEDIATYPE_IMAGE = params.MediaType then
    begin
      if canclear then
        model.clear;
      // canclear := false;
      if addsep then
        model.AddSeparator;
      fn := extracturlfilename(params.SourceUrl);
      if Length(fn) <= 50 then
        model.AddItem(CRMMENU_ID_OPENIMAGE, 'Open Image (' + fn + ')')
      else
        model.AddItem(CRMMENU_ID_OPENIMAGE, 'Open Image');
      model.AddItem(CRMMENU_ID_OPENIMAGE_INNEWTAB, 'Open Image in New Tab');
      model.AddSeparator;
      model.AddItem(CRMMENU_ID_COPYIMAGEADDRESS, 'Copy Image Location');
      model.AddItem(CRMMENU_ID_SAVEIMAGEAS, 'Save Image As...');
    end;
  end;
end;

procedure TCatChromium.crmContextMenuCommand(Sender: TObject;
const Browser: ICefBrowser; const frame: ICefFrame;
const params: ICefContextMenuParams; commandId: integer;
eventFlags: TCefEventFlags; out Result: Boolean);
begin
  case commandId of
    CRMMENU_ID_OPENLINK:
      Load(params.LinkUrl);
    CRMMENU_ID_OPENLINK_INNEWTAB:
      SendMessageToTab(CRM_NEWTAB, params.LinkUrl);
    CRMMENU_ID_OPENLINK_INBGTAB:
      SendMessageToTab(CRM_NEWTAB_INBACKGROUND, params.LinkUrl);
    CRMMENU_ID_OPENIMAGE:
      Load(params.SourceUrl);
    CRMMENU_ID_OPENIMAGE_INNEWTAB:
      SendMessageToTab(CRM_NEWTAB, params.SourceUrl);
    CRMMENU_ID_COPYIMAGEADDRESS:
      clipboard.AsText := params.SourceUrl;
    CRMMENU_ID_SAVEIMAGEAS:
      SendMessageToTab(CRM_SAVECACHEDRESOURCE, params.SourceUrl);
    CRMMENU_ID_COPYADDRESS:
      clipboard.AsText := GetURL;
    CRMMENU_ID_SEARCH:
      SendMessageToTab(CRM_SEARCHWITHENGINE, params.SelectionText);
    CRMMENU_ID_SEARCH_INNEWTAB:
      SendMessageToTab(CRM_SEARCHWITHENGINE_INNEWTAB, params.SelectionText);
    CRMMENU_ID_LINK_COPYADDRESS:
      clipboard.AsText := params.LinkUrl;
    CRMMENU_ID_FRAMEMENU_OPEN:
      Load(params.FrameUrl);
    CRMMENU_ID_FRAMEMENU_OPEN_INNEWTAB:
      SendMessageToTab(CRM_NEWTAB, params.FrameUrl);
    CRMMENU_ID_FRAMEMENU_OPEN_INBGTAB:
      SendMessageToTab(CRM_NEWTAB_INBACKGROUND, params.FrameUrl);
    CRMMENU_ID_FRAMEMENU_COPYADDRESS:
      clipboard.AsText := params.FrameUrl;
    CRMMENU_ID_PAGE_SAVEAS:
      SendMessageToTab(CRM_SAVECACHEDRESOURCE, GetURL);
    CRMMENU_ID_LINK_SAVEAS:
      SendMessageToTab(CRM_SAVECLOUDRESOURCE, params.LinkUrl);
    CRMMENU_ID_PAGE_BOOKMARK:
      SendMessageToTab(CRM_BOOKMARKURL, GetURL);
    CRMMENU_ID_LINK_BOOKMARK:
      SendMessageToTab(CRM_BOOKMARKURL, params.LinkUrl);
  end;
end;

procedure TCatChromium.AddToResourceList(const url: string);
  function ResourceAllowed: Boolean;
  begin
    Result := true;
    if fResourceList.Count > 2000 then
      Result := false;
    if fResourceList.IndexOf(url) <> -1 then
      Result := false;
    if url = GetURL then
      Result := false;
    if pos('?', url) <> 0 then
      Result := false; // url with params, most likely not an object
  end;

begin
  if ResourceAllowed = false then
    exit;
  fResourceList.Add(url);
  if assigned(OnBrowserMessage) then
    OnBrowserMessage(CRM_NEWPAGERESOURCE, url);
end;

procedure TCatChromium.crmGetResourceHandler(Sender: TObject;
const Browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest;
out Result: ICefResourceHandler);
var
  req: ICefUrlRequest;
  reqown: TSpecialCEFReq;
  reqctx: ICefRequestContext;
begin
  if fInterceptRequests = false then
    exit;
  if Browser = nil then
    exit;
  fSentRequests := fSentRequests + 1;
  // sendmessagetotab(fmsg.msghandle,CRM_LOGWRITELN,'getresourcehandler:'+request.getUrl);
  reqown := TSpecialCEFReq.Create;
  reqown.MsgHandle := self.fMsg.MsgHandle;
  req := TCefUrlRequestRef.New(request, reqown, reqctx) as ICefUrlRequest;
end;

procedure TCatChromium.crmBeforeResourceLoad(Sender: TObject;
const Browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest;
const Callback: ICefRequestCallback; out Result: TCefReturnValue);
begin
  // result:=false; // unnecessary
  // This would avoid a weird crash during navigation with past CEF3 releases
end;

procedure TCatChromium.SendMessageToTab(const id: integer; const s: string);
begin
  SendCDMessage(fMsg.MsgHandle, id, s);
end;

procedure TCatChromium.crmBeforePopup(Sender: TObject;
const aBrowser: ICefBrowser; const aFrame: ICefFrame;
{$IFDEF USEWACEF}
var aTargetUrl: ustring; const aTargetFrameName: ustring;
{$ELSE}
const aTargetUrl, aTargetFrameName: ustring;
{$ENDIF}
aTargetDisposition: TCefWindowOpenDisposition; aUserGesture: Boolean;
var aPopupFeatures: TCefPopupFeatures; var aWindowInfo: TCefWindowInfo;
var aClient: ICefClient; var aSettings: TCefBrowserSettings;
var aNoJavascriptAccess: Boolean; out Result: Boolean);
var
  u: string;
begin
  Result := fPreventPopup;
  u := aTargetUrl;
  SendMessageToTab(CRM_NEWTAB, u);
  // if assigned(OnBeforePopup) then onBeforePopup(sender,u,result);
end;

procedure TCatChromium.crmCertificateError(Sender: TObject;
const aBrowser: ICefBrowser; aCertError: TCefErrorCode;
const aRequestUrl: ustring; const aSslInfo: ICefSslinfo;
const aCallback: ICefRequestCallback; out Result: Boolean);
var
  button: integer;
  msg, caption: string;
begin
  if assigned(OnCertificateError) then
    OnCertificateError(Sender, aCertError, aRequestUrl, aSslInfo,
      aCallback, Result);
  msg := format('Warning: %s Proceed anyway?',
    [CertErrorCodeToErrorName(aCertError)]);
  caption := aRequestUrl;
  button := application.MessageBox(pWideChar(msg), pWideChar(caption),
    mb_YesNo + mb_DefButton1 + mb_ICONWARNING);
  case button of
    IDYes:
      aCallback.Cont(true);
    IDNo:
      aCallback.Cont(false);
  end;
end;

procedure TCatChromium.crmConsoleMessage(Sender: TObject;
const Browser: ICefBrowser; const message, source: ustring; line: integer;
out Result: Boolean);
begin
  if fLogJavaScriptErrors = false then
    exit;
  if assigned(OnConsoleMessage) then
    OnConsoleMessage(Sender, message, source, line);
end;

{$IFDEF USEWACEF}

procedure TCatChromium.crmJsdialog(Sender: TObject; const aBrowser: ICefBrowser;
const aOriginUrl: ustring; const aAcceptLang: ustring;
aDialogType: TCefJsdialogType; const aMessageText: ustring;
const aDefaultPromptText: ustring; const aCallback: ICefJsdialogCallback;
var aSuppressMessage: Boolean; out Result: Boolean);
{$ELSE}

procedure TCatChromium.crmJsdialog(Sender: TObject; const aBrowser: ICefBrowser;
const aOriginUrl, aAcceptLang: ustring; aDialogType: TCefJsdialogType;
const aMessageText, aDefaultPromptText: ustring;
aCallback: ICefJsdialogCallback; out aSuppressMessage: Boolean;
out Result: Boolean);
{$ENDIF}
begin
  case aDialogType of
    JSDIALOGTYPE_ALERT:
      begin
        if assigned(OnBrowserMessage) then
          OnBrowserMessage(CRM_JS_ALERT, aMessageText);
        // ShowMessage(MessageText);
        aSuppressMessage := true;
      end;
  end;
end;

procedure TCatChromium.ViewDevTools;
{$IFDEF USEWACEF}
var
  info: TCefWindowInfo;
  settings: TCefBrowserSettings;
{$ENDIF}
begin
  if fCrm.Browser = nil then
    exit;
  if fCrm.Browser.GetHost = nil then
    exit;
{$IFDEF USEWACEF}
  FillChar(info, SizeOf(info), 0);
  info.style := WS_OVERLAPPEDWINDOW or WS_CLIPCHILDREN or WS_CLIPSIBLINGS or
    WS_VISIBLE;
  // info.parent_window := fCrm.browser.GetHost.GetWindowHandle;
  info.x := CW_USEDEFAULT;
  info.y := CW_USEDEFAULT;
  info.width := CW_USEDEFAULT;
  info.height := CW_USEDEFAULT;
  info.window_name := StrToCefString('DevTools - ' + GetURL);
  FillChar(settings, SizeOf(TCefBrowserSettings), 0);
  settings.Size := SizeOf(TCefBrowserSettings);
  fCrm.Browser.GetHost.ShowDevTools(info, fCrm.Browser.GetHost.GetClient,
    settings);
{$ELSE}
  if fDevTools = nil then
    fDevTools := TChromiumDevTools.Create(self);
  fDevTools.Parent := self;
  fDevTools.Align := AlBottom;
  fDevTools.height := 300;
  fDevTools.ShowDevTools(fCrm.Browser);
{$ENDIF}
end;

procedure TCatChromium.ViewSourceExternalEditor;
begin
  if fCrm.Browser = nil then
    exit;
  if fCrm.Browser.GetMainFrame = nil then
    exit;
  fCrm.Browser.GetMainFrame.ViewSource;
end;

procedure TCatChromium.crmBeforeDownload(Sender: TObject;
const Browser: ICefBrowser; const downloadItem: ICefDownloadItem;
const suggestedName: ustring; const Callback: ICefBeforeDownloadCallback);
var
  s: string;
  fn: ustring;
begin
  if fEnableDownloads = false then
    exit;
  s := suggestedName;
  // debug
  // sendmessagetotab(fmsg.msghandle,CRM_LOGWRITELN,'beforedownload:'+inttostr(downloaditem.getid));
  fn := GetSpecialFolderPath(CSIDL_PERSONAL, false) + '\' + suggestedName;
  Callback.Cont(fn, true);
  if assigned(OnBeforeDownload) then
    OnBeforeDownload(Sender, downloadItem.getid, s);
end;

procedure TCatChromium.crmDownloadUpdated(Sender: TObject;
const Browser: ICefBrowser; const downloadItem: ICefDownloadItem;
const Callback: ICefDownloadItemCallback);
var
  cancel: Boolean;
  state: integer;
  function statetostr(s: integer): string;
  begin
    case s of
      SCD_INPROGRESS:
        Result := 'inprogress';
      SCD_CANCELED:
        Result := 'canceled';
      SCD_COMPLETE:
        Result := 'complete';
    end;
  end;

begin
  if fEnableDownloads = false then
    exit;
  cancel := false;
  state := SCD_UNKNOWN;
  if downloadItem.IsInProgress then
  begin
    if downloadItem.IsValid then
      if downloadItem.getPercentComplete <> 0 then
        state := SCD_INPROGRESS;
  end;
  if downloadItem.IsComplete then
    state := SCD_COMPLETE;
  if downloadItem.IsCanceled then
  begin
    state := SCD_CANCELED;
  end;
  // debug
  // sendmessagetotab(fmsg.msghandle,CRM_LOGWRITELN,'downloadupdated: '+statetostr(state)+downloaditem.getfullpath);
  if assigned(OnDownloadUpdated) then
    OnDownloadUpdated(Sender, cancel, downloadItem.getid, state,
      downloadItem.getPercentComplete, downloadItem.getfullpath);
  if cancel = true then
    Callback.cancel;
end;

procedure TCatChromium.LogURL(const url: string);
begin
  if url = emptystr then
    exit;
  if fURLLog.IndexOf(url) = -1 then
    fURLLog.Add(url);
end;

procedure TCatChromium.WMCopyData(const msgid: integer; const str: string);
var
  j: TCatJSON;
  procedure HandleResponse(json: string);
  begin
    j := TCatJSON.Create;
    j.text := json;
    if fLogURLs = true then
      LogURL(j['url']);
    if j['mimetype'] <> 'text/html' then
      AddToResourceList(j['url']);
    if fHeaders.StatusCode = emptystr then
    begin // we just want the response for the first request
      if j['url'] = GetURL then
      begin
        fHeaders.SentHead := j['headers'];
        fHeaders.RcvdHead := j['responseheaders'];
        fHeaders.StatusCode := j['status'];
      end;
    end;
    j.Free;
  end;

begin
  case msgid of
    CRM_LOG_REQUEST_JSON:
      HandleResponse(str);
  end;
  if assigned(OnBrowserMessage) then
    OnBrowserMessage(msgid, str);
end;

procedure TCatChromium.crmRenderProcessTerminated(Sender: TObject;
const Browser: ICefBrowser; status: TCefTerminationStatus);
begin
  if assigned(OnLoadEnd) then
    OnLoadEnd(Sender, 0);
end;

{
  // No longer supported in DCEF
  procedure TCatChromium.LoadCustomCSS;
  begin
  fneedrecreate:=true;
  FChrome.crm.options.UserStyleSheetEnabled :=true;
  FChrome.crm.UserStyleSheetLocation:=UserScript.CSS_UserStyleSheet;
  //FChrome.crm.Options.UniversalAccessFromFileUrlsAllowed:=true;
  //FChrome.Crm.Options.FileAccessFromFileUrlsAllowed:=true;
  end;
}

procedure TCatChromium.SetOptionState(settings, DefaultSettings: TCatJSON;
const curstate: TCefState; const propname: string);
var
  CID: string;
  value, DefaultValue: Boolean;
begin
  CID := cOptions + lowercase(propname);
  DefaultValue := DefaultSettings[CID];
  value := settings.GetValue(CID, DefaultValue);
  // GetPropValue(fCrm.Options,'ApplicationCache');
  if value <> CEFStateToBool(curstate) then
  begin
    fNeedRecreate := true;
    if value = true then
      SetPropValue(fCrm.Options, propname, STATE_ENABLED)
    else
      SetPropValue(fCrm.Options, propname, STATE_DISABLED);
  end;
end;

procedure TCatChromium.LoadSettings(settings, DefaultSettings: TCatJSON);
  procedure SetState(const curstate: TCefState; const propname: string);
  begin
    SetOptionState(settings, DefaultSettings, curstate, propname);
  end;

begin
  // LoadCustomCSS;
  fNeedRecreate := false;
  SetState(fCrm.Options.ApplicationCache, 'ApplicationCache');
  SetState(fCrm.Options.CaretBrowsing, 'CaretBrowsing');
  SetState(fCrm.Options.Databases, 'Databases');
  SetState(fCrm.Options.FileAccessFromFileUrls, 'FileAccessFromFileUrls');
  SetState(fCrm.Options.ImageLoading, 'ImageLoading');
  SetState(fCrm.Options.ImageShrinkStandaloneToFit,
    'ImageShrinkStandaloneToFit');
  SetState(fCrm.Options.Java, 'Java');
  SetState(fCrm.Options.Javascript, 'Javascript');
  SetState(fCrm.Options.JavascriptAccessClipboard, 'JavascriptAccessClipboard');
  SetState(fCrm.Options.JavascriptCloseWindows, 'JavascriptCloseWindows');
  SetState(fCrm.Options.JavascriptDomPaste, 'JavascriptDomPaste');
  SetState(fCrm.Options.JavascriptOpenWindows, 'JavascriptOpenWindows');
  SetState(fCrm.Options.LocalStorage, 'LocalStorage');
  SetState(fCrm.Options.Plugins, 'Plugins');
  SetState(fCrm.Options.TabToLinks, 'TabToLinks');
  SetState(fCrm.Options.TextAreaResize, 'TextAreaResize');
  SetState(fCrm.Options.UniversalAccessFromFileUrls,
    'UniversalAccessFromFileUrls');
  SetState(fCrm.Options.Webgl, 'Webgl');
  SetState(fCrm.Options.WebSecurity, 'WebSecurity');

  if fNeedRecreate then
  begin
    // showmessage(CEFStateToStr(crm.Options.Javascript));
    if fCrm.Browser <> nil then
    begin
      fNeedRecreate := false;
      ReCreateBrowser(GetURL);
    end;
  end;

end;

procedure TCatChromium.ReCreateBrowser(const aURL: string);
begin
{$IFNDEF USEWACEF}
  fCrm.ReCreateBrowser(aURL);
{$ENDIF}
end;

constructor TCatChromium.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csAcceptsControls];
  Color := clWindow;
  fMsg := TCatMsg.Create;
  fMsg.OnCopyDataMessage := WMCopyData;
  fCriticalSection := TCriticalSection.Create;
  fPreventPopup := true;
  fInterceptRequests := true;
  fLogURLs := false;
  fEnableDownloads := true;
  fAdjustSourceDisplayMethod := true;
  fAutoGetSource := false;
  fLogJavaScriptErrors := false;
  fResourceList := TStringList.Create;
  fURLLog := TStringList.Create;
  fCrm := TChromium.Create(nil);
  fCrm.Visible := false;
  fCrm.Color := clWindow;
  fCrm.Parent := self;
  fCrm.Align := alclient;
  fCrm.OnTitleChange := crmTitleChange;
  fCrm.OnLoadEnd := crmLoadEnd;
  fCrm.OnGetAuthCredentials := crmGetAuthCredentials;
  fCrm.OnLoadStart := crmLoadStart;
  fCrm.OnAddressChange := crmAddressChange;
  fCrm.OnStatusMessage := crmStatusMessage;
  fCrm.OnBeforeContextMenu := crmBeforeContextMenu;
  fCrm.OnBeforeResourceLoad := crmBeforeResourceLoad;
  fCrm.OnGetAuthCredentials := crmGetAuthCredentials;
  fCrm.OnBeforePopup := crmBeforePopup;
  fCrm.OnCertificateError := crmCertificateError;
  fCrm.OnConsoleMessage := crmConsoleMessage;
  fCrm.OnJsdialog := crmJsdialog;
  fCrm.OnBeforeDownload := crmBeforeDownload;
  fCrm.OnDownloadUpdated := crmDownloadUpdated;
  fCrm.OnGetResourceHandler := crmGetResourceHandler;
  fCrm.OnProcessMessageReceived := crmProcessMessageReceived;
  fCrm.OnLoadError := crmLoadError;
  fCrm.OnLoadingStateChange := crmLoadingStateChange;
  fCrm.OnPluginCrashed := crmPluginCrashed;
  fCrm.OnRenderProcessTerminated := crmRenderProcessTerminated;
  fCrm.OnContextMenuCommand := crmContextMenuCommand;
end;

procedure TCatChromium.ClearRequestData;
begin
  fLastStatusCode := 0;
  fSentRequests := 0;
  fHeaders.SentHead := emptystr;
  fHeaders.RcvdHead := emptystr;
  fHeaders.StatusCode := emptystr;
end;

procedure TCatChromium.SendRequest(const req: TCatChromiumRequest;
const Load: Boolean = false);
var
  r: ICefRequest;
  Map: ICefStringMultimap;
  data: ICefPostData;
  reqctx: ICefRequestContext;
var
  slp: TStringLoop;
  rheader, rvalue: string;
var
  reqown: TSpecialCEFReq;
  urlreq: ICefUrlRequest;
  function CreateField(const str: String): ICefPostDataElement;
  begin
    Result := TCefPostDataElementRef.New;
    Result.SetToBytes(Length(AnsiString(str)), PAnsiChar(AnsiString(str)));
  end;

begin
  if (Load = true) and (IsFrameNil) then
    exit;
  fSentRequests := fSentRequests + 1;
  r := TCefRequestRef.New;
  r.url := req.url;
  r.Method := req.Method;
  if req.Method = emptystr then
    r.Method := 'GET';

  if req.IgnoreCache then
    r.Flags := r.Flags + [UR_FLAG_SKIP_CACHE];
  // if req.UseCookies then
  // r.Flags := r.Flags + [UR_FLAG_ALLOW_COOKIES];
  if req.UseCachedCredentials then
    r.Flags := r.Flags + [UR_FLAG_ALLOW_CACHED_CREDENTIALS];
  if req.PostData <> emptystr then
  begin
    // r.Flags := UR_FLAG_SKIP_CACHE;
    data := TCefPostDataRef.New;
    data.AddElement(CreateField(req.PostData));
    { postdata.AddElement(CreateField('data.id=27'));
      postdata.AddElement(CreateField('&data.title=title'));
      postdata.AddElement(CreateField('&data.body=body')); }
    r.PostData := data;
  end;
  if req.Headers <> emptystr then
  begin
    Map := TCefStringMultiMapOwn.Create;
    r.GetHeaderMap(Map);
    slp := TStringLoop.Create;
    slp.LoadFromString(req.Headers);
    while slp.found do
    begin
      if MatchStrings(slp.current, '*:*') then
      begin
        rheader := before(slp.current, ': ');
        rvalue := after(slp.current, ': ');
        Map.Append(rheader, rvalue);
      end;
    end;
    slp.Free;
    // map.Append('Authorization','Basic '+base64encode(u+':'+p));
    r.SetHeaderMap(Map);
  end;
  if Load then
    fCrm.Browser.MainFrame.LoadRequest(r)
  else
  begin
    reqown := TSpecialCEFReq.Create;
    reqown.MsgHandle := self.fMsg.MsgHandle;
    reqown.Details := req.Details;
    urlreq := TCefUrlRequestRef.New(r, reqown, reqctx) as ICefUrlRequest;
  end;
end;

procedure TCatChromium.LoadFromString(const s, url: string);
begin
  ClearRequestData;
  if GetURL = emptystr then
    LoadBlank; // CEF3 LoadString Bug Workaround
  if fCrm.Browser = nil then
    exit;
  if fCrm.Browser.GetMainFrame = nil then
    exit;
  fCrm.Browser.GetMainFrame.LoadString(s, url);
  fCrm.Visible := true;
end;

procedure TCatChromium.Load(const url: string);
begin
  ClearRequestData;
  // Better to use crm.Load() instead of:
  // if crm.Browser.GetMainFrame<>nil then crm.Browser.GetMainFrame.LoadURL(url);
  fCrm.Load(url);
  if fNeedRecreate then
    ReCreateBrowser(url);
  fCrm.Visible := true;
end;

procedure TCatChromium.LoadBlank(const WaitLoad: Boolean = false);
begin
  ClearRequestData;
  fCrm.Load(cURL_HOME);
  fCrm.Visible := true;
  if WaitLoad then
    exit;
  while isLoading do
    application.ProcessMessages;
end;

function TCatChromium.isLoading: Boolean;
begin
  Result := false;
  if fCrm.Browser = nil then
    exit;
  Result := fCrm.Browser.isLoading;
end;

procedure TCatChromium.Reload(const IgnoreCache: Boolean = false);
begin
  ClearRequestData;
  if isLoading then
    Stop;
  if fCrm.Browser = nil then
    exit;
  if IgnoreCache then
    fCrm.Browser.reloadignorecache
  else
    fCrm.Browser.Reload; // standard reload
end;

procedure TCatChromium.Stop(const waitstop: Boolean = false);
begin
  if fCrm.Browser <> nil then
    fCrm.Browser.StopLoad;
  if waitstop = false then
    exit;
  while isLoading do
    application.ProcessMessages;
end;

// Stop loading and load a blank page
procedure TCatChromium.StopLoadBlank;
begin
  if isLoading then
  begin
    Stop;
    LoadBlank(true);
  end;
  application.ProcessMessages;
end;

destructor TCatChromium.Destroy;
begin
  fMsg.Free;
  fInterceptRequests := false;
  OnAfterSetSource := nil;
  OnBrowserMessage := nil;
  NilComponentMethods(fCrm);
{$IFDEF USEWACEF}
  StopLoadBlank; // helps avoid some AV cases when closing an active tab
  // TODO: check if this is still needed for the latest WACEF
{$ENDIF}
{$IFNDEF USEWACEF}
  if fDevTools <> nil then
    fDevTools.Free;
{$ENDIF}
  fCrm.Free;
  fURLLog.Free;
  fResourceList.Free;
  fCriticalSection.Free;
  inherited Destroy;
end;

// ------------------------------------------------------------------------//
// TSpecialCEFReq                                                          //
// ------------------------------------------------------------------------//

function TSpecialCEFReq.CEF_GetRcvdHeader(Response: ICefResponse): string;
var
  I: integer;
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
    for I := 0 to GetSize do
    begin
      if I < GetSize then
        Add(GetKey(I), GetValue(I));
    end;
  end;
  Result := s;
end;

function TSpecialCEFReq.CEF_GetSentHeader(request: ICefRequest;
IncludePostData: Boolean = true): string;
var
  I: integer;
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
    for I := 0 to GetSize do
    begin
      if I < GetSize then
        Add(GetKey(I), GetValue(I));
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
  I: integer;
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
    List := PostData.GetElements(elcount);
    for I := 0 to List.Count - 1 do
    begin
      postElement := List[I] as ICefPostDataElement;
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
      SendCDMessage(MsgHandle, CRM_LOG_REQUEST_JSON, fr.AsJson(true));
    fr := nil;
  finally
    fCriticalSection.Leave;
  end;
end;

// ------------------------------------------------------------------------//
// TCatSourceVisitorOwn                                                    //
// ------------------------------------------------------------------------//

constructor TCatSourceVisitorOwn.Create;
begin
  inherited Create;
  fCriticalSection := TCriticalSection.Create;
end;

destructor TCatSourceVisitorOwn.Destroy;
begin
  fCriticalSection.Free;
  inherited;
end;

procedure TCatSourceVisitorOwn.Visit(const str: ustring);
var
  t: TCatChromium;
begin
  fCriticalSection.Enter;
  try
    if self.Browser <> nil then
    begin
      t := TCatChromium(self.Browser);
      self.Browser := nil;
      t.SetSource(str);
      // t := nil;
    end;
  finally
    fCriticalSection.Leave;
  end;
end;

initialization

CefRemoteDebuggingPort := 8000;

end.
