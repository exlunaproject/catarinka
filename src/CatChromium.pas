unit CatChromium;

{
  Catarinka Browser Component
  Copyright (c) 2011-2014 Syhunt Informatica
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details
}

interface

{$I Catarinka.inc}

uses
{$IFDEF DXE2_OR_UP}
  System.Classes, Winapi.Windows, Winapi.Messages, Vcl.Controls, Vcl.Graphics,
  Vcl.Forms, System.SysUtils, System.SyncObjs, Vcl.Dialogs, Vcl.Clipbrd,
{$ELSE}
  Classes, Windows, Messages, Controls, Graphics, Forms, SysUtils, SyncObjs,
  Dialogs, Clipbrd,
{$ENDIF}
  cefvcl, ceflib, superobject, CatJSON;

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
    fMsgHandle: HWND;
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
    procedure ClearEvents;
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
    procedure crmBeforeResourceLoad(Sender: TObject; const Browser: ICefBrowser;
      const frame: ICefFrame; const request: ICefRequest; out Result: Boolean);
    procedure crmBeforePopup(Sender: TObject; const Browser: ICefBrowser;
      const frame: ICefFrame; const targetUrl, targetFrameName: ustring;
      var popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo;
      var client: ICefClient; var settings: TCefBrowserSettings;
      var noJavascriptAccess: Boolean; out Result: Boolean);
    procedure crmConsoleMessage(Sender: TObject; const Browser: ICefBrowser;
      const message, source: ustring; line: integer; out Result: Boolean);
    procedure crmJsdialog(Sender: TObject; const Browser: ICefBrowser;
      const originUrl, acceptLang: ustring; dialogType: TCefJsDialogType;
      const messageText, defaultPromptText: ustring;
      Callback: ICefJsDialogCallback; out suppressMessage, Result: Boolean);
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
      const frame: ICefFrame; errorCode: integer;
      const errorText, failedUrl: ustring);
    procedure crmLoadingStateChange(Sender: TObject; const Browser: ICefBrowser;
      isLoading, canGoBack, canGoForward: Boolean);
    procedure crmPluginCrashed(Sender: TObject; const Browser: ICefBrowser;
      const pluginPath: ustring);
    procedure crmGetAuthCredentials(Sender: TObject; const Browser: ICefBrowser;
      const frame: ICefFrame; isProxy: Boolean; const host: ustring;
      port: integer; const realm, scheme: ustring;
      const Callback: ICefAuthCallback; out Result: Boolean);
    procedure crmMessage(var AMsg: TMessage);
    procedure crmRenderProcessTerminated(Sender: TObject;
      const Browser: ICefBrowser; status: TCefTerminationStatus);
    procedure LogURL(const url: string);
    procedure SendMessageToTab(const id: integer; const s: string);
    procedure SetZoomLevel(const zl: double);
    function GetZoomLevel: double;
    function GetURLShort: string;
    function GetDevToolsURL: string;
    procedure StopLoadBlank;
    procedure WMCopyData(var message: TMessage);
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
    procedure Stop(const waitstop:boolean=false);
    procedure GetSource; // callback
    procedure GetSourceAsText; // callback
    procedure SetSource(const s: string);
    function isLoading: Boolean;
    procedure ShowAuthDialog(const Username: string = '';
      const Password: string = '');
    procedure ViewSourceExternalEditor;
    // properties
    property AdjustSourceDisplayMethod: Boolean read fAdjustSourceDisplayMethod
      write fAdjustSourceDisplayMethod;
    property Crm: TChromium read fCrm;
    property DevToolsURL: string read GetDevToolsURL;
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
    procedure OnDownloadData(const request: ICefUrlRequest; data: Pointer;
      dataLength: NativeUInt); override;
  public
    MsgHandle: HWND;
    Details: string;
    constructor Create; override;
    destructor Destroy; override;
  end;

  TSandcatV8Extension = class(TCefv8HandlerOwn)
  private
    fV8MsgHandle: integer;
  protected
    function Execute(const name: ustring; const obj: ICefv8Value;
      const arguments: TCefv8ValueArray; var retval: ICefv8Value;
      var exception: ustring): Boolean; override;
  public
    constructor Create; override;
  end;

  TCustomRenderProcessHandler = class(TCefRenderProcessHandlerOwn)
  private
    fSandcatV8Extension: TSandcatV8Extension;
  protected
    function OnProcessMessageReceived(const Browser: ICefBrowser;
      sourceProcess: TCefProcessId; const message: ICefProcessMessage)
      : Boolean; override;
    procedure OnWebKitInitialized; override;
  end;

type
  TCatChromiumXHR = record
    Details: string;
    Method: string;
    url: string;
    Headers: string;
    PostData: string;
    Filters: string;
    Username: string;
    Password: string;
    Callback: string;
    Tab: string;
  end;

const
  cABOUTBLANK = 'about:blank';
  cHOMEURL = 'sandcat:home';

const // Chromium settings
  cOptions = 'chrome.options.';
  CRMO_ACCELERATED_COMPOSITING = cOptions + 'accelerated_compositing';
  CRMO_APPLICATION_CACHE = cOptions + 'application_cache';
  CRMO_AUTHOR_AND_USER_STYLES = cOptions + 'author_and_user_styles';
  CRMO_CARET_BROWSING = cOptions + 'caret_browsing';
  CRMO_DATABASES = cOptions + 'databases';
  CRMO_DEVELOPER_TOOLS = cOptions + 'developer_tools';
  CRMO_FILE_ACCESS_FROM_FILE_URLS = cOptions + 'fileurl.access';
  CRMO_IMAGE_LOADING = cOptions + 'image.loading';
  CRMO_IMAGE_SHRINK_STANDALONE_TO_FIT = cOptions +
    'image.shrink_stand_alone_to_fit';
  CRMO_JAVA = cOptions + 'java';
  CRMO_JAVASCRIPT = cOptions + 'javascript.enabled';
  CRMO_JAVASCRIPT_ACCESS_CLIPBOARD = cOptions + 'javascript.access_clipboard';
  CRMO_JAVASCRIPT_CLOSE_WINDOWS = cOptions + 'javascript.close_windows';
  CRMO_JAVASCRIPT_DOM_PASTE = cOptions + 'javascript.dom_paste';
  CRMO_JAVASCRIPT_OPEN_WINDOWS = cOptions + 'javascript.open_windows';
  CRMO_LOCAL_STORAGE = cOptions + 'local_storage';
  CRMO_PAGE_CACHE = cOptions + 'page_cache';
  CRMO_PLUGINS = cOptions + 'plugins';
  CRMO_TAB_TO_LINKS = cOptions + 'tab_to_links';
  CRMO_TEXT_AREA_RESIZE = cOptions + 'text_area_resize';
  CRMO_UNIVERSAL_ACCESS_FROM_FILE_URLS = cOptions + 'fileurl.universal_access';
  CRMO_WEBGL = cOptions + 'webgl';
  CRMO_WEBSECURITY = cOptions + 'websecurity';

const // Messages from the Chromium renderer to the Sandcat Tab object
  CRM_LOG_REQUEST_JSON = 1;
  CRM_CONSOLE_ENDEXTERNALOUTPUT = 2;
  CRM_RENDER_PROCESSTERMINATED = 3;
  CRM_NEWTAB = 4;
  CRM_XHR_LOG = 5;
  CRM_JS_RUN_WHITELISTED_LUA = 6;
  CRM_JS_WRITELN = 7;
  CRM_JS_WRITE = 8;
  CRM_JS_WRITEVALUE = 9;
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
  SHTD_NORMAL = 10;
  SHTD_FORCED = 11;
  SHTD_MANUAL = 12;

function GetCEFUserAgent: string;
function GetCEFDefaults(settings: TCatJSON): string;
function CEFV8ValueToStr(v: ICefv8Value): string;
function BuildRequest(Method, url: string; PostData: string = '')
  : TCatChromiumRequest;
procedure CatCEFShutdown(mode:integer);
procedure SendCDMessage(desthandle, msgid: integer; l: string);
procedure Send_WriteValue(desthandle: integer; key, value: string);

implementation

uses uAuthentication, CatJINI, CatStringLoop, CatTasks, CatUI, CatFiles, CatStrings,
  CatHTTP, CatUtils, CatTime, CatPointer;

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

procedure CatCEFShutdown(mode:integer);
begin
  case mode of
    SHTD_NORMAL: ; // do nothing
    SHTD_FORCED: KillProcessbyPID(GetCurrentProcessId);
    SHTD_MANUAL: begin
      ceflib.CefShutDown;
      ExitProcess(0);
      end;
  end;
end;

function GetCEFDefaults(settings: TCatJSON): string;
var
  list: TStringList;
  procedure s(CID: string; DefaultValue: Boolean);
  begin
    settings[CID] := DefaultValue;
    list.Add(CID);
  end;

begin
  list := TStringList.Create;
  s(CRMO_ACCELERATED_COMPOSITING, true);
  s(CRMO_APPLICATION_CACHE, true);
  s(CRMO_AUTHOR_AND_USER_STYLES, true);
  s(CRMO_CARET_BROWSING, true);
  s(CRMO_DATABASES, true);
  s(CRMO_DEVELOPER_TOOLS, true);
  s(CRMO_FILE_ACCESS_FROM_FILE_URLS, true);
  s(CRMO_IMAGE_LOADING, true);
  s(CRMO_IMAGE_SHRINK_STANDALONE_TO_FIT, true);
  s(CRMO_JAVA, true);
  s(CRMO_JAVASCRIPT, true);
  s(CRMO_JAVASCRIPT_ACCESS_CLIPBOARD, true);
  s(CRMO_JAVASCRIPT_CLOSE_WINDOWS, true);
  s(CRMO_JAVASCRIPT_DOM_PASTE, true);
  s(CRMO_JAVASCRIPT_OPEN_WINDOWS, true);
  s(CRMO_LOCAL_STORAGE, true);
  s(CRMO_PAGE_CACHE, true);
  s(CRMO_PLUGINS, true);
  s(CRMO_TAB_TO_LINKS, true);
  s(CRMO_TEXT_AREA_RESIZE, true);
  s(CRMO_UNIVERSAL_ACCESS_FROM_FILE_URLS, true);
  s(CRMO_WEBGL, true);
  s(CRMO_WEBSECURITY, true);
  Result := list.text;
  list.free;
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
  Result := CefCache + 'Headers\' + f;
end;

function SaveResponseToFile(s: string): string;
var
  sl: TStringList;
begin
  Result := GetTempFile;
  if CefCache = emptystr then
    exit;
  sl := TStringList.Create;
  sl.text := s;
  sl.SaveToFile(Result);
  sl.free;
end;

procedure SendCDMessage(desthandle, msgid: integer; l: string);
var
  pData: PCopyDataStruct;
begin
  pData := nil;
  try
    New(pData);
    pData^.dwData := msgid;
    pData^.cbData := Length(l) + 1;
    pData^.lpData := PAnsiChar(AnsiString(l));
    SendMessage(desthandle, WM_COPYDATA, application.handle, integer(pData));
  finally
    Dispose(pData);
  end;
end;

procedure Send_WriteValue(desthandle: integer; key, value: string);
var
  j: TCatJSON;
begin
  j := TCatJSON.Create;
  j['k'] := key;
  j['v'] := value;
  SendCDMessage(desthandle, CRM_JS_WRITEVALUE, j.text);
  j.free;
end;

function TSpecialCEFReq.CEF_GetRcvdHeader(Response: ICefResponse): string;
var
  i: integer;
  s, kv, lastkv: string;
  Map: TCefStringMultiMapOwn;
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
  with Map as ICefStringMultiMap do
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
  Map: TCefStringMultiMapOwn;
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
  with Map as ICefStringMultiMap do
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
  list: IInterfaceList;
begin
  ansi := '';
  PostData := request.getPostData;
  if PostData <> nil then
  begin
    list := PostData.GetElements(PostData.GetCount);
    for i := 0 to list.Count - 1 do
    begin
      postElement := list[i] as ICefPostDataElement;
      case postElement.GetType of
        PDE_TYPE_BYTES:
          begin
            SetLength(datastr, postElement.GetBytesCount);
            postElement.GetBytes(postElement.GetBytesCount, PAnsiChar(datastr));
            ansi := ansi + datastr;
          end;
        PDE_TYPE_FILE:
          ;
        PDE_TYPE_EMPTY:
          ;
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
  fResponseStream.free;
  fCriticalSection.free;
  inherited;
end;

procedure TSpecialCEFReq.OnDownloadData(const request: ICefUrlRequest;
  data: Pointer; dataLength: NativeUInt);
begin
  fResponseStream.WriteData(data, dataLength);
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

constructor TSandcatV8Extension.Create;
begin
  inherited Create;
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
    else if v.IsDate then
      Result := datetimetostr(v.GetDateValue)
    else if v.IsObject then
      Result := '[object]'
    else if v.IsArray then
      Result := '[array]'
    else if v.IsFunction then
      Result := '[function ' + v.GetFunctionName + ']';
  end;
end;

function TSandcatV8Extension.Execute(const name: ustring;
  const obj: ICefv8Value; const arguments: TCefv8ValueArray;
  var retval: ICefv8Value; var exception: ustring): Boolean;
  procedure LogRequest(Details, rid, Method, url, rcvdheader, Response: string);
  var
    j: tjinilist;
  begin
    j := tjinilist.Create;
    j.values['ReqID'] := rid;
    j.values['Details'] := Details;
    j.values['ResponseHeaders'] := rcvdheader;
    j.values['Method'] := Method;
    j.values['URL'] := url;
    j.values['ResponseFilename'] := SaveResponseToFile(Response);
    SendCDMessage(fV8MsgHandle, CRM_XHR_LOG, j.text);
    j.free;
  end;

begin
  Result := false;
  if (name = 'base64encode') then
  begin
    if (Length(arguments) <> 1) or (not arguments[0].IsString) then
    begin
      Result := false;
      exit;
    end;
    retval := TCefv8ValueRef.NewString
      (base64encode(arguments[0].GetStringValue));
    Result := true;
  end
  else if (name = 'base64decode') then
  begin
    if (Length(arguments) <> 1) or (not arguments[0].IsString) then
    begin
      Result := false;
      exit;
    end;
    retval := TCefv8ValueRef.NewString
      (base64decode(arguments[0].GetStringValue));
    Result := true;
  end
  else if (name = 'consoleoutput') then
  begin
    if (Length(arguments) = 0) or (not arguments[0].IsBool) then
    begin
      Result := false;
      exit;
    end;
    if arguments[0].GetBoolValue = false then
      SendCDMessage(fV8MsgHandle, CRM_CONSOLE_ENDEXTERNALOUTPUT, emptystr);
    Result := true;
  end
  else if (name = 'logrequest') then
  begin
    if (Length(arguments) <> 6) or (not arguments[0].IsString) or
      (not arguments[1].IsString) or (not arguments[2].IsString) or
      (not arguments[3].IsString) or (not arguments[4].IsString) or
      (not arguments[5].IsString) then
    begin
      Result := false;
      exit;
    end;
    LogRequest(arguments[0].GetStringValue, arguments[1].GetStringValue,
      arguments[2].GetStringValue, arguments[3].GetStringValue,
      arguments[4].GetStringValue, arguments[5].GetStringValue);
    Result := true;
  end
  else if (name = 'callwl') then
  begin
    if (Length(arguments) <> 1) or (not arguments[0].IsString) then
    begin
      Result := false;
      exit;
    end;
    SendCDMessage(fV8MsgHandle, CRM_JS_RUN_WHITELISTED_LUA,
      arguments[0].GetStringValue);
    Result := true;
  end
  else if (name = 'writevalue') then
  begin
    if (Length(arguments) <> 2) or (not arguments[0].IsString) or
      (not arguments[1].IsString) then
    begin
      Result := false;
      exit;
    end;
    Send_WriteValue(fV8MsgHandle, arguments[0].GetStringValue,
      CEFV8ValueToStr(arguments[1]));
    Result := true;
  end
  else if (name = 'writeln') then
  begin
    if (Length(arguments) <> 1) then
    begin
      Result := false;
      exit;
    end;
    SendCDMessage(fV8MsgHandle, CRM_JS_WRITELN, CEFV8ValueToStr(arguments[0]));
    Result := true;
  end
  else if (name = 'write') then
  begin
    if (Length(arguments) <> 1) then
    begin
      Result := false;
      exit;
    end;
    SendCDMessage(fV8MsgHandle, CRM_JS_WRITE, CEFV8ValueToStr(arguments[0]));
    Result := true;
  end;
end;

procedure TCustomRenderProcessHandler.OnWebKitInitialized;
const
  v8extension = '' + 'var Sandcat;' + 'if (!Sandcat) Sandcat = {};' +
    '(function() {' +
    'Sandcat.Base64Encode = function(s) { native function base64encode(); return base64encode(s); };'
    + 'Sandcat.Base64Decode = function(s) { native function base64decode(); return base64decode(s); };'
    + 'Sandcat.LogRequest = function(details,rid,method,url,rcvdhead,response) { native function logrequest(); logrequest(details,rid,method,url,rcvdhead,response); };'
    + 'Sandcat.CallWL = function(s) { native function callwl(); callwl(s); };' +
    'Sandcat.ConsoleOutput = function(b) { native function consoleoutput(); consoleoutput(b); };'
    + 'Sandcat.WriteLn = function(s) { native function writeln(); writeln(s); };'
    + 'Sandcat.WriteValue = function(key,value) { native function writevalue(); writevalue(key,value); };'
    + 'Sandcat.Write = function(s) { native function write(); write(s); };'
    + '})();';
begin
  fSandcatV8Extension := TSandcatV8Extension.Create;
  CefRegisterExtension('v8/browser', v8extension,
    fSandcatV8Extension as ICefV8Handler);
end;

function TCustomRenderProcessHandler.OnProcessMessageReceived
  (const Browser: ICefBrowser; sourceProcess: TCefProcessId;
  const message: ICefProcessMessage): Boolean;
begin
  Result := false;
  if (message.getName = 'msg') then
  begin
    Result := true;
    case message.getArgumentList.GetInt(0) of
      SCTM_SET_V8_MSGHANDLE:
        fSandcatV8Extension.fV8MsgHandle := message.getArgumentList.GetInt(1);
      SCTM_V8_REGISTEREXTENSION:
        begin // CefRegisterExtension not working from here
          // CefRegisterExtension('v8/browserx',message.getArgumentList.GetString(1), SandcatV8Extension as ICefV8Handler);
        end;
    end;
    // browser.SendProcessMessage(PID_BROWSER,message);  // is crashing the renderer, review later
  end;
end;

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

function TCatChromium.GetDevToolsURL: string;
begin
  Result := emptystr;
  if fCrm.Browser = nil then
    exit;
  if fCrm.Browser.GetHost <> nil then
    Result := fCrm.Browser.GetHost.GetDevToolsURL(true);
end;

function TCatChromium.GetURLShort: string;
var
  u: string;
begin
  u := GetURL;
  if u = cHOMEURL then
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
          free;
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
  Map: ICefStringMultiMap;
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
      free;
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
const frame: ICefFrame; errorCode: integer;
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
      //canclear := false;
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
begin
  if fResourceList.Count > 2000 then
    exit;
  if fResourceList.IndexOf(url) <> -1 then
    exit;
  if url = GetURL then
    exit;
  if pos('?', url) <> 0 then
    exit; // url with params, most likely not an object
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
begin
  if fInterceptRequests = false then
    exit;
  if Browser = nil then
    exit;
  fSentRequests := fSentRequests + 1;
  // sendmessagetotab(msghandle,CRM_LOGWRITELN,'getresourcehandler:'+request.getUrl);
  reqown := TSpecialCEFReq.Create;
  reqown.MsgHandle := self.fMsgHandle;
  req := TCefUrlRequestRef.New(request, reqown) as ICefUrlRequest;
end;

procedure TCatChromium.crmBeforeResourceLoad(Sender: TObject;
const Browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest;
out Result: Boolean);
begin
  // result:=false; // unnecessary
  // This would avoid a weird crash during navigation with past CEF3 releases
end;

procedure TCatChromium.SendMessageToTab(const id: integer; const s: string);
begin
  SendCDMessage(fMsgHandle, id, s);
end;

procedure TCatChromium.crmBeforePopup(Sender: TObject;
const Browser: ICefBrowser; const frame: ICefFrame;
const targetUrl, targetFrameName: ustring; var popupFeatures: TCefPopupFeatures;
var windowInfo: TCefWindowInfo; var client: ICefClient;
var settings: TCefBrowserSettings; var noJavascriptAccess: Boolean;
out Result: Boolean);
var
  u: string;
begin
  Result := fPreventPopup;
  u := targetUrl;
  SendMessageToTab(CRM_NEWTAB, u);
  // if assigned(OnBeforePopup) then onBeforePopup(sender,u,result);
  // targetUrl:=u; // ToDo: CEF3 latest returns a constant
end;

procedure TCatChromium.crmConsoleMessage(Sender: TObject;
const Browser: ICefBrowser; const message, source: ustring; line: integer;
out Result: Boolean);
begin
  if fLogJavaScriptErrors = false then
    exit;
  fLogJavaScriptErrors := false;
  if assigned(OnConsoleMessage) then
    OnConsoleMessage(Sender, message, source, line);
end;

procedure TCatChromium.crmJsdialog(Sender: TObject; const Browser: ICefBrowser;
const originUrl, acceptLang: ustring; dialogType: TCefJsDialogType;
const messageText, defaultPromptText: ustring; Callback: ICefJsDialogCallback;
out suppressMessage, Result: Boolean);
begin
  case dialogType of
    JSDIALOGTYPE_ALERT:
      begin
        if assigned(OnBrowserMessage) then
          OnBrowserMessage(CRM_JS_ALERT, messageText);
        // ShowMessage(MessageText);
        suppressMessage := true;
      end;
  end;
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
begin
  if fEnableDownloads = false then
    exit;
  s := suggestedName;
  // debug
  // sendmessagetotab(msghandle,CRM_LOGWRITELN,'beforedownload:'+inttostr(downloaditem.getid));
  Callback.Cont(GetSpecialFolderPath(CSIDL_PERSONAL, false) + '\' +
    suggestedName, true);
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
  // sendmessagetotab(msghandle,CRM_LOGWRITELN,'downloadupdated: '+statetostr(state)+downloaditem.getfullpath);
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

procedure TCatChromium.WMCopyData(var message: TMessage);
var
  pData: PCopyDataStruct;
  msgid: integer;
  str: string;
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
    j.free;
  end;

begin
  message.Result := 0;
  pData := PCopyDataStruct(message.LParam);
  if (pData = nil) then
    exit;
  str := string(StrPas(PAnsiChar(pData^.lpData)));
  msgid := pData^.dwData;
  case msgid of
    CRM_LOG_REQUEST_JSON:
      HandleResponse(str);
  end;
  if assigned(OnBrowserMessage) then
    OnBrowserMessage(msgid, str);
  message.Result := 1;
end;

procedure TCatChromium.crmMessage(var AMsg: TMessage);
begin
  try
    case AMsg.msg of
      WM_COPYDATA:
        WMCopyData(AMsg);
    end;
  except
  end;
end;

procedure TCatChromium.crmRenderProcessTerminated(Sender: TObject;
const Browser: ICefBrowser; status: TCefTerminationStatus);
begin
  if assigned(OnBrowserMessage) then
    OnBrowserMessage(CRM_CONSOLE_ENDEXTERNALOUTPUT, emptystr);
  if assigned(OnLoadEnd) then
    OnLoadEnd(Sender, 0);
end;

{ procedure TCatChromium.LoadCustomCSS;
  begin
  needrecreate:=true;
  FChrome.crm.options.UserStyleSheetEnabled :=true;
  FChrome.crm.UserStyleSheetLocation:=UserScript.CSS_UserStyleSheet;
  //FChrome.crm.Options.UniversalAccessFromFileUrlsAllowed:=true;
  //FChrome.Crm.Options.FileAccessFromFileUrlsAllowed:=true;
  end;
}

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

procedure TCatChromium.LoadSettings(settings, DefaultSettings: TCatJSON);
  function GetState(CID: string): TCefState;
  var
    value, DefaultValue: Boolean;
  begin
    Result := STATE_DEFAULT;
    DefaultValue := DefaultSettings[CID];
    value := settings.GetValue(CID, DefaultValue);
    if value <> DefaultValue then
    begin
      fNeedRecreate := true;
      if value = true then
        Result := STATE_ENABLED
      else
        Result := STATE_DISABLED;
    end;
  end;

begin
  // LoadCustomCSS;
  fNeedRecreate := false;
  fCrm.Options.AcceleratedCompositing := GetState(CRMO_ACCELERATED_COMPOSITING);
  fCrm.Options.ApplicationCache := GetState(CRMO_APPLICATION_CACHE);
  fCrm.Options.AuthorAndUserStyles := GetState(CRMO_AUTHOR_AND_USER_STYLES);
  fCrm.Options.CaretBrowsing := GetState(CRMO_CARET_BROWSING);
  fCrm.Options.Databases := GetState(CRMO_DATABASES);
  // fCrm.Options.DeveloperTools := GetState(CRMO_DEVELOPER_TOOLS);
  fCrm.Options.FileAccessFromFileUrls :=
    GetState(CRMO_FILE_ACCESS_FROM_FILE_URLS);
  fCrm.Options.ImageLoading := GetState(CRMO_IMAGE_LOADING);
  fCrm.Options.ImageShrinkStandaloneToFit :=
    GetState(CRMO_IMAGE_SHRINK_STANDALONE_TO_FIT);
  fCrm.Options.Java := GetState(CRMO_JAVA);
  fCrm.Options.Javascript := GetState(CRMO_JAVASCRIPT);
  fCrm.Options.JavascriptAccessClipboard :=
    GetState(CRMO_JAVASCRIPT_ACCESS_CLIPBOARD);
  fCrm.Options.JavascriptCloseWindows :=
    GetState(CRMO_JAVASCRIPT_CLOSE_WINDOWS);
  fCrm.Options.JavascriptDomPaste := GetState(CRMO_JAVASCRIPT_DOM_PASTE);
  fCrm.Options.JavascriptOpenWindows := GetState(CRMO_JAVASCRIPT_OPEN_WINDOWS);
  fCrm.Options.LocalStorage := GetState(CRMO_LOCAL_STORAGE);
  // fCrm.Options.PageCache := GetState(CRMO_PAGE_CACHE);
  fCrm.Options.Plugins := GetState(CRMO_PLUGINS);
  fCrm.Options.TabToLinks := GetState(CRMO_TAB_TO_LINKS);
  fCrm.Options.TextAreaResize := GetState(CRMO_TEXT_AREA_RESIZE);
  fCrm.Options.UniversalAccessFromFileUrls :=
    GetState(CRMO_UNIVERSAL_ACCESS_FROM_FILE_URLS);
  fCrm.Options.Webgl := GetState(CRMO_WEBGL);
  fCrm.Options.WebSecurity := GetState(CRMO_WEBSECURITY);
  {
  // Recreating the browser causing a crash with the latest CEF
  // (investigate later)
  if fNeedRecreate then
  begin
    // showmessage(CEFStateToStr(crm.Options.Javascript));
    if fCrm.Browser <> nil then
    begin
      fNeedRecreate := false;
      fCrm.ReCreateBrowser(GetURL);
    end;
  end; }
end;

constructor TCatChromium.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csAcceptsControls];
  Color := clWindow;
  fMsgHandle :=
{$IFDEF DXE2_OR_UP}System.{$ENDIF}Classes.AllocateHWnd(crmMessage);
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

procedure TCatChromium.ClearEvents;
begin
  OnAfterSetSource := nil;
  OnBrowserMessage := nil;
  with fCrm do
  begin
    OnContextMenuCommand := nil;
    OnAddressChange := nil;
    OnBeforeContextMenu := nil;
    OnBeforeDownload := nil;
    OnBeforePopup := nil;
    OnBeforeResourceLoad := nil;
    OnConsoleMessage := nil;
    OnDownloadUpdated := nil;
    OnGetAuthCredentials := nil;
    OnGetResourceHandler := nil;
    OnJsdialog := nil;
    OnLoadEnd := nil;
    OnLoadError := nil;
    OnLoadingStateChange := nil;
    OnLoadStart := nil;
    OnPluginCrashed := nil;
    OnProcessMessageReceived := nil;
    OnRenderProcessTerminated := nil;
    OnStatusMessage := nil;
    OnTitleChange := nil;
  end;
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
  Map: ICefStringMultiMap;
  data: ICefPostData;
var
  slp: TStringLoop;
  rheader, rvalue: string;
var
  reqown: TSpecialCEFReq;
  urlreq: ICefUrlRequest;
  function CreateField(const str: String): ICefPostDataElement;
  begin
    Result := TCefPostDataElementRef.New;
    Result.SetToBytes(Length(ansistring(str)), PAnsiChar(ansistring(str)));
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
  if req.UseCookies then
    r.Flags := r.Flags + [UR_FLAG_ALLOW_COOKIES];
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
    slp.free;
    // map.Append('Authorization','Basic '+base64encode(u+':'+p));
    r.SetHeaderMap(Map);
  end;
  if Load then
    fCrm.Browser.MainFrame.LoadRequest(r)
  else
  begin
    reqown := TSpecialCEFReq.Create;
    reqown.MsgHandle := self.fMsgHandle;
    reqown.Details := req.Details;
    urlreq := TCefUrlRequestRef.New(r, reqown) as ICefUrlRequest;
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
    fCrm.ReCreateBrowser(url);
  fCrm.Visible := true;
end;

procedure TCatChromium.LoadBlank(const WaitLoad: Boolean = false);
begin
  ClearRequestData;
  fCrm.Load(cHOMEURL);
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

procedure TCatChromium.Stop(const waitstop:boolean=false);
begin
  if fCrm.Browser <> nil then
    fCrm.Browser.StopLoad;
  if waitstop = false then
    exit;
  while isLoading do
  begin
    application.ProcessMessages;
    catdelay(1000);
  end;
end;

// Stop loading and load a blank page - helps avoid some AV cases when
// closing an active tab
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
{$IFDEF DXE2_OR_UP}System.{$ENDIF}Classes.DeallocateHWnd(fMsgHandle);
  fInterceptRequests := false;
  ClearEvents;
  StopLoadBlank;
  fCrm.free;
  fURLLog.free;
  fResourceList.free;
  fCriticalSection.free;
  inherited Destroy;
end;

constructor TCatSourceVisitorOwn.Create;
begin
  inherited Create;
  fCriticalSection := TCriticalSection.Create;
end;

destructor TCatSourceVisitorOwn.Destroy;
begin
  fCriticalSection.free;
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
      //t := nil;
    end;
  finally
    fCriticalSection.Leave;
  end;
end;

initialization

CefRenderProcessHandler := TCustomRenderProcessHandler.Create;
CefRemoteDebuggingPort := 8000;

end.
