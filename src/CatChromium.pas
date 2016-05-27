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
  System.TypInfo, Vcl.ExtCtrls,
{$ELSE}
  Classes, Windows, Messages, Controls, Graphics, Forms, SysUtils, SyncObjs,
  Dialogs, Clipbrd, TypInfo, ExtCtrls,
{$ENDIF}
{$IFDEF USEWACEF}
  WACefComponent, WACefInterfaces, WACefTypes, WACefOwns, WACefCExports,
  WACefLib, WACefRefs,
{$ELSE}
  cefgui, cefvcl, ceflib,
{$ENDIF}
  CatJSON, CatMsg, CatChromiumLib;

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
  TCatChromium = class(TCustomControl)
  private
    fDevTools: TCatDevTools;
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
    fSplitter: TSplitter;
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
    procedure LogRequest(const json: string);
    procedure LogURL(const url: string);
    procedure SendMessageToTab(const id: integer; const s: string);
    procedure SetOptionState(settings, DefaultSettings: TCatJSON;
      const curstate: TCefState; const propname: string);
    procedure SetZoomLevel(const zl: double);
    function GetZoomLevel: double;
    function GetURLShort: string;
    procedure ReCreateBrowser(const aURL: string);
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

implementation

uses uAuthentication, CatStringLoop, CatUI, CatStrings, CatHTTP;

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
begin
  SendMessage(SCTM_V8_REGISTEREXTENSION, v8js);
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
    Callback.Cont(u, p);
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
    OnLoadError(Sender, integer(errorCode), errorText, failedUrl);
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
  if assigned(OnBeforePopup) then
    OnBeforePopup(Sender, u, Result);
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
  fDevTools.Align := AlBottom;
  fDevTools.View(fCrm.Browser);
  if fSplitter.visible = false then
  begin
    fDevTools.height := 300;
    fSplitter.visible := true;
    fSplitter.Align := AlBottom;
    fSplitter.Top := fDevTools.Top + 10;
  end;
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
    state := SCD_CANCELED;
  // debug
  // sendmessagetotab(fmsg.msghandle,CRM_LOGWRITELN,'downloadupdated: '+downloadstatetostr(state)+downloaditem.getfullpath);
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

procedure TCatChromium.LogRequest(const json: string);
var
  j: TCatJSON;
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

procedure TCatChromium.WMCopyData(const msgid: integer; const str: string);
begin
  case msgid of
    CRM_LOG_REQUEST_JSON:
      LogRequest(str);
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
  // SetState(fCrm.Options.Java, 'Java');
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

  fSplitter := TSplitter.Create(self);
  fSplitter.Parent := self;
  fSplitter.width := 2;
  fSplitter.visible := false;
  fSplitter.Color := clBtnShadow;
  fDevTools := TCatDevTools.Create(self);
  fDevTools.Parent := self;
  fDevTools.Splitter := fSplitter;

  fCrm := TChromium.Create(nil);
  fCrm.visible := false;
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
  // fCrm.OnGetAuthCredentials := crmGetAuthCredentials;
  fCrm.OnBeforePopup := crmBeforePopup;
  fCrm.OnCertificateError := crmCertificateError;
  fCrm.OnConsoleMessage := crmConsoleMessage;
  fCrm.OnJsdialog := crmJsdialog;
  fCrm.OnBeforeDownload := crmBeforeDownload;
  fCrm.OnDownloadUpdated := crmDownloadUpdated;
  fCrm.OnGetResourceHandler := crmGetResourceHandler;
  fCrm.OnProcessMessageReceived := crmProcessMessageReceived;
{$IFDEF CANUSECEFLOADERROR}
  fCrm.OnLoadError := crmLoadError;
{$ENDIF}
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
  slp: TStringLoop;
  rheader, rvalue: string;
  reqown: TSpecialCEFReq;
  urlreq: ICefUrlRequest;
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
    data.AddElement(CreateCEFPOSTField(req.PostData));
    { postdata.AddElement(CreateCEFPOSTField('data.id=27'));
      postdata.AddElement(CreateCEFPOSTField('&data.title=title'));
      postdata.AddElement(CreateCEFPOSTField('&data.body=body')); }
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
  fCrm.visible := true;
end;

procedure TCatChromium.Load(const url: string);
begin
  ClearRequestData;
  // Better to use crm.Load() instead of:
  // if crm.Browser.GetMainFrame<>nil then crm.Browser.GetMainFrame.LoadURL(url);
  fCrm.Load(url);
  if fNeedRecreate then
    ReCreateBrowser(url);
  fCrm.visible := true;
end;

procedure TCatChromium.LoadBlank(const WaitLoad: Boolean = false);
begin
  ClearRequestData;
  fCrm.Load(cURL_HOME);
  fCrm.visible := true;
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

destructor TCatChromium.Destroy;
begin
  fMsg.Free;
  fInterceptRequests := false;
  OnAfterSetSource := nil;
  OnBrowserMessage := nil;
  NilComponentMethods(fCrm);
  fSplitter.Free;
  fDevTools.Free;
  fCrm.Free;
  fURLLog.Free;
  fResourceList.Free;
  fCriticalSection.Free;
  inherited Destroy;
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

end.
