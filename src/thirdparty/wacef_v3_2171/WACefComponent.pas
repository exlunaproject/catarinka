 (*
 *                       WaspAce Chromium Embedded 3
 *
 * Usage allowed under the restrictions of the Lesser GNU General Public License
 * or alternatively the restrictions of the Mozilla Public License 1.1
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
 * the specific language governing rights and limitations under the License.
 *
 * Unit owner : WaspAce.Architector <Architector.Waspace@gmail.com>
 * Web site   : http://www.waspace.net
 * Repository : https://bitbucket.org/WaspAce/wacef
 * Group      : http://groups.google.com/group/wacef
 *
 * The code of WACEF is based on DCEF3 code: http://groups.google.com/group/delphichromiumembedded
 *
 * Embarcadero Technologies, Inc is not permitted to use or redistribute
 * this source code without explicit permission.
 *
 *)

unit WACefComponent;
{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

{$I WACef.inc}
{$R wachromium.dcr}

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  {$IFDEF FPC}
  ctypes,
  LCLProc,
  LCLType,
  LCLIntf,
  {$IFDEF LCLGTK2}
  gdk2x,
  gtk2,
  gdk2,
  {$ENDIF}
  LResources,
  InterfaceBase,
  LMessages,
  WSLCLClasses,
  WSControls,
  Controls,
  Graphics,
  {$ELSE}
  Vcl.Controls,
  Vcl.Graphics,
  {$ENDIF}
  Messages,
  Classes,
  WACefGUI,
  WACefLib,
  WACefInterfaces,
  WACefTypes,
  SysUtils;

type
  TWACustomChromium = class(TWinControl, IChromiumEvents)
  private
    FCanvas: TCanvas;
    FHandler: ICefClient;
    FBrowser: ICefBrowser;
    FBrowserId: Integer;
    FDefaultUrl: ustring;

    FPopup: boolean;

    {ContextMenuHandler}
    FOnBeforeContextMenu: TOnBeforeContextMenu;
    FOnContextMenuCommand: TOnContextMenuCommand;
    FOnContextMenuDismissed: TOnContextMenuDismissed;
    {DisplayHandler}
    FOnAddressChange: TOnAddressChange;
    FOnTitleChange: TOnTitleChange;
    FOnTooltip: TOnTooltip;
    FOnStatusMessage: TOnStatusMessage;
    FOnConsoleMessage: TOnConsoleMessage;
    {DownloadHandler}
    FOnBeforeDownload: TOnBeforeDownload;
    FOnDownloadUpdated: TOnDownloadUpdated;
    {DragHandler}
    FOnDragEnter:TOnDragEnter;
    {FocusHandler}
    FOnTakeFocus: TOnTakeFocus;
    FOnSetFocus: TOnSetFocus;
    FOnGotFocus: TOnGotFocus;
    {KeyboardHandler}
    FOnPreKeyEvent: TOnPreKeyEvent;
    FOnKeyEvent: TOnKeyEvent;
    {JsDialogHandler}
    FOnJsdialog: TOnJsdialog;
    FOnBeforeUnloadDialog: TOnBeforeUnloadDialog;
    FOnResetDialogState: TOnResetDialogState;
    {GeolocationHandler}
    FOnRequestGeolocationPermission: TOnRequestGeolocationPermission;
    FOnCancelGeolocationPermission: TOnCancelGeolocationPermission;
    {LifeSpanHandler}
    FOnBeforePopup: TOnBeforePopup;
    FOnAfterCreated: TOnAfterCreated;
    FOnBeforeClose: TOnBeforeClose;
    FOnRunModal: TOnRunModal;
    FOnClose: TOnClose;
    {LoadHandler}
    FOnLoadingStateChange: TOnLoadingStateChange;
    FOnLoadStart: TOnLoadStart;
    FOnLoadEnd: TOnLoadEnd;
    FOnLoadError: TOnLoadError;
    {RenderHandler}
    FOnPopupShow:TOnPopupShow;
    FOnPopupSize:TOnPopupSize;
    FOnPaint:TOnPaint;
    FOnCursorChange:TOnCursorChange;
    FOnScrollOffsetChanged: TOnScrollOffsetChanged;
    {RequestHandler}
    FOnBeforeBrowse:TOnBeforeBrowse;
    FOnBeforeResourceLoad:TOnBeforeResourceLoad;
    FOnGetResourceHandler:TOnGetResourceHandler;
    FOnResourceRedirect:TOnResourceRedirect;
    FOnGetAuthCredentials:TOnGetAuthCredentials;
    FOnQuotaRequest:TOnQuotaRequest;
    FOnGetCookieManager: TOnGetCookieManager;
    FOnCertificateError:TOnCertificateError;
    FOnProtocolExecution: TOnProtocolExecution;
    FOnBeforePluginLoad: TOnBeforePluginLoad;
    FOnPluginCrashed: TOnPluginCrashed;
    FOnRenderProcessTerminated: TOnRenderProcessTerminated;
    {CompletionHandler}
    FOnTaskComplete:TOnTaskComplete;
    {DialogHandler}
    FOnFileDialog: TOnFileDialog;
    {CefClient}
    FOnProcessMessageReceived: TOnProcessMessageReceived;

    FOptions: TChromiumOptions;
    FUserStyleSheetLocation: ustring;
    FDefaultEncoding: ustring;
    FFontOptions: TChromiumFontOptions;

    FData: Pointer;

    procedure Init;
    procedure GetSettings(var settings: TCefBrowserSettings);
    procedure CreateBrowser;
  protected
    {$IFDEF MSWINDOWS}
    procedure WndProc(var Message: TMessage); override;
    procedure Resize; override;
    {$ENDIF}
    procedure Loaded; override;
    {$IFDEF FPC}
    procedure WMPaint(var Msg : TLMPaint); message LM_PAINT;
    procedure CreateWnd; override;
    {$ELSE}
    procedure WMPaint(var Msg : TWMPaint); message WM_PAINT;
    procedure CreateWindowHandle(const Params: TCreateParams); override;
    {$ENDIF}

    { CefClient }
    function doOnProcessMessageReceived(const Browser: ICefBrowser;
      sourceProcess: TCefProcessId; const message: ICefProcessMessage): Boolean;

    { CefContextMenuHandler }
    procedure doOnBeforeContextMenu(const browser: ICefBrowser; const frame: ICefFrame;
      const params: ICefContextMenuParams; const model: ICefMenuModel);
    function doOnContextMenuCommand(const browser: ICefBrowser; const frame: ICefFrame;
      const params: ICefContextMenuParams; commandId: Integer; eventFlags: TCefEventFlags): Boolean;
    procedure doOnContextMenuDismissed(const browser: ICefBrowser; const frame: ICefFrame);

    { CefDialogHandler }
    function doOnFileDialog(const Browser: ICefBrowser; mode: TCefFileDialogMode;
      const title, defaultFileName: ustring; acceptTypes: TStrings;
      const callback: ICefFileDialogCallback): Boolean;

    { CefDisplayHandler }
    procedure doOnAddressChange(const browser: ICefBrowser; const frame: ICefFrame; const url: ustring);
    procedure doOnTitleChange(const browser: ICefBrowser; const title: ustring);
    function doOnTooltip(const browser: ICefBrowser; var text: ustring): Boolean;
    procedure doOnStatusMessage(const browser: ICefBrowser; const value: ustring);
    function doOnConsoleMessage(const browser: ICefBrowser; const message, source: ustring; line: Integer): Boolean;

    { CefDownloadHandler }
    procedure doOnBeforeDownload(const Browser: ICefBrowser; const downloadItem: ICefDownloadItem;
      const suggestedName: ustring; const callback: ICefBeforeDownloadCallback);
    procedure doOnDownloadUpdated(const Browser: ICefBrowser; const downloadItem: ICefDownloadItem;
        const callback: ICefDownloadItemCallback);

    { CefDragHandler }
    function doOnDragEnter(const browser: ICefBrowser; const dragData: ICefDragData; mask: TCefDragOperationsMask): Boolean;

    { CefFocusHandler }
    procedure doOnTakeFocus(const Browser: ICefBrowser; next: Boolean);
    function doOnSetFocus(const Browser: ICefBrowser; Source: TCefFocusSource): Boolean;
    procedure doOnGotFocus(const Browser: ICefBrowser);

    { CefGeolocationHandler }
    procedure doOnRequestGeolocationPermission(const Browser: ICefBrowser;
      const requestingUrl: ustring; requestId: Integer; const callback: ICefGeolocationCallback);
    procedure doOnCancelGeolocationPermission(const Browser: ICefBrowser;
      const requestingUrl: ustring; requestId: Integer);

    { CefJsDialogHandler }
    function doOnJsdialog(const Browser: ICefBrowser; const originUrl, acceptLang: ustring;
      dialogType: TCefJsDialogType; const messageText, defaultPromptText: ustring;
      callback: ICefJsDialogCallback; out suppressMessage: Boolean): Boolean;
    function doOnBeforeUnloadDialog(const Browser: ICefBrowser; const messageText: ustring;
      isReload: Boolean; const callback: ICefJsDialogCallback): Boolean;
    procedure doOnResetDialogState(const Browser: ICefBrowser);

    { CefKeyboardHandler }
    function doOnPreKeyEvent(const Browser: ICefBrowser; const event: PCefKeyEvent;
      osEvent: TCefEventHandle; out isKeyboardShortcut: Boolean): Boolean;
    function doOnKeyEvent(const Browser: ICefBrowser; const event: PCefKeyEvent;
      osEvent: TCefEventHandle): Boolean;

    { CefLifeSpanHandler }
    function doOnBeforePopup(const Browser: ICefBrowser;
      const Frame: ICefFrame; const targetUrl, targetFrameName: ustring;
      var popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo;
      var client: ICefClient; var settings: TCefBrowserSettings;
      var noJavascriptAccess: Boolean): Boolean;
    procedure doOnAfterCreated(const Browser: ICefBrowser);
    procedure doOnBeforeClose(const Browser: ICefBrowser);
    function doOnRunModal(const Browser: ICefBrowser): Boolean;
    function doOnClose(const Browser: ICefBrowser): Boolean;

    { CefLoadHandler }
    procedure doOnLoadingStateChange(const browser: ICefBrowser; isLoading, canGoBack, canGoForward: Boolean);
    procedure doOnLoadStart(const browser: ICefBrowser; const frame: ICefFrame);
    procedure doOnLoadEnd(const browser: ICefBrowser; const frame: ICefFrame; httpStatusCode: Integer);
    procedure doOnLoadError(const browser: ICefBrowser; const frame: ICefFrame; errorCode: TCefErrorCode;
      const errorText, failedUrl: ustring);

    { CefRenderHandler }
    function doOnGetRootScreenRect(const browser: ICefBrowser; rect: PCefRect): Boolean;
    function doOnGetViewRect(const browser: ICefBrowser; rect: PCefRect): Boolean;
    function doOnGetScreenPoint(const browser: ICefBrowser; viewX, viewY: Integer;
      screenX, screenY: pcint): Boolean;
    function doOnGetScreenInfo(const browser: ICefBrowser; var screenInfo: TCefScreenInfo): Boolean;
    procedure doOnPopupShow(const browser: ICefBrowser; show: Boolean);
    procedure doOnPopupSize(const browser: ICefBrowser; const rect: PCefRect);
    procedure doOnPaint(const browser: ICefBrowser; kind: TCefPaintElementType;
      dirtyRectsCount: csize_t; const dirtyRects: PCefRectArray;
      const buffer: Pointer; width, height: Integer);
    procedure doOnCursorChange(const browser: ICefBrowser; cursor: TCefCursorHandle);
    procedure doOnScrollOffsetChanged(const browser: ICefBrowser);

    { CefRequestHandler }
    function doOnBeforeBrowse(const browser: ICefBrowser; const frame: ICefFrame;
      const request: ICefRequest; isRedirect: Boolean): Boolean;
    function doOnBeforeResourceLoad(const browser: ICefBrowser; const frame: ICefFrame;
      const request: ICefRequest): Boolean;
    function doOnGetResourceHandler(const browser: ICefBrowser; const frame: ICefFrame;
      const request: ICefRequest): ICefResourceHandler;
    procedure doOnResourceRedirect(const browser: ICefBrowser; const frame: ICefFrame;
      const oldUrl: ustring; var newUrl: ustring);
    function doOnGetAuthCredentials(const browser: ICefBrowser; const frame: ICefFrame;
      isProxy: Boolean; const host: ustring; port: Integer; const realm, scheme: ustring;
      const callback: ICefAuthCallback): Boolean;
    function doOnQuotaRequest(const browser: ICefBrowser; const originUrl: ustring;
      newSize: Int64; const callback: ICefQuotaCallback): Boolean;
    function doOnGetCookieManager(const browser: ICefBrowser; const mainUrl: ustring): ICefCookieManager;
    procedure doOnProtocolExecution(const browser: ICefBrowser; const url: ustring; out allowOsExecution: Boolean);
    function doOnCertificateError(certError: TCefErrorcode; const requestUrl: ustring;
      callback: ICefAllowCertificateErrorCallback): Boolean;
    function doOnBeforePluginLoad(const browser: ICefBrowser; const url, policyUrl: ustring;
      const info: ICefWebPluginInfo): Boolean;
    procedure doOnPluginCrashed(const browser: ICefBrowser; const plugin_path: ustring);
    procedure doOnRenderProcessTerminated(const browser: ICefBrowser; status: TCefTerminationStatus);

    procedure doOnTaskComplete;

    {ContextMenuHandler}
    property OnBeforeContextMenu: TOnBeforeContextMenu read FOnBeforeContextMenu write FOnBeforeContextMenu;
    property OnContextMenuCommand: TOnContextMenuCommand read FOnContextMenuCommand write FOnContextMenuCommand;
    property OnContextMenuDismissed: TOnContextMenuDismissed read FOnContextMenuDismissed write FOnContextMenuDismissed;
    {DisplayHandler}
    property OnAddressChange: TOnAddressChange read FOnAddressChange write FOnAddressChange;
    property OnTitleChange: TOnTitleChange read FOnTitleChange write FOnTitleChange;
    property OnTooltip: TOnTooltip read FOnTooltip write FOnTooltip;
    property OnStatusMessage: TOnStatusMessage read FOnStatusMessage write FOnStatusMessage;
    property OnConsoleMessage: TOnConsoleMessage read FOnConsoleMessage write FOnConsoleMessage;
    {DownloadHandler}
    property OnBeforeDownload: TOnBeforeDownload read FOnBeforeDownload write FOnBeforeDownload;
    property OnDownloadUpdated: TOnDownloadUpdated read FOnDownloadUpdated write FOnDownloadUpdated;
    {DragHandler}
    property OnDragEnter:TOnDragEnter read FOnDragEnter write FOnDragEnter;
    {FocusHandler}
    property OnTakeFocus: TOnTakeFocus read FOnTakeFocus write FOnTakeFocus;
    property OnSetFocus: TOnSetFocus read FOnSetFocus write FOnSetFocus;
    property OnGotFocus: TOnGotFocus read FOnGotFocus write FOnGotFocus;
    {KeyboardHandler}
    property OnPreKeyEvent: TOnPreKeyEvent read FOnPreKeyEvent write FOnPreKeyEvent;
    property OnKeyEvent: TOnKeyEvent read FOnKeyEvent write FOnKeyEvent;
    {JsDialogHandler}
    property OnJsdialog: TOnJsdialog read FOnJsdialog write FOnJsdialog;
    property OnBeforeUnloadDialog: TOnBeforeUnloadDialog read FOnBeforeUnloadDialog write FOnBeforeUnloadDialog;
    property OnResetDialogState: TOnResetDialogState read FOnResetDialogState write FOnResetDialogState;
    {GeolocationHandler}
    property OnRequestGeolocationPermission: TOnRequestGeolocationPermission read FOnRequestGeolocationPermission write FOnRequestGeolocationPermission;
    property OnCancelGeolocationPermission: TOnCancelGeolocationPermission read FOnCancelGeolocationPermission write FOnCancelGeolocationPermission;
    {LifeSpanHandler}
    property OnBeforePopup: TOnBeforePopup read FOnBeforePopup write FOnBeforePopup;
    property OnAfterCreated: TOnAfterCreated read FOnAfterCreated write FOnAfterCreated;
    property OnBeforeClose: TOnBeforeClose read FOnBeforeClose write FOnBeforeClose;
    property OnRunModal: TOnRunModal read FOnRunModal write FOnRunModal;
    property OnClose: TOnClose read FOnClose write FOnClose;
    {LoadHandler}
    property OnLoadingStateChange: TOnLoadingStateChange read FOnLoadingStateChange write FOnLoadingStateChange;
    property OnLoadStart: TOnLoadStart read FOnLoadStart write FOnLoadStart;
    property OnLoadEnd: TOnLoadEnd read FOnLoadEnd write FOnLoadEnd;
    property OnLoadError: TOnLoadError read FOnLoadError write FOnLoadError;
    {RenderHandler}
    property OnPopupShow:TOnPopupShow read FOnPopupShow write FOnPopupShow;
    property OnPopupSize:TOnPopupSize read FOnPopupSize write FOnPopupSize;
    property OnPaint:TOnPaint read FOnPaint write FOnPaint;
    property OnCursorChange:TOnCursorChange read FOnCursorChange write FOnCursorChange;
    {RequestHandler}
    property OnBeforeBrowse:TOnBeforeBrowse read FOnBeforeBrowse write FOnBeforeBrowse;
    property OnBeforeResourceLoad: TOnBeforeResourceLoad read FOnBeforeResourceLoad write FOnBeforeResourceLoad;
    property OnGetResourceHandler: TOnGetResourceHandler read FOnGetResourceHandler write FOnGetResourceHandler;
    property OnResourceRedirect: TOnResourceRedirect read FOnResourceRedirect write FOnResourceRedirect;
    property OnGetAuthCredentials: TOnGetAuthCredentials read FOnGetAuthCredentials write FOnGetAuthCredentials;
    property OnQuotaRequest: TOnQuotaRequest read FOnQuotaRequest write FOnQuotaRequest;
    property OnCertificateError: TOnCertificateError read FOnCertificateError write FOnCertificateError;
    property OnProtocolExecution: TOnProtocolExecution read FOnProtocolExecution write FOnProtocolExecution;
    property OnBeforePluginLoad: TOnBeforePluginLoad read FOnBeforePluginLoad write FOnBeforePluginLoad;
    property OnPluginCrashed: TOnPluginCrashed read FOnPluginCrashed write FOnPluginCrashed;
    property OnRenderProcessTerminated: TOnRenderProcessTerminated read FOnRenderProcessTerminated write FOnRenderProcessTerminated;
    {CompletionHandler}
    property OnTaskComplete: TOnTaskComplete read FOnTaskComplete write FOnTaskComplete;
    {DialogHandler}
    property OnFileDialog: TOnFileDialog read FOnFileDialog write FOnFileDialog;
    {CefClient}
    property OnProcessMessageReceived: TOnProcessMessageReceived read FOnProcessMessageReceived write FOnProcessMessageReceived;

    property DefaultUrl: ustring read FDefaultUrl write FDefaultUrl;
    property Options: TChromiumOptions read FOptions write FOptions;
    property FontOptions: TChromiumFontOptions read FFontOptions;
    property DefaultEncoding: ustring read FDefaultEncoding write FDefaultEncoding;
    property UserStyleSheetLocation: ustring read FUserStyleSheetLocation write FUserStyleSheetLocation;
    property BrowserId: Integer read FBrowserId;
    property Browser: ICefBrowser read FBrowser;
    property Data: Pointer read FData write FData;
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateAsPopup(AOwner: TComponent);
    destructor Destroy; override;
    procedure Load(const url: ustring);
    property Handler:ICefClient read FHandler;
  end;

  TWAChromium = class(TWACustomChromium)
  public
    property BrowserId;
    property Browser;
    property Data;
  published
    property Color;
    property Constraints;
    property TabStop;
    property Align;
    property Anchors;
    property DefaultUrl;
    property TabOrder;
    property Visible;

    {ContextMenuHandler}
    property OnBeforeContextMenu;
    property OnContextMenuCommand;
    property OnContextMenuDismissed;
    {DisplayHandler}
    property OnAddressChange;
    property OnTitleChange;
    property OnTooltip;
    property OnStatusMessage;
    property OnConsoleMessage;
    {DownloadHandler}
    property OnBeforeDownload;
    property OnDownloadUpdated;
    {FocusHandler}
    property OnTakeFocus;
    property OnSetFocus;
    property OnGotFocus;
    {KeyboardHandler}
    property OnPreKeyEvent;
    property OnKeyEvent;
    {JsDialogHandler}
    property OnJsdialog;
    property OnBeforeUnloadDialog;
    property OnResetDialogState;
    {GeolocationHandler}
    property OnRequestGeolocationPermission;
    property OnCancelGeolocationPermission;
    {LifeSpanHandler}
    property OnBeforePopup;
    property OnAfterCreated;
    property OnBeforeClose;
    property OnRunModal;
    property OnClose;
    {LoadHandler}
    property OnLoadingStateChange;
    property OnLoadStart;
    property OnLoadEnd;
    property OnLoadError;
    {RenderHandler}
    property OnPopupShow;
    property OnPopupSize;
    property OnPaint;
    property OnCursorChange;
    {RequestHandler}
    property OnBeforeBrowse;
    property OnBeforeResourceLoad;
    property OnGetResourceHandler;
    property OnResourceRedirect;
    property OnGetAuthCredentials;
    property OnQuotaRequest;
    property OnCertificateError;
    property OnProtocolExecution;
    property OnBeforePluginLoad;
    property OnPluginCrashed;
    property OnRenderProcessTerminated;
    {CompletionHandler}
    property OnTaskComplete;
    {DialogHandler}
    property OnFileDialog;
    {CefClient}
    property OnProcessMessageReceived;

    property Options;
    property FontOptions;
    property DefaultEncoding;
    property UserStyleSheetLocation;

    property Handler;
  end;

procedure Register;

implementation

{$IFNDEF CEF_MULTI_THREADED_MESSAGE_LOOP}
uses
  {$IFDEF FPC}
  ExtCtrls;
  {$ELSE}
	Vcl.ExtCtrls;
  {$ENDIF}
var
  Timer:TTimer;
  CefInstances:Integer = 0;
  Looping:boolean;
{$ENDIF}

procedure Register;
begin
  RegisterComponents('WAChromium',[TWAChromium]);
end;

type
  TWACefClientHandler = class(TCustomClientHandler)
  private
   	class procedure OnTimer(Sender:TObject);
  public
    constructor Create(const crm: IChromiumEvents); override;
    destructor Destroy; override;
    procedure StartTimer;
  end;

{ TVCLClientHandler }

class procedure TWACefClientHandler.OnTimer(Sender : TObject);
begin
  {$IFNDEF CEF_MULTI_THREADED_MESSAGE_LOOP}
  If Looping then
  	Exit;
  If CefInstances > 0 then
  begin
    Looping := True;
    try
      TWACef.DoMessageLoopWork;
    finally
      Looping := False;
    end;
  end;
  {$ENDIF}
end;

constructor TWAcefClientHandler.Create(const crm: IChromiumEvents);
begin
  inherited Create(crm);
{$IFNDEF CEF_MULTI_THREADED_MESSAGE_LOOP}
  If not assigned(Timer) then
    begin
      Timer:=TTimer.Create(nil);
      Timer.Interval:=15;
      Timer.Enabled:=false;
      Timer.OnTimer:=OnTimer;
    end;
  InterlockedIncrement(CefInstances);
{$ENDIF}
end;

destructor TWACefClientHandler.Destroy;
begin
{$IFNDEF CEF_MULTI_THREADED_MESSAGE_LOOP}
  InterlockedDecrement(CefInstances);
  if CefInstances = 0 then
    Timer.Enabled:=false;
{$ENDIF}
  inherited;
end;

procedure TWACefClientHandler.StartTimer;
begin
  {$IFNDEF CEF_MULTI_THREADED_MESSAGE_LOOP}
	If not Assigned(Timer) then
  	Exit;
  Timer.Enabled := true;
  {$ENDIF}
end;

//..............................................................................TWACustomChromium
{Private section}
procedure TWACustomChromium.Init;
begin
  FDefaultUrl := 'about:blank';

  if not (csDesigning in ComponentState) then
  	begin
      TWACef.Initialize;
	    FHandler := TWACefClientHandler.Create(Self);
		  If not Assigned(FHandler) then
		  	raise	Exception.Create('FHandler is nil');
	  end
    else
    begin
      FCanvas := TControlCanvas.Create;
      TControlCanvas(FCanvas).Control := Self;
    end;

  FOptions := TChromiumOptions.Create;
  FFontOptions := TChromiumFontOptions.Create;

  FUserStyleSheetLocation := '';
  FDefaultEncoding := '';
  FBrowserId := 0;
  FBrowser := nil;
  FData := nil;
end;

procedure TWACustomChromium.GetSettings(var settings: TCefBrowserSettings);
begin
  Assert(settings.size >= SizeOf(settings));

  settings.standard_font_family := TWACef.ToCefString(FFontOptions.StandardFontFamily);
  settings.fixed_font_family := TWACef.ToCefString(FFontOptions.FixedFontFamily);
  settings.serif_font_family := TWACef.ToCefString(FFontOptions.SerifFontFamily);
  settings.sans_serif_font_family := TWACef.ToCefString(FFontOptions.SansSerifFontFamily);
  settings.cursive_font_family := TWACef.ToCefString(FFontOptions.CursiveFontFamily);
  settings.fantasy_font_family := TWACef.ToCefString(FFontOptions.FantasyFontFamily);
  settings.default_font_size := FFontOptions.DefaultFontSize;
  settings.default_fixed_font_size := FFontOptions.DefaultFixedFontSize;
  settings.minimum_font_size := FFontOptions.MinimumFontSize;
  settings.minimum_logical_font_size := FFontOptions.MinimumLogicalFontSize;
  settings.remote_fonts := FFontOptions.RemoteFonts;
  settings.default_encoding := TWACef.ToCefString(DefaultEncoding);

  settings.windowless_frame_rate := FOptions.WindowlessFrameRate;
  settings.remote_fonts := FOptions.RemoteFonts;
  settings.javascript := FOptions.Javascript;
  settings.javascript_open_windows := FOptions.JavascriptOpenWindows;
  settings.javascript_close_windows := FOptions.JavascriptCloseWindows;
  settings.javascript_access_clipboard := FOptions.JavascriptAccessClipboard;
  settings.javascript_dom_paste := FOptions.JavascriptDomPaste;
  settings.caret_browsing := FOptions.CaretBrowsing;
  settings.java := FOptions.Java;
  settings.plugins := FOptions.Plugins;
  settings.universal_access_from_file_urls := FOptions.UniversalAccessFromFileUrls;
  settings.file_access_from_file_urls := FOptions.FileAccessFromFileUrls;
  settings.web_security := FOptions.WebSecurity;
  settings.image_loading := FOptions.ImageLoading;
  settings.image_shrink_standalone_to_fit := FOptions.ImageShrinkStandaloneToFit;
  settings.text_area_resize := FOptions.TextAreaResize;
  settings.tab_to_links := FOptions.TabToLinks;
  settings.local_storage := FOptions.LocalStorage;
  settings.databases := FOptions.Databases;
  settings.application_cache := FOptions.ApplicationCache;
  settings.webgl := FOptions.Webgl;
  settings.background_color := FOptions.BackgroundColor;
end;

procedure TWACustomChromium.CreateBrowser;
var
  info: TCefWindowInfo;
  settings: TCefBrowserSettings;
  rect: TRect;
  hn: HWND;
begin
  if not (csDesigning in ComponentState) and
    (not FPopup) then
  begin
    FillChar(info, SizeOf(info), 0);
    rect := GetClientRect;
    info.x := rect.left;
    info.y := rect.top;
    info.Width := rect.right-rect.left;
    info.Height := rect.bottom-rect.top;
    {$IFDEF MSWINDOWS}
    info.Style:=
      WS_CHILD or
      WS_VISIBLE or
      WS_CLIPCHILDREN or
      WS_CLIPSIBLINGS or
      WS_TABSTOP;
    info.parent_window:=Handle;
    info.ex_style:=0;
    {$ENDIF}
    {$IFDEF LINUX}
    info.parent_window := Pointer(GDK_WINDOW_XID(PGtkWidget(Parent.Handle)^.window));
    {$ENDIF}
    FillChar(settings, SizeOf(TCefBrowserSettings), 0);
    settings.size := SizeOf(TCefBrowserSettings);
    GetSettings(settings);
    {$IFDEF CEF_MULTI_THREADED_MESSAGE_LOOP}
    TWACef.BrowserHostCreateBrowser(@info,FHandler,FDefaultUrl,@settings,nil);
    {$ELSE}
    FBrowser:=TWACef.BrowserHostCreateSync(@info, FHandler, '', @settings, nil);
		if FBrowser<>nil then
			FBrowserId:=FBrowser.Identifier;
{$ENDIF}
		(FHandler as TWACefClientHandler).StartTimer;
  end;
end;

{Protected section}
{$IFDEF MSWINDOWS}
procedure TWACustomChromium.WndProc(var Message: TMessage);
begin
  case Message.Msg of
    WM_SETFOCUS:
      begin
        if (FBrowser <> nil) and (FBrowser.Host.WindowHandle <> 0) then
          PostMessage(FBrowser.Host.WindowHandle, WM_SETFOCUS, Message.WParam, 0);
        inherited WndProc(Message);
      end;
    WM_ERASEBKGND:
      if (csDesigning in ComponentState) or (FBrowser = nil) then
        inherited WndProc(Message);
    CM_WANTSPECIALKEY:
      if not (TWMKey(Message).CharCode in [VK_LEFT .. VK_DOWN]) then
        Message.Result := 1 else
        inherited WndProc(Message);
    WM_GETDLGCODE:
      Message.Result := DLGC_WANTARROWS or DLGC_WANTCHARS;
  else
    inherited WndProc(Message);
  end;
end;

procedure TWACustomChromium.Resize;
var
  brws:ICefBrowser;
  rect:TRect;
  hdwp:THandle;
begin
  inherited;
  if not (csDesigning in ComponentState) then
  begin
    brws:=FBrowser;
    if (brws<>nil)and
      (brws.Host.WindowHandle<>INVALID_HANDLE_VALUE) then
    begin
      rect:=GetClientRect;
      hdwp:=BeginDeferWindowPos(1);
      try
        hdwp:=DeferWindowPos(
          hdwp,
          brws.Host.WindowHandle,
          0,
          rect.left,
          rect.top,
          rect.right-rect.left,
          rect.bottom-rect.top,
          SWP_NOZORDER
        );
      finally
        EndDeferWindowPos(hdwp);
      end;
    end;
  end;
end;
{$ENDIF}

procedure TWACustomChromium.Loaded;
begin
  inherited Loaded;
  Load(FDefaultUrl);
end;

{$IFDEF FPC}
procedure TWACustomChromium.WMPaint(var Msg : TLMPaint);
begin
  Include(FControlState, csCustomPaint);
  inherited WMPaint(Msg);
  if (csDesigning in ComponentState) and (FCanvas <> nil) then
    with FCanvas do
    begin
      If Msg.DC <> 0 then
        Handle := Msg.DC;
      Brush.Color := clWhite;
      Pen.Color   := clWhite;
      Rectangle(0,0,Self.Width,Self.Height);
      If Msg.DC <> 0 then
        Handle := 0;
    end;
  Exclude(FControlState, csCustomPaint);
end;

procedure TWACustomChromium.CreateWnd;
begin
  inherited CreateWnd;
  CreateBrowser;
end;

{$ELSE}
procedure TWACustomChromium.WMPaint(var Msg : TWMPaint);
begin
  inherited;
  if (csDesigning in ComponentState) and (FCanvas <> nil) then
    with FCanvas do
    begin
      If Msg.DC <> 0 then
        Handle := Msg.DC;
      Brush.Color := clWhite;
      Pen.Color   := clWhite;
      Rectangle(0,0,Self.Width,Self.Height);
      If Msg.DC <> 0 then
        Handle := 0;
    end;
end;

procedure TWACustomChromium.CreateWindowHandle(const Params: TCreateParams);
begin
  inherited;
  CreateBrowser;
end;
{$ENDIF}

{...............................................................................ContextMenuHandler}
procedure TWACustomChromium.doOnBeforeContextMenu(const browser: ICefBrowser;
  const frame: ICefFrame; const params: ICefContextMenuParams;
  const model: ICefMenuModel);
begin
  if Assigned(FOnBeforeContextMenu) then
    FOnBeforeContextMenu(Self, browser, frame, params, model);
end;

function TWACustomChromium.doOnContextMenuCommand(const browser: ICefBrowser;
  const frame: ICefFrame; const params: ICefContextMenuParams;
  commandId: Integer; eventFlags: TCefEventFlags): Boolean;
begin
  Result := False;
  if Assigned(FOnContextMenuCommand) then
    FOnContextMenuCommand(Self, browser, frame, params, commandId, eventFlags, Result);
end;

procedure TWACustomChromium.doOnContextMenuDismissed(const browser: ICefBrowser;
  const frame: ICefFrame);
begin
  if Assigned(FOnContextMenuDismissed) then
    FOnContextMenuDismissed(Self, browser, frame);
end;

{...............................................................................DisplayHandler}
procedure TWACustomChromium.doOnAddressChange(const browser: ICefBrowser;
  const frame: ICefFrame; const url: ustring);
begin
  if Assigned(FOnAddressChange) then
    FOnAddressChange(Self, browser, frame, url);
end;

procedure TWACustomChromium.doOnTitleChange(const browser: ICefBrowser;
  const title: ustring);
begin
  if Assigned(FOnTitleChange) then
    FOnTitleChange(Self, browser, title);
end;

function TWACustomChromium.doOnTooltip(const browser: ICefBrowser;
  var text: ustring): Boolean;
begin
  Result := False;
  if Assigned(FOnTooltip) then
    FOnTooltip(Self, browser, text, Result);
end;

procedure TWACustomChromium.doOnStatusMessage(const browser: ICefBrowser;
  const value: ustring);
begin
  if Assigned(FOnStatusMessage) then
    FOnStatusMessage(Self, browser, value);
end;

function TWACustomChromium.doOnConsoleMessage(const browser: ICefBrowser;
  const message, source: ustring; line: Integer): Boolean;
begin
  Result := False;
  if Assigned(FOnConsoleMessage) then
    FOnConsoleMessage(Self, browser, message, source, line, Result);
end;

{...............................................................................DownloadHandler}
procedure TWACustomChromium.doOnBeforeDownload(const browser: ICefBrowser;
  const downloadItem: ICefDownloadItem; const suggestedName: ustring;
  const callback: ICefBeforeDownloadCallback);
begin
  if Assigned(FOnBeforeDownload) then
    FOnBeforeDownload(Self, browser, downloadItem, suggestedName, callback);
end;

procedure TWACustomChromium.doOnDownloadUpdated(const browser: ICefBrowser;
  const downloadItem: ICefDownloadItem;
  const callback: ICefDownloadItemCallback);
begin
  if Assigned(FOnDownloadUpdated) then
    FOnDownloadUpdated(Self, browser, downloadItem, callback);
end;

{...............................................................................DragHandler}
function TWACustomChromium.doOnDragEnter(const browser:ICefBrowser; const dragData:ICefDragData;
  mask:TCefDragOperationsMask):Boolean;
begin
  if Assigned(FOnDragEnter) then
    FOnDragEnter(Self,browser,dragData,mask,result);
end;

{...............................................................................FocusHandler}
procedure TWACustomChromium.doOnTakeFocus(const browser: ICefBrowser;
  next: Boolean);
begin
  if Assigned(FOnTakeFocus) then
    FOnTakeFocus(Self, browser, next);
end;

function TWACustomChromium.doOnSetFocus(const browser: ICefBrowser;
  source: TCefFocusSource): Boolean;
begin
  Result := False;
  if Assigned(FOnSetFocus) then
    FOnSetFocus(Self, browser, source, Result);
end;

procedure TWACustomChromium.doOnGotFocus(const browser: ICefBrowser);
begin
  if Assigned(FOnGotFocus) then
    FOnGotFocus(Self, browser)
end;

{...............................................................................KeyboardHandler}
function TWACustomChromium.doOnPreKeyEvent(const browser: ICefBrowser;
  const event: PCefKeyEvent; osEvent: TCefEventHandle;
  out isKeyboardShortcut: Boolean): Boolean;
begin
  Result := False;
  if Assigned(FOnPreKeyEvent) then
    FOnPreKeyEvent(Self, browser, event, osEvent, isKeyboardShortcut, Result);
end;

function TWACustomChromium.doOnKeyEvent(const browser: ICefBrowser;
  const event: PCefKeyEvent; osEvent: TCefEventHandle): Boolean;
begin
  Result := False;
  if Assigned(FOnKeyEvent) then
    FOnKeyEvent(Self, browser, event, osEvent, Result);
end;

{...............................................................................JsDialogHandler}
function TWACustomChromium.doOnJsdialog(const browser: ICefBrowser;
  const originUrl, acceptLang: ustring; dialogType: TCefJsDialogType;
  const messageText, defaultPromptText: ustring; callback: ICefJsDialogCallback;
  out suppressMessage: Boolean): Boolean;
begin
  Result := False;
  if Assigned(FOnJsdialog) then
    FOnJsdialog(Self, browser, originUrl, acceptLang, dialogType,
      messageText, defaultPromptText, callback, suppressMessage, Result);
end;

function TWACustomChromium.doOnBeforeUnloadDialog(const browser: ICefBrowser;
  const messageText: ustring; isReload: Boolean;
  const callback: ICefJsDialogCallback): Boolean;
begin
  Result := False;
  if Assigned(FOnBeforeUnloadDialog) then
    FOnBeforeUnloadDialog(Self, browser, messageText, isReload, callback, Result);
end;

procedure TWACustomChromium.doOnResetDialogState(const browser: ICefBrowser);
begin
  if Assigned(FOnResetDialogState) then
    FOnResetDialogState(Self, browser);
end;

{...............................................................................GeolocationHandler}
procedure TWACustomChromium.doOnRequestGeolocationPermission(
  const browser: ICefBrowser; const requestingUrl: ustring; requestId: Integer;
  const callback: ICefGeolocationCallback);
begin
  if Assigned(FOnRequestGeolocationPermission) then
    FOnRequestGeolocationPermission(Self, browser, requestingUrl, requestId, callback);
end;

procedure TWACustomChromium.doOnCancelGeolocationPermission(
  const browser: ICefBrowser; const requestingUrl: ustring; requestId: Integer);
begin
  if Assigned(FOnCancelGeolocationPermission) then
    FOnCancelGeolocationPermission(Self, browser, requestingUrl, requestId);
end;

{...............................................................................LifeSpanHandler}
function TWACustomChromium.doOnBeforePopup(const Browser: ICefBrowser;
      const Frame: ICefFrame; const targetUrl, targetFrameName: ustring;
      var popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo;
      var client: ICefClient; var settings: TCefBrowserSettings;
      var noJavascriptAccess: Boolean): Boolean;
begin
  Result := False;
  if Assigned(FOnBeforePopup) then
    FOnBeforePopup(
      Self,
      Browser,
      frame,
      targetUrl,
      targetFrameName,
      popupFeatures,
      windowInfo,
      client,
      settings,
      noJavascriptAccess,
      Result
    );
end;

procedure TWACustomChromium.doOnAfterCreated(const browser: ICefBrowser);
begin
  {$IFDEF CEF_MULTI_THREADED_MESSAGE_LOOP}
  FBrowser:=browser;
  {$ENDIF}
  if FPopup then
    FBrowser := browser;
  if Assigned(FOnAfterCreated) then
    FOnAfterCreated(Self, browser);
end;

procedure TWACustomChromium.doOnBeforeClose(const browser: ICefBrowser);
begin
  if Assigned(FOnBeforeClose) then
    FOnBeforeClose(Self, browser);
end;

function TWACustomChromium.doOnRunModal(const browser: ICefBrowser): Boolean;
begin
  Result := False;
  if Assigned(FOnRunModal) then
    FOnRunModal(Self, browser, Result);
end;

function TWACustomChromium.doOnClose(const browser: ICefBrowser): Boolean;
begin
  Result := False;
  if Assigned(FOnClose) then
    FOnClose(Self, browser, Result);
end;

{...............................................................................LoadHandler}
procedure TWACustomChromium.doOnLoadingStateChange(const browser: ICefBrowser;
  isLoading, canGoBack, canGoForward: Boolean);
begin
  if Assigned(FOnLoadingStateChange) then
    FOnLoadingStateChange(Self, browser, isLoading, canGoBack, canGoForward);
end;

procedure TWACustomChromium.doOnLoadStart(const browser: ICefBrowser;
  const frame: ICefFrame);
begin
  if Assigned(FOnLoadStart) then
    FOnLoadStart(Self, browser, frame);
end;

procedure TWACustomChromium.doOnLoadEnd(const browser: ICefBrowser;
  const frame: ICefFrame; httpStatusCode: Integer);
begin
  if Assigned(FOnLoadEnd) then
    FOnLoadEnd(Self, browser, frame, httpStatusCode);
end;

procedure TWACustomChromium.doOnLoadError(const browser: ICefBrowser;
  const frame: ICefFrame; errorCode: TCefErrorCode; const errorText,
  failedUrl: ustring);
begin
  if Assigned(FOnLoadError) then
    FOnLoadError(Self, browser, frame, errorCode, errorText, failedUrl);
end;

{...............................................................................RenderHandler}
function TWACustomChromium.doOnGetRootScreenRect(const browser: ICefBrowser;
  rect: PCefRect): Boolean;
begin
  Result := False;
end;

function TWACustomChromium.doOnGetViewRect(const browser: ICefBrowser;
  rect: PCefRect): Boolean;
begin
  Result := False;
end;

function TWACustomChromium.doOnGetScreenPoint(const browser: ICefBrowser; viewX, viewY: Integer;
      screenX, screenY: pcint): Boolean;
begin
  Result := False;
end;

function TWACustomChromium.doOnGetScreenInfo(const browser: ICefBrowser; var screenInfo: TCefScreenInfo): Boolean;
begin
  Result := false;
end;

procedure TWACustomChromium.doOnPopupShow(const browser: ICefBrowser;
  show: Boolean);
begin

end;

procedure TWACustomChromium.doOnPopupSize(const browser: ICefBrowser;
  const rect: PCefRect);
begin

end;

procedure TWACustomChromium.doOnPaint(const browser: ICefBrowser;
  kind: TCefPaintElementType; dirtyRectsCount: csize_t;
  const dirtyRects: PCefRectArray; const buffer: Pointer; width, height: Integer);
begin

end;

procedure TWACustomChromium.doOnCursorChange(const browser: ICefBrowser;
  cursor: TCefCursorHandle);
begin

end;

procedure TWACustomChromium.doOnScrollOffsetChanged(const browser : ICefBrowser);
begin
  If Assigned(FOnScrollOffsetChanged) then
    FOnScrollOffsetChanged(Self, browser);
end;

{...............................................................................RequestHandler}
function TWACustomChromium.doOnBeforeBrowse(const browser: ICefBrowser; const frame: ICefFrame;
  const request: ICefRequest; IsRedirect:boolean):Boolean;
begin
  Result:=False;
  if Assigned(FOnBeforeBrowse) then
    FOnBeforeBrowse(Self,browser,frame,request,IsRedirect,Result);
end;

function TWACustomChromium.doOnBeforeResourceLoad(const browser: ICefBrowser;
  const frame: ICefFrame; const request: ICefRequest): Boolean;
begin
  Result := False;
  if Assigned(FOnBeforeResourceLoad) then
    FOnBeforeResourceLoad(Self, browser, frame, request, Result);
end;

function TWACustomChromium.doOnGetResourceHandler(const browser: ICefBrowser;
  const frame: ICefFrame; const request: ICefRequest): ICefResourceHandler;
begin
  if Assigned(FOnGetResourceHandler) then
    FOnGetResourceHandler(Self, browser, frame, request, Result) else
    Result := nil;
end;

procedure TWACustomChromium.doOnResourceRedirect(const browser: ICefBrowser;
  const frame: ICefFrame; const oldUrl: ustring; var newUrl: ustring);
begin
  if Assigned(FOnResourceRedirect) then
    FOnResourceRedirect(Self, browser, frame, oldUrl, newUrl);
end;

function TWACustomChromium.doOnGetAuthCredentials(const browser: ICefBrowser;
  const frame: ICefFrame; isProxy: Boolean; const host: ustring; port: Integer;
  const realm, scheme: ustring; const callback: ICefAuthCallback): Boolean;
begin
  Result := False;
  if Assigned(FOnGetAuthCredentials) then
    FOnGetAuthCredentials(Self, browser, frame, isProxy, host,
      port, realm, scheme, callback, Result);
end;

function TWACustomChromium.doOnQuotaRequest(const browser: ICefBrowser;
  const originUrl: ustring; newSize: Int64;
  const callback: ICefQuotaCallback): Boolean;
begin
  Result := False;
  if Assigned(FOnQuotaRequest) then
    FOnQuotaRequest(Self, browser, originUrl, newSize, callback, Result);
end;

function TWACustomChromium.doOnGetCookieManager(const Browser: ICefBrowser;
  const mainUrl: ustring): ICefCookieManager;
begin
  if Assigned(FOnGetCookieManager) then
    FOnGetCookieManager(Self, Browser, mainUrl, Result)
  else
    Result := nil;
end;

function TWACustomChromium.doOnCertificateError(certError: TCefErrorcode; const requestUrl: ustring;
      callback: ICefAllowCertificateErrorCallback): Boolean;
begin
  Result:=False;
  if Assigned(FOnCertificateError) then
    FOnCertificateError(Self, CertError, requestUrl, callback, Result);
end;

procedure TWACustomChromium.doOnProtocolExecution(const browser: ICefBrowser;
  const url: ustring; out allowOsExecution: Boolean);
begin
  if Assigned(FOnProtocolExecution) then
    FOnProtocolExecution(Self, browser, url, allowOsExecution);
end;

function TWACustomChromium.doOnBeforePluginLoad(const browser: ICefBrowser;
  const url, policyUrl: ustring; const info: ICefWebPluginInfo): Boolean;
begin
  Result := False;
  if Assigned(FOnBeforePluginLoad) then
    FOnBeforePluginLoad(Self, browser, url, policyUrl, info, Result);
end;

procedure TWACustomChromium.doOnPluginCrashed(const browser: ICefBrowser; const plugin_path: ustring);
begin
  if Assigned(FOnPluginCrashed) then
    FOnPluginCrashed(Self, browser, plugin_path);
end;

procedure TWACustomChromium.doOnRenderProcessTerminated(const browser: ICefBrowser;
  status: TCefTerminationStatus);
begin
  if Assigned(FOnRenderProcessTerminated) then
    FOnRenderProcessTerminated(Self, browser, status);
end;

{...............................................................................CompletionHandler}
procedure TWACustomChromium.doOnTaskComplete;
begin
  if Assigned(FOnTaskComplete) then
    FOnTaskComplete(Self);
end;

{...............................................................................DialogHandler}
function TWACustomChromium.doOnFileDialog(const browser: ICefBrowser;
  mode: TCefFileDialogMode; const title, defaultFileName: ustring;
  acceptTypes: TStrings; const callback: ICefFileDialogCallback): Boolean;
begin
  Result := False;
  if Assigned(FOnFileDialog) then
    FOnFileDialog(Self, browser, mode, title, defaultFileName, acceptTypes,
      callback, Result);
end;

{...............................................................................CefClient}
function TWACustomChromium.doOnProcessMessageReceived(const browser: ICefBrowser;
  sourceProcess: TCefProcessId; const message: ICefProcessMessage): Boolean;
begin
  Result := False;
  if Assigned(FOnProcessMessageReceived) then
    FOnProcessMessageReceived(Self, browser, sourceProcess, message, Result);
end;

{Private section}
constructor TWACustomChromium.Create(AOwner: TComponent);
begin
  FPopup := false;
  inherited;
  Init;
end;

constructor TWACustomChromium.CreateAsPopup(AOwner: TComponent);
begin
  FPopup := true;
  inherited Create(AOwner);
  Init;
end;

destructor TWACustomChromium.Destroy;
begin
  if Assigned(FCanvas) then
    FreeAndNil(FCanvas);
{  if FBrowser <> nil then
    FBrowser.Host.CloseBrowser(false);}
  if FHandler <> nil then
    (FHandler as ICefClientHandler).Disconnect;
  FHandler := nil;
  FBrowser := nil;
  FFontOptions.Free;
  FOptions.Free;
  inherited;
end;

procedure TWACustomChromium.Load(const url: ustring);
var
  frm: ICefFrame;
begin
  HandleNeeded;
  if FBrowser <> nil then
  begin
    frm := FBrowser.MainFrame;
    if frm <> nil then
      frm.LoadUrl(url);
  end;
end;

initialization

end.
