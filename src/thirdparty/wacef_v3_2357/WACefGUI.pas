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

unit WACefGUI;

{$I WACef.inc}

interface

uses
  {$IFDEF FPC}
  ctypes,
  {$ENDIF}
  Classes,
  WACefLib,
  WACefInterfaces,
  WACefTypes,
  WACefOwns;

type
  { Client }
  TOnProcessMessageReceived = procedure(Sender: TObject; const Browser: ICefBrowser;
    sourceProcess: TCefProcessId; const message: ICefProcessMessage; out Result: Boolean) of object;

  { ContextMenuHandler }
  TOnBeforeContextMenu = procedure(Sender: TObject; const Browser: ICefBrowser; const Frame: ICefFrame;
    const params: ICefContextMenuParams; const model: ICefMenuModel) of object;
  TOnContextMenuCommand = procedure(Sender: TObject; const Browser: ICefBrowser; const Frame: ICefFrame;
    const params: ICefContextMenuParams; commandId: cint;
    eventFlags: TCefEventFlags; out Result: Boolean) of object;
  TOnContextMenuDismissed = procedure(Sender: TObject; const Browser: ICefBrowser; const Frame: ICefFrame) of object;

  { DialogHandler }
  TOnFileDialog = procedure(Sender: TObject; const Browser: ICefBrowser;
    mode: TCefFileDialogMode; const title, defaultFileName: ustring;
    acceptTypes: TStrings; const callback: ICefFileDialogCallback;
    out Result: Boolean) of object;

  {CompletionHandler}
  TOnTaskComplete=procedure(Sender: TObject) of object;

  { DisplayHandler }
  TOnAddressChange = procedure(Sender: TObject; const Browser: ICefBrowser; const Frame: ICefFrame; const url: ustring) of object;
  TOnTitleChange = procedure(Sender: TObject; const Browser: ICefBrowser; const title: ustring) of object;
  TOnTooltip = procedure(Sender: TObject; const Browser: ICefBrowser; var text: ustring; out Result: Boolean) of object;
  TOnStatusMessage = procedure(Sender: TObject; const Browser: ICefBrowser; const value: ustring) of object;
  TOnConsoleMessage = procedure(Sender: TObject; const Browser: ICefBrowser; const message, Source: ustring; line: cint; out Result: Boolean) of object;

  { DownloadHandler }
  TOnBeforeDownload = procedure(Sender: TObject; const Browser: ICefBrowser; const downloadItem: ICefDownloadItem;
    const suggestedName: ustring; const callback: ICefBeforeDownloadCallback) of object;
  TOnDownloadUpdated = procedure(Sender: TObject; const Browser: ICefBrowser; const downloadItem: ICefDownloadItem;
      const callback: ICefDownloadItemCallback) of object;

  { DragHandler }
  TOnDragEnter = procedure(Sender: TObject; const Browser: ICefBrowser; const dragData: ICefDragData; mask: TCefDragOperationsMask; out Result: Boolean) of object;

  { FocusHandler }
  TOnTakeFocus = procedure(Sender: TObject; const Browser: ICefBrowser; next: Boolean) of object;
  TOnSetFocus = procedure(Sender: TObject; const Browser: ICefBrowser; Source: TCefFocusSource; out Result: Boolean) of object;
  TOnGotFocus = procedure(Sender: TObject; const Browser: ICefBrowser) of object;

  { GeolocationHandler }
  TOnRequestGeolocationPermission = procedure(Sender: TObject; const Browser: ICefBrowser;
    const requestingUrl: ustring; requestId: cint; const callback: ICefGeolocationCallback) of object;
  TOnCancelGeolocationPermission = procedure(Sender: TObject; const Browser: ICefBrowser;
    const requestingUrl: ustring; requestId: cint) of object;

  { JsDialogHandler }
  TOnJsdialog = procedure(Sender: TObject; const Browser: ICefBrowser; const originUrl, acceptLang: ustring;
    dialogType: TCefJsDialogType; const messageText, defaultPromptText: ustring;
    callback: ICefJsDialogCallback; out suppressMessage: Boolean; out Result: Boolean) of object;
  TOnBeforeUnloadDialog = procedure(Sender: TObject; const Browser: ICefBrowser;
    const messageText: ustring; isReload: Boolean;
    const callback: ICefJsDialogCallback; out Result: Boolean) of object;
  TOnResetDialogState = procedure(Sender: TObject; const Browser: ICefBrowser) of object;
  TOnDialogClosed = procedure(Sender: TObject; const Browser: ICefBrowser) of object;

  { KeyboardHandler }
  TOnPreKeyEvent = procedure(Sender: TObject; const Browser: ICefBrowser; const event: PCefKeyEvent;
    osEvent: TCefEventHandle; out isKeyboardShortcut: Boolean; out Result: Boolean) of object;
  TOnKeyEvent = procedure(Sender: TObject; const Browser: ICefBrowser; const event: PCefKeyEvent;
    osEvent: TCefEventHandle; out Result: Boolean) of object;

  { LifespanHandler }
  TOnBeforePopup = procedure(Sender: TObject; const Browser: ICefBrowser;
    const Frame: ICefFrame; const targetUrl, targetFrameName: ustring;
    var popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo;
    var client: ICefClient; var settings: TCefBrowserSettings;
    var noJavascriptAccess: Boolean; out Result: Boolean) of object;
  TOnAfterCreated = procedure(Sender: TObject; const Browser: ICefBrowser) of object;
  TOnBeforeClose = procedure(Sender: TObject; const Browser: ICefBrowser) of object;
  TOnRunModal = procedure(Sender: TObject; const Browser: ICefBrowser; out Result: Boolean) of object;
  TOnClose = procedure(Sender: TObject; const Browser: ICefBrowser; out Result: Boolean) of object;

  { LoadHandler }
  TOnLoadingStateChange = procedure(Sender: TObject; const browser: ICefBrowser; isLoading, canGoBack, canGoForward: Boolean) of object;
  TOnLoadStart = procedure(Sender: TObject; const Browser: ICefBrowser; const Frame: ICefFrame) of object;
  TOnLoadEnd = procedure(Sender: TObject; const Browser: ICefBrowser; const Frame: ICefFrame; httpStatusCode: cint) of object;
  TOnLoadError = procedure(Sender: TObject; const Browser: ICefBrowser; const Frame: ICefFrame; errorCode: TCefErrorCode;
    const errorText, failedUrl: ustring) of object;

  { RenderHandler }
  TOnGetRootScreenRect = procedure(Sender: TObject; const Browser: ICefBrowser;
    rect: PCefRect; out Result: Boolean) of object;
  TOnGetViewRect = procedure(Sender: TObject; const Browser: ICefBrowser;
    rect: PCefRect; out Result: Boolean) of object;
  TOnGetScreenPoint = procedure(Sender: TObject; const Browser: ICefBrowser;
    viewX, viewY: cint; screenX, screenY: pcint; out Result: Boolean) of object;
  TOnGetScreenInfo = procedure(Sender: TObject; const browser : ICefBrowser;
    var screenInfo : TCefScreenInfo; out Result: Boolean) of object;
  TOnPopupShow = procedure(Sender: TObject; const Browser: ICefBrowser;
    show_: Boolean) of object;
  TOnPopupSize = procedure(Sender: TObject; const Browser: ICefBrowser;
    const rect: PCefRect) of object;
  TOnPaint = procedure(Sender: TObject; const Browser: ICefBrowser;
    kind: TCefPaintElementType; dirtyRectsCount: Cardinal; const dirtyRects: PCefRectArray;
    const buffer: Pointer; awidth, aheight: cint) of object;
  TOnCursorChange = procedure(Sender: TObject; const Browser: ICefBrowser;
    cursor: TCefCursorHandle) of object;
  TOnScrollOffsetChanged = procedure(Sender: TObject; const Browser: ICefBrowser) of object;

  { RequestHandler }
  TOnBeforeBrowse = procedure(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame;
      const request: ICefRequest; isRedirect: Boolean; out Result: Boolean) of object;
  TOnBeforeResourceLoad = procedure(Sender: TObject; const Browser: ICefBrowser; const Frame: ICefFrame;
    const request: ICefRequest; out Result: Boolean) of object;
  TOnGetResourceHandler = procedure(Sender: TObject; const Browser: ICefBrowser; const Frame: ICefFrame;
    const request: ICefRequest; out Result: ICefResourceHandler) of object;
  TOnResourceRedirect = procedure(Sender: TObject; const Browser: ICefBrowser; const Frame: ICefFrame;
    const oldUrl: ustring; var newUrl: ustring) of object;
  TOnGetAuthCredentials = procedure(Sender: TObject; const Browser: ICefBrowser; const Frame: ICefFrame;
    isProxy: Boolean; const host: ustring; port: cint; const realm, scheme: ustring;
    const callback: ICefAuthCallback; out Result: Boolean) of object;
  TOnQuotaRequest = procedure(Sender: TObject; const Browser: ICefBrowser;
    const originUrl: ustring; newSize: Int64; const callback: ICefQuotaCallback;
    out Result: Boolean) of object;
  TOnGetCookieManager = procedure(Sender: TObject; const Browser: ICefBrowser;
    const mainUrl: ustring; out Result: ICefCookieManager) of object;
  TOnProtocolExecution = procedure(Sender: TObject; const Browser: ICefBrowser;
    const url: ustring; out allowOsExecution: Boolean) of object;
  TOnCertificateError = procedure(Sender: TObject; certError: TCefErrorcode; const requestUrl: ustring;
      callback: ICefAllowCertificateErrorCallback; out Result: Boolean) of object;
  TOnBeforePluginLoad = procedure(Sender: TObject; const Browser: ICefBrowser;
    const url, policyUrl: ustring; const info: ICefWebPluginInfo; out Result: Boolean) of object;
  TOnPluginCrashed = procedure(Sender: TObject; const browser: ICefBrowser; const PluginPath: ustring) of object;
  TOnRenderProcessTerminated= procedure(Sender: TObject; const browser: ICefBrowser; status: TCefTerminationStatus) of object;

  TChromiumOptions = class(TPersistent)
  private
    FWindowlessFrameRate: cint;
    FRemoteFonts: TCefState;
    FJavascript: TCefState;
    FJavascriptOpenWindows: TCefState;
    FJavascriptCloseWindows: TCefState;
    FJavascriptAccessClipboard: TCefState;
    FJavascriptDomPaste: TCefState;
    FCaretBrowsing: TCefState;
    FJava: TCefState;
    FPlugins: TCefState;
    FUniversalAccessFromFileUrls: TCefState;
    FFileAccessFromFileUrls: TCefState;
    FWebSecurity: TCefState;
    FImageLoading: TCefState;
    FImageShrinkStandaloneToFit: TCefState;
    FTextAreaResize: TCefState;
    FPageCache: TCefState;
    FTabToLinks: TCefState;
    FAuthorAndUserStyles: TCefState;
    FLocalStorage: TCefState;
    FDatabases: TCefState;
    FApplicationCache: TCefState;
    FWebgl: TCefState;
    FBackgroundColor: TCefColor;
  published
    property WindowlessFrameRate: cint read FWindowlessFrameRate write FWindowlessFrameRate default 30;
    property RemoteFonts: TCefState read FRemoteFonts write FRemoteFonts default STATE_DEFAULT;
    property Javascript: TCefState read FJavascript write FJavascript default STATE_DEFAULT;
    property JavascriptOpenWindows: TCefState read FJavascriptOpenWindows write FJavascriptOpenWindows default STATE_DEFAULT;
    property JavascriptCloseWindows: TCefState read FJavascriptCloseWindows write FJavascriptCloseWindows default STATE_DEFAULT;
    property JavascriptAccessClipboard: TCefState read FJavascriptAccessClipboard write FJavascriptAccessClipboard default STATE_DEFAULT;
    property JavascriptDomPaste: TCefState read FJavascriptDomPaste write FJavascriptDomPaste default STATE_DEFAULT;
    property CaretBrowsing: TCefState read FCaretBrowsing write FCaretBrowsing default STATE_DEFAULT;
    property Java: TCefState read FJava write FJava default STATE_DEFAULT;
    property Plugins: TCefState read FPlugins write FPlugins default STATE_DEFAULT;
    property UniversalAccessFromFileUrls: TCefState read FUniversalAccessFromFileUrls write FUniversalAccessFromFileUrls default STATE_DEFAULT;
    property FileAccessFromFileUrls: TCefState read FFileAccessFromFileUrls write FFileAccessFromFileUrls default STATE_DEFAULT;
    property WebSecurity: TCefState read FWebSecurity write FWebSecurity default STATE_DEFAULT;
    property ImageLoading: TCefState read FImageLoading write FImageLoading default STATE_DEFAULT;
    property ImageShrinkStandaloneToFit: TCefState read FImageShrinkStandaloneToFit write FImageShrinkStandaloneToFit default STATE_DEFAULT;
    property TextAreaResize: TCefState read FTextAreaResize write FTextAreaResize default STATE_DEFAULT;
    property PageCache: TCefState read FPageCache write FPageCache default STATE_DEFAULT;
    property TabToLinks: TCefState read FTabToLinks write FTabToLinks default STATE_DEFAULT;
    property AuthorAndUserStyles: TCefState read FAuthorAndUserStyles write FAuthorAndUserStyles default STATE_DEFAULT;
    property LocalStorage: TCefState read FLocalStorage write FLocalStorage default STATE_DEFAULT;
    property Databases: TCefState read FDatabases write FDatabases default STATE_DEFAULT;
    property ApplicationCache: TCefState read FApplicationCache write FApplicationCache default STATE_DEFAULT;
    property Webgl: TCefState read FWebgl write FWebgl default STATE_DEFAULT;
    property BackgroundColor: TCefColor read FBackgroundColor write FBackgroundColor default 0;
  end;

  TChromiumFontOptions = class(TPersistent)
  private
    FStandardFontFamily: ustring;
    FCursiveFontFamily: ustring;
    FSansSerifFontFamily: ustring;
    FMinimumLogicalFontSize: cint;
    FFantasyFontFamily: ustring;
    FSerifFontFamily: ustring;
    FDefaultFixedFontSize: cint;
    FDefaultFontSize: cint;
    FRemoteFontsDisabled: TCefState;
    FFixedFontFamily: ustring;
    FMinimumFontSize: cint;
  public
    constructor Create; virtual;
  published
    property StandardFontFamily: ustring read FStandardFontFamily;
    property FixedFontFamily: ustring read FFixedFontFamily write FFixedFontFamily;
    property SerifFontFamily: ustring read FSerifFontFamily write FSerifFontFamily;
    property SansSerifFontFamily: ustring read FSansSerifFontFamily write FSansSerifFontFamily;
    property CursiveFontFamily: ustring read FCursiveFontFamily write FCursiveFontFamily;
    property FantasyFontFamily: ustring read FFantasyFontFamily write FFantasyFontFamily;
    property DefaultFontSize: cint read FDefaultFontSize write FDefaultFontSize default 0;
    property DefaultFixedFontSize: cint read FDefaultFixedFontSize write FDefaultFixedFontSize default 0;
    property MinimumFontSize: cint read FMinimumFontSize write FMinimumFontSize default 0;
    property MinimumLogicalFontSize: cint read FMinimumLogicalFontSize write FMinimumLogicalFontSize default 0;
    property RemoteFonts: TCefState read FRemoteFontsDisabled write FRemoteFontsDisabled default STATE_DEFAULT;
  end;

  IChromiumEvents = interface ['{0C139DB1-0349-4D7F-8155-76FEA6A0126D}']
    procedure GetSettings(var settings: TCefBrowserSettings);

    { CefClient }
    function doOnProcessMessageReceived(const Browser: ICefBrowser;
      sourceProcess: TCefProcessId; const message: ICefProcessMessage): Boolean;

    { CefContextMenuHandler }
    procedure doOnBeforeContextMenu(const browser: ICefBrowser; const frame: ICefFrame;
      const params: ICefContextMenuParams; const model: ICefMenuModel);
    function doOnContextMenuCommand(const browser: ICefBrowser; const frame: ICefFrame;
      const params: ICefContextMenuParams; commandId: cint; eventFlags: TCefEventFlags): Boolean;
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
    function doOnConsoleMessage(const browser: ICefBrowser; const message, source: ustring; line: cint): Boolean;

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
      const requestingUrl: ustring; requestId: cint; const callback: ICefGeolocationCallback);
    procedure doOnCancelGeolocationPermission(const Browser: ICefBrowser;
      const requestingUrl: ustring; requestId: cint);

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
    procedure doOnLoadEnd(const browser: ICefBrowser; const frame: ICefFrame; httpStatusCode: cint);
    procedure doOnLoadError(const browser: ICefBrowser; const frame: ICefFrame; errorCode: TCefErrorCode;
      const errorText, failedUrl: ustring);

    { CefRenderHandler }
    function doOnGetRootScreenRect(const browser: ICefBrowser; rect: PCefRect): Boolean;
    function doOnGetViewRect(const browser: ICefBrowser; rect: PCefRect): Boolean;
    function doOnGetScreenPoint(const browser: ICefBrowser; viewX, viewY: cint;
      screenX, screenY: pcint): Boolean;
    function doOnGetScreenInfo(const browser: ICefBrowser; var screenInfo: TCefScreenInfo): Boolean;
    procedure doOnPopupShow(const browser: ICefBrowser; show: Boolean);
    procedure doOnPopupSize(const browser: ICefBrowser; const rect: PCefRect);
    procedure doOnPaint(const browser: ICefBrowser; kind: TCefPaintElementType;
      dirtyRectsCount: csize_t; const dirtyRects: PCefRectArray;
      const buffer: Pointer; width, height: cint);
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
      isProxy: Boolean; const host: ustring; port: cint; const realm, scheme: ustring;
      const callback: ICefAuthCallback): Boolean;
    function doOnQuotaRequest(const browser: ICefBrowser; const originUrl: ustring;
      newSize: Int64; const callback: ICefQuotaCallback): Boolean;
    function doOnGetCookieManager(const browser: ICefBrowser; const mainUrl: ustring): ICefCookieManager;
    procedure doOnProtocolExecution(const browser: ICefBrowser; const url: ustring; out allowOsExecution: Boolean);
    function doOnCertificateError(certError: TCefErrorcode; const requestUrl: ustring;
      callback: ICefAllowCertificateErrorCallback): Boolean;
    function doOnBeforePluginLoad(const browser: ICefBrowser; const url, policyUrl: ustring;
      const info: ICefWebPluginInfo): Boolean;
    procedure doOnPluginCrashed(const browser: ICefBrowser; const PluginPath: ustring);
    procedure doOnRenderProcessTerminated(const browser: ICefBrowser; status: TCefTerminationStatus);
  end;

  ICefClientHandler = interface
    ['{E76F6888-D9C3-4FCE-9C23-E89659820A36}']
    procedure Disconnect;
  end;

  TCustomClientHandler = class(TCefClientOwn, ICefClientHandler)
  private
    FEvents: IChromiumEvents;
    FLoadHandler: ICefLoadHandler;
    FFocusHandler: ICefFocusHandler;
    FContextMenuHandler: ICefContextMenuHandler;
    FKeyboardHandler: ICefKeyboardHandler;
    FDisplayHandler: ICefDisplayHandler;
    FDownloadHandler: ICefDownloadHandler;
    FDragHandler: ICefDragHandler;
    FGeolocationHandler: ICefGeolocationHandler;
    FJsDialogHandler: ICefJsDialogHandler;
    FLifeSpanHandler: ICefLifeSpanHandler;
    FRequestHandler: ICefRequestHandler;
    FDialogHandler: ICefDialogHandler;
    FRenderHandler: ICefRenderHandler;
  protected
    function GetContextMenuHandler: ICefContextMenuHandler; override;
    function GetDisplayHandler: ICefDisplayHandler; override;
    function GetDownloadHandler: ICefDownloadHandler; override;
    function GetDragHandler: ICefDragHandler; override;
    function GetFocusHandler: ICefFocusHandler; override;
    function GetGeolocationHandler: ICefGeolocationHandler; override;
    function GetJsdialogHandler: ICefJsdialogHandler; override;
    function GetKeyboardHandler: ICefKeyboardHandler; override;
    function GetLifeSpanHandler: ICefLifeSpanHandler; override;
    function GetLoadHandler: ICefLoadHandler; override;
    function GetRequestHandler: ICefRequestHandler; override;
    function OnProcessMessageReceived(const browser: ICefBrowser;
      sourceProcess: TCefProcessId; const message: ICefProcessMessage): Boolean; override;
    procedure Disconnect;
    function GetDialogHandler: ICefDialogHandler; override;
    function GetRenderHandler: ICefRenderHandler; override;
  public
    constructor Create(const events: IChromiumEvents); reintroduce; virtual;
  end;

  TCustomClientHandlerClass = class of TCustomClientHandler;

  TCustomLoadHandler = class(TCefLoadHandlerOwn)
  private
    FEvent: IChromiumEvents;
  protected
    procedure OnLoadingStateChange(const browser: ICefBrowser; IsLoading:boolean; CanGoBack:boolean; CanGoForward:boolean); override;
    procedure OnLoadStart(const browser: ICefBrowser; const frame: ICefFrame); override;
    procedure OnLoadEnd(const browser: ICefBrowser; const frame: ICefFrame; httpStatusCode: cint); override;
    procedure OnLoadError(const browser: ICefBrowser; const frame: ICefFrame; errorCode: TCefErrorCode;
      const errorText, failedUrl: ustring); override;
  public
    constructor Create(const events: IChromiumEvents); reintroduce; virtual;
  end;

  TCustomFocusHandler = class(TCefFocusHandlerOwn)
  private
    FEvent: IChromiumEvents;
  protected
    procedure OnTakeFocus(const browser: ICefBrowser; next: Boolean); override;
    function OnSetFocus(const browser: ICefBrowser; source: TCefFocusSource): Boolean; override;
    procedure OnGotFocus(const browser: ICefBrowser); override;
  public
    constructor Create(const events: IChromiumEvents); reintroduce; virtual;
  end;

  TCustomContextMenuHandler = class(TCefContextMenuHandlerOwn)
  private
    FEvent: IChromiumEvents;
  protected
    procedure OnBeforeContextMenu(const browser: ICefBrowser; const frame: ICefFrame;
      const params: ICefContextMenuParams; const model: ICefMenuModel); override;
    function OnContextMenuCommand(const browser: ICefBrowser; const frame: ICefFrame;
      const params: ICefContextMenuParams; commandId: cint;
      eventFlags: TCefEventFlags): Boolean; override;
    procedure OnContextMenuDismissed(const browser: ICefBrowser; const frame: ICefFrame); override;
  public
    constructor Create(const events: IChromiumEvents); reintroduce; virtual;
  end;

  TCustomKeyboardHandler = class(TCefKeyboardHandlerOwn)
  private
    FEvent: IChromiumEvents;
  protected
    function OnPreKeyEvent(const browser: ICefBrowser; const event: PCefKeyEvent;
      osEvent: TCefEventHandle; out isKeyboardShortcut: Boolean): Boolean; override;
    function OnKeyEvent(const browser: ICefBrowser; const event: PCefKeyEvent;
      osEvent: TCefEventHandle): Boolean; override;
  public
    constructor Create(const events: IChromiumEvents); reintroduce; virtual;
  end;

  TCustomDisplayHandler = class(TCefDisplayHandlerOwn)
  private
    FEvent: IChromiumEvents;
  protected
    procedure OnAddressChange(const browser: ICefBrowser; const frame: ICefFrame; const url: ustring); override;
    procedure OnTitleChange(const browser: ICefBrowser; const title: ustring); override;
    function OnTooltip(const browser: ICefBrowser; var text: ustring): Boolean; override;
    procedure OnStatusMessage(const browser: ICefBrowser; const value: ustring); override;
    function OnConsoleMessage(const browser: ICefBrowser; const message, source: ustring; line: cint): Boolean; override;
  public
    constructor Create(const events: IChromiumEvents); reintroduce; virtual;
  end;

  TCustomDownloadHandler = class(TCefDownloadHandlerOwn)
  private
    FEvent: IChromiumEvents;
  protected
    procedure OnBeforeDownload(const browser: ICefBrowser; const downloadItem: ICefDownloadItem;
      const suggestedName: ustring; const callback: ICefBeforeDownloadCallback); override;
    procedure OnDownloadUpdated(const browser: ICefBrowser; const downloadItem: ICefDownloadItem;
        const callback: ICefDownloadItemCallback); override;
  public
    constructor Create(const events: IChromiumEvents); reintroduce; virtual;
  end;

  TCustomDragHandler = class(TCefDragHandlerOwn)
  private
    FEvent: IChromiumEvents;
  protected
    function OnDragEnter(const browser: ICefBrowser; const dragData: ICefDragData; mask: TCefDragOperationsMask):Boolean; override;
  public
    constructor Create(const events: IChromiumEvents); reintroduce; virtual;
  end;

  TCustomJsDialogHandler = class(TCefJsDialogHandlerOwn)
  private
    FEvent: IChromiumEvents;
  protected
    function OnJsdialog(const browser: ICefBrowser; const originUrl, acceptLang: ustring;
      dialogType: TCefJsDialogType; const messageText, defaultPromptText: ustring;
      callback: ICefJsDialogCallback; out suppressMessage: Boolean): Boolean; override;
    function OnBeforeUnloadDialog(const browser: ICefBrowser;
      const messageText: ustring; isReload: Boolean;
      const callback: ICefJsDialogCallback): Boolean; override;
    procedure OnResetDialogState(const browser: ICefBrowser); override;
  public
    constructor Create(const events: IChromiumEvents); reintroduce; virtual;
  end;

  TCustomLifeSpanHandler = class(TCefLifeSpanHandlerOwn)
  private
    FEvent: IChromiumEvents;
  protected
    function OnBeforePopup(const parentBrowser: ICefBrowser; const frame: ICefFrame;
      var TargetURL: ustring; const TargetFrameName: ustring;
      var popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo;
      var client: ICefClient; var settings: TCefBrowserSettings; var NoJavascriptAccess:boolean): Boolean; override;
    procedure OnAfterCreated(const browser: ICefBrowser); override;
    procedure OnBeforeClose(const browser: ICefBrowser); override;
    function RunModal(const browser: ICefBrowser): Boolean; override;
    function DoClose(const browser: ICefBrowser): Boolean; override;
  public
    constructor Create(const events: IChromiumEvents); reintroduce; virtual;
  end;

  TCustomRequestHandler = class(TCefRequestHandlerOwn)
  private
    FEvent: IChromiumEvents;
  protected
    function OnBeforeBrowse(const browser: ICefBrowser; const frame: ICefFrame;
      const request: ICefRequest; IsRedirect:boolean): Boolean; override;
    function OnBeforeResourceLoad(const browser: ICefBrowser; const frame: ICefFrame;
      const request: ICefRequest): Boolean; override;
    function GetResourceHandler(const browser: ICefBrowser; const frame: ICefFrame;
      const request: ICefRequest): ICefResourceHandler; override;
    procedure OnResourceRedirect(const browser: ICefBrowser; const frame: ICefFrame;
      const oldUrl: ustring; var newUrl: ustring); override;
    function GetAuthCredentials(const browser: ICefBrowser; const frame: ICefFrame;
      isProxy: Boolean; const host: ustring; port: cint; const realm, scheme: ustring;
      const callback: ICefAuthCallback): Boolean; override;
    function OnQuotaRequest(const browser: ICefBrowser; const originUrl: ustring;
      newSize: Int64; const callback: ICefQuotaCallback): Boolean; override;
    function OnCertificateError(CertError: TCefErrorcode; const RequestUrl: ustring;
      const Callback: ICefAllowCertificateErrorCallback): boolean; override;
    procedure OnProtocolExecution(const browser: ICefBrowser; const url: ustring; out allowOsExecution: Boolean); override;
    function OnBeforePluginLoad(const browser: ICefBrowser; const url, policyUrl: ustring;
      const info: ICefWebPluginInfo): Boolean; override;
    procedure OnPluginCrashed(const browser: ICefBrowser; const pluginPath: ustring); override;
    procedure OnRenderProcessTerminated(const browser: ICefBrowser; status: TCefTerminationStatus); override;
  public
    constructor Create(const events: IChromiumEvents); reintroduce; virtual;
  end;

  TCustomDialogHandler = class(TCefDialogHandlerOwn)
  private
    FEvent: IChromiumEvents;
  protected
    function OnFileDialog(const browser: ICefBrowser; mode: TCefFileDialogMode;
      const title: ustring; const DefaultFileName: ustring;
      AcceptTypes: TStrings; const callback: ICefFileDialogCallback):Boolean; override;
  public
    constructor Create(const events: IChromiumEvents); reintroduce; virtual;
  end;

  TCustomRenderHandler = class(TCefRenderHandlerOwn)
  private
    FEvent: IChromiumEvents;
  protected
    function GetRootScreenRect(const browser: ICefBrowser; rect: PCefRect): Boolean; override;
    function GetViewRect(const browser: ICefBrowser; rect: PCefRect): Boolean; override;
    function GetScreenPoint(const browser: ICefBrowser; viewX, viewY: cint;
      screenX, screenY: pcint): Boolean; override;
    procedure OnPopupShow(const browser: ICefBrowser; show: Boolean); override;
    procedure OnPopupSize(const browser: ICefBrowser; const rect: PCefRect); override;
    procedure OnPaint(const browser: ICefBrowser; kind: TCefPaintElementType;
      dirtyRectsCount: csize_t; const dirtyRects: PCefRectArray;
      const buffer: Pointer; width, height: cint); override;
    procedure OnCursorChange(const browser: ICefBrowser; cursor: TCefCursorHandle); override;
  public
    constructor Create(const events: IChromiumEvents); reintroduce; virtual;
  end;

implementation

{ TChromiumFontOptions }

constructor TChromiumFontOptions.Create;
begin
  FStandardFontFamily := '';
  FCursiveFontFamily := '';
  FSansSerifFontFamily := '';
  FMinimumLogicalFontSize := 0;
  FFantasyFontFamily := '';
  FSerifFontFamily := '';
  FDefaultFixedFontSize := 0;
  FDefaultFontSize := 0;
  FRemoteFontsDisabled := STATE_DEFAULT;
  FFixedFontFamily := '';
  FMinimumFontSize := 0;
end;

{ TCefCustomHandler }

constructor TCustomClientHandler.Create(const events: IChromiumEvents);
begin
  inherited Create;
  FEvents := events;
  FLoadHandler := TCustomLoadHandler.Create(events);
  FFocusHandler := TCustomFocusHandler.Create(events);
  FContextMenuHandler := TCustomContextMenuHandler.Create(events);
  FKeyboardHandler := TCustomKeyboardHandler.Create(events);
  FDisplayHandler := TCustomDisplayHandler.Create(events);
  FDownloadHandler := TCustomDownloadHandler.Create(events);
  FDragHandler := TCustomDragHandler.Create(events);
  FJsDialogHandler := TCustomJsDialogHandler.Create(events);
  FLifeSpanHandler := TCustomLifeSpanHandler.Create(events);
  FRequestHandler := TCustomRequestHandler.Create(events);
  FDialogHandler := TCustomDialogHandler.Create(events);
  FRenderHandler := TCustomRenderHandler.Create(events);
end;

procedure TCustomClientHandler.Disconnect;
begin
  FEvents := nil;
  FLoadHandler := nil;
  FFocusHandler := nil;
  FContextMenuHandler := nil;
  FKeyboardHandler := nil;
  FDisplayHandler := nil;
  FDownloadHandler := nil;
  FGeolocationHandler := nil;
  FJsDialogHandler := nil;
  FLifeSpanHandler := nil;
  FRequestHandler := nil;
  FDialogHandler := nil;
  FRenderHandler := nil;
end;

function TCustomClientHandler.GetContextMenuHandler: ICefContextMenuHandler;
begin
  Result := FContextMenuHandler;
end;

function TCustomClientHandler.GetDisplayHandler: ICefDisplayHandler;
begin
  Result := FDisplayHandler;
end;

function TCustomClientHandler.GetDownloadHandler: ICefDownloadHandler;
begin
  Result := FDownloadHandler;
end;

function TCustomClientHandler.GetDragHandler: ICefDragHandler;
begin
  Result := FDragHandler;
end;

function TCustomClientHandler.GetFocusHandler: ICefFocusHandler;
begin
  Result := FFocusHandler;
end;

function TCustomClientHandler.GetGeolocationHandler: ICefGeolocationHandler;
begin
  Result := FGeolocationHandler;
end;

function TCustomClientHandler.GetJsdialogHandler: ICefJsDialogHandler;
begin
  Result := FJsDialogHandler;
end;

function TCustomClientHandler.GetKeyboardHandler: ICefKeyboardHandler;
begin
  Result := FKeyboardHandler;
end;

function TCustomClientHandler.GetLifeSpanHandler: ICefLifeSpanHandler;
begin
  Result := FLifeSpanHandler;
end;

function TCustomClientHandler.GetLoadHandler: ICefLoadHandler;
begin
  Result := FLoadHandler;
end;

function TCustomClientHandler.GetRequestHandler: ICefRequestHandler;
begin
  Result := FRequestHandler;
end;

function TCustomClientHandler.OnProcessMessageReceived(
  const browser: ICefBrowser; sourceProcess: TCefProcessId;
  const message: ICefProcessMessage): Boolean;
begin
  Result := FEvents.doOnProcessMessageReceived(browser, sourceProcess, message);
end;

function TCustomClientHandler.GetDialogHandler: ICefDialogHandler;
begin
  Result := FDialogHandler;
end;

function TCustomClientHandler.GetRenderHandler: ICefRenderHandler;
begin
  Result := FRenderHandler;
end;

{ TCustomLoadHandler }

constructor TCustomLoadHandler.Create(const events: IChromiumEvents);
begin
  inherited Create;
  FEvent := events;
end;

procedure TCustomLoadHandler.OnLoadEnd(const browser: ICefBrowser;
  const frame: ICefFrame; httpStatusCode: cint);
begin
  FEvent.doOnLoadEnd(browser, frame, httpStatusCode);
end;

procedure TCustomLoadHandler.OnLoadError(const browser: ICefBrowser;
  const frame: ICefFrame; errorCode: TCefErrorCode; const errorText,
  failedUrl: ustring);
begin
  FEvent.doOnLoadError(browser, frame, errorCode, errorText, failedUrl);
end;

procedure TCustomLoadHandler.OnLoadingStateChange(const browser: ICefBrowser;
  isLoading, canGoBack, canGoForward: Boolean);
begin
  FEvent.doOnLoadingStateChange(browser, isLoading, canGoBack, canGoForward);
end;

procedure TCustomLoadHandler.OnLoadStart(const browser: ICefBrowser;
  const frame: ICefFrame);
begin
  FEvent.doOnLoadStart(browser, frame);
end;

{ TCustomFocusHandler }

constructor TCustomFocusHandler.Create(const events: IChromiumEvents);
begin
  inherited Create;
  FEvent := events;
end;

procedure TCustomFocusHandler.OnGotFocus(const browser: ICefBrowser);
begin
  FEvent.doOnGotFocus(browser);
end;

function TCustomFocusHandler.OnSetFocus(const browser: ICefBrowser;
  source: TCefFocusSource): Boolean;
begin
  Result := FEvent.doOnSetFocus(browser, source);
end;

procedure TCustomFocusHandler.OnTakeFocus(const browser: ICefBrowser;
  next: Boolean);
begin
  FEvent.doOnTakeFocus(browser, next);
end;

{ TCustomContextMenuHandler }

constructor TCustomContextMenuHandler.Create(const events: IChromiumEvents);
begin
  inherited Create;
  FEvent := events;
end;

procedure TCustomContextMenuHandler.OnBeforeContextMenu(
  const browser: ICefBrowser; const frame: ICefFrame;
  const params: ICefContextMenuParams; const model: ICefMenuModel);
begin
  FEvent.doOnBeforeContextMenu(browser, frame, params, model);
end;

function TCustomContextMenuHandler.OnContextMenuCommand(
  const browser: ICefBrowser; const frame: ICefFrame;
  const params: ICefContextMenuParams; commandId: cint;
  eventFlags: TCefEventFlags): Boolean;
begin
  Result := FEvent.doOnContextMenuCommand(browser, frame, params, commandId,
    eventFlags);
end;

procedure TCustomContextMenuHandler.OnContextMenuDismissed(
  const browser: ICefBrowser; const frame: ICefFrame);
begin
  FEvent.doOnContextMenuDismissed(browser, frame);
end;

{ TCustomKeyboardHandler }

constructor TCustomKeyboardHandler.Create(const events: IChromiumEvents);
begin
  inherited Create;
  FEvent := events;
end;

function TCustomKeyboardHandler.OnKeyEvent(const browser: ICefBrowser;
  const event: PCefKeyEvent; osEvent: TCefEventHandle): Boolean;
begin
  Result := FEvent.doOnKeyEvent(browser, event, osEvent);
end;

function TCustomKeyboardHandler.OnPreKeyEvent(const browser: ICefBrowser;
  const event: PCefKeyEvent; osEvent: TCefEventHandle;
  out isKeyboardShortcut: Boolean): Boolean;
begin
  Result := FEvent.doOnPreKeyEvent(browser, event, osEvent, isKeyboardShortcut);
end;

{ TCustomDisplayHandler }

constructor TCustomDisplayHandler.Create(const events: IChromiumEvents);
begin
  inherited Create;
  FEvent := events;
end;

procedure TCustomDisplayHandler.OnAddressChange(const browser: ICefBrowser;
  const frame: ICefFrame; const url: ustring);
begin
  FEvent.doOnAddressChange(browser, frame, url);
end;

function TCustomDisplayHandler.OnConsoleMessage(const browser: ICefBrowser;
  const message, source: ustring; line: cint): Boolean;
begin
  Result := FEvent.doOnConsoleMessage(browser, message, source, line);
end;

procedure TCustomDisplayHandler.OnStatusMessage(const browser: ICefBrowser;
  const value: ustring);
begin
  FEvent.doOnStatusMessage(browser, value);
end;

procedure TCustomDisplayHandler.OnTitleChange(const browser: ICefBrowser;
  const title: ustring);
begin
  FEvent.doOnTitleChange(browser, title);
end;

function TCustomDisplayHandler.OnTooltip(const browser: ICefBrowser;
  var text: ustring): Boolean;
begin
  Result := FEvent.doOnTooltip(browser, text);
end;

{ TCustomDownloadHandler }

constructor TCustomDownloadHandler.Create(const events: IChromiumEvents);
begin
  inherited Create;
  FEvent := events;
end;

procedure TCustomDownloadHandler.OnBeforeDownload(const browser: ICefBrowser;
  const downloadItem: ICefDownloadItem; const suggestedName: ustring;
  const callback: ICefBeforeDownloadCallback);
begin
  FEvent.doOnBeforeDownload(browser, downloadItem, suggestedName, callback);
end;

procedure TCustomDownloadHandler.OnDownloadUpdated(const browser: ICefBrowser;
  const downloadItem: ICefDownloadItem;
  const callback: ICefDownloadItemCallback);
begin
  FEvent.doOnDownloadUpdated(browser, downloadItem, callback);
end;

//..............................................................................TCustomDragHandler
//Protected section
function TCustomDragHandler.OnDragEnter(const browser: ICefBrowser; const dragData: ICefDragData;
  mask: TCefDragOperationsMask):boolean;
begin
  result:=FEvent.doOnDragEnter(browser,dragData,mask);
end;

//Public section
constructor TCustomDragHandler.Create(const events: IChromiumEvents);
begin
  inherited Create;
  FEvent:=events;
end;

{ TCustomGeolocationHandler }

{constructor TCustomGeolocationHandler.Create(const events: IChromiumEvents);
begin
  inherited Create;
  FEvent := events;
end;

procedure TCustomGeolocationHandler.OnCancelGeolocationPermission(
  const browser: ICefBrowser; const requestingUrl: ustring; requestId: cint);
begin
  FEvent.doOnCancelGeolocationPermission(browser, requestingUrl, requestId);
end;

procedure TCustomGeolocationHandler.OnRequestGeolocationPermission(
  const browser: ICefBrowser; const requestingUrl: ustring; requestId: cint;
  const callback: ICefGeolocationCallback);
begin
  FEvent.doOnRequestGeolocationPermission(browser, requestingUrl, requestId, callback);
end;}

{ TCustomJsDialogHandler }

constructor TCustomJsDialogHandler.Create(const events: IChromiumEvents);
begin
  inherited Create;
  FEvent := events;
end;

function TCustomJsDialogHandler.OnBeforeUnloadDialog(const browser: ICefBrowser;
  const messageText: ustring; isReload: Boolean;
  const callback: ICefJsDialogCallback): Boolean;
begin
  Result := FEvent.doOnBeforeUnloadDialog(browser, messageText, isReload, callback);
end;

function TCustomJsDialogHandler.OnJsdialog(const browser: ICefBrowser;
  const originUrl, acceptLang: ustring; dialogType: TCefJsDialogType;
  const messageText, defaultPromptText: ustring; callback: ICefJsDialogCallback;
  out suppressMessage: Boolean): Boolean;
begin
  Result := FEvent.doOnJsdialog(browser, originUrl, acceptLang, dialogType,
    messageText, defaultPromptText, callback, suppressMessage);
end;

procedure TCustomJsDialogHandler.OnResetDialogState(const browser: ICefBrowser);
begin
  FEvent.doOnResetDialogState(browser);
end;

{ TCustomLifeSpanHandler }
constructor TCustomLifeSpanHandler.Create(const events: IChromiumEvents);
begin
  inherited Create;
  FEvent := events;
end;

function TCustomLifeSpanHandler.DoClose(const browser: ICefBrowser): Boolean;
begin
  Result := FEvent.doOnClose(browser);
end;

procedure TCustomLifeSpanHandler.OnAfterCreated(const browser: ICefBrowser);
begin
  FEvent.doOnAfterCreated(browser);
end;

procedure TCustomLifeSpanHandler.OnBeforeClose(const browser: ICefBrowser);
begin
  FEvent.doOnBeforeClose(browser);
end;

function TCustomLifeSpanHandler.OnBeforePopup(const parentBrowser: ICefBrowser; const frame: ICefFrame;
      var TargetURL: ustring; const TargetFrameName: ustring;
      var popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo;
      var client: ICefClient; var settings: TCefBrowserSettings; var NoJavascriptAccess:boolean): Boolean;
begin
  Result := FEvent.doOnBeforePopup(
    parentBrowser,
    frame,
    TargetURL,
    TargetFrameName,
    popupFeatures,
    windowInfo,
    client,
    settings,
    NoJavascriptAccess
  );
end;

function TCustomLifeSpanHandler.RunModal(const browser: ICefBrowser): Boolean;
begin
  Result := FEvent.doOnRunModal(browser);
end;

{ TCustomRequestHandler }
function TCustomRequestHandler.OnBeforeBrowse(const browser: ICefBrowser; const frame: ICefFrame;
      const request: ICefRequest; IsRedirect:boolean): Boolean;
begin
  Result:=FEvent.doOnBeforeBrowse(browser,frame,request,IsRedirect);
end;

function TCustomRequestHandler.OnBeforeResourceLoad(const browser: ICefBrowser;
  const frame: ICefFrame; const request: ICefRequest): Boolean;
begin
  Result := FEvent.doOnBeforeResourceLoad(browser, frame, request);
end;

function TCustomRequestHandler.GetResourceHandler(const browser: ICefBrowser;
  const frame: ICefFrame; const request: ICefRequest): ICefResourceHandler;
begin
  Result := FEvent.doOnGetResourceHandler(browser, frame, request);
end;

procedure TCustomRequestHandler.OnResourceRedirect(const browser: ICefBrowser;
  const frame: ICefFrame; const oldUrl: ustring; var newUrl: ustring);
begin
  FEvent.doOnResourceRedirect(browser, frame, oldUrl, newUrl);
end;

function TCustomRequestHandler.GetAuthCredentials(const browser: ICefBrowser;
  const frame: ICefFrame; isProxy: Boolean; const host: ustring; port: cint;
  const realm, scheme: ustring; const callback: ICefAuthCallback): Boolean;
begin
  Result := FEvent.doOnGetAuthCredentials(browser, frame, isProxy, host, port,
    realm, scheme, callback);
end;

function TCustomRequestHandler.OnQuotaRequest(const browser: ICefBrowser;
  const originUrl: ustring; newSize: Int64;
  const callback: ICefQuotaCallback): Boolean;
begin
  Result:=FEvent.doOnQuotaRequest(browser, originUrl, newSize, callback);
end;

function TCustomRequestHandler.OnCertificateError(CertError: TCefErrorcode; const RequestUrl: ustring;
  const Callback: ICefAllowCertificateErrorCallback): boolean;
begin
  Result:=FEvent.doOnCertificateError(CertError,RequestUrl,Callback);
end;

constructor TCustomRequestHandler.Create(const events: IChromiumEvents);
begin
  inherited Create;
  FEvent := events;
end;

function TCustomRequestHandler.OnBeforePluginLoad(const browser: ICefBrowser;
  const url, policyUrl: ustring; const info: ICefWebPluginInfo): Boolean;
begin
  Result := FEvent.doOnBeforePluginLoad(browser, url, policyUrl, info);
end;

procedure TCustomRequestHandler.OnPluginCrashed(const browser: ICefBrowser;
  const pluginPath: ustring);
begin
  FEvent.doOnPluginCrashed(browser, pluginPath);
end;

procedure TCustomRequestHandler.OnRenderProcessTerminated(
  const browser: ICefBrowser; status: TCefTerminationStatus);
begin
  FEvent.doOnRenderProcessTerminated(browser, status);
end;

procedure TCustomRequestHandler.OnProtocolExecution(const browser: ICefBrowser;
  const url: ustring; out allowOsExecution: Boolean);
begin
  FEvent.doOnProtocolExecution(browser, url, allowOsExecution);
end;

{ TCustomDialogHandler }

constructor TCustomDialogHandler.Create(const events: IChromiumEvents);
begin
  inherited Create;
  FEvent := events;
end;

function TCustomDialogHandler.OnFileDialog(const browser: ICefBrowser; mode: TCefFileDialogMode;
      const title: ustring; const DefaultFileName: ustring;
      AcceptTypes: TStrings; const callback: ICefFileDialogCallback): Boolean;
begin
  Result := FEvent.doOnFileDialog(browser, mode, title,
    DefaultFileName, AcceptTypes, callback)
end;

{ TCustomRenderHandler }

constructor TCustomRenderHandler.Create(const events: IChromiumEvents);
begin
  inherited Create;
  FEvent := events;
end;

function TCustomRenderHandler.GetRootScreenRect(const browser: ICefBrowser;
  rect: PCefRect): Boolean;
begin
  Result := FEvent.doOnGetRootScreenRect(browser, rect);
end;

function TCustomRenderHandler.GetScreenPoint(const browser: ICefBrowser; viewX,
  viewY: cint; screenX, screenY: pcint): Boolean;
begin
  Result := FEvent.doOnGetScreenPoint(browser, viewX, viewY, screenX, screenY);
end;

function TCustomRenderHandler.GetViewRect(const browser: ICefBrowser;
  rect: PCefRect): Boolean;
begin
  Result := FEvent.doOnGetViewRect(browser, rect);
end;

procedure TCustomRenderHandler.OnCursorChange(const browser: ICefBrowser;
  cursor: TCefCursorHandle);
begin
  FEvent.doOnCursorChange(browser, cursor);
end;

procedure TCustomRenderHandler.OnPaint(const browser: ICefBrowser;
  kind: TCefPaintElementType; dirtyRectsCount: csize_t;
  const dirtyRects: PCefRectArray; const buffer: Pointer; width, height: cint);
begin
  FEvent.doOnPaint(browser, kind, dirtyRectsCount, dirtyRects, buffer, width, height);
end;

procedure TCustomRenderHandler.OnPopupShow(const browser: ICefBrowser;
  show: Boolean);
begin
  FEvent.doOnPopupShow(browser, show);
end;

procedure TCustomRenderHandler.OnPopupSize(const browser: ICefBrowser;
  const rect: PCefRect);
begin
  FEvent.doOnPopupSize(browser, rect);
end;

end.