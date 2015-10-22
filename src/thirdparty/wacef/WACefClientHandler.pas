unit WACefClientHandler;

interface

uses
  Classes,
  WACefTypes,
  WACefInterfaces,
  WACefOwns,
  WACefContextMenuHandler,
  WACefDialogHandler,
  WACefDisplayHandler,
  WACefDownloadHandler,
  WACefDragHandler,
  WACefFindHandler,
  WACefFocusHandler,
  WACefGeolocationHandler,
  WACefJsDialogHandler,
  WACefKeyboardHandler,
  WACefLifeSpanHandler,
  WACefLoadHandler,
  WACefRenderHandler,
  WACefRequestHandler;

type
  { CefClient }
  TOnProcessMessageReceived = procedure(aSender: TObject; const aBrowser: ICefBrowser; aSourceProcess: TCefProcessId; const aMessage: ICefProcessMessage; out Result: Boolean) of object;

  { CefContextMenuHandler }
  TOnBeforeContextMenu = procedure(aSender: TObject; const aBrowser: ICefBrowser; const aFrame: ICefFrame; const aParams: ICefContextMenuParams; const aModel: ICefMenuModel) of object;
  TOnContextMenuCommand = procedure(aSender: TObject; const aBrowser: ICefBrowser; const aFrame: ICefFrame; const aParams: ICefContextMenuParams; aCommandId: cint; aEventFlags: TCefEventFlags; out Result: Boolean) of object;
  TOnContextMenuDismissed = procedure(aSender: TObject; const aBrowser: ICefBrowser; const aFrame: ICefFrame) of object;

  { CefDialogHandler }
  TOnFileDialog = procedure(aSender: TObject; const aBrowser: ICefBrowser; aMode: TCefFileDialogMode; const aTitle: ustring; const aDefaultFilePath: ustring; aAcceptFilters: TStrings; aSelectedAcceptFilter: cint; const aCallback: ICefFileDialogCallback; out Result: Boolean) of object;

  { CefDisplayHandler }
  TOnAddressChange = procedure(aSender: TObject; const aBrowser: ICefBrowser; const aFrame: ICefFrame; const aUrl: ustring) of object;
  TOnTitleChange = procedure(aSender: TObject; const aBrowser: ICefBrowser; const aTitle: ustring) of object;
  TOnFaviconUrlchange = procedure(aSender: TObject; const aBrowser: ICefBrowser; aIconUrls: TStrings) of object;
  TOnTooltip = procedure(aSender: TObject; const aBrowser: ICefBrowser; var aText: ustring; out Result: Boolean) of object;
  TOnStatusMessage = procedure(aSender: TObject; const aBrowser: ICefBrowser; const aValue: ustring) of object;
  TOnConsoleMessage = procedure(aSender: TObject; const aBrowser: ICefBrowser; const aMessage: ustring; const aSource: ustring; aLine: cint; out Result: Boolean) of object;

  { CefDownloadHandler }
  TOnBeforeDownload = procedure(aSender: TObject; const aBrowser: ICefBrowser; const aDownloadItem: ICefDownloadItem; const aSuggestedName: ustring; const aCallback: ICefBeforeDownloadCallback) of object;
  TOnDownloadUpdated = procedure(aSender: TObject; const aBrowser: ICefBrowser; const aDownloadItem: ICefDownloadItem; const aCallback: ICefDownloadItemCallback) of object;

  { CefDragHandler }
  TOnDragEnter = procedure(aSender: TObject; const aBrowser: ICefBrowser; const aDragData: ICefDragData; aMask: TCefDragOperationsMask; out Result: Boolean) of object;

  { CefFindHandler }
  TOnFindResult = procedure(aSender: TObject; const aBrowser: ICefBrowser; aIdentifier: cint; aCount: cint; const aSelectionRect: TCefRect; aActiveMatchOrdinal: cint; aFinalUpdate: Boolean) of object;

  { CefFocusHandler }
  TOnTakeFocus = procedure(aSender: TObject; const aBrowser: ICefBrowser; aNext: Boolean) of object;
  TOnSetFocus = procedure(aSender: TObject; const aBrowser: ICefBrowser; aSource: TCefFocusSource; out Result: Boolean) of object;
  TOnGotFocus = procedure(aSender: TObject; const aBrowser: ICefBrowser) of object;

  { CefGeolocationHandler }
  TOnRequestGeolocationPermission = procedure(aSender: TObject; const aBrowser: ICefBrowser; const aRequestingUrl: ustring; aRequestId: cint; const aCallback: ICefGeolocationCallback; out Result: Boolean) of object;
  TOnCancelGeolocationPermission = procedure(aSender: TObject; const aBrowser: ICefBrowser; const aRequestingUrl: ustring; aRequestId: cint) of object;

  { CefJsDialogHandler }
  TOnJsdialog = procedure(aSender: TObject; const aBrowser: ICefBrowser; const aOriginUrl: ustring; const aAcceptLang: ustring; aDialogType: TCefJsdialogType; const aMessageText: ustring; const aDefaultPromptText: ustring; const aCallback: ICefJsdialogCallback; var aSuppressMessage: Boolean; out Result: Boolean) of object;
  TOnBeforeUnloadDialog = procedure(aSender: TObject; const aBrowser: ICefBrowser; const aMessageText: ustring; aIsReload: Boolean; const aCallback: ICefJsdialogCallback; out Result: Boolean) of object;
  TOnResetDialogState = procedure(aSender: TObject; const aBrowser: ICefBrowser) of object;
  TOnDialogClosed = procedure(aSender: TObject; const aBrowser: ICefBrowser) of object;

  { CefKeyboardHandler }
  TOnPreKeyEvent = procedure(aSender: TObject; const aBrowser: ICefBrowser; const aEvent: TCefKeyEvent; aOsEvent: TCefEventHandle; var aIsKeyboardShortcut: Boolean; out Result: Boolean) of object;
  TOnKeyEvent = procedure(aSender: TObject; const aBrowser: ICefBrowser; const aEvent: TCefKeyEvent; aOsEvent: TCefEventHandle; out Result: Boolean) of object;

  { CefLifeSpanHandler }
  TOnBeforePopup = procedure(aSender: TObject; const aBrowser: ICefBrowser; const aFrame: ICefFrame; var aTargetUrl: ustring; const aTargetFrameName: ustring; aTargetDisposition: TCefWindowOpenDisposition; aUserGesture: Boolean; var aPopupFeatures: TCefPopupFeatures; var aWindowInfo: TCefWindowInfo; var aClient: ICefClient; var aSettings: TCefBrowserSettings; var aNoJavascriptAccess: Boolean; out Result: Boolean) of object;
  TOnAfterCreated = procedure(aSender: TObject; const aBrowser: ICefBrowser) of object;
  TOnRunModal = procedure(aSender: TObject; const aBrowser: ICefBrowser; out Result: Boolean) of object;
  TOnClose = procedure(aSender: TObject; const aBrowser: ICefBrowser; out Result: Boolean) of object;
  TOnBeforeClose = procedure(aSender: TObject; const aBrowser: ICefBrowser) of object;

  { CefLoadHandler }
  TOnLoadingStateChange = procedure(aSender: TObject; const aBrowser: ICefBrowser; aIsLoading: Boolean; aCanGoBack: Boolean; aCanGoForward: Boolean) of object;
  TOnLoadStart = procedure(aSender: TObject; const aBrowser: ICefBrowser; const aFrame: ICefFrame) of object;
  TOnLoadEnd = procedure(aSender: TObject; const aBrowser: ICefBrowser; const aFrame: ICefFrame; aHttpStatusCode: cint) of object;
  TOnLoadError = procedure(aSender: TObject; const aBrowser: ICefBrowser; const aFrame: ICefFrame; aErrorCode: TCefErrorcode; const aErrorText: ustring; const aFailedUrl: ustring) of object;

  { CefRenderHandler }
  TOnGetRootScreenRect = procedure(aSender: TObject; const aBrowser: ICefBrowser; var aRect: TCefRect; out Result: Boolean) of object;
  TOnGetViewRect = procedure(aSender: TObject; const aBrowser: ICefBrowser; var aRect: TCefRect; out Result: Boolean) of object;
  TOnGetScreenPoint = procedure(aSender: TObject; const aBrowser: ICefBrowser; aViewX: cint; aViewY: cint; var aScreenX: cint; var aScreenY: cint; out Result: Boolean) of object;
  TOnGetScreenInfo = procedure(aSender: TObject; const aBrowser: ICefBrowser; var aScreenInfo: TCefScreenInfo; out Result: Boolean) of object;
  TOnPopupShow = procedure(aSender: TObject; const aBrowser: ICefBrowser; aShow: Boolean) of object;
  TOnPopupSize = procedure(aSender: TObject; const aBrowser: ICefBrowser; var aRect: TCefRect) of object;
  TOnPaint = procedure(aSender: TObject; const aBrowser: ICefBrowser; aType: TCefPaintElementType; aDirtyRectsCount: csize_t; const aDirtyRects: TCefRectArray; const aBuffer: cvoid; aWidth: cint; aHeight: cint) of object;
  TOnCursorChange = procedure(aSender: TObject; const aBrowser: ICefBrowser; aCursor: TCefCursorHandle; aType: TCefCursorType; var aCustomCursorInfo: TCefCursorInfo) of object;
  TOnStartDragging = procedure(aSender: TObject; const aBrowser: ICefBrowser; const aDragData: ICefDragData; aAllowedOps: TCefDragOperationsMask; aX: cint; aY: cint; out Result: Boolean) of object;
  TOnUpdateDragCursor = procedure(aSender: TObject; const aBrowser: ICefBrowser; aOperation: TCefDragOperationsMask) of object;
  TOnScrollOffsetChanged = procedure(aSender: TObject; const aBrowser: ICefBrowser; aX: cdouble; aY: cdouble) of object;

  { CefRequestHandler }
  TOnBeforeBrowse = procedure(aSender: TObject; const aBrowser: ICefBrowser; const aFrame: ICefFrame; const aRequest: ICefRequest; aIsRedirect: Boolean; out Result: Boolean) of object;
  TOnOpenUrlfromTab = procedure(aSender: TObject; const aBrowser: ICefBrowser; const aFrame: ICefFrame; const aTargetUrl: ustring; aTargetDisposition: TCefWindowOpenDisposition; aUserGesture: Boolean; out Result: Boolean) of object;
  TOnBeforeResourceLoad = procedure(aSender: TObject; const aBrowser: ICefBrowser; const aFrame: ICefFrame; const aRequest: ICefRequest; const aCallback: ICefRequestCallback; out Result: TCefReturnValue) of object;
  TOnGetResourceHandler = procedure(aSender: TObject; const aBrowser: ICefBrowser; const aFrame: ICefFrame; const aRequest: ICefRequest; out Result: ICefResourceHandler) of object;
  TOnResourceRedirect = procedure(aSender: TObject; const aBrowser: ICefBrowser; const aFrame: ICefFrame; const aRequest: ICefRequest; var aNewUrl: ustring) of object;
  TOnResourceResponse = procedure(aSender: TObject; const aBrowser: ICefBrowser; const aFrame: ICefFrame; const aRequest: ICefRequest; const aResponse: ICefResponse; out Result: Boolean) of object;
  TOnGetAuthCredentials = procedure(aSender: TObject; const aBrowser: ICefBrowser; const aFrame: ICefFrame; aIsProxy: Boolean; const aHost: ustring; aPort: cint; const aRealm: ustring; const aScheme: ustring; const aCallback: ICefAuthCallback; out Result: Boolean) of object;
  TOnQuotaRequest = procedure(aSender: TObject; const aBrowser: ICefBrowser; const aOriginUrl: ustring; aNewSize: cint64; const aCallback: ICefRequestCallback; out Result: Boolean) of object;
  TOnProtocolExecution = procedure(aSender: TObject; const aBrowser: ICefBrowser; const aUrl: ustring; var aAllowOsExecution: Boolean) of object;
  TOnCertificateError = procedure(aSender: TObject; const aBrowser: ICefBrowser; aCertError: TCefErrorcode; const aRequestUrl: ustring; const aSslInfo: ICefSslinfo; const aCallback: ICefRequestCallback; out Result: Boolean) of object;
  TOnBeforePluginLoad = procedure(aSender: TObject; const aBrowser: ICefBrowser; const aUrl: ustring; const aPolicyUrl: ustring; const aInfo: ICefWebPluginInfo; out Result: Boolean) of object;
  TOnPluginCrashed = procedure(aSender: TObject; const aBrowser: ICefBrowser; const aPluginPath: ustring) of object;
  TOnRenderViewReady = procedure(aSender: TObject; const aBrowser: ICefBrowser) of object;
  TOnRenderProcessTerminated = procedure(aSender: TObject; const aBrowser: ICefBrowser; aStatus: TCefTerminationStatus) of object;

  TCustomClientHandlerClass = class of TWACefClientHandler;
  TWACefClientHandler = class(TCefClientOwn, ICefClientHandler)
  private
    FEvents: IChromiumEvents;
    FContextMenuHandler: ICefContextMenuHandler;
    FDialogHandler: ICefDialogHandler;
    FDisplayHandler: ICefDisplayHandler;
    FDownloadHandler: ICefDownloadHandler;
    FDragHandler: ICefDragHandler;
    FFindHandler: ICefFindHandler;
    FFocusHandler: ICefFocusHandler;
    FGeolocationHandler: ICefGeolocationHandler;
    FJsDialogHandler: ICefJsDialogHandler;
    FKeyboardHandler: ICefKeyboardHandler;
    FLifeSpanHandler: ICefLifeSpanHandler;
    FLoadHandler: ICefLoadHandler;
    FRenderHandler: ICefRenderHandler;
    FRequestHandler: ICefRequestHandler;
  protected
    // Return the handler for context menus. If no handler is provided the default
		// implementation will be used.
		function GetContextMenuHandler: ICefContextMenuHandler; override;
		// Return the handler for dialogs. If no handler is provided the default
		// implementation will be used.
		function GetDialogHandler: ICefDialogHandler; override;
		// Return the handler for browser display state events.
		function GetDisplayHandler: ICefDisplayHandler; override;
		// Return the handler for download events. If no handler is returned downloads
		// will not be allowed.
		function GetDownloadHandler: ICefDownloadHandler; override;
		// Return the handler for drag events.
		function GetDragHandler: ICefDragHandler; override;
		// Return the handler for find result events.
		function GetFindHandler: ICefFindHandler; override;
		// Return the handler for focus events.
		function GetFocusHandler: ICefFocusHandler; override;
		// Return the handler for geolocation permissions requests. If no handler is
		// provided geolocation access will be denied by default.
		function GetGeolocationHandler: ICefGeolocationHandler; override;
		// Return the handler for JavaScript dialogs. If no handler is provided the
		// default implementation will be used.
		function GetJsdialogHandler: ICefJsdialogHandler; override;
		// Return the handler for keyboard events.
		function GetKeyboardHandler: ICefKeyboardHandler; override;
		// Return the handler for browser life span events.
		function GetLifeSpanHandler: ICefLifeSpanHandler; override;
		// Return the handler for browser load status events.
		function GetLoadHandler: ICefLoadHandler; override;
		// Return the handler for off-screen rendering events.
		function GetRenderHandler: ICefRenderHandler; override;
		// Return the handler for browser request events.
		function GetRequestHandler: ICefRequestHandler; override;
		// Called when a new message is received from a different process. Return true
		// (1) if the message was handled or false (0) otherwise. Do not keep a
		// reference to or attempt to access the message outside of this callback.
		function OnProcessMessageReceived(const aBrowser: ICefBrowser; aSourceProcess: TCefProcessId; const aMessage: ICefProcessMessage): Boolean; override;
    procedure Disconnect;
  public
    constructor Create(const aEvents: IChromiumEvents; aRenderer: Boolean); reintroduce; virtual;
  end;

implementation

{Private section}
{Public section}

constructor TWACefClientHandler.Create(const aEvents: IChromiumEvents; aRenderer: Boolean);
begin
  inherited Create;
  Disconnect;
  FEvents := aEvents;
  FContextMenuHandler := TWACefContextMenuHandler.Create(FEvents);
  FDialogHandler := TWACefDialogHandler.Create(FEvents);
  FDisplayHandler := TWACefDisplayHandler.Create(FEvents);
  FDownloadHandler := TWACefDownloadHandler.Create(FEvents);
  FDragHandler := TWACefDragHandler.Create(FEvents);
  FFindHandler := TWACefFindHandler.Create(FEvents);
  FFocusHandler := TWACefFocusHandler.Create(FEvents);
  FGeolocationHandler := TWACefGeolocationHandler.Create(FEvents);
  FJsDialogHandler := TWACefJsDialogHandler.Create(FEvents);
  FKeyboardHandler := TWACefKeyboardHandler.Create(FEvents);
  FLifeSpanHandler := TWACefLifeSpanHandler.Create(FEvents);
  FLoadHandler := TWACefLoadHandler.Create(FEvents);
  if aRenderer then
    FRenderHandler := TWACefRenderHandler.Create(FEvents)
  else
    FRenderHandler := nil;
  FRequestHandler := TWACefRequestHandler.Create(FEvents);
end;

procedure TWACefClientHandler.Disconnect;
begin
  FContextMenuHandler := nil;
  FDialogHandler := nil;
  FDisplayHandler := nil;
  FDownloadHandler := nil;
  FDragHandler := nil;
  FFindHandler := nil;
  FFocusHandler := nil;
  FGeolocationHandler := nil;
  FJsDialogHandler := nil;
  FKeyboardHandler := nil;
  FLifeSpanHandler := nil;
  FLoadHandler := nil;
  FRenderHandler := nil;
  FRequestHandler := nil;
end;

function TWACefClientHandler.GetContextMenuHandler: ICefContextMenuHandler;
begin
  Result := FContextMenuHandler;
end;

function TWACefClientHandler.GetDialogHandler: ICefDialogHandler;
begin
  Result := FDialogHandler;
end;

function TWACefClientHandler.GetDisplayHandler: ICefDisplayHandler;
begin
  Result := FDisplayHandler;
end;

function TWACefClientHandler.GetDownloadHandler: ICefDownloadHandler;
begin
  Result := FDownloadHandler;
end;

function TWACefClientHandler.GetDragHandler: ICefDragHandler;
begin
  Result := FDragHandler;
end;

function TWACefClientHandler.GetFindHandler;
begin
  Result := FFindHandler;
end;

function TWACefClientHandler.GetFocusHandler: ICefFocusHandler;
begin
  Result := FFocusHandler;
end;

function TWACefClientHandler.GetGeolocationHandler: ICefGeolocationHandler;
begin
  Result := FGeolocationHandler;
end;

function TWACefClientHandler.GetJsdialogHandler: ICefJsDialogHandler;
begin
  Result := FJsDialogHandler;
end;

function TWACefClientHandler.GetKeyboardHandler: ICefKeyboardHandler;
begin
  Result := FKeyboardHandler;
end;

function TWACefClientHandler.GetLifeSpanHandler: ICefLifeSpanHandler;
begin
  Result := FLifeSpanHandler;
end;

function TWACefClientHandler.GetLoadHandler: ICefLoadHandler;
begin
  Result := FLoadHandler;
end;

function TWACefClientHandler.GetRequestHandler: ICefRequestHandler;
begin
  Result := FRequestHandler;
end;

function TWACefClientHandler.OnProcessMessageReceived(const aBrowser: ICefBrowser; aSourceProcess: TCefProcessId;
  const aMessage: ICefProcessMessage): Boolean;
begin
  Result := FEvents.doOnProcessMessageReceived(aBrowser, aSourceProcess, aMessage);
end;

function TWACefClientHandler.GetRenderHandler: ICefRenderHandler;
begin
  Result := FRenderHandler;
end;

end.
