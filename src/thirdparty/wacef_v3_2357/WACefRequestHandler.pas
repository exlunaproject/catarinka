unit WACefRequestHandler;

interface

uses
  WACefTypes,
  WACefInterfaces,
  WAcefOwns;

type
  TWACefRequestHandler = class(TCefRequestHandlerOwn)
  private
    FEvents: IChromiumEvents;
  protected
    // Called on the UI thread before browser navigation. Return true (1) to
		// cancel the navigation or false (0) to allow the navigation to proceed. The
		// |request| object cannot be modified in this callback.
		// cef_load_handler_t::OnLoadingStateChange will be called twice in all cases.
		// If the navigation is allowed cef_load_handler_t::OnLoadStart and
		// cef_load_handler_t::OnLoadEnd will be called. If the navigation is canceled
		// cef_load_handler_t::OnLoadError will be called with an |errorCode| value of
		// ERR_ABORTED.
		function OnBeforeBrowse(const aBrowser: ICefBrowser; const aFrame: ICefFrame;
      const aRequest: ICefRequest; aIsRedirect: Boolean): Boolean; override;
		// Called on the UI thread before OnBeforeBrowse in certain limited cases
		// where navigating a new or different browser might be desirable. This
		// includes user-initiated navigation that might open in a special way (e.g.
		// links clicked via middle-click or ctrl + left-click) and certain types of
		// cross-origin navigation initiated from the renderer process (e.g.
		// navigating the top-level frame to/from a file URL). The |browser| and
		// |frame| values represent the source of the navigation. The
		// |target_disposition| value indicates where the user intended to navigate
		// the browser based on standard Chromium behaviors (e.g. current tab, new
		// tab, etc). The |user_gesture| value will be true (1) if the browser
		// navigated via explicit user gesture (e.g. clicking a link) or false (0) if
		// it navigated automatically (e.g. via the DomContentLoaded event). Return
		// true (1) to cancel the navigation or false (0) to allow the navigation to
		// proceed in the source browser's top-level frame.
		function OnOpenUrlfromTab(const aBrowser: ICefBrowser; const aFrame: ICefFrame; const aTargetUrl: ustring;
      aTargetDisposition: TCefWindowOpenDisposition; aUserGesture: Boolean): Boolean; override;
		// Called on the IO thread before a resource request is loaded. The |request|
		// object may be modified. Return RV_CONTINUE to continue the request
		// immediately. Return RV_CONTINUE_ASYNC and call cef_request_tCallback::
		// cont() at a later time to continue or cancel the request asynchronously.
		// Return RV_CANCEL to cancel the request immediately.
		//
		function OnBeforeResourceLoad(const aBrowser: ICefBrowser; const aFrame: ICefFrame; const aRequest: ICefRequest;
      const aCallback: ICefRequestCallback): TCefReturnValue; override;
		// Called on the IO thread before a resource is loaded. To allow the resource
		// to load normally return NULL. To specify a handler for the resource return
		// a cef_resource_handler_t object. The |request| object should not be
		// modified in this callback.
		function GetResourceHandler(const aBrowser: ICefBrowser; const aFrame: ICefFrame;
      const aRequest: ICefRequest): ICefResourceHandler; override;
		// Called on the IO thread when a resource load is redirected. The |request|
		// parameter will contain the old URL and other request-related information.
		// The |new_url| parameter will contain the new URL and can be changed if
		// desired. The |request| object cannot be modified in this callback.
		procedure OnResourceRedirect(const aBrowser: ICefBrowser; const aFrame: ICefFrame;
      const aRequest: ICefRequest; var aNewUrl: ustring); override;
		// Called on the IO thread when a resource response is received. To allow the
		// resource to load normally return false (0). To redirect or retry the
		// resource modify |request| (url, headers or post body) and return true (1).
		// The |response| object cannot be modified in this callback.
		function OnResourceResponse(const aBrowser: ICefBrowser; const aFrame: ICefFrame;
      const aRequest: ICefRequest; const aResponse: ICefResponse): Boolean; override;
		// Called on the IO thread when the browser needs credentials from the user.
		// |isProxy| indicates whether the host is a proxy server. |host| contains the
		// hostname and |port| contains the port number. Return true (1) to continue
		// the request and call cef_auth_callback_t::cont() either in this function or
		// at a later time when the authentication information is available. Return
		// false (0) to cancel the request immediately.
		function GetAuthCredentials(const aBrowser: ICefBrowser; const aFrame: ICefFrame; aIsProxy: Boolean;
      const aHost: ustring; aPort: cint; const aRealm: ustring; const aScheme: ustring; const aCallback: ICefAuthCallback): Boolean; override;
		// Called on the IO thread when JavaScript requests a specific storage quota
		// size via the webkitStorageInfo.requestQuota function. |origin_url| is the
		// origin of the page making the request. |new_size| is the requested quota
		// size in bytes. Return true (1) to continue the request and call
		// cef_request_tCallback::cont() either in this function or at a later time to
		// grant or deny the request. Return false (0) to cancel the request
		// immediately.
		function OnQuotaRequest(const aBrowser: ICefBrowser; const aOriginUrl: ustring; aNewSize: cint64;
      const aCallback: ICefRequestCallback): Boolean; override;
		// Called on the UI thread to handle requests for URLs with an unknown
		// protocol component. Set |allow_os_execution| to true (1) to attempt
		// execution via the registered OS protocol handler, if any. SECURITY WARNING:
		// YOU SHOULD USE THIS METHOD TO ENFORCE RESTRICTIONS BASED ON SCHEME, HOST OR
		// OTHER URL ANALYSIS BEFORE ALLOWING OS EXECUTION.
		procedure OnProtocolExecution(const aBrowser: ICefBrowser; const aUrl: ustring; out aAllowOsExecution: Boolean); override;
		// Called on the UI thread to handle requests for URLs with an invalid SSL
		// certificate. Return true (1) and call cef_request_tCallback::cont() either
		// in this function or at a later time to continue or cancel the request.
		// Return false (0) to cancel the request immediately. If |callback| is NULL
		// the error cannot be recovered from and the request will be canceled
		// automatically. If CefSettings.ignore_certificate_errors is set all invalid
		// certificates will be accepted without calling this function.
		function OnCertificateError(const aBrowser: ICefBrowser; aCertError: TCefErrorcode; const aRequestUrl: ustring;
      const aSslInfo: ICefSslinfo; const aCallback: ICefRequestCallback): Boolean; override;
		// Called on the browser process IO thread before a plugin is loaded. Return
		// true (1) to block loading of the plugin.
		function OnBeforePluginLoad(const aBrowser: ICefBrowser; const aUrl: ustring; const aPolicyUrl: ustring;
      const aInfo: ICefWebPluginInfo): Boolean; override;
		// Called on the browser process UI thread when a plugin has crashed.
		// |plugin_path| is the path of the plugin that crashed.
		procedure OnPluginCrashed(const aBrowser: ICefBrowser; const aPluginPath: ustring); override;
		// Called on the browser process UI thread when the render view associated
		// with |browser| is ready to receive/handle IPC messages in the render
		// process.
		procedure OnRenderViewReady(const aBrowser: ICefBrowser); override;
		// Called on the browser process UI thread when the render process terminates
		// unexpectedly. |status| indicates how the process terminated.
		procedure OnRenderProcessTerminated(const aBrowser: ICefBrowser; aStatus: TCefTerminationStatus); override;
  public
    constructor Create(const aEvents: IChromiumEvents); reintroduce; virtual;
  end;


implementation

//..............................................................................TWACefRequestHandler
{Private section}
{Protected section}
// Called on the UI thread before browser navigation. Return true (1) to
// cancel the navigation or false (0) to allow the navigation to proceed. The
// |request| object cannot be modified in this callback.
// cef_load_handler_t::OnLoadingStateChange will be called twice in all cases.
// If the navigation is allowed cef_load_handler_t::OnLoadStart and
// cef_load_handler_t::OnLoadEnd will be called. If the navigation is canceled
// cef_load_handler_t::OnLoadError will be called with an |errorCode| value of
// ERR_ABORTED.
function TWACefRequestHandler.OnBeforeBrowse(const aBrowser: ICefBrowser; const aFrame: ICefFrame;
  const aRequest: ICefRequest; aIsRedirect:boolean): Boolean;
begin
  Result:=FEvents.doOnBeforeBrowse(
    aBrowser,
    aFrame,
    aRequest,
    aIsRedirect
  );
end;

// Called on the UI thread before OnBeforeBrowse in certain limited cases
// where navigating a new or different browser might be desirable. This
// includes user-initiated navigation that might open in a special way (e.g.
// links clicked via middle-click or ctrl + left-click) and certain types of
// cross-origin navigation initiated from the renderer process (e.g.
// navigating the top-level frame to/from a file URL). The |browser| and
// |frame| values represent the source of the navigation. The
// |target_disposition| value indicates where the user intended to navigate
// the browser based on standard Chromium behaviors (e.g. current tab, new
// tab, etc). The |user_gesture| value will be true (1) if the browser
// navigated via explicit user gesture (e.g. clicking a link) or false (0) if
// it navigated automatically (e.g. via the DomContentLoaded event). Return
// true (1) to cancel the navigation or false (0) to allow the navigation to
// proceed in the source browser's top-level frame.
function TWACefRequestHandler.OnOpenUrlfromTab(const aBrowser: ICefBrowser; const aFrame: ICefFrame; const aTargetUrl: ustring;
  aTargetDisposition: TCefWindowOpenDisposition; aUserGesture: Boolean): Boolean;
begin
  Result := FEvents.doOnOpenUrlfromTab(
    aBrowser,
    aFrame,
    aTargetUrl,
    aTargetDisposition,
    aUserGesture
  );
end;

// Called on the IO thread before a resource request is loaded. The |request|
// object may be modified. Return RV_CONTINUE to continue the request
// immediately. Return RV_CONTINUE_ASYNC and call cef_request_tCallback::
// cont() at a later time to continue or cancel the request asynchronously.
// Return RV_CANCEL to cancel the request immediately.
//
function TWACefRequestHandler.OnBeforeResourceLoad(const aBrowser: ICefBrowser; const aFrame: ICefFrame;
  const aRequest: ICefRequest; const aCallback: ICefRequestCallback): TCefReturnValue;
begin
  Result := FEvents.doOnBeforeResourceLoad(
    aBrowser,
    aFrame,
    aRequest,
    aCallback
  );
end;

// Called on the IO thread before a resource is loaded. To allow the resource
// to load normally return NULL. To specify a handler for the resource return
// a cef_resource_handler_t object. The |request| object should not be
// modified in this callback.
function TWACefRequestHandler.GetResourceHandler(const aBrowser: ICefBrowser; const aFrame: ICefFrame;
  const aRequest: ICefRequest): ICefResourceHandler;
begin
  Result := FEvents.doOnGetResourceHandler(
    aBrowser,
    aFrame,
    aRequest
  );
end;

// Called on the IO thread when a resource load is redirected. The |request|
// parameter will contain the old URL and other request-related information.
// The |new_url| parameter will contain the new URL and can be changed if
// desired. The |request| object cannot be modified in this callback.
procedure TWACefRequestHandler.OnResourceRedirect(const aBrowser: ICefBrowser; const aFrame: ICefFrame;
  const aRequest: ICefRequest; var aNewUrl: ustring);
begin
  FEvents.doOnResourceRedirect(
    aBrowser,
    aFrame,
    aRequest,
    aNewUrl
  );
end;

// Called on the IO thread when a resource response is received. To allow the
// resource to load normally return false (0). To redirect or retry the
// resource modify |request| (url, headers or post body) and return true (1).
// The |response| object cannot be modified in this callback.
function TWACefRequestHandler.OnResourceResponse(const aBrowser: ICefBrowser; const aFrame: ICefFrame;
  const aRequest: ICefRequest; const aResponse: ICefResponse): Boolean;
begin
  Result := FEvents.doOnResourceResponse(
    aBrowser,
    aFrame,
    aRequest,
    aResponse
  );
end;

// Called on the IO thread when the browser needs credentials from the user.
// |isProxy| indicates whether the host is a proxy server. |host| contains the
// hostname and |port| contains the port number. Return true (1) to continue
// the request and call cef_auth_callback_t::cont() either in this function or
// at a later time when the authentication information is available. Return
// false (0) to cancel the request immediately.
function TWACefRequestHandler.GetAuthCredentials(const aBrowser: ICefBrowser; const aFrame: ICefFrame; aIsProxy: Boolean;
  const aHost: ustring; aPort: cint; const aRealm: ustring; const aScheme: ustring; const aCallback: ICefAuthCallback): Boolean;
begin
  Result := FEvents.doOnGetAuthCredentials(
    aBrowser,
    aFrame,
    aIsProxy,
    aHost,
    aPort,
    aRealm,
    aScheme,
    aCallback
  );
end;

// Called on the IO thread when JavaScript requests a specific storage quota
// size via the webkitStorageInfo.requestQuota function. |origin_url| is the
// origin of the page making the request. |new_size| is the requested quota
// size in bytes. Return true (1) to continue the request and call
// cef_request_tCallback::cont() either in this function or at a later time to
// grant or deny the request. Return false (0) to cancel the request
// immediately.
function TWACefRequestHandler.OnQuotaRequest(const aBrowser: ICefBrowser; const aOriginUrl: ustring; aNewSize: cint64;
  const aCallback: ICefRequestCallback): Boolean;
begin
  Result := FEvents.doOnQuotaRequest(
    aBrowser,
    aOriginUrl,
    aNewSize,
    aCallback
  );
end;

// Called on the UI thread to handle requests for URLs with an unknown
// protocol component. Set |allow_os_execution| to true (1) to attempt
// execution via the registered OS protocol handler, if any. SECURITY WARNING:
// YOU SHOULD USE THIS METHOD TO ENFORCE RESTRICTIONS BASED ON SCHEME, HOST OR
// OTHER URL ANALYSIS BEFORE ALLOWING OS EXECUTION.
procedure TWACefRequestHandler.OnProtocolExecution(const aBrowser: ICefBrowser; const aUrl: ustring; out aAllowOsExecution: Boolean);
begin
  FEvents.doOnProtocolExecution(
    aBrowser,
    aUrl,
    aAllowOsExecution
  );
end;

// Called on the UI thread to handle requests for URLs with an invalid SSL
// certificate. Return true (1) and call cef_request_tCallback::cont() either
// in this function or at a later time to continue or cancel the request.
// Return false (0) to cancel the request immediately. If |callback| is NULL
// the error cannot be recovered from and the request will be canceled
// automatically. If CefSettings.ignore_certificate_errors is set all invalid
// certificates will be accepted without calling this function.
function TWACefRequestHandler.OnCertificateError(const aBrowser: ICefBrowser; aCertError: TCefErrorcode;
  const aRequestUrl: ustring; const aSslInfo: ICefSslinfo; const aCallback: ICefRequestCallback): Boolean;
begin
  Result := FEvents.doOnCertificateError(
    aBrowser,
    aCertError,
    aRequestUrl,
    aSslInfo,
    aCallback
  );
end;

// Called on the browser process IO thread before a plugin is loaded. Return
// true (1) to block loading of the plugin.
function TWACefRequestHandler.OnBeforePluginLoad(const aBrowser: ICefBrowser; const aUrl: ustring; const aPolicyUrl: ustring;
  const aInfo: ICefWebPluginInfo): Boolean;
begin
  Result := FEvents.doOnBeforePluginLoad(
    aBrowser,
    aUrl,
    aPolicyUrl,
    aInfo
  );
end;

// Called on the browser process UI thread when a plugin has crashed.
// |plugin_path| is the path of the plugin that crashed.
procedure TWACefRequestHandler.OnPluginCrashed(const aBrowser: ICefBrowser;
  const aPluginPath: ustring);
begin
  FEvents.doOnPluginCrashed(
    aBrowser,
    aPluginPath
  );
end;

// Called on the browser process UI thread when the render view associated
// with |browser| is ready to receive/handle IPC messages in the render
// process.
procedure TWACefRequestHandler.OnRenderViewReady(const aBrowser: ICefBrowser);
begin
  FEvents.doOnRenderViewReady(aBrowser);
end;

// Called on the browser process UI thread when the render process terminates
// unexpectedly. |status| indicates how the process terminated.
procedure TWACefRequestHandler.OnRenderProcessTerminated(const aBrowser: ICefBrowser; aStatus: TCefTerminationStatus);
begin
  FEvents.doOnRenderProcessTerminated(
    aBrowser,
    aStatus
  );
end;

{Public section}
constructor TWACefRequestHandler.Create(const aEvents: IChromiumEvents);
begin
  inherited Create;
  FEvents := aEvents;
end;

end.

