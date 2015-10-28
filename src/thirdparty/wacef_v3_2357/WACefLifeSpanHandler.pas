unit WACefLifeSpanHandler;

interface

uses
  WACefTypes,
  WACefInterfaces,
  WAcefOwns;

type
  TWACefLifeSpanHandler = class(TCefLifeSpanHandlerOwn)
  private
    FEvents: IChromiumEvents;
  protected
    // Called on the IO thread before a new popup browser is created. The
		// |browser| and |frame| values represent the source of the popup request. The
		// |target_url| and |target_frame_name| values indicate where the popup
		// browser should navigate and may be NULL if not specified with the request.
		// The |target_disposition| value indicates where the user intended to open
		// the popup (e.g. current tab, new tab, etc). The |user_gesture| value will
		// be true (1) if the popup was opened via explicit user gesture (e.g.
		// clicking a link) or false (0) if the popup opened automatically (e.g. via
		// the DomContentLoaded event). The |popupFeatures| structure contains
		// additional information about the requested popup window. To allow creation
		// of the popup browser optionally modify |windowInfo|, |client|, |settings|
		// and |no_javascript_access| and return false (0). To cancel creation of the
		// popup browser return true (1). The |client| and |settings| values will
		// default to the source browser's values. If the |no_javascript_access| value
		// is set to false (0) the new browser will not be scriptable and may not be
		// hosted in the same renderer process as the source browser.
		function OnBeforePopup(const aBrowser: ICefBrowser; const aFrame: ICefFrame; var aTargetUrl: ustring;
      const aTargetFrameName: ustring; aTargetDisposition: TCefWindowOpenDisposition; aUserGesture: Boolean;
      var aPopupFeatures: TCefPopupFeatures; var aWindowInfo: TCefWindowInfo; var aClient: ICefClient;
      var aSettings: TCefBrowserSettings; var aNoJavascriptAccess: Boolean): Boolean; override;
		// Called after a new browser is created.
		procedure OnAfterCreated(const aBrowser: ICefBrowser); override;
		// Called when a modal window is about to display and the modal loop should
		// begin running. Return false (0) to use the default modal loop
		// implementation or true (1) to use a custom implementation.
		function RunModal(const aBrowser: ICefBrowser): Boolean; override;
		// Called when a browser has recieved a request to close. This may result
		// directly from a call to cef_browser_host_t::close_browser() or indirectly
		// if the browser is a top-level OS window created by CEF and the user
		// attempts to close the window. This function will be called after the
		// JavaScript 'onunload' event has been fired. It will not be called for
		// browsers after the associated OS window has been destroyed (for those
		// browsers it is no longer possible to cancel the close).
		//
		// If CEF created an OS window for the browser returning false (0) will send
		// an OS close notification to the browser window's top-level owner (e.g.
		// WM_CLOSE on Windows, performClose: on OS-X and "delete_event" on Linux). If
		// no OS window exists (window rendering disabled) returning false (0) will
		// cause the browser object to be destroyed immediately. Return true (1) if
		// the browser is parented to another window and that other window needs to
		// receive close notification via some non-standard technique.
		//
		// If an application provides its own top-level window it should handle OS
		// close notifications by calling cef_browser_host_t::CloseBrowser(false (0))
		// instead of immediately closing (see the example below). This gives CEF an
		// opportunity to process the 'onbeforeunload' event and optionally cancel the
		// close before do_close() is called.
		//
		// The cef_life_span_handler_t::on_before_close() function will be called
		// immediately before the browser object is destroyed. The application should
		// only exit after on_before_close() has been called for all existing
		// browsers.
		//
		// If the browser represents a modal window and a custom modal loop
		// implementation was provided in cef_life_span_handler_t::run_modal() this
		// callback should be used to restore the opener window to a usable state.
		//
		// By way of example consider what should happen during window close when the
		// browser is parented to an application-provided top-level OS window. 1.
		// User clicks the window close button which sends an OS close
		//     notification (e.g. WM_CLOSE on Windows, performClose: on OS-X and
		//     "delete_event" on Linux).
		// 2.  Application's top-level window receives the close notification and:
		//     A. Calls CefBrowserHost::CloseBrowser(false).
		//     B. Cancels the window close.
		// 3.  JavaScript 'onbeforeunload' handler executes and shows the close
		//     confirmation dialog (which can be overridden via
		//     CefJSDialogHandler::OnBeforeUnloadDialog()).
		// 4.  User approves the close. 5.  JavaScript 'onunload' handler executes. 6.
		// Application's do_close() handler is called. Application will:
		//     A. Set a flag to indicate that the next close attempt will be allowed.
		//     B. Return false.
		// 7.  CEF sends an OS close notification. 8.  Application's top-level window
		// receives the OS close notification and
		//     allows the window to close based on the flag from #6B.
		// 9.  Browser OS window is destroyed. 10. Application's
		// cef_life_span_handler_t::on_before_close() handler is called and
		//     the browser object is destroyed.
		// 11. Application exits by calling cef_quit_message_loop() if no other
		// browsers
		//     exist.
		function DoClose(const aBrowser: ICefBrowser): Boolean; override;
		// Called just before a browser is destroyed. Release all references to the
		// browser object and do not attempt to execute any functions on the browser
		// object after this callback returns. If this is a modal window and a custom
		// modal loop implementation was provided in run_modal() this callback should
		// be used to exit the custom modal loop. See do_close() documentation for
		// additional usage information.
		procedure OnBeforeClose(const aBrowser: ICefBrowser); override;
  public
    constructor Create(const aEvents: IChromiumEvents); reintroduce; virtual;
  end;


implementation

//..............................................................................TWACefLifeSpanHandler
{Private section}
{Protected section}
// Called on the IO thread before a new popup browser is created. The
// |browser| and |frame| values represent the source of the popup request. The
// |target_url| and |target_frame_name| values indicate where the popup
// browser should navigate and may be NULL if not specified with the request.
// The |target_disposition| value indicates where the user intended to open
// the popup (e.g. current tab, new tab, etc). The |user_gesture| value will
// be true (1) if the popup was opened via explicit user gesture (e.g.
// clicking a link) or false (0) if the popup opened automatically (e.g. via
// the DomContentLoaded event). The |popupFeatures| structure contains
// additional information about the requested popup window. To allow creation
// of the popup browser optionally modify |windowInfo|, |client|, |settings|
// and |no_javascript_access| and return false (0). To cancel creation of the
// popup browser return true (1). The |client| and |settings| values will
// default to the source browser's values. If the |no_javascript_access| value
// is set to false (0) the new browser will not be scriptable and may not be
// hosted in the same renderer process as the source browser.
function TWACefLifeSpanHandler.OnBeforePopup(const aBrowser: ICefBrowser; const aFrame: ICefFrame;
  var aTargetUrl: ustring; const aTargetFrameName: ustring; aTargetDisposition: TCefWindowOpenDisposition;
  aUserGesture: Boolean; var aPopupFeatures: TCefPopupFeatures; var aWindowInfo: TCefWindowInfo;
  var aClient: ICefClient; var aSettings: TCefBrowserSettings; var aNoJavascriptAccess: Boolean): Boolean;
begin
  Result := FEvents.doOnBeforePopup(
    aBrowser,
    aFrame,
    aTargetURL,
    aTargetFrameName,
    aTargetDisposition,
    aUserGesture,
    aPopupFeatures,
    aWindowInfo,
    aClient,
    aSettings,
    aNoJavascriptAccess
  );
end;

// Called after a new browser is created.
procedure TWACefLifeSpanHandler.OnAfterCreated(const aBrowser: ICefBrowser);
begin
  FEvents.doOnAfterCreated(aBrowser);
end;

// Called when a modal window is about to display and the modal loop should
// begin running. Return false (0) to use the default modal loop
// implementation or true (1) to use a custom implementation.
function TWACefLifeSpanHandler.RunModal(const aBrowser: ICefBrowser): Boolean;
begin
  Result := FEvents.doRunModal(aBrowser);
end;

// Called when a browser has recieved a request to close. This may result
// directly from a call to cef_browser_host_t::close_browser() or indirectly
// if the browser is a top-level OS window created by CEF and the user
// attempts to close the window. This function will be called after the
// JavaScript 'onunload' event has been fired. It will not be called for
// browsers after the associated OS window has been destroyed (for those
// browsers it is no longer possible to cancel the close).
//
// If CEF created an OS window for the browser returning false (0) will send
// an OS close notification to the browser window's top-level owner (e.g.
// WM_CLOSE on Windows, performClose: on OS-X and "delete_event" on Linux). If
// no OS window exists (window rendering disabled) returning false (0) will
// cause the browser object to be destroyed immediately. Return true (1) if
// the browser is parented to another window and that other window needs to
// receive close notification via some non-standard technique.
//
// If an application provides its own top-level window it should handle OS
// close notifications by calling cef_browser_host_t::CloseBrowser(false (0))
// instead of immediately closing (see the example below). This gives CEF an
// opportunity to process the 'onbeforeunload' event and optionally cancel the
// close before do_close() is called.
//
// The cef_life_span_handler_t::on_before_close() function will be called
// immediately before the browser object is destroyed. The application should
// only exit after on_before_close() has been called for all existing
// browsers.
//
// If the browser represents a modal window and a custom modal loop
// implementation was provided in cef_life_span_handler_t::run_modal() this
// callback should be used to restore the opener window to a usable state.
//
// By way of example consider what should happen during window close when the
// browser is parented to an application-provided top-level OS window. 1.
// User clicks the window close button which sends an OS close
//     notification (e.g. WM_CLOSE on Windows, performClose: on OS-X and
//     "delete_event" on Linux).
// 2.  Application's top-level window receives the close notification and:
//     A. Calls CefBrowserHost::CloseBrowser(false).
//     B. Cancels the window close.
// 3.  JavaScript 'onbeforeunload' handler executes and shows the close
//     confirmation dialog (which can be overridden via
//     CefJSDialogHandler::OnBeforeUnloadDialog()).
// 4.  User approves the close. 5.  JavaScript 'onunload' handler executes. 6.
// Application's do_close() handler is called. Application will:
//     A. Set a flag to indicate that the next close attempt will be allowed.
//     B. Return false.
// 7.  CEF sends an OS close notification. 8.  Application's top-level window
// receives the OS close notification and
//     allows the window to close based on the flag from #6B.
// 9.  Browser OS window is destroyed. 10. Application's
// cef_life_span_handler_t::on_before_close() handler is called and
//     the browser object is destroyed.
// 11. Application exits by calling cef_quit_message_loop() if no other
// browsers
//     exist.
function TWACefLifeSpanHandler.DoClose(const aBrowser: ICefBrowser): Boolean;
begin
  Result := FEvents.doDoClose(aBrowser);
end;

// Called just before a browser is destroyed. Release all references to the
// browser object and do not attempt to execute any functions on the browser
// object after this callback returns. If this is a modal window and a custom
// modal loop implementation was provided in run_modal() this callback should
// be used to exit the custom modal loop. See do_close() documentation for
// additional usage information.
procedure TWACefLifeSpanHandler.OnBeforeClose(const aBrowser: ICefBrowser);
begin
  FEvents.doOnBeforeClose(aBrowser);
end;

{Public section}
constructor TWACefLifeSpanHandler.Create(const aEvents: IChromiumEvents);
begin
  inherited Create;
  FEvents := aEvents;
end;

end.

