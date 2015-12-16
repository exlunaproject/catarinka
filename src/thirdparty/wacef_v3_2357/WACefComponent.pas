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
  WACefClientHandler,
  WACefLib,
  WACefInterfaces,
  WACefRefs,
  WACefOwns,
  WACefTypes,
  SysUtils;

type
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

  TWACustomChromium = class(TWinControl, IChromiumEvents)
  private
    FCanvas: TCanvas;
    FHandler: ICefClient;
    FBrowser: ICefBrowser;
    FBrowserId: Integer;
    FDefaultUrl: ustring;

    FPopup: Boolean;
    FOSR: Boolean;

    { CefClient }
		FOnProcessMessageReceived: TOnProcessMessageReceived;
    { CefContextMenuHandler }
		FOnBeforeContextMenu: TOnBeforeContextMenu;
		FOnContextMenuCommand: TOnContextMenuCommand;
		FOnContextMenuDismissed: TOnContextMenuDismissed;
    { CefDialogHandler }
		FOnFileDialog: TOnFileDialog;
    { CefDisplayHandler }
		FOnAddressChange: TOnAddressChange;
		FOnTitleChange: TOnTitleChange;
		FOnFaviconUrlchange: TOnFaviconUrlchange;
		FOnTooltip: TOnTooltip;
		FOnStatusMessage: TOnStatusMessage;
		FOnConsoleMessage: TOnConsoleMessage;
    { CefDownloadHandler }
		FOnBeforeDownload: TOnBeforeDownload;
		FOnDownloadUpdated: TOnDownloadUpdated;
    { CefDragHandler }
		FOnDragEnter: TOnDragEnter;
    { CefFindHandler }
		FOnFindResult: TOnFindResult;
    { CefFocusHandler }
		FOnTakeFocus: TOnTakeFocus;
		FOnSetFocus: TOnSetFocus;
		FOnGotFocus: TOnGotFocus;
    { CefGeolocationHandler }
		FOnRequestGeolocationPermission: TOnRequestGeolocationPermission;
		FOnCancelGeolocationPermission: TOnCancelGeolocationPermission;
    { CefJsDialogHandler }
		FOnJsdialog: TOnJsdialog;
		FOnBeforeUnloadDialog: TOnBeforeUnloadDialog;
		FOnResetDialogState: TOnResetDialogState;
		FOnDialogClosed: TOnDialogClosed;
    { CefKeyboardHandler }
		FOnPreKeyEvent: TOnPreKeyEvent;
		FOnKeyEvent: TOnKeyEvent;
    { CefLifeSpanHandler }
		FOnBeforePopup: TOnBeforePopup;
		FOnAfterCreated: TOnAfterCreated;
		FOnRunModal: TOnRunModal;
		FOnClose: TOnClose;
		FOnBeforeClose: TOnBeforeClose;
    { CefLoadHandler }
		FOnLoadingStateChange: TOnLoadingStateChange;
		FOnLoadStart: TOnLoadStart;
		FOnLoadEnd: TOnLoadEnd;
		FOnLoadError: TOnLoadError;
    { CefRenderHandler }
		FOnGetRootScreenRect: TOnGetRootScreenRect;
		FOnGetViewRect: TOnGetViewRect;
		FOnGetScreenPoint: TOnGetScreenPoint;
		FOnGetScreenInfo: TOnGetScreenInfo;
		FOnPopupShow: TOnPopupShow;
		FOnPopupSize: TOnPopupSize;
		FOnPaint: TOnPaint;
		FOnCursorChange: TOnCursorChange;
		FOnStartDragging: TOnStartDragging;
		FOnUpdateDragCursor: TOnUpdateDragCursor;
		FOnScrollOffsetChanged: TOnScrollOffsetChanged;
    { CefRequestHandler }
    FOnBeforeBrowse: TOnBeforeBrowse;
		FOnOpenUrlfromTab: TOnOpenUrlfromTab;
		FOnBeforeResourceLoad: TOnBeforeResourceLoad;
		FOnGetResourceHandler: TOnGetResourceHandler;
		FOnResourceRedirect: TOnResourceRedirect;
		FOnResourceResponse: TOnResourceResponse;
		FOnGetAuthCredentials: TOnGetAuthCredentials;
		FOnQuotaRequest: TOnQuotaRequest;
		FOnProtocolExecution: TOnProtocolExecution;
		FOnCertificateError: TOnCertificateError;
		FOnBeforePluginLoad: TOnBeforePluginLoad;
		FOnPluginCrashed: TOnPluginCrashed;
		FOnRenderViewReady: TOnRenderViewReady;
		FOnRenderProcessTerminated: TOnRenderProcessTerminated;

    FOptions: TChromiumOptions;
    FUserStyleSheetLocation: ustring;
    FDefaultEncoding: ustring;
    FFontOptions: TChromiumFontOptions;
    FTransparentPainting: Boolean;

    FData: Pointer;

    procedure Init;
    procedure GetSettings(var settings: TCefBrowserSettings);
    procedure CreateBrowser; virtual;
  protected
    {$IFDEF MSWINDOWS}
    procedure WndProc(var Message: TMessage); override;
    procedure Resize; override;
    {$ENDIF}
    {$IFDEF FPC}
    procedure WMPaint(var Msg : TLMPaint); message LM_PAINT;
    procedure CreateWnd; override;
    {$ELSE}
    procedure WMPaint(var Msg : TWMPaint); message WM_PAINT;
    procedure CreateWindowHandle(const Params: TCreateParams); override;
    {$ENDIF}

    { CefClient }

    // Called when a new message is received from a different process. Return true
		// (1) if the message was handled or false (0) otherwise. Do not keep a
		// reference to or attempt to access the message outside of this callback.
		function doOnProcessMessageReceived(const aBrowser: ICefBrowser; aSourceProcess: TCefProcessId; const aMessage: ICefProcessMessage): Boolean;

    { CefContextMenuHandler }

    // Called before a context menu is displayed. |params| provides information
		// about the context menu state. |model| initially contains the default
		// context menu. The |model| can be cleared to show no context menu or
		// modified to show a custom menu. Do not keep references to |params| or
		// |model| outside of this callback.
		procedure doOnBeforeContextMenu(const aBrowser: ICefBrowser; const aFrame: ICefFrame; const aParams: ICefContextMenuParams; const aModel: ICefMenuModel);
		// Called to execute a command selected from the context menu. Return true (1)
		// if the command was handled or false (0) for the default implementation. See
		// cef_menu_id_t for the command ids that have default implementations. All
		// user-defined command ids should be between MENU_ID_USER_FIRST and
		// MENU_ID_USER_LAST. |params| will have the same values as what was passed to
		// on_before_context_menu(). Do not keep a reference to |params| outside of
		// this callback.
		function doOnContextMenuCommand(const aBrowser: ICefBrowser; const aFrame: ICefFrame; const aParams: ICefContextMenuParams; aCommandId: cint; aEventFlags: TCefEventFlags): Boolean;
		// Called when the context menu is dismissed irregardless of whether the menu
		// was NULL or a command was selected.
		procedure doOnContextMenuDismissed(const aBrowser: ICefBrowser; const aFrame: ICefFrame);

    { CefDialogHandler }

    // Called to run a file chooser dialog. |mode| represents the type of dialog
		// to display. |title| to the title to be used for the dialog and may be NULL
		// to show the default title ("Open" or "Save" depending on the mode).
		// |default_file_path| is the path with optional directory and/or file name
		// component that should be initially selected in the dialog. |accept_filters|
		// are used to restrict the selectable file types and may any combination of
		// (a) valid lower-cased MIME types (e.g. "text/*" or "image/*"), (b)
		// individual file extensions (e.g. ".txt" or ".png"), or (c) combined
		// description and file extension delimited using "|" and ";" (e.g. "Image
		// Types|.png;.gif;.jpg"). |selected_accept_filter| is the 0-based index of
		// the filter that should be selected by default. To display a custom dialog
		// return true (1) and execute |callback| either inline or at a later time. To
		// display the default dialog return false (0).
		function doOnFileDialog(const aBrowser: ICefBrowser; aMode: TCefFileDialogMode; const aTitle: ustring; const aDefaultFilePath: ustring; aAcceptFilters: TStrings; aSelectedAcceptFilter: cint; const aCallback: ICefFileDialogCallback): Boolean;

    { CefDisplayHandler }

    // Called when a frame's address has changed.
		procedure doOnAddressChange(const aBrowser: ICefBrowser; const aFrame: ICefFrame; const aUrl: ustring);
		// Called when the page title changes.
		procedure doOnTitleChange(const aBrowser: ICefBrowser; const aTitle: ustring);
		// Called when the page icon changes.
		procedure doOnFaviconUrlchange(const aBrowser: ICefBrowser; aIconUrls: TStrings);
		// Called when the browser is about to display a tooltip. |text| contains the
		// text that will be displayed in the tooltip. To handle the display of the
		// tooltip yourself return true (1). Otherwise, you can optionally modify
		// |text| and then return false (0) to allow the browser to display the
		// tooltip. When window rendering is disabled the application is responsible
		// for drawing tooltips and the return value is ignored.
		function doOnTooltip(const aBrowser: ICefBrowser; var aText: ustring): Boolean;
		// Called when the browser receives a status message. |value| contains the
		// text that will be displayed in the status message.
		procedure doOnStatusMessage(const aBrowser: ICefBrowser; const aValue: ustring);
		// Called to display a console message. Return true (1) to stop the message
		// from being output to the console.
		function doOnConsoleMessage(const aBrowser: ICefBrowser; const aMessage: ustring; const aSource: ustring; aLine: cint): Boolean;

    { CefDownloadHandler }

    // Called before a download begins. |suggested_name| is the suggested name for
		// the download file. By default the download will be canceled. Execute
		// |callback| either asynchronously or in this function to continue the
		// download if desired. Do not keep a reference to |download_item| outside of
		// this function.
		procedure doOnBeforeDownload(const aBrowser: ICefBrowser; const aDownloadItem: ICefDownloadItem; const aSuggestedName: ustring; const aCallback: ICefBeforeDownloadCallback);
		// Called when a download's status or progress information has been updated.
		// This may be called multiple times before and after on_before_download().
		// Execute |callback| either asynchronously or in this function to cancel the
		// download if desired. Do not keep a reference to |download_item| outside of
		// this function.
		procedure doOnDownloadUpdated(const aBrowser: ICefBrowser; const aDownloadItem: ICefDownloadItem; const aCallback: ICefDownloadItemCallback);

    { CefDragHandler }

    // Called when an external drag event enters the browser window. |dragData|
		// contains the drag event data and |mask| represents the type of drag
		// operation. Return false (0) for default drag handling behavior or true (1)
		// to cancel the drag event.
		function doOnDragEnter(const aBrowser: ICefBrowser; const aDragData: ICefDragData; aMask: TCefDragOperationsMask): Boolean;

    { CefFindHandler }

    // Called to report find results returned by cef_browser_host_t::find().
		// |identifer| is the identifier passed to find(), |count| is the number of
		// matches currently identified, |selectionRect| is the location of where the
		// match was found (in window coordinates), |activeMatchOrdinal| is the
		// current position in the search results, and |finalUpdate| is true (1) if
		// this is the last find notification.
		procedure doOnFindResult(const aBrowser: ICefBrowser; aIdentifier: cint; aCount: cint; const aSelectionRect: TCefRect; aActiveMatchOrdinal: cint; aFinalUpdate: Boolean);

    { CefFocusHandler }

    // Called when the browser component is about to loose focus. For instance, if
		// focus was on the last HTML element and the user pressed the TAB key. |next|
		// will be true (1) if the browser is giving focus to the next component and
		// false (0) if the browser is giving focus to the previous component.
		procedure doOnTakeFocus(const aBrowser: ICefBrowser; aNext: Boolean);
		// Called when the browser component is requesting focus. |source| indicates
		// where the focus request is originating from. Return false (0) to allow the
		// focus to be set or true (1) to cancel setting the focus.
		function doOnSetFocus(const aBrowser: ICefBrowser; aSource: TCefFocusSource): Boolean;
		// Called when the browser component has received focus.
		procedure doOnGotFocus(const aBrowser: ICefBrowser);

    { CefGeolocationHandler }

    // Called when a page requests permission to access geolocation information.
		// |requesting_url| is the URL requesting permission and |request_id| is the
		// unique ID for the permission request. Return true (1) and call
		// cef_geolocation_callback_t::cont() either in this function or at a later
		// time to continue or cancel the request. Return false (0) to cancel the
		// request immediately.
		function doOnRequestGeolocationPermission(const aBrowser: ICefBrowser; const aRequestingUrl: ustring; aRequestId: cint; const aCallback: ICefGeolocationCallback): Boolean;
		// Called when a geolocation access request is canceled. |requesting_url| is
		// the URL that originally requested permission and |request_id| is the unique
		// ID for the permission request.
		procedure doOnCancelGeolocationPermission(const aBrowser: ICefBrowser; const aRequestingUrl: ustring; aRequestId: cint);

    { CefJsDialogHandler }

    // Called to run a JavaScript dialog. The |default_prompt_text| value will be
		// specified for prompt dialogs only. Set |suppress_message| to true (1) and
		// return false (0) to suppress the message (suppressing messages is
		// preferable to immediately executing the callback as this is used to detect
		// presumably malicious behavior like spamming alert messages in
		// onbeforeunload). Set |suppress_message| to false (0) and return false (0)
		// to use the default implementation (the default implementation will show one
		// modal dialog at a time and suppress any additional dialog requests until
		// the displayed dialog is dismissed). Return true (1) if the application will
		// use a custom dialog or if the callback has been executed immediately.
		// Custom dialogs may be either modal or modeless. If a custom dialog is used
		// the application must execute |callback| once the custom dialog is
		// dismissed.
		function doOnJsdialog(const aBrowser: ICefBrowser; const aOriginUrl: ustring; const aAcceptLang: ustring; aDialogType: TCefJsdialogType; const aMessageText: ustring; const aDefaultPromptText: ustring; const aCallback: ICefJsdialogCallback; var aSuppressMessage: Boolean): Boolean;
		// Called to run a dialog asking the user if they want to leave a page. Return
		// false (0) to use the default dialog implementation. Return true (1) if the
		// application will use a custom dialog or if the callback has been executed
		// immediately. Custom dialogs may be either modal or modeless. If a custom
		// dialog is used the application must execute |callback| once the custom
		// dialog is dismissed.
		function doOnBeforeUnloadDialog(const aBrowser: ICefBrowser; const aMessageText: ustring; aIsReload: Boolean; const aCallback: ICefJsdialogCallback): Boolean;
		// Called to cancel any pending dialogs and reset any saved dialog state. Will
		// be called due to events like page navigation irregardless of whether any
		// dialogs are currently pending.
		procedure doOnResetDialogState(const aBrowser: ICefBrowser);
		// Called when the default implementation dialog is closed.
		procedure doOnDialogClosed(const aBrowser: ICefBrowser);

    { CefKeyboardHandler }

    // Called before a keyboard event is sent to the renderer. |event| contains
		// information about the keyboard event. |os_event| is the operating system
		// event message, if any. Return true (1) if the event was handled or false
		// (0) otherwise. If the event will be handled in on_key_event() as a keyboard
		// shortcut set |is_keyboard_shortcut| to true (1) and return false (0).
		function doOnPreKeyEvent(const aBrowser: ICefBrowser; const aEvent: TCefKeyEvent; aOsEvent: TCefEventHandle; var aIsKeyboardShortcut: Boolean): Boolean;
		// Called after the renderer and JavaScript in the page has had a chance to
		// handle the event. |event| contains information about the keyboard event.
		// |os_event| is the operating system event message, if any. Return true (1)
		// if the keyboard event was handled or false (0) otherwise.
		function doOnKeyEvent(const aBrowser: ICefBrowser; const aEvent: TCefKeyEvent; aOsEvent: TCefEventHandle): Boolean;

    { CefLifeSpanHandler }

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
		function doOnBeforePopup(const aBrowser: ICefBrowser; const aFrame: ICefFrame; var aTargetUrl: ustring; const aTargetFrameName: ustring; aTargetDisposition: TCefWindowOpenDisposition; aUserGesture: Boolean; var aPopupFeatures: TCefPopupFeatures; var aWindowInfo: TCefWindowInfo; var aClient: ICefClient; var aSettings: TCefBrowserSettings; var aNoJavascriptAccess: Boolean): Boolean;
		// Called after a new browser is created.
		procedure doOnAfterCreated(const aBrowser: ICefBrowser);
		// Called when a modal window is about to display and the modal loop should
		// begin running. Return false (0) to use the default modal loop
		// implementation or true (1) to use a custom implementation.
		function doRunModal(const aBrowser: ICefBrowser): Boolean;
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
		function doDoClose(const aBrowser: ICefBrowser): Boolean;
		// Called just before a browser is destroyed. Release all references to the
		// browser object and do not attempt to execute any functions on the browser
		// object after this callback returns. If this is a modal window and a custom
		// modal loop implementation was provided in run_modal() this callback should
		// be used to exit the custom modal loop. See do_close() documentation for
		// additional usage information.
		procedure doOnBeforeClose(const aBrowser: ICefBrowser);

    { CefLoadHandler }

    // Called when the loading state has changed. This callback will be executed
		// twice -- once when loading is initiated either programmatically or by user
		// action, and once when loading is terminated due to completion, cancellation
		// of failure.
		procedure doOnLoadingStateChange(const aBrowser: ICefBrowser; aIsLoading: Boolean; aCanGoBack: Boolean; aCanGoForward: Boolean);
		// Called when the browser begins loading a frame. The |frame| value will
		// never be NULL -- call the is_main() function to check if this frame is the
		// main frame. Multiple frames may be loading at the same time. Sub-frames may
		// start or continue loading after the main frame load has ended. This
		// function may not be called for a particular frame if the load request for
		// that frame fails. For notification of overall browser load status use
		// OnLoadingStateChange instead.
		procedure doOnLoadStart(const aBrowser: ICefBrowser; const aFrame: ICefFrame);
		// Called when the browser is done loading a frame. The |frame| value will
		// never be NULL -- call the is_main() function to check if this frame is the
		// main frame. Multiple frames may be loading at the same time. Sub-frames may
		// start or continue loading after the main frame load has ended. This
		// function will always be called for all frames irrespective of whether the
		// request completes successfully.
		procedure doOnLoadEnd(const aBrowser: ICefBrowser; const aFrame: ICefFrame; aHttpStatusCode: cint);
		// Called when the resource load for a navigation fails or is canceled.
		// |errorCode| is the error code number, |errorText| is the error text and
		// |failedUrl| is the URL that failed to load. See net\base\net_error_list.h
		// for complete descriptions of the error codes.
		procedure doOnLoadError(const aBrowser: ICefBrowser; const aFrame: ICefFrame; aErrorCode: TCefErrorcode; const aErrorText: ustring; const aFailedUrl: ustring);

    { CefRenderHandler }

    // Called to retrieve the root window rectangle in screen coordinates. Return
		// true (1) if the rectangle was provided.
		function doOnGetRootScreenRect(const aBrowser: ICefBrowser; var aRect: TCefRect): Boolean;
		// Called to retrieve the view rectangle which is relative to screen
		// coordinates. Return true (1) if the rectangle was provided.
		function doOnGetViewRect(const aBrowser: ICefBrowser; var aRect: TCefRect): Boolean;
		// Called to retrieve the translation from view coordinates to actual screen
		// coordinates. Return true (1) if the screen coordinates were provided.
		function doGetScreenPoint(const aBrowser: ICefBrowser; aViewX: cint; aViewY: cint; var aScreenX: cint; var aScreenY: cint): Boolean;
		// Called to allow the client to fill in the CefScreenInfo object with
		// appropriate values. Return true (1) if the |screen_info| structure has been
		// modified.
		//
		// If the screen info rectangle is left NULL the rectangle from GetViewRect
		// will be used. If the rectangle is still NULL or invalid popups may not be
		// drawn correctly.
		function doOnGetScreenInfo(const aBrowser: ICefBrowser; var aScreenInfo: TCefScreenInfo): Boolean;
		// Called when the browser wants to show or hide the popup widget. The popup
		// should be shown if |show| is true (1) and hidden if |show| is false (0).
		procedure doOnPopupShow(const aBrowser: ICefBrowser; aShow: Boolean);
		// Called when the browser wants to move or resize the popup widget. |rect|
		// contains the new location and size in view coordinates.
		procedure doOnPopupSize(const aBrowser: ICefBrowser; var aRect: TCefRect);
		// Called when an element should be painted. Pixel values passed to this
		// function are scaled relative to view coordinates based on the value of
		// CefScreenInfo.device_scale_factor returned from GetScreenInfo. |type|
		// indicates whether the element is the view or the popup widget. |buffer|
		// contains the pixel data for the whole image. |dirtyRects| contains the set
		// of rectangles in pixel coordinates that need to be repainted. |buffer| will
		// be |width|*|height|*4 bytes in size and represents a BGRA image with an
		// upper-left origin.
		procedure doOnPaint(const aBrowser: ICefBrowser; aType: TCefPaintElementType; aDirtyRectsCount: csize_t; const aDirtyRects: TCefRectArray; const aBuffer: cvoid; aWidth: cint; aHeight: cint);
		// Called when the browser's cursor has changed. If |type| is CT_CUSTOM then
		// |custom_cursor_info| will be populated with the custom cursor information.
		procedure doOnCursorChange(const aBrowser: ICefBrowser; aCursor: TCefCursorHandle; aType: TCefCursorType; var aCustomCursorInfo: TCefCursorInfo);
		// Called when the user starts dragging content in the web view. Contextual
		// information about the dragged content is supplied by |drag_data|. (|x|,
		// |y|) is the drag start location in screen coordinates. OS APIs that run a
		// system message loop may be used within the StartDragging call.
		//
		// Return false (0) to abort the drag operation. Don't call any of
		// cef_browser_host_t::DragSource*Ended* functions after returning false (0).
		//
		// Return true (1) to handle the drag operation. Call
		// cef_browser_host_t::DragSourceEndedAt and DragSourceSystemDragEnded either
		// synchronously or asynchronously to inform the web view that the drag
		// operation has ended.
		function doOnStartDragging(const aBrowser: ICefBrowser; const aDragData: ICefDragData; aAllowedOps: TCefDragOperationsMask; aX: cint; aY: cint): Boolean;
		// Called when the web view wants to update the mouse cursor during a drag &
		// drop operation. |operation| describes the allowed operation (none, move,
		// copy, link).
		procedure doOnUpdateDragCursor(const aBrowser: ICefBrowser; aOperation: TCefDragOperationsMask);
		// Called when the scroll offset has changed.
		procedure doOnScrollOffsetChanged(const aBrowser: ICefBrowser; aX: cdouble; aY: cdouble);

    { CefRequestHandler }

    // Called on the UI thread before browser navigation. Return true (1) to
		// cancel the navigation or false (0) to allow the navigation to proceed. The
		// |request| object cannot be modified in this callback.
		// cef_load_handler_t::OnLoadingStateChange will be called twice in all cases.
		// If the navigation is allowed cef_load_handler_t::OnLoadStart and
		// cef_load_handler_t::OnLoadEnd will be called. If the navigation is canceled
		// cef_load_handler_t::OnLoadError will be called with an |errorCode| value of
		// ERR_ABORTED.
		function doOnBeforeBrowse(const aBrowser: ICefBrowser; const aFrame: ICefFrame; const aRequest: ICefRequest; aIsRedirect: Boolean): Boolean;
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
		function doOnOpenUrlfromTab(const aBrowser: ICefBrowser; const aFrame: ICefFrame; const aTargetUrl: ustring; aTargetDisposition: TCefWindowOpenDisposition; aUserGesture: Boolean): Boolean;
		// Called on the IO thread before a resource request is loaded. The |request|
		// object may be modified. Return RV_CONTINUE to continue the request
		// immediately. Return RV_CONTINUE_ASYNC and call cef_request_tCallback::
		// cont() at a later time to continue or cancel the request asynchronously.
		// Return RV_CANCEL to cancel the request immediately.
		//
		function doOnBeforeResourceLoad(const aBrowser: ICefBrowser; const aFrame: ICefFrame; const aRequest: ICefRequest; const aCallback: ICefRequestCallback): TCefReturnValue;
		// Called on the IO thread before a resource is loaded. To allow the resource
		// to load normally return NULL. To specify a handler for the resource return
		// a cef_resource_handler_t object. The |request| object should not be
		// modified in this callback.
		function doOnGetResourceHandler(const aBrowser: ICefBrowser; const aFrame: ICefFrame; const aRequest: ICefRequest): ICefResourceHandler;
		// Called on the IO thread when a resource load is redirected. The |request|
		// parameter will contain the old URL and other request-related information.
		// The |new_url| parameter will contain the new URL and can be changed if
		// desired. The |request| object cannot be modified in this callback.
		procedure doOnResourceRedirect(const aBrowser: ICefBrowser; const aFrame: ICefFrame; const aRequest: ICefRequest; var aNewUrl: ustring);
		// Called on the IO thread when a resource response is received. To allow the
		// resource to load normally return false (0). To redirect or retry the
		// resource modify |request| (url, headers or post body) and return true (1).
		// The |response| object cannot be modified in this callback.
		function doOnResourceResponse(const aBrowser: ICefBrowser; const aFrame: ICefFrame; const aRequest: ICefRequest; const aResponse: ICefResponse): Boolean;
		// Called on the IO thread when the browser needs credentials from the user.
		// |isProxy| indicates whether the host is a proxy server. |host| contains the
		// hostname and |port| contains the port number. Return true (1) to continue
		// the request and call cef_auth_callback_t::cont() either in this function or
		// at a later time when the authentication information is available. Return
		// false (0) to cancel the request immediately.
		function doOnGetAuthCredentials(const aBrowser: ICefBrowser; const aFrame: ICefFrame; aIsProxy: Boolean; const aHost: ustring; aPort: cint; const aRealm: ustring; const aScheme: ustring; const aCallback: ICefAuthCallback): Boolean;
		// Called on the IO thread when JavaScript requests a specific storage quota
		// size via the webkitStorageInfo.requestQuota function. |origin_url| is the
		// origin of the page making the request. |new_size| is the requested quota
		// size in bytes. Return true (1) to continue the request and call
		// cef_request_tCallback::cont() either in this function or at a later time to
		// grant or deny the request. Return false (0) to cancel the request
		// immediately.
		function doOnQuotaRequest(const aBrowser: ICefBrowser; const aOriginUrl: ustring; aNewSize: cint64; const aCallback: ICefRequestCallback): Boolean;
		// Called on the UI thread to handle requests for URLs with an unknown
		// protocol component. Set |allow_os_execution| to true (1) to attempt
		// execution via the registered OS protocol handler, if any. SECURITY WARNING:
		// YOU SHOULD USE THIS METHOD TO ENFORCE RESTRICTIONS BASED ON SCHEME, HOST OR
		// OTHER URL ANALYSIS BEFORE ALLOWING OS EXECUTION.
		procedure doOnProtocolExecution(const aBrowser: ICefBrowser; const aUrl: ustring; var aAllowOsExecution: Boolean);
		// Called on the UI thread to handle requests for URLs with an invalid SSL
		// certificate. Return true (1) and call cef_request_tCallback::cont() either
		// in this function or at a later time to continue or cancel the request.
		// Return false (0) to cancel the request immediately. If |callback| is NULL
		// the error cannot be recovered from and the request will be canceled
		// automatically. If CefSettings.ignore_certificate_errors is set all invalid
		// certificates will be accepted without calling this function.
		function doOnCertificateError(const aBrowser: ICefBrowser; aCertError: TCefErrorcode; const aRequestUrl: ustring; const aSslInfo: ICefSslinfo; const aCallback: ICefRequestCallback): Boolean;
		// Called on the browser process IO thread before a plugin is loaded. Return
		// true (1) to block loading of the plugin.
		function doOnBeforePluginLoad(const aBrowser: ICefBrowser; const aUrl: ustring; const aPolicyUrl: ustring; const aInfo: ICefWebPluginInfo): Boolean;
		// Called on the browser process UI thread when a plugin has crashed.
		// |plugin_path| is the path of the plugin that crashed.
		procedure doOnPluginCrashed(const aBrowser: ICefBrowser; const aPluginPath: ustring);
		// Called on the browser process UI thread when the render view associated
		// with |browser| is ready to receive/handle IPC messages in the render
		// process.
		procedure doOnRenderViewReady(const aBrowser: ICefBrowser);
		// Called on the browser process UI thread when the render process terminates
		// unexpectedly. |status| indicates how the process terminated.
		procedure doOnRenderProcessTerminated(const aBrowser: ICefBrowser; aStatus: TCefTerminationStatus);

    {CefClient}
    property OnProcessMessageReceived: TOnProcessMessageReceived read FOnProcessMessageReceived write FOnProcessMessageReceived;
    {ContextMenuHandler}
    property OnBeforeContextMenu: TOnBeforeContextMenu read FOnBeforeContextMenu write FOnBeforeContextMenu;
    property OnContextMenuCommand: TOnContextMenuCommand read FOnContextMenuCommand write FOnContextMenuCommand;
    property OnContextMenuDismissed: TOnContextMenuDismissed read FOnContextMenuDismissed write FOnContextMenuDismissed;
    {DialogHandler}
    property OnFileDialog: TOnFileDialog read FOnFileDialog write FOnFileDialog;
    {DisplayHandler}
    property OnAddressChange: TOnAddressChange read FOnAddressChange write FOnAddressChange;
    property OnTitleChange: TOnTitleChange read FOnTitleChange write FOnTitleChange;
    property OnFaviconUrlchange: TOnFaviconUrlChange read FonFaviconUrlChange write FOnFaviconUrlChange;
    property OnTooltip: TOnTooltip read FOnTooltip write FOnTooltip;
    property OnStatusMessage: TOnStatusMessage read FOnStatusMessage write FOnStatusMessage;
    property OnConsoleMessage: TOnConsoleMessage read FOnConsoleMessage write FOnConsoleMessage;
    {DownloadHandler}
    property OnBeforeDownload: TOnBeforeDownload read FOnBeforeDownload write FOnBeforeDownload;
    property OnDownloadUpdated: TOnDownloadUpdated read FOnDownloadUpdated write FOnDownloadUpdated;
    {DragHandler}
    property OnDragEnter: TOnDragEnter read FOnDragEnter write FOnDragEnter;
    {FindHandler}
    property OnFindResult: TOnFindResult read FonFindResult write FOnFindResult;
    {FocusHandler}
    property OnTakeFocus: TOnTakeFocus read FOnTakeFocus write FOnTakeFocus;
    property OnSetFocus: TOnSetFocus read FOnSetFocus write FOnSetFocus;
    property OnGotFocus: TOnGotFocus read FOnGotFocus write FOnGotFocus;
    {GeolocationHandler}
    property OnRequestGeolocationPermission: TOnRequestGeolocationPermission read FOnRequestGeolocationPermission write FOnRequestGeolocationPermission;
    property OnCancelGeolocationPermission: TOnCancelGeolocationPermission read FOnCancelGeolocationPermission write FOnCancelGeolocationPermission;
    {JsDialogHandler}
    property OnJsdialog: TOnJsdialog read FOnJsdialog write FOnJsdialog;
    property OnBeforeUnloadDialog: TOnBeforeUnloadDialog read FOnBeforeUnloadDialog write FOnBeforeUnloadDialog;
    property OnResetDialogState: TOnResetDialogState read FOnResetDialogState write FOnResetDialogState;
    property OndialogClosed: TOnDialogClosed read FOnDialogClosed write FOnDialogClosed;
    {KeyboardHandler}
    property OnPreKeyEvent: TOnPreKeyEvent read FOnPreKeyEvent write FOnPreKeyEvent;
    property OnKeyEvent: TOnKeyEvent read FOnKeyEvent write FOnKeyEvent;
    {LifeSpanHandler}
    property OnBeforePopup: TOnBeforePopup read FOnBeforePopup write FOnBeforePopup;
    property OnAfterCreated: TOnAfterCreated read FOnAfterCreated write FOnAfterCreated;
    property OnRunModal: TOnRunModal read FOnRunModal write FOnRunModal;
    property OnClose: TOnClose read FOnClose write FOnClose;
    property OnBeforeClose: TOnBeforeClose read FOnBeforeClose write FOnBeforeClose;
    {LoadHandler}
    property OnLoadingStateChange: TOnLoadingStateChange read FOnLoadingStateChange write FOnLoadingStateChange;
    property OnLoadStart: TOnLoadStart read FOnLoadStart write FOnLoadStart;
    property OnLoadEnd: TOnLoadEnd read FOnLoadEnd write FOnLoadEnd;
    property OnLoadError: TOnLoadError read FOnLoadError write FOnLoadError;
    {RenderHandler}
    property OnGetRootScreenRect: TOnGetRootScreenRect read FOnGetRootScreenRect write FOnGetRootScreenRect;
    property OnGetViewRect: TOnGetViewRect read FOnGetViewRect write FOnGetViewRect;
    property OnGetScreenPoint: TOnGetScreenPoint read FOnGetScreenPoint write FOnGetScreenPoint;
    property OnGetScreenInfo: TOnGetScreenInfo read FOnGetScreenInfo write FOnGetScreenInfo;
    property OnPopupShow: TOnPopupShow read FOnPopupShow write FOnPopupShow;
    property OnPopupSize: TOnPopupSize read FOnPopupSize write FOnPopupSize;
    property OnPaint: TOnPaint read FOnPaint write FOnPaint;
    property OnCursorChange: TOnCursorChange read FOnCursorChange write FOnCursorChange;
    property OnStartDragging: TOnStartDragging read FOnStartDragging write FOnStartDragging;
		property OnUpdateDragCursor: TOnUpdateDragCursor read FOnUpdateDragCursor write FOnUpdateDragCursor;
		property OnScrollOffsetChanged: TOnScrollOffsetChanged read FOnScrollOffsetChanged write FOnScrollOffsetChanged;
    {RequestHandler}
    property OnBeforeBrowse: TOnBeforeBrowse read FOnBeforeBrowse write FOnBeforeBrowse;
    property OnOpenUrlfromTab: TOnOpenUrlfromTab read FOnOpenUrlfromTab write FOnOpenUrlfromTab;
    property OnBeforeResourceLoad: TOnBeforeResourceLoad read FOnBeforeResourceLoad write FOnBeforeResourceLoad;
    property OnGetResourceHandler: TOnGetResourceHandler read FOnGetResourceHandler write FOnGetResourceHandler;
    property OnResourceRedirect: TOnResourceRedirect read FOnResourceRedirect write FOnResourceRedirect;
    property OnResourceResponse: TOnResourceResponse read FOnResourceResponse write FOnResourceResponse;
    property OnGetAuthCredentials: TOnGetAuthCredentials read FOnGetAuthCredentials write FOnGetAuthCredentials;
    property OnQuotaRequest: TOnQuotaRequest read FOnQuotaRequest write FOnQuotaRequest;
    property OnProtocolExecution: TOnProtocolExecution read FOnProtocolExecution write FOnProtocolExecution;
    property OnCertificateError: TOnCertificateError read FOnCertificateError write FOnCertificateError;
    property OnBeforePluginLoad: TOnBeforePluginLoad read FOnBeforePluginLoad write FOnBeforePluginLoad;
    property OnPluginCrashed: TOnPluginCrashed read FOnPluginCrashed write FOnPluginCrashed;
    property OnRenderViewReady: TOnRenderViewReady read FOnRenderViewReady write FOnRenderViewReady;
    property OnRenderProcessTerminated: TOnRenderProcessTerminated read FOnRenderProcessTerminated write FOnRenderProcessTerminated;

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
    constructor CreateAsPopup(AOwner: TComponent); virtual;
    constructor CreateAsOSR(AOwner: TComponent);
    destructor Destroy; override;
    procedure Load(const url: ustring);
    property Handler: ICefClient read FHandler;
  end;

  TWACustomChromiumOSR = class(TWACustomChromium)
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateAsPopup(AOwner: TComponent); override;
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

  TWAChromiumOSR = class(TWACustomChromiumOSR)
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
    property OnGetRootScreenRect;
    property OnGetViewRect;
    property OnGetScreenPoint;
    property OnGetScreenInfo;
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

uses
  {$IFDEF FPC}
  ExtCtrls;
  {$ELSE}
	Vcl.ExtCtrls;
  {$ENDIF}
var
  Timer: TTimer;
  CefInstances: Integer = 0;
  Looping: Boolean;

procedure Register;
begin
  RegisterComponents('WAChromium',[TWAChromium]);
  RegisterComponents('WAChromium',[TWAChromiumOSR]);
end;

type
  TWACefHandler = class(TWACefClientHandler)
  private
   	class procedure OnTimer(Sender: TObject);
  public
    constructor Create(const crm: IChromiumEvents; renderer: Boolean); override;
    destructor Destroy; override;
    procedure StartTimer;
  end;

//..............................................................................TChromiumFontOptions
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

class procedure TWACefHandler.OnTimer(Sender : TObject);
begin
  if not CefMultiThreadedMessageLoop then
  begin
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
  end;
end;

constructor TWACefHandler.Create(const crm: IChromiumEvents; renderer: Boolean);
begin
  inherited Create(crm, renderer);
  if not (CefMultiThreadedMessageLoop) then
  begin
    if not assigned(Timer) then
    begin
      Timer := TTimer.Create(nil);
      Timer.Interval := 15;
      Timer.Enabled := false;
      Timer.OnTimer := OnTimer;
    end;
    InterlockedIncrement(CefInstances);
  end;
end;

destructor TWACefHandler.Destroy;
begin
  if not (CefMultithreadedMessageLoop) then
  begin
    InterlockedDecrement(CefInstances);
    if CefInstances = 0 then
      Timer.Enabled := false;
  end;
  FreeAndNil(Timer);
  inherited;
end;

procedure TWACefHandler.StartTimer;
begin
  if not (CefMultithreadedMessageLoop) then
  begin
    if not Assigned(Timer) then
      Exit;
    Timer.Enabled := true;
  end;
end;

//..............................................................................TWACustomChromium
{Private section}
procedure TWACustomChromium.Init;
begin
  if (csDesigning in ComponentState) then
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
begin
  if not (csDesigning in ComponentState) and
    (not FPopup) then
  begin
    TWACef.Initialize;
    FHandler := TWACefHandler.Create(Self, FOSR);
    If not Assigned(FHandler) then
      raise	Exception.Create('FHandler is nil');
    FillChar(info, SizeOf(info), 0);
    {$IFDEF MSWINDOWS}
    info.parent_window := Handle;
    if not FOSR then
    begin
      rect := GetClientRect;
      info.x := rect.left;
      info.y := rect.top;
      info.Width := rect.right - rect.left;
      info.Height := rect.bottom - rect.top;
      info.Style:=
        WS_CHILD or
        WS_VISIBLE or
        WS_CLIPCHILDREN or
        WS_CLIPSIBLINGS or
        WS_TABSTOP;
      info.ex_style := 0;
    end
    else
    begin
      info.windowless_rendering_enabled := 1;
      info.transparent_painting_enabled := 0;
    end;
    {$ENDIF}
    {$IFDEF LINUX}
    info.parent_window := Pointer(GDK_WINDOW_XID(PGtkWidget(Parent.Handle)^.window));
    {$ENDIF}

    FillChar(settings, SizeOf(TCefBrowserSettings), 0);
    settings.size := SizeOf(TCefBrowserSettings);
    GetSettings(settings);
    if CefMultiThreadedMessageLoop then
      TWACef.BrowserHostCreateBrowser(@info, FHandler, FDefaultUrl, @settings, nil)
    else
    begin
      FBrowser := TWACef.BrowserHostCreateSync(@info, FHandler, FDefaultUrl, @settings, nil);
      if FBrowser <> nil then
        FBrowserId := FBrowser.Identifier;    
      (FHandler as TWACefHandler).StartTimer;
    end;
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
  rect: TRect;
  hdwp: THandle;
begin
  inherited;
  if not (csDesigning in ComponentState) then
  begin
    if Assigned(FBrowser) and
      (FBrowser.Host.WindowHandle <> INVALID_HANDLE_VALUE) then
    begin      
      rect := GetClientRect;
      hdwp := BeginDeferWindowPos(1);
      try
        hdwp:=DeferWindowPos(
          hdwp,
          FBrowser.Host.WindowHandle,
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

{ CefClient }

// Called when a new message is received from a different process. Return true
// (1) if the message was handled or false (0) otherwise. Do not keep a
// reference to or attempt to access the message outside of this callback.
function TWACustomChromium.doOnProcessMessageReceived(const aBrowser: ICefBrowser; aSourceProcess: TCefProcessId; const aMessage: ICefProcessMessage): Boolean;
begin
  Result := false;
  if Assigned(FOnProcessMessageReceived) then
    FOnProcessMessageReceived(self, aBrowser, aSourceProcess, aMessage, Result);
end;

{ CefContextMenuHandler }

// Called before a context menu is displayed. |params| provides information
// about the context menu state. |model| initially contains the default
// context menu. The |model| can be cleared to show no context menu or
// modified to show a custom menu. Do not keep references to |params| or
// |model| outside of this callback.
procedure TWACustomChromium.doOnBeforeContextMenu(const aBrowser: ICefBrowser; const aFrame: ICefFrame; const aParams: ICefContextMenuParams; const aModel: ICefMenuModel);
begin
  if Assigned(FOnBeforeContextMenu) then
    FOnBeforeContextMenu(self, aBrowser, aFrame, aParams, aModel);
end;

// Called to execute a command selected from the context menu. Return true (1)
// if the command was handled or false (0) for the default implementation. See
// cef_menu_id_t for the command ids that have default implementations. All
// user-defined command ids should be between MENU_ID_USER_FIRST and
// MENU_ID_USER_LAST. |params| will have the same values as what was passed to
// on_before_context_menu(). Do not keep a reference to |params| outside of
// this callback.
function TWACustomChromium.doOnContextMenuCommand(const aBrowser: ICefBrowser; const aFrame: ICefFrame; const aParams: ICefContextMenuParams; aCommandId: cint; aEventFlags: TCefEventFlags): Boolean;
begin
  Result := false;
  if Assigned(FOnContextMenuCommand) then
    FOnContextMenuCommand(self, aBrowser, aFrame, aParams, aCommandId, aEventFlags, Result);
end;
// Called when the context menu is dismissed irregardless of whether the menu
// was NULL or a command was selected.
procedure TWACustomChromium.doOnContextMenuDismissed(const aBrowser: ICefBrowser; const aFrame: ICefFrame);
begin
  if Assigned(FOnContextMenuDismissed) then
    FOnContextMenuDismissed(self, aBrowser, aFrame);
end;

{ CefDialogHandler }

// Called to run a file chooser dialog. |mode| represents the type of dialog
// to display. |title| to the title to be used for the dialog and may be NULL
// to show the default title ("Open" or "Save" depending on the mode).
// |default_file_path| is the path with optional directory and/or file name
// component that should be initially selected in the dialog. |accept_filters|
// are used to restrict the selectable file types and may any combination of
// (a) valid lower-cased MIME types (e.g. "text/*" or "image/*"), (b)
// individual file extensions (e.g. ".txt" or ".png"), or (c) combined
// description and file extension delimited using "|" and ";" (e.g. "Image
// Types|.png;.gif;.jpg"). |selected_accept_filter| is the 0-based index of
// the filter that should be selected by default. To display a custom dialog
// return true (1) and execute |callback| either inline or at a later time. To
// display the default dialog return false (0).
function TWACustomChromium.doOnFileDialog(const aBrowser: ICefBrowser; aMode: TCefFileDialogMode; const aTitle: ustring; const aDefaultFilePath: ustring; aAcceptFilters: TStrings; aSelectedAcceptFilter: cint; const aCallback: ICefFileDialogCallback): Boolean;
begin
  Result := false;
  if Assigned(FOnFileDialog) then
    FOnFileDialog(self, aBrowser, aMode, aTitle, aDefaultFilePath, aAcceptFilters, aSelectedAcceptFilter, aCallback, Result);
end;

{ CefDisplayHandler }

// Called when a frame's address has changed.
procedure TWACustomChromium.doOnAddressChange(const aBrowser: ICefBrowser; const aFrame: ICefFrame; const aUrl: ustring);
begin
  if Assigned(FOnAddressChange) then
    FOnAddressChange(Self, aBrowser, aFrame, aUrl);

end;
// Called when the page title changes.
procedure TWACustomChromium.doOnTitleChange(const aBrowser: ICefBrowser; const aTitle: ustring);
begin
  if Assigned(FOnTitleChange) then
    FOnTitleChange(Self, aBrowser, aTitle);
end;
// Called when the page icon changes.
procedure TWACustomChromium.doOnFaviconUrlchange(const aBrowser: ICefBrowser; aIconUrls: TStrings);
begin
  if Assigned(FOnFaviconUrlchange) then
    FOnFaviconUrlchange(Self, aBrowser, aIconUrls);
end;

// Called when the browser is about to display a tooltip. |text| contains the
// text that will be displayed in the tooltip. To handle the display of the
// tooltip yourself return true (1). Otherwise, you can optionally modify
// |text| and then return false (0) to allow the browser to display the
// tooltip. When window rendering is disabled the application is responsible
// for drawing tooltips and the return value is ignored.
function TWACustomChromium.doOnTooltip(const aBrowser: ICefBrowser; var aText: ustring): Boolean;
begin
  Result := false;
  if Assigned(FOnTooltip) then
    FOnTooltip(Self, aBrowser, aText, Result);
end;

// Called when the browser receives a status message. |value| contains the
// text that will be displayed in the status message.
procedure TWACustomChromium.doOnStatusMessage(const aBrowser: ICefBrowser; const aValue: ustring);
begin
  if Assigned(FOnStatusMessage) then
    FOnStatusMessage(Self, aBrowser, aValue);
end;

// Called to display a console message. Return true (1) to stop the message
// from being output to the console.
function TWACustomChromium.doOnConsoleMessage(const aBrowser: ICefBrowser; const aMessage: ustring; const aSource: ustring; aLine: cint): Boolean;
begin
  Result := false;
  if Assigned(FOnConsoleMessage) then
    FOnConsoleMessage(Self, aBrowser, aMessage, aSource, aLine, Result);
end;

{ CefDownloadHandler }

// Called before a download begins. |suggested_name| is the suggested name for
// the download file. By default the download will be canceled. Execute
// |callback| either asynchronously or in this function TWACustomChromium.to continue the
// download if desired. Do not keep a reference to |download_item| outside of
// this function.
procedure TWACustomChromium.doOnBeforeDownload(const aBrowser: ICefBrowser; const aDownloadItem: ICefDownloadItem; const aSuggestedName: ustring; const aCallback: ICefBeforeDownloadCallback);
begin
  if Assigned(FOnBeforeDownload) then
    FOnBeforeDownload(Self, aBrowser, aDownloadItem, aSuggestedName, aCallback);
end;

// Called when a download's status or progress information has been updated.
// This may be called multiple times before and after on_before_download().
// Execute |callback| either asynchronously or in this function TWACustomChromium.to cancel the
// download if desired. Do not keep a reference to |download_item| outside of
// this function.
procedure TWACustomChromium.doOnDownloadUpdated(const aBrowser: ICefBrowser; const aDownloadItem: ICefDownloadItem; const aCallback: ICefDownloadItemCallback);
begin
  if Assigned(FOnDownloadUpdated) then
    FOnDownloadUpdated(Self, aBrowser, aDownloadItem, aCallback);
end;

{ CefDragHandler }

// Called when an external drag event enters the browser window. |dragData|
// contains the drag event data and |mask| represents the type of drag
// operation. Return false (0) for default drag handling behavior or true (1)
// to cancel the drag event.
function TWACustomChromium.doOnDragEnter(const aBrowser: ICefBrowser; const aDragData: ICefDragData; aMask: TCefDragOperationsMask): Boolean;
begin
  Result := false;
  if Assigned(FOnDragEnter) then
    FOnDragEnter(Self, aBrowser, aDragData, aMask, Result);
end;

{ CefFindHandler }

// Called to report find results returned by cef_browser_host_t::find().
// |identifer| is the identifier passed to find(), |count| is the number of
// matches currently identified, |selectionRect| is the location of where the
// match was found (in window coordinates), |activeMatchOrdinal| is the
// current position in the search results, and |finalUpdate| is true (1) if
// this is the last find notification.
procedure TWACustomChromium.doOnFindResult(const aBrowser: ICefBrowser; aIdentifier: cint; aCount: cint; const aSelectionRect: TCefRect; aActiveMatchOrdinal: cint; aFinalUpdate: Boolean);
begin
  if Assigned(FOnFindResult) then
    FOnFindResult(Self, aBrowser, aIdentifier, aCount, aSelectionRect, aActiveMatchOrdinal, aFinalUpdate);
end;

{ CefFocusHandler }

// Called when the browser component is about to loose focus. For instance, if
// focus was on the last HTML element and the user pressed the TAB key. |next|
// will be true (1) if the browser is giving focus to the next component and
// false (0) if the browser is giving focus to the previous component.
procedure TWACustomChromium.doOnTakeFocus(const aBrowser: ICefBrowser; aNext: Boolean);
begin
  if Assigned(FOnTakeFocus) then
    FOnTakeFocus(Self, aBrowser, aNext);
end;

// Called when the browser component is requesting focus. |source| indicates
// where the focus request is originating from. Return false (0) to allow the
// focus to be set or true (1) to cancel setting the focus.
function TWACustomChromium.doOnSetFocus(const aBrowser: ICefBrowser; aSource: TCefFocusSource): Boolean;
begin
  Result := false;
  if Assigned(FOnSetFocus) then
    FOnSetFocus(Self, aBrowser, aSource, Result);
end;
// Called when the browser component has received focus.
procedure TWACustomChromium.doOnGotFocus(const aBrowser: ICefBrowser);
begin
  if Assigned(FOnGotFocus) then
    FOnGotFocus(Self, aBrowser);
end;

{ CefGeolocationHandler }

// Called when a page requests permission to access geolocation information.
// |requesting_url| is the URL requesting permission and |request_id| is the
// unique ID for the permission request. Return true (1) and call
// cef_geolocation_callback_t::cont() either in this function TWACustomChromium.or at a later
// time to continue or cancel the request. Return false (0) to cancel the
// request immediately.
function TWACustomChromium.doOnRequestGeolocationPermission(const aBrowser: ICefBrowser; const aRequestingUrl: ustring; aRequestId: cint; const aCallback: ICefGeolocationCallback): Boolean;
begin
  Result := false;
  if Assigned(FOnRequestGeolocationPermission) then
    FOnRequestGeolocationPermission(Self, aBrowser, aRequestingUrl, aRequestId, aCallback, Result);
end;

// Called when a geolocation access request is canceled. |requesting_url| is
// the URL that originally requested permission and |request_id| is the unique
// ID for the permission request.
procedure TWACustomChromium.doOnCancelGeolocationPermission(const aBrowser: ICefBrowser; const aRequestingUrl: ustring; aRequestId: cint);
begin
  if Assigned(FOnCancelGeolocationPermission) then
    FOnCancelGeolocationPermission(Self, aBrowser, aRequestingUrl, aRequestId);
end;

{ CefJsDialogHandler }

// Called to run a JavaScript dialog. The |default_prompt_text| value will be
// specified for prompt dialogs only. Set |suppress_message| to true (1) and
// return false (0) to suppress the message (suppressing messages is
// preferable to immediately executing the callback as this is used to detect
// presumably malicious behavior like spamming alert messages in
// onbeforeunload). Set |suppress_message| to false (0) and return false (0)
// to use the default implementation (the default implementation will show one
// modal dialog at a time and suppress any additional dialog requests until
// the displayed dialog is dismissed). Return true (1) if the application will
// use a custom dialog or if the callback has been executed immediately.
// Custom dialogs may be either modal or modeless. If a custom dialog is used
// the application must execute |callback| once the custom dialog is
// dismissed.
function TWACustomChromium.doOnJsdialog(const aBrowser: ICefBrowser; const aOriginUrl: ustring; const aAcceptLang: ustring; aDialogType: TCefJsdialogType; const aMessageText: ustring; const aDefaultPromptText: ustring; const aCallback: ICefJsdialogCallback; var aSuppressMessage: Boolean): Boolean;
begin
  Result := false;
  if Assigned(FOnJsdialog) then
    FOnJsdialog(Self, aBrowser, aOriginUrl, aAcceptLang, aDialogType, aMessageText, aDefaultPromptText, aCallback, aSuppressMessage, Result);
end;

// Called to run a dialog asking the user if they want to leave a page. Return
// false (0) to use the default dialog implementation. Return true (1) if the
// application will use a custom dialog or if the callback has been executed
// immediately. Custom dialogs may be either modal or modeless. If a custom
// dialog is used the application must execute |callback| once the custom
// dialog is dismissed.
function TWACustomChromium.doOnBeforeUnloadDialog(const aBrowser: ICefBrowser; const aMessageText: ustring; aIsReload: Boolean; const aCallback: ICefJsdialogCallback): Boolean;
begin
  Result := false;
  if Assigned(FOnBeforeUnloadDialog) then
    FOnBeforeUnloadDialog(Self, aBrowser, aMessageText, aIsReload, aCallback, Result);
end;

// Called to cancel any pending dialogs and reset any saved dialog state. Will
// be called due to events like page navigation irregardless of whether any
// dialogs are currently pending.
procedure TWACustomChromium.doOnResetDialogState(const aBrowser: ICefBrowser);
begin
  if Assigned(FOnResetDialogState) then
    FOnResetDialogState(Self, aBrowser);
end;
// Called when the default implementation dialog is closed.
procedure TWACustomChromium.doOnDialogClosed(const aBrowser: ICefBrowser);
begin
  if Assigned(FOnDialogClosed) then
    FOnDialogClosed(Self, aBrowser);
end;

{ CefKeyboardHandler }

// Called before a keyboard event is sent to the renderer. |event| contains
// information about the keyboard event. |os_event| is the operating system
// event message, if any. Return true (1) if the event was handled or false
// (0) otherwise. If the event will be handled in on_key_event() as a keyboard
// shortcut set |is_keyboard_shortcut| to true (1) and return false (0).
function TWACustomChromium.doOnPreKeyEvent(const aBrowser: ICefBrowser; const aEvent: TCefKeyEvent; aOsEvent: TCefEventHandle; var aIsKeyboardShortcut: Boolean): Boolean;
begin
  Result := false;
  if Assigned(FOnPreKeyEvent) then
    FOnPreKeyEvent(Self, aBrowser, aEvent, aOSEvent, aIsKeyboardShortcut, Result);
end;

// Called after the renderer and JavaScript in the page has had a chance to
// handle the event. |event| contains information about the keyboard event.
// |os_event| is the operating system event message, if any. Return true (1)
// if the keyboard event was handled or false (0) otherwise.
function TWACustomChromium.doOnKeyEvent(const aBrowser: ICefBrowser; const aEvent: TCefKeyEvent; aOsEvent: TCefEventHandle): Boolean;
begin
  Result := false;
  if Assigned(FOnKeyEvent) then
    FOnKeyEvent(Self, aBrowser, aEvent, aOsEvent, Result)
end;

{ CefLifeSpanHandler }

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
function TWACustomChromium.doOnBeforePopup(const aBrowser: ICefBrowser; const aFrame: ICefFrame; var aTargetUrl: ustring; const aTargetFrameName: ustring; aTargetDisposition: TCefWindowOpenDisposition; aUserGesture: Boolean; var aPopupFeatures: TCefPopupFeatures; var aWindowInfo: TCefWindowInfo; var aClient: ICefClient; var aSettings: TCefBrowserSettings; var aNoJavascriptAccess: Boolean): Boolean;
begin
  Result := false;
  if Assigned(FOnBeforePopup) then
    FOnBeforePopup(Self, aBrowser, aFrame, aTargetUrl, aTargetFrameName, aTargetDisposition, aUserGesture, aPopupFeatures, aWindowInfo, aClient, aSettings, aNoJavascriptAccess, Result);
end;

// Called after a new browser is created.
procedure TWACustomChromium.doOnAfterCreated(const aBrowser: ICefBrowser);
begin
  if not Assigned(FBrowser) then
  begin
    FBrowser := aBrowser;
    FBrowserID := aBrowser.Identifier;
  end;
  if Assigned(FOnAfterCreated) then
    FOnAfterCreated(Self, aBrowser);
end;

// Called when a modal window is about to display and the modal loop should
// begin running. Return false (0) to use the default modal loop
// implementation or true (1) to use a custom implementation.
function TWACustomChromium.doRunModal(const aBrowser: ICefBrowser): Boolean;
begin
  Result := false;
  if Assigned(FOnRunModal) then
    FOnRunModal(Self, aBrowser, Result);
end;

// Called when a browser has recieved a request to close. This may result
// directly from a call to cef_browser_host_t::close_browser() or indirectly
// if the browser is a top-level OS window created by CEF and the user
// attempts to close the window. This function TWACustomChromium.will be called after the
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
// The cef_life_span_handler_t::on_before_close() function TWACustomChromium.will be called
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
function TWACustomChromium.doDoClose(const aBrowser: ICefBrowser): Boolean;
begin
  Result := false;
  if Assigned(FOnClose) then
    FOnClose(Self, aBrowser, Result);
end;

// Called just before a browser is destroyed. Release all references to the
// browser object and do not attempt to execute any functions on the browser
// object after this callback returns. If this is a modal window and a custom
// modal loop implementation was provided in run_modal() this callback should
// be used to exit the custom modal loop. See do_close() documentation for
// additional usage information.
procedure TWACustomChromium.doOnBeforeClose(const aBrowser: ICefBrowser);
begin
  if Assigned(FOnBeforeClose) then
    FOnBeforeClose(Self, aBrowser);
end;

{ CefLoadHandler }

// Called when the loading state has changed. This callback will be executed
// twice -- once when loading is initiated either programmatically or by user
// action, and once when loading is terminated due to completion, cancellation
// of failure.
procedure TWACustomChromium.doOnLoadingStateChange(const aBrowser: ICefBrowser; aIsLoading: Boolean; aCanGoBack: Boolean; aCanGoForward: Boolean);
begin
  if Assigned(FOnLoadingStateChange) then
    FOnLoadingStateChange(Self, aBrowser, aIsLoading, aCanGoBack, aCanGoForward);
end;

// Called when the browser begins loading a frame. The |frame| value will
// never be NULL -- call the is_main() function TWACustomChromium.to check if this frame is the
// main frame. Multiple frames may be loading at the same time. Sub-frames may
// start or continue loading after the main frame load has ended. This
// function TWACustomChromium.may not be called for a particular frame if the load request for
// that frame fails. For notification of overall browser load status use
// OnLoadingStateChange instead.
procedure TWACustomChromium.doOnLoadStart(const aBrowser: ICefBrowser; const aFrame: ICefFrame);
begin
  if Assigned(FOnLoadStart) then
    FOnLoadStart(Self, aBrowser, aFrame);
end;

// Called when the browser is done loading a frame. The |frame| value will
// never be NULL -- call the is_main() function TWACustomChromium.to check if this frame is the
// main frame. Multiple frames may be loading at the same time. Sub-frames may
// start or continue loading after the main frame load has ended. This
// function TWACustomChromium.will always be called for all frames irrespective of whether the
// request completes successfully.
procedure TWACustomChromium.doOnLoadEnd(const aBrowser: ICefBrowser; const aFrame: ICefFrame; aHttpStatusCode: cint);
begin
  if Assigned(FOnLoadEnd) then
    FOnLoadEnd(Self, aBrowser, aFrame, aHttpStatusCode);
end;

// Called when the resource load for a navigation fails or is canceled.
// |errorCode| is the error code number, |errorText| is the error text and
// |failedUrl| is the URL that failed to load. See net\base\net_error_list.h
// for complete descriptions of the error codes.
procedure TWACustomChromium.doOnLoadError(const aBrowser: ICefBrowser; const aFrame: ICefFrame; aErrorCode: TCefErrorcode; const aErrorText: ustring; const aFailedUrl: ustring);
begin
  if Assigned(FOnLoadError) then
    FOnLoadError(Self, aBrowser, aFrame, aErrorCode, aErrorText, aFailedUrl);
end;

{ CefRenderHandler }

// Called to retrieve the root window rectangle in screen coordinates. Return
// true (1) if the rectangle was provided.
function TWACustomChromium.doOnGetRootScreenRect(const aBrowser: ICefBrowser; var aRect: TCefRect): Boolean;
begin
  Result := false;
  if Assigned(FOnGetRootScreenRect) then
    FOnGetRootScreenRect(Self, aBrowser, aRect, Result);
end;

// Called to retrieve the view rectangle which is relative to screen
// coordinates. Return true (1) if the rectangle was provided.
function TWACustomChromium.doOnGetViewRect(const aBrowser: ICefBrowser; var aRect: TCefRect): Boolean;
begin
  Result := false;
  if Assigned(FOnGetViewRect) then
    FOnGetViewRect(Self, aBrowser, aRect, Result);
end;

// Called to retrieve the translation from view coordinates to actual screen
// coordinates. Return true (1) if the screen coordinates were provided.
function TWACustomChromium.doGetScreenPoint(const aBrowser: ICefBrowser; aViewX: cint; aViewY: cint; var aScreenX: cint; var aScreenY: cint): Boolean;
begin
  Result := false;
  if Assigned(FOnGetScreenPoint) then
    FOnGetScreenPoint(Self, aBrowser, aViewX, aViewY, aScreenX, aScreenY, Result);
end;

// Called to allow the client to fill in the CefScreenInfo object with
// appropriate values. Return true (1) if the |screen_info| structure has been
// modified.
//
// If the screen info rectangle is left NULL the rectangle from GetViewRect
// will be used. If the rectangle is still NULL or invalid popups may not be
// drawn correctly.
function TWACustomChromium.doOnGetScreenInfo(const aBrowser: ICefBrowser; var aScreenInfo: TCefScreenInfo): Boolean;
begin
  Result := false;
  if Assigned(FOnGetScreenInfo) then
    FOnGetScreenInfo(Self, aBrowser, aScreenInfo, Result);
end;

// Called when the browser wants to show or hide the popup widget. The popup
// should be shown if |show| is true (1) and hidden if |show| is false (0).
procedure TWACustomChromium.doOnPopupShow(const aBrowser: ICefBrowser; aShow: Boolean);
begin
  if Assigned(FOnPopupShow) then
    FOnPopupShow(Self, aBrowser, aShow);
end;

// Called when the browser wants to move or resize the popup widget. |rect|
// contains the new location and size in view coordinates.
procedure TWACustomChromium.doOnPopupSize(const aBrowser: ICefBrowser; var aRect: TCefRect);
begin
  if Assigned(FOnPopupSize) then
    FOnPopupSize(Self, aBrowser, aRect);
end;

// Called when an element should be painted. Pixel values passed to this
// function TWACustomChromium.are scaled relative to view coordinates based on the value of
// CefScreenInfo.device_scale_factor returned from GetScreenInfo. |type|
// indicates whether the element is the view or the popup widget. |buffer|
// contains the pixel data for the whole image. |dirtyRects| contains the set
// of rectangles in pixel coordinates that need to be repainted. |buffer| will
// be |width|*|height|*4 bytes in size and represents a BGRA image with an
// upper-left origin.
procedure TWACustomChromium.doOnPaint(const aBrowser: ICefBrowser; aType: TCefPaintElementType; aDirtyRectsCount: csize_t; const aDirtyRects: TCefRectArray; const aBuffer: cvoid; aWidth: cint; aHeight: cint);
begin
  if Assigned(FOnPaint) then
    FOnPaint(Self, aBrowser, aType, aDirtyRectsCount, aDirtyRects, aBuffer, aWidth, aHeight);
end;

// Called when the browser's cursor has changed. If |type| is CT_CUSTOM then
// |custom_cursor_info| will be populated with the custom cursor information.
procedure TWACustomChromium.doOnCursorChange(const aBrowser: ICefBrowser; aCursor: TCefCursorHandle; aType: TCefCursorType; var aCustomCursorInfo: TCefCursorInfo);
begin
  if Assigned(FOnCursorChange) then
    FOnCursorChange(Self, aBrowser, aCursor, aType, aCustomCursorInfo);
end;

// Called when the user starts dragging content in the web view. Contextual
// information about the dragged content is supplied by |drag_data|. (|x|,
// |y|) is the drag start location in screen coordinates. OS APIs that run a
// system message loop may be used within the StartDragging call.
//
// Return false (0) to abort the drag operation. Don't call any of
// cef_browser_host_t::DragSource*Ended* functions after returning false (0).
//
// Return true (1) to handle the drag operation. Call
// cef_browser_host_t::DragSourceEndedAt and DragSourceSystemDragEnded either
// synchronously or asynchronously to inform the web view that the drag
// operation has ended.
function TWACustomChromium.doOnStartDragging(const aBrowser: ICefBrowser; const aDragData: ICefDragData; aAllowedOps: TCefDragOperationsMask; aX: cint; aY: cint): Boolean;
begin
  Result := false;
  if Assigned(FOnStartDragging) then
    FOnStartDragging(Self, aBrowser, aDragData, aAllowedOps, aX, aY, Result);
end;

// Called when the web view wants to update the mouse cursor during a drag &
// drop operation. |operation| describes the allowed operation (none, move,
// copy, link).
procedure TWACustomChromium.doOnUpdateDragCursor(const aBrowser: ICefBrowser; aOperation: TCefDragOperationsMask);
begin
  if Assigned(FOnUpdateDragCursor) then
    FOnUpdateDragCursor(Self, aBrowser, aOperation);
end;

// Called when the scroll offset has changed.
procedure TWACustomChromium.doOnScrollOffsetChanged(const aBrowser: ICefBrowser; aX: cdouble; aY: cdouble);
begin
  if Assigned(FOnScrollOffsetChanged) then
    FOnScrollOffsetChanged(Self, aBrowser, aX, aY);
end;

{ CefRequestHandler }

// Called on the UI thread before browser navigation. Return true (1) to
// cancel the navigation or false (0) to allow the navigation to proceed. The
// |request| object cannot be modified in this callback.
// cef_load_handler_t::OnLoadingStateChange will be called twice in all cases.
// If the navigation is allowed cef_load_handler_t::OnLoadStart and
// cef_load_handler_t::OnLoadEnd will be called. If the navigation is canceled
// cef_load_handler_t::OnLoadError will be called with an |errorCode| value of
// ERR_ABORTED.
function TWACustomChromium.doOnBeforeBrowse(const aBrowser: ICefBrowser; const aFrame: ICefFrame; const aRequest: ICefRequest; aIsRedirect: Boolean): Boolean;
begin
  Result := false;
  if Assigned(FOnBeforeBrowse) then
    FOnBeforeBrowse(Self, aBrowser, aFrame, aRequest, aIsredirect, Result);
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
function TWACustomChromium.doOnOpenUrlfromTab(const aBrowser: ICefBrowser; const aFrame: ICefFrame; const aTargetUrl: ustring; aTargetDisposition: TCefWindowOpenDisposition; aUserGesture: Boolean): Boolean;
begin
  Result := false;
  if Assigned(FOnOpenUrlfromTab) then
    FOnOpenUrlfromTab(Self, aBrowser, aFrame, aTargetUrl, aTargetDisposition, aUserGesture, Result);
end;

// Called on the IO thread before a resource request is loaded. The |request|
// object may be modified. Return RV_CONTINUE to continue the request
// immediately. Return RV_CONTINUE_ASYNC and call cef_request_tCallback::
// cont() at a later time to continue or cancel the request asynchronously.
// Return RV_CANCEL to cancel the request immediately.
//
function TWACustomChromium.doOnBeforeResourceLoad(const aBrowser: ICefBrowser; const aFrame: ICefFrame; const aRequest: ICefRequest; const aCallback: ICefRequestCallback): TCefReturnValue;
begin
  Result := RV_CONTINUE;
  if Assigned(FOnBeforeResourceLoad) then
    FOnBeforeResourceLoad(Self, aBrowser, aFrame, aRequest, aCallback, Result);
end;

// Called on the IO thread before a resource is loaded. To allow the resource
// to load normally return NULL. To specify a handler for the resource return
// a cef_resource_handler_t object. The |request| object should not be
// modified in this callback.
function TWACustomChromium.doOnGetResourceHandler(const aBrowser: ICefBrowser; const aFrame: ICefFrame; const aRequest: ICefRequest): ICefResourceHandler;
begin
  Result := nil;
  if Assigned(FOnGetResourceHandler) then
    FOnGetResourceHandler(Self, aBrowser, aFrame, aRequest, Result);
end;

// Called on the IO thread when a resource load is redirected. The |request|
// parameter will contain the old URL and other request-related information.
// The |new_url| parameter will contain the new URL and can be changed if
// desired. The |request| object cannot be modified in this callback.
procedure TWACustomChromium.doOnResourceRedirect(const aBrowser: ICefBrowser; const aFrame: ICefFrame; const aRequest: ICefRequest; var aNewUrl: ustring);
begin
  if Assigned(FOnResourceRedirect) then
    FOnResourceRedirect(Self, aBrowser, aFrame, aRequest, aNewUrl);
end;

// Called on the IO thread when a resource response is received. To allow the
// resource to load normally return false (0). To redirect or retry the
// resource modify |request| (url, headers or post body) and return true (1).
// The |response| object cannot be modified in this callback.
function TWACustomChromium.doOnResourceResponse(const aBrowser: ICefBrowser; const aFrame: ICefFrame; const aRequest: ICefRequest; const aResponse: ICefResponse): Boolean;
begin
  Result := false;
  if Assigned(FOnResourceResponse) then
    FOnResourceResponse(Self, aBrowser, aFrame, aRequest, aResponse, Result);
end;

// Called on the IO thread when the browser needs credentials from the user.
// |isProxy| indicates whether the host is a proxy server. |host| contains the
// hostname and |port| contains the port number. Return true (1) to continue
// the request and call cef_auth_callback_t::cont() either in this function TWACustomChromium.or
// at a later time when the authentication information is available. Return
// false (0) to cancel the request immediately.
function TWACustomChromium.doOnGetAuthCredentials(const aBrowser: ICefBrowser; const aFrame: ICefFrame; aIsProxy: Boolean; const aHost: ustring; aPort: cint; const aRealm: ustring; const aScheme: ustring; const aCallback: ICefAuthCallback): Boolean;
begin
  Result := false;
  if Assigned(FOnGetAuthCredentials) then
    FOnGetAuthCredentials(Self, aBrowser, aFrame, aIsProxy, aHost, aPort, aRealm, aScheme, aCallback, Result);
end;

// Called on the IO thread when JavaScript requests a specific storage quota
// size via the webkitStorageInfo.requestQuota function. |origin_url| is the
// origin of the page making the request. |new_size| is the requested quota
// size in bytes. Return true (1) to continue the request and call
// cef_request_tCallback::cont() either in this function TWACustomChromium.or at a later time to
// grant or deny the request. Return false (0) to cancel the request
// immediately.
function TWACustomChromium.doOnQuotaRequest(const aBrowser: ICefBrowser; const aOriginUrl: ustring; aNewSize: cint64; const aCallback: ICefRequestCallback): Boolean;
begin
  Result := false;
  if Assigned(FOnQuotaRequest) then
    FOnQuotaRequest(Self, aBrowser, aOriginUrl, aNewSize, aCallback, Result);
end;

// Called on the UI thread to handle requests for URLs with an unknown
// protocol component. Set |allow_os_execution| to true (1) to attempt
// execution via the registered OS protocol handler, if any. SECURITY WARNING:
// YOU SHOULD USE THIS METHOD TO ENFORCE RESTRICTIONS BASED ON SCHEME, HOST OR
// OTHER URL ANALYSIS BEFORE ALLOWING OS EXECUTION.
procedure TWACustomChromium.doOnProtocolExecution(const aBrowser: ICefBrowser; const aUrl: ustring; var aAllowOsExecution: Boolean);
begin
  if Assigned(FOnProtocolExecution) then
    FOnProtocolExecution(Self, aBrowser, aUrl, aAllowOsExecution);
end;

// Called on the UI thread to handle requests for URLs with an invalid SSL
// certificate. Return true (1) and call cef_request_tCallback::cont() either
// in this function TWACustomChromium.or at a later time to continue or cancel the request.
// Return false (0) to cancel the request immediately. If |callback| is NULL
// the error cannot be recovered from and the request will be canceled
// automatically. If CefSettings.ignore_certificate_errors is set all invalid
// certificates will be accepted without calling this function.
function TWACustomChromium.doOnCertificateError(const aBrowser: ICefBrowser; aCertError: TCefErrorcode; const aRequestUrl: ustring; const aSslInfo: ICefSslinfo; const aCallback: ICefRequestCallback): Boolean;
begin
  Result := false;
  if Assigned(FOnCertificateError) then
    FOnCertificateError(Self, aBrowser, aCertError, aRequestUrl, aSslInfo, aCallback, Result);
end;

// Called on the browser process IO thread before a plugin is loaded. Return
// true (1) to block loading of the plugin.
function TWACustomChromium.doOnBeforePluginLoad(const aBrowser: ICefBrowser; const aUrl: ustring; const aPolicyUrl: ustring; const aInfo: ICefWebPluginInfo): Boolean;
begin
  Result := false;
  if Assigned(FOnBeforePluginLoad) then
    FOnBeforePluginLoad(Self, aBrowser, aUrl, aPolicyUrl, aInfo, Result);
end;

// Called on the browser process UI thread when a plugin has crashed.
// |plugin_path| is the path of the plugin that crashed.
procedure TWACustomChromium.doOnPluginCrashed(const aBrowser: ICefBrowser; const aPluginPath: ustring);
begin
  if Assigned(FOnPluginCrashed) then
    FOnPluginCrashed(Self, aBrowser, aPluginPath);
end;

// Called on the browser process UI thread when the render view associated
// with |browser| is ready to receive/handle IPC messages in the render
// process.
procedure TWACustomChromium.doOnRenderViewReady(const aBrowser: ICefBrowser);
begin
  if Assigned(FOnRenderViewReady) then
    FOnRenderViewReady(Self, aBrowser);
end;

// Called on the browser process UI thread when the render process terminates
// unexpectedly. |status| indicates how the process terminated.
procedure TWACustomChromium.doOnRenderProcessTerminated(const aBrowser: ICefBrowser; aStatus: TCefTerminationStatus);
begin
  if Assigned(FOnRenderProcessTerminated) then
    FOnRenderProcessTerminated(Self, aBrowser, aStatus);
end;

{Private section}
constructor TWACustomChromium.Create(AOwner: TComponent);
begin
  FPopup := false;
  FOSR := false;
  inherited;
  Init;
end;

constructor TWACustomChromium.CreateAsPopup(AOwner: TComponent);
begin
  FPopup := true;
  FOSR := false;
  inherited Create(AOwner);
  Init;
end;

constructor TWACustomChromium.CreateAsOSR(AOwner: TComponent);
begin
  FPopup := false;
  FOSR := true;
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


//..............................................................................TWACustomChromiumOSR
{Private section}
{Protected section}
{Public section}
constructor TWACustomChromiumOSR.Create(AOwner: TComponent);
begin
  inherited CreateAsOSR(AOwner);
  FTransparentPainting := False;
end;

constructor TWACustomChromiumOSR.CreateAsPopup(AOwner: TComponent);
begin
  FOSR := true;
  FTransparentPainting := False;
  inherited;
end;

initialization

end.
