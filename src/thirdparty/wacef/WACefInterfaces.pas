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

unit WACefInterfaces;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

{$I WACef.inc}

interface

uses
  {$IFDEF FPC}
  ctypes,
  {$ENDIF}
  WACefCApi,
  WACefTypes,
  Classes;

type
  //..............................................................................cef_app_capi.h
	ICefApp = Interface;

	//..............................................................................cef_auth_callback_capi.h
	ICefAuthCallback = Interface;

	//..............................................................................cef_base_capi.h
	ICefBase = Interface;

	//..............................................................................cef_browser_capi.h
	ICefBrowser = Interface;
	ICefRunFileDialogCallback = Interface;
	ICefNavigationEntryVisitor = Interface;
	ICefBrowserHost = Interface;

	//..............................................................................cef_browser_process_handler_capi.h
	ICefBrowserProcessHandler = Interface;

	//..............................................................................cef_callback_capi.h
	ICefCallback = Interface;
	ICefCompletionCallback = Interface;

	//..............................................................................cef_client_capi.h
	ICefClient = Interface;

	//..............................................................................cef_command_line_capi.h
	ICefCommandLine = Interface;

	//..............................................................................cef_context_menu_handler_capi.h
	ICefContextMenuHandler = Interface;
	ICefContextMenuParams = Interface;

	//..............................................................................cef_cookie_capi.h
	ICefCookieManager = Interface;
	ICefCookieVisitor = Interface;
	ICefSetCookieCallback = Interface;
	ICefDeleteCookiesCallback = Interface;

	//..............................................................................cef_dialog_handler_capi.h
	ICefFileDialogCallback = Interface;
	ICefDialogHandler = Interface;

	//..............................................................................cef_display_handler_capi.h
	ICefDisplayHandler = Interface;

	//..............................................................................cef_dom_capi.h
	ICefDomvisitor = Interface;
	ICefDomdocument = Interface;
	ICefDomnode = Interface;

	//..............................................................................cef_download_handler_capi.h
	ICefBeforeDownloadCallback = Interface;
	ICefDownloadItemCallback = Interface;
	ICefDownloadHandler = Interface;

	//..............................................................................cef_download_item_capi.h
	ICefDownloadItem = Interface;

	//..............................................................................cef_drag_data_capi.h
	ICefDragData = Interface;

	//..............................................................................cef_drag_handler_capi.h
	ICefDragHandler = Interface;

	//..............................................................................cef_find_handler_capi.h
	ICefFindHandler = Interface;

	//..............................................................................cef_focus_handler_capi.h
	ICefFocusHandler = Interface;

	//..............................................................................cef_frame_capi.h
	ICefFrame = Interface;

	//..............................................................................cef_geolocation_capi.h
	ICefGetGeolocationCallback = Interface;

	//..............................................................................cef_geolocation_handler_capi.h
	ICefGeolocationCallback = Interface;
	ICefGeolocationHandler = Interface;

	//..............................................................................cef_jsdialog_handler_capi.h
	ICefJsdialogCallback = Interface;
	ICefJsdialogHandler = Interface;

	//..............................................................................cef_keyboard_handler_capi.h
	ICefKeyboardHandler = Interface;

	//..............................................................................cef_life_span_handler_capi.h
	ICefLifeSpanHandler = Interface;

	//..............................................................................cef_load_handler_capi.h
	ICefLoadHandler = Interface;

	//..............................................................................cef_menu_model_capi.h
	ICefMenuModel = Interface;

	//..............................................................................cef_navigation_entry_capi.h
	ICefNavigationEntry = Interface;

	//..............................................................................cef_origin_whitelist_capi.h

	//..............................................................................cef_parser_capi.h

	//..............................................................................cef_path_util_capi.h

	//..............................................................................cef_print_handler_capi.h
	ICefPrintDialogCallback = Interface;
	ICefPrintJobCallback = Interface;
	ICefPrintHandler = Interface;

	//..............................................................................cef_print_settings_capi.h
	ICefPrintSettings = Interface;

	//..............................................................................cef_process_message_capi.h
	ICefProcessMessage = Interface;

	//..............................................................................cef_process_util_capi.h

	//..............................................................................cef_render_handler_capi.h
	ICefRenderHandler = Interface;

	//..............................................................................cef_render_process_handler_capi.h
	ICefRenderProcessHandler = Interface;

	//..............................................................................cef_request_capi.h
	ICefRequest = Interface;
	ICefPostData = Interface;
	ICefPostDataElement = Interface;

	//..............................................................................cef_request_context_capi.h
	ICefRequestContext = Interface;

	//..............................................................................cef_request_context_handler_capi.h
	ICefRequestContextHandler = Interface;

	//..............................................................................cef_request_handler_capi.h
	ICefRequestCallback = Interface;
	ICefRequestHandler = Interface;

	//..............................................................................cef_resource_bundle_handler_capi.h
	ICefResourceBundleHandler = Interface;

	//..............................................................................cef_resource_handler_capi.h
	ICefResourceHandler = Interface;

	//..............................................................................cef_response_capi.h
	ICefResponse = Interface;

	//..............................................................................cef_scheme_capi.h
	ICefSchemeRegistrar = Interface;
	ICefSchemeHandlerFactory = Interface;

	//..............................................................................cef_ssl_info_capi.h
	ICefSslcertPrincipal = Interface;
	ICefSslinfo = Interface;

	//..............................................................................cef_stream_capi.h
	ICefReadHandler = Interface;
	ICefStreamReader = Interface;
	ICefWriteHandler = Interface;
	ICefStreamWriter = Interface;

	//..............................................................................cef_string_visitor_capi.h
	ICefStringVisitor = Interface;

	//..............................................................................cef_task_capi.h
	ICefTask = Interface;
	ICefTaskRunner = Interface;

	//..............................................................................cef_trace_capi.h
	ICefEndTracingCallback = Interface;

	//..............................................................................cef_urlrequest_capi.h
	ICefUrlrequest = Interface;
	ICefUrlrequestClient = Interface;

	//..............................................................................cef_v8_capi.h
	ICefV8context = Interface;
	ICefV8handler = Interface;
	ICefV8accessor = Interface;
	ICefV8exception = Interface;
	ICefV8value = Interface;
	ICefV8stackTrace = Interface;
	ICefV8stackFrame = Interface;

	//..............................................................................cef_values_capi.h
	ICefValue = Interface;
	ICefBinaryValue = Interface;
	ICefDictionaryValue = Interface;
	ICefListValue = Interface;

	//..............................................................................cef_web_plugin_capi.h
	ICefWebPluginInfo = Interface;
	ICefWebPluginInfoVisitor = Interface;
	ICefWebPluginUnstableCallback = Interface;

	//..............................................................................cef_xml_reader_capi.h
	ICefXmlReader = Interface;

	//..............................................................................cef_zip_reader_capi.h
	ICefZipReader = Interface;

  ICefStringMap = interface;
  ICefStringMultiMap = interface;
  ICefCustomStreamReader = interface;
  TCefv8ValueArray = array of ICefv8Value;

  TCefDomVisitorProc = {$IFNDEF FPC}reference to {$ENDIF} procedure(const  document: ICefDomDocument);
  TCefStringVisitorProc = {$IFNDEF FPC}reference to {$ENDIF} procedure(const  str: ustring);
  TCefCookieVisitorProc = {$IFNDEF FPC}reference to {$ENDIF} function(const  cookie:TWACefCookie; count, total: cint; out deleteCookie: Boolean): Boolean;
  TOnRegisterCustomSchemes = {$IFNDEF FPC}reference to {$ENDIF} procedure(const  registrar: ICefSchemeRegistrar);
  TOnBeforeCommandLineProcessing = {$IFNDEF FPC}reference to {$ENDIF} procedure(const  processType: ustring; const  commandLine: ICefCommandLine);
  TCefWebPluginInfoVisitorProc = {$IFNDEF FPC}reference to {$ENDIF} function(const  info: ICefWebPluginInfo; count, total: cint): Boolean;
  TCefV8AccessorGetterProc = {$IFNDEF FPC}reference to {$ENDIF} function(const  name: ustring; const  obj: ICefv8Value; var value: ICefv8Value; const exception: ustring): Boolean;
  TCefV8AccessorSetterProc = {$IFNDEF FPC}reference to {$ENDIF} function(const  name: ustring; const  obj, value: ICefv8Value; const  exception: ustring): Boolean;
  TCefRunFileDialogCallbackProc = {$IFNDEF FPC}reference to {$ENDIF} procedure(SelectedAcceptFilter: cint; filePaths: TStrings);


  ICefBase = interface
    ['{1F9A7B44-DCDC-4477-9180-3ADD44BDEB7B}']
    function Wrap: Pointer;
  end;



  //..............................................................................cef_app_capi.h
	// Implement this structure to provide handler implementations. Methods will be
	// called by the process and/or thread indicated.
	ICefApp = Interface(ICefBase)
	['{5AB941CE-2DD5-41F0-872E-A3C30EC28D39}']
		// Provides an opportunity to view and/or modify command-line arguments before
		// processing by CEF and Chromium. The |process_type| value will be NULL for
		// the browser process. Do not keep a reference to the cef_command_line_t
		// object passed to this function. The CefSettings.command_line_args_disabled
		// value can be used to start with an NULL command-line object. Any values
		// specified in CefSettings that equate to command-line arguments will be set
		// before this function is called. Be cautious when using this function to
		// modify command-line arguments for non-browser processes as this may result
		// in undefined behavior including crashes.
		procedure OnBeforeCommandLineProcessing(const aProcessType: ustring; const aCommandLine: ICefCommandLine);
		// Provides an opportunity to register custom schemes. Do not keep a reference
		// to the |registrar| object. This function is called on the main thread for
		// each process and the registered schemes should be the same across all
		// processes.
		procedure OnRegisterCustomSchemes(const aRegistrar: ICefSchemeRegistrar);
		// Return the handler for resource bundle events. If
		// CefSettings.pack_loading_disabled is true (1) a handler must be returned.
		// If no handler is returned resources will be loaded from pack files. This
		// function is called by the browser and render processes on multiple threads.
		function GetResourceBundleHandler: ICefResourceBundleHandler;
		// Return the handler for functionality specific to the browser process. This
		// function is called on multiple threads in the browser process.
		function GetBrowserProcessHandler: ICefBrowserProcessHandler;
		// Return the handler for functionality specific to the render process. This
		// function is called on the render process main thread.
		function GetRenderProcessHandler: ICefRenderProcessHandler;

		property BrowserProcessHandler: ICefBrowserProcessHandler read GetBrowserProcessHandler;
		property RenderProcessHandler: ICefRenderProcessHandler read GetRenderProcessHandler;
		property ResourceBundleHandler: ICefResourceBundleHandler read GetResourceBundleHandler;
	end;


	//..............................................................................cef_auth_callback_capi.h
	// Callback structure used for asynchronous continuation of authentication
	// requests.
	ICefAuthCallback = Interface(ICefBase)
	['{05D1B196-F26C-497B-B29F-144F2E129756}']
		// Continue the authentication request.
		procedure Cont(const aUsername: ustring; const aPassword: ustring);
		// Cancel the authentication request.
		procedure Cancel;
	end;


	//..............................................................................cef_browser_capi.h
	// Structure used to represent a browser window. When used in the browser
	// process the functions of this structure may be called on any thread unless
	// otherwise indicated in the comments. When used in the render process the
	// functions of this structure may only be called on the main thread.
	ICefBrowser = Interface(ICefBase)
	['{B832FC91-0990-4129-B402-14CEB092F56A}']
		// Returns the browser host object. This function can only be called in the
		// browser process.
		function GetHost: ICefBrowserHost;
		// Returns true (1) if the browser can navigate backwards.
		function CanGoBack: Boolean;
		// Navigate backwards.
		procedure GoBack;
		// Returns true (1) if the browser can navigate forwards.
		function CanGoForward: Boolean;
		// Navigate forwards.
		procedure GoForward;
		// Returns true (1) if the browser is currently loading.
		function IsLoading: Boolean;
		// Reload the current page.
		procedure Reload;
		// Reload the current page ignoring any cached data.
		procedure ReloadIgnoreCache;
		// Stop loading the page.
		procedure StopLoad;
		// Returns the globally unique identifier for this browser.
		function GetIdentifier: cint;
		// Returns true (1) if this object is pointing to the same handle as |that|
		// object.
		function IsSame(const aThat: ICefBrowser): Boolean;
		// Returns true (1) if the window is a popup window.
		function IsPopup: Boolean;
		// Returns true (1) if a document has been loaded in the browser.
		function HasDocument: Boolean;
		// Returns the main (top-level) frame for the browser window.
		function GetMainFrame: ICefFrame;
		// Returns the focused frame for the browser window.
		function GetFocusedFrame: ICefFrame;
		// Returns the frame with the specified identifier, or NULL if not found.
		function GetFrameByident(aIdentifier: cint64): ICefFrame;
		// Returns the frame with the specified name, or NULL if not found.
		function GetFrame(const aName: ustring): ICefFrame;
		// Returns the number of frames that currently exist.
		function GetFrameCount: csize_t;
		// Returns the identifiers of all existing frames.
		procedure GetFrameIdentifiers(var aIdentifiersCount: csize_t; var aIdentifiers: cint64);
		// Returns the names of all existing frames.
		procedure GetFrameNames(aNames: TStrings);
		//
		// Send a message to the specified |target_process|. Returns true (1) if the
		// message was sent successfully.
		function SendProcessMessage(aTargetProcess: TCefProcessId; const aMessage: ICefProcessMessage): Boolean;

		property FocusedFrame: ICefFrame read GetFocusedFrame;
		property Frame[const aName: ustring]: ICefFrame read GetFrame;
		property FrameByident[aIdentifier: cint64]: ICefFrame read GetFrameByident;
		property FrameCount: csize_t read GetFrameCount;
		property Host: ICefBrowserHost read GetHost;
		property Identifier: cint read GetIdentifier;
		property MainFrame: ICefFrame read GetMainFrame;
	end;

	// Structure used to represent a browser window. When used in the browser
	// process the functions of this structure may be called on any thread unless
	// otherwise indicated in the comments. When used in the render process the
	// functions of this structure may only be called on the main thread.
	// Callback structure for cef_browser_host_t::RunFileDialog. The functions of
	// this structure will be called on the browser process UI thread.
	ICefRunFileDialogCallback = Interface(ICefBase)
	['{F265DBF8-31E4-429B-99C2-93C04C86BDC3}']
		// Called asynchronously after the file dialog is dismissed.
		// |selected_accept_filter| is the 0-based index of the value selected from
		// the accept filters array passed to cef_browser_host_t::RunFileDialog.
		// |file_paths| will be a single value or a list of values depending on the
		// dialog mode. If the selection was cancelled |file_paths| will be NULL.
		procedure OnFileDialogDismissed(aSelectedAcceptFilter: cint; aFilePaths: TStrings);
	end;

	// Structure used to represent a browser window. When used in the browser
	// process the functions of this structure may be called on any thread unless
	// otherwise indicated in the comments. When used in the render process the
	// functions of this structure may only be called on the main thread.
	// Callback structure for cef_browser_host_t::RunFileDialog. The functions of
	// this structure will be called on the browser process UI thread.
	// Callback structure for cef_browser_host_t::GetNavigationEntries. The
	// functions of this structure will be called on the browser process UI thread.
	ICefNavigationEntryVisitor = Interface(ICefBase)
	['{60B639EC-D104-4A4A-9B67-2D3749158720}']
		// Method that will be executed. Do not keep a reference to |entry| outside of
		// this callback. Return true (1) to continue visiting entries or false (0) to
		// stop. |current| is true (1) if this entry is the currently loaded
		// navigation entry. |index| is the 0-based index of this entry and |total| is
		// the total number of entries.
		function Visit(const aEntry: ICefNavigationEntry; aCurrent: Boolean; aIndex: cint; aTotal: cint): Boolean;
	end;

	// Structure used to represent a browser window. When used in the browser
	// process the functions of this structure may be called on any thread unless
	// otherwise indicated in the comments. When used in the render process the
	// functions of this structure may only be called on the main thread.
	// Callback structure for cef_browser_host_t::RunFileDialog. The functions of
	// this structure will be called on the browser process UI thread.
	// Callback structure for cef_browser_host_t::GetNavigationEntries. The
	// functions of this structure will be called on the browser process UI thread.
	// Structure used to represent the browser process aspects of a browser window.
	// The functions of this structure can only be called in the browser process.
	// They may be called on any thread in that process unless otherwise indicated
	// in the comments.

	ICefBrowserHost = Interface(ICefBase)
	['{819DD8A4-7E95-4CC2-A06F-FCE0885F55D6}']
		// Returns the hosted browser object.
		function GetBrowser: ICefBrowser;
		// Request that the browser close. The JavaScript 'onbeforeunload' event will
		// be fired. If |force_close| is false (0) the event handler, if any, will be
		// allowed to prompt the user and the user can optionally cancel the close. If
		// |force_close| is true (1) the prompt will not be displayed and the close
		// will proceed. Results in a call to cef_life_span_handler_t::do_close() if
		// the event handler allows the close or if |force_close| is true (1). See
		// cef_life_span_handler_t::do_close() documentation for additional usage
		// information.
		procedure CloseBrowser(aForceClose: Boolean);
		// Set whether the browser is focused.
		procedure SetFocus(aFocus: Boolean);
		// Set whether the window containing the browser is visible
		// (minimized/unminimized, app hidden/unhidden, etc). Only used on Mac OS X.
		procedure SetWindowVisibility(aVisible: Boolean);
		// Retrieve the window handle for this browser.
		function GetWindowHandle: TCefWindowHandle;
		// Retrieve the window handle of the browser that opened this browser. Will
		// return NULL for non-popup windows. This function can be used in combination
		// with custom handling of modal windows.
		function GetOpenerWindowHandle: TCefWindowHandle;
		// Returns the client for this browser.
		function GetClient: ICefClient;
		// Returns the request context for this browser.
		function GetRequestContext: ICefRequestContext;
		// Get the current zoom level. The default zoom level is 0.0. This function
		// can only be called on the UI thread.
		function GetZoomLevel: cdouble;
		// Change the zoom level to the specified value. Specify 0.0 to reset the zoom
		// level. If called on the UI thread the change will be applied immediately.
		// Otherwise, the change will be applied asynchronously on the UI thread.
		procedure SetZoomLevel(aZoomLevel: cdouble);
		// Call to run a file chooser dialog. Only a single file chooser dialog may be
		// pending at any given time. |mode| represents the type of dialog to display.
		// |title| to the title to be used for the dialog and may be NULL to show the
		// default title ("Open" or "Save" depending on the mode). |default_file_path|
		// is the path with optional directory and/or file name component that will be
		// initially selected in the dialog. |accept_filters| are used to restrict the
		// selectable file types and may any combination of (a) valid lower-cased MIME
		// types (e.g. "text/*" or "image/*"), (b) individual file extensions (e.g.
		// ".txt" or ".png"), or (c) combined description and file extension delimited
		// using "|" and ";" (e.g. "Image Types|.png;.gif;.jpg").
		// |selected_accept_filter| is the 0-based index of the filter that will be
		// selected by default. |callback| will be executed after the dialog is
		// dismissed or immediately if another dialog is already pending. The dialog
		// will be initiated asynchronously on the UI thread.
		procedure RunFileDialog(aMode: TCefFileDialogMode; const aTitle: ustring; const aDefaultFilePath: ustring; aAcceptFilters: TStrings; aSelectedAcceptFilter: cint; const aCallback: ICefRunFileDialogCallback);
		// Download the file at |url| using cef_download_handler_t.
		procedure StartDownload(const aUrl: ustring);
		// Print the current browser contents.
		procedure Print;
		// Search for |searchText|. |identifier| can be used to have multiple searches
		// running simultaniously. |forward| indicates whether to search forward or
		// backward within the page. |matchCase| indicates whether the search should
		// be case-sensitive. |findNext| indicates whether this is the first request
		// or a follow-up. The cef_find_handler_t instance, if any, returned via
		// cef_client_t::GetFindHandler will be called to report find results.
		procedure Find(aIdentifier: cint; const aSearchText: ustring; aForward: Boolean; aMatchCase: Boolean; aFindNext: Boolean);
		// Cancel all searches that are currently going on.
		procedure StopFinding(aClearSelection: Boolean);
		// Open developer tools in its own window. If |inspect_element_at| is non-
		// NULL the element at the specified (x,y) location will be inspected.
		procedure ShowDevTools(var aWindowInfo: TCefWindowInfo; const aClient: ICefClient; var aSettings: TCefBrowserSettings; aInspectElementAt: TCefPoint); overload;
    procedure ShowDevTools(var aWindowInfo: TCefWindowInfo; const aClient: ICefClient; var aSettings: TCefBrowserSettings); overload;
		// Explicitly close the developer tools window if one exists for this browser
		// instance.
		procedure CloseDevTools;
		// Retrieve a snapshot of current navigation entries as values sent to the
		// specified visitor. If |current_only| is true (1) only the current
		// navigation entry will be sent, otherwise all navigation entries will be
		// sent.
		procedure GetNavigationEntries(const aVisitor: ICefNavigationEntryVisitor; aCurrentOnly: Boolean);
		// Set whether mouse cursor change is disabled.
		procedure SetMouseCursorChangeDisabled(aDisabled: Boolean);
		// Returns true (1) if mouse cursor change is disabled.
		function IsMouseCursorChangeDisabled: Boolean;
		// If a misspelled word is currently selected in an editable node calling this
		// function will replace it with the specified |word|.
		procedure ReplaceMisspelling(const aWord: ustring);
		// Add the specified |word| to the spelling dictionary.
		procedure AddWordToDictionary(const aWord: ustring);
		// Returns true (1) if window rendering is disabled.
		function IsWindowRenderingDisabled: Boolean;
		// Notify the browser that the widget has been resized. The browser will first
		// call cef_render_handler_t::GetViewRect to get the new size and then call
		// cef_render_handler_t::OnPaint asynchronously with the updated regions. This
		// function is only used when window rendering is disabled.
		procedure WasResized;
		// Notify the browser that it has been hidden or shown. Layouting and
		// cef_render_handler_t::OnPaint notification will stop when the browser is
		// hidden. This function is only used when window rendering is disabled.
		procedure WasHidden(aHidden: Boolean);
		// Send a notification to the browser that the screen info has changed. The
		// browser will then call cef_render_handler_t::GetScreenInfo to update the
		// screen information with the new values. This simulates moving the webview
		// window from one display to another, or changing the properties of the
		// current display. This function is only used when window rendering is
		// disabled.
		procedure NotifyScreenInfoChanged;
		// Invalidate the view. The browser will call cef_render_handler_t::OnPaint
		// asynchronously. This function is only used when window rendering is
		// disabled.
		procedure Invalidate(const aType: TCefPaintElementType);
		// Send a key event to the browser.
		procedure SendKeyEvent(const aEvent: TCefKeyEvent);
		// Send a mouse click event to the browser. The |x| and |y| coordinates are
		// relative to the upper-left corner of the view.
		procedure SendMouseClickEvent(const aEvent: TCefMouseEvent; aType: TCefMouseButtonType; aMouseUp: Boolean; aClickCount: cint);
		// Send a mouse move event to the browser. The |x| and |y| coordinates are
		// relative to the upper-left corner of the view.
		procedure SendMouseMoveEvent(const aEvent: TCefMouseEvent; aMouseLeave: Boolean);
		// Send a mouse wheel event to the browser. The |x| and |y| coordinates are
		// relative to the upper-left corner of the view. The |deltaX| and |deltaY|
		// values represent the movement delta in the X and Y directions respectively.
		// In order to scroll inside select popups with window rendering disabled
		// cef_render_handler_t::GetScreenPoint should be implemented properly.
		procedure SendMouseWheelEvent(const aEvent: TCefMouseEvent; aDeltaX: cint; aDeltaY: cint);
		// Send a focus event to the browser.
		procedure SendFocusEvent(aSetFocus: Boolean);
		// Send a capture lost event to the browser.
		procedure SendCaptureLostEvent;
		// Notify the browser that the window hosting it is about to be moved or
		// resized. This function is only used on Windows and Linux.
		procedure NotifyMoveOrResizeStarted;
    {$IFDEF MACOS}
		// Get the NSTextInputContext implementation for enabling IME on Mac when
		// window rendering is disabled.
		function GetNstextInputContext: TCefTextInputContext;
		// Handles a keyDown event prior to passing it through the NSTextInputClient
		// machinery.
		procedure HandleKeyEventBeforeTextInputClient(aKeyEvent: TCefEventHandle);
		// Performs any additional actions after NSTextInputClient handles the event.
		procedure HandleKeyEventAfterTextInputClient(aKeyEvent: TCefEventHandle);
    {$ENDIF}
		// Call this function when the user drags the mouse into the web view (before
		// calling DragTargetDragOver/DragTargetLeave/DragTargetDrop). |drag_data|
		// should not contain file contents as this type of data is not allowed to be
		// dragged into the web view. File contents can be removed using
		// cef_drag_data_t::ResetFileContents (for example, if |drag_data| comes from
		// cef_render_handler_t::StartDragging). This function is only used when
		// window rendering is disabled.
		procedure DragTargetDragEnter(const aDragData: ICefDragData; const aEvent: TCefMouseEvent; aAllowedOps: TCefDragOperationsMask);
		// Call this function each time the mouse is moved across the web view during
		// a drag operation (after calling DragTargetDragEnter and before calling
		// DragTargetDragLeave/DragTargetDrop). This function is only used when window
		// rendering is disabled.
		procedure DragTargetDragOver(const aEvent: TCefMouseEvent; aAllowedOps: TCefDragOperationsMask);
		// Call this function when the user drags the mouse out of the web view (after
		// calling DragTargetDragEnter). This function is only used when window
		// rendering is disabled.
		procedure DragTargetDragLeave;
		// Call this function when the user completes the drag operation by dropping
		// the object onto the web view (after calling DragTargetDragEnter). The
		// object being dropped is |drag_data|, given as an argument to the previous
		// DragTargetDragEnter call. This function is only used when window rendering
		// is disabled.
		procedure DragTargetDrop(const aEvent: TCefMouseEvent);
		// Call this function when the drag operation started by a
		// cef_render_handler_t::StartDragging call has ended either in a drop or by
		// being cancelled. |x| and |y| are mouse coordinates relative to the upper-
		// left corner of the view. If the web view is both the drag source and the
		// drag target then all DragTarget* functions should be called before
		// DragSource* mthods. This function is only used when window rendering is
		// disabled.
		procedure DragSourceEndedAt(aX: cint; aY: cint; aOp: TCefDragOperationsMask);
		// Call this function when the drag operation started by a
		// cef_render_handler_t::StartDragging call has completed. This function may
		// be called immediately without first calling DragSourceEndedAt to cancel a
		// drag operation. If the web view is both the drag source and the drag target
		// then all DragTarget* functions should be called before DragSource* mthods.
		// This function is only used when window rendering is disabled.
		procedure DragSourceSystemDragEnded;

		property Browser: ICefBrowser read GetBrowser;
		property Client: ICefClient read GetClient;
    {$IFDEF MACOS}
		property NstextInputContext: TCefTextInputContext read GetNstextInputContext;
    {$ENDIF}
		property OpenerWindowHandle: TCefWindowHandle read GetOpenerWindowHandle;
		property RequestContext: ICefRequestContext read GetRequestContext;
		property WindowHandle: TCefWindowHandle read GetWindowHandle;
		property ZoomLevel: cdouble read GetZoomLevel write SetZoomLevel;
	end;

	//..............................................................................cef_browser_process_handler_capi.h
	// Structure used to implement browser process callbacks. The functions of this
	// structure will be called on the browser process main thread unless otherwise
	// indicated.
	ICefBrowserProcessHandler = Interface(ICefBase)
	['{22CD5CCC-7567-4C42-9BCB-B86A4BCF9EBB}']
		// Called on the browser process UI thread immediately after the CEF context
		// has been initialized.
		procedure OnContextInitialized;
		// Called before a child process is launched. Will be called on the browser
		// process UI thread when launching a render process and on the browser
		// process IO thread when launching a GPU or plugin process. Provides an
		// opportunity to modify the child process command line. Do not keep a
		// reference to |command_line| outside of this function.
		procedure OnBeforeChildProcessLaunch(const aCommandLine: ICefCommandLine);
		// Called on the browser process IO thread after the main thread has been
		// created for a new render process. Provides an opportunity to specify extra
		// information that will be passed to
		// cef_render_process_handler_t::on_render_thread_created() in the render
		// process. Do not keep a reference to |extra_info| outside of this function.
		procedure OnRenderProcessThreadCreated(const aExtraInfo: ICefListValue);
		// Return the handler for printing on Linux. If a print handler is not
		// provided then printing will not be supported on the Linux platform.
		function GetPrintHandler: ICefPrintHandler;

		property PrintHandler: ICefPrintHandler read GetPrintHandler;
	end;

	//..............................................................................cef_callback_capi.h
	// Generic callback structure used for asynchronous continuation.
	ICefCallback = Interface(ICefBase)
	['{A49BC336-0998-4173-8181-BEE021C11B09}']
		// Continue processing.
		procedure Cont;
		// Cancel processing.
		procedure Cancel;
	end;

	// Generic callback structure used for asynchronous continuation.
	// Generic callback structure used for asynchronous completion.
	ICefCompletionCallback = Interface(ICefBase)
	['{201B2680-371B-4A16-B9C7-E117A71517DC}']
		// Method that will be called once the task is complete.
		procedure OnComplete;
	end;

	//..............................................................................cef_client_capi.h
	// Implement this structure to provide handler implementations.
	ICefClient = Interface(ICefBase)
	['{483C5842-3D2E-4249-8A68-F418B868AE0C}']
		// Return the handler for context menus. If no handler is provided the default
		// implementation will be used.
		function GetContextMenuHandler: ICefContextMenuHandler;
		// Return the handler for dialogs. If no handler is provided the default
		// implementation will be used.
		function GetDialogHandler: ICefDialogHandler;
		// Return the handler for browser display state events.
		function GetDisplayHandler: ICefDisplayHandler;
		// Return the handler for download events. If no handler is returned downloads
		// will not be allowed.
		function GetDownloadHandler: ICefDownloadHandler;
		// Return the handler for drag events.
		function GetDragHandler: ICefDragHandler;
		// Return the handler for find result events.
		function GetFindHandler: ICefFindHandler;
		// Return the handler for focus events.
		function GetFocusHandler: ICefFocusHandler;
		// Return the handler for geolocation permissions requests. If no handler is
		// provided geolocation access will be denied by default.
		function GetGeolocationHandler: ICefGeolocationHandler;
		// Return the handler for JavaScript dialogs. If no handler is provided the
		// default implementation will be used.
		function GetJsdialogHandler: ICefJsdialogHandler;
		// Return the handler for keyboard events.
		function GetKeyboardHandler: ICefKeyboardHandler;
		// Return the handler for browser life span events.
		function GetLifeSpanHandler: ICefLifeSpanHandler;
		// Return the handler for browser load status events.
		function GetLoadHandler: ICefLoadHandler;
		// Return the handler for off-screen rendering events.
		function GetRenderHandler: ICefRenderHandler;
		// Return the handler for browser request events.
		function GetRequestHandler: ICefRequestHandler;
		// Called when a new message is received from a different process. Return true
		// (1) if the message was handled or false (0) otherwise. Do not keep a
		// reference to or attempt to access the message outside of this callback.
		function OnProcessMessageReceived(const aBrowser: ICefBrowser; aSourceProcess: TCefProcessId; const aMessage: ICefProcessMessage): Boolean;

		property ContextMenuHandler: ICefContextMenuHandler read GetContextMenuHandler;
		property DialogHandler: ICefDialogHandler read GetDialogHandler;
		property DisplayHandler: ICefDisplayHandler read GetDisplayHandler;
		property DownloadHandler: ICefDownloadHandler read GetDownloadHandler;
		property DragHandler: ICefDragHandler read GetDragHandler;
		property FindHandler: ICefFindHandler read GetFindHandler;
		property FocusHandler: ICefFocusHandler read GetFocusHandler;
		property GeolocationHandler: ICefGeolocationHandler read GetGeolocationHandler;
		property JsdialogHandler: ICefJsdialogHandler read GetJsdialogHandler;
		property KeyboardHandler: ICefKeyboardHandler read GetKeyboardHandler;
		property LifeSpanHandler: ICefLifeSpanHandler read GetLifeSpanHandler;
		property LoadHandler: ICefLoadHandler read GetLoadHandler;
		property RenderHandler: ICefRenderHandler read GetRenderHandler;
		property RequestHandler: ICefRequestHandler read GetRequestHandler;
	end;


	//..............................................................................cef_command_line_capi.h
	// Structure used to create and/or parse command line arguments. Arguments with
	// '--', '-' and, on Windows, '/' prefixes are considered switches. Switches
	// will always precede any arguments without switch prefixes. Switches can
	// optionally have a value specified using the '=' delimiter (e.g.
	// "-switch=value"). An argument of "--" will terminate switch parsing with all
	// subsequent tokens, regardless of prefix, being interpreted as non-switch
	// arguments. Switch names are considered case-insensitive. This structure can
	// be used before cef_initialize() is called.
	ICefCommandLine = Interface(ICefBase)
	['{9784044D-A2CF-4678-8414-9DB411866524}']
		// Returns true (1) if this object is valid. Do not call any other functions
		// if this function returns false (0).
		function IsValid: Boolean;
		// Returns true (1) if the values of this object are read-only. Some APIs may
		// expose read-only objects.
		function IsReadOnly: Boolean;
		// Returns a writable copy of this object.
		function Copy: ICefCommandLine;
		// Initialize the command line with the specified |argc| and |argv| values.
		// The first argument must be the name of the program. This function is only
		// supported on non-Windows platforms.
		procedure InitFromArgv(aArgc: cint; const aArgv: PPAnsiChar);
		// Initialize the command line with the string returned by calling
		// GetCommandLineW(). This function is only supported on Windows.
		procedure InitFromString(const aCommandLine: ustring);
		// Reset the command-line switches and arguments but leave the program
		// component unchanged.
		procedure Reset;
		// Retrieve the original command line string as a vector of strings. The argv
		// array: { program, [(--|-|/)switch[=value]]*, [--], [argument]* }
		procedure GetArgv(aArgv: TStrings);
		// Constructs and returns the represented command line string. Use this
		// function cautiously because quoting behavior is unclear.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetCommandLineString: ustring;
		// Get the program part of the command line string (the first item).
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetProgram: ustring;
		// Set the program part of the command line string (the first item).
		procedure SetProgram(const aProgram: ustring);
		// Returns true (1) if the command line has switches.
		function HasSwitches: Boolean;
		// Returns true (1) if the command line contains the given switch.
		function HasSwitch(const aName: ustring): Boolean;
		// Returns the value associated with the given switch. If the switch has no
		// value or isn't present this function returns the NULL string.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetSwitchValue(const aName: ustring): ustring;
		// Returns the map of switch names and values. If a switch has no value an
		// NULL string is returned.
		procedure GetSwitches(aSwitches: TStrings);
		// Add a switch to the end of the command line. If the switch has no value
		// pass an NULL value string.
		procedure AppendSwitch(const aName: ustring);
		// Add a switch with the specified value to the end of the command line.
		procedure AppendSwitchWithValue(const aName: ustring; const aValue: ustring);
		// True if there are remaining command line arguments.
		function HasArguments: Boolean;
		// Get the remaining command line arguments.
		procedure GetArguments(aArguments: TStrings);
		// Add an argument to the end of the command line.
		procedure AppendArgument(const aArgument: ustring);
		// Insert a command before the current command. Common for debuggers, like
		// "valgrind" or "gdb --args".
		procedure PrependWrapper(const aWrapper: ustring);

		property CommandLineString: ustring read GetCommandLineString;
		property _Program: ustring read GetProgram write SetProgram;
		property SwitchValue[const aName: ustring]: ustring read GetSwitchValue;
	end;


	//..............................................................................cef_context_menu_handler_capi.h
	// Implement this structure to handle context menu events. The functions of this
	// structure will be called on the UI thread.
	ICefContextMenuHandler = Interface(ICefBase)
	['{8358E6EE-BDBC-44B9-9C64-E5F1BD003810}']
		// Called before a context menu is displayed. |params| provides information
		// about the context menu state. |model| initially contains the default
		// context menu. The |model| can be cleared to show no context menu or
		// modified to show a custom menu. Do not keep references to |params| or
		// |model| outside of this callback.
		procedure OnBeforeContextMenu(const aBrowser: ICefBrowser; const aFrame: ICefFrame; const aParams: ICefContextMenuParams; const aModel: ICefMenuModel);
		// Called to execute a command selected from the context menu. Return true (1)
		// if the command was handled or false (0) for the default implementation. See
		// cef_menu_id_t for the command ids that have default implementations. All
		// user-defined command ids should be between MENU_ID_USER_FIRST and
		// MENU_ID_USER_LAST. |params| will have the same values as what was passed to
		// on_before_context_menu(). Do not keep a reference to |params| outside of
		// this callback.
		function OnContextMenuCommand(const aBrowser: ICefBrowser; const aFrame: ICefFrame; const aParams: ICefContextMenuParams; aCommandId: cint; aEventFlags: TCefEventFlags): Boolean;
		// Called when the context menu is dismissed irregardless of whether the menu
		// was NULL or a command was selected.
		procedure OnContextMenuDismissed(const aBrowser: ICefBrowser; const aFrame: ICefFrame);
	end;

	// Implement this structure to handle context menu events. The functions of this
	// structure will be called on the UI thread.
	// Provides information about the context menu state. The ethods of this
	// structure can only be accessed on browser process the UI thread.
  ICefContextMenuParams = Interface(ICefBase)
	['{2C95BE8B-7F6C-4ABE-BAF0-BE297A354576}']
		// Returns the X coordinate of the mouse where the context menu was invoked.
		// Coords are relative to the associated RenderView's origin.
		function GetXcoord: cint;
		// Returns the Y coordinate of the mouse where the context menu was invoked.
		// Coords are relative to the associated RenderView's origin.
		function GetYcoord: cint;
		// Returns flags representing the type of node that the context menu was
		// invoked on.
		function GetTypeFlags: TCefContextMenuTypeFlags;
		// Returns the URL of the link, if any, that encloses the node that the
		// context menu was invoked on.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetLinkUrl: ustring;
		// Returns the link URL, if any, to be used ONLY for "copy link address". We
		// don't validate this field in the frontend process.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetUnfilteredLinkUrl: ustring;
		// Returns the source URL, if any, for the element that the context menu was
		// invoked on. Example of elements with source URLs are img, audio, and video.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetSourceUrl: ustring;
		// Returns true (1) if the context menu was invoked on an image which has non-
		// NULL contents.
		function HasImageContents: Boolean;
		// Returns the URL of the top level page that the context menu was invoked on.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetPageUrl: ustring;
		// Returns the URL of the subframe that the context menu was invoked on.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetFrameUrl: ustring;
		// Returns the character encoding of the subframe that the context menu was
		// invoked on.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetFrameCharset: ustring;
		// Returns the type of context node that the context menu was invoked on.
		function GetMediaType: TCefContextMenuMediaType;
		// Returns flags representing the actions supported by the media element, if
		// any, that the context menu was invoked on.
		function GetMediaStateFlags: TCefContextMenuMediaStateFlags;
		// Returns the text of the selection, if any, that the context menu was
		// invoked on.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetSelectionText: ustring;
		// Returns the text of the misspelled word, if any, that the context menu was
		// invoked on.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetMisspelledWord: ustring;
		// Returns true (1) if suggestions exist, false (0) otherwise. Fills in
		// |suggestions| from the spell check service for the misspelled word if there
		// is one.
		function GetDictionarySuggestions(aSuggestions: TStrings): Boolean;
		// Returns true (1) if the context menu was invoked on an editable node.
		function IsEditable: Boolean;
		// Returns true (1) if the context menu was invoked on an editable node where
		// spell-check is enabled.
		function IsSpellCheckEnabled: Boolean;
		// Returns flags representing the actions supported by the editable node, if
		// any, that the context menu was invoked on.
		function GetEditStateFlags: TCefContextMenuEditStateFlags;

		property DictionarySuggestions[aSuggestions: TStrings]: Boolean read GetDictionarySuggestions;
		property EditStateFlags: TCefContextMenuEditStateFlags read GetEditStateFlags;
		property FrameCharset: ustring read GetFrameCharset;
		property FrameUrl: ustring read GetFrameUrl;
		property LinkUrl: ustring read GetLinkUrl;
		property MediaStateFlags: TCefContextMenuMediaStateFlags read GetMediaStateFlags;
		property MediaType: TCefContextMenuMediaType read GetMediaType;
		property MisspelledWord: ustring read GetMisspelledWord;
		property PageUrl: ustring read GetPageUrl;
		property SelectionText: ustring read GetSelectionText;
		property SourceUrl: ustring read GetSourceUrl;
		property TypeFlags: TCefContextMenuTypeFlags read GetTypeFlags;
		property UnfilteredLinkUrl: ustring read GetUnfilteredLinkUrl;
		property Xcoord: cint read GetXcoord;
		property Ycoord: cint read GetYcoord;
	end;

	//..............................................................................cef_cookie_capi.h
	// Structure used for managing cookies. The functions of this structure may be
	// called on any thread unless otherwise indicated.
	ICefCookieManager = Interface(ICefBase)
	['{C2617B5A-F34E-427B-8DD3-103EAEFD75C0}']
		// Set the schemes supported by this manager. By default only "http" and
		// "https" schemes are supported. If |callback| is non-NULL it will be
		// executed asnychronously on the IO thread after the change has been applied.
		// Must be called before any cookies are accessed.
		procedure SetSupportedSchemes(aSchemes: TStrings; const aCallback: ICefCompletionCallback);
		// Visit all cookies on the IO thread. The returned cookies are ordered by
		// longest path, then by earliest creation date. Returns false (0) if cookies
		// cannot be accessed.
		function VisitAllCookies(const aVisitor: ICefCookieVisitor): Boolean;
    function VisitAllCookiesProc(const aVisitor: TCefCookieVisitorProc): Boolean;
		// Visit a subset of cookies on the IO thread. The results are filtered by the
		// given url scheme, host, domain and path. If |includeHttpOnly| is true (1)
		// HTTP-only cookies will also be included in the results. The returned
		// cookies are ordered by longest path, then by earliest creation date.
		// Returns false (0) if cookies cannot be accessed.
		function VisitUrlCookies(const aUrl: ustring; aIncludeHttpOnly: Boolean; const aVisitor: ICefCookieVisitor): Boolean;
    function VisitUrlCookiesProc(const aUrl: ustring; aIncludeHttpOnly: Boolean; const aVisitor: TCefCookieVisitorProc): Boolean;
		// Sets a cookie given a valid URL and explicit user-provided cookie
		// attributes. This function expects each attribute to be well-formed. It will
		// check for disallowed characters (e.g. the ';' character is disallowed
		// within the cookie value attribute) and fail without setting the cookie if
		// such characters are found. If |callback| is non-NULL it will be executed
		// asnychronously on the IO thread after the cookie has been set. Returns
		// false (0) if an invalid URL is specified or if cookies cannot be accessed.
		function SetCookie(const aUrl: ustring; const aCookie: TCefCookie; const aCallback: ICefSetCookieCallback): Boolean;
		// Delete all cookies that match the specified parameters. If both |url| and
		// |cookie_name| values are specified all host and domain cookies matching
		// both will be deleted. If only |url| is specified all host cookies (but not
		// domain cookies) irrespective of path will be deleted. If |url| is NULL all
		// cookies for all hosts and domains will be deleted. If |callback| is non-
		// NULL it will be executed asnychronously on the IO thread after the cookies
		// have been deleted. Returns false (0) if a non-NULL invalid URL is specified
		// or if cookies cannot be accessed. Cookies can alternately be deleted using
		// the Visit*Cookies() functions.
		function DeleteCookies(const aUrl: ustring; const aCookieName: ustring; const aCallback: ICefDeleteCookiesCallback): Boolean;
		// Sets the directory path that will be used for storing cookie data. If
		// |path| is NULL data will be stored in memory only. Otherwise, data will be
		// stored at the specified |path|. To persist session cookies (cookies without
		// an expiry date or validity interval) set |persist_session_cookies| to true
		// (1). Session cookies are generally intended to be transient and most Web
		// browsers do not persist them. If |callback| is non-NULL it will be executed
		// asnychronously on the IO thread after the manager's storage has been
		// initialized. Returns false (0) if cookies cannot be accessed.
		function SetStoragePath(const aPath: ustring; aPersistSessionCookies: Boolean; const aCallback: ICefCompletionCallback): Boolean;
		// Flush the backing store (if any) to disk. If |callback| is non-NULL it will
		// be executed asnychronously on the IO thread after the flush is complete.
		// Returns false (0) if cookies cannot be accessed.
		function FlushStore(const aCallback: ICefCompletionCallback): Boolean;
	end;

	// Structure to implement for visiting cookie values. The functions of this
	// structure will always be called on the IO thread.
	ICefCookieVisitor = Interface(ICefBase)
	['{25F68230-F0DF-4A6B-995B-493872DCDEC2}']
		// Method that will be called once for each cookie. |count| is the 0-based
		// index for the current cookie. |total| is the total number of cookies. Set
		// |deleteCookie| to true (1) to delete the cookie currently being visited.
		// Return false (0) to stop visiting cookies. This function may never be
		// called if no cookies are found.
		function Visit(const aCookie: TWACefCookie; aCount: cint; aTotal: cint; out aDeleteCookie: Boolean): Boolean;
	end;

	// Structure to implement for visiting cookie values. The functions of this
	// structure will always be called on the IO thread.
	// Structure to implement to be notified of asynchronous completion via
	// cef_cookie_manager_t::set_cookie().
	ICefSetCookieCallback = Interface(ICefBase)
	['{21A89F25-52EE-4CBB-8B96-34E366E31332}']
		// Method that will be called upon completion. |success| will be true (1) if
		// the cookie was set successfully.
		procedure OnComplete(aSuccess: Boolean);
	end;

	// Structure to implement for visiting cookie values. The functions of this
	// structure will always be called on the IO thread.
	// Structure to implement to be notified of asynchronous completion via
	// cef_cookie_manager_t::set_cookie().
	// Structure to implement to be notified of asynchronous completion via
	// cef_cookie_manager_t::delete_cookies().
	ICefDeleteCookiesCallback = Interface(ICefBase)
	['{2869EF83-65D4-4A84-A456-39B335EE10E1}']
		// Method that will be called upon completion. |num_deleted| will be the
		// number of cookies that were deleted or -1 if unknown.
		procedure OnComplete(aNumDeleted: cint);
	end;

	//..............................................................................cef_dialog_handler_capi.h
	// Callback structure for asynchronous continuation of file dialog requests.
	ICefFileDialogCallback = Interface(ICefBase)
	['{EA50C5B6-E9F9-4F4C-BEEC-D97FC1AC41FA}']
		// Continue the file selection. |selected_accept_filter| should be the 0-based
		// index of the value selected from the accept filters array passed to
		// cef_dialog_handler_t::OnFileDialog. |file_paths| should be a single value
		// or a list of values depending on the dialog mode. An NULL |file_paths|
		// value is treated the same as calling cancel().
		procedure Cont(aSelectedAcceptFilter: cint; aFilePaths: TStrings);
		// Cancel the file selection.
		procedure Cancel;
	end;

	// Callback structure for asynchronous continuation of file dialog requests.
	// Implement this structure to handle dialog events. The functions of this
	// structure will be called on the browser process UI thread.
	ICefDialogHandler = Interface(ICefBase)
	['{CB6598DF-202A-478D-805C-894DEBE452EC}']
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
		function OnFileDialog(const aBrowser: ICefBrowser; aMode: TCefFileDialogMode; const aTitle: ustring; const aDefaultFilePath: ustring; aAcceptFilters: TStrings; aSelectedAcceptFilter: cint; const aCallback: ICefFileDialogCallback): Boolean;
	end;

	//..............................................................................cef_display_handler_capi.h
	// Implement this structure to handle events related to browser display state.
	// The functions of this structure will be called on the UI thread.
	ICefDisplayHandler = Interface(ICefBase)
	['{564E317A-B8D2-4522-8A2F-240C6AB05016}']
		// Called when a frame's address has changed.
		procedure OnAddressChange(const aBrowser: ICefBrowser; const aFrame: ICefFrame; const aUrl: ustring);
		// Called when the page title changes.
		procedure OnTitleChange(const aBrowser: ICefBrowser; const aTitle: ustring);
		// Called when the page icon changes.
		procedure OnFaviconUrlchange(const aBrowser: ICefBrowser; aIconUrls: TStrings);
		// Called when the browser is about to display a tooltip. |text| contains the
		// text that will be displayed in the tooltip. To handle the display of the
		// tooltip yourself return true (1). Otherwise, you can optionally modify
		// |text| and then return false (0) to allow the browser to display the
		// tooltip. When window rendering is disabled the application is responsible
		// for drawing tooltips and the return value is ignored.
		function OnTooltip(const aBrowser: ICefBrowser; var aText: ustring): Boolean;
		// Called when the browser receives a status message. |value| contains the
		// text that will be displayed in the status message.
		procedure OnStatusMessage(const aBrowser: ICefBrowser; const aValue: ustring);
		// Called to display a console message. Return true (1) to stop the message
		// from being output to the console.
		function OnConsoleMessage(const aBrowser: ICefBrowser; const aMessage: ustring; const aSource: ustring; aLine: cint): Boolean;
	end;

	//..............................................................................cef_dom_capi.h
	// Structure to implement for visiting the DOM. The functions of this structure
	// will be called on the render process main thread.
	ICefDomvisitor = Interface(ICefBase)
	['{0996CC94-D306-468F-9445-9142DAD9CFD0}']
		// Method executed for visiting the DOM. The document object passed to this
		// function represents a snapshot of the DOM at the time this function is
		// executed. DOM objects are only valid for the scope of this function. Do not
		// keep references to or attempt to access any DOM objects outside the scope
		// of this function.
		procedure Visit(const aDocument: ICefDomdocument);
	end;

	// Structure to implement for visiting the DOM. The functions of this structure
	// will be called on the render process main thread.
	// Structure used to represent a DOM document. The functions of this structure
	// should only be called on the render process main thread thread.
	ICefDomdocument = Interface(ICefBase)
	['{90A1C1DD-266E-4F5E-8E7A-14815A6FEF03}']
		// Returns the document type.
		function GetType: TCefDomDocumentType;
		// Returns the root document node.
		function GetDocument: ICefDomnode;
		// Returns the BODY node of an HTML document.
		function GetBody: ICefDomnode;
		// Returns the HEAD node of an HTML document.
		function GetHead: ICefDomnode;
		// Returns the title of an HTML document.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetTitle: ustring;
		// Returns the document element with the specified ID value.
		function GetElementById(const aId: ustring): ICefDomnode;
		// Returns the node that currently has keyboard focus.
		function GetFocusedNode: ICefDomnode;
		// Returns true (1) if a portion of the document is selected.
		function HasSelection: Boolean;
		// Returns the selection offset within the start node.
		function GetSelectionStartOffset: cint;
		// Returns the selection offset within the end node.
		function GetSelectionEndOffset: cint;
		// Returns the contents of this selection as markup.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetSelectionAsMarkup: ustring;
		// Returns the contents of this selection as text.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetSelectionAsText: ustring;
		// Returns the base URL for the document.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetBaseUrl: ustring;
		// Returns a complete URL based on the document base URL and the specified
		// partial URL.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetCompleteUrl(const aPartialURL: ustring): ustring;

		property BaseUrl: ustring read GetBaseUrl;
		property Body: ICefDomnode read GetBody;
		property CompleteUrl[const aPartialURL: ustring]: ustring read GetCompleteUrl;
		property Document: ICefDomnode read GetDocument;
		property ElementById[const aId: ustring]: ICefDomnode read GetElementById;
		property FocusedNode: ICefDomnode read GetFocusedNode;
		property Head: ICefDomnode read GetHead;
		property SelectionAsMarkup: ustring read GetSelectionAsMarkup;
		property SelectionAsText: ustring read GetSelectionAsText;
		property SelectionEndOffset: cint read GetSelectionEndOffset;
		property SelectionStartOffset: cint read GetSelectionStartOffset;
		property Title: ustring read GetTitle;
		property _Type: TCefDomDocumentType read GetType;
	end;

	// Structure to implement for visiting the DOM. The functions of this structure
	// will be called on the render process main thread.
	// Structure used to represent a DOM document. The functions of this structure
	// should only be called on the render process main thread thread.
	// Structure used to represent a DOM node. The functions of this structure
	// should only be called on the render process main thread.
	ICefDomnode = Interface(ICefBase)
	['{B982A960-CD6D-4372-A9CA-F15E18DF7B91}']
		// Returns the type for this node.
		function GetType: TCefDomNodeType;
		// Returns true (1) if this is a text node.
		function IsText: Boolean;
		// Returns true (1) if this is an element node.
		function IsElement: Boolean;
		// Returns true (1) if this is an editable node.
		function IsEditable: Boolean;
		// Returns true (1) if this is a form control element node.
		function IsFormControlElement: Boolean;
		// Returns the type of this form control element node.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetFormControlElementType: ustring;
		// Returns true (1) if this object is pointing to the same handle as |that|
		// object.
		function IsSame(const aThat: ICefDomnode): Boolean;
		// Returns the name of this node.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetName: ustring;
		// Returns the value of this node.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetValue: ustring;
		// Set the value of this node. Returns true (1) on success.
		function SetValue(const aValue: ustring): Boolean;
		// Returns the contents of this node as markup.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetAsMarkup: ustring;
		// Returns the document associated with this node.
		function GetDocument: ICefDomdocument;
		// Returns the parent node.
		function GetParent: ICefDomnode;
		// Returns the previous sibling node.
		function GetPreviousSibling: ICefDomnode;
		// Returns the next sibling node.
		function GetNextSibling: ICefDomnode;
		// Returns true (1) if this node has child nodes.
		function HasChildren: Boolean;
		// Return the first child node.
		function GetFirstChild: ICefDomnode;
		// Returns the last child node.
		function GetLastChild: ICefDomnode;
		// Returns the tag name of this element.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetElementTagName: ustring;
		// Returns true (1) if this element has attributes.
		function HasElementAttributes: Boolean;
		// Returns true (1) if this element has an attribute named |attrName|.
		function HasElementAttribute(const aAttrName: ustring): Boolean;
		// Returns the element attribute named |attrName|.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetElementAttribute(const aAttrName: ustring): ustring;
		// Returns a map of all element attributes.
		procedure GetElementAttributes(aAttrMap: TStrings);
		// Set the value for the element attribute named |attrName|. Returns true (1)
		// on success.
		function SetElementAttribute(const aAttrName: ustring; const aValue: ustring): Boolean;
		// Returns the inner text of the element.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetElementInnerText: ustring;

		property AsMarkup: ustring read GetAsMarkup;
		property Document: ICefDomdocument read GetDocument;
		property ElementInnerText: ustring read GetElementInnerText;
		property ElementTagName: ustring read GetElementTagName;
		property FirstChild: ICefDomnode read GetFirstChild;
		property FormControlElementType: ustring read GetFormControlElementType;
		property LastChild: ICefDomnode read GetLastChild;
		property Name: ustring read GetName;
		property NextSibling: ICefDomnode read GetNextSibling;
		property Parent: ICefDomnode read GetParent;
		property PreviousSibling: ICefDomnode read GetPreviousSibling;
		property _Type: TCefDomNodeType read GetType;
	end;

	//..............................................................................cef_download_handler_capi.h
	// Callback structure used to asynchronously continue a download.
	ICefBeforeDownloadCallback = Interface(ICefBase)
	['{E3CA9BC1-A243-45C7-9A6C-212EDD4BC230}']
		// Call to continue the download. Set |download_path| to the full file path
		// for the download including the file name or leave blank to use the
		// suggested name and the default temp directory. Set |show_dialog| to true
		// (1) if you do wish to show the default "Save As" dialog.
		procedure Cont(var aDownloadPath: ustring; aShowDialog: Boolean);
	end;

	// Callback structure used to asynchronously continue a download.
	// Callback structure used to asynchronously cancel a download.
	ICefDownloadItemCallback = Interface(ICefBase)
	['{314D9AAA-2B37-415C-81F2-DEC28FACD910}']
		// Call to cancel the download.
		procedure Cancel;
		// Call to pause the download.
		procedure Pause;
		// Call to resume the download.
		procedure Resume;
	end;

	// Callback structure used to asynchronously continue a download.
	// Callback structure used to asynchronously cancel a download.
	// Structure used to handle file downloads. The functions of this structure will
	// called on the browser process UI thread.
	ICefDownloadHandler = Interface(ICefBase)
	['{7A786A5A-6C40-45D0-AB11-98B6C713B66B}']
		// Called before a download begins. |suggested_name| is the suggested name for
		// the download file. By default the download will be canceled. Execute
		// |callback| either asynchronously or in this function to continue the
		// download if desired. Do not keep a reference to |download_item| outside of
		// this function.
		procedure OnBeforeDownload(const aBrowser: ICefBrowser; const aDownloadItem: ICefDownloadItem; const aSuggestedName: ustring; const aCallback: ICefBeforeDownloadCallback);
		// Called when a download's status or progress information has been updated.
		// This may be called multiple times before and after on_before_download().
		// Execute |callback| either asynchronously or in this function to cancel the
		// download if desired. Do not keep a reference to |download_item| outside of
		// this function.
		procedure OnDownloadUpdated(const aBrowser: ICefBrowser; const aDownloadItem: ICefDownloadItem; const aCallback: ICefDownloadItemCallback);
	end;

	//..............................................................................cef_download_item_capi.h
	// Structure used to represent a download item.
	ICefDownloadItem = Interface(ICefBase)
	['{96CDC030-B85D-4536-8404-504669F91592}']
		// Returns true (1) if this object is valid. Do not call any other functions
		// if this function returns false (0).
		function IsValid: Boolean;
		// Returns true (1) if the download is in progress.
		function IsInProgress: Boolean;
		// Returns true (1) if the download is complete.
		function IsComplete: Boolean;
		// Returns true (1) if the download has been canceled or interrupted.
		function IsCanceled: Boolean;
		// Returns a simple speed estimate in bytes/s.
		function GetCurrentSpeed: cint64;
		// Returns the rough percent complete or -1 if the receive total size is
		// unknown.
		function GetPercentComplete: cint;
		// Returns the total number of bytes.
		function GetTotalBytes: cint64;
		// Returns the number of received bytes.
		function GetReceivedBytes: cint64;
		// Returns the time that the download started.
		function GetStartTime: TCefTime;
		// Returns the time that the download ended.
		function GetEndTime: TCefTime;
		// Returns the full path to the downloaded or downloading file.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetFullPath: ustring;
		// Returns the unique identifier for this download.
		function GetId: cuint32;
		// Returns the URL.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetUrl: ustring;
		// Returns the original URL before any redirections.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetOriginalUrl: ustring;
		// Returns the suggested file name.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetSuggestedFileName: ustring;
		// Returns the content disposition.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetContentDisposition: ustring;
		// Returns the mime type.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetMimeType: ustring;

		property ContentDisposition: ustring read GetContentDisposition;
		property CurrentSpeed: cint64 read GetCurrentSpeed;
		property EndTime: TCefTime read GetEndTime;
		property FullPath: ustring read GetFullPath;
		property Id: cuint32 read GetId;
		property MimeType: ustring read GetMimeType;
		property OriginalUrl: ustring read GetOriginalUrl;
		property PercentComplete: cint read GetPercentComplete;
		property ReceivedBytes: cint64 read GetReceivedBytes;
		property StartTime: TCefTime read GetStartTime;
		property SuggestedFileName: ustring read GetSuggestedFileName;
		property TotalBytes: cint64 read GetTotalBytes;
		property Url: ustring read GetUrl;
	end;


	//..............................................................................cef_drag_data_capi.h
	// Structure used to represent drag data. The functions of this structure may be
	// called on any thread.
	ICefDragData = Interface(ICefBase)
	['{6267A0E6-FCC6-454B-9CEB-92BC9A4DCF74}']
		// Returns a copy of the current object.
		function Clone: ICefDragData;
		// Returns true (1) if this object is read-only.
		function IsReadOnly: Boolean;
		// Returns true (1) if the drag data is a link.
		function IsLink: Boolean;
		// Returns true (1) if the drag data is a text or html fragment.
		function IsFragment: Boolean;
		// Returns true (1) if the drag data is a file.
		function IsFile: Boolean;
		// Return the link URL that is being dragged.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetLinkUrl: ustring;
		// Return the title associated with the link being dragged.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetLinkTitle: ustring;
		// Return the metadata, if any, associated with the link being dragged.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetLinkMetadata: ustring;
		// Return the plain text fragment that is being dragged.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetFragmentText: ustring;
		// Return the text/html fragment that is being dragged.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetFragmentHtml: ustring;
		// Return the base URL that the fragment came from. This value is used for
		// resolving relative URLs and may be NULL.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetFragmentBaseUrl: ustring;
		// Return the name of the file being dragged out of the browser window.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetFileName: ustring;
		// Write the contents of the file being dragged out of the web view into
		// |writer|. Returns the number of bytes sent to |writer|. If |writer| is NULL
		// this function will return the size of the file contents in bytes. Call
		// get_file_name() to get a suggested name for the file.
		function GetFileContents(const aWriter: ICefStreamWriter): csize_t;
		// Retrieve the list of file names that are being dragged into the browser
		// window.
		function GetFileNames(aNames: TStrings): Boolean;
		// Set the link URL that is being dragged.
		procedure SetLinkUrl(const aUrl: ustring);
		// Set the title associated with the link being dragged.
		procedure SetLinkTitle(const aTitle: ustring);
		// Set the metadata associated with the link being dragged.
		procedure SetLinkMetadata(const aData: ustring);
		// Set the plain text fragment that is being dragged.
		procedure SetFragmentText(const aText: ustring);
		// Set the text/html fragment that is being dragged.
		procedure SetFragmentHtml(const aHtml: ustring);
		// Set the base URL that the fragment came from.
		procedure SetFragmentBaseUrl(const aBaseUrl: ustring);
		// Reset the file contents. You should do this before calling
		// cef_browser_host_t::DragTargetDragEnter as the web view does not allow us
		// to drag in this kind of data.
		procedure ResetFileContents;
		// Add a file that is being dragged into the webview.
		procedure AddFile(const aPath: ustring; const aDisplayName: ustring);

		property FileContents[const aWriter: ICefStreamWriter]: csize_t read GetFileContents;
		property FileName: ustring read GetFileName;
		property FragmentBaseUrl: ustring read GetFragmentBaseUrl write SetFragmentBaseUrl;
		property FragmentHtml: ustring read GetFragmentHtml write SetFragmentHtml;
		property FragmentText: ustring read GetFragmentText write SetFragmentText;
		property LinkMetadata: ustring read GetLinkMetadata write SetLinkMetadata;
		property LinkTitle: ustring read GetLinkTitle write SetLinkTitle;
		property LinkUrl: ustring read GetLinkUrl write SetLinkUrl;
	end;


	//..............................................................................cef_drag_handler_capi.h
	// Implement this structure to handle events related to dragging. The functions
	// of this structure will be called on the UI thread.
	ICefDragHandler = Interface(ICefBase)
	['{1B9C4C85-7E26-4281-9C73-79F96F23E27F}']
		// Called when an external drag event enters the browser window. |dragData|
		// contains the drag event data and |mask| represents the type of drag
		// operation. Return false (0) for default drag handling behavior or true (1)
		// to cancel the drag event.
		function OnDragEnter(const aBrowser: ICefBrowser; const aDragData: ICefDragData; aMask: TCefDragOperationsMask): Boolean;
	end;


	//..............................................................................cef_find_handler_capi.h
	// Implement this structure to handle events related to find results. The
	// functions of this structure will be called on the UI thread.
	ICefFindHandler = Interface(ICefBase)
	['{43172F09-DC18-45A8-A858-C636B6ADC5B8}']
		// Called to report find results returned by cef_browser_host_t::find().
		// |identifer| is the identifier passed to find(), |count| is the number of
		// matches currently identified, |selectionRect| is the location of where the
		// match was found (in window coordinates), |activeMatchOrdinal| is the
		// current position in the search results, and |finalUpdate| is true (1) if
		// this is the last find notification.
		procedure OnFindResult(const aBrowser: ICefBrowser; aIdentifier: cint; aCount: cint; const aSelectionRect: TCefRect; aActiveMatchOrdinal: cint; aFinalUpdate: Boolean);
	end;


	//..............................................................................cef_focus_handler_capi.h
	// Implement this structure to handle events related to focus. The functions of
	// this structure will be called on the UI thread.
	ICefFocusHandler = Interface(ICefBase)
	['{00000380-03FB-46DE-803B-D3D0447015D9}']
		// Called when the browser component is about to loose focus. For instance, if
		// focus was on the last HTML element and the user pressed the TAB key. |next|
		// will be true (1) if the browser is giving focus to the next component and
		// false (0) if the browser is giving focus to the previous component.
		procedure OnTakeFocus(const aBrowser: ICefBrowser; aNext: Boolean);
		// Called when the browser component is requesting focus. |source| indicates
		// where the focus request is originating from. Return false (0) to allow the
		// focus to be set or true (1) to cancel setting the focus.
		function OnSetFocus(const aBrowser: ICefBrowser; aSource: TCefFocusSource): Boolean;
		// Called when the browser component has received focus.
		procedure OnGotFocus(const aBrowser: ICefBrowser);
	end;

	//..............................................................................cef_frame_capi.h
	// Structure used to represent a frame in the browser window. When used in the
	// browser process the functions of this structure may be called on any thread
	// unless otherwise indicated in the comments. When used in the render process
	// the functions of this structure may only be called on the main thread.
	ICefFrame = Interface(ICefBase)
	['{93E1B6D9-9A4A-4EA7-ADD0-50DE8BF156EB}']
		// True if this object is currently attached to a valid frame.
		function IsValid: Boolean;
		// Execute undo in this frame.
		procedure Undo;
		// Execute redo in this frame.
		procedure Redo;
		// Execute cut in this frame.
		procedure Cut;
		// Execute copy in this frame.
		procedure Copy;
		// Execute paste in this frame.
		procedure Paste;
		// Execute delete in this frame.
		procedure Del;
		// Execute select all in this frame.
		procedure SelectAll;
		// Save this frame's HTML source to a temporary file and open it in the
		// default text viewing application. This function can only be called from the
		// browser process.
		procedure ViewSource;
		// Retrieve this frame's HTML source as a string sent to the specified
		// visitor.
		procedure GetSource(const aVisitor: ICefStringVisitor);
    procedure GetSourceProc(const aProc: TCefStringVisitorProc);
		// Retrieve this frame's display text as a string sent to the specified
		// visitor.
		procedure GetText(const aVisitor: ICefStringVisitor);
    procedure GetTextProc(const aProc: TCefStringVisitorProc);
		// Load the request represented by the |request| object.
		procedure LoadRequest(const aRequest: ICefRequest);
		// Load the specified |url|.
		procedure LoadUrl(const aUrl: ustring);
		// Load the contents of |string_val| with the specified dummy |url|. |url|
		// should have a standard scheme (for example, http scheme) or behaviors like
		// link clicks and web security restrictions may not behave as expected.
		procedure LoadString(const aStringVal: ustring; const aUrl: ustring);
		// Execute a string of JavaScript code in this frame. The |script_url|
		// parameter is the URL where the script in question can be found, if any. The
		// renderer may request this URL to show the developer the source of the
		// error.  The |start_line| parameter is the base line number to use for error
		// reporting.
		procedure ExecuteJavaScript(const aCode: ustring; const aScriptUrl: ustring; aStartLine: cint);
		// Returns true (1) if this is the main (top-level) frame.
		function IsMain: Boolean;
		// Returns true (1) if this is the focused frame.
		function IsFocused: Boolean;
		// Returns the name for this frame. If the frame has an assigned name (for
		// example, set via the iframe "name" attribute) then that value will be
		// returned. Otherwise a unique name will be constructed based on the frame
		// parent hierarchy. The main (top-level) frame will always have an NULL name
		// value.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetName: ustring;
		// Returns the globally unique identifier for this frame.
		function GetIdentifier: cint64;
		// Returns the parent of this frame or NULL if this is the main (top-level)
		// frame.
		function GetParent: ICefFrame;
		// Returns the URL currently loaded in this frame.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetUrl: ustring;
		// Returns the browser that this frame belongs to.
		function GetBrowser: ICefBrowser;
		// Get the V8 context associated with the frame. This function can only be
		// called from the render process.
		function GetV8context: ICefV8context;
		// Visit the DOM document. This function can only be called from the render
		// process.
		procedure VisitDom(const aVisitor: ICefDomvisitor);
    procedure VisitDomProc(const aProc: TCefDomVisitorProc);

		property Browser: ICefBrowser read GetBrowser;
		property Identifier: cint64 read GetIdentifier;
		property Name: ustring read GetName;
		property Parent: ICefFrame read GetParent;
		property Url: ustring read GetUrl;
		property V8context: ICefV8context read GetV8context;
	end;


	//..............................................................................cef_geolocation_capi.h
	// Implement this structure to receive geolocation updates. The functions of
	// this structure will be called on the browser process UI thread.
	ICefGetGeolocationCallback = Interface(ICefBase)
	['{1C09C810-D797-4E0E-9CBD-66312D94B02B}']
		// Called with the 'best available' location information or, if the location
		// update failed, with error information.
		procedure OnLocationUpdate(const aPosition: TCefGeoposition);
	end;


	//..............................................................................cef_geolocation_handler_capi.h
	// Callback structure used for asynchronous continuation of geolocation
	// permission requests.
	ICefGeolocationCallback = Interface(ICefBase)
	['{5DEF5DC8-B7C4-40CE-A350-CFC4D1B9BF92}']
		// Call to allow or deny geolocation access.
		procedure Cont(aAllow: Boolean);
	end;

	// Callback structure used for asynchronous continuation of geolocation
	// permission requests.
	// Implement this structure to handle events related to geolocation permission
	// requests. The functions of this structure will be called on the browser
	// process UI thread.
	ICefGeolocationHandler = Interface(ICefBase)
	['{8AC12B7A-BAEE-4278-B866-A515ADB1DE40}']
		// Called when a page requests permission to access geolocation information.
		// |requesting_url| is the URL requesting permission and |request_id| is the
		// unique ID for the permission request. Return true (1) and call
		// cef_geolocation_callback_t::cont() either in this function or at a later
		// time to continue or cancel the request. Return false (0) to cancel the
		// request immediately.
		function OnRequestGeolocationPermission(const aBrowser: ICefBrowser; const aRequestingUrl: ustring; aRequestId: cint; const aCallback: ICefGeolocationCallback): Boolean;
		// Called when a geolocation access request is canceled. |requesting_url| is
		// the URL that originally requested permission and |request_id| is the unique
		// ID for the permission request.
		procedure OnCancelGeolocationPermission(const aBrowser: ICefBrowser; const aRequestingUrl: ustring; aRequestId: cint);
	end;

	//..............................................................................cef_jsdialog_handler_capi.h
	// Callback structure used for asynchronous continuation of JavaScript dialog
	// requests.
	ICefJsdialogCallback = Interface(ICefBase)
	['{D6B27CA9-A7E0-46F3-8F4B-A83566CDA7AC}']
		// Continue the JS dialog request. Set |success| to true (1) if the OK button
		// was pressed. The |user_input| value should be specified for prompt dialogs.
		procedure Cont(aSuccess: Boolean; const aUserInput: ustring);
	end;

	// Callback structure used for asynchronous continuation of JavaScript dialog
	// requests.
	// Implement this structure to handle events related to JavaScript dialogs. The
	// functions of this structure will be called on the UI thread.
	ICefJsdialogHandler = Interface(ICefBase)
	['{87094102-8CFA-4DB9-BF01-BE883F8D0426}']
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
		function OnJsdialog(const aBrowser: ICefBrowser; const aOriginUrl: ustring; const aAcceptLang: ustring; aDialogType: TCefJsdialogType; const aMessageText: ustring; const aDefaultPromptText: ustring; const aCallback: ICefJsdialogCallback; out aSuppressMessage: Boolean): Boolean;
		// Called to run a dialog asking the user if they want to leave a page. Return
		// false (0) to use the default dialog implementation. Return true (1) if the
		// application will use a custom dialog or if the callback has been executed
		// immediately. Custom dialogs may be either modal or modeless. If a custom
		// dialog is used the application must execute |callback| once the custom
		// dialog is dismissed.
		function OnBeforeUnloadDialog(const aBrowser: ICefBrowser; const aMessageText: ustring; aIsReload: Boolean; const aCallback: ICefJsdialogCallback): Boolean;
		// Called to cancel any pending dialogs and reset any saved dialog state. Will
		// be called due to events like page navigation irregardless of whether any
		// dialogs are currently pending.
		procedure OnResetDialogState(const aBrowser: ICefBrowser);
		// Called when the default implementation dialog is closed.
		procedure OnDialogClosed(const aBrowser: ICefBrowser);
	end;

	//..............................................................................cef_keyboard_handler_capi.h
	// Implement this structure to handle events related to keyboard input. The
	// functions of this structure will be called on the UI thread.
	ICefKeyboardHandler = Interface(ICefBase)
	['{1105FB2C-8E3D-4D30-B3B0-C18FB8490DF3}']
		// Called before a keyboard event is sent to the renderer. |event| contains
		// information about the keyboard event. |os_event| is the operating system
		// event message, if any. Return true (1) if the event was handled or false
		// (0) otherwise. If the event will be handled in on_key_event() as a keyboard
		// shortcut set |is_keyboard_shortcut| to true (1) and return false (0).
		function OnPreKeyEvent(const aBrowser: ICefBrowser; var aEvent: TCefKeyEvent; aOsEvent: TCefEventHandle; out aIsKeyboardShortcut: Boolean): Boolean;
		// Called after the renderer and JavaScript in the page has had a chance to
		// handle the event. |event| contains information about the keyboard event.
		// |os_event| is the operating system event message, if any. Return true (1)
		// if the keyboard event was handled or false (0) otherwise.
		function OnKeyEvent(const aBrowser: ICefBrowser; var aEvent: TCefKeyEvent; aOsEvent: TCefEventHandle): Boolean;
	end;

	//..............................................................................cef_life_span_handler_capi.h
	// Implement this structure to handle events related to browser life span. The
	// functions of this structure will be called on the UI thread unless otherwise
	// indicated.
	ICefLifeSpanHandler = Interface(ICefBase)
	['{485AAD8D-D94A-4956-8C22-16D80B48C939}']
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
		function OnBeforePopup(const aBrowser: ICefBrowser; const aFrame: ICefFrame; var aTargetUrl: ustring; const aTargetFrameName: ustring; aTargetDisposition: TCefWindowOpenDisposition; aUserGesture: Boolean; var aPopupFeatures: TCefPopupFeatures; var aWindowInfo: TCefWindowInfo; var aClient: ICefClient; var aSettings: TCefBrowserSettings; var aNoJavascriptAccess: Boolean): Boolean;
		// Called after a new browser is created.
		procedure OnAfterCreated(const aBrowser: ICefBrowser);
		// Called when a modal window is about to display and the modal loop should
		// begin running. Return false (0) to use the default modal loop
		// implementation or true (1) to use a custom implementation.
		function RunModal(const aBrowser: ICefBrowser): Boolean;
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
		function DoClose(const aBrowser: ICefBrowser): Boolean;
		// Called just before a browser is destroyed. Release all references to the
		// browser object and do not attempt to execute any functions on the browser
		// object after this callback returns. If this is a modal window and a custom
		// modal loop implementation was provided in run_modal() this callback should
		// be used to exit the custom modal loop. See do_close() documentation for
		// additional usage information.
		procedure OnBeforeClose(const aBrowser: ICefBrowser);
	end;


	//..............................................................................cef_load_handler_capi.h
	// Implement this structure to handle events related to browser load status. The
	// functions of this structure will be called on the browser process UI thread
	// or render process main thread (TID_RENDERER).
	ICefLoadHandler = Interface(ICefBase)
	['{B1E63838-C0E1-458D-BDD4-1FD1D7476D01}']
		// Called when the loading state has changed. This callback will be executed
		// twice -- once when loading is initiated either programmatically or by user
		// action, and once when loading is terminated due to completion, cancellation
		// of failure.
		procedure OnLoadingStateChange(const aBrowser: ICefBrowser; aIsLoading: Boolean; aCanGoBack: Boolean; aCanGoForward: Boolean);
		// Called when the browser begins loading a frame. The |frame| value will
		// never be NULL -- call the is_main() function to check if this frame is the
		// main frame. Multiple frames may be loading at the same time. Sub-frames may
		// start or continue loading after the main frame load has ended. This
		// function may not be called for a particular frame if the load request for
		// that frame fails. For notification of overall browser load status use
		// OnLoadingStateChange instead.
		procedure OnLoadStart(const aBrowser: ICefBrowser; const aFrame: ICefFrame);
		// Called when the browser is done loading a frame. The |frame| value will
		// never be NULL -- call the is_main() function to check if this frame is the
		// main frame. Multiple frames may be loading at the same time. Sub-frames may
		// start or continue loading after the main frame load has ended. This
		// function will always be called for all frames irrespective of whether the
		// request completes successfully.
		procedure OnLoadEnd(const aBrowser: ICefBrowser; const aFrame: ICefFrame; aHttpStatusCode: cint);
		// Called when the resource load for a navigation fails or is canceled.
		// |errorCode| is the error code number, |errorText| is the error text and
		// |failedUrl| is the URL that failed to load. See net\base\net_error_list.h
		// for complete descriptions of the error codes.
		procedure OnLoadError(const aBrowser: ICefBrowser; const aFrame: ICefFrame; aErrorCode: TCefErrorcode; const aErrorText: ustring; const aFailedUrl: ustring);
	end;


	//..............................................................................cef_menu_model_capi.h
	// Supports creation and modification of menus. See cef_menu_id_t for the
	// command ids that have default implementations. All user-defined command ids
	// should be between MENU_ID_USER_FIRST and MENU_ID_USER_LAST. The functions of
	// this structure can only be accessed on the browser process the UI thread.
	ICefMenuModel = Interface(ICefBase)
	['{B2BAA818-7C6A-44E6-BF0C-64922FD4A4F9}']
		// Clears the menu. Returns true (1) on success.
		function Clear: Boolean;
		// Returns the number of items in this menu.
		function GetCount: cint;
		//
		// Add a separator to the menu. Returns true (1) on success.
		function AddSeparator: Boolean;
		//
		// Add an item to the menu. Returns true (1) on success.
		function AddItem(aCommandId: cint; const aLabel: ustring): Boolean;
		//
		// Add a check item to the menu. Returns true (1) on success.
		function AddCheckItem(aCommandId: cint; const aLabel: ustring): Boolean;
		//
		// Add a radio item to the menu. Only a single item with the specified
		// |group_id| can be checked at a time. Returns true (1) on success.
		function AddRadioItem(aCommandId: cint; const aLabel: ustring; aGroupId: cint): Boolean;
		//
		// Add a sub-menu to the menu. The new sub-menu is returned.
		function AddSubMenu(aCommandId: cint; const aLabel: ustring): ICefMenuModel;
		//
		// Insert a separator in the menu at the specified |index|. Returns true (1)
		// on success.
		function InsertSeparatorAt(aIndex: cint): Boolean;
		//
		// Insert an item in the menu at the specified |index|. Returns true (1) on
		// success.
		function InsertItemAt(aIndex: cint; aCommandId: cint; const aLabel: ustring): Boolean;
		//
		// Insert a check item in the menu at the specified |index|. Returns true (1)
		// on success.
		function InsertCheckItemAt(aIndex: cint; aCommandId: cint; const aLabel: ustring): Boolean;
		//
		// Insert a radio item in the menu at the specified |index|. Only a single
		// item with the specified |group_id| can be checked at a time. Returns true
		// (1) on success.
		function InsertRadioItemAt(aIndex: cint; aCommandId: cint; const aLabel: ustring; aGroupId: cint): Boolean;
		//
		// Insert a sub-menu in the menu at the specified |index|. The new sub-menu is
		// returned.
		function InsertSubMenuAt(aIndex: cint; aCommandId: cint; const aLabel: ustring): ICefMenuModel;
		// Removes the item with the specified |command_id|. Returns true (1) on
		// success.
		function Remove(aCommandId: cint): Boolean;
		// Removes the item at the specified |index|. Returns true (1) on success.
		function RemoveAt(aIndex: cint): Boolean;
		// Returns the index associated with the specified |command_id| or -1 if not
		// found due to the command id not existing in the menu.
		function GetIndexOf(aCommandId: cint): cint;
		// Returns the command id at the specified |index| or -1 if not found due to
		// invalid range or the index being a separator.
		function GetCommandIdAt(aIndex: cint): cint;
		// Sets the command id at the specified |index|. Returns true (1) on success.
		function SetCommandIdAt(aIndex: cint; aCommandId: cint): Boolean;
		// Returns the label for the specified |command_id| or NULL if not found.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetLabel(aCommandId: cint): ustring;
		// Returns the label at the specified |index| or NULL if not found due to
		// invalid range or the index being a separator.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetLabelAt(aIndex: cint): ustring;
		// Sets the label for the specified |command_id|. Returns true (1) on success.
		function SetLabel(aCommandId: cint; const aLabel: ustring): Boolean;
		// Set the label at the specified |index|. Returns true (1) on success.
		function SetLabelAt(aIndex: cint; const aLabel: ustring): Boolean;
		// Returns the item type for the specified |command_id|.
		function GetType(aCommandId: cint): TCefMenuItemType;
		// Returns the item type at the specified |index|.
		function GetTypeAt(aIndex: cint): TCefMenuItemType;
		// Returns the group id for the specified |command_id| or -1 if invalid.
		function GetGroupId(aCommandId: cint): cint;
		// Returns the group id at the specified |index| or -1 if invalid.
		function GetGroupIdAt(aIndex: cint): cint;
		// Sets the group id for the specified |command_id|. Returns true (1) on
		// success.
		function SetGroupId(aCommandId: cint; aGroupId: cint): Boolean;
		// Sets the group id at the specified |index|. Returns true (1) on success.
		function SetGroupIdAt(aIndex: cint; aGroupId: cint): Boolean;
		// Returns the submenu for the specified |command_id| or NULL if invalid.
		function GetSubMenu(aCommandId: cint): ICefMenuModel;
		// Returns the submenu at the specified |index| or NULL if invalid.
		function GetSubMenuAt(aIndex: cint): ICefMenuModel;
		//
		// Returns true (1) if the specified |command_id| is visible.
		function IsVisible(aCommandId: cint): Boolean;
		//
		// Returns true (1) if the specified |index| is visible.
		function IsVisibleAt(aIndex: cint): Boolean;
		//
		// Change the visibility of the specified |command_id|. Returns true (1) on
		// success.
		function SetVisible(aCommandId: cint; aVisible: Boolean): Boolean;
		//
		// Change the visibility at the specified |index|. Returns true (1) on
		// success.
		function SetVisibleAt(aIndex: cint; aVisible: Boolean): Boolean;
		//
		// Returns true (1) if the specified |command_id| is enabled.
		function IsEnabled(aCommandId: cint): Boolean;
		//
		// Returns true (1) if the specified |index| is enabled.
		function IsEnabledAt(aIndex: cint): Boolean;
		//
		// Change the enabled status of the specified |command_id|. Returns true (1)
		// on success.
		function SetEnabled(aCommandId: cint; aEnabled: Boolean): Boolean;
		//
		// Change the enabled status at the specified |index|. Returns true (1) on
		// success.
		function SetEnabledAt(aIndex: cint; aEnabled: Boolean): Boolean;
		//
		// Returns true (1) if the specified |command_id| is checked. Only applies to
		// check and radio items.
		function IsChecked(aCommandId: cint): Boolean;
		//
		// Returns true (1) if the specified |index| is checked. Only applies to check
		// and radio items.
		function IsCheckedAt(aIndex: cint): Boolean;
		//
		// Check the specified |command_id|. Only applies to check and radio items.
		// Returns true (1) on success.
		function SetChecked(aCommandId: cint; aChecked: Boolean): Boolean;
		//
		// Check the specified |index|. Only applies to check and radio items. Returns
		// true (1) on success.
		function SetCheckedAt(aIndex: cint; aChecked: Boolean): Boolean;
		//
		// Returns true (1) if the specified |command_id| has a keyboard accelerator
		// assigned.
		function HasAccelerator(aCommandId: cint): Boolean;
		//
		// Returns true (1) if the specified |index| has a keyboard accelerator
		// assigned.
		function HasAcceleratorAt(aIndex: cint): Boolean;
		//
		// Set the keyboard accelerator for the specified |command_id|. |key_code| can
		// be any virtual key or character value. Returns true (1) on success.
		function SetAccelerator(aCommandId: cint; aKeyCode: cint; aShiftPressed: Boolean; aCtrlPressed: Boolean; aAltPressed: Boolean): Boolean;
		//
		// Set the keyboard accelerator at the specified |index|. |key_code| can be
		// any virtual key or character value. Returns true (1) on success.
		function SetAcceleratorAt(aIndex: cint; aKeyCode: cint; aShiftPressed: Boolean; aCtrlPressed: Boolean; aAltPressed: Boolean): Boolean;
		//
		// Remove the keyboard accelerator for the specified |command_id|. Returns
		// true (1) on success.
		function RemoveAccelerator(aCommandId: cint): Boolean;
		//
		// Remove the keyboard accelerator at the specified |index|. Returns true (1)
		// on success.
		function RemoveAcceleratorAt(aIndex: cint): Boolean;
		//
		// Retrieves the keyboard accelerator for the specified |command_id|. Returns
		// true (1) on success.
		function GetAccelerator(aCommandId: cint; out aKeyCode: cint; out aShiftPressed: Boolean; out aCtrlPressed: Boolean; out aAltPressed: Boolean): Boolean;
		//
		// Retrieves the keyboard accelerator for the specified |index|. Returns true
		// (1) on success.
		function GetAcceleratorAt(aIndex: cint; out aKeyCode: cint; out aShiftPressed: Boolean; out aCtrlPressed: Boolean; out aAltPressed: Boolean): Boolean;

		property Count: cint read GetCount;
		property IndexOf[aCommandId: cint]: cint read GetIndexOf;
		property SubMenu[aCommandId: cint]: ICefMenuModel read GetSubMenu;
		property SubMenuAt[aIndex: cint]: ICefMenuModel read GetSubMenuAt;
		property _Type[aCommandId: cint]: TCefMenuItemType read GetType;
		property TypeAt[aIndex: cint]: TCefMenuItemType read GetTypeAt;
	end;

	//..............................................................................cef_navigation_entry_capi.h
	// Structure used to represent an entry in navigation history.
	ICefNavigationEntry = Interface(ICefBase)
	['{F0ABFFC4-E177-4CF2-A9B5-4ADD05A93632}']
		// Returns true (1) if this object is valid. Do not call any other functions
		// if this function returns false (0).
		function IsValid: Boolean;
		// Returns the actual URL of the page. For some pages this may be data: URL or
		// similar. Use get_display_url() to return a display-friendly version.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetUrl: ustring;
		// Returns a display-friendly version of the URL.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetDisplayUrl: ustring;
		// Returns the original URL that was entered by the user before any redirects.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetOriginalUrl: ustring;
		// Returns the title set by the page. This value may be NULL.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetTitle: ustring;
		// Returns the transition type which indicates what the user did to move to
		// this page from the previous page.
		function GetTransitionType: TCefTransitionType;
		// Returns true (1) if this navigation includes post data.
		function HasPostData: Boolean;
		// Returns the name of the sub-frame that navigated or an NULL value if the
		// main frame navigated.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetFrameName: ustring;
		// Returns the time for the last known successful navigation completion. A
		// navigation may be completed more than once if the page is reloaded. May be
		// 0 if the navigation has not yet completed.
		function GetCompletionTime: TCefTime;
		// Returns the HTTP status code for the last known successful navigation
		// response. May be 0 if the response has not yet been received or if the
		// navigation has not yet completed.
		function GetHttpStatusCode: cint;

		property CompletionTime: TCefTime read GetCompletionTime;
		property DisplayUrl: ustring read GetDisplayUrl;
		property FrameName: ustring read GetFrameName;
		property HttpStatusCode: cint read GetHttpStatusCode;
		property OriginalUrl: ustring read GetOriginalUrl;
		property Title: ustring read GetTitle;
		property TransitionType: TCefTransitionType read GetTransitionType;
		property Url: ustring read GetUrl;
	end;


	//..............................................................................cef_origin_whitelist_capi.h

	//..............................................................................cef_parser_capi.h

	//..............................................................................cef_path_util_capi.h

	//..............................................................................cef_print_handler_capi.h
	// Callback structure for asynchronous continuation of print dialog requests.
	ICefPrintDialogCallback = Interface(ICefBase)
	['{3BBCF9F9-15AA-4EF4-9640-B4F8E6A13CF3}']
		// Continue printing with the specified |settings|.
		procedure Cont(const aSettings: ICefPrintSettings);
		// Cancel the printing.
		procedure Cancel;
	end;

	// Callback structure for asynchronous continuation of print dialog requests.
	// Callback structure for asynchronous continuation of print job requests.
	ICefPrintJobCallback = Interface(ICefBase)
	['{E0A4BF0D-298C-42C9-905F-00FC753A7637}']
		// Indicate completion of the print job.
		procedure Cont;
	end;

	// Callback structure for asynchronous continuation of print dialog requests.
	// Callback structure for asynchronous continuation of print job requests.
	// Implement this structure to handle printing on Linux. The functions of this
	// structure will be called on the browser process UI thread.
	ICefPrintHandler = Interface(ICefBase)
	['{CFD01FEA-B691-4357-A1A8-5C38DC577CB2}']
		// Synchronize |settings| with client state. If |get_defaults| is true (1)
		// then populate |settings| with the default print settings. Do not keep a
		// reference to |settings| outside of this callback.
		procedure OnPrintSettings(const aSettings: ICefPrintSettings; aGetDefaults: Boolean);
		// Show the print dialog. Execute |callback| once the dialog is dismissed.
		// Return true (1) if the dialog will be displayed or false (0) to cancel the
		// printing immediately.
		function OnPrintDialog(aHasSelection: Boolean; const aCallback: ICefPrintDialogCallback): Boolean;
		// Send the print job to the printer. Execute |callback| once the job is
		// completed. Return true (1) if the job will proceed or false (0) to cancel
		// the job immediately.
		function OnPrintJob(const aDocumentName: ustring; const aPdfFilePath: ustring; const aCallback: ICefPrintJobCallback): Boolean;
		// Reset client state related to printing.
		procedure OnPrintReset;
	end;


	//..............................................................................cef_print_settings_capi.h
	// Structure representing print settings.
	ICefPrintSettings = Interface(ICefBase)
	['{B6704F11-98F3-4210-9BDE-816EE4BABC80}']
		// Returns true (1) if this object is valid. Do not call any other functions
		// if this function returns false (0).
		function IsValid: Boolean;
		// Returns true (1) if the values of this object are read-only. Some APIs may
		// expose read-only objects.
		function IsReadOnly: Boolean;
		// Returns a writable copy of this object.
		function Copy: ICefPrintSettings;
		// Set the page orientation.
		procedure SetOrientation(aLandscape: Boolean);
		// Returns true (1) if the orientation is landscape.
		function IsLandscape: Boolean;
		// Set the printer printable area in device units. Some platforms already
		// provide flipped area. Set |landscape_needs_flip| to false (0) on those
		// platforms to avoid double flipping.
		procedure SetPrinterPrintableArea(const aPhysicalSizeDeviceUnits: TCefSize; var aPrintableAreaDeviceUnits: TCefRect; var aLandscapeNeedsFlip: Boolean);
		// Set the device name.
		procedure SetDeviceName(const aName: ustring);
		// Get the device name.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetDeviceName: ustring;
		// Set the DPI (dots per inch).
		procedure SetDpi(aDpi: cint);
		// Get the DPI (dots per inch).
		function GetDpi: cint;
		// Set the page ranges.
		procedure SetPageRanges(aRangesCount: csize_t; aRanges: TCefPageRangeArray);
		// Returns the number of page ranges that currently exist.
		function GetPageRangesCount: csize_t;
		// Retrieve the page ranges.
		procedure GetPageRanges(var aRangesCount: csize_t; const aRanges: TCefPageRangeArray);
		// Set whether only the selection will be printed.
		procedure SetSelectionOnly(aSelectionOnly: Boolean);
		// Returns true (1) if only the selection will be printed.
		function IsSelectionOnly: Boolean;
		// Set whether pages will be collated.
		procedure SetCollate(aCollate: Boolean);
		// Returns true (1) if pages will be collated.
		function WillCollate: Boolean;
		// Set the color model.
		procedure SetColorModel(aModel: TCefColorModel);
		// Get the color model.
		function GetColorModel: TCefColorModel;
		// Set the number of copies.
		procedure SetCopies(aCopies: cint);
		// Get the number of copies.
		function GetCopies: cint;
		// Set the duplex mode.
		procedure SetDuplexMode(aMode: TCefDuplexMode);
		// Get the duplex mode.
		function GetDuplexMode: TCefDuplexMode;

		property ColorModel: TCefColorModel read GetColorModel write SetColorModel;
		property Copies: cint read GetCopies write SetCopies;
		property DeviceName: ustring read GetDeviceName write SetDeviceName;
		property Dpi: cint read GetDpi write SetDpi;
		property DuplexMode: TCefDuplexMode read GetDuplexMode write SetDuplexMode;
		property PageRangesCount: csize_t read GetPageRangesCount;
	end;


	//..............................................................................cef_process_message_capi.h
	// Structure representing a message. Can be used on any process and thread.
	ICefProcessMessage = Interface(ICefBase)
	['{B2B25AA6-5B15-46DC-8AC0-1D8DADC20F49}']
		// Returns true (1) if this object is valid. Do not call any other functions
		// if this function returns false (0).
		function IsValid: Boolean;
		// Returns true (1) if the values of this object are read-only. Some APIs may
		// expose read-only objects.
		function IsReadOnly: Boolean;
		// Returns a writable copy of this object.
		function Copy: ICefProcessMessage;
		// Returns the message name.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetName: ustring;
		// Returns the list of arguments.
		function GetArgumentList: ICefListValue;

		property ArgumentList: ICefListValue read GetArgumentList;
		property Name: ustring read GetName;
	end;


	//..............................................................................cef_process_util_capi.h

	//..............................................................................cef_render_handler_capi.h
	// Implement this structure to handle events when window rendering is disabled.
	// The functions of this structure will be called on the UI thread.
	ICefRenderHandler = Interface(ICefBase)
	['{02377136-A9BD-48A1-B282-04EECD76A9C3}']
		// Called to retrieve the root window rectangle in screen coordinates. Return
		// true (1) if the rectangle was provided.
		function GetRootScreenRect(const aBrowser: ICefBrowser; var aRect: TCefRect): Boolean;
		// Called to retrieve the view rectangle which is relative to screen
		// coordinates. Return true (1) if the rectangle was provided.
		function GetViewRect(const aBrowser: ICefBrowser; var aRect: TCefRect): Boolean;
		// Called to retrieve the translation from view coordinates to actual screen
		// coordinates. Return true (1) if the screen coordinates were provided.
		function GetScreenPoint(const aBrowser: ICefBrowser; aViewX: cint; aViewY: cint; var aScreenX: cint; var aScreenY: cint): Boolean;
		// Called to allow the client to fill in the CefScreenInfo object with
		// appropriate values. Return true (1) if the |screen_info| structure has been
		// modified.
		//
		// If the screen info rectangle is left NULL the rectangle from GetViewRect
		// will be used. If the rectangle is still NULL or invalid popups may not be
		// drawn correctly.
		function GetScreenInfo(const aBrowser: ICefBrowser; out aScreenInfo: TCefScreenInfo): Boolean;
		// Called when the browser wants to show or hide the popup widget. The popup
		// should be shown if |show| is true (1) and hidden if |show| is false (0).
		procedure OnPopupShow(const aBrowser: ICefBrowser; aShow: Boolean);
		// Called when the browser wants to move or resize the popup widget. |rect|
		// contains the new location and size in view coordinates.
		procedure OnPopupSize(const aBrowser: ICefBrowser; var aRect: TCefRect);
		// Called when an element should be painted. Pixel values passed to this
		// function are scaled relative to view coordinates based on the value of
		// CefScreenInfo.device_scale_factor returned from GetScreenInfo. |type|
		// indicates whether the element is the view or the popup widget. |buffer|
		// contains the pixel data for the whole image. |dirtyRects| contains the set
		// of rectangles in pixel coordinates that need to be repainted. |buffer| will
		// be |width|*|height|*4 bytes in size and represents a BGRA image with an
		// upper-left origin.
		procedure OnPaint(const aBrowser: ICefBrowser; aType: TCefPaintElementType; aDirtyRectsCount: csize_t; const aDirtyRects: TCefRectArray; const aBuffer: cvoid; aWidth: cint; aHeight: cint);
		// Called when the browser's cursor has changed. If |type| is CT_CUSTOM then
		// |custom_cursor_info| will be populated with the custom cursor information.
		procedure OnCursorChange(const aBrowser: ICefBrowser; aCursor: TCefCursorHandle; aType: TCefCursorType; aCustomCursorInfo: TCefCursorInfo);
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
		function StartDragging(const aBrowser: ICefBrowser; const aDragData: ICefDragData; aAllowedOps: TCefDragOperationsMask; aX: cint; aY: cint): Boolean;
		// Called when the web view wants to update the mouse cursor during a drag &
		// drop operation. |operation| describes the allowed operation (none, move,
		// copy, link).
		procedure UpdateDragCursor(const aBrowser: ICefBrowser; aOperation: TCefDragOperationsMask);
		// Called when the scroll offset has changed.
		procedure OnScrollOffsetChanged(const aBrowser: ICefBrowser; aX: cdouble; aY: cdouble);
	end;

	//..............................................................................cef_render_process_handler_capi.h
	// Structure used to implement render process callbacks. The functions of this
	// structure will be called on the render process main thread (TID_RENDERER)
	// unless otherwise indicated.
	ICefRenderProcessHandler = Interface(ICefBase)
	['{93735B02-D709-4835-9434-DE9E13046921}']
		// Called after the render process main thread has been created. |extra_info|
		// is a read-only value originating from
		// cef_browser_process_handler_t::on_render_process_thread_created(). Do not
		// keep a reference to |extra_info| outside of this function.
		procedure OnRenderThreadCreated(const aExtraInfo: ICefListValue);
		// Called after WebKit has been initialized.
		procedure OnWebKitInitialized;
		// Called after a browser has been created. When browsing cross-origin a new
		// browser will be created before the old browser with the same identifier is
		// destroyed.
		procedure OnBrowserCreated(const aBrowser: ICefBrowser);
		// Called before a browser is destroyed.
		procedure OnBrowserDestroyed(const aBrowser: ICefBrowser);
		// Return the handler for browser load status events.
		function GetLoadHandler: ICefLoadHandler;
		// Called before browser navigation. Return true (1) to cancel the navigation
		// or false (0) to allow the navigation to proceed. The |request| object
		// cannot be modified in this callback.
		function OnBeforeNavigation(const aBrowser: ICefBrowser; const aFrame: ICefFrame; const aRequest: ICefRequest; aNavigationType: TCefNavigationType; aIsRedirect: Boolean): Boolean;
		// Called immediately after the V8 context for a frame has been created. To
		// retrieve the JavaScript 'window' object use the
		// cef_v8context_t::get_global() function. V8 handles can only be accessed
		// from the thread on which they are created. A task runner for posting tasks
		// on the associated thread can be retrieved via the
		// cef_v8context_t::get_task_runner() function.
		procedure OnContextCreated(const aBrowser: ICefBrowser; const aFrame: ICefFrame; const aContext: ICefV8context);
		// Called immediately before the V8 context for a frame is released. No
		// references to the context should be kept after this function is called.
		procedure OnContextReleased(const aBrowser: ICefBrowser; const aFrame: ICefFrame; const aContext: ICefV8context);
		// Called for global uncaught exceptions in a frame. Execution of this
		// callback is disabled by default. To enable set
		// CefSettings.uncaught_exception_stack_size > 0.
		procedure OnUncaughtException(const aBrowser: ICefBrowser; const aFrame: ICefFrame; const aContext: ICefV8context; const aException: ICefV8exception; const aStackTrace: ICefV8stackTrace);
		// Called when a new node in the the browser gets focus. The |node| value may
		// be NULL if no specific node has gained focus. The node object passed to
		// this function represents a snapshot of the DOM at the time this function is
		// executed. DOM objects are only valid for the scope of this function. Do not
		// keep references to or attempt to access any DOM objects outside the scope
		// of this function.
		procedure OnFocusedNodeChanged(const aBrowser: ICefBrowser; const aFrame: ICefFrame; const aNode: ICefDomnode);
		// Called when a new message is received from a different process. Return true
		// (1) if the message was handled or false (0) otherwise. Do not keep a
		// reference to or attempt to access the message outside of this callback.
		function OnProcessMessageReceived(const aBrowser: ICefBrowser; aSourceProcess: TCefProcessId; const aMessage: ICefProcessMessage): Boolean;

		property LoadHandler: ICefLoadHandler read GetLoadHandler;
	end;

	//..............................................................................cef_request_capi.h
	// Structure used to represent a web request. The functions of this structure
	// may be called on any thread.
	ICefRequest = Interface(ICefBase)
	['{014D9B84-100D-4D22-A422-3703251D2052}']
		// Returns true (1) if this object is read-only.
		function IsReadOnly: Boolean;
		// Get the fully qualified URL.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetUrl: ustring;
		// Set the fully qualified URL.
		procedure SetUrl(const aUrl: ustring);
		// Get the request function type. The value will default to POST if post data
		// is provided and GET otherwise.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetMethod: ustring;
		// Set the request function type.
		procedure SetMethod(const aMethod: ustring);
		// Get the post data.
		function GetPostData: ICefPostData;
		// Set the post data.
		procedure SetPostData(const aPostData: ICefPostData);
		// Get the header values.
		procedure GetHeaderMap(aHeaderMap: ICefStringMultimap);
		// Set the header values.
		procedure SetHeaderMap(aHeaderMap: ICefStringMultimap);
		// Set all values at one time.
		procedure _Set(const aUrl: ustring; const aMethod: ustring; const aPostData: ICefPostData; aHeaderMap: ICefStringMultimap);
		// Get the flags used in combination with cef_urlrequest_t. See
		// cef_urlrequest_flags_t for supported values.
		function GetFlags: TCefURLRequestFlags;
		// Set the flags used in combination with cef_urlrequest_t.  See
		// cef_urlrequest_flags_t for supported values.
		procedure SetFlags(aFlags: TCefURLRequestFlags);
		// Set the URL to the first party for cookies used in combination with
		// cef_urlrequest_t.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetFirstPartyForCookies: ustring;
		// Get the URL to the first party for cookies used in combination with
		// cef_urlrequest_t.
		procedure SetFirstPartyForCookies(const aUrl: ustring);
		// Get the resource type for this request. Only available in the browser
		// process.
		function GetResourceType: TCefResourceType;
		// Get the transition type for this request. Only available in the browser
		// process and only applies to requests that represent a main frame or sub-
		// frame navigation.
		function GetTransitionType: TCefTransitionType;
		// Returns the globally unique identifier for this request or 0 if not
		// specified. Can be used by cef_request_tHandler implementations in the
		// browser process to track a single request across multiple callbacks.
		function GetIdentifier: cuint64;

		property FirstPartyForCookies: ustring read GetFirstPartyForCookies write SetFirstPartyForCookies;
		property Flags: TCefURLRequestFlags read GetFlags write SetFlags;
		property Identifier: cuint64 read GetIdentifier;
		property Method: ustring read GetMethod write SetMethod;
		property PostData: ICefPostData read GetPostData write SetPostData;
		property ResourceType: TCefResourceType read GetResourceType;
		property TransitionType: TCefTransitionType read GetTransitionType;
		property Url: ustring read GetUrl write SetUrl;
	end;

	// Structure used to represent post data for a web request. The functions of
	// this structure may be called on any thread.
	ICefPostData = Interface(ICefBase)
	['{4C278AEF-4076-47C9-A870-5133A4326422}']
		// Returns true (1) if this object is read-only.
		function IsReadOnly: Boolean;
		// Returns the number of existing post data elements.
		function GetElementCount: csize_t;
		// Retrieve the post data elements.
		function GetElements(var aElementsCount: csize_t): IInterfaceList;
		// Remove the specified post data element.  Returns true (1) if the removal
		// succeeds.
		function RemoveElement(const aElement: ICefPostDataElement): Boolean;
		// Add the specified post data element.  Returns true (1) if the add succeeds.
		function AddElement(const aElement: ICefPostDataElement): Boolean;
		// Remove all existing post data elements.
		procedure RemoveElements;

		property ElementCount: csize_t read GetElementCount;
	end;

	// Structure used to represent a single element in the request post data. The
	// functions of this structure may be called on any thread.
	ICefPostDataElement = Interface(ICefBase)
	['{EAA8BEE1-ABB1-4808-82D4-600F71ECEFD1}']
		// Returns true (1) if this object is read-only.
		function IsReadOnly: Boolean;
		// Remove all contents from the post data element.
		procedure SetToEmpty;
		// The post data element will represent a file.
		procedure SetToFile(const aFileName: ustring);
		// The post data element will represent bytes.  The bytes passed in will be
		// copied.
		procedure SetToBytes(aSize: csize_t; const aBytes: cvoid);
		// Return the type of this post data element.
		function GetType: TCefPostdataelementType;
		// Return the file name.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetFile: ustring;
		// Return the number of bytes.
		function GetBytesCount: csize_t;
		// Read up to |size| bytes into |bytes| and return the number of bytes
		// actually read.
		function GetBytes(aSize: csize_t; aBytes: cvoid): csize_t;

		property BytesCount: csize_t read GetBytesCount;
		property _File: ustring read GetFile;
		property _Type: TCefPostdataelementType read GetType;
	end;

	//..............................................................................cef_request_context_capi.h
	// A request context provides request handling for a set of related browser or
	// URL request objects. A request context can be specified when creating a new
	// browser via the cef_browser_host_t static factory functions or when creating
	// a new URL request via the cef_urlrequest_t static factory functions. Browser
	// objects with different request contexts will never be hosted in the same
	// render process. Browser objects with the same request context may or may not
	// be hosted in the same render process depending on the process model. Browser
	// objects created indirectly via the JavaScript window.open function or
	// targeted links will share the same render process and the same request
	// context as the source browser. When running in single-process mode there is
	// only a single render process (the main process) and so all browsers created
	// in single-process mode will share the same request context. This will be the
	// first request context passed into a cef_browser_host_t static factory
	// function and all other request context objects will be ignored.
	ICefRequestContext = Interface(ICefBase)
	['{8A0F8527-82F6-4871-973A-20B51DD6A382}']
		// Returns true (1) if this object is pointing to the same context as |that|
		// object.
		function IsSame(const aOther: ICefRequestContext): Boolean;
		// Returns true (1) if this object is sharing the same storage as |that|
		// object.
		function IsSharingWith(const aOther: ICefRequestContext): Boolean;
		// Returns true (1) if this object is the global context. The global context
		// is used by default when creating a browser or URL request with a NULL
		// context argument.
		function IsGlobal: Boolean;
		// Returns the handler for this context if any.
		function GetHandler: ICefRequestContextHandler;
		// Returns the cache path for this object. If NULL an "incognito mode" in-
		// memory cache is being used.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetCachePath: ustring;
		// Returns the default cookie manager for this object. This will be the global
		// cookie manager if this object is the global request context. Otherwise,
		// this will be the default cookie manager used when this request context does
		// not receive a value via cef_request_tContextHandler::get_cookie_manager().
		// If |callback| is non-NULL it will be executed asnychronously on the IO
		// thread after the manager's storage has been initialized.
		function GetDefaultCookieManager(const aCallback: ICefCompletionCallback): ICefCookieManager;
		// Register a scheme handler factory for the specified |scheme_name| and
		// optional |domain_name|. An NULL |domain_name| value for a standard scheme
		// will cause the factory to match all domain names. The |domain_name| value
		// will be ignored for non-standard schemes. If |scheme_name| is a built-in
		// scheme and no handler is returned by |factory| then the built-in scheme
		// handler factory will be called. If |scheme_name| is a custom scheme then
		// you must also implement the cef_app_t::on_register_custom_schemes()
		// function in all processes. This function may be called multiple times to
		// change or remove the factory that matches the specified |scheme_name| and
		// optional |domain_name|. Returns false (0) if an error occurs. This function
		// may be called on any thread in the browser process.
		function RegisterSchemeHandlerFactory(const aSchemeName: ustring; const aDomainName: ustring; const aFactory: ICefSchemeHandlerFactory): Boolean;
		// Clear all registered scheme handler factories. Returns false (0) on error.
		// This function may be called on any thread in the browser process.
		function ClearSchemeHandlerFactories: Boolean;

		property CachePath: ustring read GetCachePath;
		property DefaultCookieManager[const aCallback: ICefCompletionCallback]: ICefCookieManager read GetDefaultCookieManager;
		property Handler: ICefRequestContextHandler read GetHandler;
	end;


	//..............................................................................cef_request_context_handler_capi.h
	// Implement this structure to provide handler implementations. The handler
	// instance will not be released until all objects related to the context have
	// been destroyed.
	ICefRequestContextHandler = Interface(ICefBase)
	['{03049079-9C05-4D24-A121-A9ABCC64E31A}']
		// Called on the IO thread to retrieve the cookie manager. If this function
		// returns NULL the default cookie manager retrievable via
		// cef_request_tContext::get_default_cookie_manager() will be used.
		function GetCookieManager: ICefCookieManager;

		property CookieManager: ICefCookieManager read GetCookieManager;
	end;


	//..............................................................................cef_request_handler_capi.h
	// Callback structure used for asynchronous continuation of url requests.
	ICefRequestCallback = Interface(ICefBase)
	['{5289D599-8176-4D9C-8BBB-BFA393936FDB}']
		// Continue the url request. If |allow| is true (1) the request will be
		// continued. Otherwise, the request will be canceled.
		procedure Cont(aAllow: Boolean);
		// Cancel the url request.
		procedure Cancel;
	end;

	// Callback structure used for asynchronous continuation of url requests.
	// Implement this structure to handle events related to browser requests. The
	// functions of this structure will be called on the thread indicated.
	ICefRequestHandler = Interface(ICefBase)
	['{D093EBE1-2C8F-43BE-BC8A-765CA929186D}']
		// Called on the UI thread before browser navigation. Return true (1) to
		// cancel the navigation or false (0) to allow the navigation to proceed. The
		// |request| object cannot be modified in this callback.
		// cef_load_handler_t::OnLoadingStateChange will be called twice in all cases.
		// If the navigation is allowed cef_load_handler_t::OnLoadStart and
		// cef_load_handler_t::OnLoadEnd will be called. If the navigation is canceled
		// cef_load_handler_t::OnLoadError will be called with an |errorCode| value of
		// ERR_ABORTED.
		function OnBeforeBrowse(const aBrowser: ICefBrowser; const aFrame: ICefFrame; const aRequest: ICefRequest; aIsRedirect: Boolean): Boolean;
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
		function OnOpenUrlfromTab(const aBrowser: ICefBrowser; const aFrame: ICefFrame; const aTargetUrl: ustring; aTargetDisposition: TCefWindowOpenDisposition; aUserGesture: Boolean): Boolean;
		// Called on the IO thread before a resource request is loaded. The |request|
		// object may be modified. Return RV_CONTINUE to continue the request
		// immediately. Return RV_CONTINUE_ASYNC and call cef_request_tCallback::
		// cont() at a later time to continue or cancel the request asynchronously.
		// Return RV_CANCEL to cancel the request immediately.
		//
		function OnBeforeResourceLoad(const aBrowser: ICefBrowser; const aFrame: ICefFrame; const aRequest: ICefRequest; const aCallback: ICefRequestCallback): TCefReturnValue;
		// Called on the IO thread before a resource is loaded. To allow the resource
		// to load normally return NULL. To specify a handler for the resource return
		// a cef_resource_handler_t object. The |request| object should not be
		// modified in this callback.
		function GetResourceHandler(const aBrowser: ICefBrowser; const aFrame: ICefFrame; const aRequest: ICefRequest): ICefResourceHandler;
		// Called on the IO thread when a resource load is redirected. The |request|
		// parameter will contain the old URL and other request-related information.
		// The |new_url| parameter will contain the new URL and can be changed if
		// desired. The |request| object cannot be modified in this callback.
		procedure OnResourceRedirect(const aBrowser: ICefBrowser; const aFrame: ICefFrame; const aRequest: ICefRequest; var aNewUrl: ustring);
		// Called on the IO thread when a resource response is received. To allow the
		// resource to load normally return false (0). To redirect or retry the
		// resource modify |request| (url, headers or post body) and return true (1).
		// The |response| object cannot be modified in this callback.
		function OnResourceResponse(const aBrowser: ICefBrowser; const aFrame: ICefFrame; const aRequest: ICefRequest; const aResponse: ICefResponse): Boolean;
		// Called on the IO thread when the browser needs credentials from the user.
		// |isProxy| indicates whether the host is a proxy server. |host| contains the
		// hostname and |port| contains the port number. Return true (1) to continue
		// the request and call cef_auth_callback_t::cont() either in this function or
		// at a later time when the authentication information is available. Return
		// false (0) to cancel the request immediately.
		function GetAuthCredentials(const aBrowser: ICefBrowser; const aFrame: ICefFrame; aIsProxy: Boolean; const aHost: ustring; aPort: cint; const aRealm: ustring; const aScheme: ustring; const aCallback: ICefAuthCallback): Boolean;
		// Called on the IO thread when JavaScript requests a specific storage quota
		// size via the webkitStorageInfo.requestQuota function. |origin_url| is the
		// origin of the page making the request. |new_size| is the requested quota
		// size in bytes. Return true (1) to continue the request and call
		// cef_request_tCallback::cont() either in this function or at a later time to
		// grant or deny the request. Return false (0) to cancel the request
		// immediately.
		function OnQuotaRequest(const aBrowser: ICefBrowser; const aOriginUrl: ustring; aNewSize: cint64; const aCallback: ICefRequestCallback): Boolean;
		// Called on the UI thread to handle requests for URLs with an unknown
		// protocol component. Set |allow_os_execution| to true (1) to attempt
		// execution via the registered OS protocol handler, if any. SECURITY WARNING:
		// YOU SHOULD USE THIS METHOD TO ENFORCE RESTRICTIONS BASED ON SCHEME, HOST OR
		// OTHER URL ANALYSIS BEFORE ALLOWING OS EXECUTION.
		procedure OnProtocolExecution(const aBrowser: ICefBrowser; const aUrl: ustring; out aAllowOsExecution: Boolean);
		// Called on the UI thread to handle requests for URLs with an invalid SSL
		// certificate. Return true (1) and call cef_request_tCallback::cont() either
		// in this function or at a later time to continue or cancel the request.
		// Return false (0) to cancel the request immediately. If |callback| is NULL
		// the error cannot be recovered from and the request will be canceled
		// automatically. If CefSettings.ignore_certificate_errors is set all invalid
		// certificates will be accepted without calling this function.
		function OnCertificateError(const aBrowser: ICefBrowser; aCertError: TCefErrorcode; const aRequestUrl: ustring; const aSslInfo: ICefSslinfo; const aCallback: ICefRequestCallback): Boolean;
		// Called on the browser process IO thread before a plugin is loaded. Return
		// true (1) to block loading of the plugin.
		function OnBeforePluginLoad(const aBrowser: ICefBrowser; const aUrl: ustring; const aPolicyUrl: ustring; const aInfo: ICefWebPluginInfo): Boolean;
		// Called on the browser process UI thread when a plugin has crashed.
		// |plugin_path| is the path of the plugin that crashed.
		procedure OnPluginCrashed(const aBrowser: ICefBrowser; const aPluginPath: ustring);
		// Called on the browser process UI thread when the render view associated
		// with |browser| is ready to receive/handle IPC messages in the render
		// process.
		procedure OnRenderViewReady(const aBrowser: ICefBrowser);
		// Called on the browser process UI thread when the render process terminates
		// unexpectedly. |status| indicates how the process terminated.
		procedure OnRenderProcessTerminated(const aBrowser: ICefBrowser; aStatus: TCefTerminationStatus);

		property AuthCredentials[const aBrowser: ICefBrowser; const aFrame: ICefFrame; aIsProxy: Boolean; const aHost: ustring; aPort: cint; const aRealm: ustring; const aScheme: ustring; const aCallback: ICefAuthCallback]: Boolean read GetAuthCredentials;
		property ResourceHandler[const aBrowser: ICefBrowser; const aFrame: ICefFrame; const aRequest: ICefRequest]: ICefResourceHandler read GetResourceHandler;
	end;

  //..............................................................................cef_resource_bundle_handler_capi.h
	// Structure used to implement a custom resource bundle structure. The functions
	// of this structure may be called on multiple threads.
	ICefResourceBundleHandler = Interface(ICefBase)
	['{B3F4E125-29B8-4EFB-B84C-D4E7656E8AA3}']
		// Called to retrieve a localized translation for the string specified by
		// |message_id|. To provide the translation set |string| to the translation
		// string and return true (1). To use the default translation return false
		// (0). Supported message IDs are listed in cef_pack_strings.h.
		function GetLocalizedString(aMessageId: cint; out aString: ustring): Boolean;
		// Called to retrieve data for the resource specified by |resource_id|. To
		// provide the resource data set |data| and |data_size| to the data pointer
		// and size respectively and return true (1). To use the default resource data
		// return false (0). The resource data will not be copied and must remain
		// resident in memory. Supported resource IDs are listed in
		// cef_pack_resources.h.
		function GetDataResource(aResourceId: cint; out aData: cvoid; out aDataSize: csize_t): Boolean;
	end;

	//..............................................................................cef_resource_handler_capi.h
	// Structure used to implement a custom request handler structure. The functions
	// of this structure will always be called on the IO thread.
	ICefResourceHandler = Interface(ICefBase)
	['{CF7F8630-4A19-4B88-90EF-3FB876521592}']
		// Begin processing the request. To handle the request return true (1) and
		// call cef_callback_t::cont() once the response header information is
		// available (cef_callback_t::cont() can also be called from inside this
		// function if header information is available immediately). To cancel the
		// request return false (0).
		function ProcessRequest(const aRequest: ICefRequest; const aCallback: ICefCallback): Boolean;
		// Retrieve response header information. If the response length is not known
		// set |response_length| to -1 and read_response() will be called until it
		// returns false (0). If the response length is known set |response_length| to
		// a positive value and read_response() will be called until it returns false
		// (0) or the specified number of bytes have been read. Use the |response|
		// object to set the mime type, http status code and other optional header
		// values. To redirect the request to a new URL set |redirectUrl| to the new
		// URL.
		procedure GetResponseHeaders(const aResponse: ICefResponse; var aResponseLength: cint64; var aRedirectUrl: ustring);
		// Read response data. If data is available immediately copy up to
		// |bytes_to_read| bytes into |data_out|, set |bytes_read| to the number of
		// bytes copied, and return true (1). To read the data at a later time set
		// |bytes_read| to 0, return true (1) and call cef_callback_t::cont() when the
		// data is available. To indicate response completion return false (0).
		function ReadResponse(var aDataOut: cvoid; aBytesToRead: cint; var aBytesRead: cint; const aCallback: ICefCallback): Boolean;
		// Return true (1) if the specified cookie can be sent with the request or
		// false (0) otherwise. If false (0) is returned for any cookie then no
		// cookies will be sent with the request.
		function CanGetCookie(const aCookie: TWACefCookie): Boolean;
		// Return true (1) if the specified cookie returned with the response can be
		// set or false (0) otherwise.
		function CanSetCookie(const aCookie: TWACefCookie): Boolean;
		// Request processing has been canceled.
		procedure Cancel;
	end;


	//..............................................................................cef_response_capi.h
	// Structure used to represent a web response. The functions of this structure
	// may be called on any thread.
	ICefResponse = Interface(ICefBase)
	['{BFB54F7A-D377-4B9E-A749-247D4B7A4C30}']
		// Returns true (1) if this object is read-only.
		function IsReadOnly: Boolean;
		// Get the response status code.
		function GetStatus: cint;
		// Set the response status code.
		procedure SetStatus(aStatus: cint);
		// Get the response status text.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetStatusText: ustring;
		// Set the response status text.
		procedure SetStatusText(const aStatusText: ustring);
		// Get the response mime type.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetMimeType: ustring;
		// Set the response mime type.
		procedure SetMimeType(const aMimeType: ustring);
		// Get the value for the specified response header field.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetHeader(const aName: ustring): ustring;
		// Get all response header fields.
		procedure GetHeaderMap(const aHeaderMap: ICefStringMultimap);
		// Set all response header fields.
		procedure SetHeaderMap(const aHeaderMap: ICefStringMultimap);

		property Header[const aName: ustring]: ustring read GetHeader;
		property MimeType: ustring read GetMimeType write SetMimeType;
		property Status: cint read GetStatus write SetStatus;
		property StatusText: ustring read GetStatusText write SetStatusText;
	end;


	//..............................................................................cef_scheme_capi.h
	// Structure that manages custom scheme registrations.
	ICefSchemeRegistrar = Interface(ICefBase)
	['{6242BA91-EA86-49AD-8821-5F81D919D2DD}']
		// Register a custom scheme. This function should not be called for the built-
		// in HTTP, HTTPS, FILE, FTP, ABOUT and DATA schemes.
		//
		// If |is_standard| is true (1) the scheme will be treated as a standard
		// scheme. Standard schemes are subject to URL canonicalization and parsing
		// rules as defined in the Common Internet Scheme Syntax RFC 1738 Section 3.1
		// available at http://www.ietf.org/rfc/rfc1738.txt
		//
		// In particular, the syntax for standard scheme URLs must be of the form:
		// <pre>
		//  [scheme]://[username]:[password]@[host]:[port]/[url-path]
		// </pre> Standard scheme URLs must have a host component that is a fully
		// qualified domain name as defined in Section 3.5 of RFC 1034 [13] and
		// Section 2.1 of RFC 1123. These URLs will be canonicalized to
		// "scheme://host/path" in the simplest case and
		// "scheme://username:password@host:port/path" in the most explicit case. For
		// example, "scheme:host/path" and "scheme:///host/path" will both be
		// canonicalized to "scheme://host/path". The origin of a standard scheme URL
		// is the combination of scheme, host and port (i.e., "scheme://host:port" in
		// the most explicit case).
		//
		// For non-standard scheme URLs only the "scheme:" component is parsed and
		// canonicalized. The remainder of the URL will be passed to the handler as-
		// is. For example, "scheme:///some%20text" will remain the same. Non-standard
		// scheme URLs cannot be used as a target for form submission.
		//
		// If |is_local| is true (1) the scheme will be treated as local (i.e., with
		// the same security rules as those applied to "file" URLs). Normal pages
		// cannot link to or access local URLs. Also, by default, local URLs can only
		// perform XMLHttpRequest calls to the same URL (origin + path) that
		// originated the request. To allow XMLHttpRequest calls from a local URL to
		// other URLs with the same origin set the
		// CefSettings.file_access_from_file_urls_allowed value to true (1). To allow
		// XMLHttpRequest calls from a local URL to all origins set the
		// CefSettings.universal_access_from_file_urls_allowed value to true (1).
		//
		// If |is_display_isolated| is true (1) the scheme will be treated as display-
		// isolated. This means that pages cannot display these URLs unless they are
		// from the same scheme. For example, pages in another origin cannot create
		// iframes or hyperlinks to URLs with this scheme.
		//
		// This function may be called on any thread. It should only be called once
		// per unique |scheme_name| value. If |scheme_name| is already registered or
		// if an error occurs this function will return false (0).
		function AddCustomScheme(const aSchemeName: ustring; aIsStandard: Boolean; aIsLocal: Boolean; aIsDisplayIsolated: Boolean): Boolean;
	end;

	// Structure that manages custom scheme registrations.
	// Structure that creates cef_resource_handler_t instances for handling scheme
	// requests. The functions of this structure will always be called on the IO
	// thread.
	ICefSchemeHandlerFactory = Interface(ICefBase)
	['{79BAAE39-343A-4C98-98BD-36095B341EC7}']
		// Return a new resource handler instance to handle the request or an NULL
		// reference to allow default handling of the request. |browser| and |frame|
		// will be the browser window and frame respectively that originated the
		// request or NULL if the request did not originate from a browser window (for
		// example, if the request came from cef_urlrequest_t). The |request| object
		// passed to this function will not contain cookie data.
		function New(const aBrowser: ICefBrowser; const aFrame: ICefFrame; const aSchemeName: ustring; const aRequest: ICefRequest): ICefResourceHandler;
	end;


	//..............................................................................cef_ssl_info_capi.h
	// Structure representing the issuer or subject field of an X.509 certificate.
	ICefSslcertPrincipal = Interface(ICefBase)
	['{1A2D9F8E-82CF-4262-9811-23F22932D2F6}']
		// Returns a name that can be used to represent the issuer.  It tries in this
		// order: CN, O and OU and returns the first non-NULL one found.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetDisplayName: ustring;
		// Returns the common name.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetCommonName: ustring;
		// Returns the locality name.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetLocalityName: ustring;
		// Returns the state or province name.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetStateOrProvinceName: ustring;
		// Returns the country name.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetCountryName: ustring;
		// Retrieve the list of street addresses.
		procedure GetStreetAddresses(aAddresses: TStrings);
		// Retrieve the list of organization names.
		procedure GetOrganizationNames(aNames: TStrings);
		// Retrieve the list of organization unit names.
		procedure GetOrganizationUnitNames(aNames: TStrings);
		// Retrieve the list of domain components.
		procedure GetDomainComponents(aComponents: TStrings);

		property CommonName: ustring read GetCommonName;
		property CountryName: ustring read GetCountryName;
		property DisplayName: ustring read GetDisplayName;
		property LocalityName: ustring read GetLocalityName;
		property StateOrProvinceName: ustring read GetStateOrProvinceName;
	end;

	// Structure representing the issuer or subject field of an X.509 certificate.
	// Structure representing SSL information.
	ICefSslinfo = Interface(ICefBase)
	['{7CE7FEBB-1C65-40A9-A180-1EC7983B5316}']
		// Returns the subject of the X.509 certificate. For HTTPS server certificates
		// this represents the web server.  The common name of the subject should
		// match the host name of the web server.
		function GetSubject: ICefSslcertPrincipal;
		// Returns the issuer of the X.509 certificate.
		function GetIssuer: ICefSslcertPrincipal;
		// Returns the DER encoded serial number for the X.509 certificate. The value
		// possibly includes a leading 00 byte.
		function GetSerialNumber: ICefBinaryValue;
		// Returns the date before which the X.509 certificate is invalid.
		// CefTime.GetTimeT() will return 0 if no date was specified.
		function GetValidStart: TCefTime;
		// Returns the date after which the X.509 certificate is invalid.
		// CefTime.GetTimeT() will return 0 if no date was specified.
		function GetValidExpiry: TCefTime;
		// Returns the DER encoded data for the X.509 certificate.
		function GetDerencoded: ICefBinaryValue;
		// Returns the PEM encoded data for the X.509 certificate.
		function GetPemencoded: ICefBinaryValue;

		property Derencoded: ICefBinaryValue read GetDerencoded;
		property Issuer: ICefSslcertPrincipal read GetIssuer;
		property Pemencoded: ICefBinaryValue read GetPemencoded;
		property SerialNumber: ICefBinaryValue read GetSerialNumber;
		property Subject: ICefSslcertPrincipal read GetSubject;
		property ValidExpiry: TCefTime read GetValidExpiry;
		property ValidStart: TCefTime read GetValidStart;
	end;


	//..............................................................................cef_stream_capi.h
	// Structure the client can implement to provide a custom stream reader. The
	// functions of this structure may be called on any thread.
	ICefReadHandler = Interface(ICefBase)
	['{E08DE47F-460E-4879-A510-EF9BA12E55E6}']
		// Read raw binary data.
		function Read(aPtr: cvoid; aSize: csize_t; aN: csize_t): csize_t;
		// Seek to the specified offset position. |whence| may be any one of SEEK_CUR,
		// SEEK_END or SEEK_SET. Return zero on success and non-zero on failure.
		function Seek(aOffset: cint64; aWhence: cint): Boolean;
		// Return the current offset position.
		function Tell: cint64;
		// Return non-zero if at end of file.
		function Eof: Boolean;
		// Return true (1) if this handler performs work like accessing the file
		// system which may block. Used as a hint for determining the thread to access
		// the handler from.
		function MayBlock: Boolean;
	end;

	// Structure the client can implement to provide a custom stream reader. The
	// functions of this structure may be called on any thread.
	// Structure used to read data from a stream. The functions of this structure
	// may be called on any thread.
	ICefStreamReader = Interface(ICefBase)
	['{1B0AD3D5-0493-40F5-B74B-CF02FDDF2F78}']
		// Read raw binary data.
		function Read(aPtr: cvoid; aSize: csize_t; aN: csize_t): csize_t;
		// Seek to the specified offset position. |whence| may be any one of SEEK_CUR,
		// SEEK_END or SEEK_SET. Returns zero on success and non-zero on failure.
		function Seek(aOffset: cint64; aWhence: cint): Boolean;
		// Return the current offset position.
		function Tell: cint64;
		// Return non-zero if at end of file.
		function Eof: Boolean;
		// Returns true (1) if this reader performs work like accessing the file
		// system which may block. Used as a hint for determining the thread to access
		// the reader from.
		function MayBlock: Boolean;
	end;

	// Structure the client can implement to provide a custom stream writer. The
	// functions of this structure may be called on any thread.
	ICefWriteHandler = Interface(ICefBase)
	['{A4676D75-B20E-4767-8653-B0BC94EA0AC7}']
		// Write raw binary data.
		function Write(aPtr: cvoid; aSize: csize_t; aN: csize_t): csize_t;
		// Seek to the specified offset position. |whence| may be any one of SEEK_CUR,
		// SEEK_END or SEEK_SET. Return zero on success and non-zero on failure.
		function Seek(aOffset: cint64; aWhence: cint): Boolean;
		// Return the current offset position.
		function Tell: cint64;
		// Flush the stream.
		function Flush: Boolean;
		// Return true (1) if this handler performs work like accessing the file
		// system which may block. Used as a hint for determining the thread to access
		// the handler from.
		function MayBlock: Boolean;
	end;

	// Structure the client can implement to provide a custom stream writer. The
	// functions of this structure may be called on any thread.
	// Structure used to write data to a stream. The functions of this structure may
	// be called on any thread.
	ICefStreamWriter = Interface(ICefBase)
	['{FB242335-B8E2-4C99-82F1-CF50A6ACBC5D}']
		// Write raw binary data.
		function Write(aPtr: cvoid; aSize: csize_t; aN: csize_t): csize_t;
		// Seek to the specified offset position. |whence| may be any one of SEEK_CUR,
		// SEEK_END or SEEK_SET. Returns zero on success and non-zero on failure.
		function Seek(aOffset: cint64; aWhence: cint): Boolean;
		// Return the current offset position.
		function Tell: cint64;
		// Flush the stream.
		function Flush: Boolean;
		// Returns true (1) if this writer performs work like accessing the file
		// system which may block. Used as a hint for determining the thread to access
		// the writer from.
		function MayBlock: Boolean;
	end;


	//..............................................................................cef_string_visitor_capi.h
	// Implement this structure to receive string values asynchronously.
	ICefStringVisitor = Interface(ICefBase)
	['{F49241C4-240F-45B8-9D77-A1257DEBA51C}']
		// Method that will be executed.
		procedure Visit(const aString: ustring);
	end;


	//..............................................................................cef_task_capi.h
	// Implement this structure for asynchronous task execution. If the task is
	// posted successfully and if the associated message loop is still running then
	// the execute() function will be called on the target thread. If the task fails
	// to post then the task object may be destroyed on the source thread instead of
	// the target thread. For this reason be cautious when performing work in the
	// task object destructor.
	ICefTask = Interface(ICefBase)
	['{90CE3B0D-CBB6-44E8-B76D-1D5A00DB179F}']
		// Method that will be executed on the target thread.
		procedure Execute;
	end;

	// Implement this structure for asynchronous task execution. If the task is
	// posted successfully and if the associated message loop is still running then
	// the execute() function will be called on the target thread. If the task fails
	// to post then the task object may be destroyed on the source thread instead of
	// the target thread. For this reason be cautious when performing work in the
	// task object destructor.
	// Structure that asynchronously executes tasks on the associated thread. It is
	// safe to call the functions of this structure on any thread.
	//
	// CEF maintains multiple internal threads that are used for handling different
	// types of tasks in different processes. The cef_thread_id_t definitions in
	// cef_types.h list the common CEF threads. Task runners are also available for
	// other CEF threads as appropriate (for example, V8 WebWorker threads).
	ICefTaskRunner = Interface(ICefBase)
	['{12DD3180-CAC8-4A37-8950-178715F57D67}']
		// Returns true (1) if this object is pointing to the same task runner as
		// |that| object.
		function IsSame(const aThat: ICefTaskRunner): Boolean;
		// Returns true (1) if this task runner belongs to the current thread.
		function BelongsToCurrentThread: Boolean;
		// Returns true (1) if this task runner is for the specified CEF thread.
		function BelongsToThread(aThreadId: TCefThreadId): Boolean;
		// Post a task for execution on the thread associated with this task runner.
		// Execution will occur asynchronously.
		function PostTask(const aTask: ICefTask): Boolean;
		// Post a task for delayed execution on the thread associated with this task
		// runner. Execution will occur asynchronously. Delayed tasks are not
		// supported on V8 WebWorker threads and will be executed without the
		// specified delay.
		function PostDelayedTask(const aTask: ICefTask; aDelayMs: cint64): Boolean;
	end;

	//..............................................................................cef_trace_capi.h
	// Implement this structure to receive notification when tracing has completed.
	// The functions of this structure will be called on the browser process UI
	// thread.
	ICefEndTracingCallback = Interface(ICefBase)
	['{E0B6923D-AFA5-414F-ADA7-BCBD5E2E3A5B}']
		// Called after all processes have sent their trace data. |tracing_file| is
		// the path at which tracing data was written. The client is responsible for
		// deleting |tracing_file|.
		procedure OnEndTracingComplete(const aTracingFile: ustring);
	end;


	//..............................................................................cef_urlrequest_capi.h
	// Structure used to make a URL request. URL requests are not associated with a
	// browser instance so no cef_client_t callbacks will be executed. URL requests
	// can be created on any valid CEF thread in either the browser or render
	// process. Once created the functions of the URL request object must be
	// accessed on the same thread that created it.
	ICefUrlrequest = Interface(ICefBase)
	['{9F490EE6-4765-4FA3-A64F-8170E039D880}']
		// Returns the request object used to create this URL request. The returned
		// object is read-only and should not be modified.
		function GetRequest: ICefRequest;
		// Returns the client.
		function GetClient: ICefUrlrequestClient;
		// Returns the request status.
		function GetRequestStatus: TCefUrlrequestStatus;
		// Returns the request error if status is UR_CANCELED or UR_FAILED, or 0
		// otherwise.
		function GetRequestError: TCefErrorcode;
		// Returns the response, or NULL if no response information is available.
		// Response information will only be available after the upload has completed.
		// The returned object is read-only and should not be modified.
		function GetResponse: ICefResponse;
		// Cancel the request.
		procedure Cancel;

		property Client: ICefUrlrequestClient read GetClient;
		property Request: ICefRequest read GetRequest;
		property RequestError: TCefErrorcode read GetRequestError;
		property RequestStatus: TCefUrlrequestStatus read GetRequestStatus;
		property Response: ICefResponse read GetResponse;
	end;

	// Structure that should be implemented by the cef_urlrequest_t client. The
	// functions of this structure will be called on the same thread that created
	// the request unless otherwise documented.
	ICefUrlrequestClient = Interface(ICefBase)
	['{770E40F3-1C36-4C01-8256-0295D9552EEA}']
		// Notifies the client that the request has completed. Use the
		// cef_urlrequest_t::GetRequestStatus function to determine if the request was
		// successful or not.
		procedure OnRequestComplete(const aRequest: ICefUrlrequest);
		// Notifies the client of upload progress. |current| denotes the number of
		// bytes sent so far and |total| is the total size of uploading data (or -1 if
		// chunked upload is enabled). This function will only be called if the
		// UR_FLAG_REPORT_UPLOAD_PROGRESS flag is set on the request.
		procedure OnUploadProgress(const aRequest: ICefUrlrequest; aCurrent: cint64; aTotal: cint64);
		// Notifies the client of download progress. |current| denotes the number of
		// bytes received up to the call and |total| is the expected total size of the
		// response (or -1 if not determined).
		procedure OnDownloadProgress(const aRequest: ICefUrlrequest; aCurrent: cint64; aTotal: cint64);
		// Called when some part of the response is read. |data| contains the current
		// bytes received since the last call. This function will not be called if the
		// UR_FLAG_NO_DOWNLOAD_DATA flag is set on the request.
		procedure OnDownloadData(const aRequest: ICefUrlrequest; const aData: cvoid; aDataLength: csize_t);
		// Called on the IO thread when the browser needs credentials from the user.
		// |isProxy| indicates whether the host is a proxy server. |host| contains the
		// hostname and |port| contains the port number. Return true (1) to continue
		// the request and call cef_auth_callback_t::cont() when the authentication
		// information is available. Return false (0) to cancel the request. This
		// function will only be called for requests initiated from the browser
		// process.
		function GetAuthCredentials(aIsProxy: Boolean; const aHost: ustring; aPort: cint; const aRealm: ustring; const aScheme: ustring; const aCallback: ICefAuthCallback): Boolean;

		property AuthCredentials[aIsProxy: Boolean; const aHost: ustring; aPort: cint; const aRealm: ustring; const aScheme: ustring; const aCallback: ICefAuthCallback]: Boolean read GetAuthCredentials;
	end;


	//..............................................................................cef_v8_capi.h
	// Structure representing a V8 context handle. V8 handles can only be accessed
	// from the thread on which they are created. Valid threads for creating a V8
	// handle include the render process main thread (TID_RENDERER) and WebWorker
	// threads. A task runner for posting tasks on the associated thread can be
	// retrieved via the cef_v8context_t::get_task_runner() function.
	ICefV8context = Interface(ICefBase)
	['{097C8EA4-515F-4624-9182-DC709FC9E8F1}']
		// Returns the task runner associated with this context. V8 handles can only
		// be accessed from the thread on which they are created. This function can be
		// called on any render process thread.
		function GetTaskRunner: ICefTaskRunner;
		// Returns true (1) if the underlying handle is valid and it can be accessed
		// on the current thread. Do not call any other functions if this function
		// returns false (0).
		function IsValid: Boolean;
		// Returns the browser for this context. This function will return an NULL
		// reference for WebWorker contexts.
		function GetBrowser: ICefBrowser;
		// Returns the frame for this context. This function will return an NULL
		// reference for WebWorker contexts.
		function GetFrame: ICefFrame;
		// Returns the global object for this context. The context must be entered
		// before calling this function.
		function GetGlobal: ICefV8value;
		// Enter this context. A context must be explicitly entered before creating a
		// V8 Object, Array, Function or Date asynchronously. exit() must be called
		// the same number of times as enter() before releasing this context. V8
		// objects belong to the context in which they are created. Returns true (1)
		// if the scope was entered successfully.
		function Enter: Boolean;
		// Exit this context. Call this function only after calling enter(). Returns
		// true (1) if the scope was exited successfully.
		function Exit: Boolean;
		// Returns true (1) if this object is pointing to the same handle as |that|
		// object.
		function IsSame(const aThat: ICefV8context): Boolean;
		// Evaluates the specified JavaScript code using this context's global object.
		// On success |retval| will be set to the return value, if any, and the
		// function will return true (1). On failure |exception| will be set to the
		// exception, if any, and the function will return false (0).
		function Eval(const aCode: ustring; var aRetval: ICefV8value; var aException: ICefV8exception): Boolean;

		property Browser: ICefBrowser read GetBrowser;
		property Frame: ICefFrame read GetFrame;
		property Global: ICefV8value read GetGlobal;
		property TaskRunner: ICefTaskRunner read GetTaskRunner;
	end;

	// Structure that should be implemented to handle V8 function calls. The
	// functions of this structure will be called on the thread associated with the
	// V8 function.
	ICefV8handler = Interface(ICefBase)
	['{D571BC87-DB2C-42AD-9CA4-EE6D7DF9E4C4}']
		// Handle execution of the function identified by |name|. |object| is the
		// receiver ('this' object) of the function. |arguments| is the list of
		// arguments passed to the function. If execution succeeds set |retval| to the
		// function return value. If execution fails set |exception| to the exception
		// that will be thrown. Return true (1) if execution was handled.
		function Execute(const aName: ustring; const aObject: ICefV8value; aArgumentsCount: csize_t; const aArguments: TCefv8ValueArray; var aRetval: ICefV8value; var aException: ustring): Boolean;
	end;

	// Structure that should be implemented to handle V8 function calls. The
	// functions of this structure will be called on the thread associated with the
	// V8 function.
	// Structure that should be implemented to handle V8 accessor calls. Accessor
	// identifiers are registered by calling cef_v8value_t::set_value(). The
	// functions of this structure will be called on the thread associated with the
	// V8 accessor.
	ICefV8accessor = Interface(ICefBase)
	['{A10AC368-4EE9-45FF-9903-463936AB58AF}']
		// Handle retrieval the accessor value identified by |name|. |object| is the
		// receiver ('this' object) of the accessor. If retrieval succeeds set
		// |retval| to the return value. If retrieval fails set |exception| to the
		// exception that will be thrown. Return true (1) if accessor retrieval was
		// handled.
		function Get(const aName: ustring; const aObject: ICefV8value; out aRetval: ICefV8value; const aException: ustring): Boolean;
		// Handle assignment of the accessor value identified by |name|. |object| is
		// the receiver ('this' object) of the accessor. |value| is the new value
		// being assigned to the accessor. If assignment fails set |exception| to the
		// exception that will be thrown. Return true (1) if accessor assignment was
		// handled.
		function _Set(const aName: ustring; const aObject: ICefV8value; const aValue: ICefV8value; const aException: ustring): Boolean;
	end;

	// Structure that should be implemented to handle V8 function calls. The
	// functions of this structure will be called on the thread associated with the
	// V8 function.
	// Structure that should be implemented to handle V8 accessor calls. Accessor
	// identifiers are registered by calling cef_v8value_t::set_value(). The
	// functions of this structure will be called on the thread associated with the
	// V8 accessor.
	// Structure representing a V8 exception. The functions of this structure may be
	// called on any render process thread.
	ICefV8exception = Interface(ICefBase)
	['{718682C6-1B63-40B0-A5BD-D4F282ED7B69}']
		// Returns the exception message.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetMessage: ustring;
		// Returns the line of source code that the exception occurred within.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetSourceLine: ustring;
		// Returns the resource name for the script from where the function causing
		// the error originates.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetScriptResourceName: ustring;
		// Returns the 1-based number of the line where the error occurred or 0 if the
		// line number is unknown.
		function GetLineNumber: cint;
		// Returns the index within the script of the first character where the error
		// occurred.
		function GetStartPosition: cint;
		// Returns the index within the script of the last character where the error
		// occurred.
		function GetEndPosition: cint;
		// Returns the index within the line of the first character where the error
		// occurred.
		function GetStartColumn: cint;
		// Returns the index within the line of the last character where the error
		// occurred.
		function GetEndColumn: cint;

		property EndColumn: cint read GetEndColumn;
		property EndPosition: cint read GetEndPosition;
		property LineNumber: cint read GetLineNumber;
		property Message: ustring read GetMessage;
		property ScriptResourceName: ustring read GetScriptResourceName;
		property SourceLine: ustring read GetSourceLine;
		property StartColumn: cint read GetStartColumn;
		property StartPosition: cint read GetStartPosition;
	end;

	// Structure that should be implemented to handle V8 function calls. The
	// functions of this structure will be called on the thread associated with the
	// V8 function.
	// Structure that should be implemented to handle V8 accessor calls. Accessor
	// identifiers are registered by calling cef_v8value_t::set_value(). The
	// functions of this structure will be called on the thread associated with the
	// V8 accessor.
	// Structure representing a V8 exception. The functions of this structure may be
	// called on any render process thread.
	// Structure representing a V8 value handle. V8 handles can only be accessed
	// from the thread on which they are created. Valid threads for creating a V8
	// handle include the render process main thread (TID_RENDERER) and WebWorker
	// threads. A task runner for posting tasks on the associated thread can be
	// retrieved via the cef_v8context_t::get_task_runner() function.
	ICefV8value = Interface(ICefBase)
	['{3D037759-B4AD-4220-AB18-3ABDB982BF34}']
		// Returns true (1) if the underlying handle is valid and it can be accessed
		// on the current thread. Do not call any other functions if this function
		// returns false (0).
		function IsValid: Boolean;
		// True if the value type is undefined.
		function IsUndefined: Boolean;
		// True if the value type is null.
		function IsNull: Boolean;
		// True if the value type is bool.
		function IsBool: Boolean;
		// True if the value type is int.
		function IsInt: Boolean;
		// True if the value type is unsigned int.
		function IsUint: Boolean;
		// True if the value type is double.
		function IsDouble: Boolean;
		// True if the value type is Date.
		function IsDate: Boolean;
		// True if the value type is string.
		function IsString: Boolean;
		// True if the value type is object.
		function IsObject: Boolean;
		// True if the value type is array.
		function IsArray: Boolean;
		// True if the value type is function.
		function IsFunction: Boolean;
		// Returns true (1) if this object is pointing to the same handle as |that|
		// object.
		function IsSame(const aThat: ICefV8value): Boolean;
		// Return a bool value.  The underlying data will be converted to if
		// necessary.
		function GetBoolValue: Boolean;
		// Return an int value.  The underlying data will be converted to if
		// necessary.
		function GetIntValue: cint32;
		// Return an unisgned int value.  The underlying data will be converted to if
		// necessary.
		function GetUintValue: cuint32;
		// Return a double value.  The underlying data will be converted to if
		// necessary.
		function GetDoubleValue: cdouble;
		// Return a Date value.  The underlying data will be converted to if
		// necessary.
		function GetDateValue: TCefTime;
		// Return a string value.  The underlying data will be converted to if
		// necessary.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetStringValue: ustring;
		// Returns true (1) if this is a user created object.
		function IsUserCreated: Boolean;
		// Returns true (1) if the last function call resulted in an exception. This
		// attribute exists only in the scope of the current CEF value object.
		function HasException: Boolean;
		// Returns the exception resulting from the last function call. This attribute
		// exists only in the scope of the current CEF value object.
		function GetException: ICefV8exception;
		// Clears the last exception and returns true (1) on success.
		function ClearException: Boolean;
		// Returns true (1) if this object will re-throw future exceptions. This
		// attribute exists only in the scope of the current CEF value object.
		function WillRethrowExceptions: Boolean;
		// Set whether this object will re-throw future exceptions. By default
		// exceptions are not re-thrown. If a exception is re-thrown the current
		// context should not be accessed again until after the exception has been
		// caught and not re-thrown. Returns true (1) on success. This attribute
		// exists only in the scope of the current CEF value object.
		function SetRethrowExceptions(aRethrow: Boolean): Boolean;
		// Returns true (1) if the object has a value with the specified identifier.
		function HasValueBykey(const aKey: ustring): Boolean;
		// Returns true (1) if the object has a value with the specified identifier.
		function HasValueByindex(aIndex: cint): Boolean;
		// Deletes the value with the specified identifier and returns true (1) on
		// success. Returns false (0) if this function is called incorrectly or an
		// exception is thrown. For read-only and don't-delete values this function
		// will return true (1) even though deletion failed.
		function DeleteValueBykey(const aKey: ustring): Boolean;
		// Deletes the value with the specified identifier and returns true (1) on
		// success. Returns false (0) if this function is called incorrectly, deletion
		// fails or an exception is thrown. For read-only and don't-delete values this
		// function will return true (1) even though deletion failed.
		function DeleteValueByindex(aIndex: cint): Boolean;
		// Returns the value with the specified identifier on success. Returns NULL if
		// this function is called incorrectly or an exception is thrown.
		function GetValueBykey(const aKey: ustring): ICefV8value;
		// Returns the value with the specified identifier on success. Returns NULL if
		// this function is called incorrectly or an exception is thrown.
		function GetValueByindex(aIndex: cint): ICefV8value;
		// Associates a value with the specified identifier and returns true (1) on
		// success. Returns false (0) if this function is called incorrectly or an
		// exception is thrown. For read-only values this function will return true
		// (1) even though assignment failed.
		function SetValueBykey(const aKey: ustring; const aValue: ICefV8value; aAttribute: TCefV8Propertyattribute): Boolean;
		// Associates a value with the specified identifier and returns true (1) on
		// success. Returns false (0) if this function is called incorrectly or an
		// exception is thrown. For read-only values this function will return true
		// (1) even though assignment failed.
		function SetValueByindex(aIndex: cint; const aValue: ICefV8value): Boolean;
		// Registers an identifier and returns true (1) on success. Access to the
		// identifier will be forwarded to the cef_v8accessor_t instance passed to
		// cef_v8value_t::cef_v8value_create_object(). Returns false (0) if this
		// function is called incorrectly or an exception is thrown. For read-only
		// values this function will return true (1) even though assignment failed.
		function SetValueByaccessor(const aKey: ustring; aSettings: TCefV8Accesscontrol; aAttribute: TCefV8Propertyattribute): Boolean;
		// Read the keys for the object's values into the specified vector. Integer-
		// based keys will also be returned as strings.
		function GetKeys(aKeys: TStrings): Boolean;
		// Sets the user data for this object and returns true (1) on success. Returns
		// false (0) if this function is called incorrectly. This function can only be
		// called on user created objects.
		function SetUserData(const aUserData: ICefBase): Boolean;
		// Returns the user data, if any, assigned to this object.
		function GetUserData: ICefBase;
		// Returns the amount of externally allocated memory registered for the
		// object.
		function GetExternallyAllocatedMemory: cint;
		// Adjusts the amount of registered external memory for the object. Used to
		// give V8 an indication of the amount of externally allocated memory that is
		// kept alive by JavaScript objects. V8 uses this information to decide when
		// to perform global garbage collection. Each cef_v8value_t tracks the amount
		// of external memory associated with it and automatically decreases the
		// global total by the appropriate amount on its destruction.
		// |change_in_bytes| specifies the number of bytes to adjust by. This function
		// returns the number of bytes associated with the object after the
		// adjustment. This function can only be called on user created objects.
		function AdjustExternallyAllocatedMemory(aChangeInBytes: cint): cint;
		// Returns the number of elements in the array.
		function GetArrayLength: cint;
		// Returns the function name.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetFunctionName: ustring;
		// Returns the function handler or NULL if not a CEF-created function.
		function GetFunctionHandler: ICefV8handler;
		// Execute the function using the current V8 context. This function should
		// only be called from within the scope of a cef_v8handler_t or
		// cef_v8accessor_t callback, or in combination with calling enter() and
		// exit() on a stored cef_v8context_t reference. |object| is the receiver
		// ('this' object) of the function. If |object| is NULL the current context's
		// global object will be used. |arguments| is the list of arguments that will
		// be passed to the function. Returns the function return value on success.
		// Returns NULL if this function is called incorrectly or an exception is
		// thrown.
		function ExecuteFunction(const aObject: ICefV8value; aArgumentsCount: csize_t; const aArguments: TCefv8ValueArray): ICefV8value;
		// Execute the function using the specified V8 context. |object| is the
		// receiver ('this' object) of the function. If |object| is NULL the specified
		// context's global object will be used. |arguments| is the list of arguments
		// that will be passed to the function. Returns the function return value on
		// success. Returns NULL if this function is called incorrectly or an
		// exception is thrown.
		function ExecuteFunctionWithContext(const aContext: ICefV8context; const aObject: ICefV8value; aArgumentsCount: csize_t; const aArguments: TCefv8ValueArray): ICefV8value;

		property ArrayLength: cint read GetArrayLength;
		property BoolValue: Boolean read GetBoolValue;
		property DateValue: TCefTime read GetDateValue;
		property DoubleValue: cdouble read GetDoubleValue;
		property Exception: ICefV8exception read GetException;
		property ExternallyAllocatedMemory: cint read GetExternallyAllocatedMemory;
		property FunctionHandler: ICefV8handler read GetFunctionHandler;
		property FunctionName: ustring read GetFunctionName;
		property IntValue: cint32 read GetIntValue;
		property StringValue: ustring read GetStringValue;
		property UintValue: cuint32 read GetUintValue;
	end;

	// Structure representing a V8 stack trace handle. V8 handles can only be
	// accessed from the thread on which they are created. Valid threads for
	// creating a V8 handle include the render process main thread (TID_RENDERER)
	// and WebWorker threads. A task runner for posting tasks on the associated
	// thread can be retrieved via the cef_v8context_t::get_task_runner() function.
	ICefV8stackTrace = Interface(ICefBase)
	['{CAE26659-0D45-4DB0-829D-BC0860AE53ED}']
		// Returns true (1) if the underlying handle is valid and it can be accessed
		// on the current thread. Do not call any other functions if this function
		// returns false (0).
		function IsValid: Boolean;
		// Returns the number of stack frames.
		function GetFrameCount: cint;
		// Returns the stack frame at the specified 0-based index.
		function GetFrame(aIndex: cint): ICefV8stackFrame;

		property Frame[aIndex: cint]: ICefV8stackFrame read GetFrame;
		property FrameCount: cint read GetFrameCount;
	end;

	// Structure representing a V8 stack frame handle. V8 handles can only be
	// accessed from the thread on which they are created. Valid threads for
	// creating a V8 handle include the render process main thread (TID_RENDERER)
	// and WebWorker threads. A task runner for posting tasks on the associated
	// thread can be retrieved via the cef_v8context_t::get_task_runner() function.
	ICefV8stackFrame = Interface(ICefBase)
	['{A26F5853-A69B-4190-84BF-7318EC423E07}']
		// Returns true (1) if the underlying handle is valid and it can be accessed
		// on the current thread. Do not call any other functions if this function
		// returns false (0).
		function IsValid: Boolean;
		// Returns the name of the resource script that contains the function.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetScriptName: ustring;
		// Returns the name of the resource script that contains the function or the
		// sourceURL value if the script name is undefined and its source ends with a
		// "//@ sourceURL=..." string.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetScriptNameOrSourceUrl: ustring;
		// Returns the name of the function.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetFunctionName: ustring;
		// Returns the 1-based line number for the function call or 0 if unknown.
		function GetLineNumber: cint;
		// Returns the 1-based column offset on the line for the function call or 0 if
		// unknown.
		function GetColumn: cint;
		// Returns true (1) if the function was compiled using eval().
		function IsEval: Boolean;
		// Returns true (1) if the function was called as a constructor via "new".
		function IsConstructor: Boolean;

		property Column: cint read GetColumn;
		property FunctionName: ustring read GetFunctionName;
		property LineNumber: cint read GetLineNumber;
		property ScriptName: ustring read GetScriptName;
		property ScriptNameOrSourceUrl: ustring read GetScriptNameOrSourceUrl;
	end;


	//..............................................................................cef_values_capi.h
	// Structure that wraps other data value types. Complex types (binary,
	// dictionary and list) will be referenced but not owned by this object. Can be
	// used on any process and thread.
	ICefValue = Interface(ICefBase)
	['{B5BBF6B5-0579-4A76-A9C6-EE1E6DE8E940}']
		// Returns true (1) if the underlying data is valid. This will always be true
		// (1) for simple types. For complex types (binary, dictionary and list) the
		// underlying data may become invalid if owned by another object (e.g. list or
		// dictionary) and that other object is then modified or destroyed. This value
		// object can be re-used by calling Set*() even if the underlying data is
		// invalid.
		function IsValid: Boolean;
		// Returns true (1) if the underlying data is owned by another object.
		function IsOwned: Boolean;
		// Returns true (1) if the underlying data is read-only. Some APIs may expose
		// read-only objects.
		function IsReadOnly: Boolean;
		// Returns true (1) if this object and |that| object have the same underlying
		// data. If true (1) modifications to this object will also affect |that|
		// object and vice-versa.
		function IsSame(const aThat: ICefValue): Boolean;
		// Returns true (1) if this object and |that| object have an equivalent
		// underlying value but are not necessarily the same object.
		function IsEqual(const aThat: ICefValue): Boolean;
		// Returns a copy of this object. The underlying data will also be copied.
		function Copy: ICefValue;
		// Returns the underlying value type.
		function GetType: TCefValueType;
		// Returns the underlying value as type bool.
		function GetBool: Boolean;
		// Returns the underlying value as type int.
		function GetInt: cint;
		// Returns the underlying value as type double.
		function GetDouble: cdouble;
		// Returns the underlying value as type string.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetString: ustring;
		// Returns the underlying value as type binary. The returned reference may
		// become invalid if the value is owned by another object or if ownership is
		// transferred to another object in the future. To maintain a reference to the
		// value after assigning ownership to a dictionary or list pass this object to
		// the set_value() function instead of passing the returned reference to
		// set_binary().
		function GetBinary: ICefBinaryValue;
		// Returns the underlying value as type dictionary. The returned reference may
		// become invalid if the value is owned by another object or if ownership is
		// transferred to another object in the future. To maintain a reference to the
		// value after assigning ownership to a dictionary or list pass this object to
		// the set_value() function instead of passing the returned reference to
		// set_dictionary().
		function GetDictionary: ICefDictionaryValue;
		// Returns the underlying value as type list. The returned reference may
		// become invalid if the value is owned by another object or if ownership is
		// transferred to another object in the future. To maintain a reference to the
		// value after assigning ownership to a dictionary or list pass this object to
		// the set_value() function instead of passing the returned reference to
		// set_list().
		function GetList: ICefListValue;
		// Sets the underlying value as type null. Returns true (1) if the value was
		// set successfully.
		function SetNull: Boolean;
		// Sets the underlying value as type bool. Returns true (1) if the value was
		// set successfully.
		function SetBool(aValue: Boolean): Boolean;
		// Sets the underlying value as type int. Returns true (1) if the value was
		// set successfully.
		function SetInt(aValue: cint): Boolean;
		// Sets the underlying value as type double. Returns true (1) if the value was
		// set successfully.
		function SetDouble(aValue: cdouble): Boolean;
		// Sets the underlying value as type string. Returns true (1) if the value was
		// set successfully.
		function SetString(const aValue: ustring): Boolean;
		// Sets the underlying value as type binary. Returns true (1) if the value was
		// set successfully. This object keeps a reference to |value| and ownership of
		// the underlying data remains unchanged.
		function SetBinary(const aValue: ICefBinaryValue): Boolean;
		// Sets the underlying value as type dict. Returns true (1) if the value was
		// set successfully. This object keeps a reference to |value| and ownership of
		// the underlying data remains unchanged.
		function SetDictionary(const aValue: ICefDictionaryValue): Boolean;
		// Sets the underlying value as type list. Returns true (1) if the value was
		// set successfully. This object keeps a reference to |value| and ownership of
		// the underlying data remains unchanged.
		function SetList(const aValue: ICefListValue): Boolean;

		property _Type: TCefValueType read GetType;
	end;

	// Structure representing a binary value. Can be used on any process and thread.
	ICefBinaryValue = Interface(ICefBase)
	['{03EE064F-4235-4641-AB02-7147D23DDEEE}']
		// Returns true (1) if this object is valid. This object may become invalid if
		// the underlying data is owned by another object (e.g. list or dictionary)
		// and that other object is then modified or destroyed. Do not call any other
		// functions if this function returns false (0).
		function IsValid: Boolean;
		// Returns true (1) if this object is currently owned by another object.
		function IsOwned: Boolean;
		// Returns true (1) if this object and |that| object have the same underlying
		// data.
		function IsSame(const aThat: ICefBinaryValue): Boolean;
		// Returns true (1) if this object and |that| object have an equivalent
		// underlying value but are not necessarily the same object.
		function IsEqual(const aThat: ICefBinaryValue): Boolean;
		// Returns a copy of this object. The data in this object will also be copied.
		function Copy: ICefBinaryValue;
		// Returns the data size.
		function GetSize: csize_t;
		// Read up to |buffer_size| number of bytes into |buffer|. Reading begins at
		// the specified byte |data_offset|. Returns the number of bytes read.
		function GetData(aBuffer: cvoid; aBufferSize: csize_t; aDataOffset: csize_t): csize_t;

		property Size: csize_t read GetSize;
	end;

	// Structure representing a dictionary value. Can be used on any process and
	// thread.
	ICefDictionaryValue = Interface(ICefBase)
	['{E8C76CD5-268B-4A5C-967F-424CCFAAB8DB}']
		// Returns true (1) if this object is valid. This object may become invalid if
		// the underlying data is owned by another object (e.g. list or dictionary)
		// and that other object is then modified or destroyed. Do not call any other
		// functions if this function returns false (0).
		function IsValid: Boolean;
		// Returns true (1) if this object is currently owned by another object.
		function IsOwned: Boolean;
		// Returns true (1) if the values of this object are read-only. Some APIs may
		// expose read-only objects.
		function IsReadOnly: Boolean;
		// Returns true (1) if this object and |that| object have the same underlying
		// data. If true (1) modifications to this object will also affect |that|
		// object and vice-versa.
		function IsSame(const aThat: ICefDictionaryValue): Boolean;
		// Returns true (1) if this object and |that| object have an equivalent
		// underlying value but are not necessarily the same object.
		function IsEqual(const aThat: ICefDictionaryValue): Boolean;
		// Returns a writable copy of this object. If |exclude_NULL_children| is true
		// (1) any NULL dictionaries or lists will be excluded from the copy.
		function Copy(aExcludeEmptyChildren: Boolean): ICefDictionaryValue;
		// Returns the number of values.
		function GetSize: csize_t;
		// Removes all values. Returns true (1) on success.
		function Clear: Boolean;
		// Returns true (1) if the current dictionary has a value for the given key.
		function HasKey(const aKey: ustring): Boolean;
		// Reads all keys for this dictionary into the specified vector.
		function GetKeys(aKeys: TStrings): Boolean;
		// Removes the value at the specified key. Returns true (1) is the value was
		// removed successfully.
		function Remove(const aKey: ustring): Boolean;
		// Returns the value type for the specified key.
		function GetType(const aKey: ustring): TCefValueType;
		// Returns the value at the specified key. For simple types the returned value
		// will copy existing data and modifications to the value will not modify this
		// object. For complex types (binary, dictionary and list) the returned value
		// will reference existing data and modifications to the value will modify
		// this object.
		function GetValue(const aKey: ustring): ICefValue;
		// Returns the value at the specified key as type bool.
		function GetBool(const aKey: ustring): Boolean;
		// Returns the value at the specified key as type int.
		function GetInt(const aKey: ustring): cint;
		// Returns the value at the specified key as type double.
		function GetDouble(const aKey: ustring): cdouble;
		// Returns the value at the specified key as type string.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetString(const aKey: ustring): ustring;
		// Returns the value at the specified key as type binary. The returned value
		// will reference existing data.
		function GetBinary(const aKey: ustring): ICefBinaryValue;
		// Returns the value at the specified key as type dictionary. The returned
		// value will reference existing data and modifications to the value will
		// modify this object.
		function GetDictionary(const aKey: ustring): ICefDictionaryValue;
		// Returns the value at the specified key as type list. The returned value
		// will reference existing data and modifications to the value will modify
		// this object.
		function GetList(const aKey: ustring): ICefListValue;
		// Sets the value at the specified key. Returns true (1) if the value was set
		// successfully. If |value| represents simple data then the underlying data
		// will be copied and modifications to |value| will not modify this object. If
		// |value| represents complex data (binary, dictionary or list) then the
		// underlying data will be referenced and modifications to |value| will modify
		// this object.
		function SetValue(const aKey: ustring; const aValue: ICefValue): Boolean;
		// Sets the value at the specified key as type null. Returns true (1) if the
		// value was set successfully.
		function SetNull(const aKey: ustring): Boolean;
		// Sets the value at the specified key as type bool. Returns true (1) if the
		// value was set successfully.
		function SetBool(const aKey: ustring; aValue: Boolean): Boolean;
		// Sets the value at the specified key as type int. Returns true (1) if the
		// value was set successfully.
		function SetInt(const aKey: ustring; aValue: cint): Boolean;
		// Sets the value at the specified key as type double. Returns true (1) if the
		// value was set successfully.
		function SetDouble(const aKey: ustring; aValue: cdouble): Boolean;
		// Sets the value at the specified key as type string. Returns true (1) if the
		// value was set successfully.
		function SetString(const aKey: ustring; const aValue: ustring): Boolean;
		// Sets the value at the specified key as type binary. Returns true (1) if the
		// value was set successfully. If |value| is currently owned by another object
		// then the value will be copied and the |value| reference will not change.
		// Otherwise, ownership will be transferred to this object and the |value|
		// reference will be invalidated.
		function SetBinary(const aKey: ustring; const aValue: ICefBinaryValue): Boolean;
		// Sets the value at the specified key as type dict. Returns true (1) if the
		// value was set successfully. If |value| is currently owned by another object
		// then the value will be copied and the |value| reference will not change.
		// Otherwise, ownership will be transferred to this object and the |value|
		// reference will be invalidated.
		function SetDictionary(const aKey: ustring; const aValue: ICefDictionaryValue): Boolean;
		// Sets the value at the specified key as type list. Returns true (1) if the
		// value was set successfully. If |value| is currently owned by another object
		// then the value will be copied and the |value| reference will not change.
		// Otherwise, ownership will be transferred to this object and the |value|
		// reference will be invalidated.
		function SetList(const aKey: ustring; const aValue: ICefListValue): Boolean;

		property Size: csize_t read GetSize;
		property _Type[const aKey: ustring]: TCefValueType read GetType;
	end;

	// Structure representing a list value. Can be used on any process and thread.
	ICefListValue = Interface(ICefBase)
	['{90B79108-89B8-4290-B496-5A6B9FF2C983}']
		// Returns true (1) if this object is valid. This object may become invalid if
		// the underlying data is owned by another object (e.g. list or dictionary)
		// and that other object is then modified or destroyed. Do not call any other
		// functions if this function returns false (0).
		function IsValid: Boolean;
		// Returns true (1) if this object is currently owned by another object.
		function IsOwned: Boolean;
		// Returns true (1) if the values of this object are read-only. Some APIs may
		// expose read-only objects.
		function IsReadOnly: Boolean;
		// Returns true (1) if this object and |that| object have the same underlying
		// data. If true (1) modifications to this object will also affect |that|
		// object and vice-versa.
		function IsSame(const aThat: ICefListValue): Boolean;
		// Returns true (1) if this object and |that| object have an equivalent
		// underlying value but are not necessarily the same object.
		function IsEqual(const aThat: ICefListValue): Boolean;
		// Returns a writable copy of this object.
		function Copy: ICefListValue;
		// Sets the number of values. If the number of values is expanded all new
		// value slots will default to type null. Returns true (1) on success.
		function SetSize(aSize: csize_t): Boolean;
		// Returns the number of values.
		function GetSize: csize_t;
		// Removes all values. Returns true (1) on success.
		function Clear: Boolean;
		// Removes the value at the specified index.
		function Remove(aIndex: cint): Boolean;
		// Returns the value type at the specified index.
		function GetType(aIndex: cint): TCefValueType;
		// Returns the value at the specified index. For simple types the returned
		// value will copy existing data and modifications to the value will not
		// modify this object. For complex types (binary, dictionary and list) the
		// returned value will reference existing data and modifications to the value
		// will modify this object.
		function GetValue(aIndex: cint): ICefValue;
		// Returns the value at the specified index as type bool.
		function GetBool(aIndex: cint): Boolean;
		// Returns the value at the specified index as type int.
		function GetInt(aIndex: cint): cint;
		// Returns the value at the specified index as type double.
		function GetDouble(aIndex: cint): cdouble;
		// Returns the value at the specified index as type string.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetString(aIndex: cint): ustring;
		// Returns the value at the specified index as type binary. The returned value
		// will reference existing data.
		function GetBinary(aIndex: cint): ICefBinaryValue;
		// Returns the value at the specified index as type dictionary. The returned
		// value will reference existing data and modifications to the value will
		// modify this object.
		function GetDictionary(aIndex: cint): ICefDictionaryValue;
		// Returns the value at the specified index as type list. The returned value
		// will reference existing data and modifications to the value will modify
		// this object.
		function GetList(aIndex: cint): ICefListValue;
		// Sets the value at the specified index. Returns true (1) if the value was
		// set successfully. If |value| represents simple data then the underlying
		// data will be copied and modifications to |value| will not modify this
		// object. If |value| represents complex data (binary, dictionary or list)
		// then the underlying data will be referenced and modifications to |value|
		// will modify this object.
		function SetValue(aIndex: cint; const aValue: ICefValue): Boolean;
		// Sets the value at the specified index as type null. Returns true (1) if the
		// value was set successfully.
		function SetNull(aIndex: cint): Boolean;
		// Sets the value at the specified index as type bool. Returns true (1) if the
		// value was set successfully.
		function SetBool(aIndex: cint; aValue: Boolean): Boolean;
		// Sets the value at the specified index as type int. Returns true (1) if the
		// value was set successfully.
		function SetInt(aIndex: cint; aValue: cint): Boolean;
		// Sets the value at the specified index as type double. Returns true (1) if
		// the value was set successfully.
		function SetDouble(aIndex: cint; aValue: cdouble): Boolean;
		// Sets the value at the specified index as type string. Returns true (1) if
		// the value was set successfully.
		function SetString(aIndex: cint; const aValue: ustring): Boolean;
		// Sets the value at the specified index as type binary. Returns true (1) if
		// the value was set successfully. If |value| is currently owned by another
		// object then the value will be copied and the |value| reference will not
		// change. Otherwise, ownership will be transferred to this object and the
		// |value| reference will be invalidated.
		function SetBinary(aIndex: cint; const aValue: ICefBinaryValue): Boolean;
		// Sets the value at the specified index as type dict. Returns true (1) if the
		// value was set successfully. If |value| is currently owned by another object
		// then the value will be copied and the |value| reference will not change.
		// Otherwise, ownership will be transferred to this object and the |value|
		// reference will be invalidated.
		function SetDictionary(aIndex: cint; const aValue: ICefDictionaryValue): Boolean;
		// Sets the value at the specified index as type list. Returns true (1) if the
		// value was set successfully. If |value| is currently owned by another object
		// then the value will be copied and the |value| reference will not change.
		// Otherwise, ownership will be transferred to this object and the |value|
		// reference will be invalidated.
		function SetList(aIndex: cint; const aValue: ICefListValue): Boolean;

		property _Type[aIndex: cint]: TCefValueType read GetType;
	end;


	//..............................................................................cef_web_plugin_capi.h
	// Information about a specific web plugin.
	ICefWebPluginInfo = Interface(ICefBase)
	['{8C2759CB-2ED0-4D1A-A8C5-3AF572A3E74F}']
		// Returns the plugin name (i.e. Flash).
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetName: ustring;
		// Returns the plugin file path (DLL/bundle/library).
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetPath: ustring;
		// Returns the version of the plugin (may be OS-specific).
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetVersion: ustring;
		// Returns a description of the plugin from the version information.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetDescription: ustring;

		property Description: ustring read GetDescription;
		property Name: ustring read GetName;
		property Path: ustring read GetPath;
		property Version: ustring read GetVersion;
	end;

	// Information about a specific web plugin.
	// Structure to implement for visiting web plugin information. The functions of
	// this structure will be called on the browser process UI thread.
	ICefWebPluginInfoVisitor = Interface(ICefBase)
	['{80E2FA37-983D-479B-8B63-487DF08E93EE}']
		// Method that will be called once for each plugin. |count| is the 0-based
		// index for the current plugin. |total| is the total number of plugins.
		// Return false (0) to stop visiting plugins. This function may never be
		// called if no plugins are found.
		function Visit(const aInfo: ICefWebPluginInfo; aCount: cint; aTotal: cint): Boolean;
	end;

	// Information about a specific web plugin.
	// Structure to implement for visiting web plugin information. The functions of
	// this structure will be called on the browser process UI thread.
	// Structure to implement for receiving unstable plugin information. The
	// functions of this structure will be called on the browser process IO thread.
	ICefWebPluginUnstableCallback = Interface(ICefBase)
	['{E768C5FF-7885-4DC8-99A2-A933168EB5BB}']
		// Method that will be called for the requested plugin. |unstable| will be
		// true (1) if the plugin has reached the crash count threshold of 3 times in
		// 120 seconds.
		procedure IsUnstable(const aPath: ustring; aUnstable: Boolean);
	end;


	//..............................................................................cef_xml_reader_capi.h
	// Structure that supports the reading of XML data via the libxml streaming API.
	// The functions of this structure should only be called on the thread that
	// creates the object.
	ICefXmlReader = Interface(ICefBase)
	['{9AE941BB-9B08-4596-AD53-0707E0B31519}']
		// Moves the cursor to the next node in the document. This function must be
		// called at least once to set the current cursor position. Returns true (1)
		// if the cursor position was set successfully.
		function MoveToNextNode: Boolean;
		// Close the document. This should be called directly to ensure that cleanup
		// occurs on the correct thread.
		function Close: Boolean;
		// Returns true (1) if an error has been reported by the XML parser.
		function HasError: Boolean;
		// Returns the error string.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetError: ustring;
		// Returns the node type.
		function GetType: TCefXmlNodeType;
		// Returns the node depth. Depth starts at 0 for the root node.
		function GetDepth: cint;
		// Returns the local name. See http://www.w3.org/TR/REC-xml-names/#NT-
		// LocalPart for additional details.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetLocalName: ustring;
		// Returns the namespace prefix. See http://www.w3.org/TR/REC-xml-names/ for
		// additional details.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetPrefix: ustring;
		// Returns the qualified name, equal to (Prefix:)LocalName. See
		// http://www.w3.org/TR/REC-xml-names/#ns-qualnames for additional details.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetQualifiedName: ustring;
		// Returns the URI defining the namespace associated with the node. See
		// http://www.w3.org/TR/REC-xml-names/ for additional details.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetNamespaceUri: ustring;
		// Returns the base URI of the node. See http://www.w3.org/TR/xmlbase/ for
		// additional details.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetBaseUri: ustring;
		// Returns the xml:lang scope within which the node resides. See
		// http://www.w3.org/TR/REC-xml/#sec-lang-tag for additional details.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetXmlLang: ustring;
		// Returns true (1) if the node represents an NULL element. <a/> is considered
		// NULL but <a></a> is not.
		function IsEmptyElement: Boolean;
		// Returns true (1) if the node has a text value.
		function HasValue: Boolean;
		// Returns the text value.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetValue: ustring;
		// Returns true (1) if the node has attributes.
		function HasAttributes: Boolean;
		// Returns the number of attributes.
		function GetAttributeCount: csize_t;
		// Returns the value of the attribute at the specified 0-based index.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetAttributeByindex(aIndex: cint): ustring;
		// Returns the value of the attribute with the specified qualified name.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetAttributeByqname(const aQualifiedName: ustring): ustring;
		// Returns the value of the attribute with the specified local name and
		// namespace URI.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetAttributeBylname(const aLocalName: ustring; const aNamespaceURI: ustring): ustring;
		// Returns an XML representation of the current node's children.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetInnerXml: ustring;
		// Returns an XML representation of the current node including its children.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetOuterXml: ustring;
		// Returns the line number for the current node.
		function GetLineNumber: cint;
		// Moves the cursor to the attribute at the specified 0-based index. Returns
		// true (1) if the cursor position was set successfully.
		function MoveToAttributeByindex(aIndex: cint): Boolean;
		// Moves the cursor to the attribute with the specified qualified name.
		// Returns true (1) if the cursor position was set successfully.
		function MoveToAttributeByqname(const aQualifiedName: ustring): Boolean;
		// Moves the cursor to the attribute with the specified local name and
		// namespace URI. Returns true (1) if the cursor position was set
		// successfully.
		function MoveToAttributeBylname(const aLocalName: ustring; const aNamespaceURI: ustring): Boolean;
		// Moves the cursor to the first attribute in the current element. Returns
		// true (1) if the cursor position was set successfully.
		function MoveToFirstAttribute: Boolean;
		// Moves the cursor to the next attribute in the current element. Returns true
		// (1) if the cursor position was set successfully.
		function MoveToNextAttribute: Boolean;
		// Moves the cursor back to the carrying element. Returns true (1) if the
		// cursor position was set successfully.
		function MoveToCarryingElement: Boolean;

		property AttributeByindex[aIndex: cint]: ustring read GetAttributeByindex;
		property AttributeBylname[const aLocalName: ustring; const aNamespaceURI: ustring]: ustring read GetAttributeBylname;
		property AttributeByqname[const aQualifiedName: ustring]: ustring read GetAttributeByqname;
		property AttributeCount: csize_t read GetAttributeCount;
		property BaseUri: ustring read GetBaseUri;
		property Depth: cint read GetDepth;
		property Error: ustring read GetError;
		property InnerXml: ustring read GetInnerXml;
		property LineNumber: cint read GetLineNumber;
		property LocalName: ustring read GetLocalName;
		property NamespaceUri: ustring read GetNamespaceUri;
		property OuterXml: ustring read GetOuterXml;
		property Prefix: ustring read GetPrefix;
		property QualifiedName: ustring read GetQualifiedName;
		property _Type: TCefXmlNodeType read GetType;
		property Value: ustring read GetValue;
		property XmlLang: ustring read GetXmlLang;
	end;


	//..............................................................................cef_zip_reader_capi.h
	// Structure that supports the reading of zip archives via the zlib unzip API.
	// The functions of this structure should only be called on the thread that
	// creates the object.
	ICefZipReader = Interface(ICefBase)
	['{751075C7-1F93-4B57-8476-CD560A5DF413}']
		// Moves the cursor to the first file in the archive. Returns true (1) if the
		// cursor position was set successfully.
		function MoveToFirstFile: Boolean;
		// Moves the cursor to the next file in the archive. Returns true (1) if the
		// cursor position was set successfully.
		function MoveToNextFile: Boolean;
		// Moves the cursor to the specified file in the archive. If |caseSensitive|
		// is true (1) then the search will be case sensitive. Returns true (1) if the
		// cursor position was set successfully.
		function MoveToFile(const aFileName: ustring; aCaseSensitive: Boolean): Boolean;
		// Closes the archive. This should be called directly to ensure that cleanup
		// occurs on the correct thread.
		function Close: Boolean;
		// Returns the name of the file.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetFileName: ustring;
		// Returns the uncompressed size of the file.
		function GetFileSize: cint64;
		// Returns the last modified timestamp for the file.
		function GetFileLastModified: ctime_t;
		// Opens the file for reading of uncompressed data. A read password may
		// optionally be specified.
		function OpenFile(const aPassword: ustring): Boolean;
		// Closes the file.
		function CloseFile: Boolean;
		// Read uncompressed file contents into the specified buffer. Returns < 0 if
		// an error occurred, 0 if at the end of file, or the number of bytes read.
		function ReadFile(const aBuffer: cvoid; aBufferSize: csize_t): cint;
		// Returns the current offset in the uncompressed file contents.
		function Tell: cint64;
		// Returns true (1) if at end of the file contents.
		function Eof: Boolean;

		property FileLastModified: ctime_t read GetFileLastModified;
		property FileName: ustring read GetFileName;
		property FileSize: cint64 read GetFileSize;
	end;


  ICefStringMap = interface
  ['{A33EBC01-B23A-4918-86A4-E24A243B342F}']
    function GetHandle: TCefStringMap;
    function GetSize: cint;
    function Find(const  Key: ustring): ustring;
    function GetKey(Index: cint): ustring;
    function GetValue(Index: cint): ustring;
    procedure Append(const  Key, Value: ustring);
    procedure Clear;

    property Handle: TCefStringMap read GetHandle;
    property Size: cint read GetSize;
    property Key[index: cint]: ustring read GetKey;
    property Value[index: cint]: ustring read GetValue;
  end;

  ICefStringMultimap = interface
  ['{583ED0C2-A9D6-4034-A7C9-20EC7E47F0C7}']
    function GetHandle: TCefStringMultimap;
    function GetSize: cint;
    function FindCount(const  Key: ustring): cint;
    function GetEnumerate(const  Key: ustring; ValueIndex: cint): ustring;
    function GetKey(Index: cint): ustring;
    function GetValue(Index: cint): ustring;
    procedure Append(const  Key, Value: ustring);
    procedure Clear;

    property Handle: TCefStringMap read GetHandle;
    property Size: cint read GetSize;
    property Key[index: cint]: ustring read GetKey;
    property Value[index: cint]: ustring read GetValue;
    property Enumerate[const  aKey: ustring; ValueIndex: cint]: ustring read GetEnumerate;
  end;

  ICefCustomStreamReader = interface(ICefBase)
  ['{BBCFF23A-6FE7-4C28-B13E-6D2ACA5C83B7}']
    function Read(ptr: Pointer; size, n: csize_t): csize_t;
    function Seek(offset: Int64; whence: cint): cint;
    function Tell: Int64;
    function Eof: Boolean;
  end;

  IChromiumEvents = interface ['{0C139DB1-0349-4D7F-8155-76FEA6A0126D}']
    procedure GetSettings(var settings: TCefBrowserSettings);

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
  end;

  ICefClientHandler = interface
    ['{E76F6888-D9C3-4FCE-9C23-E89659820A36}']
    procedure Disconnect;
  end;

implementation

end.
