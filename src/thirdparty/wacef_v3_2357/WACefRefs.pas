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

unit WACefRefs;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

{$I WACef.inc}

interface

uses
  {$IFDEF FPC}
  ctypes,
  {$ENDIF}
  WACefTypes,
  WACefInterfaces,
  WACefCApi,
  WACefOwns,
  WACefCExports,
  Classes;

type
  //..............................................................................cef_app_capi.h
	TCefAppRef = class;

	//..............................................................................cef_auth_callback_capi.h
	TCefAuthCallbackRef = class;

	//..............................................................................cef_base_capi.h
	TCefBaseRef = class;

	//..............................................................................cef_browser_capi.h
	TCefBrowserRef = class;
	TCefRunFileDialogCallbackRef = class;
	TCefNavigationEntryVisitorRef = class;
	TCefBrowserHostRef = class;

	//..............................................................................cef_browser_process_handler_capi.h
	TCefBrowserProcessHandlerRef = class;

	//..............................................................................cef_callback_capi.h
	TCefCallbackRef = class;
	TCefCompletionCallbackRef = class;

	//..............................................................................cef_client_capi.h
	TCefClientRef = class;

	//..............................................................................cef_command_line_capi.h
	TCefCommandLineRef = class;

	//..............................................................................cef_context_menu_handler_capi.h
	TCefContextMenuHandlerRef = class;
	TCefContextMenuParamsRef = class;

	//..............................................................................cef_cookie_capi.h
	TCefCookieManagerRef = class;
	TCefCookieVisitorRef = class;
	TCefSetCookieCallbackRef = class;
	TCefDeleteCookiesCallbackRef = class;

	//..............................................................................cef_dialog_handler_capi.h
	TCefFileDialogCallbackRef = class;
	TCefDialogHandlerRef = class;

	//..............................................................................cef_display_handler_capi.h
	TCefDisplayHandlerRef = class;

	//..............................................................................cef_dom_capi.h
	TCefDomvisitorRef = class;
	TCefDomdocumentRef = class;
	TCefDomnodeRef = class;

	//..............................................................................cef_download_handler_capi.h
	TCefBeforeDownloadCallbackRef = class;
	TCefDownloadItemCallbackRef = class;
	TCefDownloadHandlerRef = class;

	//..............................................................................cef_download_item_capi.h
	TCefDownloadItemRef = class;

	//..............................................................................cef_drag_data_capi.h
	TCefDragDataRef = class;

	//..............................................................................cef_drag_handler_capi.h
	TCefDragHandlerRef = class;

	//..............................................................................cef_find_handler_capi.h
	TCefFindHandlerRef = class;

	//..............................................................................cef_focus_handler_capi.h
	TCefFocusHandlerRef = class;

	//..............................................................................cef_frame_capi.h
	TCefFrameRef = class;

	//..............................................................................cef_geolocation_capi.h
	TCefGetGeolocationCallbackRef = class;

	//..............................................................................cef_geolocation_handler_capi.h
	TCefGeolocationCallbackRef = class;
	TCefGeolocationHandlerRef = class;

	//..............................................................................cef_jsdialog_handler_capi.h
	TCefJsdialogCallbackRef = class;
	TCefJsdialogHandlerRef = class;

	//..............................................................................cef_keyboard_handler_capi.h
	TCefKeyboardHandlerRef = class;

	//..............................................................................cef_life_span_handler_capi.h
	TCefLifeSpanHandlerRef = class;

	//..............................................................................cef_load_handler_capi.h
	TCefLoadHandlerRef = class;

	//..............................................................................cef_menu_model_capi.h
	TCefMenuModelRef = class;

	//..............................................................................cef_navigation_entry_capi.h
	TCefNavigationEntryRef = class;

	//..............................................................................cef_origin_whitelist_capi.h

	//..............................................................................cef_parser_capi.h

	//..............................................................................cef_path_util_capi.h

	//..............................................................................cef_print_handler_capi.h
	TCefPrintDialogCallbackRef = class;
	TCefPrintJobCallbackRef = class;
	TCefPrintHandlerRef = class;

	//..............................................................................cef_print_settings_capi.h
	TCefPrintSettingsRef = class;

	//..............................................................................cef_process_message_capi.h
	TCefProcessMessageRef = class;

	//..............................................................................cef_process_util_capi.h

	//..............................................................................cef_render_handler_capi.h
	TCefRenderHandlerRef = class;

	//..............................................................................cef_render_process_handler_capi.h
	TCefRenderProcessHandlerRef = class;

	//..............................................................................cef_request_capi.h
	TCefRequestRef = class;
	TCefPostDataRef = class;
	TCefPostDataElementRef = class;

	//..............................................................................cef_request_context_capi.h
	TCefRequestContextRef = class;

	//..............................................................................cef_request_context_handler_capi.h
	TCefRequestContextHandlerRef = class;

	//..............................................................................cef_request_handler_capi.h
	TCefRequestCallbackRef = class;
	TCefRequestHandlerRef = class;

	//..............................................................................cef_resource_bundle_handler_capi.h
	TCefResourceBundleHandlerRef = class;

	//..............................................................................cef_resource_handler_capi.h
	TCefResourceHandlerRef = class;

	//..............................................................................cef_response_capi.h
	TCefResponseRef = class;

	//..............................................................................cef_scheme_capi.h
	TCefSchemeRegistrarRef = class;
	TCefSchemeHandlerFactoryRef = class;

	//..............................................................................cef_ssl_info_capi.h
	TCefSslcertPrincipalRef = class;
	TCefSslinfoRef = class;

	//..............................................................................cef_stream_capi.h
	TCefReadHandlerRef = class;
	TCefStreamReaderRef = class;
	TCefWriteHandlerRef = class;
	TCefStreamWriterRef = class;

	//..............................................................................cef_string_visitor_capi.h
	TCefStringVisitorRef = class;

	//..............................................................................cef_task_capi.h
	TCefTaskRef = class;
	TCefTaskRunnerRef = class;

	//..............................................................................cef_trace_capi.h
	TCefEndTracingCallbackRef = class;

	//..............................................................................cef_urlrequest_capi.h
	TCefUrlrequestRef = class;
	TCefUrlrequestClientRef = class;

	//..............................................................................cef_v8_capi.h
	TCefV8contextRef = class;
	TCefV8handlerRef = class;
	TCefV8accessorRef = class;
	TCefV8exceptionRef = class;
	TCefV8valueRef = class;
	TCefV8stackTraceRef = class;
	TCefV8stackFrameRef = class;

	//..............................................................................cef_values_capi.h
	TCefValueRef = class;
	TCefBinaryValueRef = class;
	TCefDictionaryValueRef = class;
	TCefListValueRef = class;

	//..............................................................................cef_web_plugin_capi.h
	TCefWebPluginInfoRef = class;
	TCefWebPluginInfoVisitorRef = class;
	TCefWebPluginUnstableCallbackRef = class;

	//..............................................................................cef_xml_reader_capi.h
	TCefXmlReaderRef = class;

	//..............................................................................cef_zip_reader_capi.h
	TCefZipReaderRef = class;

  TCefBaseRef=class(TInterfacedObject, ICefBase)
  private
    FData: Pointer;
  public
    constructor Create(data: Pointer); virtual;
    destructor Destroy; override;
    function Wrap: Pointer;
    class function UnWrap(data: Pointer): ICefBase;
  end;

  //..............................................................................cef_app_capi.h
	// Implement this structure to provide handler implementations. Methods will be
	// called by the process and/or thread indicated.
	TCefAppRef = class(TCefBaseRef, ICefApp)
	protected
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
	public
		class function UnWrap(data: Pointer): ICefApp;
	end;


	//..............................................................................cef_auth_callback_capi.h
	// Callback structure used for asynchronous continuation of authentication
	// requests.
	TCefAuthCallbackRef = class(TCefBaseRef, ICefAuthCallback)
	protected
		// Continue the authentication request.
		procedure Cont(const aUsername: ustring; const aPassword: ustring);
		// Cancel the authentication request.
		procedure Cancel;
	public
		class function UnWrap(data: Pointer): ICefAuthCallback;
	end;

	//..............................................................................cef_browser_capi.h
	// Structure used to represent a browser window. When used in the browser
	// process the functions of this structure may be called on any thread unless
	// otherwise indicated in the comments. When used in the render process the
	// functions of this structure may only be called on the main thread.
	TCefBrowserRef = class(TCefBaseRef, ICefBrowser)
	protected
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
	public
		class function UnWrap(data: Pointer): ICefBrowser;
		// Create a new browser window using the window parameters specified by
		// |windowInfo|. All values will be copied internally and the actual window will
		// be created on the UI thread. If |request_context| is NULL the global request
		// context will be used. This function can be called on any browser process
		// thread and will not block.
		class function HostCreateBrowser(const aWindowInfo: TCefWindowInfo; const aClient: ICefClient; const aUrl: ustring; var aSettings: TCefBrowserSettings; const aRequestContext: ICefRequestContext): cint;
		// Create a new browser window using the window parameters specified by
		// |windowInfo|. All values will be copied internally and the actual window will
		// be created on the UI thread. If |request_context| is NULL the global request
		// context will be used. This function can be called on any browser process
		// thread and will not block.
		// Create a new browser window using the window parameters specified by
		// |windowInfo|. If |request_context| is NULL the global request context will be
		// used. This function can only be called on the browser process UI thread.
		class function HostCreateBrowserSync(const aWindowInfo: TCefWindowInfo; const aClient: ICefClient; const aUrl: ustring; var aSettings: TCefBrowserSettings; const aRequestContext: ICefRequestContext): ICefBrowser;
	end;

	// Callback structure for cef_browser_host_t::RunFileDialog. The functions of
	// this structure will be called on the browser process UI thread.
	TCefRunFileDialogCallbackRef = class(TCefBaseRef, ICefRunFileDialogCallback)
	protected
		// Called asynchronously after the file dialog is dismissed.
		// |selected_accept_filter| is the 0-based index of the value selected from
		// the accept filters array passed to cef_browser_host_t::RunFileDialog.
		// |file_paths| will be a single value or a list of values depending on the
		// dialog mode. If the selection was cancelled |file_paths| will be NULL.
		procedure OnFileDialogDismissed(aSelectedAcceptFilter: cint; aFilePaths: TStrings);
	public
		class function UnWrap(data: Pointer): ICefRunFileDialogCallback;
	end;

	// Callback structure for cef_browser_host_t::GetNavigationEntries. The
	// functions of this structure will be called on the browser process UI thread.
	TCefNavigationEntryVisitorRef = class(TCefBaseRef, ICefNavigationEntryVisitor)
	protected
		// Method that will be executed. Do not keep a reference to |entry| outside of
		// this callback. Return true (1) to continue visiting entries or false (0) to
		// stop. |current| is true (1) if this entry is the currently loaded
		// navigation entry. |index| is the 0-based index of this entry and |total| is
		// the total number of entries.
		function Visit(const aEntry: ICefNavigationEntry; aCurrent: Boolean; aIndex: cint; aTotal: cint): Boolean;
	public
		class function UnWrap(data: Pointer): ICefNavigationEntryVisitor;
	end;

	// Structure used to represent the browser process aspects of a browser window.
	// The functions of this structure can only be called in the browser process.
	// They may be called on any thread in that process unless otherwise indicated
	// in the comments.
	TCefBrowserHostRef = class(TCefBaseRef, ICefBrowserHost)
	protected
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
    procedure RunFileDialogProc(aMode: TCefFileDialogMode; const aTitle: ustring; const aDefaultFilePath: ustring; aAcceptFilters: TStrings; aSelectedAcceptFilter: cint; const aCallback: TCefRunFileDialogCallbackProc);
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
	public
		class function UnWrap(data: Pointer): ICefBrowserHost;
		// Create a new browser window using the window parameters specified by
		// |windowInfo|. All values will be copied internally and the actual window will
		// be created on the UI thread. If |request_context| is NULL the global request
		// context will be used. This function can be called on any browser process
		// thread and will not block.
		class function CreateBrowser(const aWindowInfo: TCefWindowInfo; const aClient: ICefClient; const aUrl: ustring; var aSettings: TCefBrowserSettings; const aRequestContext: ICefRequestContext): Boolean;
		// Create a new browser window using the window parameters specified by
		// |windowInfo|. All values will be copied internally and the actual window will
		// be created on the UI thread. If |request_context| is NULL the global request
		// context will be used. This function can be called on any browser process
		// thread and will not block.
		// Create a new browser window using the window parameters specified by
		// |windowInfo|. If |request_context| is NULL the global request context will be
		// used. This function can only be called on the browser process UI thread.
		class function CreateBrowserSync(const aWindowInfo: TCefWindowInfo; const aClient: ICefClient; const aUrl: ustring; var aSettings: TCefBrowserSettings; const aRequestContext: ICefRequestContext): ICefBrowser;
	end;


	//..............................................................................cef_browser_process_handler_capi.h
	// Structure used to implement browser process callbacks. The functions of this
	// structure will be called on the browser process main thread unless otherwise
	// indicated.
	TCefBrowserProcessHandlerRef = class(TCefBaseRef, ICefBrowserProcessHandler)
	protected
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
	public
		class function UnWrap(data: Pointer): ICefBrowserProcessHandler;
	end;


	//..............................................................................cef_callback_capi.h
	// Generic callback structure used for asynchronous continuation.
	TCefCallbackRef = class(TCefBaseRef, ICefCallback)
	protected
		// Continue processing.
		procedure Cont;
		// Cancel processing.
		procedure Cancel;
	public
		class function UnWrap(data: Pointer): ICefCallback;
	end;

	// Generic callback structure used for asynchronous completion.
	TCefCompletionCallbackRef = class(TCefBaseRef, ICefCompletionCallback)
	protected
		// Method that will be called once the task is complete.
		procedure OnComplete;
	public
		class function UnWrap(data: Pointer): ICefCompletionCallback;
	end;


	//..............................................................................cef_client_capi.h
	// Implement this structure to provide handler implementations.
	TCefClientRef = class(TCefBaseRef, ICefClient)
	protected
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
	public
		class function UnWrap(data: Pointer): ICefClient;
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
	TCefCommandLineRef = class(TCefBaseRef, ICefCommandLine)
	protected
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
	public
		class function UnWrap(data: Pointer): ICefCommandLine;
		// Create a new cef_command_line_t instance.
		class function New: ICefCommandLine;
		// Create a new cef_command_line_t instance.
		// Returns the singleton global cef_command_line_t object. The returned object
		// will be read-only.
		class function GetGlobal: ICefCommandLine;
	end;


	//..............................................................................cef_context_menu_handler_capi.h
	// Implement this structure to handle context menu events. The functions of this
	// structure will be called on the UI thread.
	TCefContextMenuHandlerRef = class(TCefBaseRef, ICefContextMenuHandler)
	protected
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
	public
		class function UnWrap(data: Pointer): ICefContextMenuHandler;
	end;

	// Provides information about the context menu state. The ethods of this
	// structure can only be accessed on browser process the UI thread.
	TCefContextMenuParamsRef = class(TCefBaseRef, ICefContextMenuParams)
	protected
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
	public
		class function UnWrap(data: Pointer): ICefContextMenuParams;
	end;


	//..............................................................................cef_cookie_capi.h
	// Structure used for managing cookies. The functions of this structure may be
	// called on any thread unless otherwise indicated.
	TCefCookieManagerRef = class(TCefBaseRef, ICefCookieManager)
	protected
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
	public
		class function UnWrap(data: Pointer): ICefCookieManager;
		// Returns the global cookie manager. By default data will be stored at
		// CefSettings.cache_path if specified or in memory otherwise. If |callback| is
		// non-NULL it will be executed asnychronously on the IO thread after the
		// manager's storage has been initialized. Using this function is equivalent to
		// calling cef_request_tContext::cef_request_context_get_global_context()->get_d
		// efault_cookie_manager().
		class function GetGlobalManager(const aCallback: ICefCompletionCallback): ICefCookieManager;
		// Returns the global cookie manager. By default data will be stored at
		// CefSettings.cache_path if specified or in memory otherwise. If |callback| is
		// non-NULL it will be executed asnychronously on the IO thread after the
		// manager's storage has been initialized. Using this function is equivalent to
		// calling cef_request_tContext::cef_request_context_get_global_context()->get_d
		// efault_cookie_manager().
		// Creates a new cookie manager. If |path| is NULL data will be stored in memory
		// only. Otherwise, data will be stored at the specified |path|. To persist
		// session cookies (cookies without an expiry date or validity interval) set
		// |persist_session_cookies| to true (1). Session cookies are generally intended
		// to be transient and most Web browsers do not persist them. If |callback| is
		// non-NULL it will be executed asnychronously on the IO thread after the
		// manager's storage has been initialized.
		class function CreateManager(const aPath: ustring; var aPersistSessionCookies: Boolean; const aCallback: ICefCompletionCallback): ICefCookieManager;
	end;

	// Returns the global cookie manager. By default data will be stored at
	// CefSettings.cache_path if specified or in memory otherwise. If |callback| is
	// non-NULL it will be executed asnychronously on the IO thread after the
	// manager's storage has been initialized. Using this function is equivalent to
	// calling cef_request_tContext::cef_request_context_get_global_context()->get_d
	// efault_cookie_manager().
	// Creates a new cookie manager. If |path| is NULL data will be stored in memory
	// only. Otherwise, data will be stored at the specified |path|. To persist
	// session cookies (cookies without an expiry date or validity interval) set
	// |persist_session_cookies| to true (1). Session cookies are generally intended
	// to be transient and most Web browsers do not persist them. If |callback| is
	// non-NULL it will be executed asnychronously on the IO thread after the
	// manager's storage has been initialized.
	// Structure to implement for visiting cookie values. The functions of this
	// structure will always be called on the IO thread.
	TCefCookieVisitorRef = class(TCefBaseRef, ICefCookieVisitor)
	protected
		// Method that will be called once for each cookie. |count| is the 0-based
		// index for the current cookie. |total| is the total number of cookies. Set
		// |deleteCookie| to true (1) to delete the cookie currently being visited.
		// Return false (0) to stop visiting cookies. This function may never be
		// called if no cookies are found.
		function Visit(const aCookie: TWACefCookie; aCount: cint; aTotal: cint; out aDeleteCookie: Boolean): Boolean;
	public
		class function UnWrap(data: Pointer): ICefCookieVisitor;
	end;

	// Structure to implement to be notified of asynchronous completion via
	// cef_cookie_manager_t::set_cookie().
	TCefSetCookieCallbackRef = class(TCefBaseRef, ICefSetCookieCallback)
	protected
		// Method that will be called upon completion. |success| will be true (1) if
		// the cookie was set successfully.
		procedure OnComplete(aSuccess: Boolean);
	public
		class function UnWrap(data: Pointer): ICefSetCookieCallback;
	end;

	// Structure to implement to be notified of asynchronous completion via
	// cef_cookie_manager_t::delete_cookies().
	TCefDeleteCookiesCallbackRef = class(TCefBaseRef, ICefDeleteCookiesCallback)
	protected
		// Method that will be called upon completion. |num_deleted| will be the
		// number of cookies that were deleted or -1 if unknown.
		procedure OnComplete(aNumDeleted: cint);
	public
		class function UnWrap(data: Pointer): ICefDeleteCookiesCallback;
	end;


	//..............................................................................cef_dialog_handler_capi.h
	// Callback structure for asynchronous continuation of file dialog requests.
	TCefFileDialogCallbackRef = class(TCefBaseRef, ICefFileDialogCallback)
	protected
		// Continue the file selection. |selected_accept_filter| should be the 0-based
		// index of the value selected from the accept filters array passed to
		// cef_dialog_handler_t::OnFileDialog. |file_paths| should be a single value
		// or a list of values depending on the dialog mode. An NULL |file_paths|
		// value is treated the same as calling cancel().
		procedure Cont(aSelectedAcceptFilter: cint; aFilePaths: TStrings);
		// Cancel the file selection.
		procedure Cancel;
	public
		class function UnWrap(data: Pointer): ICefFileDialogCallback;
	end;

	// Implement this structure to handle dialog events. The functions of this
	// structure will be called on the browser process UI thread.
	TCefDialogHandlerRef = class(TCefBaseRef, ICefDialogHandler)
	protected
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
	public
		class function UnWrap(data: Pointer): ICefDialogHandler;
	end;

	//..............................................................................cef_display_handler_capi.h
	// Implement this structure to handle events related to browser display state.
	// The functions of this structure will be called on the UI thread.
	TCefDisplayHandlerRef = class(TCefBaseRef, ICefDisplayHandler)
	protected
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
	public
		class function UnWrap(data: Pointer): ICefDisplayHandler;
	end;


	//..............................................................................cef_dom_capi.h
	// Structure to implement for visiting the DOM. The functions of this structure
	// will be called on the render process main thread.
	TCefDomvisitorRef = class(TCefBaseRef, ICefDomvisitor)
	protected
		// Method executed for visiting the DOM. The document object passed to this
		// function represents a snapshot of the DOM at the time this function is
		// executed. DOM objects are only valid for the scope of this function. Do not
		// keep references to or attempt to access any DOM objects outside the scope
		// of this function.
		procedure Visit(const aDocument: ICefDomdocument);
	public
		class function UnWrap(data: Pointer): ICefDomvisitor;
	end;

	// Structure used to represent a DOM document. The functions of this structure
	// should only be called on the render process main thread thread.
	TCefDomdocumentRef = class(TCefBaseRef, ICefDomdocument)
	protected
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
	public
		class function UnWrap(data: Pointer): ICefDomdocument;
	end;

	// Structure used to represent a DOM node. The functions of this structure
	// should only be called on the render process main thread.
	TCefDomnodeRef = class(TCefBaseRef, ICefDomnode)
	protected
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
	public
		class function UnWrap(data: Pointer): ICefDomnode;
	end;


	//..............................................................................cef_download_handler_capi.h
	// Callback structure used to asynchronously continue a download.
	TCefBeforeDownloadCallbackRef = class(TCefBaseRef, ICefBeforeDownloadCallback)
	protected
		// Call to continue the download. Set |download_path| to the full file path
		// for the download including the file name or leave blank to use the
		// suggested name and the default temp directory. Set |show_dialog| to true
		// (1) if you do wish to show the default "Save As" dialog.
		procedure Cont(var aDownloadPath: ustring; aShowDialog: Boolean);
	public
		class function UnWrap(data: Pointer): ICefBeforeDownloadCallback;
	end;

	// Callback structure used to asynchronously cancel a download.
	TCefDownloadItemCallbackRef = class(TCefBaseRef, ICefDownloadItemCallback)
	protected
		// Call to cancel the download.
		procedure Cancel;
		// Call to pause the download.
		procedure Pause;
		// Call to resume the download.
		procedure Resume;
	public
		class function UnWrap(data: Pointer): ICefDownloadItemCallback;
	end;

	// Structure used to handle file downloads. The functions of this structure will
	// called on the browser process UI thread.
	TCefDownloadHandlerRef = class(TCefBaseRef, ICefDownloadHandler)
	protected
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
	public
		class function UnWrap(data: Pointer): ICefDownloadHandler;
	end;


	//..............................................................................cef_download_item_capi.h
	// Structure used to represent a download item.
	TCefDownloadItemRef = class(TCefBaseRef, ICefDownloadItem)
	protected
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
	public
		class function UnWrap(data: Pointer): ICefDownloadItem;
	end;


	//..............................................................................cef_drag_data_capi.h
	// Structure used to represent drag data. The functions of this structure may be
	// called on any thread.
	TCefDragDataRef = class(TCefBaseRef, ICefDragData)
	protected
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
	public
		class function UnWrap(data: Pointer): ICefDragData;
		// Create a new cef_drag_data_t object.
		class function New: ICefDragData;
	end;


	//..............................................................................cef_drag_handler_capi.h
	// Implement this structure to handle events related to dragging. The functions
	// of this structure will be called on the UI thread.
	TCefDragHandlerRef = class(TCefBaseRef, ICefDragHandler)
	protected
		// Called when an external drag event enters the browser window. |dragData|
		// contains the drag event data and |mask| represents the type of drag
		// operation. Return false (0) for default drag handling behavior or true (1)
		// to cancel the drag event.
		function OnDragEnter(const aBrowser: ICefBrowser; const aDragData: ICefDragData; aMask: TCefDragOperationsMask): Boolean;
	public
		class function UnWrap(data: Pointer): ICefDragHandler;
	end;


	//..............................................................................cef_find_handler_capi.h
	// Implement this structure to handle events related to find results. The
	// functions of this structure will be called on the UI thread.
	TCefFindHandlerRef = class(TCefBaseRef, ICefFindHandler)
	protected
		// Called to report find results returned by cef_browser_host_t::find().
		// |identifer| is the identifier passed to find(), |count| is the number of
		// matches currently identified, |selectionRect| is the location of where the
		// match was found (in window coordinates), |activeMatchOrdinal| is the
		// current position in the search results, and |finalUpdate| is true (1) if
		// this is the last find notification.
		procedure OnFindResult(const aBrowser: ICefBrowser; aIdentifier: cint; aCount: cint; const aSelectionRect: TCefRect; aActiveMatchOrdinal: cint; aFinalUpdate: Boolean);
	public
		class function UnWrap(data: Pointer): ICefFindHandler;
	end;


	//..............................................................................cef_focus_handler_capi.h
	// Implement this structure to handle events related to focus. The functions of
	// this structure will be called on the UI thread.
	TCefFocusHandlerRef = class(TCefBaseRef, ICefFocusHandler)
	protected
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
	public
		class function UnWrap(data: Pointer): ICefFocusHandler;
	end;


	//..............................................................................cef_frame_capi.h
	// Structure used to represent a frame in the browser window. When used in the
	// browser process the functions of this structure may be called on any thread
	// unless otherwise indicated in the comments. When used in the render process
	// the functions of this structure may only be called on the main thread.
	TCefFrameRef = class(TCefBaseRef, ICefFrame)
	protected
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
	public
		class function UnWrap(data: Pointer): ICefFrame;
	end;


	//..............................................................................cef_geolocation_capi.h
	// Implement this structure to receive geolocation updates. The functions of
	// this structure will be called on the browser process UI thread.
	TCefGetGeolocationCallbackRef = class(TCefBaseRef, ICefGetGeolocationCallback)
	protected
		// Called with the 'best available' location information or, if the location
		// update failed, with error information.
		procedure OnLocationUpdate(const aPosition: TCefGeoposition);
	public
		class function UnWrap(data: Pointer): ICefGetGeolocationCallback;
	end;


	//..............................................................................cef_geolocation_handler_capi.h
	// Callback structure used for asynchronous continuation of geolocation
	// permission requests.
	TCefGeolocationCallbackRef = class(TCefBaseRef, ICefGeolocationCallback)
	protected
		// Call to allow or deny geolocation access.
		procedure Cont(aAllow: Boolean);
	public
		class function UnWrap(data: Pointer): ICefGeolocationCallback;
	end;

	// Implement this structure to handle events related to geolocation permission
	// requests. The functions of this structure will be called on the browser
	// process UI thread.
	TCefGeolocationHandlerRef = class(TCefBaseRef, ICefGeolocationHandler)
	protected
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
	public
		class function UnWrap(data: Pointer): ICefGeolocationHandler;
	end;


	//..............................................................................cef_jsdialog_handler_capi.h
	// Callback structure used for asynchronous continuation of JavaScript dialog
	// requests.
	TCefJsdialogCallbackRef = class(TCefBaseRef, ICefJsdialogCallback)
	protected
		// Continue the JS dialog request. Set |success| to true (1) if the OK button
		// was pressed. The |user_input| value should be specified for prompt dialogs.
		procedure Cont(aSuccess: Boolean; const aUserInput: ustring);
	public
		class function UnWrap(data: Pointer): ICefJsdialogCallback;
	end;

	// Implement this structure to handle events related to JavaScript dialogs. The
	// functions of this structure will be called on the UI thread.
	TCefJsdialogHandlerRef = class(TCefBaseRef, ICefJsdialogHandler)
	protected
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
	public
		class function UnWrap(data: Pointer): ICefJsdialogHandler;
	end;


	//..............................................................................cef_keyboard_handler_capi.h
	// Implement this structure to handle events related to keyboard input. The
	// functions of this structure will be called on the UI thread.
	TCefKeyboardHandlerRef = class(TCefBaseRef, ICefKeyboardHandler)
	protected
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
	public
		class function UnWrap(data: Pointer): ICefKeyboardHandler;
	end;


	//..............................................................................cef_life_span_handler_capi.h
	// Implement this structure to handle events related to browser life span. The
	// functions of this structure will be called on the UI thread unless otherwise
	// indicated.
	TCefLifeSpanHandlerRef = class(TCefBaseRef, ICefLifeSpanHandler)
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
	public
		class function UnWrap(data: Pointer): ICefLifeSpanHandler;
	end;


	//..............................................................................cef_load_handler_capi.h
	// Implement this structure to handle events related to browser load status. The
	// functions of this structure will be called on the browser process UI thread
	// or render process main thread (TID_RENDERER).
	TCefLoadHandlerRef = class(TCefBaseRef, ICefLoadHandler)
	protected
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
	public
		class function UnWrap(data: Pointer): ICefLoadHandler;
	end;


	//..............................................................................cef_menu_model_capi.h
	// Supports creation and modification of menus. See cef_menu_id_t for the
	// command ids that have default implementations. All user-defined command ids
	// should be between MENU_ID_USER_FIRST and MENU_ID_USER_LAST. The functions of
	// this structure can only be accessed on the browser process the UI thread.
	TCefMenuModelRef = class(TCefBaseRef, ICefMenuModel)
	protected
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
	public
		class function UnWrap(data: Pointer): ICefMenuModel;
	end;


	//..............................................................................cef_navigation_entry_capi.h
	// Structure used to represent an entry in navigation history.
	TCefNavigationEntryRef = class(TCefBaseRef, ICefNavigationEntry)
	protected
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
	public
		class function UnWrap(data: Pointer): ICefNavigationEntry;
	end;


	//..............................................................................cef_origin_whitelist_capi.h

	//..............................................................................cef_parser_capi.h

	//..............................................................................cef_path_util_capi.h

	//..............................................................................cef_print_handler_capi.h
	// Callback structure for asynchronous continuation of print dialog requests.
	TCefPrintDialogCallbackRef = class(TCefBaseRef, ICefPrintDialogCallback)
	protected
		// Continue printing with the specified |settings|.
		procedure Cont(const aSettings: ICefPrintSettings);
		// Cancel the printing.
		procedure Cancel;
	public
		class function UnWrap(data: Pointer): ICefPrintDialogCallback;
	end;

	// Callback structure for asynchronous continuation of print job requests.
	TCefPrintJobCallbackRef = class(TCefBaseRef, ICefPrintJobCallback)
	protected
		// Indicate completion of the print job.
		procedure Cont;
	public
		class function UnWrap(data: Pointer): ICefPrintJobCallback;
	end;

	// Implement this structure to handle printing on Linux. The functions of this
	// structure will be called on the browser process UI thread.
	TCefPrintHandlerRef = class(TCefBaseRef, ICefPrintHandler)
	protected
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
	public
		class function UnWrap(data: Pointer): ICefPrintHandler;
	end;


	//..............................................................................cef_print_settings_capi.h
	// Structure representing print settings.
	TCefPrintSettingsRef = class(TCefBaseRef, ICefPrintSettings)
	protected
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
	public
		class function UnWrap(data: Pointer): ICefPrintSettings;
		// Create a new cef_print_settings_t object.
		class function New: ICefPrintSettings;
	end;


	//..............................................................................cef_process_message_capi.h
	// Structure representing a message. Can be used on any process and thread.
	TCefProcessMessageRef = class(TCefBaseRef, ICefProcessMessage)
	protected
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
	public
		class function UnWrap(data: Pointer): ICefProcessMessage;
		// Create a new cef_process_message_t object with the specified name.
		class function New(const aName: ustring): ICefProcessMessage;
	end;


	//..............................................................................cef_process_util_capi.h

	//..............................................................................cef_render_handler_capi.h
	// Implement this structure to handle events when window rendering is disabled.
	// The functions of this structure will be called on the UI thread.
	TCefRenderHandlerRef = class(TCefBaseRef, ICefRenderHandler)
	protected
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
	public
		class function UnWrap(data: Pointer): ICefRenderHandler;
	end;


	//..............................................................................cef_render_process_handler_capi.h
	// Structure used to implement render process callbacks. The functions of this
	// structure will be called on the render process main thread (TID_RENDERER)
	// unless otherwise indicated.
	TCefRenderProcessHandlerRef = class(TCefBaseRef, ICefRenderProcessHandler)
	protected
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
	public
		class function UnWrap(data: Pointer): ICefRenderProcessHandler;
	end;


	//..............................................................................cef_request_capi.h
	// Structure used to represent a web request. The functions of this structure
	// may be called on any thread.
	TCefRequestRef = class(TCefBaseRef, ICefRequest)
	protected
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
	public
		class function UnWrap(data: Pointer): ICefRequest;
		// Create a new cef_request_t object.
		class function New: ICefRequest;
	end;

	// Create a new cef_request_t object.
	// Structure used to represent post data for a web request. The functions of
	// this structure may be called on any thread.
	TCefPostDataRef = class(TCefBaseRef, ICefPostData)
	protected
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
	public
		class function UnWrap(data: Pointer): ICefPostData;
		// Create a new cef_post_data_t object.
		class function New: ICefPostData;
		// Create a new cef_post_data_element_t object.
		class function ElementCreate: ICefPostDataElement;
	end;

	// Create a new cef_post_data_t object.
	// Structure used to represent a single element in the request post data. The
	// functions of this structure may be called on any thread.
	TCefPostDataElementRef = class(TCefBaseRef, ICefPostDataElement)
	protected
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
	public
		class function UnWrap(data: Pointer): ICefPostDataElement;
		// Create a new cef_post_data_element_t object.
		class function New: ICefPostDataElement;
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
	TCefRequestContextRef=class(TCefBaseRef, ICefRequestContext)
  protected
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
  public
    class function UnWrap(data: Pointer): ICefRequestContext;
    class function GetGlobalContext: ICefRequestContext;
    class function CreateContext(const aSettings: TCefRequestContextSettings; const aHandler: ICefRequestContextHandler): ICefRequestContext;
  end;


	//..............................................................................cef_request_context_handler_capi.h
	// Implement this structure to provide handler implementations. The handler
	// instance will not be released until all objects related to the context have
	// been destroyed.
	TCefRequestContextHandlerRef = class(TCefBaseRef, ICefRequestContextHandler)
	protected
		// Called on the IO thread to retrieve the cookie manager. If this function
		// returns NULL the default cookie manager retrievable via
		// cef_request_tContext::get_default_cookie_manager() will be used.
		function GetCookieManager: ICefCookieManager;
	public
		class function UnWrap(data: Pointer): ICefRequestContextHandler;
	end;


	//..............................................................................cef_request_handler_capi.h
	// Callback structure used for asynchronous continuation of url requests.
	TCefRequestCallbackRef = class(TCefBaseRef, ICefRequestCallback)
	protected
		// Continue the url request. If |allow| is true (1) the request will be
		// continued. Otherwise, the request will be canceled.
		procedure Cont(aAllow: Boolean);
		// Cancel the url request.
		procedure Cancel;
	public
		class function UnWrap(data: Pointer): ICefRequestCallback;
	end;

	// Implement this structure to handle events related to browser requests. The
	// functions of this structure will be called on the thread indicated.
	TCefRequestHandlerRef = class(TCefBaseRef, ICefRequestHandler)
	protected
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
	public
		class function UnWrap(data: Pointer): ICefRequestHandler;
	end;


	//..............................................................................cef_resource_bundle_handler_capi.h
	// Structure used to implement a custom resource bundle structure. The functions
	// of this structure may be called on multiple threads.
	TCefResourceBundleHandlerRef = class(TCefBaseRef, ICefResourceBundleHandler)
	protected
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
	public
		class function UnWrap(data: Pointer): ICefResourceBundleHandler;
	end;


	//..............................................................................cef_resource_handler_capi.h
	// Structure used to implement a custom request handler structure. The functions
	// of this structure will always be called on the IO thread.
	TCefResourceHandlerRef = class(TCefBaseRef, ICefResourceHandler)
	protected
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
	public
		class function UnWrap(data: Pointer): ICefResourceHandler;
	end;


	//..............................................................................cef_response_capi.h
	// Structure used to represent a web response. The functions of this structure
	// may be called on any thread.
	TCefResponseRef = class(TCefBaseRef, ICefResponse)
	protected
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
	public
		class function UnWrap(data: Pointer): ICefResponse;
		// Create a new cef_response_t object.
		class function New: ICefResponse;
	end;


	//..............................................................................cef_scheme_capi.h
	// Structure that manages custom scheme registrations.
	TCefSchemeRegistrarRef = class(TCefBaseRef, ICefSchemeRegistrar)
	protected
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
	public
		class function UnWrap(data: Pointer): ICefSchemeRegistrar;
	end;

	// Structure that creates cef_resource_handler_t instances for handling scheme
	// requests. The functions of this structure will always be called on the IO
	// thread.
	TCefSchemeHandlerFactoryRef = class(TCefBaseRef, ICefSchemeHandlerFactory)
	protected
		// Return a new resource handler instance to handle the request or an NULL
		// reference to allow default handling of the request. |browser| and |frame|
		// will be the browser window and frame respectively that originated the
		// request or NULL if the request did not originate from a browser window (for
		// example, if the request came from cef_urlrequest_t). The |request| object
		// passed to this function will not contain cookie data.
		function New(const aBrowser: ICefBrowser; const aFrame: ICefFrame; const aSchemeName: ustring; const aRequest: ICefRequest): ICefResourceHandler;
	public
		class function UnWrap(data: Pointer): ICefSchemeHandlerFactory;
	end;


	//..............................................................................cef_ssl_info_capi.h
	// Structure representing the issuer or subject field of an X.509 certificate.
	TCefSslcertPrincipalRef = class(TCefBaseRef, ICefSslcertPrincipal)
	protected
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
	public
		class function UnWrap(data: Pointer): ICefSslcertPrincipal;
	end;

	// Structure representing SSL information.
	TCefSslinfoRef = class(TCefBaseRef, ICefSslinfo)
	protected
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
	public
		class function UnWrap(data: Pointer): ICefSslinfo;
	end;


	//..............................................................................cef_stream_capi.h
	// Structure the client can implement to provide a custom stream reader. The
	// functions of this structure may be called on any thread.
	TCefReadHandlerRef = class(TCefBaseRef, ICefReadHandler)
	protected
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
	public
		class function UnWrap(data: Pointer): ICefReadHandler;
	end;

	// Structure used to read data from a stream. The functions of this structure
	// may be called on any thread.
	TCefStreamReaderRef = class(TCefBaseRef, ICefStreamReader)
	protected
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
	public
		class function UnWrap(data: Pointer): ICefStreamReader;
		// Create a new cef_stream_reader_t object from a file.
		class function CreateForFile(const aFileName: ustring): ICefStreamReader;
		// Create a new cef_stream_reader_t object from a file.
		// Create a new cef_stream_reader_t object from data.
		class function CreateForData(var aData: cvoid; aSize: csize_t): ICefStreamReader;
		// Create a new cef_stream_reader_t object from a file.
		// Create a new cef_stream_reader_t object from data.
		// Create a new cef_stream_reader_t object from a custom handler.
		class function CreateForHandler(const aHandler: ICefReadHandler): ICefStreamReader;
	end;

	// Create a new cef_stream_reader_t object from a file.
	// Create a new cef_stream_reader_t object from data.
	// Create a new cef_stream_reader_t object from a custom handler.
	// Structure the client can implement to provide a custom stream writer. The
	// functions of this structure may be called on any thread.
	TCefWriteHandlerRef = class(TCefBaseRef, ICefWriteHandler)
	protected
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
	public
		class function UnWrap(data: Pointer): ICefWriteHandler;
	end;

	// Structure used to write data to a stream. The functions of this structure may
	// be called on any thread.
	TCefStreamWriterRef = class(TCefBaseRef, ICefStreamWriter)
	protected
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
	public
		class function UnWrap(data: Pointer): ICefStreamWriter;
		// Create a new cef_stream_writer_t object for a file.
		class function CreateForFile(const aFileName: ustring): ICefStreamWriter;
		// Create a new cef_stream_writer_t object for a file.
		// Create a new cef_stream_writer_t object for a custom handler.
		class function CreateForHandler(const aHandler: ICefWriteHandler): ICefStreamWriter;
	end;


	//..............................................................................cef_string_visitor_capi.h
	// Implement this structure to receive string values asynchronously.
	TCefStringVisitorRef = class(TCefBaseRef, ICefStringVisitor)
	protected
		// Method that will be executed.
		procedure Visit(const aString: ustring);
	public
		class function UnWrap(data: Pointer): ICefStringVisitor;
	end;


	//..............................................................................cef_task_capi.h
	// Implement this structure for asynchronous task execution. If the task is
	// posted successfully and if the associated message loop is still running then
	// the execute() function will be called on the target thread. If the task fails
	// to post then the task object may be destroyed on the source thread instead of
	// the target thread. For this reason be cautious when performing work in the
	// task object destructor.
	TCefTaskRef = class(TCefBaseRef, ICefTask)
	protected
		// Method that will be executed on the target thread.
		procedure Execute;
	public
		class function UnWrap(data: Pointer): ICefTask;
		// Returns the task runner for the current thread. Only CEF threads will have
		// task runners. An NULL reference will be returned if this function is called
		// on an invalid thread.
		class function RunnerGetForCurrentThread: ICefTaskRunner;
		// Returns the task runner for the current thread. Only CEF threads will have
		// task runners. An NULL reference will be returned if this function is called
		// on an invalid thread.
		// Returns the task runner for the specified CEF thread.
		class function RunnerGetForThread(aThreadId: TCefThreadId): ICefTaskRunner;
	end;

	// Structure that asynchronously executes tasks on the associated thread. It is
	// safe to call the functions of this structure on any thread.
	// CEF maintains multiple internal threads that are used for handling different
	// types of tasks in different processes. The cef_thread_id_t definitions in
	// cef_types.h list the common CEF threads. Task runners are also available for
	// other CEF threads as appropriate (for example, V8 WebWorker threads).
	TCefTaskRunnerRef = class(TCefBaseRef, ICefTaskRunner)
	protected
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
	public
		class function UnWrap(data: Pointer): ICefTaskRunner;
		// Returns the task runner for the current thread. Only CEF threads will have
		// task runners. An NULL reference will be returned if this function is called
		// on an invalid thread.
		class function GetForCurrentThread: ICefTaskRunner;
		// Returns the task runner for the current thread. Only CEF threads will have
		// task runners. An NULL reference will be returned if this function is called
		// on an invalid thread.
		// Returns the task runner for the specified CEF thread.
		class function GetForThread(aThreadId: TCefThreadId): ICefTaskRunner;
	end;


	//..............................................................................cef_trace_capi.h
	// Implement this structure to receive notification when tracing has completed.
	// The functions of this structure will be called on the browser process UI
	// thread.
	TCefEndTracingCallbackRef = class(TCefBaseRef, ICefEndTracingCallback)
	protected
		// Called after all processes have sent their trace data. |tracing_file| is
		// the path at which tracing data was written. The client is responsible for
		// deleting |tracing_file|.
		procedure OnEndTracingComplete(const aTracingFile: ustring);
	public
		class function UnWrap(data: Pointer): ICefEndTracingCallback;
	end;


	//..............................................................................cef_urlrequest_capi.h
	// Structure used to make a URL request. URL requests are not associated with a
	// browser instance so no cef_client_t callbacks will be executed. URL requests
	// can be created on any valid CEF thread in either the browser or render
	// process. Once created the functions of the URL request object must be
	// accessed on the same thread that created it.
	TCefUrlrequestRef = class(TCefBaseRef, ICefUrlrequest)
	protected
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
	public
		class function UnWrap(data: Pointer): ICefUrlrequest;
		// Create a new URL request. Only GET, POST, HEAD, DELETE and PUT request
		// functions are supported. Multiple post data elements are not supported and
		// elements of type PDE_TYPE_FILE are only supported for requests originating
		// from the browser process. Requests originating from the render process will
		// receive the same handling as requests originating from Web content -- if the
		// response contains Content-Disposition or Mime-Type header values that would
		// not normally be rendered then the response may receive special handling
		// inside the browser (for example, via the file download code path instead of
		// the URL request code path). The |request| object will be marked as read-only
		// after calling this function. In the browser process if |request_context| is
		// NULL the global request context will be used. In the render process
		// |request_context| must be NULL and the context associated with the current
		// renderer process' browser will be used.
		class function New(const aRequest: ICefRequest; const aClient: ICefUrlrequestClient; const aRequestContext: ICefRequestContext): ICefUrlrequest;
	end;

	// Create a new URL request. Only GET, POST, HEAD, DELETE and PUT request
	// functions are supported. Multiple post data elements are not supported and
	// elements of type PDE_TYPE_FILE are only supported for requests originating
	// from the browser process. Requests originating from the render process will
	// receive the same handling as requests originating from Web content -- if the
	// response contains Content-Disposition or Mime-Type header values that would
	// not normally be rendered then the response may receive special handling
	// inside the browser (for example, via the file download code path instead of
	// the URL request code path). The |request| object will be marked as read-only
	// after calling this function. In the browser process if |request_context| is
	// NULL the global request context will be used. In the render process
	// |request_context| must be NULL and the context associated with the current
	// renderer process' browser will be used.
	// Structure that should be implemented by the cef_urlrequest_t client. The
	// functions of this structure will be called on the same thread that created
	// the request unless otherwise documented.
	TCefUrlrequestClientRef = class(TCefBaseRef, ICefUrlrequestClient)
	protected
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
	public
		class function UnWrap(data: Pointer): ICefUrlrequestClient;
	end;


	//..............................................................................cef_v8_capi.h
	// Structure representing a V8 context handle. V8 handles can only be accessed
	// from the thread on which they are created. Valid threads for creating a V8
	// handle include the render process main thread (TID_RENDERER) and WebWorker
	// threads. A task runner for posting tasks on the associated thread can be
	// retrieved via the cef_v8context_t::get_task_runner() function.
	TCefV8contextRef = class(TCefBaseRef, ICefV8context)
	protected
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
	public
		class function UnWrap(data: Pointer): ICefV8context;
		// Returns the current (top) context object in the V8 context stack.
		class function GetCurrentContext: ICefV8context;
		// Returns the current (top) context object in the V8 context stack.
		// Returns the entered (bottom) context object in the V8 context stack.
		class function GetEnteredContext: ICefV8context;
		// Returns the current (top) context object in the V8 context stack.
		// Returns the entered (bottom) context object in the V8 context stack.
		// Returns true (1) if V8 is currently inside a context.
		class function InContext: Boolean;
	end;

	// Returns the current (top) context object in the V8 context stack.
	// Returns the entered (bottom) context object in the V8 context stack.
	// Returns true (1) if V8 is currently inside a context.
	// Structure that should be implemented to handle V8 function calls. The
	// functions of this structure will be called on the thread associated with the
	// V8 function.
	TCefV8handlerRef = class(TCefBaseRef, ICefV8handler)
	protected
		// Handle execution of the function identified by |name|. |object| is the
		// receiver ('this' object) of the function. |arguments| is the list of
		// arguments passed to the function. If execution succeeds set |retval| to the
		// function return value. If execution fails set |exception| to the exception
		// that will be thrown. Return true (1) if execution was handled.
		function Execute(const aName: ustring; const aObject: ICefV8value; aArgumentsCount: csize_t; const aArguments: TCefv8ValueArray; var aRetval: ICefV8value; var aException: ustring): Boolean;
	public
		class function UnWrap(data: Pointer): ICefV8handler;
	end;

	// Structure that should be implemented to handle V8 accessor calls. Accessor
	// identifiers are registered by calling cef_v8value_t::set_value(). The
	// functions of this structure will be called on the thread associated with the
	// V8 accessor.
	TCefV8accessorRef = class(TCefBaseRef, ICefV8accessor)
	protected
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
	public
		class function UnWrap(data: Pointer): ICefV8accessor;
	end;

	// Structure representing a V8 exception. The functions of this structure may be
	// called on any render process thread.
	TCefV8exceptionRef = class(TCefBaseRef, ICefV8exception)
	protected
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
	public
		class function UnWrap(data: Pointer): ICefV8exception;
	end;

	// Structure representing a V8 value handle. V8 handles can only be accessed
	// from the thread on which they are created. Valid threads for creating a V8
	// handle include the render process main thread (TID_RENDERER) and WebWorker
	// threads. A task runner for posting tasks on the associated thread can be
	// retrieved via the cef_v8context_t::get_task_runner() function.
	TCefV8valueRef = class(TCefBaseRef, ICefV8value)
	protected
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
	public
		class function UnWrap(data: Pointer): ICefV8value;
		// Create a new cef_v8value_t object of type undefined.
		class function CreateUndefined: ICefV8value;
		// Create a new cef_v8value_t object of type null.
		class function CreateNull: ICefV8value;
		// Create a new cef_v8value_t object of type bool.
		class function CreateBool(aValue: Boolean): ICefV8value;
		// Create a new cef_v8value_t object of type int.
		class function CreateInt(aValue: cint32): ICefV8value;
		// Create a new cef_v8value_t object of type unsigned int.
		class function CreateUint(aValue: cuint32): ICefV8value;
		// Create a new cef_v8value_t object of type double.
		class function CreateDouble(aValue: cdouble): ICefV8value;
    class function CreateDate(aDate: TCefTime): ICefv8Value;
		// Create a new cef_v8value_t object of type Date. This function should only be
		// called from within the scope of a cef_render_process_handler_t,
		// cef_v8handler_t or cef_v8accessor_t callback, or in combination with calling
		// enter() and exit() on a stored cef_v8context_t reference.
		// Create a new cef_v8value_t object of type string.
		class function CreateString(const aValue: ustring): ICefV8value;
		// Create a new cef_v8value_t object of type object with optional accessor. This
		// function should only be called from within the scope of a
		// cef_render_process_handler_t, cef_v8handler_t or cef_v8accessor_t callback,
		// or in combination with calling enter() and exit() on a stored cef_v8context_t
		// reference.
		class function CreateObject(const aAccessor: ICefV8accessor): ICefV8value;
		// Create a new cef_v8value_t object of type array with the specified |length|.
		// If |length| is negative the returned array will have length 0. This function
		// should only be called from within the scope of a
		// cef_render_process_handler_t, cef_v8handler_t or cef_v8accessor_t callback,
		// or in combination with calling enter() and exit() on a stored cef_v8context_t
		// reference.
		class function CreateArray(aLength: cint): ICefV8value;
		// Create a new cef_v8value_t object of type function. This function should only
		// be called from within the scope of a cef_render_process_handler_t,
		// cef_v8handler_t or cef_v8accessor_t callback, or in combination with calling
		// enter() and exit() on a stored cef_v8context_t reference.
		class function CreateFunction(const aName: ustring; const aHandler: ICefV8handler): ICefV8value;
	end;

	// Structure representing a V8 stack trace handle. V8 handles can only be
	// accessed from the thread on which they are created. Valid threads for
	// creating a V8 handle include the render process main thread (TID_RENDERER)
	// and WebWorker threads. A task runner for posting tasks on the associated
	// thread can be retrieved via the cef_v8context_t::get_task_runner() function.
	TCefV8stackTraceRef = class(TCefBaseRef, ICefV8stackTrace)
	protected
		// Returns true (1) if the underlying handle is valid and it can be accessed
		// on the current thread. Do not call any other functions if this function
		// returns false (0).
		function IsValid: Boolean;
		// Returns the number of stack frames.
		function GetFrameCount: cint;
		// Returns the stack frame at the specified 0-based index.
		function GetFrame(aIndex: cint): ICefV8stackFrame;
	public
		class function UnWrap(data: Pointer): ICefV8stackTrace;
		// Returns the stack trace for the currently active context. |frame_limit| is
		// the maximum number of frames that will be captured.
		class function GetCurrent(aFrameLimit: cint): ICefV8stackTrace;
	end;

	// Returns the stack trace for the currently active context. |frame_limit| is
	// the maximum number of frames that will be captured.
	// Structure representing a V8 stack frame handle. V8 handles can only be
	// accessed from the thread on which they are created. Valid threads for
	// creating a V8 handle include the render process main thread (TID_RENDERER)
	// and WebWorker threads. A task runner for posting tasks on the associated
	// thread can be retrieved via the cef_v8context_t::get_task_runner() function.
	TCefV8stackFrameRef = class(TCefBaseRef, ICefV8stackFrame)
	protected
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
	public
		class function UnWrap(data: Pointer): ICefV8stackFrame;
	end;


	//..............................................................................cef_values_capi.h
	// Structure that wraps other data value types. Complex types (binary,
	// dictionary and list) will be referenced but not owned by this object. Can be
	// used on any process and thread.
	TCefValueRef = class(TCefBaseRef, ICefValue)
	protected
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
	public
		class function UnWrap(data: Pointer): ICefValue;
		// Creates a new object.
		class function New: ICefValue;
	end;

	// Creates a new object.
	// Structure representing a binary value. Can be used on any process and thread.
	TCefBinaryValueRef = class(TCefBaseRef, ICefBinaryValue)
	protected
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
	public
		class function UnWrap(data: Pointer): ICefBinaryValue;
		// Creates a new object that is not owned by any other object. The specified
		// |data| will be copied.
		class function New(var aData: cvoid; aDataSize: csize_t): ICefBinaryValue;
	end;

	// Creates a new object that is not owned by any other object. The specified
	// |data| will be copied.
	// Structure representing a dictionary value. Can be used on any process and
	// thread.
	TCefDictionaryValueRef = class(TCefBaseRef, ICefDictionaryValue)
	protected
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
	public
		class function UnWrap(data: Pointer): ICefDictionaryValue;
		// Creates a new object that is not owned by any other object.
		class function New: ICefDictionaryValue;
	end;

	// Creates a new object that is not owned by any other object.
	// Structure representing a list value. Can be used on any process and thread.
	TCefListValueRef = class(TCefBaseRef, ICefListValue)
	protected
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
	public
		class function UnWrap(data: Pointer): ICefListValue;
		// Creates a new object that is not owned by any other object.
		class function New: ICefListValue;
	end;


	//..............................................................................cef_web_plugin_capi.h
	// Information about a specific web plugin.
	TCefWebPluginInfoRef = class(TCefBaseRef, ICefWebPluginInfo)
	protected
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
	public
		class function UnWrap(data: Pointer): ICefWebPluginInfo;
	end;

	// Structure to implement for visiting web plugin information. The functions of
	// this structure will be called on the browser process UI thread.
	TCefWebPluginInfoVisitorRef = class(TCefBaseRef, ICefWebPluginInfoVisitor)
	protected
		// Method that will be called once for each plugin. |count| is the 0-based
		// index for the current plugin. |total| is the total number of plugins.
		// Return false (0) to stop visiting plugins. This function may never be
		// called if no plugins are found.
		function Visit(const aInfo: ICefWebPluginInfo; aCount: cint; aTotal: cint): Boolean;
	public
		class function UnWrap(data: Pointer): ICefWebPluginInfoVisitor;
	end;

	// Structure to implement for receiving unstable plugin information. The
	// functions of this structure will be called on the browser process IO thread.
	TCefWebPluginUnstableCallbackRef = class(TCefBaseRef, ICefWebPluginUnstableCallback)
	protected
		// Method that will be called for the requested plugin. |unstable| will be
		// true (1) if the plugin has reached the crash count threshold of 3 times in
		// 120 seconds.
		procedure IsUnstable(const aPath: ustring; aUnstable: Boolean);
	public
		class function UnWrap(data: Pointer): ICefWebPluginUnstableCallback;
	end;


	//..............................................................................cef_xml_reader_capi.h
	// Structure that supports the reading of XML data via the libxml streaming API.
	// The functions of this structure should only be called on the thread that
	// creates the object.
	TCefXmlReaderRef = class(TCefBaseRef, ICefXmlReader)
	protected
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
	public
		class function UnWrap(data: Pointer): ICefXmlReader;
		// Create a new cef_xml_reader_t object. The returned object's functions can
		// only be called from the thread that created the object.
		class function New(const aStream: ICefStreamReader; aEncodingType: TCefXmlEncodingType; const aURI: ustring): ICefXmlReader;
	end;


	//..............................................................................cef_zip_reader_capi.h
	// Structure that supports the reading of zip archives via the zlib unzip API.
	// The functions of this structure should only be called on the thread that
	// creates the object.
	TCefZipReaderRef = class(TCefBaseRef, ICefZipReader)
	protected
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
	public
		class function UnWrap(data: Pointer): ICefZipReader;
		// Create a new cef_zip_reader_t object. The returned object's functions can
		// only be called from the thread that created the object.
		class function New(const aStream: ICefStreamReader): ICefZipReader;
	end;

implementation

uses
  WACefLib;

//..............................................................................TCefBaseRef
constructor TCefBaseRef.Create(data: Pointer);
begin
  Assert(data <> nil);
  FData := data;
end;

destructor TCefBaseRef.Destroy;
begin
  if Assigned(PCefBase(FData)^.release) then
    PCefBase(FData)^.release(PCefBase(FData));
  inherited;
end;

class function TCefBaseRef.UnWrap(data: Pointer): ICefBase;
begin
  if data <> nil then
    Result := Create(data) as ICefBase
  else
    Result := nil;
end;

function TCefBaseRef.Wrap: Pointer;
begin
  Result := FData;
  if Assigned(PCefBase(FData)^.add_ref) then
    PCefBase(FData)^.add_ref(PCefBase(FData));
end;

//..............................................................................TCefAppRef
{Protected section}
// Provides an opportunity to view and/or modify command-line arguments before
// processing by CEF and Chromium. The |process_type| value will be NULL for
// the browser process. Do not keep a reference to the cef_command_line_t
// object passed to this function. The CefSettings.command_line_args_disabled
// value can be used to start with an NULL command-line object. Any values
// specified in CefSettings that equate to command-line arguments will be set
// before this function is called. Be cautious when using this function to
// modify command-line arguments for non-browser processes as this may result
// in undefined behavior including crashes.
procedure TCefAppRef.OnBeforeCommandLineProcessing(const aProcessType: ustring; const aCommandLine: ICefCommandLine);
var
	process_type_str: TCefString;
begin
	process_type_str := TWACef.ToCefString(aProcessType);
	PCefApp(FData).on_before_command_line_processing(
		PCefApp(FData),
		@process_type_str,
		TWACef.GetData(aCommandLine)
	);
end;

// Provides an opportunity to register custom schemes. Do not keep a reference
// to the |registrar| object. This function is called on the main thread for
// each process and the registered schemes should be the same across all
// processes.
procedure TCefAppRef.OnRegisterCustomSchemes(const aRegistrar: ICefSchemeRegistrar);
begin
	PCefApp(FData).on_register_custom_schemes(
		PCefApp(FData),
		TWACef.GetData(aRegistrar)
	);
end;

// Return the handler for resource bundle events. If
// CefSettings.pack_loading_disabled is true (1) a handler must be returned.
// If no handler is returned resources will be loaded from pack files. This
// function is called by the browser and render processes on multiple threads.
function TCefAppRef.GetResourceBundleHandler: ICefResourceBundleHandler;
begin
	Result := TCefResourceBundleHandlerRef.UnWrap(PCefApp(FData).get_resource_bundle_handler(
		PCefApp(FData)
	));
end;

// Return the handler for functionality specific to the browser process. This
// function is called on multiple threads in the browser process.
function TCefAppRef.GetBrowserProcessHandler: ICefBrowserProcessHandler;
begin
	Result := TCefBrowserProcessHandlerRef.UnWrap(PCefApp(FData).get_browser_process_handler(
		PCefApp(FData)
	));
end;

// Return the handler for functionality specific to the render process. This
// function is called on the render process main thread.
function TCefAppRef.GetRenderProcessHandler: ICefRenderProcessHandler;
begin
	Result := TCefRenderProcessHandlerRef.UnWrap(PCefApp(FData).get_render_process_handler(
		PCefApp(FData)
	));
end;

{Public section}
class function TCefAppRef.UnWrap(data: Pointer): ICefApp;
begin
	if data <> nil then
		Result := Create(data) as ICefApp
	else
		Result := nil;
end;

//..............................................................................TCefAuthCallbackRef
{Protected section}
// Continue the authentication request.
procedure TCefAuthCallbackRef.Cont(const aUsername: ustring; const aPassword: ustring);
var
	username_str: TCefString;
	password_str: TCefString;
begin
	username_str := TWACef.ToCefString(aUsername);
	password_str := TWACef.ToCefString(aPassword);
	PCefAuthCallback(FData).cont(
		PCefAuthCallback(FData),
		@username_str,
		@password_str
	);
end;

// Cancel the authentication request.
procedure TCefAuthCallbackRef.Cancel;
begin
	PCefAuthCallback(FData).cancel(
		PCefAuthCallback(FData)
	);
end;

{Public section}
class function TCefAuthCallbackRef.UnWrap(data: Pointer): ICefAuthCallback;
begin
	if data <> nil then
		Result := Create(data) as ICefAuthCallback
	else
		Result := nil;
end;

//..............................................................................TCefBrowserRef
// Returns the browser host object. This function can only be called in the
// browser process.
function TCefBrowserRef.GetHost: ICefBrowserHost;
begin
	Result := TCefBrowserHostRef.UnWrap(PCefBrowser(FData).get_host(
		PCefBrowser(FData)
	));
end;

// Returns true (1) if the browser can navigate backwards.
function TCefBrowserRef.CanGoBack: Boolean;
begin
	Result := PCefBrowser(FData).can_go_back(
		PCefBrowser(FData)
	) <> 0;
end;

// Navigate backwards.
procedure TCefBrowserRef.GoBack;
begin
	PCefBrowser(FData).go_back(
		PCefBrowser(FData)
	);
end;

// Returns true (1) if the browser can navigate forwards.
function TCefBrowserRef.CanGoForward: Boolean;
begin
	Result := PCefBrowser(FData).can_go_forward(
		PCefBrowser(FData)
	) <> 0;
end;

// Navigate forwards.
procedure TCefBrowserRef.GoForward;
begin
	PCefBrowser(FData).go_forward(
		PCefBrowser(FData)
	);
end;

// Returns true (1) if the browser is currently loading.
function TCefBrowserRef.IsLoading: Boolean;
begin
	Result := PCefBrowser(FData).is_loading(
		PCefBrowser(FData)
	) <> 0;
end;

// Reload the current page.
procedure TCefBrowserRef.Reload;
begin
	PCefBrowser(FData).reload(
		PCefBrowser(FData)
	);
end;

// Reload the current page ignoring any cached data.
procedure TCefBrowserRef.ReloadIgnoreCache;
begin
	PCefBrowser(FData).reload_ignore_cache(
		PCefBrowser(FData)
	);
end;

// Stop loading the page.
procedure TCefBrowserRef.StopLoad;
begin
	PCefBrowser(FData).stop_load(
		PCefBrowser(FData)
	);
end;

// Returns the globally unique identifier for this browser.
function TCefBrowserRef.GetIdentifier: cint;
begin
	Result := PCefBrowser(FData).get_identifier(
		PCefBrowser(FData)
	);
end;

// Returns true (1) if this object is pointing to the same handle as |that|
// object.
function TCefBrowserRef.IsSame(const aThat: ICefBrowser): Boolean;
begin
	Result := PCefBrowser(FData).is_same(
		PCefBrowser(FData),
		TWACef.GetData(aThat)
	) <> 0;
end;

// Returns true (1) if the window is a popup window.
function TCefBrowserRef.IsPopup: Boolean;
begin
	Result := PCefBrowser(FData).is_popup(
		PCefBrowser(FData)
	) <> 0;
end;

// Returns true (1) if a document has been loaded in the browser.
function TCefBrowserRef.HasDocument: Boolean;
begin
	Result := PCefBrowser(FData).has_document(
		PCefBrowser(FData)
	) <> 0;
end;

// Returns the main (top-level) frame for the browser window.
function TCefBrowserRef.GetMainFrame: ICefFrame;
begin
	Result := TCefFrameRef.UnWrap(PCefBrowser(FData).get_main_frame(
		PCefBrowser(FData)
	));
end;

// Returns the focused frame for the browser window.
function TCefBrowserRef.GetFocusedFrame: ICefFrame;
begin
	Result := TCefFrameRef.UnWrap(PCefBrowser(FData).get_focused_frame(
		PCefBrowser(FData)
	));
end;

// Returns the frame with the specified identifier, or NULL if not found.
function TCefBrowserRef.GetFrameByident(aIdentifier: cint64): ICefFrame;
begin
	Result := TCefFrameRef.UnWrap(PCefBrowser(FData).get_frame_byident(
		PCefBrowser(FData),
		aIdentifier
	));
end;

// Returns the frame with the specified name, or NULL if not found.
function TCefBrowserRef.GetFrame(const aName: ustring): ICefFrame;
var
	name_str: TCefString;
begin
	name_str := TWACef.ToCefString(aName);
	Result := TCefFrameRef.UnWrap(PCefBrowser(FData).get_frame(
		PCefBrowser(FData),
		@name_str
	));
end;

// Returns the number of frames that currently exist.
function TCefBrowserRef.GetFrameCount: csize_t;
begin
	Result := PCefBrowser(FData).get_frame_count(
		PCefBrowser(FData)
	);
end;

// Returns the identifiers of all existing frames.
procedure TCefBrowserRef.GetFrameIdentifiers(var aIdentifiersCount: csize_t; var aIdentifiers: cint64);
var
	identifiersCount_proxy: csize_t;
	identifiers_proxy: cint64;
begin
	identifiersCount_proxy := aIdentifiersCount;
	identifiers_proxy := aIdentifiers;
	PCefBrowser(FData).get_frame_identifiers(
		PCefBrowser(FData),
		@identifiersCount_proxy,
		@identifiers_proxy
	);
	aIdentifiersCount := identifiersCount_proxy;
	aIdentifiers := identifiers_proxy;
end;

// Returns the names of all existing frames.
procedure TCefBrowserRef.GetFrameNames(aNames: TStrings);
var
	names_list: TCefStringList;
	names_iter: Integer;
	names_item: TCefString;
begin
	names_list := cef_string_list_alloc();
	for names_iter := 0 to aNames.Count - 1 do
	begin
		names_item := TWACef.ToCefString(aNames[names_iter]);
		cef_string_list_append(names_list, @names_item);
	end;
	PCefBrowser(FData).get_frame_names(
		PCefBrowser(FData),
		names_list
	);
	aNames.Clear;
	FillChar(names_item, SizeOf(names_item), 0);
	for names_iter := 0 to cef_string_list_size(names_list) - 1 do
	begin
		FillChar(names_item, SizeOf(names_item), 0);
		cef_string_list_value(names_list, names_iter, @names_item);
		aNames.Add(TWACef.StringClearAndGet(names_item));
	end;
  cef_string_list_free(names_list);
end;

//
// Send a message to the specified |target_process|. Returns true (1) if the
// message was sent successfully.
function TCefBrowserRef.SendProcessMessage(aTargetProcess: TCefProcessId; const aMessage: ICefProcessMessage): Boolean;
begin
	Result := PCefBrowser(FData).send_process_message(
		PCefBrowser(FData),
		aTargetProcess,
		TWACef.GetData(aMessage)
	) <> 0;
end;

{Public section}
class function TCefBrowserRef.UnWrap(data: Pointer): ICefBrowser;
begin
	if data <> nil then
		Result := Create(data) as ICefBrowser
	else
		Result := nil;
end;
// Create a new browser window using the window parameters specified by
// |windowInfo|. All values will be copied internally and the actual window will
// be created on the UI thread. If |request_context| is NULL the global request
// context will be used. This function can be called on any browser process
// thread and will not block.
class function TCefBrowserRef.HostCreateBrowser(const aWindowInfo: TCefWindowInfo; const aClient: ICefClient; const aUrl: ustring; var aSettings: TCefBrowserSettings; const aRequestContext: ICefRequestContext): cint;
var
	url_str: TCefString;
	settings_proxy: TCefBrowserSettings;
begin
	url_str := TWACef.ToCefString(aUrl);
	settings_proxy := aSettings;
	Result := cef_browser_host_create_browser(
		@aWindowInfo,
		TWACef.GetData(aClient),
		@url_str,
		@settings_proxy,
		TWACef.GetData(aRequestContext)
	);
	aSettings := settings_proxy;
end;

// Create a new browser window using the window parameters specified by
// |windowInfo|. All values will be copied internally and the actual window will
// be created on the UI thread. If |request_context| is NULL the global request
// context will be used. This function can be called on any browser process
// thread and will not block.
// Create a new browser window using the window parameters specified by
// |windowInfo|. If |request_context| is NULL the global request context will be
// used. This function can only be called on the browser process UI thread.
class function TCefBrowserRef.HostCreateBrowserSync(const aWindowInfo: TCefWindowInfo; const aClient: ICefClient; const aUrl: ustring; var aSettings: TCefBrowserSettings; const aRequestContext: ICefRequestContext): ICefBrowser;
var
	url_str: TCefString;
	settings_proxy: TCefBrowserSettings;
begin
	url_str := TWACef.ToCefString(aUrl);
	settings_proxy := aSettings;
	Result := TCefBrowserRef.UnWrap(cef_browser_host_create_browser_sync(
		@aWindowInfo,
		TWACef.GetData(aClient),
		@url_str,
		@settings_proxy,
		TWACef.GetData(aRequestContext)
	));
	aSettings := settings_proxy;
end;

//..............................................................................TCefRunFileDialogCallbackRef
{Protected section}
// Called asynchronously after the file dialog is dismissed.
// |selected_accept_filter| is the 0-based index of the value selected from
// the accept filters array passed to cef_browser_host_t::RunFileDialog.
// |file_paths| will be a single value or a list of values depending on the
// dialog mode. If the selection was cancelled |file_paths| will be NULL.
procedure TCefRunFileDialogCallbackRef.OnFileDialogDismissed(aSelectedAcceptFilter: cint; aFilePaths: TStrings);
var
	file_paths_list: TCefStringList;
	file_paths_iter: Integer;
	file_paths_item: TCefString;
begin
	file_paths_list := cef_string_list_alloc();
	for file_paths_iter := 0 to aFilePaths.Count - 1 do
	begin
		file_paths_item := TWACef.ToCefString(aFilePaths[file_paths_iter]);
		cef_string_list_append(file_paths_list, @file_paths_item);
	end;
	PCefRunFileDialogCallback(FData).on_file_dialog_dismissed(
		PCefRunFileDialogCallback(FData),
		aSelectedAcceptFilter,
		file_paths_list
	);
	aFilePaths.Clear;
	FillChar(file_paths_item, SizeOf(file_paths_item), 0);
	for file_paths_iter := 0 to cef_string_list_size(file_paths_list) - 1 do
	begin
		FillChar(file_paths_item, SizeOf(file_paths_item), 0);
		cef_string_list_value(file_paths_list, file_paths_iter, @file_paths_item);
		aFilePaths.Add(TWACef.StringClearAndGet(file_paths_item));
	end;
	cef_string_list_free(file_paths_list);
end;

{Public section}
class function TCefRunFileDialogCallbackRef.UnWrap(data: Pointer): ICefRunFileDialogCallback;
begin
	if data <> nil then
		Result := Create(data) as ICefRunFileDialogCallback
	else
		Result := nil;
end;
//..............................................................................TCefNavigationEntryVisitorRef
{Protected section}
// Method that will be executed. Do not keep a reference to |entry| outside of
// this callback. Return true (1) to continue visiting entries or false (0) to
// stop. |current| is true (1) if this entry is the currently loaded
// navigation entry. |index| is the 0-based index of this entry and |total| is
// the total number of entries.
function TCefNavigationEntryVisitorRef.Visit(const aEntry: ICefNavigationEntry; aCurrent: Boolean; aIndex: cint; aTotal: cint): Boolean;
begin
	Result := PCefNavigationEntryVisitor(FData).visit(
		PCefNavigationEntryVisitor(FData),
		TWACef.GetData(aEntry),
		Ord(aCurrent),
		aIndex,
		aTotal
	) <> 0;
end;

{Public section}
class function TCefNavigationEntryVisitorRef.UnWrap(data: Pointer): ICefNavigationEntryVisitor;
begin
	if data <> nil then
		Result := Create(data) as ICefNavigationEntryVisitor
	else
		Result := nil;
end;

//..............................................................................TCefBrowserHostRef
// Returns the hosted browser object.
function TCefBrowserHostRef.GetBrowser: ICefBrowser;
begin
	Result := TCefBrowserRef.UnWrap(PCefBrowserHost(FData).get_browser(
		PCefBrowserHost(FData)
	));
end;

// Request that the browser close. The JavaScript 'onbeforeunload' event will
// be fired. If |force_close| is false (0) the event handler, if any, will be
// allowed to prompt the user and the user can optionally cancel the close. If
// |force_close| is true (1) the prompt will not be displayed and the close
// will proceed. Results in a call to cef_life_span_handler_t::do_close() if
// the event handler allows the close or if |force_close| is true (1). See
// cef_life_span_handler_t::do_close() documentation for additional usage
// information.
procedure TCefBrowserHostRef.CloseBrowser(aForceClose: Boolean);
begin
	PCefBrowserHost(FData).close_browser(
		PCefBrowserHost(FData),
		Ord(aForceClose)
	);
end;

// Set whether the browser is focused.
procedure TCefBrowserHostRef.SetFocus(aFocus: Boolean);
begin
	PCefBrowserHost(FData).set_focus(
		PCefBrowserHost(FData),
		Ord(aFocus)
	);
end;

// Set whether the window containing the browser is visible
// (minimized/unminimized, app hidden/unhidden, etc). Only used on Mac OS X.
procedure TCefBrowserHostRef.SetWindowVisibility(aVisible: Boolean);
begin
	PCefBrowserHost(FData).set_window_visibility(
		PCefBrowserHost(FData),
		Ord(aVisible)
	);
end;

// Retrieve the window handle for this browser.
function TCefBrowserHostRef.GetWindowHandle: TCefWindowHandle;
begin
	Result := PCefBrowserHost(FData).get_window_handle(
		PCefBrowserHost(FData)
	);
end;

// Retrieve the window handle of the browser that opened this browser. Will
// return NULL for non-popup windows. This function can be used in combination
// with custom handling of modal windows.
function TCefBrowserHostRef.GetOpenerWindowHandle: TCefWindowHandle;
begin
	Result := PCefBrowserHost(FData).get_opener_window_handle(
		PCefBrowserHost(FData)
	);
end;

// Returns the client for this browser.
function TCefBrowserHostRef.GetClient: ICefClient;
begin
	Result := TCefClientRef.UnWrap(PCefBrowserHost(FData).get_client(
		PCefBrowserHost(FData)
	));
end;

// Returns the request context for this browser.
function TCefBrowserHostRef.GetRequestContext: ICefRequestContext;
begin
	Result := TCefRequestContextRef.UnWrap(PCefBrowserHost(FData).get_request_context(
		PCefBrowserHost(FData)
	));
end;

// Get the current zoom level. The default zoom level is 0.0. This function
// can only be called on the UI thread.
function TCefBrowserHostRef.GetZoomLevel: cdouble;
begin
	Result := PCefBrowserHost(FData).get_zoom_level(
		PCefBrowserHost(FData)
	);
end;

// Change the zoom level to the specified value. Specify 0.0 to reset the zoom
// level. If called on the UI thread the change will be applied immediately.
// Otherwise, the change will be applied asynchronously on the UI thread.
procedure TCefBrowserHostRef.SetZoomLevel(aZoomLevel: cdouble);
begin
	PCefBrowserHost(FData).set_zoom_level(
		PCefBrowserHost(FData),
		aZoomLevel
	);
end;

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
procedure TCefBrowserHostRef.RunFileDialog(aMode: TCefFileDialogMode; const aTitle: ustring; const aDefaultFilePath: ustring; aAcceptFilters: TStrings; aSelectedAcceptFilter: cint; const aCallback: ICefRunFileDialogCallback);
var
	title_str: TCefString;
	default_file_path_str: TCefString;
	accept_filters_list: TCefStringList;
	accept_filters_iter: Integer;
	accept_filters_item: TCefString;
begin
	title_str := TWACef.ToCefString(aTitle);
	default_file_path_str := TWACef.ToCefString(aDefaultFilePath);
	accept_filters_list := cef_string_list_alloc();
	for accept_filters_iter := 0 to aAcceptFilters.Count - 1 do
	begin
		accept_filters_item := TWACef.ToCefString(aAcceptFilters[accept_filters_iter]);
		cef_string_list_append(accept_filters_list, @accept_filters_item);
	end;
	PCefBrowserHost(FData).run_file_dialog(
		PCefBrowserHost(FData),
		aMode,
		@title_str,
		@default_file_path_str,
		accept_filters_list,
		aSelectedAcceptFilter,
		TWACef.GetData(aCallback)
	);
	aAcceptFilters.Clear;
	FillChar(accept_filters_item, SizeOf(accept_filters_item), 0);
	for accept_filters_iter := 0 to cef_string_list_size(accept_filters_list) - 1 do
	begin
		FillChar(accept_filters_item, SizeOf(accept_filters_item), 0);
		cef_string_list_value(accept_filters_list, accept_filters_iter, @accept_filters_item);
		aAcceptFilters.Add(TWACef.StringClearAndGet(accept_filters_item));
	end;	cef_string_list_free(accept_filters_list);
end;

procedure TCefBrowserHostRef.RunFileDialogProc(aMode: TCefFileDialogMode; const aTitle: ustring; const aDefaultFilePath: ustring; aAcceptFilters: TStrings; aSelectedAcceptFilter: cint; const aCallback: TCefRunFileDialogCallbackProc);
begin
  RunFileDialog(aMode, aTitle, aDefaultFilePath, aAcceptFilters, aSelectedAcceptFilter, TCefFastRunFileDialogCallback.Create(aCallback));
end;

// Download the file at |url| using cef_download_handler_t.
procedure TCefBrowserHostRef.StartDownload(const aUrl: ustring);
var
	url_str: TCefString;
begin
	url_str := TWACef.ToCefString(aUrl);
	PCefBrowserHost(FData).start_download(
		PCefBrowserHost(FData),
		@url_str
	);
end;

// Print the current browser contents.
procedure TCefBrowserHostRef.Print;
begin
	PCefBrowserHost(FData).print(
		PCefBrowserHost(FData)
	);
end;

// Search for |searchText|. |identifier| can be used to have multiple searches
// running simultaniously. |forward| indicates whether to search forward or
// backward within the page. |matchCase| indicates whether the search should
// be case-sensitive. |findNext| indicates whether this is the first request
// or a follow-up. The cef_find_handler_t instance, if any, returned via
// cef_client_t::GetFindHandler will be called to report find results.
procedure TCefBrowserHostRef.Find(aIdentifier: cint; const aSearchText: ustring; aForward: Boolean; aMatchCase: Boolean; aFindNext: Boolean);
var
	searchText_str: TCefString;
begin
	searchText_str := TWACef.ToCefString(aSearchText);
	PCefBrowserHost(FData).find(
		PCefBrowserHost(FData),
		aIdentifier,
		@searchText_str,
		Ord(aForward),
		Ord(aMatchCase),
		Ord(aFindNext)
	);
end;

// Cancel all searches that are currently going on.
procedure TCefBrowserHostRef.StopFinding(aClearSelection: Boolean);
begin
	PCefBrowserHost(FData).stop_finding(
		PCefBrowserHost(FData),
		Ord(aClearSelection)
	);
end;

// Open developer tools in its own window. If |inspect_element_at| is non-
// NULL the element at the specified (x,y) location will be inspected.
procedure TCefBrowserHostRef.ShowDevTools(var aWindowInfo: TCefWindowInfo; const aClient: ICefClient; var aSettings: TCefBrowserSettings; aInspectElementAt: TCefPoint);
var
	windowInfo_proxy: TCefWindowInfo;
	settings_proxy: TCefBrowserSettings;
	inspect_element_at_proxy: TCefPoint;
begin
	windowInfo_proxy := aWindowInfo;
	settings_proxy := aSettings;
	inspect_element_at_proxy := aInspectElementAt;
	PCefBrowserHost(FData).show_dev_tools(
		PCefBrowserHost(FData),
		@windowInfo_proxy,
		TWACef.GetData(aClient),
		@settings_proxy,
		@inspect_element_at_proxy
	);
	aWindowInfo := windowInfo_proxy;
	aSettings := settings_proxy;
	aInspectElementAt := inspect_element_at_proxy;
end;

procedure TCefBrowserHostRef.ShowDevTools(var aWindowInfo: TCefWindowInfo; const aClient: ICefClient; var aSettings: TCefBrowserSettings);
var
	windowInfo_proxy: TCefWindowInfo;
	settings_proxy: TCefBrowserSettings;
begin
	windowInfo_proxy := aWindowInfo;
	settings_proxy := aSettings;
	PCefBrowserHost(FData).show_dev_tools(
		PCefBrowserHost(FData),
		@windowInfo_proxy,
		TWACef.GetData(aClient),
		@settings_proxy,
		nil
	);
	aWindowInfo := windowInfo_proxy;
	aSettings := settings_proxy;
end;

// Explicitly close the developer tools window if one exists for this browser
// instance.
procedure TCefBrowserHostRef.CloseDevTools;
begin
	PCefBrowserHost(FData).close_dev_tools(
		PCefBrowserHost(FData)
	);
end;

// Retrieve a snapshot of current navigation entries as values sent to the
// specified visitor. If |current_only| is true (1) only the current
// navigation entry will be sent, otherwise all navigation entries will be
// sent.
procedure TCefBrowserHostRef.GetNavigationEntries(const aVisitor: ICefNavigationEntryVisitor; aCurrentOnly: Boolean);
begin
	PCefBrowserHost(FData).get_navigation_entries(
		PCefBrowserHost(FData),
		TWACef.GetData(aVisitor),
		Ord(aCurrentOnly)
	);
end;

// Set whether mouse cursor change is disabled.
procedure TCefBrowserHostRef.SetMouseCursorChangeDisabled(aDisabled: Boolean);
begin
	PCefBrowserHost(FData).set_mouse_cursor_change_disabled(
		PCefBrowserHost(FData),
		Ord(aDisabled)
	);
end;

// Returns true (1) if mouse cursor change is disabled.
function TCefBrowserHostRef.IsMouseCursorChangeDisabled: Boolean;
begin
	Result := PCefBrowserHost(FData).is_mouse_cursor_change_disabled(
		PCefBrowserHost(FData)
	) <> 0;
end;

// If a misspelled word is currently selected in an editable node calling this
// function will replace it with the specified |word|.
procedure TCefBrowserHostRef.ReplaceMisspelling(const aWord: ustring);
var
	word_str: TCefString;
begin
	word_str := TWACef.ToCefString(aWord);
	PCefBrowserHost(FData).replace_misspelling(
		PCefBrowserHost(FData),
		@word_str
	);
end;

// Add the specified |word| to the spelling dictionary.
procedure TCefBrowserHostRef.AddWordToDictionary(const aWord: ustring);
var
	word_str: TCefString;
begin
	word_str := TWACef.ToCefString(aWord);
	PCefBrowserHost(FData).add_word_to_dictionary(
		PCefBrowserHost(FData),
		@word_str
	);
end;

// Returns true (1) if window rendering is disabled.
function TCefBrowserHostRef.IsWindowRenderingDisabled: Boolean;
begin
	Result := PCefBrowserHost(FData).is_window_rendering_disabled(
		PCefBrowserHost(FData)
	) <> 0;
end;

// Notify the browser that the widget has been resized. The browser will first
// call cef_render_handler_t::GetViewRect to get the new size and then call
// cef_render_handler_t::OnPaint asynchronously with the updated regions. This
// function is only used when window rendering is disabled.
procedure TCefBrowserHostRef.WasResized;
begin
	PCefBrowserHost(FData).was_resized(
		PCefBrowserHost(FData)
	);
end;

// Notify the browser that it has been hidden or shown. Layouting and
// cef_render_handler_t::OnPaint notification will stop when the browser is
// hidden. This function is only used when window rendering is disabled.
procedure TCefBrowserHostRef.WasHidden(aHidden: Boolean);
begin
	PCefBrowserHost(FData).was_hidden(
		PCefBrowserHost(FData),
		Ord(aHidden)
	);
end;

// Send a notification to the browser that the screen info has changed. The
// browser will then call cef_render_handler_t::GetScreenInfo to update the
// screen information with the new values. This simulates moving the webview
// window from one display to another, or changing the properties of the
// current display. This function is only used when window rendering is
// disabled.
procedure TCefBrowserHostRef.NotifyScreenInfoChanged;
begin
	PCefBrowserHost(FData).notify_screen_info_changed(
		PCefBrowserHost(FData)
	);
end;

// Invalidate the view. The browser will call cef_render_handler_t::OnPaint
// asynchronously. This function is only used when window rendering is
// disabled.
procedure TCefBrowserHostRef.Invalidate(const aType: TCefPaintElementType);
begin
	PCefBrowserHost(FData).invalidate(
		PCefBrowserHost(FData),
		aType
	);
end;

// Send a key event to the browser.
procedure TCefBrowserHostRef.SendKeyEvent(const aEvent: TCefKeyEvent);
begin
	PCefBrowserHost(FData).send_key_event(
		PCefBrowserHost(FData),
		@aEvent
	);
end;

// Send a mouse click event to the browser. The |x| and |y| coordinates are
// relative to the upper-left corner of the view.
procedure TCefBrowserHostRef.SendMouseClickEvent(const aEvent: TCefMouseEvent; aType: TCefMouseButtonType; aMouseUp: Boolean; aClickCount: cint);
begin
	PCefBrowserHost(FData).send_mouse_click_event(
		PCefBrowserHost(FData),
		@aEvent,
		aType,
		Ord(aMouseUp),
		aClickCount
	);
end;

// Send a mouse move event to the browser. The |x| and |y| coordinates are
// relative to the upper-left corner of the view.
procedure TCefBrowserHostRef.SendMouseMoveEvent(const aEvent: TCefMouseEvent; aMouseLeave: Boolean);
begin
	PCefBrowserHost(FData).send_mouse_move_event(
		PCefBrowserHost(FData),
		@aEvent,
		Ord(aMouseLeave)
	);
end;

// Send a mouse wheel event to the browser. The |x| and |y| coordinates are
// relative to the upper-left corner of the view. The |deltaX| and |deltaY|
// values represent the movement delta in the X and Y directions respectively.
// In order to scroll inside select popups with window rendering disabled
// cef_render_handler_t::GetScreenPoint should be implemented properly.
procedure TCefBrowserHostRef.SendMouseWheelEvent(const aEvent: TCefMouseEvent; aDeltaX: cint; aDeltaY: cint);
begin
	PCefBrowserHost(FData).send_mouse_wheel_event(
		PCefBrowserHost(FData),
		@aEvent,
		aDeltaX,
		aDeltaY
	);
end;

// Send a focus event to the browser.
procedure TCefBrowserHostRef.SendFocusEvent(aSetFocus: Boolean);
begin
	PCefBrowserHost(FData).send_focus_event(
		PCefBrowserHost(FData),
		Ord(aSetFocus)
	);
end;

// Send a capture lost event to the browser.
procedure TCefBrowserHostRef.SendCaptureLostEvent;
begin
	PCefBrowserHost(FData).send_capture_lost_event(
		PCefBrowserHost(FData)
	);
end;

// Notify the browser that the window hosting it is about to be moved or
// resized. This function is only used on Windows and Linux.
procedure TCefBrowserHostRef.NotifyMoveOrResizeStarted;
begin
	PCefBrowserHost(FData).notify_move_or_resize_started(
		PCefBrowserHost(FData)
	);
end;

{$IFDEF MACOS}
// Get the NSTextInputContext implementation forenabling IME on Mac when
// window rendering is disabled.
function TCefBrowserHostRef.GetNstextInputContext: TCefTextInputContext;
begin
	Result := PCefBrowserHost(FData).get_nstext_input_context(
		PCefBrowserHost(FData)
	);
end;

// Handles a keyDown event prior to passing it through the NSTextInputClient
// machinery.
procedure TCefBrowserHostRef.HandleKeyEventBeforeTextInputClient(aKeyEvent: TCefEventHandle);
begin
	PCefBrowserHost(FData).handle_key_event_before_text_input_client(
		PCefBrowserHost(FData),
		aKeyEvent
	);
end;

// Performs any additional actions after NSTextInputClient handles the event.
procedure TCefBrowserHostRef.HandleKeyEventAfterTextInputClient(aKeyEvent: TCefEventHandle);
begin
	PCefBrowserHost(FData).handle_key_event_after_text_input_client(
		PCefBrowserHost(FData),
		aKeyEvent
	);
end;
{$ENDIF}

// Call this function when the user drags the mouse into the web view (before
// calling DragTargetDragOver/DragTargetLeave/DragTargetDrop). |drag_data|
// should not contain file contents as this type of data is not allowed to be
// dragged into the web view. File contents can be removed using
// cef_drag_data_t::ResetFileContents (for example, if |drag_data| comes from
// cef_render_handler_t::StartDragging). This function is only used when
// window rendering is disabled.
procedure TCefBrowserHostRef.DragTargetDragEnter(const aDragData: ICefDragData; const aEvent: TCefMouseEvent; aAllowedOps: TCefDragOperationsMask);
begin
	PCefBrowserHost(FData).drag_target_drag_enter(
		PCefBrowserHost(FData),
		TWACef.GetData(aDragData),
		@aEvent,
		aAllowedOps
	);
end;

// Call this function each time the mouse is moved across the web view during
// a drag operation (after calling DragTargetDragEnter and before calling
// DragTargetDragLeave/DragTargetDrop). This function is only used when window
// rendering is disabled.
procedure TCefBrowserHostRef.DragTargetDragOver(const aEvent: TCefMouseEvent; aAllowedOps: TCefDragOperationsMask);
begin
	PCefBrowserHost(FData).drag_target_drag_over(
		PCefBrowserHost(FData),
		@aEvent,
		aAllowedOps
	);
end;

// Call this function when the user drags the mouse out of the web view (after
// calling DragTargetDragEnter). This function is only used when window
// rendering is disabled.
procedure TCefBrowserHostRef.DragTargetDragLeave;
begin
	PCefBrowserHost(FData).drag_target_drag_leave(
		PCefBrowserHost(FData)
	);
end;

// Call this function when the user completes the drag operation by dropping
// the object onto the web view (after calling DragTargetDragEnter). The
// object being dropped is |drag_data|, given as an argument to the previous
// DragTargetDragEnter call. This function is only used when window rendering
// is disabled.
procedure TCefBrowserHostRef.DragTargetDrop(const aEvent: TCefMouseEvent);
begin
	PCefBrowserHost(FData).drag_target_drop(
		PCefBrowserHost(FData),
		@aEvent
	);
end;

// Call this function when the drag operation started by a
// cef_render_handler_t::StartDragging call has ended either in a drop or by
// being cancelled. |x| and |y| are mouse coordinates relative to the upper-
// left corner of the view. If the web view is both the drag source and the
// drag target then all DragTarget* functions should be called before
// DragSource* mthods. This function is only used when window rendering is
// disabled.
procedure TCefBrowserHostRef.DragSourceEndedAt(aX: cint; aY: cint; aOp: TCefDragOperationsMask);
begin
	PCefBrowserHost(FData).drag_source_ended_at(
		PCefBrowserHost(FData),
		aX,
		aY,
		aOp
	);
end;

// Call this function when the drag operation started by a
// cef_render_handler_t::StartDragging call has completed. This function may
// be called immediately without first calling DragSourceEndedAt to cancel a
// drag operation. If the web view is both the drag source and the drag target
// then all DragTarget* functions should be called before DragSource* mthods.
// This function is only used when window rendering is disabled.
procedure TCefBrowserHostRef.DragSourceSystemDragEnded;
begin
	PCefBrowserHost(FData).drag_source_system_drag_ended(
		PCefBrowserHost(FData)
	);
end;

{Public section}
class function TCefBrowserHostRef.UnWrap(data: Pointer): ICefBrowserHost;
begin
	if data <> nil then
		Result := Create(data) as ICefBrowserHost
	else
		Result := nil;
end;
// Create a new browser window using the window parameters specified by
// |windowInfo|. All values will be copied internally and the actual window will
// be created on the UI thread. If |request_context| is NULL the global request
// context will be used. This function can be called on any browser process
// thread and will not block.
class function TCefBrowserHostRef.CreateBrowser(const aWindowInfo: TCefWindowInfo; const aClient: ICefClient; const aUrl: ustring; var aSettings: TCefBrowserSettings; const aRequestContext: ICefRequestContext): Boolean;
var
	url_str: TCefString;
	settings_proxy: TCefBrowserSettings;
begin
	url_str := TWACef.ToCefString(aUrl);
	settings_proxy := aSettings;
	Result := cef_browser_host_create_browser(
		@aWindowInfo,
		TWACef.GetData(aClient),
		@url_str,
		@settings_proxy,
		TWACef.GetData(aRequestContext)
	) <> 0;
	aSettings := settings_proxy;
end;

// Create a new browser window using the window parameters specified by
// |windowInfo|. All values will be copied internally and the actual window will
// be created on the UI thread. If |request_context| is NULL the global request
// context will be used. This function can be called on any browser process
// thread and will not block.
// Create a new browser window using the window parameters specified by
// |windowInfo|. If |request_context| is NULL the global request context will be
// used. This function can only be called on the browser process UI thread.
class function TCefBrowserHostRef.CreateBrowserSync(const aWindowInfo: TCefWindowInfo; const aClient: ICefClient; const aUrl: ustring; var aSettings: TCefBrowserSettings; const aRequestContext: ICefRequestContext): ICefBrowser;
var
	url_str: TCefString;
	settings_proxy: TCefBrowserSettings;
begin
	url_str := TWACef.ToCefString(aUrl);
	settings_proxy := aSettings;
	Result := TCefBrowserRef.UnWrap(cef_browser_host_create_browser_sync(
		@aWindowInfo,
		TWACef.GetData(aClient),
		@url_str,
		@settings_proxy,
		TWACef.GetData(aRequestContext)
	));
	aSettings := settings_proxy;
end;


//..............................................................................TCefBrowserProcessHandlerRef
{Protected section}
// Called on the browser process UI thread immediately after the CEF context
// has been initialized.
procedure TCefBrowserProcessHandlerRef.OnContextInitialized;
begin
	PCefBrowserProcessHandler(FData).on_context_initialized(
		PCefBrowserProcessHandler(FData)
	);
end;

// Called before a child process is launched. Will be called on the browser
// process UI thread when launching a render process and on the browser
// process IO thread when launching a GPU or plugin process. Provides an
// opportunity to modify the child process command line. Do not keep a
// reference to |command_line| outside of this function.
procedure TCefBrowserProcessHandlerRef.OnBeforeChildProcessLaunch(const aCommandLine: ICefCommandLine);
begin
	PCefBrowserProcessHandler(FData).on_before_child_process_launch(
		PCefBrowserProcessHandler(FData),
		TWACef.GetData(aCommandLine)
	);
end;

// Called on the browser process IO thread after the main thread has been
// created for a new render process. Provides an opportunity to specify extra
// information that will be passed to
// cef_render_process_handler_t::on_render_thread_created() in the render
// process. Do not keep a reference to |extra_info| outside of this function.
procedure TCefBrowserProcessHandlerRef.OnRenderProcessThreadCreated(const aExtraInfo: ICefListValue);
begin
	PCefBrowserProcessHandler(FData).on_render_process_thread_created(
		PCefBrowserProcessHandler(FData),
		TWACef.GetData(aExtraInfo)
	);
end;

// Return the handler for printing on Linux. If a print handler is not
// provided then printing will not be supported on the Linux platform.
function TCefBrowserProcessHandlerRef.GetPrintHandler: ICefPrintHandler;
begin
	Result := TCefPrintHandlerRef.UnWrap(PCefBrowserProcessHandler(FData).get_print_handler(
		PCefBrowserProcessHandler(FData)
	));
end;

{Public section}
class function TCefBrowserProcessHandlerRef.UnWrap(data: Pointer): ICefBrowserProcessHandler;
begin
	if data <> nil then
		Result := Create(data) as ICefBrowserProcessHandler
	else
		Result := nil;
end;

//..............................................................................TCefCallbackRef
{Protected section}
// Continue processing.
procedure TCefCallbackRef.Cont;
begin
	PCefCallback(FData).cont(
		PCefCallback(FData)
	);
end;

// Cancel processing.
procedure TCefCallbackRef.Cancel;
begin
	PCefCallback(FData).cancel(
		PCefCallback(FData)
	);
end;

{Public section}
class function TCefCallbackRef.UnWrap(data: Pointer): ICefCallback;
begin
	if data <> nil then
		Result := Create(data) as ICefCallback
	else
		Result := nil;
end;
//..............................................................................TCefCompletionCallbackRef
{Protected section}
// Method that will be called once the task is complete.
procedure TCefCompletionCallbackRef.OnComplete;
begin
	PCefCompletionCallback(FData).on_complete(
		PCefCompletionCallback(FData)
	);
end;

{Public section}
class function TCefCompletionCallbackRef.UnWrap(data: Pointer): ICefCompletionCallback;
begin
	if data <> nil then
		Result := Create(data) as ICefCompletionCallback
	else
		Result := nil;
end;

//..............................................................................TCefClientRef
{Protected section}
// Return the handler for context menus. If no handler is provided the default
// implementation will be used.
function TCefClientRef.GetContextMenuHandler: ICefContextMenuHandler;
begin
	Result := TCefContextMenuHandlerRef.UnWrap(PCefClient(FData).get_context_menu_handler(
		PCefClient(FData)
	));
end;

// Return the handler for dialogs. If no handler is provided the default
// implementation will be used.
function TCefClientRef.GetDialogHandler: ICefDialogHandler;
begin
	Result := TCefDialogHandlerRef.UnWrap(PCefClient(FData).get_dialog_handler(
		PCefClient(FData)
	));
end;

// Return the handler for browser display state events.
function TCefClientRef.GetDisplayHandler: ICefDisplayHandler;
begin
	Result := TCefDisplayHandlerRef.UnWrap(PCefClient(FData).get_display_handler(
		PCefClient(FData)
	));
end;

// Return the handler for download events. If no handler is returned downloads
// will not be allowed.
function TCefClientRef.GetDownloadHandler: ICefDownloadHandler;
begin
	Result := TCefDownloadHandlerRef.UnWrap(PCefClient(FData).get_download_handler(
		PCefClient(FData)
	));
end;

// Return the handler for drag events.
function TCefClientRef.GetDragHandler: ICefDragHandler;
begin
	Result := TCefDragHandlerRef.UnWrap(PCefClient(FData).get_drag_handler(
		PCefClient(FData)
	));
end;

// Return the handler for find result events.
function TCefClientRef.GetFindHandler: ICefFindHandler;
begin
	Result := TCefFindHandlerRef.UnWrap(PCefClient(FData).get_find_handler(
		PCefClient(FData)
	));
end;

// Return the handler for focus events.
function TCefClientRef.GetFocusHandler: ICefFocusHandler;
begin
	Result := TCefFocusHandlerRef.UnWrap(PCefClient(FData).get_focus_handler(
		PCefClient(FData)
	));
end;

// Return the handler for geolocation permissions requests. If no handler is
// provided geolocation access will be denied by default.
function TCefClientRef.GetGeolocationHandler: ICefGeolocationHandler;
begin
	Result := TCefGeolocationHandlerRef.UnWrap(PCefClient(FData).get_geolocation_handler(
		PCefClient(FData)
	));
end;

// Return the handler for JavaScript dialogs. If no handler is provided the
// default implementation will be used.
function TCefClientRef.GetJsdialogHandler: ICefJsdialogHandler;
begin
	Result := TCefJsdialogHandlerRef.UnWrap(PCefClient(FData).get_jsdialog_handler(
		PCefClient(FData)
	));
end;

// Return the handler for keyboard events.
function TCefClientRef.GetKeyboardHandler: ICefKeyboardHandler;
begin
	Result := TCefKeyboardHandlerRef.UnWrap(PCefClient(FData).get_keyboard_handler(
		PCefClient(FData)
	));
end;

// Return the handler for browser life span events.
function TCefClientRef.GetLifeSpanHandler: ICefLifeSpanHandler;
begin
	Result := TCefLifeSpanHandlerRef.UnWrap(PCefClient(FData).get_life_span_handler(
		PCefClient(FData)
	));
end;

// Return the handler for browser load status events.
function TCefClientRef.GetLoadHandler: ICefLoadHandler;
begin
	Result := TCefLoadHandlerRef.UnWrap(PCefClient(FData).get_load_handler(
		PCefClient(FData)
	));
end;

// Return the handler for off-screen rendering events.
function TCefClientRef.GetRenderHandler: ICefRenderHandler;
begin
	Result := TCefRenderHandlerRef.UnWrap(PCefClient(FData).get_render_handler(
		PCefClient(FData)
	));
end;

// Return the handler for browser request events.
function TCefClientRef.GetRequestHandler: ICefRequestHandler;
begin
	Result := TCefRequestHandlerRef.UnWrap(PCefClient(FData).get_request_handler(
		PCefClient(FData)
	));
end;

// Called when a new message is received from a different process. Return true
// (1) if the message was handled or false (0) otherwise. Do not keep a
// reference to or attempt to access the message outside of this callback.
function TCefClientRef.OnProcessMessageReceived(const aBrowser: ICefBrowser; aSourceProcess: TCefProcessId; const aMessage: ICefProcessMessage): Boolean;
begin
	Result := PCefClient(FData).on_process_message_received(
		PCefClient(FData),
		TWACef.GetData(aBrowser),
		aSourceProcess,
		TWACef.GetData(aMessage)
	) <> 0;
end;

{Public section}
class function TCefClientRef.UnWrap(data: Pointer): ICefClient;
begin
	if data <> nil then
		Result := Create(data) as ICefClient
	else
		Result := nil;
end;

//..............................................................................TCefCommandLineRef
// Returns true (1) if this object is valid. Do not call any other functions
// if this function returns false (0).
function TCefCommandLineRef.IsValid: Boolean;
begin
	Result := PCefCommandLine(FData).is_valid(
		PCefCommandLine(FData)
	) <> 0;
end;

// Returns true (1) if the values of this object are read-only. Some APIs may
// expose read-only objects.
function TCefCommandLineRef.IsReadOnly: Boolean;
begin
	Result := PCefCommandLine(FData).is_read_only(
		PCefCommandLine(FData)
	) <> 0;
end;

// Returns a writable copy of this object.
function TCefCommandLineRef.Copy: ICefCommandLine;
begin
	Result := TCefCommandLineRef.UnWrap(PCefCommandLine(FData).copy(
		PCefCommandLine(FData)
	));
end;

// Initialize the command line with the specified |argc| and |argv| values.
// The first argument must be the name of the program. This function is only
// supported on non-Windows platforms.
procedure TCefCommandLineRef.InitFromArgv(aArgc: cint; const aArgv: PPAnsiChar);
begin
	PCefCommandLine(FData).init_from_argv(
		PCefCommandLine(FData),
		aArgc,
		aArgv
	);
end;

// Initialize the command line with the string returned by calling
// GetCommandLineW(). This function is only supported on Windows.
procedure TCefCommandLineRef.InitFromString(const aCommandLine: ustring);
var
	command_line_str: TCefString;
begin
	command_line_str := TWACef.ToCefString(aCommandLine);
	PCefCommandLine(FData).init_from_string(
		PCefCommandLine(FData),
		@command_line_str
	);
end;

// Reset the command-line switches and arguments but leave the program
// component unchanged.
procedure TCefCommandLineRef.Reset;
begin
	PCefCommandLine(FData).reset(
		PCefCommandLine(FData)
	);
end;

// Retrieve the original command line string as a vector of strings. The argv
// array: { program, [(--|-|/)switch[=value]]*, [--], [argument]* }
procedure TCefCommandLineRef.GetArgv(aArgv: TStrings);
var
	argv_list: TCefStringList;
	argv_iter: Integer;
	argv_item: TCefString;
begin
	argv_list := cef_string_list_alloc();
	for argv_iter := 0 to aArgv.Count - 1 do
	begin
		argv_item := TWACef.ToCefString(aArgv[argv_iter]);
		cef_string_list_append(argv_list, @argv_item);
	end;
	PCefCommandLine(FData).get_argv(
		PCefCommandLine(FData),
		argv_list
	);
	aArgv.Clear;
	FillChar(argv_item, SizeOf(argv_item), 0);
	for argv_iter := 0 to cef_string_list_size(argv_list) - 1 do
	begin
		FillChar(argv_item, SizeOf(argv_item), 0);
		cef_string_list_value(argv_list, argv_iter, @argv_item);
		aArgv.Add(TWACef.StringClearAndGet(argv_item));
	end;	cef_string_list_free(argv_list);
end;

// Constructs and returns the represented command line string. Use this
// function cautiously because quoting behavior is unclear.
// The resulting string must be freed by calling cef_string_userfree_free().
function TCefCommandLineRef.GetCommandLineString: ustring;
begin
	Result := TWACef.StringFreeAndGet(PCefCommandLine(FData).get_command_line_string(
		PCefCommandLine(FData)
	));
end;

// Get the program part of the command line string (the first item).
// The resulting string must be freed by calling cef_string_userfree_free().
function TCefCommandLineRef.GetProgram: ustring;
begin
	Result := TWACef.StringFreeAndGet(PCefCommandLine(FData).get_program(
		PCefCommandLine(FData)
	));
end;

// Set the program part of the command line string (the first item).
procedure TCefCommandLineRef.SetProgram(const aProgram: ustring);
var
	program_str: TCefString;
begin
	program_str := TWACef.ToCefString(aProgram);
	PCefCommandLine(FData).set_program(
		PCefCommandLine(FData),
		@program_str
	);
end;

// Returns true (1) if the command line has switches.
function TCefCommandLineRef.HasSwitches: Boolean;
begin
	Result := PCefCommandLine(FData).has_switches(
		PCefCommandLine(FData)
	) <> 0;
end;

// Returns true (1) if the command line contains the given switch.
function TCefCommandLineRef.HasSwitch(const aName: ustring): Boolean;
var
	name_str: TCefString;
begin
	name_str := TWACef.ToCefString(aName);
	Result := PCefCommandLine(FData).has_switch(
		PCefCommandLine(FData),
		@name_str
	) <> 0;
end;

// Returns the value associated with the given switch. If the switch has no
// value or isn't present this function returns the NULL string.
// The resulting string must be freed by calling cef_string_userfree_free().
function TCefCommandLineRef.GetSwitchValue(const aName: ustring): ustring;
var
	name_str: TCefString;
begin
	name_str := TWACef.ToCefString(aName);
	Result := TWACef.StringFreeAndGet(PCefCommandLine(FData).get_switch_value(
		PCefCommandLine(FData),
		@name_str
	));
end;

// Returns the map of switch names and values. If a switch has no value an
// NULL string is returned.
procedure TCefCommandLineRef.GetSwitches(aSwitches: TStrings);
var
  list: TCefStringList;
  i: Integer;
  str: TCefString;
begin
  list := cef_string_list_alloc;
  try
    PCefCommandLine(FData).get_switches(PCefCommandLine(FData), list);
    for i := 0 to cef_string_list_size(list) - 1 do
    begin
      FillChar(str, SizeOf(str), 0);
      cef_string_list_value(list, i, @str);
      aSwitches.Add(TWACef.StringClearAndGet(str));
    end;
  finally
    cef_string_list_free(list);
  end;
end;

// Add a switch to the end of the command line. If the switch has no value
// pass an NULL value string.
procedure TCefCommandLineRef.AppendSwitch(const aName: ustring);
var
	name_str: TCefString;
begin
	name_str := TWACef.ToCefString(aName);
	PCefCommandLine(FData).append_switch(
		PCefCommandLine(FData),
		@name_str
	);
end;

// Add a switch with the specified value to the end of the command line.
procedure TCefCommandLineRef.AppendSwitchWithValue(const aName: ustring; const aValue: ustring);
var
	name_str: TCefString;
	value_str: TCefString;
begin
	name_str := TWACef.ToCefString(aName);
	value_str := TWACef.ToCefString(aValue);
	PCefCommandLine(FData).append_switch_with_value(
		PCefCommandLine(FData),
		@name_str,
		@value_str
	);
end;

// True if there are remaining command line arguments.
function TCefCommandLineRef.HasArguments: Boolean;
begin
	Result := PCefCommandLine(FData).has_arguments(
		PCefCommandLine(FData)
	) <> 0;
end;

// Get the remaining command line arguments.
procedure TCefCommandLineRef.GetArguments(aArguments: TStrings);
var
	arguments_list: TCefStringList;
	arguments_iter: Integer;
	arguments_item: TCefString;
begin
	arguments_list := cef_string_list_alloc();
	for arguments_iter := 0 to aArguments.Count - 1 do
	begin
		arguments_item := TWACef.ToCefString(aArguments[arguments_iter]);
		cef_string_list_append(arguments_list, @arguments_item);
	end;
	PCefCommandLine(FData).get_arguments(
		PCefCommandLine(FData),
		arguments_list
	);
	aArguments.Clear;
	FillChar(arguments_item, SizeOf(arguments_item), 0);
	for arguments_iter := 0 to cef_string_list_size(arguments_list) - 1 do
	begin
		FillChar(arguments_item, SizeOf(arguments_item), 0);
		cef_string_list_value(arguments_list, arguments_iter, @arguments_item);
		aArguments.Add(TWACef.StringClearAndGet(arguments_item));
	end;	cef_string_list_free(arguments_list);
end;

// Add an argument to the end of the command line.
procedure TCefCommandLineRef.AppendArgument(const aArgument: ustring);
var
	argument_str: TCefString;
begin
	argument_str := TWACef.ToCefString(aArgument);
	PCefCommandLine(FData).append_argument(
		PCefCommandLine(FData),
		@argument_str
	);
end;

// Insert a command before the current command. Common for debuggers, like
// "valgrind" or "gdb --args".
procedure TCefCommandLineRef.PrependWrapper(const aWrapper: ustring);
var
	wrapper_str: TCefString;
begin
	wrapper_str := TWACef.ToCefString(aWrapper);
	PCefCommandLine(FData).prepend_wrapper(
		PCefCommandLine(FData),
		@wrapper_str
	);
end;

{Public section}
class function TCefCommandLineRef.UnWrap(data: Pointer): ICefCommandLine;
begin
	if data <> nil then
		Result := Create(data) as ICefCommandLine
	else
		Result := nil;
end;
// Create a new cef_command_line_t instance.
class function TCefCommandLineRef.New: ICefCommandLine;
begin
	Result := TCefCommandLineRef.UnWrap(cef_command_line_create(
	));
end;

// Create a new cef_command_line_t instance.
// Returns the singleton global cef_command_line_t object. The returned object
// will be read-only.
class function TCefCommandLineRef.GetGlobal: ICefCommandLine;
begin
	Result := TCefCommandLineRef.UnWrap(cef_command_line_get_global(
	));
end;


//..............................................................................TCefContextMenuHandlerRef
{Protected section}
// Called before a context menu is displayed. |params| provides information
// about the context menu state. |model| initially contains the default
// context menu. The |model| can be cleared to show no context menu or
// modified to show a custom menu. Do not keep references to |params| or
// |model| outside of this callback.
procedure TCefContextMenuHandlerRef.OnBeforeContextMenu(const aBrowser: ICefBrowser; const aFrame: ICefFrame; const aParams: ICefContextMenuParams; const aModel: ICefMenuModel);
begin
	PCefContextMenuHandler(FData).on_before_context_menu(
		PCefContextMenuHandler(FData),
		TWACef.GetData(aBrowser),
		TWACef.GetData(aFrame),
		TWACef.GetData(aParams),
		TWACef.GetData(aModel)
	);
end;

// Called to execute a command selected from the context menu. Return true (1)
// if the command was handled or false (0) for the default implementation. See
// cef_menu_id_t for the command ids that have default implementations. All
// user-defined command ids should be between MENU_ID_USER_FIRST and
// MENU_ID_USER_LAST. |params| will have the same values as what was passed to
// on_before_context_menu(). Do not keep a reference to |params| outside of
// this callback.
function TCefContextMenuHandlerRef.OnContextMenuCommand(const aBrowser: ICefBrowser; const aFrame: ICefFrame; const aParams: ICefContextMenuParams; aCommandId: cint; aEventFlags: TCefEventFlags): Boolean;
begin
	Result := PCefContextMenuHandler(FData).on_context_menu_command(
		PCefContextMenuHandler(FData),
		TWACef.GetData(aBrowser),
		TWACef.GetData(aFrame),
		TWACef.GetData(aParams),
		aCommandId,
		aEventFlags
	) <> 0;
end;

// Called when the context menu is dismissed irregardless of whether the menu
// was NULL or a command was selected.
procedure TCefContextMenuHandlerRef.OnContextMenuDismissed(const aBrowser: ICefBrowser; const aFrame: ICefFrame);
begin
	PCefContextMenuHandler(FData).on_context_menu_dismissed(
		PCefContextMenuHandler(FData),
		TWACef.GetData(aBrowser),
		TWACef.GetData(aFrame)
	);
end;

{Public section}
class function TCefContextMenuHandlerRef.UnWrap(data: Pointer): ICefContextMenuHandler;
begin
	if data <> nil then
		Result := Create(data) as ICefContextMenuHandler
	else
		Result := nil;
end;

//..............................................................................TCefContextMenuParamsRef
{Protected section}
// Returns the X coordinate of the mouse where the context menu was invoked.
// Coords are relative to the associated RenderView's origin.
function TCefContextMenuParamsRef.GetXcoord: cint;
begin
	Result := PCefContextMenuParams(FData).get_xcoord(
		PCefContextMenuParams(FData)
	);
end;

// Returns the Y coordinate of the mouse where the context menu was invoked.
// Coords are relative to the associated RenderView's origin.
function TCefContextMenuParamsRef.GetYcoord: cint;
begin
	Result := PCefContextMenuParams(FData).get_ycoord(
		PCefContextMenuParams(FData)
	);
end;

// Returns flags representing the type of node that the context menu was
// invoked on.
function TCefContextMenuParamsRef.GetTypeFlags: TCefContextMenuTypeFlags;
begin
	Result := PCefContextMenuParams(FData).get_type_flags(
		PCefContextMenuParams(FData)
	);
end;

// Returns the URL of the link, if any, that encloses the node that the
// context menu was invoked on.
// The resulting string must be freed by calling cef_string_userfree_free().
function TCefContextMenuParamsRef.GetLinkUrl: ustring;
begin
	Result := TWACef.StringFreeAndGet(PCefContextMenuParams(FData).get_link_url(
		PCefContextMenuParams(FData)
	));
end;

// Returns the link URL, if any, to be used ONLY for "copy link address". We
// don't validate this field in the frontend process.
// The resulting string must be freed by calling cef_string_userfree_free().
function TCefContextMenuParamsRef.GetUnfilteredLinkUrl: ustring;
begin
	Result := TWACef.StringFreeAndGet(PCefContextMenuParams(FData).get_unfiltered_link_url(
		PCefContextMenuParams(FData)
	));
end;

// Returns the source URL, if any, for the element that the context menu was
// invoked on. Example of elements with source URLs are img, audio, and video.
// The resulting string must be freed by calling cef_string_userfree_free().
function TCefContextMenuParamsRef.GetSourceUrl: ustring;
begin
	Result := TWACef.StringFreeAndGet(PCefContextMenuParams(FData).get_source_url(
		PCefContextMenuParams(FData)
	));
end;

// Returns true (1) if the context menu was invoked on an image which has non-
// NULL contents.
function TCefContextMenuParamsRef.HasImageContents: Boolean;
begin
	Result := PCefContextMenuParams(FData).has_image_contents(
		PCefContextMenuParams(FData)
	) <> 0;
end;

// Returns the URL of the top level page that the context menu was invoked on.
// The resulting string must be freed by calling cef_string_userfree_free().
function TCefContextMenuParamsRef.GetPageUrl: ustring;
begin
	Result := TWACef.StringFreeAndGet(PCefContextMenuParams(FData).get_page_url(
		PCefContextMenuParams(FData)
	));
end;

// Returns the URL of the subframe that the context menu was invoked on.
// The resulting string must be freed by calling cef_string_userfree_free().
function TCefContextMenuParamsRef.GetFrameUrl: ustring;
begin
	Result := TWACef.StringFreeAndGet(PCefContextMenuParams(FData).get_frame_url(
		PCefContextMenuParams(FData)
	));
end;

// Returns the character encoding of the subframe that the context menu was
// invoked on.
// The resulting string must be freed by calling cef_string_userfree_free().
function TCefContextMenuParamsRef.GetFrameCharset: ustring;
begin
	Result := TWACef.StringFreeAndGet(PCefContextMenuParams(FData).get_frame_charset(
		PCefContextMenuParams(FData)
	));
end;

// Returns the type of context node that the context menu was invoked on.
function TCefContextMenuParamsRef.GetMediaType: TCefContextMenuMediaType;
begin
	Result := PCefContextMenuParams(FData).get_media_type(
		PCefContextMenuParams(FData)
	);
end;

// Returns flags representing the actions supported by the media element, if
// any, that the context menu was invoked on.
function TCefContextMenuParamsRef.GetMediaStateFlags: TCefContextMenuMediaStateFlags;
begin
	Result := PCefContextMenuParams(FData).get_media_state_flags(
		PCefContextMenuParams(FData)
	);
end;

// Returns the text of the selection, if any, that the context menu was
// invoked on.
// The resulting string must be freed by calling cef_string_userfree_free().
function TCefContextMenuParamsRef.GetSelectionText: ustring;
begin
	Result := TWACef.StringFreeAndGet(PCefContextMenuParams(FData).get_selection_text(
		PCefContextMenuParams(FData)
	));
end;

// Returns the text of the misspelled word, if any, that the context menu was
// invoked on.
// The resulting string must be freed by calling cef_string_userfree_free().
function TCefContextMenuParamsRef.GetMisspelledWord: ustring;
begin
	Result := TWACef.StringFreeAndGet(PCefContextMenuParams(FData).get_misspelled_word(
		PCefContextMenuParams(FData)
	));
end;

// Returns true (1) if suggestions exist, false (0) otherwise. Fills in
// |suggestions| from the spell check service for the misspelled word if there
// is one.
function TCefContextMenuParamsRef.GetDictionarySuggestions(aSuggestions: TStrings): Boolean;
var
	suggestions_list: TCefStringList;
	suggestions_iter: Integer;
	suggestions_item: TCefString;
begin
	suggestions_list := cef_string_list_alloc();
	for suggestions_iter := 0 to aSuggestions.Count - 1 do
	begin
		suggestions_item := TWACef.ToCefString(aSuggestions[suggestions_iter]);
		cef_string_list_append(suggestions_list, @suggestions_item);
	end;
	Result := PCefContextMenuParams(FData).get_dictionary_suggestions(
		PCefContextMenuParams(FData),
		suggestions_list
	) <> 0;
	aSuggestions.Clear;
	FillChar(suggestions_item, SizeOf(suggestions_item), 0);
	for suggestions_iter := 0 to cef_string_list_size(suggestions_list) - 1 do
	begin
		FillChar(suggestions_item, SizeOf(suggestions_item), 0);
		cef_string_list_value(suggestions_list, suggestions_iter, @suggestions_item);
		aSuggestions.Add(TWACef.StringClearAndGet(suggestions_item));
	end;
	cef_string_list_free(suggestions_list);
end;

// Returns true (1) if the context menu was invoked on an editable node.
function TCefContextMenuParamsRef.IsEditable: Boolean;
begin
	Result := PCefContextMenuParams(FData).is_editable(
		PCefContextMenuParams(FData)
	) <> 0;
end;

// Returns true (1) if the context menu was invoked on an editable node where
// spell-check is enabled.
function TCefContextMenuParamsRef.IsSpellCheckEnabled: Boolean;
begin
	Result := PCefContextMenuParams(FData).is_spell_check_enabled(
		PCefContextMenuParams(FData)
	) <> 0;
end;

// Returns flags representing the actions supported by the editable node, if
// any, that the context menu was invoked on.
function TCefContextMenuParamsRef.GetEditStateFlags: TCefContextMenuEditStateFlags;
begin
	Result := PCefContextMenuParams(FData).get_edit_state_flags(
		PCefContextMenuParams(FData)
	);
end;

{Public section}
class function TCefContextMenuParamsRef.UnWrap(data: Pointer): ICefContextMenuParams;
begin
	if data <> nil then
		Result := Create(data) as ICefContextMenuParams
	else
		Result := nil;
end;

//..............................................................................TCefCookieManagerRef
// Set the schemes supported by this manager. By default only "http" and
// "https" schemes are supported. If |callback| is non-NULL it will be
// executed asnychronously on the IO thread after the change has been applied.
// Must be called before any cookies are accessed.
procedure TCefCookieManagerRef.SetSupportedSchemes(aSchemes: TStrings; const aCallback: ICefCompletionCallback);
var
	schemes_list: TCefStringList;
	schemes_iter: Integer;
	schemes_item: TCefString;
begin
	schemes_list := cef_string_list_alloc();
	for schemes_iter := 0 to aSchemes.Count - 1 do
	begin
		schemes_item := TWACef.ToCefString(aSchemes[schemes_iter]);
		cef_string_list_append(schemes_list, @schemes_item);
	end;
	PCefCookieManager(FData).set_supported_schemes(
		PCefCookieManager(FData),
		schemes_list,
		TWACef.GetData(aCallback)
	);
	aSchemes.Clear;
	FillChar(schemes_item, SizeOf(schemes_item), 0);
	for schemes_iter := 0 to cef_string_list_size(schemes_list) - 1 do
	begin
		FillChar(schemes_item, SizeOf(schemes_item), 0);
		cef_string_list_value(schemes_list, schemes_iter, @schemes_item);
		aSchemes.Add(TWACef.StringClearAndGet(schemes_item));
	end;	cef_string_list_free(schemes_list);
end;

// Visit all cookies on the IO thread. The returned cookies are ordered by
// longest path, then by earliest creation date. Returns false (0) if cookies
// cannot be accessed.
function TCefCookieManagerRef.VisitAllCookies(const aVisitor: ICefCookieVisitor): Boolean;
begin
	Result := PCefCookieManager(FData).visit_all_cookies(
		PCefCookieManager(FData),
		TWACef.GetData(aVisitor)
	) <> 0;
end;

function TCefCookieManagerRef.VisitAllCookiesProc(const aVisitor: TCefCookieVisitorProc): Boolean;
var
  visit: ICefCookieVisitor;
begin
  visit := TCefFastCookieVisitor.Create(aVisitor);
  Result := VisitAllCookies(visit);
end;


// Visit a subset of cookies on the IO thread. The results are filtered by the
// given url scheme, host, domain and path. If |includeHttpOnly| is true (1)
// HTTP-only cookies will also be included in the results. The returned
// cookies are ordered by longest path, then by earliest creation date.
// Returns false (0) if cookies cannot be accessed.
function TCefCookieManagerRef.VisitUrlCookies(const aUrl: ustring; aIncludeHttpOnly: Boolean; const aVisitor: ICefCookieVisitor): Boolean;
var
	url_str: TCefString;
begin
	url_str := TWACef.ToCefString(aUrl);
	Result := PCefCookieManager(FData).visit_url_cookies(
		PCefCookieManager(FData),
		@url_str,
		Ord(aIncludeHttpOnly),
		TWACef.GetData(aVisitor)
	) <> 0;
end;

function TCefCookieManagerRef.VisitUrlCookiesProc(const aUrl: ustring; aIncludeHttpOnly: Boolean; const avisitor: TCefCookieVisitorProc): Boolean;
begin
  Result := VisitUrlCookies(aUrl, aIncludeHttpOnly, TCefFastCookieVisitor.Create(aVisitor) as ICefCookieVisitor);
end;

// Sets a cookie given a valid URL and explicit user-provided cookie
// attributes. This function expects each attribute to be well-formed. It will
// check for disallowed characters (e.g. the ';' character is disallowed
// within the cookie value attribute) and fail without setting the cookie if
// such characters are found. If |callback| is non-NULL it will be executed
// asnychronously on the IO thread after the cookie has been set. Returns
// false (0) if an invalid URL is specified or if cookies cannot be accessed.
function TCefCookieManagerRef.SetCookie(const aUrl: ustring; const aCookie: TCefCookie; const aCallback: ICefSetCookieCallback): Boolean;
var
	url_str: TCefString;
begin
	url_str := TWACef.ToCefString(aUrl);
	Result := PCefCookieManager(FData).set_cookie(
		PCefCookieManager(FData),
		@url_str,
		@aCookie,
		TWACef.GetData(aCallback)
	) <> 0;
end;

// Delete all cookies that match the specified parameters. If both |url| and
// |cookie_name| values are specified all host and domain cookies matching
// both will be deleted. If only |url| is specified all host cookies (but not
// domain cookies) irrespective of path will be deleted. If |url| is NULL all
// cookies for all hosts and domains will be deleted. If |callback| is non-
// NULL it will be executed asnychronously on the IO thread after the cookies
// have been deleted. Returns false (0) if a non-NULL invalid URL is specified
// or if cookies cannot be accessed. Cookies can alternately be deleted using
// the Visit*Cookies() functions.
function TCefCookieManagerRef.DeleteCookies(const aUrl: ustring; const aCookieName: ustring; const aCallback: ICefDeleteCookiesCallback): Boolean;
var
	url_str: TCefString;
	cookie_name_str: TCefString;
begin
	url_str := TWACef.ToCefString(aUrl);
	cookie_name_str := TWACef.ToCefString(aCookieName);
	Result := PCefCookieManager(FData).delete_cookies(
		PCefCookieManager(FData),
		@url_str,
		@cookie_name_str,
		TWACef.GetData(aCallback)
	) <> 0;
end;

// Sets the directory path that will be used for storing cookie data. If
// |path| is NULL data will be stored in memory only. Otherwise, data will be
// stored at the specified |path|. To persist session cookies (cookies without
// an expiry date or validity interval) set |persist_session_cookies| to true
// (1). Session cookies are generally intended to be transient and most Web
// browsers do not persist them. If |callback| is non-NULL it will be executed
// asnychronously on the IO thread after the manager's storage has been
// initialized. Returns false (0) if cookies cannot be accessed.
function TCefCookieManagerRef.SetStoragePath(const aPath: ustring; aPersistSessionCookies: Boolean; const aCallback: ICefCompletionCallback): Boolean;
var
  p: TCefString;
begin
  p := TWACef.ToCefString(aPath);
  if aPath <> '' then
    Result := PCefCookieManager(FData)^.set_storage_path(
      PCefCookieManager(FData),
      @p,
      Ord(aPersistSessionCookies),
      TWACef.GetData(aCallback)
    ) <> 0
  else
    Result := PCefCookieManager(FData)^.set_storage_path(
      PCefCookieManager(FData),
      nil,
      Ord(aPersistSessionCookies),
      TWACef.GetData(aCallback)
    ) <> 0;
end;

// Flush the backing store (if any) to disk. If |callback| is non-NULL it will
// be executed asnychronously on the IO thread after the flush is complete.
// Returns false (0) if cookies cannot be accessed.
function TCefCookieManagerRef.FlushStore(const aCallback: ICefCompletionCallback): Boolean;
begin
	Result := PCefCookieManager(FData).flush_store(
		PCefCookieManager(FData),
		TWACef.GetData(aCallback)
	) <> 0;
end;

{Public section}
class function TCefCookieManagerRef.UnWrap(data: Pointer): ICefCookieManager;
begin
	if data <> nil then
		Result := Create(data) as ICefCookieManager
	else
		Result := nil;
end;
// Returns the global cookie manager. By default data will be stored at
// CefSettings.cache_path if specified or in memory otherwise. If |callback| is
// non-NULL it will be executed asnychronously on the IO thread after the
// manager's storage has been initialized. Using this function is equivalent to
// calling cef_request_tContext::cef_request_context_get_global_context()->get_d
// efault_cookie_manager().
class function TCefCookieManagerRef.GetGlobalManager(const aCallback: ICefCompletionCallback): ICefCookieManager;
begin
	Result := TCefCookieManagerRef.UnWrap(cef_cookie_manager_get_global_manager(
		TWACef.GetData(aCallback)
	));
end;

// Returns the global cookie manager. By default data will be stored at
// CefSettings.cache_path if specified or in memory otherwise. If |callback| is
// non-NULL it will be executed asnychronously on the IO thread after the
// manager's storage has been initialized. Using this function is equivalent to
// calling cef_request_tContext::cef_request_context_get_global_context()->get_d
// efault_cookie_manager().
// Creates a new cookie manager. If |path| is NULL data will be stored in memory
// only. Otherwise, data will be stored at the specified |path|. To persist
// session cookies (cookies without an expiry date or validity interval) set
// |persist_session_cookies| to true (1). Session cookies are generally intended
// to be transient and most Web browsers do not persist them. If |callback| is
// non-NULL it will be executed asnychronously on the IO thread after the
// manager's storage has been initialized.
class function TCefCookieManagerRef.CreateManager(const aPath: ustring; var aPersistSessionCookies: Boolean; const aCallback: ICefCompletionCallback): ICefCookieManager;
var
	path_str: TCefString;
	persist_session_cookies_proxy: cint;
begin
	path_str := TWACef.ToCefString(aPath);
	persist_session_cookies_proxy := Ord(aPersistSessionCookies);
	Result := TCefCookieManagerRef.UnWrap(cef_cookie_manager_create_manager(
		@path_str,
		persist_session_cookies_proxy,
		TWACef.GetData(aCallback)
	));
	aPersistSessionCookies := persist_session_cookies_proxy <> 0;
end;

//..............................................................................TCefCookieVisitorRef
{Protected section}
// Method that will be called once for each cookie. |count| is the 0-based
// index for the current cookie. |total| is the total number of cookies. Set
// |deleteCookie| to true (1) to delete the cookie currently being visited.
// Return false (0) to stop visiting cookies. This function may never be
// called if no cookies are found.
function TCefCookieVisitorRef.Visit(const aCookie: TWACefCookie; aCount: cint; aTotal: cint; out aDeleteCookie: Boolean): Boolean;
var
	cookie_proxy: TCefCookie;
	deleteCookie_proxy: cint;
begin
	cookie_proxy.name := TWACef.ToCefString(aCookie.name);
  cookie_proxy.value := TWACef.ToCefString(aCookie.value);
  cookie_proxy.domain := TWACef.ToCefString(aCookie.domain);
  cookie_proxy.path := TWACef.ToCefString(aCookie.path);
  cookie_proxy.secure := aCookie.secure;
  cookie_proxy.httponly := aCookie.httponly;
  cookie_proxy.creation := aCookie.creation;
  cookie_proxy.last_access := aCookie.last_access;
  cookie_proxy.has_expires := aCookie.has_expires;
  cookie_proxy.expires := aCookie.expires;
	deleteCookie_proxy := Ord(aDeleteCookie);
	Result := PCefCookieVisitor(FData).visit(
		PCefCookieVisitor(FData),
		@cookie_proxy,
		aCount,
		aTotal,
		@deleteCookie_proxy
	) <> 0;
	aDeleteCookie := deleteCookie_proxy <> 0;
end;

{Public section}
class function TCefCookieVisitorRef.UnWrap(data: Pointer): ICefCookieVisitor;
begin
	if data <> nil then
		Result := Create(data) as ICefCookieVisitor
	else
		Result := nil;
end;
//..............................................................................TCefSetCookieCallbackRef
{Protected section}
// Method that will be called upon completion. |success| will be true (1) if
// the cookie was set successfully.
procedure TCefSetCookieCallbackRef.OnComplete(aSuccess: Boolean);
begin
	PCefSetCookieCallback(FData).on_complete(
		PCefSetCookieCallback(FData),
		Ord(aSuccess)
	);
end;

{Public section}
class function TCefSetCookieCallbackRef.UnWrap(data: Pointer): ICefSetCookieCallback;
begin
	if data <> nil then
		Result := Create(data) as ICefSetCookieCallback
	else
		Result := nil;
end;
//..............................................................................TCefDeleteCookiesCallbackRef
{Protected section}
// Method that will be called upon completion. |num_deleted| will be the
// number of cookies that were deleted or -1 if unknown.
procedure TCefDeleteCookiesCallbackRef.OnComplete(aNumDeleted: cint);
begin
	PCefDeleteCookiesCallback(FData).on_complete(
		PCefDeleteCookiesCallback(FData),
		aNumDeleted
	);
end;

{Public section}
class function TCefDeleteCookiesCallbackRef.UnWrap(data: Pointer): ICefDeleteCookiesCallback;
begin
	if data <> nil then
		Result := Create(data) as ICefDeleteCookiesCallback
	else
		Result := nil;
end;

//..............................................................................TCefFileDialogCallbackRef
{Protected section}
// Continue the file selection. |selected_accept_filter| should be the 0-based
// index of the value selected from the accept filters array passed to
// cef_dialog_handler_t::OnFileDialog. |file_paths| should be a single value
// or a list of values depending on the dialog mode. An NULL |file_paths|
// value is treated the same as calling cancel().
procedure TCefFileDialogCallbackRef.Cont(aSelectedAcceptFilter: cint; aFilePaths: TStrings);
var
	file_paths_list: TCefStringList;
	file_paths_iter: Integer;
	file_paths_item: TCefString;
begin
	file_paths_list := cef_string_list_alloc();
	for file_paths_iter := 0 to aFilePaths.Count - 1 do
	begin
		file_paths_item := TWACef.ToCefString(aFilePaths[file_paths_iter]);
		cef_string_list_append(file_paths_list, @file_paths_item);
	end;
	PCefFileDialogCallback(FData).cont(
		PCefFileDialogCallback(FData),
		aSelectedAcceptFilter,
		file_paths_list
	);
	aFilePaths.Clear;
	FillChar(file_paths_item, SizeOf(file_paths_item), 0);
	for file_paths_iter := 0 to cef_string_list_size(file_paths_list) - 1 do
	begin
		FillChar(file_paths_item, SizeOf(file_paths_item), 0);
		cef_string_list_value(file_paths_list, file_paths_iter, @file_paths_item);
		aFilePaths.Add(TWACef.StringClearAndGet(file_paths_item));
	end;
	cef_string_list_free(file_paths_list);
end;

// Cancel the file selection.
procedure TCefFileDialogCallbackRef.Cancel;
begin
	PCefFileDialogCallback(FData).cancel(
		PCefFileDialogCallback(FData)
	);
end;

{Public section}
class function TCefFileDialogCallbackRef.UnWrap(data: Pointer): ICefFileDialogCallback;
begin
	if data <> nil then
		Result := Create(data) as ICefFileDialogCallback
	else
		Result := nil;
end;

//..............................................................................TCefDialogHandlerRef
{Protected section}
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
function TCefDialogHandlerRef.OnFileDialog(const aBrowser: ICefBrowser; aMode: TCefFileDialogMode; const aTitle: ustring; const aDefaultFilePath: ustring; aAcceptFilters: TStrings; aSelectedAcceptFilter: cint; const aCallback: ICefFileDialogCallback): Boolean;
var
	title_str: TCefString;
	default_file_path_str: TCefString;
	accept_filters_list: TCefStringList;
	accept_filters_iter: Integer;
	accept_filters_item: TCefString;
begin
	title_str := TWACef.ToCefString(aTitle);
	default_file_path_str := TWACef.ToCefString(aDefaultFilePath);
	accept_filters_list := cef_string_list_alloc();
	for accept_filters_iter := 0 to aAcceptFilters.Count - 1 do
	begin
		accept_filters_item := TWACef.ToCefString(aAcceptFilters[accept_filters_iter]);
		cef_string_list_append(accept_filters_list, @accept_filters_item);
	end;
	Result := PCefDialogHandler(FData).on_file_dialog(
		PCefDialogHandler(FData),
		TWACef.GetData(aBrowser),
		aMode,
		@title_str,
		@default_file_path_str,
		accept_filters_list,
		aSelectedAcceptFilter,
		TWACef.GetData(aCallback)
	) <> 0;
	aAcceptFilters.Clear;
	FillChar(accept_filters_item, SizeOf(accept_filters_item), 0);
	for accept_filters_iter := 0 to cef_string_list_size(accept_filters_list) - 1 do
	begin
		FillChar(accept_filters_item, SizeOf(accept_filters_item), 0);
		cef_string_list_value(accept_filters_list, accept_filters_iter, @accept_filters_item);
		aAcceptFilters.Add(TWACef.StringClearAndGet(accept_filters_item));
	end;
	cef_string_list_free(accept_filters_list);
end;

{Public section}
class function TCefDialogHandlerRef.UnWrap(data: Pointer): ICefDialogHandler;
begin
	if data <> nil then
		Result := Create(data) as ICefDialogHandler
	else
		Result := nil;
end;

//..............................................................................TCefDisplayHandlerRef
{Protected section}
// Called when a frame's address has changed.
procedure TCefDisplayHandlerRef.OnAddressChange(const aBrowser: ICefBrowser; const aFrame: ICefFrame; const aUrl: ustring);
var
	url_str: TCefString;
begin
	url_str := TWACef.ToCefString(aUrl);
	PCefDisplayHandler(FData).on_address_change(
		PCefDisplayHandler(FData),
		TWACef.GetData(aBrowser),
		TWACef.GetData(aFrame),
		@url_str
	);
end;

// Called when the page title changes.
procedure TCefDisplayHandlerRef.OnTitleChange(const aBrowser: ICefBrowser; const aTitle: ustring);
var
	title_str: TCefString;
begin
	title_str := TWACef.ToCefString(aTitle);
	PCefDisplayHandler(FData).on_title_change(
		PCefDisplayHandler(FData),
		TWACef.GetData(aBrowser),
		@title_str
	);
end;

// Called when the page icon changes.
procedure TCefDisplayHandlerRef.OnFaviconUrlchange(const aBrowser: ICefBrowser; aIconUrls: TStrings);
var
	icon_urls_list: TCefStringList;
	icon_urls_iter: Integer;
	icon_urls_item: TCefString;
begin
	icon_urls_list := cef_string_list_alloc();
	for icon_urls_iter := 0 to aIconUrls.Count - 1 do
	begin
		icon_urls_item := TWACef.ToCefString(aIconUrls[icon_urls_iter]);
		cef_string_list_append(icon_urls_list, @icon_urls_item);
	end;
	PCefDisplayHandler(FData).on_favicon_urlchange(
		PCefDisplayHandler(FData),
		TWACef.GetData(aBrowser),
		icon_urls_list
	);
	aIconUrls.Clear;
	FillChar(icon_urls_item, SizeOf(icon_urls_item), 0);
	for icon_urls_iter := 0 to cef_string_list_size(icon_urls_list) - 1 do
	begin
		FillChar(icon_urls_item, SizeOf(icon_urls_item), 0);
		cef_string_list_value(icon_urls_list, icon_urls_iter, @icon_urls_item);
		aIconUrls.Add(TWACef.StringClearAndGet(icon_urls_item));
	end;
	cef_string_list_free(icon_urls_list);
end;

// Called when the browser is about to display a tooltip. |text| contains the
// text that will be displayed in the tooltip. To handle the display of the
// tooltip yourself return true (1). Otherwise, you can optionally modify
// |text| and then return false (0) to allow the browser to display the
// tooltip. When window rendering is disabled the application is responsible
// for drawing tooltips and the return value is ignored.
function TCefDisplayHandlerRef.OnTooltip(const aBrowser: ICefBrowser; var aText: ustring): Boolean;
var
	text_str: TCefString;
begin
	text_str := TWACef.ToCefString(aText);
	Result := PCefDisplayHandler(FData).on_tooltip(
		PCefDisplayHandler(FData),
		TWACef.GetData(aBrowser),
		@text_str
	) <> 0;
  aText := TWACef.ToString(@text_str);
end;

// Called when the browser receives a status message. |value| contains the
// text that will be displayed in the status message.
procedure TCefDisplayHandlerRef.OnStatusMessage(const aBrowser: ICefBrowser; const aValue: ustring);
var
	value_str: TCefString;
begin
	value_str := TWACef.ToCefString(aValue);
	PCefDisplayHandler(FData).on_status_message(
		PCefDisplayHandler(FData),
		TWACef.GetData(aBrowser),
		@value_str
	);
end;

// Called to display a console message. Return true (1) to stop the message
// from being output to the console.
function TCefDisplayHandlerRef.OnConsoleMessage(const aBrowser: ICefBrowser; const aMessage: ustring; const aSource: ustring; aLine: cint): Boolean;
var
	message_str: TCefString;
	source_str: TCefString;
begin
	message_str := TWACef.ToCefString(aMessage);
	source_str := TWACef.ToCefString(aSource);
	Result := PCefDisplayHandler(FData).on_console_message(
		PCefDisplayHandler(FData),
		TWACef.GetData(aBrowser),
		@message_str,
		@source_str,
		aLine
	) <> 0;
end;

{Public section}
class function TCefDisplayHandlerRef.UnWrap(data: Pointer): ICefDisplayHandler;
begin
	if data <> nil then
		Result := Create(data) as ICefDisplayHandler
	else
		Result := nil;
end;

//..............................................................................TCefDomvisitorRef
{Protected section}
// Method executed for visiting the DOM. The document object passed to this
// function represents a snapshot of the DOM at the time this function is
// executed. DOM objects are only valid for the scope of this function. Do not
// keep references to or attempt to access any DOM objects outside the scope
// of this function.
procedure TCefDomvisitorRef.Visit(const aDocument: ICefDomdocument);
begin
	PCefDomvisitor(FData).visit(
		PCefDomvisitor(FData),
		TWACef.GetData(aDocument)
	);
end;

{Public section}
class function TCefDomvisitorRef.UnWrap(data: Pointer): ICefDomvisitor;
begin
	if data <> nil then
		Result := Create(data) as ICefDomvisitor
	else
		Result := nil;
end;
//..............................................................................TCefDomdocumentRef
{Protected section}
// Returns the document type.
function TCefDomdocumentRef.GetType: TCefDomDocumentType;
begin
	Result := PCefDomdocument(FData).get_type(
		PCefDomdocument(FData)
	);
end;

// Returns the root document node.
function TCefDomdocumentRef.GetDocument: ICefDomnode;
begin
	Result := TCefDomnodeRef.UnWrap(PCefDomdocument(FData).get_document(
		PCefDomdocument(FData)
	));
end;

// Returns the BODY node of an HTML document.
function TCefDomdocumentRef.GetBody: ICefDomnode;
begin
	Result := TCefDomnodeRef.UnWrap(PCefDomdocument(FData).get_body(
		PCefDomdocument(FData)
	));
end;

// Returns the HEAD node of an HTML document.
function TCefDomdocumentRef.GetHead: ICefDomnode;
begin
	Result := TCefDomnodeRef.UnWrap(PCefDomdocument(FData).get_head(
		PCefDomdocument(FData)
	));
end;

// Returns the title of an HTML document.
// The resulting string must be freed by calling cef_string_userfree_free().
function TCefDomdocumentRef.GetTitle: ustring;
begin
	Result := TWACef.StringFreeAndGet(PCefDomdocument(FData).get_title(
		PCefDomdocument(FData)
	));
end;

// Returns the document element with the specified ID value.
function TCefDomdocumentRef.GetElementById(const aId: ustring): ICefDomnode;
var
	id_str: TCefString;
begin
	id_str := TWACef.ToCefString(aId);
	Result := TCefDomnodeRef.UnWrap(PCefDomdocument(FData).get_element_by_id(
		PCefDomdocument(FData),
		@id_str
	));
end;

// Returns the node that currently has keyboard focus.
function TCefDomdocumentRef.GetFocusedNode: ICefDomnode;
begin
	Result := TCefDomnodeRef.UnWrap(PCefDomdocument(FData).get_focused_node(
		PCefDomdocument(FData)
	));
end;

// Returns true (1) if a portion of the document is selected.
function TCefDomdocumentRef.HasSelection: Boolean;
begin
	Result := PCefDomdocument(FData).has_selection(
		PCefDomdocument(FData)
	) <> 0;
end;

// Returns the selection offset within the start node.
function TCefDomdocumentRef.GetSelectionStartOffset: cint;
begin
	Result := PCefDomdocument(FData).get_selection_start_offset(
		PCefDomdocument(FData)
	);
end;

// Returns the selection offset within the end node.
function TCefDomdocumentRef.GetSelectionEndOffset: cint;
begin
	Result := PCefDomdocument(FData).get_selection_end_offset(
		PCefDomdocument(FData)
	);
end;

// Returns the contents of this selection as markup.
// The resulting string must be freed by calling cef_string_userfree_free().
function TCefDomdocumentRef.GetSelectionAsMarkup: ustring;
begin
	Result := TWACef.StringFreeAndGet(PCefDomdocument(FData).get_selection_as_markup(
		PCefDomdocument(FData)
	));
end;

// Returns the contents of this selection as text.
// The resulting string must be freed by calling cef_string_userfree_free().
function TCefDomdocumentRef.GetSelectionAsText: ustring;
begin
	Result := TWACef.StringFreeAndGet(PCefDomdocument(FData).get_selection_as_text(
		PCefDomdocument(FData)
	));
end;

// Returns the base URL for the document.
// The resulting string must be freed by calling cef_string_userfree_free().
function TCefDomdocumentRef.GetBaseUrl: ustring;
begin
	Result := TWACef.StringFreeAndGet(PCefDomdocument(FData).get_base_url(
		PCefDomdocument(FData)
	));
end;

// Returns a complete URL based on the document base URL and the specified
// partial URL.
// The resulting string must be freed by calling cef_string_userfree_free().
function TCefDomdocumentRef.GetCompleteUrl(const aPartialURL: ustring): ustring;
var
	partialURL_str: TCefString;
begin
	partialURL_str := TWACef.ToCefString(aPartialURL);
	Result := TWACef.StringFreeAndGet(PCefDomdocument(FData).get_complete_url(
		PCefDomdocument(FData),
		@partialURL_str
	));
end;

{Public section}
class function TCefDomdocumentRef.UnWrap(data: Pointer): ICefDomdocument;
begin
	if data <> nil then
		Result := Create(data) as ICefDomdocument
	else
		Result := nil;
end;

//..............................................................................TCefDomnodeRef
// Returns the type for this node.
function TCefDomnodeRef.GetType: TCefDomNodeType;
begin
	Result := PCefDomnode(FData).get_type(
		PCefDomnode(FData)
	);
end;

// Returns true (1) if this is a text node.
function TCefDomnodeRef.IsText: Boolean;
begin
	Result := PCefDomnode(FData).is_text(
		PCefDomnode(FData)
	) <> 0;
end;

// Returns true (1) if this is an element node.
function TCefDomnodeRef.IsElement: Boolean;
begin
	Result := PCefDomnode(FData).is_element(
		PCefDomnode(FData)
	) <> 0;
end;

// Returns true (1) if this is an editable node.
function TCefDomnodeRef.IsEditable: Boolean;
begin
	Result := PCefDomnode(FData).is_editable(
		PCefDomnode(FData)
	) <> 0;
end;

// Returns true (1) if this is a form control element node.
function TCefDomnodeRef.IsFormControlElement: Boolean;
begin
	Result := PCefDomnode(FData).is_form_control_element(
		PCefDomnode(FData)
	) <> 0;
end;

// Returns the type of this form control element node.
// The resulting string must be freed by calling cef_string_userfree_free().
function TCefDomnodeRef.GetFormControlElementType: ustring;
begin
	Result := TWACef.StringFreeAndGet(PCefDomnode(FData).get_form_control_element_type(
		PCefDomnode(FData)
	));
end;

// Returns true (1) if this object is pointing to the same handle as |that|
// object.
function TCefDomnodeRef.IsSame(const aThat: ICefDomnode): Boolean;
begin
	Result := PCefDomnode(FData).is_same(
		PCefDomnode(FData),
		TWACef.GetData(aThat)
	) <> 0;
end;

// Returns the name of this node.
// The resulting string must be freed by calling cef_string_userfree_free().
function TCefDomnodeRef.GetName: ustring;
begin
	Result := TWACef.StringFreeAndGet(PCefDomnode(FData).get_name(
		PCefDomnode(FData)
	));
end;

// Returns the value of this node.
// The resulting string must be freed by calling cef_string_userfree_free().
function TCefDomnodeRef.GetValue: ustring;
begin
	Result := TWACef.StringFreeAndGet(PCefDomnode(FData).get_value(
		PCefDomnode(FData)
	));
end;

// Set the value of this node. Returns true (1) on success.
function TCefDomnodeRef.SetValue(const aValue: ustring): Boolean;
var
	value_str: TCefString;
begin
	value_str := TWACef.ToCefString(aValue);
	Result := PCefDomnode(FData).set_value(
		PCefDomnode(FData),
		@value_str
	) <> 0;
end;

// Returns the contents of this node as markup.
// The resulting string must be freed by calling cef_string_userfree_free().
function TCefDomnodeRef.GetAsMarkup: ustring;
begin
	Result := TWACef.StringFreeAndGet(PCefDomnode(FData).get_as_markup(
		PCefDomnode(FData)
	));
end;

// Returns the document associated with this node.
function TCefDomnodeRef.GetDocument: ICefDomdocument;
begin
	Result := TCefDomdocumentRef.UnWrap(PCefDomnode(FData).get_document(
		PCefDomnode(FData)
	));
end;

// Returns the parent node.
function TCefDomnodeRef.GetParent: ICefDomnode;
begin
	Result := TCefDomnodeRef.UnWrap(PCefDomnode(FData).get_parent(
		PCefDomnode(FData)
	));
end;

// Returns the previous sibling node.
function TCefDomnodeRef.GetPreviousSibling: ICefDomnode;
begin
	Result := TCefDomnodeRef.UnWrap(PCefDomnode(FData).get_previous_sibling(
		PCefDomnode(FData)
	));
end;

// Returns the next sibling node.
function TCefDomnodeRef.GetNextSibling: ICefDomnode;
begin
	Result := TCefDomnodeRef.UnWrap(PCefDomnode(FData).get_next_sibling(
		PCefDomnode(FData)
	));
end;

// Returns true (1) if this node has child nodes.
function TCefDomnodeRef.HasChildren: Boolean;
begin
	Result := PCefDomnode(FData).has_children(
		PCefDomnode(FData)
	) <> 0;
end;

// Return the first child node.
function TCefDomnodeRef.GetFirstChild: ICefDomnode;
begin
	Result := TCefDomnodeRef.UnWrap(PCefDomnode(FData).get_first_child(
		PCefDomnode(FData)
	));
end;

// Returns the last child node.
function TCefDomnodeRef.GetLastChild: ICefDomnode;
begin
	Result := TCefDomnodeRef.UnWrap(PCefDomnode(FData).get_last_child(
		PCefDomnode(FData)
	));
end;

// Returns the tag name of this element.
// The resulting string must be freed by calling cef_string_userfree_free().
function TCefDomnodeRef.GetElementTagName: ustring;
begin
	Result := TWACef.StringFreeAndGet(PCefDomnode(FData).get_element_tag_name(
		PCefDomnode(FData)
	));
end;

// Returns true (1) if this element has attributes.
function TCefDomnodeRef.HasElementAttributes: Boolean;
begin
	Result := PCefDomnode(FData).has_element_attributes(
		PCefDomnode(FData)
	) <> 0;
end;

// Returns true (1) if this element has an attribute named |attrName|.
function TCefDomnodeRef.HasElementAttribute(const aAttrName: ustring): Boolean;
var
	attrName_str: TCefString;
begin
	attrName_str := TWACef.ToCefString(aAttrName);
	Result := PCefDomnode(FData).has_element_attribute(
		PCefDomnode(FData),
		@attrName_str
	) <> 0;
end;

// Returns the element attribute named |attrName|.
// The resulting string must be freed by calling cef_string_userfree_free().
function TCefDomnodeRef.GetElementAttribute(const aAttrName: ustring): ustring;
var
	attrName_str: TCefString;
begin
	attrName_str := TWACef.ToCefString(aAttrName);
	Result := TWACef.StringFreeAndGet(PCefDomnode(FData).get_element_attribute(
		PCefDomnode(FData),
		@attrName_str
	));
end;

// Returns a map of all element attributes.
procedure TCefDomnodeRef.GetElementAttributes(aAttrMap: TStrings);
var
  list: TCefStringList;
  i: Integer;
  str: TCefString;
begin
  list := cef_string_list_alloc;
  try
    PCefDomnode(FData).get_element_attributes(
      PCefDomnode(FData),
      list
    );
    for i := 0 to cef_string_list_size(list) - 1 do
    begin
      FillChar(str, SizeOf(str), 0);
      cef_string_list_value(list, i, @str);
      aAttrMap.Add(TWACef.StringClearAndGet(str));
    end;
  finally
    cef_string_list_free(list);
  end;
end;

// Set the value for the element attribute named |attrName|. Returns true (1)
// on success.
function TCefDomnodeRef.SetElementAttribute(const aAttrName: ustring; const aValue: ustring): Boolean;
var
	attrName_str: TCefString;
	value_str: TCefString;
begin
	attrName_str := TWACef.ToCefString(aAttrName);
	value_str := TWACef.ToCefString(aValue);
	Result := PCefDomnode(FData).set_element_attribute(
		PCefDomnode(FData),
		@attrName_str,
		@value_str
	) <> 0;
end;

// Returns the inner text of the element.
// The resulting string must be freed by calling cef_string_userfree_free().
function TCefDomnodeRef.GetElementInnerText: ustring;
begin
	Result := TWACef.StringFreeAndGet(PCefDomnode(FData).get_element_inner_text(
		PCefDomnode(FData)
	));
end;

{Public section}
class function TCefDomnodeRef.UnWrap(data: Pointer): ICefDomnode;
begin
	if data <> nil then
		Result := Create(data) as ICefDomnode
	else
		Result := nil;
end;

//..............................................................................TCefBeforeDownloadCallbackRef
{Protected section}
// Call to continue the download. Set |download_path| to the full file path
// for the download including the file name or leave blank to use the
// suggested name and the default temp directory. Set |show_dialog| to true
// (1) if you do wish to show the default "Save As" dialog.
procedure TCefBeforeDownloadCallbackRef.Cont(var aDownloadPath: ustring; aShowDialog: Boolean);
var
	download_path_str: TCefString;
	show_dialog_proxy: cint;
begin
	download_path_str := TWACef.ToCefString(aDownloadPath);
	show_dialog_proxy := Ord(aShowDialog);
	PCefBeforeDownloadCallback(FData).cont(
		PCefBeforeDownloadCallback(FData),
		@download_path_str,
		show_dialog_proxy
	);
end;

{Public section}
class function TCefBeforeDownloadCallbackRef.UnWrap(data: Pointer): ICefBeforeDownloadCallback;
begin
	if data <> nil then
		Result := Create(data) as ICefBeforeDownloadCallback
	else
		Result := nil;
end;
//..............................................................................TCefDownloadItemCallbackRef
{Protected section}
// Call to cancel the download.
procedure TCefDownloadItemCallbackRef.Cancel;
begin
	PCefDownloadItemCallback(FData).cancel(
		PCefDownloadItemCallback(FData)
	);
end;

// Call to pause the download.
procedure TCefDownloadItemCallbackRef.Pause;
begin
	PCefDownloadItemCallback(FData).pause(
		PCefDownloadItemCallback(FData)
	);
end;

// Call to resume the download.
procedure TCefDownloadItemCallbackRef.Resume;
begin
	PCefDownloadItemCallback(FData).resume(
		PCefDownloadItemCallback(FData)
	);
end;

{Public section}
class function TCefDownloadItemCallbackRef.UnWrap(data: Pointer): ICefDownloadItemCallback;
begin
	if data <> nil then
		Result := Create(data) as ICefDownloadItemCallback
	else
		Result := nil;
end;
//..............................................................................TCefDownloadHandlerRef
{Protected section}
// Called before a download begins. |suggested_name| is the suggested name for
// the download file. By default the download will be canceled. Execute
// |callback| either asynchronously or in this function to continue the
// download if desired. Do not keep a reference to |download_item| outside of
// this function.
procedure TCefDownloadHandlerRef.OnBeforeDownload(const aBrowser: ICefBrowser; const aDownloadItem: ICefDownloadItem; const aSuggestedName: ustring; const aCallback: ICefBeforeDownloadCallback);
var
	suggested_name_str: TCefString;
begin
	suggested_name_str := TWACef.ToCefString(aSuggestedName);
	PCefDownloadHandler(FData).on_before_download(
		PCefDownloadHandler(FData),
		TWACef.GetData(aBrowser),
		TWACef.GetData(aDownloadItem),
		@suggested_name_str,
		TWACef.GetData(aCallback)
	);
end;

// Called when a download's status or progress information has been updated.
// This may be called multiple times before and after on_before_download().
// Execute |callback| either asynchronously or in this function to cancel the
// download if desired. Do not keep a reference to |download_item| outside of
// this function.
procedure TCefDownloadHandlerRef.OnDownloadUpdated(const aBrowser: ICefBrowser; const aDownloadItem: ICefDownloadItem; const aCallback: ICefDownloadItemCallback);
begin
	PCefDownloadHandler(FData).on_download_updated(
		PCefDownloadHandler(FData),
		TWACef.GetData(aBrowser),
		TWACef.GetData(aDownloadItem),
		TWACef.GetData(aCallback)
	);
end;

{Public section}
class function TCefDownloadHandlerRef.UnWrap(data: Pointer): ICefDownloadHandler;
begin
	if data <> nil then
		Result := Create(data) as ICefDownloadHandler
	else
		Result := nil;
end;

//..............................................................................TCefDownloadItemRef
{Protected section}
// Returns true (1) if this object is valid. Do not call any other functions
// if this function returns false (0).
function TCefDownloadItemRef.IsValid: Boolean;
begin
	Result := PCefDownloadItem(FData).is_valid(
		PCefDownloadItem(FData)
	) <> 0;
end;

// Returns true (1) if the download is in progress.
function TCefDownloadItemRef.IsInProgress: Boolean;
begin
	Result := PCefDownloadItem(FData).is_in_progress(
		PCefDownloadItem(FData)
	) <> 0;
end;

// Returns true (1) if the download is complete.
function TCefDownloadItemRef.IsComplete: Boolean;
begin
	Result := PCefDownloadItem(FData).is_complete(
		PCefDownloadItem(FData)
	) <> 0;
end;

// Returns true (1) if the download has been canceled or interrupted.
function TCefDownloadItemRef.IsCanceled: Boolean;
begin
	Result := PCefDownloadItem(FData).is_canceled(
		PCefDownloadItem(FData)
	) <> 0;
end;

// Returns a simple speed estimate in bytes/s.
function TCefDownloadItemRef.GetCurrentSpeed: cint64;
begin
	Result := PCefDownloadItem(FData).get_current_speed(
		PCefDownloadItem(FData)
	);
end;

// Returns the rough percent complete or -1 if the receive total size is
// unknown.
function TCefDownloadItemRef.GetPercentComplete: cint;
begin
	Result := PCefDownloadItem(FData).get_percent_complete(
		PCefDownloadItem(FData)
	);
end;

// Returns the total number of bytes.
function TCefDownloadItemRef.GetTotalBytes: cint64;
begin
	Result := PCefDownloadItem(FData).get_total_bytes(
		PCefDownloadItem(FData)
	);
end;

// Returns the number of received bytes.
function TCefDownloadItemRef.GetReceivedBytes: cint64;
begin
	Result := PCefDownloadItem(FData).get_received_bytes(
		PCefDownloadItem(FData)
	);
end;

// Returns the time that the download started.
function TCefDownloadItemRef.GetStartTime: TCefTime;
begin
	Result := PCefDownloadItem(FData).get_start_time(
		PCefDownloadItem(FData)
	);
end;

// Returns the time that the download ended.
function TCefDownloadItemRef.GetEndTime: TCefTime;
begin
	Result := PCefDownloadItem(FData).get_end_time(
		PCefDownloadItem(FData)
	);
end;

// Returns the full path to the downloaded or downloading file.
// The resulting string must be freed by calling cef_string_userfree_free().
function TCefDownloadItemRef.GetFullPath: ustring;
begin
	Result := TWACef.StringFreeAndGet(PCefDownloadItem(FData).get_full_path(
		PCefDownloadItem(FData)
	));
end;

// Returns the unique identifier for this download.
function TCefDownloadItemRef.GetId: cuint32;
begin
	Result := PCefDownloadItem(FData).get_id(
		PCefDownloadItem(FData)
	);
end;

// Returns the URL.
// The resulting string must be freed by calling cef_string_userfree_free().
function TCefDownloadItemRef.GetUrl: ustring;
begin
	Result := TWACef.StringFreeAndGet(PCefDownloadItem(FData).get_url(
		PCefDownloadItem(FData)
	));
end;

// Returns the original URL before any redirections.
// The resulting string must be freed by calling cef_string_userfree_free().
function TCefDownloadItemRef.GetOriginalUrl: ustring;
begin
	Result := TWACef.StringFreeAndGet(PCefDownloadItem(FData).get_original_url(
		PCefDownloadItem(FData)
	));
end;

// Returns the suggested file name.
// The resulting string must be freed by calling cef_string_userfree_free().
function TCefDownloadItemRef.GetSuggestedFileName: ustring;
begin
	Result := TWACef.StringFreeAndGet(PCefDownloadItem(FData).get_suggested_file_name(
		PCefDownloadItem(FData)
	));
end;

// Returns the content disposition.
// The resulting string must be freed by calling cef_string_userfree_free().
function TCefDownloadItemRef.GetContentDisposition: ustring;
begin
	Result := TWACef.StringFreeAndGet(PCefDownloadItem(FData).get_content_disposition(
		PCefDownloadItem(FData)
	));
end;

// Returns the mime type.
// The resulting string must be freed by calling cef_string_userfree_free().
function TCefDownloadItemRef.GetMimeType: ustring;
begin
	Result := TWACef.StringFreeAndGet(PCefDownloadItem(FData).get_mime_type(
		PCefDownloadItem(FData)
	));
end;

{Public section}
class function TCefDownloadItemRef.UnWrap(data: Pointer): ICefDownloadItem;
begin
	if data <> nil then
		Result := Create(data) as ICefDownloadItem
	else
		Result := nil;
end;

//..............................................................................TCefDragDataRef
// Returns a copy of the current object.
function TCefDragDataRef.Clone: ICefDragData;
begin
	Result := TCefDragDataRef.UnWrap(PCefDragData(FData).clone(
		PCefDragData(FData)
	));
end;

// Returns true (1) if this object is read-only.
function TCefDragDataRef.IsReadOnly: Boolean;
begin
	Result := PCefDragData(FData).is_read_only(
		PCefDragData(FData)
	) <> 0;
end;

// Returns true (1) if the drag data is a link.
function TCefDragDataRef.IsLink: Boolean;
begin
	Result := PCefDragData(FData).is_link(
		PCefDragData(FData)
	) <> 0;
end;

// Returns true (1) if the drag data is a text or html fragment.
function TCefDragDataRef.IsFragment: Boolean;
begin
	Result := PCefDragData(FData).is_fragment(
		PCefDragData(FData)
	) <> 0;
end;

// Returns true (1) if the drag data is a file.
function TCefDragDataRef.IsFile: Boolean;
begin
	Result := PCefDragData(FData).is_file(
		PCefDragData(FData)
	) <> 0;
end;

// Return the link URL that is being dragged.
// The resulting string must be freed by calling cef_string_userfree_free().
function TCefDragDataRef.GetLinkUrl: ustring;
begin
	Result := TWACef.StringFreeAndGet(PCefDragData(FData).get_link_url(
		PCefDragData(FData)
	));
end;

// Return the title associated with the link being dragged.
// The resulting string must be freed by calling cef_string_userfree_free().
function TCefDragDataRef.GetLinkTitle: ustring;
begin
	Result := TWACef.StringFreeAndGet(PCefDragData(FData).get_link_title(
		PCefDragData(FData)
	));
end;

// Return the metadata, if any, associated with the link being dragged.
// The resulting string must be freed by calling cef_string_userfree_free().
function TCefDragDataRef.GetLinkMetadata: ustring;
begin
	Result := TWACef.StringFreeAndGet(PCefDragData(FData).get_link_metadata(
		PCefDragData(FData)
	));
end;

// Return the plain text fragment that is being dragged.
// The resulting string must be freed by calling cef_string_userfree_free().
function TCefDragDataRef.GetFragmentText: ustring;
begin
	Result := TWACef.StringFreeAndGet(PCefDragData(FData).get_fragment_text(
		PCefDragData(FData)
	));
end;

// Return the text/html fragment that is being dragged.
// The resulting string must be freed by calling cef_string_userfree_free().
function TCefDragDataRef.GetFragmentHtml: ustring;
begin
	Result := TWACef.StringFreeAndGet(PCefDragData(FData).get_fragment_html(
		PCefDragData(FData)
	));
end;

// Return the base URL that the fragment came from. This value is used for
// resolving relative URLs and may be NULL.
// The resulting string must be freed by calling cef_string_userfree_free().
function TCefDragDataRef.GetFragmentBaseUrl: ustring;
begin
	Result := TWACef.StringFreeAndGet(PCefDragData(FData).get_fragment_base_url(
		PCefDragData(FData)
	));
end;

// Return the name of the file being dragged out of the browser window.
// The resulting string must be freed by calling cef_string_userfree_free().
function TCefDragDataRef.GetFileName: ustring;
begin
	Result := TWACef.StringFreeAndGet(PCefDragData(FData).get_file_name(
		PCefDragData(FData)
	));
end;

// Write the contents of the file being dragged out of the web view into
// |writer|. Returns the number of bytes sent to |writer|. If |writer| is NULL
// this function will return the size of the file contents in bytes. Call
// get_file_name() to get a suggested name for the file.
function TCefDragDataRef.GetFileContents(const aWriter: ICefStreamWriter): csize_t;
begin
	Result := PCefDragData(FData).get_file_contents(
		PCefDragData(FData),
		TWACef.GetData(aWriter)
	);
end;

// Retrieve the list of file names that are being dragged into the browser
// window.
function TCefDragDataRef.GetFileNames(aNames: TStrings): Boolean;
var
	names_list: TCefStringList;
	names_iter: Integer;
	names_item: TCefString;
begin
	names_list := cef_string_list_alloc();
	for names_iter := 0 to aNames.Count - 1 do
	begin
		names_item := TWACef.ToCefString(aNames[names_iter]);
		cef_string_list_append(names_list, @names_item);
	end;
	Result := PCefDragData(FData).get_file_names(
		PCefDragData(FData),
		names_list
	) <> 0;
	aNames.Clear;
	FillChar(names_item, SizeOf(names_item), 0);
	for names_iter := 0 to cef_string_list_size(names_list) - 1 do
	begin
		FillChar(names_item, SizeOf(names_item), 0);
		cef_string_list_value(names_list, names_iter, @names_item);
		aNames.Add(TWACef.StringClearAndGet(names_item));
	end;	cef_string_list_free(names_list);
end;

// Set the link URL that is being dragged.
procedure TCefDragDataRef.SetLinkUrl(const aUrl: ustring);
var
	url_str: TCefString;
begin
	url_str := TWACef.ToCefString(aUrl);
	PCefDragData(FData).set_link_url(
		PCefDragData(FData),
		@url_str
	);
end;

// Set the title associated with the link being dragged.
procedure TCefDragDataRef.SetLinkTitle(const aTitle: ustring);
var
	title_str: TCefString;
begin
	title_str := TWACef.ToCefString(aTitle);
	PCefDragData(FData).set_link_title(
		PCefDragData(FData),
		@title_str
	);
end;

// Set the metadata associated with the link being dragged.
procedure TCefDragDataRef.SetLinkMetadata(const aData: ustring);
var
	data_str: TCefString;
begin
	data_str := TWACef.ToCefString(aData);
	PCefDragData(FData).set_link_metadata(
		PCefDragData(FData),
		@data_str
	);
end;

// Set the plain text fragment that is being dragged.
procedure TCefDragDataRef.SetFragmentText(const aText: ustring);
var
	text_str: TCefString;
begin
	text_str := TWACef.ToCefString(aText);
	PCefDragData(FData).set_fragment_text(
		PCefDragData(FData),
		@text_str
	);
end;

// Set the text/html fragment that is being dragged.
procedure TCefDragDataRef.SetFragmentHtml(const aHtml: ustring);
var
	html_str: TCefString;
begin
	html_str := TWACef.ToCefString(aHtml);
	PCefDragData(FData).set_fragment_html(
		PCefDragData(FData),
		@html_str
	);
end;

// Set the base URL that the fragment came from.
procedure TCefDragDataRef.SetFragmentBaseUrl(const aBaseUrl: ustring);
var
	base_url_str: TCefString;
begin
	base_url_str := TWACef.ToCefString(aBaseUrl);
	PCefDragData(FData).set_fragment_base_url(
		PCefDragData(FData),
		@base_url_str
	);
end;

// Reset the file contents. You should do this before calling
// cef_browser_host_t::DragTargetDragEnter as the web view does not allow us
// to drag in this kind of data.
procedure TCefDragDataRef.ResetFileContents;
begin
	PCefDragData(FData).reset_file_contents(
		PCefDragData(FData)
	);
end;

// Add a file that is being dragged into the webview.
procedure TCefDragDataRef.AddFile(const aPath: ustring; const aDisplayName: ustring);
var
	path_str: TCefString;
	display_name_str: TCefString;
begin
	path_str := TWACef.ToCefString(aPath);
	display_name_str := TWACef.ToCefString(aDisplayName);
	PCefDragData(FData).add_file(
		PCefDragData(FData),
		@path_str,
		@display_name_str
	);
end;

{Public section}
class function TCefDragDataRef.UnWrap(data: Pointer): ICefDragData;
begin
	if data <> nil then
		Result := Create(data) as ICefDragData
	else
		Result := nil;
end;
// Create a new cef_drag_data_t object.
class function TCefDragDataRef.New: ICefDragData;
begin
	Result := TCefDragDataRef.UnWrap(cef_drag_data_create(
	));
end;


//..............................................................................TCefDragHandlerRef
{Protected section}
// Called when an external drag event enters the browser window. |dragData|
// contains the drag event data and |mask| represents the type of drag
// operation. Return false (0) for default drag handling behavior or true (1)
// to cancel the drag event.
function TCefDragHandlerRef.OnDragEnter(const aBrowser: ICefBrowser; const aDragData: ICefDragData; aMask: TCefDragOperationsMask): Boolean;
begin
	Result := PCefDragHandler(FData).on_drag_enter(
		PCefDragHandler(FData),
		TWACef.GetData(aBrowser),
		TWACef.GetData(aDragData),
		aMask
	) <> 0;
end;

{Public section}
class function TCefDragHandlerRef.UnWrap(data: Pointer): ICefDragHandler;
begin
	if data <> nil then
		Result := Create(data) as ICefDragHandler
	else
		Result := nil;
end;

//..............................................................................TCefFindHandlerRef
{Protected section}
// Called to report find results returned by cef_browser_host_t::find().
// |identifer| is the identifier passed to find(), |count| is the number of
// matches currently identified, |selectionRect| is the location of where the
// match was found (in window coordinates), |activeMatchOrdinal| is the
// current position in the search results, and |finalUpdate| is true (1) if
// this is the last find notification.
procedure TCefFindHandlerRef.OnFindResult(const aBrowser: ICefBrowser; aIdentifier: cint; aCount: cint; const aSelectionRect: TCefRect; aActiveMatchOrdinal: cint; aFinalUpdate: Boolean);
begin
	PCefFindHandler(FData).on_find_result(
		PCefFindHandler(FData),
		TWACef.GetData(aBrowser),
		aIdentifier,
		aCount,
		@aSelectionRect,
		aActiveMatchOrdinal,
		Ord(aFinalUpdate)
	);
end;

{Public section}
class function TCefFindHandlerRef.UnWrap(data: Pointer): ICefFindHandler;
begin
	if data <> nil then
		Result := Create(data) as ICefFindHandler
	else
		Result := nil;
end;

//..............................................................................TCefFocusHandlerRef
{Protected section}
// Called when the browser component is about to loose focus. For instance, if
// focus was on the last HTML element and the user pressed the TAB key. |next|
// will be true (1) if the browser is giving focus to the next component and
// false (0) if the browser is giving focus to the previous component.
procedure TCefFocusHandlerRef.OnTakeFocus(const aBrowser: ICefBrowser; aNext: Boolean);
begin
	PCefFocusHandler(FData).on_take_focus(
		PCefFocusHandler(FData),
		TWACef.GetData(aBrowser),
		Ord(aNext)
	);
end;

// Called when the browser component is requesting focus. |source| indicates
// where the focus request is originating from. Return false (0) to allow the
// focus to be set or true (1) to cancel setting the focus.
function TCefFocusHandlerRef.OnSetFocus(const aBrowser: ICefBrowser; aSource: TCefFocusSource): Boolean;
begin
	Result := PCefFocusHandler(FData).on_set_focus(
		PCefFocusHandler(FData),
		TWACef.GetData(aBrowser),
		aSource
	) <> 0;
end;

// Called when the browser component has received focus.
procedure TCefFocusHandlerRef.OnGotFocus(const aBrowser: ICefBrowser);
begin
	PCefFocusHandler(FData).on_got_focus(
		PCefFocusHandler(FData),
		TWACef.GetData(aBrowser)
	);
end;

{Public section}
class function TCefFocusHandlerRef.UnWrap(data: Pointer): ICefFocusHandler;
begin
	if data <> nil then
		Result := Create(data) as ICefFocusHandler
	else
		Result := nil;
end;

//..............................................................................TCefFrameRef
{Protected section}
// True if this object is currently attached to a valid frame.
function TCefFrameRef.IsValid: Boolean;
begin
	Result := PCefFrame(FData).is_valid(
		PCefFrame(FData)
	) <> 0;
end;

// Execute undo in this frame.
procedure TCefFrameRef.Undo;
begin
	PCefFrame(FData).undo(
		PCefFrame(FData)
	);
end;

// Execute redo in this frame.
procedure TCefFrameRef.Redo;
begin
	PCefFrame(FData).redo(
		PCefFrame(FData)
	);
end;

// Execute cut in this frame.
procedure TCefFrameRef.Cut;
begin
	PCefFrame(FData).cut(
		PCefFrame(FData)
	);
end;

// Execute copy in this frame.
procedure TCefFrameRef.Copy;
begin
	PCefFrame(FData).copy(
		PCefFrame(FData)
	);
end;

// Execute paste in this frame.
procedure TCefFrameRef.Paste;
begin
	PCefFrame(FData).paste(
		PCefFrame(FData)
	);
end;

// Execute delete in this frame.
procedure TCefFrameRef.Del;
begin
	PCefFrame(FData).del(
		PCefFrame(FData)
	);
end;

// Execute select all in this frame.
procedure TCefFrameRef.SelectAll;
begin
	PCefFrame(FData).select_all(
		PCefFrame(FData)
	);
end;

// Save this frame's HTML source to a temporary file and open it in the
// default text viewing application. This function can only be called from the
// browser process.
procedure TCefFrameRef.ViewSource;
begin
	PCefFrame(FData).view_source(
		PCefFrame(FData)
	);
end;

// Retrieve this frame's HTML source as a string sent to the specified
// visitor.
procedure TCefFrameRef.GetSource(const aVisitor: ICefStringVisitor);
begin
	PCefFrame(FData).get_source(
		PCefFrame(FData),
		TWACef.GetData(aVisitor)
	);
end;

procedure TCefFrameRef.GetSourceProc(const aProc: TCefStringVisitorProc);
begin
  GetSource(TCefFastStringVisitor.Create(aProc));
end;

// Retrieve this frame's display text as a string sent to the specified
// visitor.
procedure TCefFrameRef.GetText(const aVisitor: ICefStringVisitor);
begin
	PCefFrame(FData).get_text(
		PCefFrame(FData),
		TWACef.GetData(aVisitor)
	);
end;

procedure TCefFrameRef.GetTextProc(const aProc: TCefStringVisitorProc);
begin
  GetText(TCefFastStringVisitor.Create(aProc));
end;

// Load the request represented by the |request| object.
procedure TCefFrameRef.LoadRequest(const aRequest: ICefRequest);
begin
	PCefFrame(FData).load_request(
		PCefFrame(FData),
		TWACef.GetData(aRequest)
	);
end;

// Load the specified |url|.
procedure TCefFrameRef.LoadUrl(const aUrl: ustring);
var
	url_str: TCefString;
begin
	url_str := TWACef.ToCefString(aUrl);
	PCefFrame(FData).load_url(
		PCefFrame(FData),
		@url_str
	);
end;

// Load the contents of |string_val| with the specified dummy |url|. |url|
// should have a standard scheme (for example, http scheme) or behaviors like
// link clicks and web security restrictions may not behave as expected.
procedure TCefFrameRef.LoadString(const aStringVal: ustring; const aUrl: ustring);
var
	string_val_str: TCefString;
	url_str: TCefString;
begin
	string_val_str := TWACef.ToCefString(aStringVal);
	url_str := TWACef.ToCefString(aUrl);
	PCefFrame(FData).load_string(
		PCefFrame(FData),
		@string_val_str,
		@url_str
	);
end;

// Execute a string of JavaScript code in this frame. The |script_url|
// parameter is the URL where the script in question can be found, if any. The
// renderer may request this URL to show the developer the source of the
// error.  The |start_line| parameter is the base line number to use for error
// reporting.
procedure TCefFrameRef.ExecuteJavaScript(const aCode: ustring; const aScriptUrl: ustring; aStartLine: cint);
var
	code_str: TCefString;
	script_url_str: TCefString;
begin
	code_str := TWACef.ToCefString(aCode);
	script_url_str := TWACef.ToCefString(aScriptUrl);
	PCefFrame(FData).execute_java_script(
		PCefFrame(FData),
		@code_str,
		@script_url_str,
		aStartLine
	);
end;

// Returns true (1) if this is the main (top-level) frame.
function TCefFrameRef.IsMain: Boolean;
begin
	Result := PCefFrame(FData).is_main(
		PCefFrame(FData)
	) <> 0;
end;

// Returns true (1) if this is the focused frame.
function TCefFrameRef.IsFocused: Boolean;
begin
	Result := PCefFrame(FData).is_focused(
		PCefFrame(FData)
	) <> 0;
end;

// Returns the name for this frame. If the frame has an assigned name (for
// example, set via the iframe "name" attribute) then that value will be
// returned. Otherwise a unique name will be constructed based on the frame
// parent hierarchy. The main (top-level) frame will always have an NULL name
// value.
// The resulting string must be freed by calling cef_string_userfree_free().
function TCefFrameRef.GetName: ustring;
begin
	Result := TWACef.StringFreeAndGet(PCefFrame(FData).get_name(
		PCefFrame(FData)
	));
end;

// Returns the globally unique identifier for this frame.
function TCefFrameRef.GetIdentifier: cint64;
begin
	Result := PCefFrame(FData).get_identifier(
		PCefFrame(FData)
	);
end;

// Returns the parent of this frame or NULL if this is the main (top-level)
// frame.
function TCefFrameRef.GetParent: ICefFrame;
begin
	Result := TCefFrameRef.UnWrap(PCefFrame(FData).get_parent(
		PCefFrame(FData)
	));
end;

// Returns the URL currently loaded in this frame.
// The resulting string must be freed by calling cef_string_userfree_free().
function TCefFrameRef.GetUrl: ustring;
begin
	Result := TWACef.StringFreeAndGet(PCefFrame(FData).get_url(
		PCefFrame(FData)
	));
end;

// Returns the browser that this frame belongs to.
function TCefFrameRef.GetBrowser: ICefBrowser;
begin
	Result := TCefBrowserRef.UnWrap(PCefFrame(FData).get_browser(
		PCefFrame(FData)
	));
end;

// Get the V8 context associated with the frame. This function can only be
// called from the render process.
function TCefFrameRef.GetV8context: ICefV8context;
begin
	Result := TCefV8contextRef.UnWrap(PCefFrame(FData).get_v8context(
		PCefFrame(FData)
	));
end;

// Visit the DOM document. This function can only be called from the render
// process.
procedure TCefFrameRef.VisitDom(const aVisitor: ICefDomvisitor);
begin
	PCefFrame(FData).visit_dom(
		PCefFrame(FData),
		TWACef.GetData(aVisitor)
	);
end;

procedure TCefFrameRef.VisitDomProc(const aProc: TCefDomVisitorProc);
begin
  VisitDom(TCefFastDomVisitor.Create(aProc) as ICefDomVisitor);
end;


{Public section}
class function TCefFrameRef.UnWrap(data: Pointer): ICefFrame;
begin
	if data <> nil then
		Result := Create(data) as ICefFrame
	else
		Result := nil;
end;

//..............................................................................TCefGetGeolocationCallbackRef
{Protected section}
// Called with the 'best available' location information or, if the location
// update failed, with error information.
procedure TCefGetGeolocationCallbackRef.OnLocationUpdate(const aPosition: TCefGeoposition);
var
	position_proxy: TCefGeoposition;
begin
	position_proxy := aPosition;
	PCefGetGeolocationCallback(FData).on_location_update(
		PCefGetGeolocationCallback(FData),
		@position_proxy
	);
end;

{Public section}
class function TCefGetGeolocationCallbackRef.UnWrap(data: Pointer): ICefGetGeolocationCallback;
begin
	if data <> nil then
		Result := Create(data) as ICefGetGeolocationCallback
	else
		Result := nil;
end;

//..............................................................................TCefGeolocationCallbackRef
{Protected section}
// Call to allow or deny geolocation access.
procedure TCefGeolocationCallbackRef.Cont(aAllow: Boolean);
begin
	PCefGeolocationCallback(FData).cont(
		PCefGeolocationCallback(FData),
		Ord(aAllow)
	);
end;

{Public section}
class function TCefGeolocationCallbackRef.UnWrap(data: Pointer): ICefGeolocationCallback;
begin
	if data <> nil then
		Result := Create(data) as ICefGeolocationCallback
	else
		Result := nil;
end;
//..............................................................................TCefGeolocationHandlerRef
{Protected section}
// Called when a page requests permission to access geolocation information.
// |requesting_url| is the URL requesting permission and |request_id| is the
// unique ID for the permission request. Return true (1) and call
// cef_geolocation_callback_t::cont() either in this function or at a later
// time to continue or cancel the request. Return false (0) to cancel the
// request immediately.
function TCefGeolocationHandlerRef.OnRequestGeolocationPermission(const aBrowser: ICefBrowser; const aRequestingUrl: ustring; aRequestId: cint; const aCallback: ICefGeolocationCallback): Boolean;
var
	requesting_url_str: TCefString;
begin
	requesting_url_str := TWACef.ToCefString(aRequestingUrl);
	Result := PCefGeolocationHandler(FData).on_request_geolocation_permission(
		PCefGeolocationHandler(FData),
		TWACef.GetData(aBrowser),
		@requesting_url_str,
		aRequestId,
		TWACef.GetData(aCallback)
	) <> 0;
end;

// Called when a geolocation access request is canceled. |requesting_url| is
// the URL that originally requested permission and |request_id| is the unique
// ID for the permission request.
procedure TCefGeolocationHandlerRef.OnCancelGeolocationPermission(const aBrowser: ICefBrowser; const aRequestingUrl: ustring; aRequestId: cint);
var
	requesting_url_str: TCefString;
begin
	requesting_url_str := TWACef.ToCefString(aRequestingUrl);
	PCefGeolocationHandler(FData).on_cancel_geolocation_permission(
		PCefGeolocationHandler(FData),
		TWACef.GetData(aBrowser),
		@requesting_url_str,
		aRequestId
	);
end;

{Public section}
class function TCefGeolocationHandlerRef.UnWrap(data: Pointer): ICefGeolocationHandler;
begin
	if data <> nil then
		Result := Create(data) as ICefGeolocationHandler
	else
		Result := nil;
end;

//..............................................................................TCefJsdialogCallbackRef
{Protected section}
// Continue the JS dialog request. Set |success| to true (1) if the OK button
// was pressed. The |user_input| value should be specified for prompt dialogs.
procedure TCefJsdialogCallbackRef.Cont(aSuccess: Boolean; const aUserInput: ustring);
var
	success_proxy: cint;
	user_input_str: TCefString;
begin
	success_proxy := Ord(aSuccess);
	user_input_str := TWACef.ToCefString(aUserInput);
	PCefJsdialogCallback(FData).cont(
		PCefJsdialogCallback(FData),
		success_proxy,
		@user_input_str
	);
end;

{Public section}
class function TCefJsdialogCallbackRef.UnWrap(data: Pointer): ICefJsdialogCallback;
begin
	if data <> nil then
		Result := Create(data) as ICefJsdialogCallback
	else
		Result := nil;
end;
//..............................................................................TCefJsdialogHandlerRef
{Protected section}
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
function TCefJsdialogHandlerRef.OnJsdialog(const aBrowser: ICefBrowser; const aOriginUrl: ustring; const aAcceptLang: ustring; aDialogType: TCefJsdialogType; const aMessageText: ustring; const aDefaultPromptText: ustring; const aCallback: ICefJsdialogCallback; out aSuppressMessage: Boolean): Boolean;
var
	origin_url_str: TCefString;
	accept_lang_str: TCefString;
	message_text_str: TCefString;
	default_prompt_text_str: TCefString;
	suppress_message_proxy: cint;
begin
	origin_url_str := TWACef.ToCefString(aOriginUrl);
	accept_lang_str := TWACef.ToCefString(aAcceptLang);
	message_text_str := TWACef.ToCefString(aMessageText);
	default_prompt_text_str := TWACef.ToCefString(aDefaultPromptText);
	suppress_message_proxy := Ord(aSuppressMessage);
	Result := PCefJsdialogHandler(FData).on_jsdialog(
		PCefJsdialogHandler(FData),
		TWACef.GetData(aBrowser),
		@origin_url_str,
		@accept_lang_str,
		aDialogType,
		@message_text_str,
		@default_prompt_text_str,
		TWACef.GetData(aCallback),
		@suppress_message_proxy
	) <> 0;
	aSuppressMessage := suppress_message_proxy <> 0;
end;

// Called to run a dialog asking the user if they want to leave a page. Return
// false (0) to use the default dialog implementation. Return true (1) if the
// application will use a custom dialog or if the callback has been executed
// immediately. Custom dialogs may be either modal or modeless. If a custom
// dialog is used the application must execute |callback| once the custom
// dialog is dismissed.
function TCefJsdialogHandlerRef.OnBeforeUnloadDialog(const aBrowser: ICefBrowser; const aMessageText: ustring; aIsReload: Boolean; const aCallback: ICefJsdialogCallback): Boolean;
var
	message_text_str: TCefString;
begin
	message_text_str := TWACef.ToCefString(aMessageText);
	Result := PCefJsdialogHandler(FData).on_before_unload_dialog(
		PCefJsdialogHandler(FData),
		TWACef.GetData(aBrowser),
		@message_text_str,
		Ord(aIsReload),
		TWACef.GetData(aCallback)
	) <> 0;
end;

// Called to cancel any pending dialogs and reset any saved dialog state. Will
// be called due to events like page navigation irregardless of whether any
// dialogs are currently pending.
procedure TCefJsdialogHandlerRef.OnResetDialogState(const aBrowser: ICefBrowser);
begin
	PCefJsdialogHandler(FData).on_reset_dialog_state(
		PCefJsdialogHandler(FData),
		TWACef.GetData(aBrowser)
	);
end;

// Called when the default implementation dialog is closed.
procedure TCefJsdialogHandlerRef.OnDialogClosed(const aBrowser: ICefBrowser);
begin
	PCefJsdialogHandler(FData).on_dialog_closed(
		PCefJsdialogHandler(FData),
		TWACef.GetData(aBrowser)
	);
end;

{Public section}
class function TCefJsdialogHandlerRef.UnWrap(data: Pointer): ICefJsdialogHandler;
begin
	if data <> nil then
		Result := Create(data) as ICefJsdialogHandler
	else
		Result := nil;
end;

//..............................................................................TCefKeyboardHandlerRef
// Called before a keyboard event is sent to the renderer. |event| contains
// information about the keyboard event. |os_event| is the operating system
// event message, if any. Return true (1) if the event was handled or false
// (0) otherwise. If the event will be handled in on_key_event() as a keyboard
// shortcut set |is_keyboard_shortcut| to true (1) and return false (0).
function TCefKeyboardHandlerRef.OnPreKeyEvent(const aBrowser: ICefBrowser; var aEvent: TCefKeyEvent; aOsEvent: TCefEventHandle; out aIsKeyboardShortcut: Boolean): Boolean;
var
	is_keyboard_shortcut_proxy: cint;
begin
	is_keyboard_shortcut_proxy := Ord(aIsKeyboardShortcut);
	Result := PCefKeyboardHandler(FData).on_pre_key_event(
		PCefKeyboardHandler(FData),
		TWACef.GetData(aBrowser),
		@aEvent,
		aOsEvent,
		@is_keyboard_shortcut_proxy
	) <> 0;
	aIsKeyboardShortcut := is_keyboard_shortcut_proxy <> 0;
end;

// Called after the renderer and JavaScript in the page has had a chance to
// handle the event. |event| contains information about the keyboard event.
// |os_event| is the operating system event message, if any. Return true (1)
// if the keyboard event was handled or false (0) otherwise.
function TCefKeyboardHandlerRef.OnKeyEvent(const aBrowser: ICefBrowser; var aEvent: TCefKeyEvent; aOsEvent: TCefEventHandle): Boolean;
begin
	Result := PCefKeyboardHandler(FData).on_key_event(
		PCefKeyboardHandler(FData),
		TWACef.GetData(aBrowser),
		@aEvent,
		aOsEvent
	) <> 0;
end;

{Public section}
class function TCefKeyboardHandlerRef.UnWrap(data: Pointer): ICefKeyboardHandler;
begin
	if data <> nil then
		Result := Create(data) as ICefKeyboardHandler
	else
		Result := nil;
end;

//..............................................................................TCefLifeSpanHandlerRef
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
function TCefLifeSpanHandlerRef.OnBeforePopup(const aBrowser: ICefBrowser; const aFrame: ICefFrame;
  var aTargetUrl: ustring; const aTargetFrameName: ustring; aTargetDisposition: TCefWindowOpenDisposition;
  aUserGesture: Boolean; var aPopupFeatures: TCefPopupFeatures; var aWindowInfo: TCefWindowInfo;
  var aClient: ICefClient; var aSettings: TCefBrowserSettings; var aNoJavascriptAccess: Boolean): Boolean;
var
	target_url_str: TCefString;
	target_frame_name_str: TCefString;
	popupFeatures_proxy: TCefPopupFeatures;
	windowInfo_proxy: TCefWindowInfo;
	settings_proxy: TCefBrowserSettings;
	no_javascript_access_proxy: cint;
  client_proxy: PCefClient;
begin
	target_url_str := TWACef.ToCefString(aTargetUrl);
	target_frame_name_str := TWACef.ToCefString(aTargetFrameName);
	popupFeatures_proxy := aPopupFeatures;
	windowInfo_proxy := aWindowInfo;
	settings_proxy := aSettings;
	no_javascript_access_proxy := Ord(aNoJavascriptAccess);
  client_proxy := TWACef.GetData(aClient);
	Result := PCefLifeSpanHandler(FData).on_before_popup(
		PCefLifeSpanHandler(FData),
		TWACef.GetData(aBrowser),
		TWACef.GetData(aFrame),
		@target_url_str,
		@target_frame_name_str,
		aTargetDisposition,
		Ord(aUserGesture),
		@popupFeatures_proxy,
		@windowInfo_proxy,
		client_proxy,
		@settings_proxy,
		@no_javascript_access_proxy
	) <> 0;
	aPopupFeatures := popupFeatures_proxy;
	aWindowInfo := windowInfo_proxy;
	aSettings := settings_proxy;
	aNoJavascriptAccess := no_javascript_access_proxy <> 0;
  aClient := TCefClientRef.UnWrap(client_proxy);
end;

// Called after a new browser is created.
procedure TCefLifeSpanHandlerRef.OnAfterCreated(const aBrowser: ICefBrowser);
begin
	PCefLifeSpanHandler(FData).on_after_created(
		PCefLifeSpanHandler(FData),
		TWACef.GetData(aBrowser)
	);
end;

// Called when a modal window is about to display and the modal loop should
// begin running. Return false (0) to use the default modal loop
// implementation or true (1) to use a custom implementation.
function TCefLifeSpanHandlerRef.RunModal(const aBrowser: ICefBrowser): Boolean;
begin
	Result := PCefLifeSpanHandler(FData).run_modal(
		PCefLifeSpanHandler(FData),
		TWACef.GetData(aBrowser)
	) <> 0;
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
function TCefLifeSpanHandlerRef.DoClose(const aBrowser: ICefBrowser): Boolean;
begin
	Result := PCefLifeSpanHandler(FData).do_close(
		PCefLifeSpanHandler(FData),
		TWACef.GetData(aBrowser)
	) <> 0;
end;

// Called just before a browser is destroyed. Release all references to the
// browser object and do not attempt to execute any functions on the browser
// object after this callback returns. If this is a modal window and a custom
// modal loop implementation was provided in run_modal() this callback should
// be used to exit the custom modal loop. See do_close() documentation for
// additional usage information.
procedure TCefLifeSpanHandlerRef.OnBeforeClose(const aBrowser: ICefBrowser);
begin
	PCefLifeSpanHandler(FData).on_before_close(
		PCefLifeSpanHandler(FData),
		TWACef.GetData(aBrowser)
	);
end;

{Public section}
class function TCefLifeSpanHandlerRef.UnWrap(data: Pointer): ICefLifeSpanHandler;
begin
	if data <> nil then
		Result := Create(data) as ICefLifeSpanHandler
	else
		Result := nil;
end;

//..............................................................................TCefLoadHandlerRef
{Protected section}
// Called when the loading state has changed. This callback will be executed
// twice -- once when loading is initiated either programmatically or by user
// action, and once when loading is terminated due to completion, cancellation
// of failure.
procedure TCefLoadHandlerRef.OnLoadingStateChange(const aBrowser: ICefBrowser; aIsLoading: Boolean; aCanGoBack: Boolean; aCanGoForward: Boolean);
begin
	PCefLoadHandler(FData).on_loading_state_change(
		PCefLoadHandler(FData),
		TWACef.GetData(aBrowser),
		Ord(aIsLoading),
		Ord(aCanGoBack),
		Ord(aCanGoForward)
	);
end;

// Called when the browser begins loading a frame. The |frame| value will
// never be NULL -- call the is_main() function to check if this frame is the
// main frame. Multiple frames may be loading at the same time. Sub-frames may
// start or continue loading after the main frame load has ended. This
// function may not be called for a particular frame if the load request for
// that frame fails. For notification of overall browser load status use
// OnLoadingStateChange instead.
procedure TCefLoadHandlerRef.OnLoadStart(const aBrowser: ICefBrowser; const aFrame: ICefFrame);
begin
	PCefLoadHandler(FData).on_load_start(
		PCefLoadHandler(FData),
		TWACef.GetData(aBrowser),
		TWACef.GetData(aFrame)
	);
end;

// Called when the browser is done loading a frame. The |frame| value will
// never be NULL -- call the is_main() function to check if this frame is the
// main frame. Multiple frames may be loading at the same time. Sub-frames may
// start or continue loading after the main frame load has ended. This
// function will always be called for all frames irrespective of whether the
// request completes successfully.
procedure TCefLoadHandlerRef.OnLoadEnd(const aBrowser: ICefBrowser; const aFrame: ICefFrame; aHttpStatusCode: cint);
begin
	PCefLoadHandler(FData).on_load_end(
		PCefLoadHandler(FData),
		TWACef.GetData(aBrowser),
		TWACef.GetData(aFrame),
		aHttpStatusCode
	);
end;

// Called when the resource load for a navigation fails or is canceled.
// |errorCode| is the error code number, |errorText| is the error text and
// |failedUrl| is the URL that failed to load. See net\base\net_error_list.h
// for complete descriptions of the error codes.
procedure TCefLoadHandlerRef.OnLoadError(const aBrowser: ICefBrowser; const aFrame: ICefFrame; aErrorCode: TCefErrorcode; const aErrorText: ustring; const aFailedUrl: ustring);
var
	errorText_str: TCefString;
	failedUrl_str: TCefString;
begin
	errorText_str := TWACef.ToCefString(aErrorText);
	failedUrl_str := TWACef.ToCefString(aFailedUrl);
	PCefLoadHandler(FData).on_load_error(
		PCefLoadHandler(FData),
		TWACef.GetData(aBrowser),
		TWACef.GetData(aFrame),
		aErrorCode,
		@errorText_str,
		@failedUrl_str
	);
end;

{Public section}
class function TCefLoadHandlerRef.UnWrap(data: Pointer): ICefLoadHandler;
begin
	if data <> nil then
		Result := Create(data) as ICefLoadHandler
	else
		Result := nil;
end;

//..............................................................................TCefMenuModelRef
// Clears the menu. Returns true (1) on success.
function TCefMenuModelRef.Clear: Boolean;
begin
	Result := PCefMenuModel(FData).clear(
		PCefMenuModel(FData)
	) <> 0;
end;

// Returns the number of items in this menu.
function TCefMenuModelRef.GetCount: cint;
begin
	Result := PCefMenuModel(FData).get_count(
		PCefMenuModel(FData)
	);
end;

//
// Add a separator to the menu. Returns true (1) on success.
function TCefMenuModelRef.AddSeparator: Boolean;
begin
	Result := PCefMenuModel(FData).add_separator(
		PCefMenuModel(FData)
	) <> 0;
end;

//
// Add an item to the menu. Returns true (1) on success.
function TCefMenuModelRef.AddItem(aCommandId: cint; const aLabel: ustring): Boolean;
var
	label_str: TCefString;
begin
	label_str := TWACef.ToCefString(aLabel);
	Result := PCefMenuModel(FData).add_item(
		PCefMenuModel(FData),
		aCommandId,
		@label_str
	) <> 0;
end;

//
// Add a check item to the menu. Returns true (1) on success.
function TCefMenuModelRef.AddCheckItem(aCommandId: cint; const aLabel: ustring): Boolean;
var
	label_str: TCefString;
begin
	label_str := TWACef.ToCefString(aLabel);
	Result := PCefMenuModel(FData).add_check_item(
		PCefMenuModel(FData),
		aCommandId,
		@label_str
	) <> 0;
end;

//
// Add a radio item to the menu. Only a single item with the specified
// |group_id| can be checked at a time. Returns true (1) on success.
function TCefMenuModelRef.AddRadioItem(aCommandId: cint; const aLabel: ustring; aGroupId: cint): Boolean;
var
	label_str: TCefString;
begin
	label_str := TWACef.ToCefString(aLabel);
	Result := PCefMenuModel(FData).add_radio_item(
		PCefMenuModel(FData),
		aCommandId,
		@label_str,
		aGroupId
	) <> 0;
end;

//
// Add a sub-menu to the menu. The new sub-menu is returned.
function TCefMenuModelRef.AddSubMenu(aCommandId: cint; const aLabel: ustring): ICefMenuModel;
var
	label_str: TCefString;
begin
	label_str := TWACef.ToCefString(aLabel);
	Result := TCefMenuModelRef.UnWrap(PCefMenuModel(FData).add_sub_menu(
		PCefMenuModel(FData),
		aCommandId,
		@label_str
	));
end;

//
// Insert a separator in the menu at the specified |index|. Returns true (1)
// on success.
function TCefMenuModelRef.InsertSeparatorAt(aIndex: cint): Boolean;
begin
	Result := PCefMenuModel(FData).insert_separator_at(
		PCefMenuModel(FData),
		aIndex
	) <> 0;
end;

//
// Insert an item in the menu at the specified |index|. Returns true (1) on
// success.
function TCefMenuModelRef.InsertItemAt(aIndex: cint; aCommandId: cint; const aLabel: ustring): Boolean;
var
	label_str: TCefString;
begin
	label_str := TWACef.ToCefString(aLabel);
	Result := PCefMenuModel(FData).insert_item_at(
		PCefMenuModel(FData),
		aIndex,
		aCommandId,
		@label_str
	) <> 0;
end;

//
// Insert a check item in the menu at the specified |index|. Returns true (1)
// on success.
function TCefMenuModelRef.InsertCheckItemAt(aIndex: cint; aCommandId: cint; const aLabel: ustring): Boolean;
var
	label_str: TCefString;
begin
	label_str := TWACef.ToCefString(aLabel);
	Result := PCefMenuModel(FData).insert_check_item_at(
		PCefMenuModel(FData),
		aIndex,
		aCommandId,
		@label_str
	) <> 0;
end;

//
// Insert a radio item in the menu at the specified |index|. Only a single
// item with the specified |group_id| can be checked at a time. Returns true
// (1) on success.
function TCefMenuModelRef.InsertRadioItemAt(aIndex: cint; aCommandId: cint; const aLabel: ustring; aGroupId: cint): Boolean;
var
	label_str: TCefString;
begin
	label_str := TWACef.ToCefString(aLabel);
	Result := PCefMenuModel(FData).insert_radio_item_at(
		PCefMenuModel(FData),
		aIndex,
		aCommandId,
		@label_str,
		aGroupId
	) <> 0;
end;

//
// Insert a sub-menu in the menu at the specified |index|. The new sub-menu is
// returned.
function TCefMenuModelRef.InsertSubMenuAt(aIndex: cint; aCommandId: cint; const aLabel: ustring): ICefMenuModel;
var
	label_str: TCefString;
begin
	label_str := TWACef.ToCefString(aLabel);
	Result := TCefMenuModelRef.UnWrap(PCefMenuModel(FData).insert_sub_menu_at(
		PCefMenuModel(FData),
		aIndex,
		aCommandId,
		@label_str
	));
end;

// Removes the item with the specified |command_id|. Returns true (1) on
// success.
function TCefMenuModelRef.Remove(aCommandId: cint): Boolean;
begin
	Result := PCefMenuModel(FData).remove(
		PCefMenuModel(FData),
		aCommandId
	) <> 0;
end;

// Removes the item at the specified |index|. Returns true (1) on success.
function TCefMenuModelRef.RemoveAt(aIndex: cint): Boolean;
begin
	Result := PCefMenuModel(FData).remove_at(
		PCefMenuModel(FData),
		aIndex
	) <> 0;
end;

// Returns the index associated with the specified |command_id| or -1 if not
// found due to the command id not existing in the menu.
function TCefMenuModelRef.GetIndexOf(aCommandId: cint): cint;
begin
	Result := PCefMenuModel(FData).get_index_of(
		PCefMenuModel(FData),
		aCommandId
	);
end;

// Returns the command id at the specified |index| or -1 if not found due to
// invalid range or the index being a separator.
function TCefMenuModelRef.GetCommandIdAt(aIndex: cint): cint;
begin
	Result := PCefMenuModel(FData).get_command_id_at(
		PCefMenuModel(FData),
		aIndex
	);
end;

// Sets the command id at the specified |index|. Returns true (1) on success.
function TCefMenuModelRef.SetCommandIdAt(aIndex: cint; aCommandId: cint): Boolean;
begin
	Result := PCefMenuModel(FData).set_command_id_at(
		PCefMenuModel(FData),
		aIndex,
		aCommandId
	) <> 0;
end;

// Returns the label for the specified |command_id| or NULL if not found.
// The resulting string must be freed by calling cef_string_userfree_free().
function TCefMenuModelRef.GetLabel(aCommandId: cint): ustring;
begin
	Result := TWACef.StringFreeAndGet(PCefMenuModel(FData).get_label(
		PCefMenuModel(FData),
		aCommandId
	));
end;

// Returns the label at the specified |index| or NULL if not found due to
// invalid range or the index being a separator.
// The resulting string must be freed by calling cef_string_userfree_free().
function TCefMenuModelRef.GetLabelAt(aIndex: cint): ustring;
begin
	Result := TWACef.StringFreeAndGet(PCefMenuModel(FData).get_label_at(
		PCefMenuModel(FData),
		aIndex
	));
end;

// Sets the label for the specified |command_id|. Returns true (1) on success.
function TCefMenuModelRef.SetLabel(aCommandId: cint; const aLabel: ustring): Boolean;
var
	label_str: TCefString;
begin
	label_str := TWACef.ToCefString(aLabel);
	Result := PCefMenuModel(FData).set_label(
		PCefMenuModel(FData),
		aCommandId,
		@label_str
	) <> 0;
end;

// Set the label at the specified |index|. Returns true (1) on success.
function TCefMenuModelRef.SetLabelAt(aIndex: cint; const aLabel: ustring): Boolean;
var
	label_str: TCefString;
begin
	label_str := TWACef.ToCefString(aLabel);
	Result := PCefMenuModel(FData).set_label_at(
		PCefMenuModel(FData),
		aIndex,
		@label_str
	) <> 0;
end;

// Returns the item type for the specified |command_id|.
function TCefMenuModelRef.GetType(aCommandId: cint): TCefMenuItemType;
begin
	Result := PCefMenuModel(FData).get_type(
		PCefMenuModel(FData),
		aCommandId
	);
end;

// Returns the item type at the specified |index|.
function TCefMenuModelRef.GetTypeAt(aIndex: cint): TCefMenuItemType;
begin
	Result := PCefMenuModel(FData).get_type_at(
		PCefMenuModel(FData),
		aIndex
	);
end;

// Returns the group id for the specified |command_id| or -1 if invalid.
function TCefMenuModelRef.GetGroupId(aCommandId: cint): cint;
begin
	Result := PCefMenuModel(FData).get_group_id(
		PCefMenuModel(FData),
		aCommandId
	);
end;

// Returns the group id at the specified |index| or -1 if invalid.
function TCefMenuModelRef.GetGroupIdAt(aIndex: cint): cint;
begin
	Result := PCefMenuModel(FData).get_group_id_at(
		PCefMenuModel(FData),
		aIndex
	);
end;

// Sets the group id for the specified |command_id|. Returns true (1) on
// success.
function TCefMenuModelRef.SetGroupId(aCommandId: cint; aGroupId: cint): Boolean;
begin
	Result := PCefMenuModel(FData).set_group_id(
		PCefMenuModel(FData),
		aCommandId,
		aGroupId
	) <> 0;
end;

// Sets the group id at the specified |index|. Returns true (1) on success.
function TCefMenuModelRef.SetGroupIdAt(aIndex: cint; aGroupId: cint): Boolean;
begin
	Result := PCefMenuModel(FData).set_group_id_at(
		PCefMenuModel(FData),
		aIndex,
		aGroupId
	) <> 0;
end;

// Returns the submenu for the specified |command_id| or NULL if invalid.
function TCefMenuModelRef.GetSubMenu(aCommandId: cint): ICefMenuModel;
begin
	Result := TCefMenuModelRef.UnWrap(PCefMenuModel(FData).get_sub_menu(
		PCefMenuModel(FData),
		aCommandId
	));
end;

// Returns the submenu at the specified |index| or NULL if invalid.
function TCefMenuModelRef.GetSubMenuAt(aIndex: cint): ICefMenuModel;
begin
	Result := TCefMenuModelRef.UnWrap(PCefMenuModel(FData).get_sub_menu_at(
		PCefMenuModel(FData),
		aIndex
	));
end;

//
// Returns true (1) if the specified |command_id| is visible.
function TCefMenuModelRef.IsVisible(aCommandId: cint): Boolean;
begin
	Result := PCefMenuModel(FData).is_visible(
		PCefMenuModel(FData),
		aCommandId
	) <> 0;
end;

//
// Returns true (1) if the specified |index| is visible.
function TCefMenuModelRef.IsVisibleAt(aIndex: cint): Boolean;
begin
	Result := PCefMenuModel(FData).is_visible_at(
		PCefMenuModel(FData),
		aIndex
	) <> 0;
end;

//
// Change the visibility of the specified |command_id|. Returns true (1) on
// success.
function TCefMenuModelRef.SetVisible(aCommandId: cint; aVisible: Boolean): Boolean;
begin
	Result := PCefMenuModel(FData).set_visible(
		PCefMenuModel(FData),
		aCommandId,
		ord(aVisible)
	) <> 0;
end;

//
// Change the visibility at the specified |index|. Returns true (1) on
// success.
function TCefMenuModelRef.SetVisibleAt(aIndex: cint; aVisible: Boolean): Boolean;
begin
	Result := PCefMenuModel(FData).set_visible_at(
		PCefMenuModel(FData),
		aIndex,
		ord(aVisible)
	) <> 0;
end;

//
// Returns true (1) if the specified |command_id| is enabled.
function TCefMenuModelRef.IsEnabled(aCommandId: cint): Boolean;
begin
	Result := PCefMenuModel(FData).is_enabled(
		PCefMenuModel(FData),
		aCommandId
	) <> 0;
end;

//
// Returns true (1) if the specified |index| is enabled.
function TCefMenuModelRef.IsEnabledAt(aIndex: cint): Boolean;
begin
	Result := PCefMenuModel(FData).is_enabled_at(
		PCefMenuModel(FData),
		aIndex
	) <> 0;
end;

//
// Change the enabled status of the specified |command_id|. Returns true (1)
// on success.
function TCefMenuModelRef.SetEnabled(aCommandId: cint; aEnabled: Boolean): Boolean;
begin
	Result := PCefMenuModel(FData).set_enabled(
		PCefMenuModel(FData),
		aCommandId,
		ord(aEnabled)
	) <> 0;
end;

//
// Change the enabled status at the specified |index|. Returns true (1) on
// success.
function TCefMenuModelRef.SetEnabledAt(aIndex: cint; aEnabled: Boolean): Boolean;
begin
	Result := PCefMenuModel(FData).set_enabled_at(
		PCefMenuModel(FData),
		aIndex,
		ord(aEnabled)
	) <> 0;
end;

//
// Returns true (1) if the specified |command_id| is checked. Only applies to
// check and radio items.
function TCefMenuModelRef.IsChecked(aCommandId: cint): Boolean;
begin
	Result := PCefMenuModel(FData).is_checked(
		PCefMenuModel(FData),
		aCommandId
	) <> 0;
end;

//
// Returns true (1) if the specified |index| is checked. Only applies to check
// and radio items.
function TCefMenuModelRef.IsCheckedAt(aIndex: cint): Boolean;
begin
	Result := PCefMenuModel(FData).is_checked_at(
		PCefMenuModel(FData),
		aIndex
	) <> 0;
end;

//
// Check the specified |command_id|. Only applies to check and radio items.
// Returns true (1) on success.
function TCefMenuModelRef.SetChecked(aCommandId: cint; aChecked: Boolean): Boolean;
begin
	Result := PCefMenuModel(FData).set_checked(
		PCefMenuModel(FData),
		aCommandId,
		ord(aChecked)
	) <> 0;
end;

//
// Check the specified |index|. Only applies to check and radio items. Returns
// true (1) on success.
function TCefMenuModelRef.SetCheckedAt(aIndex: cint; aChecked: Boolean): Boolean;
begin
	Result := PCefMenuModel(FData).set_checked_at(
		PCefMenuModel(FData),
		aIndex,
		ord(aChecked)
	) <> 0;
end;

//
// Returns true (1) if the specified |command_id| has a keyboard accelerator
// assigned.
function TCefMenuModelRef.HasAccelerator(aCommandId: cint): Boolean;
begin
	Result := PCefMenuModel(FData).has_accelerator(
		PCefMenuModel(FData),
		aCommandId
	) <> 0;
end;

//
// Returns true (1) if the specified |index| has a keyboard accelerator
// assigned.
function TCefMenuModelRef.HasAcceleratorAt(aIndex: cint): Boolean;
begin
	Result := PCefMenuModel(FData).has_accelerator_at(
		PCefMenuModel(FData),
		aIndex
	) <> 0;
end;

//
// Set the keyboard accelerator for the specified |command_id|. |key_code| can
// be any virtual key or character value. Returns true (1) on success.
function TCefMenuModelRef.SetAccelerator(aCommandId: cint; aKeyCode: cint; aShiftPressed: Boolean; aCtrlPressed: Boolean; aAltPressed: Boolean): Boolean;
begin
	Result := PCefMenuModel(FData).set_accelerator(
		PCefMenuModel(FData),
		aCommandId,
		aKeyCode,
		ord(aShiftPressed),
		ord(aCtrlPressed),
		ord(aAltPressed)
	) <> 0;
end;

//
// Set the keyboard accelerator at the specified |index|. |key_code| can be
// any virtual key or character value. Returns true (1) on success.
function TCefMenuModelRef.SetAcceleratorAt(aIndex: cint; aKeyCode: cint; aShiftPressed: Boolean; aCtrlPressed: Boolean; aAltPressed: Boolean): Boolean;
begin
	Result := PCefMenuModel(FData).set_accelerator_at(
		PCefMenuModel(FData),
		aIndex,
		aKeyCode,
		Ord(aShiftPressed),
		Ord(aCtrlPressed),
		Ord(aAltPressed)
	) <> 0;
end;

//
// Remove the keyboard accelerator for the specified |command_id|. Returns
// true (1) on success.
function TCefMenuModelRef.RemoveAccelerator(aCommandId: cint): Boolean;
begin
	Result := PCefMenuModel(FData).remove_accelerator(
		PCefMenuModel(FData),
		aCommandId
	) <> 0;
end;

//
// Remove the keyboard accelerator at the specified |index|. Returns true (1)
// on success.
function TCefMenuModelRef.RemoveAcceleratorAt(aIndex: cint): Boolean;
begin
	Result := PCefMenuModel(FData).remove_accelerator_at(
		PCefMenuModel(FData),
		aIndex
	) <> 0;
end;

//
// Retrieves the keyboard accelerator for the specified |command_id|. Returns
// true (1) on success.
function TCefMenuModelRef.GetAccelerator(aCommandId: cint; out aKeyCode: cint; out aShiftPressed: Boolean; out aCtrlPressed: Boolean; out aAltPressed: Boolean): Boolean;
var
	key_code_proxy: cint;
	shift_pressed_proxy: cint;
	ctrl_pressed_proxy: cint;
	alt_pressed_proxy: cint;
begin
	key_code_proxy := aKeyCode;
	shift_pressed_proxy := Ord(aShiftPressed);
	ctrl_pressed_proxy := Ord(aCtrlPressed);
	alt_pressed_proxy := Ord(aAltPressed);
	Result := PCefMenuModel(FData).get_accelerator(
		PCefMenuModel(FData),
		aCommandId,
		@key_code_proxy,
		@shift_pressed_proxy,
		@ctrl_pressed_proxy,
		@alt_pressed_proxy
	) <> 0;
	aKeyCode := key_code_proxy;
	aShiftPressed := shift_pressed_proxy <> 0;
	aCtrlPressed := ctrl_pressed_proxy <> 0;
	aAltPressed := alt_pressed_proxy <> 0;
end;

//
// Retrieves the keyboard accelerator for the specified |index|. Returns true
// (1) on success.
function TCefMenuModelRef.GetAcceleratorAt(aIndex: cint; out aKeyCode: cint; out aShiftPressed: Boolean; out aCtrlPressed: Boolean; out aAltPressed: Boolean): Boolean;
var
	key_code_proxy: cint;
	shift_pressed_proxy: cint;
	ctrl_pressed_proxy: cint;
	alt_pressed_proxy: cint;
begin
	key_code_proxy := aKeyCode;
	shift_pressed_proxy := Ord(aShiftPressed);
	ctrl_pressed_proxy := Ord(aCtrlPressed);
	alt_pressed_proxy := Ord(aAltPressed);
	Result := PCefMenuModel(FData).get_accelerator_at(
		PCefMenuModel(FData),
		aIndex,
		@key_code_proxy,
		@shift_pressed_proxy,
		@ctrl_pressed_proxy,
		@alt_pressed_proxy
	) <> 0;
	aKeyCode := key_code_proxy;
	aShiftPressed := shift_pressed_proxy <> 0;
	aCtrlPressed := ctrl_pressed_proxy <> 0;
	aAltPressed := alt_pressed_proxy <> 0;
end;

{Public section}
class function TCefMenuModelRef.UnWrap(data: Pointer): ICefMenuModel;
begin
	if data <> nil then
		Result := Create(data) as ICefMenuModel
	else
		Result := nil;
end;

//..............................................................................TCefNavigationEntryRef
{Protected section}
// Returns true (1) if this object is valid. Do not call any other functions
// if this function returns false (0).
function TCefNavigationEntryRef.IsValid: Boolean;
begin
	Result := PCefNavigationEntry(FData).is_valid(
		PCefNavigationEntry(FData)
	) <> 0;
end;

// Returns the actual URL of the page. For some pages this may be data: URL or
// similar. Use get_display_url() to return a display-friendly version.
// The resulting string must be freed by calling cef_string_userfree_free().
function TCefNavigationEntryRef.GetUrl: ustring;
begin
	Result := TWACef.StringFreeAndGet(PCefNavigationEntry(FData).get_url(
		PCefNavigationEntry(FData)
	));
end;

// Returns a display-friendly version of the URL.
// The resulting string must be freed by calling cef_string_userfree_free().
function TCefNavigationEntryRef.GetDisplayUrl: ustring;
begin
	Result := TWACef.StringFreeAndGet(PCefNavigationEntry(FData).get_display_url(
		PCefNavigationEntry(FData)
	));
end;

// Returns the original URL that was entered by the user before any redirects.
// The resulting string must be freed by calling cef_string_userfree_free().
function TCefNavigationEntryRef.GetOriginalUrl: ustring;
begin
	Result := TWACef.StringFreeAndGet(PCefNavigationEntry(FData).get_original_url(
		PCefNavigationEntry(FData)
	));
end;

// Returns the title set by the page. This value may be NULL.
// The resulting string must be freed by calling cef_string_userfree_free().
function TCefNavigationEntryRef.GetTitle: ustring;
begin
	Result := TWACef.StringFreeAndGet(PCefNavigationEntry(FData).get_title(
		PCefNavigationEntry(FData)
	));
end;

// Returns the transition type which indicates what the user did to move to
// this page from the previous page.
function TCefNavigationEntryRef.GetTransitionType: TCefTransitionType;
begin
	Result := PCefNavigationEntry(FData).get_transition_type(
		PCefNavigationEntry(FData)
	);
end;

// Returns true (1) if this navigation includes post data.
function TCefNavigationEntryRef.HasPostData: Boolean;
begin
	Result := PCefNavigationEntry(FData).has_post_data(
		PCefNavigationEntry(FData)
	) <> 0;
end;

// Returns the name of the sub-frame that navigated or an NULL value if the
// main frame navigated.
// The resulting string must be freed by calling cef_string_userfree_free().
function TCefNavigationEntryRef.GetFrameName: ustring;
begin
	Result := TWACef.StringFreeAndGet(PCefNavigationEntry(FData).get_frame_name(
		PCefNavigationEntry(FData)
	));
end;

// Returns the time for the last known successful navigation completion. A
// navigation may be completed more than once if the page is reloaded. May be
// 0 if the navigation has not yet completed.
function TCefNavigationEntryRef.GetCompletionTime: TCefTime;
begin
	Result := PCefNavigationEntry(FData).get_completion_time(
		PCefNavigationEntry(FData)
	);
end;

// Returns the HTTP status code for the last known successful navigation
// response. May be 0 if the response has not yet been received or if the
// navigation has not yet completed.
function TCefNavigationEntryRef.GetHttpStatusCode: cint;
begin
	Result := PCefNavigationEntry(FData).get_http_status_code(
		PCefNavigationEntry(FData)
	);
end;

{Public section}
class function TCefNavigationEntryRef.UnWrap(data: Pointer): ICefNavigationEntry;
begin
	if data <> nil then
		Result := Create(data) as ICefNavigationEntry
	else
		Result := nil;
end;




//..............................................................................TCefPrintDialogCallbackRef
{Protected section}
// Continue printing with the specified |settings|.
procedure TCefPrintDialogCallbackRef.Cont(const aSettings: ICefPrintSettings);
begin
	PCefPrintDialogCallback(FData).cont(
		PCefPrintDialogCallback(FData),
		TWACef.GetData(aSettings)
	);
end;

// Cancel the printing.
procedure TCefPrintDialogCallbackRef.Cancel;
begin
	PCefPrintDialogCallback(FData).cancel(
		PCefPrintDialogCallback(FData)
	);
end;

{Public section}
class function TCefPrintDialogCallbackRef.UnWrap(data: Pointer): ICefPrintDialogCallback;
begin
	if data <> nil then
		Result := Create(data) as ICefPrintDialogCallback
	else
		Result := nil;
end;
//..............................................................................TCefPrintJobCallbackRef
{Protected section}
// Indicate completion of the print job.
procedure TCefPrintJobCallbackRef.Cont;
begin
	PCefPrintJobCallback(FData).cont(
		PCefPrintJobCallback(FData)
	);
end;

{Public section}
class function TCefPrintJobCallbackRef.UnWrap(data: Pointer): ICefPrintJobCallback;
begin
	if data <> nil then
		Result := Create(data) as ICefPrintJobCallback
	else
		Result := nil;
end;
//..............................................................................TCefPrintHandlerRef
{Protected section}
// Synchronize |settings| with client state. If |get_defaults| is true (1)
// then populate |settings| with the default print settings. Do not keep a
// reference to |settings| outside of this callback.
procedure TCefPrintHandlerRef.OnPrintSettings(const aSettings: ICefPrintSettings; aGetDefaults: Boolean);
begin
	PCefPrintHandler(FData).on_print_settings(
		PCefPrintHandler(FData),
		TWACef.GetData(aSettings),
		Ord(aGetDefaults)
	);
end;

// Show the print dialog. Execute |callback| once the dialog is dismissed.
// Return true (1) if the dialog will be displayed or false (0) to cancel the
// printing immediately.
function TCefPrintHandlerRef.OnPrintDialog(aHasSelection: Boolean; const aCallback: ICefPrintDialogCallback): Boolean;
begin
	Result := PCefPrintHandler(FData).on_print_dialog(
		PCefPrintHandler(FData),
		Ord(aHasSelection),
		TWACef.GetData(aCallback)
	) <> 0;
end;

// Send the print job to the printer. Execute |callback| once the job is
// completed. Return true (1) if the job will proceed or false (0) to cancel
// the job immediately.
function TCefPrintHandlerRef.OnPrintJob(const aDocumentName: ustring; const aPdfFilePath: ustring; const aCallback: ICefPrintJobCallback): Boolean;
var
	document_name_str: TCefString;
	pdf_file_path_str: TCefString;
begin
	document_name_str := TWACef.ToCefString(aDocumentName);
	pdf_file_path_str := TWACef.ToCefString(aPdfFilePath);
	Result := PCefPrintHandler(FData).on_print_job(
		PCefPrintHandler(FData),
		@document_name_str,
		@pdf_file_path_str,
		TWACef.GetData(aCallback)
	) <> 0;
end;

// Reset client state related to printing.
procedure TCefPrintHandlerRef.OnPrintReset;
begin
	PCefPrintHandler(FData).on_print_reset(
		PCefPrintHandler(FData)
	);
end;

{Public section}
class function TCefPrintHandlerRef.UnWrap(data: Pointer): ICefPrintHandler;
begin
	if data <> nil then
		Result := Create(data) as ICefPrintHandler
	else
		Result := nil;
end;

//..............................................................................TCefPrintSettingsRef
// Returns true (1) if this object is valid. Do not call any other functions
// if this function returns false (0).
function TCefPrintSettingsRef.IsValid: Boolean;
begin
	Result := PCefPrintSettings(FData).is_valid(
		PCefPrintSettings(FData)
	) <> 0;
end;

// Returns true (1) if the values of this object are read-only. Some APIs may
// expose read-only objects.
function TCefPrintSettingsRef.IsReadOnly: Boolean;
begin
	Result := PCefPrintSettings(FData).is_read_only(
		PCefPrintSettings(FData)
	) <> 0;
end;

// Returns a writable copy of this object.
function TCefPrintSettingsRef.Copy: ICefPrintSettings;
begin
	Result := TCefPrintSettingsRef.UnWrap(PCefPrintSettings(FData).copy(
		PCefPrintSettings(FData)
	));
end;

// Set the page orientation.
procedure TCefPrintSettingsRef.SetOrientation(aLandscape: Boolean);
begin
	PCefPrintSettings(FData).set_orientation(
		PCefPrintSettings(FData),
		Ord(aLandscape)
	);
end;

// Returns true (1) if the orientation is landscape.
function TCefPrintSettingsRef.IsLandscape: Boolean;
begin
	Result := PCefPrintSettings(FData).is_landscape(
		PCefPrintSettings(FData)
	) <> 0;
end;

// Set the printer printable area in device units. Some platforms already
// provide flipped area. Set |landscape_needs_flip| to false (0) on those
// platforms to avoid double flipping.
procedure TCefPrintSettingsRef.SetPrinterPrintableArea(const aPhysicalSizeDeviceUnits: TCefSize; var aPrintableAreaDeviceUnits: TCefRect; var aLandscapeNeedsFlip: Boolean);
var
	printable_area_device_units_proxy: TCefRect;
	landscape_needs_flip_proxy: cint;
begin
	printable_area_device_units_proxy := aPrintableAreaDeviceUnits;
	landscape_needs_flip_proxy := Ord(aLandscapeNeedsFlip);
	PCefPrintSettings(FData).set_printer_printable_area(
		PCefPrintSettings(FData),
		@aPhysicalSizeDeviceUnits,
		@printable_area_device_units_proxy,
		landscape_needs_flip_proxy
	);
	aPrintableAreaDeviceUnits := printable_area_device_units_proxy;
	aLandscapeNeedsFlip := landscape_needs_flip_proxy <> 0;
end;

// Set the device name.
procedure TCefPrintSettingsRef.SetDeviceName(const aName: ustring);
var
	name_str: TCefString;
begin
	name_str := TWACef.ToCefString(aName);
	PCefPrintSettings(FData).set_device_name(
		PCefPrintSettings(FData),
		@name_str
	);
end;

// Get the device name.
// The resulting string must be freed by calling cef_string_userfree_free().
function TCefPrintSettingsRef.GetDeviceName: ustring;
begin
	Result := TWACef.StringFreeAndGet(PCefPrintSettings(FData).get_device_name(
		PCefPrintSettings(FData)
	));
end;

// Set the DPI (dots per inch).
procedure TCefPrintSettingsRef.SetDpi(aDpi: cint);
begin
	PCefPrintSettings(FData).set_dpi(
		PCefPrintSettings(FData),
		aDpi
	);
end;

// Get the DPI (dots per inch).
function TCefPrintSettingsRef.GetDpi: cint;
begin
	Result := PCefPrintSettings(FData).get_dpi(
		PCefPrintSettings(FData)
	);
end;

// Set the page ranges.
procedure TCefPrintSettingsRef.SetPageRanges(aRangesCount: csize_t; aRanges: TCefPageRangeArray);
begin
  {$IFDEF WIN32}
  // FIX ME: not compiling for 64 (Felipe)
	PCefPrintSettings(FData).set_page_ranges(
		PCefPrintSettings(FData),
		aRangesCount,
		@aRanges
	);
 {$ENDIF}
end;

// Returns the number of page ranges that currently exist.
function TCefPrintSettingsRef.GetPageRangesCount: csize_t;
begin
	Result := PCefPrintSettings(FData).get_page_ranges_count(
		PCefPrintSettings(FData)
	);
end;

// Retrieve the page ranges.
procedure TCefPrintSettingsRef.GetPageRanges(var aRangesCount: csize_t; const aRanges: TCefPageRangeArray);
var
	rangesCount_proxy: csize_t;
begin
	rangesCount_proxy := aRangesCount;
	PCefPrintSettings(FData).get_page_ranges(
		PCefPrintSettings(FData),
		@rangesCount_proxy,
		@aRanges
	);
	aRangesCount := rangesCount_proxy;
end;

// Set whether only the selection will be printed.
procedure TCefPrintSettingsRef.SetSelectionOnly(aSelectionOnly: Boolean);
begin
	PCefPrintSettings(FData).set_selection_only(
		PCefPrintSettings(FData),
		Ord(aSelectionOnly)
	);
end;

// Returns true (1) if only the selection will be printed.
function TCefPrintSettingsRef.IsSelectionOnly: Boolean;
begin
	Result := PCefPrintSettings(FData).is_selection_only(
		PCefPrintSettings(FData)
	) <> 0;
end;

// Set whether pages will be collated.
procedure TCefPrintSettingsRef.SetCollate(aCollate: Boolean);
begin
	PCefPrintSettings(FData).set_collate(
		PCefPrintSettings(FData),
		Ord(aCollate)
	);
end;

// Returns true (1) if pages will be collated.
function TCefPrintSettingsRef.WillCollate: Boolean;
begin
	Result := PCefPrintSettings(FData).will_collate(
		PCefPrintSettings(FData)
	) <> 0;
end;

// Set the color model.
procedure TCefPrintSettingsRef.SetColorModel(aModel: TCefColorModel);
begin
	PCefPrintSettings(FData).set_color_model(
		PCefPrintSettings(FData),
		aModel
	);
end;

// Get the color model.
function TCefPrintSettingsRef.GetColorModel: TCefColorModel;
begin
	Result := PCefPrintSettings(FData).get_color_model(
		PCefPrintSettings(FData)
	);
end;

// Set the number of copies.
procedure TCefPrintSettingsRef.SetCopies(aCopies: cint);
begin
	PCefPrintSettings(FData).set_copies(
		PCefPrintSettings(FData),
		aCopies
	);
end;

// Get the number of copies.
function TCefPrintSettingsRef.GetCopies: cint;
begin
	Result := PCefPrintSettings(FData).get_copies(
		PCefPrintSettings(FData)
	);
end;

// Set the duplex mode.
procedure TCefPrintSettingsRef.SetDuplexMode(aMode: TCefDuplexMode);
begin
	PCefPrintSettings(FData).set_duplex_mode(
		PCefPrintSettings(FData),
		aMode
	);
end;

// Get the duplex mode.
function TCefPrintSettingsRef.GetDuplexMode: TCefDuplexMode;
begin
	Result := PCefPrintSettings(FData).get_duplex_mode(
		PCefPrintSettings(FData)
	);
end;

{Public section}
class function TCefPrintSettingsRef.UnWrap(data: Pointer): ICefPrintSettings;
begin
	if data <> nil then
		Result := Create(data) as ICefPrintSettings
	else
		Result := nil;
end;
// Create a new cef_print_settings_t object.
class function TCefPrintSettingsRef.New: ICefPrintSettings;
begin
	Result := TCefPrintSettingsRef.UnWrap(cef_print_settings_create(
	));
end;

//..............................................................................TCefProcessMessageRef
{Protected section}
// Returns true (1) if this object is valid. Do not call any other functions
// if this function returns false (0).
function TCefProcessMessageRef.IsValid: Boolean;
begin
	Result := PCefProcessMessage(FData).is_valid(
		PCefProcessMessage(FData)
	) <> 0;
end;

// Returns true (1) if the values of this object are read-only. Some APIs may
// expose read-only objects.
function TCefProcessMessageRef.IsReadOnly: Boolean;
begin
	Result := PCefProcessMessage(FData).is_read_only(
		PCefProcessMessage(FData)
	) <> 0;
end;

// Returns a writable copy of this object.
function TCefProcessMessageRef.Copy: ICefProcessMessage;
begin
	Result := TCefProcessMessageRef.UnWrap(PCefProcessMessage(FData).copy(
		PCefProcessMessage(FData)
	));
end;

// Returns the message name.
// The resulting string must be freed by calling cef_string_userfree_free().
function TCefProcessMessageRef.GetName: ustring;
begin
	Result := TWACef.StringFreeAndGet(PCefProcessMessage(FData).get_name(
		PCefProcessMessage(FData)
	));
end;

// Returns the list of arguments.
function TCefProcessMessageRef.GetArgumentList: ICefListValue;
begin
	Result := TCefListValueRef.UnWrap(PCefProcessMessage(FData).get_argument_list(
		PCefProcessMessage(FData)
	));
end;

{Public section}
class function TCefProcessMessageRef.UnWrap(data: Pointer): ICefProcessMessage;
begin
	if data <> nil then
		Result := Create(data) as ICefProcessMessage
	else
		Result := nil;
end;
// Create a new cef_process_message_t object with the specified name.
class function TCefProcessMessageRef.New(const aName: ustring): ICefProcessMessage;
var
	name_str: TCefString;
begin
	name_str := TWACef.ToCefString(aName);
	Result := TCefProcessMessageRef.UnWrap(cef_process_message_create(
		@name_str
	));
end;

//..............................................................................TCefRenderHandlerRef
// Called to retrieve the root window rectangle in screen coordinates. Return
// true (1) if the rectangle was provided.
function TCefRenderHandlerRef.GetRootScreenRect(const aBrowser: ICefBrowser; var aRect: TCefRect): Boolean;
begin
	Result := PCefRenderHandler(FData).get_root_screen_rect(
		PCefRenderHandler(FData),
		TWACef.GetData(aBrowser),
		@aRect
	) <> 0;
end;

// Called to retrieve the view rectangle which is relative to screen
// coordinates. Return true (1) if the rectangle was provided.
function TCefRenderHandlerRef.GetViewRect(const aBrowser: ICefBrowser; var aRect: TCefRect): Boolean;
begin
	Result := PCefRenderHandler(FData).get_view_rect(
		PCefRenderHandler(FData),
		TWACef.GetData(aBrowser),
		@aRect
	) <> 0;
end;

// Called to retrieve the translation from view coordinates to actual screen
// coordinates. Return true (1) if the screen coordinates were provided.
function TCefRenderHandlerRef.GetScreenPoint(const aBrowser: ICefBrowser; aViewX: cint; aViewY: cint; var aScreenX: cint; var aScreenY: cint): Boolean;
var
	screenX_proxy: cint;
	screenY_proxy: cint;
begin
	screenX_proxy := aScreenX;
	screenY_proxy := aScreenY;
	Result := PCefRenderHandler(FData).get_screen_point(
		PCefRenderHandler(FData),
		TWACef.GetData(aBrowser),
		aViewX,
		aViewY,
		@screenX_proxy,
		@screenY_proxy
	) <> 0;
	aScreenX := screenX_proxy;
	aScreenY := screenY_proxy;
end;

// Called to allow the client to fill in the CefScreenInfo object with
// appropriate values. Return true (1) if the |screen_info| structure has been
// modified.
//
// If the screen info rectangle is left NULL the rectangle from GetViewRect
// will be used. If the rectangle is still NULL or invalid popups may not be
// drawn correctly.
function TCefRenderHandlerRef.GetScreenInfo(const aBrowser: ICefBrowser; out aScreenInfo: TCefScreenInfo): Boolean;
var
	screen_info_proxy: TCefScreenInfo;
begin
	screen_info_proxy := aScreenInfo;
	Result := PCefRenderHandler(FData).get_screen_info(
		PCefRenderHandler(FData),
		TWACef.GetData(aBrowser),
		@screen_info_proxy
	) <> 0;
	aScreenInfo := screen_info_proxy;
end;

// Called when the browser wants to show or hide the popup widget. The popup
// should be shown if |show| is true (1) and hidden if |show| is false (0).
procedure TCefRenderHandlerRef.OnPopupShow(const aBrowser: ICefBrowser; aShow: Boolean);
begin
	PCefRenderHandler(FData).on_popup_show(
		PCefRenderHandler(FData),
		TWACef.GetData(aBrowser),
		Ord(aShow)
	);
end;

// Called when the browser wants to move or resize the popup widget. |rect|
// contains the new location and size in view coordinates.
procedure TCefRenderHandlerRef.OnPopupSize(const aBrowser: ICefBrowser; var aRect: TCefRect);
begin
	PCefRenderHandler(FData).on_popup_size(
		PCefRenderHandler(FData),
		TWACef.GetData(aBrowser),
		@aRect
	);
end;

// Called when an element should be painted. Pixel values passed to this
// function are scaled relative to view coordinates based on the value of
// CefScreenInfo.device_scale_factor returned from GetScreenInfo. |type|
// indicates whether the element is the view or the popup widget. |buffer|
// contains the pixel data for the whole image. |dirtyRects| contains the set
// of rectangles in pixel coordinates that need to be repainted. |buffer| will
// be |width|*|height|*4 bytes in size and represents a BGRA image with an
// upper-left origin.
procedure TCefRenderHandlerRef.OnPaint(const aBrowser: ICefBrowser; aType: TCefPaintElementType; aDirtyRectsCount: csize_t; const aDirtyRects: TCefRectArray; const aBuffer: cvoid; aWidth: cint; aHeight: cint);
begin
	PCefRenderHandler(FData).on_paint(
		PCefRenderHandler(FData),
		TWACef.GetData(aBrowser),
		aType,
		aDirtyRectsCount,
		@aDirtyRects,
		aBuffer,
		aWidth,
		aHeight
	);
end;

// Called when the browser's cursor has changed. If |type| is CT_CUSTOM then
// |custom_cursor_info| will be populated with the custom cursor information.
procedure TCefRenderHandlerRef.OnCursorChange(const aBrowser: ICefBrowser; aCursor: TCefCursorHandle; aType: TCefCursorType; aCustomCursorInfo: TCefCursorInfo);
var
	custom_cursor_info_proxy: TCefCursorInfo;
begin
	custom_cursor_info_proxy := aCustomCursorInfo;
	PCefRenderHandler(FData).on_cursor_change(
		PCefRenderHandler(FData),
		TWACef.GetData(aBrowser),
		aCursor,
		aType,
		@custom_cursor_info_proxy
	);
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
function TCefRenderHandlerRef.StartDragging(const aBrowser: ICefBrowser; const aDragData: ICefDragData; aAllowedOps: TCefDragOperationsMask; aX: cint; aY: cint): Boolean;
begin
	Result := PCefRenderHandler(FData).start_dragging(
		PCefRenderHandler(FData),
		TWACef.GetData(aBrowser),
		TWACef.GetData(aDragData),
		aAllowedOps,
		aX,
		aY
	) <> 0;
end;

// Called when the web view wants to update the mouse cursor during a drag &
// drop operation. |operation| describes the allowed operation (none, move,
// copy, link).
procedure TCefRenderHandlerRef.UpdateDragCursor(const aBrowser: ICefBrowser; aOperation: TCefDragOperationsMask);
begin
	PCefRenderHandler(FData).update_drag_cursor(
		PCefRenderHandler(FData),
		TWACef.GetData(aBrowser),
		aOperation
	);
end;

// Called when the scroll offset has changed.
procedure TCefRenderHandlerRef.OnScrollOffsetChanged(const aBrowser: ICefBrowser; aX: cdouble; aY: cdouble);
begin
	PCefRenderHandler(FData).on_scroll_offset_changed(
		PCefRenderHandler(FData),
		TWACef.GetData(aBrowser),
		aX,
		aY
	);
end;

{Public section}
class function TCefRenderHandlerRef.UnWrap(data: Pointer): ICefRenderHandler;
begin
	if data <> nil then
		Result := Create(data) as ICefRenderHandler
	else
		Result := nil;
end;

//..............................................................................TCefRenderProcessHandlerRef
{Protected section}
// Called after the render process main thread has been created. |extra_info|
// is a read-only value originating from
// cef_browser_process_handler_t::on_render_process_thread_created(). Do not
// keep a reference to |extra_info| outside of this function.
procedure TCefRenderProcessHandlerRef.OnRenderThreadCreated(const aExtraInfo: ICefListValue);
begin
	PCefRenderProcessHandler(FData).on_render_thread_created(
		PCefRenderProcessHandler(FData),
		TWACef.GetData(aExtraInfo)
	);
end;

// Called after WebKit has been initialized.
procedure TCefRenderProcessHandlerRef.OnWebKitInitialized;
begin
	PCefRenderProcessHandler(FData).on_web_kit_initialized(
		PCefRenderProcessHandler(FData)
	);
end;

// Called after a browser has been created. When browsing cross-origin a new
// browser will be created before the old browser with the same identifier is
// destroyed.
procedure TCefRenderProcessHandlerRef.OnBrowserCreated(const aBrowser: ICefBrowser);
begin
	PCefRenderProcessHandler(FData).on_browser_created(
		PCefRenderProcessHandler(FData),
		TWACef.GetData(aBrowser)
	);
end;

// Called before a browser is destroyed.
procedure TCefRenderProcessHandlerRef.OnBrowserDestroyed(const aBrowser: ICefBrowser);
begin
	PCefRenderProcessHandler(FData).on_browser_destroyed(
		PCefRenderProcessHandler(FData),
		TWACef.GetData(aBrowser)
	);
end;

// Return the handler for browser load status events.
function TCefRenderProcessHandlerRef.GetLoadHandler: ICefLoadHandler;
begin
	Result := TCefLoadHandlerRef.UnWrap(PCefRenderProcessHandler(FData).get_load_handler(
		PCefRenderProcessHandler(FData)
	));
end;

// Called before browser navigation. Return true (1) to cancel the navigation
// or false (0) to allow the navigation to proceed. The |request| object
// cannot be modified in this callback.
function TCefRenderProcessHandlerRef.OnBeforeNavigation(const aBrowser: ICefBrowser; const aFrame: ICefFrame; const aRequest: ICefRequest; aNavigationType: TCefNavigationType; aIsRedirect: Boolean): Boolean;
begin
	Result := PCefRenderProcessHandler(FData).on_before_navigation(
		PCefRenderProcessHandler(FData),
		TWACef.GetData(aBrowser),
		TWACef.GetData(aFrame),
		TWACef.GetData(aRequest),
		aNavigationType,
		Ord(aIsRedirect)
	) <> 0;
end;

// Called immediately after the V8 context for a frame has been created. To
// retrieve the JavaScript 'window' object use the
// cef_v8context_t::get_global() function. V8 handles can only be accessed
// from the thread on which they are created. A task runner for posting tasks
// on the associated thread can be retrieved via the
// cef_v8context_t::get_task_runner() function.
procedure TCefRenderProcessHandlerRef.OnContextCreated(const aBrowser: ICefBrowser; const aFrame: ICefFrame; const aContext: ICefV8context);
begin
	PCefRenderProcessHandler(FData).on_context_created(
		PCefRenderProcessHandler(FData),
		TWACef.GetData(aBrowser),
		TWACef.GetData(aFrame),
		TWACef.GetData(aContext)
	);
end;

// Called immediately before the V8 context for a frame is released. No
// references to the context should be kept after this function is called.
procedure TCefRenderProcessHandlerRef.OnContextReleased(const aBrowser: ICefBrowser; const aFrame: ICefFrame; const aContext: ICefV8context);
begin
	PCefRenderProcessHandler(FData).on_context_released(
		PCefRenderProcessHandler(FData),
		TWACef.GetData(aBrowser),
		TWACef.GetData(aFrame),
		TWACef.GetData(aContext)
	);
end;

// Called for global uncaught exceptions in a frame. Execution of this
// callback is disabled by default. To enable set
// CefSettings.uncaught_exception_stack_size > 0.
procedure TCefRenderProcessHandlerRef.OnUncaughtException(const aBrowser: ICefBrowser; const aFrame: ICefFrame; const aContext: ICefV8context; const aException: ICefV8exception; const aStackTrace: ICefV8stackTrace);
begin
	PCefRenderProcessHandler(FData).on_uncaught_exception(
		PCefRenderProcessHandler(FData),
		TWACef.GetData(aBrowser),
		TWACef.GetData(aFrame),
		TWACef.GetData(aContext),
		TWACef.GetData(aException),
		TWACef.GetData(aStackTrace)
	);
end;

// Called when a new node in the the browser gets focus. The |node| value may
// be NULL if no specific node has gained focus. The node object passed to
// this function represents a snapshot of the DOM at the time this function is
// executed. DOM objects are only valid for the scope of this function. Do not
// keep references to or attempt to access any DOM objects outside the scope
// of this function.
procedure TCefRenderProcessHandlerRef.OnFocusedNodeChanged(const aBrowser: ICefBrowser; const aFrame: ICefFrame; const aNode: ICefDomnode);
begin
	PCefRenderProcessHandler(FData).on_focused_node_changed(
		PCefRenderProcessHandler(FData),
		TWACef.GetData(aBrowser),
		TWACef.GetData(aFrame),
		TWACef.GetData(aNode)
	);
end;

// Called when a new message is received from a different process. Return true
// (1) if the message was handled or false (0) otherwise. Do not keep a
// reference to or attempt to access the message outside of this callback.
function TCefRenderProcessHandlerRef.OnProcessMessageReceived(const aBrowser: ICefBrowser; aSourceProcess: TCefProcessId; const aMessage: ICefProcessMessage): Boolean;
begin
	Result := PCefRenderProcessHandler(FData).on_process_message_received(
		PCefRenderProcessHandler(FData),
		TWACef.GetData(aBrowser),
		aSourceProcess,
		TWACef.GetData(aMessage)
	) <> 0;
end;

{Public section}
class function TCefRenderProcessHandlerRef.UnWrap(data: Pointer): ICefRenderProcessHandler;
begin
	if data <> nil then
		Result := Create(data) as ICefRenderProcessHandler
	else
		Result := nil;
end;

//..............................................................................TCefRequestRef
// Returns true (1) if this object is read-only.
function TCefRequestRef.IsReadOnly: Boolean;
begin
	Result := PCefRequest(FData).is_read_only(
		PCefRequest(FData)
	) <> 0;
end;

// Get the fully qualified URL.
// The resulting string must be freed by calling cef_string_userfree_free().
function TCefRequestRef.GetUrl: ustring;
begin
	Result := TWACef.StringFreeAndGet(PCefRequest(FData).get_url(
		PCefRequest(FData)
	));
end;

// Set the fully qualified URL.
procedure TCefRequestRef.SetUrl(const aUrl: ustring);
var
	url_str: TCefString;
begin
	url_str := TWACef.ToCefString(aUrl);
	PCefRequest(FData).set_url(
		PCefRequest(FData),
		@url_str
	);
end;

// Get the request function type. The value will default to POST if post data
// is provided and GET otherwise.
// The resulting string must be freed by calling cef_string_userfree_free().
function TCefRequestRef.GetMethod: ustring;
begin
	Result := TWACef.StringFreeAndGet(PCefRequest(FData).get_method(
		PCefRequest(FData)
	));
end;

// Set the request function type.
procedure TCefRequestRef.SetMethod(const aMethod: ustring);
var
	method_str: TCefString;
begin
	method_str := TWACef.ToCefString(aMethod);
	PCefRequest(FData).set_method(
		PCefRequest(FData),
		@method_str
	);
end;

// Get the post data.
function TCefRequestRef.GetPostData: ICefPostData;
begin
	Result := TCefPostDataRef.UnWrap(PCefRequest(FData).get_post_data(
		PCefRequest(FData)
	));
end;

// Set the post data.
procedure TCefRequestRef.SetPostData(const aPostData: ICefPostData);
begin
	PCefRequest(FData).set_post_data(
		PCefRequest(FData),
		TWACef.GetData(aPostData)
	);
end;

// Get the header values.
procedure TCefRequestRef.GetHeaderMap(aHeaderMap: ICefStringMultimap);
begin
	PCefRequest(FData).get_header_map(
		PCefRequest(FData),
		aHeaderMap.Handle
	);
end;

// Set the header values.
procedure TCefRequestRef.SetHeaderMap(aHeaderMap: ICefStringMultimap);
begin
	PCefRequest(FData).set_header_map(
		PCefRequest(FData),
		aHeaderMap.Handle
	);
end;

// Set all values at one time.
procedure TCefRequestRef._Set(const aUrl: ustring; const aMethod: ustring; const aPostData: ICefPostData; aHeaderMap: ICefStringMultimap);
var
	url_str: TCefString;
	method_str: TCefString;
begin
	url_str := TWACef.ToCefString(aUrl);
	method_str := TWACef.ToCefString(aMethod);
	PCefRequest(FData)._set(
		PCefRequest(FData),
		@url_str,
		@method_str,
		TWACef.GetData(aPostData),
		aHeaderMap.Handle
	);
end;

// Get the flags used in combination with cef_urlrequest_t. See
// cef_urlrequest_flags_t for supported values.
function TCefRequestRef.GetFlags: TCefURLRequestFlags;
begin
	Result := PCefRequest(FData).get_flags(
		PCefRequest(FData)
	);
end;

// Set the flags used in combination with cef_urlrequest_t.  See
// cef_urlrequest_flags_t for supported values.
procedure TCefRequestRef.SetFlags(aFlags: TCefURLrequestFlags);
begin
	PCefRequest(FData).set_flags(
		PCefRequest(FData),
		aFlags
	);
end;

// Set the URL to the first party for cookies used in combination with
// cef_urlrequest_t.
// The resulting string must be freed by calling cef_string_userfree_free().
function TCefRequestRef.GetFirstPartyForCookies: ustring;
begin
	Result := TWACef.StringFreeAndGet(PCefRequest(FData).get_first_party_for_cookies(
		PCefRequest(FData)
	));
end;

// Get the URL to the first party for cookies used in combination with
// cef_urlrequest_t.
procedure TCefRequestRef.SetFirstPartyForCookies(const aUrl: ustring);
var
	url_str: TCefString;
begin
	url_str := TWACef.ToCefString(aUrl);
	PCefRequest(FData).set_first_party_for_cookies(
		PCefRequest(FData),
		@url_str
	);
end;

// Get the resource type for this request. Only available in the browser
// process.
function TCefRequestRef.GetResourceType: TCefResourceType;
begin
	Result := PCefRequest(FData).get_resource_type(
		PCefRequest(FData)
	);
end;

// Get the transition type for this request. Only available in the browser
// process and only applies to requests that represent a main frame or sub-
// frame navigation.
function TCefRequestRef.GetTransitionType: TCefTransitionType;
begin
	Result := PCefRequest(FData).get_transition_type(
		PCefRequest(FData)
	);
end;

// Returns the globally unique identifier for this request or 0 if not
// specified. Can be used by cef_request_tHandler implementations in the
// browser process to track a single request across multiple callbacks.
function TCefRequestRef.GetIdentifier: cuint64;
begin
	Result := PCefRequest(FData).get_identifier(
		PCefRequest(FData)
	);
end;

{Public section}
class function TCefRequestRef.UnWrap(data: Pointer): ICefRequest;
begin
	if data <> nil then
		Result := Create(data) as ICefRequest
	else
		Result := nil;
end;
// Create a new cef_request_t object.
class function TCefRequestRef.New: ICefRequest;
begin
	Result := TCefRequestRef.UnWrap(cef_request_create(
	));
end;

//..............................................................................TCefPostDataRef
// Returns true (1) if this object is read-only.
function TCefPostDataRef.IsReadOnly: Boolean;
begin
	Result := PCefPostData(FData).is_read_only(
		PCefPostData(FData)
	) <> 0;
end;

// Returns the number of existing post data elements.
function TCefPostDataRef.GetElementCount: csize_t;
begin
	Result := PCefPostData(FData).get_element_count(
		PCefPostData(FData)
	);
end;

// Retrieve the post data elements.
function TCefPostDataRef.GetElements(var aElementsCount: csize_t): IInterfaceList;
var
  items: PCefPostDataElementArray;
  i: cint;
begin
  Result := TInterfaceList.Create;
  GetMem(items, SizeOf(PCefPostDataElement) * aElementsCount);
  FillChar(items^, SizeOf(PCefPostDataElement) * aElementsCount, 0);
  try
    PCefPostData(FData)^.get_elements(PCefPostData(FData), @aElementsCount, items);
    for i := 0 to aElementsCount - 1 do
      Result.Add(TCefPostDataElementRef.UnWrap(items[i]));
  finally
    FreeMem(items);
  end;
end;

// Remove the specified post data element.  Returns true (1) if the removal
// succeeds.
function TCefPostDataRef.RemoveElement(const aElement: ICefPostDataElement): Boolean;
begin
	Result := PCefPostData(FData).remove_element(
		PCefPostData(FData),
		TWACef.GetData(aElement)
	) <> 0;
end;

// Add the specified post data element.  Returns true (1) if the add succeeds.
function TCefPostDataRef.AddElement(const aElement: ICefPostDataElement): Boolean;
begin
	Result := PCefPostData(FData).add_element(
		PCefPostData(FData),
		TWACef.GetData(aElement)
	) <> 0;
end;

// Remove all existing post data elements.
procedure TCefPostDataRef.RemoveElements;
begin
	PCefPostData(FData).remove_elements(
		PCefPostData(FData)
	);
end;

{Public section}
class function TCefPostDataRef.UnWrap(data: Pointer): ICefPostData;
begin
	if data <> nil then
		Result := Create(data) as ICefPostData
	else
		Result := nil;
end;
// Create a new cef_post_data_t object.
class function TCefPostDataRef.New: ICefPostData;
begin
	Result := TCefPostDataRef.UnWrap(cef_post_data_create(
	));
end;

// Create a new cef_post_data_element_t object.
class function TCefPostDataRef.ElementCreate: ICefPostDataElement;
begin
	Result := TCefPostDataElementRef.UnWrap(cef_post_data_element_create(
	));
end;

//..............................................................................TCefPostDataElementRef
{Protected section}
// Returns true (1) if this object is read-only.
function TCefPostDataElementRef.IsReadOnly: Boolean;
begin
	Result := PCefPostDataElement(FData).is_read_only(
		PCefPostDataElement(FData)
	) <> 0;
end;

// Remove all contents from the post data element.
procedure TCefPostDataElementRef.SetToEmpty;
begin
	PCefPostDataElement(FData).set_to_empty(
		PCefPostDataElement(FData)
	);
end;

// The post data element will represent a file.
procedure TCefPostDataElementRef.SetToFile(const aFileName: ustring);
var
	fileName_str: TCefString;
begin
	fileName_str := TWACef.ToCefString(aFileName);
	PCefPostDataElement(FData).set_to_file(
		PCefPostDataElement(FData),
		@fileName_str
	);
end;

// The post data element will represent bytes.  The bytes passed in will be
// copied.
procedure TCefPostDataElementRef.SetToBytes(aSize: csize_t; const aBytes: cvoid);
begin
	PCefPostDataElement(FData).set_to_bytes(
		PCefPostDataElement(FData),
		aSize,
		aBytes
	);
end;

// Return the type of this post data element.
function TCefPostDataElementRef.GetType: TCefPostdataelementType;
begin
	Result := PCefPostDataElement(FData).get_type(
		PCefPostDataElement(FData)
	);
end;

// Return the file name.
// The resulting string must be freed by calling cef_string_userfree_free().
function TCefPostDataElementRef.GetFile: ustring;
begin
	Result := TWACef.StringFreeAndGet(PCefPostDataElement(FData).get_file(
		PCefPostDataElement(FData)
	));
end;

// Return the number of bytes.
function TCefPostDataElementRef.GetBytesCount: csize_t;
begin
	Result := PCefPostDataElement(FData).get_bytes_count(
		PCefPostDataElement(FData)
	);
end;

// Read up to |size| bytes into |bytes| and return the number of bytes
// actually read.
function TCefPostDataElementRef.GetBytes(aSize: csize_t; aBytes: cvoid): csize_t;
begin
	Result := PCefPostDataElement(FData).get_bytes(
		PCefPostDataElement(FData),
		aSize,
		aBytes
	);
end;

{Public section}
class function TCefPostDataElementRef.UnWrap(data: Pointer): ICefPostDataElement;
begin
	if data <> nil then
		Result := Create(data) as ICefPostDataElement
	else
		Result := nil;
end;
// Create a new cef_post_data_element_t object.
class function TCefPostDataElementRef.New: ICefPostDataElement;
begin
	Result := TCefPostDataElementRef.UnWrap(cef_post_data_element_create(
	));
end;


//..............................................................................TCefRequestContextRef
{Protected section}
// Returns true (1) if this object is pointing to the same context as |that|
// object.
function TCefRequestContextRef.IsSame(const aOther: ICefRequestContext): Boolean;
begin
	Result := PCefRequestContext(FData).is_same(
		PCefRequestContext(FData),
		TWACef.GetData(aOther)
	) <> 0;
end;

// Returns true (1) if this object is sharing the same storage as |that|
// object.
function TCefRequestContextRef.IsSharingWith(const aOther: ICefRequestContext): Boolean;
begin
	Result := PCefRequestContext(FData).is_sharing_with(
		PCefRequestContext(FData),
		TWACef.GetData(aOther)
	) <> 0;
end;

// Returns true (1) if this object is the global context. The global context
// is used by default when creating a browser or URL request with a NULL
// context argument.
function TCefRequestContextRef.IsGlobal: Boolean;
begin
	Result := PCefRequestContext(FData).is_global(
		PCefRequestContext(FData)
	) <> 0;
end;

// Returns the handler for this context if any.
function TCefRequestContextRef.GetHandler: ICefRequestContextHandler;
begin
	Result := TCefRequestContextHandlerRef.UnWrap(PCefRequestContext(FData).get_handler(
		PCefRequestContext(FData)
	));
end;

// Returns the cache path for this object. If NULL an "incognito mode" in-
// memory cache is being used.
// The resulting string must be freed by calling cef_string_userfree_free().
function TCefRequestContextRef.GetCachePath: ustring;
begin
	Result := TWACef.StringFreeAndGet(PCefRequestContext(FData).get_cache_path(
		PCefRequestContext(FData)
	));
end;

// Returns the default cookie manager for this object. This will be the global
// cookie manager if this object is the global request context. Otherwise,
// this will be the default cookie manager used when this request context does
// not receive a value via cef_request_tContextHandler::get_cookie_manager().
// If |callback| is non-NULL it will be executed asnychronously on the IO
// thread after the manager's storage has been initialized.
function TCefRequestContextRef.GetDefaultCookieManager(const aCallback: ICefCompletionCallback): ICefCookieManager;
begin
	Result := TCefCookieManagerRef.UnWrap(PCefRequestContext(FData).get_default_cookie_manager(
		PCefRequestContext(FData),
		TWACef.GetData(aCallback)
	));
end;

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
function TCefRequestContextRef.RegisterSchemeHandlerFactory(const aSchemeName: ustring; const aDomainName: ustring; const aFactory: ICefSchemeHandlerFactory): Boolean;
var
	scheme_name_str: TCefString;
	domain_name_str: TCefString;
begin
	scheme_name_str := TWACef.ToCefString(aSchemeName);
	domain_name_str := TWACef.ToCefString(aDomainName);
	Result := PCefRequestContext(FData).register_scheme_handler_factory(
		PCefRequestContext(FData),
		@scheme_name_str,
		@domain_name_str,
		TWACef.GetData(aFactory)
	) <> 0;
end;

// Clear all registered scheme handler factories. Returns false (0) on error.
// This function may be called on any thread in the browser process.
function TCefRequestContextRef.ClearSchemeHandlerFactories: Boolean;
begin
	Result := PCefRequestContext(FData).clear_scheme_handler_factories(
		PCefRequestContext(FData)
	) <> 0;
end;

{Public section}
class function TCefRequestContextRef.UnWrap(data: Pointer): ICefRequestContext;
begin
	if data <> nil then
		Result := Create(data) as ICefRequestContext
	else
		Result := nil;
end;
// Returns the global context object.
class function TCefRequestContextRef.GetGlobalContext: ICefRequestContext;
begin
	Result := TCefRequestContextRef.UnWrap(cef_request_context_get_global_context(
	));
end;

// Returns the global context object.
// Creates a new context object with the specified |settings| and optional
// |handler|.
class function TCefRequestContextRef.CreateContext(const aSettings: TCefRequestContextSettings; const aHandler: ICefRequestContextHandler): ICefRequestContext;
begin
	Result := TCefRequestContextRef.UnWrap(cef_request_context_create_context(
		@aSettings,
		TWACef.GetData(aHandler)
	));
end;


//..............................................................................TCefRequestContextHandlerRef
{Protected section}
// Called on the IO thread to retrieve the cookie manager. If this function
// returns NULL the default cookie manager retrievable via
// cef_request_tContext::get_default_cookie_manager() will be used.
function TCefRequestContextHandlerRef.GetCookieManager: ICefCookieManager;
begin
	Result := TCefCookieManagerRef.UnWrap(PCefRequestContextHandler(FData).get_cookie_manager(
		PCefRequestContextHandler(FData)
	));
end;

{Public section}
class function TCefRequestContextHandlerRef.UnWrap(data: Pointer): ICefRequestContextHandler;
begin
	if data <> nil then
		Result := Create(data) as ICefRequestContextHandler
	else
		Result := nil;
end;

//..............................................................................TCefRequestCallbackRef
{Protected section}
// Continue the url request. If |allow| is true (1) the request will be
// continued. Otherwise, the request will be canceled.
procedure TCefRequestCallbackRef.Cont(aAllow: Boolean);
begin
	PCefRequestCallback(FData).cont(
		PCefRequestCallback(FData),
		Ord(aAllow)
	);
end;

// Cancel the url request.
procedure TCefRequestCallbackRef.Cancel;
begin
	PCefRequestCallback(FData).cancel(
		PCefRequestCallback(FData)
	);
end;

{Public section}
class function TCefRequestCallbackRef.UnWrap(data: Pointer): ICefRequestCallback;
begin
	if data <> nil then
		Result := Create(data) as ICefRequestCallback
	else
		Result := nil;
end;

//..............................................................................TCefRequestHandlerRef
// Called on the UI thread before browser navigation. Return true (1) to
// cancel the navigation or false (0) to allow the navigation to proceed. The
// |request| object cannot be modified in this callback.
// cef_load_handler_t::OnLoadingStateChange will be called twice in all cases.
// If the navigation is allowed cef_load_handler_t::OnLoadStart and
// cef_load_handler_t::OnLoadEnd will be called. If the navigation is canceled
// cef_load_handler_t::OnLoadError will be called with an |errorCode| value of
// ERR_ABORTED.
function TCefRequestHandlerRef.OnBeforeBrowse(const aBrowser: ICefBrowser; const aFrame: ICefFrame; const aRequest: ICefRequest; aIsRedirect: Boolean): Boolean;
begin
	Result := PCefRequestHandler(FData).on_before_browse(
		PCefRequestHandler(FData),
		TWACef.GetData(aBrowser),
		TWACef.GetData(aFrame),
		TWACef.GetData(aRequest),
		Ord(aIsRedirect)
	) <> 0;
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
function TCefRequestHandlerRef.OnOpenUrlfromTab(const aBrowser: ICefBrowser; const aFrame: ICefFrame; const aTargetUrl: ustring; aTargetDisposition: TCefWindowOpenDisposition; aUserGesture: Boolean): Boolean;
var
	target_url_str: TCefString;
begin
	target_url_str := TWACef.ToCefString(aTargetUrl);
	Result := PCefRequestHandler(FData).on_open_urlfrom_tab(
		PCefRequestHandler(FData),
		TWACef.GetData(aBrowser),
		TWACef.GetData(aFrame),
		@target_url_str,
		aTargetDisposition,
		Ord(aUserGesture)
	) <> 0;
end;

// Called on the IO thread before a resource request is loaded. The |request|
// object may be modified. Return RV_CONTINUE to continue the request
// immediately. Return RV_CONTINUE_ASYNC and call cef_request_tCallback::
// cont() at a later time to continue or cancel the request asynchronously.
// Return RV_CANCEL to cancel the request immediately.
//
function TCefRequestHandlerRef.OnBeforeResourceLoad(const aBrowser: ICefBrowser; const aFrame: ICefFrame; const aRequest: ICefRequest; const aCallback: ICefRequestCallback): TCefReturnValue;
begin
	Result := PCefRequestHandler(FData).on_before_resource_load(
		PCefRequestHandler(FData),
		TWACef.GetData(aBrowser),
		TWACef.GetData(aFrame),
		TWACef.GetData(aRequest),
		TWACef.GetData(aCallback)
	);
end;

// Called on the IO thread before a resource is loaded. To allow the resource
// to load normally return NULL. To specify a handler for the resource return
// a cef_resource_handler_t object. The |request| object should not be
// modified in this callback.
function TCefRequestHandlerRef.GetResourceHandler(const aBrowser: ICefBrowser; const aFrame: ICefFrame; const aRequest: ICefRequest): ICefResourceHandler;
begin
	Result := TCefResourceHandlerRef.UnWrap(PCefRequestHandler(FData).get_resource_handler(
		PCefRequestHandler(FData),
		TWACef.GetData(aBrowser),
		TWACef.GetData(aFrame),
		TWACef.GetData(aRequest)
	));
end;

// Called on the IO thread when a resource load is redirected. The |request|
// parameter will contain the old URL and other request-related information.
// The |new_url| parameter will contain the new URL and can be changed if
// desired. The |request| object cannot be modified in this callback.
procedure TCefRequestHandlerRef.OnResourceRedirect(const aBrowser: ICefBrowser; const aFrame: ICefFrame; const aRequest: ICefRequest; var aNewUrl: ustring);
var
	new_url_str: TCefString;
begin
	new_url_str := TWACef.ToCefString(aNewUrl);
	PCefRequestHandler(FData).on_resource_redirect(
		PCefRequestHandler(FData),
		TWACef.GetData(aBrowser),
		TWACef.GetData(aFrame),
		TWACef.GetData(aRequest),
		@new_url_str
	);
  aNewUrl := TWACef.ToString(@new_url_str);
end;

// Called on the IO thread when a resource response is received. To allow the
// resource to load normally return false (0). To redirect or retry the
// resource modify |request| (url, headers or post body) and return true (1).
// The |response| object cannot be modified in this callback.
function TCefRequestHandlerRef.OnResourceResponse(const aBrowser: ICefBrowser; const aFrame: ICefFrame; const aRequest: ICefRequest; const aResponse: ICefResponse): Boolean;
begin
	Result := PCefRequestHandler(FData).on_resource_response(
		PCefRequestHandler(FData),
		TWACef.GetData(aBrowser),
		TWACef.GetData(aFrame),
		TWACef.GetData(aRequest),
		TWACef.GetData(aResponse)
	) <> 0;
end;

// Called on the IO thread when the browser needs credentials from the user.
// |isProxy| indicates whether the host is a proxy server. |host| contains the
// hostname and |port| contains the port number. Return true (1) to continue
// the request and call cef_auth_callback_t::cont() either in this function or
// at a later time when the authentication information is available. Return
// false (0) to cancel the request immediately.
function TCefRequestHandlerRef.GetAuthCredentials(const aBrowser: ICefBrowser; const aFrame: ICefFrame; aIsProxy: Boolean; const aHost: ustring; aPort: cint; const aRealm: ustring; const aScheme: ustring; const aCallback: ICefAuthCallback): Boolean;
var
	host_str: TCefString;
	realm_str: TCefString;
	scheme_str: TCefString;
begin
	host_str := TWACef.ToCefString(aHost);
	realm_str := TWACef.ToCefString(aRealm);
	scheme_str := TWACef.ToCefString(aScheme);
	Result := PCefRequestHandler(FData).get_auth_credentials(
		PCefRequestHandler(FData),
		TWACef.GetData(aBrowser),
		TWACef.GetData(aFrame),
		Ord(aIsProxy),
		@host_str,
		aPort,
		@realm_str,
		@scheme_str,
		TWACef.GetData(aCallback)
	) <> 0;
end;

// Called on the IO thread when JavaScript requests a specific storage quota
// size via the webkitStorageInfo.requestQuota function. |origin_url| is the
// origin of the page making the request. |new_size| is the requested quota
// size in bytes. Return true (1) to continue the request and call
// cef_request_tCallback::cont() either in this function or at a later time to
// grant or deny the request. Return false (0) to cancel the request
// immediately.
function TCefRequestHandlerRef.OnQuotaRequest(const aBrowser: ICefBrowser; const aOriginUrl: ustring; aNewSize: cint64; const aCallback: ICefRequestCallback): Boolean;
var
	origin_url_str: TCefString;
begin
	origin_url_str := TWACef.ToCefString(aOriginUrl);
	Result := PCefRequestHandler(FData).on_quota_request(
		PCefRequestHandler(FData),
		TWACef.GetData(aBrowser),
		@origin_url_str,
		aNewSize,
		TWACef.GetData(aCallback)
	) <> 0;
end;

// Called on the UI thread to handle requests for URLs with an unknown
// protocol component. Set |allow_os_execution| to true (1) to attempt
// execution via the registered OS protocol handler, if any. SECURITY WARNING:
// YOU SHOULD USE THIS METHOD TO ENFORCE RESTRICTIONS BASED ON SCHEME, HOST OR
// OTHER URL ANALYSIS BEFORE ALLOWING OS EXECUTION.
procedure TCefRequestHandlerRef.OnProtocolExecution(const aBrowser: ICefBrowser; const aUrl: ustring; out aAllowOsExecution: Boolean);
var
	url_str: TCefString;
	allow_os_execution_proxy: cint;
begin
	url_str := TWACef.ToCefString(aUrl);
	allow_os_execution_proxy := Ord(aAllowOsExecution);
	PCefRequestHandler(FData).on_protocol_execution(
		PCefRequestHandler(FData),
		TWACef.GetData(aBrowser),
		@url_str,
		@allow_os_execution_proxy
	);
	aAllowOsExecution := allow_os_execution_proxy <> 0;
end;

// Called on the UI thread to handle requests for URLs with an invalid SSL
// certificate. Return true (1) and call cef_request_tCallback::cont() either
// in this function or at a later time to continue or cancel the request.
// Return false (0) to cancel the request immediately. If |callback| is NULL
// the error cannot be recovered from and the request will be canceled
// automatically. If CefSettings.ignore_certificate_errors is set all invalid
// certificates will be accepted without calling this function.
function TCefRequestHandlerRef.OnCertificateError(const aBrowser: ICefBrowser; aCertError: TCefErrorcode; const aRequestUrl: ustring; const aSslInfo: ICefSslinfo; const aCallback: ICefRequestCallback): Boolean;
var
	request_url_str: TCefString;
begin
	request_url_str := TWACef.ToCefString(aRequestUrl);
	Result := PCefRequestHandler(FData).on_certificate_error(
		PCefRequestHandler(FData),
		TWACef.GetData(aBrowser),
		aCertError,
		@request_url_str,
		TWACef.GetData(aSslInfo),
		TWACef.GetData(aCallback)
	) <> 0;
end;

// Called on the browser process IO thread before a plugin is loaded. Return
// true (1) to block loading of the plugin.
function TCefRequestHandlerRef.OnBeforePluginLoad(const aBrowser: ICefBrowser; const aUrl: ustring; const aPolicyUrl: ustring; const aInfo: ICefWebPluginInfo): Boolean;
var
	url_str: TCefString;
	policy_url_str: TCefString;
begin
	url_str := TWACef.ToCefString(aUrl);
	policy_url_str := TWACef.ToCefString(aPolicyUrl);
	Result := PCefRequestHandler(FData).on_before_plugin_load(
		PCefRequestHandler(FData),
		TWACef.GetData(aBrowser),
		@url_str,
		@policy_url_str,
		TWACef.GetData(aInfo)
	) <> 0;
end;

// Called on the browser process UI thread when a plugin has crashed.
// |plugin_path| is the path of the plugin that crashed.
procedure TCefRequestHandlerRef.OnPluginCrashed(const aBrowser: ICefBrowser; const aPluginPath: ustring);
var
	plugin_path_str: TCefString;
begin
	plugin_path_str := TWACef.ToCefString(aPluginPath);
	PCefRequestHandler(FData).on_plugin_crashed(
		PCefRequestHandler(FData),
		TWACef.GetData(aBrowser),
		@plugin_path_str
	);
end;

// Called on the browser process UI thread when the render view associated
// with |browser| is ready to receive/handle IPC messages in the render
// process.
procedure TCefRequestHandlerRef.OnRenderViewReady(const aBrowser: ICefBrowser);
begin
	PCefRequestHandler(FData).on_render_view_ready(
		PCefRequestHandler(FData),
		TWACef.GetData(aBrowser)
	);
end;

// Called on the browser process UI thread when the render process terminates
// unexpectedly. |status| indicates how the process terminated.
procedure TCefRequestHandlerRef.OnRenderProcessTerminated(const aBrowser: ICefBrowser; aStatus: TCefTerminationStatus);
begin
	PCefRequestHandler(FData).on_render_process_terminated(
		PCefRequestHandler(FData),
		TWACef.GetData(aBrowser),
		aStatus
	);
end;

{Public section}
class function TCefRequestHandlerRef.UnWrap(data: Pointer): ICefRequestHandler;
begin
	if data <> nil then
		Result := Create(data) as ICefRequestHandler
	else
		Result := nil;
end;

//..............................................................................TCefResourceBundleHandlerRef
{Protected section}
// Called to retrieve a localized translation for the string specified by
// |message_id|. To provide the translation set |string| to the translation
// string and return true (1). To use the default translation return false
// (0). Supported message IDs are listed in cef_pack_strings.h.
function TCefResourceBundleHandlerRef.GetLocalizedString(aMessageId: cint; out aString: ustring): Boolean;
var
	string_str: TCefString;
begin
	string_str := TWACef.ToCefString(aString);
	Result := PCefResourceBundleHandler(FData).get_localized_string(
		PCefResourceBundleHandler(FData),
		aMessageId,
		@string_str
	) <> 0;
end;

// Called to retrieve data for the resource specified by |resource_id|. To
// provide the resource data set |data| and |data_size| to the data pointer
// and size respectively and return true (1). To use the default resource data
// return false (0). The resource data will not be copied and must remain
// resident in memory. Supported resource IDs are listed in
// cef_pack_resources.h.
function TCefResourceBundleHandlerRef.GetDataResource(aResourceId: cint; out aData: cvoid; out aDataSize: csize_t): Boolean;
var
	data_size_proxy: csize_t;
begin
	data_size_proxy := aDataSize;
	Result := PCefResourceBundleHandler(FData).get_data_resource(
		PCefResourceBundleHandler(FData),
		aResourceId,
		aData,
		@data_size_proxy
	) <> 0;
	aDataSize := data_size_proxy;
end;

{Public section}
class function TCefResourceBundleHandlerRef.UnWrap(data: Pointer): ICefResourceBundleHandler;
begin
	if data <> nil then
		Result := Create(data) as ICefResourceBundleHandler
	else
		Result := nil;
end;

//..............................................................................TCefResourceHandlerRef
// Begin processing the request. To handle the request return true (1) and
// call cef_callback_t::cont() once the response header information is
// available (cef_callback_t::cont() can also be called from inside this
// function if header information is available immediately). To cancel the
// request return false (0).
function TCefResourceHandlerRef.ProcessRequest(const aRequest: ICefRequest; const aCallback: ICefCallback): Boolean;
begin
	Result := PCefResourceHandler(FData).process_request(
		PCefResourceHandler(FData),
		TWACef.GetData(aRequest),
		TWACef.GetData(aCallback)
	) <> 0;
end;

// Retrieve response header information. If the response length is not known
// set |response_length| to -1 and read_response() will be called until it
// returns false (0). If the response length is known set |response_length| to
// a positive value and read_response() will be called until it returns false
// (0) or the specified number of bytes have been read. Use the |response|
// object to set the mime type, http status code and other optional header
// values. To redirect the request to a new URL set |redirectUrl| to the new
// URL.
procedure TCefResourceHandlerRef.GetResponseHeaders(const aResponse: ICefResponse; var aResponseLength: cint64; var aRedirectUrl: ustring);
var
	response_length_proxy: cint64;
	redirectUrl_str: TCefString;
begin
	response_length_proxy := aResponseLength;
	redirectUrl_str := TWACef.ToCefString(aRedirectUrl);
	PCefResourceHandler(FData).get_response_headers(
		PCefResourceHandler(FData),
		TWACef.GetData(aResponse),
		@response_length_proxy,
		@redirectUrl_str
	);
	aResponseLength := response_length_proxy;
end;

// Read response data. If data is available immediately copy up to
// |bytes_to_read| bytes into |data_out|, set |bytes_read| to the number of
// bytes copied, and return true (1). To read the data at a later time set
// |bytes_read| to 0, return true (1) and call cef_callback_t::cont() when the
// data is available. To indicate response completion return false (0).
function TCefResourceHandlerRef.ReadResponse(var aDataOut: cvoid; aBytesToRead: cint; var aBytesRead: cint; const aCallback: ICefCallback): Boolean;
var
	bytes_read_proxy: cint;
begin
	bytes_read_proxy := aBytesRead;
	Result := PCefResourceHandler(FData).read_response(
		PCefResourceHandler(FData),
		aDataOut,
		aBytesToRead,
		@bytes_read_proxy,
		TWACef.GetData(aCallback)
	) <> 0;
	aBytesRead := bytes_read_proxy;
end;

// Return true (1) if the specified cookie can be sent with the request or
// false (0) otherwise. If false (0) is returned for any cookie then no
// cookies will be sent with the request.
function TCefResourceHandlerRef.CanGetCookie(const aCookie: TWACefCookie): Boolean;
var
	cookie_proxy: TCefCookie;
begin
	cookie_proxy.name := TWACef.ToCefString(aCookie.name);
  cookie_proxy.value := TWACef.ToCefString(aCookie.value);
  cookie_proxy.domain := TWACef.ToCefString(aCookie.domain);
  cookie_proxy.path := TWACef.ToCefString(aCookie.path);
  cookie_proxy.secure := aCookie.secure;
  cookie_proxy.httponly := aCookie.httponly;
  cookie_proxy.creation := aCookie.creation;
  cookie_proxy.last_access := aCookie.last_access;
  cookie_proxy.has_expires := aCookie.has_expires;
  cookie_proxy.expires := aCookie.expires;

	Result := PCefResourceHandler(FData).can_get_cookie(
		PCefResourceHandler(FData),
		@cookie_proxy
	) <> 0;
end;

// Return true (1) if the specified cookie returned with the response can be
// set or false (0) otherwise.
function TCefResourceHandlerRef.CanSetCookie(const aCookie: TWACefCookie): Boolean;
var
	cookie_proxy: TCefCookie;
begin
	cookie_proxy.name := TWACef.ToCefString(aCookie.name);
  cookie_proxy.value := TWACef.ToCefString(aCookie.value);
  cookie_proxy.domain := TWACef.ToCefString(aCookie.domain);
  cookie_proxy.path := TWACef.ToCefString(aCookie.path);
  cookie_proxy.secure := aCookie.secure;
  cookie_proxy.httponly := aCookie.httponly;
  cookie_proxy.creation := aCookie.creation;
  cookie_proxy.last_access := aCookie.last_access;
  cookie_proxy.has_expires := aCookie.has_expires;
  cookie_proxy.expires := aCookie.expires;

	Result := PCefResourceHandler(FData).can_set_cookie(
		PCefResourceHandler(FData),
		@cookie_proxy
	) <> 0;
end;

// Request processing has been canceled.
procedure TCefResourceHandlerRef.Cancel;
begin
	PCefResourceHandler(FData).cancel(
		PCefResourceHandler(FData)
	);
end;

{Public section}
class function TCefResourceHandlerRef.UnWrap(data: Pointer): ICefResourceHandler;
begin
	if data <> nil then
		Result := Create(data) as ICefResourceHandler
	else
		Result := nil;
end;

//..............................................................................TCefResponseRef
// Returns true (1) if this object is read-only.
function TCefResponseRef.IsReadOnly: Boolean;
begin
	Result := PCefResponse(FData).is_read_only(
		PCefResponse(FData)
	) <> 0;
end;

// Get the response status code.
function TCefResponseRef.GetStatus: cint;
begin
	Result := PCefResponse(FData).get_status(
		PCefResponse(FData)
	);
end;

// Set the response status code.
procedure TCefResponseRef.SetStatus(aStatus: cint);
begin
	PCefResponse(FData).set_status(
		PCefResponse(FData),
		aStatus
	);
end;

// Get the response status text.
// The resulting string must be freed by calling cef_string_userfree_free().
function TCefResponseRef.GetStatusText: ustring;
begin
	Result := TWACef.StringFreeAndGet(PCefResponse(FData).get_status_text(
		PCefResponse(FData)
	));
end;

// Set the response status text.
procedure TCefResponseRef.SetStatusText(const aStatusText: ustring);
var
	statusText_str: TCefString;
begin
	statusText_str := TWACef.ToCefString(aStatusText);
	PCefResponse(FData).set_status_text(
		PCefResponse(FData),
		@statusText_str
	);
end;

// Get the response mime type.
// The resulting string must be freed by calling cef_string_userfree_free().
function TCefResponseRef.GetMimeType: ustring;
begin
	Result := TWACef.StringFreeAndGet(PCefResponse(FData).get_mime_type(
		PCefResponse(FData)
	));
end;

// Set the response mime type.
procedure TCefResponseRef.SetMimeType(const aMimeType: ustring);
var
	mimeType_str: TCefString;
begin
	mimeType_str := TWACef.ToCefString(aMimeType);
	PCefResponse(FData).set_mime_type(
		PCefResponse(FData),
		@mimeType_str
	);
end;

// Get the value for the specified response header field.
// The resulting string must be freed by calling cef_string_userfree_free().
function TCefResponseRef.GetHeader(const aName: ustring): ustring;
var
	name_str: TCefString;
begin
	name_str := TWACef.ToCefString(aName);
	Result := TWACef.StringFreeAndGet(PCefResponse(FData).get_header(
		PCefResponse(FData),
		@name_str
	));
end;

// Get all response header fields.
procedure TCefResponseRef.GetHeaderMap(const aHeaderMap: ICefStringMultimap);
begin
	PCefResponse(FData).get_header_map(
		PCefResponse(FData),
		aHeaderMap.Handle
	);
end;

// Set all response header fields.
procedure TCefResponseRef.SetHeaderMap(const aHeaderMap: ICefStringMultimap);
begin
	PCefResponse(FData).set_header_map(
		PCefResponse(FData),
		aHeaderMap.Handle
	);
end;

{Public section}
class function TCefResponseRef.UnWrap(data: Pointer): ICefResponse;
begin
	if data <> nil then
		Result := Create(data) as ICefResponse
	else
		Result := nil;
end;
// Create a new cef_response_t object.
class function TCefResponseRef.New: ICefResponse;
begin
	Result := TCefResponseRef.UnWrap(cef_response_create(
	));
end;


//..............................................................................TCefSchemeRegistrarRef
{Protected section}
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
function TCefSchemeRegistrarRef.AddCustomScheme(const aSchemeName: ustring; aIsStandard: Boolean; aIsLocal: Boolean; aIsDisplayIsolated: Boolean): Boolean;
var
	scheme_name_str: TCefString;
begin
	scheme_name_str := TWACef.ToCefString(aSchemeName);
	Result := PCefSchemeRegistrar(FData).add_custom_scheme(
		PCefSchemeRegistrar(FData),
		@scheme_name_str,
		Ord(aIsStandard),
		Ord(aIsLocal),
		Ord(aIsDisplayIsolated)
	) <> 0;
end;

{Public section}
class function TCefSchemeRegistrarRef.UnWrap(data: Pointer): ICefSchemeRegistrar;
begin
	if data <> nil then
		Result := Create(data) as ICefSchemeRegistrar
	else
		Result := nil;
end;
//..............................................................................TCefSchemeHandlerFactoryRef
{Protected section}
// Return a new resource handler instance to handle the request or an NULL
// reference to allow default handling of the request. |browser| and |frame|
// will be the browser window and frame respectively that originated the
// request or NULL if the request did not originate from a browser window (for
// example, if the request came from cef_urlrequest_t). The |request| object
// passed to this function will not contain cookie data.
function TCefSchemeHandlerFactoryRef.New(const aBrowser: ICefBrowser; const aFrame: ICefFrame; const aSchemeName: ustring; const aRequest: ICefRequest): ICefResourceHandler;
var
	scheme_name_str: TCefString;
begin
	scheme_name_str := TWACef.ToCefString(aSchemeName);
	Result := TCefResourceHandlerRef.UnWrap(PCefSchemeHandlerFactory(FData).create(
		PCefSchemeHandlerFactory(FData),
		TWACef.GetData(aBrowser),
		TWACef.GetData(aFrame),
		@scheme_name_str,
		TWACef.GetData(aRequest)
	));
end;

{Public section}
class function TCefSchemeHandlerFactoryRef.UnWrap(data: Pointer): ICefSchemeHandlerFactory;
begin
	if data <> nil then
		Result := Create(data) as ICefSchemeHandlerFactory
	else
		Result := nil;
end;

//..............................................................................TCefSslcertPrincipalRef
{Protected section}
// Returns a name that can be used to represent the issuer.  It tries in this
// order: CN, O and OU and returns the first non-NULL one found.
// The resulting string must be freed by calling cef_string_userfree_free().
function TCefSslcertPrincipalRef.GetDisplayName: ustring;
begin
	Result := TWACef.StringFreeAndGet(PCefSslcertPrincipal(FData).get_display_name(
		PCefSslcertPrincipal(FData)
	));
end;

// Returns the common name.
// The resulting string must be freed by calling cef_string_userfree_free().
function TCefSslcertPrincipalRef.GetCommonName: ustring;
begin
	Result := TWACef.StringFreeAndGet(PCefSslcertPrincipal(FData).get_common_name(
		PCefSslcertPrincipal(FData)
	));
end;

// Returns the locality name.
// The resulting string must be freed by calling cef_string_userfree_free().
function TCefSslcertPrincipalRef.GetLocalityName: ustring;
begin
	Result := TWACef.StringFreeAndGet(PCefSslcertPrincipal(FData).get_locality_name(
		PCefSslcertPrincipal(FData)
	));
end;

// Returns the state or province name.
// The resulting string must be freed by calling cef_string_userfree_free().
function TCefSslcertPrincipalRef.GetStateOrProvinceName: ustring;
begin
	Result := TWACef.StringFreeAndGet(PCefSslcertPrincipal(FData).get_state_or_province_name(
		PCefSslcertPrincipal(FData)
	));
end;

// Returns the country name.
// The resulting string must be freed by calling cef_string_userfree_free().
function TCefSslcertPrincipalRef.GetCountryName: ustring;
begin
	Result := TWACef.StringFreeAndGet(PCefSslcertPrincipal(FData).get_country_name(
		PCefSslcertPrincipal(FData)
	));
end;

// Retrieve the list of street addresses.
procedure TCefSslcertPrincipalRef.GetStreetAddresses(aAddresses: TStrings);
var
	addresses_list: TCefStringList;
	addresses_iter: Integer;
	addresses_item: TCefString;
begin
	addresses_list := cef_string_list_alloc();
	for addresses_iter := 0 to aAddresses.Count - 1 do
	begin
		addresses_item := TWACef.ToCefString(aAddresses[addresses_iter]);
		cef_string_list_append(addresses_list, @addresses_item);
	end;
	PCefSslcertPrincipal(FData).get_street_addresses(
		PCefSslcertPrincipal(FData),
		addresses_list
	);
	aAddresses.Clear;
	FillChar(addresses_item, SizeOf(addresses_item), 0);
	for addresses_iter := 0 to cef_string_list_size(addresses_list) - 1 do
	begin
		FillChar(addresses_item, SizeOf(addresses_item), 0);
		cef_string_list_value(addresses_list, addresses_iter, @addresses_item);
		aAddresses.Add(TWACef.StringClearAndGet(addresses_item));
	end;
	cef_string_list_free(addresses_list);
end;

// Retrieve the list of organization names.
procedure TCefSslcertPrincipalRef.GetOrganizationNames(aNames: TStrings);
var
	names_list: TCefStringList;
	names_iter: Integer;
	names_item: TCefString;
begin
	names_list := cef_string_list_alloc();
	for names_iter := 0 to aNames.Count - 1 do
	begin
		names_item := TWACef.ToCefString(aNames[names_iter]);
		cef_string_list_append(names_list, @names_item);
	end;
	PCefSslcertPrincipal(FData).get_organization_names(
		PCefSslcertPrincipal(FData),
		names_list
	);
	aNames.Clear;
	FillChar(names_item, SizeOf(names_item), 0);
	for names_iter := 0 to cef_string_list_size(names_list) - 1 do
	begin
		FillChar(names_item, SizeOf(names_item), 0);
		cef_string_list_value(names_list, names_iter, @names_item);
		aNames.Add(TWACef.StringClearAndGet(names_item));
	end;
	cef_string_list_free(names_list);
end;

// Retrieve the list of organization unit names.
procedure TCefSslcertPrincipalRef.GetOrganizationUnitNames(aNames: TStrings);
var
	names_list: TCefStringList;
	names_iter: Integer;
	names_item: TCefString;
begin
	names_list := cef_string_list_alloc();
	for names_iter := 0 to aNames.Count - 1 do
	begin
		names_item := TWACef.ToCefString(aNames[names_iter]);
		cef_string_list_append(names_list, @names_item);
	end;
	PCefSslcertPrincipal(FData).get_organization_unit_names(
		PCefSslcertPrincipal(FData),
		names_list
	);
	aNames.Clear;
	FillChar(names_item, SizeOf(names_item), 0);
	for names_iter := 0 to cef_string_list_size(names_list) - 1 do
	begin
		FillChar(names_item, SizeOf(names_item), 0);
		cef_string_list_value(names_list, names_iter, @names_item);
		aNames.Add(TWACef.StringClearAndGet(names_item));
	end;
	cef_string_list_free(names_list);
end;

// Retrieve the list of domain components.
procedure TCefSslcertPrincipalRef.GetDomainComponents(aComponents: TStrings);
var
	components_list: TCefStringList;
	components_iter: Integer;
	components_item: TCefString;
begin
	components_list := cef_string_list_alloc();
	for components_iter := 0 to aComponents.Count - 1 do
	begin
		components_item := TWACef.ToCefString(aComponents[components_iter]);
		cef_string_list_append(components_list, @components_item);
	end;
	PCefSslcertPrincipal(FData).get_domain_components(
		PCefSslcertPrincipal(FData),
		components_list
	);
	aComponents.Clear;
	FillChar(components_item, SizeOf(components_item), 0);
	for components_iter := 0 to cef_string_list_size(components_list) - 1 do
	begin
		FillChar(components_item, SizeOf(components_item), 0);
		cef_string_list_value(components_list, components_iter, @components_item);
		aComponents.Add(TWACef.StringClearAndGet(components_item));
	end;
	cef_string_list_free(components_list);
end;

{Public section}
class function TCefSslcertPrincipalRef.UnWrap(data: Pointer): ICefSslcertPrincipal;
begin
	if data <> nil then
		Result := Create(data) as ICefSslcertPrincipal
	else
		Result := nil;
end;
//..............................................................................TCefSslinfoRef
{Protected section}
// Returns the subject of the X.509 certificate. For HTTPS server certificates
// this represents the web server.  The common name of the subject should
// match the host name of the web server.
function TCefSslinfoRef.GetSubject: ICefSslcertPrincipal;
begin
	Result := TCefSslcertPrincipalRef.UnWrap(PCefSslinfo(FData).get_subject(
		PCefSslinfo(FData)
	));
end;

// Returns the issuer of the X.509 certificate.
function TCefSslinfoRef.GetIssuer: ICefSslcertPrincipal;
begin
	Result := TCefSslcertPrincipalRef.UnWrap(PCefSslinfo(FData).get_issuer(
		PCefSslinfo(FData)
	));
end;

// Returns the DER encoded serial number for the X.509 certificate. The value
// possibly includes a leading 00 byte.
function TCefSslinfoRef.GetSerialNumber: ICefBinaryValue;
begin
	Result := TCefBinaryValueRef.UnWrap(PCefSslinfo(FData).get_serial_number(
		PCefSslinfo(FData)
	));
end;

// Returns the date before which the X.509 certificate is invalid.
// CefTime.GetTimeT() will return 0 if no date was specified.
function TCefSslinfoRef.GetValidStart: TCefTime;
begin
	Result := PCefSslinfo(FData).get_valid_start(
		PCefSslinfo(FData)
	);
end;

// Returns the date after which the X.509 certificate is invalid.
// CefTime.GetTimeT() will return 0 if no date was specified.
function TCefSslinfoRef.GetValidExpiry: TCefTime;
begin
	Result := PCefSslinfo(FData).get_valid_expiry(
		PCefSslinfo(FData)
	);
end;

// Returns the DER encoded data for the X.509 certificate.
function TCefSslinfoRef.GetDerencoded: ICefBinaryValue;
begin
	Result := TCefBinaryValueRef.UnWrap(PCefSslinfo(FData).get_derencoded(
		PCefSslinfo(FData)
	));
end;

// Returns the PEM encoded data for the X.509 certificate.
function TCefSslinfoRef.GetPemencoded: ICefBinaryValue;
begin
	Result := TCefBinaryValueRef.UnWrap(PCefSslinfo(FData).get_pemencoded(
		PCefSslinfo(FData)
	));
end;

{Public section}
class function TCefSslinfoRef.UnWrap(data: Pointer): ICefSslinfo;
begin
	if data <> nil then
		Result := Create(data) as ICefSslinfo
	else
		Result := nil;
end;

//..............................................................................TCefReadHandlerRef
{Protected section}
// Read raw binary data.
function TCefReadHandlerRef.Read(aPtr: cvoid; aSize: csize_t; aN: csize_t): csize_t;
begin
	Result := PCefReadHandler(FData).read(
		PCefReadHandler(FData),
		aPtr,
		aSize,
		aN
	);
end;

// Seek to the specified offset position. |whence| may be any one of SEEK_CUR,
// SEEK_END or SEEK_SET. Return zero on success and non-zero on failure.
function TCefReadHandlerRef.Seek(aOffset: cint64; aWhence: cint): Boolean;
begin
	Result := PCefReadHandler(FData).seek(
		PCefReadHandler(FData),
		aOffset,
		aWhence
	) <> 0;
end;

// Return the current offset position.
function TCefReadHandlerRef.Tell: cint64;
begin
	Result := PCefReadHandler(FData).tell(
		PCefReadHandler(FData)
	);
end;

// Return non-zero if at end of file.
function TCefReadHandlerRef.Eof: Boolean;
begin
	Result := PCefReadHandler(FData).eof(
		PCefReadHandler(FData)
	) <> 0;
end;

// Return true (1) if this handler performs work like accessing the file
// system which may block. Used as a hint for determining the thread to access
// the handler from.
function TCefReadHandlerRef.MayBlock: Boolean;
begin
	Result := PCefReadHandler(FData).may_block(
		PCefReadHandler(FData)
	) <> 0;
end;

{Public section}
class function TCefReadHandlerRef.UnWrap(data: Pointer): ICefReadHandler;
begin
	if data <> nil then
		Result := Create(data) as ICefReadHandler
	else
		Result := nil;
end;
//..............................................................................TCefStreamReaderRef
// Read raw binary data.
function TCefStreamReaderRef.Read(aPtr: cvoid; aSize: csize_t; aN: csize_t): csize_t;
begin
	Result := PCefStreamReader(FData).read(
		PCefStreamReader(FData),
		aPtr,
		aSize,
		aN
	);
end;

// Seek to the specified offset position. |whence| may be any one of SEEK_CUR,
// SEEK_END or SEEK_SET. Returns zero on success and non-zero on failure.
function TCefStreamReaderRef.Seek(aOffset: cint64; aWhence: cint): Boolean;
begin
	Result := PCefStreamReader(FData).seek(
		PCefStreamReader(FData),
		aOffset,
		aWhence
	) <> 0;
end;

// Return the current offset position.
function TCefStreamReaderRef.Tell: cint64;
begin
	Result := PCefStreamReader(FData).tell(
		PCefStreamReader(FData)
	);
end;

// Return non-zero if at end of file.
function TCefStreamReaderRef.Eof: Boolean;
begin
	Result := PCefStreamReader(FData).eof(
		PCefStreamReader(FData)
	) <> 0;
end;

// Returns true (1) if this reader performs work like accessing the file
// system which may block. Used as a hint for determining the thread to access
// the reader from.
function TCefStreamReaderRef.MayBlock: Boolean;
begin
	Result := PCefStreamReader(FData).may_block(
		PCefStreamReader(FData)
	) <> 0;
end;

{Public section}
class function TCefStreamReaderRef.UnWrap(data: Pointer): ICefStreamReader;
begin
	if data <> nil then
		Result := Create(data) as ICefStreamReader
	else
		Result := nil;
end;
// Create a new cef_stream_reader_t object from a file.
class function TCefStreamReaderRef.CreateForFile(const aFileName: ustring): ICefStreamReader;
var
	fileName_str: TCefString;
begin
	fileName_str := TWACef.ToCefString(aFileName);
	Result := TCefStreamReaderRef.UnWrap(cef_stream_reader_create_for_file(
		@fileName_str
	));
end;

// Create a new cef_stream_reader_t object from data.
class function TCefStreamReaderRef.CreateForData(var aData: cvoid; aSize: csize_t): ICefStreamReader;
begin
	Result := TCefStreamReaderRef.UnWrap(cef_stream_reader_create_for_data(
		aData,
		aSize
	));
end;

// Create a new cef_stream_reader_t object from a custom handler.
class function TCefStreamReaderRef.CreateForHandler(const aHandler: ICefReadHandler): ICefStreamReader;
begin
	Result := TCefStreamReaderRef.UnWrap(cef_stream_reader_create_for_handler(
		TWACef.GetData(aHandler)
	));
end;

//..............................................................................TCefWriteHandlerRef
{Protected section}
// Write raw binary data.
function TCefWriteHandlerRef.Write(aPtr: cvoid; aSize: csize_t; aN: csize_t): csize_t;
begin
	Result := PCefWriteHandler(FData).write(
		PCefWriteHandler(FData),
		aPtr,
		aSize,
		aN
	);
end;

// Seek to the specified offset position. |whence| may be any one of SEEK_CUR,
// SEEK_END or SEEK_SET. Return zero on success and non-zero on failure.
function TCefWriteHandlerRef.Seek(aOffset: cint64; aWhence: cint): Boolean;
begin
	Result := PCefWriteHandler(FData).seek(
		PCefWriteHandler(FData),
		aOffset,
		aWhence
	) <> 0;
end;

// Return the current offset position.
function TCefWriteHandlerRef.Tell: cint64;
begin
	Result := PCefWriteHandler(FData).tell(
		PCefWriteHandler(FData)
	);
end;

// Flush the stream.
function TCefWriteHandlerRef.Flush: Boolean;
begin
	Result := PCefWriteHandler(FData).flush(
		PCefWriteHandler(FData)
	) <> 0;
end;

// Return true (1) if this handler performs work like accessing the file
// system which may block. Used as a hint for determining the thread to access
// the handler from.
function TCefWriteHandlerRef.MayBlock: Boolean;
begin
	Result := PCefWriteHandler(FData).may_block(
		PCefWriteHandler(FData)
	) <> 0;
end;

{Public section}
class function TCefWriteHandlerRef.UnWrap(data: Pointer): ICefWriteHandler;
begin
	if data <> nil then
		Result := Create(data) as ICefWriteHandler
	else
		Result := nil;
end;
//..............................................................................TCefStreamWriterRef
{Protected section}
// Write raw binary data.
function TCefStreamWriterRef.Write(aPtr: cvoid; aSize: csize_t; aN: csize_t): csize_t;
begin
	Result := PCefStreamWriter(FData).write(
		PCefStreamWriter(FData),
		aPtr,
		aSize,
		aN
	);
end;

// Seek to the specified offset position. |whence| may be any one of SEEK_CUR,
// SEEK_END or SEEK_SET. Returns zero on success and non-zero on failure.
function TCefStreamWriterRef.Seek(aOffset: cint64; aWhence: cint): Boolean;
begin
	Result := PCefStreamWriter(FData).seek(
		PCefStreamWriter(FData),
		aOffset,
		aWhence
	) <> 0;
end;

// Return the current offset position.
function TCefStreamWriterRef.Tell: cint64;
begin
	Result := PCefStreamWriter(FData).tell(
		PCefStreamWriter(FData)
	);
end;

// Flush the stream.
function TCefStreamWriterRef.Flush: Boolean;
begin
	Result := PCefStreamWriter(FData).flush(
		PCefStreamWriter(FData)
	) <> 0;
end;

// Returns true (1) if this writer performs work like accessing the file
// system which may block. Used as a hint for determining the thread to access
// the writer from.
function TCefStreamWriterRef.MayBlock: Boolean;
begin
	Result := PCefStreamWriter(FData).may_block(
		PCefStreamWriter(FData)
	) <> 0;
end;

{Public section}
class function TCefStreamWriterRef.UnWrap(data: Pointer): ICefStreamWriter;
begin
	if data <> nil then
		Result := Create(data) as ICefStreamWriter
	else
		Result := nil;
end;
// Create a new cef_stream_writer_t object for a file.
class function TCefStreamWriterRef.CreateForFile(const aFileName: ustring): ICefStreamWriter;
var
	fileName_str: TCefString;
begin
	fileName_str := TWACef.ToCefString(aFileName);
	Result := TCefStreamWriterRef.UnWrap(cef_stream_writer_create_for_file(
		@fileName_str
	));
end;

// Create a new cef_stream_writer_t object for a file.
// Create a new cef_stream_writer_t object for a custom handler.
class function TCefStreamWriterRef.CreateForHandler(const aHandler: ICefWriteHandler): ICefStreamWriter;
begin
	Result := TCefStreamWriterRef.UnWrap(cef_stream_writer_create_for_handler(
		TWACef.GetData(aHandler)
	));
end;


//..............................................................................TCefStringVisitorRef
{Protected section}
// Method that will be executed.
procedure TCefStringVisitorRef.Visit(const aString: ustring);
var
	string_str: TCefString;
begin
	string_str := TWACef.ToCefString(aString);
	PCefStringVisitor(FData).visit(
		PCefStringVisitor(FData),
		@string_str
	);
end;

{Public section}
class function TCefStringVisitorRef.UnWrap(data: Pointer): ICefStringVisitor;
begin
	if data <> nil then
		Result := Create(data) as ICefStringVisitor
	else
		Result := nil;
end;

//..............................................................................TCefTaskRef
{Protected section}
// Method that will be executed on the target thread.
procedure TCefTaskRef.Execute;
begin
	PCefTask(FData).execute(
		PCefTask(FData)
	);
end;

{Public section}
class function TCefTaskRef.UnWrap(data: Pointer): ICefTask;
begin
	if data <> nil then
		Result := Create(data) as ICefTask
	else
		Result := nil;
end;
// Returns the task runner for the current thread. Only CEF threads will have
// task runners. An NULL reference will be returned if this function is called
// on an invalid thread.
class function TCefTaskRef.RunnerGetForCurrentThread: ICefTaskRunner;
begin
	Result := TCefTaskRunnerRef.UnWrap(cef_task_runner_get_for_current_thread(
	));
end;

// Returns the task runner for the current thread. Only CEF threads will have
// task runners. An NULL reference will be returned if this function is called
// on an invalid thread.
// Returns the task runner for the specified CEF thread.
class function TCefTaskRef.RunnerGetForThread(aThreadId: TCefThreadId): ICefTaskRunner;
begin
	Result := TCefTaskRunnerRef.UnWrap(cef_task_runner_get_for_thread(
		aThreadId
	));
end;

//..............................................................................TCefTaskRunnerRef
// Returns true (1) if this object is pointing to the same task runner as
// |that| object.
function TCefTaskRunnerRef.IsSame(const aThat: ICefTaskRunner): Boolean;
begin
	Result := PCefTaskRunner(FData).is_same(
		PCefTaskRunner(FData),
		TWACef.GetData(aThat)
	) <> 0;
end;

// Returns true (1) if this task runner belongs to the current thread.
function TCefTaskRunnerRef.BelongsToCurrentThread: Boolean;
begin
	Result := PCefTaskRunner(FData).belongs_to_current_thread(
		PCefTaskRunner(FData)
	) <> 0;
end;

// Returns true (1) if this task runner is for the specified CEF thread.
function TCefTaskRunnerRef.BelongsToThread(aThreadId: TCefThreadId): Boolean;
begin
	Result := PCefTaskRunner(FData).belongs_to_thread(
		PCefTaskRunner(FData),
		aThreadId
	) <> 0;
end;

// Post a task for execution on the thread associated with this task runner.
// Execution will occur asynchronously.
function TCefTaskRunnerRef.PostTask(const aTask: ICefTask): Boolean;
begin
	Result := PCefTaskRunner(FData).post_task(
		PCefTaskRunner(FData),
		TWACef.GetData(aTask)
	) <> 0;
end;

// Post a task for delayed execution on the thread associated with this task
// runner. Execution will occur asynchronously. Delayed tasks are not
// supported on V8 WebWorker threads and will be executed without the
// specified delay.
function TCefTaskRunnerRef.PostDelayedTask(const aTask: ICefTask; aDelayMs: cint64): Boolean;
begin
	Result := PCefTaskRunner(FData).post_delayed_task(
		PCefTaskRunner(FData),
		TWACef.GetData(aTask),
		aDelayMs
	) <> 0;
end;

{Public section}
class function TCefTaskRunnerRef.UnWrap(data: Pointer): ICefTaskRunner;
begin
	if data <> nil then
		Result := Create(data) as ICefTaskRunner
	else
		Result := nil;
end;
// Returns the task runner for the current thread. Only CEF threads will have
// task runners. An NULL reference will be returned if this function is called
// on an invalid thread.
class function TCefTaskRunnerRef.GetForCurrentThread: ICefTaskRunner;
begin
	Result := TCefTaskRunnerRef.UnWrap(cef_task_runner_get_for_current_thread(
	));
end;

// Returns the task runner for the current thread. Only CEF threads will have
// task runners. An NULL reference will be returned if this function is called
// on an invalid thread.
// Returns the task runner for the specified CEF thread.
class function TCefTaskRunnerRef.GetForThread(aThreadId: TCefThreadId): ICefTaskRunner;
begin
	Result := TCefTaskRunnerRef.UnWrap(cef_task_runner_get_for_thread(
		aThreadId
	));
end;


//..............................................................................TCefEndTracingCallbackRef
{Protected section}
// Called after all processes have sent their trace data. |tracing_file| is
// the path at which tracing data was written. The client is responsible for
// deleting |tracing_file|.
procedure TCefEndTracingCallbackRef.OnEndTracingComplete(const aTracingFile: ustring);
var
	tracing_file_str: TCefString;
begin
	tracing_file_str := TWACef.ToCefString(aTracingFile);
	PCefEndTracingCallback(FData).on_end_tracing_complete(
		PCefEndTracingCallback(FData),
		@tracing_file_str
	);
end;

{Public section}
class function TCefEndTracingCallbackRef.UnWrap(data: Pointer): ICefEndTracingCallback;
begin
	if data <> nil then
		Result := Create(data) as ICefEndTracingCallback
	else
		Result := nil;
end;

//..............................................................................TCefUrlrequestRef
// Returns the request object used to create this URL request. The returned
// object is read-only and should not be modified.
function TCefUrlrequestRef.GetRequest: ICefRequest;
begin
	Result := TCefRequestRef.UnWrap(PCefUrlrequest(FData).get_request(
		PCefUrlrequest(FData)
	));
end;

// Returns the client.
function TCefUrlrequestRef.GetClient: ICefUrlrequestClient;
begin
	Result := TCefUrlrequestClientRef.UnWrap(PCefUrlrequest(FData).get_client(
		PCefUrlrequest(FData)
	));
end;

// Returns the request status.
function TCefUrlrequestRef.GetRequestStatus: TCefUrlrequestStatus;
begin
	Result := PCefUrlrequest(FData).get_request_status(
		PCefUrlrequest(FData)
	);
end;

// Returns the request error if status is UR_CANCELED or UR_FAILED, or 0
// otherwise.
function TCefUrlrequestRef.GetRequestError: TCefErrorcode;
begin
	Result := PCefUrlrequest(FData).get_request_error(
		PCefUrlrequest(FData)
	);
end;

// Returns the response, or NULL if no response information is available.
// Response information will only be available after the upload has completed.
// The returned object is read-only and should not be modified.
function TCefUrlrequestRef.GetResponse: ICefResponse;
begin
	Result := TCefResponseRef.UnWrap(PCefUrlrequest(FData).get_response(
		PCefUrlrequest(FData)
	));
end;

// Cancel the request.
procedure TCefUrlrequestRef.Cancel;
begin
	PCefUrlrequest(FData).cancel(
		PCefUrlrequest(FData)
	);
end;

{Public section}
class function TCefUrlrequestRef.UnWrap(data: Pointer): ICefUrlrequest;
begin
	if data <> nil then
		Result := Create(data) as ICefUrlrequest
	else
		Result := nil;
end;
// Create a new URL request. Only GET, POST, HEAD, DELETE and PUT request
// functions are supported. Multiple post data elements are not supported and
// elements of type PDE_TYPE_FILE are only supported for requests originating
// from the browser process. Requests originating from the render process will
// receive the same handling as requests originating from Web content -- if the
// response contains Content-Disposition or Mime-Type header values that would
// not normally be rendered then the response may receive special handling
// inside the browser (for example, via the file download code path instead of
// the URL request code path). The |request| object will be marked as read-only
// after calling this function. In the browser process if |request_context| is
// NULL the global request context will be used. In the render process
// |request_context| must be NULL and the context associated with the current
// renderer process' browser will be used.
class function TCefUrlrequestRef.New(const aRequest: ICefRequest; const aClient: ICefUrlrequestClient; const aRequestContext: ICefRequestContext): ICefUrlrequest;
begin
	Result := TCefUrlrequestRef.UnWrap(cef_urlrequest_create(
		TWACef.GetData(aRequest),
		TWACef.GetData(aClient),
		TWACef.GetData(aRequestContext)
	));
end;

//..............................................................................TCefUrlrequestClientRef
{Protected section}
// Notifies the client that the request has completed. Use the
// cef_urlrequest_t::GetRequestStatus function to determine if the request was
// successful or not.
procedure TCefUrlrequestClientRef.OnRequestComplete(const aRequest: ICefUrlrequest);
begin
	PCefUrlrequestClient(FData).on_request_complete(
		PCefUrlrequestClient(FData),
		TWACef.GetData(aRequest)
	);
end;

// Notifies the client of upload progress. |current| denotes the number of
// bytes sent so far and |total| is the total size of uploading data (or -1 if
// chunked upload is enabled). This function will only be called if the
// UR_FLAG_REPORT_UPLOAD_PROGRESS flag is set on the request.
procedure TCefUrlrequestClientRef.OnUploadProgress(const aRequest: ICefUrlrequest; aCurrent: cint64; aTotal: cint64);
begin
	PCefUrlrequestClient(FData).on_upload_progress(
		PCefUrlrequestClient(FData),
		TWACef.GetData(aRequest),
		aCurrent,
		aTotal
	);
end;

// Notifies the client of download progress. |current| denotes the number of
// bytes received up to the call and |total| is the expected total size of the
// response (or -1 if not determined).
procedure TCefUrlrequestClientRef.OnDownloadProgress(const aRequest: ICefUrlrequest; aCurrent: cint64; aTotal: cint64);
begin
	PCefUrlrequestClient(FData).on_download_progress(
		PCefUrlrequestClient(FData),
		TWACef.GetData(aRequest),
		aCurrent,
		aTotal
	);
end;

// Called when some part of the response is read. |data| contains the current
// bytes received since the last call. This function will not be called if the
// UR_FLAG_NO_DOWNLOAD_DATA flag is set on the request.
procedure TCefUrlrequestClientRef.OnDownloadData(const aRequest: ICefUrlrequest; const aData: cvoid; aDataLength: csize_t);
begin
	PCefUrlrequestClient(FData).on_download_data(
		PCefUrlrequestClient(FData),
		TWACef.GetData(aRequest),
		aData,
		aDataLength
	);
end;

// Called on the IO thread when the browser needs credentials from the user.
// |isProxy| indicates whether the host is a proxy server. |host| contains the
// hostname and |port| contains the port number. Return true (1) to continue
// the request and call cef_auth_callback_t::cont() when the authentication
// information is available. Return false (0) to cancel the request. This
// function will only be called for requests initiated from the browser
// process.
function TCefUrlrequestClientRef.GetAuthCredentials(aIsProxy: Boolean; const aHost: ustring; aPort: cint; const aRealm: ustring; const aScheme: ustring; const aCallback: ICefAuthCallback): Boolean;
var
	host_str: TCefString;
	realm_str: TCefString;
	scheme_str: TCefString;
begin
	host_str := TWACef.ToCefString(aHost);
	realm_str := TWACef.ToCefString(aRealm);
	scheme_str := TWACef.ToCefString(aScheme);
	Result := PCefUrlrequestClient(FData).get_auth_credentials(
		PCefUrlrequestClient(FData),
		Ord(aIsProxy),
		@host_str,
		aPort,
		@realm_str,
		@scheme_str,
		TWACef.GetData(aCallback)
	) <> 0;
end;

{Public section}
class function TCefUrlrequestClientRef.UnWrap(data: Pointer): ICefUrlrequestClient;
begin
	if data <> nil then
		Result := Create(data) as ICefUrlrequestClient
	else
		Result := nil;
end;

//..............................................................................TCefV8contextRef
// Returns the task runner associated with this context. V8 handles can only
// be accessed from the thread on which they are created. This function can be
// called on any render process thread.
function TCefV8contextRef.GetTaskRunner: ICefTaskRunner;
begin
	Result := TCefTaskRunnerRef.UnWrap(PCefV8context(FData).get_task_runner(
		PCefV8context(FData)
	));
end;

// Returns true (1) if the underlying handle is valid and it can be accessed
// on the current thread. Do not call any other functions if this function
// returns false (0).
function TCefV8contextRef.IsValid: Boolean;
begin
	Result := PCefV8context(FData).is_valid(
		PCefV8context(FData)
	) <> 0;
end;

// Returns the browser for this context. This function will return an NULL
// reference for WebWorker contexts.
function TCefV8contextRef.GetBrowser: ICefBrowser;
begin
	Result := TCefBrowserRef.UnWrap(PCefV8context(FData).get_browser(
		PCefV8context(FData)
	));
end;

// Returns the frame for this context. This function will return an NULL
// reference for WebWorker contexts.
function TCefV8contextRef.GetFrame: ICefFrame;
begin
	Result := TCefFrameRef.UnWrap(PCefV8context(FData).get_frame(
		PCefV8context(FData)
	));
end;

// Returns the global object for this context. The context must be entered
// before calling this function.
function TCefV8contextRef.GetGlobal: ICefV8value;
begin
	Result := TCefV8valueRef.UnWrap(PCefV8context(FData).get_global(
		PCefV8context(FData)
	));
end;

// Enter this context. A context must be explicitly entered before creating a
// V8 Object, Array, Function or Date asynchronously. exit() must be called
// the same number of times as enter() before releasing this context. V8
// objects belong to the context in which they are created. Returns true (1)
// if the scope was entered successfully.
function TCefV8contextRef.Enter: Boolean;
begin
	Result := PCefV8context(FData).enter(
		PCefV8context(FData)
	) <> 0;
end;

// Exit this context. Call this function only after calling enter(). Returns
// true (1) if the scope was exited successfully.
function TCefV8contextRef.Exit: Boolean;
begin
	Result := PCefV8context(FData).exit(
		PCefV8context(FData)
	) <> 0;
end;

// Returns true (1) if this object is pointing to the same handle as |that|
// object.
function TCefV8contextRef.IsSame(const aThat: ICefV8context): Boolean;
begin
	Result := PCefV8context(FData).is_same(
		PCefV8context(FData),
		TWACef.GetData(aThat)
	) <> 0;
end;

// Evaluates the specified JavaScript code using this context's global object.
// On success |retval| will be set to the return value, if any, and the
// function will return true (1). On failure |exception| will be set to the
// exception, if any, and the function will return false (0).
function TCefV8contextRef.Eval(const aCode: ustring; var aRetval: ICefV8value; var aException: ICefV8exception): Boolean;
var
	code_str: TCefString;
  retval_proxy: PCefV8Value;
  exception_proxy: PCefV8Exception;
begin
	code_str := TWACef.ToCefString(aCode);
  retval_proxy := TWACef.GetData(aRetval);
  exception_proxy := TWACef.GetData(aException);
	Result := PCefV8context(FData).eval(
		PCefV8context(FData),
		@code_str,
		retval_proxy,
		exception_proxy
	) <> 0;
end;

{Public section}
class function TCefV8contextRef.UnWrap(data: Pointer): ICefV8context;
begin
	if data <> nil then
		Result := Create(data) as ICefV8context
	else
		Result := nil;
end;
// Returns the current (top) context object in the V8 context stack.
class function TCefV8contextRef.GetCurrentContext: ICefV8context;
begin
	Result := TCefV8contextRef.UnWrap(cef_v8context_get_current_context(
	));
end;

// Returns the current (top) context object in the V8 context stack.
// Returns the entered (bottom) context object in the V8 context stack.
class function TCefV8contextRef.GetEnteredContext: ICefV8context;
begin
	Result := TCefV8contextRef.UnWrap(cef_v8context_get_entered_context(
	));
end;

// Returns the current (top) context object in the V8 context stack.
// Returns the entered (bottom) context object in the V8 context stack.
// Returns true (1) if V8 is currently inside a context.
class function TCefV8contextRef.InContext: Boolean;
begin
	Result := cef_v8context_in_context(
	) <> 0;
end;

//..............................................................................TCefV8handlerRef
// Handle execution of the function identified by |name|. |object| is the
// receiver ('this' object) of the function. |arguments| is the list of
// arguments passed to the function. If execution succeeds set |retval| to the
// function return value. If execution fails set |exception| to the exception
// that will be thrown. Return true (1) if execution was handled.
function TCefV8handlerRef.Execute(const aName: ustring; const aObject: ICefV8value; aArgumentsCount: csize_t; const aArguments: TCefv8ValueArray; var aRetval: ICefV8value; var aException: ustring): Boolean;
var
	name_str: TCefString;
	exception_str: TCefString;
  retval_proxy: PCefV8Value;
  arguments_proxy: array of PCefV8Value;
  i: Integer;
begin
	name_str := TWACef.ToCefString(aName);
	exception_str := TWACef.ToCefString(aException);
  retval_proxy := TWACef.GetData(aRetval);
  SetLength(arguments_proxy, aArgumentsCount);
  for i := 0 to aArgumentsCount - 1 do
    arguments_proxy[i] := TWACef.GetData(aArguments[i]);
  retval_proxy := nil;

	Result := PCefV8handler(FData).execute(
		PCefV8handler(FData),
		@name_str,
		TWACef.GetData(aObject),
		aArgumentsCount,
		@arguments_proxy,
    retval_proxy,
		@exception_str
	) <> 0;
  aRetval := TCefv8ValueRef.UnWrap(retval_proxy);
  aException := TWACef.StringClearAndGet(exception_str);
end;

{Public section}
class function TCefV8handlerRef.UnWrap(data: Pointer): ICefV8handler;
begin
	if data <> nil then
		Result := Create(data) as ICefV8handler
	else
		Result := nil;
end;
//..............................................................................TCefV8accessorRef
{Protected section}
// Handle retrieval the accessor value identified by |name|. |object| is the
// receiver ('this' object) of the accessor. If retrieval succeeds set
// |retval| to the return value. If retrieval fails set |exception| to the
// exception that will be thrown. Return true (1) if accessor retrieval was
// handled.
function TCefV8accessorRef.Get(const aName: ustring; const aObject: ICefV8value; out aRetval: ICefV8value; const aException: ustring): Boolean;
var
	name_str: TCefString;
	exception_str: TCefString;
  retval_proxy: PCefV8Value;
begin
	name_str := TWACef.ToCefString(aName);
	exception_str := TWACef.ToCefString(aException);
  retval_proxy := TWACef.GetData(aRetval);
	Result := PCefV8accessor(FData).get(
		PCefV8accessor(FData),
		@name_str,
		TWACef.GetData(aObject),
		retval_proxy,
		@exception_str
	) <> 0;
  aRetval := TCefV8ValueRef.UnWrap(retval_proxy);
end;

// Handle assignment of the accessor value identified by |name|. |object| is
// the receiver ('this' object) of the accessor. |value| is the new value
// being assigned to the accessor. If assignment fails set |exception| to the
// exception that will be thrown. Return true (1) if accessor assignment was
// handled.
function TCefV8accessorRef._Set(const aName: ustring; const aObject: ICefV8value; const aValue: ICefV8value; const aException: ustring): Boolean;
var
	name_str: TCefString;
	exception_str: TCefString;
begin
	name_str := TWACef.ToCefString(aName);
	exception_str := TWACef.ToCefString(aException);
	Result := PCefV8accessor(FData)._set(
		PCefV8accessor(FData),
		@name_str,
		TWACef.GetData(aObject),
		TWACef.GetData(aValue),
		@exception_str
	) <> 0;
end;

{Public section}
class function TCefV8accessorRef.UnWrap(data: Pointer): ICefV8accessor;
begin
	if data <> nil then
		Result := Create(data) as ICefV8accessor
	else
		Result := nil;
end;
//..............................................................................TCefV8exceptionRef
{Protected section}
// Returns the exception message.
// The resulting string must be freed by calling cef_string_userfree_free().
function TCefV8exceptionRef.GetMessage: ustring;
begin
	Result := TWACef.StringFreeAndGet(PCefV8exception(FData).get_message(
		PCefV8exception(FData)
	));
end;

// Returns the line of source code that the exception occurred within.
// The resulting string must be freed by calling cef_string_userfree_free().
function TCefV8exceptionRef.GetSourceLine: ustring;
begin
	Result := TWACef.StringFreeAndGet(PCefV8exception(FData).get_source_line(
		PCefV8exception(FData)
	));
end;

// Returns the resource name for the script from where the function causing
// the error originates.
// The resulting string must be freed by calling cef_string_userfree_free().
function TCefV8exceptionRef.GetScriptResourceName: ustring;
begin
	Result := TWACef.StringFreeAndGet(PCefV8exception(FData).get_script_resource_name(
		PCefV8exception(FData)
	));
end;

// Returns the 1-based number of the line where the error occurred or 0 if the
// line number is unknown.
function TCefV8exceptionRef.GetLineNumber: cint;
begin
	Result := PCefV8exception(FData).get_line_number(
		PCefV8exception(FData)
	);
end;

// Returns the index within the script of the first character where the error
// occurred.
function TCefV8exceptionRef.GetStartPosition: cint;
begin
	Result := PCefV8exception(FData).get_start_position(
		PCefV8exception(FData)
	);
end;

// Returns the index within the script of the last character where the error
// occurred.
function TCefV8exceptionRef.GetEndPosition: cint;
begin
	Result := PCefV8exception(FData).get_end_position(
		PCefV8exception(FData)
	);
end;

// Returns the index within the line of the first character where the error
// occurred.
function TCefV8exceptionRef.GetStartColumn: cint;
begin
	Result := PCefV8exception(FData).get_start_column(
		PCefV8exception(FData)
	);
end;

// Returns the index within the line of the last character where the error
// occurred.
function TCefV8exceptionRef.GetEndColumn: cint;
begin
	Result := PCefV8exception(FData).get_end_column(
		PCefV8exception(FData)
	);
end;

{Public section}
class function TCefV8exceptionRef.UnWrap(data: Pointer): ICefV8exception;
begin
	if data <> nil then
		Result := Create(data) as ICefV8exception
	else
		Result := nil;
end;

//..............................................................................TCefV8valueRef
// Returns true (1) if the underlying handle is valid and it can be accessed
// on the current thread. Do not call any other functions if this function
// returns false (0).
function TCefV8valueRef.IsValid: Boolean;
begin
	Result := PCefV8value(FData).is_valid(
		PCefV8value(FData)
	) <> 0;
end;

// True if the value type is undefined.
function TCefV8valueRef.IsUndefined: Boolean;
begin
	Result := PCefV8value(FData).is_undefined(
		PCefV8value(FData)
	) <> 0;
end;

// True if the value type is null.
function TCefV8valueRef.IsNull: Boolean;
begin
	Result := PCefV8value(FData).is_null(
		PCefV8value(FData)
	) <> 0;
end;

// True if the value type is bool.
function TCefV8valueRef.IsBool: Boolean;
begin
	Result := PCefV8value(FData).is_bool(
		PCefV8value(FData)
	) <> 0;
end;

// True if the value type is int.
function TCefV8valueRef.IsInt: Boolean;
begin
	Result := PCefV8value(FData).is_int(
		PCefV8value(FData)
	) <> 0;
end;

// True if the value type is unsigned int.
function TCefV8valueRef.IsUint: Boolean;
begin
	Result := PCefV8value(FData).is_uint(
		PCefV8value(FData)
	) <> 0;
end;

// True if the value type is double.
function TCefV8valueRef.IsDouble: Boolean;
begin
	Result := PCefV8value(FData).is_double(
		PCefV8value(FData)
	) <> 0;
end;

// True if the value type is Date.
function TCefV8valueRef.IsDate: Boolean;
begin
	Result := PCefV8value(FData).is_date(
		PCefV8value(FData)
	) <> 0;
end;

// True if the value type is string.
function TCefV8valueRef.IsString: Boolean;
begin
	Result := PCefV8value(FData).is_string(
		PCefV8value(FData)
	) <> 0;
end;

// True if the value type is object.
function TCefV8valueRef.IsObject: Boolean;
begin
	Result := PCefV8value(FData).is_object(
		PCefV8value(FData)
	) <> 0;
end;

// True if the value type is array.
function TCefV8valueRef.IsArray: Boolean;
begin
	Result := PCefV8value(FData).is_array(
		PCefV8value(FData)
	) <> 0;
end;

// True if the value type is function.
function TCefV8valueRef.IsFunction: Boolean;
begin
	Result := PCefV8value(FData).is_function(
		PCefV8value(FData)
	) <> 0;
end;

// Returns true (1) if this object is pointing to the same handle as |that|
// object.
function TCefV8valueRef.IsSame(const aThat: ICefV8value): Boolean;
begin
	Result := PCefV8value(FData).is_same(
		PCefV8value(FData),
		TWACef.GetData(aThat)
	) <> 0;
end;

// Return a bool value.  The underlying data will be converted to if
// necessary.
function TCefV8valueRef.GetBoolValue: Boolean;
begin
	Result := PCefV8value(FData).get_bool_value(
		PCefV8value(FData)
	) <> 0;
end;

// Return an int value.  The underlying data will be converted to if
// necessary.
function TCefV8valueRef.GetIntValue: cint32;
begin
	Result := PCefV8value(FData).get_int_value(
		PCefV8value(FData)
	);
end;

// Return an unisgned int value.  The underlying data will be converted to if
// necessary.
function TCefV8valueRef.GetUintValue: cuint32;
begin
	Result := PCefV8value(FData).get_uint_value(
		PCefV8value(FData)
	);
end;

// Return a double value.  The underlying data will be converted to if
// necessary.
function TCefV8valueRef.GetDoubleValue: cdouble;
begin
	Result := PCefV8value(FData).get_double_value(
		PCefV8value(FData)
	);
end;

// Return a Date value.  The underlying data will be converted to if
// necessary.
function TCefV8valueRef.GetDateValue: TCefTime;
begin
	Result := PCefV8value(FData).get_date_value(
		PCefV8value(FData)
	);
end;

// Return a string value.  The underlying data will be converted to if
// necessary.
// The resulting string must be freed by calling cef_string_userfree_free().
function TCefV8valueRef.GetStringValue: ustring;
begin
	Result := TWACef.StringFreeAndGet(PCefV8value(FData).get_string_value(
		PCefV8value(FData)
	));
end;

// Returns true (1) if this is a user created object.
function TCefV8valueRef.IsUserCreated: Boolean;
begin
	Result := PCefV8value(FData).is_user_created(
		PCefV8value(FData)
	) <> 0;
end;

// Returns true (1) if the last function call resulted in an exception. This
// attribute exists only in the scope of the current CEF value object.
function TCefV8valueRef.HasException: Boolean;
begin
	Result := PCefV8value(FData).has_exception(
		PCefV8value(FData)
	) <> 0;
end;

// Returns the exception resulting from the last function call. This attribute
// exists only in the scope of the current CEF value object.
function TCefV8valueRef.GetException: ICefV8exception;
begin
	Result := TCefV8exceptionRef.UnWrap(PCefV8value(FData).get_exception(
		PCefV8value(FData)
	));
end;

// Clears the last exception and returns true (1) on success.
function TCefV8valueRef.ClearException: Boolean;
begin
	Result := PCefV8value(FData).clear_exception(
		PCefV8value(FData)
	) <> 0;
end;

// Returns true (1) if this object will re-throw future exceptions. This
// attribute exists only in the scope of the current CEF value object.
function TCefV8valueRef.WillRethrowExceptions: Boolean;
begin
	Result := PCefV8value(FData).will_rethrow_exceptions(
		PCefV8value(FData)
	) <> 0;
end;

// Set whether this object will re-throw future exceptions. By default
// exceptions are not re-thrown. If a exception is re-thrown the current
// context should not be accessed again until after the exception has been
// caught and not re-thrown. Returns true (1) on success. This attribute
// exists only in the scope of the current CEF value object.
function TCefV8valueRef.SetRethrowExceptions(aRethrow: Boolean): Boolean;
begin
	Result := PCefV8value(FData).set_rethrow_exceptions(
		PCefV8value(FData),
		Ord(aRethrow)
	) <> 0;
end;

// Returns true (1) if the object has a value with the specified identifier.
function TCefV8valueRef.HasValueBykey(const aKey: ustring): Boolean;
var
	key_str: TCefString;
begin
	key_str := TWACef.ToCefString(aKey);
	Result := PCefV8value(FData).has_value_bykey(
		PCefV8value(FData),
		@key_str
	) <> 0;
end;

// Returns true (1) if the object has a value with the specified identifier.
function TCefV8valueRef.HasValueByindex(aIndex: cint): Boolean;
begin
	Result := PCefV8value(FData).has_value_byindex(
		PCefV8value(FData),
		aIndex
	) <> 0;
end;

// Deletes the value with the specified identifier and returns true (1) on
// success. Returns false (0) if this function is called incorrectly or an
// exception is thrown. For read-only and don't-delete values this function
// will return true (1) even though deletion failed.
function TCefV8valueRef.DeleteValueBykey(const aKey: ustring): Boolean;
var
	key_str: TCefString;
begin
	key_str := TWACef.ToCefString(aKey);
	Result := PCefV8value(FData).delete_value_bykey(
		PCefV8value(FData),
		@key_str
	) <> 0;
end;

// Deletes the value with the specified identifier and returns true (1) on
// success. Returns false (0) if this function is called incorrectly, deletion
// fails or an exception is thrown. For read-only and don't-delete values this
// function will return true (1) even though deletion failed.
function TCefV8valueRef.DeleteValueByindex(aIndex: cint): Boolean;
begin
	Result := PCefV8value(FData).delete_value_byindex(
		PCefV8value(FData),
		aIndex
	) <> 0;
end;

// Returns the value with the specified identifier on success. Returns NULL if
// this function is called incorrectly or an exception is thrown.
function TCefV8valueRef.GetValueBykey(const aKey: ustring): ICefV8value;
var
	key_str: TCefString;
begin
	key_str := TWACef.ToCefString(aKey);
	Result := TCefV8valueRef.UnWrap(PCefV8value(FData).get_value_bykey(
		PCefV8value(FData),
		@key_str
	));
end;

// Returns the value with the specified identifier on success. Returns NULL if
// this function is called incorrectly or an exception is thrown.
function TCefV8valueRef.GetValueByindex(aIndex: cint): ICefV8value;
begin
	Result := TCefV8valueRef.UnWrap(PCefV8value(FData).get_value_byindex(
		PCefV8value(FData),
		aIndex
	));
end;

// Associates a value with the specified identifier and returns true (1) on
// success. Returns false (0) if this function is called incorrectly or an
// exception is thrown. For read-only values this function will return true
// (1) even though assignment failed.
function TCefV8valueRef.SetValueBykey(const aKey: ustring; const aValue: ICefV8value; aAttribute: TCefV8Propertyattribute): Boolean;
var
	key_str: TCefString;
begin
	key_str := TWACef.ToCefString(aKey);
	Result := PCefV8value(FData).set_value_bykey(
		PCefV8value(FData),
		@key_str,
		TWACef.GetData(aValue),
		aAttribute
	) <> 0;
end;

// Associates a value with the specified identifier and returns true (1) on
// success. Returns false (0) if this function is called incorrectly or an
// exception is thrown. For read-only values this function will return true
// (1) even though assignment failed.
function TCefV8valueRef.SetValueByindex(aIndex: cint; const aValue: ICefV8value): Boolean;
begin
	Result := PCefV8value(FData).set_value_byindex(
		PCefV8value(FData),
		aIndex,
		TWACef.GetData(aValue)
	) <> 0;
end;

// Registers an identifier and returns true (1) on success. Access to the
// identifier will be forwarded to the cef_v8accessor_t instance passed to
// cef_v8value_t::cef_v8value_create_object(). Returns false (0) if this
// function is called incorrectly or an exception is thrown. For read-only
// values this function will return true (1) even though assignment failed.
function TCefV8valueRef.SetValueByaccessor(const aKey: ustring; aSettings: TCefV8Accesscontrol; aAttribute: TCefV8Propertyattribute): Boolean;
var
	key_str: TCefString;
begin
	key_str := TWACef.ToCefString(aKey);
	Result := PCefV8value(FData).set_value_byaccessor(
		PCefV8value(FData),
		@key_str,
		aSettings,
		aAttribute
	) <> 0;
end;

// Read the keys for the object's values into the specified vector. Integer-
// based keys will also be returned as strings.
function TCefV8valueRef.GetKeys(aKeys: TStrings): Boolean;
var
	keys_list: TCefStringList;
	keys_iter: Integer;
	keys_item: TCefString;
begin
	keys_list := cef_string_list_alloc();
	for keys_iter := 0 to aKeys.Count - 1 do
	begin
		keys_item := TWACef.ToCefString(aKeys[keys_iter]);
		cef_string_list_append(keys_list, @keys_item);
	end;
	Result := PCefV8value(FData).get_keys(
		PCefV8value(FData),
		keys_list
	) <> 0;
	aKeys.Clear;
	FillChar(keys_item, SizeOf(keys_item), 0);
	for keys_iter := 0 to cef_string_list_size(keys_list) - 1 do
	begin
		FillChar(keys_item, SizeOf(keys_item), 0);
		cef_string_list_value(keys_list, keys_iter, @keys_item);
		aKeys.Add(TWACef.StringClearAndGet(keys_item));
	end;
  cef_string_list_free(keys_list);
end;

// Sets the user data for this object and returns true (1) on success. Returns
// false (0) if this function is called incorrectly. This function can only be
// called on user created objects.
function TCefV8valueRef.SetUserData(const aUserData: ICefBase): Boolean;
begin
	Result := PCefV8value(FData).set_user_data(
		PCefV8value(FData),
		TWACef.GetData(aUserData)
	) <> 0;
end;

// Returns the user data, if any, assigned to this object.
function TCefV8valueRef.GetUserData: ICefBase;
begin
	Result := TCefBaseRef.UnWrap(PCefV8value(FData).get_user_data(
		PCefV8value(FData)
	));
end;

// Returns the amount of externally allocated memory registered for the
// object.
function TCefV8valueRef.GetExternallyAllocatedMemory: cint;
begin
	Result := PCefV8value(FData).get_externally_allocated_memory(
		PCefV8value(FData)
	);
end;

// Adjusts the amount of registered external memory for the object. Used to
// give V8 an indication of the amount of externally allocated memory that is
// kept alive by JavaScript objects. V8 uses this information to decide when
// to perform global garbage collection. Each cef_v8value_t tracks the amount
// of external memory associated with it and automatically decreases the
// global total by the appropriate amount on its destruction.
// |change_in_bytes| specifies the number of bytes to adjust by. This function
// returns the number of bytes associated with the object after the
// adjustment. This function can only be called on user created objects.
function TCefV8valueRef.AdjustExternallyAllocatedMemory(aChangeInBytes: cint): cint;
begin
	Result := PCefV8value(FData).adjust_externally_allocated_memory(
		PCefV8value(FData),
		aChangeInBytes
	);
end;

// Returns the number of elements in the array.
function TCefV8valueRef.GetArrayLength: cint;
begin
	Result := PCefV8value(FData).get_array_length(
		PCefV8value(FData)
	);
end;

// Returns the function name.
// The resulting string must be freed by calling cef_string_userfree_free().
function TCefV8valueRef.GetFunctionName: ustring;
begin
	Result := TWACef.StringFreeAndGet(PCefV8value(FData).get_function_name(
		PCefV8value(FData)
	));
end;

// Returns the function handler or NULL if not a CEF-created function.
function TCefV8valueRef.GetFunctionHandler: ICefV8handler;
begin
	Result := TCefV8handlerRef.UnWrap(PCefV8value(FData).get_function_handler(
		PCefV8value(FData)
	));
end;

// Execute the function using the current V8 context. This function should
// only be called from within the scope of a cef_v8handler_t or
// cef_v8accessor_t callback, or in combination with calling enter() and
// exit() on a stored cef_v8context_t reference. |object| is the receiver
// ('this' object) of the function. If |object| is NULL the current context's
// global object will be used. |arguments| is the list of arguments that will
// be passed to the function. Returns the function return value on success.
// Returns NULL if this function is called incorrectly or an exception is
// thrown.
function TCefV8valueRef.ExecuteFunction(const aObject: ICefV8value; aArgumentsCount: csize_t; const aArguments: TCefv8ValueArray): ICefV8value;
var
  args: PPCefV8Value;
  i: cint;
begin
  GetMem(args, SizeOf(PCefV8Value) * aArgumentsCount);
  try
    for i := 0 to aArgumentsCount - 1 do
      args[i] := TWACef.GetData(aArguments[i]);
    Result := TCefv8ValueRef.UnWrap(PCefV8Value(FData)^.execute_function(PCefV8Value(FData), TWACef.GetData(aObject), aArgumentsCount, args));
  finally
    FreeMem(args);
  end;
end;

// Execute the function using the specified V8 context. |object| is the
// receiver ('this' object) of the function. If |object| is NULL the specified
// context's global object will be used. |arguments| is the list of arguments
// that will be passed to the function. Returns the function return value on
// success. Returns NULL if this function is called incorrectly or an
// exception is thrown.
function TCefV8valueRef.ExecuteFunctionWithContext(const aContext: ICefV8context; const aObject: ICefV8value; aArgumentsCount: csize_t; const aArguments: TCefv8ValueArray): ICefV8value;
var
  args: PPCefV8Value;
  i: cint;
begin
  GetMem(args, SizeOf(PCefV8Value) * aArgumentsCount);
  try
    for i := 0 to aArgumentsCount - 1 do
      args[i] := TWACef.GetData(aArguments[i]);
    Result := TCefv8ValueRef.UnWrap(PCefV8Value(FData)^.execute_function_with_context(PCefV8Value(FData), TWACef.GetData(aContext), TWACef.GetData(aObject), aArgumentsCount, args));
  finally
    FreeMem(args);
  end;
end;

{Public section}
class function TCefV8valueRef.UnWrap(data: Pointer): ICefV8value;
begin
	if data <> nil then
		Result := Create(data) as ICefV8value
	else
		Result := nil;
end;

// Create a new cef_v8value_t object of type undefined.
class function TCefV8valueRef.CreateUndefined: ICefV8value;
begin
	Result := TCefV8valueRef.UnWrap(cef_v8value_create_undefined(
	));
end;

// Create a new cef_v8value_t object of type null.
class function TCefV8valueRef.CreateNull: ICefV8value;
begin
	Result := TCefV8valueRef.UnWrap(cef_v8value_create_null(
	));
end;

// Create a new cef_v8value_t object of type bool.
class function TCefV8valueRef.CreateBool(aValue: Boolean): ICefV8value;
begin
	Result := TCefV8valueRef.UnWrap(cef_v8value_create_bool(
		Ord(aValue)
	));
end;

// Create a new cef_v8value_t object of type int.
class function TCefV8valueRef.CreateInt(aValue: cint32): ICefV8value;
begin
	Result := TCefV8valueRef.UnWrap(cef_v8value_create_int(
		aValue
	));
end;

// Create a new cef_v8value_t object of type unsigned int.
class function TCefV8valueRef.CreateUint(aValue: cuint32): ICefV8value;
begin
	Result := TCefV8valueRef.UnWrap(cef_v8value_create_uint(
		aValue
	));
end;

// Create a new cef_v8value_t object of type double.
class function TCefV8valueRef.CreateDouble(aValue: cdouble): ICefV8value;
begin
	Result := TCefV8valueRef.UnWrap(cef_v8value_create_double(
		aValue
	));
end;

// Create a new cef_v8value_t object of type Date. This function should only be
// called from within the scope of a cef_render_process_handler_t,
// cef_v8handler_t or cef_v8accessor_t callback, or in combination with calling
// enter() and exit() on a stored cef_v8context_t reference.
class function TCefV8valueRef.CreateDate(aDate: TCefTime): ICefV8value;
var
	date_proxy: TCefTime;
begin
	date_proxy := aDate;
	Result := TCefV8valueRef.UnWrap(cef_v8value_create_date(
		@date_proxy
	));
	aDate := date_proxy;
end;

// Create a new cef_v8value_t object of type string.
class function TCefV8valueRef.CreateString(const aValue: ustring): ICefV8value;
var
	value_str: TCefString;
begin
	value_str := TWACef.ToCefString(aValue);
	Result := TCefV8valueRef.UnWrap(cef_v8value_create_string(
		@value_str
	));
end;

// Create a new cef_v8value_t object of type object with optional accessor. This
// function should only be called from within the scope of a
// cef_render_process_handler_t, cef_v8handler_t or cef_v8accessor_t callback,
// or in combination with calling enter() and exit() on a stored cef_v8context_t
// reference.
class function TCefV8valueRef.CreateObject(const aAccessor: ICefV8accessor): ICefV8value;
begin
	Result := TCefV8valueRef.UnWrap(cef_v8value_create_object(
		TWACef.GetData(aAccessor)
	));
end;

// Create a new cef_v8value_t object of type array with the specified |length|.
// If |length| is negative the returned array will have length 0. This function
// should only be called from within the scope of a
// cef_render_process_handler_t, cef_v8handler_t or cef_v8accessor_t callback,
// or in combination with calling enter() and exit() on a stored cef_v8context_t
// reference.
class function TCefV8valueRef.CreateArray(aLength: cint): ICefV8value;
begin
	Result := TCefV8valueRef.UnWrap(cef_v8value_create_array(
		aLength
	));
end;

// Create a new cef_v8value_t object of type function. This function should only
// be called from within the scope of a cef_render_process_handler_t,
// cef_v8handler_t or cef_v8accessor_t callback, or in combination with calling
// enter() and exit() on a stored cef_v8context_t reference.
class function TCefV8valueRef.CreateFunction(const aName: ustring; const aHandler: ICefV8handler): ICefV8value;
var
	name_str: TCefString;
begin
	name_str := TWACef.ToCefString(aName);
	Result := TCefV8valueRef.UnWrap(cef_v8value_create_function(
		@name_str,
		TWACef.GetData(aHandler)
	));
end;

//..............................................................................TCefV8stackTraceRef
{Protected section}
// Returns true (1) if the underlying handle is valid and it can be accessed
// on the current thread. Do not call any other functions if this function
// returns false (0).
function TCefV8stackTraceRef.IsValid: Boolean;
begin
	Result := PCefV8stackTrace(FData).is_valid(
		PCefV8stackTrace(FData)
	) <> 0;
end;

// Returns the number of stack frames.
function TCefV8stackTraceRef.GetFrameCount: cint;
begin
	Result := PCefV8stackTrace(FData).get_frame_count(
		PCefV8stackTrace(FData)
	);
end;

// Returns the stack frame at the specified 0-based index.
function TCefV8stackTraceRef.GetFrame(aIndex: cint): ICefV8stackFrame;
begin
	Result := TCefV8stackFrameRef.UnWrap(PCefV8stackTrace(FData).get_frame(
		PCefV8stackTrace(FData),
		aIndex
	));
end;

{Public section}
class function TCefV8stackTraceRef.UnWrap(data: Pointer): ICefV8stackTrace;
begin
	if data <> nil then
		Result := Create(data) as ICefV8stackTrace
	else
		Result := nil;
end;

// Returns the stack trace for the currently active context. |frame_limit| is
// the maximum number of frames that will be captured.
class function TCefV8stackTraceRef.GetCurrent(aFrameLimit: cint): ICefV8stackTrace;
begin
	Result := TCefV8stackTraceRef.UnWrap(cef_v8stack_trace_get_current(
		aFrameLimit
	));
end;

//..............................................................................TCefV8stackFrameRef
{Protected section}
// Returns true (1) if the underlying handle is valid and it can be accessed
// on the current thread. Do not call any other functions if this function
// returns false (0).
function TCefV8stackFrameRef.IsValid: Boolean;
begin
	Result := PCefV8stackFrame(FData).is_valid(
		PCefV8stackFrame(FData)
	) <> 0;
end;

// Returns the name of the resource script that contains the function.
// The resulting string must be freed by calling cef_string_userfree_free().
function TCefV8stackFrameRef.GetScriptName: ustring;
begin
	Result := TWACef.StringFreeAndGet(PCefV8stackFrame(FData).get_script_name(
		PCefV8stackFrame(FData)
	));
end;

// Returns the name of the resource script that contains the function or the
// sourceURL value if the script name is undefined and its source ends with a
// "//@ sourceURL=..." string.
// The resulting string must be freed by calling cef_string_userfree_free().
function TCefV8stackFrameRef.GetScriptNameOrSourceUrl: ustring;
begin
	Result := TWACef.StringFreeAndGet(PCefV8stackFrame(FData).get_script_name_or_source_url(
		PCefV8stackFrame(FData)
	));
end;

// Returns the name of the function.
// The resulting string must be freed by calling cef_string_userfree_free().
function TCefV8stackFrameRef.GetFunctionName: ustring;
begin
	Result := TWACef.StringFreeAndGet(PCefV8stackFrame(FData).get_function_name(
		PCefV8stackFrame(FData)
	));
end;

// Returns the 1-based line number for the function call or 0 if unknown.
function TCefV8stackFrameRef.GetLineNumber: cint;
begin
	Result := PCefV8stackFrame(FData).get_line_number(
		PCefV8stackFrame(FData)
	);
end;

// Returns the 1-based column offset on the line for the function call or 0 if
// unknown.
function TCefV8stackFrameRef.GetColumn: cint;
begin
	Result := PCefV8stackFrame(FData).get_column(
		PCefV8stackFrame(FData)
	);
end;

// Returns true (1) if the function was compiled using eval().
function TCefV8stackFrameRef.IsEval: Boolean;
begin
	Result := PCefV8stackFrame(FData).is_eval(
		PCefV8stackFrame(FData)
	) <> 0;
end;

// Returns true (1) if the function was called as a constructor via "new".
function TCefV8stackFrameRef.IsConstructor: Boolean;
begin
	Result := PCefV8stackFrame(FData).is_constructor(
		PCefV8stackFrame(FData)
	) <> 0;
end;

{Public section}
class function TCefV8stackFrameRef.UnWrap(data: Pointer): ICefV8stackFrame;
begin
	if data <> nil then
		Result := Create(data) as ICefV8stackFrame
	else
		Result := nil;
end;

//..............................................................................TCefValueRef
// Returns true (1) if the underlying data is valid. This will always be true
// (1) for simple types. For complex types (binary, dictionary and list) the
// underlying data may become invalid if owned by another object (e.g. list or
// dictionary) and that other object is then modified or destroyed. This value
// object can be re-used by calling Set*() even if the underlying data is
// invalid.
function TCefValueRef.IsValid: Boolean;
begin
	Result := PCefValue(FData).is_valid(
		PCefValue(FData)
	) <> 0;
end;

// Returns true (1) if the underlying data is owned by another object.
function TCefValueRef.IsOwned: Boolean;
begin
	Result := PCefValue(FData).is_owned(
		PCefValue(FData)
	) <> 0;
end;

// Returns true (1) if the underlying data is read-only. Some APIs may expose
// read-only objects.
function TCefValueRef.IsReadOnly: Boolean;
begin
	Result := PCefValue(FData).is_read_only(
		PCefValue(FData)
	) <> 0;
end;

// Returns true (1) if this object and |that| object have the same underlying
// data. If true (1) modifications to this object will also affect |that|
// object and vice-versa.
function TCefValueRef.IsSame(const aThat: ICefValue): Boolean;
begin
	Result := PCefValue(FData).is_same(
		PCefValue(FData),
		TWACef.GetData(aThat)
	) <> 0;
end;

// Returns true (1) if this object and |that| object have an equivalent
// underlying value but are not necessarily the same object.
function TCefValueRef.IsEqual(const aThat: ICefValue): Boolean;
begin
	Result := PCefValue(FData).is_equal(
		PCefValue(FData),
		TWACef.GetData(aThat)
	) <> 0;
end;

// Returns a copy of this object. The underlying data will also be copied.
function TCefValueRef.Copy: ICefValue;
begin
	Result := TCefValueRef.UnWrap(PCefValue(FData).copy(
		PCefValue(FData)
	));
end;

// Returns the underlying value type.
function TCefValueRef.GetType: TCefValueType;
begin
	Result := PCefValue(FData).get_type(
		PCefValue(FData)
	);
end;

// Returns the underlying value as type bool.
function TCefValueRef.GetBool: Boolean;
begin
	Result := PCefValue(FData).get_bool(
		PCefValue(FData)
	) <> 0;
end;

// Returns the underlying value as type int.
function TCefValueRef.GetInt: cint;
begin
	Result := PCefValue(FData).get_int(
		PCefValue(FData)
	);
end;

// Returns the underlying value as type double.
function TCefValueRef.GetDouble: cdouble;
begin
	Result := PCefValue(FData).get_double(
		PCefValue(FData)
	);
end;

// Returns the underlying value as type string.
// The resulting string must be freed by calling cef_string_userfree_free().
function TCefValueRef.GetString: ustring;
begin
	Result := TWACef.StringFreeAndGet(PCefValue(FData).get_string(
		PCefValue(FData)
	));
end;

// Returns the underlying value as type binary. The returned reference may
// become invalid if the value is owned by another object or if ownership is
// transferred to another object in the future. To maintain a reference to the
// value after assigning ownership to a dictionary or list pass this object to
// the set_value() function instead of passing the returned reference to
// set_binary().
function TCefValueRef.GetBinary: ICefBinaryValue;
begin
	Result := TCefBinaryValueRef.UnWrap(PCefValue(FData).get_binary(
		PCefValue(FData)
	));
end;

// Returns the underlying value as type dictionary. The returned reference may
// become invalid if the value is owned by another object or if ownership is
// transferred to another object in the future. To maintain a reference to the
// value after assigning ownership to a dictionary or list pass this object to
// the set_value() function instead of passing the returned reference to
// set_dictionary().
function TCefValueRef.GetDictionary: ICefDictionaryValue;
begin
	Result := TCefDictionaryValueRef.UnWrap(PCefValue(FData).get_dictionary(
		PCefValue(FData)
	));
end;

// Returns the underlying value as type list. The returned reference may
// become invalid if the value is owned by another object or if ownership is
// transferred to another object in the future. To maintain a reference to the
// value after assigning ownership to a dictionary or list pass this object to
// the set_value() function instead of passing the returned reference to
// set_list().
function TCefValueRef.GetList: ICefListValue;
begin
	Result := TCefListValueRef.UnWrap(PCefValue(FData).get_list(
		PCefValue(FData)
	));
end;

// Sets the underlying value as type null. Returns true (1) if the value was
// set successfully.
function TCefValueRef.SetNull: Boolean;
begin
	Result := PCefValue(FData).set_null(
		PCefValue(FData)
	) <> 0;
end;

// Sets the underlying value as type bool. Returns true (1) if the value was
// set successfully.
function TCefValueRef.SetBool(aValue: Boolean): Boolean;
begin
	Result := PCefValue(FData).set_bool(
		PCefValue(FData),
		Ord(aValue)
	) <> 0;
end;

// Sets the underlying value as type int. Returns true (1) if the value was
// set successfully.
function TCefValueRef.SetInt(aValue: cint): Boolean;
begin
	Result := PCefValue(FData).set_int(
		PCefValue(FData),
		aValue
	) <> 0;
end;

// Sets the underlying value as type double. Returns true (1) if the value was
// set successfully.
function TCefValueRef.SetDouble(aValue: cdouble): Boolean;
begin
	Result := PCefValue(FData).set_double(
		PCefValue(FData),
		aValue
	) <> 0;
end;

// Sets the underlying value as type string. Returns true (1) if the value was
// set successfully.
function TCefValueRef.SetString(const aValue: ustring): Boolean;
var
	value_str: TCefString;
begin
	value_str := TWACef.ToCefString(aValue);
	Result := PCefValue(FData).set_string(
		PCefValue(FData),
		@value_str
	) <> 0;
end;

// Sets the underlying value as type binary. Returns true (1) if the value was
// set successfully. This object keeps a reference to |value| and ownership of
// the underlying data remains unchanged.
function TCefValueRef.SetBinary(const aValue: ICefBinaryValue): Boolean;
begin
	Result := PCefValue(FData).set_binary(
		PCefValue(FData),
		TWACef.GetData(aValue)
	) <> 0;
end;

// Sets the underlying value as type dict. Returns true (1) if the value was
// set successfully. This object keeps a reference to |value| and ownership of
// the underlying data remains unchanged.
function TCefValueRef.SetDictionary(const aValue: ICefDictionaryValue): Boolean;
begin
	Result := PCefValue(FData).set_dictionary(
		PCefValue(FData),
		TWACef.GetData(aValue)
	) <> 0;
end;

// Sets the underlying value as type list. Returns true (1) if the value was
// set successfully. This object keeps a reference to |value| and ownership of
// the underlying data remains unchanged.
function TCefValueRef.SetList(const aValue: ICefListValue): Boolean;
begin
	Result := PCefValue(FData).set_list(
		PCefValue(FData),
		TWACef.GetData(aValue)
	) <> 0;
end;

{Public section}
class function TCefValueRef.UnWrap(data: Pointer): ICefValue;
begin
	if data <> nil then
		Result := Create(data) as ICefValue
	else
		Result := nil;
end;
// Creates a new object.
class function TCefValueRef.New: ICefValue;
begin
	Result := TCefValueRef.UnWrap(cef_value_create(
	));
end;

//..............................................................................TCefBinaryValueRef
{Protected section}
// Returns true (1) if this object is valid. This object may become invalid if
// the underlying data is owned by another object (e.g. list or dictionary)
// and that other object is then modified or destroyed. Do not call any other
// functions if this function returns false (0).
function TCefBinaryValueRef.IsValid: Boolean;
begin
	Result := PCefBinaryValue(FData).is_valid(
		PCefBinaryValue(FData)
	) <> 0;
end;

// Returns true (1) if this object is currently owned by another object.
function TCefBinaryValueRef.IsOwned: Boolean;
begin
	Result := PCefBinaryValue(FData).is_owned(
		PCefBinaryValue(FData)
	) <> 0;
end;

// Returns true (1) if this object and |that| object have the same underlying
// data.
function TCefBinaryValueRef.IsSame(const aThat: ICefBinaryValue): Boolean;
begin
	Result := PCefBinaryValue(FData).is_same(
		PCefBinaryValue(FData),
		TWACef.GetData(aThat)
	) <> 0;
end;

// Returns true (1) if this object and |that| object have an equivalent
// underlying value but are not necessarily the same object.
function TCefBinaryValueRef.IsEqual(const aThat: ICefBinaryValue): Boolean;
begin
	Result := PCefBinaryValue(FData).is_equal(
		PCefBinaryValue(FData),
		TWACef.GetData(aThat)
	) <> 0;
end;

// Returns a copy of this object. The data in this object will also be copied.
function TCefBinaryValueRef.Copy: ICefBinaryValue;
begin
	Result := TCefBinaryValueRef.UnWrap(PCefBinaryValue(FData).copy(
		PCefBinaryValue(FData)
	));
end;

// Returns the data size.
function TCefBinaryValueRef.GetSize: csize_t;
begin
	Result := PCefBinaryValue(FData).get_size(
		PCefBinaryValue(FData)
	);
end;

// Read up to |buffer_size| number of bytes into |buffer|. Reading begins at
// the specified byte |data_offset|. Returns the number of bytes read.
function TCefBinaryValueRef.GetData(aBuffer: cvoid; aBufferSize: csize_t; aDataOffset: csize_t): csize_t;
begin
	Result := PCefBinaryValue(FData).get_data(
		PCefBinaryValue(FData),
		aBuffer,
		aBufferSize,
		aDataOffset
	);
end;

{Public section}
class function TCefBinaryValueRef.UnWrap(data: Pointer): ICefBinaryValue;
begin
	if data <> nil then
		Result := Create(data) as ICefBinaryValue
	else
		Result := nil;
end;
// Creates a new object that is not owned by any other object. The specified
// |data| will be copied.
class function TCefBinaryValueRef.New(var aData: cvoid; aDataSize: csize_t): ICefBinaryValue;
begin
	Result := TCefBinaryValueRef.UnWrap(cef_binary_value_create(
		aData,
		aDataSize
	));
end;

//..............................................................................TCefDictionaryValueRef
// Returns true (1) if this object is valid. This object may become invalid if
// the underlying data is owned by another object (e.g. list or dictionary)
// and that other object is then modified or destroyed. Do not call any other
// functions if this function returns false (0).
function TCefDictionaryValueRef.IsValid: Boolean;
begin
	Result := PCefDictionaryValue(FData).is_valid(
		PCefDictionaryValue(FData)
	) <> 0;
end;

// Returns true (1) if this object is currently owned by another object.
function TCefDictionaryValueRef.IsOwned: Boolean;
begin
	Result := PCefDictionaryValue(FData).is_owned(
		PCefDictionaryValue(FData)
	) <> 0;
end;

// Returns true (1) if the values of this object are read-only. Some APIs may
// expose read-only objects.
function TCefDictionaryValueRef.IsReadOnly: Boolean;
begin
	Result := PCefDictionaryValue(FData).is_read_only(
		PCefDictionaryValue(FData)
	) <> 0;
end;

// Returns true (1) if this object and |that| object have the same underlying
// data. If true (1) modifications to this object will also affect |that|
// object and vice-versa.
function TCefDictionaryValueRef.IsSame(const aThat: ICefDictionaryValue): Boolean;
begin
	Result := PCefDictionaryValue(FData).is_same(
		PCefDictionaryValue(FData),
		TWACef.GetData(aThat)
	) <> 0;
end;

// Returns true (1) if this object and |that| object have an equivalent
// underlying value but are not necessarily the same object.
function TCefDictionaryValueRef.IsEqual(const aThat: ICefDictionaryValue): Boolean;
begin
	Result := PCefDictionaryValue(FData).is_equal(
		PCefDictionaryValue(FData),
		TWACef.GetData(aThat)
	) <> 0;
end;

// Returns a writable copy of this object. If |exclude_NULL_children| is true
// (1) any NULL dictionaries or lists will be excluded from the copy.
function TCefDictionaryValueRef.Copy(aExcludeEmptyChildren: Boolean): ICefDictionaryValue;
begin
	Result := TCefDictionaryValueRef.UnWrap(PCefDictionaryValue(FData).copy(
		PCefDictionaryValue(FData),
		Ord(aExcludeEmptyChildren)
	));
end;

// Returns the number of values.
function TCefDictionaryValueRef.GetSize: csize_t;
begin
	Result := PCefDictionaryValue(FData).get_size(
		PCefDictionaryValue(FData)
	);
end;

// Removes all values. Returns true (1) on success.
function TCefDictionaryValueRef.Clear: Boolean;
begin
	Result := PCefDictionaryValue(FData).clear(
		PCefDictionaryValue(FData)
	) <> 0;
end;

// Returns true (1) if the current dictionary has a value for the given key.
function TCefDictionaryValueRef.HasKey(const aKey: ustring): Boolean;
var
	key_str: TCefString;
begin
	key_str := TWACef.ToCefString(aKey);
	Result := PCefDictionaryValue(FData).has_key(
		PCefDictionaryValue(FData),
		@key_str
	) <> 0;
end;

// Reads all keys for this dictionary into the specified vector.
function TCefDictionaryValueRef.GetKeys(aKeys: TStrings): Boolean;
var
	keys_list: TCefStringList;
	keys_iter: Integer;
	keys_item: TCefString;
begin
	keys_list := cef_string_list_alloc();
	for keys_iter := 0 to aKeys.Count - 1 do
	begin
		keys_item := TWACef.ToCefString(aKeys[keys_iter]);
		cef_string_list_append(keys_list, @keys_item);
	end;
	Result := PCefDictionaryValue(FData).get_keys(
		PCefDictionaryValue(FData),
		keys_list
	) <> 0;
	aKeys.Clear;
	FillChar(keys_item, SizeOf(keys_item), 0);
	for keys_iter := 0 to cef_string_list_size(keys_list) - 1 do
	begin
		FillChar(keys_item, SizeOf(keys_item), 0);
		cef_string_list_value(keys_list, keys_iter, @keys_item);
		aKeys.Add(TWACef.StringClearAndGet(keys_item));
	end;	cef_string_list_free(keys_list);
end;

// Removes the value at the specified key. Returns true (1) is the value was
// removed successfully.
function TCefDictionaryValueRef.Remove(const aKey: ustring): Boolean;
var
	key_str: TCefString;
begin
	key_str := TWACef.ToCefString(aKey);
	Result := PCefDictionaryValue(FData).remove(
		PCefDictionaryValue(FData),
		@key_str
	) <> 0;
end;

// Returns the value type for the specified key.
function TCefDictionaryValueRef.GetType(const aKey: ustring): TCefValueType;
var
	key_str: TCefString;
begin
	key_str := TWACef.ToCefString(aKey);
	Result := PCefDictionaryValue(FData).get_type(
		PCefDictionaryValue(FData),
		@key_str
	);
end;

// Returns the value at the specified key. For simple types the returned value
// will copy existing data and modifications to the value will not modify this
// object. For complex types (binary, dictionary and list) the returned value
// will reference existing data and modifications to the value will modify
// this object.
function TCefDictionaryValueRef.GetValue(const aKey: ustring): ICefValue;
var
	key_str: TCefString;
begin
	key_str := TWACef.ToCefString(aKey);
	Result := TCefValueRef.UnWrap(PCefDictionaryValue(FData).get_value(
		PCefDictionaryValue(FData),
		@key_str
	));
end;

// Returns the value at the specified key as type bool.
function TCefDictionaryValueRef.GetBool(const aKey: ustring): Boolean;
var
	key_str: TCefString;
begin
	key_str := TWACef.ToCefString(aKey);
	Result := PCefDictionaryValue(FData).get_bool(
		PCefDictionaryValue(FData),
		@key_str
	) <> 0;
end;

// Returns the value at the specified key as type int.
function TCefDictionaryValueRef.GetInt(const aKey: ustring): cint;
var
	key_str: TCefString;
begin
	key_str := TWACef.ToCefString(aKey);
	Result := PCefDictionaryValue(FData).get_int(
		PCefDictionaryValue(FData),
		@key_str
	);
end;

// Returns the value at the specified key as type double.
function TCefDictionaryValueRef.GetDouble(const aKey: ustring): cdouble;
var
	key_str: TCefString;
begin
	key_str := TWACef.ToCefString(aKey);
	Result := PCefDictionaryValue(FData).get_double(
		PCefDictionaryValue(FData),
		@key_str
	);
end;

// Returns the value at the specified key as type string.
// The resulting string must be freed by calling cef_string_userfree_free().
function TCefDictionaryValueRef.GetString(const aKey: ustring): ustring;
var
	key_str: TCefString;
begin
	key_str := TWACef.ToCefString(aKey);
	Result := TWACef.StringFreeAndGet(PCefDictionaryValue(FData).get_string(
		PCefDictionaryValue(FData),
		@key_str
	));
end;

// Returns the value at the specified key as type binary. The returned value
// will reference existing data.
function TCefDictionaryValueRef.GetBinary(const aKey: ustring): ICefBinaryValue;
var
	key_str: TCefString;
begin
	key_str := TWACef.ToCefString(aKey);
	Result := TCefBinaryValueRef.UnWrap(PCefDictionaryValue(FData).get_binary(
		PCefDictionaryValue(FData),
		@key_str
	));
end;

// Returns the value at the specified key as type dictionary. The returned
// value will reference existing data and modifications to the value will
// modify this object.
function TCefDictionaryValueRef.GetDictionary(const aKey: ustring): ICefDictionaryValue;
var
	key_str: TCefString;
begin
	key_str := TWACef.ToCefString(aKey);
	Result := TCefDictionaryValueRef.UnWrap(PCefDictionaryValue(FData).get_dictionary(
		PCefDictionaryValue(FData),
		@key_str
	));
end;

// Returns the value at the specified key as type list. The returned value
// will reference existing data and modifications to the value will modify
// this object.
function TCefDictionaryValueRef.GetList(const aKey: ustring): ICefListValue;
var
	key_str: TCefString;
begin
	key_str := TWACef.ToCefString(aKey);
	Result := TCefListValueRef.UnWrap(PCefDictionaryValue(FData).get_list(
		PCefDictionaryValue(FData),
		@key_str
	));
end;

// Sets the value at the specified key. Returns true (1) if the value was set
// successfully. If |value| represents simple data then the underlying data
// will be copied and modifications to |value| will not modify this object. If
// |value| represents complex data (binary, dictionary or list) then the
// underlying data will be referenced and modifications to |value| will modify
// this object.
function TCefDictionaryValueRef.SetValue(const aKey: ustring; const aValue: ICefValue): Boolean;
var
	key_str: TCefString;
begin
	key_str := TWACef.ToCefString(aKey);
	Result := PCefDictionaryValue(FData).set_value(
		PCefDictionaryValue(FData),
		@key_str,
		TWACef.GetData(aValue)
	) <> 0;
end;

// Sets the value at the specified key as type null. Returns true (1) if the
// value was set successfully.
function TCefDictionaryValueRef.SetNull(const aKey: ustring): Boolean;
var
	key_str: TCefString;
begin
	key_str := TWACef.ToCefString(aKey);
	Result := PCefDictionaryValue(FData).set_null(
		PCefDictionaryValue(FData),
		@key_str
	) <> 0;
end;

// Sets the value at the specified key as type bool. Returns true (1) if the
// value was set successfully.
function TCefDictionaryValueRef.SetBool(const aKey: ustring; aValue: Boolean): Boolean;
var
	key_str: TCefString;
begin
	key_str := TWACef.ToCefString(aKey);
	Result := PCefDictionaryValue(FData).set_bool(
		PCefDictionaryValue(FData),
		@key_str,
		Ord(aValue)
	) <> 0;
end;

// Sets the value at the specified key as type int. Returns true (1) if the
// value was set successfully.
function TCefDictionaryValueRef.SetInt(const aKey: ustring; aValue: cint): Boolean;
var
	key_str: TCefString;
begin
	key_str := TWACef.ToCefString(aKey);
	Result := PCefDictionaryValue(FData).set_int(
		PCefDictionaryValue(FData),
		@key_str,
		aValue
	) <> 0;
end;

// Sets the value at the specified key as type double. Returns true (1) if the
// value was set successfully.
function TCefDictionaryValueRef.SetDouble(const aKey: ustring; aValue: cdouble): Boolean;
var
	key_str: TCefString;
begin
	key_str := TWACef.ToCefString(aKey);
	Result := PCefDictionaryValue(FData).set_double(
		PCefDictionaryValue(FData),
		@key_str,
		aValue
	) <> 0;
end;

// Sets the value at the specified key as type string. Returns true (1) if the
// value was set successfully.
function TCefDictionaryValueRef.SetString(const aKey: ustring; const aValue: ustring): Boolean;
var
	key_str: TCefString;
	value_str: TCefString;
begin
	key_str := TWACef.ToCefString(aKey);
	value_str := TWACef.ToCefString(aValue);
	Result := PCefDictionaryValue(FData).set_string(
		PCefDictionaryValue(FData),
		@key_str,
		@value_str
	) <> 0;
end;

// Sets the value at the specified key as type binary. Returns true (1) if the
// value was set successfully. If |value| is currently owned by another object
// then the value will be copied and the |value| reference will not change.
// Otherwise, ownership will be transferred to this object and the |value|
// reference will be invalidated.
function TCefDictionaryValueRef.SetBinary(const aKey: ustring; const aValue: ICefBinaryValue): Boolean;
var
	key_str: TCefString;
begin
	key_str := TWACef.ToCefString(aKey);
	Result := PCefDictionaryValue(FData).set_binary(
		PCefDictionaryValue(FData),
		@key_str,
		TWACef.GetData(aValue)
	) <> 0;
end;

// Sets the value at the specified key as type dict. Returns true (1) if the
// value was set successfully. If |value| is currently owned by another object
// then the value will be copied and the |value| reference will not change.
// Otherwise, ownership will be transferred to this object and the |value|
// reference will be invalidated.
function TCefDictionaryValueRef.SetDictionary(const aKey: ustring; const aValue: ICefDictionaryValue): Boolean;
var
	key_str: TCefString;
begin
	key_str := TWACef.ToCefString(aKey);
	Result := PCefDictionaryValue(FData).set_dictionary(
		PCefDictionaryValue(FData),
		@key_str,
		TWACef.GetData(aValue)
	) <> 0;
end;

// Sets the value at the specified key as type list. Returns true (1) if the
// value was set successfully. If |value| is currently owned by another object
// then the value will be copied and the |value| reference will not change.
// Otherwise, ownership will be transferred to this object and the |value|
// reference will be invalidated.
function TCefDictionaryValueRef.SetList(const aKey: ustring; const aValue: ICefListValue): Boolean;
var
	key_str: TCefString;
begin
	key_str := TWACef.ToCefString(aKey);
	Result := PCefDictionaryValue(FData).set_list(
		PCefDictionaryValue(FData),
		@key_str,
		TWACef.GetData(aValue)
	) <> 0;
end;

{Public section}
class function TCefDictionaryValueRef.UnWrap(data: Pointer): ICefDictionaryValue;
begin
	if data <> nil then
		Result := Create(data) as ICefDictionaryValue
	else
		Result := nil;
end;
// Creates a new object that is not owned by any other object.
class function TCefDictionaryValueRef.New: ICefDictionaryValue;
begin
	Result := TCefDictionaryValueRef.UnWrap(cef_dictionary_value_create(
	));
end;

//..............................................................................TCefListValueRef
// Returns true (1) if this object is valid. This object may become invalid if
// the underlying data is owned by another object (e.g. list or dictionary)
// and that other object is then modified or destroyed. Do not call any other
// functions if this function returns false (0).
function TCefListValueRef.IsValid: Boolean;
begin
	Result := PCefListValue(FData).is_valid(
		PCefListValue(FData)
	) <> 0;
end;

// Returns true (1) if this object is currently owned by another object.
function TCefListValueRef.IsOwned: Boolean;
begin
	Result := PCefListValue(FData).is_owned(
		PCefListValue(FData)
	) <> 0;
end;

// Returns true (1) if the values of this object are read-only. Some APIs may
// expose read-only objects.
function TCefListValueRef.IsReadOnly: Boolean;
begin
	Result := PCefListValue(FData).is_read_only(
		PCefListValue(FData)
	) <> 0;
end;

// Returns true (1) if this object and |that| object have the same underlying
// data. If true (1) modifications to this object will also affect |that|
// object and vice-versa.
function TCefListValueRef.IsSame(const aThat: ICefListValue): Boolean;
begin
	Result := PCefListValue(FData).is_same(
		PCefListValue(FData),
		TWACef.GetData(aThat)
	) <> 0;
end;

// Returns true (1) if this object and |that| object have an equivalent
// underlying value but are not necessarily the same object.
function TCefListValueRef.IsEqual(const aThat: ICefListValue): Boolean;
begin
	Result := PCefListValue(FData).is_equal(
		PCefListValue(FData),
		TWACef.GetData(aThat)
	) <> 0;
end;

// Returns a writable copy of this object.
function TCefListValueRef.Copy: ICefListValue;
begin
	Result := TCefListValueRef.UnWrap(PCefListValue(FData).copy(
		PCefListValue(FData)
	));
end;

// Sets the number of values. If the number of values is expanded all new
// value slots will default to type null. Returns true (1) on success.
function TCefListValueRef.SetSize(aSize: csize_t): Boolean;
begin
	Result := PCefListValue(FData).set_size(
		PCefListValue(FData),
		aSize
	) <> 0;
end;

// Returns the number of values.
function TCefListValueRef.GetSize: csize_t;
begin
	Result := PCefListValue(FData).get_size(
		PCefListValue(FData)
	);
end;

// Removes all values. Returns true (1) on success.
function TCefListValueRef.Clear: Boolean;
begin
	Result := PCefListValue(FData).clear(
		PCefListValue(FData)
	) <> 0;
end;

// Removes the value at the specified index.
function TCefListValueRef.Remove(aIndex: cint): Boolean;
begin
	Result := PCefListValue(FData).remove(
		PCefListValue(FData),
		aIndex
	) <> 0;
end;

// Returns the value type at the specified index.
function TCefListValueRef.GetType(aIndex: cint): TCefValueType;
begin
	Result := PCefListValue(FData).get_type(
		PCefListValue(FData),
		aIndex
	);
end;

// Returns the value at the specified index. For simple types the returned
// value will copy existing data and modifications to the value will not
// modify this object. For complex types (binary, dictionary and list) the
// returned value will reference existing data and modifications to the value
// will modify this object.
function TCefListValueRef.GetValue(aIndex: cint): ICefValue;
begin
	Result := TCefValueRef.UnWrap(PCefListValue(FData).get_value(
		PCefListValue(FData),
		aIndex
	));
end;

// Returns the value at the specified index as type bool.
function TCefListValueRef.GetBool(aIndex: cint): Boolean;
begin
	Result := PCefListValue(FData).get_bool(
		PCefListValue(FData),
		aIndex
	) <> 0;
end;

// Returns the value at the specified index as type int.
function TCefListValueRef.GetInt(aIndex: cint): cint;
begin
	Result := PCefListValue(FData).get_int(
		PCefListValue(FData),
		aIndex
	);
end;

// Returns the value at the specified index as type double.
function TCefListValueRef.GetDouble(aIndex: cint): cdouble;
begin
	Result := PCefListValue(FData).get_double(
		PCefListValue(FData),
		aIndex
	);
end;

// Returns the value at the specified index as type string.
// The resulting string must be freed by calling cef_string_userfree_free().
function TCefListValueRef.GetString(aIndex: cint): ustring;
begin
	Result := TWACef.StringFreeAndGet(PCefListValue(FData).get_string(
		PCefListValue(FData),
		aIndex
	));
end;

// Returns the value at the specified index as type binary. The returned value
// will reference existing data.
function TCefListValueRef.GetBinary(aIndex: cint): ICefBinaryValue;
begin
	Result := TCefBinaryValueRef.UnWrap(PCefListValue(FData).get_binary(
		PCefListValue(FData),
		aIndex
	));
end;

// Returns the value at the specified index as type dictionary. The returned
// value will reference existing data and modifications to the value will
// modify this object.
function TCefListValueRef.GetDictionary(aIndex: cint): ICefDictionaryValue;
begin
	Result := TCefDictionaryValueRef.UnWrap(PCefListValue(FData).get_dictionary(
		PCefListValue(FData),
		aIndex
	));
end;

// Returns the value at the specified index as type list. The returned value
// will reference existing data and modifications to the value will modify
// this object.
function TCefListValueRef.GetList(aIndex: cint): ICefListValue;
begin
	Result := TCefListValueRef.UnWrap(PCefListValue(FData).get_list(
		PCefListValue(FData),
		aIndex
	));
end;

// Sets the value at the specified index. Returns true (1) if the value was
// set successfully. If |value| represents simple data then the underlying
// data will be copied and modifications to |value| will not modify this
// object. If |value| represents complex data (binary, dictionary or list)
// then the underlying data will be referenced and modifications to |value|
// will modify this object.
function TCefListValueRef.SetValue(aIndex: cint; const aValue: ICefValue): Boolean;
begin
	Result := PCefListValue(FData).set_value(
		PCefListValue(FData),
		aIndex,
		TWACef.GetData(aValue)
	) <> 0;
end;

// Sets the value at the specified index as type null. Returns true (1) if the
// value was set successfully.
function TCefListValueRef.SetNull(aIndex: cint): Boolean;
begin
	Result := PCefListValue(FData).set_null(
		PCefListValue(FData),
		aIndex
	) <> 0;
end;

// Sets the value at the specified index as type bool. Returns true (1) if the
// value was set successfully.
function TCefListValueRef.SetBool(aIndex: cint; aValue: Boolean): Boolean;
begin
	Result := PCefListValue(FData).set_bool(
		PCefListValue(FData),
		aIndex,
		Ord(aValue)
	) <> 0;
end;

// Sets the value at the specified index as type int. Returns true (1) if the
// value was set successfully.
function TCefListValueRef.SetInt(aIndex: cint; aValue: cint): Boolean;
begin
	Result := PCefListValue(FData).set_int(
		PCefListValue(FData),
		aIndex,
		aValue
	) <> 0;
end;

// Sets the value at the specified index as type double. Returns true (1) if
// the value was set successfully.
function TCefListValueRef.SetDouble(aIndex: cint; aValue: cdouble): Boolean;
begin
	Result := PCefListValue(FData).set_double(
		PCefListValue(FData),
		aIndex,
		aValue
	) <> 0;
end;

// Sets the value at the specified index as type string. Returns true (1) if
// the value was set successfully.
function TCefListValueRef.SetString(aIndex: cint; const aValue: ustring): Boolean;
var
	value_str: TCefString;
begin
	value_str := TWACef.ToCefString(aValue);
	Result := PCefListValue(FData).set_string(
		PCefListValue(FData),
		aIndex,
		@value_str
	) <> 0;
end;

// Sets the value at the specified index as type binary. Returns true (1) if
// the value was set successfully. If |value| is currently owned by another
// object then the value will be copied and the |value| reference will not
// change. Otherwise, ownership will be transferred to this object and the
// |value| reference will be invalidated.
function TCefListValueRef.SetBinary(aIndex: cint; const aValue: ICefBinaryValue): Boolean;
begin
	Result := PCefListValue(FData).set_binary(
		PCefListValue(FData),
		aIndex,
		TWACef.GetData(aValue)
	) <> 0;
end;

// Sets the value at the specified index as type dict. Returns true (1) if the
// value was set successfully. If |value| is currently owned by another object
// then the value will be copied and the |value| reference will not change.
// Otherwise, ownership will be transferred to this object and the |value|
// reference will be invalidated.
function TCefListValueRef.SetDictionary(aIndex: cint; const aValue: ICefDictionaryValue): Boolean;
begin
	Result := PCefListValue(FData).set_dictionary(
		PCefListValue(FData),
		aIndex,
		TWACef.GetData(aValue)
	) <> 0;
end;

// Sets the value at the specified index as type list. Returns true (1) if the
// value was set successfully. If |value| is currently owned by another object
// then the value will be copied and the |value| reference will not change.
// Otherwise, ownership will be transferred to this object and the |value|
// reference will be invalidated.
function TCefListValueRef.SetList(aIndex: cint; const aValue: ICefListValue): Boolean;
begin
	Result := PCefListValue(FData).set_list(
		PCefListValue(FData),
		aIndex,
		TWACef.GetData(aValue)
	) <> 0;
end;

{Public section}
class function TCefListValueRef.UnWrap(data: Pointer): ICefListValue;
begin
	if data <> nil then
		Result := Create(data) as ICefListValue
	else
		Result := nil;
end;
// Creates a new object that is not owned by any other object.
class function TCefListValueRef.New: ICefListValue;
begin
	Result := TCefListValueRef.UnWrap(cef_list_value_create(
	));
end;


//..............................................................................TCefWebPluginInfoRef
{Protected section}
// Returns the plugin name (i.e. Flash).
// The resulting string must be freed by calling cef_string_userfree_free().
function TCefWebPluginInfoRef.GetName: ustring;
begin
	Result := TWACef.StringFreeAndGet(PCefWebPluginInfo(FData).get_name(
		PCefWebPluginInfo(FData)
	));
end;

// Returns the plugin file path (DLL/bundle/library).
// The resulting string must be freed by calling cef_string_userfree_free().
function TCefWebPluginInfoRef.GetPath: ustring;
begin
	Result := TWACef.StringFreeAndGet(PCefWebPluginInfo(FData).get_path(
		PCefWebPluginInfo(FData)
	));
end;

// Returns the version of the plugin (may be OS-specific).
// The resulting string must be freed by calling cef_string_userfree_free().
function TCefWebPluginInfoRef.GetVersion: ustring;
begin
	Result := TWACef.StringFreeAndGet(PCefWebPluginInfo(FData).get_version(
		PCefWebPluginInfo(FData)
	));
end;

// Returns a description of the plugin from the version information.
// The resulting string must be freed by calling cef_string_userfree_free().
function TCefWebPluginInfoRef.GetDescription: ustring;
begin
	Result := TWACef.StringFreeAndGet(PCefWebPluginInfo(FData).get_description(
		PCefWebPluginInfo(FData)
	));
end;

{Public section}
class function TCefWebPluginInfoRef.UnWrap(data: Pointer): ICefWebPluginInfo;
begin
	if data <> nil then
		Result := Create(data) as ICefWebPluginInfo
	else
		Result := nil;
end;
//..............................................................................TCefWebPluginInfoVisitorRef
{Protected section}
// Method that will be called once for each plugin. |count| is the 0-based
// index for the current plugin. |total| is the total number of plugins.
// Return false (0) to stop visiting plugins. This function may never be
// called if no plugins are found.
function TCefWebPluginInfoVisitorRef.Visit(const aInfo: ICefWebPluginInfo; aCount: cint; aTotal: cint): Boolean;
begin
	Result := PCefWebPluginInfoVisitor(FData).visit(
		PCefWebPluginInfoVisitor(FData),
		TWACef.GetData(aInfo),
		aCount,
		aTotal
	) <> 0;
end;

{Public section}
class function TCefWebPluginInfoVisitorRef.UnWrap(data: Pointer): ICefWebPluginInfoVisitor;
begin
	if data <> nil then
		Result := Create(data) as ICefWebPluginInfoVisitor
	else
		Result := nil;
end;
//..............................................................................TCefWebPluginUnstableCallbackRef
{Protected section}
// Method that will be called for the requested plugin. |unstable| will be
// true (1) if the plugin has reached the crash count threshold of 3 times in
// 120 seconds.
procedure TCefWebPluginUnstableCallbackRef.IsUnstable(const aPath: ustring; aUnstable: Boolean);
var
	path_str: TCefString;
begin
	path_str := TWACef.ToCefString(aPath);
	PCefWebPluginUnstableCallback(FData).is_unstable(
		PCefWebPluginUnstableCallback(FData),
		@path_str,
		Ord(aUnstable)
	);
end;

{Public section}
class function TCefWebPluginUnstableCallbackRef.UnWrap(data: Pointer): ICefWebPluginUnstableCallback;
begin
	if data <> nil then
		Result := Create(data) as ICefWebPluginUnstableCallback
	else
		Result := nil;
end;

//..............................................................................TCefXmlReaderRef
// Moves the cursor to the next node in the document. This function must be
// called at least once to set the current cursor position. Returns true (1)
// if the cursor position was set successfully.
function TCefXmlReaderRef.MoveToNextNode: Boolean;
begin
	Result := PCefXmlReader(FData).move_to_next_node(
		PCefXmlReader(FData)
	) <> 0;
end;

// Close the document. This should be called directly to ensure that cleanup
// occurs on the correct thread.
function TCefXmlReaderRef.Close: Boolean;
begin
	Result := PCefXmlReader(FData).close(
		PCefXmlReader(FData)
	) <> 0;
end;

// Returns true (1) if an error has been reported by the XML parser.
function TCefXmlReaderRef.HasError: Boolean;
begin
	Result := PCefXmlReader(FData).has_error(
		PCefXmlReader(FData)
	) <> 0;
end;

// Returns the error string.
// The resulting string must be freed by calling cef_string_userfree_free().
function TCefXmlReaderRef.GetError: ustring;
begin
	Result := TWACef.StringFreeAndGet(PCefXmlReader(FData).get_error(
		PCefXmlReader(FData)
	));
end;

// Returns the node type.
function TCefXmlReaderRef.GetType: TCefXmlNodeType;
begin
	Result := PCefXmlReader(FData).get_type(
		PCefXmlReader(FData)
	);
end;

// Returns the node depth. Depth starts at 0 for the root node.
function TCefXmlReaderRef.GetDepth: cint;
begin
	Result := PCefXmlReader(FData).get_depth(
		PCefXmlReader(FData)
	);
end;

// Returns the local name. See http://www.w3.org/TR/REC-xml-names/#NT-
// LocalPart for additional details.
// The resulting string must be freed by calling cef_string_userfree_free().
function TCefXmlReaderRef.GetLocalName: ustring;
begin
	Result := TWACef.StringFreeAndGet(PCefXmlReader(FData).get_local_name(
		PCefXmlReader(FData)
	));
end;

// Returns the namespace prefix. See http://www.w3.org/TR/REC-xml-names/ for
// additional details.
// The resulting string must be freed by calling cef_string_userfree_free().
function TCefXmlReaderRef.GetPrefix: ustring;
begin
	Result := TWACef.StringFreeAndGet(PCefXmlReader(FData).get_prefix(
		PCefXmlReader(FData)
	));
end;

// Returns the qualified name, equal to (Prefix:)LocalName. See
// http://www.w3.org/TR/REC-xml-names/#ns-qualnames for additional details.
// The resulting string must be freed by calling cef_string_userfree_free().
function TCefXmlReaderRef.GetQualifiedName: ustring;
begin
	Result := TWACef.StringFreeAndGet(PCefXmlReader(FData).get_qualified_name(
		PCefXmlReader(FData)
	));
end;

// Returns the URI defining the namespace associated with the node. See
// http://www.w3.org/TR/REC-xml-names/ for additional details.
// The resulting string must be freed by calling cef_string_userfree_free().
function TCefXmlReaderRef.GetNamespaceUri: ustring;
begin
	Result := TWACef.StringFreeAndGet(PCefXmlReader(FData).get_namespace_uri(
		PCefXmlReader(FData)
	));
end;

// Returns the base URI of the node. See http://www.w3.org/TR/xmlbase/ for
// additional details.
// The resulting string must be freed by calling cef_string_userfree_free().
function TCefXmlReaderRef.GetBaseUri: ustring;
begin
	Result := TWACef.StringFreeAndGet(PCefXmlReader(FData).get_base_uri(
		PCefXmlReader(FData)
	));
end;

// Returns the xml:lang scope within which the node resides. See
// http://www.w3.org/TR/REC-xml/#sec-lang-tag for additional details.
// The resulting string must be freed by calling cef_string_userfree_free().
function TCefXmlReaderRef.GetXmlLang: ustring;
begin
	Result := TWACef.StringFreeAndGet(PCefXmlReader(FData).get_xml_lang(
		PCefXmlReader(FData)
	));
end;

// Returns true (1) if the node represents an NULL element. <a/> is considered
// NULL but <a></a> is not.
function TCefXmlReaderRef.IsEmptyElement: Boolean;
begin
	Result := PCefXmlReader(FData).is_empty_element(
		PCefXmlReader(FData)
	) <> 0;
end;

// Returns true (1) if the node has a text value.
function TCefXmlReaderRef.HasValue: Boolean;
begin
	Result := PCefXmlReader(FData).has_value(
		PCefXmlReader(FData)
	) <> 0;
end;

// Returns the text value.
// The resulting string must be freed by calling cef_string_userfree_free().
function TCefXmlReaderRef.GetValue: ustring;
begin
	Result := TWACef.StringFreeAndGet(PCefXmlReader(FData).get_value(
		PCefXmlReader(FData)
	));
end;

// Returns true (1) if the node has attributes.
function TCefXmlReaderRef.HasAttributes: Boolean;
begin
	Result := PCefXmlReader(FData).has_attributes(
		PCefXmlReader(FData)
	) <> 0;
end;

// Returns the number of attributes.
function TCefXmlReaderRef.GetAttributeCount: csize_t;
begin
	Result := PCefXmlReader(FData).get_attribute_count(
		PCefXmlReader(FData)
	);
end;

// Returns the value of the attribute at the specified 0-based index.
// The resulting string must be freed by calling cef_string_userfree_free().
function TCefXmlReaderRef.GetAttributeByindex(aIndex: cint): ustring;
begin
	Result := TWACef.StringFreeAndGet(PCefXmlReader(FData).get_attribute_byindex(
		PCefXmlReader(FData),
		aIndex
	));
end;

// Returns the value of the attribute with the specified qualified name.
// The resulting string must be freed by calling cef_string_userfree_free().
function TCefXmlReaderRef.GetAttributeByqname(const aQualifiedName: ustring): ustring;
var
	qualifiedName_str: TCefString;
begin
	qualifiedName_str := TWACef.ToCefString(aQualifiedName);
	Result := TWACef.StringFreeAndGet(PCefXmlReader(FData).get_attribute_byqname(
		PCefXmlReader(FData),
		@qualifiedName_str
	));
end;

// Returns the value of the attribute with the specified local name and
// namespace URI.
// The resulting string must be freed by calling cef_string_userfree_free().
function TCefXmlReaderRef.GetAttributeBylname(const aLocalName: ustring; const aNamespaceURI: ustring): ustring;
var
	localName_str: TCefString;
	namespaceURI_str: TCefString;
begin
	localName_str := TWACef.ToCefString(aLocalName);
	namespaceURI_str := TWACef.ToCefString(aNamespaceURI);
	Result := TWACef.StringFreeAndGet(PCefXmlReader(FData).get_attribute_bylname(
		PCefXmlReader(FData),
		@localName_str,
		@namespaceURI_str
	));
end;

// Returns an XML representation of the current node's children.
// The resulting string must be freed by calling cef_string_userfree_free().
function TCefXmlReaderRef.GetInnerXml: ustring;
begin
	Result := TWACef.StringFreeAndGet(PCefXmlReader(FData).get_inner_xml(
		PCefXmlReader(FData)
	));
end;

// Returns an XML representation of the current node including its children.
// The resulting string must be freed by calling cef_string_userfree_free().
function TCefXmlReaderRef.GetOuterXml: ustring;
begin
	Result := TWACef.StringFreeAndGet(PCefXmlReader(FData).get_outer_xml(
		PCefXmlReader(FData)
	));
end;

// Returns the line number for the current node.
function TCefXmlReaderRef.GetLineNumber: cint;
begin
	Result := PCefXmlReader(FData).get_line_number(
		PCefXmlReader(FData)
	);
end;

// Moves the cursor to the attribute at the specified 0-based index. Returns
// true (1) if the cursor position was set successfully.
function TCefXmlReaderRef.MoveToAttributeByindex(aIndex: cint): Boolean;
begin
	Result := PCefXmlReader(FData).move_to_attribute_byindex(
		PCefXmlReader(FData),
		aIndex
	) <> 0;
end;

// Moves the cursor to the attribute with the specified qualified name.
// Returns true (1) if the cursor position was set successfully.
function TCefXmlReaderRef.MoveToAttributeByqname(const aQualifiedName: ustring): Boolean;
var
	qualifiedName_str: TCefString;
begin
	qualifiedName_str := TWACef.ToCefString(aQualifiedName);
	Result := PCefXmlReader(FData).move_to_attribute_byqname(
		PCefXmlReader(FData),
		@qualifiedName_str
	) <> 0;
end;

// Moves the cursor to the attribute with the specified local name and
// namespace URI. Returns true (1) if the cursor position was set
// successfully.
function TCefXmlReaderRef.MoveToAttributeBylname(const aLocalName: ustring; const aNamespaceURI: ustring): Boolean;
var
	localName_str: TCefString;
	namespaceURI_str: TCefString;
begin
	localName_str := TWACef.ToCefString(aLocalName);
	namespaceURI_str := TWACef.ToCefString(aNamespaceURI);
	Result := PCefXmlReader(FData).move_to_attribute_bylname(
		PCefXmlReader(FData),
		@localName_str,
		@namespaceURI_str
	) <> 0;
end;

// Moves the cursor to the first attribute in the current element. Returns
// true (1) if the cursor position was set successfully.
function TCefXmlReaderRef.MoveToFirstAttribute: Boolean;
begin
	Result := PCefXmlReader(FData).move_to_first_attribute(
		PCefXmlReader(FData)
	) <> 0;
end;

// Moves the cursor to the next attribute in the current element. Returns true
// (1) if the cursor position was set successfully.
function TCefXmlReaderRef.MoveToNextAttribute: Boolean;
begin
	Result := PCefXmlReader(FData).move_to_next_attribute(
		PCefXmlReader(FData)
	) <> 0;
end;

// Moves the cursor back to the carrying element. Returns true (1) if the
// cursor position was set successfully.
function TCefXmlReaderRef.MoveToCarryingElement: Boolean;
begin
	Result := PCefXmlReader(FData).move_to_carrying_element(
		PCefXmlReader(FData)
	) <> 0;
end;

{Public section}
class function TCefXmlReaderRef.UnWrap(data: Pointer): ICefXmlReader;
begin
	if data <> nil then
		Result := Create(data) as ICefXmlReader
	else
		Result := nil;
end;
// Create a new cef_xml_reader_t object. The returned object's functions can
// only be called from the thread that created the object.
class function TCefXmlReaderRef.New(const aStream: ICefStreamReader; aEncodingType: TCefXmlEncodingType; const aURI: ustring): ICefXmlReader;
var
	URI_str: TCefString;
begin
	URI_str := TWACef.ToCefString(aURI);
	Result := TCefXmlReaderRef.UnWrap(cef_xml_reader_create(
		TWACef.GetData(aStream),
		aEncodingType,
		@URI_str
	));
end;


//..............................................................................TCefZipReaderRef
// Moves the cursor to the first file in the archive. Returns true (1) if the
// cursor position was set successfully.
function TCefZipReaderRef.MoveToFirstFile: Boolean;
begin
	Result := PCefZipReader(FData).move_to_first_file(
		PCefZipReader(FData)
	) <> 0;
end;

// Moves the cursor to the next file in the archive. Returns true (1) if the
// cursor position was set successfully.
function TCefZipReaderRef.MoveToNextFile: Boolean;
begin
	Result := PCefZipReader(FData).move_to_next_file(
		PCefZipReader(FData)
	) <> 0;
end;

// Moves the cursor to the specified file in the archive. If |caseSensitive|
// is true (1) then the search will be case sensitive. Returns true (1) if the
// cursor position was set successfully.
function TCefZipReaderRef.MoveToFile(const aFileName: ustring; aCaseSensitive: Boolean): Boolean;
var
	fileName_str: TCefString;
begin
	fileName_str := TWACef.ToCefString(aFileName);
	Result := PCefZipReader(FData).move_to_file(
		PCefZipReader(FData),
		@fileName_str,
		Ord(aCaseSensitive)
	) <> 0;
end;

// Closes the archive. This should be called directly to ensure that cleanup
// occurs on the correct thread.
function TCefZipReaderRef.Close: Boolean;
begin
	Result := PCefZipReader(FData).close(
		PCefZipReader(FData)
	) <> 0;
end;

// Returns the name of the file.
// The resulting string must be freed by calling cef_string_userfree_free().
function TCefZipReaderRef.GetFileName: ustring;
begin
	Result := TWACef.StringFreeAndGet(PCefZipReader(FData).get_file_name(
		PCefZipReader(FData)
	));
end;

// Returns the uncompressed size of the file.
function TCefZipReaderRef.GetFileSize: cint64;
begin
	Result := PCefZipReader(FData).get_file_size(
		PCefZipReader(FData)
	);
end;

// Returns the last modified timestamp for the file.
function TCefZipReaderRef.GetFileLastModified: ctime_t;
begin
	Result := PCefZipReader(FData).get_file_last_modified(
		PCefZipReader(FData)
	);
end;

// Opens the file for reading of uncompressed data. A read password may
// optionally be specified.
function TCefZipReaderRef.OpenFile(const aPassword: ustring): Boolean;
var
	password_str: TCefString;
begin
	password_str := TWACef.ToCefString(aPassword);
	Result := PCefZipReader(FData).open_file(
		PCefZipReader(FData),
		@password_str
	) <> 0;
end;

// Closes the file.
function TCefZipReaderRef.CloseFile: Boolean;
begin
	Result := PCefZipReader(FData).close_file(
		PCefZipReader(FData)
	) <> 0;
end;

// Read uncompressed file contents into the specified buffer. Returns < 0 if
// an error occurred, 0 if at the end of file, or the number of bytes read.
function TCefZipReaderRef.ReadFile(const aBuffer: cvoid; aBufferSize: csize_t): cint;
begin
	Result := PCefZipReader(FData).read_file(
		PCefZipReader(FData),
		aBuffer,
		aBufferSize
	);
end;

// Returns the current offset in the uncompressed file contents.
function TCefZipReaderRef.Tell: cint64;
begin
	Result := PCefZipReader(FData).tell(
		PCefZipReader(FData)
	);
end;

// Returns true (1) if at end of the file contents.
function TCefZipReaderRef.Eof: Boolean;
begin
	Result := PCefZipReader(FData).eof(
		PCefZipReader(FData)
	) <> 0;
end;

{Public section}
class function TCefZipReaderRef.UnWrap(data: Pointer): ICefZipReader;
begin
	if data <> nil then
		Result := Create(data) as ICefZipReader
	else
		Result := nil;
end;
// Create a new cef_zip_reader_t object. The returned object's functions can
// only be called from the thread that created the object.
class function TCefZipReaderRef.New(const aStream: ICefStreamReader): ICefZipReader;
begin
	Result := TCefZipReaderRef.UnWrap(cef_zip_reader_create(
		TWACef.GetData(aStream)
	));
end;

end.

