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

unit WACefOwns;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

{$I WACef.inc}

interface

uses
  WACefInterfaces,
  WACefCApi,
  WACefTypes,
  WACefCExports,
  Classes,
  SysUtils,
  {$IFNDEF FPC}
  Rtti,
  Generics.Collections,
  {$ELSE}
  ctypes,
  {$ENDIF}
  TypInfo,
  Variants;

type
  //..............................................................................cef_app_capi.h
	TCefAppOwn = class;

	//..............................................................................cef_auth_callback_capi.h
	TCefAuthCallbackOwn = class;

	//..............................................................................cef_base_capi.h
	TCefBaseOwn = class;

	//..............................................................................cef_browser_capi.h
	TCefBrowserOwn = class;
	TCefRunFileDialogCallbackOwn = class;
	TCefNavigationEntryVisitorOwn = class;
	TCefBrowserHostOwn = class;

	//..............................................................................cef_browser_process_handler_capi.h
	TCefBrowserProcessHandlerOwn = class;

	//..............................................................................cef_callback_capi.h
	TCefCallbackOwn = class;
	TCefCompletionCallbackOwn = class;

	//..............................................................................cef_client_capi.h
	TCefClientOwn = class;

	//..............................................................................cef_command_line_capi.h
	TCefCommandLineOwn = class;

	//..............................................................................cef_context_menu_handler_capi.h
	TCefContextMenuHandlerOwn = class;
	TCefContextMenuParamsOwn = class;

	//..............................................................................cef_cookie_capi.h
	TCefCookieManagerOwn = class;
	TCefCookieVisitorOwn = class;
	TCefSetCookieCallbackOwn = class;
	TCefDeleteCookiesCallbackOwn = class;

	//..............................................................................cef_dialog_handler_capi.h
	TCefFileDialogCallbackOwn = class;
	TCefDialogHandlerOwn = class;

	//..............................................................................cef_display_handler_capi.h
	TCefDisplayHandlerOwn = class;

	//..............................................................................cef_dom_capi.h
	TCefDomvisitorOwn = class;
	TCefDomdocumentOwn = class;
	TCefDomnodeOwn = class;

	//..............................................................................cef_download_handler_capi.h
	TCefBeforeDownloadCallbackOwn = class;
	TCefDownloadItemCallbackOwn = class;
	TCefDownloadHandlerOwn = class;

	//..............................................................................cef_download_item_capi.h
	TCefDownloadItemOwn = class;

	//..............................................................................cef_drag_data_capi.h
	TCefDragDataOwn = class;

	//..............................................................................cef_drag_handler_capi.h
	TCefDragHandlerOwn = class;

	//..............................................................................cef_find_handler_capi.h
	TCefFindHandlerOwn = class;

	//..............................................................................cef_focus_handler_capi.h
	TCefFocusHandlerOwn = class;

	//..............................................................................cef_frame_capi.h
	TCefFrameOwn = class;

	//..............................................................................cef_geolocation_capi.h
	TCefGetGeolocationCallbackOwn = class;

	//..............................................................................cef_geolocation_handler_capi.h
	TCefGeolocationCallbackOwn = class;
	TCefGeolocationHandlerOwn = class;

	//..............................................................................cef_jsdialog_handler_capi.h
	TCefJsdialogCallbackOwn = class;
	TCefJsdialogHandlerOwn = class;

	//..............................................................................cef_keyboard_handler_capi.h
	TCefKeyboardHandlerOwn = class;

	//..............................................................................cef_life_span_handler_capi.h
	TCefLifeSpanHandlerOwn = class;

	//..............................................................................cef_load_handler_capi.h
	TCefLoadHandlerOwn = class;

	//..............................................................................cef_menu_model_capi.h
	TCefMenuModelOwn = class;

	//..............................................................................cef_navigation_entry_capi.h
	TCefNavigationEntryOwn = class;

	//..............................................................................cef_origin_whitelist_capi.h

	//..............................................................................cef_parser_capi.h

	//..............................................................................cef_path_util_capi.h

	//..............................................................................cef_print_handler_capi.h
	TCefPrintDialogCallbackOwn = class;
	TCefPrintJobCallbackOwn = class;
	TCefPrintHandlerOwn = class;

	//..............................................................................cef_print_settings_capi.h
	TCefPrintSettingsOwn = class;

	//..............................................................................cef_process_message_capi.h
	TCefProcessMessageOwn = class;

	//..............................................................................cef_process_util_capi.h

	//..............................................................................cef_render_handler_capi.h
	TCefRenderHandlerOwn = class;

	//..............................................................................cef_render_process_handler_capi.h
	TCefRenderProcessHandlerOwn = class;

	//..............................................................................cef_request_capi.h
	TCefRequestOwn = class;
	TCefPostDataOwn = class;
	TCefPostDataElementOwn = class;

	//..............................................................................cef_request_context_capi.h
	TCefRequestContextOwn = class;

	//..............................................................................cef_request_context_handler_capi.h
	TCefRequestContextHandlerOwn = class;

	//..............................................................................cef_request_handler_capi.h
	TCefRequestCallbackOwn = class;
	TCefRequestHandlerOwn = class;

	//..............................................................................cef_resource_bundle_handler_capi.h
	TCefResourceBundleHandlerOwn = class;

	//..............................................................................cef_resource_handler_capi.h
	TCefResourceHandlerOwn = class;

	//..............................................................................cef_response_capi.h
	TCefResponseOwn = class;

	//..............................................................................cef_scheme_capi.h
	TCefSchemeRegistrarOwn = class;
	TCefSchemeHandlerFactoryOwn = class;

	//..............................................................................cef_ssl_info_capi.h
	TCefSslcertPrincipalOwn = class;
	TCefSslinfoOwn = class;

	//..............................................................................cef_stream_capi.h
	TCefReadHandlerOwn = class;
	TCefStreamReaderOwn = class;
	TCefWriteHandlerOwn = class;
	TCefStreamWriterOwn = class;

	//..............................................................................cef_string_visitor_capi.h
	TCefStringVisitorOwn = class;

	//..............................................................................cef_task_capi.h
	TCefTaskOwn = class;
	TCefTaskRunnerOwn = class;

	//..............................................................................cef_trace_capi.h
	TCefEndTracingCallbackOwn = class;

	//..............................................................................cef_urlrequest_capi.h
	TCefUrlrequestOwn = class;
	TCefUrlrequestClientOwn = class;

	//..............................................................................cef_v8_capi.h
	TCefV8contextOwn = class;
	TCefV8handlerOwn = class;
	TCefV8accessorOwn = class;
	TCefV8exceptionOwn = class;
	TCefV8valueOwn = class;
	TCefV8stackTraceOwn = class;
	TCefV8stackFrameOwn = class;

	//..............................................................................cef_values_capi.h
	TCefValueOwn = class;
	TCefBinaryValueOwn = class;
	TCefDictionaryValueOwn = class;
	TCefListValueOwn = class;

	//..............................................................................cef_web_plugin_capi.h
	TCefWebPluginInfoOwn = class;
	TCefWebPluginInfoVisitorOwn = class;
	TCefWebPluginUnstableCallbackOwn = class;

	//..............................................................................cef_xml_reader_capi.h
	TCefXmlReaderOwn = class;

	//..............................................................................cef_zip_reader_capi.h
	TCefZipReaderOwn = class;

  TCefResourceHandlerClass = class of TCefResourceHandlerOwn;


  TCefBaseOwn = class(TInterfacedObject, ICefBase)
  private
    FData: Pointer;
  public
    function Wrap: Pointer;
    constructor CreateData(size: csize_t; owned: Boolean = False); virtual;
    destructor Destroy; override;
  end;

  //..............................................................................cef_app_capi.h
	// Implement this structure to provide handler implementations. Methods will be
	// called by the process and/or thread indicated.
	TCefAppOwn = class(TCefBaseOwn, ICefApp)
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
		procedure OnBeforeCommandLineProcessing(const aProcessType: ustring; const aCommandLine: ICefCommandLine); virtual;
		// Provides an opportunity to register custom schemes. Do not keep a reference
		// to the |registrar| object. This function is called on the main thread for
		// each process and the registered schemes should be the same across all
		// processes.
		procedure OnRegisterCustomSchemes(const aRegistrar: ICefSchemeRegistrar); virtual;
		// Return the handler for resource bundle events. If
		// CefSettings.pack_loading_disabled is true (1) a handler must be returned.
		// If no handler is returned resources will be loaded from pack files. This
		// function is called by the browser and render processes on multiple threads.
		function GetResourceBundleHandler: ICefResourceBundleHandler; virtual;
		// Return the handler for functionality specific to the browser process. This
		// function is called on multiple threads in the browser process.
		function GetBrowserProcessHandler: ICefBrowserProcessHandler; virtual;
		// Return the handler for functionality specific to the render process. This
		// function is called on the render process main thread.
		function GetRenderProcessHandler: ICefRenderProcessHandler; virtual;
	public
		constructor Create; virtual;
	end;


	//..............................................................................cef_auth_callback_capi.h
	// Callback structure used for asynchronous continuation of authentication
	// requests.
	TCefAuthCallbackOwn = class(TCefBaseOwn, ICefAuthCallback)
	protected
		// Continue the authentication request.
		procedure Cont(const aUsername: ustring; const aPassword: ustring); virtual;
		// Cancel the authentication request.
		procedure Cancel; virtual;
	public
		constructor Create; virtual;
	end;

	//..............................................................................cef_browser_capi.h
	// Structure used to represent a browser window. When used in the browser
	// process the functions of this structure may be called on any thread unless
	// otherwise indicated in the comments. When used in the render process the
	// functions of this structure may only be called on the main thread.
	TCefBrowserOwn = class(TCefBaseOwn, ICefBrowser)
	protected
		// Returns the browser host object. This function can only be called in the
		// browser process.
		function GetHost: ICefBrowserHost; virtual;
		// Returns true (1) if the browser can navigate backwards.
		function CanGoBack: Boolean; virtual;
		// Navigate backwards.
		procedure GoBack; virtual;
		// Returns true (1) if the browser can navigate forwards.
		function CanGoForward: Boolean; virtual;
		// Navigate forwards.
		procedure GoForward; virtual;
		// Returns true (1) if the browser is currently loading.
		function IsLoading: Boolean; virtual;
		// Reload the current page.
		procedure Reload; virtual;
		// Reload the current page ignoring any cached data.
		procedure ReloadIgnoreCache; virtual;
		// Stop loading the page.
		procedure StopLoad; virtual;
		// Returns the globally unique identifier for this browser.
		function GetIdentifier: cint; virtual;
		// Returns true (1) if this object is pointing to the same handle as |that|
		// object.
		function IsSame(const aThat: ICefBrowser): Boolean; virtual;
		// Returns true (1) if the window is a popup window.
		function IsPopup: Boolean; virtual;
		// Returns true (1) if a document has been loaded in the browser.
		function HasDocument: Boolean; virtual;
		// Returns the main (top-level) frame for the browser window.
		function GetMainFrame: ICefFrame; virtual;
		// Returns the focused frame for the browser window.
		function GetFocusedFrame: ICefFrame; virtual;
		// Returns the frame with the specified identifier, or NULL if not found.
		function GetFrameByident(aIdentifier: cint64): ICefFrame; virtual;
		// Returns the frame with the specified name, or NULL if not found.
		function GetFrame(const aName: ustring): ICefFrame; virtual;
		// Returns the number of frames that currently exist.
		function GetFrameCount: csize_t; virtual;
		// Returns the identifiers of all existing frames.
		procedure GetFrameIdentifiers(var aIdentifiersCount: csize_t; var aIdentifiers: cint64); virtual;
		// Returns the names of all existing frames.
		procedure GetFrameNames(aNames: TStrings); virtual;
		//
		// Send a message to the specified |target_process|. Returns true (1) if the
		// message was sent successfully.
		function SendProcessMessage(aTargetProcess: TCefProcessId; const aMessage: ICefProcessMessage): Boolean; virtual;
	public
		constructor Create; virtual;
	end;

	// Callback structure for cef_browser_host_t::RunFileDialog. The functions of
	// this structure will be called on the browser process UI thread.
	TCefRunFileDialogCallbackOwn = class(TCefBaseOwn, ICefRunFileDialogCallback)
	protected
		// Called asynchronously after the file dialog is dismissed.
		// |selected_accept_filter| is the 0-based index of the value selected from
		// the accept filters array passed to cef_browser_host_t::RunFileDialog.
		// |file_paths| will be a single value or a list of values depending on the
		// dialog mode. If the selection was cancelled |file_paths| will be NULL.
		procedure OnFileDialogDismissed(aSelectedAcceptFilter: cint; aFilePaths: TStrings); virtual;
	public
		constructor Create; virtual;
	end;

	// Callback structure for cef_browser_host_t::GetNavigationEntries. The
	// functions of this structure will be called on the browser process UI thread.
	TCefNavigationEntryVisitorOwn = class(TCefBaseOwn, ICefNavigationEntryVisitor)
	protected
		// Method that will be executed. Do not keep a reference to |entry| outside of
		// this callback. Return true (1) to continue visiting entries or false (0) to
		// stop. |current| is true (1) if this entry is the currently loaded
		// navigation entry. |index| is the 0-based index of this entry and |total| is
		// the total number of entries.
		function Visit(const aEntry: ICefNavigationEntry; aCurrent: Boolean; aIndex: cint; aTotal: cint): Boolean; virtual;
	public
		constructor Create; virtual;
	end;

	// Structure used to represent the browser process aspects of a browser window.
	// The functions of this structure can only be called in the browser process.
	// They may be called on any thread in that process unless otherwise indicated
	// in the comments.
	TCefBrowserHostOwn = class(TCefBaseOwn, ICefBrowserHost)
	protected
		// Returns the hosted browser object.
		function GetBrowser: ICefBrowser; virtual;
		// Request that the browser close. The JavaScript 'onbeforeunload' event will
		// be fired. If |force_close| is false (0) the event handler, if any, will be
		// allowed to prompt the user and the user can optionally cancel the close. If
		// |force_close| is true (1) the prompt will not be displayed and the close
		// will proceed. Results in a call to cef_life_span_handler_t::do_close() if
		// the event handler allows the close or if |force_close| is true (1). See
		// cef_life_span_handler_t::do_close() documentation for additional usage
		// information.
		procedure CloseBrowser(aForceClose: Boolean); virtual;
		// Set whether the browser is focused.
		procedure SetFocus(aFocus: Boolean); virtual;
		// Set whether the window containing the browser is visible
		// (minimized/unminimized, app hidden/unhidden, etc). Only used on Mac OS X.
		procedure SetWindowVisibility(aVisible: Boolean); virtual;
		// Retrieve the window handle for this browser.
		function GetWindowHandle: TCefWindowHandle; virtual;
		// Retrieve the window handle of the browser that opened this browser. Will
		// return NULL for non-popup windows. This function can be used in combination
		// with custom handling of modal windows.
		function GetOpenerWindowHandle: TCefWindowHandle; virtual;
		// Returns the client for this browser.
		function GetClient: ICefClient; virtual;
		// Returns the request context for this browser.
		function GetRequestContext: ICefRequestContext; virtual;
		// Get the current zoom level. The default zoom level is 0.0. This function
		// can only be called on the UI thread.
		function GetZoomLevel: cdouble; virtual;
		// Change the zoom level to the specified value. Specify 0.0 to reset the zoom
		// level. If called on the UI thread the change will be applied immediately.
		// Otherwise, the change will be applied asynchronously on the UI thread.
		procedure SetZoomLevel(aZoomLevel: cdouble); virtual;
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
		procedure RunFileDialog(aMode: TCefFileDialogMode; const aTitle: ustring; const aDefaultFilePath: ustring; aAcceptFilters: TStrings; aSelectedAcceptFilter: cint; const aCallback: ICefRunFileDialogCallback); virtual;
		// Download the file at |url| using cef_download_handler_t.
		procedure StartDownload(const aUrl: ustring); virtual;
		// Print the current browser contents.
		procedure Print; virtual;
		// Search for |searchText|. |identifier| can be used to have multiple searches
		// running simultaniously. |forward| indicates whether to search forward or
		// backward within the page. |matchCase| indicates whether the search should
		// be case-sensitive. |findNext| indicates whether this is the first request
		// or a follow-up. The cef_find_handler_t instance, if any, returned via
		// cef_client_t::GetFindHandler will be called to report find results.
		procedure Find(aIdentifier: cint; const aSearchText: ustring; aForward: Boolean; aMatchCase: Boolean; aFindNext: Boolean); virtual;
		// Cancel all searches that are currently going on.
		procedure StopFinding(aClearSelection: Boolean); virtual;
		// Open developer tools in its own window. If |inspect_element_at| is non-
		// NULL the element at the specified (x,y) location will be inspected.
		procedure ShowDevTools(var aWindowInfo: TCefWindowInfo; const aClient: ICefClient; var aSettings: TCefBrowserSettings; aInspectElementAt: TCefPoint); overload; virtual;
    procedure ShowDevTools(var aWindowInfo: TCefWindowInfo; const aClient: ICefClient; var aSettings: TCefBrowserSettings); overload; virtual;
		// Explicitly close the developer tools window if one exists for this browser
		// instance.
		procedure CloseDevTools; virtual;
		// Retrieve a snapshot of current navigation entries as values sent to the
		// specified visitor. If |current_only| is true (1) only the current
		// navigation entry will be sent, otherwise all navigation entries will be
		// sent.
		procedure GetNavigationEntries(const aVisitor: ICefNavigationEntryVisitor; aCurrentOnly: Boolean); virtual;
		// Set whether mouse cursor change is disabled.
		procedure SetMouseCursorChangeDisabled(aDisabled: Boolean); virtual;
		// Returns true (1) if mouse cursor change is disabled.
		function IsMouseCursorChangeDisabled: Boolean; virtual;
		// If a misspelled word is currently selected in an editable node calling this
		// function will replace it with the specified |word|.
		procedure ReplaceMisspelling(const aWord: ustring); virtual;
		// Add the specified |word| to the spelling dictionary.
		procedure AddWordToDictionary(const aWord: ustring); virtual;
		// Returns true (1) if window rendering is disabled.
		function IsWindowRenderingDisabled: Boolean; virtual;
		// Notify the browser that the widget has been resized. The browser will first
		// call cef_render_handler_t::GetViewRect to get the new size and then call
		// cef_render_handler_t::OnPaint asynchronously with the updated regions. This
		// function is only used when window rendering is disabled.
		procedure WasResized; virtual;
		// Notify the browser that it has been hidden or shown. Layouting and
		// cef_render_handler_t::OnPaint notification will stop when the browser is
		// hidden. This function is only used when window rendering is disabled.
		procedure WasHidden(aHidden: Boolean); virtual;
		// Send a notification to the browser that the screen info has changed. The
		// browser will then call cef_render_handler_t::GetScreenInfo to update the
		// screen information with the new values. This simulates moving the webview
		// window from one display to another, or changing the properties of the
		// current display. This function is only used when window rendering is
		// disabled.
		procedure NotifyScreenInfoChanged; virtual;
		// Invalidate the view. The browser will call cef_render_handler_t::OnPaint
		// asynchronously. This function is only used when window rendering is
		// disabled.
		procedure Invalidate(const aType: TCefPaintElementType); virtual;
		// Send a key event to the browser.
		procedure SendKeyEvent(const aEvent: TCefKeyEvent); virtual;
		// Send a mouse click event to the browser. The |x| and |y| coordinates are
		// relative to the upper-left corner of the view.
		procedure SendMouseClickEvent(const aEvent: TCefMouseEvent; aType: TCefMouseButtonType; aMouseUp: Boolean; aClickCount: cint); virtual;
		// Send a mouse move event to the browser. The |x| and |y| coordinates are
		// relative to the upper-left corner of the view.
		procedure SendMouseMoveEvent(const aEvent: TCefMouseEvent; aMouseLeave: Boolean); virtual;
		// Send a mouse wheel event to the browser. The |x| and |y| coordinates are
		// relative to the upper-left corner of the view. The |deltaX| and |deltaY|
		// values represent the movement delta in the X and Y directions respectively.
		// In order to scroll inside select popups with window rendering disabled
		// cef_render_handler_t::GetScreenPoint should be implemented properly.
		procedure SendMouseWheelEvent(const aEvent: TCefMouseEvent; aDeltaX: cint; aDeltaY: cint); virtual;
		// Send a focus event to the browser.
		procedure SendFocusEvent(aSetFocus: Boolean); virtual;
		// Send a capture lost event to the browser.
		procedure SendCaptureLostEvent; virtual;
		// Notify the browser that the window hosting it is about to be moved or
		// resized. This function is only used on Windows and Linux.
		procedure NotifyMoveOrResizeStarted; virtual;
		// Get the NSTextInputContext implementation for enabling IME on Mac when
		// window rendering is disabled.
		function GetNstextInputContext: TCefTextInputContext; virtual;
		// Handles a keyDown event prior to passing it through the NSTextInputClient
		// machinery.
		procedure HandleKeyEventBeforeTextInputClient(aKeyEvent: TCefEventHandle); virtual;
		// Performs any additional actions after NSTextInputClient handles the event.
		procedure HandleKeyEventAfterTextInputClient(aKeyEvent: TCefEventHandle); virtual;
		// Call this function when the user drags the mouse into the web view (before
		// calling DragTargetDragOver/DragTargetLeave/DragTargetDrop). |drag_data|
		// should not contain file contents as this type of data is not allowed to be
		// dragged into the web view. File contents can be removed using
		// cef_drag_data_t::ResetFileContents (for example, if |drag_data| comes from
		// cef_render_handler_t::StartDragging). This function is only used when
		// window rendering is disabled.
		procedure DragTargetDragEnter(const aDragData: ICefDragData; const aEvent: TCefMouseEvent; aAllowedOps: TCefDragOperationsMask); virtual;
		// Call this function each time the mouse is moved across the web view during
		// a drag operation (after calling DragTargetDragEnter and before calling
		// DragTargetDragLeave/DragTargetDrop). This function is only used when window
		// rendering is disabled.
		procedure DragTargetDragOver(const aEvent: TCefMouseEvent; aAllowedOps: TCefDragOperationsMask); virtual;
		// Call this function when the user drags the mouse out of the web view (after
		// calling DragTargetDragEnter). This function is only used when window
		// rendering is disabled.
		procedure DragTargetDragLeave; virtual;
		// Call this function when the user completes the drag operation by dropping
		// the object onto the web view (after calling DragTargetDragEnter). The
		// object being dropped is |drag_data|, given as an argument to the previous
		// DragTargetDragEnter call. This function is only used when window rendering
		// is disabled.
		procedure DragTargetDrop(const aEvent: TCefMouseEvent); virtual;
		// Call this function when the drag operation started by a
		// cef_render_handler_t::StartDragging call has ended either in a drop or by
		// being cancelled. |x| and |y| are mouse coordinates relative to the upper-
		// left corner of the view. If the web view is both the drag source and the
		// drag target then all DragTarget* functions should be called before
		// DragSource* mthods. This function is only used when window rendering is
		// disabled.
		procedure DragSourceEndedAt(aX: cint; aY: cint; aOp: TCefDragOperationsMask); virtual;
		// Call this function when the drag operation started by a
		// cef_render_handler_t::StartDragging call has completed. This function may
		// be called immediately without first calling DragSourceEndedAt to cancel a
		// drag operation. If the web view is both the drag source and the drag target
		// then all DragTarget* functions should be called before DragSource* mthods.
		// This function is only used when window rendering is disabled.
		procedure DragSourceSystemDragEnded; virtual;
	public
		constructor Create; virtual;
	end;


	//..............................................................................cef_browser_process_handler_capi.h
	// Structure used to implement browser process callbacks. The functions of this
	// structure will be called on the browser process main thread unless otherwise
	// indicated.
	TCefBrowserProcessHandlerOwn = class(TCefBaseOwn, ICefBrowserProcessHandler)
	protected
		// Called on the browser process UI thread immediately after the CEF context
		// has been initialized.
		procedure OnContextInitialized; virtual;
		// Called before a child process is launched. Will be called on the browser
		// process UI thread when launching a render process and on the browser
		// process IO thread when launching a GPU or plugin process. Provides an
		// opportunity to modify the child process command line. Do not keep a
		// reference to |command_line| outside of this function.
		procedure OnBeforeChildProcessLaunch(const aCommandLine: ICefCommandLine); virtual;
		// Called on the browser process IO thread after the main thread has been
		// created for a new render process. Provides an opportunity to specify extra
		// information that will be passed to
		// cef_render_process_handler_t::on_render_thread_created() in the render
		// process. Do not keep a reference to |extra_info| outside of this function.
		procedure OnRenderProcessThreadCreated(const aExtraInfo: ICefListValue); virtual;
		// Return the handler for printing on Linux. If a print handler is not
		// provided then printing will not be supported on the Linux platform.
		function GetPrintHandler: ICefPrintHandler; virtual;
	public
		constructor Create; virtual;
	end;


	//..............................................................................cef_callback_capi.h
	// Generic callback structure used for asynchronous continuation.
	TCefCallbackOwn = class(TCefBaseOwn, ICefCallback)
	protected
		// Continue processing.
		procedure Cont; virtual;
		// Cancel processing.
		procedure Cancel; virtual;
	public
		constructor Create; virtual;
	end;

	// Generic callback structure used for asynchronous completion.
	TCefCompletionCallbackOwn = class(TCefBaseOwn, ICefCompletionCallback)
	protected
		// Method that will be called once the task is complete.
		procedure OnComplete; virtual;
	public
		constructor Create; virtual;
	end;


	//..............................................................................cef_client_capi.h
	// Implement this structure to provide handler implementations.
	TCefClientOwn = class(TCefBaseOwn, ICefClient)
	protected
		// Return the handler for context menus. If no handler is provided the default
		// implementation will be used.
		function GetContextMenuHandler: ICefContextMenuHandler; virtual;
		// Return the handler for dialogs. If no handler is provided the default
		// implementation will be used.
		function GetDialogHandler: ICefDialogHandler; virtual;
		// Return the handler for browser display state events.
		function GetDisplayHandler: ICefDisplayHandler; virtual;
		// Return the handler for download events. If no handler is returned downloads
		// will not be allowed.
		function GetDownloadHandler: ICefDownloadHandler; virtual;
		// Return the handler for drag events.
		function GetDragHandler: ICefDragHandler; virtual;
		// Return the handler for find result events.
		function GetFindHandler: ICefFindHandler; virtual;
		// Return the handler for focus events.
		function GetFocusHandler: ICefFocusHandler; virtual;
		// Return the handler for geolocation permissions requests. If no handler is
		// provided geolocation access will be denied by default.
		function GetGeolocationHandler: ICefGeolocationHandler; virtual;
		// Return the handler for JavaScript dialogs. If no handler is provided the
		// default implementation will be used.
		function GetJsdialogHandler: ICefJsdialogHandler; virtual;
		// Return the handler for keyboard events.
		function GetKeyboardHandler: ICefKeyboardHandler; virtual;
		// Return the handler for browser life span events.
		function GetLifeSpanHandler: ICefLifeSpanHandler; virtual;
		// Return the handler for browser load status events.
		function GetLoadHandler: ICefLoadHandler; virtual;
		// Return the handler for off-screen rendering events.
		function GetRenderHandler: ICefRenderHandler; virtual;
		// Return the handler for browser request events.
		function GetRequestHandler: ICefRequestHandler; virtual;
		// Called when a new message is received from a different process. Return true
		// (1) if the message was handled or false (0) otherwise. Do not keep a
		// reference to or attempt to access the message outside of this callback.
		function OnProcessMessageReceived(const aBrowser: ICefBrowser; aSourceProcess: TCefProcessId; const aMessage: ICefProcessMessage): Boolean; virtual;
	public
		constructor Create; virtual;
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
	TCefCommandLineOwn = class(TCefBaseOwn, ICefCommandLine)
	protected
		// Returns true (1) if this object is valid. Do not call any other functions
		// if this function returns false (0).
		function IsValid: Boolean; virtual;
		// Returns true (1) if the values of this object are read-only. Some APIs may
		// expose read-only objects.
		function IsReadOnly: Boolean; virtual;
		// Returns a writable copy of this object.
		function Copy: ICefCommandLine; virtual;
		// Initialize the command line with the specified |argc| and |argv| values.
		// The first argument must be the name of the program. This function is only
		// supported on non-Windows platforms.
		procedure InitFromArgv(aArgc: cint; const aArgv: PPAnsiChar); virtual;
		// Initialize the command line with the string returned by calling
		// GetCommandLineW(). This function is only supported on Windows.
		procedure InitFromString(const aCommandLine: ustring); virtual;
		// Reset the command-line switches and arguments but leave the program
		// component unchanged.
		procedure Reset; virtual;
		// Retrieve the original command line string as a vector of strings. The argv
		// array: { program, [(--|-|/)switch[=value]]*, [--], [argument]* }
		procedure GetArgv(aArgv: TStrings); virtual;
		// Constructs and returns the represented command line string. Use this
		// function cautiously because quoting behavior is unclear.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetCommandLineString: ustring; virtual;
		// Get the program part of the command line string (the first item).
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetProgram: ustring; virtual;
		// Set the program part of the command line string (the first item).
		procedure SetProgram(const aProgram: ustring); virtual;
		// Returns true (1) if the command line has switches.
		function HasSwitches: Boolean; virtual;
		// Returns true (1) if the command line contains the given switch.
		function HasSwitch(const aName: ustring): Boolean; virtual;
		// Returns the value associated with the given switch. If the switch has no
		// value or isn't present this function returns the NULL string.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetSwitchValue(const aName: ustring): ustring; virtual;
		// Returns the map of switch names and values. If a switch has no value an
		// NULL string is returned.
		procedure GetSwitches(aSwitches: TStrings); virtual;
		// Add a switch to the end of the command line. If the switch has no value
		// pass an NULL value string.
		procedure AppendSwitch(const aName: ustring); virtual;
		// Add a switch with the specified value to the end of the command line.
		procedure AppendSwitchWithValue(const aName: ustring; const aValue: ustring); virtual;
		// True if there are remaining command line arguments.
		function HasArguments: Boolean; virtual;
		// Get the remaining command line arguments.
		procedure GetArguments(aArguments: TStrings); virtual;
		// Add an argument to the end of the command line.
		procedure AppendArgument(const aArgument: ustring); virtual;
		// Insert a command before the current command. Common for debuggers, like
		// "valgrind" or "gdb --args".
		procedure PrependWrapper(const aWrapper: ustring); virtual;
	public
		constructor Create; virtual;
	end;


	//..............................................................................cef_context_menu_handler_capi.h
	// Implement this structure to handle context menu events. The functions of this
	// structure will be called on the UI thread.
	TCefContextMenuHandlerOwn = class(TCefBaseOwn, ICefContextMenuHandler)
	protected
		// Called before a context menu is displayed. |params| provides information
		// about the context menu state. |model| initially contains the default
		// context menu. The |model| can be cleared to show no context menu or
		// modified to show a custom menu. Do not keep references to |params| or
		// |model| outside of this callback.
		procedure OnBeforeContextMenu(const aBrowser: ICefBrowser; const aFrame: ICefFrame; const aParams: ICefContextMenuParams; const aModel: ICefMenuModel); virtual;
		// Called to execute a command selected from the context menu. Return true (1)
		// if the command was handled or false (0) for the default implementation. See
		// cef_menu_id_t for the command ids that have default implementations. All
		// user-defined command ids should be between MENU_ID_USER_FIRST and
		// MENU_ID_USER_LAST. |params| will have the same values as what was passed to
		// on_before_context_menu(). Do not keep a reference to |params| outside of
		// this callback.
		function OnContextMenuCommand(const aBrowser: ICefBrowser; const aFrame: ICefFrame; const aParams: ICefContextMenuParams; aCommandId: cint; aEventFlags: TCefEventFlags): Boolean; virtual;
		// Called when the context menu is dismissed irregardless of whether the menu
		// was NULL or a command was selected.
		procedure OnContextMenuDismissed(const aBrowser: ICefBrowser; const aFrame: ICefFrame); virtual;
	public
		constructor Create; virtual;
	end;

	// Provides information about the context menu state. The ethods of this
	// structure can only be accessed on browser process the UI thread.
	TCefContextMenuParamsOwn = class(TCefBaseOwn, ICefContextMenuParams)
	protected
		// Returns the X coordinate of the mouse where the context menu was invoked.
		// Coords are relative to the associated RenderView's origin.
		function GetXcoord: cint; virtual;
		// Returns the Y coordinate of the mouse where the context menu was invoked.
		// Coords are relative to the associated RenderView's origin.
		function GetYcoord: cint; virtual;
		// Returns flags representing the type of node that the context menu was
		// invoked on.
		function GetTypeFlags: TCefContextMenuTypeFlags; virtual;
		// Returns the URL of the link, if any, that encloses the node that the
		// context menu was invoked on.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetLinkUrl: ustring; virtual;
		// Returns the link URL, if any, to be used ONLY for "copy link address". We
		// don't validate this field in the frontend process.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetUnfilteredLinkUrl: ustring; virtual;
		// Returns the source URL, if any, for the element that the context menu was
		// invoked on. Example of elements with source URLs are img, audio, and video.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetSourceUrl: ustring; virtual;
		// Returns true (1) if the context menu was invoked on an image which has non-
		// NULL contents.
		function HasImageContents: Boolean; virtual;
		// Returns the URL of the top level page that the context menu was invoked on.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetPageUrl: ustring; virtual;
		// Returns the URL of the subframe that the context menu was invoked on.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetFrameUrl: ustring; virtual;
		// Returns the character encoding of the subframe that the context menu was
		// invoked on.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetFrameCharset: ustring; virtual;
		// Returns the type of context node that the context menu was invoked on.
		function GetMediaType: TCefContextMenuMediaType; virtual;
		// Returns flags representing the actions supported by the media element, if
		// any, that the context menu was invoked on.
		function GetMediaStateFlags: TCefContextMenuMediaStateFlags; virtual;
		// Returns the text of the selection, if any, that the context menu was
		// invoked on.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetSelectionText: ustring; virtual;
		// Returns the text of the misspelled word, if any, that the context menu was
		// invoked on.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetMisspelledWord: ustring; virtual;
		// Returns true (1) if suggestions exist, false (0) otherwise. Fills in
		// |suggestions| from the spell check service for the misspelled word if there
		// is one.
		function GetDictionarySuggestions(aSuggestions: TStrings): Boolean; virtual;
		// Returns true (1) if the context menu was invoked on an editable node.
		function IsEditable: Boolean; virtual;
		// Returns true (1) if the context menu was invoked on an editable node where
		// spell-check is enabled.
		function IsSpellCheckEnabled: Boolean; virtual;
		// Returns flags representing the actions supported by the editable node, if
		// any, that the context menu was invoked on.
		function GetEditStateFlags: TCefContextMenuEditStateFlags; virtual;
	public
		constructor Create; virtual;
	end;


	//..............................................................................cef_cookie_capi.h
	// Structure used for managing cookies. The functions of this structure may be
	// called on any thread unless otherwise indicated.
	TCefCookieManagerOwn = class(TCefBaseOwn, ICefCookieManager)
	protected
		// Set the schemes supported by this manager. By default only "http" and
		// "https" schemes are supported. If |callback| is non-NULL it will be
		// executed asnychronously on the IO thread after the change has been applied.
		// Must be called before any cookies are accessed.
		procedure SetSupportedSchemes(aSchemes: TStrings; const aCallback: ICefCompletionCallback); virtual;
		// Visit all cookies on the IO thread. The returned cookies are ordered by
		// longest path, then by earliest creation date. Returns false (0) if cookies
		// cannot be accessed.
		function VisitAllCookies(const aVisitor: ICefCookieVisitor): Boolean; virtual;
    function VisitAllCookiesProc(const aVisitor: TCefCookieVisitorProc): Boolean; virtual;
		// Visit a subset of cookies on the IO thread. The results are filtered by the
		// given url scheme, host, domain and path. If |includeHttpOnly| is true (1)
		// HTTP-only cookies will also be included in the results. The returned
		// cookies are ordered by longest path, then by earliest creation date.
		// Returns false (0) if cookies cannot be accessed.
		function VisitUrlCookies(const aUrl: ustring; aIncludeHttpOnly: Boolean; const aVisitor: ICefCookieVisitor): Boolean; virtual;
    function VisitUrlCookiesProc(const aUrl: ustring; aIncludeHttpOnly: Boolean; const aVisitor: TCefCookieVisitorProc): Boolean; virtual;
		// Sets a cookie given a valid URL and explicit user-provided cookie
		// attributes. This function expects each attribute to be well-formed. It will
		// check for disallowed characters (e.g. the ';' character is disallowed
		// within the cookie value attribute) and fail without setting the cookie if
		// such characters are found. If |callback| is non-NULL it will be executed
		// asnychronously on the IO thread after the cookie has been set. Returns
		// false (0) if an invalid URL is specified or if cookies cannot be accessed.
		function SetCookie(const aUrl: ustring; const aCookie: TCefCookie; const aCallback: ICefSetCookieCallback): Boolean; virtual;
		// Delete all cookies that match the specified parameters. If both |url| and
		// |cookie_name| values are specified all host and domain cookies matching
		// both will be deleted. If only |url| is specified all host cookies (but not
		// domain cookies) irrespective of path will be deleted. If |url| is NULL all
		// cookies for all hosts and domains will be deleted. If |callback| is non-
		// NULL it will be executed asnychronously on the IO thread after the cookies
		// have been deleted. Returns false (0) if a non-NULL invalid URL is specified
		// or if cookies cannot be accessed. Cookies can alternately be deleted using
		// the Visit*Cookies() functions.
		function DeleteCookies(const aUrl: ustring; const aCookieName: ustring; const aCallback: ICefDeleteCookiesCallback): Boolean; virtual;
		// Sets the directory path that will be used for storing cookie data. If
		// |path| is NULL data will be stored in memory only. Otherwise, data will be
		// stored at the specified |path|. To persist session cookies (cookies without
		// an expiry date or validity interval) set |persist_session_cookies| to true
		// (1). Session cookies are generally intended to be transient and most Web
		// browsers do not persist them. If |callback| is non-NULL it will be executed
		// asnychronously on the IO thread after the manager's storage has been
		// initialized. Returns false (0) if cookies cannot be accessed.
		function SetStoragePath(const aPath: ustring; aPersistSessionCookies: Boolean; const aCallback: ICefCompletionCallback): Boolean; virtual;
		// Flush the backing store (if any) to disk. If |callback| is non-NULL it will
		// be executed asnychronously on the IO thread after the flush is complete.
		// Returns false (0) if cookies cannot be accessed.
		function FlushStore(const aCallback: ICefCompletionCallback): Boolean; virtual;
	public
		constructor Create; virtual;
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
	TCefCookieVisitorOwn = class(TCefBaseOwn, ICefCookieVisitor)
	protected
		// Method that will be called once for each cookie. |count| is the 0-based
		// index for the current cookie. |total| is the total number of cookies. Set
		// |deleteCookie| to true (1) to delete the cookie currently being visited.
		// Return false (0) to stop visiting cookies. This function may never be
		// called if no cookies are found.
		function Visit(const aCookie: TWACefCookie; aCount: cint; aTotal: cint; out aDeleteCookie: Boolean): Boolean; virtual;
	public
		constructor Create; virtual;
	end;

	// Structure to implement to be notified of asynchronous completion via
	// cef_cookie_manager_t::set_cookie().
	TCefSetCookieCallbackOwn = class(TCefBaseOwn, ICefSetCookieCallback)
	protected
		// Method that will be called upon completion. |success| will be true (1) if
		// the cookie was set successfully.
		procedure OnComplete(aSuccess: Boolean); virtual;
	public
		constructor Create; virtual;
	end;

	// Structure to implement to be notified of asynchronous completion via
	// cef_cookie_manager_t::delete_cookies().
	TCefDeleteCookiesCallbackOwn = class(TCefBaseOwn, ICefDeleteCookiesCallback)
	protected
		// Method that will be called upon completion. |num_deleted| will be the
		// number of cookies that were deleted or -1 if unknown.
		procedure OnComplete(aNumDeleted: cint); virtual;
	public
		constructor Create; virtual;
	end;


	//..............................................................................cef_dialog_handler_capi.h
	// Callback structure for asynchronous continuation of file dialog requests.
	TCefFileDialogCallbackOwn = class(TCefBaseOwn, ICefFileDialogCallback)
	protected
		// Continue the file selection. |selected_accept_filter| should be the 0-based
		// index of the value selected from the accept filters array passed to
		// cef_dialog_handler_t::OnFileDialog. |file_paths| should be a single value
		// or a list of values depending on the dialog mode. An NULL |file_paths|
		// value is treated the same as calling cancel().
		procedure Cont(aSelectedAcceptFilter: cint; aFilePaths: TStrings); virtual;
		// Cancel the file selection.
		procedure Cancel; virtual;
	public
		constructor Create; virtual;
	end;

	// Implement this structure to handle dialog events. The functions of this
	// structure will be called on the browser process UI thread.
	TCefDialogHandlerOwn = class(TCefBaseOwn, ICefDialogHandler)
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
		function OnFileDialog(const aBrowser: ICefBrowser; aMode: TCefFileDialogMode; const aTitle: ustring; const aDefaultFilePath: ustring; aAcceptFilters: TStrings; aSelectedAcceptFilter: cint; const aCallback: ICefFileDialogCallback): Boolean; virtual;
	public
		constructor Create; virtual;
	end;


	//..............................................................................cef_display_handler_capi.h
	// Implement this structure to handle events related to browser display state.
	// The functions of this structure will be called on the UI thread.
	TCefDisplayHandlerOwn = class(TCefBaseOwn, ICefDisplayHandler)
	protected
		// Called when a frame's address has changed.
		procedure OnAddressChange(const aBrowser: ICefBrowser; const aFrame: ICefFrame; const aUrl: ustring); virtual;
		// Called when the page title changes.
		procedure OnTitleChange(const aBrowser: ICefBrowser; const aTitle: ustring); virtual;
		// Called when the page icon changes.
		procedure OnFaviconUrlchange(const aBrowser: ICefBrowser; aIconUrls: TStrings); virtual;
		// Called when the browser is about to display a tooltip. |text| contains the
		// text that will be displayed in the tooltip. To handle the display of the
		// tooltip yourself return true (1). Otherwise, you can optionally modify
		// |text| and then return false (0) to allow the browser to display the
		// tooltip. When window rendering is disabled the application is responsible
		// for drawing tooltips and the return value is ignored.
		function OnTooltip(const aBrowser: ICefBrowser; var aText: ustring): Boolean; virtual;
		// Called when the browser receives a status message. |value| contains the
		// text that will be displayed in the status message.
		procedure OnStatusMessage(const aBrowser: ICefBrowser; const aValue: ustring); virtual;
		// Called to display a console message. Return true (1) to stop the message
		// from being output to the console.
		function OnConsoleMessage(const aBrowser: ICefBrowser; const aMessage: ustring; const aSource: ustring; aLine: cint): Boolean; virtual;
	public
		constructor Create; virtual;
	end;


	//..............................................................................cef_dom_capi.h
	// Structure to implement for visiting the DOM. The functions of this structure
	// will be called on the render process main thread.
	TCefDomvisitorOwn = class(TCefBaseOwn, ICefDomvisitor)
	protected
		// Method executed for visiting the DOM. The document object passed to this
		// function represents a snapshot of the DOM at the time this function is
		// executed. DOM objects are only valid for the scope of this function. Do not
		// keep references to or attempt to access any DOM objects outside the scope
		// of this function.
		procedure Visit(const aDocument: ICefDomdocument); virtual;
	public
		constructor Create; virtual;
	end;

	// Structure used to represent a DOM document. The functions of this structure
	// should only be called on the render process main thread thread.
	TCefDomdocumentOwn = class(TCefBaseOwn, ICefDomdocument)
	protected
		// Returns the document type.
		function GetType: TCefDomDocumentType; virtual;
		// Returns the root document node.
		function GetDocument: ICefDomnode; virtual;
		// Returns the BODY node of an HTML document.
		function GetBody: ICefDomnode; virtual;
		// Returns the HEAD node of an HTML document.
		function GetHead: ICefDomnode; virtual;
		// Returns the title of an HTML document.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetTitle: ustring; virtual;
		// Returns the document element with the specified ID value.
		function GetElementById(const aId: ustring): ICefDomnode; virtual;
		// Returns the node that currently has keyboard focus.
		function GetFocusedNode: ICefDomnode; virtual;
		// Returns true (1) if a portion of the document is selected.
		function HasSelection: Boolean; virtual;
		// Returns the selection offset within the start node.
		function GetSelectionStartOffset: cint; virtual;
		// Returns the selection offset within the end node.
		function GetSelectionEndOffset: cint; virtual;
		// Returns the contents of this selection as markup.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetSelectionAsMarkup: ustring; virtual;
		// Returns the contents of this selection as text.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetSelectionAsText: ustring; virtual;
		// Returns the base URL for the document.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetBaseUrl: ustring; virtual;
		// Returns a complete URL based on the document base URL and the specified
		// partial URL.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetCompleteUrl(const aPartialURL: ustring): ustring; virtual;
	public
		constructor Create; virtual;
	end;

	// Structure used to represent a DOM node. The functions of this structure
	// should only be called on the render process main thread.
	TCefDomnodeOwn = class(TCefBaseOwn, ICefDomnode)
	protected
		// Returns the type for this node.
		function GetType: TCefDomNodeType; virtual;
		// Returns true (1) if this is a text node.
		function IsText: Boolean; virtual;
		// Returns true (1) if this is an element node.
		function IsElement: Boolean; virtual;
		// Returns true (1) if this is an editable node.
		function IsEditable: Boolean; virtual;
		// Returns true (1) if this is a form control element node.
		function IsFormControlElement: Boolean; virtual;
		// Returns the type of this form control element node.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetFormControlElementType: ustring; virtual;
		// Returns true (1) if this object is pointing to the same handle as |that|
		// object.
		function IsSame(const aThat: ICefDomnode): Boolean; virtual;
		// Returns the name of this node.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetName: ustring; virtual;
		// Returns the value of this node.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetValue: ustring; virtual;
		// Set the value of this node. Returns true (1) on success.
		function SetValue(const aValue: ustring): Boolean; virtual;
		// Returns the contents of this node as markup.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetAsMarkup: ustring; virtual;
		// Returns the document associated with this node.
		function GetDocument: ICefDomdocument; virtual;
		// Returns the parent node.
		function GetParent: ICefDomnode; virtual;
		// Returns the previous sibling node.
		function GetPreviousSibling: ICefDomnode; virtual;
		// Returns the next sibling node.
		function GetNextSibling: ICefDomnode; virtual;
		// Returns true (1) if this node has child nodes.
		function HasChildren: Boolean; virtual;
		// Return the first child node.
		function GetFirstChild: ICefDomnode; virtual;
		// Returns the last child node.
		function GetLastChild: ICefDomnode; virtual;
		// Returns the tag name of this element.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetElementTagName: ustring; virtual;
		// Returns true (1) if this element has attributes.
		function HasElementAttributes: Boolean; virtual;
		// Returns true (1) if this element has an attribute named |attrName|.
		function HasElementAttribute(const aAttrName: ustring): Boolean; virtual;
		// Returns the element attribute named |attrName|.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetElementAttribute(const aAttrName: ustring): ustring; virtual;
		// Returns a map of all element attributes.
		procedure GetElementAttributes(aAttrMap: TStrings); virtual;
		// Set the value for the element attribute named |attrName|. Returns true (1)
		// on success.
		function SetElementAttribute(const aAttrName: ustring; const aValue: ustring): Boolean; virtual;
		// Returns the inner text of the element.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetElementInnerText: ustring; virtual;
	public
		constructor Create; virtual;
	end;


	//..............................................................................cef_download_handler_capi.h
	// Callback structure used to asynchronously continue a download.
	TCefBeforeDownloadCallbackOwn = class(TCefBaseOwn, ICefBeforeDownloadCallback)
	protected
		// Call to continue the download. Set |download_path| to the full file path
		// for the download including the file name or leave blank to use the
		// suggested name and the default temp directory. Set |show_dialog| to true
		// (1) if you do wish to show the default "Save As" dialog.
		procedure Cont(var aDownloadPath: ustring; aShowDialog: Boolean); virtual;
	public
		constructor Create; virtual;
	end;

	// Callback structure used to asynchronously cancel a download.
	TCefDownloadItemCallbackOwn = class(TCefBaseOwn, ICefDownloadItemCallback)
	protected
		// Call to cancel the download.
		procedure Cancel; virtual;
		// Call to pause the download.
		procedure Pause; virtual;
		// Call to resume the download.
		procedure Resume; virtual;
	public
		constructor Create; virtual;
	end;

	// Structure used to handle file downloads. The functions of this structure will
	// called on the browser process UI thread.
	TCefDownloadHandlerOwn = class(TCefBaseOwn, ICefDownloadHandler)
	protected
		// Called before a download begins. |suggested_name| is the suggested name for
		// the download file. By default the download will be canceled. Execute
		// |callback| either asynchronously or in this function to continue the
		// download if desired. Do not keep a reference to |download_item| outside of
		// this function.
		procedure OnBeforeDownload(const aBrowser: ICefBrowser; const aDownloadItem: ICefDownloadItem; const aSuggestedName: ustring; const aCallback: ICefBeforeDownloadCallback); virtual;
		// Called when a download's status or progress information has been updated.
		// This may be called multiple times before and after on_before_download().
		// Execute |callback| either asynchronously or in this function to cancel the
		// download if desired. Do not keep a reference to |download_item| outside of
		// this function.
		procedure OnDownloadUpdated(const aBrowser: ICefBrowser; const aDownloadItem: ICefDownloadItem; const aCallback: ICefDownloadItemCallback); virtual;
	public
		constructor Create; virtual;
	end;


	//..............................................................................cef_download_item_capi.h
	// Structure used to represent a download item.
	TCefDownloadItemOwn = class(TCefBaseOwn, ICefDownloadItem)
	protected
		// Returns true (1) if this object is valid. Do not call any other functions
		// if this function returns false (0).
		function IsValid: Boolean; virtual;
		// Returns true (1) if the download is in progress.
		function IsInProgress: Boolean; virtual;
		// Returns true (1) if the download is complete.
		function IsComplete: Boolean; virtual;
		// Returns true (1) if the download has been canceled or interrupted.
		function IsCanceled: Boolean; virtual;
		// Returns a simple speed estimate in bytes/s.
		function GetCurrentSpeed: cint64; virtual;
		// Returns the rough percent complete or -1 if the receive total size is
		// unknown.
		function GetPercentComplete: cint; virtual;
		// Returns the total number of bytes.
		function GetTotalBytes: cint64; virtual;
		// Returns the number of received bytes.
		function GetReceivedBytes: cint64; virtual;
		// Returns the time that the download started.
		function GetStartTime: TCefTime; virtual;
		// Returns the time that the download ended.
		function GetEndTime: TCefTime; virtual;
		// Returns the full path to the downloaded or downloading file.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetFullPath: ustring; virtual;
		// Returns the unique identifier for this download.
		function GetId: cuint32; virtual;
		// Returns the URL.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetUrl: ustring; virtual;
		// Returns the original URL before any redirections.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetOriginalUrl: ustring; virtual;
		// Returns the suggested file name.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetSuggestedFileName: ustring; virtual;
		// Returns the content disposition.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetContentDisposition: ustring; virtual;
		// Returns the mime type.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetMimeType: ustring; virtual;
	public
		constructor Create; virtual;
	end;


	//..............................................................................cef_drag_data_capi.h
	// Structure used to represent drag data. The functions of this structure may be
	// called on any thread.
	TCefDragDataOwn = class(TCefBaseOwn, ICefDragData)
	protected
		// Returns a copy of the current object.
		function Clone: ICefDragData; virtual;
		// Returns true (1) if this object is read-only.
		function IsReadOnly: Boolean; virtual;
		// Returns true (1) if the drag data is a link.
		function IsLink: Boolean; virtual;
		// Returns true (1) if the drag data is a text or html fragment.
		function IsFragment: Boolean; virtual;
		// Returns true (1) if the drag data is a file.
		function IsFile: Boolean; virtual;
		// Return the link URL that is being dragged.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetLinkUrl: ustring; virtual;
		// Return the title associated with the link being dragged.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetLinkTitle: ustring; virtual;
		// Return the metadata, if any, associated with the link being dragged.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetLinkMetadata: ustring; virtual;
		// Return the plain text fragment that is being dragged.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetFragmentText: ustring; virtual;
		// Return the text/html fragment that is being dragged.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetFragmentHtml: ustring; virtual;
		// Return the base URL that the fragment came from. This value is used for
		// resolving relative URLs and may be NULL.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetFragmentBaseUrl: ustring; virtual;
		// Return the name of the file being dragged out of the browser window.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetFileName: ustring; virtual;
		// Write the contents of the file being dragged out of the web view into
		// |writer|. Returns the number of bytes sent to |writer|. If |writer| is NULL
		// this function will return the size of the file contents in bytes. Call
		// get_file_name() to get a suggested name for the file.
		function GetFileContents(const aWriter: ICefStreamWriter): csize_t; virtual;
		// Retrieve the list of file names that are being dragged into the browser
		// window.
		function GetFileNames(aNames: TStrings): Boolean; virtual;
		// Set the link URL that is being dragged.
		procedure SetLinkUrl(const aUrl: ustring); virtual;
		// Set the title associated with the link being dragged.
		procedure SetLinkTitle(const aTitle: ustring); virtual;
		// Set the metadata associated with the link being dragged.
		procedure SetLinkMetadata(const aData: ustring); virtual;
		// Set the plain text fragment that is being dragged.
		procedure SetFragmentText(const aText: ustring); virtual;
		// Set the text/html fragment that is being dragged.
		procedure SetFragmentHtml(const aHtml: ustring); virtual;
		// Set the base URL that the fragment came from.
		procedure SetFragmentBaseUrl(const aBaseUrl: ustring); virtual;
		// Reset the file contents. You should do this before calling
		// cef_browser_host_t::DragTargetDragEnter as the web view does not allow us
		// to drag in this kind of data.
		procedure ResetFileContents; virtual;
		// Add a file that is being dragged into the webview.
		procedure AddFile(const aPath: ustring; const aDisplayName: ustring); virtual;
	public
		constructor Create; virtual;
	end;


	//..............................................................................cef_drag_handler_capi.h
	// Implement this structure to handle events related to dragging. The functions
	// of this structure will be called on the UI thread.
	TCefDragHandlerOwn = class(TCefBaseOwn, ICefDragHandler)
	protected
		// Called when an external drag event enters the browser window. |dragData|
		// contains the drag event data and |mask| represents the type of drag
		// operation. Return false (0) for default drag handling behavior or true (1)
		// to cancel the drag event.
		function OnDragEnter(const aBrowser: ICefBrowser; const aDragData: ICefDragData; aMask: TCefDragOperationsMask): Boolean; virtual;
	public
		constructor Create; virtual;
	end;


	//..............................................................................cef_find_handler_capi.h
	// Implement this structure to handle events related to find results. The
	// functions of this structure will be called on the UI thread.
	TCefFindHandlerOwn = class(TCefBaseOwn, ICefFindHandler)
	protected
		// Called to report find results returned by cef_browser_host_t::find().
		// |identifer| is the identifier passed to find(), |count| is the number of
		// matches currently identified, |selectionRect| is the location of where the
		// match was found (in window coordinates), |activeMatchOrdinal| is the
		// current position in the search results, and |finalUpdate| is true (1) if
		// this is the last find notification.
		procedure OnFindResult(const aBrowser: ICefBrowser; aIdentifier: cint; aCount: cint; const aSelectionRect: TCefRect; aActiveMatchOrdinal: cint; aFinalUpdate: Boolean); virtual;
	public
		constructor Create; virtual;
	end;


	//..............................................................................cef_focus_handler_capi.h
	// Implement this structure to handle events related to focus. The functions of
	// this structure will be called on the UI thread.
	TCefFocusHandlerOwn = class(TCefBaseOwn, ICefFocusHandler)
	protected
		// Called when the browser component is about to loose focus. For instance, if
		// focus was on the last HTML element and the user pressed the TAB key. |next|
		// will be true (1) if the browser is giving focus to the next component and
		// false (0) if the browser is giving focus to the previous component.
		procedure OnTakeFocus(const aBrowser: ICefBrowser; aNext: Boolean); virtual;
		// Called when the browser component is requesting focus. |source| indicates
		// where the focus request is originating from. Return false (0) to allow the
		// focus to be set or true (1) to cancel setting the focus.
		function OnSetFocus(const aBrowser: ICefBrowser; aSource: TCefFocusSource): Boolean; virtual;
		// Called when the browser component has received focus.
		procedure OnGotFocus(const aBrowser: ICefBrowser); virtual;
	public
		constructor Create; virtual;
	end;


	//..............................................................................cef_frame_capi.h
	// Structure used to represent a frame in the browser window. When used in the
	// browser process the functions of this structure may be called on any thread
	// unless otherwise indicated in the comments. When used in the render process
	// the functions of this structure may only be called on the main thread.
	TCefFrameOwn = class(TCefBaseOwn, ICefFrame)
	protected
		// True if this object is currently attached to a valid frame.
		function IsValid: Boolean; virtual;
		// Execute undo in this frame.
		procedure Undo; virtual;
		// Execute redo in this frame.
		procedure Redo; virtual;
		// Execute cut in this frame.
		procedure Cut; virtual;
		// Execute copy in this frame.
		procedure Copy; virtual;
		// Execute paste in this frame.
		procedure Paste; virtual;
		// Execute delete in this frame.
		procedure Del; virtual;
		// Execute select all in this frame.
		procedure SelectAll; virtual;
		// Save this frame's HTML source to a temporary file and open it in the
		// default text viewing application. This function can only be called from the
		// browser process.
		procedure ViewSource; virtual;
		// Retrieve this frame's HTML source as a string sent to the specified
		// visitor.
		procedure GetSource(const aVisitor: ICefStringVisitor); virtual;
    procedure GetSourceProc(const aProc: TCefStringVisitorProc); virtual;
		// Retrieve this frame's display text as a string sent to the specified
		// visitor.
		procedure GetText(const aVisitor: ICefStringVisitor); virtual;
    procedure GetTextProc(const aProc: TCefStringVisitorProc); virtual;
		// Load the request represented by the |request| object.
		procedure LoadRequest(const aRequest: ICefRequest); virtual;
		// Load the specified |url|.
		procedure LoadUrl(const aUrl: ustring); virtual;
		// Load the contents of |string_val| with the specified dummy |url|. |url|
		// should have a standard scheme (for example, http scheme) or behaviors like
		// link clicks and web security restrictions may not behave as expected.
		procedure LoadString(const aStringVal: ustring; const aUrl: ustring); virtual;
		// Execute a string of JavaScript code in this frame. The |script_url|
		// parameter is the URL where the script in question can be found, if any. The
		// renderer may request this URL to show the developer the source of the
		// error.  The |start_line| parameter is the base line number to use for error
		// reporting.
		procedure ExecuteJavaScript(const aCode: ustring; const aScriptUrl: ustring; aStartLine: cint); virtual;
		// Returns true (1) if this is the main (top-level) frame.
		function IsMain: Boolean; virtual;
		// Returns true (1) if this is the focused frame.
		function IsFocused: Boolean; virtual;
		// Returns the name for this frame. If the frame has an assigned name (for
		// example, set via the iframe "name" attribute) then that value will be
		// returned. Otherwise a unique name will be constructed based on the frame
		// parent hierarchy. The main (top-level) frame will always have an NULL name
		// value.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetName: ustring; virtual;
		// Returns the globally unique identifier for this frame.
		function GetIdentifier: cint64; virtual;
		// Returns the parent of this frame or NULL if this is the main (top-level)
		// frame.
		function GetParent: ICefFrame; virtual;
		// Returns the URL currently loaded in this frame.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetUrl: ustring; virtual;
		// Returns the browser that this frame belongs to.
		function GetBrowser: ICefBrowser; virtual;
		// Get the V8 context associated with the frame. This function can only be
		// called from the render process.
		function GetV8context: ICefV8context; virtual;
		// Visit the DOM document. This function can only be called from the render
		// process.
		procedure VisitDom(const aVisitor: ICefDomvisitor); virtual;
    procedure VisitDomProc(const aProc: TCefDomVisitorProc); virtual;
	public
		constructor Create; virtual;
	end;


	//..............................................................................cef_geolocation_capi.h
	// Implement this structure to receive geolocation updates. The functions of
	// this structure will be called on the browser process UI thread.
	TCefGetGeolocationCallbackOwn = class(TCefBaseOwn, ICefGetGeolocationCallback)
	protected
		// Called with the 'best available' location information or, if the location
		// update failed, with error information.
		procedure OnLocationUpdate(const aPosition: TCefGeoposition); virtual;
	public
		constructor Create; virtual;
	end;


	//..............................................................................cef_geolocation_handler_capi.h
	// Callback structure used for asynchronous continuation of geolocation
	// permission requests.
	TCefGeolocationCallbackOwn = class(TCefBaseOwn, ICefGeolocationCallback)
	protected
		// Call to allow or deny geolocation access.
		procedure Cont(aAllow: Boolean); virtual;
	public
		constructor Create; virtual;
	end;

	// Implement this structure to handle events related to geolocation permission
	// requests. The functions of this structure will be called on the browser
	// process UI thread.
	TCefGeolocationHandlerOwn = class(TCefBaseOwn, ICefGeolocationHandler)
	protected
		// Called when a page requests permission to access geolocation information.
		// |requesting_url| is the URL requesting permission and |request_id| is the
		// unique ID for the permission request. Return true (1) and call
		// cef_geolocation_callback_t::cont() either in this function or at a later
		// time to continue or cancel the request. Return false (0) to cancel the
		// request immediately.
		function OnRequestGeolocationPermission(const aBrowser: ICefBrowser; const aRequestingUrl: ustring; aRequestId: cint; const aCallback: ICefGeolocationCallback): Boolean; virtual;
		// Called when a geolocation access request is canceled. |requesting_url| is
		// the URL that originally requested permission and |request_id| is the unique
		// ID for the permission request.
		procedure OnCancelGeolocationPermission(const aBrowser: ICefBrowser; const aRequestingUrl: ustring; aRequestId: cint); virtual;
	public
		constructor Create; virtual;
	end;


	//..............................................................................cef_jsdialog_handler_capi.h
	// Callback structure used for asynchronous continuation of JavaScript dialog
	// requests.
	TCefJsdialogCallbackOwn = class(TCefBaseOwn, ICefJsdialogCallback)
	protected
		// Continue the JS dialog request. Set |success| to true (1) if the OK button
		// was pressed. The |user_input| value should be specified for prompt dialogs.
		procedure Cont(aSuccess: Boolean; const aUserInput: ustring); virtual;
	public
		constructor Create; virtual;
	end;

	// Implement this structure to handle events related to JavaScript dialogs. The
	// functions of this structure will be called on the UI thread.
	TCefJsdialogHandlerOwn = class(TCefBaseOwn, ICefJsdialogHandler)
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
		function OnJsdialog(const aBrowser: ICefBrowser; const aOriginUrl: ustring; const aAcceptLang: ustring; aDialogType: TCefJsdialogType; const aMessageText: ustring; const aDefaultPromptText: ustring; const aCallback: ICefJsdialogCallback; out aSuppressMessage: Boolean): Boolean; virtual;
		// Called to run a dialog asking the user if they want to leave a page. Return
		// false (0) to use the default dialog implementation. Return true (1) if the
		// application will use a custom dialog or if the callback has been executed
		// immediately. Custom dialogs may be either modal or modeless. If a custom
		// dialog is used the application must execute |callback| once the custom
		// dialog is dismissed.
		function OnBeforeUnloadDialog(const aBrowser: ICefBrowser; const aMessageText: ustring; aIsReload: Boolean; const aCallback: ICefJsdialogCallback): Boolean; virtual;
		// Called to cancel any pending dialogs and reset any saved dialog state. Will
		// be called due to events like page navigation irregardless of whether any
		// dialogs are currently pending.
		procedure OnResetDialogState(const aBrowser: ICefBrowser); virtual;
		// Called when the default implementation dialog is closed.
		procedure OnDialogClosed(const aBrowser: ICefBrowser); virtual;
	public
		constructor Create; virtual;
	end;


	//..............................................................................cef_keyboard_handler_capi.h
	// Implement this structure to handle events related to keyboard input. The
	// functions of this structure will be called on the UI thread.
	TCefKeyboardHandlerOwn = class(TCefBaseOwn, ICefKeyboardHandler)
	protected
		// Called before a keyboard event is sent to the renderer. |event| contains
		// information about the keyboard event. |os_event| is the operating system
		// event message, if any. Return true (1) if the event was handled or false
		// (0) otherwise. If the event will be handled in on_key_event() as a keyboard
		// shortcut set |is_keyboard_shortcut| to true (1) and return false (0).
		function OnPreKeyEvent(const aBrowser: ICefBrowser; var aEvent: TCefKeyEvent; aOsEvent: TCefEventHandle; out aIsKeyboardShortcut: Boolean): Boolean; virtual;
		// Called after the renderer and JavaScript in the page has had a chance to
		// handle the event. |event| contains information about the keyboard event.
		// |os_event| is the operating system event message, if any. Return true (1)
		// if the keyboard event was handled or false (0) otherwise.
		function OnKeyEvent(const aBrowser: ICefBrowser; var aEvent: TCefKeyEvent; aOsEvent: TCefEventHandle): Boolean; virtual;
	public
		constructor Create; virtual;
	end;


	//..............................................................................cef_life_span_handler_capi.h
	// Implement this structure to handle events related to browser life span. The
	// functions of this structure will be called on the UI thread unless otherwise
	// indicated.
	TCefLifeSpanHandlerOwn = class(TCefBaseOwn, ICefLifeSpanHandler)
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
		function OnBeforePopup(const aBrowser: ICefBrowser; const aFrame: ICefFrame; var aTargetUrl: ustring; const aTargetFrameName: ustring; aTargetDisposition: TCefWindowOpenDisposition; aUserGesture: Boolean; var aPopupFeatures: TCefPopupFeatures; var aWindowInfo: TCefWindowInfo; var aClient: ICefClient; var aSettings: TCefBrowserSettings; var aNoJavascriptAccess: Boolean): Boolean; virtual;
		// Called after a new browser is created.
		procedure OnAfterCreated(const aBrowser: ICefBrowser); virtual;
		// Called when a modal window is about to display and the modal loop should
		// begin running. Return false (0) to use the default modal loop
		// implementation or true (1) to use a custom implementation.
		function RunModal(const aBrowser: ICefBrowser): Boolean; virtual;
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
		function DoClose(const aBrowser: ICefBrowser): Boolean; virtual;
		// Called just before a browser is destroyed. Release all references to the
		// browser object and do not attempt to execute any functions on the browser
		// object after this callback returns. If this is a modal window and a custom
		// modal loop implementation was provided in run_modal() this callback should
		// be used to exit the custom modal loop. See do_close() documentation for
		// additional usage information.
		procedure OnBeforeClose(const aBrowser: ICefBrowser); virtual;
	public
		constructor Create; virtual;
	end;


	//..............................................................................cef_load_handler_capi.h
	// Implement this structure to handle events related to browser load status. The
	// functions of this structure will be called on the browser process UI thread
	// or render process main thread (TID_RENDERER).
	TCefLoadHandlerOwn = class(TCefBaseOwn, ICefLoadHandler)
	protected
		// Called when the loading state has changed. This callback will be executed
		// twice -- once when loading is initiated either programmatically or by user
		// action, and once when loading is terminated due to completion, cancellation
		// of failure.
		procedure OnLoadingStateChange(const aBrowser: ICefBrowser; aIsLoading: Boolean; aCanGoBack: Boolean; aCanGoForward: Boolean); virtual;
		// Called when the browser begins loading a frame. The |frame| value will
		// never be NULL -- call the is_main() function to check if this frame is the
		// main frame. Multiple frames may be loading at the same time. Sub-frames may
		// start or continue loading after the main frame load has ended. This
		// function may not be called for a particular frame if the load request for
		// that frame fails. For notification of overall browser load status use
		// OnLoadingStateChange instead.
		procedure OnLoadStart(const aBrowser: ICefBrowser; const aFrame: ICefFrame); virtual;
		// Called when the browser is done loading a frame. The |frame| value will
		// never be NULL -- call the is_main() function to check if this frame is the
		// main frame. Multiple frames may be loading at the same time. Sub-frames may
		// start or continue loading after the main frame load has ended. This
		// function will always be called for all frames irrespective of whether the
		// request completes successfully.
		procedure OnLoadEnd(const aBrowser: ICefBrowser; const aFrame: ICefFrame; aHttpStatusCode: cint); virtual;
		// Called when the resource load for a navigation fails or is canceled.
		// |errorCode| is the error code number, |errorText| is the error text and
		// |failedUrl| is the URL that failed to load. See net\base\net_error_list.h
		// for complete descriptions of the error codes.
		procedure OnLoadError(const aBrowser: ICefBrowser; const aFrame: ICefFrame; aErrorCode: TCefErrorcode; const aErrorText: ustring; const aFailedUrl: ustring); virtual;
	public
		constructor Create; virtual;
	end;


	//..............................................................................cef_menu_model_capi.h
	// Supports creation and modification of menus. See cef_menu_id_t for the
	// command ids that have default implementations. All user-defined command ids
	// should be between MENU_ID_USER_FIRST and MENU_ID_USER_LAST. The functions of
	// this structure can only be accessed on the browser process the UI thread.
	TCefMenuModelOwn = class(TCefBaseOwn, ICefMenuModel)
	protected
		// Clears the menu. Returns true (1) on success.
		function Clear: Boolean; virtual;
		// Returns the number of items in this menu.
		function GetCount: cint; virtual;
		//
		// Add a separator to the menu. Returns true (1) on success.
		function AddSeparator: Boolean; virtual;
		//
		// Add an item to the menu. Returns true (1) on success.
		function AddItem(aCommandId: cint; const aLabel: ustring): Boolean; virtual;
		//
		// Add a check item to the menu. Returns true (1) on success.
		function AddCheckItem(aCommandId: cint; const aLabel: ustring): Boolean; virtual;
		//
		// Add a radio item to the menu. Only a single item with the specified
		// |group_id| can be checked at a time. Returns true (1) on success.
		function AddRadioItem(aCommandId: cint; const aLabel: ustring; aGroupId: cint): Boolean; virtual;
		//
		// Add a sub-menu to the menu. The new sub-menu is returned.
		function AddSubMenu(aCommandId: cint; const aLabel: ustring): ICefMenuModel; virtual;
		//
		// Insert a separator in the menu at the specified |index|. Returns true (1)
		// on success.
		function InsertSeparatorAt(aIndex: cint): Boolean; virtual;
		//
		// Insert an item in the menu at the specified |index|. Returns true (1) on
		// success.
		function InsertItemAt(aIndex: cint; aCommandId: cint; const aLabel: ustring): Boolean; virtual;
		//
		// Insert a check item in the menu at the specified |index|. Returns true (1)
		// on success.
		function InsertCheckItemAt(aIndex: cint; aCommandId: cint; const aLabel: ustring): Boolean; virtual;
		//
		// Insert a radio item in the menu at the specified |index|. Only a single
		// item with the specified |group_id| can be checked at a time. Returns true
		// (1) on success.
		function InsertRadioItemAt(aIndex: cint; aCommandId: cint; const aLabel: ustring; aGroupId: cint): Boolean; virtual;
		//
		// Insert a sub-menu in the menu at the specified |index|. The new sub-menu is
		// returned.
		function InsertSubMenuAt(aIndex: cint; aCommandId: cint; const aLabel: ustring): ICefMenuModel; virtual;
		// Removes the item with the specified |command_id|. Returns true (1) on
		// success.
		function Remove(aCommandId: cint): Boolean; virtual;
		// Removes the item at the specified |index|. Returns true (1) on success.
		function RemoveAt(aIndex: cint): Boolean; virtual;
		// Returns the index associated with the specified |command_id| or -1 if not
		// found due to the command id not existing in the menu.
		function GetIndexOf(aCommandId: cint): cint; virtual;
		// Returns the command id at the specified |index| or -1 if not found due to
		// invalid range or the index being a separator.
		function GetCommandIdAt(aIndex: cint): cint; virtual;
		// Sets the command id at the specified |index|. Returns true (1) on success.
		function SetCommandIdAt(aIndex: cint; aCommandId: cint): Boolean; virtual;
		// Returns the label for the specified |command_id| or NULL if not found.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetLabel(aCommandId: cint): ustring; virtual;
		// Returns the label at the specified |index| or NULL if not found due to
		// invalid range or the index being a separator.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetLabelAt(aIndex: cint): ustring; virtual;
		// Sets the label for the specified |command_id|. Returns true (1) on success.
		function SetLabel(aCommandId: cint; const aLabel: ustring): Boolean; virtual;
		// Set the label at the specified |index|. Returns true (1) on success.
		function SetLabelAt(aIndex: cint; const aLabel: ustring): Boolean; virtual;
		// Returns the item type for the specified |command_id|.
		function GetType(aCommandId: cint): TCefMenuItemType; virtual;
		// Returns the item type at the specified |index|.
		function GetTypeAt(aIndex: cint): TCefMenuItemType; virtual;
		// Returns the group id for the specified |command_id| or -1 if invalid.
		function GetGroupId(aCommandId: cint): cint; virtual;
		// Returns the group id at the specified |index| or -1 if invalid.
		function GetGroupIdAt(aIndex: cint): cint; virtual;
		// Sets the group id for the specified |command_id|. Returns true (1) on
		// success.
		function SetGroupId(aCommandId: cint; aGroupId: cint): Boolean; virtual;
		// Sets the group id at the specified |index|. Returns true (1) on success.
		function SetGroupIdAt(aIndex: cint; aGroupId: cint): Boolean; virtual;
		// Returns the submenu for the specified |command_id| or NULL if invalid.
		function GetSubMenu(aCommandId: cint): ICefMenuModel; virtual;
		// Returns the submenu at the specified |index| or NULL if invalid.
		function GetSubMenuAt(aIndex: cint): ICefMenuModel; virtual;
		//
		// Returns true (1) if the specified |command_id| is visible.
		function IsVisible(aCommandId: cint): Boolean; virtual;
		//
		// Returns true (1) if the specified |index| is visible.
		function IsVisibleAt(aIndex: cint): Boolean; virtual;
		//
		// Change the visibility of the specified |command_id|. Returns true (1) on
		// success.
		function SetVisible(aCommandId: cint; aVisible: Boolean): Boolean; virtual;
		//
		// Change the visibility at the specified |index|. Returns true (1) on
		// success.
		function SetVisibleAt(aIndex: cint; aVisible: Boolean): Boolean; virtual;
		//
		// Returns true (1) if the specified |command_id| is enabled.
		function IsEnabled(aCommandId: cint): Boolean; virtual;
		//
		// Returns true (1) if the specified |index| is enabled.
		function IsEnabledAt(aIndex: cint): Boolean; virtual;
		//
		// Change the enabled status of the specified |command_id|. Returns true (1)
		// on success.
		function SetEnabled(aCommandId: cint; aEnabled: Boolean): Boolean; virtual;
		//
		// Change the enabled status at the specified |index|. Returns true (1) on
		// success.
		function SetEnabledAt(aIndex: cint; aEnabled: Boolean): Boolean; virtual;
		//
		// Returns true (1) if the specified |command_id| is checked. Only applies to
		// check and radio items.
		function IsChecked(aCommandId: cint): Boolean; virtual;
		//
		// Returns true (1) if the specified |index| is checked. Only applies to check
		// and radio items.
		function IsCheckedAt(aIndex: cint): Boolean; virtual;
		//
		// Check the specified |command_id|. Only applies to check and radio items.
		// Returns true (1) on success.
		function SetChecked(aCommandId: cint; aChecked: Boolean): Boolean; virtual;
		//
		// Check the specified |index|. Only applies to check and radio items. Returns
		// true (1) on success.
		function SetCheckedAt(aIndex: cint; aChecked: Boolean): Boolean; virtual;
		//
		// Returns true (1) if the specified |command_id| has a keyboard accelerator
		// assigned.
		function HasAccelerator(aCommandId: cint): Boolean; virtual;
		//
		// Returns true (1) if the specified |index| has a keyboard accelerator
		// assigned.
		function HasAcceleratorAt(aIndex: cint): Boolean; virtual;
		//
		// Set the keyboard accelerator for the specified |command_id|. |key_code| can
		// be any virtual key or character value. Returns true (1) on success.
		function SetAccelerator(aCommandId: cint; aKeyCode: cint; aShiftPressed: Boolean; aCtrlPressed: Boolean; aAltPressed: Boolean): Boolean; virtual;
		//
		// Set the keyboard accelerator at the specified |index|. |key_code| can be
		// any virtual key or character value. Returns true (1) on success.
		function SetAcceleratorAt(aIndex: cint; aKeyCode: cint; aShiftPressed: Boolean; aCtrlPressed: Boolean; aAltPressed: Boolean): Boolean; virtual;
		//
		// Remove the keyboard accelerator for the specified |command_id|. Returns
		// true (1) on success.
		function RemoveAccelerator(aCommandId: cint): Boolean; virtual;
		//
		// Remove the keyboard accelerator at the specified |index|. Returns true (1)
		// on success.
		function RemoveAcceleratorAt(aIndex: cint): Boolean; virtual;
		//
		// Retrieves the keyboard accelerator for the specified |command_id|. Returns
		// true (1) on success.
		function GetAccelerator(aCommandId: cint; out aKeyCode: cint; out aShiftPressed: Boolean; out aCtrlPressed: Boolean; out aAltPressed: Boolean): Boolean; virtual;
		//
		// Retrieves the keyboard accelerator for the specified |index|. Returns true
		// (1) on success.
		function GetAcceleratorAt(aIndex: cint; out aKeyCode: cint; out aShiftPressed: Boolean; out aCtrlPressed: Boolean; out aAltPressed: Boolean): Boolean; virtual;
	public
		constructor Create; virtual;
	end;


	//..............................................................................cef_navigation_entry_capi.h
	// Structure used to represent an entry in navigation history.
	TCefNavigationEntryOwn = class(TCefBaseOwn, ICefNavigationEntry)
	protected
		// Returns true (1) if this object is valid. Do not call any other functions
		// if this function returns false (0).
		function IsValid: Boolean; virtual;
		// Returns the actual URL of the page. For some pages this may be data: URL or
		// similar. Use get_display_url() to return a display-friendly version.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetUrl: ustring; virtual;
		// Returns a display-friendly version of the URL.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetDisplayUrl: ustring; virtual;
		// Returns the original URL that was entered by the user before any redirects.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetOriginalUrl: ustring; virtual;
		// Returns the title set by the page. This value may be NULL.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetTitle: ustring; virtual;
		// Returns the transition type which indicates what the user did to move to
		// this page from the previous page.
		function GetTransitionType: TCefTransitionType; virtual;
		// Returns true (1) if this navigation includes post data.
		function HasPostData: Boolean; virtual;
		// Returns the name of the sub-frame that navigated or an NULL value if the
		// main frame navigated.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetFrameName: ustring; virtual;
		// Returns the time for the last known successful navigation completion. A
		// navigation may be completed more than once if the page is reloaded. May be
		// 0 if the navigation has not yet completed.
		function GetCompletionTime: TCefTime; virtual;
		// Returns the HTTP status code for the last known successful navigation
		// response. May be 0 if the response has not yet been received or if the
		// navigation has not yet completed.
		function GetHttpStatusCode: cint; virtual;
	public
		constructor Create; virtual;
	end;


	//..............................................................................cef_origin_whitelist_capi.h

	//..............................................................................cef_parser_capi.h

	//..............................................................................cef_path_util_capi.h

	//..............................................................................cef_print_handler_capi.h
	// Callback structure for asynchronous continuation of print dialog requests.
	TCefPrintDialogCallbackOwn = class(TCefBaseOwn, ICefPrintDialogCallback)
	protected
		// Continue printing with the specified |settings|.
		procedure Cont(const aSettings: ICefPrintSettings); virtual;
		// Cancel the printing.
		procedure Cancel; virtual;
	public
		constructor Create; virtual;
	end;

	// Callback structure for asynchronous continuation of print job requests.
	TCefPrintJobCallbackOwn = class(TCefBaseOwn, ICefPrintJobCallback)
	protected
		// Indicate completion of the print job.
		procedure Cont; virtual;
	public
		constructor Create; virtual;
	end;

	// Implement this structure to handle printing on Linux. The functions of this
	// structure will be called on the browser process UI thread.
	TCefPrintHandlerOwn = class(TCefBaseOwn, ICefPrintHandler)
	protected
		// Synchronize |settings| with client state. If |get_defaults| is true (1)
		// then populate |settings| with the default print settings. Do not keep a
		// reference to |settings| outside of this callback.
		procedure OnPrintSettings(const aSettings: ICefPrintSettings; aGetDefaults: Boolean); virtual;
		// Show the print dialog. Execute |callback| once the dialog is dismissed.
		// Return true (1) if the dialog will be displayed or false (0) to cancel the
		// printing immediately.
		function OnPrintDialog(aHasSelection: Boolean; const aCallback: ICefPrintDialogCallback): Boolean; virtual;
		// Send the print job to the printer. Execute |callback| once the job is
		// completed. Return true (1) if the job will proceed or false (0) to cancel
		// the job immediately.
		function OnPrintJob(const aDocumentName: ustring; const aPdfFilePath: ustring; const aCallback: ICefPrintJobCallback): Boolean; virtual;
		// Reset client state related to printing.
		procedure OnPrintReset; virtual;
	public
		constructor Create; virtual;
	end;


	//..............................................................................cef_print_settings_capi.h
	// Structure representing print settings.
	TCefPrintSettingsOwn = class(TCefBaseOwn, ICefPrintSettings)
	protected
		// Returns true (1) if this object is valid. Do not call any other functions
		// if this function returns false (0).
		function IsValid: Boolean; virtual;
		// Returns true (1) if the values of this object are read-only. Some APIs may
		// expose read-only objects.
		function IsReadOnly: Boolean; virtual;
		// Returns a writable copy of this object.
		function Copy: ICefPrintSettings; virtual;
		// Set the page orientation.
		procedure SetOrientation(aLandscape: Boolean); virtual;
		// Returns true (1) if the orientation is landscape.
		function IsLandscape: Boolean; virtual;
		// Set the printer printable area in device units. Some platforms already
		// provide flipped area. Set |landscape_needs_flip| to false (0) on those
		// platforms to avoid double flipping.
		procedure SetPrinterPrintableArea(const aPhysicalSizeDeviceUnits: TCefSize; var aPrintableAreaDeviceUnits: TCefRect; var aLandscapeNeedsFlip: Boolean); virtual;
		// Set the device name.
		procedure SetDeviceName(const aName: ustring); virtual;
		// Get the device name.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetDeviceName: ustring; virtual;
		// Set the DPI (dots per inch).
		procedure SetDpi(aDpi: cint); virtual;
		// Get the DPI (dots per inch).
		function GetDpi: cint; virtual;
		// Set the page ranges.
		procedure SetPageRanges(aRangesCount: csize_t; aRanges: TCefPageRangeArray); virtual;
		// Returns the number of page ranges that currently exist.
		function GetPageRangesCount: csize_t; virtual;
		// Retrieve the page ranges.
		procedure GetPageRanges(var aRangesCount: csize_t; const aRanges: TCefPageRangeArray); virtual;
		// Set whether only the selection will be printed.
		procedure SetSelectionOnly(aSelectionOnly: Boolean); virtual;
		// Returns true (1) if only the selection will be printed.
		function IsSelectionOnly: Boolean; virtual;
		// Set whether pages will be collated.
		procedure SetCollate(aCollate: Boolean); virtual;
		// Returns true (1) if pages will be collated.
		function WillCollate: Boolean; virtual;
		// Set the color model.
		procedure SetColorModel(aModel: TCefColorModel); virtual;
		// Get the color model.
		function GetColorModel: TCefColorModel; virtual;
		// Set the number of copies.
		procedure SetCopies(aCopies: cint); virtual;
		// Get the number of copies.
		function GetCopies: cint; virtual;
		// Set the duplex mode.
		procedure SetDuplexMode(aMode: TCefDuplexMode); virtual;
		// Get the duplex mode.
		function GetDuplexMode: TCefDuplexMode; virtual;
	public
		constructor Create; virtual;
	end;


	//..............................................................................cef_process_message_capi.h
	// Structure representing a message. Can be used on any process and thread.
	TCefProcessMessageOwn = class(TCefBaseOwn, ICefProcessMessage)
	protected
		// Returns true (1) if this object is valid. Do not call any other functions
		// if this function returns false (0).
		function IsValid: Boolean; virtual;
		// Returns true (1) if the values of this object are read-only. Some APIs may
		// expose read-only objects.
		function IsReadOnly: Boolean; virtual;
		// Returns a writable copy of this object.
		function Copy: ICefProcessMessage; virtual;
		// Returns the message name.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetName: ustring; virtual;
		// Returns the list of arguments.
		function GetArgumentList: ICefListValue; virtual;
	public
		constructor Create; virtual;
	end;


	//..............................................................................cef_process_util_capi.h

	//..............................................................................cef_render_handler_capi.h
	// Implement this structure to handle events when window rendering is disabled.
	// The functions of this structure will be called on the UI thread.
	TCefRenderHandlerOwn = class(TCefBaseOwn, ICefRenderHandler)
	protected
		// Called to retrieve the root window rectangle in screen coordinates. Return
		// true (1) if the rectangle was provided.
		function GetRootScreenRect(const aBrowser: ICefBrowser; var aRect: TCefRect): Boolean; virtual;
		// Called to retrieve the view rectangle which is relative to screen
		// coordinates. Return true (1) if the rectangle was provided.
		function GetViewRect(const aBrowser: ICefBrowser; var aRect: TCefRect): Boolean; virtual;
		// Called to retrieve the translation from view coordinates to actual screen
		// coordinates. Return true (1) if the screen coordinates were provided.
		function GetScreenPoint(const aBrowser: ICefBrowser; aViewX: cint; aViewY: cint; var aScreenX: cint; var aScreenY: cint): Boolean; virtual;
		// Called to allow the client to fill in the CefScreenInfo object with
		// appropriate values. Return true (1) if the |screen_info| structure has been
		// modified.
		//
		// If the screen info rectangle is left NULL the rectangle from GetViewRect
		// will be used. If the rectangle is still NULL or invalid popups may not be
		// drawn correctly.
		function GetScreenInfo(const aBrowser: ICefBrowser; out aScreenInfo: TCefScreenInfo): Boolean; virtual;
		// Called when the browser wants to show or hide the popup widget. The popup
		// should be shown if |show| is true (1) and hidden if |show| is false (0).
		procedure OnPopupShow(const aBrowser: ICefBrowser; aShow: Boolean); virtual;
		// Called when the browser wants to move or resize the popup widget. |rect|
		// contains the new location and size in view coordinates.
		procedure OnPopupSize(const aBrowser: ICefBrowser; var aRect: TCefRect); virtual;
		// Called when an element should be painted. Pixel values passed to this
		// function are scaled relative to view coordinates based on the value of
		// CefScreenInfo.device_scale_factor returned from GetScreenInfo. |type|
		// indicates whether the element is the view or the popup widget. |buffer|
		// contains the pixel data for the whole image. |dirtyRects| contains the set
		// of rectangles in pixel coordinates that need to be repainted. |buffer| will
		// be |width|*|height|*4 bytes in size and represents a BGRA image with an
		// upper-left origin.
		procedure OnPaint(const aBrowser: ICefBrowser; aType: TCefPaintElementType; aDirtyRectsCount: csize_t; const aDirtyRects: TCefRectArray; const aBuffer: cvoid; aWidth: cint; aHeight: cint); virtual;
		// Called when the browser's cursor has changed. If |type| is CT_CUSTOM then
		// |custom_cursor_info| will be populated with the custom cursor information.
		procedure OnCursorChange(const aBrowser: ICefBrowser; aCursor: TCefCursorHandle; aType: TCefCursorType; aCustomCursorInfo: TCefCursorInfo); virtual;
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
		function StartDragging(const aBrowser: ICefBrowser; const aDragData: ICefDragData; aAllowedOps: TCefDragOperationsMask; aX: cint; aY: cint): Boolean; virtual;
		// Called when the web view wants to update the mouse cursor during a drag &
		// drop operation. |operation| describes the allowed operation (none, move,
		// copy, link).
		procedure UpdateDragCursor(const aBrowser: ICefBrowser; aOperation: TCefDragOperationsMask); virtual;
		// Called when the scroll offset has changed.
		procedure OnScrollOffsetChanged(const aBrowser: ICefBrowser; aX: cdouble; aY: cdouble); virtual;
	public
		constructor Create; virtual;
	end;


	//..............................................................................cef_render_process_handler_capi.h
	// Structure used to implement render process callbacks. The functions of this
	// structure will be called on the render process main thread (TID_RENDERER)
	// unless otherwise indicated.
	TCefRenderProcessHandlerOwn = class(TCefBaseOwn, ICefRenderProcessHandler)
	protected
		// Called after the render process main thread has been created. |extra_info|
		// is a read-only value originating from
		// cef_browser_process_handler_t::on_render_process_thread_created(). Do not
		// keep a reference to |extra_info| outside of this function.
		procedure OnRenderThreadCreated(const aExtraInfo: ICefListValue); virtual;
		// Called after WebKit has been initialized.
		procedure OnWebKitInitialized; virtual;
		// Called after a browser has been created. When browsing cross-origin a new
		// browser will be created before the old browser with the same identifier is
		// destroyed.
		procedure OnBrowserCreated(const aBrowser: ICefBrowser); virtual;
		// Called before a browser is destroyed.
		procedure OnBrowserDestroyed(const aBrowser: ICefBrowser); virtual;
		// Return the handler for browser load status events.
		function GetLoadHandler: ICefLoadHandler; virtual;
		// Called before browser navigation. Return true (1) to cancel the navigation
		// or false (0) to allow the navigation to proceed. The |request| object
		// cannot be modified in this callback.
		function OnBeforeNavigation(const aBrowser: ICefBrowser; const aFrame: ICefFrame; const aRequest: ICefRequest; aNavigationType: TCefNavigationType; aIsRedirect: Boolean): Boolean; virtual;
		// Called immediately after the V8 context for a frame has been created. To
		// retrieve the JavaScript 'window' object use the
		// cef_v8context_t::get_global() function. V8 handles can only be accessed
		// from the thread on which they are created. A task runner for posting tasks
		// on the associated thread can be retrieved via the
		// cef_v8context_t::get_task_runner() function.
		procedure OnContextCreated(const aBrowser: ICefBrowser; const aFrame: ICefFrame; const aContext: ICefV8context); virtual;
		// Called immediately before the V8 context for a frame is released. No
		// references to the context should be kept after this function is called.
		procedure OnContextReleased(const aBrowser: ICefBrowser; const aFrame: ICefFrame; const aContext: ICefV8context); virtual;
		// Called for global uncaught exceptions in a frame. Execution of this
		// callback is disabled by default. To enable set
		// CefSettings.uncaught_exception_stack_size > 0.
		procedure OnUncaughtException(const aBrowser: ICefBrowser; const aFrame: ICefFrame; const aContext: ICefV8context; const aException: ICefV8exception; const aStackTrace: ICefV8stackTrace); virtual;
		// Called when a new node in the the browser gets focus. The |node| value may
		// be NULL if no specific node has gained focus. The node object passed to
		// this function represents a snapshot of the DOM at the time this function is
		// executed. DOM objects are only valid for the scope of this function. Do not
		// keep references to or attempt to access any DOM objects outside the scope
		// of this function.
		procedure OnFocusedNodeChanged(const aBrowser: ICefBrowser; const aFrame: ICefFrame; const aNode: ICefDomnode); virtual;
		// Called when a new message is received from a different process. Return true
		// (1) if the message was handled or false (0) otherwise. Do not keep a
		// reference to or attempt to access the message outside of this callback.
		function OnProcessMessageReceived(const aBrowser: ICefBrowser; aSourceProcess: TCefProcessId; const aMessage: ICefProcessMessage): Boolean; virtual;
	public
		constructor Create; virtual;
	end;


	//..............................................................................cef_request_capi.h
	// Structure used to represent a web request. The functions of this structure
	// may be called on any thread.
	TCefRequestOwn = class(TCefBaseOwn, ICefRequest)
	protected
		// Returns true (1) if this object is read-only.
		function IsReadOnly: Boolean; virtual;
		// Get the fully qualified URL.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetUrl: ustring; virtual;
		// Set the fully qualified URL.
		procedure SetUrl(const aUrl: ustring); virtual;
		// Get the request function type. The value will default to POST if post data
		// is provided and GET otherwise.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetMethod: ustring; virtual;
		// Set the request function type.
		procedure SetMethod(const aMethod: ustring); virtual;
		// Get the post data.
		function GetPostData: ICefPostData; virtual;
		// Set the post data.
		procedure SetPostData(const aPostData: ICefPostData); virtual;
		// Get the header values.
		procedure GetHeaderMap(aHeaderMap: ICefStringMultimap); virtual;
		// Set the header values.
		procedure SetHeaderMap(aHeaderMap: ICefStringMultimap); virtual;
		// Set all values at one time.
		procedure _Set(const aUrl: ustring; const aMethod: ustring; const aPostData: ICefPostData; aHeaderMap: ICefStringMultimap); virtual;
		// Get the flags used in combination with cef_urlrequest_t. See
		// cef_urlrequest_flags_t for supported values.
		function GetFlags: TCefURLRequestFlags; virtual;
		// Set the flags used in combination with cef_urlrequest_t.  See
		// cef_urlrequest_flags_t for supported values.
		procedure SetFlags(aFlags: TCefURLRequestFlags); virtual;
		// Set the URL to the first party for cookies used in combination with
		// cef_urlrequest_t.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetFirstPartyForCookies: ustring; virtual;
		// Get the URL to the first party for cookies used in combination with
		// cef_urlrequest_t.
		procedure SetFirstPartyForCookies(const aUrl: ustring); virtual;
		// Get the resource type for this request. Only available in the browser
		// process.
		function GetResourceType: TCefResourceType; virtual;
		// Get the transition type for this request. Only available in the browser
		// process and only applies to requests that represent a main frame or sub-
		// frame navigation.
		function GetTransitionType: TCefTransitionType; virtual;
		// Returns the globally unique identifier for this request or 0 if not
		// specified. Can be used by cef_request_tHandler implementations in the
		// browser process to track a single request across multiple callbacks.
		function GetIdentifier: cuint64; virtual;
	public
		constructor Create; virtual;
	end;

	// Create a new cef_request_t object.
	// Structure used to represent post data for a web request. The functions of
	// this structure may be called on any thread.
	TCefPostDataOwn = class(TCefBaseOwn, ICefPostData)
	protected
		// Returns true (1) if this object is read-only.
		function IsReadOnly: Boolean; virtual;
		// Returns the number of existing post data elements.
		function GetElementCount: csize_t; virtual;
		// Retrieve the post data elements.
		function GetElements(var aElementsCount: csize_t): IInterfaceList; virtual;
		// Remove the specified post data element.  Returns true (1) if the removal
		// succeeds.
		function RemoveElement(const aElement: ICefPostDataElement): Boolean; virtual;
		// Add the specified post data element.  Returns true (1) if the add succeeds.
		function AddElement(const aElement: ICefPostDataElement): Boolean; virtual;
		// Remove all existing post data elements.
		procedure RemoveElements; virtual;
	public
		constructor Create; virtual;
	end;

	// Create a new cef_post_data_t object.
	// Structure used to represent a single element in the request post data. The
	// functions of this structure may be called on any thread.
	TCefPostDataElementOwn = class(TCefBaseOwn, ICefPostDataElement)
  private
    FDataType: TCefPostDataElementType;
    FValueByte: Pointer;
    FValueStr: TCefString;
    FSize: csize_t;
    FReadOnly: Boolean;
    procedure Clear;
	protected
		// Returns true (1) if this object is read-only.
		function IsReadOnly: Boolean; virtual;
		// Remove all contents from the post data element.
		procedure SetToEmpty; virtual;
		// The post data element will represent a file.
		procedure SetToFile(const aFileName: ustring); virtual;
		// The post data element will represent bytes.  The bytes passed in will be
		// copied.
		procedure SetToBytes(aSize: csize_t; const aBytes: cvoid); virtual;
		// Return the type of this post data element.
		function GetType: TCefPostdataelementType; virtual;
		// Return the file name.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetFile: ustring; virtual;
		// Return the number of bytes.
		function GetBytesCount: csize_t; virtual;
		// Read up to |size| bytes into |bytes| and return the number of bytes
		// actually read.
		function GetBytes(aSize: csize_t; aBytes: cvoid): csize_t; virtual;
	public
		constructor Create(aReadonly: Boolean); virtual;
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
	TCefRequestContextOwn = class(TCefBaseOwn, ICefRequestContext)
	protected
		// Returns true (1) if this object is pointing to the same context as |that|
		// object.
		function IsSame(const aOther: ICefRequestContext): Boolean; virtual;
		// Returns true (1) if this object is sharing the same storage as |that|
		// object.
		function IsSharingWith(const aOther: ICefRequestContext): Boolean; virtual;
		// Returns true (1) if this object is the global context. The global context
		// is used by default when creating a browser or URL request with a NULL
		// context argument.
		function IsGlobal: Boolean; virtual;
		// Returns the handler for this context if any.
		function GetHandler: ICefRequestContextHandler; virtual;
		// Returns the cache path for this object. If NULL an "incognito mode" in-
		// memory cache is being used.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetCachePath: ustring; virtual;
		// Returns the default cookie manager for this object. This will be the global
		// cookie manager if this object is the global request context. Otherwise,
		// this will be the default cookie manager used when this request context does
		// not receive a value via cef_request_tContextHandler::get_cookie_manager().
		// If |callback| is non-NULL it will be executed asnychronously on the IO
		// thread after the manager's storage has been initialized.
		function GetDefaultCookieManager(const aCallback: ICefCompletionCallback): ICefCookieManager; virtual;
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
		function RegisterSchemeHandlerFactory(const aSchemeName: ustring; const aDomainName: ustring; const aFactory: ICefSchemeHandlerFactory): Boolean; virtual;
		// Clear all registered scheme handler factories. Returns false (0) on error.
		// This function may be called on any thread in the browser process.
		function ClearSchemeHandlerFactories: Boolean; virtual;
	public
		constructor Create; virtual;
	end;


	//..............................................................................cef_request_context_handler_capi.h
	// Implement this structure to provide handler implementations. The handler
	// instance will not be released until all objects related to the context have
	// been destroyed.
	TCefRequestContextHandlerOwn = class(TCefBaseOwn, ICefRequestContextHandler)
	protected
		// Called on the IO thread to retrieve the cookie manager. If this function
		// returns NULL the default cookie manager retrievable via
		// cef_request_tContext::get_default_cookie_manager() will be used.
		function GetCookieManager: ICefCookieManager; virtual;
	public
		constructor Create; virtual;
	end;


	//..............................................................................cef_request_handler_capi.h
	// Callback structure used for asynchronous continuation of url requests.
	TCefRequestCallbackOwn = class(TCefBaseOwn, ICefRequestCallback)
	protected
		// Continue the url request. If |allow| is true (1) the request will be
		// continued. Otherwise, the request will be canceled.
		procedure Cont(aAllow: Boolean); virtual;
		// Cancel the url request.
		procedure Cancel; virtual;
	public
		constructor Create; virtual;
	end;

	// Implement this structure to handle events related to browser requests. The
	// functions of this structure will be called on the thread indicated.
	TCefRequestHandlerOwn = class(TCefBaseOwn, ICefRequestHandler)
	protected
		// Called on the UI thread before browser navigation. Return true (1) to
		// cancel the navigation or false (0) to allow the navigation to proceed. The
		// |request| object cannot be modified in this callback.
		// cef_load_handler_t::OnLoadingStateChange will be called twice in all cases.
		// If the navigation is allowed cef_load_handler_t::OnLoadStart and
		// cef_load_handler_t::OnLoadEnd will be called. If the navigation is canceled
		// cef_load_handler_t::OnLoadError will be called with an |errorCode| value of
		// ERR_ABORTED.
		function OnBeforeBrowse(const aBrowser: ICefBrowser; const aFrame: ICefFrame; const aRequest: ICefRequest; aIsRedirect: Boolean): Boolean; virtual;
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
		function OnOpenUrlfromTab(const aBrowser: ICefBrowser; const aFrame: ICefFrame; const aTargetUrl: ustring; aTargetDisposition: TCefWindowOpenDisposition; aUserGesture: Boolean): Boolean; virtual;
		// Called on the IO thread before a resource request is loaded. The |request|
		// object may be modified. Return RV_CONTINUE to continue the request
		// immediately. Return RV_CONTINUE_ASYNC and call cef_request_tCallback::
		// cont() at a later time to continue or cancel the request asynchronously.
		// Return RV_CANCEL to cancel the request immediately.
		//
		function OnBeforeResourceLoad(const aBrowser: ICefBrowser; const aFrame: ICefFrame; const aRequest: ICefRequest; const aCallback: ICefRequestCallback): TCefReturnValue; virtual;
		// Called on the IO thread before a resource is loaded. To allow the resource
		// to load normally return NULL. To specify a handler for the resource return
		// a cef_resource_handler_t object. The |request| object should not be
		// modified in this callback.
		function GetResourceHandler(const aBrowser: ICefBrowser; const aFrame: ICefFrame; const aRequest: ICefRequest): ICefResourceHandler; virtual;
		// Called on the IO thread when a resource load is redirected. The |request|
		// parameter will contain the old URL and other request-related information.
		// The |new_url| parameter will contain the new URL and can be changed if
		// desired. The |request| object cannot be modified in this callback.
		procedure OnResourceRedirect(const aBrowser: ICefBrowser; const aFrame: ICefFrame; const aRequest: ICefRequest; var aNewUrl: ustring); virtual;
		// Called on the IO thread when a resource response is received. To allow the
		// resource to load normally return false (0). To redirect or retry the
		// resource modify |request| (url, headers or post body) and return true (1).
		// The |response| object cannot be modified in this callback.
		function OnResourceResponse(const aBrowser: ICefBrowser; const aFrame: ICefFrame; const aRequest: ICefRequest; const aResponse: ICefResponse): Boolean; virtual;
		// Called on the IO thread when the browser needs credentials from the user.
		// |isProxy| indicates whether the host is a proxy server. |host| contains the
		// hostname and |port| contains the port number. Return true (1) to continue
		// the request and call cef_auth_callback_t::cont() either in this function or
		// at a later time when the authentication information is available. Return
		// false (0) to cancel the request immediately.
		function GetAuthCredentials(const aBrowser: ICefBrowser; const aFrame: ICefFrame; aIsProxy: Boolean; const aHost: ustring; aPort: cint; const aRealm: ustring; const aScheme: ustring; const aCallback: ICefAuthCallback): Boolean; virtual;
		// Called on the IO thread when JavaScript requests a specific storage quota
		// size via the webkitStorageInfo.requestQuota function. |origin_url| is the
		// origin of the page making the request. |new_size| is the requested quota
		// size in bytes. Return true (1) to continue the request and call
		// cef_request_tCallback::cont() either in this function or at a later time to
		// grant or deny the request. Return false (0) to cancel the request
		// immediately.
		function OnQuotaRequest(const aBrowser: ICefBrowser; const aOriginUrl: ustring; aNewSize: cint64; const aCallback: ICefRequestCallback): Boolean; virtual;
		// Called on the UI thread to handle requests for URLs with an unknown
		// protocol component. Set |allow_os_execution| to true (1) to attempt
		// execution via the registered OS protocol handler, if any. SECURITY WARNING:
		// YOU SHOULD USE THIS METHOD TO ENFORCE RESTRICTIONS BASED ON SCHEME, HOST OR
		// OTHER URL ANALYSIS BEFORE ALLOWING OS EXECUTION.
		procedure OnProtocolExecution(const aBrowser: ICefBrowser; const aUrl: ustring; out aAllowOsExecution: Boolean); virtual;
		// Called on the UI thread to handle requests for URLs with an invalid SSL
		// certificate. Return true (1) and call cef_request_tCallback::cont() either
		// in this function or at a later time to continue or cancel the request.
		// Return false (0) to cancel the request immediately. If |callback| is NULL
		// the error cannot be recovered from and the request will be canceled
		// automatically. If CefSettings.ignore_certificate_errors is set all invalid
		// certificates will be accepted without calling this function.
		function OnCertificateError(const aBrowser: ICefBrowser; aCertError: TCefErrorcode; const aRequestUrl: ustring; const aSslInfo: ICefSslinfo; const aCallback: ICefRequestCallback): Boolean; virtual;
		// Called on the browser process IO thread before a plugin is loaded. Return
		// true (1) to block loading of the plugin.
		function OnBeforePluginLoad(const aBrowser: ICefBrowser; const aUrl: ustring; const aPolicyUrl: ustring; const aInfo: ICefWebPluginInfo): Boolean; virtual;
		// Called on the browser process UI thread when a plugin has crashed.
		// |plugin_path| is the path of the plugin that crashed.
		procedure OnPluginCrashed(const aBrowser: ICefBrowser; const aPluginPath: ustring); virtual;
		// Called on the browser process UI thread when the render view associated
		// with |browser| is ready to receive/handle IPC messages in the render
		// process.
		procedure OnRenderViewReady(const aBrowser: ICefBrowser); virtual;
		// Called on the browser process UI thread when the render process terminates
		// unexpectedly. |status| indicates how the process terminated.
		procedure OnRenderProcessTerminated(const aBrowser: ICefBrowser; aStatus: TCefTerminationStatus); virtual;
	public
		constructor Create; virtual;
	end;


	//..............................................................................cef_resource_bundle_handler_capi.h
	// Structure used to implement a custom resource bundle structure. The functions
	// of this structure may be called on multiple threads.
	TCefResourceBundleHandlerOwn = class(TCefBaseOwn, ICefResourceBundleHandler)
	protected
		// Called to retrieve a localized translation for the string specified by
		// |message_id|. To provide the translation set |string| to the translation
		// string and return true (1). To use the default translation return false
		// (0). Supported message IDs are listed in cef_pack_strings.h.
		function GetLocalizedString(aMessageId: cint; out aString: ustring): Boolean; virtual;
		// Called to retrieve data for the resource specified by |resource_id|. To
		// provide the resource data set |data| and |data_size| to the data pointer
		// and size respectively and return true (1). To use the default resource data
		// return false (0). The resource data will not be copied and must remain
		// resident in memory. Supported resource IDs are listed in
		// cef_pack_resources.h.
		function GetDataResource(aResourceId: cint; out aData: cvoid; out aDataSize: csize_t): Boolean; virtual;
	public
		constructor Create; virtual;
	end;


	//..............................................................................cef_resource_handler_capi.h
	// Structure used to implement a custom request handler structure. The functions
	// of this structure will always be called on the IO thread.
	TCefResourceHandlerOwn = class(TCefBaseOwn, ICefResourceHandler)
	protected
		// Begin processing the request. To handle the request return true (1) and
		// call cef_callback_t::cont() once the response header information is
		// available (cef_callback_t::cont() can also be called from inside this
		// function if header information is available immediately). To cancel the
		// request return false (0).
		function ProcessRequest(const aRequest: ICefRequest; const aCallback: ICefCallback): Boolean; virtual;
		// Retrieve response header information. If the response length is not known
		// set |response_length| to -1 and read_response() will be called until it
		// returns false (0). If the response length is known set |response_length| to
		// a positive value and read_response() will be called until it returns false
		// (0) or the specified number of bytes have been read. Use the |response|
		// object to set the mime type, http status code and other optional header
		// values. To redirect the request to a new URL set |redirectUrl| to the new
		// URL.
		procedure GetResponseHeaders(const aResponse: ICefResponse; var aResponseLength: cint64; var aRedirectUrl: ustring); virtual;
		// Read response data. If data is available immediately copy up to
		// |bytes_to_read| bytes into |data_out|, set |bytes_read| to the number of
		// bytes copied, and return true (1). To read the data at a later time set
		// |bytes_read| to 0, return true (1) and call cef_callback_t::cont() when the
		// data is available. To indicate response completion return false (0).
		function ReadResponse(var aDataOut: cvoid; aBytesToRead: cint; var aBytesRead: cint; const aCallback: ICefCallback): Boolean; virtual;
		// Return true (1) if the specified cookie can be sent with the request or
		// false (0) otherwise. If false (0) is returned for any cookie then no
		// cookies will be sent with the request.
		function CanGetCookie(const aCookie: TWACefCookie): Boolean; virtual;
		// Return true (1) if the specified cookie returned with the response can be
		// set or false (0) otherwise.
		function CanSetCookie(const aCookie: TWACefCookie): Boolean; virtual;
		// Request processing has been canceled.
		procedure Cancel; virtual;
	public
		constructor Create; virtual;
	end;


	//..............................................................................cef_response_capi.h
	// Structure used to represent a web response. The functions of this structure
	// may be called on any thread.
	TCefResponseOwn = class(TCefBaseOwn, ICefResponse)
	protected
		// Returns true (1) if this object is read-only.
		function IsReadOnly: Boolean; virtual;
		// Get the response status code.
		function GetStatus: cint; virtual;
		// Set the response status code.
		procedure SetStatus(aStatus: cint); virtual;
		// Get the response status text.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetStatusText: ustring; virtual;
		// Set the response status text.
		procedure SetStatusText(const aStatusText: ustring); virtual;
		// Get the response mime type.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetMimeType: ustring; virtual;
		// Set the response mime type.
		procedure SetMimeType(const aMimeType: ustring); virtual;
		// Get the value for the specified response header field.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetHeader(const aName: ustring): ustring; virtual;
		// Get all response header fields.
		procedure GetHeaderMap(const aHeaderMap: ICefStringMultimap); virtual;
		// Set all response header fields.
		procedure SetHeaderMap(const aHeaderMap: ICefStringMultimap); virtual;
	public
		constructor Create; virtual;
	end;


	//..............................................................................cef_scheme_capi.h
	// Structure that manages custom scheme registrations.
	TCefSchemeRegistrarOwn = class(TCefBaseOwn, ICefSchemeRegistrar)
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
		function AddCustomScheme(const aSchemeName: ustring; aIsStandard: Boolean; aIsLocal: Boolean; aIsDisplayIsolated: Boolean): Boolean; virtual;
	public
		constructor Create; virtual;
	end;

	// Structure that creates cef_resource_handler_t instances for handling scheme
	// requests. The functions of this structure will always be called on the IO
	// thread.
	TCefSchemeHandlerFactoryOwn = class(TCefBaseOwn, ICefSchemeHandlerFactory)
	protected
		// Return a new resource handler instance to handle the request or an NULL
		// reference to allow default handling of the request. |browser| and |frame|
		// will be the browser window and frame respectively that originated the
		// request or NULL if the request did not originate from a browser window (for
		// example, if the request came from cef_urlrequest_t). The |request| object
		// passed to this function will not contain cookie data.
		function New(const aBrowser: ICefBrowser; const aFrame: ICefFrame; const aSchemeName: ustring; const aRequest: ICefRequest): ICefResourceHandler; virtual;
	public
		constructor Create(const AClass: TCefResourceHandlerClass; SyncMainThread: Boolean); virtual;
	end;


	//..............................................................................cef_ssl_info_capi.h
	// Structure representing the issuer or subject field of an X.509 certificate.
	TCefSslcertPrincipalOwn = class(TCefBaseOwn, ICefSslcertPrincipal)
	protected
		// Returns a name that can be used to represent the issuer.  It tries in this
		// order: CN, O and OU and returns the first non-NULL one found.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetDisplayName: ustring; virtual;
		// Returns the common name.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetCommonName: ustring; virtual;
		// Returns the locality name.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetLocalityName: ustring; virtual;
		// Returns the state or province name.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetStateOrProvinceName: ustring; virtual;
		// Returns the country name.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetCountryName: ustring; virtual;
		// Retrieve the list of street addresses.
		procedure GetStreetAddresses(aAddresses: TStrings); virtual;
		// Retrieve the list of organization names.
		procedure GetOrganizationNames(aNames: TStrings); virtual;
		// Retrieve the list of organization unit names.
		procedure GetOrganizationUnitNames(aNames: TStrings); virtual;
		// Retrieve the list of domain components.
		procedure GetDomainComponents(aComponents: TStrings); virtual;
	public
		constructor Create; virtual;
	end;

	// Structure representing SSL information.
	TCefSslinfoOwn = class(TCefBaseOwn, ICefSslinfo)
	protected
		// Returns the subject of the X.509 certificate. For HTTPS server certificates
		// this represents the web server.  The common name of the subject should
		// match the host name of the web server.
		function GetSubject: ICefSslcertPrincipal; virtual;
		// Returns the issuer of the X.509 certificate.
		function GetIssuer: ICefSslcertPrincipal; virtual;
		// Returns the DER encoded serial number for the X.509 certificate. The value
		// possibly includes a leading 00 byte.
		function GetSerialNumber: ICefBinaryValue; virtual;
		// Returns the date before which the X.509 certificate is invalid.
		// CefTime.GetTimeT() will return 0 if no date was specified.
		function GetValidStart: TCefTime; virtual;
		// Returns the date after which the X.509 certificate is invalid.
		// CefTime.GetTimeT() will return 0 if no date was specified.
		function GetValidExpiry: TCefTime; virtual;
		// Returns the DER encoded data for the X.509 certificate.
		function GetDerencoded: ICefBinaryValue; virtual;
		// Returns the PEM encoded data for the X.509 certificate.
		function GetPemencoded: ICefBinaryValue; virtual;
	public
		constructor Create; virtual;
	end;


	//..............................................................................cef_stream_capi.h
	// Structure the client can implement to provide a custom stream reader. The
	// functions of this structure may be called on any thread.
	TCefReadHandlerOwn = class(TCefBaseOwn, ICefReadHandler)
	protected
		// Read raw binary data.
		function Read(aPtr: cvoid; aSize: csize_t; aN: csize_t): csize_t; virtual;
		// Seek to the specified offset position. |whence| may be any one of SEEK_CUR,
		// SEEK_END or SEEK_SET. Return zero on success and non-zero on failure.
		function Seek(aOffset: cint64; aWhence: cint): Boolean; virtual;
		// Return the current offset position.
		function Tell: cint64; virtual;
		// Return non-zero if at end of file.
		function Eof: Boolean; virtual;
		// Return true (1) if this handler performs work like accessing the file
		// system which may block. Used as a hint for determining the thread to access
		// the handler from.
		function MayBlock: Boolean; virtual;
	public
		constructor Create; virtual;
	end;

	// Structure used to read data from a stream. The functions of this structure
	// may be called on any thread.
	TCefStreamReaderOwn = class(TCefBaseOwn, ICefStreamReader)
	protected
		// Read raw binary data.
		function Read(aPtr: cvoid; aSize: csize_t; aN: csize_t): csize_t; virtual;
		// Seek to the specified offset position. |whence| may be any one of SEEK_CUR,
		// SEEK_END or SEEK_SET. Returns zero on success and non-zero on failure.
		function Seek(aOffset: cint64; aWhence: cint): Boolean; virtual;
		// Return the current offset position.
		function Tell: cint64; virtual;
		// Return non-zero if at end of file.
		function Eof: Boolean; virtual;
		// Returns true (1) if this reader performs work like accessing the file
		// system which may block. Used as a hint for determining the thread to access
		// the reader from.
		function MayBlock: Boolean; virtual;
	public
		constructor Create; virtual;
	end;

	// Create a new cef_stream_reader_t object from a file.
	// Create a new cef_stream_reader_t object from data.
	// Create a new cef_stream_reader_t object from a custom handler.
	// Structure the client can implement to provide a custom stream writer. The
	// functions of this structure may be called on any thread.
	TCefWriteHandlerOwn = class(TCefBaseOwn, ICefWriteHandler)
	protected
		// Write raw binary data.
		function Write(aPtr: cvoid; aSize: csize_t; aN: csize_t): csize_t; virtual;
		// Seek to the specified offset position. |whence| may be any one of SEEK_CUR,
		// SEEK_END or SEEK_SET. Return zero on success and non-zero on failure.
		function Seek(aOffset: cint64; aWhence: cint): Boolean; virtual;
		// Return the current offset position.
		function Tell: cint64; virtual;
		// Flush the stream.
		function Flush: Boolean; virtual;
		// Return true (1) if this handler performs work like accessing the file
		// system which may block. Used as a hint for determining the thread to access
		// the handler from.
		function MayBlock: Boolean; virtual;
	public
		constructor Create; virtual;
	end;

	// Structure used to write data to a stream. The functions of this structure may
	// be called on any thread.
	TCefStreamWriterOwn = class(TCefBaseOwn, ICefStreamWriter)
	protected
		// Write raw binary data.
		function Write(aPtr: cvoid; aSize: csize_t; aN: csize_t): csize_t; virtual;
		// Seek to the specified offset position. |whence| may be any one of SEEK_CUR,
		// SEEK_END or SEEK_SET. Returns zero on success and non-zero on failure.
		function Seek(aOffset: cint64; aWhence: cint): Boolean; virtual;
		// Return the current offset position.
		function Tell: cint64; virtual;
		// Flush the stream.
		function Flush: Boolean; virtual;
		// Returns true (1) if this writer performs work like accessing the file
		// system which may block. Used as a hint for determining the thread to access
		// the writer from.
		function MayBlock: Boolean; virtual;
	public
		constructor Create; virtual;
	end;


	//..............................................................................cef_string_visitor_capi.h
	// Implement this structure to receive string values asynchronously.
	TCefStringVisitorOwn = class(TCefBaseOwn, ICefStringVisitor)
	protected
		// Method that will be executed.
		procedure Visit(const aString: ustring); virtual;
	public
		constructor Create; virtual;
	end;


	//..............................................................................cef_task_capi.h
	// Implement this structure for asynchronous task execution. If the task is
	// posted successfully and if the associated message loop is still running then
	// the execute() function will be called on the target thread. If the task fails
	// to post then the task object may be destroyed on the source thread instead of
	// the target thread. For this reason be cautious when performing work in the
	// task object destructor.
	TCefTaskOwn = class(TCefBaseOwn, ICefTask)
	protected
		// Method that will be executed on the target thread.
		procedure Execute; virtual;
	public
		constructor Create; virtual;
	end;

	// Structure that asynchronously executes tasks on the associated thread. It is
	// safe to call the functions of this structure on any thread.
	// CEF maintains multiple internal threads that are used for handling different
	// types of tasks in different processes. The cef_thread_id_t definitions in
	// cef_types.h list the common CEF threads. Task runners are also available for
	// other CEF threads as appropriate (for example, V8 WebWorker threads).
	TCefTaskRunnerOwn = class(TCefBaseOwn, ICefTaskRunner)
	protected
		// Returns true (1) if this object is pointing to the same task runner as
		// |that| object.
		function IsSame(const aThat: ICefTaskRunner): Boolean; virtual;
		// Returns true (1) if this task runner belongs to the current thread.
		function BelongsToCurrentThread: Boolean; virtual;
		// Returns true (1) if this task runner is for the specified CEF thread.
		function BelongsToThread(aThreadId: TCefThreadId): Boolean; virtual;
		// Post a task for execution on the thread associated with this task runner.
		// Execution will occur asynchronously.
		function PostTask(const aTask: ICefTask): Boolean; virtual;
		// Post a task for delayed execution on the thread associated with this task
		// runner. Execution will occur asynchronously. Delayed tasks are not
		// supported on V8 WebWorker threads and will be executed without the
		// specified delay.
		function PostDelayedTask(const aTask: ICefTask; aDelayMs: cint64): Boolean; virtual;
	public
		constructor Create; virtual;
	end;


	//..............................................................................cef_trace_capi.h
	// Implement this structure to receive notification when tracing has completed.
	// The functions of this structure will be called on the browser process UI
	// thread.
	TCefEndTracingCallbackOwn = class(TCefBaseOwn, ICefEndTracingCallback)
	protected
		// Called after all processes have sent their trace data. |tracing_file| is
		// the path at which tracing data was written. The client is responsible for
		// deleting |tracing_file|.
		procedure OnEndTracingComplete(const aTracingFile: ustring); virtual;
	public
		constructor Create; virtual;
	end;


	//..............................................................................cef_urlrequest_capi.h
	// Structure used to make a URL request. URL requests are not associated with a
	// browser instance so no cef_client_t callbacks will be executed. URL requests
	// can be created on any valid CEF thread in either the browser or render
	// process. Once created the functions of the URL request object must be
	// accessed on the same thread that created it.
	TCefUrlrequestOwn = class(TCefBaseOwn, ICefUrlrequest)
	protected
		// Returns the request object used to create this URL request. The returned
		// object is read-only and should not be modified.
		function GetRequest: ICefRequest; virtual;
		// Returns the client.
		function GetClient: ICefUrlrequestClient; virtual;
		// Returns the request status.
		function GetRequestStatus: TCefUrlrequestStatus; virtual;
		// Returns the request error if status is UR_CANCELED or UR_FAILED, or 0
		// otherwise.
		function GetRequestError: TCefErrorcode; virtual;
		// Returns the response, or NULL if no response information is available.
		// Response information will only be available after the upload has completed.
		// The returned object is read-only and should not be modified.
		function GetResponse: ICefResponse; virtual;
		// Cancel the request.
		procedure Cancel; virtual;
	public
		constructor Create; virtual;
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
	TCefUrlrequestClientOwn = class(TCefBaseOwn, ICefUrlrequestClient)
	protected
		// Notifies the client that the request has completed. Use the
		// cef_urlrequest_t::GetRequestStatus function to determine if the request was
		// successful or not.
		procedure OnRequestComplete(const aRequest: ICefUrlrequest); virtual;
		// Notifies the client of upload progress. |current| denotes the number of
		// bytes sent so far and |total| is the total size of uploading data (or -1 if
		// chunked upload is enabled). This function will only be called if the
		// UR_FLAG_REPORT_UPLOAD_PROGRESS flag is set on the request.
		procedure OnUploadProgress(const aRequest: ICefUrlrequest; aCurrent: cint64; aTotal: cint64); virtual;
		// Notifies the client of download progress. |current| denotes the number of
		// bytes received up to the call and |total| is the expected total size of the
		// response (or -1 if not determined).
		procedure OnDownloadProgress(const aRequest: ICefUrlrequest; aCurrent: cint64; aTotal: cint64); virtual;
		// Called when some part of the response is read. |data| contains the current
		// bytes received since the last call. This function will not be called if the
		// UR_FLAG_NO_DOWNLOAD_DATA flag is set on the request.
		procedure OnDownloadData(const aRequest: ICefUrlrequest; const aData: cvoid; aDataLength: csize_t); virtual;
		// Called on the IO thread when the browser needs credentials from the user.
		// |isProxy| indicates whether the host is a proxy server. |host| contains the
		// hostname and |port| contains the port number. Return true (1) to continue
		// the request and call cef_auth_callback_t::cont() when the authentication
		// information is available. Return false (0) to cancel the request. This
		// function will only be called for requests initiated from the browser
		// process.
		function GetAuthCredentials(aIsProxy: Boolean; const aHost: ustring; aPort: cint; const aRealm: ustring; const aScheme: ustring; const aCallback: ICefAuthCallback): Boolean; virtual;
	public
		constructor Create; virtual;
	end;


	//..............................................................................cef_v8_capi.h
	// Structure representing a V8 context handle. V8 handles can only be accessed
	// from the thread on which they are created. Valid threads for creating a V8
	// handle include the render process main thread (TID_RENDERER) and WebWorker
	// threads. A task runner for posting tasks on the associated thread can be
	// retrieved via the cef_v8context_t::get_task_runner() function.
	TCefV8contextOwn = class(TCefBaseOwn, ICefV8context)
	protected
		// Returns the task runner associated with this context. V8 handles can only
		// be accessed from the thread on which they are created. This function can be
		// called on any render process thread.
		function GetTaskRunner: ICefTaskRunner; virtual;
		// Returns true (1) if the underlying handle is valid and it can be accessed
		// on the current thread. Do not call any other functions if this function
		// returns false (0).
		function IsValid: Boolean; virtual;
		// Returns the browser for this context. This function will return an NULL
		// reference for WebWorker contexts.
		function GetBrowser: ICefBrowser; virtual;
		// Returns the frame for this context. This function will return an NULL
		// reference for WebWorker contexts.
		function GetFrame: ICefFrame; virtual;
		// Returns the global object for this context. The context must be entered
		// before calling this function.
		function GetGlobal: ICefV8value; virtual;
		// Enter this context. A context must be explicitly entered before creating a
		// V8 Object, Array, Function or Date asynchronously. exit() must be called
		// the same number of times as enter() before releasing this context. V8
		// objects belong to the context in which they are created. Returns true (1)
		// if the scope was entered successfully.
		function Enter: Boolean; virtual;
		// Exit this context. Call this function only after calling enter(). Returns
		// true (1) if the scope was exited successfully.
		function Exit: Boolean; virtual;
		// Returns true (1) if this object is pointing to the same handle as |that|
		// object.
		function IsSame(const aThat: ICefV8context): Boolean; virtual;
		// Evaluates the specified JavaScript code using this context's global object.
		// On success |retval| will be set to the return value, if any, and the
		// function will return true (1). On failure |exception| will be set to the
		// exception, if any, and the function will return false (0).
		function Eval(const aCode: ustring; var aRetval: ICefV8value; var aException: ICefV8exception): Boolean; virtual;
	public
		constructor Create; virtual;
	end;

	// Returns the current (top) context object in the V8 context stack.
	// Returns the entered (bottom) context object in the V8 context stack.
	// Returns true (1) if V8 is currently inside a context.
	// Structure that should be implemented to handle V8 function calls. The
	// functions of this structure will be called on the thread associated with the
	// V8 function.
	TCefV8handlerOwn = class(TCefBaseOwn, ICefV8handler)
	protected
		// Handle execution of the function identified by |name|. |object| is the
		// receiver ('this' object) of the function. |arguments| is the list of
		// arguments passed to the function. If execution succeeds set |retval| to the
		// function return value. If execution fails set |exception| to the exception
		// that will be thrown. Return true (1) if execution was handled.
		function Execute(const aName: ustring; const aObject: ICefV8value; aArgumentsCount: csize_t; const aArguments: TCefv8ValueArray; var aRetval: ICefV8value; var aException: ustring): Boolean; virtual;
	public
		constructor Create; virtual;
	end;

	// Structure that should be implemented to handle V8 accessor calls. Accessor
	// identifiers are registered by calling cef_v8value_t::set_value(). The
	// functions of this structure will be called on the thread associated with the
	// V8 accessor.
	TCefV8accessorOwn = class(TCefBaseOwn, ICefV8accessor)
	protected
		// Handle retrieval the accessor value identified by |name|. |object| is the
		// receiver ('this' object) of the accessor. If retrieval succeeds set
		// |retval| to the return value. If retrieval fails set |exception| to the
		// exception that will be thrown. Return true (1) if accessor retrieval was
		// handled.
		function Get(const aName: ustring; const aObject: ICefV8value; out aRetval: ICefV8value; const aException: ustring): Boolean; virtual;
		// Handle assignment of the accessor value identified by |name|. |object| is
		// the receiver ('this' object) of the accessor. |value| is the new value
		// being assigned to the accessor. If assignment fails set |exception| to the
		// exception that will be thrown. Return true (1) if accessor assignment was
		// handled.
		function _Set(const aName: ustring; const aObject: ICefV8value; const aValue: ICefV8value; const aException: ustring): Boolean; virtual;
	public
		constructor Create; virtual;
	end;

	// Structure representing a V8 exception. The functions of this structure may be
	// called on any render process thread.
	TCefV8exceptionOwn = class(TCefBaseOwn, ICefV8exception)
  private
    FMessage: ustring;
    FSourceLine: ustring;
    FScriptResourceName: ustring;
    FLineNumber: cint;
    FStartPosition: cint;
    FEndPosition: cint;
    FStartColumn: cint;
    FEndColumn: cint;
	protected
		// Returns the exception message.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetMessage: ustring; virtual;
		// Returns the line of source code that the exception occurred within.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetSourceLine: ustring; virtual;
		// Returns the resource name for the script from where the function causing
		// the error originates.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetScriptResourceName: ustring; virtual;
		// Returns the 1-based number of the line where the error occurred or 0 if the
		// line number is unknown.
		function GetLineNumber: cint; virtual;
		// Returns the index within the script of the first character where the error
		// occurred.
		function GetStartPosition: cint; virtual;
		// Returns the index within the script of the last character where the error
		// occurred.
		function GetEndPosition: cint; virtual;
		// Returns the index within the line of the first character where the error
		// occurred.
		function GetStartColumn: cint; virtual;
		// Returns the index within the line of the last character where the error
		// occurred.
		function GetEndColumn: cint; virtual;
	public
		constructor Create; virtual;
	end;

	// Structure representing a V8 value handle. V8 handles can only be accessed
	// from the thread on which they are created. Valid threads for creating a V8
	// handle include the render process main thread (TID_RENDERER) and WebWorker
	// threads. A task runner for posting tasks on the associated thread can be
	// retrieved via the cef_v8context_t::get_task_runner() function.
	TCefV8valueOwn = class(TCefBaseOwn, ICefV8value)
	protected
		// Returns true (1) if the underlying handle is valid and it can be accessed
		// on the current thread. Do not call any other functions if this function
		// returns false (0).
		function IsValid: Boolean; virtual;
		// True if the value type is undefined.
		function IsUndefined: Boolean; virtual;
		// True if the value type is null.
		function IsNull: Boolean; virtual;
		// True if the value type is bool.
		function IsBool: Boolean; virtual;
		// True if the value type is int.
		function IsInt: Boolean; virtual;
		// True if the value type is unsigned int.
		function IsUint: Boolean; virtual;
		// True if the value type is double.
		function IsDouble: Boolean; virtual;
		// True if the value type is Date.
		function IsDate: Boolean; virtual;
		// True if the value type is string.
		function IsString: Boolean; virtual;
		// True if the value type is object.
		function IsObject: Boolean; virtual;
		// True if the value type is array.
		function IsArray: Boolean; virtual;
		// True if the value type is function.
		function IsFunction: Boolean; virtual;
		// Returns true (1) if this object is pointing to the same handle as |that|
		// object.
		function IsSame(const aThat: ICefV8value): Boolean; virtual;
		// Return a bool value.  The underlying data will be converted to if
		// necessary.
		function GetBoolValue: Boolean; virtual;
		// Return an int value.  The underlying data will be converted to if
		// necessary.
		function GetIntValue: cint32; virtual;
		// Return an unisgned int value.  The underlying data will be converted to if
		// necessary.
		function GetUintValue: cuint32; virtual;
		// Return a double value.  The underlying data will be converted to if
		// necessary.
		function GetDoubleValue: cdouble; virtual;
		// Return a Date value.  The underlying data will be converted to if
		// necessary.
		function GetDateValue: TCefTime; virtual;
		// Return a string value.  The underlying data will be converted to if
		// necessary.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetStringValue: ustring; virtual;
		// Returns true (1) if this is a user created object.
		function IsUserCreated: Boolean; virtual;
		// Returns true (1) if the last function call resulted in an exception. This
		// attribute exists only in the scope of the current CEF value object.
		function HasException: Boolean; virtual;
		// Returns the exception resulting from the last function call. This attribute
		// exists only in the scope of the current CEF value object.
		function GetException: ICefV8exception; virtual;
		// Clears the last exception and returns true (1) on success.
		function ClearException: Boolean; virtual;
		// Returns true (1) if this object will re-throw future exceptions. This
		// attribute exists only in the scope of the current CEF value object.
		function WillRethrowExceptions: Boolean; virtual;
		// Set whether this object will re-throw future exceptions. By default
		// exceptions are not re-thrown. If a exception is re-thrown the current
		// context should not be accessed again until after the exception has been
		// caught and not re-thrown. Returns true (1) on success. This attribute
		// exists only in the scope of the current CEF value object.
		function SetRethrowExceptions(aRethrow: Boolean): Boolean; virtual;
		// Returns true (1) if the object has a value with the specified identifier.
		function HasValueBykey(const aKey: ustring): Boolean; virtual;
		// Returns true (1) if the object has a value with the specified identifier.
		function HasValueByindex(aIndex: cint): Boolean; virtual;
		// Deletes the value with the specified identifier and returns true (1) on
		// success. Returns false (0) if this function is called incorrectly or an
		// exception is thrown. For read-only and don't-delete values this function
		// will return true (1) even though deletion failed.
		function DeleteValueBykey(const aKey: ustring): Boolean; virtual;
		// Deletes the value with the specified identifier and returns true (1) on
		// success. Returns false (0) if this function is called incorrectly, deletion
		// fails or an exception is thrown. For read-only and don't-delete values this
		// function will return true (1) even though deletion failed.
		function DeleteValueByindex(aIndex: cint): Boolean; virtual;
		// Returns the value with the specified identifier on success. Returns NULL if
		// this function is called incorrectly or an exception is thrown.
		function GetValueBykey(const aKey: ustring): ICefV8value; virtual;
		// Returns the value with the specified identifier on success. Returns NULL if
		// this function is called incorrectly or an exception is thrown.
		function GetValueByindex(aIndex: cint): ICefV8value; virtual;
		// Associates a value with the specified identifier and returns true (1) on
		// success. Returns false (0) if this function is called incorrectly or an
		// exception is thrown. For read-only values this function will return true
		// (1) even though assignment failed.
		function SetValueBykey(const aKey: ustring; const aValue: ICefV8value; aAttribute: TCefV8Propertyattribute): Boolean; virtual;
		// Associates a value with the specified identifier and returns true (1) on
		// success. Returns false (0) if this function is called incorrectly or an
		// exception is thrown. For read-only values this function will return true
		// (1) even though assignment failed.
		function SetValueByindex(aIndex: cint; const aValue: ICefV8value): Boolean; virtual;
		// Registers an identifier and returns true (1) on success. Access to the
		// identifier will be forwarded to the cef_v8accessor_t instance passed to
		// cef_v8value_t::cef_v8value_create_object(). Returns false (0) if this
		// function is called incorrectly or an exception is thrown. For read-only
		// values this function will return true (1) even though assignment failed.
		function SetValueByaccessor(const aKey: ustring; aSettings: TCefV8Accesscontrol; aAttribute: TCefV8Propertyattribute): Boolean; virtual;
		// Read the keys for the object's values into the specified vector. Integer-
		// based keys will also be returned as strings.
		function GetKeys(aKeys: TStrings): Boolean; virtual;
		// Sets the user data for this object and returns true (1) on success. Returns
		// false (0) if this function is called incorrectly. This function can only be
		// called on user created objects.
		function SetUserData(const aUserData: ICefBase): Boolean; virtual;
		// Returns the user data, if any, assigned to this object.
		function GetUserData: ICefBase; virtual;
		// Returns the amount of externally allocated memory registered for the
		// object.
		function GetExternallyAllocatedMemory: cint; virtual;
		// Adjusts the amount of registered external memory for the object. Used to
		// give V8 an indication of the amount of externally allocated memory that is
		// kept alive by JavaScript objects. V8 uses this information to decide when
		// to perform global garbage collection. Each cef_v8value_t tracks the amount
		// of external memory associated with it and automatically decreases the
		// global total by the appropriate amount on its destruction.
		// |change_in_bytes| specifies the number of bytes to adjust by. This function
		// returns the number of bytes associated with the object after the
		// adjustment. This function can only be called on user created objects.
		function AdjustExternallyAllocatedMemory(aChangeInBytes: cint): cint; virtual;
		// Returns the number of elements in the array.
		function GetArrayLength: cint; virtual;
		// Returns the function name.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetFunctionName: ustring; virtual;
		// Returns the function handler or NULL if not a CEF-created function.
		function GetFunctionHandler: ICefV8handler; virtual;
		// Execute the function using the current V8 context. This function should
		// only be called from within the scope of a cef_v8handler_t or
		// cef_v8accessor_t callback, or in combination with calling enter() and
		// exit() on a stored cef_v8context_t reference. |object| is the receiver
		// ('this' object) of the function. If |object| is NULL the current context's
		// global object will be used. |arguments| is the list of arguments that will
		// be passed to the function. Returns the function return value on success.
		// Returns NULL if this function is called incorrectly or an exception is
		// thrown.
		function ExecuteFunction(const aObject: ICefV8value; aArgumentsCount: csize_t; const aArguments: TCefv8ValueArray): ICefV8value; virtual;
		// Execute the function using the specified V8 context. |object| is the
		// receiver ('this' object) of the function. If |object| is NULL the specified
		// context's global object will be used. |arguments| is the list of arguments
		// that will be passed to the function. Returns the function return value on
		// success. Returns NULL if this function is called incorrectly or an
		// exception is thrown.
		function ExecuteFunctionWithContext(const aContext: ICefV8context; const aObject: ICefV8value; aArgumentsCount: csize_t; const aArguments: TCefv8ValueArray): ICefV8Value; virtual;
	public
		constructor Create; virtual;
	end;

	// Create a new cef_v8value_t object of type undefined.
	// Create a new cef_v8value_t object of type null.
	// Create a new cef_v8value_t object of type bool.
	// Create a new cef_v8value_t object of type int.
	// Create a new cef_v8value_t object of type unsigned int.
	// Create a new cef_v8value_t object of type double.
	// Create a new cef_v8value_t object of type Date. This function should only be
	// called from within the scope of a cef_render_process_handler_t,
	// cef_v8handler_t or cef_v8accessor_t callback, or in combination with calling
	// enter() and exit() on a stored cef_v8context_t reference.
	// Create a new cef_v8value_t object of type string.
	// Create a new cef_v8value_t object of type object with optional accessor. This
	// function should only be called from within the scope of a
	// cef_render_process_handler_t, cef_v8handler_t or cef_v8accessor_t callback,
	// or in combination with calling enter() and exit() on a stored cef_v8context_t
	// reference.
	// Create a new cef_v8value_t object of type array with the specified |length|.
	// If |length| is negative the returned array will have length 0. This function
	// should only be called from within the scope of a
	// cef_render_process_handler_t, cef_v8handler_t or cef_v8accessor_t callback,
	// or in combination with calling enter() and exit() on a stored cef_v8context_t
	// reference.
	// Create a new cef_v8value_t object of type function. This function should only
	// be called from within the scope of a cef_render_process_handler_t,
	// cef_v8handler_t or cef_v8accessor_t callback, or in combination with calling
	// enter() and exit() on a stored cef_v8context_t reference.
	// Structure representing a V8 stack trace handle. V8 handles can only be
	// accessed from the thread on which they are created. Valid threads for
	// creating a V8 handle include the render process main thread (TID_RENDERER)
	// and WebWorker threads. A task runner for posting tasks on the associated
	// thread can be retrieved via the cef_v8context_t::get_task_runner() function.
	TCefV8stackTraceOwn = class(TCefBaseOwn, ICefV8stackTrace)
	protected
		// Returns true (1) if the underlying handle is valid and it can be accessed
		// on the current thread. Do not call any other functions if this function
		// returns false (0).
		function IsValid: Boolean; virtual;
		// Returns the number of stack frames.
		function GetFrameCount: cint; virtual;
		// Returns the stack frame at the specified 0-based index.
		function GetFrame(aIndex: cint): ICefV8stackFrame; virtual;
	public
		constructor Create; virtual;
	end;

	// Returns the stack trace for the currently active context. |frame_limit| is
	// the maximum number of frames that will be captured.
	// Structure representing a V8 stack frame handle. V8 handles can only be
	// accessed from the thread on which they are created. Valid threads for
	// creating a V8 handle include the render process main thread (TID_RENDERER)
	// and WebWorker threads. A task runner for posting tasks on the associated
	// thread can be retrieved via the cef_v8context_t::get_task_runner() function.
	TCefV8stackFrameOwn = class(TCefBaseOwn, ICefV8stackFrame)
	protected
		// Returns true (1) if the underlying handle is valid and it can be accessed
		// on the current thread. Do not call any other functions if this function
		// returns false (0).
		function IsValid: Boolean; virtual;
		// Returns the name of the resource script that contains the function.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetScriptName: ustring; virtual;
		// Returns the name of the resource script that contains the function or the
		// sourceURL value if the script name is undefined and its source ends with a
		// "//@ sourceURL=..." string.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetScriptNameOrSourceUrl: ustring; virtual;
		// Returns the name of the function.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetFunctionName: ustring; virtual;
		// Returns the 1-based line number for the function call or 0 if unknown.
		function GetLineNumber: cint; virtual;
		// Returns the 1-based column offset on the line for the function call or 0 if
		// unknown.
		function GetColumn: cint; virtual;
		// Returns true (1) if the function was compiled using eval().
		function IsEval: Boolean; virtual;
		// Returns true (1) if the function was called as a constructor via "new".
		function IsConstructor: Boolean; virtual;
	public
		constructor Create; virtual;
	end;


	//..............................................................................cef_values_capi.h
	// Structure that wraps other data value types. Complex types (binary,
	// dictionary and list) will be referenced but not owned by this object. Can be
	// used on any process and thread.
	TCefValueOwn = class(TCefBaseOwn, ICefValue)
	protected
		// Returns true (1) if the underlying data is valid. This will always be true
		// (1) for simple types. For complex types (binary, dictionary and list) the
		// underlying data may become invalid if owned by another object (e.g. list or
		// dictionary) and that other object is then modified or destroyed. This value
		// object can be re-used by calling Set*() even if the underlying data is
		// invalid.
		function IsValid: Boolean; virtual;
		// Returns true (1) if the underlying data is owned by another object.
		function IsOwned: Boolean; virtual;
		// Returns true (1) if the underlying data is read-only. Some APIs may expose
		// read-only objects.
		function IsReadOnly: Boolean; virtual;
		// Returns true (1) if this object and |that| object have the same underlying
		// data. If true (1) modifications to this object will also affect |that|
		// object and vice-versa.
		function IsSame(const aThat: ICefValue): Boolean; virtual;
		// Returns true (1) if this object and |that| object have an equivalent
		// underlying value but are not necessarily the same object.
		function IsEqual(const aThat: ICefValue): Boolean; virtual;
		// Returns a copy of this object. The underlying data will also be copied.
		function Copy: ICefValue; virtual;
		// Returns the underlying value type.
		function GetType: TCefValueType; virtual;
		// Returns the underlying value as type bool.
		function GetBool: Boolean; virtual;
		// Returns the underlying value as type int.
		function GetInt: cint; virtual;
		// Returns the underlying value as type double.
		function GetDouble: cdouble; virtual;
		// Returns the underlying value as type string.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetString: ustring; virtual;
		// Returns the underlying value as type binary. The returned reference may
		// become invalid if the value is owned by another object or if ownership is
		// transferred to another object in the future. To maintain a reference to the
		// value after assigning ownership to a dictionary or list pass this object to
		// the set_value() function instead of passing the returned reference to
		// set_binary().
		function GetBinary: ICefBinaryValue; virtual;
		// Returns the underlying value as type dictionary. The returned reference may
		// become invalid if the value is owned by another object or if ownership is
		// transferred to another object in the future. To maintain a reference to the
		// value after assigning ownership to a dictionary or list pass this object to
		// the set_value() function instead of passing the returned reference to
		// set_dictionary().
		function GetDictionary: ICefDictionaryValue; virtual;
		// Returns the underlying value as type list. The returned reference may
		// become invalid if the value is owned by another object or if ownership is
		// transferred to another object in the future. To maintain a reference to the
		// value after assigning ownership to a dictionary or list pass this object to
		// the set_value() function instead of passing the returned reference to
		// set_list().
		function GetList: ICefListValue; virtual;
		// Sets the underlying value as type null. Returns true (1) if the value was
		// set successfully.
		function SetNull: Boolean; virtual;
		// Sets the underlying value as type bool. Returns true (1) if the value was
		// set successfully.
		function SetBool(aValue: Boolean): Boolean; virtual;
		// Sets the underlying value as type int. Returns true (1) if the value was
		// set successfully.
		function SetInt(aValue: cint): Boolean; virtual;
		// Sets the underlying value as type double. Returns true (1) if the value was
		// set successfully.
		function SetDouble(aValue: cdouble): Boolean; virtual;
		// Sets the underlying value as type string. Returns true (1) if the value was
		// set successfully.
		function SetString(const aValue: ustring): Boolean; virtual;
		// Sets the underlying value as type binary. Returns true (1) if the value was
		// set successfully. This object keeps a reference to |value| and ownership of
		// the underlying data remains unchanged.
		function SetBinary(const aValue: ICefBinaryValue): Boolean; virtual;
		// Sets the underlying value as type dict. Returns true (1) if the value was
		// set successfully. This object keeps a reference to |value| and ownership of
		// the underlying data remains unchanged.
		function SetDictionary(const aValue: ICefDictionaryValue): Boolean; virtual;
		// Sets the underlying value as type list. Returns true (1) if the value was
		// set successfully. This object keeps a reference to |value| and ownership of
		// the underlying data remains unchanged.
		function SetList(const aValue: ICefListValue): Boolean; virtual;
	public
		constructor Create; virtual;
	end;

	// Creates a new object.
	// Structure representing a binary value. Can be used on any process and thread.
	TCefBinaryValueOwn = class(TCefBaseOwn, ICefBinaryValue)
	protected
		// Returns true (1) if this object is valid. This object may become invalid if
		// the underlying data is owned by another object (e.g. list or dictionary)
		// and that other object is then modified or destroyed. Do not call any other
		// functions if this function returns false (0).
		function IsValid: Boolean; virtual;
		// Returns true (1) if this object is currently owned by another object.
		function IsOwned: Boolean; virtual;
		// Returns true (1) if this object and |that| object have the same underlying
		// data.
		function IsSame(const aThat: ICefBinaryValue): Boolean; virtual;
		// Returns true (1) if this object and |that| object have an equivalent
		// underlying value but are not necessarily the same object.
		function IsEqual(const aThat: ICefBinaryValue): Boolean; virtual;
		// Returns a copy of this object. The data in this object will also be copied.
		function Copy: ICefBinaryValue; virtual;
		// Returns the data size.
		function GetSize: csize_t; virtual;
		// Read up to |buffer_size| number of bytes into |buffer|. Reading begins at
		// the specified byte |data_offset|. Returns the number of bytes read.
		function GetData(aBuffer: cvoid; aBufferSize: csize_t; aDataOffset: csize_t): csize_t; virtual;
	public
		constructor Create; virtual;
	end;

	// Creates a new object that is not owned by any other object. The specified
	// |data| will be copied.
	// Structure representing a dictionary value. Can be used on any process and
	// thread.
	TCefDictionaryValueOwn = class(TCefBaseOwn, ICefDictionaryValue)
	protected
		// Returns true (1) if this object is valid. This object may become invalid if
		// the underlying data is owned by another object (e.g. list or dictionary)
		// and that other object is then modified or destroyed. Do not call any other
		// functions if this function returns false (0).
		function IsValid: Boolean; virtual;
		// Returns true (1) if this object is currently owned by another object.
		function IsOwned: Boolean; virtual;
		// Returns true (1) if the values of this object are read-only. Some APIs may
		// expose read-only objects.
		function IsReadOnly: Boolean; virtual;
		// Returns true (1) if this object and |that| object have the same underlying
		// data. If true (1) modifications to this object will also affect |that|
		// object and vice-versa.
		function IsSame(const aThat: ICefDictionaryValue): Boolean; virtual;
		// Returns true (1) if this object and |that| object have an equivalent
		// underlying value but are not necessarily the same object.
		function IsEqual(const aThat: ICefDictionaryValue): Boolean; virtual;
		// Returns a writable copy of this object. If |exclude_NULL_children| is true
		// (1) any NULL dictionaries or lists will be excluded from the copy.
		function Copy(aExcludeEmptyChildren: Boolean): ICefDictionaryValue; virtual;
		// Returns the number of values.
		function GetSize: csize_t; virtual;
		// Removes all values. Returns true (1) on success.
		function Clear: Boolean; virtual;
		// Returns true (1) if the current dictionary has a value for the given key.
		function HasKey(const aKey: ustring): Boolean; virtual;
		// Reads all keys for this dictionary into the specified vector.
		function GetKeys(aKeys: TStrings): Boolean; virtual;
		// Removes the value at the specified key. Returns true (1) is the value was
		// removed successfully.
		function Remove(const aKey: ustring): Boolean; virtual;
		// Returns the value type for the specified key.
		function GetType(const aKey: ustring): TCefValueType; virtual;
		// Returns the value at the specified key. For simple types the returned value
		// will copy existing data and modifications to the value will not modify this
		// object. For complex types (binary, dictionary and list) the returned value
		// will reference existing data and modifications to the value will modify
		// this object.
		function GetValue(const aKey: ustring): ICefValue; virtual;
		// Returns the value at the specified key as type bool.
		function GetBool(const aKey: ustring): Boolean; virtual;
		// Returns the value at the specified key as type int.
		function GetInt(const aKey: ustring): cint; virtual;
		// Returns the value at the specified key as type double.
		function GetDouble(const aKey: ustring): cdouble; virtual;
		// Returns the value at the specified key as type string.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetString(const aKey: ustring): ustring; virtual;
		// Returns the value at the specified key as type binary. The returned value
		// will reference existing data.
		function GetBinary(const aKey: ustring): ICefBinaryValue; virtual;
		// Returns the value at the specified key as type dictionary. The returned
		// value will reference existing data and modifications to the value will
		// modify this object.
		function GetDictionary(const aKey: ustring): ICefDictionaryValue; virtual;
		// Returns the value at the specified key as type list. The returned value
		// will reference existing data and modifications to the value will modify
		// this object.
		function GetList(const aKey: ustring): ICefListValue; virtual;
		// Sets the value at the specified key. Returns true (1) if the value was set
		// successfully. If |value| represents simple data then the underlying data
		// will be copied and modifications to |value| will not modify this object. If
		// |value| represents complex data (binary, dictionary or list) then the
		// underlying data will be referenced and modifications to |value| will modify
		// this object.
		function SetValue(const aKey: ustring; const aValue: ICefValue): Boolean; virtual;
		// Sets the value at the specified key as type null. Returns true (1) if the
		// value was set successfully.
		function SetNull(const aKey: ustring): Boolean; virtual;
		// Sets the value at the specified key as type bool. Returns true (1) if the
		// value was set successfully.
		function SetBool(const aKey: ustring; aValue: Boolean): Boolean; virtual;
		// Sets the value at the specified key as type int. Returns true (1) if the
		// value was set successfully.
		function SetInt(const aKey: ustring; aValue: cint): Boolean; virtual;
		// Sets the value at the specified key as type double. Returns true (1) if the
		// value was set successfully.
		function SetDouble(const aKey: ustring; aValue: cdouble): Boolean; virtual;
		// Sets the value at the specified key as type string. Returns true (1) if the
		// value was set successfully.
		function SetString(const aKey: ustring; const aValue: ustring): Boolean; virtual;
		// Sets the value at the specified key as type binary. Returns true (1) if the
		// value was set successfully. If |value| is currently owned by another object
		// then the value will be copied and the |value| reference will not change.
		// Otherwise, ownership will be transferred to this object and the |value|
		// reference will be invalidated.
		function SetBinary(const aKey: ustring; const aValue: ICefBinaryValue): Boolean; virtual;
		// Sets the value at the specified key as type dict. Returns true (1) if the
		// value was set successfully. If |value| is currently owned by another object
		// then the value will be copied and the |value| reference will not change.
		// Otherwise, ownership will be transferred to this object and the |value|
		// reference will be invalidated.
		function SetDictionary(const aKey: ustring; const aValue: ICefDictionaryValue): Boolean; virtual;
		// Sets the value at the specified key as type list. Returns true (1) if the
		// value was set successfully. If |value| is currently owned by another object
		// then the value will be copied and the |value| reference will not change.
		// Otherwise, ownership will be transferred to this object and the |value|
		// reference will be invalidated.
		function SetList(const aKey: ustring; const aValue: ICefListValue): Boolean; virtual;
	public
		constructor Create; virtual;
	end;

	// Creates a new object that is not owned by any other object.
	// Structure representing a list value. Can be used on any process and thread.
	TCefListValueOwn = class(TCefBaseOwn, ICefListValue)
	protected
		// Returns true (1) if this object is valid. This object may become invalid if
		// the underlying data is owned by another object (e.g. list or dictionary)
		// and that other object is then modified or destroyed. Do not call any other
		// functions if this function returns false (0).
		function IsValid: Boolean; virtual;
		// Returns true (1) if this object is currently owned by another object.
		function IsOwned: Boolean; virtual;
		// Returns true (1) if the values of this object are read-only. Some APIs may
		// expose read-only objects.
		function IsReadOnly: Boolean; virtual;
		// Returns true (1) if this object and |that| object have the same underlying
		// data. If true (1) modifications to this object will also affect |that|
		// object and vice-versa.
		function IsSame(const aThat: ICefListValue): Boolean; virtual;
		// Returns true (1) if this object and |that| object have an equivalent
		// underlying value but are not necessarily the same object.
		function IsEqual(const aThat: ICefListValue): Boolean; virtual;
		// Returns a writable copy of this object.
		function Copy: ICefListValue; virtual;
		// Sets the number of values. If the number of values is expanded all new
		// value slots will default to type null. Returns true (1) on success.
		function SetSize(aSize: csize_t): Boolean; virtual;
		// Returns the number of values.
		function GetSize: csize_t; virtual;
		// Removes all values. Returns true (1) on success.
		function Clear: Boolean; virtual;
		// Removes the value at the specified index.
		function Remove(aIndex: cint): Boolean; virtual;
		// Returns the value type at the specified index.
		function GetType(aIndex: cint): TCefValueType; virtual;
		// Returns the value at the specified index. For simple types the returned
		// value will copy existing data and modifications to the value will not
		// modify this object. For complex types (binary, dictionary and list) the
		// returned value will reference existing data and modifications to the value
		// will modify this object.
		function GetValue(aIndex: cint): ICefValue; virtual;
		// Returns the value at the specified index as type bool.
		function GetBool(aIndex: cint): Boolean; virtual;
		// Returns the value at the specified index as type int.
		function GetInt(aIndex: cint): cint; virtual;
		// Returns the value at the specified index as type double.
		function GetDouble(aIndex: cint): cdouble; virtual;
		// Returns the value at the specified index as type string.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetString(aIndex: cint): ustring; virtual;
		// Returns the value at the specified index as type binary. The returned value
		// will reference existing data.
		function GetBinary(aIndex: cint): ICefBinaryValue; virtual;
		// Returns the value at the specified index as type dictionary. The returned
		// value will reference existing data and modifications to the value will
		// modify this object.
		function GetDictionary(aIndex: cint): ICefDictionaryValue; virtual;
		// Returns the value at the specified index as type list. The returned value
		// will reference existing data and modifications to the value will modify
		// this object.
		function GetList(aIndex: cint): ICefListValue; virtual;
		// Sets the value at the specified index. Returns true (1) if the value was
		// set successfully. If |value| represents simple data then the underlying
		// data will be copied and modifications to |value| will not modify this
		// object. If |value| represents complex data (binary, dictionary or list)
		// then the underlying data will be referenced and modifications to |value|
		// will modify this object.
		function SetValue(aIndex: cint; const aValue: ICefValue): Boolean; virtual;
		// Sets the value at the specified index as type null. Returns true (1) if the
		// value was set successfully.
		function SetNull(aIndex: cint): Boolean; virtual;
		// Sets the value at the specified index as type bool. Returns true (1) if the
		// value was set successfully.
		function SetBool(aIndex: cint; aValue: Boolean): Boolean; virtual;
		// Sets the value at the specified index as type int. Returns true (1) if the
		// value was set successfully.
		function SetInt(aIndex: cint; aValue: cint): Boolean; virtual;
		// Sets the value at the specified index as type double. Returns true (1) if
		// the value was set successfully.
		function SetDouble(aIndex: cint; aValue: cdouble): Boolean; virtual;
		// Sets the value at the specified index as type string. Returns true (1) if
		// the value was set successfully.
		function SetString(aIndex: cint; const aValue: ustring): Boolean; virtual;
		// Sets the value at the specified index as type binary. Returns true (1) if
		// the value was set successfully. If |value| is currently owned by another
		// object then the value will be copied and the |value| reference will not
		// change. Otherwise, ownership will be transferred to this object and the
		// |value| reference will be invalidated.
		function SetBinary(aIndex: cint; const aValue: ICefBinaryValue): Boolean; virtual;
		// Sets the value at the specified index as type dict. Returns true (1) if the
		// value was set successfully. If |value| is currently owned by another object
		// then the value will be copied and the |value| reference will not change.
		// Otherwise, ownership will be transferred to this object and the |value|
		// reference will be invalidated.
		function SetDictionary(aIndex: cint; const aValue: ICefDictionaryValue): Boolean; virtual;
		// Sets the value at the specified index as type list. Returns true (1) if the
		// value was set successfully. If |value| is currently owned by another object
		// then the value will be copied and the |value| reference will not change.
		// Otherwise, ownership will be transferred to this object and the |value|
		// reference will be invalidated.
		function SetList(aIndex: cint; const aValue: ICefListValue): Boolean; virtual;
	public
		constructor Create; virtual;
	end;


	//..............................................................................cef_web_plugin_capi.h
	// Information about a specific web plugin.
	TCefWebPluginInfoOwn = class(TCefBaseOwn, ICefWebPluginInfo)
	protected
		// Returns the plugin name (i.e. Flash).
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetName: ustring; virtual;
		// Returns the plugin file path (DLL/bundle/library).
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetPath: ustring; virtual;
		// Returns the version of the plugin (may be OS-specific).
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetVersion: ustring; virtual;
		// Returns a description of the plugin from the version information.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetDescription: ustring; virtual;
	public
		constructor Create; virtual;
	end;

	// Structure to implement for visiting web plugin information. The functions of
	// this structure will be called on the browser process UI thread.
	TCefWebPluginInfoVisitorOwn = class(TCefBaseOwn, ICefWebPluginInfoVisitor)
	protected
		// Method that will be called once for each plugin. |count| is the 0-based
		// index for the current plugin. |total| is the total number of plugins.
		// Return false (0) to stop visiting plugins. This function may never be
		// called if no plugins are found.
		function Visit(const aInfo: ICefWebPluginInfo; aCount: cint; aTotal: cint): Boolean; virtual;
	public
		constructor Create; virtual;
	end;

	// Structure to implement for receiving unstable plugin information. The
	// functions of this structure will be called on the browser process IO thread.
	TCefWebPluginUnstableCallbackOwn = class(TCefBaseOwn, ICefWebPluginUnstableCallback)
	protected
		// Method that will be called for the requested plugin. |unstable| will be
		// true (1) if the plugin has reached the crash count threshold of 3 times in
		// 120 seconds.
		procedure IsUnstable(const aPath: ustring; aUnstable: Boolean); virtual;
	public
		constructor Create; virtual;
	end;


	//..............................................................................cef_xml_reader_capi.h
	// Structure that supports the reading of XML data via the libxml streaming API.
	// The functions of this structure should only be called on the thread that
	// creates the object.
	TCefXmlReaderOwn = class(TCefBaseOwn, ICefXmlReader)
	protected
		// Moves the cursor to the next node in the document. This function must be
		// called at least once to set the current cursor position. Returns true (1)
		// if the cursor position was set successfully.
		function MoveToNextNode: Boolean; virtual;
		// Close the document. This should be called directly to ensure that cleanup
		// occurs on the correct thread.
		function Close: Boolean; virtual;
		// Returns true (1) if an error has been reported by the XML parser.
		function HasError: Boolean; virtual;
		// Returns the error string.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetError: ustring; virtual;
		// Returns the node type.
		function GetType: TCefXmlNodeType; virtual;
		// Returns the node depth. Depth starts at 0 for the root node.
		function GetDepth: cint; virtual;
		// Returns the local name. See http://www.w3.org/TR/REC-xml-names/#NT-
		// LocalPart for additional details.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetLocalName: ustring; virtual;
		// Returns the namespace prefix. See http://www.w3.org/TR/REC-xml-names/ for
		// additional details.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetPrefix: ustring; virtual;
		// Returns the qualified name, equal to (Prefix:)LocalName. See
		// http://www.w3.org/TR/REC-xml-names/#ns-qualnames for additional details.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetQualifiedName: ustring; virtual;
		// Returns the URI defining the namespace associated with the node. See
		// http://www.w3.org/TR/REC-xml-names/ for additional details.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetNamespaceUri: ustring; virtual;
		// Returns the base URI of the node. See http://www.w3.org/TR/xmlbase/ for
		// additional details.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetBaseUri: ustring; virtual;
		// Returns the xml:lang scope within which the node resides. See
		// http://www.w3.org/TR/REC-xml/#sec-lang-tag for additional details.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetXmlLang: ustring; virtual;
		// Returns true (1) if the node represents an NULL element. <a/> is considered
		// NULL but <a></a> is not.
		function IsEmptyElement: Boolean; virtual;
		// Returns true (1) if the node has a text value.
		function HasValue: Boolean; virtual;
		// Returns the text value.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetValue: ustring; virtual;
		// Returns true (1) if the node has attributes.
		function HasAttributes: Boolean; virtual;
		// Returns the number of attributes.
		function GetAttributeCount: csize_t; virtual;
		// Returns the value of the attribute at the specified 0-based index.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetAttributeByindex(aIndex: cint): ustring; virtual;
		// Returns the value of the attribute with the specified qualified name.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetAttributeByqname(const aQualifiedName: ustring): ustring; virtual;
		// Returns the value of the attribute with the specified local name and
		// namespace URI.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetAttributeBylname(const aLocalName: ustring; const aNamespaceURI: ustring): ustring; virtual;
		// Returns an XML representation of the current node's children.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetInnerXml: ustring; virtual;
		// Returns an XML representation of the current node including its children.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetOuterXml: ustring; virtual;
		// Returns the line number for the current node.
		function GetLineNumber: cint; virtual;
		// Moves the cursor to the attribute at the specified 0-based index. Returns
		// true (1) if the cursor position was set successfully.
		function MoveToAttributeByindex(aIndex: cint): Boolean; virtual;
		// Moves the cursor to the attribute with the specified qualified name.
		// Returns true (1) if the cursor position was set successfully.
		function MoveToAttributeByqname(const aQualifiedName: ustring): Boolean; virtual;
		// Moves the cursor to the attribute with the specified local name and
		// namespace URI. Returns true (1) if the cursor position was set
		// successfully.
		function MoveToAttributeBylname(const aLocalName: ustring; const aNamespaceURI: ustring): Boolean; virtual;
		// Moves the cursor to the first attribute in the current element. Returns
		// true (1) if the cursor position was set successfully.
		function MoveToFirstAttribute: Boolean; virtual;
		// Moves the cursor to the next attribute in the current element. Returns true
		// (1) if the cursor position was set successfully.
		function MoveToNextAttribute: Boolean; virtual;
		// Moves the cursor back to the carrying element. Returns true (1) if the
		// cursor position was set successfully.
		function MoveToCarryingElement: Boolean; virtual;
	public
		constructor Create; virtual;
	end;


	//..............................................................................cef_zip_reader_capi.h
	// Structure that supports the reading of zip archives via the zlib unzip API.
	// The functions of this structure should only be called on the thread that
	// creates the object.
	TCefZipReaderOwn = class(TCefBaseOwn, ICefZipReader)
	protected
		// Moves the cursor to the first file in the archive. Returns true (1) if the
		// cursor position was set successfully.
		function MoveToFirstFile: Boolean; virtual;
		// Moves the cursor to the next file in the archive. Returns true (1) if the
		// cursor position was set successfully.
		function MoveToNextFile: Boolean; virtual;
		// Moves the cursor to the specified file in the archive. If |caseSensitive|
		// is true (1) then the search will be case sensitive. Returns true (1) if the
		// cursor position was set successfully.
		function MoveToFile(const aFileName: ustring; aCaseSensitive: Boolean): Boolean; virtual;
		// Closes the archive. This should be called directly to ensure that cleanup
		// occurs on the correct thread.
		function Close: Boolean; virtual;
		// Returns the name of the file.
		// The resulting string must be freed by calling cef_string_userfree_free().
		function GetFileName: ustring; virtual;
		// Returns the uncompressed size of the file.
		function GetFileSize: cint64; virtual;
		// Returns the last modified timestamp for the file.
		function GetFileLastModified: ctime_t; virtual;
		// Opens the file for reading of uncompressed data. A read password may
		// optionally be specified.
		function OpenFile(const aPassword: ustring): Boolean; virtual;
		// Closes the file.
		function CloseFile: Boolean; virtual;
		// Read uncompressed file contents into the specified buffer. Returns < 0 if
		// an error occurred, 0 if at the end of file, or the number of bytes read.
		function ReadFile(const aBuffer: cvoid; aBufferSize: csize_t): cint; virtual;
		// Returns the current offset in the uncompressed file contents.
		function Tell: cint64; virtual;
		// Returns true (1) if at end of the file contents.
		function Eof: Boolean; virtual;
	public
		constructor Create; virtual;
	end;


  //............................................................................Custom classes
  TCefFastStringVisitor = class(TCefStringVisitorOwn, ICefStringVisitor)
  private
    FVisit: TCefStringVisitorProc;
  protected
    procedure Visit(const str: ustring); override;
  public
    constructor Create(const callback: TCefStringVisitorProc); reintroduce;
  end;

  TCefFastDomVisitor = class(TCefDomVisitorOwn)
  private
    FProc: TCefDomVisitorProc;
  protected
    procedure visit(const document: ICefDomDocument); override;
  public
    constructor Create(const proc: TCefDomVisitorProc); reintroduce; virtual;
  end;

  TCefCustomStreamReader = class(TCefBaseOwn, ICefCustomStreamReader)
  private
    FStream: TStream;
    FOwned: Boolean;
  protected
    function Read(ptr: Pointer; size, n: csize_t): csize_t; virtual;
    function Seek(offset: Int64; whence: cint): cint; virtual;
    function Tell: Int64; virtual;
    function Eof: Boolean; virtual;
  public
    constructor Create(Stream: TStream; Owned: Boolean); overload; virtual;
    constructor Create(const filename: ustring); overload; virtual;
    destructor Destroy; override;
  end;

  TCefFastResourceBundle = class(TCefResourceBundleHandlerOwn)
  private
    FGetDataResource: TGetDataResource;
    FGetLocalizedString: TGetLocalizedString;
  protected
    function GetLocalizedString(aMessageId: cint; out aString: ustring): Boolean; override;
    function GetDataResource(aResourceId: cint; out aData: cvoid; out aDataSize: csize_t): Boolean; override;
  public
    constructor Create(AGetDataResource: TGetDataResource;
      AGetLocalizedString: TGetLocalizedString); reintroduce;
  end;

  TCefFastRunFileDialogCallback = class(TCefRunFileDialogCallbackOwn)
  private
    FCallback: TCefRunFileDialogCallbackProc;
  protected
    procedure OnFileDialogDismissed(SelectedAcceptFilter: cint; filePaths: TStrings); override;
  public
    constructor Create(callback: TCefRunFileDialogCallbackProc); reintroduce; virtual;
  end;

  {$IFNDEF FPC}
  TCefRTTIExtension = class(TCefv8HandlerOwn)
  private
    FValue: TValue;
    FCtx: TRttiContext;
    {$IFDEF CEF_MULTI_THREADED_MESSAGE_LOOP}
    FSyncMainThread: Boolean;
    {$ENDIF}
    function GetValue(pi: PTypeInfo; const v: ICefv8Value; var ret: TValue): Boolean;
    function SetValue(const v: TValue; var ret: ICefv8Value): Boolean;
  protected
    function Execute(const aName: ustring; const aObject: ICefV8value; aArgumentsCount: csize_t; const aArguments: TCefv8ValueArray; var aRetval: ICefV8value; var aException: ustring): Boolean; override;
  public
    constructor Create(const value: TValue
    {$IFDEF CEF_MULTI_THREADED_MESSAGE_LOOP}
    ; SyncMainThread: Boolean
    {$ENDIF}
    ); reintroduce;
    destructor Destroy; override;
    class procedure Register(const name: ustring; const value: TValue
      {$IFDEF CEF_MULTI_THREADED_MESSAGE_LOOP}; SyncMainThread: Boolean{$ENDIF});
  end;
  {$ENDIF}

  ECefException = class(Exception)
  end;

  TInternalApp = class(TCefAppOwn)
  protected
    procedure OnBeforeCommandLineProcessing(const processType: ustring; const commandLine: ICefCommandLine); override;
    procedure OnRegisterCustomSchemes(const registrar: ICefSchemeRegistrar); override;
    function GetResourceBundleHandler: ICefResourceBundleHandler; override;
    function GetBrowserProcessHandler: ICefBrowserProcessHandler; override;
    function GetRenderProcessHandler: ICefRenderProcessHandler; override;
  public
    function GetProcessType(const aCommandLine: ICefCommandLine): TCefProcessType;
  end;

  TCefFastCookieVisitor = class(TCefCookieVisitorOwn)
  private
    FVisitor: TCefCookieVisitorProc;
  protected
    function Visit(const aCookie: TWACefCookie; aCount: cint; aTotal: cint; var aDeleteCookie: Boolean): Boolean; virtual;
  public
    constructor Create(const visitor: TCefCookieVisitorProc); reintroduce;
  end;

  TCefStringMapOwn = class(TInterfacedObject, ICefStringMap)
  private
    FStringMap: TCefStringMap;
  protected
    function GetHandle: TCefStringMap; virtual;
    function GetSize: cint; virtual;
    function Find(const key: ustring): ustring; virtual;
    function GetKey(index: cint): ustring; virtual;
    function GetValue(index: cint): ustring; virtual;
    procedure Append(const key, value: ustring); virtual;
    procedure Clear; virtual;
  public
    constructor Create(aStringMap: TCefStringMap = nil); virtual;
    destructor Destroy; override;
  end;

  TCefStringMultimapOwn = class(TInterfacedObject, ICefStringMultimap)
  private
    FStringMap: TCefStringMultimap;
  protected
    function GetHandle: TCefStringMultimap; virtual;
    function GetSize: cint; virtual;
    function FindCount(const Key: ustring): cint; virtual;
    function GetEnumerate(const Key: ustring; ValueIndex: cint): ustring; virtual;
    function GetKey(Index: cint): ustring; virtual;
    function GetValue(Index: cint): ustring; virtual;
    procedure Append(const Key, Value: ustring); virtual;
    procedure Clear; virtual;
  public
    constructor Create(aStringMap: TCefStringMultiMap = nil); virtual;
    destructor Destroy; override;
  end;

  TCefFastWebPluginInfoVisitor = class(TCefWebPluginInfoVisitorOwn)
  private
    FProc: TCefWebPluginInfoVisitorProc;
  protected
    function Visit(const info: ICefWebPluginInfo; count, total: cint): Boolean; override;
  public
    constructor Create(const proc: TCefWebPluginInfoVisitorProc); reintroduce;
  end;

  TCefFastWebPluginUnstableCallback = class(TCefWebPluginUnstableCallbackOwn)
  private
    FCallback: TCefWebPluginIsUnstableProc;
  protected
    procedure IsUnstable(const path: ustring; unstable: Boolean); override;
  public
    constructor Create(const callback: TCefWebPluginIsUnstableProc); reintroduce;
  end;

  TCefFastV8Accessor = class(TCefV8AccessorOwn)
  private
    FGetter: TCefV8AccessorGetterProc;
    FSetter: TCefV8AccessorSetterProc;
  protected
    function Get(const aName: ustring; const aObject: ICefV8value; out aRetval: ICefV8value; const aException: ustring): Boolean; override;
    function _Set(const aName: ustring; const aObject: ICefV8value; const aValue: ICefV8value; const aException: ustring): Boolean; override;
  public
    constructor Create(const getter: TCefV8AccessorGetterProc;
      const setter: TCefV8AccessorSetterProc); reintroduce;
  end;


implementation

uses
  WACefRefs,
  WACefLib;

//..............................................................................TCefBaseOwn
procedure cef_base_add_ref(self: PCefBase); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  TCefBaseOwn(TWACef.GetObject(self))._AddRef;
end;

function cef_base_release(self: PCefBase): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  Result := TCefBaseOwn(TWACef.GetObject(self))._Release;
end;

function cef_base_has_one_ref(self: PCefBase): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  Result := Ord(TCefBaseOwn(TWACef.GetObject(self)).FRefCount = 1);
end;

procedure cef_base_add_ref_owned(self: PCefBase); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin

end;

function cef_base_release_owned(self: PCefBase): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  Result := 1;
end;

function cef_base_has_one_ref_owned(self: PCefBase): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  Result := 1;
end;

constructor TCefBaseOwn.CreateData(size: csize_t; owned: Boolean = False);
begin
  GetMem(FData, size + SizeOf(Pointer));
  PPointer(FData)^ := Self;
  Inc(PByte(FData), SizeOf(Pointer));
  FillChar(FData^, size, 0);
  PCefBase(FData)^.size := size;
  if owned then
  begin
    PCefBase(FData)^.add_ref := {$IFDEF FPC}@{$ENDIF}cef_base_add_ref_owned;
    PCefBase(FData)^.release := {$IFDEF FPC}@{$ENDIF}cef_base_release_owned;
    PCefBase(FData)^.has_one_ref := {$IFDEF FPC}@{$ENDIF}cef_base_has_one_ref_owned;
  end
  else
  begin
    PCefBase(FData)^.add_ref := {$IFDEF FPC}@{$ENDIF}cef_base_add_ref;
    PCefBase(FData)^.release := {$IFDEF FPC}@{$ENDIF}cef_base_release;
    PCefBase(FData)^.has_one_ref := {$IFDEF FPC}@{$ENDIF}cef_base_has_one_ref;
  end;
end;

destructor TCefBaseOwn.Destroy;
begin
  Dec(PByte(FData), SizeOf(Pointer));
  FreeMem(FData);
  inherited;
end;

function TCefBaseOwn.Wrap: Pointer;
begin
  Result := FData;
  if Assigned(PCefBase(FData)^.add_ref) then
    PCefBase(FData)^.add_ref(PCefBase(FData));
end;

//..............................................................................TCefAppOwn
// Provides an opportunity to view and/or modify command-line arguments before
// processing by CEF and Chromium. The |process_type| value will be NULL for
// the browser process. Do not keep a reference to the cef_command_line_t
// object passed to this function. The CefSettings.command_line_args_disabled
// value can be used to start with an NULL command-line object. Any values
// specified in CefSettings that equate to command-line arguments will be set
// before this function is called. Be cautious when using this function to
// modify command-line arguments for non-browser processes as this may result
// in undefined behavior including crashes.
procedure cef_app_on_before_command_line_processing(self: PCefApp; const process_type: PCefString; command_line: PCefCommandLine); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	process_type_str: ustring;
begin
  if Assigned(process_type) then
  	process_type_str := TWACef.ToString(process_type);
	TCefAppOwn(TWACef.GetObject(self)).OnBeforeCommandLineProcessing(
		process_type_str,
		TCefCommandLineRef.UnWrap(command_line)
	);
end;

// Provides an opportunity to register custom schemes. Do not keep a reference
// to the |registrar| object. This function is called on the main thread for
// each process and the registered schemes should be the same across all
// processes.
procedure cef_app_on_register_custom_schemes(self: PCefApp; registrar: PCefSchemeRegistrar); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefAppOwn(TWACef.GetObject(self)).OnRegisterCustomSchemes(
		TCefSchemeRegistrarRef.UnWrap(registrar)
	);
end;

// Return the handler for resource bundle events. If
// CefSettings.pack_loading_disabled is true (1) a handler must be returned.
// If no handler is returned resources will be loaded from pack files. This
// function is called by the browser and render processes on multiple threads.
function cef_app_get_resource_bundle_handler(self: PCefApp): PCefResourceBundleHandler; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.GetData(
		TCefAppOwn(TWACef.GetObject(self)).GetResourceBundleHandler
	);
end;

// Return the handler for functionality specific to the browser process. This
// function is called on multiple threads in the browser process.
function cef_app_get_browser_process_handler(self: PCefApp): PCefBrowserProcessHandler; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.GetData(
		TCefAppOwn(TWACef.GetObject(self)).GetBrowserProcessHandler
	);
end;

// Return the handler for functionality specific to the render process. This
// function is called on the render process main thread.
function cef_app_get_render_process_handler(self: PCefApp): PCefRenderProcessHandler; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.GetData(
		TCefAppOwn(TWACef.GetObject(self)).GetRenderProcessHandler
	);
end;

{Protected section}
procedure TCefAppOwn.OnBeforeCommandLineProcessing(const aProcessType: ustring; const aCommandLine: ICefCommandLine);
begin
end;

procedure TCefAppOwn.OnRegisterCustomSchemes(const aRegistrar: ICefSchemeRegistrar);
begin
end;

function TCefAppOwn.GetResourceBundleHandler: ICefResourceBundleHandler;
begin
	Result := nil;
end;

function TCefAppOwn.GetBrowserProcessHandler: ICefBrowserProcessHandler;
begin
	Result := nil;
end;

function TCefAppOwn.GetRenderProcessHandler: ICefRenderProcessHandler;
begin
	Result := nil;
end;

{Public section}
constructor TCefAppOwn.Create;
begin
	inherited CreateData(SizeOf(TCefApp));
	with PCefApp(FData)^ do
	begin
		on_before_command_line_processing := {$IFDEF FPC}@{$ENDIF}cef_app_on_before_command_line_processing;
		on_register_custom_schemes := {$IFDEF FPC}@{$ENDIF}cef_app_on_register_custom_schemes;
		get_resource_bundle_handler := {$IFDEF FPC}@{$ENDIF}cef_app_get_resource_bundle_handler;
		get_browser_process_handler := {$IFDEF FPC}@{$ENDIF}cef_app_get_browser_process_handler;
		get_render_process_handler := {$IFDEF FPC}@{$ENDIF}cef_app_get_render_process_handler;
	end;
end;

//..............................................................................TCefAuthCallbackOwn
// Continue the authentication request.
procedure cef_auth_callback_cont(self: PCefAuthCallback; const username: PCefString; const password: PCefString); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	username_str: ustring;
	password_str: ustring;
begin
	username_str := TWACef.ToString(username);
	password_str := TWACef.ToString(password);
	TCefAuthCallbackOwn(TWACef.GetObject(self)).Cont(
		username_str,
		password_str
	);
end;

// Cancel the authentication request.
procedure cef_auth_callback_cancel(self: PCefAuthCallback); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefAuthCallbackOwn(TWACef.GetObject(self)).Cancel;
end;

{Protected section}
procedure TCefAuthCallbackOwn.Cont(const aUsername: ustring; const aPassword: ustring);
begin
end;

procedure TCefAuthCallbackOwn.Cancel;
begin
end;

{Public section}
constructor TCefAuthCallbackOwn.Create;
begin
	inherited CreateData(SizeOf(TCefAuthCallback));
	with PCefAuthCallback(FData)^ do
	begin
		cont := {$IFDEF FPC}@{$ENDIF}cef_auth_callback_cont;
		cancel := {$IFDEF FPC}@{$ENDIF}cef_auth_callback_cancel;
	end;
end;

//..............................................................................TCefBrowserOwn
// Returns the browser host object. This function can only be called in the
// browser process.
function cef_browser_get_host(self: PCefBrowser): PCefBrowserHost; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.GetData(
		TCefBrowserOwn(TWACef.GetObject(self)).GetHost
	);
end;

// Returns true (1) if the browser can navigate backwards.
function cef_browser_can_go_back(self: PCefBrowser): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefBrowserOwn(TWACef.GetObject(self)).CanGoBack
	);
end;

// Navigate backwards.
procedure cef_browser_go_back(self: PCefBrowser); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefBrowserOwn(TWACef.GetObject(self)).GoBack;
end;

// Returns true (1) if the browser can navigate forwards.
function cef_browser_can_go_forward(self: PCefBrowser): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefBrowserOwn(TWACef.GetObject(self)).CanGoForward
	);
end;

// Navigate forwards.
procedure cef_browser_go_forward(self: PCefBrowser); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefBrowserOwn(TWACef.GetObject(self)).GoForward;
end;

// Returns true (1) if the browser is currently loading.
function cef_browser_is_loading(self: PCefBrowser): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefBrowserOwn(TWACef.GetObject(self)).IsLoading
	);
end;

// Reload the current page.
procedure cef_browser_reload(self: PCefBrowser); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefBrowserOwn(TWACef.GetObject(self)).Reload;
end;

// Reload the current page ignoring any cached data.
procedure cef_browser_reload_ignore_cache(self: PCefBrowser); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefBrowserOwn(TWACef.GetObject(self)).ReloadIgnoreCache;
end;

// Stop loading the page.
procedure cef_browser_stop_load(self: PCefBrowser); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefBrowserOwn(TWACef.GetObject(self)).StopLoad;
end;

// Returns the globally unique identifier for this browser.
function cef_browser_get_identifier(self: PCefBrowser): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := (
		TCefBrowserOwn(TWACef.GetObject(self)).GetIdentifier
	);
end;

// Returns true (1) if this object is pointing to the same handle as |that|
// object.
function cef_browser_is_same(self: PCefBrowser; that: PCefBrowser): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefBrowserOwn(TWACef.GetObject(self)).IsSame(
			TCefBrowserRef.UnWrap(that)
		)
	);
end;

// Returns true (1) if the window is a popup window.
function cef_browser_is_popup(self: PCefBrowser): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefBrowserOwn(TWACef.GetObject(self)).IsPopup
	);
end;

// Returns true (1) if a document has been loaded in the browser.
function cef_browser_has_document(self: PCefBrowser): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefBrowserOwn(TWACef.GetObject(self)).HasDocument
	);
end;

// Returns the main (top-level) frame for the browser window.
function cef_browser_get_main_frame(self: PCefBrowser): PCefFrame; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.GetData(
		TCefBrowserOwn(TWACef.GetObject(self)).GetMainFrame
	);
end;

// Returns the focused frame for the browser window.
function cef_browser_get_focused_frame(self: PCefBrowser): PCefFrame; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.GetData(
		TCefBrowserOwn(TWACef.GetObject(self)).GetFocusedFrame
	);
end;

// Returns the frame with the specified identifier, or NULL if not found.
function cef_browser_get_frame_byident(self: PCefBrowser; identifier: cint64): PCefFrame; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.GetData(
		TCefBrowserOwn(TWACef.GetObject(self)).GetFrameByident(
			identifier
		)
	);
end;

// Returns the frame with the specified name, or NULL if not found.
function cef_browser_get_frame(self: PCefBrowser; const name: PCefString): PCefFrame; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	name_str: ustring;
begin
	name_str := TWACef.ToString(name);
	Result := TWACef.GetData(
		TCefBrowserOwn(TWACef.GetObject(self)).GetFrame(
			name_str
		)
	);
end;

// Returns the number of frames that currently exist.
function cef_browser_get_frame_count(self: PCefBrowser): csize_t; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := (
		TCefBrowserOwn(TWACef.GetObject(self)).GetFrameCount
	);
end;

// Returns the identifiers of all existing frames.
procedure cef_browser_get_frame_identifiers(self: PCefBrowser; identifiersCount: pcsize_t; identifiers: pcint64); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	identifiersCount_proxy: csize_t;
	identifiers_proxy: cint64;
begin
	TCefBrowserOwn(TWACef.GetObject(self)).GetFrameIdentifiers(
		identifiersCount_proxy,
		identifiers_proxy
	);
end;

// Returns the names of all existing frames.
procedure cef_browser_get_frame_names(self: PCefBrowser; names: TCefStringList); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefBrowserOwn(TWACef.GetObject(self)).GetFrameNames(
		names
	);
end;

//
// Send a message to the specified |target_process|. Returns true (1) if the
// message was sent successfully.
function cef_browser_send_process_message(self: PCefBrowser; target_process: TCefProcessId; message: PCefProcessMessage): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefBrowserOwn(TWACef.GetObject(self)).SendProcessMessage(
			target_process,
			TCefProcessMessageRef.UnWrap(message)
		)
	);
end;

{Protected section}
function TCefBrowserOwn.GetHost: ICefBrowserHost;
begin
	Result := nil;
end;

function TCefBrowserOwn.CanGoBack: Boolean;
begin
	Result := false;
end;

procedure TCefBrowserOwn.GoBack;
begin
end;

function TCefBrowserOwn.CanGoForward: Boolean;
begin
	Result := false;
end;

procedure TCefBrowserOwn.GoForward;
begin
end;

function TCefBrowserOwn.IsLoading: Boolean;
begin
	Result := false;
end;

procedure TCefBrowserOwn.Reload;
begin
end;

procedure TCefBrowserOwn.ReloadIgnoreCache;
begin
end;

procedure TCefBrowserOwn.StopLoad;
begin
end;

function TCefBrowserOwn.GetIdentifier: cint;
begin
	Result := 0;
end;

function TCefBrowserOwn.IsSame(const aThat: ICefBrowser): Boolean;
begin
	Result := false;
end;

function TCefBrowserOwn.IsPopup: Boolean;
begin
	Result := false;
end;

function TCefBrowserOwn.HasDocument: Boolean;
begin
	Result := false;
end;

function TCefBrowserOwn.GetMainFrame: ICefFrame;
begin
	Result := nil;
end;

function TCefBrowserOwn.GetFocusedFrame: ICefFrame;
begin
	Result := nil;
end;

function TCefBrowserOwn.GetFrameByident(aIdentifier: cint64): ICefFrame;
begin
	Result := nil;
end;

function TCefBrowserOwn.GetFrame(const aName: ustring): ICefFrame;
begin
	Result := nil;
end;

function TCefBrowserOwn.GetFrameCount: csize_t;
begin
end;

procedure TCefBrowserOwn.GetFrameIdentifiers(var aIdentifiersCount: csize_t; var aIdentifiers: cint64);
begin
end;

procedure TCefBrowserOwn.GetFrameNames(aNames: TStrings);
begin
end;

function TCefBrowserOwn.SendProcessMessage(aTargetProcess: TCefProcessId; const aMessage: ICefProcessMessage): Boolean;
begin
	Result := false;
end;

{Public section}
constructor TCefBrowserOwn.Create;
begin
	inherited CreateData(SizeOf(TCefBrowser));
	with PCefBrowser(FData)^ do
	begin
		get_host := {$IFDEF FPC}@{$ENDIF}cef_browser_get_host;
		can_go_back := {$IFDEF FPC}@{$ENDIF}cef_browser_can_go_back;
		go_back := {$IFDEF FPC}@{$ENDIF}cef_browser_go_back;
		can_go_forward := {$IFDEF FPC}@{$ENDIF}cef_browser_can_go_forward;
		go_forward := {$IFDEF FPC}@{$ENDIF}cef_browser_go_forward;
		is_loading := {$IFDEF FPC}@{$ENDIF}cef_browser_is_loading;
		reload := {$IFDEF FPC}@{$ENDIF}cef_browser_reload;
		reload_ignore_cache := {$IFDEF FPC}@{$ENDIF}cef_browser_reload_ignore_cache;
		stop_load := {$IFDEF FPC}@{$ENDIF}cef_browser_stop_load;
		get_identifier := {$IFDEF FPC}@{$ENDIF}cef_browser_get_identifier;
		is_same := {$IFDEF FPC}@{$ENDIF}cef_browser_is_same;
		is_popup := {$IFDEF FPC}@{$ENDIF}cef_browser_is_popup;
		has_document := {$IFDEF FPC}@{$ENDIF}cef_browser_has_document;
		get_main_frame := {$IFDEF FPC}@{$ENDIF}cef_browser_get_main_frame;
		get_focused_frame := {$IFDEF FPC}@{$ENDIF}cef_browser_get_focused_frame;
		get_frame_byident := {$IFDEF FPC}@{$ENDIF}cef_browser_get_frame_byident;
		get_frame := {$IFDEF FPC}@{$ENDIF}cef_browser_get_frame;
		get_frame_count := {$IFDEF FPC}@{$ENDIF}cef_browser_get_frame_count;
		get_frame_identifiers := {$IFDEF FPC}@{$ENDIF}cef_browser_get_frame_identifiers;
		get_frame_names := {$IFDEF FPC}@{$ENDIF}cef_browser_get_frame_names;
		send_process_message := {$IFDEF FPC}@{$ENDIF}cef_browser_send_process_message;
	end;
end;
//..............................................................................TCefRunFileDialogCallbackOwn
// Called asynchronously after the file dialog is dismissed.
// |selected_accept_filter| is the 0-based index of the value selected from
// the accept filters array passed to cef_browser_host_t::RunFileDialog.
// |file_paths| will be a single value or a list of values depending on the
// dialog mode. If the selection was cancelled |file_paths| will be NULL.
procedure cef_run_file_dialog_callback_on_file_dialog_dismissed(self: PCefRunFileDialogCallback; selected_accept_filter: cint; file_paths: TCefStringList); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefRunFileDialogCallbackOwn(TWACef.GetObject(self)).OnFileDialogDismissed(
		selected_accept_filter,
		file_paths
	);
end;

{Protected section}
procedure TCefRunFileDialogCallbackOwn.OnFileDialogDismissed(aSelectedAcceptFilter: cint; aFilePaths: TStrings);
begin
end;

{Public section}
constructor TCefRunFileDialogCallbackOwn.Create;
begin
	inherited CreateData(SizeOf(TCefRunFileDialogCallback));
	with PCefRunFileDialogCallback(FData)^ do
	begin
		on_file_dialog_dismissed := {$IFDEF FPC}@{$ENDIF}cef_run_file_dialog_callback_on_file_dialog_dismissed;
	end;
end;
//..............................................................................TCefNavigationEntryVisitorOwn
// Method that will be executed. Do not keep a reference to |entry| outside of
// this callback. Return true (1) to continue visiting entries or false (0) to
// stop. |current| is true (1) if this entry is the currently loaded
// navigation entry. |index| is the 0-based index of this entry and |total| is
// the total number of entries.
function cef_navigation_entry_visitor_visit(self: PCefNavigationEntryVisitor; entry: PCefNavigationEntry; current: cint; index: cint; total: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefNavigationEntryVisitorOwn(TWACef.GetObject(self)).Visit(
			TCefNavigationEntryRef.UnWrap(entry),
			current <> 0,
			index,
			total
		)
	);
end;

{Protected section}
function TCefNavigationEntryVisitorOwn.Visit(const aEntry: ICefNavigationEntry; aCurrent: Boolean; aIndex: cint; aTotal: cint): Boolean;
begin
	Result := false;
end;

{Public section}
constructor TCefNavigationEntryVisitorOwn.Create;
begin
	inherited CreateData(SizeOf(TCefNavigationEntryVisitor));
	with PCefNavigationEntryVisitor(FData)^ do
	begin
		visit := {$IFDEF FPC}@{$ENDIF}cef_navigation_entry_visitor_visit;
	end;
end;
//..............................................................................TCefBrowserHostOwn
// Returns the hosted browser object.
function cef_browser_host_get_browser(self: PCefBrowserHost): PCefBrowser; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.GetData(
		TCefBrowserHostOwn(TWACef.GetObject(self)).GetBrowser
	);
end;

// Request that the browser close. The JavaScript 'onbeforeunload' event will
// be fired. If |force_close| is false (0) the event handler, if any, will be
// allowed to prompt the user and the user can optionally cancel the close. If
// |force_close| is true (1) the prompt will not be displayed and the close
// will proceed. Results in a call to cef_life_span_handler_t::do_close() if
// the event handler allows the close or if |force_close| is true (1). See
// cef_life_span_handler_t::do_close() documentation for additional usage
// information.
procedure cef_browser_host_close_browser(self: PCefBrowserHost; force_close: cint); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefBrowserHostOwn(TWACef.GetObject(self)).CloseBrowser(
		force_close <> 0
	);
end;

// Set whether the browser is focused.
procedure cef_browser_host_set_focus(self: PCefBrowserHost; focus: cint); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefBrowserHostOwn(TWACef.GetObject(self)).SetFocus(
		focus <> 0
	);
end;

// Set whether the window containing the browser is visible
// (minimized/unminimized, app hidden/unhidden, etc). Only used on Mac OS X.
procedure cef_browser_host_set_window_visibility(self: PCefBrowserHost; visible: cint); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefBrowserHostOwn(TWACef.GetObject(self)).SetWindowVisibility(
		visible <> 0
	);
end;

// Retrieve the window handle for this browser.
function cef_browser_host_get_window_handle(self: PCefBrowserHost): TCefWindowHandle; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := (
		TCefBrowserHostOwn(TWACef.GetObject(self)).GetWindowHandle
	);
end;

// Retrieve the window handle of the browser that opened this browser. Will
// return NULL for non-popup windows. This function can be used in combination
// with custom handling of modal windows.
function cef_browser_host_get_opener_window_handle(self: PCefBrowserHost): TCefWindowHandle; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := (
		TCefBrowserHostOwn(TWACef.GetObject(self)).GetOpenerWindowHandle
	);
end;

// Returns the client for this browser.
function cef_browser_host_get_client(self: PCefBrowserHost): PCefClient; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.GetData(
		TCefBrowserHostOwn(TWACef.GetObject(self)).GetClient
	);
end;

// Returns the request context for this browser.
function cef_browser_host_get_request_context(self: PCefBrowserHost): PCefRequestContext; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.GetData(
		TCefBrowserHostOwn(TWACef.GetObject(self)).GetRequestContext
	);
end;

// Get the current zoom level. The default zoom level is 0.0. This function
// can only be called on the UI thread.
function cef_browser_host_get_zoom_level(self: PCefBrowserHost): cdouble; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := (
		TCefBrowserHostOwn(TWACef.GetObject(self)).GetZoomLevel
	);
end;

// Change the zoom level to the specified value. Specify 0.0 to reset the zoom
// level. If called on the UI thread the change will be applied immediately.
// Otherwise, the change will be applied asynchronously on the UI thread.
procedure cef_browser_host_set_zoom_level(self: PCefBrowserHost; zoomLevel: cdouble); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefBrowserHostOwn(TWACef.GetObject(self)).SetZoomLevel(
		zoomLevel
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
procedure cef_browser_host_run_file_dialog(self: PCefBrowserHost; mode: TCefFileDialogMode; const title: PCefString; const default_file_path: PCefString; accept_filters: TCefStringList; selected_accept_filter: cint; callback: PCefRunFileDialogCallback); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	title_str: ustring;
	default_file_path_str: ustring;
begin
	title_str := TWACef.ToString(title);
	default_file_path_str := TWACef.ToString(default_file_path);
	TCefBrowserHostOwn(TWACef.GetObject(self)).RunFileDialog(
		mode,
		title_str,
		default_file_path_str,
		accept_filters,
		selected_accept_filter,
		TCefRunFileDialogCallbackRef.UnWrap(callback)
	);
end;

// Download the file at |url| using cef_download_handler_t.
procedure cef_browser_host_start_download(self: PCefBrowserHost; const url: PCefString); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	url_str: ustring;
begin
	url_str := TWACef.ToString(url);
	TCefBrowserHostOwn(TWACef.GetObject(self)).StartDownload(
		url_str
	);
end;

// Print the current browser contents.
procedure cef_browser_host_print(self: PCefBrowserHost); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefBrowserHostOwn(TWACef.GetObject(self)).Print;
end;

// Search for |searchText|. |identifier| can be used to have multiple searches
// running simultaniously. |forward| indicates whether to search forward or
// backward within the page. |matchCase| indicates whether the search should
// be case-sensitive. |findNext| indicates whether this is the first request
// or a follow-up. The cef_find_handler_t instance, if any, returned via
// cef_client_t::GetFindHandler will be called to report find results.
procedure cef_browser_host_find(self: PCefBrowserHost; identifier: cint; const searchText: PCefString; forward: cint; matchCase: cint; findNext: cint); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	searchText_str: ustring;
begin
	searchText_str := TWACef.ToString(searchText);
	TCefBrowserHostOwn(TWACef.GetObject(self)).Find(
		identifier,
		searchText_str,
		forward <> 0,
		matchCase <> 0,
		findNext <> 0
	);
end;

// Cancel all searches that are currently going on.
procedure cef_browser_host_stop_finding(self: PCefBrowserHost; clearSelection: cint); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefBrowserHostOwn(TWACef.GetObject(self)).StopFinding(
		clearSelection <> 0
	);
end;

// Open developer tools in its own window. If |inspect_element_at| is non-
// NULL the element at the specified (x,y) location will be inspected.
procedure cef_browser_host_show_dev_tools(self: PCefBrowserHost; const windowInfo: PCefWindowInfo; client: PCefClient; const settings: PCefBrowserSettings; const inspect_element_at: PCefPoint); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefBrowserHostOwn(TWACef.GetObject(self)).ShowDevTools(
		windowInfo^,
		TCefClientRef.UnWrap(client),
		settings^,
		inspect_element_at^
	);
end;

// Explicitly close the developer tools window if one exists for this browser
// instance.
procedure cef_browser_host_close_dev_tools(self: PCefBrowserHost); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefBrowserHostOwn(TWACef.GetObject(self)).CloseDevTools;
end;

// Retrieve a snapshot of current navigation entries as values sent to the
// specified visitor. If |current_only| is true (1) only the current
// navigation entry will be sent, otherwise all navigation entries will be
// sent.
procedure cef_browser_host_get_navigation_entries(self: PCefBrowserHost; visitor: PCefNavigationEntryVisitor; current_only: cint); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefBrowserHostOwn(TWACef.GetObject(self)).GetNavigationEntries(
		TCefNavigationEntryVisitorRef.UnWrap(visitor),
		current_only <> 0
	);
end;

// Set whether mouse cursor change is disabled.
procedure cef_browser_host_set_mouse_cursor_change_disabled(self: PCefBrowserHost; disabled: cint); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefBrowserHostOwn(TWACef.GetObject(self)).SetMouseCursorChangeDisabled(
		disabled <> 0
	);
end;

// Returns true (1) if mouse cursor change is disabled.
function cef_browser_host_is_mouse_cursor_change_disabled(self: PCefBrowserHost): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefBrowserHostOwn(TWACef.GetObject(self)).IsMouseCursorChangeDisabled
	);
end;

// If a misspelled word is currently selected in an editable node calling this
// function will replace it with the specified |word|.
procedure cef_browser_host_replace_misspelling(self: PCefBrowserHost; const word: PCefString); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	word_str: ustring;
begin
	word_str := TWACef.ToString(word);
	TCefBrowserHostOwn(TWACef.GetObject(self)).ReplaceMisspelling(
		word_str
	);
end;

// Add the specified |word| to the spelling dictionary.
procedure cef_browser_host_add_word_to_dictionary(self: PCefBrowserHost; const word: PCefString); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	word_str: ustring;
begin
	word_str := TWACef.ToString(word);
	TCefBrowserHostOwn(TWACef.GetObject(self)).AddWordToDictionary(
		word_str
	);
end;

// Returns true (1) if window rendering is disabled.
function cef_browser_host_is_window_rendering_disabled(self: PCefBrowserHost): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefBrowserHostOwn(TWACef.GetObject(self)).IsWindowRenderingDisabled
	);
end;

// Notify the browser that the widget has been resized. The browser will first
// call cef_render_handler_t::GetViewRect to get the new size and then call
// cef_render_handler_t::OnPaint asynchronously with the updated regions. This
// function is only used when window rendering is disabled.
procedure cef_browser_host_was_resized(self: PCefBrowserHost); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefBrowserHostOwn(TWACef.GetObject(self)).WasResized;
end;

// Notify the browser that it has been hidden or shown. Layouting and
// cef_render_handler_t::OnPaint notification will stop when the browser is
// hidden. This function is only used when window rendering is disabled.
procedure cef_browser_host_was_hidden(self: PCefBrowserHost; hidden: cint); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefBrowserHostOwn(TWACef.GetObject(self)).WasHidden(
		hidden <> 0
	);
end;

// Send a notification to the browser that the screen info has changed. The
// browser will then call cef_render_handler_t::GetScreenInfo to update the
// screen information with the new values. This simulates moving the webview
// window from one display to another, or changing the properties of the
// current display. This function is only used when window rendering is
// disabled.
procedure cef_browser_host_notify_screen_info_changed(self: PCefBrowserHost); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefBrowserHostOwn(TWACef.GetObject(self)).NotifyScreenInfoChanged;
end;

// Invalidate the view. The browser will call cef_render_handler_t::OnPaint
// asynchronously. This function is only used when window rendering is
// disabled.
procedure cef_browser_host_invalidate(self: PCefBrowserHost; _type: TCefPaintElementType); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefBrowserHostOwn(TWACef.GetObject(self)).Invalidate(
		_type
	);
end;

// Send a key event to the browser.
procedure cef_browser_host_send_key_event(self: PCefBrowserHost; const event: PCefKeyEvent); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefBrowserHostOwn(TWACef.GetObject(self)).SendKeyEvent(
		event^
	);
end;

// Send a mouse click event to the browser. The |x| and |y| coordinates are
// relative to the upper-left corner of the view.
procedure cef_browser_host_send_mouse_click_event(self: PCefBrowserHost; const event: PCefMouseEvent; _type: TCefMouseButtonType; mouseUp: cint; clickCount: cint); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefBrowserHostOwn(TWACef.GetObject(self)).SendMouseClickEvent(
		event^,
		_type,
		mouseUp <> 0,
		clickCount
	);
end;

// Send a mouse move event to the browser. The |x| and |y| coordinates are
// relative to the upper-left corner of the view.
procedure cef_browser_host_send_mouse_move_event(self: PCefBrowserHost; const event: PCefMouseEvent; mouseLeave: cint); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefBrowserHostOwn(TWACef.GetObject(self)).SendMouseMoveEvent(
		event^,
		mouseLeave <> 0
	);
end;

// Send a mouse wheel event to the browser. The |x| and |y| coordinates are
// relative to the upper-left corner of the view. The |deltaX| and |deltaY|
// values represent the movement delta in the X and Y directions respectively.
// In order to scroll inside select popups with window rendering disabled
// cef_render_handler_t::GetScreenPoint should be implemented properly.
procedure cef_browser_host_send_mouse_wheel_event(self: PCefBrowserHost; const event: PCefMouseEvent; deltaX: cint; deltaY: cint); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefBrowserHostOwn(TWACef.GetObject(self)).SendMouseWheelEvent(
		event^,
		deltaX,
		deltaY
	);
end;

// Send a focus event to the browser.
procedure cef_browser_host_send_focus_event(self: PCefBrowserHost; setFocus: cint); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefBrowserHostOwn(TWACef.GetObject(self)).SendFocusEvent(
		setFocus <> 0
	);
end;

// Send a capture lost event to the browser.
procedure cef_browser_host_send_capture_lost_event(self: PCefBrowserHost); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefBrowserHostOwn(TWACef.GetObject(self)).SendCaptureLostEvent;
end;

// Notify the browser that the window hosting it is about to be moved or
// resized. This function is only used on Windows and Linux.
procedure cef_browser_host_notify_move_or_resize_started(self: PCefBrowserHost); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefBrowserHostOwn(TWACef.GetObject(self)).NotifyMoveOrResizeStarted;
end;

// Get the NSTextInputContext implementation for enabling IME on Mac when
// window rendering is disabled.
function cef_browser_host_get_nstext_input_context(self: PCefBrowserHost): TCefTextInputContext; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := (
		TCefBrowserHostOwn(TWACef.GetObject(self)).GetNstextInputContext
	);
end;

// Handles a keyDown event prior to passing it through the NSTextInputClient
// machinery.
procedure cef_browser_host_handle_key_event_before_text_input_client(self: PCefBrowserHost; keyEvent: TCefEventHandle); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefBrowserHostOwn(TWACef.GetObject(self)).HandleKeyEventBeforeTextInputClient(
		keyEvent
	);
end;

// Performs any additional actions after NSTextInputClient handles the event.
procedure cef_browser_host_handle_key_event_after_text_input_client(self: PCefBrowserHost; keyEvent: TCefEventHandle); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefBrowserHostOwn(TWACef.GetObject(self)).HandleKeyEventAfterTextInputClient(
		keyEvent
	);
end;

// Call this function when the user drags the mouse into the web view (before
// calling DragTargetDragOver/DragTargetLeave/DragTargetDrop). |drag_data|
// should not contain file contents as this type of data is not allowed to be
// dragged into the web view. File contents can be removed using
// cef_drag_data_t::ResetFileContents (for example, if |drag_data| comes from
// cef_render_handler_t::StartDragging). This function is only used when
// window rendering is disabled.
procedure cef_browser_host_drag_target_drag_enter(self: PCefBrowserHost; drag_data: PCefDragData; const event: PCefMouseEvent; allowed_ops: TCefDragOperationsMask); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefBrowserHostOwn(TWACef.GetObject(self)).DragTargetDragEnter(
		TCefDragDataRef.UnWrap(drag_data),
		event^,
		allowed_ops
	);
end;

// Call this function each time the mouse is moved across the web view during
// a drag operation (after calling DragTargetDragEnter and before calling
// DragTargetDragLeave/DragTargetDrop). This function is only used when window
// rendering is disabled.
procedure cef_browser_host_drag_target_drag_over(self: PCefBrowserHost; const event: PCefMouseEvent; allowed_ops: TCefDragOperationsMask); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefBrowserHostOwn(TWACef.GetObject(self)).DragTargetDragOver(
		event^,
		allowed_ops
	);
end;

// Call this function when the user drags the mouse out of the web view (after
// calling DragTargetDragEnter). This function is only used when window
// rendering is disabled.
procedure cef_browser_host_drag_target_drag_leave(self: PCefBrowserHost); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefBrowserHostOwn(TWACef.GetObject(self)).DragTargetDragLeave;
end;

// Call this function when the user completes the drag operation by dropping
// the object onto the web view (after calling DragTargetDragEnter). The
// object being dropped is |drag_data|, given as an argument to the previous
// DragTargetDragEnter call. This function is only used when window rendering
// is disabled.
procedure cef_browser_host_drag_target_drop(self: PCefBrowserHost; const event: PCefMouseEvent); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefBrowserHostOwn(TWACef.GetObject(self)).DragTargetDrop(
		event^
	);
end;

// Call this function when the drag operation started by a
// cef_render_handler_t::StartDragging call has ended either in a drop or by
// being cancelled. |x| and |y| are mouse coordinates relative to the upper-
// left corner of the view. If the web view is both the drag source and the
// drag target then all DragTarget* functions should be called before
// DragSource* mthods. This function is only used when window rendering is
// disabled.
procedure cef_browser_host_drag_source_ended_at(self: PCefBrowserHost; x: cint; y: cint; op: TCefDragOperationsMask); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefBrowserHostOwn(TWACef.GetObject(self)).DragSourceEndedAt(
		x,
		y,
		op
	);
end;

// Call this function when the drag operation started by a
// cef_render_handler_t::StartDragging call has completed. This function may
// be called immediately without first calling DragSourceEndedAt to cancel a
// drag operation. If the web view is both the drag source and the drag target
// then all DragTarget* functions should be called before DragSource* mthods.
// This function is only used when window rendering is disabled.
procedure cef_browser_host_drag_source_system_drag_ended(self: PCefBrowserHost); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefBrowserHostOwn(TWACef.GetObject(self)).DragSourceSystemDragEnded;
end;

{Protected section}
function TCefBrowserHostOwn.GetBrowser: ICefBrowser;
begin
	Result := nil;
end;

procedure TCefBrowserHostOwn.CloseBrowser(aForceClose: Boolean);
begin
end;

procedure TCefBrowserHostOwn.SetFocus(aFocus: Boolean);
begin
end;

procedure TCefBrowserHostOwn.SetWindowVisibility(aVisible: Boolean);
begin
end;

function TCefBrowserHostOwn.GetWindowHandle: TCefWindowHandle;
begin
end;

function TCefBrowserHostOwn.GetOpenerWindowHandle: TCefWindowHandle;
begin
end;

function TCefBrowserHostOwn.GetClient: ICefClient;
begin
	Result := nil;
end;

function TCefBrowserHostOwn.GetRequestContext: ICefRequestContext;
begin
	Result := nil;
end;

function TCefBrowserHostOwn.GetZoomLevel: cdouble;
begin
end;

procedure TCefBrowserHostOwn.SetZoomLevel(aZoomLevel: cdouble);
begin
end;

procedure TCefBrowserHostOwn.RunFileDialog(aMode: TCefFileDialogMode; const aTitle: ustring; const aDefaultFilePath: ustring; aAcceptFilters: TStrings; aSelectedAcceptFilter: cint; const aCallback: ICefRunFileDialogCallback);
begin
end;

procedure TCefBrowserHostOwn.StartDownload(const aUrl: ustring);
begin
end;

procedure TCefBrowserHostOwn.Print;
begin
end;

procedure TCefBrowserHostOwn.Find(aIdentifier: cint; const aSearchText: ustring; aForward: Boolean; aMatchCase: Boolean; aFindNext: Boolean);
begin
end;

procedure TCefBrowserHostOwn.StopFinding(aClearSelection: Boolean);
begin
end;

procedure TCefBrowserHostOwn.ShowDevTools(var aWindowInfo: TCefWindowInfo; const aClient: ICefClient; var aSettings: TCefBrowserSettings; aInspectElementAt: TCefPoint);
begin
end;

procedure TCefBrowserHostOwn.ShowDevTools(var aWindowInfo: TCefWindowInfo; const aClient: ICefClient; var aSettings: TCefBrowserSettings);
begin
end;

procedure TCefBrowserHostOwn.CloseDevTools;
begin
end;

procedure TCefBrowserHostOwn.GetNavigationEntries(const aVisitor: ICefNavigationEntryVisitor; aCurrentOnly: Boolean);
begin
end;

procedure TCefBrowserHostOwn.SetMouseCursorChangeDisabled(aDisabled: Boolean);
begin
end;

function TCefBrowserHostOwn.IsMouseCursorChangeDisabled: Boolean;
begin
	Result := false;
end;

procedure TCefBrowserHostOwn.ReplaceMisspelling(const aWord: ustring);
begin
end;

procedure TCefBrowserHostOwn.AddWordToDictionary(const aWord: ustring);
begin
end;

function TCefBrowserHostOwn.IsWindowRenderingDisabled: Boolean;
begin
	Result := false;
end;

procedure TCefBrowserHostOwn.WasResized;
begin
end;

procedure TCefBrowserHostOwn.WasHidden(aHidden: Boolean);
begin
end;

procedure TCefBrowserHostOwn.NotifyScreenInfoChanged;
begin
end;

procedure TCefBrowserHostOwn.Invalidate(const aType: TCefPaintElementType);
begin
end;

procedure TCefBrowserHostOwn.SendKeyEvent(const aEvent: TCefKeyEvent);
begin
end;

procedure TCefBrowserHostOwn.SendMouseClickEvent(const aEvent: TCefMouseEvent; aType: TCefMouseButtonType; aMouseUp: Boolean; aClickCount: cint);
begin
end;

procedure TCefBrowserHostOwn.SendMouseMoveEvent(const aEvent: TCefMouseEvent; aMouseLeave: Boolean);
begin
end;

procedure TCefBrowserHostOwn.SendMouseWheelEvent(const aEvent: TCefMouseEvent; aDeltaX: cint; aDeltaY: cint);
begin
end;

procedure TCefBrowserHostOwn.SendFocusEvent(aSetFocus: Boolean);
begin
end;

procedure TCefBrowserHostOwn.SendCaptureLostEvent;
begin
end;

procedure TCefBrowserHostOwn.NotifyMoveOrResizeStarted;
begin
end;

function TCefBrowserHostOwn.GetNstextInputContext: TCefTextInputContext;
begin
end;

procedure TCefBrowserHostOwn.HandleKeyEventBeforeTextInputClient(aKeyEvent: TCefEventHandle);
begin
end;

procedure TCefBrowserHostOwn.HandleKeyEventAfterTextInputClient(aKeyEvent: TCefEventHandle);
begin
end;

procedure TCefBrowserHostOwn.DragTargetDragEnter(const aDragData: ICefDragData; const aEvent: TCefMouseEvent; aAllowedOps: TCefDragOperationsMask);
begin
end;

procedure TCefBrowserHostOwn.DragTargetDragOver(const aEvent: TCefMouseEvent; aAllowedOps: TCefDragOperationsMask);
begin
end;

procedure TCefBrowserHostOwn.DragTargetDragLeave;
begin
end;

procedure TCefBrowserHostOwn.DragTargetDrop(const aEvent: TCefMouseEvent);
begin
end;

procedure TCefBrowserHostOwn.DragSourceEndedAt(aX: cint; aY: cint; aOp: TCefDragOperationsMask);
begin
end;

procedure TCefBrowserHostOwn.DragSourceSystemDragEnded;
begin
end;

{Public section}
constructor TCefBrowserHostOwn.Create;
begin
	inherited CreateData(SizeOf(TCefBrowserHost));
	with PCefBrowserHost(FData)^ do
	begin
		get_browser := {$IFDEF FPC}@{$ENDIF}cef_browser_host_get_browser;
		close_browser := {$IFDEF FPC}@{$ENDIF}cef_browser_host_close_browser;
		set_focus := {$IFDEF FPC}@{$ENDIF}cef_browser_host_set_focus;
		set_window_visibility := {$IFDEF FPC}@{$ENDIF}cef_browser_host_set_window_visibility;
		get_window_handle := {$IFDEF FPC}@{$ENDIF}cef_browser_host_get_window_handle;
		get_opener_window_handle := {$IFDEF FPC}@{$ENDIF}cef_browser_host_get_opener_window_handle;
		get_client := {$IFDEF FPC}@{$ENDIF}cef_browser_host_get_client;
		get_request_context := {$IFDEF FPC}@{$ENDIF}cef_browser_host_get_request_context;
		get_zoom_level := {$IFDEF FPC}@{$ENDIF}cef_browser_host_get_zoom_level;
		set_zoom_level := {$IFDEF FPC}@{$ENDIF}cef_browser_host_set_zoom_level;
		run_file_dialog := {$IFDEF FPC}@{$ENDIF}cef_browser_host_run_file_dialog;
		start_download := {$IFDEF FPC}@{$ENDIF}cef_browser_host_start_download;
		print := {$IFDEF FPC}@{$ENDIF}cef_browser_host_print;
		find := {$IFDEF FPC}@{$ENDIF}cef_browser_host_find;
		stop_finding := {$IFDEF FPC}@{$ENDIF}cef_browser_host_stop_finding;
		show_dev_tools := {$IFDEF FPC}@{$ENDIF}cef_browser_host_show_dev_tools;
		close_dev_tools := {$IFDEF FPC}@{$ENDIF}cef_browser_host_close_dev_tools;
		get_navigation_entries := {$IFDEF FPC}@{$ENDIF}cef_browser_host_get_navigation_entries;
		set_mouse_cursor_change_disabled := {$IFDEF FPC}@{$ENDIF}cef_browser_host_set_mouse_cursor_change_disabled;
		is_mouse_cursor_change_disabled := {$IFDEF FPC}@{$ENDIF}cef_browser_host_is_mouse_cursor_change_disabled;
		replace_misspelling := {$IFDEF FPC}@{$ENDIF}cef_browser_host_replace_misspelling;
		add_word_to_dictionary := {$IFDEF FPC}@{$ENDIF}cef_browser_host_add_word_to_dictionary;
		is_window_rendering_disabled := {$IFDEF FPC}@{$ENDIF}cef_browser_host_is_window_rendering_disabled;
		was_resized := {$IFDEF FPC}@{$ENDIF}cef_browser_host_was_resized;
		was_hidden := {$IFDEF FPC}@{$ENDIF}cef_browser_host_was_hidden;
		notify_screen_info_changed := {$IFDEF FPC}@{$ENDIF}cef_browser_host_notify_screen_info_changed;
		invalidate := {$IFDEF FPC}@{$ENDIF}cef_browser_host_invalidate;
		send_key_event := {$IFDEF FPC}@{$ENDIF}cef_browser_host_send_key_event;
		send_mouse_click_event := {$IFDEF FPC}@{$ENDIF}cef_browser_host_send_mouse_click_event;
		send_mouse_move_event := {$IFDEF FPC}@{$ENDIF}cef_browser_host_send_mouse_move_event;
		send_mouse_wheel_event := {$IFDEF FPC}@{$ENDIF}cef_browser_host_send_mouse_wheel_event;
		send_focus_event := {$IFDEF FPC}@{$ENDIF}cef_browser_host_send_focus_event;
		send_capture_lost_event := {$IFDEF FPC}@{$ENDIF}cef_browser_host_send_capture_lost_event;
		notify_move_or_resize_started := {$IFDEF FPC}@{$ENDIF}cef_browser_host_notify_move_or_resize_started;
		get_nstext_input_context := {$IFDEF FPC}@{$ENDIF}cef_browser_host_get_nstext_input_context;
		handle_key_event_before_text_input_client := {$IFDEF FPC}@{$ENDIF}cef_browser_host_handle_key_event_before_text_input_client;
		handle_key_event_after_text_input_client := {$IFDEF FPC}@{$ENDIF}cef_browser_host_handle_key_event_after_text_input_client;
		drag_target_drag_enter := {$IFDEF FPC}@{$ENDIF}cef_browser_host_drag_target_drag_enter;
		drag_target_drag_over := {$IFDEF FPC}@{$ENDIF}cef_browser_host_drag_target_drag_over;
		drag_target_drag_leave := {$IFDEF FPC}@{$ENDIF}cef_browser_host_drag_target_drag_leave;
		drag_target_drop := {$IFDEF FPC}@{$ENDIF}cef_browser_host_drag_target_drop;
		drag_source_ended_at := {$IFDEF FPC}@{$ENDIF}cef_browser_host_drag_source_ended_at;
		drag_source_system_drag_ended := {$IFDEF FPC}@{$ENDIF}cef_browser_host_drag_source_system_drag_ended;
	end;
end;

//..............................................................................TCefBrowserProcessHandlerOwn
// Called on the browser process UI thread immediately after the CEF context
// has been initialized.
procedure cef_browser_process_handler_on_context_initialized(self: PCefBrowserProcessHandler); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefBrowserProcessHandlerOwn(TWACef.GetObject(self)).OnContextInitialized;
end;

// Called before a child process is launched. Will be called on the browser
// process UI thread when launching a render process and on the browser
// process IO thread when launching a GPU or plugin process. Provides an
// opportunity to modify the child process command line. Do not keep a
// reference to |command_line| outside of this function.
procedure cef_browser_process_handler_on_before_child_process_launch(self: PCefBrowserProcessHandler; command_line: PCefCommandLine); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefBrowserProcessHandlerOwn(TWACef.GetObject(self)).OnBeforeChildProcessLaunch(
		TCefCommandLineRef.UnWrap(command_line)
	);
end;

// Called on the browser process IO thread after the main thread has been
// created for a new render process. Provides an opportunity to specify extra
// information that will be passed to
// cef_render_process_handler_t::on_render_thread_created() in the render
// process. Do not keep a reference to |extra_info| outside of this function.
procedure cef_browser_process_handler_on_render_process_thread_created(self: PCefBrowserProcessHandler; extra_info: PCefListValue); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefBrowserProcessHandlerOwn(TWACef.GetObject(self)).OnRenderProcessThreadCreated(
		TCefListValueRef.UnWrap(extra_info)
	);
end;

// Return the handler for printing on Linux. If a print handler is not
// provided then printing will not be supported on the Linux platform.
function cef_browser_process_handler_get_print_handler(self: PCefBrowserProcessHandler): PCefPrintHandler; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.GetData(
		TCefBrowserProcessHandlerOwn(TWACef.GetObject(self)).GetPrintHandler
	);
end;

{Protected section}
procedure TCefBrowserProcessHandlerOwn.OnContextInitialized;
begin
end;

procedure TCefBrowserProcessHandlerOwn.OnBeforeChildProcessLaunch(const aCommandLine: ICefCommandLine);
begin
end;

procedure TCefBrowserProcessHandlerOwn.OnRenderProcessThreadCreated(const aExtraInfo: ICefListValue);
begin
end;

function TCefBrowserProcessHandlerOwn.GetPrintHandler: ICefPrintHandler;
begin
	Result := nil;
end;

{Public section}
constructor TCefBrowserProcessHandlerOwn.Create;
begin
	inherited CreateData(SizeOf(TCefBrowserProcessHandler));
	with PCefBrowserProcessHandler(FData)^ do
	begin
		on_context_initialized := {$IFDEF FPC}@{$ENDIF}cef_browser_process_handler_on_context_initialized;
		on_before_child_process_launch := {$IFDEF FPC}@{$ENDIF}cef_browser_process_handler_on_before_child_process_launch;
		on_render_process_thread_created := {$IFDEF FPC}@{$ENDIF}cef_browser_process_handler_on_render_process_thread_created;
		get_print_handler := {$IFDEF FPC}@{$ENDIF}cef_browser_process_handler_get_print_handler;
	end;
end;

//..............................................................................TCefCallbackOwn
// Continue processing.
procedure cef_callback_cont(self: PCefCallback); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefCallbackOwn(TWACef.GetObject(self)).Cont;
end;

// Cancel processing.
procedure cef_callback_cancel(self: PCefCallback); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefCallbackOwn(TWACef.GetObject(self)).Cancel;
end;

{Protected section}
procedure TCefCallbackOwn.Cont;
begin
end;

procedure TCefCallbackOwn.Cancel;
begin
end;

{Public section}
constructor TCefCallbackOwn.Create;
begin
	inherited CreateData(SizeOf(TCefCallback));
	with PCefCallback(FData)^ do
	begin
		cont := {$IFDEF FPC}@{$ENDIF}cef_callback_cont;
		cancel := {$IFDEF FPC}@{$ENDIF}cef_callback_cancel;
	end;
end;
//..............................................................................TCefCompletionCallbackOwn
// Method that will be called once the task is complete.
procedure cef_completion_callback_on_complete(self: PCefCompletionCallback); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefCompletionCallbackOwn(TWACef.GetObject(self)).OnComplete;
end;

{Protected section}
procedure TCefCompletionCallbackOwn.OnComplete;
begin
end;

{Public section}
constructor TCefCompletionCallbackOwn.Create;
begin
	inherited CreateData(SizeOf(TCefCompletionCallback));
	with PCefCompletionCallback(FData)^ do
	begin
		on_complete := {$IFDEF FPC}@{$ENDIF}cef_completion_callback_on_complete;
	end;
end;

//..............................................................................TCefClientOwn
// Return the handler for context menus. If no handler is provided the default
// implementation will be used.
function cef_client_get_context_menu_handler(self: PCefClient): PCefContextMenuHandler; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.GetData(
    TCefClientOwn(TWACef.GetObject(self)).GetContextMenuHandler
  );
end;

// Return the handler for dialogs. If no handler is provided the default
// implementation will be used.
function cef_client_get_dialog_handler(self: PCefClient): PCefDialogHandler; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.GetData(
		TCefClientOwn(TWACef.GetObject(self)).GetDialogHandler
	);
end;

// Return the handler for browser display state events.
function cef_client_get_display_handler(self: PCefClient): PCefDisplayHandler; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.GetData(
		TCefClientOwn(TWACef.GetObject(self)).GetDisplayHandler
	);
end;

// Return the handler for download events. If no handler is returned downloads
// will not be allowed.
function cef_client_get_download_handler(self: PCefClient): PCefDownloadHandler; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.GetData(
		TCefClientOwn(TWACef.GetObject(self)).GetDownloadHandler
	);
end;

// Return the handler for drag events.
function cef_client_get_drag_handler(self: PCefClient): PCefDragHandler; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.GetData(
		TCefClientOwn(TWACef.GetObject(self)).GetDragHandler
	);
end;

// Return the handler for find result events.
function cef_client_get_find_handler(self: PCefClient): PCefFindHandler; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.GetData(
		TCefClientOwn(TWACef.GetObject(self)).GetFindHandler
	);
end;

// Return the handler for focus events.
function cef_client_get_focus_handler(self: PCefClient): PCefFocusHandler; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.GetData(
		TCefClientOwn(TWACef.GetObject(self)).GetFocusHandler
	);
end;

// Return the handler for geolocation permissions requests. If no handler is
// provided geolocation access will be denied by default.
function cef_client_get_geolocation_handler(self: PCefClient): PCefGeolocationHandler; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.GetData(
		TCefClientOwn(TWACef.GetObject(self)).GetGeolocationHandler
	);
end;

// Return the handler for JavaScript dialogs. If no handler is provided the
// default implementation will be used.
function cef_client_get_jsdialog_handler(self: PCefClient): PCefJsdialogHandler; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.GetData(
		TCefClientOwn(TWACef.GetObject(self)).GetJsdialogHandler
	);
end;

// Return the handler for keyboard events.
function cef_client_get_keyboard_handler(self: PCefClient): PCefKeyboardHandler; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.GetData(
		TCefClientOwn(TWACef.GetObject(self)).GetKeyboardHandler
	);
end;

// Return the handler for browser life span events.
function cef_client_get_life_span_handler(self: PCefClient): PCefLifeSpanHandler; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.GetData(
		TCefClientOwn(TWACef.GetObject(self)).GetLifeSpanHandler
	);
end;

// Return the handler for browser load status events.
function cef_client_get_load_handler(self: PCefClient): PCefLoadHandler; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.GetData(
		TCefClientOwn(TWACef.GetObject(self)).GetLoadHandler
	);
end;

// Return the handler for off-screen rendering events.
function cef_client_get_render_handler(self: PCefClient): PCefRenderHandler; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.GetData(
		TCefClientOwn(TWACef.GetObject(self)).GetRenderHandler
	);
end;

// Return the handler for browser request events.
function cef_client_get_request_handler(self: PCefClient): PCefRequestHandler; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.GetData(
		TCefClientOwn(TWACef.GetObject(self)).GetRequestHandler
	);
end;

// Called when a new message is received from a different process. Return true
// (1) if the message was handled or false (0) otherwise. Do not keep a
// reference to or attempt to access the message outside of this callback.
function cef_client_on_process_message_received(self: PCefClient; browser: PCefBrowser; source_process: TCefProcessId; message: PCefProcessMessage): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefClientOwn(TWACef.GetObject(self)).OnProcessMessageReceived(
			TCefBrowserRef.UnWrap(browser),
			source_process,
			TCefProcessMessageRef.UnWrap(message)
		)
	);
end;

{Protected section}
function TCefClientOwn.GetContextMenuHandler: ICefContextMenuHandler;
begin
	Result := nil;
end;

function TCefClientOwn.GetDialogHandler: ICefDialogHandler;
begin
	Result := nil;
end;

function TCefClientOwn.GetDisplayHandler: ICefDisplayHandler;
begin
	Result := nil;
end;

function TCefClientOwn.GetDownloadHandler: ICefDownloadHandler;
begin
	Result := nil;
end;

function TCefClientOwn.GetDragHandler: ICefDragHandler;
begin
	Result := nil;
end;

function TCefClientOwn.GetFindHandler: ICefFindHandler;
begin
	Result := nil;
end;

function TCefClientOwn.GetFocusHandler: ICefFocusHandler;
begin
	Result := nil;
end;

function TCefClientOwn.GetGeolocationHandler: ICefGeolocationHandler;
begin
	Result := nil;
end;

function TCefClientOwn.GetJsdialogHandler: ICefJsdialogHandler;
begin
	Result := nil;
end;

function TCefClientOwn.GetKeyboardHandler: ICefKeyboardHandler;
begin
	Result := nil;
end;

function TCefClientOwn.GetLifeSpanHandler: ICefLifeSpanHandler;
begin
	Result := nil;
end;

function TCefClientOwn.GetLoadHandler: ICefLoadHandler;
begin
	Result := nil;
end;

function TCefClientOwn.GetRenderHandler: ICefRenderHandler;
begin
	Result := nil;
end;

function TCefClientOwn.GetRequestHandler: ICefRequestHandler;
begin
	Result := nil;
end;

function TCefClientOwn.OnProcessMessageReceived(const aBrowser: ICefBrowser; aSourceProcess: TCefProcessId; const aMessage: ICefProcessMessage): Boolean;
begin
	Result := false;
end;

{Public section}
constructor TCefClientOwn.Create;
begin
	inherited CreateData(SizeOf(TCefClient));
	with PCefClient(FData)^ do
	begin
		get_context_menu_handler := {$IFDEF FPC}@{$ENDIF}cef_client_get_context_menu_handler;
		get_dialog_handler := {$IFDEF FPC}@{$ENDIF}cef_client_get_dialog_handler;
		get_display_handler := {$IFDEF FPC}@{$ENDIF}cef_client_get_display_handler;
		get_download_handler := {$IFDEF FPC}@{$ENDIF}cef_client_get_download_handler;
		get_drag_handler := {$IFDEF FPC}@{$ENDIF}cef_client_get_drag_handler;
		get_find_handler := {$IFDEF FPC}@{$ENDIF}cef_client_get_find_handler;
		get_focus_handler := {$IFDEF FPC}@{$ENDIF}cef_client_get_focus_handler;
		get_geolocation_handler := {$IFDEF FPC}@{$ENDIF}cef_client_get_geolocation_handler;
		get_jsdialog_handler := {$IFDEF FPC}@{$ENDIF}cef_client_get_jsdialog_handler;
		get_keyboard_handler := {$IFDEF FPC}@{$ENDIF}cef_client_get_keyboard_handler;
		get_life_span_handler := {$IFDEF FPC}@{$ENDIF}cef_client_get_life_span_handler;
		get_load_handler := {$IFDEF FPC}@{$ENDIF}cef_client_get_load_handler;
		get_render_handler := {$IFDEF FPC}@{$ENDIF}cef_client_get_render_handler;
		get_request_handler := {$IFDEF FPC}@{$ENDIF}cef_client_get_request_handler;
		on_process_message_received := {$IFDEF FPC}@{$ENDIF}cef_client_on_process_message_received;
	end;
end;

//..............................................................................TCefCommandLineOwn
// Returns true (1) if this object is valid. Do not call any other functions
// if this function returns false (0).
function cef_command_line_is_valid(self: PCefCommandLine): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefCommandLineOwn(TWACef.GetObject(self)).IsValid
	);
end;

// Returns true (1) if the values of this object are read-only. Some APIs may
// expose read-only objects.
function cef_command_line_is_read_only(self: PCefCommandLine): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefCommandLineOwn(TWACef.GetObject(self)).IsReadOnly
	);
end;

// Returns a writable copy of this object.
function cef_command_line_copy(self: PCefCommandLine): PCefCommandLine; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.GetData(
		TCefCommandLineOwn(TWACef.GetObject(self)).Copy
	);
end;

// Initialize the command line with the specified |argc| and |argv| values.
// The first argument must be the name of the program. This function is only
// supported on non-Windows platforms.
procedure cef_command_line_init_from_argv(self: PCefCommandLine; argc: cint; const argv: PPAnsiChar); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefCommandLineOwn(TWACef.GetObject(self)).InitFromArgv(
		argc,
		argv
	);
end;

// Initialize the command line with the string returned by calling
// GetCommandLineW(). This function is only supported on Windows.
procedure cef_command_line_init_from_string(self: PCefCommandLine; const command_line: PCefString); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	command_line_str: ustring;
begin
	command_line_str := TWACef.ToString(command_line);
	TCefCommandLineOwn(TWACef.GetObject(self)).InitFromString(
		command_line_str
	);
end;

// Reset the command-line switches and arguments but leave the program
// component unchanged.
procedure cef_command_line_reset(self: PCefCommandLine); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefCommandLineOwn(TWACef.GetObject(self)).Reset;
end;

// Retrieve the original command line string as a vector of strings. The argv
// array: { program, [(--|-|/)switch[=value]]*, [--], [argument]* }
procedure cef_command_line_get_argv(self: PCefCommandLine; argv: TCefStringList); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefCommandLineOwn(TWACef.GetObject(self)).GetArgv(
		argv
	);
end;

// Constructs and returns the represented command line string. Use this
// function cautiously because quoting behavior is unclear.
// The resulting string must be freed by calling cef_string_userfree_free().
function cef_command_line_get_command_line_string(self: PCefCommandLine): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.UserFreeString(
		TCefCommandLineOwn(TWACef.GetObject(self)).GetCommandLineString
	);
end;

// Get the program part of the command line string (the first item).
// The resulting string must be freed by calling cef_string_userfree_free().
function cef_command_line_get_program(self: PCefCommandLine): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.UserFreeString(
		TCefCommandLineOwn(TWACef.GetObject(self)).GetProgram
	);
end;

// Set the program part of the command line string (the first item).
procedure cef_command_line_set_program(self: PCefCommandLine; const _program: PCefString); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	program_str: ustring;
begin
	program_str := TWACef.ToString(_program);
	TCefCommandLineOwn(TWACef.GetObject(self)).SetProgram(
		program_str
	);
end;

// Returns true (1) if the command line has switches.
function cef_command_line_has_switches(self: PCefCommandLine): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefCommandLineOwn(TWACef.GetObject(self)).HasSwitches
	);
end;

// Returns true (1) if the command line contains the given switch.
function cef_command_line_has_switch(self: PCefCommandLine; const name: PCefString): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	name_str: ustring;
begin
	name_str := TWACef.ToString(name);
	Result := Ord(
		TCefCommandLineOwn(TWACef.GetObject(self)).HasSwitch(
			name_str
		)
	);
end;

// Returns the value associated with the given switch. If the switch has no
// value or isn't present this function returns the NULL string.
// The resulting string must be freed by calling cef_string_userfree_free().
function cef_command_line_get_switch_value(self: PCefCommandLine; const name: PCefString): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	name_str: ustring;
begin
	name_str := TWACef.ToString(name);
	Result := TWACef.UserFreeString(
		TCefCommandLineOwn(TWACef.GetObject(self)).GetSwitchValue(
			name_str
		)
	);
end;

// Returns the map of switch names and values. If a switch has no value an
// NULL string is returned.
procedure cef_command_line_get_switches(self: PCefCommandLine; switches: TCefStringMap); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefCommandLineOwn(TWACef.GetObject(self)).GetSwitches(
		switches
	);
end;

// Add a switch to the end of the command line. If the switch has no value
// pass an NULL value string.
procedure cef_command_line_append_switch(self: PCefCommandLine; const name: PCefString); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	name_str: ustring;
begin
	name_str := TWACef.ToString(name);
	TCefCommandLineOwn(TWACef.GetObject(self)).AppendSwitch(
		name_str
	);
end;

// Add a switch with the specified value to the end of the command line.
procedure cef_command_line_append_switch_with_value(self: PCefCommandLine; const name: PCefString; const value: PCefString); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	name_str: ustring;
	value_str: ustring;
begin
	name_str := TWACef.ToString(name);
	value_str := TWACef.ToString(value);
	TCefCommandLineOwn(TWACef.GetObject(self)).AppendSwitchWithValue(
		name_str,
		value_str
	);
end;

// True if there are remaining command line arguments.
function cef_command_line_has_arguments(self: PCefCommandLine): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefCommandLineOwn(TWACef.GetObject(self)).HasArguments
	);
end;

// Get the remaining command line arguments.
procedure cef_command_line_get_arguments(self: PCefCommandLine; arguments: TCefStringList); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefCommandLineOwn(TWACef.GetObject(self)).GetArguments(
		arguments
	);
end;

// Add an argument to the end of the command line.
procedure cef_command_line_append_argument(self: PCefCommandLine; const argument: PCefString); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	argument_str: ustring;
begin
	argument_str := TWACef.ToString(argument);
	TCefCommandLineOwn(TWACef.GetObject(self)).AppendArgument(
		argument_str
	);
end;

// Insert a command before the current command. Common for debuggers, like
// "valgrind" or "gdb --args".
procedure cef_command_line_prepend_wrapper(self: PCefCommandLine; const wrapper: PCefString); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	wrapper_str: ustring;
begin
	wrapper_str := TWACef.ToString(wrapper);
	TCefCommandLineOwn(TWACef.GetObject(self)).PrependWrapper(
		wrapper_str
	);
end;

{Protected section}
function TCefCommandLineOwn.IsValid: Boolean;
begin
	Result := false;
end;

function TCefCommandLineOwn.IsReadOnly: Boolean;
begin
	Result := false;
end;

function TCefCommandLineOwn.Copy: ICefCommandLine;
begin
	Result := nil;
end;

procedure TCefCommandLineOwn.InitFromArgv(aArgc: cint; const aArgv: PPAnsiChar);
begin
end;

procedure TCefCommandLineOwn.InitFromString(const aCommandLine: ustring);
begin
end;

procedure TCefCommandLineOwn.Reset;
begin
end;

procedure TCefCommandLineOwn.GetArgv(aArgv: TStrings);
begin
end;

function TCefCommandLineOwn.GetCommandLineString: ustring;
begin
end;

function TCefCommandLineOwn.GetProgram: ustring;
begin
end;

procedure TCefCommandLineOwn.SetProgram(const aProgram: ustring);
begin
end;

function TCefCommandLineOwn.HasSwitches: Boolean;
begin
	Result := false;
end;

function TCefCommandLineOwn.HasSwitch(const aName: ustring): Boolean;
begin
	Result := false;
end;

function TCefCommandLineOwn.GetSwitchValue(const aName: ustring): ustring;
begin
end;

procedure TCefCommandLineOwn.GetSwitches(aSwitches: TStrings);
begin
end;

procedure TCefCommandLineOwn.AppendSwitch(const aName: ustring);
begin
end;

procedure TCefCommandLineOwn.AppendSwitchWithValue(const aName: ustring; const aValue: ustring);
begin
end;

function TCefCommandLineOwn.HasArguments: Boolean;
begin
	Result := false;
end;

procedure TCefCommandLineOwn.GetArguments(aArguments: TStrings);
begin
end;

procedure TCefCommandLineOwn.AppendArgument(const aArgument: ustring);
begin
end;

procedure TCefCommandLineOwn.PrependWrapper(const aWrapper: ustring);
begin
end;

{Public section}
constructor TCefCommandLineOwn.Create;
begin
	inherited CreateData(SizeOf(TCefCommandLine));
	with PCefCommandLine(FData)^ do
	begin
		is_valid := {$IFDEF FPC}@{$ENDIF}cef_command_line_is_valid;
		is_read_only := {$IFDEF FPC}@{$ENDIF}cef_command_line_is_read_only;
		copy := {$IFDEF FPC}@{$ENDIF}cef_command_line_copy;
		init_from_argv := {$IFDEF FPC}@{$ENDIF}cef_command_line_init_from_argv;
		init_from_string := {$IFDEF FPC}@{$ENDIF}cef_command_line_init_from_string;
		reset := {$IFDEF FPC}@{$ENDIF}cef_command_line_reset;
		get_argv := {$IFDEF FPC}@{$ENDIF}cef_command_line_get_argv;
		get_command_line_string := {$IFDEF FPC}@{$ENDIF}cef_command_line_get_command_line_string;
		get_program := {$IFDEF FPC}@{$ENDIF}cef_command_line_get_program;
		set_program := {$IFDEF FPC}@{$ENDIF}cef_command_line_set_program;
		has_switches := {$IFDEF FPC}@{$ENDIF}cef_command_line_has_switches;
		has_switch := {$IFDEF FPC}@{$ENDIF}cef_command_line_has_switch;
		get_switch_value := {$IFDEF FPC}@{$ENDIF}cef_command_line_get_switch_value;
		get_switches := {$IFDEF FPC}@{$ENDIF}cef_command_line_get_switches;
		append_switch := {$IFDEF FPC}@{$ENDIF}cef_command_line_append_switch;
		append_switch_with_value := {$IFDEF FPC}@{$ENDIF}cef_command_line_append_switch_with_value;
		has_arguments := {$IFDEF FPC}@{$ENDIF}cef_command_line_has_arguments;
		get_arguments := {$IFDEF FPC}@{$ENDIF}cef_command_line_get_arguments;
		append_argument := {$IFDEF FPC}@{$ENDIF}cef_command_line_append_argument;
		prepend_wrapper := {$IFDEF FPC}@{$ENDIF}cef_command_line_prepend_wrapper;
	end;
end;

//..............................................................................TCefContextMenuHandlerOwn
// Called before a context menu is displayed. |params| provides information
// about the context menu state. |model| initially contains the default
// context menu. The |model| can be cleared to show no context menu or
// modified to show a custom menu. Do not keep references to |params| or
// |model| outside of this callback.
procedure cef_context_menu_handler_on_before_context_menu(self: PCefContextMenuHandler; browser: PCefBrowser; frame: PCefFrame; params: PCefContextMenuParams; model: PCefMenuModel); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefContextMenuHandlerOwn(TWACef.GetObject(self)).OnBeforeContextMenu(
		TCefBrowserRef.UnWrap(browser),
		TCefFrameRef.UnWrap(frame),
		TCefContextMenuParamsRef.UnWrap(params),
		TCefMenuModelRef.UnWrap(model)
	);
end;

// Called to execute a command selected from the context menu. Return true (1)
// if the command was handled or false (0) for the default implementation. See
// cef_menu_id_t for the command ids that have default implementations. All
// user-defined command ids should be between MENU_ID_USER_FIRST and
// MENU_ID_USER_LAST. |params| will have the same values as what was passed to
// on_before_context_menu(). Do not keep a reference to |params| outside of
// this callback.
function cef_context_menu_handler_on_context_menu_command(self: PCefContextMenuHandler; browser: PCefBrowser; frame: PCefFrame; params: PCefContextMenuParams; command_id: cint; event_flags: TCefEventFlags): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefContextMenuHandlerOwn(TWACef.GetObject(self)).OnContextMenuCommand(
			TCefBrowserRef.UnWrap(browser),
			TCefFrameRef.UnWrap(frame),
			TCefContextMenuParamsRef.UnWrap(params),
			command_id,
			event_flags
		)
	);
end;

// Called when the context menu is dismissed irregardless of whether the menu
// was NULL or a command was selected.
procedure cef_context_menu_handler_on_context_menu_dismissed(self: PCefContextMenuHandler; browser: PCefBrowser; frame: PCefFrame); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefContextMenuHandlerOwn(TWACef.GetObject(self)).OnContextMenuDismissed(
		TCefBrowserRef.UnWrap(browser),
		TCefFrameRef.UnWrap(frame)
	);
end;

{Protected section}
procedure TCefContextMenuHandlerOwn.OnBeforeContextMenu(const aBrowser: ICefBrowser; const aFrame: ICefFrame; const aParams: ICefContextMenuParams; const aModel: ICefMenuModel);
begin
end;

function TCefContextMenuHandlerOwn.OnContextMenuCommand(const aBrowser: ICefBrowser; const aFrame: ICefFrame; const aParams: ICefContextMenuParams; aCommandId: cint; aEventFlags: TCefEventFlags): Boolean;
begin
	Result := false;
end;

procedure TCefContextMenuHandlerOwn.OnContextMenuDismissed(const aBrowser: ICefBrowser; const aFrame: ICefFrame);
begin
end;

{Public section}
constructor TCefContextMenuHandlerOwn.Create;
begin
	inherited CreateData(SizeOf(TCefContextMenuHandler));
	with PCefContextMenuHandler(FData)^ do
	begin
		on_before_context_menu := {$IFDEF FPC}@{$ENDIF}cef_context_menu_handler_on_before_context_menu;
		on_context_menu_command := {$IFDEF FPC}@{$ENDIF}cef_context_menu_handler_on_context_menu_command;
		on_context_menu_dismissed := {$IFDEF FPC}@{$ENDIF}cef_context_menu_handler_on_context_menu_dismissed;
	end;
end;
//..............................................................................TCefContextMenuParamsOwn
// Returns the X coordinate of the mouse where the context menu was invoked.
// Coords are relative to the associated RenderView's origin.
function cef_context_menu_params_get_xcoord(self: PCefContextMenuParams): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := (
		TCefContextMenuParamsOwn(TWACef.GetObject(self)).GetXcoord
	);
end;

// Returns the Y coordinate of the mouse where the context menu was invoked.
// Coords are relative to the associated RenderView's origin.
function cef_context_menu_params_get_ycoord(self: PCefContextMenuParams): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := (
		TCefContextMenuParamsOwn(TWACef.GetObject(self)).GetYcoord
	);
end;

// Returns flags representing the type of node that the context menu was
// invoked on.
function cef_context_menu_params_get_type_flags(self: PCefContextMenuParams): TCefContextMenuTypeFlags; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := (
		TCefContextMenuParamsOwn(TWACef.GetObject(self)).GetTypeFlags
	);
end;

// Returns the URL of the link, if any, that encloses the node that the
// context menu was invoked on.
// The resulting string must be freed by calling cef_string_userfree_free().
function cef_context_menu_params_get_link_url(self: PCefContextMenuParams): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.UserFreeString(
		TCefContextMenuParamsOwn(TWACef.GetObject(self)).GetLinkUrl
	);
end;

// Returns the link URL, if any, to be used ONLY for "copy link address". We
// don't validate this field in the frontend process.
// The resulting string must be freed by calling cef_string_userfree_free().
function cef_context_menu_params_get_unfiltered_link_url(self: PCefContextMenuParams): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.UserFreeString(
		TCefContextMenuParamsOwn(TWACef.GetObject(self)).GetUnfilteredLinkUrl
	);
end;

// Returns the source URL, if any, for the element that the context menu was
// invoked on. Example of elements with source URLs are img, audio, and video.
// The resulting string must be freed by calling cef_string_userfree_free().
function cef_context_menu_params_get_source_url(self: PCefContextMenuParams): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.UserFreeString(
		TCefContextMenuParamsOwn(TWACef.GetObject(self)).GetSourceUrl
	);
end;

// Returns true (1) if the context menu was invoked on an image which has non-
// NULL contents.
function cef_context_menu_params_has_image_contents(self: PCefContextMenuParams): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefContextMenuParamsOwn(TWACef.GetObject(self)).HasImageContents
	);
end;

// Returns the URL of the top level page that the context menu was invoked on.
// The resulting string must be freed by calling cef_string_userfree_free().
function cef_context_menu_params_get_page_url(self: PCefContextMenuParams): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.UserFreeString(
		TCefContextMenuParamsOwn(TWACef.GetObject(self)).GetPageUrl
	);
end;

// Returns the URL of the subframe that the context menu was invoked on.
// The resulting string must be freed by calling cef_string_userfree_free().
function cef_context_menu_params_get_frame_url(self: PCefContextMenuParams): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.UserFreeString(
		TCefContextMenuParamsOwn(TWACef.GetObject(self)).GetFrameUrl
	);
end;

// Returns the character encoding of the subframe that the context menu was
// invoked on.
// The resulting string must be freed by calling cef_string_userfree_free().
function cef_context_menu_params_get_frame_charset(self: PCefContextMenuParams): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.UserFreeString(
		TCefContextMenuParamsOwn(TWACef.GetObject(self)).GetFrameCharset
	);
end;

// Returns the type of context node that the context menu was invoked on.
function cef_context_menu_params_get_media_type(self: PCefContextMenuParams): TCefContextMenuMediaType; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := (
		TCefContextMenuParamsOwn(TWACef.GetObject(self)).GetMediaType
	);
end;

// Returns flags representing the actions supported by the media element, if
// any, that the context menu was invoked on.
function cef_context_menu_params_get_media_state_flags(self: PCefContextMenuParams): TCefContextMenuMediaStateFlags; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := (
		TCefContextMenuParamsOwn(TWACef.GetObject(self)).GetMediaStateFlags
	);
end;

// Returns the text of the selection, if any, that the context menu was
// invoked on.
// The resulting string must be freed by calling cef_string_userfree_free().
function cef_context_menu_params_get_selection_text(self: PCefContextMenuParams): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.UserFreeString(
		TCefContextMenuParamsOwn(TWACef.GetObject(self)).GetSelectionText
	);
end;

// Returns the text of the misspelled word, if any, that the context menu was
// invoked on.
// The resulting string must be freed by calling cef_string_userfree_free().
function cef_context_menu_params_get_misspelled_word(self: PCefContextMenuParams): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.UserFreeString(
		TCefContextMenuParamsOwn(TWACef.GetObject(self)).GetMisspelledWord
	);
end;

// Returns true (1) if suggestions exist, false (0) otherwise. Fills in
// |suggestions| from the spell check service for the misspelled word if there
// is one.
function cef_context_menu_params_get_dictionary_suggestions(self: PCefContextMenuParams; suggestions: TCefStringList): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefContextMenuParamsOwn(TWACef.GetObject(self)).GetDictionarySuggestions(
			suggestions
		)
	);
end;

// Returns true (1) if the context menu was invoked on an editable node.
function cef_context_menu_params_is_editable(self: PCefContextMenuParams): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefContextMenuParamsOwn(TWACef.GetObject(self)).IsEditable
	);
end;

// Returns true (1) if the context menu was invoked on an editable node where
// spell-check is enabled.
function cef_context_menu_params_is_spell_check_enabled(self: PCefContextMenuParams): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefContextMenuParamsOwn(TWACef.GetObject(self)).IsSpellCheckEnabled
	);
end;

// Returns flags representing the actions supported by the editable node, if
// any, that the context menu was invoked on.
function cef_context_menu_params_get_edit_state_flags(self: PCefContextMenuParams): TCefContextMenuEditStateFlags; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := (
		TCefContextMenuParamsOwn(TWACef.GetObject(self)).GetEditStateFlags
	);
end;

{Protected section}
function TCefContextMenuParamsOwn.GetXcoord: cint;
begin
	Result := 0;
end;

function TCefContextMenuParamsOwn.GetYcoord: cint;
begin
	Result := 0;
end;

function TCefContextMenuParamsOwn.GetTypeFlags: TCefContextMenuTypeFlags;
begin
end;

function TCefContextMenuParamsOwn.GetLinkUrl: ustring;
begin
end;

function TCefContextMenuParamsOwn.GetUnfilteredLinkUrl: ustring;
begin
end;

function TCefContextMenuParamsOwn.GetSourceUrl: ustring;
begin
end;

function TCefContextMenuParamsOwn.HasImageContents: Boolean;
begin
	Result := false;
end;

function TCefContextMenuParamsOwn.GetPageUrl: ustring;
begin
end;

function TCefContextMenuParamsOwn.GetFrameUrl: ustring;
begin
end;

function TCefContextMenuParamsOwn.GetFrameCharset: ustring;
begin
end;

function TCefContextMenuParamsOwn.GetMediaType: TCefContextMenuMediaType;
begin
end;

function TCefContextMenuParamsOwn.GetMediaStateFlags: TCefContextMenuMediaStateFlags;
begin
end;

function TCefContextMenuParamsOwn.GetSelectionText: ustring;
begin
end;

function TCefContextMenuParamsOwn.GetMisspelledWord: ustring;
begin
end;

function TCefContextMenuParamsOwn.GetDictionarySuggestions(aSuggestions: TStrings): Boolean;
begin
	Result := false;
end;

function TCefContextMenuParamsOwn.IsEditable: Boolean;
begin
	Result := false;
end;

function TCefContextMenuParamsOwn.IsSpellCheckEnabled: Boolean;
begin
	Result := false;
end;

function TCefContextMenuParamsOwn.GetEditStateFlags: TCefContextMenuEditStateFlags;
begin
end;

{Public section}
constructor TCefContextMenuParamsOwn.Create;
begin
	inherited CreateData(SizeOf(TCefContextMenuParams));
	with PCefContextMenuParams(FData)^ do
	begin
		get_xcoord := {$IFDEF FPC}@{$ENDIF}cef_context_menu_params_get_xcoord;
		get_ycoord := {$IFDEF FPC}@{$ENDIF}cef_context_menu_params_get_ycoord;
		get_type_flags := {$IFDEF FPC}@{$ENDIF}cef_context_menu_params_get_type_flags;
		get_link_url := {$IFDEF FPC}@{$ENDIF}cef_context_menu_params_get_link_url;
		get_unfiltered_link_url := {$IFDEF FPC}@{$ENDIF}cef_context_menu_params_get_unfiltered_link_url;
		get_source_url := {$IFDEF FPC}@{$ENDIF}cef_context_menu_params_get_source_url;
		has_image_contents := {$IFDEF FPC}@{$ENDIF}cef_context_menu_params_has_image_contents;
		get_page_url := {$IFDEF FPC}@{$ENDIF}cef_context_menu_params_get_page_url;
		get_frame_url := {$IFDEF FPC}@{$ENDIF}cef_context_menu_params_get_frame_url;
		get_frame_charset := {$IFDEF FPC}@{$ENDIF}cef_context_menu_params_get_frame_charset;
		get_media_type := {$IFDEF FPC}@{$ENDIF}cef_context_menu_params_get_media_type;
		get_media_state_flags := {$IFDEF FPC}@{$ENDIF}cef_context_menu_params_get_media_state_flags;
		get_selection_text := {$IFDEF FPC}@{$ENDIF}cef_context_menu_params_get_selection_text;
		get_misspelled_word := {$IFDEF FPC}@{$ENDIF}cef_context_menu_params_get_misspelled_word;
		get_dictionary_suggestions := {$IFDEF FPC}@{$ENDIF}cef_context_menu_params_get_dictionary_suggestions;
		is_editable := {$IFDEF FPC}@{$ENDIF}cef_context_menu_params_is_editable;
		is_spell_check_enabled := {$IFDEF FPC}@{$ENDIF}cef_context_menu_params_is_spell_check_enabled;
		get_edit_state_flags := {$IFDEF FPC}@{$ENDIF}cef_context_menu_params_get_edit_state_flags;
	end;
end;

//..............................................................................TCefCookieManagerOwn
// Set the schemes supported by this manager. By default only "http" and
// "https" schemes are supported. If |callback| is non-NULL it will be
// executed asnychronously on the IO thread after the change has been applied.
// Must be called before any cookies are accessed.
procedure cef_cookie_manager_set_supported_schemes(self: PCefCookieManager; schemes: TCefStringList; callback: PCefCompletionCallback); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	schemes_list: TSTringList;
	schemes_iter: Integer;
	schemes_item: TCefString;
begin
	schemes_list := TStringList.Create;
	for schemes_iter := 0 to cef_string_list_size(schemes) - 1 do
	begin
		FillChar(schemes_item, SizeOf(schemes_item), 0);
		cef_string_list_value(schemes, schemes_iter, @schemes_item);
		schemes_list.Add(TWACef.StringClearAndGet(schemes_item));
	end;
	TCefCookieManagerOwn(TWACef.GetObject(self)).SetSupportedSchemes(
		schemes_list,
		TCefCompletionCallbackRef.UnWrap(callback)
	);

	schemes_list.Free;
end;

// Visit all cookies on the IO thread. The returned cookies are ordered by
// longest path, then by earliest creation date. Returns false (0) if cookies
// cannot be accessed.
function cef_cookie_manager_visit_all_cookies(self: PCefCookieManager; visitor: PCefCookieVisitor): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefCookieManagerOwn(TWACef.GetObject(self)).VisitAllCookies(
			TCefCookieVisitorRef.UnWrap(visitor)
		)
	);
end;

// Visit a subset of cookies on the IO thread. The results are filtered by the
// given url scheme, host, domain and path. If |includeHttpOnly| is true (1)
// HTTP-only cookies will also be included in the results. The returned
// cookies are ordered by longest path, then by earliest creation date.
// Returns false (0) if cookies cannot be accessed.
function cef_cookie_manager_visit_url_cookies(self: PCefCookieManager; const url: PCefString; includeHttpOnly: cint; visitor: PCefCookieVisitor): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	url_str: ustring;
begin
	url_str := TWACef.ToString(url);
	Result := Ord(
		TCefCookieManagerOwn(TWACef.GetObject(self)).VisitUrlCookies(
			url_str,
			includeHttpOnly <> 0,
			TCefCookieVisitorRef.UnWrap(visitor)
		)
	);
end;

// Sets a cookie given a valid URL and explicit user-provided cookie
// attributes. This function expects each attribute to be well-formed. It will
// check for disallowed characters (e.g. the ';' character is disallowed
// within the cookie value attribute) and fail without setting the cookie if
// such characters are found. If |callback| is non-NULL it will be executed
// asnychronously on the IO thread after the cookie has been set. Returns
// false (0) if an invalid URL is specified or if cookies cannot be accessed.
function cef_cookie_manager_set_cookie(self: PCefCookieManager; const url: PCefString; const cookie: PCefCookie; callback: PCefSetCookieCallback): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	url_str: ustring;
begin
	url_str := TWACef.ToString(url);
	Result := Ord(
		TCefCookieManagerOwn(TWACef.GetObject(self)).SetCookie(
			url_str,
			cookie^,
			TCefSetCookieCallbackRef.UnWrap(callback)
		)
	);
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
function cef_cookie_manager_delete_cookies(self: PCefCookieManager; const url: PCefString; const cookie_name: PCefString; callback: PCefDeleteCookiesCallback): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	url_str: ustring;
	cookie_name_str: ustring;
begin
	url_str := TWACef.ToString(url);
	cookie_name_str := TWACef.ToString(cookie_name);
	Result := Ord(
		TCefCookieManagerOwn(TWACef.GetObject(self)).DeleteCookies(
			url_str,
			cookie_name_str,
			TCefDeleteCookiesCallbackRef.UnWrap(callback)
		)
	);
end;

// Sets the directory path that will be used for storing cookie data. If
// |path| is NULL data will be stored in memory only. Otherwise, data will be
// stored at the specified |path|. To persist session cookies (cookies without
// an expiry date or validity interval) set |persist_session_cookies| to true
// (1). Session cookies are generally intended to be transient and most Web
// browsers do not persist them. If |callback| is non-NULL it will be executed
// asnychronously on the IO thread after the manager's storage has been
// initialized. Returns false (0) if cookies cannot be accessed.
function cef_cookie_manager_set_storage_path(self: PCefCookieManager; const path: PCefString; persist_session_cookies: cint; callback: PCefCompletionCallback): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	path_str: ustring;
	persist_session_cookies_proxy: Boolean;
begin
	path_str := TWACef.ToString(path);
	persist_session_cookies_proxy := persist_session_cookies <> 0;
	Result := Ord(
		TCefCookieManagerOwn(TWACef.GetObject(self)).SetStoragePath(
			path_str,
			persist_session_cookies_proxy,
			TCefCompletionCallbackRef.UnWrap(callback)
		)
	);
	persist_session_cookies := Ord(persist_session_cookies_proxy);
end;

// Flush the backing store (if any) to disk. If |callback| is non-NULL it will
// be executed asnychronously on the IO thread after the flush is complete.
// Returns false (0) if cookies cannot be accessed.
function cef_cookie_manager_flush_store(self: PCefCookieManager; callback: PCefCompletionCallback): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefCookieManagerOwn(TWACef.GetObject(self)).FlushStore(
			TCefCompletionCallbackRef.UnWrap(callback)
		)
	);
end;

{Protected section}
procedure TCefCookieManagerOwn.SetSupportedSchemes(aSchemes: TStrings; const aCallback: ICefCompletionCallback);
begin
end;

function TCefCookieManagerOwn.VisitAllCookies(const aVisitor: ICefCookieVisitor): Boolean;
begin
	Result := false;
end;

function TCefCookieManagerOwn.VisitAllCookiesProc(const aVisitor: TCefCookieVisitorProc): Boolean;
begin
  Result := false;
end;

function TCefCookieManagerOwn.VisitUrlCookies(const aUrl: ustring; aIncludeHttpOnly: Boolean; const aVisitor: ICefCookieVisitor): Boolean;
begin
	Result := false;
end;

function TCefCookieManagerOwn.VisitUrlCookiesProc(const aUrl: ustring; aIncludeHttpOnly: Boolean; const aVisitor: TCefCookieVisitorProc): Boolean;
begin
	Result := false;
end;

function TCefCookieManagerOwn.SetCookie(const aUrl: ustring; const aCookie: TCefCookie; const aCallback: ICefSetCookieCallback): Boolean;
begin
	Result := false;
end;

function TCefCookieManagerOwn.DeleteCookies(const aUrl: ustring; const aCookieName: ustring; const aCallback: ICefDeleteCookiesCallback): Boolean;
begin
	Result := false;
end;

function TCefCookieManagerOwn.SetStoragePath(const aPath: ustring; aPersistSessionCookies: Boolean; const aCallback: ICefCompletionCallback): Boolean;
begin
	Result := false;
end;

function TCefCookieManagerOwn.FlushStore(const aCallback: ICefCompletionCallback): Boolean;
begin
	Result := false;
end;

{Public section}
constructor TCefCookieManagerOwn.Create;
begin
	inherited CreateData(SizeOf(TCefCookieManager));
	with PCefCookieManager(FData)^ do
	begin
		set_supported_schemes := {$IFDEF FPC}@{$ENDIF}cef_cookie_manager_set_supported_schemes;
		visit_all_cookies := {$IFDEF FPC}@{$ENDIF}cef_cookie_manager_visit_all_cookies;
		visit_url_cookies := {$IFDEF FPC}@{$ENDIF}cef_cookie_manager_visit_url_cookies;
		set_cookie := {$IFDEF FPC}@{$ENDIF}cef_cookie_manager_set_cookie;
		delete_cookies := {$IFDEF FPC}@{$ENDIF}cef_cookie_manager_delete_cookies;
		set_storage_path := {$IFDEF FPC}@{$ENDIF}cef_cookie_manager_set_storage_path;
		flush_store := {$IFDEF FPC}@{$ENDIF}cef_cookie_manager_flush_store;
	end;
end;

//..............................................................................TCefCookieVisitorOwn
// Method that will be called once for each cookie. |count| is the 0-based
// index for the current cookie. |total| is the total number of cookies. Set
// |deleteCookie| to true (1) to delete the cookie currently being visited.
// Return false (0) to stop visiting cookies. This function may never be
// called if no cookies are found.
function cef_cookie_visitor_visit(self: PCefCookieVisitor; const cookie: PCefCookie; count: cint; total: cint; deleteCookie: pcint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	deleteCookie_proxy: Boolean;
  cookie_proxy: TWACefCookie;
begin
  cookie_proxy.name := TWACef.ToString(@cookie^.name);
  cookie_proxy.value := TWACef.ToString(@cookie^.value);
  cookie_proxy.domain := TWACef.ToString(@cookie^.domain);
  cookie_proxy.path := TWACef.ToString(@cookie^.path);
  cookie_proxy.secure := cookie^.secure;
  cookie_proxy.httponly := cookie^.httponly;
  cookie_proxy.creation := cookie^.creation;
  cookie_proxy.last_access := cookie^.last_access;
  cookie_proxy.has_expires := cookie^.has_expires;
  cookie_proxy.expires := cookie^.expires;

	deleteCookie_proxy := deleteCookie^  <> 0;
	Result := Ord(
		TCefCookieVisitorOwn(TWACef.GetObject(self)).Visit(
			cookie_proxy,
			count,
			total,
			deleteCookie_proxy
		)
	);
	deleteCookie^  := Ord(deleteCookie_proxy);
end;

{Protected section}
function TCefCookieVisitorOwn.Visit(const aCookie: TWACefCookie; aCount: cint; aTotal: cint; out aDeleteCookie: Boolean): Boolean;
begin
	Result := false;
end;

{Public section}
constructor TCefCookieVisitorOwn.Create;
begin
	inherited CreateData(SizeOf(TCefCookieVisitor));
	with PCefCookieVisitor(FData)^ do
	begin
		visit := {$IFDEF FPC}@{$ENDIF}cef_cookie_visitor_visit;
	end;
end;
//..............................................................................TCefSetCookieCallbackOwn
// Method that will be called upon completion. |success| will be true (1) if
// the cookie was set successfully.
procedure cef_set_cookie_callback_on_complete(self: PCefSetCookieCallback; success: cint); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefSetCookieCallbackOwn(TWACef.GetObject(self)).OnComplete(
		success <> 0
	);
end;

{Protected section}
procedure TCefSetCookieCallbackOwn.OnComplete(aSuccess: Boolean);
begin
end;

{Public section}
constructor TCefSetCookieCallbackOwn.Create;
begin
	inherited CreateData(SizeOf(TCefSetCookieCallback));
	with PCefSetCookieCallback(FData)^ do
	begin
		on_complete := {$IFDEF FPC}@{$ENDIF}cef_set_cookie_callback_on_complete;
	end;
end;
//..............................................................................TCefDeleteCookiesCallbackOwn
// Method that will be called upon completion. |num_deleted| will be the
// number of cookies that were deleted or -1 if unknown.
procedure cef_delete_cookies_callback_on_complete(self: PCefDeleteCookiesCallback; num_deleted: cint); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefDeleteCookiesCallbackOwn(TWACef.GetObject(self)).OnComplete(
		num_deleted
	);
end;

{Protected section}
procedure TCefDeleteCookiesCallbackOwn.OnComplete(aNumDeleted: cint);
begin
end;

{Public section}
constructor TCefDeleteCookiesCallbackOwn.Create;
begin
	inherited CreateData(SizeOf(TCefDeleteCookiesCallback));
	with PCefDeleteCookiesCallback(FData)^ do
	begin
		on_complete := {$IFDEF FPC}@{$ENDIF}cef_delete_cookies_callback_on_complete;
	end;
end;

//..............................................................................TCefFileDialogCallbackOwn
// Continue the file selection. |selected_accept_filter| should be the 0-based
// index of the value selected from the accept filters array passed to
// cef_dialog_handler_t::OnFileDialog. |file_paths| should be a single value
// or a list of values depending on the dialog mode. An NULL |file_paths|
// value is treated the same as calling cancel().
procedure cef_file_dialog_callback_cont(self: PCefFileDialogCallback; selected_accept_filter: cint; file_paths: TCefStringList); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefFileDialogCallbackOwn(TWACef.GetObject(self)).Cont(
		selected_accept_filter,
		file_paths
	);
end;

// Cancel the file selection.
procedure cef_file_dialog_callback_cancel(self: PCefFileDialogCallback); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefFileDialogCallbackOwn(TWACef.GetObject(self)).Cancel;
end;

{Protected section}
procedure TCefFileDialogCallbackOwn.Cont(aSelectedAcceptFilter: cint; aFilePaths: TStrings);
begin
end;

procedure TCefFileDialogCallbackOwn.Cancel;
begin
end;

{Public section}
constructor TCefFileDialogCallbackOwn.Create;
begin
	inherited CreateData(SizeOf(TCefFileDialogCallback));
	with PCefFileDialogCallback(FData)^ do
	begin
		cont := {$IFDEF FPC}@{$ENDIF}cef_file_dialog_callback_cont;
		cancel := {$IFDEF FPC}@{$ENDIF}cef_file_dialog_callback_cancel;
	end;
end;
//..............................................................................TCefDialogHandlerOwn
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
function cef_dialog_handler_on_file_dialog(self: PCefDialogHandler; browser: PCefBrowser; mode: TCefFileDialogMode; const title: PCefString; const default_file_path: PCefString; accept_filters: TCefStringList; selected_accept_filter: cint; callback: PCefFileDialogCallback): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	title_str: ustring;
	default_file_path_str: ustring;
begin
	title_str := TWACef.ToString(title);
	default_file_path_str := TWACef.ToString(default_file_path);
	Result := Ord(
		TCefDialogHandlerOwn(TWACef.GetObject(self)).OnFileDialog(
			TCefBrowserRef.UnWrap(browser),
			mode,
			title_str,
			default_file_path_str,
			accept_filters,
			selected_accept_filter,
			TCefFileDialogCallbackRef.UnWrap(callback)
		)
	);
end;

{Protected section}
function TCefDialogHandlerOwn.OnFileDialog(const aBrowser: ICefBrowser; aMode: TCefFileDialogMode; const aTitle: ustring; const aDefaultFilePath: ustring; aAcceptFilters: TStrings; aSelectedAcceptFilter: cint; const aCallback: ICefFileDialogCallback): Boolean;
begin
	Result := false;
end;

{Public section}
constructor TCefDialogHandlerOwn.Create;
begin
	inherited CreateData(SizeOf(TCefDialogHandler));
	with PCefDialogHandler(FData)^ do
	begin
		on_file_dialog := {$IFDEF FPC}@{$ENDIF}cef_dialog_handler_on_file_dialog;
	end;
end;

//..............................................................................TCefDisplayHandlerOwn
// Called when a frame's address has changed.
procedure cef_display_handler_on_address_change(self: PCefDisplayHandler; browser: PCefBrowser; frame: PCefFrame; const url: PCefString); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	url_str: ustring;
begin
	url_str := TWACef.ToString(url);
	TCefDisplayHandlerOwn(TWACef.GetObject(self)).OnAddressChange(
		TCefBrowserRef.UnWrap(browser),
		TCefFrameRef.UnWrap(frame),
		url_str
	);
end;

// Called when the page title changes.
procedure cef_display_handler_on_title_change(self: PCefDisplayHandler; browser: PCefBrowser; const title: PCefString); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	title_str: ustring;
begin
	title_str := TWACef.ToString(title);
	TCefDisplayHandlerOwn(TWACef.GetObject(self)).OnTitleChange(
		TCefBrowserRef.UnWrap(browser),
		title_str
	);
end;

// Called when the page icon changes.
procedure cef_display_handler_on_favicon_urlchange(self: PCefDisplayHandler; browser: PCefBrowser; icon_urls: TCefStringList); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefDisplayHandlerOwn(TWACef.GetObject(self)).OnFaviconUrlchange(
		TCefBrowserRef.UnWrap(browser),
		icon_urls
	);
end;

// Called when the browser is about to display a tooltip. |text| contains the
// text that will be displayed in the tooltip. To handle the display of the
// tooltip yourself return true (1). Otherwise, you can optionally modify
// |text| and then return false (0) to allow the browser to display the
// tooltip. When window rendering is disabled the application is responsible
// for drawing tooltips and the return value is ignored.
function cef_display_handler_on_tooltip(self: PCefDisplayHandler; browser: PCefBrowser; text: PCefString): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	text_str: ustring;
begin
	text_str := TWACef.ToString(text);
	Result := Ord(
		TCefDisplayHandlerOwn(TWACef.GetObject(self)).OnTooltip(
			TCefBrowserRef.UnWrap(browser),
			text_str
		)
	);
end;

// Called when the browser receives a status message. |value| contains the
// text that will be displayed in the status message.
procedure cef_display_handler_on_status_message(self: PCefDisplayHandler; browser: PCefBrowser; const value: PCefString); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	value_str: ustring;
begin
	value_str := TWACef.ToString(value);
	TCefDisplayHandlerOwn(TWACef.GetObject(self)).OnStatusMessage(
		TCefBrowserRef.UnWrap(browser),
		value_str
	);
end;

// Called to display a console message. Return true (1) to stop the message
// from being output to the console.
function cef_display_handler_on_console_message(self: PCefDisplayHandler; browser: PCefBrowser; const message: PCefString; const source: PCefString; line: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	message_str: ustring;
	source_str: ustring;
begin
	message_str := TWACef.ToString(message);
	source_str := TWACef.ToString(source);
	Result := Ord(
		TCefDisplayHandlerOwn(TWACef.GetObject(self)).OnConsoleMessage(
			TCefBrowserRef.UnWrap(browser),
			message_str,
			source_str,
			line
		)
	);
end;

{Protected section}
procedure TCefDisplayHandlerOwn.OnAddressChange(const aBrowser: ICefBrowser; const aFrame: ICefFrame; const aUrl: ustring);
begin
end;

procedure TCefDisplayHandlerOwn.OnTitleChange(const aBrowser: ICefBrowser; const aTitle: ustring);
begin
end;

procedure TCefDisplayHandlerOwn.OnFaviconUrlchange(const aBrowser: ICefBrowser; aIconUrls: TStrings);
begin
end;

function TCefDisplayHandlerOwn.OnTooltip(const aBrowser: ICefBrowser; var aText: ustring): Boolean;
begin
	Result := false;
end;

procedure TCefDisplayHandlerOwn.OnStatusMessage(const aBrowser: ICefBrowser; const aValue: ustring);
begin
end;

function TCefDisplayHandlerOwn.OnConsoleMessage(const aBrowser: ICefBrowser; const aMessage: ustring; const aSource: ustring; aLine: cint): Boolean;
begin
	Result := false;
end;

{Public section}
constructor TCefDisplayHandlerOwn.Create;
begin
	inherited CreateData(SizeOf(TCefDisplayHandler));
	with PCefDisplayHandler(FData)^ do
	begin
		on_address_change := {$IFDEF FPC}@{$ENDIF}cef_display_handler_on_address_change;
		on_title_change := {$IFDEF FPC}@{$ENDIF}cef_display_handler_on_title_change;
		on_favicon_urlchange := {$IFDEF FPC}@{$ENDIF}cef_display_handler_on_favicon_urlchange;
		on_tooltip := {$IFDEF FPC}@{$ENDIF}cef_display_handler_on_tooltip;
		on_status_message := {$IFDEF FPC}@{$ENDIF}cef_display_handler_on_status_message;
		on_console_message := {$IFDEF FPC}@{$ENDIF}cef_display_handler_on_console_message;
	end;
end;

//..............................................................................TCefDomvisitorOwn
// Method executed for visiting the DOM. The document object passed to this
// function represents a snapshot of the DOM at the time this function is
// executed. DOM objects are only valid for the scope of this function. Do not
// keep references to or attempt to access any DOM objects outside the scope
// of this function.
procedure cef_domvisitor_visit(self: PCefDomvisitor; document: PCefDomdocument); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefDomvisitorOwn(TWACef.GetObject(self)).Visit(
		TCefDomdocumentRef.UnWrap(document)
	);
end;

{Protected section}
procedure TCefDomvisitorOwn.Visit(const aDocument: ICefDomdocument);
begin
end;

{Public section}
constructor TCefDomvisitorOwn.Create;
begin
	inherited CreateData(SizeOf(TCefDomvisitor));
	with PCefDomvisitor(FData)^ do
	begin
		visit := {$IFDEF FPC}@{$ENDIF}cef_domvisitor_visit;
	end;
end;
//..............................................................................TCefDomdocumentOwn
// Returns the document type.
function cef_domdocument_get_type(self: PCefDomdocument): TCefDomDocumentType; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := (
		TCefDomdocumentOwn(TWACef.GetObject(self)).GetType
	);
end;

// Returns the root document node.
function cef_domdocument_get_document(self: PCefDomdocument): PCefDomnode; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.GetData(
		TCefDomdocumentOwn(TWACef.GetObject(self)).GetDocument
	);
end;

// Returns the BODY node of an HTML document.
function cef_domdocument_get_body(self: PCefDomdocument): PCefDomnode; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.GetData(
		TCefDomdocumentOwn(TWACef.GetObject(self)).GetBody
	);
end;

// Returns the HEAD node of an HTML document.
function cef_domdocument_get_head(self: PCefDomdocument): PCefDomnode; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.GetData(
		TCefDomdocumentOwn(TWACef.GetObject(self)).GetHead
	);
end;

// Returns the title of an HTML document.
// The resulting string must be freed by calling cef_string_userfree_free().
function cef_domdocument_get_title(self: PCefDomdocument): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.UserFreeString(
		TCefDomdocumentOwn(TWACef.GetObject(self)).GetTitle
	);
end;

// Returns the document element with the specified ID value.
function cef_domdocument_get_element_by_id(self: PCefDomdocument; const id: PCefString): PCefDomnode; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	id_str: ustring;
begin
	id_str := TWACef.ToString(id);
	Result := TWACef.GetData(
		TCefDomdocumentOwn(TWACef.GetObject(self)).GetElementById(
			id_str
		)
	);
end;

// Returns the node that currently has keyboard focus.
function cef_domdocument_get_focused_node(self: PCefDomdocument): PCefDomnode; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.GetData(
		TCefDomdocumentOwn(TWACef.GetObject(self)).GetFocusedNode
	);
end;

// Returns true (1) if a portion of the document is selected.
function cef_domdocument_has_selection(self: PCefDomdocument): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefDomdocumentOwn(TWACef.GetObject(self)).HasSelection
	);
end;

// Returns the selection offset within the start node.
function cef_domdocument_get_selection_start_offset(self: PCefDomdocument): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := (
		TCefDomdocumentOwn(TWACef.GetObject(self)).GetSelectionStartOffset
	);
end;

// Returns the selection offset within the end node.
function cef_domdocument_get_selection_end_offset(self: PCefDomdocument): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := (
		TCefDomdocumentOwn(TWACef.GetObject(self)).GetSelectionEndOffset
	);
end;

// Returns the contents of this selection as markup.
// The resulting string must be freed by calling cef_string_userfree_free().
function cef_domdocument_get_selection_as_markup(self: PCefDomdocument): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.UserFreeString(
		TCefDomdocumentOwn(TWACef.GetObject(self)).GetSelectionAsMarkup
	);
end;

// Returns the contents of this selection as text.
// The resulting string must be freed by calling cef_string_userfree_free().
function cef_domdocument_get_selection_as_text(self: PCefDomdocument): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.UserFreeString(
		TCefDomdocumentOwn(TWACef.GetObject(self)).GetSelectionAsText
	);
end;

// Returns the base URL for the document.
// The resulting string must be freed by calling cef_string_userfree_free().
function cef_domdocument_get_base_url(self: PCefDomdocument): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.UserFreeString(
		TCefDomdocumentOwn(TWACef.GetObject(self)).GetBaseUrl
	);
end;

// Returns a complete URL based on the document base URL and the specified
// partial URL.
// The resulting string must be freed by calling cef_string_userfree_free().
function cef_domdocument_get_complete_url(self: PCefDomdocument; const partialURL: PCefString): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	partialURL_str: ustring;
begin
	partialURL_str := TWACef.ToString(partialURL);
	Result := TWACef.UserFreeString(
		TCefDomdocumentOwn(TWACef.GetObject(self)).GetCompleteUrl(
			partialURL_str
		)
	);
end;

{Protected section}
function TCefDomdocumentOwn.GetType: TCefDomDocumentType;
begin
end;

function TCefDomdocumentOwn.GetDocument: ICefDomnode;
begin
	Result := nil;
end;

function TCefDomdocumentOwn.GetBody: ICefDomnode;
begin
	Result := nil;
end;

function TCefDomdocumentOwn.GetHead: ICefDomnode;
begin
	Result := nil;
end;

function TCefDomdocumentOwn.GetTitle: ustring;
begin
end;

function TCefDomdocumentOwn.GetElementById(const aId: ustring): ICefDomnode;
begin
	Result := nil;
end;

function TCefDomdocumentOwn.GetFocusedNode: ICefDomnode;
begin
	Result := nil;
end;

function TCefDomdocumentOwn.HasSelection: Boolean;
begin
	Result := false;
end;

function TCefDomdocumentOwn.GetSelectionStartOffset: cint;
begin
	Result := 0;
end;

function TCefDomdocumentOwn.GetSelectionEndOffset: cint;
begin
	Result := 0;
end;

function TCefDomdocumentOwn.GetSelectionAsMarkup: ustring;
begin
end;

function TCefDomdocumentOwn.GetSelectionAsText: ustring;
begin
end;

function TCefDomdocumentOwn.GetBaseUrl: ustring;
begin
end;

function TCefDomdocumentOwn.GetCompleteUrl(const aPartialURL: ustring): ustring;
begin
end;

{Public section}
constructor TCefDomdocumentOwn.Create;
begin
	inherited CreateData(SizeOf(TCefDomdocument));
	with PCefDomdocument(FData)^ do
	begin
		get_type := {$IFDEF FPC}@{$ENDIF}cef_domdocument_get_type;
		get_document := {$IFDEF FPC}@{$ENDIF}cef_domdocument_get_document;
		get_body := {$IFDEF FPC}@{$ENDIF}cef_domdocument_get_body;
		get_head := {$IFDEF FPC}@{$ENDIF}cef_domdocument_get_head;
		get_title := {$IFDEF FPC}@{$ENDIF}cef_domdocument_get_title;
		get_element_by_id := {$IFDEF FPC}@{$ENDIF}cef_domdocument_get_element_by_id;
		get_focused_node := {$IFDEF FPC}@{$ENDIF}cef_domdocument_get_focused_node;
		has_selection := {$IFDEF FPC}@{$ENDIF}cef_domdocument_has_selection;
		get_selection_start_offset := {$IFDEF FPC}@{$ENDIF}cef_domdocument_get_selection_start_offset;
		get_selection_end_offset := {$IFDEF FPC}@{$ENDIF}cef_domdocument_get_selection_end_offset;
		get_selection_as_markup := {$IFDEF FPC}@{$ENDIF}cef_domdocument_get_selection_as_markup;
		get_selection_as_text := {$IFDEF FPC}@{$ENDIF}cef_domdocument_get_selection_as_text;
		get_base_url := {$IFDEF FPC}@{$ENDIF}cef_domdocument_get_base_url;
		get_complete_url := {$IFDEF FPC}@{$ENDIF}cef_domdocument_get_complete_url;
	end;
end;
//..............................................................................TCefDomnodeOwn
// Returns the type for this node.
function cef_domnode_get_type(self: PCefDomnode): TCefDomNodeType; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := (
		TCefDomnodeOwn(TWACef.GetObject(self)).GetType
	);
end;

// Returns true (1) if this is a text node.
function cef_domnode_is_text(self: PCefDomnode): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefDomnodeOwn(TWACef.GetObject(self)).IsText
	);
end;

// Returns true (1) if this is an element node.
function cef_domnode_is_element(self: PCefDomnode): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefDomnodeOwn(TWACef.GetObject(self)).IsElement
	);
end;

// Returns true (1) if this is an editable node.
function cef_domnode_is_editable(self: PCefDomnode): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefDomnodeOwn(TWACef.GetObject(self)).IsEditable
	);
end;

// Returns true (1) if this is a form control element node.
function cef_domnode_is_form_control_element(self: PCefDomnode): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefDomnodeOwn(TWACef.GetObject(self)).IsFormControlElement
	);
end;

// Returns the type of this form control element node.
// The resulting string must be freed by calling cef_string_userfree_free().
function cef_domnode_get_form_control_element_type(self: PCefDomnode): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.UserFreeString(
		TCefDomnodeOwn(TWACef.GetObject(self)).GetFormControlElementType
	);
end;

// Returns true (1) if this object is pointing to the same handle as |that|
// object.
function cef_domnode_is_same(self: PCefDomnode; that: PCefDomnode): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefDomnodeOwn(TWACef.GetObject(self)).IsSame(
			TCefDomnodeRef.UnWrap(that)
		)
	);
end;

// Returns the name of this node.
// The resulting string must be freed by calling cef_string_userfree_free().
function cef_domnode_get_name(self: PCefDomnode): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.UserFreeString(
		TCefDomnodeOwn(TWACef.GetObject(self)).GetName
	);
end;

// Returns the value of this node.
// The resulting string must be freed by calling cef_string_userfree_free().
function cef_domnode_get_value(self: PCefDomnode): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.UserFreeString(
		TCefDomnodeOwn(TWACef.GetObject(self)).GetValue
	);
end;

// Set the value of this node. Returns true (1) on success.
function cef_domnode_set_value(self: PCefDomnode; const value: PCefString): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	value_str: ustring;
begin
	value_str := TWACef.ToString(value);
	Result := Ord(
		TCefDomnodeOwn(TWACef.GetObject(self)).SetValue(
			value_str
		)
	);
end;

// Returns the contents of this node as markup.
// The resulting string must be freed by calling cef_string_userfree_free().
function cef_domnode_get_as_markup(self: PCefDomnode): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.UserFreeString(
		TCefDomnodeOwn(TWACef.GetObject(self)).GetAsMarkup
	);
end;

// Returns the document associated with this node.
function cef_domnode_get_document(self: PCefDomnode): PCefDomdocument; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.GetData(
		TCefDomnodeOwn(TWACef.GetObject(self)).GetDocument
	);
end;

// Returns the parent node.
function cef_domnode_get_parent(self: PCefDomnode): PCefDomnode; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.GetData(
		TCefDomnodeOwn(TWACef.GetObject(self)).GetParent
	);
end;

// Returns the previous sibling node.
function cef_domnode_get_previous_sibling(self: PCefDomnode): PCefDomnode; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.GetData(
		TCefDomnodeOwn(TWACef.GetObject(self)).GetPreviousSibling
	);
end;

// Returns the next sibling node.
function cef_domnode_get_next_sibling(self: PCefDomnode): PCefDomnode; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.GetData(
		TCefDomnodeOwn(TWACef.GetObject(self)).GetNextSibling
	);
end;

// Returns true (1) if this node has child nodes.
function cef_domnode_has_children(self: PCefDomnode): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefDomnodeOwn(TWACef.GetObject(self)).HasChildren
	);
end;

// Return the first child node.
function cef_domnode_get_first_child(self: PCefDomnode): PCefDomnode; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.GetData(
		TCefDomnodeOwn(TWACef.GetObject(self)).GetFirstChild
	);
end;

// Returns the last child node.
function cef_domnode_get_last_child(self: PCefDomnode): PCefDomnode; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.GetData(
		TCefDomnodeOwn(TWACef.GetObject(self)).GetLastChild
	);
end;

// Returns the tag name of this element.
// The resulting string must be freed by calling cef_string_userfree_free().
function cef_domnode_get_element_tag_name(self: PCefDomnode): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.UserFreeString(
		TCefDomnodeOwn(TWACef.GetObject(self)).GetElementTagName
	);
end;

// Returns true (1) if this element has attributes.
function cef_domnode_has_element_attributes(self: PCefDomnode): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefDomnodeOwn(TWACef.GetObject(self)).HasElementAttributes
	);
end;

// Returns true (1) if this element has an attribute named |attrName|.
function cef_domnode_has_element_attribute(self: PCefDomnode; const attrName: PCefString): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	attrName_str: ustring;
begin
	attrName_str := TWACef.ToString(attrName);
	Result := Ord(
		TCefDomnodeOwn(TWACef.GetObject(self)).HasElementAttribute(
			attrName_str
		)
	);
end;

// Returns the element attribute named |attrName|.
// The resulting string must be freed by calling cef_string_userfree_free().
function cef_domnode_get_element_attribute(self: PCefDomnode; const attrName: PCefString): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	attrName_str: ustring;
begin
	attrName_str := TWACef.ToString(attrName);
	Result := TWACef.UserFreeString(
		TCefDomnodeOwn(TWACef.GetObject(self)).GetElementAttribute(
			attrName_str
		)
	);
end;

// Returns a map of all element attributes.
procedure cef_domnode_get_element_attributes(self: PCefDomnode; attrMap: TCefStringMap); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
  attrMap_proxy: ICefStringMap;
  attrs: TStringList;
  i: Integer;
begin
  attrMap_proxy := TCefStringMapOwn.Create(attrMap);
  attrs := TStringList.Create;
  try
    for i := 0 to attrMap_proxy.Size - 1 do
      attrs.Add(attrMap_proxy.Key[i] + '=' + attrMap_proxy.Value[i]);
    TCefDomnodeOwn(TWACef.GetObject(self)).GetElementAttributes(
      attrs
    );
  finally
    attrs.Free;
  end;
end;

// Set the value for the element attribute named |attrName|. Returns true (1)
// on success.
function cef_domnode_set_element_attribute(self: PCefDomnode; const attrName: PCefString; const value: PCefString): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	attrName_str: ustring;
	value_str: ustring;
begin
	attrName_str := TWACef.ToString(attrName);
	value_str := TWACef.ToString(value);
	Result := Ord(
		TCefDomnodeOwn(TWACef.GetObject(self)).SetElementAttribute(
			attrName_str,
			value_str
		)
	);
end;

// Returns the inner text of the element.
// The resulting string must be freed by calling cef_string_userfree_free().
function cef_domnode_get_element_inner_text(self: PCefDomnode): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.UserFreeString(
		TCefDomnodeOwn(TWACef.GetObject(self)).GetElementInnerText
	);
end;

{Protected section}
function TCefDomnodeOwn.GetType: TCefDomNodeType;
begin
end;

function TCefDomnodeOwn.IsText: Boolean;
begin
	Result := false;
end;

function TCefDomnodeOwn.IsElement: Boolean;
begin
	Result := false;
end;

function TCefDomnodeOwn.IsEditable: Boolean;
begin
	Result := false;
end;

function TCefDomnodeOwn.IsFormControlElement: Boolean;
begin
	Result := false;
end;

function TCefDomnodeOwn.GetFormControlElementType: ustring;
begin
end;

function TCefDomnodeOwn.IsSame(const aThat: ICefDomnode): Boolean;
begin
	Result := false;
end;

function TCefDomnodeOwn.GetName: ustring;
begin
end;

function TCefDomnodeOwn.GetValue: ustring;
begin
end;

function TCefDomnodeOwn.SetValue(const aValue: ustring): Boolean;
begin
	Result := false;
end;

function TCefDomnodeOwn.GetAsMarkup: ustring;
begin
end;

function TCefDomnodeOwn.GetDocument: ICefDomdocument;
begin
	Result := nil;
end;

function TCefDomnodeOwn.GetParent: ICefDomnode;
begin
	Result := nil;
end;

function TCefDomnodeOwn.GetPreviousSibling: ICefDomnode;
begin
	Result := nil;
end;

function TCefDomnodeOwn.GetNextSibling: ICefDomnode;
begin
	Result := nil;
end;

function TCefDomnodeOwn.HasChildren: Boolean;
begin
	Result := false;
end;

function TCefDomnodeOwn.GetFirstChild: ICefDomnode;
begin
	Result := nil;
end;

function TCefDomnodeOwn.GetLastChild: ICefDomnode;
begin
	Result := nil;
end;

function TCefDomnodeOwn.GetElementTagName: ustring;
begin
end;

function TCefDomnodeOwn.HasElementAttributes: Boolean;
begin
	Result := false;
end;

function TCefDomnodeOwn.HasElementAttribute(const aAttrName: ustring): Boolean;
begin
	Result := false;
end;

function TCefDomnodeOwn.GetElementAttribute(const aAttrName: ustring): ustring;
begin
end;

procedure TCefDomnodeOwn.GetElementAttributes(aAttrMap: TStrings);
begin
end;

function TCefDomnodeOwn.SetElementAttribute(const aAttrName: ustring; const aValue: ustring): Boolean;
begin
	Result := false;
end;

function TCefDomnodeOwn.GetElementInnerText: ustring;
begin
end;

{Public section}
constructor TCefDomnodeOwn.Create;
begin
	inherited CreateData(SizeOf(TCefDomnode));
	with PCefDomnode(FData)^ do
	begin
		get_type := {$IFDEF FPC}@{$ENDIF}cef_domnode_get_type;
		is_text := {$IFDEF FPC}@{$ENDIF}cef_domnode_is_text;
		is_element := {$IFDEF FPC}@{$ENDIF}cef_domnode_is_element;
		is_editable := {$IFDEF FPC}@{$ENDIF}cef_domnode_is_editable;
		is_form_control_element := {$IFDEF FPC}@{$ENDIF}cef_domnode_is_form_control_element;
		get_form_control_element_type := {$IFDEF FPC}@{$ENDIF}cef_domnode_get_form_control_element_type;
		is_same := {$IFDEF FPC}@{$ENDIF}cef_domnode_is_same;
		get_name := {$IFDEF FPC}@{$ENDIF}cef_domnode_get_name;
		get_value := {$IFDEF FPC}@{$ENDIF}cef_domnode_get_value;
		set_value := {$IFDEF FPC}@{$ENDIF}cef_domnode_set_value;
		get_as_markup := {$IFDEF FPC}@{$ENDIF}cef_domnode_get_as_markup;
		get_document := {$IFDEF FPC}@{$ENDIF}cef_domnode_get_document;
		get_parent := {$IFDEF FPC}@{$ENDIF}cef_domnode_get_parent;
		get_previous_sibling := {$IFDEF FPC}@{$ENDIF}cef_domnode_get_previous_sibling;
		get_next_sibling := {$IFDEF FPC}@{$ENDIF}cef_domnode_get_next_sibling;
		has_children := {$IFDEF FPC}@{$ENDIF}cef_domnode_has_children;
		get_first_child := {$IFDEF FPC}@{$ENDIF}cef_domnode_get_first_child;
		get_last_child := {$IFDEF FPC}@{$ENDIF}cef_domnode_get_last_child;
		get_element_tag_name := {$IFDEF FPC}@{$ENDIF}cef_domnode_get_element_tag_name;
		has_element_attributes := {$IFDEF FPC}@{$ENDIF}cef_domnode_has_element_attributes;
		has_element_attribute := {$IFDEF FPC}@{$ENDIF}cef_domnode_has_element_attribute;
		get_element_attribute := {$IFDEF FPC}@{$ENDIF}cef_domnode_get_element_attribute;
		get_element_attributes := {$IFDEF FPC}@{$ENDIF}cef_domnode_get_element_attributes;
		set_element_attribute := {$IFDEF FPC}@{$ENDIF}cef_domnode_set_element_attribute;
		get_element_inner_text := {$IFDEF FPC}@{$ENDIF}cef_domnode_get_element_inner_text;
	end;
end;

//..............................................................................TCefBeforeDownloadCallbackOwn
// Call to continue the download. Set |download_path| to the full file path
// for the download including the file name or leave blank to use the
// suggested name and the default temp directory. Set |show_dialog| to true
// (1) if you do wish to show the default "Save As" dialog.
procedure cef_before_download_callback_cont(self: PCefBeforeDownloadCallback; const download_path: PCefString; show_dialog: cint); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	download_path_str: ustring;
	show_dialog_proxy: Boolean;
begin
	download_path_str := TWACef.ToString(download_path);
	show_dialog_proxy := show_dialog <> 0;
	TCefBeforeDownloadCallbackOwn(TWACef.GetObject(self)).Cont(
		download_path_str,
		show_dialog_proxy
	);
end;

{Protected section}
procedure TCefBeforeDownloadCallbackOwn.Cont(var aDownloadPath: ustring; aShowDialog: Boolean);
begin
end;

{Public section}
constructor TCefBeforeDownloadCallbackOwn.Create;
begin
	inherited CreateData(SizeOf(TCefBeforeDownloadCallback));
	with PCefBeforeDownloadCallback(FData)^ do
	begin
		cont := {$IFDEF FPC}@{$ENDIF}cef_before_download_callback_cont;
	end;
end;
//..............................................................................TCefDownloadItemCallbackOwn
// Call to cancel the download.
procedure cef_download_item_callback_cancel(self: PCefDownloadItemCallback); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefDownloadItemCallbackOwn(TWACef.GetObject(self)).Cancel;
end;

// Call to pause the download.
procedure cef_download_item_callback_pause(self: PCefDownloadItemCallback); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefDownloadItemCallbackOwn(TWACef.GetObject(self)).Pause;
end;

// Call to resume the download.
procedure cef_download_item_callback_resume(self: PCefDownloadItemCallback); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefDownloadItemCallbackOwn(TWACef.GetObject(self)).Resume;
end;

{Protected section}
procedure TCefDownloadItemCallbackOwn.Cancel;
begin
end;

procedure TCefDownloadItemCallbackOwn.Pause;
begin
end;

procedure TCefDownloadItemCallbackOwn.Resume;
begin
end;

{Public section}
constructor TCefDownloadItemCallbackOwn.Create;
begin
	inherited CreateData(SizeOf(TCefDownloadItemCallback));
	with PCefDownloadItemCallback(FData)^ do
	begin
		cancel := {$IFDEF FPC}@{$ENDIF}cef_download_item_callback_cancel;
		pause := {$IFDEF FPC}@{$ENDIF}cef_download_item_callback_pause;
		resume := {$IFDEF FPC}@{$ENDIF}cef_download_item_callback_resume;
	end;
end;
//..............................................................................TCefDownloadHandlerOwn
// Called before a download begins. |suggested_name| is the suggested name for
// the download file. By default the download will be canceled. Execute
// |callback| either asynchronously or in this function to continue the
// download if desired. Do not keep a reference to |download_item| outside of
// this function.
procedure cef_download_handler_on_before_download(self: PCefDownloadHandler; browser: PCefBrowser; download_item: PCefDownloadItem; const suggested_name: PCefString; callback: PCefBeforeDownloadCallback); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	suggested_name_str: ustring;
begin
	suggested_name_str := TWACef.ToString(suggested_name);
	TCefDownloadHandlerOwn(TWACef.GetObject(self)).OnBeforeDownload(
		TCefBrowserRef.UnWrap(browser),
		TCefDownloadItemRef.UnWrap(download_item),
		suggested_name_str,
		TCefBeforeDownloadCallbackRef.UnWrap(callback)
	);
end;

// Called when a download's status or progress information has been updated.
// This may be called multiple times before and after on_before_download().
// Execute |callback| either asynchronously or in this function to cancel the
// download if desired. Do not keep a reference to |download_item| outside of
// this function.
procedure cef_download_handler_on_download_updated(self: PCefDownloadHandler; browser: PCefBrowser; download_item: PCefDownloadItem; callback: PCefDownloadItemCallback); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefDownloadHandlerOwn(TWACef.GetObject(self)).OnDownloadUpdated(
		TCefBrowserRef.UnWrap(browser),
		TCefDownloadItemRef.UnWrap(download_item),
		TCefDownloadItemCallbackRef.UnWrap(callback)
	);
end;

{Protected section}
procedure TCefDownloadHandlerOwn.OnBeforeDownload(const aBrowser: ICefBrowser; const aDownloadItem: ICefDownloadItem; const aSuggestedName: ustring; const aCallback: ICefBeforeDownloadCallback);
begin
end;

procedure TCefDownloadHandlerOwn.OnDownloadUpdated(const aBrowser: ICefBrowser; const aDownloadItem: ICefDownloadItem; const aCallback: ICefDownloadItemCallback);
begin
end;

{Public section}
constructor TCefDownloadHandlerOwn.Create;
begin
	inherited CreateData(SizeOf(TCefDownloadHandler));
	with PCefDownloadHandler(FData)^ do
	begin
		on_before_download := {$IFDEF FPC}@{$ENDIF}cef_download_handler_on_before_download;
		on_download_updated := {$IFDEF FPC}@{$ENDIF}cef_download_handler_on_download_updated;
	end;
end;

//..............................................................................TCefDownloadItemOwn
// Returns true (1) if this object is valid. Do not call any other functions
// if this function returns false (0).
function cef_download_item_is_valid(self: PCefDownloadItem): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefDownloadItemOwn(TWACef.GetObject(self)).IsValid
	);
end;

// Returns true (1) if the download is in progress.
function cef_download_item_is_in_progress(self: PCefDownloadItem): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefDownloadItemOwn(TWACef.GetObject(self)).IsInProgress
	);
end;

// Returns true (1) if the download is complete.
function cef_download_item_is_complete(self: PCefDownloadItem): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefDownloadItemOwn(TWACef.GetObject(self)).IsComplete
	);
end;

// Returns true (1) if the download has been canceled or interrupted.
function cef_download_item_is_canceled(self: PCefDownloadItem): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefDownloadItemOwn(TWACef.GetObject(self)).IsCanceled
	);
end;

// Returns a simple speed estimate in bytes/s.
function cef_download_item_get_current_speed(self: PCefDownloadItem): cint64; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := (
		TCefDownloadItemOwn(TWACef.GetObject(self)).GetCurrentSpeed
	);
end;

// Returns the rough percent complete or -1 if the receive total size is
// unknown.
function cef_download_item_get_percent_complete(self: PCefDownloadItem): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := (
		TCefDownloadItemOwn(TWACef.GetObject(self)).GetPercentComplete
	);
end;

// Returns the total number of bytes.
function cef_download_item_get_total_bytes(self: PCefDownloadItem): cint64; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := (
		TCefDownloadItemOwn(TWACef.GetObject(self)).GetTotalBytes
	);
end;

// Returns the number of received bytes.
function cef_download_item_get_received_bytes(self: PCefDownloadItem): cint64; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := (
		TCefDownloadItemOwn(TWACef.GetObject(self)).GetReceivedBytes
	);
end;

// Returns the time that the download started.
function cef_download_item_get_start_time(self: PCefDownloadItem): TCefTime; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := (
		TCefDownloadItemOwn(TWACef.GetObject(self)).GetStartTime
	);
end;

// Returns the time that the download ended.
function cef_download_item_get_end_time(self: PCefDownloadItem): TCefTime; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := (
		TCefDownloadItemOwn(TWACef.GetObject(self)).GetEndTime
	);
end;

// Returns the full path to the downloaded or downloading file.
// The resulting string must be freed by calling cef_string_userfree_free().
function cef_download_item_get_full_path(self: PCefDownloadItem): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.UserFreeString(
		TCefDownloadItemOwn(TWACef.GetObject(self)).GetFullPath
	);
end;

// Returns the unique identifier for this download.
function cef_download_item_get_id(self: PCefDownloadItem): cuint32; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := (
		TCefDownloadItemOwn(TWACef.GetObject(self)).GetId
	);
end;

// Returns the URL.
// The resulting string must be freed by calling cef_string_userfree_free().
function cef_download_item_get_url(self: PCefDownloadItem): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.UserFreeString(
		TCefDownloadItemOwn(TWACef.GetObject(self)).GetUrl
	);
end;

// Returns the original URL before any redirections.
// The resulting string must be freed by calling cef_string_userfree_free().
function cef_download_item_get_original_url(self: PCefDownloadItem): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.UserFreeString(
		TCefDownloadItemOwn(TWACef.GetObject(self)).GetOriginalUrl
	);
end;

// Returns the suggested file name.
// The resulting string must be freed by calling cef_string_userfree_free().
function cef_download_item_get_suggested_file_name(self: PCefDownloadItem): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.UserFreeString(
		TCefDownloadItemOwn(TWACef.GetObject(self)).GetSuggestedFileName
	);
end;

// Returns the content disposition.
// The resulting string must be freed by calling cef_string_userfree_free().
function cef_download_item_get_content_disposition(self: PCefDownloadItem): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.UserFreeString(
		TCefDownloadItemOwn(TWACef.GetObject(self)).GetContentDisposition
	);
end;

// Returns the mime type.
// The resulting string must be freed by calling cef_string_userfree_free().
function cef_download_item_get_mime_type(self: PCefDownloadItem): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.UserFreeString(
		TCefDownloadItemOwn(TWACef.GetObject(self)).GetMimeType
	);
end;

{Protected section}
function TCefDownloadItemOwn.IsValid: Boolean;
begin
	Result := false;
end;

function TCefDownloadItemOwn.IsInProgress: Boolean;
begin
	Result := false;
end;

function TCefDownloadItemOwn.IsComplete: Boolean;
begin
	Result := false;
end;

function TCefDownloadItemOwn.IsCanceled: Boolean;
begin
	Result := false;
end;

function TCefDownloadItemOwn.GetCurrentSpeed: cint64;
begin
end;

function TCefDownloadItemOwn.GetPercentComplete: cint;
begin
	Result := 0;
end;

function TCefDownloadItemOwn.GetTotalBytes: cint64;
begin
end;

function TCefDownloadItemOwn.GetReceivedBytes: cint64;
begin
end;

function TCefDownloadItemOwn.GetStartTime: TCefTime;
begin
end;

function TCefDownloadItemOwn.GetEndTime: TCefTime;
begin
end;

function TCefDownloadItemOwn.GetFullPath: ustring;
begin
end;

function TCefDownloadItemOwn.GetId: cuint32;
begin
end;

function TCefDownloadItemOwn.GetUrl: ustring;
begin
end;

function TCefDownloadItemOwn.GetOriginalUrl: ustring;
begin
end;

function TCefDownloadItemOwn.GetSuggestedFileName: ustring;
begin
end;

function TCefDownloadItemOwn.GetContentDisposition: ustring;
begin
end;

function TCefDownloadItemOwn.GetMimeType: ustring;
begin
end;

{Public section}
constructor TCefDownloadItemOwn.Create;
begin
	inherited CreateData(SizeOf(TCefDownloadItem));
	with PCefDownloadItem(FData)^ do
	begin
		is_valid := {$IFDEF FPC}@{$ENDIF}cef_download_item_is_valid;
		is_in_progress := {$IFDEF FPC}@{$ENDIF}cef_download_item_is_in_progress;
		is_complete := {$IFDEF FPC}@{$ENDIF}cef_download_item_is_complete;
		is_canceled := {$IFDEF FPC}@{$ENDIF}cef_download_item_is_canceled;
		get_current_speed := {$IFDEF FPC}@{$ENDIF}cef_download_item_get_current_speed;
		get_percent_complete := {$IFDEF FPC}@{$ENDIF}cef_download_item_get_percent_complete;
		get_total_bytes := {$IFDEF FPC}@{$ENDIF}cef_download_item_get_total_bytes;
		get_received_bytes := {$IFDEF FPC}@{$ENDIF}cef_download_item_get_received_bytes;
		get_start_time := {$IFDEF FPC}@{$ENDIF}cef_download_item_get_start_time;
		get_end_time := {$IFDEF FPC}@{$ENDIF}cef_download_item_get_end_time;
		get_full_path := {$IFDEF FPC}@{$ENDIF}cef_download_item_get_full_path;
		get_id := {$IFDEF FPC}@{$ENDIF}cef_download_item_get_id;
		get_url := {$IFDEF FPC}@{$ENDIF}cef_download_item_get_url;
		get_original_url := {$IFDEF FPC}@{$ENDIF}cef_download_item_get_original_url;
		get_suggested_file_name := {$IFDEF FPC}@{$ENDIF}cef_download_item_get_suggested_file_name;
		get_content_disposition := {$IFDEF FPC}@{$ENDIF}cef_download_item_get_content_disposition;
		get_mime_type := {$IFDEF FPC}@{$ENDIF}cef_download_item_get_mime_type;
	end;
end;

//..............................................................................TCefDragDataOwn
// Returns a copy of the current object.
function cef_drag_data_clone(self: PCefDragData): PCefDragData; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.GetData(
		TCefDragDataOwn(TWACef.GetObject(self)).Clone
	);
end;

// Returns true (1) if this object is read-only.
function cef_drag_data_is_read_only(self: PCefDragData): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefDragDataOwn(TWACef.GetObject(self)).IsReadOnly
	);
end;

// Returns true (1) if the drag data is a link.
function cef_drag_data_is_link(self: PCefDragData): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefDragDataOwn(TWACef.GetObject(self)).IsLink
	);
end;

// Returns true (1) if the drag data is a text or html fragment.
function cef_drag_data_is_fragment(self: PCefDragData): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefDragDataOwn(TWACef.GetObject(self)).IsFragment
	);
end;

// Returns true (1) if the drag data is a file.
function cef_drag_data_is_file(self: PCefDragData): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefDragDataOwn(TWACef.GetObject(self)).IsFile
	);
end;

// Return the link URL that is being dragged.
// The resulting string must be freed by calling cef_string_userfree_free().
function cef_drag_data_get_link_url(self: PCefDragData): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.UserFreeString(
		TCefDragDataOwn(TWACef.GetObject(self)).GetLinkUrl
	);
end;

// Return the title associated with the link being dragged.
// The resulting string must be freed by calling cef_string_userfree_free().
function cef_drag_data_get_link_title(self: PCefDragData): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.UserFreeString(
		TCefDragDataOwn(TWACef.GetObject(self)).GetLinkTitle
	);
end;

// Return the metadata, if any, associated with the link being dragged.
// The resulting string must be freed by calling cef_string_userfree_free().
function cef_drag_data_get_link_metadata(self: PCefDragData): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.UserFreeString(
		TCefDragDataOwn(TWACef.GetObject(self)).GetLinkMetadata
	);
end;

// Return the plain text fragment that is being dragged.
// The resulting string must be freed by calling cef_string_userfree_free().
function cef_drag_data_get_fragment_text(self: PCefDragData): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.UserFreeString(
		TCefDragDataOwn(TWACef.GetObject(self)).GetFragmentText
	);
end;

// Return the text/html fragment that is being dragged.
// The resulting string must be freed by calling cef_string_userfree_free().
function cef_drag_data_get_fragment_html(self: PCefDragData): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.UserFreeString(
		TCefDragDataOwn(TWACef.GetObject(self)).GetFragmentHtml
	);
end;

// Return the base URL that the fragment came from. This value is used for
// resolving relative URLs and may be NULL.
// The resulting string must be freed by calling cef_string_userfree_free().
function cef_drag_data_get_fragment_base_url(self: PCefDragData): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.UserFreeString(
		TCefDragDataOwn(TWACef.GetObject(self)).GetFragmentBaseUrl
	);
end;

// Return the name of the file being dragged out of the browser window.
// The resulting string must be freed by calling cef_string_userfree_free().
function cef_drag_data_get_file_name(self: PCefDragData): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.UserFreeString(
		TCefDragDataOwn(TWACef.GetObject(self)).GetFileName
	);
end;

// Write the contents of the file being dragged out of the web view into
// |writer|. Returns the number of bytes sent to |writer|. If |writer| is NULL
// this function will return the size of the file contents in bytes. Call
// get_file_name() to get a suggested name for the file.
function cef_drag_data_get_file_contents(self: PCefDragData; writer: PCefStreamWriter): csize_t; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := (
		TCefDragDataOwn(TWACef.GetObject(self)).GetFileContents(
			TCefStreamWriterRef.UnWrap(writer)
		)
	);
end;

// Retrieve the list of file names that are being dragged into the browser
// window.
function cef_drag_data_get_file_names(self: PCefDragData; names: TCefStringList): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefDragDataOwn(TWACef.GetObject(self)).GetFileNames(
			names
		)
	);
end;

// Set the link URL that is being dragged.
procedure cef_drag_data_set_link_url(self: PCefDragData; const url: PCefString); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	url_str: ustring;
begin
	url_str := TWACef.ToString(url);
	TCefDragDataOwn(TWACef.GetObject(self)).SetLinkUrl(
		url_str
	);
end;

// Set the title associated with the link being dragged.
procedure cef_drag_data_set_link_title(self: PCefDragData; const title: PCefString); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	title_str: ustring;
begin
	title_str := TWACef.ToString(title);
	TCefDragDataOwn(TWACef.GetObject(self)).SetLinkTitle(
		title_str
	);
end;

// Set the metadata associated with the link being dragged.
procedure cef_drag_data_set_link_metadata(self: PCefDragData; const data: PCefString); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	data_str: ustring;
begin
	data_str := TWACef.ToString(data);
	TCefDragDataOwn(TWACef.GetObject(self)).SetLinkMetadata(
		data_str
	);
end;

// Set the plain text fragment that is being dragged.
procedure cef_drag_data_set_fragment_text(self: PCefDragData; const text: PCefString); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	text_str: ustring;
begin
	text_str := TWACef.ToString(text);
	TCefDragDataOwn(TWACef.GetObject(self)).SetFragmentText(
		text_str
	);
end;

// Set the text/html fragment that is being dragged.
procedure cef_drag_data_set_fragment_html(self: PCefDragData; const html: PCefString); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	html_str: ustring;
begin
	html_str := TWACef.ToString(html);
	TCefDragDataOwn(TWACef.GetObject(self)).SetFragmentHtml(
		html_str
	);
end;

// Set the base URL that the fragment came from.
procedure cef_drag_data_set_fragment_base_url(self: PCefDragData; const base_url: PCefString); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	base_url_str: ustring;
begin
	base_url_str := TWACef.ToString(base_url);
	TCefDragDataOwn(TWACef.GetObject(self)).SetFragmentBaseUrl(
		base_url_str
	);
end;

// Reset the file contents. You should do this before calling
// cef_browser_host_t::DragTargetDragEnter as the web view does not allow us
// to drag in this kind of data.
procedure cef_drag_data_reset_file_contents(self: PCefDragData); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefDragDataOwn(TWACef.GetObject(self)).ResetFileContents;
end;

// Add a file that is being dragged into the webview.
procedure cef_drag_data_add_file(self: PCefDragData; const path: PCefString; const display_name: PCefString); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	path_str: ustring;
	display_name_str: ustring;
begin
	path_str := TWACef.ToString(path);
	display_name_str := TWACef.ToString(display_name);
	TCefDragDataOwn(TWACef.GetObject(self)).AddFile(
		path_str,
		display_name_str
	);
end;

{Protected section}
function TCefDragDataOwn.Clone: ICefDragData;
begin
	Result := nil;
end;

function TCefDragDataOwn.IsReadOnly: Boolean;
begin
	Result := false;
end;

function TCefDragDataOwn.IsLink: Boolean;
begin
	Result := false;
end;

function TCefDragDataOwn.IsFragment: Boolean;
begin
	Result := false;
end;

function TCefDragDataOwn.IsFile: Boolean;
begin
	Result := false;
end;

function TCefDragDataOwn.GetLinkUrl: ustring;
begin
end;

function TCefDragDataOwn.GetLinkTitle: ustring;
begin
end;

function TCefDragDataOwn.GetLinkMetadata: ustring;
begin
end;

function TCefDragDataOwn.GetFragmentText: ustring;
begin
end;

function TCefDragDataOwn.GetFragmentHtml: ustring;
begin
end;

function TCefDragDataOwn.GetFragmentBaseUrl: ustring;
begin
end;

function TCefDragDataOwn.GetFileName: ustring;
begin
end;

function TCefDragDataOwn.GetFileContents(const aWriter: ICefStreamWriter): csize_t;
begin
end;

function TCefDragDataOwn.GetFileNames(aNames: TStrings): Boolean;
begin
	Result := false;
end;

procedure TCefDragDataOwn.SetLinkUrl(const aUrl: ustring);
begin
end;

procedure TCefDragDataOwn.SetLinkTitle(const aTitle: ustring);
begin
end;

procedure TCefDragDataOwn.SetLinkMetadata(const aData: ustring);
begin
end;

procedure TCefDragDataOwn.SetFragmentText(const aText: ustring);
begin
end;

procedure TCefDragDataOwn.SetFragmentHtml(const aHtml: ustring);
begin
end;

procedure TCefDragDataOwn.SetFragmentBaseUrl(const aBaseUrl: ustring);
begin
end;

procedure TCefDragDataOwn.ResetFileContents;
begin
end;

procedure TCefDragDataOwn.AddFile(const aPath: ustring; const aDisplayName: ustring);
begin
end;

{Public section}
constructor TCefDragDataOwn.Create;
begin
	inherited CreateData(SizeOf(TCefDragData));
	with PCefDragData(FData)^ do
	begin
		clone := {$IFDEF FPC}@{$ENDIF}cef_drag_data_clone;
		is_read_only := {$IFDEF FPC}@{$ENDIF}cef_drag_data_is_read_only;
		is_link := {$IFDEF FPC}@{$ENDIF}cef_drag_data_is_link;
		is_fragment := {$IFDEF FPC}@{$ENDIF}cef_drag_data_is_fragment;
		is_file := {$IFDEF FPC}@{$ENDIF}cef_drag_data_is_file;
		get_link_url := {$IFDEF FPC}@{$ENDIF}cef_drag_data_get_link_url;
		get_link_title := {$IFDEF FPC}@{$ENDIF}cef_drag_data_get_link_title;
		get_link_metadata := {$IFDEF FPC}@{$ENDIF}cef_drag_data_get_link_metadata;
		get_fragment_text := {$IFDEF FPC}@{$ENDIF}cef_drag_data_get_fragment_text;
		get_fragment_html := {$IFDEF FPC}@{$ENDIF}cef_drag_data_get_fragment_html;
		get_fragment_base_url := {$IFDEF FPC}@{$ENDIF}cef_drag_data_get_fragment_base_url;
		get_file_name := {$IFDEF FPC}@{$ENDIF}cef_drag_data_get_file_name;
		get_file_contents := {$IFDEF FPC}@{$ENDIF}cef_drag_data_get_file_contents;
		get_file_names := {$IFDEF FPC}@{$ENDIF}cef_drag_data_get_file_names;
		set_link_url := {$IFDEF FPC}@{$ENDIF}cef_drag_data_set_link_url;
		set_link_title := {$IFDEF FPC}@{$ENDIF}cef_drag_data_set_link_title;
		set_link_metadata := {$IFDEF FPC}@{$ENDIF}cef_drag_data_set_link_metadata;
		set_fragment_text := {$IFDEF FPC}@{$ENDIF}cef_drag_data_set_fragment_text;
		set_fragment_html := {$IFDEF FPC}@{$ENDIF}cef_drag_data_set_fragment_html;
		set_fragment_base_url := {$IFDEF FPC}@{$ENDIF}cef_drag_data_set_fragment_base_url;
		reset_file_contents := {$IFDEF FPC}@{$ENDIF}cef_drag_data_reset_file_contents;
		add_file := {$IFDEF FPC}@{$ENDIF}cef_drag_data_add_file;
	end;
end;

//..............................................................................TCefDragHandlerOwn
// Called when an external drag event enters the browser window. |dragData|
// contains the drag event data and |mask| represents the type of drag
// operation. Return false (0) for default drag handling behavior or true (1)
// to cancel the drag event.
function cef_drag_handler_on_drag_enter(self: PCefDragHandler; browser: PCefBrowser; dragData: PCefDragData; mask: TCefDragOperationsMask): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefDragHandlerOwn(TWACef.GetObject(self)).OnDragEnter(
			TCefBrowserRef.UnWrap(browser),
			TCefDragDataRef.UnWrap(dragData),
			mask
		)
	);
end;

{Protected section}
function TCefDragHandlerOwn.OnDragEnter(const aBrowser: ICefBrowser; const aDragData: ICefDragData; aMask: TCefDragOperationsMask): Boolean;
begin
	Result := false;
end;

{Public section}
constructor TCefDragHandlerOwn.Create;
begin
	inherited CreateData(SizeOf(TCefDragHandler));
	with PCefDragHandler(FData)^ do
	begin
		on_drag_enter := {$IFDEF FPC}@{$ENDIF}cef_drag_handler_on_drag_enter;
	end;
end;

//..............................................................................TCefFindHandlerOwn
// Called to report find results returned by cef_browser_host_t::find().
// |identifer| is the identifier passed to find(), |count| is the number of
// matches currently identified, |selectionRect| is the location of where the
// match was found (in window coordinates), |activeMatchOrdinal| is the
// current position in the search results, and |finalUpdate| is true (1) if
// this is the last find notification.
procedure cef_find_handler_on_find_result(self: PCefFindHandler; browser: PCefBrowser; identifier: cint; count: cint; const selectionRect: PCefRect; activeMatchOrdinal: cint; finalUpdate: cint); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefFindHandlerOwn(TWACef.GetObject(self)).OnFindResult(
		TCefBrowserRef.UnWrap(browser),
		identifier,
		count,
		selectionRect^,
		activeMatchOrdinal,
		finalUpdate <> 0
	);
end;

{Protected section}
procedure TCefFindHandlerOwn.OnFindResult(const aBrowser: ICefBrowser; aIdentifier: cint; aCount: cint; const aSelectionRect: TCefRect; aActiveMatchOrdinal: cint; aFinalUpdate: Boolean);
begin
end;

{Public section}
constructor TCefFindHandlerOwn.Create;
begin
	inherited CreateData(SizeOf(TCefFindHandler));
	with PCefFindHandler(FData)^ do
	begin
		on_find_result := {$IFDEF FPC}@{$ENDIF}cef_find_handler_on_find_result;
	end;
end;

//..............................................................................TCefFocusHandlerOwn
// Called when the browser component is about to loose focus. For instance, if
// focus was on the last HTML element and the user pressed the TAB key. |next|
// will be true (1) if the browser is giving focus to the next component and
// false (0) if the browser is giving focus to the previous component.
procedure cef_focus_handler_on_take_focus(self: PCefFocusHandler; browser: PCefBrowser; next: cint); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefFocusHandlerOwn(TWACef.GetObject(self)).OnTakeFocus(
		TCefBrowserRef.UnWrap(browser),
		next <> 0
	);
end;

// Called when the browser component is requesting focus. |source| indicates
// where the focus request is originating from. Return false (0) to allow the
// focus to be set or true (1) to cancel setting the focus.
function cef_focus_handler_on_set_focus(self: PCefFocusHandler; browser: PCefBrowser; source: TCefFocusSource): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefFocusHandlerOwn(TWACef.GetObject(self)).OnSetFocus(
			TCefBrowserRef.UnWrap(browser),
			source
		)
	);
end;

// Called when the browser component has received focus.
procedure cef_focus_handler_on_got_focus(self: PCefFocusHandler; browser: PCefBrowser); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefFocusHandlerOwn(TWACef.GetObject(self)).OnGotFocus(
		TCefBrowserRef.UnWrap(browser)
	);
end;

{Protected section}
procedure TCefFocusHandlerOwn.OnTakeFocus(const aBrowser: ICefBrowser; aNext: Boolean);
begin
end;

function TCefFocusHandlerOwn.OnSetFocus(const aBrowser: ICefBrowser; aSource: TCefFocusSource): Boolean;
begin
	Result := false;
end;

procedure TCefFocusHandlerOwn.OnGotFocus(const aBrowser: ICefBrowser);
begin
end;

{Public section}
constructor TCefFocusHandlerOwn.Create;
begin
	inherited CreateData(SizeOf(TCefFocusHandler));
	with PCefFocusHandler(FData)^ do
	begin
		on_take_focus := {$IFDEF FPC}@{$ENDIF}cef_focus_handler_on_take_focus;
		on_set_focus := {$IFDEF FPC}@{$ENDIF}cef_focus_handler_on_set_focus;
		on_got_focus := {$IFDEF FPC}@{$ENDIF}cef_focus_handler_on_got_focus;
	end;
end;

//..............................................................................TCefFrameOwn
// True if this object is currently attached to a valid frame.
function cef_frame_is_valid(self: PCefFrame): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefFrameOwn(TWACef.GetObject(self)).IsValid
	);
end;

// Execute undo in this frame.
procedure cef_frame_undo(self: PCefFrame); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefFrameOwn(TWACef.GetObject(self)).Undo;
end;

// Execute redo in this frame.
procedure cef_frame_redo(self: PCefFrame); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefFrameOwn(TWACef.GetObject(self)).Redo;
end;

// Execute cut in this frame.
procedure cef_frame_cut(self: PCefFrame); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefFrameOwn(TWACef.GetObject(self)).Cut;
end;

// Execute copy in this frame.
procedure cef_frame_copy(self: PCefFrame); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefFrameOwn(TWACef.GetObject(self)).Copy;
end;

// Execute paste in this frame.
procedure cef_frame_paste(self: PCefFrame); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefFrameOwn(TWACef.GetObject(self)).Paste;
end;

// Execute delete in this frame.
procedure cef_frame_del(self: PCefFrame); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefFrameOwn(TWACef.GetObject(self)).Del;
end;

// Execute select all in this frame.
procedure cef_frame_select_all(self: PCefFrame); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefFrameOwn(TWACef.GetObject(self)).SelectAll;
end;

// Save this frame's HTML source to a temporary file and open it in the
// default text viewing application. This function can only be called from the
// browser process.
procedure cef_frame_view_source(self: PCefFrame); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefFrameOwn(TWACef.GetObject(self)).ViewSource;
end;

// Retrieve this frame's HTML source as a string sent to the specified
// visitor.
procedure cef_frame_get_source(self: PCefFrame; visitor: PCefStringVisitor); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefFrameOwn(TWACef.GetObject(self)).GetSource(
		TCefStringVisitorRef.UnWrap(visitor)
	);
end;

// Retrieve this frame's display text as a string sent to the specified
// visitor.
procedure cef_frame_get_text(self: PCefFrame; visitor: PCefStringVisitor); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefFrameOwn(TWACef.GetObject(self)).GetText(
		TCefStringVisitorRef.UnWrap(visitor)
	);
end;

// Load the request represented by the |request| object.
procedure cef_frame_load_request(self: PCefFrame; request: PCefRequest); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefFrameOwn(TWACef.GetObject(self)).LoadRequest(
		TCefRequestRef.UnWrap(request)
	);
end;

// Load the specified |url|.
procedure cef_frame_load_url(self: PCefFrame; const url: PCefString); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	url_str: ustring;
begin
	url_str := TWACef.ToString(url);
	TCefFrameOwn(TWACef.GetObject(self)).LoadUrl(
		url_str
	);
end;

// Load the contents of |string_val| with the specified dummy |url|. |url|
// should have a standard scheme (for example, http scheme) or behaviors like
// link clicks and web security restrictions may not behave as expected.
procedure cef_frame_load_string(self: PCefFrame; const string_val: PCefString; const url: PCefString); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	string_val_str: ustring;
	url_str: ustring;
begin
	string_val_str := TWACef.ToString(string_val);
	url_str := TWACef.ToString(url);
	TCefFrameOwn(TWACef.GetObject(self)).LoadString(
		string_val_str,
		url_str
	);
end;

// Execute a string of JavaScript code in this frame. The |script_url|
// parameter is the URL where the script in question can be found, if any. The
// renderer may request this URL to show the developer the source of the
// error.  The |start_line| parameter is the base line number to use for error
// reporting.
procedure cef_frame_execute_java_script(self: PCefFrame; const code: PCefString; const script_url: PCefString; start_line: cint); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	code_str: ustring;
	script_url_str: ustring;
begin
	code_str := TWACef.ToString(code);
	script_url_str := TWACef.ToString(script_url);
	TCefFrameOwn(TWACef.GetObject(self)).ExecuteJavaScript(
		code_str,
		script_url_str,
		start_line
	);
end;

// Returns true (1) if this is the main (top-level) frame.
function cef_frame_is_main(self: PCefFrame): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefFrameOwn(TWACef.GetObject(self)).IsMain
	);
end;

// Returns true (1) if this is the focused frame.
function cef_frame_is_focused(self: PCefFrame): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefFrameOwn(TWACef.GetObject(self)).IsFocused
	);
end;

// Returns the name for this frame. If the frame has an assigned name (for
// example, set via the iframe "name" attribute) then that value will be
// returned. Otherwise a unique name will be constructed based on the frame
// parent hierarchy. The main (top-level) frame will always have an NULL name
// value.
// The resulting string must be freed by calling cef_string_userfree_free().
function cef_frame_get_name(self: PCefFrame): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.UserFreeString(
		TCefFrameOwn(TWACef.GetObject(self)).GetName
	);
end;

// Returns the globally unique identifier for this frame.
function cef_frame_get_identifier(self: PCefFrame): cint64; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := (
		TCefFrameOwn(TWACef.GetObject(self)).GetIdentifier
	);
end;

// Returns the parent of this frame or NULL if this is the main (top-level)
// frame.
function cef_frame_get_parent(self: PCefFrame): PCefFrame; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.GetData(
		TCefFrameOwn(TWACef.GetObject(self)).GetParent
	);
end;

// Returns the URL currently loaded in this frame.
// The resulting string must be freed by calling cef_string_userfree_free().
function cef_frame_get_url(self: PCefFrame): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.UserFreeString(
		TCefFrameOwn(TWACef.GetObject(self)).GetUrl
	);
end;

// Returns the browser that this frame belongs to.
function cef_frame_get_browser(self: PCefFrame): PCefBrowser; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.GetData(
		TCefFrameOwn(TWACef.GetObject(self)).GetBrowser
	);
end;

// Get the V8 context associated with the frame. This function can only be
// called from the render process.
function cef_frame_get_v8context(self: PCefFrame): PCefV8context; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.GetData(
		TCefFrameOwn(TWACef.GetObject(self)).GetV8context
	);
end;

// Visit the DOM document. This function can only be called from the render
// process.
procedure cef_frame_visit_dom(self: PCefFrame; visitor: PCefDomvisitor); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefFrameOwn(TWACef.GetObject(self)).VisitDom(
		TCefDomvisitorRef.UnWrap(visitor)
	);
end;

{Protected section}
function TCefFrameOwn.IsValid: Boolean;
begin
	Result := false;
end;

procedure TCefFrameOwn.Undo;
begin
end;

procedure TCefFrameOwn.Redo;
begin
end;

procedure TCefFrameOwn.Cut;
begin
end;

procedure TCefFrameOwn.Copy;
begin
end;

procedure TCefFrameOwn.Paste;
begin
end;

procedure TCefFrameOwn.Del;
begin
end;

procedure TCefFrameOwn.SelectAll;
begin
end;

procedure TCefFrameOwn.ViewSource;
begin
end;

procedure TCefFrameOwn.GetSource(const aVisitor: ICefStringVisitor);
begin
end;

procedure TCefFrameOwn.GetSourceProc(const aProc: TCefStringVisitorProc);
begin
end;

procedure TCefFrameOwn.GetText(const aVisitor: ICefStringVisitor);
begin
end;

procedure TCefFrameOwn.GetTextProc(const aProc: TCefStringVisitorProc);
begin
end;

procedure TCefFrameOwn.LoadRequest(const aRequest: ICefRequest);
begin
end;

procedure TCefFrameOwn.LoadUrl(const aUrl: ustring);
begin
end;

procedure TCefFrameOwn.LoadString(const aStringVal: ustring; const aUrl: ustring);
begin
end;

procedure TCefFrameOwn.ExecuteJavaScript(const aCode: ustring; const aScriptUrl: ustring; aStartLine: cint);
begin
end;

function TCefFrameOwn.IsMain: Boolean;
begin
	Result := false;
end;

function TCefFrameOwn.IsFocused: Boolean;
begin
	Result := false;
end;

function TCefFrameOwn.GetName: ustring;
begin
end;

function TCefFrameOwn.GetIdentifier: cint64;
begin
end;

function TCefFrameOwn.GetParent: ICefFrame;
begin
	Result := nil;
end;

function TCefFrameOwn.GetUrl: ustring;
begin
end;

function TCefFrameOwn.GetBrowser: ICefBrowser;
begin
	Result := nil;
end;

function TCefFrameOwn.GetV8context: ICefV8context;
begin
	Result := nil;
end;

procedure TCefFrameOwn.VisitDom(const aVisitor: ICefDomvisitor);
begin
end;

procedure TCefFrameOwn.VisitDomProc(const aProc: TCefDomVisitorProc);
begin
end;

{Public section}
constructor TCefFrameOwn.Create;
begin
	inherited CreateData(SizeOf(TCefFrame));
	with PCefFrame(FData)^ do
	begin
		is_valid := {$IFDEF FPC}@{$ENDIF}cef_frame_is_valid;
		undo := {$IFDEF FPC}@{$ENDIF}cef_frame_undo;
		redo := {$IFDEF FPC}@{$ENDIF}cef_frame_redo;
		cut := {$IFDEF FPC}@{$ENDIF}cef_frame_cut;
		copy := {$IFDEF FPC}@{$ENDIF}cef_frame_copy;
		paste := {$IFDEF FPC}@{$ENDIF}cef_frame_paste;
		del := {$IFDEF FPC}@{$ENDIF}cef_frame_del;
		select_all := {$IFDEF FPC}@{$ENDIF}cef_frame_select_all;
		view_source := {$IFDEF FPC}@{$ENDIF}cef_frame_view_source;
		get_source := {$IFDEF FPC}@{$ENDIF}cef_frame_get_source;
		get_text := {$IFDEF FPC}@{$ENDIF}cef_frame_get_text;
		load_request := {$IFDEF FPC}@{$ENDIF}cef_frame_load_request;
		load_url := {$IFDEF FPC}@{$ENDIF}cef_frame_load_url;
		load_string := {$IFDEF FPC}@{$ENDIF}cef_frame_load_string;
		execute_java_script := {$IFDEF FPC}@{$ENDIF}cef_frame_execute_java_script;
		is_main := {$IFDEF FPC}@{$ENDIF}cef_frame_is_main;
		is_focused := {$IFDEF FPC}@{$ENDIF}cef_frame_is_focused;
		get_name := {$IFDEF FPC}@{$ENDIF}cef_frame_get_name;
		get_identifier := {$IFDEF FPC}@{$ENDIF}cef_frame_get_identifier;
		get_parent := {$IFDEF FPC}@{$ENDIF}cef_frame_get_parent;
		get_url := {$IFDEF FPC}@{$ENDIF}cef_frame_get_url;
		get_browser := {$IFDEF FPC}@{$ENDIF}cef_frame_get_browser;
		get_v8context := {$IFDEF FPC}@{$ENDIF}cef_frame_get_v8context;
		visit_dom := {$IFDEF FPC}@{$ENDIF}cef_frame_visit_dom;
	end;
end;

//..............................................................................TCefGetGeolocationCallbackOwn
// Called with the 'best available' location information or, if the location
// update failed, with error information.
procedure cef_get_geolocation_callback_on_location_update(self: PCefGetGeolocationCallback; const position: PCefGeoposition); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefGetGeolocationCallbackOwn(TWACef.GetObject(self)).OnLocationUpdate(
		position^
	);
end;

{Protected section}
procedure TCefGetGeolocationCallbackOwn.OnLocationUpdate(const aPosition: TCefGeoposition);
begin
end;

{Public section}
constructor TCefGetGeolocationCallbackOwn.Create;
begin
	inherited CreateData(SizeOf(TCefGetGeolocationCallback));
	with PCefGetGeolocationCallback(FData)^ do
	begin
		on_location_update := {$IFDEF FPC}@{$ENDIF}cef_get_geolocation_callback_on_location_update;
	end;
end;

//..............................................................................TCefGeolocationCallbackOwn
// Call to allow or deny geolocation access.
procedure cef_geolocation_callback_cont(self: PCefGeolocationCallback; allow: cint); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefGeolocationCallbackOwn(TWACef.GetObject(self)).Cont(
		allow <> 0
	);
end;

{Protected section}
procedure TCefGeolocationCallbackOwn.Cont(aAllow: Boolean);
begin
end;

{Public section}
constructor TCefGeolocationCallbackOwn.Create;
begin
	inherited CreateData(SizeOf(TCefGeolocationCallback));
	with PCefGeolocationCallback(FData)^ do
	begin
		cont := {$IFDEF FPC}@{$ENDIF}cef_geolocation_callback_cont;
	end;
end;
//..............................................................................TCefGeolocationHandlerOwn
// Called when a page requests permission to access geolocation information.
// |requesting_url| is the URL requesting permission and |request_id| is the
// unique ID for the permission request. Return true (1) and call
// cef_geolocation_callback_t::cont() either in this function or at a later
// time to continue or cancel the request. Return false (0) to cancel the
// request immediately.
function cef_geolocation_handler_on_request_geolocation_permission(self: PCefGeolocationHandler; browser: PCefBrowser; const requesting_url: PCefString; request_id: cint; callback: PCefGeolocationCallback): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	requesting_url_str: ustring;
begin
	requesting_url_str := TWACef.ToString(requesting_url);
	Result := Ord(
		TCefGeolocationHandlerOwn(TWACef.GetObject(self)).OnRequestGeolocationPermission(
			TCefBrowserRef.UnWrap(browser),
			requesting_url_str,
			request_id,
			TCefGeolocationCallbackRef.UnWrap(callback)
		)
	);
end;

// Called when a geolocation access request is canceled. |requesting_url| is
// the URL that originally requested permission and |request_id| is the unique
// ID for the permission request.
procedure cef_geolocation_handler_on_cancel_geolocation_permission(self: PCefGeolocationHandler; browser: PCefBrowser; const requesting_url: PCefString; request_id: cint); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	requesting_url_str: ustring;
begin
	requesting_url_str := TWACef.ToString(requesting_url);
	TCefGeolocationHandlerOwn(TWACef.GetObject(self)).OnCancelGeolocationPermission(
		TCefBrowserRef.UnWrap(browser),
		requesting_url_str,
		request_id
	);
end;

{Protected section}
function TCefGeolocationHandlerOwn.OnRequestGeolocationPermission(const aBrowser: ICefBrowser; const aRequestingUrl: ustring; aRequestId: cint; const aCallback: ICefGeolocationCallback): Boolean;
begin
	Result := false;
end;

procedure TCefGeolocationHandlerOwn.OnCancelGeolocationPermission(const aBrowser: ICefBrowser; const aRequestingUrl: ustring; aRequestId: cint);
begin
end;

{Public section}
constructor TCefGeolocationHandlerOwn.Create;
begin
	inherited CreateData(SizeOf(TCefGeolocationHandler));
	with PCefGeolocationHandler(FData)^ do
	begin
		on_request_geolocation_permission := {$IFDEF FPC}@{$ENDIF}cef_geolocation_handler_on_request_geolocation_permission;
		on_cancel_geolocation_permission := {$IFDEF FPC}@{$ENDIF}cef_geolocation_handler_on_cancel_geolocation_permission;
	end;
end;

//..............................................................................TCefJsdialogCallbackOwn
// Continue the JS dialog request. Set |success| to true (1) if the OK button
// was pressed. The |user_input| value should be specified for prompt dialogs.
procedure cef_jsdialog_callback_cont(self: PCefJsdialogCallback; success: cint; const user_input: PCefString); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	success_proxy: Boolean;
	user_input_str: ustring;
begin
	success_proxy := success <> 0;
	user_input_str := TWACef.ToString(user_input);
	TCefJsdialogCallbackOwn(TWACef.GetObject(self)).Cont(
		success_proxy,
		user_input_str
	);
end;

{Protected section}
procedure TCefJsdialogCallbackOwn.Cont(aSuccess: Boolean; const aUserInput: ustring);
begin
end;

{Public section}
constructor TCefJsdialogCallbackOwn.Create;
begin
	inherited CreateData(SizeOf(TCefJsdialogCallback));
	with PCefJsdialogCallback(FData)^ do
	begin
		cont := {$IFDEF FPC}@{$ENDIF}cef_jsdialog_callback_cont;
	end;
end;
//..............................................................................TCefJsdialogHandlerOwn
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
function cef_jsdialog_handler_on_jsdialog(self: PCefJsdialogHandler; browser: PCefBrowser; const origin_url: PCefString; const accept_lang: PCefString; dialog_type: TCefJsdialogType; const message_text: PCefString; const default_prompt_text: PCefString; callback: PCefJsdialogCallback; suppress_message: pcint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	origin_url_str: ustring;
	accept_lang_str: ustring;
	message_text_str: ustring;
	default_prompt_text_str: ustring;
	suppress_message_proxy: Boolean;
begin
	origin_url_str := TWACef.ToString(origin_url);
	accept_lang_str := TWACef.ToString(accept_lang);
	message_text_str := TWACef.ToString(message_text);
	default_prompt_text_str := TWACef.ToString(default_prompt_text);
	suppress_message_proxy := suppress_message^  <> 0;
	Result := Ord(
		TCefJsdialogHandlerOwn(TWACef.GetObject(self)).OnJsdialog(
			TCefBrowserRef.UnWrap(browser),
			origin_url_str,
			accept_lang_str,
			dialog_type,
			message_text_str,
			default_prompt_text_str,
			TCefJsdialogCallbackRef.UnWrap(callback),
			suppress_message_proxy
		)
	);
	suppress_message^  := Ord(suppress_message_proxy);
end;

// Called to run a dialog asking the user if they want to leave a page. Return
// false (0) to use the default dialog implementation. Return true (1) if the
// application will use a custom dialog or if the callback has been executed
// immediately. Custom dialogs may be either modal or modeless. If a custom
// dialog is used the application must execute |callback| once the custom
// dialog is dismissed.
function cef_jsdialog_handler_on_before_unload_dialog(self: PCefJsdialogHandler; browser: PCefBrowser; const message_text: PCefString; is_reload: cint; callback: PCefJsdialogCallback): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	message_text_str: ustring;
begin
	message_text_str := TWACef.ToString(message_text);
	Result := Ord(
		TCefJsdialogHandlerOwn(TWACef.GetObject(self)).OnBeforeUnloadDialog(
			TCefBrowserRef.UnWrap(browser),
			message_text_str,
			is_reload <> 0,
			TCefJsdialogCallbackRef.UnWrap(callback)
		)
	);
end;

// Called to cancel any pending dialogs and reset any saved dialog state. Will
// be called due to events like page navigation irregardless of whether any
// dialogs are currently pending.
procedure cef_jsdialog_handler_on_reset_dialog_state(self: PCefJsdialogHandler; browser: PCefBrowser); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefJsdialogHandlerOwn(TWACef.GetObject(self)).OnResetDialogState(
		TCefBrowserRef.UnWrap(browser)
	);
end;

// Called when the default implementation dialog is closed.
procedure cef_jsdialog_handler_on_dialog_closed(self: PCefJsdialogHandler; browser: PCefBrowser); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefJsdialogHandlerOwn(TWACef.GetObject(self)).OnDialogClosed(
		TCefBrowserRef.UnWrap(browser)
	);
end;

{Protected section}
function TCefJsdialogHandlerOwn.OnJsdialog(const aBrowser: ICefBrowser; const aOriginUrl: ustring; const aAcceptLang: ustring; aDialogType: TCefJsdialogType; const aMessageText: ustring; const aDefaultPromptText: ustring; const aCallback: ICefJsdialogCallback; out aSuppressMessage: Boolean): Boolean;
begin
	Result := false;
end;

function TCefJsdialogHandlerOwn.OnBeforeUnloadDialog(const aBrowser: ICefBrowser; const aMessageText: ustring; aIsReload: Boolean; const aCallback: ICefJsdialogCallback): Boolean;
begin
	Result := false;
end;

procedure TCefJsdialogHandlerOwn.OnResetDialogState(const aBrowser: ICefBrowser);
begin
end;

procedure TCefJsdialogHandlerOwn.OnDialogClosed(const aBrowser: ICefBrowser);
begin
end;

{Public section}
constructor TCefJsdialogHandlerOwn.Create;
begin
	inherited CreateData(SizeOf(TCefJsdialogHandler));
	with PCefJsdialogHandler(FData)^ do
	begin
		on_jsdialog := {$IFDEF FPC}@{$ENDIF}cef_jsdialog_handler_on_jsdialog;
		on_before_unload_dialog := {$IFDEF FPC}@{$ENDIF}cef_jsdialog_handler_on_before_unload_dialog;
		on_reset_dialog_state := {$IFDEF FPC}@{$ENDIF}cef_jsdialog_handler_on_reset_dialog_state;
		on_dialog_closed := {$IFDEF FPC}@{$ENDIF}cef_jsdialog_handler_on_dialog_closed;
	end;
end;

//..............................................................................TCefKeyboardHandlerOwn
// Called before a keyboard event is sent to the renderer. |event| contains
// information about the keyboard event. |os_event| is the operating system
// event message, if any. Return true (1) if the event was handled or false
// (0) otherwise. If the event will be handled in on_key_event() as a keyboard
// shortcut set |is_keyboard_shortcut| to true (1) and return false (0).
function cef_keyboard_handler_on_pre_key_event(self: PCefKeyboardHandler; browser: PCefBrowser; const event: PCefKeyEvent; os_event: TCefEventHandle; is_keyboard_shortcut: pcint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	is_keyboard_shortcut_proxy: Boolean;
begin
	is_keyboard_shortcut_proxy := is_keyboard_shortcut^  <> 0;
	Result := Ord(
		TCefKeyboardHandlerOwn(TWACef.GetObject(self)).OnPreKeyEvent(
			TCefBrowserRef.UnWrap(browser),
			event^,
			os_event,
			is_keyboard_shortcut_proxy
		)
	);
	is_keyboard_shortcut^  := Ord(is_keyboard_shortcut_proxy);
end;

// Called after the renderer and JavaScript in the page has had a chance to
// handle the event. |event| contains information about the keyboard event.
// |os_event| is the operating system event message, if any. Return true (1)
// if the keyboard event was handled or false (0) otherwise.
function cef_keyboard_handler_on_key_event(self: PCefKeyboardHandler; browser: PCefBrowser; const event: PCefKeyEvent; os_event: TCefEventHandle): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefKeyboardHandlerOwn(TWACef.GetObject(self)).OnKeyEvent(
			TCefBrowserRef.UnWrap(browser),
			event^,
			os_event
		)
	);
end;

{Protected section}
function TCefKeyboardHandlerOwn.OnPreKeyEvent(const aBrowser: ICefBrowser; var aEvent: TCefKeyEvent; aOsEvent: TCefEventHandle; out aIsKeyboardShortcut: Boolean): Boolean;
begin
	Result := false;
end;

function TCefKeyboardHandlerOwn.OnKeyEvent(const aBrowser: ICefBrowser; var aEvent: TCefKeyEvent; aOsEvent: TCefEventHandle): Boolean;
begin
	Result := false;
end;

{Public section}
constructor TCefKeyboardHandlerOwn.Create;
begin
	inherited CreateData(SizeOf(TCefKeyboardHandler));
	with PCefKeyboardHandler(FData)^ do
	begin
		on_pre_key_event := {$IFDEF FPC}@{$ENDIF}cef_keyboard_handler_on_pre_key_event;
		on_key_event := {$IFDEF FPC}@{$ENDIF}cef_keyboard_handler_on_key_event;
	end;
end;

//..............................................................................TCefLifeSpanHandlerOwn
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
function cef_life_span_handler_on_before_popup(self: PCefLifeSpanHandler; browser: PCefBrowser; frame: PCefFrame; const target_url: PCefString; const target_frame_name: PCefString; target_disposition: TCefWindowOpenDisposition; user_gesture: cint; const popupFeatures: PCefPopupFeatures; windowInfo: PCefWindowInfo; var client: PCefClient; settings: PCefBrowserSettings; no_javascript_access: pcint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	target_url_str: ustring;
	target_frame_name_str: ustring;
	client_proxy: ICefClient;
	no_javascript_access_proxy: Boolean;
begin
	target_url_str := TWACef.ToString(target_url);
	target_frame_name_str := TWACef.ToString(target_frame_name);
	client_proxy := TCefClientOwn(TWACef.GetObject(client)) as ICefClient;
	no_javascript_access_proxy := no_javascript_access^  <> 0;
	Result := Ord(
		TCefLifeSpanHandlerOwn(TWACef.GetObject(self)).OnBeforePopup(
			TCefBrowserRef.UnWrap(browser),
			TCefFrameRef.UnWrap(frame),
			target_url_str,
			target_frame_name_str,
			target_disposition,
			user_gesture <> 0,
			popupFeatures^,
			windowInfo^,
			client_proxy,
			settings^,
			no_javascript_access_proxy
		)
	);
	client := TWACef.GetData(client_proxy);
	no_javascript_access^  := Ord(no_javascript_access_proxy);
end;

// Called after a new browser is created.
procedure cef_life_span_handler_on_after_created(self: PCefLifeSpanHandler; browser: PCefBrowser); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefLifeSpanHandlerOwn(TWACef.GetObject(self)).OnAfterCreated(
		TCefBrowserRef.UnWrap(browser)
	);
end;

// Called when a modal window is about to display and the modal loop should
// begin running. Return false (0) to use the default modal loop
// implementation or true (1) to use a custom implementation.
function cef_life_span_handler_run_modal(self: PCefLifeSpanHandler; browser: PCefBrowser): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefLifeSpanHandlerOwn(TWACef.GetObject(self)).RunModal(
			TCefBrowserRef.UnWrap(browser)
		)
	);
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
function cef_life_span_handler_do_close(self: PCefLifeSpanHandler; browser: PCefBrowser): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefLifeSpanHandlerOwn(TWACef.GetObject(self)).DoClose(
			TCefBrowserRef.UnWrap(browser)
		)
	);
end;

// Called just before a browser is destroyed. Release all references to the
// browser object and do not attempt to execute any functions on the browser
// object after this callback returns. If this is a modal window and a custom
// modal loop implementation was provided in run_modal() this callback should
// be used to exit the custom modal loop. See do_close() documentation for
// additional usage information.
procedure cef_life_span_handler_on_before_close(self: PCefLifeSpanHandler; browser: PCefBrowser); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefLifeSpanHandlerOwn(TWACef.GetObject(self)).OnBeforeClose(
		TCefBrowserRef.UnWrap(browser)
	);
end;

{Protected section}
function TCefLifeSpanHandlerOwn.OnBeforePopup(const aBrowser: ICefBrowser; const aFrame: ICefFrame; var aTargetUrl: ustring; const aTargetFrameName: ustring; aTargetDisposition: TCefWindowOpenDisposition; aUserGesture: Boolean; var aPopupFeatures: TCefPopupFeatures; var aWindowInfo: TCefWindowInfo; var aClient: ICefClient; var aSettings: TCefBrowserSettings; var aNoJavascriptAccess: Boolean): Boolean;
begin
	Result := false;
end;

procedure TCefLifeSpanHandlerOwn.OnAfterCreated(const aBrowser: ICefBrowser);
begin
end;

function TCefLifeSpanHandlerOwn.RunModal(const aBrowser: ICefBrowser): Boolean;
begin
	Result := false;
end;

function TCefLifeSpanHandlerOwn.DoClose(const aBrowser: ICefBrowser): Boolean;
begin
	Result := false;
end;

procedure TCefLifeSpanHandlerOwn.OnBeforeClose(const aBrowser: ICefBrowser);
begin
end;

{Public section}
constructor TCefLifeSpanHandlerOwn.Create;
begin
	inherited CreateData(SizeOf(TCefLifeSpanHandler));
	with PCefLifeSpanHandler(FData)^ do
	begin
		on_before_popup := {$IFDEF FPC}@{$ENDIF}cef_life_span_handler_on_before_popup;
		on_after_created := {$IFDEF FPC}@{$ENDIF}cef_life_span_handler_on_after_created;
		run_modal := {$IFDEF FPC}@{$ENDIF}cef_life_span_handler_run_modal;
		do_close := {$IFDEF FPC}@{$ENDIF}cef_life_span_handler_do_close;
		on_before_close := {$IFDEF FPC}@{$ENDIF}cef_life_span_handler_on_before_close;
	end;
end;

//..............................................................................TCefLoadHandlerOwn
// Called when the loading state has changed. This callback will be executed
// twice -- once when loading is initiated either programmatically or by user
// action, and once when loading is terminated due to completion, cancellation
// of failure.
procedure cef_load_handler_on_loading_state_change(self: PCefLoadHandler; browser: PCefBrowser; isLoading: cint; canGoBack: cint; canGoForward: cint); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefLoadHandlerOwn(TWACef.GetObject(self)).OnLoadingStateChange(
		TCefBrowserRef.UnWrap(browser),
		isLoading <> 0,
		canGoBack <> 0,
		canGoForward <> 0
	);
end;

// Called when the browser begins loading a frame. The |frame| value will
// never be NULL -- call the is_main() function to check if this frame is the
// main frame. Multiple frames may be loading at the same time. Sub-frames may
// start or continue loading after the main frame load has ended. This
// function may not be called for a particular frame if the load request for
// that frame fails. For notification of overall browser load status use
// OnLoadingStateChange instead.
procedure cef_load_handler_on_load_start(self: PCefLoadHandler; browser: PCefBrowser; frame: PCefFrame); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefLoadHandlerOwn(TWACef.GetObject(self)).OnLoadStart(
		TCefBrowserRef.UnWrap(browser),
		TCefFrameRef.UnWrap(frame)
	);
end;

// Called when the browser is done loading a frame. The |frame| value will
// never be NULL -- call the is_main() function to check if this frame is the
// main frame. Multiple frames may be loading at the same time. Sub-frames may
// start or continue loading after the main frame load has ended. This
// function will always be called for all frames irrespective of whether the
// request completes successfully.
procedure cef_load_handler_on_load_end(self: PCefLoadHandler; browser: PCefBrowser; frame: PCefFrame; httpStatusCode: cint); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefLoadHandlerOwn(TWACef.GetObject(self)).OnLoadEnd(
		TCefBrowserRef.UnWrap(browser),
		TCefFrameRef.UnWrap(frame),
		httpStatusCode
	);
end;

// Called when the resource load for a navigation fails or is canceled.
// |errorCode| is the error code number, |errorText| is the error text and
// |failedUrl| is the URL that failed to load. See net\base\net_error_list.h
// for complete descriptions of the error codes.
procedure cef_load_handler_on_load_error(self: PCefLoadHandler; browser: PCefBrowser; frame: PCefFrame; errorCode: TCefErrorcode; const errorText: PCefString; const failedUrl: PCefString); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	errorText_str: ustring;
	failedUrl_str: ustring;
begin
	errorText_str := TWACef.ToString(errorText);
	failedUrl_str := TWACef.ToString(failedUrl);
	TCefLoadHandlerOwn(TWACef.GetObject(self)).OnLoadError(
		TCefBrowserRef.UnWrap(browser),
		TCefFrameRef.UnWrap(frame),
		errorCode,
		errorText_str,
		failedUrl_str
	);
end;

{Protected section}
procedure TCefLoadHandlerOwn.OnLoadingStateChange(const aBrowser: ICefBrowser; aIsLoading: Boolean; aCanGoBack: Boolean; aCanGoForward: Boolean);
begin
end;

procedure TCefLoadHandlerOwn.OnLoadStart(const aBrowser: ICefBrowser; const aFrame: ICefFrame);
begin
end;

procedure TCefLoadHandlerOwn.OnLoadEnd(const aBrowser: ICefBrowser; const aFrame: ICefFrame; aHttpStatusCode: cint);
begin
end;

procedure TCefLoadHandlerOwn.OnLoadError(const aBrowser: ICefBrowser; const aFrame: ICefFrame; aErrorCode: TCefErrorcode; const aErrorText: ustring; const aFailedUrl: ustring);
begin
end;

{Public section}
constructor TCefLoadHandlerOwn.Create;
begin
	inherited CreateData(SizeOf(TCefLoadHandler));
	with PCefLoadHandler(FData)^ do
	begin
		on_loading_state_change := {$IFDEF FPC}@{$ENDIF}cef_load_handler_on_loading_state_change;
		on_load_start := {$IFDEF FPC}@{$ENDIF}cef_load_handler_on_load_start;
		on_load_end := {$IFDEF FPC}@{$ENDIF}cef_load_handler_on_load_end;
		on_load_error := {$IFDEF FPC}@{$ENDIF}cef_load_handler_on_load_error;
	end;
end;

//..............................................................................TCefMenuModelOwn
// Clears the menu. Returns true (1) on success.
function cef_menu_model_clear(self: PCefMenuModel): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefMenuModelOwn(TWACef.GetObject(self)).Clear
	);
end;

// Returns the number of items in this menu.
function cef_menu_model_get_count(self: PCefMenuModel): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := (
		TCefMenuModelOwn(TWACef.GetObject(self)).GetCount
	);
end;

//
// Add a separator to the menu. Returns true (1) on success.
function cef_menu_model_add_separator(self: PCefMenuModel): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefMenuModelOwn(TWACef.GetObject(self)).AddSeparator
	);
end;

//
// Add an item to the menu. Returns true (1) on success.
function cef_menu_model_add_item(self: PCefMenuModel; command_id: cint; const _label: PCefString): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	label_str: ustring;
begin
	label_str := TWACef.ToString(_label);
	Result := Ord(
		TCefMenuModelOwn(TWACef.GetObject(self)).AddItem(
			command_id,
			label_str
		)
	);
end;

//
// Add a check item to the menu. Returns true (1) on success.
function cef_menu_model_add_check_item(self: PCefMenuModel; command_id: cint; const _label: PCefString): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	label_str: ustring;
begin
	label_str := TWACef.ToString(_label);
	Result := Ord(
		TCefMenuModelOwn(TWACef.GetObject(self)).AddCheckItem(
			command_id,
			label_str
		)
	);
end;

//
// Add a radio item to the menu. Only a single item with the specified
// |group_id| can be checked at a time. Returns true (1) on success.
function cef_menu_model_add_radio_item(self: PCefMenuModel; command_id: cint; const _label: PCefString; group_id: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	label_str: ustring;
begin
	label_str := TWACef.ToString(_label);
	Result := Ord(
		TCefMenuModelOwn(TWACef.GetObject(self)).AddRadioItem(
			command_id,
			label_str,
			group_id
		)
	);
end;

//
// Add a sub-menu to the menu. The new sub-menu is returned.
function cef_menu_model_add_sub_menu(self: PCefMenuModel; command_id: cint; const _label: PCefString): PCefMenuModel; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	label_str: ustring;
begin
	label_str := TWACef.ToString(_label);
	Result := TWACef.GetData(
		TCefMenuModelOwn(TWACef.GetObject(self)).AddSubMenu(
			command_id,
			label_str
		)
	);
end;

//
// Insert a separator in the menu at the specified |index|. Returns true (1)
// on success.
function cef_menu_model_insert_separator_at(self: PCefMenuModel; index: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefMenuModelOwn(TWACef.GetObject(self)).InsertSeparatorAt(
			index
		)
	);
end;

//
// Insert an item in the menu at the specified |index|. Returns true (1) on
// success.
function cef_menu_model_insert_item_at(self: PCefMenuModel; index: cint; command_id: cint; const _label: PCefString): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	label_str: ustring;
begin
	label_str := TWACef.ToString(_label);
	Result := Ord(
		TCefMenuModelOwn(TWACef.GetObject(self)).InsertItemAt(
			index,
			command_id,
			label_str
		)
	);
end;

//
// Insert a check item in the menu at the specified |index|. Returns true (1)
// on success.
function cef_menu_model_insert_check_item_at(self: PCefMenuModel; index: cint; command_id: cint; const _label: PCefString): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	label_str: ustring;
begin
	label_str := TWACef.ToString(_label);
	Result := Ord(
		TCefMenuModelOwn(TWACef.GetObject(self)).InsertCheckItemAt(
			index,
			command_id,
			label_str
		)
	);
end;

//
// Insert a radio item in the menu at the specified |index|. Only a single
// item with the specified |group_id| can be checked at a time. Returns true
// (1) on success.
function cef_menu_model_insert_radio_item_at(self: PCefMenuModel; index: cint; command_id: cint; const _label: PCefString; group_id: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	label_str: ustring;
begin
	label_str := TWACef.ToString(_label);
	Result := Ord(
		TCefMenuModelOwn(TWACef.GetObject(self)).InsertRadioItemAt(
			index,
			command_id,
			label_str,
			group_id
		)
	);
end;

//
// Insert a sub-menu in the menu at the specified |index|. The new sub-menu is
// returned.
function cef_menu_model_insert_sub_menu_at(self: PCefMenuModel; index: cint; command_id: cint; const _label: PCefString): PCefMenuModel; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	label_str: ustring;
begin
	label_str := TWACef.ToString(_label);
	Result := TWACef.GetData(
		TCefMenuModelOwn(TWACef.GetObject(self)).InsertSubMenuAt(
			index,
			command_id,
			label_str
		)
	);
end;

// Removes the item with the specified |command_id|. Returns true (1) on
// success.
function cef_menu_model_remove(self: PCefMenuModel; command_id: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefMenuModelOwn(TWACef.GetObject(self)).Remove(
			command_id
		)
	);
end;

// Removes the item at the specified |index|. Returns true (1) on success.
function cef_menu_model_remove_at(self: PCefMenuModel; index: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefMenuModelOwn(TWACef.GetObject(self)).RemoveAt(
			index
		)
	);
end;

// Returns the index associated with the specified |command_id| or -1 if not
// found due to the command id not existing in the menu.
function cef_menu_model_get_index_of(self: PCefMenuModel; command_id: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := (
		TCefMenuModelOwn(TWACef.GetObject(self)).GetIndexOf(
			command_id
		)
	);
end;

// Returns the command id at the specified |index| or -1 if not found due to
// invalid range or the index being a separator.
function cef_menu_model_get_command_id_at(self: PCefMenuModel; index: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := (
		TCefMenuModelOwn(TWACef.GetObject(self)).GetCommandIdAt(
			index
		)
	);
end;

// Sets the command id at the specified |index|. Returns true (1) on success.
function cef_menu_model_set_command_id_at(self: PCefMenuModel; index: cint; command_id: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefMenuModelOwn(TWACef.GetObject(self)).SetCommandIdAt(
			index,
			command_id
		)
	);
end;

// Returns the label for the specified |command_id| or NULL if not found.
// The resulting string must be freed by calling cef_string_userfree_free().
function cef_menu_model_get_label(self: PCefMenuModel; command_id: cint): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.UserFreeString(
		TCefMenuModelOwn(TWACef.GetObject(self)).GetLabel(
			command_id
		)
	);
end;

// Returns the label at the specified |index| or NULL if not found due to
// invalid range or the index being a separator.
// The resulting string must be freed by calling cef_string_userfree_free().
function cef_menu_model_get_label_at(self: PCefMenuModel; index: cint): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.UserFreeString(
		TCefMenuModelOwn(TWACef.GetObject(self)).GetLabelAt(
			index
		)
	);
end;

// Sets the label for the specified |command_id|. Returns true (1) on success.
function cef_menu_model_set_label(self: PCefMenuModel; command_id: cint; const _label: PCefString): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	label_str: ustring;
begin
	label_str := TWACef.ToString(_label);
	Result := Ord(
		TCefMenuModelOwn(TWACef.GetObject(self)).SetLabel(
			command_id,
			label_str
		)
	);
end;

// Set the label at the specified |index|. Returns true (1) on success.
function cef_menu_model_set_label_at(self: PCefMenuModel; index: cint; const _label: PCefString): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	label_str: ustring;
begin
	label_str := TWACef.ToString(_label);
	Result := Ord(
		TCefMenuModelOwn(TWACef.GetObject(self)).SetLabelAt(
			index,
			label_str
		)
	);
end;

// Returns the item type for the specified |command_id|.
function cef_menu_model_get_type(self: PCefMenuModel; command_id: cint): TCefMenuItemType; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := (
		TCefMenuModelOwn(TWACef.GetObject(self)).GetType(
			command_id
		)
	);
end;

// Returns the item type at the specified |index|.
function cef_menu_model_get_type_at(self: PCefMenuModel; index: cint): TCefMenuItemType; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := (
		TCefMenuModelOwn(TWACef.GetObject(self)).GetTypeAt(
			index
		)
	);
end;

// Returns the group id for the specified |command_id| or -1 if invalid.
function cef_menu_model_get_group_id(self: PCefMenuModel; command_id: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := (
		TCefMenuModelOwn(TWACef.GetObject(self)).GetGroupId(
			command_id
		)
	);
end;

// Returns the group id at the specified |index| or -1 if invalid.
function cef_menu_model_get_group_id_at(self: PCefMenuModel; index: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := (
		TCefMenuModelOwn(TWACef.GetObject(self)).GetGroupIdAt(
			index
		)
	);
end;

// Sets the group id for the specified |command_id|. Returns true (1) on
// success.
function cef_menu_model_set_group_id(self: PCefMenuModel; command_id: cint; group_id: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefMenuModelOwn(TWACef.GetObject(self)).SetGroupId(
			command_id,
			group_id
		)
	);
end;

// Sets the group id at the specified |index|. Returns true (1) on success.
function cef_menu_model_set_group_id_at(self: PCefMenuModel; index: cint; group_id: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefMenuModelOwn(TWACef.GetObject(self)).SetGroupIdAt(
			index,
			group_id
		)
	);
end;

// Returns the submenu for the specified |command_id| or NULL if invalid.
function cef_menu_model_get_sub_menu(self: PCefMenuModel; command_id: cint): PCefMenuModel; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.GetData(
		TCefMenuModelOwn(TWACef.GetObject(self)).GetSubMenu(
			command_id
		)
	);
end;

// Returns the submenu at the specified |index| or NULL if invalid.
function cef_menu_model_get_sub_menu_at(self: PCefMenuModel; index: cint): PCefMenuModel; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.GetData(
		TCefMenuModelOwn(TWACef.GetObject(self)).GetSubMenuAt(
			index
		)
	);
end;

//
// Returns true (1) if the specified |command_id| is visible.
function cef_menu_model_is_visible(self: PCefMenuModel; command_id: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefMenuModelOwn(TWACef.GetObject(self)).IsVisible(
			command_id
		)
	);
end;

//
// Returns true (1) if the specified |index| is visible.
function cef_menu_model_is_visible_at(self: PCefMenuModel; index: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefMenuModelOwn(TWACef.GetObject(self)).IsVisibleAt(
			index
		)
	);
end;

//
// Change the visibility of the specified |command_id|. Returns true (1) on
// success.
function cef_menu_model_set_visible(self: PCefMenuModel; command_id: cint; visible: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefMenuModelOwn(TWACef.GetObject(self)).SetVisible(
			command_id,
			visible <> 0
		)
	);
end;

//
// Change the visibility at the specified |index|. Returns true (1) on
// success.
function cef_menu_model_set_visible_at(self: PCefMenuModel; index: cint; visible: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefMenuModelOwn(TWACef.GetObject(self)).SetVisibleAt(
			index,
			visible <> 0
		)
	);
end;

//
// Returns true (1) if the specified |command_id| is enabled.
function cef_menu_model_is_enabled(self: PCefMenuModel; command_id: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefMenuModelOwn(TWACef.GetObject(self)).IsEnabled(
			command_id
		)
	);
end;

//
// Returns true (1) if the specified |index| is enabled.
function cef_menu_model_is_enabled_at(self: PCefMenuModel; index: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefMenuModelOwn(TWACef.GetObject(self)).IsEnabledAt(
			index
		)
	);
end;

//
// Change the enabled status of the specified |command_id|. Returns true (1)
// on success.
function cef_menu_model_set_enabled(self: PCefMenuModel; command_id: cint; enabled: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefMenuModelOwn(TWACef.GetObject(self)).SetEnabled(
			command_id,
			enabled <> 0
		)
	);
end;

//
// Change the enabled status at the specified |index|. Returns true (1) on
// success.
function cef_menu_model_set_enabled_at(self: PCefMenuModel; index: cint; enabled: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefMenuModelOwn(TWACef.GetObject(self)).SetEnabledAt(
			index,
			enabled <> 0
		)
	);
end;

//
// Returns true (1) if the specified |command_id| is checked. Only applies to
// check and radio items.
function cef_menu_model_is_checked(self: PCefMenuModel; command_id: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefMenuModelOwn(TWACef.GetObject(self)).IsChecked(
			command_id
		)
	);
end;

//
// Returns true (1) if the specified |index| is checked. Only applies to check
// and radio items.
function cef_menu_model_is_checked_at(self: PCefMenuModel; index: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefMenuModelOwn(TWACef.GetObject(self)).IsCheckedAt(
			index
		)
	);
end;

//
// Check the specified |command_id|. Only applies to check and radio items.
// Returns true (1) on success.
function cef_menu_model_set_checked(self: PCefMenuModel; command_id: cint; checked: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefMenuModelOwn(TWACef.GetObject(self)).SetChecked(
			command_id,
			checked <> 0
		)
	);
end;

//
// Check the specified |index|. Only applies to check and radio items. Returns
// true (1) on success.
function cef_menu_model_set_checked_at(self: PCefMenuModel; index: cint; checked: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefMenuModelOwn(TWACef.GetObject(self)).SetCheckedAt(
			index,
			checked <> 0
		)
	);
end;

//
// Returns true (1) if the specified |command_id| has a keyboard accelerator
// assigned.
function cef_menu_model_has_accelerator(self: PCefMenuModel; command_id: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefMenuModelOwn(TWACef.GetObject(self)).HasAccelerator(
			command_id
		)
	);
end;

//
// Returns true (1) if the specified |index| has a keyboard accelerator
// assigned.
function cef_menu_model_has_accelerator_at(self: PCefMenuModel; index: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefMenuModelOwn(TWACef.GetObject(self)).HasAcceleratorAt(
			index
		)
	);
end;

//
// Set the keyboard accelerator for the specified |command_id|. |key_code| can
// be any virtual key or character value. Returns true (1) on success.
function cef_menu_model_set_accelerator(self: PCefMenuModel; command_id: cint; key_code: cint; shift_pressed: cint; ctrl_pressed: cint; alt_pressed: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefMenuModelOwn(TWACef.GetObject(self)).SetAccelerator(
			command_id,
			key_code,
			shift_pressed <> 0,
			ctrl_pressed <> 0,
			alt_pressed <> 0
		)
	);
end;

//
// Set the keyboard accelerator at the specified |index|. |key_code| can be
// any virtual key or character value. Returns true (1) on success.
function cef_menu_model_set_accelerator_at(self: PCefMenuModel; index: cint; key_code: cint; shift_pressed: cint; ctrl_pressed: cint; alt_pressed: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefMenuModelOwn(TWACef.GetObject(self)).SetAcceleratorAt(
			index,
			key_code,
			shift_pressed <> 0,
			ctrl_pressed <> 0,
			alt_pressed <> 0
		)
	);
end;

//
// Remove the keyboard accelerator for the specified |command_id|. Returns
// true (1) on success.
function cef_menu_model_remove_accelerator(self: PCefMenuModel; command_id: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefMenuModelOwn(TWACef.GetObject(self)).RemoveAccelerator(
			command_id
		)
	);
end;

//
// Remove the keyboard accelerator at the specified |index|. Returns true (1)
// on success.
function cef_menu_model_remove_accelerator_at(self: PCefMenuModel; index: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefMenuModelOwn(TWACef.GetObject(self)).RemoveAcceleratorAt(
			index
		)
	);
end;

//
// Retrieves the keyboard accelerator for the specified |command_id|. Returns
// true (1) on success.
function cef_menu_model_get_accelerator(self: PCefMenuModel; command_id: cint; key_code: pcint; shift_pressed: pcint; ctrl_pressed: pcint; alt_pressed: pcint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	shift_pressed_proxy: Boolean;
	ctrl_pressed_proxy: Boolean;
	alt_pressed_proxy: Boolean;
begin
	shift_pressed_proxy := shift_pressed^ <> 0;
	ctrl_pressed_proxy := ctrl_pressed^ <> 0;
	alt_pressed_proxy := alt_pressed^ <> 0;
	Result := Ord(
		TCefMenuModelOwn(TWACef.GetObject(self)).GetAccelerator(
			command_id,
			key_code^,
			shift_pressed_proxy,
			ctrl_pressed_proxy,
			alt_pressed_proxy
		)
	);
  shift_pressed^ := Ord(shift_pressed_proxy);
	ctrl_pressed^ := Ord(ctrl_pressed_proxy);
	alt_pressed^ := Ord(alt_pressed_proxy);
end;

//
// Retrieves the keyboard accelerator for the specified |index|. Returns true
// (1) on success.
function cef_menu_model_get_accelerator_at(self: PCefMenuModel; index: cint; key_code: pcint; shift_pressed: pcint; ctrl_pressed: pcint; alt_pressed: pcint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	shift_pressed_proxy: Boolean;
	ctrl_pressed_proxy: Boolean;
	alt_pressed_proxy: Boolean;
begin
  shift_pressed_proxy := shift_pressed^ <> 0;
	ctrl_pressed_proxy := ctrl_pressed^ <> 0;
	alt_pressed_proxy := alt_pressed^ <> 0;
	Result := Ord(
		TCefMenuModelOwn(TWACef.GetObject(self)).GetAcceleratorAt(
			index,
			key_code^,
			shift_pressed_proxy,
			ctrl_pressed_proxy,
			alt_pressed_proxy
		)
	);
  shift_pressed^ := Ord(shift_pressed_proxy);
	ctrl_pressed^ := Ord(ctrl_pressed_proxy);
	alt_pressed^ := Ord(alt_pressed_proxy);
end;

{Protected section}
function TCefMenuModelOwn.Clear: Boolean;
begin
	Result := false;
end;

function TCefMenuModelOwn.GetCount: cint;
begin
	Result := 0;
end;

function TCefMenuModelOwn.AddSeparator: Boolean;
begin
	Result := false;
end;

function TCefMenuModelOwn.AddItem(aCommandId: cint; const aLabel: ustring): Boolean;
begin
	Result := false;
end;

function TCefMenuModelOwn.AddCheckItem(aCommandId: cint; const aLabel: ustring): Boolean;
begin
	Result := false;
end;

function TCefMenuModelOwn.AddRadioItem(aCommandId: cint; const aLabel: ustring; aGroupId: cint): Boolean;
begin
	Result := false;
end;

function TCefMenuModelOwn.AddSubMenu(aCommandId: cint; const aLabel: ustring): ICefMenuModel;
begin
	Result := nil;
end;

function TCefMenuModelOwn.InsertSeparatorAt(aIndex: cint): Boolean;
begin
	Result := false;
end;

function TCefMenuModelOwn.InsertItemAt(aIndex: cint; aCommandId: cint; const aLabel: ustring): Boolean;
begin
	Result := false;
end;

function TCefMenuModelOwn.InsertCheckItemAt(aIndex: cint; aCommandId: cint; const aLabel: ustring): Boolean;
begin
	Result := false;
end;

function TCefMenuModelOwn.InsertRadioItemAt(aIndex: cint; aCommandId: cint; const aLabel: ustring; aGroupId: cint): Boolean;
begin
	Result := false;
end;

function TCefMenuModelOwn.InsertSubMenuAt(aIndex: cint; aCommandId: cint; const aLabel: ustring): ICefMenuModel;
begin
	Result := nil;
end;

function TCefMenuModelOwn.Remove(aCommandId: cint): Boolean;
begin
	Result := false;
end;

function TCefMenuModelOwn.RemoveAt(aIndex: cint): Boolean;
begin
	Result := false;
end;

function TCefMenuModelOwn.GetIndexOf(aCommandId: cint): cint;
begin
	Result := 0;
end;

function TCefMenuModelOwn.GetCommandIdAt(aIndex: cint): cint;
begin
	Result := 0;
end;

function TCefMenuModelOwn.SetCommandIdAt(aIndex: cint; aCommandId: cint): Boolean;
begin
	Result := false;
end;

function TCefMenuModelOwn.GetLabel(aCommandId: cint): ustring;
begin
end;

function TCefMenuModelOwn.GetLabelAt(aIndex: cint): ustring;
begin
end;

function TCefMenuModelOwn.SetLabel(aCommandId: cint; const aLabel: ustring): Boolean;
begin
	Result := false;
end;

function TCefMenuModelOwn.SetLabelAt(aIndex: cint; const aLabel: ustring): Boolean;
begin
	Result := false;
end;

function TCefMenuModelOwn.GetType(aCommandId: cint): TCefMenuItemType;
begin
end;

function TCefMenuModelOwn.GetTypeAt(aIndex: cint): TCefMenuItemType;
begin
end;

function TCefMenuModelOwn.GetGroupId(aCommandId: cint): cint;
begin
	Result := 0;
end;

function TCefMenuModelOwn.GetGroupIdAt(aIndex: cint): cint;
begin
	Result := 0;
end;

function TCefMenuModelOwn.SetGroupId(aCommandId: cint; aGroupId: cint): Boolean;
begin
	Result := false;
end;

function TCefMenuModelOwn.SetGroupIdAt(aIndex: cint; aGroupId: cint): Boolean;
begin
	Result := false;
end;

function TCefMenuModelOwn.GetSubMenu(aCommandId: cint): ICefMenuModel;
begin
	Result := nil;
end;

function TCefMenuModelOwn.GetSubMenuAt(aIndex: cint): ICefMenuModel;
begin
	Result := nil;
end;

function TCefMenuModelOwn.IsVisible(aCommandId: cint): Boolean;
begin
	Result := false;
end;

function TCefMenuModelOwn.IsVisibleAt(aIndex: cint): Boolean;
begin
	Result := false;
end;

function TCefMenuModelOwn.SetVisible(aCommandId: cint; aVisible: Boolean): Boolean;
begin
	Result := false;
end;

function TCefMenuModelOwn.SetVisibleAt(aIndex: cint; aVisible: Boolean): Boolean;
begin
	Result := false;
end;

function TCefMenuModelOwn.IsEnabled(aCommandId: cint): Boolean;
begin
	Result := false;
end;

function TCefMenuModelOwn.IsEnabledAt(aIndex: cint): Boolean;
begin
	Result := false;
end;

function TCefMenuModelOwn.SetEnabled(aCommandId: cint; aEnabled: Boolean): Boolean;
begin
	Result := false;
end;

function TCefMenuModelOwn.SetEnabledAt(aIndex: cint; aEnabled: Boolean): Boolean;
begin
	Result := false;
end;

function TCefMenuModelOwn.IsChecked(aCommandId: cint): Boolean;
begin
	Result := false;
end;

function TCefMenuModelOwn.IsCheckedAt(aIndex: cint): Boolean;
begin
	Result := false;
end;

function TCefMenuModelOwn.SetChecked(aCommandId: cint; aChecked: Boolean): Boolean;
begin
	Result := false;
end;

function TCefMenuModelOwn.SetCheckedAt(aIndex: cint; aChecked: Boolean): Boolean;
begin
	Result := false;
end;

function TCefMenuModelOwn.HasAccelerator(aCommandId: cint): Boolean;
begin
	Result := false;
end;

function TCefMenuModelOwn.HasAcceleratorAt(aIndex: cint): Boolean;
begin
	Result := false;
end;

function TCefMenuModelOwn.SetAccelerator(aCommandId: cint; aKeyCode: cint; aShiftPressed: Boolean; aCtrlPressed: Boolean; aAltPressed: Boolean): Boolean;
begin
	Result := false;
end;

function TCefMenuModelOwn.SetAcceleratorAt(aIndex: cint; aKeyCode: cint; aShiftPressed: Boolean; aCtrlPressed: Boolean; aAltPressed: Boolean): Boolean;
begin
	Result := false;
end;

function TCefMenuModelOwn.RemoveAccelerator(aCommandId: cint): Boolean;
begin
	Result := false;
end;

function TCefMenuModelOwn.RemoveAcceleratorAt(aIndex: cint): Boolean;
begin
	Result := false;
end;

function TCefMenuModelOwn.GetAccelerator(aCommandId: cint; out aKeyCode: cint; out aShiftPressed: Boolean; out aCtrlPressed: Boolean; out aAltPressed: Boolean): Boolean;
begin
	Result := false;
end;

function TCefMenuModelOwn.GetAcceleratorAt(aIndex: cint; out aKeyCode: cint; out aShiftPressed: Boolean; out aCtrlPressed: Boolean; out aAltPressed: Boolean): Boolean;
begin
	Result := false;
end;

{Public section}
constructor TCefMenuModelOwn.Create;
begin
	inherited CreateData(SizeOf(TCefMenuModel));
	with PCefMenuModel(FData)^ do
	begin
		clear := {$IFDEF FPC}@{$ENDIF}cef_menu_model_clear;
		get_count := {$IFDEF FPC}@{$ENDIF}cef_menu_model_get_count;
		add_separator := {$IFDEF FPC}@{$ENDIF}cef_menu_model_add_separator;
		add_item := {$IFDEF FPC}@{$ENDIF}cef_menu_model_add_item;
		add_check_item := {$IFDEF FPC}@{$ENDIF}cef_menu_model_add_check_item;
		add_radio_item := {$IFDEF FPC}@{$ENDIF}cef_menu_model_add_radio_item;
		add_sub_menu := {$IFDEF FPC}@{$ENDIF}cef_menu_model_add_sub_menu;
		insert_separator_at := {$IFDEF FPC}@{$ENDIF}cef_menu_model_insert_separator_at;
		insert_item_at := {$IFDEF FPC}@{$ENDIF}cef_menu_model_insert_item_at;
		insert_check_item_at := {$IFDEF FPC}@{$ENDIF}cef_menu_model_insert_check_item_at;
		insert_radio_item_at := {$IFDEF FPC}@{$ENDIF}cef_menu_model_insert_radio_item_at;
		insert_sub_menu_at := {$IFDEF FPC}@{$ENDIF}cef_menu_model_insert_sub_menu_at;
		remove := {$IFDEF FPC}@{$ENDIF}cef_menu_model_remove;
		remove_at := {$IFDEF FPC}@{$ENDIF}cef_menu_model_remove_at;
		get_index_of := {$IFDEF FPC}@{$ENDIF}cef_menu_model_get_index_of;
		get_command_id_at := {$IFDEF FPC}@{$ENDIF}cef_menu_model_get_command_id_at;
		set_command_id_at := {$IFDEF FPC}@{$ENDIF}cef_menu_model_set_command_id_at;
		get_label := {$IFDEF FPC}@{$ENDIF}cef_menu_model_get_label;
		get_label_at := {$IFDEF FPC}@{$ENDIF}cef_menu_model_get_label_at;
		set_label := {$IFDEF FPC}@{$ENDIF}cef_menu_model_set_label;
		set_label_at := {$IFDEF FPC}@{$ENDIF}cef_menu_model_set_label_at;
		get_type := {$IFDEF FPC}@{$ENDIF}cef_menu_model_get_type;
		get_type_at := {$IFDEF FPC}@{$ENDIF}cef_menu_model_get_type_at;
		get_group_id := {$IFDEF FPC}@{$ENDIF}cef_menu_model_get_group_id;
		get_group_id_at := {$IFDEF FPC}@{$ENDIF}cef_menu_model_get_group_id_at;
		set_group_id := {$IFDEF FPC}@{$ENDIF}cef_menu_model_set_group_id;
		set_group_id_at := {$IFDEF FPC}@{$ENDIF}cef_menu_model_set_group_id_at;
		get_sub_menu := {$IFDEF FPC}@{$ENDIF}cef_menu_model_get_sub_menu;
		get_sub_menu_at := {$IFDEF FPC}@{$ENDIF}cef_menu_model_get_sub_menu_at;
		is_visible := {$IFDEF FPC}@{$ENDIF}cef_menu_model_is_visible;
		is_visible_at := {$IFDEF FPC}@{$ENDIF}cef_menu_model_is_visible_at;
		set_visible := {$IFDEF FPC}@{$ENDIF}cef_menu_model_set_visible;
		set_visible_at := {$IFDEF FPC}@{$ENDIF}cef_menu_model_set_visible_at;
		is_enabled := {$IFDEF FPC}@{$ENDIF}cef_menu_model_is_enabled;
		is_enabled_at := {$IFDEF FPC}@{$ENDIF}cef_menu_model_is_enabled_at;
		set_enabled := {$IFDEF FPC}@{$ENDIF}cef_menu_model_set_enabled;
		set_enabled_at := {$IFDEF FPC}@{$ENDIF}cef_menu_model_set_enabled_at;
		is_checked := {$IFDEF FPC}@{$ENDIF}cef_menu_model_is_checked;
		is_checked_at := {$IFDEF FPC}@{$ENDIF}cef_menu_model_is_checked_at;
		set_checked := {$IFDEF FPC}@{$ENDIF}cef_menu_model_set_checked;
		set_checked_at := {$IFDEF FPC}@{$ENDIF}cef_menu_model_set_checked_at;
		has_accelerator := {$IFDEF FPC}@{$ENDIF}cef_menu_model_has_accelerator;
		has_accelerator_at := {$IFDEF FPC}@{$ENDIF}cef_menu_model_has_accelerator_at;
		set_accelerator := {$IFDEF FPC}@{$ENDIF}cef_menu_model_set_accelerator;
		set_accelerator_at := {$IFDEF FPC}@{$ENDIF}cef_menu_model_set_accelerator_at;
		remove_accelerator := {$IFDEF FPC}@{$ENDIF}cef_menu_model_remove_accelerator;
		remove_accelerator_at := {$IFDEF FPC}@{$ENDIF}cef_menu_model_remove_accelerator_at;
		get_accelerator := {$IFDEF FPC}@{$ENDIF}cef_menu_model_get_accelerator;
		get_accelerator_at := {$IFDEF FPC}@{$ENDIF}cef_menu_model_get_accelerator_at;
	end;
end;

//..............................................................................TCefNavigationEntryOwn
// Returns true (1) if this object is valid. Do not call any other functions
// if this function returns false (0).
function cef_navigation_entry_is_valid(self: PCefNavigationEntry): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefNavigationEntryOwn(TWACef.GetObject(self)).IsValid
	);
end;

// Returns the actual URL of the page. For some pages this may be data: URL or
// similar. Use get_display_url() to return a display-friendly version.
// The resulting string must be freed by calling cef_string_userfree_free().
function cef_navigation_entry_get_url(self: PCefNavigationEntry): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.UserFreeString(
		TCefNavigationEntryOwn(TWACef.GetObject(self)).GetUrl
	);
end;

// Returns a display-friendly version of the URL.
// The resulting string must be freed by calling cef_string_userfree_free().
function cef_navigation_entry_get_display_url(self: PCefNavigationEntry): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.UserFreeString(
		TCefNavigationEntryOwn(TWACef.GetObject(self)).GetDisplayUrl
	);
end;

// Returns the original URL that was entered by the user before any redirects.
// The resulting string must be freed by calling cef_string_userfree_free().
function cef_navigation_entry_get_original_url(self: PCefNavigationEntry): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.UserFreeString(
		TCefNavigationEntryOwn(TWACef.GetObject(self)).GetOriginalUrl
	);
end;

// Returns the title set by the page. This value may be NULL.
// The resulting string must be freed by calling cef_string_userfree_free().
function cef_navigation_entry_get_title(self: PCefNavigationEntry): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.UserFreeString(
		TCefNavigationEntryOwn(TWACef.GetObject(self)).GetTitle
	);
end;

// Returns the transition type which indicates what the user did to move to
// this page from the previous page.
function cef_navigation_entry_get_transition_type(self: PCefNavigationEntry): TCefTransitionType; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := (
		TCefNavigationEntryOwn(TWACef.GetObject(self)).GetTransitionType
	);
end;

// Returns true (1) if this navigation includes post data.
function cef_navigation_entry_has_post_data(self: PCefNavigationEntry): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefNavigationEntryOwn(TWACef.GetObject(self)).HasPostData
	);
end;

// Returns the name of the sub-frame that navigated or an NULL value if the
// main frame navigated.
// The resulting string must be freed by calling cef_string_userfree_free().
function cef_navigation_entry_get_frame_name(self: PCefNavigationEntry): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.UserFreeString(
		TCefNavigationEntryOwn(TWACef.GetObject(self)).GetFrameName
	);
end;

// Returns the time for the last known successful navigation completion. A
// navigation may be completed more than once if the page is reloaded. May be
// 0 if the navigation has not yet completed.
function cef_navigation_entry_get_completion_time(self: PCefNavigationEntry): TCefTime; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := (
		TCefNavigationEntryOwn(TWACef.GetObject(self)).GetCompletionTime
	);
end;

// Returns the HTTP status code for the last known successful navigation
// response. May be 0 if the response has not yet been received or if the
// navigation has not yet completed.
function cef_navigation_entry_get_http_status_code(self: PCefNavigationEntry): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := (
		TCefNavigationEntryOwn(TWACef.GetObject(self)).GetHttpStatusCode
	);
end;

{Protected section}
function TCefNavigationEntryOwn.IsValid: Boolean;
begin
	Result := false;
end;

function TCefNavigationEntryOwn.GetUrl: ustring;
begin
end;

function TCefNavigationEntryOwn.GetDisplayUrl: ustring;
begin
end;

function TCefNavigationEntryOwn.GetOriginalUrl: ustring;
begin
end;

function TCefNavigationEntryOwn.GetTitle: ustring;
begin
end;

function TCefNavigationEntryOwn.GetTransitionType: TCefTransitionType;
begin
end;

function TCefNavigationEntryOwn.HasPostData: Boolean;
begin
	Result := false;
end;

function TCefNavigationEntryOwn.GetFrameName: ustring;
begin
end;

function TCefNavigationEntryOwn.GetCompletionTime: TCefTime;
begin
end;

function TCefNavigationEntryOwn.GetHttpStatusCode: cint;
begin
	Result := 0;
end;

{Public section}
constructor TCefNavigationEntryOwn.Create;
begin
	inherited CreateData(SizeOf(TCefNavigationEntry));
	with PCefNavigationEntry(FData)^ do
	begin
		is_valid := {$IFDEF FPC}@{$ENDIF}cef_navigation_entry_is_valid;
		get_url := {$IFDEF FPC}@{$ENDIF}cef_navigation_entry_get_url;
		get_display_url := {$IFDEF FPC}@{$ENDIF}cef_navigation_entry_get_display_url;
		get_original_url := {$IFDEF FPC}@{$ENDIF}cef_navigation_entry_get_original_url;
		get_title := {$IFDEF FPC}@{$ENDIF}cef_navigation_entry_get_title;
		get_transition_type := {$IFDEF FPC}@{$ENDIF}cef_navigation_entry_get_transition_type;
		has_post_data := {$IFDEF FPC}@{$ENDIF}cef_navigation_entry_has_post_data;
		get_frame_name := {$IFDEF FPC}@{$ENDIF}cef_navigation_entry_get_frame_name;
		get_completion_time := {$IFDEF FPC}@{$ENDIF}cef_navigation_entry_get_completion_time;
		get_http_status_code := {$IFDEF FPC}@{$ENDIF}cef_navigation_entry_get_http_status_code;
	end;
end;




//..............................................................................TCefPrintDialogCallbackOwn
// Continue printing with the specified |settings|.
procedure cef_print_dialog_callback_cont(self: PCefPrintDialogCallback; settings: PCefPrintSettings); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefPrintDialogCallbackOwn(TWACef.GetObject(self)).Cont(
		TCefPrintSettingsRef.UnWrap(settings)
	);
end;

// Cancel the printing.
procedure cef_print_dialog_callback_cancel(self: PCefPrintDialogCallback); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefPrintDialogCallbackOwn(TWACef.GetObject(self)).Cancel;
end;

{Protected section}
procedure TCefPrintDialogCallbackOwn.Cont(const aSettings: ICefPrintSettings);
begin
end;

procedure TCefPrintDialogCallbackOwn.Cancel;
begin
end;

{Public section}
constructor TCefPrintDialogCallbackOwn.Create;
begin
	inherited CreateData(SizeOf(TCefPrintDialogCallback));
	with PCefPrintDialogCallback(FData)^ do
	begin
		cont := {$IFDEF FPC}@{$ENDIF}cef_print_dialog_callback_cont;
		cancel := {$IFDEF FPC}@{$ENDIF}cef_print_dialog_callback_cancel;
	end;
end;
//..............................................................................TCefPrintJobCallbackOwn
// Indicate completion of the print job.
procedure cef_print_job_callback_cont(self: PCefPrintJobCallback); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefPrintJobCallbackOwn(TWACef.GetObject(self)).Cont;
end;

{Protected section}
procedure TCefPrintJobCallbackOwn.Cont;
begin
end;

{Public section}
constructor TCefPrintJobCallbackOwn.Create;
begin
	inherited CreateData(SizeOf(TCefPrintJobCallback));
	with PCefPrintJobCallback(FData)^ do
	begin
		cont := {$IFDEF FPC}@{$ENDIF}cef_print_job_callback_cont;
	end;
end;
//..............................................................................TCefPrintHandlerOwn
// Synchronize |settings| with client state. If |get_defaults| is true (1)
// then populate |settings| with the default print settings. Do not keep a
// reference to |settings| outside of this callback.
procedure cef_print_handler_on_print_settings(self: PCefPrintHandler; settings: PCefPrintSettings; get_defaults: cint); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefPrintHandlerOwn(TWACef.GetObject(self)).OnPrintSettings(
		TCefPrintSettingsRef.UnWrap(settings),
		get_defaults <> 0
	);
end;

// Show the print dialog. Execute |callback| once the dialog is dismissed.
// Return true (1) if the dialog will be displayed or false (0) to cancel the
// printing immediately.
function cef_print_handler_on_print_dialog(self: PCefPrintHandler; has_selection: cint; callback: PCefPrintDialogCallback): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefPrintHandlerOwn(TWACef.GetObject(self)).OnPrintDialog(
			has_selection <> 0,
			TCefPrintDialogCallbackRef.UnWrap(callback)
		)
	);
end;

// Send the print job to the printer. Execute |callback| once the job is
// completed. Return true (1) if the job will proceed or false (0) to cancel
// the job immediately.
function cef_print_handler_on_print_job(self: PCefPrintHandler; const document_name: PCefString; const pdf_file_path: PCefString; callback: PCefPrintJobCallback): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	document_name_str: ustring;
	pdf_file_path_str: ustring;
begin
	document_name_str := TWACef.ToString(document_name);
	pdf_file_path_str := TWACef.ToString(pdf_file_path);
	Result := Ord(
		TCefPrintHandlerOwn(TWACef.GetObject(self)).OnPrintJob(
			document_name_str,
			pdf_file_path_str,
			TCefPrintJobCallbackRef.UnWrap(callback)
		)
	);
end;

// Reset client state related to printing.
procedure cef_print_handler_on_print_reset(self: PCefPrintHandler); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefPrintHandlerOwn(TWACef.GetObject(self)).OnPrintReset;
end;

{Protected section}
procedure TCefPrintHandlerOwn.OnPrintSettings(const aSettings: ICefPrintSettings; aGetDefaults: Boolean);
begin
end;

function TCefPrintHandlerOwn.OnPrintDialog(aHasSelection: Boolean; const aCallback: ICefPrintDialogCallback): Boolean;
begin
	Result := false;
end;

function TCefPrintHandlerOwn.OnPrintJob(const aDocumentName: ustring; const aPdfFilePath: ustring; const aCallback: ICefPrintJobCallback): Boolean;
begin
	Result := false;
end;

procedure TCefPrintHandlerOwn.OnPrintReset;
begin
end;

{Public section}
constructor TCefPrintHandlerOwn.Create;
begin
	inherited CreateData(SizeOf(TCefPrintHandler));
	with PCefPrintHandler(FData)^ do
	begin
		on_print_settings := {$IFDEF FPC}@{$ENDIF}cef_print_handler_on_print_settings;
		on_print_dialog := {$IFDEF FPC}@{$ENDIF}cef_print_handler_on_print_dialog;
		on_print_job := {$IFDEF FPC}@{$ENDIF}cef_print_handler_on_print_job;
		on_print_reset := {$IFDEF FPC}@{$ENDIF}cef_print_handler_on_print_reset;
	end;
end;

//..............................................................................TCefPrintSettingsOwn
// Returns true (1) if this object is valid. Do not call any other functions
// if this function returns false (0).
function cef_print_settings_is_valid(self: PCefPrintSettings): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefPrintSettingsOwn(TWACef.GetObject(self)).IsValid
	);
end;

// Returns true (1) if the values of this object are read-only. Some APIs may
// expose read-only objects.
function cef_print_settings_is_read_only(self: PCefPrintSettings): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefPrintSettingsOwn(TWACef.GetObject(self)).IsReadOnly
	);
end;

// Returns a writable copy of this object.
function cef_print_settings_copy(self: PCefPrintSettings): PCefPrintSettings; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.GetData(
		TCefPrintSettingsOwn(TWACef.GetObject(self)).Copy
	);
end;

// Set the page orientation.
procedure cef_print_settings_set_orientation(self: PCefPrintSettings; landscape: cint); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefPrintSettingsOwn(TWACef.GetObject(self)).SetOrientation(
		landscape <> 0
	);
end;

// Returns true (1) if the orientation is landscape.
function cef_print_settings_is_landscape(self: PCefPrintSettings): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefPrintSettingsOwn(TWACef.GetObject(self)).IsLandscape
	);
end;

// Set the printer printable area in device units. Some platforms already
// provide flipped area. Set |landscape_needs_flip| to false (0) on those
// platforms to avoid double flipping.
procedure cef_print_settings_set_printer_printable_area(self: PCefPrintSettings; const physical_size_device_units: PCefSize; const printable_area_device_units: PCefRect; landscape_needs_flip: cint); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	landscape_needs_flip_proxy: Boolean;
begin
	landscape_needs_flip_proxy := landscape_needs_flip <> 0;
	TCefPrintSettingsOwn(TWACef.GetObject(self)).SetPrinterPrintableArea(
		physical_size_device_units^,
		printable_area_device_units^,
		landscape_needs_flip_proxy
	);
	landscape_needs_flip := Ord(landscape_needs_flip_proxy);
end;

// Set the device name.
procedure cef_print_settings_set_device_name(self: PCefPrintSettings; const name: PCefString); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	name_str: ustring;
begin
	name_str := TWACef.ToString(name);
	TCefPrintSettingsOwn(TWACef.GetObject(self)).SetDeviceName(
		name_str
	);
end;

// Get the device name.
// The resulting string must be freed by calling cef_string_userfree_free().
function cef_print_settings_get_device_name(self: PCefPrintSettings): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.UserFreeString(
		TCefPrintSettingsOwn(TWACef.GetObject(self)).GetDeviceName
	);
end;

// Set the DPI (dots per inch).
procedure cef_print_settings_set_dpi(self: PCefPrintSettings; dpi: cint); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefPrintSettingsOwn(TWACef.GetObject(self)).SetDpi(
		dpi
	);
end;

// Get the DPI (dots per inch).
function cef_print_settings_get_dpi(self: PCefPrintSettings): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := (
		TCefPrintSettingsOwn(TWACef.GetObject(self)).GetDpi
	);
end;

// Set the page ranges.
procedure cef_print_settings_set_page_ranges(self: PCefPrintSettings; rangesCount: csize_t; ranges: PCefPageRangeArray); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefPrintSettingsOwn(TWACef.GetObject(self)).SetPageRanges(
		rangesCount,
		ranges^
	);
end;

// Returns the number of page ranges that currently exist.
function cef_print_settings_get_page_ranges_count(self: PCefPrintSettings): csize_t; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := (
		TCefPrintSettingsOwn(TWACef.GetObject(self)).GetPageRangesCount
	);
end;

// Retrieve the page ranges.
procedure cef_print_settings_get_page_ranges(self: PCefPrintSettings; rangesCount: pcsize_t; ranges: PCefPageRangeArray); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	rangesCount_proxy: csize_t;
begin
	TCefPrintSettingsOwn(TWACef.GetObject(self)).GetPageRanges(
		rangesCount_proxy,
		ranges^
	);
end;

// Set whether only the selection will be printed.
procedure cef_print_settings_set_selection_only(self: PCefPrintSettings; selection_only: cint); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefPrintSettingsOwn(TWACef.GetObject(self)).SetSelectionOnly(
		selection_only <> 0
	);
end;

// Returns true (1) if only the selection will be printed.
function cef_print_settings_is_selection_only(self: PCefPrintSettings): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefPrintSettingsOwn(TWACef.GetObject(self)).IsSelectionOnly
	);
end;

// Set whether pages will be collated.
procedure cef_print_settings_set_collate(self: PCefPrintSettings; collate: cint); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefPrintSettingsOwn(TWACef.GetObject(self)).SetCollate(
		collate <> 0
	);
end;

// Returns true (1) if pages will be collated.
function cef_print_settings_will_collate(self: PCefPrintSettings): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefPrintSettingsOwn(TWACef.GetObject(self)).WillCollate
	);
end;

// Set the color model.
procedure cef_print_settings_set_color_model(self: PCefPrintSettings; model: TCefColorModel); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefPrintSettingsOwn(TWACef.GetObject(self)).SetColorModel(
		model
	);
end;

// Get the color model.
function cef_print_settings_get_color_model(self: PCefPrintSettings): TCefColorModel; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := (
		TCefPrintSettingsOwn(TWACef.GetObject(self)).GetColorModel
	);
end;

// Set the number of copies.
procedure cef_print_settings_set_copies(self: PCefPrintSettings; copies: cint); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefPrintSettingsOwn(TWACef.GetObject(self)).SetCopies(
		copies
	);
end;

// Get the number of copies.
function cef_print_settings_get_copies(self: PCefPrintSettings): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := (
		TCefPrintSettingsOwn(TWACef.GetObject(self)).GetCopies
	);
end;

// Set the duplex mode.
procedure cef_print_settings_set_duplex_mode(self: PCefPrintSettings; mode: TCefDuplexMode); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefPrintSettingsOwn(TWACef.GetObject(self)).SetDuplexMode(
		mode
	);
end;

// Get the duplex mode.
function cef_print_settings_get_duplex_mode(self: PCefPrintSettings): TCefDuplexMode; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := (
		TCefPrintSettingsOwn(TWACef.GetObject(self)).GetDuplexMode
	);
end;

{Protected section}
function TCefPrintSettingsOwn.IsValid: Boolean;
begin
	Result := false;
end;

function TCefPrintSettingsOwn.IsReadOnly: Boolean;
begin
	Result := false;
end;

function TCefPrintSettingsOwn.Copy: ICefPrintSettings;
begin
	Result := nil;
end;

procedure TCefPrintSettingsOwn.SetOrientation(aLandscape: Boolean);
begin
end;

function TCefPrintSettingsOwn.IsLandscape: Boolean;
begin
	Result := false;
end;

procedure TCefPrintSettingsOwn.SetPrinterPrintableArea(const aPhysicalSizeDeviceUnits: TCefSize; var aPrintableAreaDeviceUnits: TCefRect; var aLandscapeNeedsFlip: Boolean);
begin
end;

procedure TCefPrintSettingsOwn.SetDeviceName(const aName: ustring);
begin
end;

function TCefPrintSettingsOwn.GetDeviceName: ustring;
begin
end;

procedure TCefPrintSettingsOwn.SetDpi(aDpi: cint);
begin
end;

function TCefPrintSettingsOwn.GetDpi: cint;
begin
	Result := 0;
end;

procedure TCefPrintSettingsOwn.SetPageRanges(aRangesCount: csize_t; aRanges: TCefPageRangeArray);
begin
end;

function TCefPrintSettingsOwn.GetPageRangesCount: csize_t;
begin
end;

procedure TCefPrintSettingsOwn.GetPageRanges(var aRangesCount: csize_t; const aRanges: TCefPageRangeArray);
begin
end;

procedure TCefPrintSettingsOwn.SetSelectionOnly(aSelectionOnly: Boolean);
begin
end;

function TCefPrintSettingsOwn.IsSelectionOnly: Boolean;
begin
	Result := false;
end;

procedure TCefPrintSettingsOwn.SetCollate(aCollate: Boolean);
begin
end;

function TCefPrintSettingsOwn.WillCollate: Boolean;
begin
	Result := false;
end;

procedure TCefPrintSettingsOwn.SetColorModel(aModel: TCefColorModel);
begin
end;

function TCefPrintSettingsOwn.GetColorModel: TCefColorModel;
begin
end;

procedure TCefPrintSettingsOwn.SetCopies(aCopies: cint);
begin
end;

function TCefPrintSettingsOwn.GetCopies: cint;
begin
	Result := 0;
end;

procedure TCefPrintSettingsOwn.SetDuplexMode(aMode: TCefDuplexMode);
begin
end;

function TCefPrintSettingsOwn.GetDuplexMode: TCefDuplexMode;
begin
end;

{Public section}
constructor TCefPrintSettingsOwn.Create;
begin
	inherited CreateData(SizeOf(TCefPrintSettings));
	with PCefPrintSettings(FData)^ do
	begin
		is_valid := {$IFDEF FPC}@{$ENDIF}cef_print_settings_is_valid;
		is_read_only := {$IFDEF FPC}@{$ENDIF}cef_print_settings_is_read_only;
		copy := {$IFDEF FPC}@{$ENDIF}cef_print_settings_copy;
		set_orientation := {$IFDEF FPC}@{$ENDIF}cef_print_settings_set_orientation;
		is_landscape := {$IFDEF FPC}@{$ENDIF}cef_print_settings_is_landscape;
		set_printer_printable_area := {$IFDEF FPC}@{$ENDIF}cef_print_settings_set_printer_printable_area;
		set_device_name := {$IFDEF FPC}@{$ENDIF}cef_print_settings_set_device_name;
		get_device_name := {$IFDEF FPC}@{$ENDIF}cef_print_settings_get_device_name;
		set_dpi := {$IFDEF FPC}@{$ENDIF}cef_print_settings_set_dpi;
		get_dpi := {$IFDEF FPC}@{$ENDIF}cef_print_settings_get_dpi;
		set_page_ranges := {$IFDEF FPC}@{$ENDIF}cef_print_settings_set_page_ranges;
		get_page_ranges_count := {$IFDEF FPC}@{$ENDIF}cef_print_settings_get_page_ranges_count;
		get_page_ranges := {$IFDEF FPC}@{$ENDIF}cef_print_settings_get_page_ranges;
		set_selection_only := {$IFDEF FPC}@{$ENDIF}cef_print_settings_set_selection_only;
		is_selection_only := {$IFDEF FPC}@{$ENDIF}cef_print_settings_is_selection_only;
		set_collate := {$IFDEF FPC}@{$ENDIF}cef_print_settings_set_collate;
		will_collate := {$IFDEF FPC}@{$ENDIF}cef_print_settings_will_collate;
		set_color_model := {$IFDEF FPC}@{$ENDIF}cef_print_settings_set_color_model;
		get_color_model := {$IFDEF FPC}@{$ENDIF}cef_print_settings_get_color_model;
		set_copies := {$IFDEF FPC}@{$ENDIF}cef_print_settings_set_copies;
		get_copies := {$IFDEF FPC}@{$ENDIF}cef_print_settings_get_copies;
		set_duplex_mode := {$IFDEF FPC}@{$ENDIF}cef_print_settings_set_duplex_mode;
		get_duplex_mode := {$IFDEF FPC}@{$ENDIF}cef_print_settings_get_duplex_mode;
	end;
end;

//..............................................................................TCefProcessMessageOwn
// Returns true (1) if this object is valid. Do not call any other functions
// if this function returns false (0).
function cef_process_message_is_valid(self: PCefProcessMessage): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefProcessMessageOwn(TWACef.GetObject(self)).IsValid
	);
end;

// Returns true (1) if the values of this object are read-only. Some APIs may
// expose read-only objects.
function cef_process_message_is_read_only(self: PCefProcessMessage): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefProcessMessageOwn(TWACef.GetObject(self)).IsReadOnly
	);
end;

// Returns a writable copy of this object.
function cef_process_message_copy(self: PCefProcessMessage): PCefProcessMessage; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.GetData(
		TCefProcessMessageOwn(TWACef.GetObject(self)).Copy
	);
end;

// Returns the message name.
// The resulting string must be freed by calling cef_string_userfree_free().
function cef_process_message_get_name(self: PCefProcessMessage): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.UserFreeString(
		TCefProcessMessageOwn(TWACef.GetObject(self)).GetName
	);
end;

// Returns the list of arguments.
function cef_process_message_get_argument_list(self: PCefProcessMessage): PCefListValue; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.GetData(
		TCefProcessMessageOwn(TWACef.GetObject(self)).GetArgumentList
	);
end;

{Protected section}
function TCefProcessMessageOwn.IsValid: Boolean;
begin
	Result := false;
end;

function TCefProcessMessageOwn.IsReadOnly: Boolean;
begin
	Result := false;
end;

function TCefProcessMessageOwn.Copy: ICefProcessMessage;
begin
	Result := nil;
end;

function TCefProcessMessageOwn.GetName: ustring;
begin
end;

function TCefProcessMessageOwn.GetArgumentList: ICefListValue;
begin
	Result := nil;
end;

{Public section}
constructor TCefProcessMessageOwn.Create;
begin
	inherited CreateData(SizeOf(TCefProcessMessage));
	with PCefProcessMessage(FData)^ do
	begin
		is_valid := {$IFDEF FPC}@{$ENDIF}cef_process_message_is_valid;
		is_read_only := {$IFDEF FPC}@{$ENDIF}cef_process_message_is_read_only;
		copy := {$IFDEF FPC}@{$ENDIF}cef_process_message_copy;
		get_name := {$IFDEF FPC}@{$ENDIF}cef_process_message_get_name;
		get_argument_list := {$IFDEF FPC}@{$ENDIF}cef_process_message_get_argument_list;
	end;
end;


//..............................................................................TCefRenderHandlerOwn
// Called to retrieve the root window rectangle in screen coordinates. Return
// true (1) if the rectangle was provided.
function cef_render_handler_get_root_screen_rect(self: PCefRenderHandler; browser: PCefBrowser; rect: PCefRect): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefRenderHandlerOwn(TWACef.GetObject(self)).GetRootScreenRect(
			TCefBrowserRef.UnWrap(browser),
			rect^
		)
	);
end;

// Called to retrieve the view rectangle which is relative to screen
// coordinates. Return true (1) if the rectangle was provided.
function cef_render_handler_get_view_rect(self: PCefRenderHandler; browser: PCefBrowser; rect: PCefRect): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefRenderHandlerOwn(TWACef.GetObject(self)).GetViewRect(
			TCefBrowserRef.UnWrap(browser),
			rect^
		)
	);
end;

// Called to retrieve the translation from view coordinates to actual screen
// coordinates. Return true (1) if the screen coordinates were provided.
function cef_render_handler_get_screen_point(self: PCefRenderHandler; browser: PCefBrowser; viewX: cint; viewY: cint; screenX: pcint; screenY: pcint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	screenX_proxy: cint;
	screenY_proxy: cint;
begin
	Result := Ord(
		TCefRenderHandlerOwn(TWACef.GetObject(self)).GetScreenPoint(
			TCefBrowserRef.UnWrap(browser),
			viewX,
			viewY,
			screenX_proxy,
			screenY_proxy
		)
	);
end;

// Called to allow the client to fill in the CefScreenInfo object with
// appropriate values. Return true (1) if the |screen_info| structure has been
// modified.
//
// If the screen info rectangle is left NULL the rectangle from GetViewRect
// will be used. If the rectangle is still NULL or invalid popups may not be
// drawn correctly.
function cef_render_handler_get_screen_info(self: PCefRenderHandler; browser: PCefBrowser; screen_info: PCefScreenInfo): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefRenderHandlerOwn(TWACef.GetObject(self)).GetScreenInfo(
			TCefBrowserRef.UnWrap(browser),
			screen_info^
		)
	);
end;

// Called when the browser wants to show or hide the popup widget. The popup
// should be shown if |show| is true (1) and hidden if |show| is false (0).
procedure cef_render_handler_on_popup_show(self: PCefRenderHandler; browser: PCefBrowser; show: cint); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefRenderHandlerOwn(TWACef.GetObject(self)).OnPopupShow(
		TCefBrowserRef.UnWrap(browser),
		show <> 0
	);
end;

// Called when the browser wants to move or resize the popup widget. |rect|
// contains the new location and size in view coordinates.
procedure cef_render_handler_on_popup_size(self: PCefRenderHandler; browser: PCefBrowser; const rect: PCefRect); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefRenderHandlerOwn(TWACef.GetObject(self)).OnPopupSize(
		TCefBrowserRef.UnWrap(browser),
		rect^
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
procedure cef_render_handler_on_paint(self: PCefRenderHandler; browser: PCefBrowser; _type: TCefPaintElementType; dirtyRectsCount: csize_t; dirtyRects: PCefRectArray; const buffer: cvoid; width: cint; height: cint); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefRenderHandlerOwn(TWACef.GetObject(self)).OnPaint(
		TCefBrowserRef.UnWrap(browser),
		_type,
		dirtyRectsCount,
		dirtyRects^,
		buffer,
		width,
		height
	);
end;

// Called when the browser's cursor has changed. If |type| is CT_CUSTOM then
// |custom_cursor_info| will be populated with the custom cursor information.
procedure cef_render_handler_on_cursor_change(self: PCefRenderHandler; browser: PCefBrowser; cursor: TCefCursorHandle; _type: TCefCursorType; const custom_cursor_info: PCefCursorInfo); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefRenderHandlerOwn(TWACef.GetObject(self)).OnCursorChange(
		TCefBrowserRef.UnWrap(browser),
		cursor,
		_type,
		custom_cursor_info^
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
function cef_render_handler_start_dragging(self: PCefRenderHandler; browser: PCefBrowser; drag_data: PCefDragData; allowed_ops: TCefDragOperationsMask; x: cint; y: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefRenderHandlerOwn(TWACef.GetObject(self)).StartDragging(
			TCefBrowserRef.UnWrap(browser),
			TCefDragDataRef.UnWrap(drag_data),
			allowed_ops,
			x,
			y
		)
	);
end;

// Called when the web view wants to update the mouse cursor during a drag &
// drop operation. |operation| describes the allowed operation (none, move,
// copy, link).
procedure cef_render_handler_update_drag_cursor(self: PCefRenderHandler; browser: PCefBrowser; operation: TCefDragOperationsMask); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefRenderHandlerOwn(TWACef.GetObject(self)).UpdateDragCursor(
		TCefBrowserRef.UnWrap(browser),
		operation
	);
end;

// Called when the scroll offset has changed.
procedure cef_render_handler_on_scroll_offset_changed(self: PCefRenderHandler; browser: PCefBrowser; x: cdouble; y: cdouble); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefRenderHandlerOwn(TWACef.GetObject(self)).OnScrollOffsetChanged(
		TCefBrowserRef.UnWrap(browser),
		x,
		y
	);
end;

{Protected section}
function TCefRenderHandlerOwn.GetRootScreenRect(const aBrowser: ICefBrowser; var aRect: TCefRect): Boolean;
begin
	Result := false;
end;

function TCefRenderHandlerOwn.GetViewRect(const aBrowser: ICefBrowser; var aRect: TCefRect): Boolean;
begin
	Result := false;
end;

function TCefRenderHandlerOwn.GetScreenPoint(const aBrowser: ICefBrowser; aViewX: cint; aViewY: cint; var aScreenX: cint; var aScreenY: cint): Boolean;
begin
	Result := false;
end;

function TCefRenderHandlerOwn.GetScreenInfo(const aBrowser: ICefBrowser; out aScreenInfo: TCefScreenInfo): Boolean;
begin
	Result := false;
end;

procedure TCefRenderHandlerOwn.OnPopupShow(const aBrowser: ICefBrowser; aShow: Boolean);
begin
end;

procedure TCefRenderHandlerOwn.OnPopupSize(const aBrowser: ICefBrowser; var aRect: TCefRect);
begin
end;

procedure TCefRenderHandlerOwn.OnPaint(const aBrowser: ICefBrowser; aType: TCefPaintElementType; aDirtyRectsCount: csize_t; const aDirtyRects: TCefRectArray; const aBuffer: cvoid; aWidth: cint; aHeight: cint);
begin
end;

procedure TCefRenderHandlerOwn.OnCursorChange(const aBrowser: ICefBrowser; aCursor: TCefCursorHandle; aType: TCefCursorType; aCustomCursorInfo: TCefCursorInfo);
begin
end;

function TCefRenderHandlerOwn.StartDragging(const aBrowser: ICefBrowser; const aDragData: ICefDragData; aAllowedOps: TCefDragOperationsMask; aX: cint; aY: cint): Boolean;
begin
	Result := false;
end;

procedure TCefRenderHandlerOwn.UpdateDragCursor(const aBrowser: ICefBrowser; aOperation: TCefDragOperationsMask);
begin
end;

procedure TCefRenderHandlerOwn.OnScrollOffsetChanged(const aBrowser: ICefBrowser; aX: cdouble; aY: cdouble);
begin
end;

{Public section}
constructor TCefRenderHandlerOwn.Create;
begin
	inherited CreateData(SizeOf(TCefRenderHandler));
	with PCefRenderHandler(FData)^ do
	begin
		get_root_screen_rect := {$IFDEF FPC}@{$ENDIF}cef_render_handler_get_root_screen_rect;
		get_view_rect := {$IFDEF FPC}@{$ENDIF}cef_render_handler_get_view_rect;
		get_screen_point := {$IFDEF FPC}@{$ENDIF}cef_render_handler_get_screen_point;
		get_screen_info := {$IFDEF FPC}@{$ENDIF}cef_render_handler_get_screen_info;
		on_popup_show := {$IFDEF FPC}@{$ENDIF}cef_render_handler_on_popup_show;
		on_popup_size := {$IFDEF FPC}@{$ENDIF}cef_render_handler_on_popup_size;
		on_paint := {$IFDEF FPC}@{$ENDIF}cef_render_handler_on_paint;
		on_cursor_change := {$IFDEF FPC}@{$ENDIF}cef_render_handler_on_cursor_change;
		start_dragging := {$IFDEF FPC}@{$ENDIF}cef_render_handler_start_dragging;
		update_drag_cursor := {$IFDEF FPC}@{$ENDIF}cef_render_handler_update_drag_cursor;
		on_scroll_offset_changed := {$IFDEF FPC}@{$ENDIF}cef_render_handler_on_scroll_offset_changed;
	end;
end;

//..............................................................................TCefRenderProcessHandlerOwn
// Called after the render process main thread has been created. |extra_info|
// is a read-only value originating from
// cef_browser_process_handler_t::on_render_process_thread_created(). Do not
// keep a reference to |extra_info| outside of this function.
procedure cef_render_process_handler_on_render_thread_created(self: PCefRenderProcessHandler; extra_info: PCefListValue); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefRenderProcessHandlerOwn(TWACef.GetObject(self)).OnRenderThreadCreated(
		TCefListValueRef.UnWrap(extra_info)
	);
end;

// Called after WebKit has been initialized.
procedure cef_render_process_handler_on_web_kit_initialized(self: PCefRenderProcessHandler); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefRenderProcessHandlerOwn(TWACef.GetObject(self)).OnWebKitInitialized;
end;

// Called after a browser has been created. When browsing cross-origin a new
// browser will be created before the old browser with the same identifier is
// destroyed.
procedure cef_render_process_handler_on_browser_created(self: PCefRenderProcessHandler; browser: PCefBrowser); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefRenderProcessHandlerOwn(TWACef.GetObject(self)).OnBrowserCreated(
		TCefBrowserRef.UnWrap(browser)
	);
end;

// Called before a browser is destroyed.
procedure cef_render_process_handler_on_browser_destroyed(self: PCefRenderProcessHandler; browser: PCefBrowser); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefRenderProcessHandlerOwn(TWACef.GetObject(self)).OnBrowserDestroyed(
		TCefBrowserRef.UnWrap(browser)
	);
end;

// Return the handler for browser load status events.
function cef_render_process_handler_get_load_handler(self: PCefRenderProcessHandler): PCefLoadHandler; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.GetData(
		TCefRenderProcessHandlerOwn(TWACef.GetObject(self)).GetLoadHandler
	);
end;

// Called before browser navigation. Return true (1) to cancel the navigation
// or false (0) to allow the navigation to proceed. The |request| object
// cannot be modified in this callback.
function cef_render_process_handler_on_before_navigation(self: PCefRenderProcessHandler; browser: PCefBrowser; frame: PCefFrame; request: PCefRequest; navigation_type: TCefNavigationType; is_redirect: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefRenderProcessHandlerOwn(TWACef.GetObject(self)).OnBeforeNavigation(
			TCefBrowserRef.UnWrap(browser),
			TCefFrameRef.UnWrap(frame),
			TCefRequestRef.UnWrap(request),
			navigation_type,
			is_redirect <> 0
		)
	);
end;

// Called immediately after the V8 context for a frame has been created. To
// retrieve the JavaScript 'window' object use the
// cef_v8context_t::get_global() function. V8 handles can only be accessed
// from the thread on which they are created. A task runner for posting tasks
// on the associated thread can be retrieved via the
// cef_v8context_t::get_task_runner() function.
procedure cef_render_process_handler_on_context_created(self: PCefRenderProcessHandler; browser: PCefBrowser; frame: PCefFrame; context: PCefV8context); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefRenderProcessHandlerOwn(TWACef.GetObject(self)).OnContextCreated(
		TCefBrowserRef.UnWrap(browser),
		TCefFrameRef.UnWrap(frame),
		TCefV8contextRef.UnWrap(context)
	);
end;

// Called immediately before the V8 context for a frame is released. No
// references to the context should be kept after this function is called.
procedure cef_render_process_handler_on_context_released(self: PCefRenderProcessHandler; browser: PCefBrowser; frame: PCefFrame; context: PCefV8context); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefRenderProcessHandlerOwn(TWACef.GetObject(self)).OnContextReleased(
		TCefBrowserRef.UnWrap(browser),
		TCefFrameRef.UnWrap(frame),
		TCefV8contextRef.UnWrap(context)
	);
end;

// Called for global uncaught exceptions in a frame. Execution of this
// callback is disabled by default. To enable set
// CefSettings.uncaught_exception_stack_size > 0.
procedure cef_render_process_handler_on_uncaught_exception(self: PCefRenderProcessHandler; browser: PCefBrowser; frame: PCefFrame; context: PCefV8context; exception: PCefV8exception; stackTrace: PCefV8stackTrace); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefRenderProcessHandlerOwn(TWACef.GetObject(self)).OnUncaughtException(
		TCefBrowserRef.UnWrap(browser),
		TCefFrameRef.UnWrap(frame),
		TCefV8contextRef.UnWrap(context),
		TCefV8exceptionRef.UnWrap(exception),
		TCefV8stackTraceRef.UnWrap(stackTrace)
	);
end;

// Called when a new node in the the browser gets focus. The |node| value may
// be NULL if no specific node has gained focus. The node object passed to
// this function represents a snapshot of the DOM at the time this function is
// executed. DOM objects are only valid for the scope of this function. Do not
// keep references to or attempt to access any DOM objects outside the scope
// of this function.
procedure cef_render_process_handler_on_focused_node_changed(self: PCefRenderProcessHandler; browser: PCefBrowser; frame: PCefFrame; node: PCefDomnode); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefRenderProcessHandlerOwn(TWACef.GetObject(self)).OnFocusedNodeChanged(
		TCefBrowserRef.UnWrap(browser),
		TCefFrameRef.UnWrap(frame),
		TCefDomnodeRef.UnWrap(node)
	);
end;

// Called when a new message is received from a different process. Return true
// (1) if the message was handled or false (0) otherwise. Do not keep a
// reference to or attempt to access the message outside of this callback.
function cef_render_process_handler_on_process_message_received(self: PCefRenderProcessHandler; browser: PCefBrowser; source_process: TCefProcessId; message: PCefProcessMessage): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefRenderProcessHandlerOwn(TWACef.GetObject(self)).OnProcessMessageReceived(
			TCefBrowserRef.UnWrap(browser),
			source_process,
			TCefProcessMessageRef.UnWrap(message)
		)
	);
end;

{Protected section}
procedure TCefRenderProcessHandlerOwn.OnRenderThreadCreated(const aExtraInfo: ICefListValue);
begin
end;

procedure TCefRenderProcessHandlerOwn.OnWebKitInitialized;
begin
end;

procedure TCefRenderProcessHandlerOwn.OnBrowserCreated(const aBrowser: ICefBrowser);
begin
end;

procedure TCefRenderProcessHandlerOwn.OnBrowserDestroyed(const aBrowser: ICefBrowser);
begin
end;

function TCefRenderProcessHandlerOwn.GetLoadHandler: ICefLoadHandler;
begin
	Result := nil;
end;

function TCefRenderProcessHandlerOwn.OnBeforeNavigation(const aBrowser: ICefBrowser; const aFrame: ICefFrame; const aRequest: ICefRequest; aNavigationType: TCefNavigationType; aIsRedirect: Boolean): Boolean;
begin
	Result := false;
end;

procedure TCefRenderProcessHandlerOwn.OnContextCreated(const aBrowser: ICefBrowser; const aFrame: ICefFrame; const aContext: ICefV8context);
begin
end;

procedure TCefRenderProcessHandlerOwn.OnContextReleased(const aBrowser: ICefBrowser; const aFrame: ICefFrame; const aContext: ICefV8context);
begin
end;

procedure TCefRenderProcessHandlerOwn.OnUncaughtException(const aBrowser: ICefBrowser; const aFrame: ICefFrame; const aContext: ICefV8context; const aException: ICefV8exception; const aStackTrace: ICefV8stackTrace);
begin
end;

procedure TCefRenderProcessHandlerOwn.OnFocusedNodeChanged(const aBrowser: ICefBrowser; const aFrame: ICefFrame; const aNode: ICefDomnode);
begin
end;

function TCefRenderProcessHandlerOwn.OnProcessMessageReceived(const aBrowser: ICefBrowser; aSourceProcess: TCefProcessId; const aMessage: ICefProcessMessage): Boolean;
begin
	Result := false;
end;

{Public section}
constructor TCefRenderProcessHandlerOwn.Create;
begin
	inherited CreateData(SizeOf(TCefRenderProcessHandler));
	with PCefRenderProcessHandler(FData)^ do
	begin
		on_render_thread_created := {$IFDEF FPC}@{$ENDIF}cef_render_process_handler_on_render_thread_created;
		on_web_kit_initialized := {$IFDEF FPC}@{$ENDIF}cef_render_process_handler_on_web_kit_initialized;
		on_browser_created := {$IFDEF FPC}@{$ENDIF}cef_render_process_handler_on_browser_created;
		on_browser_destroyed := {$IFDEF FPC}@{$ENDIF}cef_render_process_handler_on_browser_destroyed;
		get_load_handler := {$IFDEF FPC}@{$ENDIF}cef_render_process_handler_get_load_handler;
		on_before_navigation := {$IFDEF FPC}@{$ENDIF}cef_render_process_handler_on_before_navigation;
		on_context_created := {$IFDEF FPC}@{$ENDIF}cef_render_process_handler_on_context_created;
		on_context_released := {$IFDEF FPC}@{$ENDIF}cef_render_process_handler_on_context_released;
		on_uncaught_exception := {$IFDEF FPC}@{$ENDIF}cef_render_process_handler_on_uncaught_exception;
		on_focused_node_changed := {$IFDEF FPC}@{$ENDIF}cef_render_process_handler_on_focused_node_changed;
		on_process_message_received := {$IFDEF FPC}@{$ENDIF}cef_render_process_handler_on_process_message_received;
	end;
end;

//..............................................................................TCefRequestOwn
// Returns true (1) if this object is read-only.
function cef_request_is_read_only(self: PCefRequest): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefRequestOwn(TWACef.GetObject(self)).IsReadOnly
	);
end;

// Get the fully qualified URL.
// The resulting string must be freed by calling cef_string_userfree_free().
function cef_request_get_url(self: PCefRequest): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.UserFreeString(
		TCefRequestOwn(TWACef.GetObject(self)).GetUrl
	);
end;

// Set the fully qualified URL.
procedure cef_request_set_url(self: PCefRequest; const url: PCefString); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	url_str: ustring;
begin
	url_str := TWACef.ToString(url);
	TCefRequestOwn(TWACef.GetObject(self)).SetUrl(
		url_str
	);
end;

// Get the request function type. The value will default to POST if post data
// is provided and GET otherwise.
// The resulting string must be freed by calling cef_string_userfree_free().
function cef_request_get_method(self: PCefRequest): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.UserFreeString(
		TCefRequestOwn(TWACef.GetObject(self)).GetMethod
	);
end;

// Set the request function type.
procedure cef_request_set_method(self: PCefRequest; const method: PCefString); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	method_str: ustring;
begin
	method_str := TWACef.ToString(method);
	TCefRequestOwn(TWACef.GetObject(self)).SetMethod(
		method_str
	);
end;

// Get the post data.
function cef_request_get_post_data(self: PCefRequest): PCefPostData; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.GetData(
		TCefRequestOwn(TWACef.GetObject(self)).GetPostData
	);
end;

// Set the post data.
procedure cef_request_set_post_data(self: PCefRequest; postData: PCefPostData); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefRequestOwn(TWACef.GetObject(self)).SetPostData(
		TCefPostDataRef.UnWrap(postData)
	);
end;

// Get the header values.
procedure cef_request_get_header_map(self: PCefRequest; headerMap: TCefStringMultimap); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefRequestOwn(TWACef.GetObject(self)).GetHeaderMap(
		TCefStringMultiMapOwn.Create(headerMap)
	);
end;

// Set the header values.
procedure cef_request_set_header_map(self: PCefRequest; headerMap: TCefStringMultimap); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefRequestOwn(TWACef.GetObject(self)).SetHeaderMap(
		TCefStringMultiMapOwn.Create(headerMap)
	);
end;

// Set all values at one time.
procedure cef_request__set(self: PCefRequest; const url: PCefString; const method: PCefString; postData: PCefPostData; headerMap: TCefStringMultimap); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	url_str: ustring;
	method_str: ustring;
begin
	url_str := TWACef.ToString(url);
	method_str := TWACef.ToString(method);
	TCefRequestOwn(TWACef.GetObject(self))._Set(
		url_str,
		method_str,
		TCefPostDataRef.UnWrap(postData),
		TCefStringMultiMapOwn.Create(headerMap)
	);
end;

// Get the flags used in combination with cef_urlrequest_t. See
// cef_urlrequest_flags_t for supported values.
function cef_request_get_flags(self: PCefRequest): TCefURLrequestFlags; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := (
		TCefRequestOwn(TWACef.GetObject(self)).GetFlags
	);
end;

// Set the flags used in combination with cef_urlrequest_t.  See
// cef_urlrequest_flags_t for supported values.
procedure cef_request_set_flags(self: PCefRequest; flags: TCefURLRequestFlags); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefRequestOwn(TWACef.GetObject(self)).SetFlags(
		flags
	);
end;

// Set the URL to the first party for cookies used in combination with
// cef_urlrequest_t.
// The resulting string must be freed by calling cef_string_userfree_free().
function cef_request_get_first_party_for_cookies(self: PCefRequest): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.UserFreeString(
		TCefRequestOwn(TWACef.GetObject(self)).GetFirstPartyForCookies
	);
end;

// Get the URL to the first party for cookies used in combination with
// cef_urlrequest_t.
procedure cef_request_set_first_party_for_cookies(self: PCefRequest; const url: PCefString); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	url_str: ustring;
begin
	url_str := TWACef.ToString(url);
	TCefRequestOwn(TWACef.GetObject(self)).SetFirstPartyForCookies(
		url_str
	);
end;

// Get the resource type for this request. Only available in the browser
// process.
function cef_request_get_resource_type(self: PCefRequest): TCefResourceType; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := (
		TCefRequestOwn(TWACef.GetObject(self)).GetResourceType
	);
end;

// Get the transition type for this request. Only available in the browser
// process and only applies to requests that represent a main frame or sub-
// frame navigation.
function cef_request_get_transition_type(self: PCefRequest): TCefTransitionType; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := (
		TCefRequestOwn(TWACef.GetObject(self)).GetTransitionType
	);
end;

// Returns the globally unique identifier for this request or 0 if not
// specified. Can be used by cef_request_tHandler implementations in the
// browser process to track a single request across multiple callbacks.
function cef_request_get_identifier(self: PCefRequest): cuint64; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := (
		TCefRequestOwn(TWACef.GetObject(self)).GetIdentifier
	);
end;

{Protected section}
function TCefRequestOwn.IsReadOnly: Boolean;
begin
	Result := false;
end;

function TCefRequestOwn.GetUrl: ustring;
begin
end;

procedure TCefRequestOwn.SetUrl(const aUrl: ustring);
begin
end;

function TCefRequestOwn.GetMethod: ustring;
begin
end;

procedure TCefRequestOwn.SetMethod(const aMethod: ustring);
begin
end;

function TCefRequestOwn.GetPostData: ICefPostData;
begin
	Result := nil;
end;

procedure TCefRequestOwn.SetPostData(const aPostData: ICefPostData);
begin
end;

procedure TCefRequestOwn.GetHeaderMap(aHeaderMap: ICefStringMultimap);
begin
end;

procedure TCefRequestOwn.SetHeaderMap(aHeaderMap: ICefStringMultimap);
begin
end;

procedure TCefRequestOwn._Set(const aUrl: ustring; const aMethod: ustring; const aPostData: ICefPostData; aHeaderMap: ICefStringMultimap);
begin
end;

function TCefRequestOwn.GetFlags: TCefURLRequestFlags;
begin
	Result := [UR_FLAG_NONE];
end;

procedure TCefRequestOwn.SetFlags(aFlags: TCefURLRequestFlags);
begin
end;

function TCefRequestOwn.GetFirstPartyForCookies: ustring;
begin
end;

procedure TCefRequestOwn.SetFirstPartyForCookies(const aUrl: ustring);
begin
end;

function TCefRequestOwn.GetResourceType: TCefResourceType;
begin
end;

function TCefRequestOwn.GetTransitionType: TCefTransitionType;
begin
end;

function TCefRequestOwn.GetIdentifier: cuint64;
begin
end;

{Public section}
constructor TCefRequestOwn.Create;
begin
	inherited CreateData(SizeOf(TCefRequest));
	with PCefRequest(FData)^ do
	begin
		is_read_only := {$IFDEF FPC}@{$ENDIF}cef_request_is_read_only;
		get_url := {$IFDEF FPC}@{$ENDIF}cef_request_get_url;
		set_url := {$IFDEF FPC}@{$ENDIF}cef_request_set_url;
		get_method := {$IFDEF FPC}@{$ENDIF}cef_request_get_method;
		set_method := {$IFDEF FPC}@{$ENDIF}cef_request_set_method;
		get_post_data := {$IFDEF FPC}@{$ENDIF}cef_request_get_post_data;
		set_post_data := {$IFDEF FPC}@{$ENDIF}cef_request_set_post_data;
		get_header_map := {$IFDEF FPC}@{$ENDIF}cef_request_get_header_map;
		set_header_map := {$IFDEF FPC}@{$ENDIF}cef_request_set_header_map;
		_set := {$IFDEF FPC}@{$ENDIF}cef_request__set;
		get_flags := {$IFDEF FPC}@{$ENDIF}cef_request_get_flags;
		set_flags := {$IFDEF FPC}@{$ENDIF}cef_request_set_flags;
		get_first_party_for_cookies := {$IFDEF FPC}@{$ENDIF}cef_request_get_first_party_for_cookies;
		set_first_party_for_cookies := {$IFDEF FPC}@{$ENDIF}cef_request_set_first_party_for_cookies;
		get_resource_type := {$IFDEF FPC}@{$ENDIF}cef_request_get_resource_type;
		get_transition_type := {$IFDEF FPC}@{$ENDIF}cef_request_get_transition_type;
		get_identifier := {$IFDEF FPC}@{$ENDIF}cef_request_get_identifier;
	end;
end;
//..............................................................................TCefPostDataOwn
// Returns true (1) if this object is read-only.
function cef_post_data_is_read_only(self: PCefPostData): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefPostDataOwn(TWACef.GetObject(self)).IsReadOnly
	);
end;

// Returns the number of existing post data elements.
function cef_post_data_get_element_count(self: PCefPostData): csize_t; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := (
		TCefPostDataOwn(TWACef.GetObject(self)).GetElementCount
	);
end;

// Retrieve the post data elements.
procedure cef_post_data_get_elements(self: PCefPostData; elementsCount: pcsize_t; var elements: PCefPostDataElementArray); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	elementsCount_proxy: csize_t;
  elements_proxy: IInterfaceList;
  i: cint;
begin
  elements_proxy := TCefPostDataOwn(TWACef.GetObject(self)).GetElements(elementsCount_proxy);
  new(elements);
  for i := 0 to elementsCount_proxy - 0 do
    elements^[i] := TWACef.GetData(elements_proxy[i] as ICefPostData);
end;

// Remove the specified post data element.  Returns true (1) if the removal
// succeeds.
function cef_post_data_remove_element(self: PCefPostData; element: PCefPostDataElement): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefPostDataOwn(TWACef.GetObject(self)).RemoveElement(
			TCefPostDataElementRef.UnWrap(element)
		)
	);
end;

// Add the specified post data element.  Returns true (1) if the add succeeds.
function cef_post_data_add_element(self: PCefPostData; element: PCefPostDataElement): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefPostDataOwn(TWACef.GetObject(self)).AddElement(
			TCefPostDataElementRef.UnWrap(element)
		)
	);
end;

// Remove all existing post data elements.
procedure cef_post_data_remove_elements(self: PCefPostData); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefPostDataOwn(TWACef.GetObject(self)).RemoveElements;
end;

{Protected section}
function TCefPostDataOwn.IsReadOnly: Boolean;
begin
	Result := false;
end;

function TCefPostDataOwn.GetElementCount: csize_t;
begin
end;

function TCefPostDataOwn.GetElements(var aElementsCount: csize_t): IInterfaceList;
begin
  Result := nil;
end;

function TCefPostDataOwn.RemoveElement(const aElement: ICefPostDataElement): Boolean;
begin
	Result := false;
end;

function TCefPostDataOwn.AddElement(const aElement: ICefPostDataElement): Boolean;
begin
	Result := false;
end;

procedure TCefPostDataOwn.RemoveElements;
begin
end;

{Public section}
constructor TCefPostDataOwn.Create;
begin
	inherited CreateData(SizeOf(TCefPostData));
	with PCefPostData(FData)^ do
	begin
		is_read_only := {$IFDEF FPC}@{$ENDIF}cef_post_data_is_read_only;
		get_element_count := {$IFDEF FPC}@{$ENDIF}cef_post_data_get_element_count;
		get_elements := {$IFDEF FPC}@{$ENDIF}cef_post_data_get_elements;
		remove_element := {$IFDEF FPC}@{$ENDIF}cef_post_data_remove_element;
		add_element := {$IFDEF FPC}@{$ENDIF}cef_post_data_add_element;
		remove_elements := {$IFDEF FPC}@{$ENDIF}cef_post_data_remove_elements;
	end;
end;
//..............................................................................TCefPostDataElementOwn
// Returns true (1) if this object is read-only.
function cef_post_data_element_is_read_only(self: PCefPostDataElement): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefPostDataElementOwn(TWACef.GetObject(self)).IsReadOnly
	);
end;

// Remove all contents from the post data element.
procedure cef_post_data_element_set_to_empty(self: PCefPostDataElement); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefPostDataElementOwn(TWACef.GetObject(self)).SetToEmpty;
end;

// The post data element will represent a file.
procedure cef_post_data_element_set_to_file(self: PCefPostDataElement; const fileName: PCefString); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	fileName_str: ustring;
begin
	fileName_str := TWACef.ToString(fileName);
	TCefPostDataElementOwn(TWACef.GetObject(self)).SetToFile(
		fileName_str
	);
end;

// The post data element will represent bytes.  The bytes passed in will be
// copied.
procedure cef_post_data_element_set_to_bytes(self: PCefPostDataElement; size: csize_t; const bytes: cvoid); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefPostDataElementOwn(TWACef.GetObject(self)).SetToBytes(
		size,
		bytes
	);
end;

// Return the type of this post data element.
function cef_post_data_element_get_type(self: PCefPostDataElement): TCefPostdataelementType; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := (
		TCefPostDataElementOwn(TWACef.GetObject(self)).GetType
	);
end;

// Return the file name.
// The resulting string must be freed by calling cef_string_userfree_free().
function cef_post_data_element_get_file(self: PCefPostDataElement): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.UserFreeString(
		TCefPostDataElementOwn(TWACef.GetObject(self)).GetFile
	);
end;

// Return the number of bytes.
function cef_post_data_element_get_bytes_count(self: PCefPostDataElement): csize_t; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := (
		TCefPostDataElementOwn(TWACef.GetObject(self)).GetBytesCount
	);
end;

// Read up to |size| bytes into |bytes| and return the number of bytes
// actually read.
function cef_post_data_element_get_bytes(self: PCefPostDataElement; size: csize_t; bytes: cvoid): csize_t; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := (
		TCefPostDataElementOwn(TWACef.GetObject(self)).GetBytes(
			size,
			bytes
		)
	);
end;

{Private section}
procedure TCefPostDataElementOwn.Clear;
begin
  case FDataType of
    PDE_TYPE_BYTES:
      if (FValueByte <> nil) then
      begin
        FreeMem(FValueByte);
        FValueByte := nil;
      end;
    PDE_TYPE_FILE:
      TWACef.StringFree(@FValueStr)
  end;
  FDataType := PDE_TYPE_EMPTY;
  FSize := 0;
end;

{Protected section}
function TCefPostDataElementOwn.GetBytes(aSize: csize_t; aBytes: cvoid): csize_t;
begin
  if (FDataType = PDE_TYPE_BYTES) and (FValueByte <> nil) then
  begin
    if aSize > FSize then
      Result := FSize else
      Result := aSize;
    Move(FValueByte^, aBytes^, Result);
  end else
    Result := 0;
end;

function TCefPostDataElementOwn.GetBytesCount: csize_t;
begin
  if (FDataType = PDE_TYPE_BYTES) then
    Result := FSize else
    Result := 0;
end;

function TCefPostDataElementOwn.GetFile: ustring;
begin
  if (FDataType = PDE_TYPE_FILE) then
    Result := TWACef.ToString(@FValueStr) else
    Result := '';
end;

function TCefPostDataElementOwn.GetType: TCefPostDataElementType;
begin
  Result := FDataType;
end;

function TCefPostDataElementOwn.IsReadOnly: Boolean;
begin
  Result := FReadOnly;
end;

procedure TCefPostDataElementOwn.SetToBytes(aSize: csize_t; const aBytes: cvoid);
begin
  Clear;
  if (aSize > 0) and
    (aBytes <> nil) then
  begin
    GetMem(FValueByte, aSize);
    Move(aBytes^, FValueByte, aSize);
    FSize := aSize;
  end
  else
  begin
    FValueByte := nil;
    FSize := 0;
  end;
  FDataType := PDE_TYPE_BYTES;
end;

procedure TCefPostDataElementOwn.SetToEmpty;
begin
  Clear;
end;

procedure TCefPostDataElementOwn.SetToFile(const aFileName: ustring);
begin
  Clear;
  FSize := 0;
  FValueStr := TWACef.StringAlloc(aFileName);
  FDataType := PDE_TYPE_FILE;
end;

{Public section}
constructor TCefPostDataElementOwn.Create(aReadonly: Boolean);
begin
  inherited CreateData(SizeOf(TCefPostDataElement));
  FReadOnly := aReadonly;
  FDataType := PDE_TYPE_EMPTY;
  FValueByte := nil;
  FillChar(FValueStr, SizeOf(FValueStr), 0);
  FSize := 0;
  with PCefPostDataElement(FData)^ do
  begin
    is_read_only := {$IFDEF FPC}@{$ENDIF}cef_post_data_element_is_read_only;
    set_to_empty := {$IFDEF FPC}@{$ENDIF}cef_post_data_element_set_to_empty;
    set_to_file := {$IFDEF FPC}@{$ENDIF}cef_post_data_element_set_to_file;
    set_to_bytes := {$IFDEF FPC}@{$ENDIF}cef_post_data_element_set_to_bytes;
    get_type := {$IFDEF FPC}@{$ENDIF}cef_post_data_element_get_type;
    get_file := {$IFDEF FPC}@{$ENDIF}cef_post_data_element_get_file;
    get_bytes_count := {$IFDEF FPC}@{$ENDIF}cef_post_data_element_get_bytes_count;
    get_bytes := {$IFDEF FPC}@{$ENDIF}cef_post_data_element_get_bytes;
  end;
end;

//..............................................................................TCefRequestContextOwn
// Returns true (1) if this object is pointing to the same context as |that|
// object.
function cef_request_context_is_same(self: PCefRequestContext; other: PCefRequestContext): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefRequestContextOwn(TWACef.GetObject(self)).IsSame(
			TCefRequestContextRef.UnWrap(other)
		)
	);
end;

// Returns true (1) if this object is sharing the same storage as |that|
// object.
function cef_request_context_is_sharing_with(self: PCefRequestContext; other: PCefRequestContext): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefRequestContextOwn(TWACef.GetObject(self)).IsSharingWith(
			TCefRequestContextRef.UnWrap(other)
		)
	);
end;

// Returns true (1) if this object is the global context. The global context
// is used by default when creating a browser or URL request with a NULL
// context argument.
function cef_request_context_is_global(self: PCefRequestContext): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefRequestContextOwn(TWACef.GetObject(self)).IsGlobal
	);
end;

// Returns the handler for this context if any.
function cef_request_context_get_handler(self: PCefRequestContext): PCefRequestContextHandler; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.GetData(
		TCefRequestContextOwn(TWACef.GetObject(self)).GetHandler
	);
end;

// Returns the cache path for this object. If NULL an "incognito mode" in-
// memory cache is being used.
// The resulting string must be freed by calling cef_string_userfree_free().
function cef_request_context_get_cache_path(self: PCefRequestContext): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.UserFreeString(
		TCefRequestContextOwn(TWACef.GetObject(self)).GetCachePath
	);
end;

// Returns the default cookie manager for this object. This will be the global
// cookie manager if this object is the global request context. Otherwise,
// this will be the default cookie manager used when this request context does
// not receive a value via cef_request_tContextHandler::get_cookie_manager().
// If |callback| is non-NULL it will be executed asnychronously on the IO
// thread after the manager's storage has been initialized.
function cef_request_context_get_default_cookie_manager(self: PCefRequestContext; callback: PCefCompletionCallback): PCefCookieManager; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.GetData(
		TCefRequestContextOwn(TWACef.GetObject(self)).GetDefaultCookieManager(
			TCefCompletionCallbackRef.UnWrap(callback)
		)
	);
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
function cef_request_context_register_scheme_handler_factory(self: PCefRequestContext; const scheme_name: PCefString; const domain_name: PCefString; factory: PCefSchemeHandlerFactory): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	scheme_name_str: ustring;
	domain_name_str: ustring;
begin
	scheme_name_str := TWACef.ToString(scheme_name);
	domain_name_str := TWACef.ToString(domain_name);
	Result := Ord(
		TCefRequestContextOwn(TWACef.GetObject(self)).RegisterSchemeHandlerFactory(
			scheme_name_str,
			domain_name_str,
			TCefSchemeHandlerFactoryRef.UnWrap(factory)
		)
	);
end;

// Clear all registered scheme handler factories. Returns false (0) on error.
// This function may be called on any thread in the browser process.
function cef_request_context_clear_scheme_handler_factories(self: PCefRequestContext): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefRequestContextOwn(TWACef.GetObject(self)).ClearSchemeHandlerFactories
	);
end;

{Protected section}
function TCefRequestContextOwn.IsSame(const aOther: ICefRequestContext): Boolean;
begin
	Result := false;
end;

function TCefRequestContextOwn.IsSharingWith(const aOther: ICefRequestContext): Boolean;
begin
	Result := false;
end;

function TCefRequestContextOwn.IsGlobal: Boolean;
begin
	Result := false;
end;

function TCefRequestContextOwn.GetHandler: ICefRequestContextHandler;
begin
	Result := nil;
end;

function TCefRequestContextOwn.GetCachePath: ustring;
begin
end;

function TCefRequestContextOwn.GetDefaultCookieManager(const aCallback: ICefCompletionCallback): ICefCookieManager;
begin
	Result := nil;
end;

function TCefRequestContextOwn.RegisterSchemeHandlerFactory(const aSchemeName: ustring; const aDomainName: ustring; const aFactory: ICefSchemeHandlerFactory): Boolean;
begin
	Result := false;
end;

function TCefRequestContextOwn.ClearSchemeHandlerFactories: Boolean;
begin
	Result := false;
end;

{Public section}
constructor TCefRequestContextOwn.Create;
begin
	inherited CreateData(SizeOf(TCefRequestContext));
	with PCefRequestContext(FData)^ do
	begin
		is_same := {$IFDEF FPC}@{$ENDIF}cef_request_context_is_same;
		is_sharing_with := {$IFDEF FPC}@{$ENDIF}cef_request_context_is_sharing_with;
		is_global := {$IFDEF FPC}@{$ENDIF}cef_request_context_is_global;
		get_handler := {$IFDEF FPC}@{$ENDIF}cef_request_context_get_handler;
		get_cache_path := {$IFDEF FPC}@{$ENDIF}cef_request_context_get_cache_path;
		get_default_cookie_manager := {$IFDEF FPC}@{$ENDIF}cef_request_context_get_default_cookie_manager;
		register_scheme_handler_factory := {$IFDEF FPC}@{$ENDIF}cef_request_context_register_scheme_handler_factory;
		clear_scheme_handler_factories := {$IFDEF FPC}@{$ENDIF}cef_request_context_clear_scheme_handler_factories;
	end;
end;

//..............................................................................TCefRequestContextHandlerOwn
// Called on the IO thread to retrieve the cookie manager. If this function
// returns NULL the default cookie manager retrievable via
// cef_request_tContext::get_default_cookie_manager() will be used.
function cef_request_context_handler_get_cookie_manager(self: PCefRequestContextHandler): PCefCookieManager; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.GetData(
		TCefRequestContextHandlerOwn(TWACef.GetObject(self)).GetCookieManager
	);
end;

{Protected section}
function TCefRequestContextHandlerOwn.GetCookieManager: ICefCookieManager;
begin
	Result := nil;
end;

{Public section}
constructor TCefRequestContextHandlerOwn.Create;
begin
	inherited CreateData(SizeOf(TCefRequestContextHandler));
	with PCefRequestContextHandler(FData)^ do
	begin
		get_cookie_manager := {$IFDEF FPC}@{$ENDIF}cef_request_context_handler_get_cookie_manager;
	end;
end;

//..............................................................................TCefRequestCallbackOwn
// Continue the url request. If |allow| is true (1) the request will be
// continued. Otherwise, the request will be canceled.
procedure cef_request_callback_cont(self: PCefRequestCallback; allow: cint); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefRequestCallbackOwn(TWACef.GetObject(self)).Cont(
		allow <> 0
	);
end;

// Cancel the url request.
procedure cef_request_callback_cancel(self: PCefRequestCallback); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefRequestCallbackOwn(TWACef.GetObject(self)).Cancel;
end;

{Protected section}
procedure TCefRequestCallbackOwn.Cont(aAllow: Boolean);
begin
end;

procedure TCefRequestCallbackOwn.Cancel;
begin
end;

{Public section}
constructor TCefRequestCallbackOwn.Create;
begin
	inherited CreateData(SizeOf(TCefRequestCallback));
	with PCefRequestCallback(FData)^ do
	begin
		cont := {$IFDEF FPC}@{$ENDIF}cef_request_callback_cont;
		cancel := {$IFDEF FPC}@{$ENDIF}cef_request_callback_cancel;
	end;
end;
//..............................................................................TCefRequestHandlerOwn
// Called on the UI thread before browser navigation. Return true (1) to
// cancel the navigation or false (0) to allow the navigation to proceed. The
// |request| object cannot be modified in this callback.
// cef_load_handler_t::OnLoadingStateChange will be called twice in all cases.
// If the navigation is allowed cef_load_handler_t::OnLoadStart and
// cef_load_handler_t::OnLoadEnd will be called. If the navigation is canceled
// cef_load_handler_t::OnLoadError will be called with an |errorCode| value of
// ERR_ABORTED.
function cef_request_handler_on_before_browse(self: PCefRequestHandler; browser: PCefBrowser; frame: PCefFrame; request: PCefRequest; is_redirect: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefRequestHandlerOwn(TWACef.GetObject(self)).OnBeforeBrowse(
			TCefBrowserRef.UnWrap(browser),
			TCefFrameRef.UnWrap(frame),
			TCefRequestRef.UnWrap(request),
			is_redirect <> 0
		)
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
function cef_request_handler_on_open_urlfrom_tab(self: PCefRequestHandler; browser: PCefBrowser; frame: PCefFrame; const target_url: PCefString; target_disposition: TCefWindowOpenDisposition; user_gesture: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	target_url_str: ustring;
begin
	target_url_str := TWACef.ToString(target_url);
	Result := Ord(
		TCefRequestHandlerOwn(TWACef.GetObject(self)).OnOpenUrlfromTab(
			TCefBrowserRef.UnWrap(browser),
			TCefFrameRef.UnWrap(frame),
			target_url_str,
			target_disposition,
			user_gesture <> 0
		)
	);
end;

// Called on the IO thread before a resource request is loaded. The |request|
// object may be modified. Return RV_CONTINUE to continue the request
// immediately. Return RV_CONTINUE_ASYNC and call cef_request_tCallback::
// cont() at a later time to continue or cancel the request asynchronously.
// Return RV_CANCEL to cancel the request immediately.
//
function cef_request_handler_on_before_resource_load(self: PCefRequestHandler; browser: PCefBrowser; frame: PCefFrame; request: PCefRequest; callback: PCefRequestCallback): TCefReturnValue; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := (
		TCefRequestHandlerOwn(TWACef.GetObject(self)).OnBeforeResourceLoad(
			TCefBrowserRef.UnWrap(browser),
			TCefFrameRef.UnWrap(frame),
			TCefRequestRef.UnWrap(request),
			TCefRequestCallbackRef.UnWrap(callback)
		)
	);
end;

// Called on the IO thread before a resource is loaded. To allow the resource
// to load normally return NULL. To specify a handler for the resource return
// a cef_resource_handler_t object. The |request| object should not be
// modified in this callback.
function cef_request_handler_get_resource_handler(self: PCefRequestHandler; browser: PCefBrowser; frame: PCefFrame; request: PCefRequest): PCefResourceHandler; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.GetData(
		TCefRequestHandlerOwn(TWACef.GetObject(self)).GetResourceHandler(
			TCefBrowserRef.UnWrap(browser),
			TCefFrameRef.UnWrap(frame),
			TCefRequestRef.UnWrap(request)
		)
	);
end;

// Called on the IO thread when a resource load is redirected. The |request|
// parameter will contain the old URL and other request-related information.
// The |new_url| parameter will contain the new URL and can be changed if
// desired. The |request| object cannot be modified in this callback.
procedure cef_request_handler_on_resource_redirect(self: PCefRequestHandler; browser: PCefBrowser; frame: PCefFrame; request: PCefRequest; new_url: PCefString); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	new_url_str: ustring;
begin
	new_url_str := TWACef.ToString(new_url);
	TCefRequestHandlerOwn(TWACef.GetObject(self)).OnResourceRedirect(
		TCefBrowserRef.UnWrap(browser),
		TCefFrameRef.UnWrap(frame),
		TCefRequestRef.UnWrap(request),
		new_url_str
	);
  if new_url_str <> '' then
    TWACef.StringSet(new_url, new_url_str);
end;

// Called on the IO thread when a resource response is received. To allow the
// resource to load normally return false (0). To redirect or retry the
// resource modify |request| (url, headers or post body) and return true (1).
// The |response| object cannot be modified in this callback.
function cef_request_handler_on_resource_response(self: PCefRequestHandler; browser: PCefBrowser; frame: PCefFrame; request: PCefRequest; response: PCefResponse): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefRequestHandlerOwn(TWACef.GetObject(self)).OnResourceResponse(
			TCefBrowserRef.UnWrap(browser),
			TCefFrameRef.UnWrap(frame),
			TCefRequestRef.UnWrap(request),
			TCefResponseRef.UnWrap(response)
		)
	);
end;

// Called on the IO thread when the browser needs credentials from the user.
// |isProxy| indicates whether the host is a proxy server. |host| contains the
// hostname and |port| contains the port number. Return true (1) to continue
// the request and call cef_auth_callback_t::cont() either in this function or
// at a later time when the authentication information is available. Return
// false (0) to cancel the request immediately.
function cef_request_handler_get_auth_credentials(self: PCefRequestHandler; browser: PCefBrowser; frame: PCefFrame; isProxy: cint; const host: PCefString; port: cint; const realm: PCefString; const scheme: PCefString; callback: PCefAuthCallback): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	host_str: ustring;
	realm_str: ustring;
	scheme_str: ustring;
begin
	host_str := TWACef.ToString(host);
	realm_str := TWACef.ToString(realm);
	scheme_str := TWACef.ToString(scheme);
	Result := Ord(
		TCefRequestHandlerOwn(TWACef.GetObject(self)).GetAuthCredentials(
			TCefBrowserRef.UnWrap(browser),
			TCefFrameRef.UnWrap(frame),
			isProxy <> 0,
			host_str,
			port,
			realm_str,
			scheme_str,
			TCefAuthCallbackRef.UnWrap(callback)
		)
	);
end;

// Called on the IO thread when JavaScript requests a specific storage quota
// size via the webkitStorageInfo.requestQuota function. |origin_url| is the
// origin of the page making the request. |new_size| is the requested quota
// size in bytes. Return true (1) to continue the request and call
// cef_request_tCallback::cont() either in this function or at a later time to
// grant or deny the request. Return false (0) to cancel the request
// immediately.
function cef_request_handler_on_quota_request(self: PCefRequestHandler; browser: PCefBrowser; const origin_url: PCefString; new_size: cint64; callback: PCefRequestCallback): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	origin_url_str: ustring;
begin
	origin_url_str := TWACef.ToString(origin_url);
	Result := Ord(
		TCefRequestHandlerOwn(TWACef.GetObject(self)).OnQuotaRequest(
			TCefBrowserRef.UnWrap(browser),
			origin_url_str,
			new_size,
			TCefRequestCallbackRef.UnWrap(callback)
		)
	);
end;

// Called on the UI thread to handle requests for URLs with an unknown
// protocol component. Set |allow_os_execution| to true (1) to attempt
// execution via the registered OS protocol handler, if any. SECURITY WARNING:
// YOU SHOULD USE THIS METHOD TO ENFORCE RESTRICTIONS BASED ON SCHEME, HOST OR
// OTHER URL ANALYSIS BEFORE ALLOWING OS EXECUTION.
procedure cef_request_handler_on_protocol_execution(self: PCefRequestHandler; browser: PCefBrowser; const url: PCefString; allow_os_execution: pcint); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	url_str: ustring;
	allow_os_execution_proxy: Boolean;
begin
	url_str := TWACef.ToString(url);
	allow_os_execution_proxy := allow_os_execution^  <> 0;
	TCefRequestHandlerOwn(TWACef.GetObject(self)).OnProtocolExecution(
		TCefBrowserRef.UnWrap(browser),
		url_str,
		allow_os_execution_proxy
	);
	allow_os_execution^  := Ord(allow_os_execution_proxy);
end;

// Called on the UI thread to handle requests for URLs with an invalid SSL
// certificate. Return true (1) and call cef_request_tCallback::cont() either
// in this function or at a later time to continue or cancel the request.
// Return false (0) to cancel the request immediately. If |callback| is NULL
// the error cannot be recovered from and the request will be canceled
// automatically. If CefSettings.ignore_certificate_errors is set all invalid
// certificates will be accepted without calling this function.
function cef_request_handler_on_certificate_error(self: PCefRequestHandler; browser: PCefBrowser; cert_error: TCefErrorcode; const request_url: PCefString; ssl_info: PCefSslinfo; callback: PCefRequestCallback): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	request_url_str: ustring;
begin
	request_url_str := TWACef.ToString(request_url);
	Result := Ord(
		TCefRequestHandlerOwn(TWACef.GetObject(self)).OnCertificateError(
			TCefBrowserRef.UnWrap(browser),
			cert_error,
			request_url_str,
			TCefSslinfoRef.UnWrap(ssl_info),
			TCefRequestCallbackRef.UnWrap(callback)
		)
	);
end;

// Called on the browser process IO thread before a plugin is loaded. Return
// true (1) to block loading of the plugin.
function cef_request_handler_on_before_plugin_load(self: PCefRequestHandler; browser: PCefBrowser; const url: PCefString; const policy_url: PCefString; info: PCefWebPluginInfo): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	url_str: ustring;
	policy_url_str: ustring;
begin
	url_str := TWACef.ToString(url);
	policy_url_str := TWACef.ToString(policy_url);
	Result := Ord(
		TCefRequestHandlerOwn(TWACef.GetObject(self)).OnBeforePluginLoad(
			TCefBrowserRef.UnWrap(browser),
			url_str,
			policy_url_str,
			TCefWebPluginInfoRef.UnWrap(info)
		)
	);
end;

// Called on the browser process UI thread when a plugin has crashed.
// |plugin_path| is the path of the plugin that crashed.
procedure cef_request_handler_on_plugin_crashed(self: PCefRequestHandler; browser: PCefBrowser; const plugin_path: PCefString); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	plugin_path_str: ustring;
begin
	plugin_path_str := TWACef.ToString(plugin_path);
	TCefRequestHandlerOwn(TWACef.GetObject(self)).OnPluginCrashed(
		TCefBrowserRef.UnWrap(browser),
		plugin_path_str
	);
end;

// Called on the browser process UI thread when the render view associated
// with |browser| is ready to receive/handle IPC messages in the render
// process.
procedure cef_request_handler_on_render_view_ready(self: PCefRequestHandler; browser: PCefBrowser); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefRequestHandlerOwn(TWACef.GetObject(self)).OnRenderViewReady(
		TCefBrowserRef.UnWrap(browser)
	);
end;

// Called on the browser process UI thread when the render process terminates
// unexpectedly. |status| indicates how the process terminated.
procedure cef_request_handler_on_render_process_terminated(self: PCefRequestHandler; browser: PCefBrowser; status: TCefTerminationStatus); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefRequestHandlerOwn(TWACef.GetObject(self)).OnRenderProcessTerminated(
		TCefBrowserRef.UnWrap(browser),
		status
	);
end;

{Protected section}
function TCefRequestHandlerOwn.OnBeforeBrowse(const aBrowser: ICefBrowser; const aFrame: ICefFrame; const aRequest: ICefRequest; aIsRedirect: Boolean): Boolean;
begin
	Result := false;
end;

function TCefRequestHandlerOwn.OnOpenUrlfromTab(const aBrowser: ICefBrowser; const aFrame: ICefFrame; const aTargetUrl: ustring; aTargetDisposition: TCefWindowOpenDisposition; aUserGesture: Boolean): Boolean;
begin
	Result := false;
end;

function TCefRequestHandlerOwn.OnBeforeResourceLoad(const aBrowser: ICefBrowser; const aFrame: ICefFrame; const aRequest: ICefRequest; const aCallback: ICefRequestCallback): TCefReturnValue;
begin
end;

function TCefRequestHandlerOwn.GetResourceHandler(const aBrowser: ICefBrowser; const aFrame: ICefFrame; const aRequest: ICefRequest): ICefResourceHandler;
begin
	Result := nil;
end;

procedure TCefRequestHandlerOwn.OnResourceRedirect(const aBrowser: ICefBrowser; const aFrame: ICefFrame; const aRequest: ICefRequest; var aNewUrl: ustring);
begin
end;

function TCefRequestHandlerOwn.OnResourceResponse(const aBrowser: ICefBrowser; const aFrame: ICefFrame; const aRequest: ICefRequest; const aResponse: ICefResponse): Boolean;
begin
	Result := false;
end;

function TCefRequestHandlerOwn.GetAuthCredentials(const aBrowser: ICefBrowser; const aFrame: ICefFrame; aIsProxy: Boolean; const aHost: ustring; aPort: cint; const aRealm: ustring; const aScheme: ustring; const aCallback: ICefAuthCallback): Boolean;
begin
	Result := false;
end;

function TCefRequestHandlerOwn.OnQuotaRequest(const aBrowser: ICefBrowser; const aOriginUrl: ustring; aNewSize: cint64; const aCallback: ICefRequestCallback): Boolean;
begin
	Result := false;
end;

procedure TCefRequestHandlerOwn.OnProtocolExecution(const aBrowser: ICefBrowser; const aUrl: ustring; out aAllowOsExecution: Boolean);
begin
end;

function TCefRequestHandlerOwn.OnCertificateError(const aBrowser: ICefBrowser; aCertError: TCefErrorcode; const aRequestUrl: ustring; const aSslInfo: ICefSslinfo; const aCallback: ICefRequestCallback): Boolean;
begin
	Result := false;
end;

function TCefRequestHandlerOwn.OnBeforePluginLoad(const aBrowser: ICefBrowser; const aUrl: ustring; const aPolicyUrl: ustring; const aInfo: ICefWebPluginInfo): Boolean;
begin
	Result := false;
end;

procedure TCefRequestHandlerOwn.OnPluginCrashed(const aBrowser: ICefBrowser; const aPluginPath: ustring);
begin
end;

procedure TCefRequestHandlerOwn.OnRenderViewReady(const aBrowser: ICefBrowser);
begin
end;

procedure TCefRequestHandlerOwn.OnRenderProcessTerminated(const aBrowser: ICefBrowser; aStatus: TCefTerminationStatus);
begin
end;

{Public section}
constructor TCefRequestHandlerOwn.Create;
begin
	inherited CreateData(SizeOf(TCefRequestHandler));
	with PCefRequestHandler(FData)^ do
	begin
		on_before_browse := {$IFDEF FPC}@{$ENDIF}cef_request_handler_on_before_browse;
		on_open_urlfrom_tab := {$IFDEF FPC}@{$ENDIF}cef_request_handler_on_open_urlfrom_tab;
		on_before_resource_load := {$IFDEF FPC}@{$ENDIF}cef_request_handler_on_before_resource_load;
		get_resource_handler := {$IFDEF FPC}@{$ENDIF}cef_request_handler_get_resource_handler;
		on_resource_redirect := {$IFDEF FPC}@{$ENDIF}cef_request_handler_on_resource_redirect;
		on_resource_response := {$IFDEF FPC}@{$ENDIF}cef_request_handler_on_resource_response;
		get_auth_credentials := {$IFDEF FPC}@{$ENDIF}cef_request_handler_get_auth_credentials;
		on_quota_request := {$IFDEF FPC}@{$ENDIF}cef_request_handler_on_quota_request;
		on_protocol_execution := {$IFDEF FPC}@{$ENDIF}cef_request_handler_on_protocol_execution;
		on_certificate_error := {$IFDEF FPC}@{$ENDIF}cef_request_handler_on_certificate_error;
		on_before_plugin_load := {$IFDEF FPC}@{$ENDIF}cef_request_handler_on_before_plugin_load;
		on_plugin_crashed := {$IFDEF FPC}@{$ENDIF}cef_request_handler_on_plugin_crashed;
		on_render_view_ready := {$IFDEF FPC}@{$ENDIF}cef_request_handler_on_render_view_ready;
		on_render_process_terminated := {$IFDEF FPC}@{$ENDIF}cef_request_handler_on_render_process_terminated;
	end;
end;

//..............................................................................TCefResourceBundleHandlerOwn
// Called to retrieve a localized translation for the string specified by
// |message_id|. To provide the translation set |string| to the translation
// string and return true (1). To use the default translation return false
// (0). Supported message IDs are listed in cef_pack_strings.h.
function cef_resource_bundle_handler_get_localized_string(self: PCefResourceBundleHandler; message_id: cint; _string: PCefString): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	string_str: ustring;
begin
	string_str := TWACef.ToString(_string);
	Result := Ord(
		TCefResourceBundleHandlerOwn(TWACef.GetObject(self)).GetLocalizedString(
			message_id,
			string_str
		)
	);
	TWACef.StringSet(_string, string_str);
end;

// Called to retrieve data for the resource specified by |resource_id|. To
// provide the resource data set |data| and |data_size| to the data pointer
// and size respectively and return true (1). To use the default resource data
// return false (0). The resource data will not be copied and must remain
// resident in memory. Supported resource IDs are listed in
// cef_pack_resources.h.
function cef_resource_bundle_handler_get_data_resource(self: PCefResourceBundleHandler; resource_id: cint; var data: cvoid; data_size: pcsize_t): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	data_size_proxy: csize_t;
begin
	Result := Ord(
		TCefResourceBundleHandlerOwn(TWACef.GetObject(self)).GetDataResource(
			resource_id,
			data,
			data_size_proxy
		)
	);
end;

{Protected section}
function TCefResourceBundleHandlerOwn.GetLocalizedString(aMessageId: cint; out aString: ustring): Boolean;
begin
	Result := false;
end;

function TCefResourceBundleHandlerOwn.GetDataResource(aResourceId: cint; out aData: cvoid; out aDataSize: csize_t): Boolean;
begin
	Result := false;
end;

{Public section}
constructor TCefResourceBundleHandlerOwn.Create;
begin
	inherited CreateData(SizeOf(TCefResourceBundleHandler));
	with PCefResourceBundleHandler(FData)^ do
	begin
		get_localized_string := {$IFDEF FPC}@{$ENDIF}cef_resource_bundle_handler_get_localized_string;
		get_data_resource := {$IFDEF FPC}@{$ENDIF}cef_resource_bundle_handler_get_data_resource;
	end;
end;

//..............................................................................TCefResourceHandlerOwn
// Begin processing the request. To handle the request return true (1) and
// call cef_callback_t::cont() once the response header information is
// available (cef_callback_t::cont() can also be called from inside this
// function if header information is available immediately). To cancel the
// request return false (0).
function cef_resource_handler_process_request(self: PCefResourceHandler; request: PCefRequest; callback: PCefCallback): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefResourceHandlerOwn(TWACef.GetObject(self)).ProcessRequest(
			TCefRequestRef.UnWrap(request),
			TCefCallbackRef.UnWrap(callback)
		)
	);
end;

// Retrieve response header information. If the response length is not known
// set |response_length| to -1 and read_response() will be called until it
// returns false (0). If the response length is known set |response_length| to
// a positive value and read_response() will be called until it returns false
// (0) or the specified number of bytes have been read. Use the |response|
// object to set the mime type, http status code and other optional header
// values. To redirect the request to a new URL set |redirectUrl| to the new
// URL.
procedure cef_resource_handler_get_response_headers(self: PCefResourceHandler; response: PCefResponse; response_length: pcint64; redirectUrl: PCefString); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	response_length_proxy: cint64;
	redirectUrl_str: ustring;
begin
	redirectUrl_str := TWACef.ToString(redirectUrl);
	TCefResourceHandlerOwn(TWACef.GetObject(self)).GetResponseHeaders(
		TCefResponseRef.UnWrap(response),
		response_length_proxy,
		redirectUrl_str
	);
	TWACef.StringSet(redirectUrl, redirectUrl_str);
end;

// Read response data. If data is available immediately copy up to
// |bytes_to_read| bytes into |data_out|, set |bytes_read| to the number of
// bytes copied, and return true (1). To read the data at a later time set
// |bytes_read| to 0, return true (1) and call cef_callback_t::cont() when the
// data is available. To indicate response completion return false (0).
function cef_resource_handler_read_response(self: PCefResourceHandler; data_out: cvoid; bytes_to_read: cint; bytes_read: pcint; callback: PCefCallback): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	bytes_read_proxy: cint;
begin
	Result := Ord(
		TCefResourceHandlerOwn(TWACef.GetObject(self)).ReadResponse(
			data_out,
			bytes_to_read,
			bytes_read_proxy,
			TCefCallbackRef.UnWrap(callback)
		)
	);
end;

// Return true (1) if the specified cookie can be sent with the request or
// false (0) otherwise. If false (0) is returned for any cookie then no
// cookies will be sent with the request.
function cef_resource_handler_can_get_cookie(self: PCefResourceHandler; const cookie: PCefCookie): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
  cookie_proxy: TWACefCookie;
begin
  cookie_proxy.name := TWACef.ToString(@cookie^.name);
  cookie_proxy.value := TWACef.ToString(@cookie^.value);
  cookie_proxy.domain := TWACef.ToString(@cookie^.domain);
  cookie_proxy.path := TWACef.ToString(@cookie^.path);
  cookie_proxy.secure := cookie^.secure;
  cookie_proxy.httponly := cookie^.httponly;
  cookie_proxy.creation := cookie^.creation;
  cookie_proxy.last_access := cookie^.last_access;
  cookie_proxy.has_expires := cookie^.has_expires;
  cookie_proxy.expires := cookie^.expires;

	Result := Ord(
		TCefResourceHandlerOwn(TWACef.GetObject(self)).CanGetCookie(
			cookie_proxy
		)
	);
end;

// Return true (1) if the specified cookie returned with the response can be
// set or false (0) otherwise.
function cef_resource_handler_can_set_cookie(self: PCefResourceHandler; const cookie: PCefCookie): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
  cookie_proxy: TWACefCookie;
begin
  cookie_proxy.name := TWACef.ToString(@cookie^.name);
  cookie_proxy.value := TWACef.ToString(@cookie^.value);
  cookie_proxy.domain := TWACef.ToString(@cookie^.domain);
  cookie_proxy.path := TWACef.ToString(@cookie^.path);
  cookie_proxy.secure := cookie^.secure;
  cookie_proxy.httponly := cookie^.httponly;
  cookie_proxy.creation := cookie^.creation;
  cookie_proxy.last_access := cookie^.last_access;
  cookie_proxy.has_expires := cookie^.has_expires;
  cookie_proxy.expires := cookie^.expires;
	Result := Ord(
		TCefResourceHandlerOwn(TWACef.GetObject(self)).CanSetCookie(
			cookie_proxy
		)
	);
end;

// Request processing has been canceled.
procedure cef_resource_handler_cancel(self: PCefResourceHandler); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefResourceHandlerOwn(TWACef.GetObject(self)).Cancel;
end;

{Protected section}
function TCefResourceHandlerOwn.ProcessRequest(const aRequest: ICefRequest; const aCallback: ICefCallback): Boolean;
begin
	Result := false;
end;

procedure TCefResourceHandlerOwn.GetResponseHeaders(const aResponse: ICefResponse; var aResponseLength: cint64; var aRedirectUrl: ustring);
begin
end;

function TCefResourceHandlerOwn.ReadResponse(var aDataOut: cvoid; aBytesToRead: cint; var aBytesRead: cint; const aCallback: ICefCallback): Boolean;
begin
	Result := false;
end;

function TCefResourceHandlerOwn.CanGetCookie(const aCookie: TWACefCookie): Boolean;
begin
	Result := false;
end;

function TCefResourceHandlerOwn.CanSetCookie(const aCookie: TWACefCookie): Boolean;
begin
	Result := false;
end;

procedure TCefResourceHandlerOwn.Cancel;
begin
end;

{Public section}
constructor TCefResourceHandlerOwn.Create;
begin
	inherited CreateData(SizeOf(TCefResourceHandler));
	with PCefResourceHandler(FData)^ do
	begin
		process_request := {$IFDEF FPC}@{$ENDIF}cef_resource_handler_process_request;
		get_response_headers := {$IFDEF FPC}@{$ENDIF}cef_resource_handler_get_response_headers;
		read_response := {$IFDEF FPC}@{$ENDIF}cef_resource_handler_read_response;
		can_get_cookie := {$IFDEF FPC}@{$ENDIF}cef_resource_handler_can_get_cookie;
		can_set_cookie := {$IFDEF FPC}@{$ENDIF}cef_resource_handler_can_set_cookie;
		cancel := {$IFDEF FPC}@{$ENDIF}cef_resource_handler_cancel;
	end;
end;

//..............................................................................TCefResponseOwn
// Returns true (1) if this object is read-only.
function cef_response_is_read_only(self: PCefResponse): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefResponseOwn(TWACef.GetObject(self)).IsReadOnly
	);
end;

// Get the response status code.
function cef_response_get_status(self: PCefResponse): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := (
		TCefResponseOwn(TWACef.GetObject(self)).GetStatus
	);
end;

// Set the response status code.
procedure cef_response_set_status(self: PCefResponse; status: cint); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefResponseOwn(TWACef.GetObject(self)).SetStatus(
		status
	);
end;

// Get the response status text.
// The resulting string must be freed by calling cef_string_userfree_free().
function cef_response_get_status_text(self: PCefResponse): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.UserFreeString(
		TCefResponseOwn(TWACef.GetObject(self)).GetStatusText
	);
end;

// Set the response status text.
procedure cef_response_set_status_text(self: PCefResponse; const statusText: PCefString); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	statusText_str: ustring;
begin
	statusText_str := TWACef.ToString(statusText);
	TCefResponseOwn(TWACef.GetObject(self)).SetStatusText(
		statusText_str
	);
end;

// Get the response mime type.
// The resulting string must be freed by calling cef_string_userfree_free().
function cef_response_get_mime_type(self: PCefResponse): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.UserFreeString(
		TCefResponseOwn(TWACef.GetObject(self)).GetMimeType
	);
end;

// Set the response mime type.
procedure cef_response_set_mime_type(self: PCefResponse; const mimeType: PCefString); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	mimeType_str: ustring;
begin
	mimeType_str := TWACef.ToString(mimeType);
	TCefResponseOwn(TWACef.GetObject(self)).SetMimeType(
		mimeType_str
	);
end;

// Get the value for the specified response header field.
// The resulting string must be freed by calling cef_string_userfree_free().
function cef_response_get_header(self: PCefResponse; const name: PCefString): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	name_str: ustring;
begin
	name_str := TWACef.ToString(name);
	Result := TWACef.UserFreeString(
		TCefResponseOwn(TWACef.GetObject(self)).GetHeader(
			name_str
		)
	);
end;

// Get all response header fields.
procedure cef_response_get_header_map(self: PCefResponse; headerMap: TCefStringMultimap); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefResponseOwn(TWACef.GetObject(self)).GetHeaderMap(
		TCefStringMultimapOwn.Create(headerMap)
	);
end;

// Set all response header fields.
procedure cef_response_set_header_map(self: PCefResponse; headerMap: TCefStringMultimap); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefResponseOwn(TWACef.GetObject(self)).SetHeaderMap(
	 TCefStringMultiMapOwn.Create(headerMap)
	);
end;

{Protected section}
function TCefResponseOwn.IsReadOnly: Boolean;
begin
	Result := false;
end;

function TCefResponseOwn.GetStatus: cint;
begin
	Result := 0;
end;

procedure TCefResponseOwn.SetStatus(aStatus: cint);
begin
end;

function TCefResponseOwn.GetStatusText: ustring;
begin
end;

procedure TCefResponseOwn.SetStatusText(const aStatusText: ustring);
begin
end;

function TCefResponseOwn.GetMimeType: ustring;
begin
end;

procedure TCefResponseOwn.SetMimeType(const aMimeType: ustring);
begin
end;

function TCefResponseOwn.GetHeader(const aName: ustring): ustring;
begin
end;

procedure TCefResponseOwn.GetHeaderMap(const aHeaderMap: ICefStringMultimap);
begin
end;

procedure TCefResponseOwn.SetHeaderMap(const aHeaderMap: ICefStringMultimap);
begin
end;

{Public section}
constructor TCefResponseOwn.Create;
begin
	inherited CreateData(SizeOf(TCefResponse));
	with PCefResponse(FData)^ do
	begin
		is_read_only := {$IFDEF FPC}@{$ENDIF}cef_response_is_read_only;
		get_status := {$IFDEF FPC}@{$ENDIF}cef_response_get_status;
		set_status := {$IFDEF FPC}@{$ENDIF}cef_response_set_status;
		get_status_text := {$IFDEF FPC}@{$ENDIF}cef_response_get_status_text;
		set_status_text := {$IFDEF FPC}@{$ENDIF}cef_response_set_status_text;
		get_mime_type := {$IFDEF FPC}@{$ENDIF}cef_response_get_mime_type;
		set_mime_type := {$IFDEF FPC}@{$ENDIF}cef_response_set_mime_type;
		get_header := {$IFDEF FPC}@{$ENDIF}cef_response_get_header;
		get_header_map := {$IFDEF FPC}@{$ENDIF}cef_response_get_header_map;
		set_header_map := {$IFDEF FPC}@{$ENDIF}cef_response_set_header_map;
	end;
end;

//..............................................................................TCefSchemeRegistrarOwn
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
function cef_scheme_registrar_add_custom_scheme(self: PCefSchemeRegistrar; const scheme_name: PCefString; is_standard: cint; is_local: cint; is_display_isolated: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	scheme_name_str: ustring;
begin
	scheme_name_str := TWACef.ToString(scheme_name);
	Result := Ord(
		TCefSchemeRegistrarOwn(TWACef.GetObject(self)).AddCustomScheme(
			scheme_name_str,
			is_standard <> 0,
			is_local <> 0,
			is_display_isolated <> 0
		)
	);
end;

{Protected section}
function TCefSchemeRegistrarOwn.AddCustomScheme(const aSchemeName: ustring; aIsStandard: Boolean; aIsLocal: Boolean; aIsDisplayIsolated: Boolean): Boolean;
begin
	Result := false;
end;

{Public section}
constructor TCefSchemeRegistrarOwn.Create;
begin
	inherited CreateData(SizeOf(TCefSchemeRegistrar));
	with PCefSchemeRegistrar(FData)^ do
	begin
		add_custom_scheme := {$IFDEF FPC}@{$ENDIF}cef_scheme_registrar_add_custom_scheme;
	end;
end;
//..............................................................................TCefSchemeHandlerFactoryOwn
// Return a new resource handler instance to handle the request or an NULL
// reference to allow default handling of the request. |browser| and |frame|
// will be the browser window and frame respectively that originated the
// request or NULL if the request did not originate from a browser window (for
// example, if the request came from cef_urlrequest_t). The |request| object
// passed to this function will not contain cookie data.
function cef_scheme_handler_factory__create(self: PCefSchemeHandlerFactory; browser: PCefBrowser; frame: PCefFrame; const scheme_name: PCefString; request: PCefRequest): PCefResourceHandler; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	scheme_name_str: ustring;
begin
	scheme_name_str := TWACef.ToString(scheme_name);
	Result := TWACef.GetData(
		TCefSchemeHandlerFactoryOwn(TWACef.GetObject(self)).New(
			TCefBrowserRef.UnWrap(browser),
			TCefFrameRef.UnWrap(frame),
			scheme_name_str,
			TCefRequestRef.UnWrap(request)
		)
	);
end;

{Protected section}
function TCefSchemeHandlerFactoryOwn.New(const aBrowser: ICefBrowser; const aFrame: ICefFrame; const aSchemeName: ustring; const aRequest: ICefRequest): ICefResourceHandler;
begin
	Result := nil;
end;

{Public section}
constructor TCefSchemeHandlerFactoryOwn.Create;
begin
	inherited CreateData(SizeOf(TCefSchemeHandlerFactory));
	with PCefSchemeHandlerFactory(FData)^ do
	begin
		create := {$IFDEF FPC}@{$ENDIF}cef_scheme_handler_factory__create;
	end;
end;

//..............................................................................TCefSslcertPrincipalOwn
// Returns a name that can be used to represent the issuer.  It tries in this
// order: CN, O and OU and returns the first non-NULL one found.
// The resulting string must be freed by calling cef_string_userfree_free().
function cef_sslcert_principal_get_display_name(self: PCefSslcertPrincipal): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.UserFreeString(
		TCefSslcertPrincipalOwn(TWACef.GetObject(self)).GetDisplayName
	);
end;

// Returns the common name.
// The resulting string must be freed by calling cef_string_userfree_free().
function cef_sslcert_principal_get_common_name(self: PCefSslcertPrincipal): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.UserFreeString(
		TCefSslcertPrincipalOwn(TWACef.GetObject(self)).GetCommonName
	);
end;

// Returns the locality name.
// The resulting string must be freed by calling cef_string_userfree_free().
function cef_sslcert_principal_get_locality_name(self: PCefSslcertPrincipal): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.UserFreeString(
		TCefSslcertPrincipalOwn(TWACef.GetObject(self)).GetLocalityName
	);
end;

// Returns the state or province name.
// The resulting string must be freed by calling cef_string_userfree_free().
function cef_sslcert_principal_get_state_or_province_name(self: PCefSslcertPrincipal): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.UserFreeString(
		TCefSslcertPrincipalOwn(TWACef.GetObject(self)).GetStateOrProvinceName
	);
end;

// Returns the country name.
// The resulting string must be freed by calling cef_string_userfree_free().
function cef_sslcert_principal_get_country_name(self: PCefSslcertPrincipal): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.UserFreeString(
		TCefSslcertPrincipalOwn(TWACef.GetObject(self)).GetCountryName
	);
end;

// Retrieve the list of street addresses.
procedure cef_sslcert_principal_get_street_addresses(self: PCefSslcertPrincipal; addresses: TCefStringList); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefSslcertPrincipalOwn(TWACef.GetObject(self)).GetStreetAddresses(
		addresses
	);
end;

// Retrieve the list of organization names.
procedure cef_sslcert_principal_get_organization_names(self: PCefSslcertPrincipal; names: TCefStringList); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefSslcertPrincipalOwn(TWACef.GetObject(self)).GetOrganizationNames(
		names
	);
end;

// Retrieve the list of organization unit names.
procedure cef_sslcert_principal_get_organization_unit_names(self: PCefSslcertPrincipal; names: TCefStringList); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefSslcertPrincipalOwn(TWACef.GetObject(self)).GetOrganizationUnitNames(
		names
	);
end;

// Retrieve the list of domain components.
procedure cef_sslcert_principal_get_domain_components(self: PCefSslcertPrincipal; components: TCefStringList); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefSslcertPrincipalOwn(TWACef.GetObject(self)).GetDomainComponents(
		components
	);
end;

{Protected section}
function TCefSslcertPrincipalOwn.GetDisplayName: ustring;
begin
end;

function TCefSslcertPrincipalOwn.GetCommonName: ustring;
begin
end;

function TCefSslcertPrincipalOwn.GetLocalityName: ustring;
begin
end;

function TCefSslcertPrincipalOwn.GetStateOrProvinceName: ustring;
begin
end;

function TCefSslcertPrincipalOwn.GetCountryName: ustring;
begin
end;

procedure TCefSslcertPrincipalOwn.GetStreetAddresses(aAddresses: TStrings);
begin
end;

procedure TCefSslcertPrincipalOwn.GetOrganizationNames(aNames: TStrings);
begin
end;

procedure TCefSslcertPrincipalOwn.GetOrganizationUnitNames(aNames: TStrings);
begin
end;

procedure TCefSslcertPrincipalOwn.GetDomainComponents(aComponents: TStrings);
begin
end;

{Public section}
constructor TCefSslcertPrincipalOwn.Create;
begin
	inherited CreateData(SizeOf(TCefSslcertPrincipal));
	with PCefSslcertPrincipal(FData)^ do
	begin
		get_display_name := {$IFDEF FPC}@{$ENDIF}cef_sslcert_principal_get_display_name;
		get_common_name := {$IFDEF FPC}@{$ENDIF}cef_sslcert_principal_get_common_name;
		get_locality_name := {$IFDEF FPC}@{$ENDIF}cef_sslcert_principal_get_locality_name;
		get_state_or_province_name := {$IFDEF FPC}@{$ENDIF}cef_sslcert_principal_get_state_or_province_name;
		get_country_name := {$IFDEF FPC}@{$ENDIF}cef_sslcert_principal_get_country_name;
		get_street_addresses := {$IFDEF FPC}@{$ENDIF}cef_sslcert_principal_get_street_addresses;
		get_organization_names := {$IFDEF FPC}@{$ENDIF}cef_sslcert_principal_get_organization_names;
		get_organization_unit_names := {$IFDEF FPC}@{$ENDIF}cef_sslcert_principal_get_organization_unit_names;
		get_domain_components := {$IFDEF FPC}@{$ENDIF}cef_sslcert_principal_get_domain_components;
	end;
end;
//..............................................................................TCefSslinfoOwn
// Returns the subject of the X.509 certificate. For HTTPS server certificates
// this represents the web server.  The common name of the subject should
// match the host name of the web server.
function cef_sslinfo_get_subject(self: PCefSslinfo): PCefSslcertPrincipal; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.GetData(
		TCefSslinfoOwn(TWACef.GetObject(self)).GetSubject
	);
end;

// Returns the issuer of the X.509 certificate.
function cef_sslinfo_get_issuer(self: PCefSslinfo): PCefSslcertPrincipal; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.GetData(
		TCefSslinfoOwn(TWACef.GetObject(self)).GetIssuer
	);
end;

// Returns the DER encoded serial number for the X.509 certificate. The value
// possibly includes a leading 00 byte.
function cef_sslinfo_get_serial_number(self: PCefSslinfo): PCefBinaryValue; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.GetData(
		TCefSslinfoOwn(TWACef.GetObject(self)).GetSerialNumber
	);
end;

// Returns the date before which the X.509 certificate is invalid.
// CefTime.GetTimeT() will return 0 if no date was specified.
function cef_sslinfo_get_valid_start(self: PCefSslinfo): TCefTime; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := (
		TCefSslinfoOwn(TWACef.GetObject(self)).GetValidStart
	);
end;

// Returns the date after which the X.509 certificate is invalid.
// CefTime.GetTimeT() will return 0 if no date was specified.
function cef_sslinfo_get_valid_expiry(self: PCefSslinfo): TCefTime; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := (
		TCefSslinfoOwn(TWACef.GetObject(self)).GetValidExpiry
	);
end;

// Returns the DER encoded data for the X.509 certificate.
function cef_sslinfo_get_derencoded(self: PCefSslinfo): PCefBinaryValue; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.GetData(
		TCefSslinfoOwn(TWACef.GetObject(self)).GetDerencoded
	);
end;

// Returns the PEM encoded data for the X.509 certificate.
function cef_sslinfo_get_pemencoded(self: PCefSslinfo): PCefBinaryValue; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.GetData(
		TCefSslinfoOwn(TWACef.GetObject(self)).GetPemencoded
	);
end;

{Protected section}
function TCefSslinfoOwn.GetSubject: ICefSslcertPrincipal;
begin
	Result := nil;
end;

function TCefSslinfoOwn.GetIssuer: ICefSslcertPrincipal;
begin
	Result := nil;
end;

function TCefSslinfoOwn.GetSerialNumber: ICefBinaryValue;
begin
	Result := nil;
end;

function TCefSslinfoOwn.GetValidStart: TCefTime;
begin
end;

function TCefSslinfoOwn.GetValidExpiry: TCefTime;
begin
end;

function TCefSslinfoOwn.GetDerencoded: ICefBinaryValue;
begin
	Result := nil;
end;

function TCefSslinfoOwn.GetPemencoded: ICefBinaryValue;
begin
	Result := nil;
end;

{Public section}
constructor TCefSslinfoOwn.Create;
begin
	inherited CreateData(SizeOf(TCefSslinfo));
	with PCefSslinfo(FData)^ do
	begin
		get_subject := {$IFDEF FPC}@{$ENDIF}cef_sslinfo_get_subject;
		get_issuer := {$IFDEF FPC}@{$ENDIF}cef_sslinfo_get_issuer;
		get_serial_number := {$IFDEF FPC}@{$ENDIF}cef_sslinfo_get_serial_number;
		get_valid_start := {$IFDEF FPC}@{$ENDIF}cef_sslinfo_get_valid_start;
		get_valid_expiry := {$IFDEF FPC}@{$ENDIF}cef_sslinfo_get_valid_expiry;
		get_derencoded := {$IFDEF FPC}@{$ENDIF}cef_sslinfo_get_derencoded;
		get_pemencoded := {$IFDEF FPC}@{$ENDIF}cef_sslinfo_get_pemencoded;
	end;
end;

//..............................................................................TCefReadHandlerOwn
// Read raw binary data.
function cef_read_handler_read(self: PCefReadHandler; ptr: cvoid; size: csize_t; n: csize_t): csize_t; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := (
		TCefReadHandlerOwn(TWACef.GetObject(self)).Read(
			ptr,
			size,
			n
		)
	);
end;

// Seek to the specified offset position. |whence| may be any one of SEEK_CUR,
// SEEK_END or SEEK_SET. Return zero on success and non-zero on failure.
function cef_read_handler_seek(self: PCefReadHandler; offset: cint64; whence: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefReadHandlerOwn(TWACef.GetObject(self)).Seek(
			offset,
			whence
		)
	);
end;

// Return the current offset position.
function cef_read_handler_tell(self: PCefReadHandler): cint64; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := (
		TCefReadHandlerOwn(TWACef.GetObject(self)).Tell
	);
end;

// Return non-zero if at end of file.
function cef_read_handler_eof(self: PCefReadHandler): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefReadHandlerOwn(TWACef.GetObject(self)).Eof
	);
end;

// Return true (1) if this handler performs work like accessing the file
// system which may block. Used as a hint for determining the thread to access
// the handler from.
function cef_read_handler_may_block(self: PCefReadHandler): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefReadHandlerOwn(TWACef.GetObject(self)).MayBlock
	);
end;

{Protected section}
function TCefReadHandlerOwn.Read(aPtr: cvoid; aSize: csize_t; aN: csize_t): csize_t;
begin
end;

function TCefReadHandlerOwn.Seek(aOffset: cint64; aWhence: cint): Boolean;
begin
	Result := false;
end;

function TCefReadHandlerOwn.Tell: cint64;
begin
end;

function TCefReadHandlerOwn.Eof: Boolean;
begin
	Result := false;
end;

function TCefReadHandlerOwn.MayBlock: Boolean;
begin
	Result := false;
end;

{Public section}
constructor TCefReadHandlerOwn.Create;
begin
	inherited CreateData(SizeOf(TCefReadHandler));
	with PCefReadHandler(FData)^ do
	begin
		read := {$IFDEF FPC}@{$ENDIF}cef_read_handler_read;
		seek := {$IFDEF FPC}@{$ENDIF}cef_read_handler_seek;
		tell := {$IFDEF FPC}@{$ENDIF}cef_read_handler_tell;
		eof := {$IFDEF FPC}@{$ENDIF}cef_read_handler_eof;
		may_block := {$IFDEF FPC}@{$ENDIF}cef_read_handler_may_block;
	end;
end;
//..............................................................................TCefStreamReaderOwn
// Read raw binary data.
function cef_stream_reader_read(self: PCefStreamReader; ptr: cvoid; size: csize_t; n: csize_t): csize_t; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := (
		TCefStreamReaderOwn(TWACef.GetObject(self)).Read(
			ptr,
			size,
			n
		)
	);
end;

// Seek to the specified offset position. |whence| may be any one of SEEK_CUR,
// SEEK_END or SEEK_SET. Returns zero on success and non-zero on failure.
function cef_stream_reader_seek(self: PCefStreamReader; offset: cint64; whence: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefStreamReaderOwn(TWACef.GetObject(self)).Seek(
			offset,
			whence
		)
	);
end;

// Return the current offset position.
function cef_stream_reader_tell(self: PCefStreamReader): cint64; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := (
		TCefStreamReaderOwn(TWACef.GetObject(self)).Tell
	);
end;

// Return non-zero if at end of file.
function cef_stream_reader_eof(self: PCefStreamReader): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefStreamReaderOwn(TWACef.GetObject(self)).Eof
	);
end;

// Returns true (1) if this reader performs work like accessing the file
// system which may block. Used as a hint for determining the thread to access
// the reader from.
function cef_stream_reader_may_block(self: PCefStreamReader): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefStreamReaderOwn(TWACef.GetObject(self)).MayBlock
	);
end;

{Protected section}
function TCefStreamReaderOwn.Read(aPtr: cvoid; aSize: csize_t; aN: csize_t): csize_t;
begin
end;

function TCefStreamReaderOwn.Seek(aOffset: cint64; aWhence: cint): Boolean;
begin
	Result := false;
end;

function TCefStreamReaderOwn.Tell: cint64;
begin
end;

function TCefStreamReaderOwn.Eof: Boolean;
begin
	Result := false;
end;

function TCefStreamReaderOwn.MayBlock: Boolean;
begin
	Result := false;
end;

{Public section}
constructor TCefStreamReaderOwn.Create;
begin
	inherited CreateData(SizeOf(TCefStreamReader));
	with PCefStreamReader(FData)^ do
	begin
		read := {$IFDEF FPC}@{$ENDIF}cef_stream_reader_read;
		seek := {$IFDEF FPC}@{$ENDIF}cef_stream_reader_seek;
		tell := {$IFDEF FPC}@{$ENDIF}cef_stream_reader_tell;
		eof := {$IFDEF FPC}@{$ENDIF}cef_stream_reader_eof;
		may_block := {$IFDEF FPC}@{$ENDIF}cef_stream_reader_may_block;
	end;
end;
//..............................................................................TCefWriteHandlerOwn
// Write raw binary data.
function cef_write_handler_write(self: PCefWriteHandler; const ptr: cvoid; size: csize_t; n: csize_t): csize_t; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := (
		TCefWriteHandlerOwn(TWACef.GetObject(self)).Write(
			ptr,
			size,
			n
		)
	);
end;

// Seek to the specified offset position. |whence| may be any one of SEEK_CUR,
// SEEK_END or SEEK_SET. Return zero on success and non-zero on failure.
function cef_write_handler_seek(self: PCefWriteHandler; offset: cint64; whence: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefWriteHandlerOwn(TWACef.GetObject(self)).Seek(
			offset,
			whence
		)
	);
end;

// Return the current offset position.
function cef_write_handler_tell(self: PCefWriteHandler): cint64; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := (
		TCefWriteHandlerOwn(TWACef.GetObject(self)).Tell
	);
end;

// Flush the stream.
function cef_write_handler_flush(self: PCefWriteHandler): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefWriteHandlerOwn(TWACef.GetObject(self)).Flush
	);
end;

// Return true (1) if this handler performs work like accessing the file
// system which may block. Used as a hint for determining the thread to access
// the handler from.
function cef_write_handler_may_block(self: PCefWriteHandler): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefWriteHandlerOwn(TWACef.GetObject(self)).MayBlock
	);
end;

{Protected section}
function TCefWriteHandlerOwn.Write(aPtr: cvoid; aSize: csize_t; aN: csize_t): csize_t;
begin
end;

function TCefWriteHandlerOwn.Seek(aOffset: cint64; aWhence: cint): Boolean;
begin
	Result := false;
end;

function TCefWriteHandlerOwn.Tell: cint64;
begin
end;

function TCefWriteHandlerOwn.Flush: Boolean;
begin
	Result := false;
end;

function TCefWriteHandlerOwn.MayBlock: Boolean;
begin
	Result := false;
end;

{Public section}
constructor TCefWriteHandlerOwn.Create;
begin
	inherited CreateData(SizeOf(TCefWriteHandler));
	with PCefWriteHandler(FData)^ do
	begin
		write := {$IFDEF FPC}@{$ENDIF}cef_write_handler_write;
		seek := {$IFDEF FPC}@{$ENDIF}cef_write_handler_seek;
		tell := {$IFDEF FPC}@{$ENDIF}cef_write_handler_tell;
		flush := {$IFDEF FPC}@{$ENDIF}cef_write_handler_flush;
		may_block := {$IFDEF FPC}@{$ENDIF}cef_write_handler_may_block;
	end;
end;
//..............................................................................TCefStreamWriterOwn
// Write raw binary data.
function cef_stream_writer_write(self: PCefStreamWriter; const ptr: cvoid; size: csize_t; n: csize_t): csize_t; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := (
		TCefStreamWriterOwn(TWACef.GetObject(self)).Write(
			ptr,
			size,
			n
		)
	);
end;

// Seek to the specified offset position. |whence| may be any one of SEEK_CUR,
// SEEK_END or SEEK_SET. Returns zero on success and non-zero on failure.
function cef_stream_writer_seek(self: PCefStreamWriter; offset: cint64; whence: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefStreamWriterOwn(TWACef.GetObject(self)).Seek(
			offset,
			whence
		)
	);
end;

// Return the current offset position.
function cef_stream_writer_tell(self: PCefStreamWriter): cint64; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := (
		TCefStreamWriterOwn(TWACef.GetObject(self)).Tell
	);
end;

// Flush the stream.
function cef_stream_writer_flush(self: PCefStreamWriter): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefStreamWriterOwn(TWACef.GetObject(self)).Flush
	);
end;

// Returns true (1) if this writer performs work like accessing the file
// system which may block. Used as a hint for determining the thread to access
// the writer from.
function cef_stream_writer_may_block(self: PCefStreamWriter): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefStreamWriterOwn(TWACef.GetObject(self)).MayBlock
	);
end;

{Protected section}
function TCefStreamWriterOwn.Write(aPtr: cvoid; aSize: csize_t; aN: csize_t): csize_t;
begin
end;

function TCefStreamWriterOwn.Seek(aOffset: cint64; aWhence: cint): Boolean;
begin
	Result := false;
end;

function TCefStreamWriterOwn.Tell: cint64;
begin
end;

function TCefStreamWriterOwn.Flush: Boolean;
begin
	Result := false;
end;

function TCefStreamWriterOwn.MayBlock: Boolean;
begin
	Result := false;
end;

{Public section}
constructor TCefStreamWriterOwn.Create;
begin
	inherited CreateData(SizeOf(TCefStreamWriter));
	with PCefStreamWriter(FData)^ do
	begin
		write := {$IFDEF FPC}@{$ENDIF}cef_stream_writer_write;
		seek := {$IFDEF FPC}@{$ENDIF}cef_stream_writer_seek;
		tell := {$IFDEF FPC}@{$ENDIF}cef_stream_writer_tell;
		flush := {$IFDEF FPC}@{$ENDIF}cef_stream_writer_flush;
		may_block := {$IFDEF FPC}@{$ENDIF}cef_stream_writer_may_block;
	end;
end;

//..............................................................................TCefStringVisitorOwn
// Method that will be executed.
procedure cef_string_visitor_visit(self: PCefStringVisitor; const _string: PCefString); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	string_str: ustring;
begin
	string_str := TWACef.ToString(_string);
	TCefStringVisitorOwn(TWACef.GetObject(self)).Visit(
		string_str
	);
end;

{Protected section}
procedure TCefStringVisitorOwn.Visit(const aString: ustring);
begin
end;

{Public section}
constructor TCefStringVisitorOwn.Create;
begin
	inherited CreateData(SizeOf(TCefStringVisitor));
	with PCefStringVisitor(FData)^ do
	begin
		visit := {$IFDEF FPC}@{$ENDIF}cef_string_visitor_visit;
	end;
end;

//..............................................................................TCefTaskOwn
// Method that will be executed on the target thread.
procedure cef_task_execute(self: PCefTask); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefTaskOwn(TWACef.GetObject(self)).Execute;
end;

{Protected section}
procedure TCefTaskOwn.Execute;
begin
end;

{Public section}
constructor TCefTaskOwn.Create;
begin
	inherited CreateData(SizeOf(TCefTask));
	with PCefTask(FData)^ do
	begin
		execute := {$IFDEF FPC}@{$ENDIF}cef_task_execute;
	end;
end;
//..............................................................................TCefTaskRunnerOwn
// Returns true (1) if this object is pointing to the same task runner as
// |that| object.
function cef_task_runner_is_same(self: PCefTaskRunner; that: PCefTaskRunner): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefTaskRunnerOwn(TWACef.GetObject(self)).IsSame(
			TCefTaskRunnerRef.UnWrap(that)
		)
	);
end;

// Returns true (1) if this task runner belongs to the current thread.
function cef_task_runner_belongs_to_current_thread(self: PCefTaskRunner): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefTaskRunnerOwn(TWACef.GetObject(self)).BelongsToCurrentThread
	);
end;

// Returns true (1) if this task runner is for the specified CEF thread.
function cef_task_runner_belongs_to_thread(self: PCefTaskRunner; threadId: TCefThreadId): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefTaskRunnerOwn(TWACef.GetObject(self)).BelongsToThread(
			threadId
		)
	);
end;

// Post a task for execution on the thread associated with this task runner.
// Execution will occur asynchronously.
function cef_task_runner_post_task(self: PCefTaskRunner; task: PCefTask): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefTaskRunnerOwn(TWACef.GetObject(self)).PostTask(
			TCefTaskRef.UnWrap(task)
		)
	);
end;

// Post a task for delayed execution on the thread associated with this task
// runner. Execution will occur asynchronously. Delayed tasks are not
// supported on V8 WebWorker threads and will be executed without the
// specified delay.
function cef_task_runner_post_delayed_task(self: PCefTaskRunner; task: PCefTask; delay_ms: cint64): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefTaskRunnerOwn(TWACef.GetObject(self)).PostDelayedTask(
			TCefTaskRef.UnWrap(task),
			delay_ms
		)
	);
end;

{Protected section}
function TCefTaskRunnerOwn.IsSame(const aThat: ICefTaskRunner): Boolean;
begin
	Result := false;
end;

function TCefTaskRunnerOwn.BelongsToCurrentThread: Boolean;
begin
	Result := false;
end;

function TCefTaskRunnerOwn.BelongsToThread(aThreadId: TCefThreadId): Boolean;
begin
	Result := false;
end;

function TCefTaskRunnerOwn.PostTask(const aTask: ICefTask): Boolean;
begin
	Result := false;
end;

function TCefTaskRunnerOwn.PostDelayedTask(const aTask: ICefTask; aDelayMs: cint64): Boolean;
begin
	Result := false;
end;

{Public section}
constructor TCefTaskRunnerOwn.Create;
begin
	inherited CreateData(SizeOf(TCefTaskRunner));
	with PCefTaskRunner(FData)^ do
	begin
		is_same := {$IFDEF FPC}@{$ENDIF}cef_task_runner_is_same;
		belongs_to_current_thread := {$IFDEF FPC}@{$ENDIF}cef_task_runner_belongs_to_current_thread;
		belongs_to_thread := {$IFDEF FPC}@{$ENDIF}cef_task_runner_belongs_to_thread;
		post_task := {$IFDEF FPC}@{$ENDIF}cef_task_runner_post_task;
		post_delayed_task := {$IFDEF FPC}@{$ENDIF}cef_task_runner_post_delayed_task;
	end;
end;

//..............................................................................TCefEndTracingCallbackOwn
// Called after all processes have sent their trace data. |tracing_file| is
// the path at which tracing data was written. The client is responsible for
// deleting |tracing_file|.
procedure cef_end_tracing_callback_on_end_tracing_complete(self: PCefEndTracingCallback; const tracing_file: PCefString); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	tracing_file_str: ustring;
begin
	tracing_file_str := TWACef.ToString(tracing_file);
	TCefEndTracingCallbackOwn(TWACef.GetObject(self)).OnEndTracingComplete(
		tracing_file_str
	);
end;

{Protected section}
procedure TCefEndTracingCallbackOwn.OnEndTracingComplete(const aTracingFile: ustring);
begin
end;

{Public section}
constructor TCefEndTracingCallbackOwn.Create;
begin
	inherited CreateData(SizeOf(TCefEndTracingCallback));
	with PCefEndTracingCallback(FData)^ do
	begin
		on_end_tracing_complete := {$IFDEF FPC}@{$ENDIF}cef_end_tracing_callback_on_end_tracing_complete;
	end;
end;

//..............................................................................TCefUrlrequestOwn
// Returns the request object used to create this URL request. The returned
// object is read-only and should not be modified.
function cef_urlrequest_get_request(self: PCefUrlrequest): PCefRequest; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.GetData(
		TCefUrlrequestOwn(TWACef.GetObject(self)).GetRequest
	);
end;

// Returns the client.
function cef_urlrequest_get_client(self: PCefUrlrequest): PCefUrlrequestClient; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.GetData(
		TCefUrlrequestOwn(TWACef.GetObject(self)).GetClient
	);
end;

// Returns the request status.
function cef_urlrequest_get_request_status(self: PCefUrlrequest): TCefUrlrequestStatus; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := (
		TCefUrlrequestOwn(TWACef.GetObject(self)).GetRequestStatus
	);
end;

// Returns the request error if status is UR_CANCELED or UR_FAILED, or 0
// otherwise.
function cef_urlrequest_get_request_error(self: PCefUrlrequest): TCefErrorcode; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := (
		TCefUrlrequestOwn(TWACef.GetObject(self)).GetRequestError
	);
end;

// Returns the response, or NULL if no response information is available.
// Response information will only be available after the upload has completed.
// The returned object is read-only and should not be modified.
function cef_urlrequest_get_response(self: PCefUrlrequest): PCefResponse; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.GetData(
		TCefUrlrequestOwn(TWACef.GetObject(self)).GetResponse
	);
end;

// Cancel the request.
procedure cef_urlrequest_cancel(self: PCefUrlrequest); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefUrlrequestOwn(TWACef.GetObject(self)).Cancel;
end;

{Protected section}
function TCefUrlrequestOwn.GetRequest: ICefRequest;
begin
	Result := nil;
end;

function TCefUrlrequestOwn.GetClient: ICefUrlrequestClient;
begin
	Result := nil;
end;

function TCefUrlrequestOwn.GetRequestStatus: TCefUrlrequestStatus;
begin
end;

function TCefUrlrequestOwn.GetRequestError: TCefErrorcode;
begin
end;

function TCefUrlrequestOwn.GetResponse: ICefResponse;
begin
	Result := nil;
end;

procedure TCefUrlrequestOwn.Cancel;
begin
end;

{Public section}
constructor TCefUrlrequestOwn.Create;
begin
	inherited CreateData(SizeOf(TCefUrlrequest));
	with PCefUrlrequest(FData)^ do
	begin
		get_request := {$IFDEF FPC}@{$ENDIF}cef_urlrequest_get_request;
		get_client := {$IFDEF FPC}@{$ENDIF}cef_urlrequest_get_client;
		get_request_status := {$IFDEF FPC}@{$ENDIF}cef_urlrequest_get_request_status;
		get_request_error := {$IFDEF FPC}@{$ENDIF}cef_urlrequest_get_request_error;
		get_response := {$IFDEF FPC}@{$ENDIF}cef_urlrequest_get_response;
		cancel := {$IFDEF FPC}@{$ENDIF}cef_urlrequest_cancel;
	end;
end;
//..............................................................................TCefUrlrequestClientOwn
// Notifies the client that the request has completed. Use the
// cef_urlrequest_t::GetRequestStatus function to determine if the request was
// successful or not.
procedure cef_urlrequest_client_on_request_complete(self: PCefUrlrequestClient; request: PCefUrlrequest); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefUrlrequestClientOwn(TWACef.GetObject(self)).OnRequestComplete(
		TCefUrlrequestRef.UnWrap(request)
	);
end;

// Notifies the client of upload progress. |current| denotes the number of
// bytes sent so far and |total| is the total size of uploading data (or -1 if
// chunked upload is enabled). This function will only be called if the
// UR_FLAG_REPORT_UPLOAD_PROGRESS flag is set on the request.
procedure cef_urlrequest_client_on_upload_progress(self: PCefUrlrequestClient; request: PCefUrlrequest; current: cint64; total: cint64); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefUrlrequestClientOwn(TWACef.GetObject(self)).OnUploadProgress(
		TCefUrlrequestRef.UnWrap(request),
		current,
		total
	);
end;

// Notifies the client of download progress. |current| denotes the number of
// bytes received up to the call and |total| is the expected total size of the
// response (or -1 if not determined).
procedure cef_urlrequest_client_on_download_progress(self: PCefUrlrequestClient; request: PCefUrlrequest; current: cint64; total: cint64); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefUrlrequestClientOwn(TWACef.GetObject(self)).OnDownloadProgress(
		TCefUrlrequestRef.UnWrap(request),
		current,
		total
	);
end;

// Called when some part of the response is read. |data| contains the current
// bytes received since the last call. This function will not be called if the
// UR_FLAG_NO_DOWNLOAD_DATA flag is set on the request.
procedure cef_urlrequest_client_on_download_data(self: PCefUrlrequestClient; request: PCefUrlrequest; const data: cvoid; data_length: csize_t); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TCefUrlrequestClientOwn(TWACef.GetObject(self)).OnDownloadData(
		TCefUrlrequestRef.UnWrap(request),
		data,
		data_length
	);
end;

// Called on the IO thread when the browser needs credentials from the user.
// |isProxy| indicates whether the host is a proxy server. |host| contains the
// hostname and |port| contains the port number. Return true (1) to continue
// the request and call cef_auth_callback_t::cont() when the authentication
// information is available. Return false (0) to cancel the request. This
// function will only be called for requests initiated from the browser
// process.
function cef_urlrequest_client_get_auth_credentials(self: PCefUrlrequestClient; isProxy: cint; const host: PCefString; port: cint; const realm: PCefString; const scheme: PCefString; callback: PCefAuthCallback): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	host_str: ustring;
	realm_str: ustring;
	scheme_str: ustring;
begin
	host_str := TWACef.ToString(host);
	realm_str := TWACef.ToString(realm);
	scheme_str := TWACef.ToString(scheme);
	Result := Ord(
		TCefUrlrequestClientOwn(TWACef.GetObject(self)).GetAuthCredentials(
			isProxy <> 0,
			host_str,
			port,
			realm_str,
			scheme_str,
			TCefAuthCallbackRef.UnWrap(callback)
		)
	);
end;

{Protected section}
procedure TCefUrlrequestClientOwn.OnRequestComplete(const aRequest: ICefUrlrequest);
begin
end;

procedure TCefUrlrequestClientOwn.OnUploadProgress(const aRequest: ICefUrlrequest; aCurrent: cint64; aTotal: cint64);
begin
end;

procedure TCefUrlrequestClientOwn.OnDownloadProgress(const aRequest: ICefUrlrequest; aCurrent: cint64; aTotal: cint64);
begin
end;

procedure TCefUrlrequestClientOwn.OnDownloadData(const aRequest: ICefUrlrequest; const aData: cvoid; aDataLength: csize_t);
begin
end;

function TCefUrlrequestClientOwn.GetAuthCredentials(aIsProxy: Boolean; const aHost: ustring; aPort: cint; const aRealm: ustring; const aScheme: ustring; const aCallback: ICefAuthCallback): Boolean;
begin
	Result := false;
end;

{Public section}
constructor TCefUrlrequestClientOwn.Create;
begin
	inherited CreateData(SizeOf(TCefUrlrequestClient));
	with PCefUrlrequestClient(FData)^ do
	begin
		on_request_complete := {$IFDEF FPC}@{$ENDIF}cef_urlrequest_client_on_request_complete;
		on_upload_progress := {$IFDEF FPC}@{$ENDIF}cef_urlrequest_client_on_upload_progress;
		on_download_progress := {$IFDEF FPC}@{$ENDIF}cef_urlrequest_client_on_download_progress;
		on_download_data := {$IFDEF FPC}@{$ENDIF}cef_urlrequest_client_on_download_data;
		get_auth_credentials := {$IFDEF FPC}@{$ENDIF}cef_urlrequest_client_get_auth_credentials;
	end;
end;

//..............................................................................TCefV8contextOwn
// Returns the task runner associated with this context. V8 handles can only
// be accessed from the thread on which they are created. This function can be
// called on any render process thread.
function cef_v8context_get_task_runner(self: PCefV8context): PCefTaskRunner; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.GetData(
		TCefV8contextOwn(TWACef.GetObject(self)).GetTaskRunner
	);
end;

// Returns true (1) if the underlying handle is valid and it can be accessed
// on the current thread. Do not call any other functions if this function
// returns false (0).
function cef_v8context_is_valid(self: PCefV8context): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefV8contextOwn(TWACef.GetObject(self)).IsValid
	);
end;

// Returns the browser for this context. This function will return an NULL
// reference for WebWorker contexts.
function cef_v8context_get_browser(self: PCefV8context): PCefBrowser; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.GetData(
		TCefV8contextOwn(TWACef.GetObject(self)).GetBrowser
	);
end;

// Returns the frame for this context. This function will return an NULL
// reference for WebWorker contexts.
function cef_v8context_get_frame(self: PCefV8context): PCefFrame; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.GetData(
		TCefV8contextOwn(TWACef.GetObject(self)).GetFrame
	);
end;

// Returns the global object for this context. The context must be entered
// before calling this function.
function cef_v8context_get_global(self: PCefV8context): PCefV8value; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.GetData(
		TCefV8contextOwn(TWACef.GetObject(self)).GetGlobal
	);
end;

// Enter this context. A context must be explicitly entered before creating a
// V8 Object, Array, Function or Date asynchronously. exit() must be called
// the same number of times as enter() before releasing this context. V8
// objects belong to the context in which they are created. Returns true (1)
// if the scope was entered successfully.
function cef_v8context_enter(self: PCefV8context): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefV8contextOwn(TWACef.GetObject(self)).Enter
	);
end;

// Exit this context. Call this function only after calling enter(). Returns
// true (1) if the scope was exited successfully.
function cef_v8context_exit(self: PCefV8context): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefV8contextOwn(TWACef.GetObject(self)).Exit
	);
end;

// Returns true (1) if this object is pointing to the same handle as |that|
// object.
function cef_v8context_is_same(self: PCefV8context; that: PCefV8context): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefV8contextOwn(TWACef.GetObject(self)).IsSame(
			TCefV8contextRef.UnWrap(that)
		)
	);
end;

// Evaluates the specified JavaScript code using this context's global object.
// On success |retval| will be set to the return value, if any, and the
// function will return true (1). On failure |exception| will be set to the
// exception, if any, and the function will return false (0).
function cef_v8context_eval(self: PCefV8context; const code: PCefString; var retval: PCefV8value; var exception: PCefV8exception): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	code_str: ustring;
	retval_proxy: ICefV8value;
	exception_proxy: ICefV8exception;
begin
	code_str := TWACef.ToString(code);
	retval_proxy := TCefV8valueOwn(TWACef.GetObject(retval)) as ICefV8value;
	exception_proxy := TCefV8exceptionOwn(TWACef.GetObject(exception)) as ICefV8exception;
	Result := Ord(
		TCefV8contextOwn(TWACef.GetObject(self)).Eval(
			code_str,
			retval_proxy,
			exception_proxy
		)
	);
	retval := TWACef.GetData(retval_proxy);
	exception := TWACef.GetData(exception_proxy);
end;

{Protected section}
function TCefV8contextOwn.GetTaskRunner: ICefTaskRunner;
begin
	Result := nil;
end;

function TCefV8contextOwn.IsValid: Boolean;
begin
	Result := false;
end;

function TCefV8contextOwn.GetBrowser: ICefBrowser;
begin
	Result := nil;
end;

function TCefV8contextOwn.GetFrame: ICefFrame;
begin
	Result := nil;
end;

function TCefV8contextOwn.GetGlobal: ICefV8value;
begin
	Result := nil;
end;

function TCefV8contextOwn.Enter: Boolean;
begin
	Result := false;
end;

function TCefV8contextOwn.Exit: Boolean;
begin
	Result := false;
end;

function TCefV8contextOwn.IsSame(const aThat: ICefV8context): Boolean;
begin
	Result := false;
end;

function TCefV8contextOwn.Eval(const aCode: ustring; var aRetval: ICefV8value; var aException: ICefV8exception): Boolean;
begin
	Result := false;
end;

{Public section}
constructor TCefV8contextOwn.Create;
begin
	inherited CreateData(SizeOf(TCefV8context));
	with PCefV8context(FData)^ do
	begin
		get_task_runner := {$IFDEF FPC}@{$ENDIF}cef_v8context_get_task_runner;
		is_valid := {$IFDEF FPC}@{$ENDIF}cef_v8context_is_valid;
		get_browser := {$IFDEF FPC}@{$ENDIF}cef_v8context_get_browser;
		get_frame := {$IFDEF FPC}@{$ENDIF}cef_v8context_get_frame;
		get_global := {$IFDEF FPC}@{$ENDIF}cef_v8context_get_global;
		enter := {$IFDEF FPC}@{$ENDIF}cef_v8context_enter;
		exit := {$IFDEF FPC}@{$ENDIF}cef_v8context_exit;
		is_same := {$IFDEF FPC}@{$ENDIF}cef_v8context_is_same;
		eval := {$IFDEF FPC}@{$ENDIF}cef_v8context_eval;
	end;
end;
//..............................................................................TCefV8handlerOwn
// Handle execution of the function identified by |name|. |object| is the
// receiver ('this' object) of the function. |arguments| is the list of
// arguments passed to the function. If execution succeeds set |retval| to the
// function return value. If execution fails set |exception| to the exception
// that will be thrown. Return true (1) if execution was handled.
function cef_v8handler_execute(self: PCefV8handler; const name: PCefString; _object: PCefV8value; argumentsCount: csize_t; arguments: PPCefV8value; var retval: PCefV8value; exception: PCefString): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	name_str: ustring;
	retval_proxy: ICefV8value;
	exception_str: ustring;
  arguments_proxy: TCefV8ValueArray;
  i: Integer;
begin
	name_str := TWACef.ToString(name);
	retval_proxy := TCefV8valueOwn(TWACef.GetObject(retval)) as ICefV8value;
	exception_str := TWACef.ToString(exception);

  SetLength(arguments_proxy, argumentsCount);
  for i := 0 to argumentsCount - 1 do
    arguments_proxy[i] := TCefV8valueOwn(TWACef.GetObject(arguments^[i]));

	Result := Ord(
		TCefV8handlerOwn(TWACef.GetObject(self)).Execute(
			name_str,
			TCefV8valueRef.UnWrap(_object),
			argumentsCount,
			arguments_proxy,
			retval_proxy,
			exception_str
		)
	);
	retval := TWACef.GetData(retval_proxy);
	TWACef.StringSet(exception, exception_str);
end;

{Protected section}
function TCefV8handlerOwn.Execute(const aName: ustring; const aObject: ICefV8value; aArgumentsCount: csize_t; const aArguments: TCefv8ValueArray; var aRetval: ICefV8value; var aException: ustring): Boolean;
begin
	Result := false;
end;

{Public section}
constructor TCefV8handlerOwn.Create;
begin
	inherited CreateData(SizeOf(TCefV8handler));
	with PCefV8handler(FData)^ do
	begin
		execute := {$IFDEF FPC}@{$ENDIF}cef_v8handler_execute;
	end;
end;
//..............................................................................TCefV8accessorOwn
// Handle retrieval the accessor value identified by |name|. |object| is the
// receiver ('this' object) of the accessor. If retrieval succeeds set
// |retval| to the return value. If retrieval fails set |exception| to the
// exception that will be thrown. Return true (1) if accessor retrieval was
// handled.
function cef_v8accessor_get(self: PCefV8accessor; const name: PCefString; _object: PCefV8value; var retval: PCefV8value; exception: PCefString): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	name_str: ustring;
	retval_proxy: ICefV8value;
	exception_str: ustring;
begin
	name_str := TWACef.ToString(name);
	retval_proxy := TCefV8valueOwn(TWACef.GetObject(retval)) as ICefV8value;
	exception_str := TWACef.ToString(exception);
	Result := Ord(
		TCefV8accessorOwn(TWACef.GetObject(self)).Get(
			name_str,
			TCefV8valueRef.UnWrap(_object),
			retval_proxy,
			exception_str
		)
	);
	retval := TWACef.GetData(retval_proxy);
	TWACef.StringSet(exception, exception_str);
end;

// Handle assignment of the accessor value identified by |name|. |object| is
// the receiver ('this' object) of the accessor. |value| is the new value
// being assigned to the accessor. If assignment fails set |exception| to the
// exception that will be thrown. Return true (1) if accessor assignment was
// handled.
function cef_v8accessor__set(self: PCefV8accessor; const name: PCefString; _object: PCefV8value; value: PCefV8value; exception: PCefString): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	name_str: ustring;
	exception_str: ustring;
begin
	name_str := TWACef.ToString(name);
	exception_str := TWACef.ToString(exception);
	Result := Ord(
		TCefV8accessorOwn(TWACef.GetObject(self))._Set(
			name_str,
			TCefV8valueRef.UnWrap(_object),
			TCefV8valueRef.UnWrap(value),
			exception_str
		)
	);
	TWACef.StringSet(exception, exception_str);
end;

{Protected section}
function TCefV8accessorOwn.Get(const aName: ustring; const aObject: ICefV8value; out aRetval: ICefV8value; const aException: ustring): Boolean;
begin
	Result := false;
end;

function TCefV8accessorOwn._Set(const aName: ustring; const aObject: ICefV8value; const aValue: ICefV8value; const aException: ustring): Boolean;
begin
	Result := false;
end;

{Public section}
constructor TCefV8accessorOwn.Create;
begin
	inherited CreateData(SizeOf(TCefV8accessor));
	with PCefV8accessor(FData)^ do
	begin
		get := {$IFDEF FPC}@{$ENDIF}cef_v8accessor_get;
		_set := {$IFDEF FPC}@{$ENDIF}cef_v8accessor__set;
	end;
end;
//..............................................................................TCefV8exceptionOwn
// Returns the exception message.
// The resulting string must be freed by calling cef_string_userfree_free().
function cef_v8exception_get_message(self: PCefV8exception): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.UserFreeString(
		TCefV8exceptionOwn(TWACef.GetObject(self)).GetMessage
	);
end;

// Returns the line of source code that the exception occurred within.
// The resulting string must be freed by calling cef_string_userfree_free().
function cef_v8exception_get_source_line(self: PCefV8exception): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.UserFreeString(
		TCefV8exceptionOwn(TWACef.GetObject(self)).GetSourceLine
	);
end;

// Returns the resource name for the script from where the function causing
// the error originates.
// The resulting string must be freed by calling cef_string_userfree_free().
function cef_v8exception_get_script_resource_name(self: PCefV8exception): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.UserFreeString(
		TCefV8exceptionOwn(TWACef.GetObject(self)).GetScriptResourceName
	);
end;

// Returns the 1-based number of the line where the error occurred or 0 if the
// line number is unknown.
function cef_v8exception_get_line_number(self: PCefV8exception): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := (
		TCefV8exceptionOwn(TWACef.GetObject(self)).GetLineNumber
	);
end;

// Returns the index within the script of the first character where the error
// occurred.
function cef_v8exception_get_start_position(self: PCefV8exception): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := (
		TCefV8exceptionOwn(TWACef.GetObject(self)).GetStartPosition
	);
end;

// Returns the index within the script of the last character where the error
// occurred.
function cef_v8exception_get_end_position(self: PCefV8exception): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := (
		TCefV8exceptionOwn(TWACef.GetObject(self)).GetEndPosition
	);
end;

// Returns the index within the line of the first character where the error
// occurred.
function cef_v8exception_get_start_column(self: PCefV8exception): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := (
		TCefV8exceptionOwn(TWACef.GetObject(self)).GetStartColumn
	);
end;

// Returns the index within the line of the last character where the error
// occurred.
function cef_v8exception_get_end_column(self: PCefV8exception): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := (
		TCefV8exceptionOwn(TWACef.GetObject(self)).GetEndColumn
	);
end;

{Protected section}
function TCefV8ExceptionOwn.GetMessage: ustring;
begin
  Result := FMessage;
end;

function TCefV8ExceptionOwn.GetSourceLine: ustring;
begin
  Result := FSourceLine;
end;

function TCefV8ExceptionOwn.GetScriptResourceName: ustring;
begin
  Result := FScriptResourceName;
end;

function TCefV8ExceptionOwn.GetLineNumber: cint;
begin
  Result := FLineNumber;
end;

function TCefV8ExceptionOwn.GetStartPosition: cint;
begin
  Result := FStartPosition;
end;

function TCefV8ExceptionOwn.GetEndPosition: cint;
begin
  Result := FEndPosition;
end;

function TCefV8ExceptionOwn.GetStartColumn: cint;
begin
  Result := FStartColumn;
end;

function TCefV8ExceptionOwn.GetEndColumn;
begin
  Result := FEndColumn;
end;

{Public section}
constructor TCefV8exceptionOwn.Create;
begin
  FMessage := '';
  FSourceLine := '';
  FScriptResourceName := '';
  FLineNumber := 0;
  FStartPosition := 0;
  FEndPosition := 0;
  FStartColumn := 0;
  FEndColumn := 0;

	inherited CreateData(SizeOf(TCefV8exception));
	with PCefV8exception(FData)^ do
	begin
		get_message := {$IFDEF FPC}@{$ENDIF}cef_v8exception_get_message;
		get_source_line := {$IFDEF FPC}@{$ENDIF}cef_v8exception_get_source_line;
		get_script_resource_name := {$IFDEF FPC}@{$ENDIF}cef_v8exception_get_script_resource_name;
		get_line_number := {$IFDEF FPC}@{$ENDIF}cef_v8exception_get_line_number;
		get_start_position := {$IFDEF FPC}@{$ENDIF}cef_v8exception_get_start_position;
		get_end_position := {$IFDEF FPC}@{$ENDIF}cef_v8exception_get_end_position;
		get_start_column := {$IFDEF FPC}@{$ENDIF}cef_v8exception_get_start_column;
		get_end_column := {$IFDEF FPC}@{$ENDIF}cef_v8exception_get_end_column;
	end;
end;
//..............................................................................TCefV8valueOwn
// Returns true (1) if the underlying handle is valid and it can be accessed
// on the current thread. Do not call any other functions if this function
// returns false (0).
function cef_v8value_is_valid(self: PCefV8value): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefV8valueOwn(TWACef.GetObject(self)).IsValid
	);
end;

// True if the value type is undefined.
function cef_v8value_is_undefined(self: PCefV8value): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefV8valueOwn(TWACef.GetObject(self)).IsUndefined
	);
end;

// True if the value type is null.
function cef_v8value_is_null(self: PCefV8value): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefV8valueOwn(TWACef.GetObject(self)).IsNull
	);
end;

// True if the value type is bool.
function cef_v8value_is_bool(self: PCefV8value): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefV8valueOwn(TWACef.GetObject(self)).IsBool
	);
end;

// True if the value type is int.
function cef_v8value_is_int(self: PCefV8value): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefV8valueOwn(TWACef.GetObject(self)).IsInt
	);
end;

// True if the value type is unsigned int.
function cef_v8value_is_uint(self: PCefV8value): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefV8valueOwn(TWACef.GetObject(self)).IsUint
	);
end;

// True if the value type is double.
function cef_v8value_is_double(self: PCefV8value): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefV8valueOwn(TWACef.GetObject(self)).IsDouble
	);
end;

// True if the value type is Date.
function cef_v8value_is_date(self: PCefV8value): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefV8valueOwn(TWACef.GetObject(self)).IsDate
	);
end;

// True if the value type is string.
function cef_v8value_is_string(self: PCefV8value): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefV8valueOwn(TWACef.GetObject(self)).IsString
	);
end;

// True if the value type is object.
function cef_v8value_is_object(self: PCefV8value): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefV8valueOwn(TWACef.GetObject(self)).IsObject
	);
end;

// True if the value type is array.
function cef_v8value_is_array(self: PCefV8value): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefV8valueOwn(TWACef.GetObject(self)).IsArray
	);
end;

// True if the value type is function.
function cef_v8value_is_function(self: PCefV8value): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefV8valueOwn(TWACef.GetObject(self)).IsFunction
	);
end;

// Returns true (1) if this object is pointing to the same handle as |that|
// object.
function cef_v8value_is_same(self: PCefV8value; that: PCefV8value): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefV8valueOwn(TWACef.GetObject(self)).IsSame(
			TCefV8valueRef.UnWrap(that)
		)
	);
end;

// Return a bool value.  The underlying data will be converted to if
// necessary.
function cef_v8value_get_bool_value(self: PCefV8value): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefV8valueOwn(TWACef.GetObject(self)).GetBoolValue
	);
end;

// Return an int value.  The underlying data will be converted to if
// necessary.
function cef_v8value_get_int_value(self: PCefV8value): cint32; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := (
		TCefV8valueOwn(TWACef.GetObject(self)).GetIntValue
	);
end;

// Return an unisgned int value.  The underlying data will be converted to if
// necessary.
function cef_v8value_get_uint_value(self: PCefV8value): cuint32; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := (
		TCefV8valueOwn(TWACef.GetObject(self)).GetUintValue
	);
end;

// Return a double value.  The underlying data will be converted to if
// necessary.
function cef_v8value_get_double_value(self: PCefV8value): cdouble; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := (
		TCefV8valueOwn(TWACef.GetObject(self)).GetDoubleValue
	);
end;

// Return a Date value.  The underlying data will be converted to if
// necessary.
function cef_v8value_get_date_value(self: PCefV8value): TCefTime; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := (
		TCefV8valueOwn(TWACef.GetObject(self)).GetDateValue
	);
end;

// Return a string value.  The underlying data will be converted to if
// necessary.
// The resulting string must be freed by calling cef_string_userfree_free().
function cef_v8value_get_string_value(self: PCefV8value): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.UserFreeString(
		TCefV8valueOwn(TWACef.GetObject(self)).GetStringValue
	);
end;

// Returns true (1) if this is a user created object.
function cef_v8value_is_user_created(self: PCefV8value): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefV8valueOwn(TWACef.GetObject(self)).IsUserCreated
	);
end;

// Returns true (1) if the last function call resulted in an exception. This
// attribute exists only in the scope of the current CEF value object.
function cef_v8value_has_exception(self: PCefV8value): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefV8valueOwn(TWACef.GetObject(self)).HasException
	);
end;

// Returns the exception resulting from the last function call. This attribute
// exists only in the scope of the current CEF value object.
function cef_v8value_get_exception(self: PCefV8value): PCefV8exception; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.GetData(
		TCefV8valueOwn(TWACef.GetObject(self)).GetException
	);
end;

// Clears the last exception and returns true (1) on success.
function cef_v8value_clear_exception(self: PCefV8value): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefV8valueOwn(TWACef.GetObject(self)).ClearException
	);
end;

// Returns true (1) if this object will re-throw future exceptions. This
// attribute exists only in the scope of the current CEF value object.
function cef_v8value_will_rethrow_exceptions(self: PCefV8value): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefV8valueOwn(TWACef.GetObject(self)).WillRethrowExceptions
	);
end;

// Set whether this object will re-throw future exceptions. By default
// exceptions are not re-thrown. If a exception is re-thrown the current
// context should not be accessed again until after the exception has been
// caught and not re-thrown. Returns true (1) on success. This attribute
// exists only in the scope of the current CEF value object.
function cef_v8value_set_rethrow_exceptions(self: PCefV8value; rethrow: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefV8valueOwn(TWACef.GetObject(self)).SetRethrowExceptions(
			rethrow <> 0
		)
	);
end;

// Returns true (1) if the object has a value with the specified identifier.
function cef_v8value_has_value_bykey(self: PCefV8value; const key: PCefString): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	key_str: ustring;
begin
	key_str := TWACef.ToString(key);
	Result := Ord(
		TCefV8valueOwn(TWACef.GetObject(self)).HasValueBykey(
			key_str
		)
	);
end;

// Returns true (1) if the object has a value with the specified identifier.
function cef_v8value_has_value_byindex(self: PCefV8value; index: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefV8valueOwn(TWACef.GetObject(self)).HasValueByindex(
			index
		)
	);
end;

// Deletes the value with the specified identifier and returns true (1) on
// success. Returns false (0) if this function is called incorrectly or an
// exception is thrown. For read-only and don't-delete values this function
// will return true (1) even though deletion failed.
function cef_v8value_delete_value_bykey(self: PCefV8value; const key: PCefString): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	key_str: ustring;
begin
	key_str := TWACef.ToString(key);
	Result := Ord(
		TCefV8valueOwn(TWACef.GetObject(self)).DeleteValueBykey(
			key_str
		)
	);
end;

// Deletes the value with the specified identifier and returns true (1) on
// success. Returns false (0) if this function is called incorrectly, deletion
// fails or an exception is thrown. For read-only and don't-delete values this
// function will return true (1) even though deletion failed.
function cef_v8value_delete_value_byindex(self: PCefV8value; index: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefV8valueOwn(TWACef.GetObject(self)).DeleteValueByindex(
			index
		)
	);
end;

// Returns the value with the specified identifier on success. Returns NULL if
// this function is called incorrectly or an exception is thrown.
function cef_v8value_get_value_bykey(self: PCefV8value; const key: PCefString): PCefV8value; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	key_str: ustring;
begin
	key_str := TWACef.ToString(key);
	Result := TWACef.GetData(
		TCefV8valueOwn(TWACef.GetObject(self)).GetValueBykey(
			key_str
		)
	);
end;

// Returns the value with the specified identifier on success. Returns NULL if
// this function is called incorrectly or an exception is thrown.
function cef_v8value_get_value_byindex(self: PCefV8value; index: cint): PCefV8value; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.GetData(
		TCefV8valueOwn(TWACef.GetObject(self)).GetValueByindex(
			index
		)
	);
end;

// Associates a value with the specified identifier and returns true (1) on
// success. Returns false (0) if this function is called incorrectly or an
// exception is thrown. For read-only values this function will return true
// (1) even though assignment failed.
function cef_v8value_set_value_bykey(self: PCefV8value; const key: PCefString; value: PCefV8value; attribute: TCefV8Propertyattribute): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	key_str: ustring;
begin
	key_str := TWACef.ToString(key);
	Result := Ord(
		TCefV8valueOwn(TWACef.GetObject(self)).SetValueBykey(
			key_str,
			TCefV8valueRef.UnWrap(value),
			attribute
		)
	);
end;

// Associates a value with the specified identifier and returns true (1) on
// success. Returns false (0) if this function is called incorrectly or an
// exception is thrown. For read-only values this function will return true
// (1) even though assignment failed.
function cef_v8value_set_value_byindex(self: PCefV8value; index: cint; value: PCefV8value): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefV8valueOwn(TWACef.GetObject(self)).SetValueByindex(
			index,
			TCefV8valueRef.UnWrap(value)
		)
	);
end;

// Registers an identifier and returns true (1) on success. Access to the
// identifier will be forwarded to the cef_v8accessor_t instance passed to
// cef_v8value_t::cef_v8value_create_object(). Returns false (0) if this
// function is called incorrectly or an exception is thrown. For read-only
// values this function will return true (1) even though assignment failed.
function cef_v8value_set_value_byaccessor(self: PCefV8value; const key: PCefString; settings: TCefV8Accesscontrol; attribute: TCefV8Propertyattribute): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	key_str: ustring;
begin
	key_str := TWACef.ToString(key);
	Result := Ord(
		TCefV8valueOwn(TWACef.GetObject(self)).SetValueByaccessor(
			key_str,
			settings,
			attribute
		)
	);
end;

// Read the keys for the object's values into the specified vector. Integer-
// based keys will also be returned as strings.
function cef_v8value_get_keys(self: PCefV8value; keys: TCefStringList): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	keys_list: TSTringList;
	keys_iter: Integer;
	keys_item: TCefString;
begin
	keys_list := TStringList.Create;
	for keys_iter := 0 to cef_string_list_size(keys) - 1 do
	begin
		FillChar(keys_item, SizeOf(keys_item), 0);
		cef_string_list_value(keys, keys_iter, @keys_item);
		keys_list.Add(TWACef.StringClearAndGet(keys_item));
	end;

  Result := Ord(
		TCefV8valueOwn(TWACef.GetObject(self)).GetKeys(
			keys_list
		)
	);

	keys_list.Free;
end;

// Sets the user data for this object and returns true (1) on success. Returns
// false (0) if this function is called incorrectly. This function can only be
// called on user created objects.
function cef_v8value_set_user_data(self: PCefV8value; user_data: PCefBase): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefV8valueOwn(TWACef.GetObject(self)).SetUserData(
			TCefBaseRef.UnWrap(user_data)
		)
	);
end;

// Returns the user data, if any, assigned to this object.
function cef_v8value_get_user_data(self: PCefV8value): PCefBase; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.GetData(
		TCefV8valueOwn(TWACef.GetObject(self)).GetUserData
	);
end;

// Returns the amount of externally allocated memory registered for the
// object.
function cef_v8value_get_externally_allocated_memory(self: PCefV8value): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := (
		TCefV8valueOwn(TWACef.GetObject(self)).GetExternallyAllocatedMemory
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
function cef_v8value_adjust_externally_allocated_memory(self: PCefV8value; change_in_bytes: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := (
		TCefV8valueOwn(TWACef.GetObject(self)).AdjustExternallyAllocatedMemory(
			change_in_bytes
		)
	);
end;

// Returns the number of elements in the array.
function cef_v8value_get_array_length(self: PCefV8value): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := (
		TCefV8valueOwn(TWACef.GetObject(self)).GetArrayLength
	);
end;

// Returns the function name.
// The resulting string must be freed by calling cef_string_userfree_free().
function cef_v8value_get_function_name(self: PCefV8value): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.UserFreeString(
		TCefV8valueOwn(TWACef.GetObject(self)).GetFunctionName
	);
end;

// Returns the function handler or NULL if not a CEF-created function.
function cef_v8value_get_function_handler(self: PCefV8value): PCefV8handler; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.GetData(
		TCefV8valueOwn(TWACef.GetObject(self)).GetFunctionHandler
	);
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
function cef_v8value_execute_function(self: PCefV8value; _object: PCefV8value; argumentsCount: csize_t; arguments: PPCefV8value): PCefV8value; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
  arguments_proxy: TCefV8ValueArray;
  i: Integer;
begin
  SetLength(arguments_proxy, argumentsCount);
  for i := 0 to argumentsCount - 1 do
    arguments_proxy[i] := TCefV8valueOwn(TWACef.GetObject(arguments^[i]));

	Result := TWACef.GetData(
		TCefV8valueOwn(TWACef.GetObject(self)).ExecuteFunction(
			TCefV8valueRef.UnWrap(_object),
			argumentsCount,
			arguments_proxy
		)
	);
end;

// Execute the function using the specified V8 context. |object| is the
// receiver ('this' object) of the function. If |object| is NULL the specified
// context's global object will be used. |arguments| is the list of arguments
// that will be passed to the function. Returns the function return value on
// success. Returns NULL if this function is called incorrectly or an
// exception is thrown.
function cef_v8value_execute_function_with_context(self: PCefV8value; context: PCefV8context; _object: PCefV8value; argumentsCount: csize_t; arguments: PPCefV8value): PCefV8value; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
  arguments_proxy: TCefV8ValueArray;
  i: Integer;
begin
  SetLength(arguments_proxy, argumentsCount);
  for i := 0 to argumentsCount - 1 do
    arguments_proxy[i] := TCefV8valueOwn(TWACef.GetObject(arguments^[i]));

	Result := TWACef.GetData(
		TCefV8valueOwn(TWACef.GetObject(self)).ExecuteFunctionWithContext(
			TCefV8contextRef.UnWrap(context),
			TCefV8valueRef.UnWrap(_object),
			argumentsCount,
			arguments_proxy
		)
	);
end;

{Protected section}
function TCefV8valueOwn.IsValid: Boolean;
begin
	Result := false;
end;

function TCefV8valueOwn.IsUndefined: Boolean;
begin
	Result := false;
end;

function TCefV8valueOwn.IsNull: Boolean;
begin
	Result := false;
end;

function TCefV8valueOwn.IsBool: Boolean;
begin
	Result := false;
end;

function TCefV8valueOwn.IsInt: Boolean;
begin
	Result := false;
end;

function TCefV8valueOwn.IsUint: Boolean;
begin
	Result := false;
end;

function TCefV8valueOwn.IsDouble: Boolean;
begin
	Result := false;
end;

function TCefV8valueOwn.IsDate: Boolean;
begin
	Result := false;
end;

function TCefV8valueOwn.IsString: Boolean;
begin
	Result := false;
end;

function TCefV8valueOwn.IsObject: Boolean;
begin
	Result := false;
end;

function TCefV8valueOwn.IsArray: Boolean;
begin
	Result := false;
end;

function TCefV8valueOwn.IsFunction: Boolean;
begin
	Result := false;
end;

function TCefV8valueOwn.IsSame(const aThat: ICefV8value): Boolean;
begin
	Result := false;
end;

function TCefV8valueOwn.GetBoolValue: Boolean;
begin
	Result := false;
end;

function TCefV8valueOwn.GetIntValue: cint32;
begin
end;

function TCefV8valueOwn.GetUintValue: cuint32;
begin
end;

function TCefV8valueOwn.GetDoubleValue: cdouble;
begin
end;

function TCefV8valueOwn.GetDateValue: TCefTime;
begin
end;

function TCefV8valueOwn.GetStringValue: ustring;
begin
end;

function TCefV8valueOwn.IsUserCreated: Boolean;
begin
	Result := false;
end;

function TCefV8valueOwn.HasException: Boolean;
begin
	Result := false;
end;

function TCefV8valueOwn.GetException: ICefV8exception;
begin
	Result := nil;
end;

function TCefV8valueOwn.ClearException: Boolean;
begin
	Result := false;
end;

function TCefV8valueOwn.WillRethrowExceptions: Boolean;
begin
	Result := false;
end;

function TCefV8valueOwn.SetRethrowExceptions(aRethrow: Boolean): Boolean;
begin
	Result := false;
end;

function TCefV8valueOwn.HasValueBykey(const aKey: ustring): Boolean;
begin
	Result := false;
end;

function TCefV8valueOwn.HasValueByindex(aIndex: cint): Boolean;
begin
	Result := false;
end;

function TCefV8valueOwn.DeleteValueBykey(const aKey: ustring): Boolean;
begin
	Result := false;
end;

function TCefV8valueOwn.DeleteValueByindex(aIndex: cint): Boolean;
begin
	Result := false;
end;

function TCefV8valueOwn.GetValueBykey(const aKey: ustring): ICefV8value;
begin
	Result := nil;
end;

function TCefV8valueOwn.GetValueByindex(aIndex: cint): ICefV8value;
begin
	Result := nil;
end;

function TCefV8valueOwn.SetValueBykey(const aKey: ustring; const aValue: ICefV8value; aAttribute: TCefV8Propertyattribute): Boolean;
begin
	Result := false;
end;

function TCefV8valueOwn.SetValueByindex(aIndex: cint; const aValue: ICefV8value): Boolean;
begin
	Result := false;
end;

function TCefV8valueOwn.SetValueByaccessor(const aKey: ustring; aSettings: TCefV8Accesscontrol; aAttribute: TCefV8Propertyattribute): Boolean;
begin
	Result := false;
end;

function TCefV8valueOwn.GetKeys(aKeys: TStrings): Boolean;
begin
	Result := false;
end;

function TCefV8valueOwn.SetUserData(const aUserData: ICefBase): Boolean;
begin
	Result := false;
end;

function TCefV8valueOwn.GetUserData: ICefBase;
begin
	Result := nil;
end;

function TCefV8valueOwn.GetExternallyAllocatedMemory: cint;
begin
	Result := 0;
end;

function TCefV8valueOwn.AdjustExternallyAllocatedMemory(aChangeInBytes: cint): cint;
begin
	Result := 0;
end;

function TCefV8valueOwn.GetArrayLength: cint;
begin
	Result := 0;
end;

function TCefV8valueOwn.GetFunctionName: ustring;
begin
end;

function TCefV8valueOwn.GetFunctionHandler: ICefV8handler;
begin
	Result := nil;
end;

function TCefV8valueOwn.ExecuteFunction(const aObject: ICefV8value; aArgumentsCount: csize_t; const aArguments: TCefV8ValueArray): ICefV8value;
begin
	Result := nil;
end;

function TCefV8valueOwn.ExecuteFunctionWithContext(const aContext: ICefV8context; const aObject: ICefV8value; aArgumentsCount: csize_t; const aArguments: TCefV8ValueArray): ICefV8value;
begin
	Result := nil;
end;

{Public section}
constructor TCefV8valueOwn.Create;
begin
	inherited CreateData(SizeOf(TCefV8value));
	with PCefV8value(FData)^ do
	begin
		is_valid := {$IFDEF FPC}@{$ENDIF}cef_v8value_is_valid;
		is_undefined := {$IFDEF FPC}@{$ENDIF}cef_v8value_is_undefined;
		is_null := {$IFDEF FPC}@{$ENDIF}cef_v8value_is_null;
		is_bool := {$IFDEF FPC}@{$ENDIF}cef_v8value_is_bool;
		is_int := {$IFDEF FPC}@{$ENDIF}cef_v8value_is_int;
		is_uint := {$IFDEF FPC}@{$ENDIF}cef_v8value_is_uint;
		is_double := {$IFDEF FPC}@{$ENDIF}cef_v8value_is_double;
		is_date := {$IFDEF FPC}@{$ENDIF}cef_v8value_is_date;
		is_string := {$IFDEF FPC}@{$ENDIF}cef_v8value_is_string;
		is_object := {$IFDEF FPC}@{$ENDIF}cef_v8value_is_object;
		is_array := {$IFDEF FPC}@{$ENDIF}cef_v8value_is_array;
		is_function := {$IFDEF FPC}@{$ENDIF}cef_v8value_is_function;
		is_same := {$IFDEF FPC}@{$ENDIF}cef_v8value_is_same;
		get_bool_value := {$IFDEF FPC}@{$ENDIF}cef_v8value_get_bool_value;
		get_int_value := {$IFDEF FPC}@{$ENDIF}cef_v8value_get_int_value;
		get_uint_value := {$IFDEF FPC}@{$ENDIF}cef_v8value_get_uint_value;
		get_double_value := {$IFDEF FPC}@{$ENDIF}cef_v8value_get_double_value;
		get_date_value := {$IFDEF FPC}@{$ENDIF}cef_v8value_get_date_value;
		get_string_value := {$IFDEF FPC}@{$ENDIF}cef_v8value_get_string_value;
		is_user_created := {$IFDEF FPC}@{$ENDIF}cef_v8value_is_user_created;
		has_exception := {$IFDEF FPC}@{$ENDIF}cef_v8value_has_exception;
		get_exception := {$IFDEF FPC}@{$ENDIF}cef_v8value_get_exception;
		clear_exception := {$IFDEF FPC}@{$ENDIF}cef_v8value_clear_exception;
		will_rethrow_exceptions := {$IFDEF FPC}@{$ENDIF}cef_v8value_will_rethrow_exceptions;
		set_rethrow_exceptions := {$IFDEF FPC}@{$ENDIF}cef_v8value_set_rethrow_exceptions;
		has_value_bykey := {$IFDEF FPC}@{$ENDIF}cef_v8value_has_value_bykey;
		has_value_byindex := {$IFDEF FPC}@{$ENDIF}cef_v8value_has_value_byindex;
		delete_value_bykey := {$IFDEF FPC}@{$ENDIF}cef_v8value_delete_value_bykey;
		delete_value_byindex := {$IFDEF FPC}@{$ENDIF}cef_v8value_delete_value_byindex;
		get_value_bykey := {$IFDEF FPC}@{$ENDIF}cef_v8value_get_value_bykey;
		get_value_byindex := {$IFDEF FPC}@{$ENDIF}cef_v8value_get_value_byindex;
		set_value_bykey := {$IFDEF FPC}@{$ENDIF}cef_v8value_set_value_bykey;
		set_value_byindex := {$IFDEF FPC}@{$ENDIF}cef_v8value_set_value_byindex;
		set_value_byaccessor := {$IFDEF FPC}@{$ENDIF}cef_v8value_set_value_byaccessor;
		get_keys := {$IFDEF FPC}@{$ENDIF}cef_v8value_get_keys;
		set_user_data := {$IFDEF FPC}@{$ENDIF}cef_v8value_set_user_data;
		get_user_data := {$IFDEF FPC}@{$ENDIF}cef_v8value_get_user_data;
		get_externally_allocated_memory := {$IFDEF FPC}@{$ENDIF}cef_v8value_get_externally_allocated_memory;
		adjust_externally_allocated_memory := {$IFDEF FPC}@{$ENDIF}cef_v8value_adjust_externally_allocated_memory;
		get_array_length := {$IFDEF FPC}@{$ENDIF}cef_v8value_get_array_length;
		get_function_name := {$IFDEF FPC}@{$ENDIF}cef_v8value_get_function_name;
		get_function_handler := {$IFDEF FPC}@{$ENDIF}cef_v8value_get_function_handler;
		execute_function := {$IFDEF FPC}@{$ENDIF}cef_v8value_execute_function;
		execute_function_with_context := {$IFDEF FPC}@{$ENDIF}cef_v8value_execute_function_with_context;
	end;
end;
//..............................................................................TCefV8stackTraceOwn
// Returns true (1) if the underlying handle is valid and it can be accessed
// on the current thread. Do not call any other functions if this function
// returns false (0).
function cef_v8stack_trace_is_valid(self: PCefV8stackTrace): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefV8stackTraceOwn(TWACef.GetObject(self)).IsValid
	);
end;

// Returns the number of stack frames.
function cef_v8stack_trace_get_frame_count(self: PCefV8stackTrace): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := (
		TCefV8stackTraceOwn(TWACef.GetObject(self)).GetFrameCount
	);
end;

// Returns the stack frame at the specified 0-based index.
function cef_v8stack_trace_get_frame(self: PCefV8stackTrace; index: cint): PCefV8stackFrame; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.GetData(
		TCefV8stackTraceOwn(TWACef.GetObject(self)).GetFrame(
			index
		)
	);
end;

{Protected section}
function TCefV8stackTraceOwn.IsValid: Boolean;
begin
	Result := false;
end;

function TCefV8stackTraceOwn.GetFrameCount: cint;
begin
	Result := 0;
end;

function TCefV8stackTraceOwn.GetFrame(aIndex: cint): ICefV8stackFrame;
begin
	Result := nil;
end;

{Public section}
constructor TCefV8stackTraceOwn.Create;
begin
	inherited CreateData(SizeOf(TCefV8stackTrace));
	with PCefV8stackTrace(FData)^ do
	begin
		is_valid := {$IFDEF FPC}@{$ENDIF}cef_v8stack_trace_is_valid;
		get_frame_count := {$IFDEF FPC}@{$ENDIF}cef_v8stack_trace_get_frame_count;
		get_frame := {$IFDEF FPC}@{$ENDIF}cef_v8stack_trace_get_frame;
	end;
end;
//..............................................................................TCefV8stackFrameOwn
// Returns true (1) if the underlying handle is valid and it can be accessed
// on the current thread. Do not call any other functions if this function
// returns false (0).
function cef_v8stack_frame_is_valid(self: PCefV8stackFrame): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefV8stackFrameOwn(TWACef.GetObject(self)).IsValid
	);
end;

// Returns the name of the resource script that contains the function.
// The resulting string must be freed by calling cef_string_userfree_free().
function cef_v8stack_frame_get_script_name(self: PCefV8stackFrame): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.UserFreeString(
		TCefV8stackFrameOwn(TWACef.GetObject(self)).GetScriptName
	);
end;

// Returns the name of the resource script that contains the function or the
// sourceURL value if the script name is undefined and its source ends with a
// "//@ sourceURL=..." string.
// The resulting string must be freed by calling cef_string_userfree_free().
function cef_v8stack_frame_get_script_name_or_source_url(self: PCefV8stackFrame): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.UserFreeString(
		TCefV8stackFrameOwn(TWACef.GetObject(self)).GetScriptNameOrSourceUrl
	);
end;

// Returns the name of the function.
// The resulting string must be freed by calling cef_string_userfree_free().
function cef_v8stack_frame_get_function_name(self: PCefV8stackFrame): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.UserFreeString(
		TCefV8stackFrameOwn(TWACef.GetObject(self)).GetFunctionName
	);
end;

// Returns the 1-based line number for the function call or 0 if unknown.
function cef_v8stack_frame_get_line_number(self: PCefV8stackFrame): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := (
		TCefV8stackFrameOwn(TWACef.GetObject(self)).GetLineNumber
	);
end;

// Returns the 1-based column offset on the line for the function call or 0 if
// unknown.
function cef_v8stack_frame_get_column(self: PCefV8stackFrame): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := (
		TCefV8stackFrameOwn(TWACef.GetObject(self)).GetColumn
	);
end;

// Returns true (1) if the function was compiled using eval().
function cef_v8stack_frame_is_eval(self: PCefV8stackFrame): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefV8stackFrameOwn(TWACef.GetObject(self)).IsEval
	);
end;

// Returns true (1) if the function was called as a constructor via "new".
function cef_v8stack_frame_is_constructor(self: PCefV8stackFrame): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefV8stackFrameOwn(TWACef.GetObject(self)).IsConstructor
	);
end;

{Protected section}
function TCefV8stackFrameOwn.IsValid: Boolean;
begin
	Result := false;
end;

function TCefV8stackFrameOwn.GetScriptName: ustring;
begin
end;

function TCefV8stackFrameOwn.GetScriptNameOrSourceUrl: ustring;
begin
end;

function TCefV8stackFrameOwn.GetFunctionName: ustring;
begin
end;

function TCefV8stackFrameOwn.GetLineNumber: cint;
begin
	Result := 0;
end;

function TCefV8stackFrameOwn.GetColumn: cint;
begin
	Result := 0;
end;

function TCefV8stackFrameOwn.IsEval: Boolean;
begin
	Result := false;
end;

function TCefV8stackFrameOwn.IsConstructor: Boolean;
begin
	Result := false;
end;

{Public section}
constructor TCefV8stackFrameOwn.Create;
begin
	inherited CreateData(SizeOf(TCefV8stackFrame));
	with PCefV8stackFrame(FData)^ do
	begin
		is_valid := {$IFDEF FPC}@{$ENDIF}cef_v8stack_frame_is_valid;
		get_script_name := {$IFDEF FPC}@{$ENDIF}cef_v8stack_frame_get_script_name;
		get_script_name_or_source_url := {$IFDEF FPC}@{$ENDIF}cef_v8stack_frame_get_script_name_or_source_url;
		get_function_name := {$IFDEF FPC}@{$ENDIF}cef_v8stack_frame_get_function_name;
		get_line_number := {$IFDEF FPC}@{$ENDIF}cef_v8stack_frame_get_line_number;
		get_column := {$IFDEF FPC}@{$ENDIF}cef_v8stack_frame_get_column;
		is_eval := {$IFDEF FPC}@{$ENDIF}cef_v8stack_frame_is_eval;
		is_constructor := {$IFDEF FPC}@{$ENDIF}cef_v8stack_frame_is_constructor;
	end;
end;

//..............................................................................TCefValueOwn
// Returns true (1) if the underlying data is valid. This will always be true
// (1) for simple types. For complex types (binary, dictionary and list) the
// underlying data may become invalid if owned by another object (e.g. list or
// dictionary) and that other object is then modified or destroyed. This value
// object can be re-used by calling Set*() even if the underlying data is
// invalid.
function cef_value_is_valid(self: PCefValue): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefValueOwn(TWACef.GetObject(self)).IsValid
	);
end;

// Returns true (1) if the underlying data is owned by another object.
function cef_value_is_owned(self: PCefValue): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefValueOwn(TWACef.GetObject(self)).IsOwned
	);
end;

// Returns true (1) if the underlying data is read-only. Some APIs may expose
// read-only objects.
function cef_value_is_read_only(self: PCefValue): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefValueOwn(TWACef.GetObject(self)).IsReadOnly
	);
end;

// Returns true (1) if this object and |that| object have the same underlying
// data. If true (1) modifications to this object will also affect |that|
// object and vice-versa.
function cef_value_is_same(self: PCefValue; that: PCefValue): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefValueOwn(TWACef.GetObject(self)).IsSame(
			TCefValueRef.UnWrap(that)
		)
	);
end;

// Returns true (1) if this object and |that| object have an equivalent
// underlying value but are not necessarily the same object.
function cef_value_is_equal(self: PCefValue; that: PCefValue): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefValueOwn(TWACef.GetObject(self)).IsEqual(
			TCefValueRef.UnWrap(that)
		)
	);
end;

// Returns a copy of this object. The underlying data will also be copied.
function cef_value_copy(self: PCefValue): PCefValue; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.GetData(
		TCefValueOwn(TWACef.GetObject(self)).Copy
	);
end;

// Returns the underlying value type.
function cef_value_get_type(self: PCefValue): TCefValueType; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := (
		TCefValueOwn(TWACef.GetObject(self)).GetType
	);
end;

// Returns the underlying value as type bool.
function cef_value_get_bool(self: PCefValue): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefValueOwn(TWACef.GetObject(self)).GetBool
	);
end;

// Returns the underlying value as type int.
function cef_value_get_int(self: PCefValue): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := (
		TCefValueOwn(TWACef.GetObject(self)).GetInt
	);
end;

// Returns the underlying value as type double.
function cef_value_get_double(self: PCefValue): cdouble; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := (
		TCefValueOwn(TWACef.GetObject(self)).GetDouble
	);
end;

// Returns the underlying value as type string.
// The resulting string must be freed by calling cef_string_userfree_free().
function cef_value_get_string(self: PCefValue): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.UserFreeString(
		TCefValueOwn(TWACef.GetObject(self)).GetString
	);
end;

// Returns the underlying value as type binary. The returned reference may
// become invalid if the value is owned by another object or if ownership is
// transferred to another object in the future. To maintain a reference to the
// value after assigning ownership to a dictionary or list pass this object to
// the set_value() function instead of passing the returned reference to
// set_binary().
function cef_value_get_binary(self: PCefValue): PCefBinaryValue; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.GetData(
		TCefValueOwn(TWACef.GetObject(self)).GetBinary
	);
end;

// Returns the underlying value as type dictionary. The returned reference may
// become invalid if the value is owned by another object or if ownership is
// transferred to another object in the future. To maintain a reference to the
// value after assigning ownership to a dictionary or list pass this object to
// the set_value() function instead of passing the returned reference to
// set_dictionary().
function cef_value_get_dictionary(self: PCefValue): PCefDictionaryValue; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.GetData(
		TCefValueOwn(TWACef.GetObject(self)).GetDictionary
	);
end;

// Returns the underlying value as type list. The returned reference may
// become invalid if the value is owned by another object or if ownership is
// transferred to another object in the future. To maintain a reference to the
// value after assigning ownership to a dictionary or list pass this object to
// the set_value() function instead of passing the returned reference to
// set_list().
function cef_value_get_list(self: PCefValue): PCefListValue; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.GetData(
		TCefValueOwn(TWACef.GetObject(self)).GetList
	);
end;

// Sets the underlying value as type null. Returns true (1) if the value was
// set successfully.
function cef_value_set_null(self: PCefValue): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefValueOwn(TWACef.GetObject(self)).SetNull
	);
end;

// Sets the underlying value as type bool. Returns true (1) if the value was
// set successfully.
function cef_value_set_bool(self: PCefValue; value: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefValueOwn(TWACef.GetObject(self)).SetBool(
      value <> 0
		)
	);
end;

// Sets the underlying value as type int. Returns true (1) if the value was
// set successfully.
function cef_value_set_int(self: PCefValue; value: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefValueOwn(TWACef.GetObject(self)).SetInt(
			value
		)
	);
end;

// Sets the underlying value as type double. Returns true (1) if the value was
// set successfully.
function cef_value_set_double(self: PCefValue; value: cdouble): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefValueOwn(TWACef.GetObject(self)).SetDouble(
			value
		)
	);
end;

// Sets the underlying value as type string. Returns true (1) if the value was
// set successfully.
function cef_value_set_string(self: PCefValue; const value: PCefString): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	value_str: ustring;
begin
	value_str := TWACef.ToString(value);
	Result := Ord(
		TCefValueOwn(TWACef.GetObject(self)).SetString(
			value_str
		)
	);
end;

// Sets the underlying value as type binary. Returns true (1) if the value was
// set successfully. This object keeps a reference to |value| and ownership of
// the underlying data remains unchanged.
function cef_value_set_binary(self: PCefValue; value: PCefBinaryValue): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefValueOwn(TWACef.GetObject(self)).SetBinary(
			TCefBinaryValueRef.UnWrap(value)
		)
	);
end;

// Sets the underlying value as type dict. Returns true (1) if the value was
// set successfully. This object keeps a reference to |value| and ownership of
// the underlying data remains unchanged.
function cef_value_set_dictionary(self: PCefValue; value: PCefDictionaryValue): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefValueOwn(TWACef.GetObject(self)).SetDictionary(
			TCefDictionaryValueRef.UnWrap(value)
		)
	);
end;

// Sets the underlying value as type list. Returns true (1) if the value was
// set successfully. This object keeps a reference to |value| and ownership of
// the underlying data remains unchanged.
function cef_value_set_list(self: PCefValue; value: PCefListValue): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefValueOwn(TWACef.GetObject(self)).SetList(
			TCefListValueRef.UnWrap(value)
		)
	);
end;

{Protected section}
function TCefValueOwn.IsValid: Boolean;
begin
	Result := false;
end;

function TCefValueOwn.IsOwned: Boolean;
begin
	Result := false;
end;

function TCefValueOwn.IsReadOnly: Boolean;
begin
	Result := false;
end;

function TCefValueOwn.IsSame(const aThat: ICefValue): Boolean;
begin
	Result := false;
end;

function TCefValueOwn.IsEqual(const aThat: ICefValue): Boolean;
begin
	Result := false;
end;

function TCefValueOwn.Copy: ICefValue;
begin
	Result := nil;
end;

function TCefValueOwn.GetType: TCefValueType;
begin
end;

function TCefValueOwn.GetBool: Boolean;
begin
	Result := false;
end;

function TCefValueOwn.GetInt: cint;
begin
	Result := 0;
end;

function TCefValueOwn.GetDouble: cdouble;
begin
end;

function TCefValueOwn.GetString: ustring;
begin
end;

function TCefValueOwn.GetBinary: ICefBinaryValue;
begin
	Result := nil;
end;

function TCefValueOwn.GetDictionary: ICefDictionaryValue;
begin
	Result := nil;
end;

function TCefValueOwn.GetList: ICefListValue;
begin
	Result := nil;
end;

function TCefValueOwn.SetNull: Boolean;
begin
	Result := false;
end;

function TCefValueOwn.SetBool(aValue: Boolean): Boolean;
begin
	Result := false;
end;

function TCefValueOwn.SetInt(aValue: cint): Boolean;
begin
	Result := false;
end;

function TCefValueOwn.SetDouble(aValue: cdouble): Boolean;
begin
	Result := false;
end;

function TCefValueOwn.SetString(const aValue: ustring): Boolean;
begin
	Result := false;
end;

function TCefValueOwn.SetBinary(const aValue: ICefBinaryValue): Boolean;
begin
	Result := false;
end;

function TCefValueOwn.SetDictionary(const aValue: ICefDictionaryValue): Boolean;
begin
	Result := false;
end;

function TCefValueOwn.SetList(const aValue: ICefListValue): Boolean;
begin
	Result := false;
end;

{Public section}
constructor TCefValueOwn.Create;
begin
	inherited CreateData(SizeOf(TCefValue));
	with PCefValue(FData)^ do
	begin
		is_valid := {$IFDEF FPC}@{$ENDIF}cef_value_is_valid;
		is_owned := {$IFDEF FPC}@{$ENDIF}cef_value_is_owned;
		is_read_only := {$IFDEF FPC}@{$ENDIF}cef_value_is_read_only;
		is_same := {$IFDEF FPC}@{$ENDIF}cef_value_is_same;
		is_equal := {$IFDEF FPC}@{$ENDIF}cef_value_is_equal;
		copy := {$IFDEF FPC}@{$ENDIF}cef_value_copy;
		get_type := {$IFDEF FPC}@{$ENDIF}cef_value_get_type;
		get_bool := {$IFDEF FPC}@{$ENDIF}cef_value_get_bool;
		get_int := {$IFDEF FPC}@{$ENDIF}cef_value_get_int;
		get_double := {$IFDEF FPC}@{$ENDIF}cef_value_get_double;
		get_string := {$IFDEF FPC}@{$ENDIF}cef_value_get_string;
		get_binary := {$IFDEF FPC}@{$ENDIF}cef_value_get_binary;
		get_dictionary := {$IFDEF FPC}@{$ENDIF}cef_value_get_dictionary;
		get_list := {$IFDEF FPC}@{$ENDIF}cef_value_get_list;
		set_null := {$IFDEF FPC}@{$ENDIF}cef_value_set_null;
		set_bool := {$IFDEF FPC}@{$ENDIF}cef_value_set_bool;
		set_int := {$IFDEF FPC}@{$ENDIF}cef_value_set_int;
		set_double := {$IFDEF FPC}@{$ENDIF}cef_value_set_double;
		set_string := {$IFDEF FPC}@{$ENDIF}cef_value_set_string;
		set_binary := {$IFDEF FPC}@{$ENDIF}cef_value_set_binary;
		set_dictionary := {$IFDEF FPC}@{$ENDIF}cef_value_set_dictionary;
		set_list := {$IFDEF FPC}@{$ENDIF}cef_value_set_list;
	end;
end;
//..............................................................................TCefBinaryValueOwn
// Returns true (1) if this object is valid. This object may become invalid if
// the underlying data is owned by another object (e.g. list or dictionary)
// and that other object is then modified or destroyed. Do not call any other
// functions if this function returns false (0).
function cef_binary_value_is_valid(self: PCefBinaryValue): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefBinaryValueOwn(TWACef.GetObject(self)).IsValid
	);
end;

// Returns true (1) if this object is currently owned by another object.
function cef_binary_value_is_owned(self: PCefBinaryValue): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefBinaryValueOwn(TWACef.GetObject(self)).IsOwned
	);
end;

// Returns true (1) if this object and |that| object have the same underlying
// data.
function cef_binary_value_is_same(self: PCefBinaryValue; that: PCefBinaryValue): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefBinaryValueOwn(TWACef.GetObject(self)).IsSame(
			TCefBinaryValueRef.UnWrap(that)
		)
	);
end;

// Returns true (1) if this object and |that| object have an equivalent
// underlying value but are not necessarily the same object.
function cef_binary_value_is_equal(self: PCefBinaryValue; that: PCefBinaryValue): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefBinaryValueOwn(TWACef.GetObject(self)).IsEqual(
			TCefBinaryValueRef.UnWrap(that)
		)
	);
end;

// Returns a copy of this object. The data in this object will also be copied.
function cef_binary_value_copy(self: PCefBinaryValue): PCefBinaryValue; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.GetData(
		TCefBinaryValueOwn(TWACef.GetObject(self)).Copy
	);
end;

// Returns the data size.
function cef_binary_value_get_size(self: PCefBinaryValue): csize_t; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := (
		TCefBinaryValueOwn(TWACef.GetObject(self)).GetSize
	);
end;

// Read up to |buffer_size| number of bytes into |buffer|. Reading begins at
// the specified byte |data_offset|. Returns the number of bytes read.
function cef_binary_value_get_data(self: PCefBinaryValue; buffer: cvoid; buffer_size: csize_t; data_offset: csize_t): csize_t; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := (
		TCefBinaryValueOwn(TWACef.GetObject(self)).GetData(
			buffer,
			buffer_size,
			data_offset
		)
	);
end;

{Protected section}
function TCefBinaryValueOwn.IsValid: Boolean;
begin
	Result := false;
end;

function TCefBinaryValueOwn.IsOwned: Boolean;
begin
	Result := false;
end;

function TCefBinaryValueOwn.IsSame(const aThat: ICefBinaryValue): Boolean;
begin
	Result := false;
end;

function TCefBinaryValueOwn.IsEqual(const aThat: ICefBinaryValue): Boolean;
begin
	Result := false;
end;

function TCefBinaryValueOwn.Copy: ICefBinaryValue;
begin
	Result := nil;
end;

function TCefBinaryValueOwn.GetSize: csize_t;
begin
end;

function TCefBinaryValueOwn.GetData(aBuffer: cvoid; aBufferSize: csize_t; aDataOffset: csize_t): csize_t;
begin
end;

{Public section}
constructor TCefBinaryValueOwn.Create;
begin
	inherited CreateData(SizeOf(TCefBinaryValue));
	with PCefBinaryValue(FData)^ do
	begin
		is_valid := {$IFDEF FPC}@{$ENDIF}cef_binary_value_is_valid;
		is_owned := {$IFDEF FPC}@{$ENDIF}cef_binary_value_is_owned;
		is_same := {$IFDEF FPC}@{$ENDIF}cef_binary_value_is_same;
		is_equal := {$IFDEF FPC}@{$ENDIF}cef_binary_value_is_equal;
		copy := {$IFDEF FPC}@{$ENDIF}cef_binary_value_copy;
		get_size := {$IFDEF FPC}@{$ENDIF}cef_binary_value_get_size;
		get_data := {$IFDEF FPC}@{$ENDIF}cef_binary_value_get_data;
	end;
end;
//..............................................................................TCefDictionaryValueOwn
// Returns true (1) if this object is valid. This object may become invalid if
// the underlying data is owned by another object (e.g. list or dictionary)
// and that other object is then modified or destroyed. Do not call any other
// functions if this function returns false (0).
function cef_dictionary_value_is_valid(self: PCefDictionaryValue): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefDictionaryValueOwn(TWACef.GetObject(self)).IsValid
	);
end;

// Returns true (1) if this object is currently owned by another object.
function cef_dictionary_value_is_owned(self: PCefDictionaryValue): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefDictionaryValueOwn(TWACef.GetObject(self)).IsOwned
	);
end;

// Returns true (1) if the values of this object are read-only. Some APIs may
// expose read-only objects.
function cef_dictionary_value_is_read_only(self: PCefDictionaryValue): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefDictionaryValueOwn(TWACef.GetObject(self)).IsReadOnly
	);
end;

// Returns true (1) if this object and |that| object have the same underlying
// data. If true (1) modifications to this object will also affect |that|
// object and vice-versa.
function cef_dictionary_value_is_same(self: PCefDictionaryValue; that: PCefDictionaryValue): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefDictionaryValueOwn(TWACef.GetObject(self)).IsSame(
			TCefDictionaryValueRef.UnWrap(that)
		)
	);
end;

// Returns true (1) if this object and |that| object have an equivalent
// underlying value but are not necessarily the same object.
function cef_dictionary_value_is_equal(self: PCefDictionaryValue; that: PCefDictionaryValue): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefDictionaryValueOwn(TWACef.GetObject(self)).IsEqual(
			TCefDictionaryValueRef.UnWrap(that)
		)
	);
end;

// Returns a writable copy of this object. If |exclude_NULL_children| is true
// (1) any NULL dictionaries or lists will be excluded from the copy.
function cef_dictionary_value_copy(self: PCefDictionaryValue; exclude_empty_children: cint): PCefDictionaryValue; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.GetData(
		TCefDictionaryValueOwn(TWACef.GetObject(self)).Copy(
			exclude_empty_children <> 0
		)
	);
end;

// Returns the number of values.
function cef_dictionary_value_get_size(self: PCefDictionaryValue): csize_t; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := (
		TCefDictionaryValueOwn(TWACef.GetObject(self)).GetSize
	);
end;

// Removes all values. Returns true (1) on success.
function cef_dictionary_value_clear(self: PCefDictionaryValue): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefDictionaryValueOwn(TWACef.GetObject(self)).Clear
	);
end;

// Returns true (1) if the current dictionary has a value for the given key.
function cef_dictionary_value_has_key(self: PCefDictionaryValue; const key: PCefString): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	key_str: ustring;
begin
	key_str := TWACef.ToString(key);
	Result := Ord(
		TCefDictionaryValueOwn(TWACef.GetObject(self)).HasKey(
			key_str
		)
	);
end;

// Reads all keys for this dictionary into the specified vector.
function cef_dictionary_value_get_keys(self: PCefDictionaryValue; keys: TCefStringList): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	keys_list: TSTringList;
	keys_iter: Integer;
	keys_item: TCefString;
begin
	keys_list := TStringList.Create;
	for keys_iter := 0 to cef_string_list_size(keys) - 1 do
	begin
		FillChar(keys_item, SizeOf(keys_item), 0);
		cef_string_list_value(keys, keys_iter, @keys_item);
		keys_list.Add(TWACef.StringClearAndGet(keys_item));
	end;
	Result := Ord(
		TCefDictionaryValueOwn(TWACef.GetObject(self)).GetKeys(
			keys_list
		)
	);

	keys_list.Free;
end;

// Removes the value at the specified key. Returns true (1) is the value was
// removed successfully.
function cef_dictionary_value_remove(self: PCefDictionaryValue; const key: PCefString): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	key_str: ustring;
begin
	key_str := TWACef.ToString(key);
	Result := Ord(
		TCefDictionaryValueOwn(TWACef.GetObject(self)).Remove(
			key_str
		)
	);
end;

// Returns the value type for the specified key.
function cef_dictionary_value_get_type(self: PCefDictionaryValue; const key: PCefString): TCefValueType; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	key_str: ustring;
begin
	key_str := TWACef.ToString(key);
	Result := (
		TCefDictionaryValueOwn(TWACef.GetObject(self)).GetType(
			key_str
		)
	);
end;

// Returns the value at the specified key. For simple types the returned value
// will copy existing data and modifications to the value will not modify this
// object. For complex types (binary, dictionary and list) the returned value
// will reference existing data and modifications to the value will modify
// this object.
function cef_dictionary_value_get_value(self: PCefDictionaryValue; const key: PCefString): PCefValue; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	key_str: ustring;
begin
	key_str := TWACef.ToString(key);
	Result := TWACef.GetData(
		TCefDictionaryValueOwn(TWACef.GetObject(self)).GetValue(
			key_str
		)
	);
end;

// Returns the value at the specified key as type bool.
function cef_dictionary_value_get_bool(self: PCefDictionaryValue; const key: PCefString): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	key_str: ustring;
begin
	key_str := TWACef.ToString(key);
	Result := Ord(
		TCefDictionaryValueOwn(TWACef.GetObject(self)).GetBool(
			key_str
		)
	);
end;

// Returns the value at the specified key as type int.
function cef_dictionary_value_get_int(self: PCefDictionaryValue; const key: PCefString): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	key_str: ustring;
begin
	key_str := TWACef.ToString(key);
	Result := (
		TCefDictionaryValueOwn(TWACef.GetObject(self)).GetInt(
			key_str
		)
	);
end;

// Returns the value at the specified key as type double.
function cef_dictionary_value_get_double(self: PCefDictionaryValue; const key: PCefString): cdouble; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	key_str: ustring;
begin
	key_str := TWACef.ToString(key);
	Result := (
		TCefDictionaryValueOwn(TWACef.GetObject(self)).GetDouble(
			key_str
		)
	);
end;

// Returns the value at the specified key as type string.
// The resulting string must be freed by calling cef_string_userfree_free().
function cef_dictionary_value_get_string(self: PCefDictionaryValue; const key: PCefString): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	key_str: ustring;
begin
	key_str := TWACef.ToString(key);
	Result := TWACef.UserFreeString(
		TCefDictionaryValueOwn(TWACef.GetObject(self)).GetString(
			key_str
		)
	);
end;

// Returns the value at the specified key as type binary. The returned value
// will reference existing data.
function cef_dictionary_value_get_binary(self: PCefDictionaryValue; const key: PCefString): PCefBinaryValue; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	key_str: ustring;
begin
	key_str := TWACef.ToString(key);
	Result := TWACef.GetData(
		TCefDictionaryValueOwn(TWACef.GetObject(self)).GetBinary(
			key_str
		)
	);
end;

// Returns the value at the specified key as type dictionary. The returned
// value will reference existing data and modifications to the value will
// modify this object.
function cef_dictionary_value_get_dictionary(self: PCefDictionaryValue; const key: PCefString): PCefDictionaryValue; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	key_str: ustring;
begin
	key_str := TWACef.ToString(key);
	Result := TWACef.GetData(
		TCefDictionaryValueOwn(TWACef.GetObject(self)).GetDictionary(
			key_str
		)
	);
end;

// Returns the value at the specified key as type list. The returned value
// will reference existing data and modifications to the value will modify
// this object.
function cef_dictionary_value_get_list(self: PCefDictionaryValue; const key: PCefString): PCefListValue; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	key_str: ustring;
begin
	key_str := TWACef.ToString(key);
	Result := TWACef.GetData(
		TCefDictionaryValueOwn(TWACef.GetObject(self)).GetList(
			key_str
		)
	);
end;

// Sets the value at the specified key. Returns true (1) if the value was set
// successfully. If |value| represents simple data then the underlying data
// will be copied and modifications to |value| will not modify this object. If
// |value| represents complex data (binary, dictionary or list) then the
// underlying data will be referenced and modifications to |value| will modify
// this object.
function cef_dictionary_value_set_value(self: PCefDictionaryValue; const key: PCefString; value: PCefValue): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	key_str: ustring;
begin
	key_str := TWACef.ToString(key);
	Result := Ord(
		TCefDictionaryValueOwn(TWACef.GetObject(self)).SetValue(
			key_str,
			TCefValueRef.UnWrap(value)
		)
	);
end;

// Sets the value at the specified key as type null. Returns true (1) if the
// value was set successfully.
function cef_dictionary_value_set_null(self: PCefDictionaryValue; const key: PCefString): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	key_str: ustring;
begin
	key_str := TWACef.ToString(key);
	Result := Ord(
		TCefDictionaryValueOwn(TWACef.GetObject(self)).SetNull(
			key_str
		)
	);
end;

// Sets the value at the specified key as type bool. Returns true (1) if the
// value was set successfully.
function cef_dictionary_value_set_bool(self: PCefDictionaryValue; const key: PCefString; value: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	key_str: ustring;
begin
	key_str := TWACef.ToString(key);
	Result := Ord(
		TCefDictionaryValueOwn(TWACef.GetObject(self)).SetBool(
			key_str,
			value <> 0
		)
	);
end;

// Sets the value at the specified key as type int. Returns true (1) if the
// value was set successfully.
function cef_dictionary_value_set_int(self: PCefDictionaryValue; const key: PCefString; value: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	key_str: ustring;
begin
	key_str := TWACef.ToString(key);
	Result := Ord(
		TCefDictionaryValueOwn(TWACef.GetObject(self)).SetInt(
			key_str,
			value
		)
	);
end;

// Sets the value at the specified key as type double. Returns true (1) if the
// value was set successfully.
function cef_dictionary_value_set_double(self: PCefDictionaryValue; const key: PCefString; value: cdouble): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	key_str: ustring;
begin
	key_str := TWACef.ToString(key);
	Result := Ord(
		TCefDictionaryValueOwn(TWACef.GetObject(self)).SetDouble(
			key_str,
			value
		)
	);
end;

// Sets the value at the specified key as type string. Returns true (1) if the
// value was set successfully.
function cef_dictionary_value_set_string(self: PCefDictionaryValue; const key: PCefString; const value: PCefString): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	key_str: ustring;
	value_str: ustring;
begin
	key_str := TWACef.ToString(key);
	value_str := TWACef.ToString(value);
	Result := Ord(
		TCefDictionaryValueOwn(TWACef.GetObject(self)).SetString(
			key_str,
			value_str
		)
	);
end;

// Sets the value at the specified key as type binary. Returns true (1) if the
// value was set successfully. If |value| is currently owned by another object
// then the value will be copied and the |value| reference will not change.
// Otherwise, ownership will be transferred to this object and the |value|
// reference will be invalidated.
function cef_dictionary_value_set_binary(self: PCefDictionaryValue; const key: PCefString; value: PCefBinaryValue): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	key_str: ustring;
begin
	key_str := TWACef.ToString(key);
	Result := Ord(
		TCefDictionaryValueOwn(TWACef.GetObject(self)).SetBinary(
			key_str,
			TCefBinaryValueRef.UnWrap(value)
		)
	);
end;

// Sets the value at the specified key as type dict. Returns true (1) if the
// value was set successfully. If |value| is currently owned by another object
// then the value will be copied and the |value| reference will not change.
// Otherwise, ownership will be transferred to this object and the |value|
// reference will be invalidated.
function cef_dictionary_value_set_dictionary(self: PCefDictionaryValue; const key: PCefString; value: PCefDictionaryValue): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	key_str: ustring;
begin
	key_str := TWACef.ToString(key);
	Result := Ord(
		TCefDictionaryValueOwn(TWACef.GetObject(self)).SetDictionary(
			key_str,
			TCefDictionaryValueRef.UnWrap(value)
		)
	);
end;

// Sets the value at the specified key as type list. Returns true (1) if the
// value was set successfully. If |value| is currently owned by another object
// then the value will be copied and the |value| reference will not change.
// Otherwise, ownership will be transferred to this object and the |value|
// reference will be invalidated.
function cef_dictionary_value_set_list(self: PCefDictionaryValue; const key: PCefString; value: PCefListValue): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	key_str: ustring;
begin
	key_str := TWACef.ToString(key);
	Result := Ord(
		TCefDictionaryValueOwn(TWACef.GetObject(self)).SetList(
			key_str,
			TCefListValueRef.UnWrap(value)
		)
	);
end;

{Protected section}
function TCefDictionaryValueOwn.IsValid: Boolean;
begin
	Result := false;
end;

function TCefDictionaryValueOwn.IsOwned: Boolean;
begin
	Result := false;
end;

function TCefDictionaryValueOwn.IsReadOnly: Boolean;
begin
	Result := false;
end;

function TCefDictionaryValueOwn.IsSame(const aThat: ICefDictionaryValue): Boolean;
begin
	Result := false;
end;

function TCefDictionaryValueOwn.IsEqual(const aThat: ICefDictionaryValue): Boolean;
begin
	Result := false;
end;

function TCefDictionaryValueOwn.Copy(aExcludeEmptyChildren: Boolean): ICefDictionaryValue;
begin
	Result := nil;
end;

function TCefDictionaryValueOwn.GetSize: csize_t;
begin
end;

function TCefDictionaryValueOwn.Clear: Boolean;
begin
	Result := false;
end;

function TCefDictionaryValueOwn.HasKey(const aKey: ustring): Boolean;
begin
	Result := false;
end;

function TCefDictionaryValueOwn.GetKeys(aKeys: TStrings): Boolean;
begin
	Result := false;
end;

function TCefDictionaryValueOwn.Remove(const aKey: ustring): Boolean;
begin
	Result := false;
end;

function TCefDictionaryValueOwn.GetType(const aKey: ustring): TCefValueType;
begin
end;

function TCefDictionaryValueOwn.GetValue(const aKey: ustring): ICefValue;
begin
	Result := nil;
end;

function TCefDictionaryValueOwn.GetBool(const aKey: ustring): Boolean;
begin
	Result := false;
end;

function TCefDictionaryValueOwn.GetInt(const aKey: ustring): cint;
begin
	Result := 0;
end;

function TCefDictionaryValueOwn.GetDouble(const aKey: ustring): cdouble;
begin
end;

function TCefDictionaryValueOwn.GetString(const aKey: ustring): ustring;
begin
end;

function TCefDictionaryValueOwn.GetBinary(const aKey: ustring): ICefBinaryValue;
begin
	Result := nil;
end;

function TCefDictionaryValueOwn.GetDictionary(const aKey: ustring): ICefDictionaryValue;
begin
	Result := nil;
end;

function TCefDictionaryValueOwn.GetList(const aKey: ustring): ICefListValue;
begin
	Result := nil;
end;

function TCefDictionaryValueOwn.SetValue(const aKey: ustring; const aValue: ICefValue): Boolean;
begin
	Result := false;
end;

function TCefDictionaryValueOwn.SetNull(const aKey: ustring): Boolean;
begin
	Result := false;
end;

function TCefDictionaryValueOwn.SetBool(const aKey: ustring; aValue: Boolean): Boolean;
begin
	Result := false;
end;

function TCefDictionaryValueOwn.SetInt(const aKey: ustring; aValue: cint): Boolean;
begin
	Result := false;
end;

function TCefDictionaryValueOwn.SetDouble(const aKey: ustring; aValue: cdouble): Boolean;
begin
	Result := false;
end;

function TCefDictionaryValueOwn.SetString(const aKey: ustring; const aValue: ustring): Boolean;
begin
	Result := false;
end;

function TCefDictionaryValueOwn.SetBinary(const aKey: ustring; const aValue: ICefBinaryValue): Boolean;
begin
	Result := false;
end;

function TCefDictionaryValueOwn.SetDictionary(const aKey: ustring; const aValue: ICefDictionaryValue): Boolean;
begin
	Result := false;
end;

function TCefDictionaryValueOwn.SetList(const aKey: ustring; const aValue: ICefListValue): Boolean;
begin
	Result := false;
end;

{Public section}
constructor TCefDictionaryValueOwn.Create;
begin
	inherited CreateData(SizeOf(TCefDictionaryValue));
	with PCefDictionaryValue(FData)^ do
	begin
		is_valid := {$IFDEF FPC}@{$ENDIF}cef_dictionary_value_is_valid;
		is_owned := {$IFDEF FPC}@{$ENDIF}cef_dictionary_value_is_owned;
		is_read_only := {$IFDEF FPC}@{$ENDIF}cef_dictionary_value_is_read_only;
		is_same := {$IFDEF FPC}@{$ENDIF}cef_dictionary_value_is_same;
		is_equal := {$IFDEF FPC}@{$ENDIF}cef_dictionary_value_is_equal;
		copy := {$IFDEF FPC}@{$ENDIF}cef_dictionary_value_copy;
		get_size := {$IFDEF FPC}@{$ENDIF}cef_dictionary_value_get_size;
		clear := {$IFDEF FPC}@{$ENDIF}cef_dictionary_value_clear;
		has_key := {$IFDEF FPC}@{$ENDIF}cef_dictionary_value_has_key;
		get_keys := {$IFDEF FPC}@{$ENDIF}cef_dictionary_value_get_keys;
		remove := {$IFDEF FPC}@{$ENDIF}cef_dictionary_value_remove;
		get_type := {$IFDEF FPC}@{$ENDIF}cef_dictionary_value_get_type;
		get_value := {$IFDEF FPC}@{$ENDIF}cef_dictionary_value_get_value;
		get_bool := {$IFDEF FPC}@{$ENDIF}cef_dictionary_value_get_bool;
		get_int := {$IFDEF FPC}@{$ENDIF}cef_dictionary_value_get_int;
		get_double := {$IFDEF FPC}@{$ENDIF}cef_dictionary_value_get_double;
		get_string := {$IFDEF FPC}@{$ENDIF}cef_dictionary_value_get_string;
		get_binary := {$IFDEF FPC}@{$ENDIF}cef_dictionary_value_get_binary;
		get_dictionary := {$IFDEF FPC}@{$ENDIF}cef_dictionary_value_get_dictionary;
		get_list := {$IFDEF FPC}@{$ENDIF}cef_dictionary_value_get_list;
		set_value := {$IFDEF FPC}@{$ENDIF}cef_dictionary_value_set_value;
		set_null := {$IFDEF FPC}@{$ENDIF}cef_dictionary_value_set_null;
		set_bool := {$IFDEF FPC}@{$ENDIF}cef_dictionary_value_set_bool;
		set_int := {$IFDEF FPC}@{$ENDIF}cef_dictionary_value_set_int;
		set_double := {$IFDEF FPC}@{$ENDIF}cef_dictionary_value_set_double;
		set_string := {$IFDEF FPC}@{$ENDIF}cef_dictionary_value_set_string;
		set_binary := {$IFDEF FPC}@{$ENDIF}cef_dictionary_value_set_binary;
		set_dictionary := {$IFDEF FPC}@{$ENDIF}cef_dictionary_value_set_dictionary;
		set_list := {$IFDEF FPC}@{$ENDIF}cef_dictionary_value_set_list;
	end;
end;
//..............................................................................TCefListValueOwn
// Returns true (1) if this object is valid. This object may become invalid if
// the underlying data is owned by another object (e.g. list or dictionary)
// and that other object is then modified or destroyed. Do not call any other
// functions if this function returns false (0).
function cef_list_value_is_valid(self: PCefListValue): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefListValueOwn(TWACef.GetObject(self)).IsValid
	);
end;

// Returns true (1) if this object is currently owned by another object.
function cef_list_value_is_owned(self: PCefListValue): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefListValueOwn(TWACef.GetObject(self)).IsOwned
	);
end;

// Returns true (1) if the values of this object are read-only. Some APIs may
// expose read-only objects.
function cef_list_value_is_read_only(self: PCefListValue): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefListValueOwn(TWACef.GetObject(self)).IsReadOnly
	);
end;

// Returns true (1) if this object and |that| object have the same underlying
// data. If true (1) modifications to this object will also affect |that|
// object and vice-versa.
function cef_list_value_is_same(self: PCefListValue; that: PCefListValue): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefListValueOwn(TWACef.GetObject(self)).IsSame(
			TCefListValueRef.UnWrap(that)
		)
	);
end;

// Returns true (1) if this object and |that| object have an equivalent
// underlying value but are not necessarily the same object.
function cef_list_value_is_equal(self: PCefListValue; that: PCefListValue): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefListValueOwn(TWACef.GetObject(self)).IsEqual(
			TCefListValueRef.UnWrap(that)
		)
	);
end;

// Returns a writable copy of this object.
function cef_list_value_copy(self: PCefListValue): PCefListValue; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.GetData(
		TCefListValueOwn(TWACef.GetObject(self)).Copy
	);
end;

// Sets the number of values. If the number of values is expanded all new
// value slots will default to type null. Returns true (1) on success.
function cef_list_value_set_size(self: PCefListValue; size: csize_t): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefListValueOwn(TWACef.GetObject(self)).SetSize(
			size
		)
	);
end;

// Returns the number of values.
function cef_list_value_get_size(self: PCefListValue): csize_t; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := (
		TCefListValueOwn(TWACef.GetObject(self)).GetSize
	);
end;

// Removes all values. Returns true (1) on success.
function cef_list_value_clear(self: PCefListValue): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefListValueOwn(TWACef.GetObject(self)).Clear
	);
end;

// Removes the value at the specified index.
function cef_list_value_remove(self: PCefListValue; index: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefListValueOwn(TWACef.GetObject(self)).Remove(
			index
		)
	);
end;

// Returns the value type at the specified index.
function cef_list_value_get_type(self: PCefListValue; index: cint): TCefValueType; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := (
		TCefListValueOwn(TWACef.GetObject(self)).GetType(
			index
		)
	);
end;

// Returns the value at the specified index. For simple types the returned
// value will copy existing data and modifications to the value will not
// modify this object. For complex types (binary, dictionary and list) the
// returned value will reference existing data and modifications to the value
// will modify this object.
function cef_list_value_get_value(self: PCefListValue; index: cint): PCefValue; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.GetData(
		TCefListValueOwn(TWACef.GetObject(self)).GetValue(
			index
		)
	);
end;

// Returns the value at the specified index as type bool.
function cef_list_value_get_bool(self: PCefListValue; index: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefListValueOwn(TWACef.GetObject(self)).GetBool(
			index
		)
	);
end;

// Returns the value at the specified index as type int.
function cef_list_value_get_int(self: PCefListValue; index: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := (
		TCefListValueOwn(TWACef.GetObject(self)).GetInt(
			index
		)
	);
end;

// Returns the value at the specified index as type double.
function cef_list_value_get_double(self: PCefListValue; index: cint): cdouble; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := (
		TCefListValueOwn(TWACef.GetObject(self)).GetDouble(
			index
		)
	);
end;

// Returns the value at the specified index as type string.
// The resulting string must be freed by calling cef_string_userfree_free().
function cef_list_value_get_string(self: PCefListValue; index: cint): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.UserFreeString(
		TCefListValueOwn(TWACef.GetObject(self)).GetString(
			index
		)
	);
end;

// Returns the value at the specified index as type binary. The returned value
// will reference existing data.
function cef_list_value_get_binary(self: PCefListValue; index: cint): PCefBinaryValue; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.GetData(
		TCefListValueOwn(TWACef.GetObject(self)).GetBinary(
			index
		)
	);
end;

// Returns the value at the specified index as type dictionary. The returned
// value will reference existing data and modifications to the value will
// modify this object.
function cef_list_value_get_dictionary(self: PCefListValue; index: cint): PCefDictionaryValue; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.GetData(
		TCefListValueOwn(TWACef.GetObject(self)).GetDictionary(
			index
		)
	);
end;

// Returns the value at the specified index as type list. The returned value
// will reference existing data and modifications to the value will modify
// this object.
function cef_list_value_get_list(self: PCefListValue; index: cint): PCefListValue; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.GetData(
		TCefListValueOwn(TWACef.GetObject(self)).GetList(
			index
		)
	);
end;

// Sets the value at the specified index. Returns true (1) if the value was
// set successfully. If |value| represents simple data then the underlying
// data will be copied and modifications to |value| will not modify this
// object. If |value| represents complex data (binary, dictionary or list)
// then the underlying data will be referenced and modifications to |value|
// will modify this object.
function cef_list_value_set_value(self: PCefListValue; index: cint; value: PCefValue): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefListValueOwn(TWACef.GetObject(self)).SetValue(
			index,
			TCefValueRef.UnWrap(value)
		)
	);
end;

// Sets the value at the specified index as type null. Returns true (1) if the
// value was set successfully.
function cef_list_value_set_null(self: PCefListValue; index: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefListValueOwn(TWACef.GetObject(self)).SetNull(
			index
		)
	);
end;

// Sets the value at the specified index as type bool. Returns true (1) if the
// value was set successfully.
function cef_list_value_set_bool(self: PCefListValue; index: cint; value: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefListValueOwn(TWACef.GetObject(self)).SetBool(
			index,
			value <> 0
		)
	);
end;

// Sets the value at the specified index as type int. Returns true (1) if the
// value was set successfully.
function cef_list_value_set_int(self: PCefListValue; index: cint; value: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefListValueOwn(TWACef.GetObject(self)).SetInt(
			index,
			value
		)
	);
end;

// Sets the value at the specified index as type double. Returns true (1) if
// the value was set successfully.
function cef_list_value_set_double(self: PCefListValue; index: cint; value: cdouble): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefListValueOwn(TWACef.GetObject(self)).SetDouble(
			index,
			value
		)
	);
end;

// Sets the value at the specified index as type string. Returns true (1) if
// the value was set successfully.
function cef_list_value_set_string(self: PCefListValue; index: cint; const value: PCefString): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	value_str: ustring;
begin
	value_str := TWACef.ToString(value);
	Result := Ord(
		TCefListValueOwn(TWACef.GetObject(self)).SetString(
			index,
			value_str
		)
	);
end;

// Sets the value at the specified index as type binary. Returns true (1) if
// the value was set successfully. If |value| is currently owned by another
// object then the value will be copied and the |value| reference will not
// change. Otherwise, ownership will be transferred to this object and the
// |value| reference will be invalidated.
function cef_list_value_set_binary(self: PCefListValue; index: cint; value: PCefBinaryValue): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefListValueOwn(TWACef.GetObject(self)).SetBinary(
			index,
			TCefBinaryValueRef.UnWrap(value)
		)
	);
end;

// Sets the value at the specified index as type dict. Returns true (1) if the
// value was set successfully. If |value| is currently owned by another object
// then the value will be copied and the |value| reference will not change.
// Otherwise, ownership will be transferred to this object and the |value|
// reference will be invalidated.
function cef_list_value_set_dictionary(self: PCefListValue; index: cint; value: PCefDictionaryValue): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefListValueOwn(TWACef.GetObject(self)).SetDictionary(
			index,
			TCefDictionaryValueRef.UnWrap(value)
		)
	);
end;

// Sets the value at the specified index as type list. Returns true (1) if the
// value was set successfully. If |value| is currently owned by another object
// then the value will be copied and the |value| reference will not change.
// Otherwise, ownership will be transferred to this object and the |value|
// reference will be invalidated.
function cef_list_value_set_list(self: PCefListValue; index: cint; value: PCefListValue): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefListValueOwn(TWACef.GetObject(self)).SetList(
			index,
			TCefListValueRef.UnWrap(value)
		)
	);
end;

{Protected section}
function TCefListValueOwn.IsValid: Boolean;
begin
	Result := false;
end;

function TCefListValueOwn.IsOwned: Boolean;
begin
	Result := false;
end;

function TCefListValueOwn.IsReadOnly: Boolean;
begin
	Result := false;
end;

function TCefListValueOwn.IsSame(const aThat: ICefListValue): Boolean;
begin
	Result := false;
end;

function TCefListValueOwn.IsEqual(const aThat: ICefListValue): Boolean;
begin
	Result := false;
end;

function TCefListValueOwn.Copy: ICefListValue;
begin
	Result := nil;
end;

function TCefListValueOwn.SetSize(aSize: csize_t): Boolean;
begin
	Result := false;
end;

function TCefListValueOwn.GetSize: csize_t;
begin
end;

function TCefListValueOwn.Clear: Boolean;
begin
	Result := false;
end;

function TCefListValueOwn.Remove(aIndex: cint): Boolean;
begin
	Result := false;
end;

function TCefListValueOwn.GetType(aIndex: cint): TCefValueType;
begin
end;

function TCefListValueOwn.GetValue(aIndex: cint): ICefValue;
begin
	Result := nil;
end;

function TCefListValueOwn.GetBool(aIndex: cint): Boolean;
begin
	Result := false;
end;

function TCefListValueOwn.GetInt(aIndex: cint): cint;
begin
	Result := 0;
end;

function TCefListValueOwn.GetDouble(aIndex: cint): cdouble;
begin
end;

function TCefListValueOwn.GetString(aIndex: cint): ustring;
begin
end;

function TCefListValueOwn.GetBinary(aIndex: cint): ICefBinaryValue;
begin
	Result := nil;
end;

function TCefListValueOwn.GetDictionary(aIndex: cint): ICefDictionaryValue;
begin
	Result := nil;
end;

function TCefListValueOwn.GetList(aIndex: cint): ICefListValue;
begin
	Result := nil;
end;

function TCefListValueOwn.SetValue(aIndex: cint; const aValue: ICefValue): Boolean;
begin
	Result := false;
end;

function TCefListValueOwn.SetNull(aIndex: cint): Boolean;
begin
	Result := false;
end;

function TCefListValueOwn.SetBool(aIndex: cint; aValue: Boolean): Boolean;
begin
	Result := false;
end;

function TCefListValueOwn.SetInt(aIndex: cint; aValue: cint): Boolean;
begin
	Result := false;
end;

function TCefListValueOwn.SetDouble(aIndex: cint; aValue: cdouble): Boolean;
begin
	Result := false;
end;

function TCefListValueOwn.SetString(aIndex: cint; const aValue: ustring): Boolean;
begin
	Result := false;
end;

function TCefListValueOwn.SetBinary(aIndex: cint; const aValue: ICefBinaryValue): Boolean;
begin
	Result := false;
end;

function TCefListValueOwn.SetDictionary(aIndex: cint; const aValue: ICefDictionaryValue): Boolean;
begin
	Result := false;
end;

function TCefListValueOwn.SetList(aIndex: cint; const aValue: ICefListValue): Boolean;
begin
	Result := false;
end;

{Public section}
constructor TCefListValueOwn.Create;
begin
	inherited CreateData(SizeOf(TCefListValue));
	with PCefListValue(FData)^ do
	begin
		is_valid := {$IFDEF FPC}@{$ENDIF}cef_list_value_is_valid;
		is_owned := {$IFDEF FPC}@{$ENDIF}cef_list_value_is_owned;
		is_read_only := {$IFDEF FPC}@{$ENDIF}cef_list_value_is_read_only;
		is_same := {$IFDEF FPC}@{$ENDIF}cef_list_value_is_same;
		is_equal := {$IFDEF FPC}@{$ENDIF}cef_list_value_is_equal;
		copy := {$IFDEF FPC}@{$ENDIF}cef_list_value_copy;
		set_size := {$IFDEF FPC}@{$ENDIF}cef_list_value_set_size;
		get_size := {$IFDEF FPC}@{$ENDIF}cef_list_value_get_size;
		clear := {$IFDEF FPC}@{$ENDIF}cef_list_value_clear;
		remove := {$IFDEF FPC}@{$ENDIF}cef_list_value_remove;
		get_type := {$IFDEF FPC}@{$ENDIF}cef_list_value_get_type;
		get_value := {$IFDEF FPC}@{$ENDIF}cef_list_value_get_value;
		get_bool := {$IFDEF FPC}@{$ENDIF}cef_list_value_get_bool;
		get_int := {$IFDEF FPC}@{$ENDIF}cef_list_value_get_int;
		get_double := {$IFDEF FPC}@{$ENDIF}cef_list_value_get_double;
		get_string := {$IFDEF FPC}@{$ENDIF}cef_list_value_get_string;
		get_binary := {$IFDEF FPC}@{$ENDIF}cef_list_value_get_binary;
		get_dictionary := {$IFDEF FPC}@{$ENDIF}cef_list_value_get_dictionary;
		get_list := {$IFDEF FPC}@{$ENDIF}cef_list_value_get_list;
		set_value := {$IFDEF FPC}@{$ENDIF}cef_list_value_set_value;
		set_null := {$IFDEF FPC}@{$ENDIF}cef_list_value_set_null;
		set_bool := {$IFDEF FPC}@{$ENDIF}cef_list_value_set_bool;
		set_int := {$IFDEF FPC}@{$ENDIF}cef_list_value_set_int;
		set_double := {$IFDEF FPC}@{$ENDIF}cef_list_value_set_double;
		set_string := {$IFDEF FPC}@{$ENDIF}cef_list_value_set_string;
		set_binary := {$IFDEF FPC}@{$ENDIF}cef_list_value_set_binary;
		set_dictionary := {$IFDEF FPC}@{$ENDIF}cef_list_value_set_dictionary;
		set_list := {$IFDEF FPC}@{$ENDIF}cef_list_value_set_list;
	end;
end;

//..............................................................................TCefWebPluginInfoOwn
// Returns the plugin name (i.e. Flash).
// The resulting string must be freed by calling cef_string_userfree_free().
function cef_web_plugin_info_get_name(self: PCefWebPluginInfo): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.UserFreeString(
		TCefWebPluginInfoOwn(TWACef.GetObject(self)).GetName
	);
end;

// Returns the plugin file path (DLL/bundle/library).
// The resulting string must be freed by calling cef_string_userfree_free().
function cef_web_plugin_info_get_path(self: PCefWebPluginInfo): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.UserFreeString(
		TCefWebPluginInfoOwn(TWACef.GetObject(self)).GetPath
	);
end;

// Returns the version of the plugin (may be OS-specific).
// The resulting string must be freed by calling cef_string_userfree_free().
function cef_web_plugin_info_get_version(self: PCefWebPluginInfo): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.UserFreeString(
		TCefWebPluginInfoOwn(TWACef.GetObject(self)).GetVersion
	);
end;

// Returns a description of the plugin from the version information.
// The resulting string must be freed by calling cef_string_userfree_free().
function cef_web_plugin_info_get_description(self: PCefWebPluginInfo): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.UserFreeString(
		TCefWebPluginInfoOwn(TWACef.GetObject(self)).GetDescription
	);
end;

{Protected section}
function TCefWebPluginInfoOwn.GetName: ustring;
begin
end;

function TCefWebPluginInfoOwn.GetPath: ustring;
begin
end;

function TCefWebPluginInfoOwn.GetVersion: ustring;
begin
end;

function TCefWebPluginInfoOwn.GetDescription: ustring;
begin
end;

{Public section}
constructor TCefWebPluginInfoOwn.Create;
begin
	inherited CreateData(SizeOf(TCefWebPluginInfo));
	with PCefWebPluginInfo(FData)^ do
	begin
		get_name := {$IFDEF FPC}@{$ENDIF}cef_web_plugin_info_get_name;
		get_path := {$IFDEF FPC}@{$ENDIF}cef_web_plugin_info_get_path;
		get_version := {$IFDEF FPC}@{$ENDIF}cef_web_plugin_info_get_version;
		get_description := {$IFDEF FPC}@{$ENDIF}cef_web_plugin_info_get_description;
	end;
end;
//..............................................................................TCefWebPluginInfoVisitorOwn
// Method that will be called once for each plugin. |count| is the 0-based
// index for the current plugin. |total| is the total number of plugins.
// Return false (0) to stop visiting plugins. This function may never be
// called if no plugins are found.
function cef_web_plugin_info_visitor_visit(self: PCefWebPluginInfoVisitor; info: PCefWebPluginInfo; count: cint; total: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefWebPluginInfoVisitorOwn(TWACef.GetObject(self)).Visit(
			TCefWebPluginInfoRef.UnWrap(info),
			count,
			total
		)
	);
end;

{Protected section}
function TCefWebPluginInfoVisitorOwn.Visit(const aInfo: ICefWebPluginInfo; aCount: cint; aTotal: cint): Boolean;
begin
	Result := false;
end;

{Public section}
constructor TCefWebPluginInfoVisitorOwn.Create;
begin
	inherited CreateData(SizeOf(TCefWebPluginInfoVisitor));
	with PCefWebPluginInfoVisitor(FData)^ do
	begin
		visit := {$IFDEF FPC}@{$ENDIF}cef_web_plugin_info_visitor_visit;
	end;
end;
//..............................................................................TCefWebPluginUnstableCallbackOwn
// Method that will be called for the requested plugin. |unstable| will be
// true (1) if the plugin has reached the crash count threshold of 3 times in
// 120 seconds.
procedure cef_web_plugin_unstable_callback_is_unstable(self: PCefWebPluginUnstableCallback; const path: PCefString; unstable: cint); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	path_str: ustring;
begin
	path_str := TWACef.ToString(path);
	TCefWebPluginUnstableCallbackOwn(TWACef.GetObject(self)).IsUnstable(
		path_str,
		unstable <> 0
	);
end;

{Protected section}
procedure TCefWebPluginUnstableCallbackOwn.IsUnstable(const aPath: ustring; aUnstable: Boolean);
begin
end;

{Public section}
constructor TCefWebPluginUnstableCallbackOwn.Create;
begin
	inherited CreateData(SizeOf(TCefWebPluginUnstableCallback));
	with PCefWebPluginUnstableCallback(FData)^ do
	begin
		is_unstable := {$IFDEF FPC}@{$ENDIF}cef_web_plugin_unstable_callback_is_unstable;
	end;
end;

//..............................................................................TCefXmlReaderOwn
// Moves the cursor to the next node in the document. This function must be
// called at least once to set the current cursor position. Returns true (1)
// if the cursor position was set successfully.
function cef_xml_reader_move_to_next_node(self: PCefXmlReader): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefXmlReaderOwn(TWACef.GetObject(self)).MoveToNextNode
	);
end;

// Close the document. This should be called directly to ensure that cleanup
// occurs on the correct thread.
function cef_xml_reader_close(self: PCefXmlReader): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefXmlReaderOwn(TWACef.GetObject(self)).Close
	);
end;

// Returns true (1) if an error has been reported by the XML parser.
function cef_xml_reader_has_error(self: PCefXmlReader): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefXmlReaderOwn(TWACef.GetObject(self)).HasError
	);
end;

// Returns the error string.
// The resulting string must be freed by calling cef_string_userfree_free().
function cef_xml_reader_get_error(self: PCefXmlReader): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.UserFreeString(
		TCefXmlReaderOwn(TWACef.GetObject(self)).GetError
	);
end;

// Returns the node type.
function cef_xml_reader_get_type(self: PCefXmlReader): TCefXmlNodeType; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := (
		TCefXmlReaderOwn(TWACef.GetObject(self)).GetType
	);
end;

// Returns the node depth. Depth starts at 0 for the root node.
function cef_xml_reader_get_depth(self: PCefXmlReader): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := (
		TCefXmlReaderOwn(TWACef.GetObject(self)).GetDepth
	);
end;

// Returns the local name. See http://www.w3.org/TR/REC-xml-names/#NT-
// LocalPart for additional details.
// The resulting string must be freed by calling cef_string_userfree_free().
function cef_xml_reader_get_local_name(self: PCefXmlReader): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.UserFreeString(
		TCefXmlReaderOwn(TWACef.GetObject(self)).GetLocalName
	);
end;

// Returns the namespace prefix. See http://www.w3.org/TR/REC-xml-names/ for
// additional details.
// The resulting string must be freed by calling cef_string_userfree_free().
function cef_xml_reader_get_prefix(self: PCefXmlReader): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.UserFreeString(
		TCefXmlReaderOwn(TWACef.GetObject(self)).GetPrefix
	);
end;

// Returns the qualified name, equal to (Prefix:)LocalName. See
// http://www.w3.org/TR/REC-xml-names/#ns-qualnames for additional details.
// The resulting string must be freed by calling cef_string_userfree_free().
function cef_xml_reader_get_qualified_name(self: PCefXmlReader): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.UserFreeString(
		TCefXmlReaderOwn(TWACef.GetObject(self)).GetQualifiedName
	);
end;

// Returns the URI defining the namespace associated with the node. See
// http://www.w3.org/TR/REC-xml-names/ for additional details.
// The resulting string must be freed by calling cef_string_userfree_free().
function cef_xml_reader_get_namespace_uri(self: PCefXmlReader): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.UserFreeString(
		TCefXmlReaderOwn(TWACef.GetObject(self)).GetNamespaceUri
	);
end;

// Returns the base URI of the node. See http://www.w3.org/TR/xmlbase/ for
// additional details.
// The resulting string must be freed by calling cef_string_userfree_free().
function cef_xml_reader_get_base_uri(self: PCefXmlReader): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.UserFreeString(
		TCefXmlReaderOwn(TWACef.GetObject(self)).GetBaseUri
	);
end;

// Returns the xml:lang scope within which the node resides. See
// http://www.w3.org/TR/REC-xml/#sec-lang-tag for additional details.
// The resulting string must be freed by calling cef_string_userfree_free().
function cef_xml_reader_get_xml_lang(self: PCefXmlReader): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.UserFreeString(
		TCefXmlReaderOwn(TWACef.GetObject(self)).GetXmlLang
	);
end;

// Returns true (1) if the node represents an NULL element. <a/> is considered
// NULL but <a></a> is not.
function cef_xml_reader_is_empty_element(self: PCefXmlReader): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefXmlReaderOwn(TWACef.GetObject(self)).IsEmptyElement
	);
end;

// Returns true (1) if the node has a text value.
function cef_xml_reader_has_value(self: PCefXmlReader): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefXmlReaderOwn(TWACef.GetObject(self)).HasValue
	);
end;

// Returns the text value.
// The resulting string must be freed by calling cef_string_userfree_free().
function cef_xml_reader_get_value(self: PCefXmlReader): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.UserFreeString(
		TCefXmlReaderOwn(TWACef.GetObject(self)).GetValue
	);
end;

// Returns true (1) if the node has attributes.
function cef_xml_reader_has_attributes(self: PCefXmlReader): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefXmlReaderOwn(TWACef.GetObject(self)).HasAttributes
	);
end;

// Returns the number of attributes.
function cef_xml_reader_get_attribute_count(self: PCefXmlReader): csize_t; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := (
		TCefXmlReaderOwn(TWACef.GetObject(self)).GetAttributeCount
	);
end;

// Returns the value of the attribute at the specified 0-based index.
// The resulting string must be freed by calling cef_string_userfree_free().
function cef_xml_reader_get_attribute_byindex(self: PCefXmlReader; index: cint): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.UserFreeString(
		TCefXmlReaderOwn(TWACef.GetObject(self)).GetAttributeByindex(
			index
		)
	);
end;

// Returns the value of the attribute with the specified qualified name.
// The resulting string must be freed by calling cef_string_userfree_free().
function cef_xml_reader_get_attribute_byqname(self: PCefXmlReader; const qualifiedName: PCefString): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	qualifiedName_str: ustring;
begin
	qualifiedName_str := TWACef.ToString(qualifiedName);
	Result := TWACef.UserFreeString(
		TCefXmlReaderOwn(TWACef.GetObject(self)).GetAttributeByqname(
			qualifiedName_str
		)
	);
end;

// Returns the value of the attribute with the specified local name and
// namespace URI.
// The resulting string must be freed by calling cef_string_userfree_free().
function cef_xml_reader_get_attribute_bylname(self: PCefXmlReader; const localName: PCefString; const namespaceURI: PCefString): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	localName_str: ustring;
	namespaceURI_str: ustring;
begin
	localName_str := TWACef.ToString(localName);
	namespaceURI_str := TWACef.ToString(namespaceURI);
	Result := TWACef.UserFreeString(
		TCefXmlReaderOwn(TWACef.GetObject(self)).GetAttributeBylname(
			localName_str,
			namespaceURI_str
		)
	);
end;

// Returns an XML representation of the current node's children.
// The resulting string must be freed by calling cef_string_userfree_free().
function cef_xml_reader_get_inner_xml(self: PCefXmlReader): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.UserFreeString(
		TCefXmlReaderOwn(TWACef.GetObject(self)).GetInnerXml
	);
end;

// Returns an XML representation of the current node including its children.
// The resulting string must be freed by calling cef_string_userfree_free().
function cef_xml_reader_get_outer_xml(self: PCefXmlReader): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.UserFreeString(
		TCefXmlReaderOwn(TWACef.GetObject(self)).GetOuterXml
	);
end;

// Returns the line number for the current node.
function cef_xml_reader_get_line_number(self: PCefXmlReader): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := (
		TCefXmlReaderOwn(TWACef.GetObject(self)).GetLineNumber
	);
end;

// Moves the cursor to the attribute at the specified 0-based index. Returns
// true (1) if the cursor position was set successfully.
function cef_xml_reader_move_to_attribute_byindex(self: PCefXmlReader; index: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefXmlReaderOwn(TWACef.GetObject(self)).MoveToAttributeByindex(
			index
		)
	);
end;

// Moves the cursor to the attribute with the specified qualified name.
// Returns true (1) if the cursor position was set successfully.
function cef_xml_reader_move_to_attribute_byqname(self: PCefXmlReader; const qualifiedName: PCefString): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	qualifiedName_str: ustring;
begin
	qualifiedName_str := TWACef.ToString(qualifiedName);
	Result := Ord(
		TCefXmlReaderOwn(TWACef.GetObject(self)).MoveToAttributeByqname(
			qualifiedName_str
		)
	);
end;

// Moves the cursor to the attribute with the specified local name and
// namespace URI. Returns true (1) if the cursor position was set
// successfully.
function cef_xml_reader_move_to_attribute_bylname(self: PCefXmlReader; const localName: PCefString; const namespaceURI: PCefString): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	localName_str: ustring;
	namespaceURI_str: ustring;
begin
	localName_str := TWACef.ToString(localName);
	namespaceURI_str := TWACef.ToString(namespaceURI);
	Result := Ord(
		TCefXmlReaderOwn(TWACef.GetObject(self)).MoveToAttributeBylname(
			localName_str,
			namespaceURI_str
		)
	);
end;

// Moves the cursor to the first attribute in the current element. Returns
// true (1) if the cursor position was set successfully.
function cef_xml_reader_move_to_first_attribute(self: PCefXmlReader): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefXmlReaderOwn(TWACef.GetObject(self)).MoveToFirstAttribute
	);
end;

// Moves the cursor to the next attribute in the current element. Returns true
// (1) if the cursor position was set successfully.
function cef_xml_reader_move_to_next_attribute(self: PCefXmlReader): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefXmlReaderOwn(TWACef.GetObject(self)).MoveToNextAttribute
	);
end;

// Moves the cursor back to the carrying element. Returns true (1) if the
// cursor position was set successfully.
function cef_xml_reader_move_to_carrying_element(self: PCefXmlReader): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefXmlReaderOwn(TWACef.GetObject(self)).MoveToCarryingElement
	);
end;

{Protected section}
function TCefXmlReaderOwn.MoveToNextNode: Boolean;
begin
	Result := false;
end;

function TCefXmlReaderOwn.Close: Boolean;
begin
	Result := false;
end;

function TCefXmlReaderOwn.HasError: Boolean;
begin
	Result := false;
end;

function TCefXmlReaderOwn.GetError: ustring;
begin
end;

function TCefXmlReaderOwn.GetType: TCefXmlNodeType;
begin
end;

function TCefXmlReaderOwn.GetDepth: cint;
begin
	Result := 0;
end;

function TCefXmlReaderOwn.GetLocalName: ustring;
begin
end;

function TCefXmlReaderOwn.GetPrefix: ustring;
begin
end;

function TCefXmlReaderOwn.GetQualifiedName: ustring;
begin
end;

function TCefXmlReaderOwn.GetNamespaceUri: ustring;
begin
end;

function TCefXmlReaderOwn.GetBaseUri: ustring;
begin
end;

function TCefXmlReaderOwn.GetXmlLang: ustring;
begin
end;

function TCefXmlReaderOwn.IsEmptyElement: Boolean;
begin
	Result := false;
end;

function TCefXmlReaderOwn.HasValue: Boolean;
begin
	Result := false;
end;

function TCefXmlReaderOwn.GetValue: ustring;
begin
end;

function TCefXmlReaderOwn.HasAttributes: Boolean;
begin
	Result := false;
end;

function TCefXmlReaderOwn.GetAttributeCount: csize_t;
begin
end;

function TCefXmlReaderOwn.GetAttributeByindex(aIndex: cint): ustring;
begin
end;

function TCefXmlReaderOwn.GetAttributeByqname(const aQualifiedName: ustring): ustring;
begin
end;

function TCefXmlReaderOwn.GetAttributeBylname(const aLocalName: ustring; const aNamespaceURI: ustring): ustring;
begin
end;

function TCefXmlReaderOwn.GetInnerXml: ustring;
begin
end;

function TCefXmlReaderOwn.GetOuterXml: ustring;
begin
end;

function TCefXmlReaderOwn.GetLineNumber: cint;
begin
	Result := 0;
end;

function TCefXmlReaderOwn.MoveToAttributeByindex(aIndex: cint): Boolean;
begin
	Result := false;
end;

function TCefXmlReaderOwn.MoveToAttributeByqname(const aQualifiedName: ustring): Boolean;
begin
	Result := false;
end;

function TCefXmlReaderOwn.MoveToAttributeBylname(const aLocalName: ustring; const aNamespaceURI: ustring): Boolean;
begin
	Result := false;
end;

function TCefXmlReaderOwn.MoveToFirstAttribute: Boolean;
begin
	Result := false;
end;

function TCefXmlReaderOwn.MoveToNextAttribute: Boolean;
begin
	Result := false;
end;

function TCefXmlReaderOwn.MoveToCarryingElement: Boolean;
begin
	Result := false;
end;

{Public section}
constructor TCefXmlReaderOwn.Create;
begin
	inherited CreateData(SizeOf(TCefXmlReader));
	with PCefXmlReader(FData)^ do
	begin
		move_to_next_node := {$IFDEF FPC}@{$ENDIF}cef_xml_reader_move_to_next_node;
		close := {$IFDEF FPC}@{$ENDIF}cef_xml_reader_close;
		has_error := {$IFDEF FPC}@{$ENDIF}cef_xml_reader_has_error;
		get_error := {$IFDEF FPC}@{$ENDIF}cef_xml_reader_get_error;
		get_type := {$IFDEF FPC}@{$ENDIF}cef_xml_reader_get_type;
		get_depth := {$IFDEF FPC}@{$ENDIF}cef_xml_reader_get_depth;
		get_local_name := {$IFDEF FPC}@{$ENDIF}cef_xml_reader_get_local_name;
		get_prefix := {$IFDEF FPC}@{$ENDIF}cef_xml_reader_get_prefix;
		get_qualified_name := {$IFDEF FPC}@{$ENDIF}cef_xml_reader_get_qualified_name;
		get_namespace_uri := {$IFDEF FPC}@{$ENDIF}cef_xml_reader_get_namespace_uri;
		get_base_uri := {$IFDEF FPC}@{$ENDIF}cef_xml_reader_get_base_uri;
		get_xml_lang := {$IFDEF FPC}@{$ENDIF}cef_xml_reader_get_xml_lang;
		is_empty_element := {$IFDEF FPC}@{$ENDIF}cef_xml_reader_is_empty_element;
		has_value := {$IFDEF FPC}@{$ENDIF}cef_xml_reader_has_value;
		get_value := {$IFDEF FPC}@{$ENDIF}cef_xml_reader_get_value;
		has_attributes := {$IFDEF FPC}@{$ENDIF}cef_xml_reader_has_attributes;
		get_attribute_count := {$IFDEF FPC}@{$ENDIF}cef_xml_reader_get_attribute_count;
		get_attribute_byindex := {$IFDEF FPC}@{$ENDIF}cef_xml_reader_get_attribute_byindex;
		get_attribute_byqname := {$IFDEF FPC}@{$ENDIF}cef_xml_reader_get_attribute_byqname;
		get_attribute_bylname := {$IFDEF FPC}@{$ENDIF}cef_xml_reader_get_attribute_bylname;
		get_inner_xml := {$IFDEF FPC}@{$ENDIF}cef_xml_reader_get_inner_xml;
		get_outer_xml := {$IFDEF FPC}@{$ENDIF}cef_xml_reader_get_outer_xml;
		get_line_number := {$IFDEF FPC}@{$ENDIF}cef_xml_reader_get_line_number;
		move_to_attribute_byindex := {$IFDEF FPC}@{$ENDIF}cef_xml_reader_move_to_attribute_byindex;
		move_to_attribute_byqname := {$IFDEF FPC}@{$ENDIF}cef_xml_reader_move_to_attribute_byqname;
		move_to_attribute_bylname := {$IFDEF FPC}@{$ENDIF}cef_xml_reader_move_to_attribute_bylname;
		move_to_first_attribute := {$IFDEF FPC}@{$ENDIF}cef_xml_reader_move_to_first_attribute;
		move_to_next_attribute := {$IFDEF FPC}@{$ENDIF}cef_xml_reader_move_to_next_attribute;
		move_to_carrying_element := {$IFDEF FPC}@{$ENDIF}cef_xml_reader_move_to_carrying_element;
	end;
end;

//..............................................................................TCefZipReaderOwn
// Moves the cursor to the first file in the archive. Returns true (1) if the
// cursor position was set successfully.
function cef_zip_reader_move_to_first_file(self: PCefZipReader): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefZipReaderOwn(TWACef.GetObject(self)).MoveToFirstFile
	);
end;

// Moves the cursor to the next file in the archive. Returns true (1) if the
// cursor position was set successfully.
function cef_zip_reader_move_to_next_file(self: PCefZipReader): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefZipReaderOwn(TWACef.GetObject(self)).MoveToNextFile
	);
end;

// Moves the cursor to the specified file in the archive. If |caseSensitive|
// is true (1) then the search will be case sensitive. Returns true (1) if the
// cursor position was set successfully.
function cef_zip_reader_move_to_file(self: PCefZipReader; const fileName: PCefString; caseSensitive: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	fileName_str: ustring;
begin
	fileName_str := TWACef.ToString(fileName);
	Result := Ord(
		TCefZipReaderOwn(TWACef.GetObject(self)).MoveToFile(
			fileName_str,
			caseSensitive <> 0
		)
	);
end;

// Closes the archive. This should be called directly to ensure that cleanup
// occurs on the correct thread.
function cef_zip_reader_close(self: PCefZipReader): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefZipReaderOwn(TWACef.GetObject(self)).Close
	);
end;

// Returns the name of the file.
// The resulting string must be freed by calling cef_string_userfree_free().
function cef_zip_reader_get_file_name(self: PCefZipReader): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := TWACef.UserFreeString(
		TCefZipReaderOwn(TWACef.GetObject(self)).GetFileName
	);
end;

// Returns the uncompressed size of the file.
function cef_zip_reader_get_file_size(self: PCefZipReader): cint64; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := (
		TCefZipReaderOwn(TWACef.GetObject(self)).GetFileSize
	);
end;

// Returns the last modified timestamp for the file.
function cef_zip_reader_get_file_last_modified(self: PCefZipReader): ctime_t; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := (
		TCefZipReaderOwn(TWACef.GetObject(self)).GetFileLastModified
	);
end;

// Opens the file for reading of uncompressed data. A read password may
// optionally be specified.
function cef_zip_reader_open_file(self: PCefZipReader; const password: PCefString): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
	password_str: ustring;
begin
	password_str := TWACef.ToString(password);
	Result := Ord(
		TCefZipReaderOwn(TWACef.GetObject(self)).OpenFile(
			password_str
		)
	);
end;

// Closes the file.
function cef_zip_reader_close_file(self: PCefZipReader): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefZipReaderOwn(TWACef.GetObject(self)).CloseFile
	);
end;

// Read uncompressed file contents into the specified buffer. Returns < 0 if
// an error occurred, 0 if at the end of file, or the number of bytes read.
function cef_zip_reader_read_file(self: PCefZipReader; buffer: cvoid; bufferSize: csize_t): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := (
		TCefZipReaderOwn(TWACef.GetObject(self)).ReadFile(
			buffer,
			bufferSize
		)
	);
end;

// Returns the current offset in the uncompressed file contents.
function cef_zip_reader_tell(self: PCefZipReader): cint64; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := (
		TCefZipReaderOwn(TWACef.GetObject(self)).Tell
	);
end;

// Returns true (1) if at end of the file contents.
function cef_zip_reader_eof(self: PCefZipReader): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
	Result := Ord(
		TCefZipReaderOwn(TWACef.GetObject(self)).Eof
	);
end;

{Protected section}
function TCefZipReaderOwn.MoveToFirstFile: Boolean;
begin
	Result := false;
end;

function TCefZipReaderOwn.MoveToNextFile: Boolean;
begin
	Result := false;
end;

function TCefZipReaderOwn.MoveToFile(const aFileName: ustring; aCaseSensitive: Boolean): Boolean;
begin
	Result := false;
end;

function TCefZipReaderOwn.Close: Boolean;
begin
	Result := false;
end;

function TCefZipReaderOwn.GetFileName: ustring;
begin
end;

function TCefZipReaderOwn.GetFileSize: cint64;
begin
end;

function TCefZipReaderOwn.GetFileLastModified: ctime_t;
begin
end;

function TCefZipReaderOwn.OpenFile(const aPassword: ustring): Boolean;
begin
	Result := false;
end;

function TCefZipReaderOwn.CloseFile: Boolean;
begin
	Result := false;
end;

function TCefZipReaderOwn.ReadFile(const aBuffer: cvoid; aBufferSize: csize_t): cint;
begin
	Result := 0;
end;

function TCefZipReaderOwn.Tell: cint64;
begin
end;

function TCefZipReaderOwn.Eof: Boolean;
begin
	Result := false;
end;

{Public section}
constructor TCefZipReaderOwn.Create;
begin
	inherited CreateData(SizeOf(TCefZipReader));
	with PCefZipReader(FData)^ do
	begin
		move_to_first_file := {$IFDEF FPC}@{$ENDIF}cef_zip_reader_move_to_first_file;
		move_to_next_file := {$IFDEF FPC}@{$ENDIF}cef_zip_reader_move_to_next_file;
		move_to_file := {$IFDEF FPC}@{$ENDIF}cef_zip_reader_move_to_file;
		close := {$IFDEF FPC}@{$ENDIF}cef_zip_reader_close;
		get_file_name := {$IFDEF FPC}@{$ENDIF}cef_zip_reader_get_file_name;
		get_file_size := {$IFDEF FPC}@{$ENDIF}cef_zip_reader_get_file_size;
		get_file_last_modified := {$IFDEF FPC}@{$ENDIF}cef_zip_reader_get_file_last_modified;
		open_file := {$IFDEF FPC}@{$ENDIF}cef_zip_reader_open_file;
		close_file := {$IFDEF FPC}@{$ENDIF}cef_zip_reader_close_file;
		read_file := {$IFDEF FPC}@{$ENDIF}cef_zip_reader_read_file;
		tell := {$IFDEF FPC}@{$ENDIF}cef_zip_reader_tell;
		eof := {$IFDEF FPC}@{$ENDIF}cef_zip_reader_eof;
	end;
end;
























//..............................................................................TCefFastStringVisitor
constructor TCefFastStringVisitor.Create(
  const callback: TCefStringVisitorProc);
begin
  inherited Create;
  FVisit := callback;
end;

procedure TCefFastStringVisitor.Visit(const str: ustring);
begin
  FVisit(str);
end;

//..............................................................................TCefFastDomVisitor
constructor TCefFastDomVisitor.Create(const proc: TCefDomVisitorProc);
begin
  inherited Create;
  FProc := proc;
end;

procedure TCefFastDomVisitor.visit(const document: ICefDomDocument);
begin
  FProc(document);
end;

//..............................................................................TCefCustomStreamReader
function cef_custom_stream_reader_read(self: PCefReadHandler; ptr: cvoid; size: csize_t; n: csize_t): csize_t; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  with TCefCustomStreamReader(TWACef.GetObject(self)) do
    Result := Read(ptr, size, n);
end;

function cef_custom_stream_reader_seek(self: PCefReadHandler; offset: Int64; whence: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  with TCefCustomStreamReader(TWACef.GetObject(self)) do
    Result := Seek(offset, whence);
end;

function cef_custom_stream_reader_tell(self: PCefReadHandler): Int64; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  with TCefCustomStreamReader(TWACef.GetObject(self)) do
    Result := Tell;
end;

function cef_custom_stream_reader_eof(self: PCefReadHandler): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  with TCefCustomStreamReader(TWACef.GetObject(self)) do
    Result := Ord(eof);
end;

constructor TCefCustomStreamReader.Create(Stream: TStream; Owned: Boolean);
begin
  inherited CreateData(SizeOf(TCefReadHandler));
  FStream := stream;
  FOwned := Owned;
  with PCefReadHandler(FData)^ do
  begin
    read := {$IFDEF FPC}@{$ENDIF}cef_custom_stream_reader_read;
    seek := {$IFDEF FPC}@{$ENDIF}cef_custom_stream_reader_seek;
    tell := {$IFDEF FPC}@{$ENDIF}cef_custom_stream_reader_tell;
    eof := {$IFDEF FPC}@{$ENDIF}cef_custom_stream_reader_eof;
  end;
end;

constructor TCefCustomStreamReader.Create(const filename: ustring);
begin
  Create(TFileStream.Create(filename, fmOpenRead or fmShareDenyWrite), True);
end;

destructor TCefCustomStreamReader.Destroy;
begin
  if FOwned then
    FStream.Free;
  inherited;
end;

function TCefCustomStreamReader.Eof: Boolean;
begin
  Result := FStream.Position = FStream.size;
end;

function TCefCustomStreamReader.Read(ptr: Pointer; size, n: csize_t): csize_t;
begin
  result := csize_t(FStream.Read(ptr^, n * size)) div size;
end;

function TCefCustomStreamReader.Seek(offset: Int64; whence: cint): cint;
begin
  Result := FStream.Seek(offset, whence);
end;

function TCefCustomStreamReader.Tell: Int64;
begin
  Result := FStream.Position;
end;

//..............................................................................TCefFastCookieVisitor
constructor TCefFastCookieVisitor.Create(const visitor: TCefCookieVisitorProc);
begin
  inherited Create;
  FVisitor := visitor;
end;

function TCefFastCookieVisitor.Visit(const aCookie: TWACefCookie; aCount, aTotal: cint; var aDeleteCookie: Boolean): Boolean;
begin
  Result := FVisitor(aCookie, aCount, aTotal, aDeleteCookie);
end;

//..............................................................................TCefStringMapOwn
procedure TCefStringMapOwn.Append(const key, value: ustring);
var
  k, v: TCefString;
begin
  k := TWACef.ToCefString(key);
  v := TWACef.ToCefString(value);
  cef_string_map_append(FStringMap, @k, @v);
end;

procedure TCefStringMapOwn.Clear;
begin
  cef_string_map_clear(FStringMap);
end;

constructor TCefStringMapOwn.Create(aStringMap: TCefStringMap = nil);
begin
  if Assigned(aStringMap) then
    FStringMap := aStringMap
  else
    FStringMap := cef_string_map_alloc();
end;

destructor TCefStringMapOwn.Destroy;
begin
  cef_string_map_free(FStringMap);
end;

function TCefStringMapOwn.Find(const key: ustring): ustring;
var
  k: TCefString;
  str: PCefString;
begin
  New(str);
  try
    FillChar(str, SizeOf(str), 0);
    k := TWACef.ToCefString(key);
    cef_string_map_find(FStringMap, @k, str);
    Result := TWACef.ToString(str);
  finally
    Dispose(str);
  end;
end;

function TCefStringMapOwn.GetHandle: TCefStringMap;
begin
  Result := FStringMap;
end;

function TCefStringMapOwn.GetKey(index: cint): ustring;
var
  str: TCefString;
begin
  FillChar(str, SizeOf(str), 0);
  cef_string_map_key(FStringMap, index, str);
  Result := TWACef.ToString(@str);
end;

function TCefStringMapOwn.GetSize: cint;
begin
  Result := cef_string_map_size(FStringMap);
end;

function TCefStringMapOwn.GetValue(index: cint): ustring;
var
  str: TCefString;
begin
  FillChar(str, SizeOf(str), 0);
  cef_string_map_value(FStringMap, index, str);
  Result := TWACef.ToString(@str);
end;

//..............................................................................TCefStringMultimapOwn

procedure TCefStringMultimapOwn.Append(const Key, Value: ustring);
var
  k, v: TCefString;
begin
  k := TWACef.ToCefString(key);
  v := TWACef.ToCefString(value);
  cef_string_multimap_append(FStringMap, @k, @v);
end;

procedure TCefStringMultimapOwn.Clear;
begin
  cef_string_multimap_clear(FStringMap);
end;

constructor TCefStringMultimapOwn.Create(aStringMap: TCefStringMultiMap = nil);
begin
  if Assigned(aStringMap) then
    FStringMap := aStringMap
  else
    FStringMap := cef_string_multimap_alloc();
end;

destructor TCefStringMultimapOwn.Destroy;
begin
  cef_string_multimap_free(FStringMap);
  inherited;
end;

function TCefStringMultimapOwn.FindCount(const Key: ustring): cint;
var
  k: TCefString;
begin
  k := TWACef.ToCefString(Key);
  Result := cef_string_multimap_find_count(FStringMap, @k);
end;

function TCefStringMultimapOwn.GetEnumerate(const Key: ustring;
  ValueIndex: cint): ustring;
var
  k, v: TCefString;
begin
  k := TWACef.ToCefString(Key);
  FillChar(v, SizeOf(v), 0);
  cef_string_multimap_enumerate(FStringMap, @k, ValueIndex, v);
  Result := TWACef.ToString(@v);
end;

function TCefStringMultimapOwn.GetHandle: TCefStringMultimap;
begin
  Result := FStringMap;
end;

function TCefStringMultimapOwn.GetKey(Index: cint): ustring;
var
  str: TCefString;
begin
  FillChar(str, SizeOf(str), 0);
  cef_string_multimap_key(FStringMap, index, str);
  Result := TWACef.ToString(@str);
end;

function TCefStringMultimapOwn.GetSize: cint;
begin
  Result := cef_string_multimap_size(FStringMap);
end;

function TCefStringMultimapOwn.GetValue(Index: cint): ustring;
var
  str: TCefString;
begin
  FillChar(str, SizeOf(str), 0);
  cef_string_multimap_value(FStringMap, index, str);
  Result := TWACef.ToString(@str);
end;

//..............................................................................TCefFastResourceBundle
constructor TCefFastResourceBundle.Create(AGetDataResource: TGetDataResource;
  AGetLocalizedString: TGetLocalizedString);
begin
  inherited Create;
  FGetDataResource := AGetDataResource;
  FGetLocalizedString := AGetLocalizedString;
end;

function TCefFastResourceBundle.GetDataResource(aResourceId: cint; out aData: cvoid; out aDataSize: csize_t): Boolean;
begin
  if Assigned(FGetDataResource) then
    Result := FGetDataResource(aResourceId, aData, aDataSize)
  else
    Result := False;
end;

function TCefFastResourceBundle.GetLocalizedString(aMessageId: cint; out aString: ustring): Boolean;
begin
  if Assigned(FGetLocalizedString) then
    Result := FGetLocalizedString(aMessageId, aString)
  else
    Result := False;
end;

//..............................................................................TCefFastWebPluginInfoVisitor
constructor TCefFastWebPluginInfoVisitor.Create(
  const proc: TCefWebPluginInfoVisitorProc);
begin
  inherited Create;
  FProc := proc;
end;

function TCefFastWebPluginInfoVisitor.Visit(const info: ICefWebPluginInfo;
  count, total: cint): Boolean;
begin
  Result := FProc(info, count, total);
end;

//..............................................................................TCefFastWebPluginUnstableCallback
constructor TCefFastWebPluginUnstableCallback.Create(
  const callback: TCefWebPluginIsUnstableProc);
begin
  FCallback := callback;
end;

procedure TCefFastWebPluginUnstableCallback.IsUnstable(const path: ustring;
  unstable: Boolean);
begin
  FCallback(path, unstable);
end;

//..............................................................................TInternalApp
procedure TInternalApp.OnBeforeCommandLineProcessing(const processType : ustring; const commandLine : ICefCommandLine);
begin
  If Assigned(CefOnBeforeCommandLineProcessing) then
    CefOnBeforeCommandLineProcessing(processType, commandLine);
end;

procedure TInternalApp.OnRegisterCustomSchemes(const registrar : ICefSchemeRegistrar);
begin
  If Assigned(CefOnRegisterCustomSchemes) then
    CefOnRegisterCustomSchemes(registrar);
end;

function TInternalApp.GetResourceBundleHandler : ICefResourceBundleHandler;
begin
  Result := CefResourceBundleHandler;
end;

function TInternalApp.GetBrowserProcessHandler : ICefBrowserProcessHandler;
begin
  Result := CefBrowserProcessHandler;
end;

function TInternalApp.GetRenderProcessHandler : ICefRenderProcessHandler;
begin
  Result := CefRenderProcessHandler;
end;

function TInternalApp.GetProcessType(const aCommandLine: ICefCommandLine): TCefProcessType;
var
  process_type: ustring;
begin
  if not aCommandLine.HasSwitch('type') then
    Result := BrowserProcess
  else
  begin
    process_type := aCommandLine.GetSwitchValue('type');
    if process_type = 'renderer' then
      Result := RendererProcess
    {$IFDEF LINUX}
    else if process_type = 'zygote' then
      Result := ZygoteProcess
    {$ENDIF}
    else
      Result := OtherProcess;
  end;
end;

{$IFNDEF FPC}
//..............................................................................TCefRTTIExtension
constructor TCefRTTIExtension.Create(const value: TValue
{$IFDEF CEF_MULTI_THREADED_MESSAGE_LOOP}
; SyncMainThread: Boolean
{$ENDIF}
);
begin
  inherited Create;
  FCtx := TRttiContext.Create;
{$IFDEF CEF_MULTI_THREADED_MESSAGE_LOOP}
  FSyncMainThread := SyncMainThread;
{$ENDIF}
  FValue := value;
end;

destructor TCefRTTIExtension.Destroy;
begin
  FCtx.Free;
  inherited;
end;

function TCefRTTIExtension.GetValue(pi: PTypeInfo; const v: ICefv8Value; var ret: TValue): Boolean;

  function ProcessInt: Boolean;
  var
    sv: record
      case byte of
      0:  (ub: Byte);
      1:  (sb: ShortInt);
      2:  (uw: Word);
      3:  (sw: SmallInt);
      4:  (si: cint);
      5:  (ui: csize_t);
    end;
    pd: PTypeData;
  begin
    pd := GetTypeData(pi);
    if (v.IsInt or v.IsBool) and (v.GetIntValue >= pd.MinValue) and (v.GetIntValue <= pd.MaxValue) then
    begin
      case pd.OrdType of
        otSByte: sv.sb := v.GetIntValue;
        otUByte: sv.ub := v.GetIntValue;
        otSWord: sv.sw := v.GetIntValue;
        otUWord: sv.uw := v.GetIntValue;
        otSLong: sv.si := v.GetIntValue;
        otULong: sv.ui := v.GetIntValue;
      end;
      TValue.Make(@sv, pi, ret);
    end else
      Exit(False);
    Result := True;
  end;

  function ProcessInt64: Boolean;
  var
    i: Int64;
  begin
    i := StrToInt64(v.GetStringValue); // hack
    TValue.Make(@i, pi, ret);
    Result := True;
  end;

  function ProcessUString: Boolean;
  var
    vus: ustring;
  begin
    if v.IsString then
    begin
      vus := v.GetStringValue;
      TValue.Make(@vus, pi, ret);
    end else
      Exit(False);
    Result := True;
  end;

  function ProcessLString: Boolean;
  var
    vas: AnsiString;
  begin
    if v.IsString then
    begin
      vas := AnsiString(v.GetStringValue);
      TValue.Make(@vas, pi, ret);
    end else
      Exit(False);
    Result := True;
  end;

  function ProcessWString: Boolean;
  var
    vws: WideString;
  begin
    if v.IsString then
    begin
      vws := v.GetStringValue;
      TValue.Make(@vws, pi, ret);
    end else
      Exit(False);
    Result := True;
  end;

  function ProcessFloat: Boolean;
  var
    sv: record
      case byte of
      0: (fs: Single);
      1: (fd: Double);
      2: (fe: Extended);
      3: (fc: Comp);
      4: (fcu: Currency);
    end;
  begin
    if v.IsDouble or v.IsInt then
    begin
      case GetTypeData(pi).FloatType of
        ftSingle: sv.fs := v.GetDoubleValue;
        ftDouble: sv.fd := v.GetDoubleValue;
        ftExtended: sv.fe := v.GetDoubleValue;
        ftComp: sv.fc := v.GetDoubleValue;
        ftCurr: sv.fcu := v.GetDoubleValue;
      end;
      TValue.Make(@sv, pi, ret);
    end else
    if v.IsDate then
    begin
      sv.fd := TWACef.CefTimeToDateTime(v.GetDateValue);
      TValue.Make(@sv, pi, ret);
    end else
      Exit(False);
    Result := True;
  end;

  function ProcessSet: Boolean;
  var
    sv: record
      case byte of
      0:  (ub: Byte);
      1:  (sb: ShortInt);
      2:  (uw: Word);
      3:  (sw: SmallInt);
      4:  (si: cint);
      5:  (ui: csize_t);
    end;
  begin
    if v.IsInt then
    begin
      case GetTypeData(pi).OrdType of
        otSByte: sv.sb := v.GetIntValue;
        otUByte: sv.ub := v.GetIntValue;
        otSWord: sv.sw := v.GetIntValue;
        otUWord: sv.uw := v.GetIntValue;
        otSLong: sv.si := v.GetIntValue;
        otULong: sv.ui := v.GetIntValue;
      end;
      TValue.Make(@sv, pi, ret);
    end else
      Exit(False);
    Result := True;
  end;

  function ProcessVariant: Boolean;
  var
    vr: Variant;
    i: cint;
    vl: TValue;
  begin
    VarClear(vr);
    if v.IsString then vr := v.GetStringValue else
    if v.IsBool then vr := v.GetBoolValue else
    if v.IsInt then vr := v.GetIntValue else
    if v.IsDouble then vr := v.GetDoubleValue else
    if v.IsUndefined then TVarData(vr).VType := varEmpty else
    if v.IsNull then TVarData(vr).VType := varNull else
    if v.IsArray then
      begin
        vr := VarArrayCreate([0, v.GetArrayLength], varVariant);
        for i := 0 to v.GetArrayLength - 1 do
        begin
          if not GetValue(pi, v.GetValueByIndex(i), vl) then Exit(False);
          VarArrayPut(vr, vl.AsVariant, i);
        end;
      end else
      Exit(False);
    TValue.Make(@vr, pi, ret);
    Result := True;
  end;

  function ProcessObject: Boolean;
  var
    ud: ICefv8Value;
    i: cint;// Pointer
    td: PTypeData;
    rt: TRttiType;
  begin
    if v.IsObject then
    begin
      ud := v.GetUserData as ICefV8Value;
      if (ud = nil) then Exit(False);
      rt := TRttiType(ud.GetValueByIndex(0).GetIntValue);
      td := GetTypeData(rt.Handle);

      if (rt.TypeKind = tkClass) and td.ClassType.InheritsFrom(GetTypeData(pi).ClassType) then
      begin
        i := ud.GetValueByIndex(1).GetIntValue;
        TValue.Make(@i, pi, ret);
      end else
        Exit(False);
    end else
      Exit(False);
    Result := True;
  end;

  function ProcessClass: Boolean;
  var
    ud: ICefv8Value;
    i: cint;// Pointer
    rt: TRttiType;
  begin
    if v.IsObject then
    begin
      ud := v.GetUserData as ICefV8Value;
      if (ud = nil) then Exit(False);
      rt := TRttiType(ud.GetValueByIndex(0).GetIntValue);
      if (rt.TypeKind = tkClassRef) then
      begin
        i := ud.GetValueByIndex(1).GetIntValue;
        TValue.Make(@i, pi, ret);
      end else
        Exit(False);
    end else
      Exit(False);
    Result := True;
  end;

  function ProcessRecord: Boolean;
  var
    r: TRttiField;
    f: TValue;
    rec: Pointer;
  begin
    if v.IsObject then
    begin
      TValue.Make(nil, pi, ret);
      rec := TValueData(ret).FValueData.GetReferenceToRawData;
      for r in FCtx.GetType(pi).GetFields do
      begin
        if not GetValue(r.FieldType.Handle, v.GetValueByKey(r.Name), f) then
          Exit(False);
        r.SetValue(rec, f);
      end;
      Result := True;
    end else
      Result := False;
  end;

  function ProcessInterface: Boolean;
  begin
    if pi = TypeInfo(ICefV8Value) then
    begin
      TValue.Make(@v, pi, ret);
      Result := True;
    end else
      Result := False; // todo
  end;
begin
  case pi.Kind of
    tkInteger, tkEnumeration: Result := ProcessInt;
    tkInt64: Result := ProcessInt64;
    tkUString: Result := ProcessUString;
    tkLString: Result := ProcessLString;
    tkWString: Result := ProcessWString;
    tkFloat: Result := ProcessFloat;
    tkSet: Result := ProcessSet;
    tkVariant: Result := ProcessVariant;
    tkClass: Result := ProcessObject;
    tkClassRef: Result := ProcessClass;
    tkRecord: Result := ProcessRecord;
    tkInterface: Result := ProcessInterface;
  else
    Result := False;
  end;
end;

function TCefRTTIExtension.SetValue(const v: TValue; var ret: ICefv8Value): Boolean;

  function ProcessRecord: Boolean;
  var
    rf: TRttiField;
    vl: TValue;
    ud, v8: ICefv8Value;
    rec: Pointer;
    rt: TRttiType;
  begin
    ud := TCefv8ValueRef.CreateArray(1);
    rt := FCtx.GetType(v.TypeInfo);
    ud.SetValueByIndex(0, TCefv8ValueRef.CreateInt(Integer(rt)));
    ret := TCefv8ValueRef.CreateObject(nil);
    ret.SetUserData(ud);
    rec := TValueData(v).FValueData.GetReferenceToRawData;
    {$IFDEF CEF_MULTI_THREADED_MESSAGE_LOOP}
    if FSyncMainThread then
    begin
      v8 := ret;
      TThread.Synchronize(nil, procedure
      var
        rf: TRttiField;
        o: ICefv8Value;
      begin
        for rf in rt.GetFields do
        begin
          vl := rf.GetValue(rec);
          SetValue(vl, o);
          v8.SetValueByKey(rf.Name, o, V8_PROPERTY_ATTRIBUTE_NONE);
        end;
      end)
    end else
    {$ENDIF}
      for rf in FCtx.GetType(v.TypeInfo).GetFields do
      begin
        vl := rf.GetValue(rec);
        if not SetValue(vl, v8) then
          Exit(False);
        ret.SetValueByKey(rf.Name, v8,  V8_PROPERTY_ATTRIBUTE_NONE);
      end;
    Result := True;
  end;

  function ProcessObject: Boolean;
  var
    m: TRttiMethod;
    p: TRttiProperty;
    fl: TRttiField;
    f: ICefv8Value;
    _r, _g, _s, ud: ICefv8Value;
    _a: TCefv8ValueArray;
    rt: TRttiType;
  begin
    rt := FCtx.GetType(v.TypeInfo);

    ud := TCefv8ValueRef.CreateArray(2);
    ud.SetValueByIndex(0, TCefv8ValueRef.CreateInt(Integer(rt)));
    ud.SetValueByIndex(1, TCefv8ValueRef.CreateInt(Integer(v.AsObject)));
    ret := TCefv8ValueRef.CreateObject(nil); // todo
    ret.SetUserData(ud);

    for m in rt.GetMethods do
      if m.Visibility > mvProtected then
      begin
        f := TCefv8ValueRef.CreateFunction(m.Name, Self);
        ret.SetValueByKey(m.Name, f, V8_PROPERTY_ATTRIBUTE_NONE);
      end;

    for p in rt.GetProperties do
      if (p.Visibility > mvProtected) then
      begin
        if _g = nil then _g := ret.GetValueByKey('__defineGetter__');
        if _s = nil then _s := ret.GetValueByKey('__defineSetter__');
        SetLength(_a, 2);
        _a[0] := TCefv8ValueRef.CreateString(p.Name);
        if p.IsReadable then
        begin
          _a[1] := TCefv8ValueRef.CreateFunction('$pg' + p.Name, Self);
          _r := _g.ExecuteFunction(ret, Length(_a), _a);
        end;
        if p.IsWritable then
        begin
          _a[1] := TCefv8ValueRef.CreateFunction('$ps' + p.Name, Self);
          _r := _s.ExecuteFunction(ret, Length(_a), _a);
        end;
      end;

    for fl in rt.GetFields do
      if (fl.Visibility > mvProtected) then
      begin
        if _g = nil then _g := ret.GetValueByKey('__defineGetter__');
        if _s = nil then _s := ret.GetValueByKey('__defineSetter__');

        SetLength(_a, 2);
        _a[0] := TCefv8ValueRef.CreateString(fl.Name);
        _a[1] := TCefv8ValueRef.CreateFunction('$vg' + fl.Name, Self);
        _r := _g.ExecuteFunction(ret, Length(_a), _a);
        _a[1] := TCefv8ValueRef.CreateFunction('$vs' + fl.Name, Self);
        _r := _s.ExecuteFunction(ret, Length(_a), _a);
      end;

    Result := True;
  end;

  function ProcessClass: Boolean;
  var
    m: TRttiMethod;
    f, ud: ICefv8Value;
    c: TClass;
    //proto: ICefv8Value;
    rt: TRttiType;
  begin
    c := v.AsClass;
    rt := FCtx.GetType(c);

    ud := TCefv8ValueRef.CreateArray(2);
    ud.SetValueByIndex(0, TCefv8ValueRef.CreateInt(Integer(rt)));
    ud.SetValueByIndex(1, TCefv8ValueRef.CreateInt(Integer(c)));
    ret := TCefv8ValueRef.CreateObject(nil); // todo
    ret.SetUserData(ud);

    if c <> nil then
    begin
      //proto := ret.GetValueByKey('__proto__');
      for m in rt.GetMethods do
        if (m.Visibility > mvProtected) and (m.MethodKind in [mkClassProcedure, mkClassFunction]) then
        begin
          f := TCefv8ValueRef.CreateFunction(m.Name, Self);
          ret.SetValueByKey(m.Name, f, V8_PROPERTY_ATTRIBUTE_NONE);
        end;
    end;

    Result := True;
  end;

  function ProcessVariant: Boolean;
  var
    vr: Variant;
  begin
    vr := v.AsVariant;
    case TVarData(vr).VType of
      varSmallint, varInteger, varShortInt:
        ret := TCefv8ValueRef.CreateInt(vr);
      varByte, varWord, varLongWord:
        ret := TCefv8ValueRef.CreateUInt(vr);
      varUString, varOleStr, varString:
        ret := TCefv8ValueRef.CreateString(vr);
      varSingle, varDouble, varCurrency, varUInt64, varInt64:
        ret := TCefv8ValueRef.CreateDouble(vr);
      varBoolean:
        ret := TCefv8ValueRef.CreateBool(vr);
      varNull:
        ret := TCefv8ValueRef.CreateNull;
      varEmpty:
        ret := TCefv8ValueRef.CreateUndefined;
    else
      ret := nil;
      Exit(False)
    end;
    Result := True;
  end;

  function ProcessInterface: Boolean;
  var
    m: TRttiMethod;
    f: ICefv8Value;
    ud: ICefv8Value;
    rt: TRttiType;
  begin
    rt := FCtx.GetType(v.TypeInfo);

    ud := TCefv8ValueRef.CreateArray(2);
    ud.SetValueByIndex(0, TCefv8ValueRef.CreateInt(Integer(rt)));
    {$IFDEF WIN32}
    ud.SetValueByIndex(1, TCefv8ValueRef.CreateInt(Integer(v.AsInterface)));
    {$ELSE}
    ud.SetValueByIndex(1, TCefv8ValueRef.CreateInt(Int64(v.AsInterface)));
    {$ENDIF}
    ret := TCefv8ValueRef.CreateObject(nil);
    ret.SetUserData(ud);

    for m in rt.GetMethods do
      if m.Visibility > mvProtected then
      begin
        f := TCefv8ValueRef.CreateFunction(m.Name, Self);
        ret.SetValueByKey(m.Name, f, V8_PROPERTY_ATTRIBUTE_NONE);
      end;

    Result := True;
  end;

  function ProcessFloat: Boolean;
  begin
    if v.TypeInfo = TypeInfo(TDateTime) then
      ret := TCefv8ValueRef.CreateDate(TWACef.DateTimeToCefTime(TValueData(v).FAsDouble)) else
      ret := TCefv8ValueRef.CreateDouble(v.AsExtended);
    Result := True;
  end;

begin
  case v.TypeInfo.Kind of
    tkUString, tkLString, tkWString, tkChar, tkWChar:
      ret := TCefv8ValueRef.CreateString(v.AsString);
    tkInteger: ret := TCefv8ValueRef.CreateInt(v.AsInteger);
    tkEnumeration:
      if v.TypeInfo = TypeInfo(Boolean) then
        ret := TCefv8ValueRef.CreateBool(v.AsBoolean) else
        ret := TCefv8ValueRef.CreateInt(TValueData(v).FAsSLong);
    tkFloat: if not ProcessFloat then Exit(False);
    tkInt64: ret := TCefv8ValueRef.CreateDouble(v.AsInt64);
    tkClass: if not ProcessObject then Exit(False);
    tkClassRef: if not ProcessClass then Exit(False);
    tkRecord: if not ProcessRecord then Exit(False);
    tkVariant: if not ProcessVariant then Exit(False);
    tkInterface: if not ProcessInterface then Exit(False);
  else
    Exit(False)
  end;
  Result := True;
end;

class procedure TCefRTTIExtension.Register(const name: ustring;
  const value: TValue{$IFDEF CEF_MULTI_THREADED_MESSAGE_LOOP}; SyncMainThread: Boolean{$ENDIF});
var
  Extension: TCefRTTIExtension;
begin
  Extension:=TCefRTTIExtension.Create(value
    {$IFDEF CEF_MULTI_THREADED_MESSAGE_LOOP}
    , SyncMainThread
    {$ENDIF}
    );
  TWACef.RegisterExtension(name,
    format('__defineSetter__(''%s'', function(v){native function $s();$s(v)});__defineGetter__(''%0:s'', function(){native function $g();return $g()});', [name]),
    Extension as ICefv8Handler);
end;

function TCefRTTIExtension.Execute(const aName: ustring; const aObject: ICefV8value; aArgumentsCount: csize_t; const aArguments: TCefv8ValueArray; var aRetval: ICefV8value; var aException: ustring): Boolean;
var
  p: PChar;
  ud: ICefv8Value;
  rt: TRttiType;
  val: TObject;
  cls: TClass;
  m: TRttiMethod;
  pr: TRttiProperty;
  vl: TRttiField;
  args: array of TValue;
  prm: TArray<TRttiParameter>;
  i: cint;
  ret: TValue;
begin
  Result := True;
  p := PChar(aName);
  m := nil;
  if aObject <> nil then
  begin
    ud := aObject.GetUserData as ICefV8Value;
    if ud <> nil then
    begin
      rt := TRttiType(ud.GetValueByIndex(0).GetIntValue);
      case rt.TypeKind of
        tkClass:
          begin
            val := TObject(ud.GetValueByIndex(1).GetIntValue);
            cls := GetTypeData(rt.Handle).ClassType;

            if p^ = '$' then
            begin
              inc(p);
              case p^ of
                'p':
                  begin
                    inc(p);
                    case p^ of
                    'g':
                      begin
                        inc(p);
                        pr := rt.GetProperty(p);
                        {$IFDEF CEF_MULTI_THREADED_MESSAGE_LOOP}
                        if FSyncMainThread then
                        begin
                          TThread.Synchronize(nil, procedure begin
                            ret := pr.GetValue(val);
                          end);
                          Exit(SetValue(ret, aRetval));
                        end else
                        {$ENDIF}
                          Exit(SetValue(pr.GetValue(val), aRetval));
                      end;
                    's':
                      begin
                        inc(p);
                        pr := rt.GetProperty(p);
                        if GetValue(pr.PropertyType.Handle, aArguments[0], ret) then
                        begin
                          {$IFDEF CEF_MULTI_THREADED_MESSAGE_LOOP}
                          if FSyncMainThread then
                            TThread.Synchronize(nil, procedure begin
                              pr.SetValue(val, ret) end) else
                          {$ENDIF}
                            pr.SetValue(val, ret);
                          Exit(True);
                        end else
                          Exit(False);
                      end;
                    end;
                  end;
                'v':
                  begin
                    inc(p);
                    case p^ of
                    'g':
                      begin
                        inc(p);
                        vl := rt.GetField(p);
                        {$IFDEF CEF_MULTI_THREADED_MESSAGE_LOOP}
                        if FSyncMainThread then
                        begin
                          TThread.Synchronize(nil, procedure begin
                            ret := vl.GetValue(val);
                          end);
                          Exit(SetValue(ret, aRetval));
                        end else
                        {$ENDIF}
                          Exit(SetValue(vl.GetValue(val), aRetval));
                      end;
                    's':
                      begin
                        inc(p);
                        vl := rt.GetField(p);
                        if GetValue(vl.FieldType.Handle, aArguments[0], ret) then
                        begin
                          {$IFDEF CEF_MULTI_THREADED_MESSAGE_LOOP}
                          if FSyncMainThread then
                            TThread.Synchronize(nil, procedure begin
                              vl.SetValue(val, ret) end) else
                          {$ENDIF}
                            vl.SetValue(val, ret);
                          Exit(True);
                        end else
                          Exit(False);
                      end;
                    end;
                  end;
              end;
            end else
              m := rt.GetMethod(aName);
          end;
        tkClassRef:
          begin
            val := nil;
            cls := TClass(ud.GetValueByIndex(1).GetIntValue);
            m := FCtx.GetType(cls).GetMethod(aName);
          end;
      else
        m := nil;
        cls := nil;
        val := nil;
      end;

      prm := m.GetParameters;
      i := Length(prm);
      if i = Length(aArguments) then
      begin
        SetLength(args, i);
        for i := 0 to i - 1 do
          if not GetValue(prm[i].ParamType.Handle, aArguments[i], args[i]) then
            Exit(False);

        case m.MethodKind of
          mkClassProcedure, mkClassFunction:
            {$IFDEF CEF_MULTI_THREADED_MESSAGE_LOOP}
            if FSyncMainThread then
              TThread.Synchronize(nil, procedure begin
                ret := m.Invoke(cls, args) end) else
            {$ENDIF}
              ret := m.Invoke(cls, args);
          mkProcedure, mkFunction:
            if (val <> nil) then
            begin
              {$IFDEF CEF_MULTI_THREADED_MESSAGE_LOOP}
              if FSyncMainThread then
                TThread.Synchronize(nil, procedure begin
                  ret := m.Invoke(val, args) end) else
              {$ENDIF}
                ret := m.Invoke(val, args);
            end else
              Exit(False)
        else
          Exit(False);
        end;

        if m.MethodKind in [mkClassFunction, mkFunction] then
          if not SetValue(ret, aRetval) then
            Exit(False);
      end else
        Exit(False);
    end else
    if p^ = '$' then
    begin
      inc(p);
      case p^ of
        'g': SetValue(FValue, aRetval);
        's': GetValue(FValue.TypeInfo, aArguments[0], FValue);
      else
        Exit(False);
      end;
    end else
      Exit(False);
  end else
    Exit(False);
end;
{$ENDIF}

//..............................................................................TCefFastRunFileDialogCallback
procedure TCefFastRunFileDialogCallback.OnFileDialogDismissed(SelectedAcceptFilter: cint; filePaths: TStrings);
begin
  FCallback(SelectedAcceptFilter, filePaths);
end;

constructor TCefFastRunFileDialogCallback.Create(
  callback: TCefRunFileDialogCallbackProc);
begin
  inherited Create;
  FCallback := callback;
end;

//..............................................................................TFastCefV8Accessor
constructor TCefFastV8Accessor.Create(
  const getter: TCefV8AccessorGetterProc;
  const setter: TCefV8AccessorSetterProc);
begin
  FGetter := getter;
  FSetter := setter;
end;

function TCefFastV8Accessor.Get(const aName: ustring; const aObject: ICefV8value; out aRetval: ICefV8value; const aException: ustring): Boolean;
begin
  if Assigned(FGetter)  then
    Result := FGetter(aName, aObject, aRetval, aException)
  else
    Result := False;
end;

function TCefFastV8Accessor._Set(const aName: ustring; const aObject: ICefV8value; const aValue: ICefV8value; const aException: ustring): Boolean;
begin
  if Assigned(FSetter)  then
    Result := FSetter(aName, aObject, aValue, aException)
  else
    Result := False;
end;

end.
