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

{$I WACef.inc}

unit WACefCApi;

interface

uses
  {$IFDEF FPC}
  ctypes,
  {$ENDIF}
  WACefTypes;

type
		//............................................................................cef_app_capi
	PCefApp=^TCefApp;
	//............................................................................cef_auth_callback_capi
	PCefAuthCallback=^TCefAuthCallback;
	//............................................................................cef_base_capi
	PCefBase=^TCefBase;
	//............................................................................cef_browser_capi
	PCefBrowser=^TCefBrowser;
	PCefRunFileDialogCallback=^TCefRunFileDialogCallback;
	PCefBrowserHost=^TCefBrowserHost;
	//............................................................................cef_browser_process_handler_capi
	PCefBrowserProcessHandler=^TCefBrowserProcessHandler;
	//............................................................................cef_callback_capi
	PCefCallback=^TCefCallback;
	PCefCompletionCallback=^TCefCompletionCallback;
	//............................................................................cef_client_capi
	PCefClient=^TCefClient;
	//............................................................................cef_command_line_capi
	PCefCommandLine=^TCefCommandLine;
	//............................................................................cef_context_menu_handler_capi
	PCefContextMenuHandler=^TCefContextMenuHandler;
	PCefContextMenuParams=^TCefContextMenuParams;
	//............................................................................cef_cookie_capi
	PCefCookieManager=^TCefCookieManager;
	PCefCookieVisitor=^TCefCookieVisitor;
	//............................................................................cef_dialog_handler_capi
	PCefFileDialogCallback=^TCefFileDialogCallback;
	PCefDialogHandler=^TCefDialogHandler;
	//............................................................................cef_display_handler_capi
	PCefDisplayHandler=^TCefDisplayHandler;
	//............................................................................cef_dom_capi
	PCefDomvisitor=^TCefDomvisitor;
	PCefDomdocument=^TCefDomdocument;
	PCefDomnode=^TCefDomnode;
	//............................................................................cef_download_handler_capi
	PCefBeforeDownloadCallback=^TCefBeforeDownloadCallback;
	PCefDownloadItemCallback=^TCefDownloadItemCallback;
	PCefDownloadHandler=^TCefDownloadHandler;
	//............................................................................cef_download_item_capi
	PCefDownloadItem=^TCefDownloadItem;
	//............................................................................cef_drag_data_capi
	PCefDragData=^TCefDragData;
	//............................................................................cef_drag_handler_capi
	PCefDragHandler=^TCefDragHandler;
	//............................................................................cef_focus_handler_capi
	PCefFocusHandler=^TCefFocusHandler;
	//............................................................................cef_frame_capi
	PCefFrame=^TCefFrame;
	//............................................................................cef_geolocation_capi
	PCefGetGeolocationCallback=^TCefGetGeolocationCallback;
	//............................................................................cef_geolocation_handler_capi
	PCefGeolocationCallback=^TCefGeolocationCallback;
	PCefGeolocationHandler=^TCefGeolocationHandler;
	//............................................................................cef_jsdialog_handler_capi
	PCefJsdialogCallback=^TCefJsdialogCallback;
	PCefJsdialogHandler=^TCefJsdialogHandler;
	//............................................................................cef_keyboard_handler_capi
	PCefKeyboardHandler=^TCefKeyboardHandler;
	//............................................................................cef_life_span_handler_capi
	PCefLifeSpanHandler=^TCefLifeSpanHandler;
	//............................................................................cef_load_handler_capi
	PCefLoadHandler=^TCefLoadHandler;
	//............................................................................cef_menu_model_capi
	PCefMenuModel=^TCefMenuModel;
	//............................................................................cef_origin_whitelist_capi
	//............................................................................cef_path_util_capi
	//............................................................................cef_print_handler_capi
	PCefPrintDialogCallback=^TCefPrintDialogCallback;
	PCefPrintJobCallback=^TCefPrintJobCallback;
	PCefPrintHandler=^TCefPrintHandler;
	//............................................................................cef_print_settings_capi
	PCefPrintSettings=^TCefPrintSettings;
	//............................................................................cef_process_message_capi
	PCefProcessMessage=^TCefProcessMessage;
	//............................................................................cef_process_util_capi
	//............................................................................cef_render_handler_capi
	PCefRenderHandler=^TCefRenderHandler;
	//............................................................................cef_render_process_handler_capi
	PCefRenderProcessHandler=^TCefRenderProcessHandler;
	//............................................................................cef_request_capi
	PCefRequest=^TCefRequest;
	PCefPostData=^TCefPostData;
	PCefPostDataElement=^TCefPostDataElement;
	//............................................................................cef_request_context_capi
	PCefRequestContext=^TCefRequestContext;
	//............................................................................cef_request_context_handler_capi
	PCefRequestContextHandler=^TCefRequestContextHandler;
	//............................................................................cef_request_handler_capi
	PCefQuotaCallback=^TCefQuotaCallback;
	PCefAllowCertificateErrorCallback=^TCefAllowCertificateErrorCallback;
	PCefRequestHandler=^TCefRequestHandler;
	//............................................................................cef_resource_bundle_handler_capi
	PCefResourceBundleHandler=^TCefResourceBundleHandler;
	//............................................................................cef_resource_handler_capi
	PCefResourceHandler=^TCefResourceHandler;
	//............................................................................cef_response_capi
	PCefResponse=^TCefResponse;
	//............................................................................cef_scheme_capi
	PCefSchemeRegistrar=^TCefSchemeRegistrar;
	PCefSchemeHandlerFactory=^TCefSchemeHandlerFactory;
	//............................................................................cef_stream_capi
	PCefReadHandler=^TCefReadHandler;
	PCefStreamReader=^TCefStreamReader;
	PCefWriteHandler=^TCefWriteHandler;
	PCefStreamWriter=^TCefStreamWriter;
	//............................................................................cef_string_visitor_capi
	PCefStringVisitor=^TCefStringVisitor;
	//............................................................................cef_task_capi
	PCefTask=^TCefTask;
	PCefTaskRunner=^TCefTaskRunner;
	//............................................................................cef_trace_capi
	PCefEndTracingCallback=^TCefEndTracingCallback;
	//............................................................................cef_urlrequest_capi
	PCefUrlrequest=^TCefUrlrequest;
	PCefUrlrequestClient=^TCefUrlrequestClient;
	//............................................................................cef_url_capi
	//............................................................................cef_v8_capi
	PCefV8context=^TCefV8context;
	PCefV8handler=^TCefV8handler;
	PCefV8accessor=^TCefV8accessor;
	PCefV8exception=^TCefV8exception;
	PCefV8value=^TCefV8value;
	PCefV8stackTrace=^TCefV8stackTrace;
	PCefV8stackFrame=^TCefV8stackFrame;
	//............................................................................cef_values_capi
	PCefBinaryValue=^TCefBinaryValue;
	PCefDictionaryValue=^TCefDictionaryValue;
	PCefListValue=^TCefListValue;
	//............................................................................cef_web_plugin_capi
	PCefWebPluginInfo=^TCefWebPluginInfo;
	PCefWebPluginInfoVisitor=^TCefWebPluginInfoVisitor;
	PCefWebPluginUnstableCallback=^TCefWebPluginUnstableCallback;
	//............................................................................cef_xml_reader_capi
	PCefXmlReader=^TCefXmlReader;
	//............................................................................cef_zip_reader_capi
	PCefZipReader=^TCefZipReader;


  TCefPostDataElementArray = array[0..(High(Integer) div SizeOf(PCefPostDataElement)) - 1] of PCefPostDataElement;
  PCefPostDataElementArray = ^TCefPostDataElementArray;
  PCefV8ValueArray = array[0..(High(Integer) div SizeOf(PCefV8Value)) - 1] of PCefV8Value;
  PPCefV8Value = ^PCefV8ValueArray;

  //............................................................................cef_base_capi
	// Structure defining the reference count implementation functions. All
	// framework structures must include the cef_base_t structure first
	TCefBase = record
		// Size of the data structure.
		size: csize_t;
		// Called to increment the reference count for the object. Should be called
		// for every new copy of a pointer to a given object.
		add_ref: procedure(self: PCefBase); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Called to decrement the reference count for the object. If the reference
		// count falls to 0 the object should self-delete. Returns true (1) if the
		// resulting reference count is 0.
		release: function(self: PCefBase): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns true (1) if the current reference count is 1.
		has_one_ref: function(self: PCefBase): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
	end;

 	//............................................................................cef_app_capi
	// Implement this structure to provide handler implementations. Methods will be
	// called by the process and/or thread indicated
	TCefApp = record
		// Base structure.
		base: TCefBase;
		// Provides an opportunity to view and/or modify command-line arguments before
		// processing by CEF and Chromium. The |process_type| value will be NULL for
		// the browser process. Do not keep a reference to the cef_command_line_t
		// object passed to this function. The CefSettings.command_line_args_disabled
		// value can be used to start with an NULL command-line object. Any values
		// specified in CefSettings that equate to command-line arguments will be set
		// before this function is called. Be cautious when using this function to
		// modify command-line arguments for non-browser processes as this may result
		// in undefined behavior including crashes.
		on_before_command_line_processing: procedure(self: PCefApp; const process_type: PCefString; command_line: PCefCommandLine); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Provides an opportunity to register custom schemes. Do not keep a reference
		// to the |registrar| object. This function is called on the main thread for
		// each process and the registered schemes should be the same across all
		// processes.
		on_register_custom_schemes: procedure(self: PCefApp; registrar: PCefSchemeRegistrar); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Return the handler for resource bundle events. If
		// CefSettings.pack_loading_disabled is true (1) a handler must be returned.
		// If no handler is returned resources will be loaded from pack files. This
		// function is called by the browser and render processes on multiple threads.
		get_resource_bundle_handler: function(self: PCefApp): PCefResourceBundleHandler; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Return the handler for functionality specific to the browser process. This
		// function is called on multiple threads in the browser process.
		get_browser_process_handler: function(self: PCefApp): PCefBrowserProcessHandler; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Return the handler for functionality specific to the render process. This
		// function is called on the render process main thread.
		get_render_process_handler: function(self: PCefApp): PCefRenderProcessHandler; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
	end;

	//............................................................................cef_auth_callback_capi
	// Callback structure used for asynchronous continuation of authentication
	// requests
	TCefAuthCallback = record
		// Base structure.
		base: TCefBase;
		// Continue the authentication request.
		cont: procedure(self: PCefAuthCallback; const username: PCefString; const password: PCefString); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Cancel the authentication request.
		cancel: procedure(self: PCefAuthCallback); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
	end;

  //............................................................................cef_browser_capi
	// Structure used to represent a browser window. When used in the browser
	// process the functions of this structure may be called on any thread unless
	// otherwise indicated in the comments. When used in the render process the
	// functions of this structure may only be called on the main thread
	TCefBrowser = record
		// Base structure.
		base: TCefBase;
		// Returns the browser host object. This function can only be called in the
		// browser process.
		get_host: function(self: PCefBrowser): PCefBrowserHost; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns true (1) if the browser can navigate backwards.
		can_go_back: function(self: PCefBrowser): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Navigate backwards.
		go_back: procedure(self: PCefBrowser); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns true (1) if the browser can navigate forwards.
		can_go_forward: function(self: PCefBrowser): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Navigate forwards.
		go_forward: procedure(self: PCefBrowser); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns true (1) if the browser is currently loading.
		is_loading: function(self: PCefBrowser): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Reload the current page.
		reload: procedure(self: PCefBrowser); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Reload the current page ignoring any cached data.
		reload_ignore_cache: procedure(self: PCefBrowser); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Stop loading the page.
		stop_load: procedure(self: PCefBrowser); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the globally unique identifier for this browser.
		get_identifier: function(self: PCefBrowser): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns true (1) if this object is pointing to the same handle as |that|
		// object.
		is_same: function(self: PCefBrowser; that: PCefBrowser): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns true (1) if the window is a popup window.
		is_popup: function(self: PCefBrowser): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns true (1) if a document has been loaded in the browser.
		has_document: function(self: PCefBrowser): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the main (top-level) frame for the browser window.
		get_main_frame: function(self: PCefBrowser): PCefFrame; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the focused frame for the browser window.
		get_focused_frame: function(self: PCefBrowser): PCefFrame; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the frame with the specified identifier, or NULL if not found.
		get_frame_byident: function(self: PCefBrowser; identifier: cint64): PCefFrame; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the frame with the specified name, or NULL if not found.
		get_frame: function(self: PCefBrowser; const name: PCefString): PCefFrame; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the number of frames that currently exist.
		get_frame_count: function(self: PCefBrowser): csize_t; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the identifiers of all existing frames.
		get_frame_identifiers: procedure(self: PCefBrowser; identifiersCount: pcsize_t; identifiers: pcint64); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the names of all existing frames.
		get_frame_names: procedure(self: PCefBrowser; names: TCefStringList); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Send a message to the specified |target_process|. Returns true (1) if the
		// message was sent successfully.
		send_process_message: function(self: PCefBrowser; target_process: TCefProcessId; message: PCefProcessMessage): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
	end;

	// Callback structure for cef_browser_host_t::RunFileDialog. The functions of
	// this structure will be called on the browser process UI thread
	TCefRunFileDialogCallback = record
		// Base structure.
		base: TCefBase;
		// Called asynchronously after the file dialog is dismissed. If the selection
		// was successful |file_paths| will be a single value or a list of values
		// depending on the dialog mode. If the selection was cancelled |file_paths|
		// will be NULL.
		cont: procedure(self: PCefRunFileDialogCallback; browser_host: PCefBrowserHost; file_paths: TCefStringList); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
	end;

	// Structure used to represent the browser process aspects of a browser window.
	// The functions of this structure can only be called in the browser process.
	// They may be called on any thread in that process unless otherwise indicated
	// in the comments
	TCefBrowserHost = record
		// Base structure.
		base: TCefBase;
		// Returns the hosted browser object.
		get_browser: function(self: PCefBrowserHost): PCefBrowser; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Request that the browser close. The JavaScript 'onbeforeunload' event will
		// be fired. If |force_close| is false (0) the event handler, if any, will be
		// allowed to prompt the user and the user can optionally cancel the close. If
		// |force_close| is true (1) the prompt will not be displayed and the close
		// will proceed. Results in a call to cef_life_span_handler_t::do_close() if
		// the event handler allows the close or if |force_close| is true (1). See
		// cef_life_span_handler_t::do_close() documentation for additional usage
		// information.
		close_browser: procedure(self: PCefBrowserHost; force_close: cint); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Set whether the browser is focused.
		set_focus: procedure(self: PCefBrowserHost; focus: cint); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Set whether the window containing the browser is visible
		// (minimized/unminimized, app hidden/unhidden, etc). Only used on Mac OS X.
		set_window_visibility: procedure(self: PCefBrowserHost; visible: cint); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Retrieve the window handle for this browser.
		get_window_handle: function(self: PCefBrowserHost): TCefWindowHandle; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Retrieve the window handle of the browser that opened this browser. Will
		// return NULL for non-popup windows. This function can be used in combination
		// with custom handling of modal windows.
		get_opener_window_handle: function(self: PCefBrowserHost): TCefWindowHandle; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the client for this browser.
		get_client: function(self: PCefBrowserHost): PCefClient; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the request context for this browser.
		get_request_context: function(self: PCefBrowserHost): PCefRequestContext; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Get the current zoom level. The default zoom level is 0.0. This function
		// can only be called on the UI thread.
		get_zoom_level: function(self: PCefBrowserHost): cdouble; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Change the zoom level to the specified value. Specify 0.0 to reset the zoom
		// level. If called on the UI thread the change will be applied immediately.
		// Otherwise, the change will be applied asynchronously on the UI thread.
		set_zoom_level: procedure(self: PCefBrowserHost; zoomLevel: cdouble); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Call to run a file chooser dialog. Only a single file chooser dialog may be
		// pending at any given time. |mode| represents the type of dialog to display.
		// |title| to the title to be used for the dialog and may be NULL to show the
		// default title ("Open" or "Save" depending on the mode). |default_file_name|
		// is the default file name to select in the dialog. |accept_types| is a list
		// of valid lower-cased MIME types or file extensions specified in an input
		// element and is used to restrict selectable files to such types. |callback|
		// will be executed after the dialog is dismissed or immediately if another
		// dialog is already pending. The dialog will be initiated asynchronously on
		// the UI thread.
		run_file_dialog: procedure(self: PCefBrowserHost; mode: TCefFileDialogMode; const title: PCefString; const default_file_name: PCefString; accept_types: TCefStringList; callback: PCefRunFileDialogCallback); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Download the file at |url| using cef_download_handler_t.
		start_download: procedure(self: PCefBrowserHost; const url: PCefString); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Print the current browser contents.
		print: procedure(self: PCefBrowserHost); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Search for |searchText|. |identifier| can be used to have multiple searches
		// running simultaniously. |forward| indicates whether to search forward or
		// backward within the page. |matchCase| indicates whether the search should
		// be case-sensitive. |findNext| indicates whether this is the first request
		// or a follow-up.
		find: procedure(self: PCefBrowserHost; identifier: cint; const searchText: PCefString; forward: cint; matchCase: cint; findNext: cint); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Cancel all searches that are currently going on.
		stop_finding: procedure(self: PCefBrowserHost; clearSelection: cint); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Open developer tools in its own window. If |inspect_element_at| is non-
		// NULL the element at the specified (x,y) location will be inspected.
		show_dev_tools: procedure(self: PCefBrowserHost; const windowInfo: PCefWindowInfo; client: PCefClient; const settings: PCefBrowserSettings; const inspect_element_at: PCefPoint); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Explicitly close the developer tools window if one exists for this browser
		// instance.
		close_dev_tools: procedure(self: PCefBrowserHost); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Set whether mouse cursor change is disabled.
		set_mouse_cursor_change_disabled: procedure(self: PCefBrowserHost; disabled: cint); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns true (1) if mouse cursor change is disabled.
		is_mouse_cursor_change_disabled: function(self: PCefBrowserHost): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// If a misspelled word is currently selected in an editable node calling this
		// function will replace it with the specified |word|.
		replace_misspelling: procedure(self: PCefBrowserHost; const word: PCefString); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Add the specified |word| to the spelling dictionary.
		add_word_to_dictionary: procedure(self: PCefBrowserHost; const word: PCefString); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns true (1) if window rendering is disabled.
		is_window_rendering_disabled: function(self: PCefBrowserHost): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Notify the browser that the widget has been resized. The browser will first
		// call cef_render_handler_t::GetViewRect to get the new size and then call
		// cef_render_handler_t::OnPaint asynchronously with the updated regions. This
		// function is only used when window rendering is disabled.
		was_resized: procedure(self: PCefBrowserHost); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Notify the browser that it has been hidden or shown. Layouting and
		// cef_render_handler_t::OnPaint notification will stop when the browser is
		// hidden. This function is only used when window rendering is disabled.
		was_hidden: procedure(self: PCefBrowserHost; hidden: cint); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Send a notification to the browser that the screen info has changed. The
		// browser will then call cef_render_handler_t::GetScreenInfo to update the
		// screen information with the new values. This simulates moving the webview
		// window from one display to another, or changing the properties of the
		// current display. This function is only used when window rendering is
		// disabled.
		notify_screen_info_changed: procedure(self: PCefBrowserHost); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Invalidate the view. The browser will call cef_render_handler_t::OnPaint
		// asynchronously. This function is only used when window rendering is
		// disabled.
		invalidate: procedure(self: PCefBrowserHost; _type: TCefPaintElementType); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Send a key event to the browser.
		send_key_event: procedure(self: PCefBrowserHost; const event: PCefKeyEvent); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Send a mouse click event to the browser. The |x| and |y| coordinates are
		// relative to the upper-left corner of the view.
		send_mouse_click_event: procedure(self: PCefBrowserHost; const event: PCefMouseEvent; _type: TCefMouseButtonType; mouseUp: cint; clickCount: cint); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Send a mouse move event to the browser. The |x| and |y| coordinates are
		// relative to the upper-left corner of the view.
		send_mouse_move_event: procedure(self: PCefBrowserHost; const event: PCefMouseEvent; mouseLeave: cint); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Send a mouse wheel event to the browser. The |x| and |y| coordinates are
		// relative to the upper-left corner of the view. The |deltaX| and |deltaY|
		// values represent the movement delta in the X and Y directions respectively.
		// In order to scroll inside select popups with window rendering disabled
		// cef_render_handler_t::GetScreenPoint should be implemented properly.
		send_mouse_wheel_event: procedure(self: PCefBrowserHost; const event: PCefMouseEvent; deltaX: cint; deltaY: cint); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Send a focus event to the browser.
		send_focus_event: procedure(self: PCefBrowserHost; setFocus: cint); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Send a capture lost event to the browser.
		send_capture_lost_event: procedure(self: PCefBrowserHost); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Notify the browser that the window hosting it is about to be moved or
		// resized. This function is only used on Windows and Linux.
		notify_move_or_resize_started: procedure(self: PCefBrowserHost); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Get the NSTextInputContext implementation for enabling IME on Mac when
		// window rendering is disabled.
		get_nstext_input_context: function(self: PCefBrowserHost): TCefTextInputContext; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Handles a keyDown event prior to passing it through the NSTextInputClient
		// machinery.
		handle_key_event_before_text_input_client: procedure(self: PCefBrowserHost; keyEvent: TCefEventHandle); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Performs any additional actions after NSTextInputClient handles the event.
		handle_key_event_after_text_input_client: procedure(self: PCefBrowserHost; keyEvent: TCefEventHandle); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Call this function when the user drags the mouse into the web view (before
		// calling DragTargetDragOver/DragTargetLeave/DragTargetDrop). |drag_data|
		// should not contain file contents as this type of data is not allowed to be
		// dragged into the web view. File contents can be removed using
		// cef_drag_data_t::ResetFileContents (for example, if |drag_data| comes from
		// cef_render_handler_t::StartDragging). This function is only used when
		// window rendering is disabled.
		drag_target_drag_enter: procedure(self: PCefBrowserHost; drag_data: PCefDragData; const event: PCefMouseEvent; allowed_ops: TCefDragOperationsMask); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Call this function each time the mouse is moved across the web view during
		// a drag operation (after calling DragTargetDragEnter and before calling
		// DragTargetDragLeave/DragTargetDrop). This function is only used when window
		// rendering is disabled.
		drag_target_drag_over: procedure(self: PCefBrowserHost; const event: PCefMouseEvent; allowed_ops: TCefDragOperationsMask); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Call this function when the user drags the mouse out of the web view (after
		// calling DragTargetDragEnter). This function is only used when window
		// rendering is disabled.
		drag_target_drag_leave: procedure(self: PCefBrowserHost); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Call this function when the user completes the drag operation by dropping
		// the object onto the web view (after calling DragTargetDragEnter). The
		// object being dropped is |drag_data|, given as an argument to the previous
		// DragTargetDragEnter call. This function is only used when window rendering
		// is disabled.
		drag_target_drop: procedure(self: PCefBrowserHost; const event: PCefMouseEvent); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Call this function when the drag operation started by a
		// cef_render_handler_t::StartDragging call has ended either in a drop or by
		// being cancelled. |x| and |y| are mouse coordinates relative to the upper-
		// left corner of the view. If the web view is both the drag source and the
		// drag target then all DragTarget* functions should be called before
		// DragSource* mthods. This function is only used when window rendering is
		// disabled.
		drag_source_ended_at: procedure(self: PCefBrowserHost; x: cint; y: cint; op: TCefDragOperationsMask); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Call this function when the drag operation started by a
		// cef_render_handler_t::StartDragging call has completed. This function may
		// be called immediately without first calling DragSourceEndedAt to cancel a
		// drag operation. If the web view is both the drag source and the drag target
		// then all DragTarget* functions should be called before DragSource* mthods.
		// This function is only used when window rendering is disabled.
		drag_source_system_drag_ended: procedure(self: PCefBrowserHost); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
	end;

	//............................................................................cef_browser_process_handler_capi
	// Structure used to implement browser process callbacks. The functions of this
	// structure will be called on the browser process main thread unless otherwise
	// indicated
	TCefBrowserProcessHandler = record
		// Base structure.
		base: TCefBase;
		// Called on the browser process UI thread immediately after the CEF context
		// has been initialized.
		on_context_initialized: procedure(self: PCefBrowserProcessHandler); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Called before a child process is launched. Will be called on the browser
		// process UI thread when launching a render process and on the browser
		// process IO thread when launching a GPU or plugin process. Provides an
		// opportunity to modify the child process command line. Do not keep a
		// reference to |command_line| outside of this function.
		on_before_child_process_launch: procedure(self: PCefBrowserProcessHandler; command_line: PCefCommandLine); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Called on the browser process IO thread after the main thread has been
		// created for a new render process. Provides an opportunity to specify extra
		// information that will be passed to
		// cef_render_process_handler_t::on_render_thread_created() in the render
		// process. Do not keep a reference to |extra_info| outside of this function.
		on_render_process_thread_created: procedure(self: PCefBrowserProcessHandler; extra_info: PCefListValue); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Return the handler for printing on Linux. If a print handler is not
		// provided then printing will not be supported on the Linux platform.
		get_print_handler: function(self: PCefBrowserProcessHandler): PCefPrintHandler; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
	end;

	//............................................................................cef_callback_capi
	// Generic callback structure used for asynchronous continuation
	TCefCallback = record
		// Base structure.
		base: TCefBase;
		// Continue processing.
		cont: procedure(self: PCefCallback); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Cancel processing.
		cancel: procedure(self: PCefCallback); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
	end;

	// Generic callback structure used for asynchronous completion
	TCefCompletionCallback = record
		// Base structure.
		base: TCefBase;
		// Method that will be called once the task is complete.
		on_complete: procedure(self: PCefCompletionCallback); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
	end;

	//............................................................................cef_client_capi
	// Implement this structure to provide handler implementations
	TCefClient = record
		// Base structure.
		base: TCefBase;
		// Return the handler for context menus. If no handler is provided the default
		// implementation will be used.
		get_context_menu_handler: function(self: PCefClient): PCefContextMenuHandler; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Return the handler for dialogs. If no handler is provided the default
		// implementation will be used.
		get_dialog_handler: function(self: PCefClient): PCefDialogHandler; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Return the handler for browser display state events.
		get_display_handler: function(self: PCefClient): PCefDisplayHandler; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Return the handler for download events. If no handler is returned downloads
		// will not be allowed.
		get_download_handler: function(self: PCefClient): PCefDownloadHandler; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Return the handler for drag events.
		get_drag_handler: function(self: PCefClient): PCefDragHandler; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Return the handler for focus events.
		get_focus_handler: function(self: PCefClient): PCefFocusHandler; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Return the handler for geolocation permissions requests. If no handler is
		// provided geolocation access will be denied by default.
		get_geolocation_handler: function(self: PCefClient): PCefGeolocationHandler; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Return the handler for JavaScript dialogs. If no handler is provided the
		// default implementation will be used.
		get_jsdialog_handler: function(self: PCefClient): PCefJsdialogHandler; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Return the handler for keyboard events.
		get_keyboard_handler: function(self: PCefClient): PCefKeyboardHandler; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Return the handler for browser life span events.
		get_life_span_handler: function(self: PCefClient): PCefLifeSpanHandler; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Return the handler for browser load status events.
		get_load_handler: function(self: PCefClient): PCefLoadHandler; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Return the handler for off-screen rendering events.
		get_render_handler: function(self: PCefClient): PCefRenderHandler; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Return the handler for browser request events.
		get_request_handler: function(self: PCefClient): PCefRequestHandler; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Called when a new message is received from a different process. Return true
		// (1) if the message was handled or false (0) otherwise. Do not keep a
		// reference to or attempt to access the message outside of this callback.
		on_process_message_received: function(self: PCefClient; browser: PCefBrowser; source_process: TCefProcessId; message: PCefProcessMessage): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
	end;

	//............................................................................cef_command_line_capi
	TCefCommandLine=record
		// Base structure.
		base:TCefBase;
		// Returns true (1) if this object is valid. Do not call any other functions
		// if this function returns false (0).
		is_valid:function(self:PCefCommandLine): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns true (1) if the values of this object are read-only. Some APIs may
		// expose read-only objects.
		is_read_only:function(self:PCefCommandLine): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns a writable copy of this object.
		copy:function(self:PCefCommandLine):PCefCommandLine; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Initialize the command line with the specified |argc| and |argv| values.
		// The first argument must be the name of the program. This function is only
		// supported on non-Windows platforms.
		init_from_argv:procedure(self:PCefCommandLine; argc: cint; const argv:PPAnsiChar); {$IFNDEF UNIX}{$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF}{$ELSE}cdecl{$ENDIF};
		// Initialize the command line with the string returned by calling
		// GetCommandLineW(). This function is only supported on Windows.
		init_from_string:procedure(self:PCefCommandLine; const command_line:PCefString); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Reset the command-line switches and arguments but leave the program
		// component unchanged.
		reset:procedure(self:PCefCommandLine); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Retrieve the original command line string as a vector of strings. The argv
		// array: { program, [(--|-|/)switch[=value]]*, [--], [argument]*
    get_argv:procedure(self: PCefCommandLine; argv: TCefStringList); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
    // Constructs and returns the represented command line string. Use this
    // function cautiously because quoting behavior is unclear.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_command_line_string:function(self: PCefCommandLine):PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
    // Get the program part of the command line string (the first item).
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_program:function(self: PCefCommandLine):PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
    // Set the program part of the command line string (the first item).
    set_program:procedure(self: PCefCommandLine; const _program: PCefString); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
    // Returns true (1) if the command line has switches.
    has_switches:function(self: PCefCommandLine): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
    // Returns true (1) if the command line contains the given switch.
    has_switch:function(self: PCefCommandLine; const name:PCefString): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
    // Returns the value associated with the given switch. If the switch has no
    // value or isn't present this function returns the NULL string.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_switch_value:function(self: PCefCommandLine; const name:PCefString):PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
    // Returns the map of switch names and values. If a switch has no value an
    // NULL string is returned.
    get_switches:procedure(self: PCefCommandLine; switches:TCefStringMap); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
    // Add a switch to the end of the command line. If the switch has no value
    // pass an NULL value string.
    append_switch:procedure(self: PCefCommandLine; const name:PCefString); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
    // Add a switch with the specified value to the end of the command line.
    append_switch_with_value:procedure(self: PCefCommandLine; const name:PCefString; const value:PCefString); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
    // True if there are remaining command line arguments.
    has_arguments:function(self: PCefCommandLine): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
    // Get the remaining command line arguments.
    get_arguments:procedure(self: PCefCommandLine; arguments:TCefStringList); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
    // Add an argument to the end of the command line.
    append_argument:procedure(self: PCefCommandLine; const argument:PCefString); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
    // Insert a command before the current command. Common for debuggers, like
    // "valgrind" or "gdb --args".
    prepend_wrapper:procedure(self: PCefCommandLine; const wrapper:PCefString); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
	end;

	//............................................................................cef_context_menu_handler_capi
	// Implement this structure to handle context menu events. The functions of this
	// structure will be called on the UI thread
	TCefContextMenuHandler = record
		// Base structure.
		base: TCefBase;
		// Called before a context menu is displayed. |params| provides information
		// about the context menu state. |model| initially contains the default
		// context menu. The |model| can be cleared to show no context menu or
		// modified to show a custom menu. Do not keep references to |params| or
		// |model| outside of this callback.
		on_before_context_menu: procedure(self: PCefContextMenuHandler; browser: PCefBrowser; frame: PCefFrame; params: PCefContextMenuParams; model: PCefMenuModel); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Called to execute a command selected from the context menu. Return true (1)
		// if the command was handled or false (0) for the default implementation. See
		// cef_menu_id_t for the command ids that have default implementations. All
		// user-defined command ids should be between MENU_ID_USER_FIRST and
		// MENU_ID_USER_LAST. |params| will have the same values as what was passed to
		// on_before_context_menu(). Do not keep a reference to |params| outside of
		// this callback.
		on_context_menu_command: function(self: PCefContextMenuHandler; browser: PCefBrowser; frame: PCefFrame; params: PCefContextMenuParams; command_id: cint; event_flags: TCefEventFlags): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Called when the context menu is dismissed irregardless of whether the menu
		// was NULL or a command was selected.
		on_context_menu_dismissed: procedure(self: PCefContextMenuHandler; browser: PCefBrowser; frame: PCefFrame); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
	end;

	// Provides information about the context menu state. The ethods of this
	// structure can only be accessed on browser process the UI thread
	TCefContextMenuParams = record
		// Base structure.
		base: TCefBase;
		// Returns the X coordinate of the mouse where the context menu was invoked.
		// Coords are relative to the associated RenderView's origin.
		get_xcoord: function(self: PCefContextMenuParams): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the Y coordinate of the mouse where the context menu was invoked.
		// Coords are relative to the associated RenderView's origin.
		get_ycoord: function(self: PCefContextMenuParams): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns flags representing the type of node that the context menu was
		// invoked on.
		get_type_flags: function(self: PCefContextMenuParams): TCefContextMenuTypeFlags; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the URL of the link, if any, that encloses the node that the
		// context menu was invoked on.
		// The resulting string must be freed by calling cef_string_userfree_free().
		get_link_url: function(self: PCefContextMenuParams): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the link URL, if any, to be used ONLY for "copy link address". We
		// don't validate this field in the frontend process.
		// The resulting string must be freed by calling cef_string_userfree_free().
		get_unfiltered_link_url: function(self: PCefContextMenuParams): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the source URL, if any, for the element that the context menu was
		// invoked on. Example of elements with source URLs are img, audio, and video.
		// The resulting string must be freed by calling cef_string_userfree_free().
		get_source_url: function(self: PCefContextMenuParams): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns true (1) if the context menu was invoked on an image which has non-
		// NULL contents.
		has_image_contents: function(self: PCefContextMenuParams): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the URL of the top level page that the context menu was invoked on.
		// The resulting string must be freed by calling cef_string_userfree_free().
		get_page_url: function(self: PCefContextMenuParams): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the URL of the subframe that the context menu was invoked on.
		// The resulting string must be freed by calling cef_string_userfree_free().
		get_frame_url: function(self: PCefContextMenuParams): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the character encoding of the subframe that the context menu was
		// invoked on.
		// The resulting string must be freed by calling cef_string_userfree_free().
		get_frame_charset: function(self: PCefContextMenuParams): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the type of context node that the context menu was invoked on.
		get_media_type: function(self: PCefContextMenuParams): TCefContextMenuMediaType; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns flags representing the actions supported by the media element, if
		// any, that the context menu was invoked on.
		get_media_state_flags: function(self: PCefContextMenuParams): TCefContextMenuMediaStateFlags; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the text of the selection, if any, that the context menu was
		// invoked on.
		// The resulting string must be freed by calling cef_string_userfree_free().
		get_selection_text: function(self: PCefContextMenuParams): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the text of the misspelled word, if any, that the context menu was
		// invoked on.
		// The resulting string must be freed by calling cef_string_userfree_free().
		get_misspelled_word: function(self: PCefContextMenuParams): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the hash of the misspelled word, if any, that the context menu was
		// invoked on.
		get_misspelling_hash: function(self: PCefContextMenuParams): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns true (1) if suggestions exist, false (0) otherwise. Fills in
		// |suggestions| from the spell check service for the misspelled word if there
		// is one.
		get_dictionary_suggestions: function(self: PCefContextMenuParams; suggestions: TCefStringList): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns true (1) if the context menu was invoked on an editable node.
		is_editable: function(self: PCefContextMenuParams): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns true (1) if the context menu was invoked on an editable node where
		// spell-check is enabled.
		is_spell_check_enabled: function(self: PCefContextMenuParams): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns flags representing the actions supported by the editable node, if
		// any, that the context menu was invoked on.
		get_edit_state_flags: function(self: PCefContextMenuParams): TCefContextMenuEditStateFlags; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
	end;

	//............................................................................cef_cookie_capi
	// Structure used for managing cookies. The functions of this structure may be
	// called on any thread unless otherwise indicated
	TCefCookieManager = record
		// Base structure.
		base: TCefBase;
		// Set the schemes supported by this manager. By default only "http" and
		// "https" schemes are supported. Must be called before any cookies are
		// accessed.
		set_supported_schemes: procedure(self: PCefCookieManager; schemes: TCefStringList); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Visit all cookies. The returned cookies are ordered by longest path, then
		// by earliest creation date. Returns false (0) if cookies cannot be accessed.
		visit_all_cookies: function(self: PCefCookieManager; visitor: PCefCookieVisitor): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Visit a subset of cookies. The results are filtered by the given url
		// scheme, host, domain and path. If |includeHttpOnly| is true (1) HTTP-only
		// cookies will also be included in the results. The returned cookies are
		// ordered by longest path, then by earliest creation date. Returns false (0)
		// if cookies cannot be accessed.
		visit_url_cookies: function(self: PCefCookieManager; const url: PCefString; includeHttpOnly: cint; visitor: PCefCookieVisitor): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Sets a cookie given a valid URL and explicit user-provided cookie
		// attributes. This function expects each attribute to be well-formed. It will
		// check for disallowed characters (e.g. the ';' character is disallowed
		// within the cookie value attribute) and will return false (0) without
		// setting the cookie if such characters are found. This function must be
		// called on the IO thread.
		set_cookie: function(self: PCefCookieManager; const url: PCefString; const cookie: PCefCookie): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Delete all cookies that match the specified parameters. If both |url| and
		// values |cookie_name| are specified all host and domain cookies matching
		// both will be deleted. If only |url| is specified all host cookies (but not
		// domain cookies) irrespective of path will be deleted. If |url| is NULL all
		// cookies for all hosts and domains will be deleted. Returns false (0) if a
		// non- NULL invalid URL is specified or if cookies cannot be accessed. This
		// function must be called on the IO thread.
		delete_cookies: function(self: PCefCookieManager; const url: PCefString; const cookie_name: PCefString): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Sets the directory path that will be used for storing cookie data. If
		// |path| is NULL data will be stored in memory only. Otherwise, data will be
		// stored at the specified |path|. To persist session cookies (cookies without
		// an expiry date or validity interval) set |persist_session_cookies| to true
		// (1). Session cookies are generally intended to be transient and most Web
		// browsers do not persist them. Returns false (0) if cookies cannot be
		// accessed.
		set_storage_path: function(self: PCefCookieManager; const path: PCefString; persist_session_cookies: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Flush the backing store (if any) to disk and execute the specified
		// |callback| on the IO thread when done. Returns false (0) if cookies cannot
		// be accessed.
		flush_store: function(self: PCefCookieManager; callback: PCefCompletionCallback): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
	end;

	// Structure to implement for visiting cookie values. The functions of this
	// structure will always be called on the IO thread
	TCefCookieVisitor = record
		// Base structure.
		base: TCefBase;
		// Method that will be called once for each cookie. |count| is the 0-based
		// index for the current cookie. |total| is the total number of cookies. Set
		// |deleteCookie| to true (1) to delete the cookie currently being visited.
		// Return false (0) to stop visiting cookies. This function may never be
		// called if no cookies are found.
		visit: function(self: PCefCookieVisitor; const cookie: PCefCookie; count: cint; total: cint; deleteCookie: pcint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
	end;

	//............................................................................cef_dialog_handler_capi
	// Callback structure for asynchronous continuation of file dialog requests
	TCefFileDialogCallback = record
		// Base structure.
		base: TCefBase;
		// Continue the file selection with the specified |file_paths|. This may be a
		// single value or a list of values depending on the dialog mode. An NULL
		// value is treated the same as calling cancel().
		cont: procedure(self: PCefFileDialogCallback; file_paths: TCefStringList); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Cancel the file selection.
		cancel: procedure(self: PCefFileDialogCallback); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
	end;

	// Implement this structure to handle dialog events. The functions of this
	// structure will be called on the browser process UI thread
	TCefDialogHandler = record
		// Base structure.
		base: TCefBase;
		// Called to run a file chooser dialog. |mode| represents the type of dialog
		// to display. |title| to the title to be used for the dialog and may be NULL
		// to show the default title ("Open" or "Save" depending on the mode).
		// |default_file_name| is the default file name to select in the dialog.
		// |accept_types| is a list of valid lower-cased MIME types or file extensions
		// specified in an input element and is used to restrict selectable files to
		// such types. To display a custom dialog return true (1) and execute
		// |callback| either inline or at a later time. To display the default dialog
		// return false (0).
		on_file_dialog: function(self: PCefDialogHandler; browser: PCefBrowser; mode: TCefFileDialogMode; const title: PCefString; const default_file_name: PCefString; accept_types: TCefStringList; callback: PCefFileDialogCallback): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
	end;

	//............................................................................cef_display_handler_capi
	// Implement this structure to handle events related to browser display state.
	// The functions of this structure will be called on the UI thread
	TCefDisplayHandler = record
		// Base structure.
		base: TCefBase;
		// Called when a frame's address has changed.
		on_address_change: procedure(self: PCefDisplayHandler; browser: PCefBrowser; frame: PCefFrame; const url: PCefString); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Called when the page title changes.
		on_title_change: procedure(self: PCefDisplayHandler; browser: PCefBrowser; const title: PCefString); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Called when the browser is about to display a tooltip. |text| contains the
		// text that will be displayed in the tooltip. To handle the display of the
		// tooltip yourself return true (1). Otherwise, you can optionally modify
		// |text| and then return false (0) to allow the browser to display the
		// tooltip. When window rendering is disabled the application is responsible
		// for drawing tooltips and the return value is ignored.
		on_tooltip: function(self: PCefDisplayHandler; browser: PCefBrowser; text: PCefString): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Called when the browser receives a status message. |value| contains the
		// text that will be displayed in the status message.
		on_status_message: procedure(self: PCefDisplayHandler; browser: PCefBrowser; const value: PCefString); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Called to display a console message. Return true (1) to stop the message
		// from being output to the console.
		on_console_message: function(self: PCefDisplayHandler; browser: PCefBrowser; const message: PCefString; const source: PCefString; line: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
	end;

	//............................................................................cef_dom_capi
	// Structure to implement for visiting the DOM. The functions of this structure
	// will be called on the render process main thread
	TCefDomvisitor = record
		// Base structure.
		base: TCefBase;
		// Method executed for visiting the DOM. The document object passed to this
		// function represents a snapshot of the DOM at the time this function is
		// executed. DOM objects are only valid for the scope of this function. Do not
		// keep references to or attempt to access any DOM objects outside the scope
		// of this function.
		visit: procedure(self: PCefDomvisitor; document: PCefDomdocument); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
	end;

	// Structure used to represent a DOM document. The functions of this structure
	// should only be called on the render process main thread thread
	TCefDomdocument = record
		// Base structure.
		base: TCefBase;
		// Returns the document type.
		get_type: function(self: PCefDomdocument): TCefDomDocumentType; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the root document node.
		get_document: function(self: PCefDomdocument): PCefDomnode; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the BODY node of an HTML document.
		get_body: function(self: PCefDomdocument): PCefDomnode; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the HEAD node of an HTML document.
		get_head: function(self: PCefDomdocument): PCefDomnode; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the title of an HTML document.
		// The resulting string must be freed by calling cef_string_userfree_free().
		get_title: function(self: PCefDomdocument): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the document element with the specified ID value.
		get_element_by_id: function(self: PCefDomdocument; const id: PCefString): PCefDomnode; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the node that currently has keyboard focus.
		get_focused_node: function(self: PCefDomdocument): PCefDomnode; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns true (1) if a portion of the document is selected.
		has_selection: function(self: PCefDomdocument): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the selection start node.
		get_selection_start_node: function(self: PCefDomdocument): PCefDomnode; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the selection offset within the start node.
		get_selection_start_offset: function(self: PCefDomdocument): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the selection end node.
		get_selection_end_node: function(self: PCefDomdocument): PCefDomnode; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the selection offset within the end node.
		get_selection_end_offset: function(self: PCefDomdocument): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the contents of this selection as markup.
		// The resulting string must be freed by calling cef_string_userfree_free().
		get_selection_as_markup: function(self: PCefDomdocument): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the contents of this selection as text.
		// The resulting string must be freed by calling cef_string_userfree_free().
		get_selection_as_text: function(self: PCefDomdocument): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the base URL for the document.
		// The resulting string must be freed by calling cef_string_userfree_free().
		get_base_url: function(self: PCefDomdocument): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns a complete URL based on the document base URL and the specified
		// partial URL.
		// The resulting string must be freed by calling cef_string_userfree_free().
		get_complete_url: function(self: PCefDomdocument; const partialURL: PCefString): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
	end;

	// Structure used to represent a DOM node. The functions of this structure
	// should only be called on the render process main thread
	TCefDomnode = record
		// Base structure.
		base: TCefBase;
		// Returns the type for this node.
		get_type: function(self: PCefDomnode): TCefDomNodeType; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns true (1) if this is a text node.
		is_text: function(self: PCefDomnode): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns true (1) if this is an element node.
		is_element: function(self: PCefDomnode): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns true (1) if this is an editable node.
		is_editable: function(self: PCefDomnode): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns true (1) if this is a form control element node.
		is_form_control_element: function(self: PCefDomnode): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the type of this form control element node.
		// The resulting string must be freed by calling cef_string_userfree_free().
		get_form_control_element_type: function(self: PCefDomnode): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns true (1) if this object is pointing to the same handle as |that|
		// object.
		is_same: function(self: PCefDomnode; that: PCefDomnode): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the name of this node.
		// The resulting string must be freed by calling cef_string_userfree_free().
		get_name: function(self: PCefDomnode): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the value of this node.
		// The resulting string must be freed by calling cef_string_userfree_free().
		get_value: function(self: PCefDomnode): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Set the value of this node. Returns true (1) on success.
		set_value: function(self: PCefDomnode; const value: PCefString): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the contents of this node as markup.
		// The resulting string must be freed by calling cef_string_userfree_free().
		get_as_markup: function(self: PCefDomnode): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the document associated with this node.
		get_document: function(self: PCefDomnode): PCefDomdocument; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the parent node.
		get_parent: function(self: PCefDomnode): PCefDomnode; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the previous sibling node.
		get_previous_sibling: function(self: PCefDomnode): PCefDomnode; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the next sibling node.
		get_next_sibling: function(self: PCefDomnode): PCefDomnode; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns true (1) if this node has child nodes.
		has_children: function(self: PCefDomnode): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Return the first child node.
		get_first_child: function(self: PCefDomnode): PCefDomnode; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the last child node.
		get_last_child: function(self: PCefDomnode): PCefDomnode; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// The following functions are valid only for element nodes.
		// Returns the tag name of this element.
		// The resulting string must be freed by calling cef_string_userfree_free().
		get_element_tag_name: function(self: PCefDomnode): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns true (1) if this element has attributes.
		has_element_attributes: function(self: PCefDomnode): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns true (1) if this element has an attribute named |attrName|.
		has_element_attribute: function(self: PCefDomnode; const attrName: PCefString): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the element attribute named |attrName|.
		// The resulting string must be freed by calling cef_string_userfree_free().
		get_element_attribute: function(self: PCefDomnode; const attrName: PCefString): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns a map of all element attributes.
		get_element_attributes: procedure(self: PCefDomnode; attrMap: TCefStringMap); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Set the value for the element attribute named |attrName|. Returns true (1)
		// on success.
		set_element_attribute: function(self: PCefDomnode; const attrName: PCefString; const value: PCefString): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the inner text of the element.
		// The resulting string must be freed by calling cef_string_userfree_free().
		get_element_inner_text: function(self: PCefDomnode): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
	end;

	//............................................................................cef_download_handler_capi
	// Callback structure used to asynchronously continue a download
	TCefBeforeDownloadCallback = record
		// Base structure.
		base: TCefBase;
		// Call to continue the download. Set |download_path| to the full file path
		// for the download including the file name or leave blank to use the
		// suggested name and the default temp directory. Set |show_dialog| to true
		// (1) if you do wish to show the default "Save As" dialog.
		cont: procedure(self: PCefBeforeDownloadCallback; const download_path: PCefString; show_dialog: cint); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
	end;

	// Callback structure used to asynchronously cancel a download
	TCefDownloadItemCallback = record
		// Base structure.
		base: TCefBase;
		// Call to cancel the download.
		cancel: procedure(self: PCefDownloadItemCallback); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
	end;

	// Structure used to handle file downloads. The functions of this structure will
	// called on the browser process UI thread
	TCefDownloadHandler = record
		// Base structure.
		base: TCefBase;
		// Called before a download begins. |suggested_name| is the suggested name for
		// the download file. By default the download will be canceled. Execute
		// |callback| either asynchronously or in this function to continue the
		// download if desired. Do not keep a reference to |download_item| outside of
		// this function.
		on_before_download: procedure(self: PCefDownloadHandler; browser: PCefBrowser; download_item: PCefDownloadItem; const suggested_name: PCefString; callback: PCefBeforeDownloadCallback); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Called when a download's status or progress information has been updated.
		// This may be called multiple times before and after on_before_download().
		// Execute |callback| either asynchronously or in this function to cancel the
		// download if desired. Do not keep a reference to |download_item| outside of
		// this function.
		on_download_updated: procedure(self: PCefDownloadHandler; browser: PCefBrowser; download_item: PCefDownloadItem; callback: PCefDownloadItemCallback); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
	end;

	//............................................................................cef_download_item_capi
	// Structure used to represent a download item
	TCefDownloadItem = record
		// Base structure.
		base: TCefBase;
		// Returns true (1) if this object is valid. Do not call any other functions
		// if this function returns false (0).
		is_valid: function(self: PCefDownloadItem): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns true (1) if the download is in progress.
		is_in_progress: function(self: PCefDownloadItem): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns true (1) if the download is complete.
		is_complete: function(self: PCefDownloadItem): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns true (1) if the download has been canceled or interrupted.
		is_canceled: function(self: PCefDownloadItem): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns a simple speed estimate in bytes/s.
		get_current_speed: function(self: PCefDownloadItem): cint64; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the rough percent complete or -1 if the receive total size is
		// unknown.
		get_percent_complete: function(self: PCefDownloadItem): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the total number of bytes.
		get_total_bytes: function(self: PCefDownloadItem): cint64; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the number of received bytes.
		get_received_bytes: function(self: PCefDownloadItem): cint64; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the time that the download started.
		get_start_time: function(self: PCefDownloadItem): TCefTime; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the time that the download ended.
		get_end_time: function(self: PCefDownloadItem): TCefTime; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the full path to the downloaded or downloading file.
		// The resulting string must be freed by calling cef_string_userfree_free().
		get_full_path: function(self: PCefDownloadItem): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the unique identifier for this download.
		get_id: function(self: PCefDownloadItem): cuint32; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the URL.
		// The resulting string must be freed by calling cef_string_userfree_free().
		get_url: function(self: PCefDownloadItem): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the suggested file name.
		// The resulting string must be freed by calling cef_string_userfree_free().
		get_suggested_file_name: function(self: PCefDownloadItem): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the content disposition.
		// The resulting string must be freed by calling cef_string_userfree_free().
		get_content_disposition: function(self: PCefDownloadItem): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the mime type.
		// The resulting string must be freed by calling cef_string_userfree_free().
		get_mime_type: function(self: PCefDownloadItem): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
	end;

	//............................................................................cef_drag_data_capi
	// Structure used to represent drag data. The functions of this structure may be
	// called on any thread
	TCefDragData = record
		// Base structure.
		base: TCefBase;
		// Returns a copy of the current object.
		clone: function(self: PCefDragData): PCefDragData; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns true (1) if this object is read-only.
		is_read_only: function(self: PCefDragData): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns true (1) if the drag data is a link.
		is_link: function(self: PCefDragData): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns true (1) if the drag data is a text or html fragment.
		is_fragment: function(self: PCefDragData): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns true (1) if the drag data is a file.
		is_file: function(self: PCefDragData): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Return the link URL that is being dragged.
		// The resulting string must be freed by calling cef_string_userfree_free().
		get_link_url: function(self: PCefDragData): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Return the title associated with the link being dragged.
		// The resulting string must be freed by calling cef_string_userfree_free().
		get_link_title: function(self: PCefDragData): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Return the metadata, if any, associated with the link being dragged.
		// The resulting string must be freed by calling cef_string_userfree_free().
		get_link_metadata: function(self: PCefDragData): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Return the plain text fragment that is being dragged.
		// The resulting string must be freed by calling cef_string_userfree_free().
		get_fragment_text: function(self: PCefDragData): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Return the text/html fragment that is being dragged.
		// The resulting string must be freed by calling cef_string_userfree_free().
		get_fragment_html: function(self: PCefDragData): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Return the base URL that the fragment came from. This value is used for
		// resolving relative URLs and may be NULL.
		// The resulting string must be freed by calling cef_string_userfree_free().
		get_fragment_base_url: function(self: PCefDragData): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Return the name of the file being dragged out of the browser window.
		// The resulting string must be freed by calling cef_string_userfree_free().
		get_file_name: function(self: PCefDragData): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Write the contents of the file being dragged out of the web view into
		// |writer|. Returns the number of bytes sent to |writer|. If |writer| is NULL
		// this function will return the size of the file contents in bytes. Call
		// get_file_name() to get a suggested name for the file.
		get_file_contents: function(self: PCefDragData; writer: PCefStreamWriter): csize_t; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Retrieve the list of file names that are being dragged into the browser
		// window.
		get_file_names: function(self: PCefDragData; names: TCefStringList): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Set the link URL that is being dragged.
		set_link_url: procedure(self: PCefDragData; const url: PCefString); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Set the title associated with the link being dragged.
		set_link_title: procedure(self: PCefDragData; const title: PCefString); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Set the metadata associated with the link being dragged.
		set_link_metadata: procedure(self: PCefDragData; const data: PCefString); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Set the plain text fragment that is being dragged.
		set_fragment_text: procedure(self: PCefDragData; const text: PCefString); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Set the text/html fragment that is being dragged.
		set_fragment_html: procedure(self: PCefDragData; const html: PCefString); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Set the base URL that the fragment came from.
		set_fragment_base_url: procedure(self: PCefDragData; const base_url: PCefString); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Reset the file contents. You should do this before calling
		// cef_browser_host_t::DragTargetDragEnter as the web view does not allow us
		// to drag in this kind of data.
		reset_file_contents: procedure(self: PCefDragData); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Add a file that is being dragged into the webview.
		add_file: procedure(self: PCefDragData; const path: PCefString; const display_name: PCefString); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
	end;

	//............................................................................cef_drag_handler_capi
	// Implement this structure to handle events related to dragging. The functions
	// of this structure will be called on the UI thread
	TCefDragHandler = record
		// Base structure.
		base: TCefBase;
		// Called when an external drag event enters the browser window. |dragData|
		// contains the drag event data and |mask| represents the type of drag
		// operation. Return false (0) for default drag handling behavior or true (1)
		// to cancel the drag event.
		on_drag_enter: function(self: PCefDragHandler; browser: PCefBrowser; dragData: PCefDragData; mask: TCefDragOperationsMask): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
	end;

	//............................................................................cef_focus_handler_capi
	// Implement this structure to handle events related to focus. The functions of
	// this structure will be called on the UI thread
	TCefFocusHandler = record
		// Base structure.
		base: TCefBase;
		// Called when the browser component is about to loose focus. For instance, if
		// focus was on the last HTML element and the user pressed the TAB key. |next|
		// will be true (1) if the browser is giving focus to the next component and
		// false (0) if the browser is giving focus to the previous component.
		on_take_focus: procedure(self: PCefFocusHandler; browser: PCefBrowser; next: cint); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Called when the browser component is requesting focus. |source| indicates
		// where the focus request is originating from. Return false (0) to allow the
		// focus to be set or true (1) to cancel setting the focus.
		on_set_focus: function(self: PCefFocusHandler; browser: PCefBrowser; source: TCefFocusSource): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Called when the browser component has received focus.
		on_got_focus: procedure(self: PCefFocusHandler; browser: PCefBrowser); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
	end;

	//............................................................................cef_frame_capi
	// Structure used to represent a frame in the browser window. When used in the
	// browser process the functions of this structure may be called on any thread
	// unless otherwise indicated in the comments. When used in the render process
	// the functions of this structure may only be called on the main thread
	TCefFrame = record
		// Base structure.
		base: TCefBase;
		// True if this object is currently attached to a valid frame.
		is_valid: function(self: PCefFrame): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Execute undo in this frame.
		undo: procedure(self: PCefFrame); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Execute redo in this frame.
		redo: procedure(self: PCefFrame); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Execute cut in this frame.
		cut: procedure(self: PCefFrame); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Execute copy in this frame.
		copy: procedure(self: PCefFrame); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Execute paste in this frame.
		paste: procedure(self: PCefFrame); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Execute delete in this frame.
		del: procedure(self: PCefFrame); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Execute select all in this frame.
		select_all: procedure(self: PCefFrame); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Save this frame's HTML source to a temporary file and open it in the
		// default text viewing application. This function can only be called from the
		// browser process.
		view_source: procedure(self: PCefFrame); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Retrieve this frame's HTML source as a string sent to the specified
		// visitor.
		get_source: procedure(self: PCefFrame; visitor: PCefStringVisitor); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Retrieve this frame's display text as a string sent to the specified
		// visitor.
		get_text: procedure(self: PCefFrame; visitor: PCefStringVisitor); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Load the request represented by the |request| object.
		load_request: procedure(self: PCefFrame; request: PCefRequest); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Load the specified |url|.
		load_url: procedure(self: PCefFrame; const url: PCefString); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Load the contents of |string_val| with the specified dummy |url|. |url|
		// should have a standard scheme (for example, http scheme) or behaviors like
		// link clicks and web security restrictions may not behave as expected.
		load_string: procedure(self: PCefFrame; const string_val: PCefString; const url: PCefString); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Execute a string of JavaScript code in this frame. The |script_url|
		// parameter is the URL where the script in question can be found, if any. The
		// renderer may request this URL to show the developer the source of the
		// error.  The |start_line| parameter is the base line number to use for error
		// reporting.
		execute_java_script: procedure(self: PCefFrame; const code: PCefString; const script_url: PCefString; start_line: cint); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns true (1) if this is the main (top-level) frame.
		is_main: function(self: PCefFrame): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns true (1) if this is the focused frame.
		is_focused: function(self: PCefFrame): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the name for this frame. If the frame has an assigned name (for
		// example, set via the iframe "name" attribute) then that value will be
		// returned. Otherwise a unique name will be constructed based on the frame
		// parent hierarchy. The main (top-level) frame will always have an NULL name
		// value.
		// The resulting string must be freed by calling cef_string_userfree_free().
		get_name: function(self: PCefFrame): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the globally unique identifier for this frame.
		get_identifier: function(self: PCefFrame): cint64; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the parent of this frame or NULL if this is the main (top-level)
		// frame.
		get_parent: function(self: PCefFrame): PCefFrame; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the URL currently loaded in this frame.
		// The resulting string must be freed by calling cef_string_userfree_free().
		get_url: function(self: PCefFrame): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the browser that this frame belongs to.
		get_browser: function(self: PCefFrame): PCefBrowser; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Get the V8 context associated with the frame. This function can only be
		// called from the render process.
		get_v8context: function(self: PCefFrame): PCefV8context; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Visit the DOM document. This function can only be called from the render
		// process.
		visit_dom: procedure(self: PCefFrame; visitor: PCefDomvisitor); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
	end;

	//............................................................................cef_geolocation_capi
	// Implement this structure to receive geolocation updates. The functions of
	// this structure will be called on the browser process UI thread
	TCefGetGeolocationCallback = record
		// Base structure.
		base: TCefBase;
		// Called with the 'best available' location information or, if the location
		// update failed, with error information.
		on_location_update: procedure(self: PCefGetGeolocationCallback; const position: PCefGeoposition); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
	end;

	//............................................................................cef_geolocation_handler_capi
	// Callback structure used for asynchronous continuation of geolocation
	// permission requests
	TCefGeolocationCallback = record
		// Base structure.
		base: TCefBase;
		// Call to allow or deny geolocation access.
		cont: procedure(self: PCefGeolocationCallback; allow: cint); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
	end;

	// Implement this structure to handle events related to geolocation permission
	// requests. The functions of this structure will be called on the browser
	// process UI thread
	TCefGeolocationHandler = record
		// Base structure.
		base: TCefBase;
		// Called when a page requests permission to access geolocation information.
		// |requesting_url| is the URL requesting permission and |request_id| is the
		// unique ID for the permission request. Return true (1) and call
		// cef_geolocation_callback_t::cont() either in this function or at a later
		// time to continue or cancel the request. Return false (0) to cancel the
		// request immediately.
		on_request_geolocation_permission: function(self: PCefGeolocationHandler; browser: PCefBrowser; const requesting_url: PCefString; request_id: cint; callback: PCefGeolocationCallback): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Called when a geolocation access request is canceled. |requesting_url| is
		// the URL that originally requested permission and |request_id| is the unique
		// ID for the permission request.
		on_cancel_geolocation_permission: procedure(self: PCefGeolocationHandler; browser: PCefBrowser; const requesting_url: PCefString; request_id: cint); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
	end;

	//............................................................................cef_jsdialog_handler_capi
	// Callback structure used for asynchronous continuation of JavaScript dialog
	// requests
	TCefJsdialogCallback = record
		// Base structure.
		base: TCefBase;
		// Continue the JS dialog request. Set |success| to true (1) if the OK button
		// was pressed. The |user_input| value should be specified for prompt dialogs.
		cont: procedure(self: PCefJsdialogCallback; success: cint; const user_input: PCefString); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
	end;

	// Implement this structure to handle events related to JavaScript dialogs. The
	// functions of this structure will be called on the UI thread
	TCefJsdialogHandler = record
		// Base structure.
		base: TCefBase;
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
		on_jsdialog: function(self: PCefJsdialogHandler; browser: PCefBrowser; const origin_url: PCefString; const accept_lang: PCefString; dialog_type: TCefJsdialogType; const message_text: PCefString; const default_prompt_text: PCefString; callback: PCefJsdialogCallback; suppress_message: pcint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Called to run a dialog asking the user if they want to leave a page. Return
		// false (0) to use the default dialog implementation. Return true (1) if the
		// application will use a custom dialog or if the callback has been executed
		// immediately. Custom dialogs may be either modal or modeless. If a custom
		// dialog is used the application must execute |callback| once the custom
		// dialog is dismissed.
		on_before_unload_dialog: function(self: PCefJsdialogHandler; browser: PCefBrowser; const message_text: PCefString; is_reload: cint; callback: PCefJsdialogCallback): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Called to cancel any pending dialogs and reset any saved dialog state. Will
		// be called due to events like page navigation irregardless of whether any
		// dialogs are currently pending.
		on_reset_dialog_state: procedure(self: PCefJsdialogHandler; browser: PCefBrowser); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Called when the default implementation dialog is closed.
		on_dialog_closed: procedure(self: PCefJsdialogHandler; browser: PCefBrowser); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
	end;

	//............................................................................cef_keyboard_handler_capi
	// Implement this structure to handle events related to keyboard input. The
	// functions of this structure will be called on the UI thread
	TCefKeyboardHandler = record
		// Base structure.
		base: TCefBase;
		// Called before a keyboard event is sent to the renderer. |event| contains
		// information about the keyboard event. |os_event| is the operating system
		// event message, if any. Return true (1) if the event was handled or false
		// (0) otherwise. If the event will be handled in on_key_event() as a keyboard
		// shortcut set |is_keyboard_shortcut| to true (1) and return false (0).
		on_pre_key_event: function(self: PCefKeyboardHandler; browser: PCefBrowser; const event: PCefKeyEvent; os_event: TCefEventHandle; is_keyboard_shortcut: pcint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Called after the renderer and JavaScript in the page has had a chance to
		// handle the event. |event| contains information about the keyboard event.
		// |os_event| is the operating system event message, if any. Return true (1)
		// if the keyboard event was handled or false (0) otherwise.
		on_key_event: function(self: PCefKeyboardHandler; browser: PCefBrowser; const event: PCefKeyEvent; os_event: TCefEventHandle): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
	end;

	//............................................................................cef_life_span_handler_capi
	// Implement this structure to handle events related to browser life span. The
	// functions of this structure will be called on the UI thread unless otherwise
	// indicated
	TCefLifeSpanHandler = record
		// Base structure.
		base: TCefBase;
		// Called on the IO thread before a new popup window is created. The |browser|
		// and |frame| parameters represent the source of the popup request. The
		// |target_url| and |target_frame_name| values may be NULL if none were
		// specified with the request. The |popupFeatures| structure contains
		// information about the requested popup window. To allow creation of the
		// popup window optionally modify |windowInfo|, |client|, |settings| and
		// |no_javascript_access| and return false (0). To cancel creation of the
		// popup window return true (1). The |client| and |settings| values will
		// default to the source browser's values. The |no_javascript_access| value
		// indicates whether the new browser window should be scriptable and in the
		// same process as the source browser.
		on_before_popup: function(self: PCefLifeSpanHandler; browser: PCefBrowser; frame: PCefFrame; const target_url: PCefString; const target_frame_name: PCefString; const popupFeatures: PCefPopupFeatures; windowInfo: PCefWindowInfo; client: PCefClient; settings: PCefBrowserSettings; no_javascript_access: pcint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Called after a new browser is created.
		on_after_created: procedure(self: PCefLifeSpanHandler; browser: PCefBrowser); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Called when a modal window is about to display and the modal loop should
		// begin running. Return false (0) to use the default modal loop
		// implementation or true (1) to use a custom implementation.
		run_modal: function(self: PCefLifeSpanHandler; browser: PCefBrowser): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Called when a browser has recieved a request to close. This may result
		// directly from a call to cef_browser_host_t::close_browser() or indirectly
		// if the browser is a top-level OS window created by CEF and the user
		// attempts to close the window. This function will be called after the
		// JavaScript 'onunload' event has been fired. It will not be called for
		// browsers after the associated OS window has been destroyed (for those
		// browsers it is no longer possible to cancel the close).
		// If CEF created an OS window for the browser returning false (0) will send
		// an OS close notification to the browser window's top-level owner (e.g.
		// WM_CLOSE on Windows, performClose: on OS-X and "delete_event" on Linux). If
		// no OS window exists (window rendering disabled) returning false (0) will
		// cause the browser object to be destroyed immediately. Return true (1) if
		// the browser is parented to another window and that other window needs to
		// receive close notification via some non-standard technique.
		// If an application provides its own top-level window it should handle OS
		// close notifications by calling cef_browser_host_t::CloseBrowser(false (0))
		// instead of immediately closing (see the example below). This gives CEF an
		// opportunity to process the 'onbeforeunload' event and optionally cancel the
		// close before do_close() is called.
		// The cef_life_span_handler_t::on_before_close() function will be called
		// immediately before the browser object is destroyed. The application should
		// only exit after on_before_close() has been called for all existing
		// browsers.
		// If the browser represents a modal window and a custom modal loop
		// implementation was provided in cef_life_span_handler_t::run_modal() this
		// callback should be used to restore the opener window to a usable state.
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
		do_close: function(self: PCefLifeSpanHandler; browser: PCefBrowser): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Called just before a browser is destroyed. Release all references to the
		// browser object and do not attempt to execute any functions on the browser
		// object after this callback returns. If this is a modal window and a custom
		// modal loop implementation was provided in run_modal() this callback should
		// be used to exit the custom modal loop. See do_close() documentation for
		// additional usage information.
		on_before_close: procedure(self: PCefLifeSpanHandler; browser: PCefBrowser); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
	end;

	//............................................................................cef_load_handler_capi
	// Implement this structure to handle events related to browser load status. The
	// functions of this structure will be called on the browser process UI thread
	// or render process main thread (TID_RENDERER
	TCefLoadHandler = record
		// Base structure.
		base: TCefBase;
		// Called when the loading state has changed. This callback will be executed
		// twice -- once when loading is initiated either programmatically or by user
		// action, and once when loading is terminated due to completion, cancellation
		// of failure.
		on_loading_state_change: procedure(self: PCefLoadHandler; browser: PCefBrowser; isLoading: cint; canGoBack: cint; canGoForward: cint); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Called when the browser begins loading a frame. The |frame| value will
		// never be NULL -- call the is_main() function to check if this frame is the
		// main frame. Multiple frames may be loading at the same time. Sub-frames may
		// start or continue loading after the main frame load has ended. This
		// function may not be called for a particular frame if the load request for
		// that frame fails. For notification of overall browser load status use
		// OnLoadingStateChange instead.
		on_load_start: procedure(self: PCefLoadHandler; browser: PCefBrowser; frame: PCefFrame); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Called when the browser is done loading a frame. The |frame| value will
		// never be NULL -- call the is_main() function to check if this frame is the
		// main frame. Multiple frames may be loading at the same time. Sub-frames may
		// start or continue loading after the main frame load has ended. This
		// function will always be called for all frames irrespective of whether the
		// request completes successfully.
		on_load_end: procedure(self: PCefLoadHandler; browser: PCefBrowser; frame: PCefFrame; httpStatusCode: cint); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Called when the resource load for a navigation fails or is canceled.
		// |errorCode| is the error code number, |errorText| is the error text and
		// |failedUrl| is the URL that failed to load. See net\base\net_error_list.h
		// for complete descriptions of the error codes.
		on_load_error: procedure(self: PCefLoadHandler; browser: PCefBrowser; frame: PCefFrame; errorCode: TCefErrorcode; const errorText: PCefString; const failedUrl: PCefString); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
	end;

	//............................................................................cef_menu_model_capi
	// Supports creation and modification of menus. See cef_menu_id_t for the
	// command ids that have default implementations. All user-defined command ids
	// should be between MENU_ID_USER_FIRST and MENU_ID_USER_LAST. The functions of
	// this structure can only be accessed on the browser process the UI thread
	TCefMenuModel = record
		// Base structure.
		base: TCefBase;
		// Clears the menu. Returns true (1) on success.
		clear: function(self: PCefMenuModel): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the number of items in this menu.
		get_count: function(self: PCefMenuModel): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Add a separator to the menu. Returns true (1) on success.
		add_separator: function(self: PCefMenuModel): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Add an item to the menu. Returns true (1) on success.
		add_item: function(self: PCefMenuModel; command_id: cint; const _label: PCefString): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Add a check item to the menu. Returns true (1) on success.
		add_check_item: function(self: PCefMenuModel; command_id: cint; const _label: PCefString): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Add a radio item to the menu. Only a single item with the specified
		// |group_id| can be checked at a time. Returns true (1) on success.
		add_radio_item: function(self: PCefMenuModel; command_id: cint; const _label: PCefString; group_id: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Add a sub-menu to the menu. The new sub-menu is returned.
		add_sub_menu: function(self: PCefMenuModel; command_id: cint; const _label: PCefString): PCefMenuModel; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Insert a separator in the menu at the specified |index|. Returns true (1)
		// on success.
		insert_separator_at: function(self: PCefMenuModel; index: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Insert an item in the menu at the specified |index|. Returns true (1) on
		// success.
		insert_item_at: function(self: PCefMenuModel; index: cint; command_id: cint; const _label: PCefString): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Insert a check item in the menu at the specified |index|. Returns true (1)
		// on success.
		insert_check_item_at: function(self: PCefMenuModel; index: cint; command_id: cint; const _label: PCefString): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Insert a radio item in the menu at the specified |index|. Only a single
		// item with the specified |group_id| can be checked at a time. Returns true
		// (1) on success.
		insert_radio_item_at: function(self: PCefMenuModel; index: cint; command_id: cint; const _label: PCefString; group_id: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Insert a sub-menu in the menu at the specified |index|. The new sub-menu is
		// returned.
		insert_sub_menu_at: function(self: PCefMenuModel; index: cint; command_id: cint; const _label: PCefString): PCefMenuModel; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Removes the item with the specified |command_id|. Returns true (1) on
		// success.
		remove: function(self: PCefMenuModel; command_id: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Removes the item at the specified |index|. Returns true (1) on success.
		remove_at: function(self: PCefMenuModel; index: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the index associated with the specified |command_id| or -1 if not
		// found due to the command id not existing in the menu.
		get_index_of: function(self: PCefMenuModel; command_id: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the command id at the specified |index| or -1 if not found due to
		// invalid range or the index being a separator.
		get_command_id_at: function(self: PCefMenuModel; index: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Sets the command id at the specified |index|. Returns true (1) on success.
		set_command_id_at: function(self: PCefMenuModel; index: cint; command_id: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the label for the specified |command_id| or NULL if not found.
		// The resulting string must be freed by calling cef_string_userfree_free().
		get_label: function(self: PCefMenuModel; command_id: cint): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the label at the specified |index| or NULL if not found due to
		// invalid range or the index being a separator.
		// The resulting string must be freed by calling cef_string_userfree_free().
		get_label_at: function(self: PCefMenuModel; index: cint): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Sets the label for the specified |command_id|. Returns true (1) on success.
		set_label: function(self: PCefMenuModel; command_id: cint; const _label: PCefString): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Set the label at the specified |index|. Returns true (1) on success.
		set_label_at: function(self: PCefMenuModel; index: cint; const _label: PCefString): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the item type for the specified |command_id|.
		get_type: function(self: PCefMenuModel; command_id: cint): TCefMenuItemType; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the item type at the specified |index|.
		get_type_at: function(self: PCefMenuModel; index: cint): TCefMenuItemType; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the group id for the specified |command_id| or -1 if invalid.
		get_group_id: function(self: PCefMenuModel; command_id: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the group id at the specified |index| or -1 if invalid.
		get_group_id_at: function(self: PCefMenuModel; index: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Sets the group id for the specified |command_id|. Returns true (1) on
		// success.
		set_group_id: function(self: PCefMenuModel; command_id: cint; group_id: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Sets the group id at the specified |index|. Returns true (1) on success.
		set_group_id_at: function(self: PCefMenuModel; index: cint; group_id: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the submenu for the specified |command_id| or NULL if invalid.
		get_sub_menu: function(self: PCefMenuModel; command_id: cint): PCefMenuModel; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the submenu at the specified |index| or NULL if invalid.
		get_sub_menu_at: function(self: PCefMenuModel; index: cint): PCefMenuModel; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns true (1) if the specified |command_id| is visible.
		is_visible: function(self: PCefMenuModel; command_id: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns true (1) if the specified |index| is visible.
		is_visible_at: function(self: PCefMenuModel; index: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Change the visibility of the specified |command_id|. Returns true (1) on
		// success.
		set_visible: function(self: PCefMenuModel; command_id: cint; visible: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Change the visibility at the specified |index|. Returns true (1) on
		// success.
		set_visible_at: function(self: PCefMenuModel; index: cint; visible: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns true (1) if the specified |command_id| is enabled.
		is_enabled: function(self: PCefMenuModel; command_id: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns true (1) if the specified |index| is enabled.
		is_enabled_at: function(self: PCefMenuModel; index: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Change the enabled status of the specified |command_id|. Returns true (1)
		// on success.
		set_enabled: function(self: PCefMenuModel; command_id: cint; enabled: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Change the enabled status at the specified |index|. Returns true (1) on
		// success.
		set_enabled_at: function(self: PCefMenuModel; index: cint; enabled: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns true (1) if the specified |command_id| is checked. Only applies to
		// check and radio items.
		is_checked: function(self: PCefMenuModel; command_id: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns true (1) if the specified |index| is checked. Only applies to check
		// and radio items.
		is_checked_at: function(self: PCefMenuModel; index: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Check the specified |command_id|. Only applies to check and radio items.
		// Returns true (1) on success.
		set_checked: function(self: PCefMenuModel; command_id: cint; checked: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Check the specified |index|. Only applies to check and radio items. Returns
		// true (1) on success.
		set_checked_at: function(self: PCefMenuModel; index: cint; checked: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns true (1) if the specified |command_id| has a keyboard accelerator
		// assigned.
		has_accelerator: function(self: PCefMenuModel; command_id: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns true (1) if the specified |index| has a keyboard accelerator
		// assigned.
		has_accelerator_at: function(self: PCefMenuModel; index: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Set the keyboard accelerator for the specified |command_id|. |key_code| can
		// be any virtual key or character value. Returns true (1) on success.
		set_accelerator: function(self: PCefMenuModel; command_id: cint; key_code: cint; shift_pressed: cint; ctrl_pressed: cint; alt_pressed: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Set the keyboard accelerator at the specified |index|. |key_code| can be
		// any virtual key or character value. Returns true (1) on success.
		set_accelerator_at: function(self: PCefMenuModel; index: cint; key_code: cint; shift_pressed: cint; ctrl_pressed: cint; alt_pressed: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Remove the keyboard accelerator for the specified |command_id|. Returns
		// true (1) on success.
		remove_accelerator: function(self: PCefMenuModel; command_id: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Remove the keyboard accelerator at the specified |index|. Returns true (1)
		// on success.
		remove_accelerator_at: function(self: PCefMenuModel; index: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Retrieves the keyboard accelerator for the specified |command_id|. Returns
		// true (1) on success.
		get_accelerator: function(self: PCefMenuModel; command_id: cint; key_code: pcint; shift_pressed: pcint; ctrl_pressed: pcint; alt_pressed: pcint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Retrieves the keyboard accelerator for the specified |index|. Returns true
		// (1) on success.
		get_accelerator_at: function(self: PCefMenuModel; index: cint; key_code: pcint; shift_pressed: pcint; ctrl_pressed: pcint; alt_pressed: pcint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
	end;

	//............................................................................cef_print_handler_capi
	// Callback structure for asynchronous continuation of print dialog requests
	TCefPrintDialogCallback = record
		// Base structure.
		base: TCefBase;
		// Continue printing with the specified |settings|.
		cont: procedure(self: PCefPrintDialogCallback; settings: PCefPrintSettings); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Cancel the printing.
		cancel: procedure(self: PCefPrintDialogCallback); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
	end;

	// Callback structure for asynchronous continuation of print job requests
	TCefPrintJobCallback = record
		// Base structure.
		base: TCefBase;
		// Indicate completion of the print job.
		cont: procedure(self: PCefPrintJobCallback); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
	end;

	// Implement this structure to handle printing on Linux. The functions of this
	// structure will be called on the browser process UI thread
	TCefPrintHandler = record
		// Base structure.
		base: TCefBase;
		// Synchronize |settings| with client state. If |get_defaults| is true (1)
		// then populate |settings| with the default print settings. Do not keep a
		// reference to |settings| outside of this callback.
		on_print_settings: procedure(self: PCefPrintHandler; settings: PCefPrintSettings; get_defaults: cint); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Show the print dialog. Execute |callback| once the dialog is dismissed.
		// Return true (1) if the dialog will be displayed or false (0) to cancel the
		// printing immediately.
		on_print_dialog: function(self: PCefPrintHandler; has_selection: cint; callback: PCefPrintDialogCallback): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Send the print job to the printer. Execute |callback| once the job is
		// completed. Return true (1) if the job will proceed or false (0) to cancel
		// the job immediately.
		on_print_job: function(self: PCefPrintHandler; const document_name: PCefString; const pdf_file_path: PCefString; callback: PCefPrintJobCallback): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Reset client state related to printing.
		on_print_reset: procedure(self: PCefPrintHandler); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
	end;

  //............................................................................cef_print_settings_capi
  // Structure representing print settings
	TCefPrintSettings = record
		// Base structure.
		base: TCefBase;
		// Returns true (1) if this object is valid. Do not call any other functions
		// if this function returns false (0).
		is_valid: function(self: PCefPrintSettings): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns true (1) if the values of this object are read-only. Some APIs may
		// expose read-only objects.
		is_read_only: function(self: PCefPrintSettings): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns a writable copy of this object.
		copy: function(self: PCefPrintSettings): PCefPrintSettings; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Set the page orientation.
		set_orientation: procedure(self: PCefPrintSettings; landscape: cint); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns true (1) if the orientation is landscape.
		is_landscape: function(self: PCefPrintSettings): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Set the printer printable area in device units. Some platforms already
		// provide flipped area. Set |landscape_needs_flip| to false (0) on those
		// platforms to avoid double flipping.
		set_printer_printable_area: procedure(self: PCefPrintSettings; const physical_size_device_units: PCefSize; const printable_area_device_units: PCefRect; landscape_needs_flip: cint); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Set the device name.
		set_device_name: procedure(self: PCefPrintSettings; const name: PCefString); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Get the device name.
		// The resulting string must be freed by calling cef_string_userfree_free().
		get_device_name: function(self: PCefPrintSettings): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Set the DPI (dots per inch).
		set_dpi: procedure(self: PCefPrintSettings; dpi: cint); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Get the DPI (dots per inch).
		get_dpi: function(self: PCefPrintSettings): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Set the page ranges.
		set_page_ranges: procedure(self: PCefPrintSettings; rangesCount: csize_t; ranges: PCefPageRangeArray); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the number of page ranges that currently exist.
		get_page_ranges_count: function(self: PCefPrintSettings): csize_t; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Retrieve the page ranges.
		get_page_ranges: procedure(self: PCefPrintSettings; rangesCount: pcsize_t; ranges: PCefPageRangeArray); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Set whether only the selection will be printed.
		set_selection_only: procedure(self: PCefPrintSettings; selection_only: cint); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns true (1) if only the selection will be printed.
		is_selection_only: function(self: PCefPrintSettings): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Set whether pages will be collated.
		set_collate: procedure(self: PCefPrintSettings; collate: cint); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns true (1) if pages will be collated.
		will_collate: function(self: PCefPrintSettings): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Set the color model.
		set_color_model: procedure(self: PCefPrintSettings; model: TCefColorModel); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Get the color model.
		get_color_model: function(self: PCefPrintSettings): TCefColorModel; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Set the number of copies.
		set_copies: procedure(self: PCefPrintSettings; copies: cint); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Get the number of copies.
		get_copies: function(self: PCefPrintSettings): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Set the duplex mode.
		set_duplex_mode: procedure(self: PCefPrintSettings; mode: TCefDuplexMode); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Get the duplex mode.
		get_duplex_mode: function(self: PCefPrintSettings): TCefDuplexMode; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
	end;

	//............................................................................cef_process_message_capi
	// Structure representing a message. Can be used on any process and thread
	TCefProcessMessage = record
		// Base structure.
		base: TCefBase;
		// Returns true (1) if this object is valid. Do not call any other functions
		// if this function returns false (0).
		is_valid: function(self: PCefProcessMessage): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns true (1) if the values of this object are read-only. Some APIs may
		// expose read-only objects.
		is_read_only: function(self: PCefProcessMessage): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns a writable copy of this object.
		copy: function(self: PCefProcessMessage): PCefProcessMessage; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the message name.
		// The resulting string must be freed by calling cef_string_userfree_free().
		get_name: function(self: PCefProcessMessage): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the list of arguments.
		get_argument_list: function(self: PCefProcessMessage): PCefListValue; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
	end;

	//............................................................................cef_render_handler_capi
  // Implement this structure to handle events when window rendering is disabled.
	// The functions of this structure will be called on the UI thread
	TCefRenderHandler = record
		// Base structure.
		base: TCefBase;
		// Called to retrieve the root window rectangle in screen coordinates. Return
		// true (1) if the rectangle was provided.
		get_root_screen_rect: function(self: PCefRenderHandler; browser: PCefBrowser; rect: PCefRect): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Called to retrieve the view rectangle which is relative to screen
		// coordinates. Return true (1) if the rectangle was provided.
		get_view_rect: function(self: PCefRenderHandler; browser: PCefBrowser; rect: PCefRect): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Called to retrieve the translation from view coordinates to actual screen
		// coordinates. Return true (1) if the screen coordinates were provided.
		get_screen_point: function(self: PCefRenderHandler; browser: PCefBrowser; viewX: cint; viewY: cint; screenX: pcint; screenY: pcint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Called to allow the client to fill in the CefScreenInfo object with
		// appropriate values. Return true (1) if the |screen_info| structure has been
		// modified.
		// If the screen info rectangle is left NULL the rectangle from GetViewRect
		// will be used. If the rectangle is still NULL or invalid popups may not be
		// drawn correctly.
		get_screen_info: function(self: PCefRenderHandler; browser: PCefBrowser; screen_info: PCefScreenInfo): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Called when the browser wants to show or hide the popup widget. The popup
		// should be shown if |show| is true (1) and hidden if |show| is false (0).
		on_popup_show: procedure(self: PCefRenderHandler; browser: PCefBrowser; show: cint); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Called when the browser wants to move or resize the popup widget. |rect|
		// contains the new location and size.
		on_popup_size: procedure(self: PCefRenderHandler; browser: PCefBrowser; const rect: PCefRect); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Called when an element should be painted. |type| indicates whether the
		// element is the view or the popup widget. |buffer| contains the pixel data
		// for the whole image. |dirtyRects| contains the set of rectangles that need
		// to be repainted. On Windows |buffer| will be |width|*|height|*4 bytes in
		// size and represents a BGRA image with an upper-left origin.
		on_paint: procedure(self: PCefRenderHandler; browser: PCefBrowser; _type: TCefPaintElementType; dirtyRectsCount: csize_t; dirtyRects: PCefRectArray; const buffer: void; width: cint; height: cint); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Called when the browser window's cursor has changed.
		on_cursor_change: procedure(self: PCefRenderHandler; browser: PCefBrowser; cursor: TCefCursorHandle); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Called when the user starts dragging content in the web view. Contextual
		// information about the dragged content is supplied by |drag_data|. OS APIs
		// that run a system message loop may be used within the StartDragging call.
		// Return false (0) to abort the drag operation. Don't call any of
		// cef_browser_host_t::DragSource*Ended* functions after returning false (0).
		// Return true (1) to handle the drag operation. Call
		// cef_browser_host_t::DragSourceEndedAt and DragSourceSystemDragEnded either
		// synchronously or asynchronously to inform the web view that the drag
		// operation has ended.
		start_dragging: function(self: PCefRenderHandler; browser: PCefBrowser; drag_data: PCefDragData; allowed_ops: TCefDragOperationsMask; x: cint; y: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Called when the web view wants to update the mouse cursor during a drag &
		// drop operation. |operation| describes the allowed operation (none, move,
		// copy, link).
		update_drag_cursor: procedure(self: PCefRenderHandler; browser: PCefBrowser; operation: TCefDragOperationsMask); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Called when the scroll offset has changed.
		on_scroll_offset_changed: procedure(self: PCefRenderHandler; browser: PCefBrowser); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
	end;

	//............................................................................cef_render_process_handler_capi
	// Structure used to implement render process callbacks. The functions of this
	// structure will be called on the render process main thread (TID_RENDERER)
	// unless otherwise indicated
	TCefRenderProcessHandler = record
		// Base structure.
		base: TCefBase;
		// Called after the render process main thread has been created. |extra_info|
		// is a read-only value originating from
		// cef_browser_process_handler_t::on_render_process_thread_created(). Do not
		// keep a reference to |extra_info| outside of this function.
		on_render_thread_created: procedure(self: PCefRenderProcessHandler; extra_info: PCefListValue); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Called after WebKit has been initialized.
		on_web_kit_initialized: procedure(self: PCefRenderProcessHandler); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Called after a browser has been created. When browsing cross-origin a new
		// browser will be created before the old browser with the same identifier is
		// destroyed.
		on_browser_created: procedure(self: PCefRenderProcessHandler; browser: PCefBrowser); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Called before a browser is destroyed.
		on_browser_destroyed: procedure(self: PCefRenderProcessHandler; browser: PCefBrowser); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Return the handler for browser load status events.
		get_load_handler: function(self: PCefRenderProcessHandler): PCefLoadHandler; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Called before browser navigation. Return true (1) to cancel the navigation
		// or false (0) to allow the navigation to proceed. The |request| object
		// cannot be modified in this callback.
		on_before_navigation: function(self: PCefRenderProcessHandler; browser: PCefBrowser; frame: PCefFrame; request: PCefRequest; navigation_type: TCefNavigationType; is_redirect: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Called immediately after the V8 context for a frame has been created. To
		// retrieve the JavaScript 'window' object use the
		// cef_v8context_t::get_global() function. V8 handles can only be accessed
		// from the thread on which they are created. A task runner for posting tasks
		// on the associated thread can be retrieved via the
		// cef_v8context_t::get_task_runner() function.
		on_context_created: procedure(self: PCefRenderProcessHandler; browser: PCefBrowser; frame: PCefFrame; context: PCefV8context); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Called immediately before the V8 context for a frame is released. No
		// references to the context should be kept after this function is called.
		on_context_released: procedure(self: PCefRenderProcessHandler; browser: PCefBrowser; frame: PCefFrame; context: PCefV8context); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Called for global uncaught exceptions in a frame. Execution of this
		// callback is disabled by default. To enable set
		// CefSettings.uncaught_exception_stack_size > 0.
		on_uncaught_exception: procedure(self: PCefRenderProcessHandler; browser: PCefBrowser; frame: PCefFrame; context: PCefV8context; exception: PCefV8exception; stackTrace: PCefV8stackTrace); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Called when a new node in the the browser gets focus. The |node| value may
		// be NULL if no specific node has gained focus. The node object passed to
		// this function represents a snapshot of the DOM at the time this function is
		// executed. DOM objects are only valid for the scope of this function. Do not
		// keep references to or attempt to access any DOM objects outside the scope
		// of this function.
		on_focused_node_changed: procedure(self: PCefRenderProcessHandler; browser: PCefBrowser; frame: PCefFrame; node: PCefDomnode); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Called when a new message is received from a different process. Return true
		// (1) if the message was handled or false (0) otherwise. Do not keep a
		// reference to or attempt to access the message outside of this callback.
		on_process_message_received: function(self: PCefRenderProcessHandler; browser: PCefBrowser; source_process: TCefProcessId; message: PCefProcessMessage): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
	end;

	//............................................................................cef_request_capi
	// Structure used to represent a web request. The functions of this structure
	// may be called on any thread
	TCefRequest = record
		// Base structure.
		base: TCefBase;
		// Returns true (1) if this object is read-only.
		is_read_only: function(self: PCefRequest): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Get the fully qualified URL.
		// The resulting string must be freed by calling cef_string_userfree_free().
		get_url: function(self: PCefRequest): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Set the fully qualified URL.
		set_url: procedure(self: PCefRequest; const url: PCefString); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Get the request function type. The value will default to POST if post data
		// is provided and GET otherwise.
		// The resulting string must be freed by calling cef_string_userfree_free().
		get_method: function(self: PCefRequest): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Set the request function type.
		set_method: procedure(self: PCefRequest; const method: PCefString); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Get the post data.
		get_post_data: function(self: PCefRequest): PCefPostData; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Set the post data.
		set_post_data: procedure(self: PCefRequest; postData: PCefPostData); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Get the header values.
		get_header_map: procedure(self: PCefRequest; headerMap: TCefStringMultimap); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Set the header values.
		set_header_map: procedure(self: PCefRequest; headerMap: TCefStringMultimap); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Set all values at one time.
		_set: procedure(self: PCefRequest; const url: PCefString; const method: PCefString; postData: PCefPostData; headerMap: TCefStringMultimap); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Get the flags used in combination with cef_urlrequest_t. See
		// cef_urlrequest_flags_t for supported values.
		get_flags: function(self: PCefRequest): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Set the flags used in combination with cef_urlrequest_t.  See
		// cef_urlrequest_flags_t for supported values.
		set_flags: procedure(self: PCefRequest; flags: cint); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Set the URL to the first party for cookies used in combination with
		// cef_urlrequest_t.
		// The resulting string must be freed by calling cef_string_userfree_free().
		get_first_party_for_cookies: function(self: PCefRequest): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Get the URL to the first party for cookies used in combination with
		// cef_urlrequest_t.
		set_first_party_for_cookies: procedure(self: PCefRequest; const url: PCefString); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Get the resource type for this request. Only available in the browser
		// process.
		get_resource_type: function(self: PCefRequest): TCefResourceType; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Get the transition type for this request. Only available in the browser
		// process and only applies to requests that represent a main frame or sub-
		// frame navigation.
		get_transition_type: function(self: PCefRequest): TCefTransitionType; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
	end;

  // Structure used to represent post data for a web request. The functions of
	// this structure may be called on any thread
	TCefPostData = record
		// Base structure.
		base: TCefBase;
		// Returns true (1) if this object is read-only.
		is_read_only: function(self: PCefPostData): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the number of existing post data elements.
		get_element_count: function(self: PCefPostData): csize_t; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Retrieve the post data elements.
		get_elements: procedure(self: PCefPostData; elementsCount: pcsize_t; elements: PCefPostDataElementArray); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Remove the specified post data element.  Returns true (1) if the removal
		// succeeds.
		remove_element: function(self: PCefPostData; element: PCefPostDataElement): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Add the specified post data element.  Returns true (1) if the add succeeds.
		add_element: function(self: PCefPostData; element: PCefPostDataElement): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Remove all existing post data elements.
		remove_elements: procedure(self: PCefPostData); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
	end;

  // Structure used to represent a single element in the request post data. The
	// functions of this structure may be called on any thread
	TCefPostDataElement = record
		// Base structure.
		base: TCefBase;
		// Returns true (1) if this object is read-only.
		is_read_only: function(self: PCefPostDataElement): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Remove all contents from the post data element.
		set_to_empty: procedure(self: PCefPostDataElement); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// The post data element will represent a file.
		set_to_file: procedure(self: PCefPostDataElement; const fileName: PCefString); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// The post data element will represent bytes.  The bytes passed in will be
		// copied.
		set_to_bytes: procedure(self: PCefPostDataElement; size: csize_t; const bytes: void); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Return the type of this post data element.
		get_type: function(self: PCefPostDataElement): TCefPostdataelementType; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Return the file name.
		// The resulting string must be freed by calling cef_string_userfree_free().
		get_file: function(self: PCefPostDataElement): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Return the number of bytes.
		get_bytes_count: function(self: PCefPostDataElement): csize_t; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Read up to |size| bytes into |bytes| and return the number of bytes
		// actually read.
		get_bytes: function(self: PCefPostDataElement; size: csize_t; bytes: void): csize_t; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
	end;

	//............................................................................cef_request_context_capi
	// A request context provides request handling for a set of related browser
	// objects. A request context is specified when creating a new browser object
	// via the cef_browser_host_t static factory functions. Browser objects with
	// different request contexts will never be hosted in the same render process.
	// Browser objects with the same request context may or may not be hosted in the
	// same render process depending on the process model. Browser objects created
	// indirectly via the JavaScript window.open function or targeted links will
	// share the same render process and the same request context as the source
	// browser. When running in single-process mode there is only a single render
	// process (the main process) and so all browsers created in single-process mode
	// will share the same request context. This will be the first request context
	// passed into a cef_browser_host_t static factory function and all other
	// request context objects will be ignored
	TCefRequestContext = record
		// Base structure.
		base: TCefBase;
		// Returns true (1) if this object is pointing to the same context as |that|
		// object.
		is_same: function(self: PCefRequestContext; other: PCefRequestContext): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns true (1) if this object is the global context.
		is_global: function(self: PCefRequestContext): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the handler for this context if any.
		get_handler: function(self: PCefRequestContext): PCefRequestContextHandler; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
	end;

	//............................................................................cef_request_context_handler_capi
	// Implement this structure to provide handler implementations
	TCefRequestContextHandler = record
		// Base structure.
		base: TCefBase;
		// Called on the IO thread to retrieve the cookie manager. The global cookie
		// manager will be used if this function returns NULL.
		get_cookie_manager: function(self: PCefRequestContextHandler): PCefCookieManager; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
	end;

	//............................................................................cef_request_handler_capi
	// Callback structure used for asynchronous continuation of quota requests
	TCefQuotaCallback = record
		// Base structure.
		base: TCefBase;
		// Continue the quota request. If |allow| is true (1) the request will be
		// allowed. Otherwise, the request will be denied.
		cont: procedure(self: PCefQuotaCallback; allow: cint); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Cancel the quota request.
		cancel: procedure(self: PCefQuotaCallback); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
	end;

	// Callback structure used for asynchronous continuation of url requests when
	// invalid SSL certificates are encountered
	TCefAllowCertificateErrorCallback = record
		// Base structure.
		base: TCefBase;
		// Continue the url request. If |allow| is true (1) the request will be
		// continued. Otherwise, the request will be canceled.
		cont: procedure(self: PCefAllowCertificateErrorCallback; allow: cint); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
	end;

	// Implement this structure to handle events related to browser requests. The
	// functions of this structure will be called on the thread indicated
	TCefRequestHandler = record
		// Base structure.
		base: TCefBase;
		// Called on the UI thread before browser navigation. Return true (1) to
		// cancel the navigation or false (0) to allow the navigation to proceed. The
		// |request| object cannot be modified in this callback.
		// cef_load_handler_t::OnLoadingStateChange will be called twice in all cases.
		// If the navigation is allowed cef_load_handler_t::OnLoadStart and
		// cef_load_handler_t::OnLoadEnd will be called. If the navigation is canceled
		// cef_load_handler_t::OnLoadError will be called with an |errorCode| value of
		// ERR_ABORTED.
		on_before_browse: function(self: PCefRequestHandler; browser: PCefBrowser; frame: PCefFrame; request: PCefRequest; is_redirect: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Called on the IO thread before a resource request is loaded. The |request|
		// object may be modified. To cancel the request return true (1) otherwise
		// return false (0).
		on_before_resource_load: function(self: PCefRequestHandler; browser: PCefBrowser; frame: PCefFrame; request: PCefRequest): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Called on the IO thread before a resource is loaded. To allow the resource
		// to load normally return NULL. To specify a handler for the resource return
		// a cef_resource_handler_t object. The |request| object should not be
		// modified in this callback.
		get_resource_handler: function(self: PCefRequestHandler; browser: PCefBrowser; frame: PCefFrame; request: PCefRequest): PCefResourceHandler; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Called on the IO thread when a resource load is redirected. The |old_url|
		// parameter will contain the old URL. The |new_url| parameter will contain
		// the new URL and can be changed if desired.
		on_resource_redirect: procedure(self: PCefRequestHandler; browser: PCefBrowser; frame: PCefFrame; const old_url: PCefString; new_url: PCefString); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Called on the IO thread when the browser needs credentials from the user.
		// |isProxy| indicates whether the host is a proxy server. |host| contains the
		// hostname and |port| contains the port number. Return true (1) to continue
		// the request and call cef_auth_callback_t::cont() when the authentication
		// information is available. Return false (0) to cancel the request.
		get_auth_credentials: function(self: PCefRequestHandler; browser: PCefBrowser; frame: PCefFrame; isProxy: cint; const host: PCefString; port: cint; const realm: PCefString; const scheme: PCefString; callback: PCefAuthCallback): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Called on the IO thread when JavaScript requests a specific storage quota
		// size via the webkitStorageInfo.requestQuota function. |origin_url| is the
		// origin of the page making the request. |new_size| is the requested quota
		// size in bytes. Return true (1) and call cef_quota_callback_t::cont() either
		// in this function or at a later time to grant or deny the request. Return
		// false (0) to cancel the request.
		on_quota_request: function(self: PCefRequestHandler; browser: PCefBrowser; const origin_url: PCefString; new_size: cint64; callback: PCefQuotaCallback): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Called on the UI thread to handle requests for URLs with an unknown
		// protocol component. Set |allow_os_execution| to true (1) to attempt
		// execution via the registered OS protocol handler, if any. SECURITY WARNING:
		// YOU SHOULD USE THIS METHOD TO ENFORCE RESTRICTIONS BASED ON SCHEME, HOST OR
		// OTHER URL ANALYSIS BEFORE ALLOWING OS EXECUTION.
		on_protocol_execution: procedure(self: PCefRequestHandler; browser: PCefBrowser; const url: PCefString; allow_os_execution: pcint); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Called on the UI thread to handle requests for URLs with an invalid SSL
		// certificate. Return true (1) and call
		// cef_allow_certificate_error_callback_t:: cont() either in this function or
		// at a later time to continue or cancel the request. Return false (0) to
		// cancel the request immediately. If |callback| is NULL the error cannot be
		// recovered from and the request will be canceled automatically. If
		// CefSettings.ignore_certificate_errors is set all invalid certificates will
		// be accepted without calling this function.
		on_certificate_error: function(self: PCefRequestHandler; cert_error: TCefErrorcode; const request_url: PCefString; callback: PCefAllowCertificateErrorCallback): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Called on the browser process IO thread before a plugin is loaded. Return
		// true (1) to block loading of the plugin.
		on_before_plugin_load: function(self: PCefRequestHandler; browser: PCefBrowser; const url: PCefString; const policy_url: PCefString; info: PCefWebPluginInfo): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Called on the browser process UI thread when a plugin has crashed.
		// |plugin_path| is the path of the plugin that crashed.
		on_plugin_crashed: procedure(self: PCefRequestHandler; browser: PCefBrowser; const plugin_path: PCefString); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Called on the browser process UI thread when the render process terminates
		// unexpectedly. |status| indicates how the process terminated.
		on_render_process_terminated: procedure(self: PCefRequestHandler; browser: PCefBrowser; status: TCefTerminationStatus); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
	end;

	//............................................................................cef_resource_bundle_handler_capi
	// Structure used to implement a custom resource bundle structure. The functions
	// of this structure may be called on multiple threads
	TCefResourceBundleHandler = record
		// Base structure.
		base: TCefBase;
		// Called to retrieve a localized translation for the string specified by
		// |message_id|. To provide the translation set |string| to the translation
		// string and return true (1). To use the default translation return false
		// (0). Supported message IDs are listed in cef_pack_strings.h.
		get_localized_string: function(self: PCefResourceBundleHandler; message_id: cint; _string: PCefString): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Called to retrieve data for the resource specified by |resource_id|. To
		// provide the resource data set |data| and |data_size| to the data pointer
		// and size respectively and return true (1). To use the default resource data
		// return false (0). The resource data will not be copied and must remain
		// resident in memory. Supported resource IDs are listed in
		// cef_pack_resources.h.
		get_data_resource: function(self: PCefResourceBundleHandler; resource_id: cint; data: void; data_size: pcsize_t): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
	end;

	//............................................................................cef_resource_handler_capi
	// Structure used to implement a custom request handler structure. The functions
	// of this structure will always be called on the IO thread
	TCefResourceHandler = record
		// Base structure.
		base: TCefBase;
		// Begin processing the request. To handle the request return true (1) and
		// call cef_callback_t::cont() once the response header information is
		// available (cef_callback_t::cont() can also be called from inside this
		// function if header information is available immediately). To cancel the
		// request return false (0).
		process_request: function(self: PCefResourceHandler; request: PCefRequest; callback: PCefCallback): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Retrieve response header information. If the response length is not known
		// set |response_length| to -1 and read_response() will be called until it
		// returns false (0). If the response length is known set |response_length| to
		// a positive value and read_response() will be called until it returns false
		// (0) or the specified number of bytes have been read. Use the |response|
		// object to set the mime type, http status code and other optional header
		// values. To redirect the request to a new URL set |redirectUrl| to the new
		// URL.
		get_response_headers: procedure(self: PCefResourceHandler; response: PCefResponse; response_length: pcint64; redirectUrl: PCefString); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Read response data. If data is available immediately copy up to
		// |bytes_to_read| bytes into |data_out|, set |bytes_read| to the number of
		// bytes copied, and return true (1). To read the data at a later time set
		// |bytes_read| to 0, return true (1) and call cef_callback_t::cont() when the
		// data is available. To indicate response completion return false (0).
		read_response: function(self: PCefResourceHandler; data_out: void; bytes_to_read: cint; bytes_read: pcint; callback: PCefCallback): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Return true (1) if the specified cookie can be sent with the request or
		// false (0) otherwise. If false (0) is returned for any cookie then no
		// cookies will be sent with the request.
		can_get_cookie: function(self: PCefResourceHandler; const cookie: PCefCookie): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Return true (1) if the specified cookie returned with the response can be
		// set or false (0) otherwise.
		can_set_cookie: function(self: PCefResourceHandler; const cookie: PCefCookie): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Request processing has been canceled.
		cancel: procedure(self: PCefResourceHandler); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
	end;

	//............................................................................cef_response_capi
	// Structure used to represent a web response. The functions of this structure
	// may be called on any thread
	TCefResponse = record
		// Base structure.
		base: TCefBase;
		// Returns true (1) if this object is read-only.
		is_read_only: function(self: PCefResponse): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Get the response status code.
		get_status: function(self: PCefResponse): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Set the response status code.
		set_status: procedure(self: PCefResponse; status: cint); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Get the response status text.
		// The resulting string must be freed by calling cef_string_userfree_free().
		get_status_text: function(self: PCefResponse): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Set the response status text.
		set_status_text: procedure(self: PCefResponse; const statusText: PCefString); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Get the response mime type.
		// The resulting string must be freed by calling cef_string_userfree_free().
		get_mime_type: function(self: PCefResponse): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Set the response mime type.
		set_mime_type: procedure(self: PCefResponse; const mimeType: PCefString); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Get the value for the specified response header field.
		// The resulting string must be freed by calling cef_string_userfree_free().
		get_header: function(self: PCefResponse; const name: PCefString): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Get all response header fields.
		get_header_map: procedure(self: PCefResponse; headerMap: TCefStringMultimap); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Set all response header fields.
		set_header_map: procedure(self: PCefResponse; headerMap: TCefStringMultimap); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
	end;

	//............................................................................cef_scheme_capi
	// Structure that manages custom scheme registrations
	TCefSchemeRegistrar = record
		// Base structure.
		base: TCefBase;
		// Register a custom scheme. This function should not be called for the built-
		// in HTTP, HTTPS, FILE, FTP, ABOUT and DATA schemes.
		// If |is_standard| is true (1) the scheme will be treated as a standard
		// scheme. Standard schemes are subject to URL canonicalization and parsing
		// rules as defined in the Common Internet Scheme Syntax RFC 1738 Section 3.1
		// available at http://www.ietf.org/rfc/rfc1738.txt
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
		// For non-standard scheme URLs only the "scheme:" component is parsed and
		// canonicalized. The remainder of the URL will be passed to the handler as-
		// is. For example, "scheme:///some%20text" will remain the same. Non-standard
		// scheme URLs cannot be used as a target for form submission.
		// If |is_local| is true (1) the scheme will be treated as local (i.e., with
		// the same security rules as those applied to "file" URLs). Normal pages
		// cannot link to or access local URLs. Also, by default, local URLs can only
		// perform XMLHttpRequest calls to the same URL (origin + path) that
		// originated the request. To allow XMLHttpRequest calls from a local URL to
		// other URLs with the same origin set the
		// CefSettings.file_access_from_file_urls_allowed value to true (1). To allow
		// XMLHttpRequest calls from a local URL to all origins set the
		// CefSettings.universal_access_from_file_urls_allowed value to true (1).
		// If |is_display_isolated| is true (1) the scheme will be treated as display-
		// isolated. This means that pages cannot display these URLs unless they are
		// from the same scheme. For example, pages in another origin cannot create
		// iframes or hyperlinks to URLs with this scheme.
		// This function may be called on any thread. It should only be called once
		// per unique |scheme_name| value. If |scheme_name| is already registered or
		// if an error occurs this function will return false (0).
		add_custom_scheme: function(self: PCefSchemeRegistrar; const scheme_name: PCefString; is_standard: cint; is_local: cint; is_display_isolated: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
	end;

	// Structure that creates cef_resource_handler_t instances for handling scheme
	// requests. The functions of this structure will always be called on the IO
	// thread
	TCefSchemeHandlerFactory = record
		// Base structure.
		base: TCefBase;
		// Return a new resource handler instance to handle the request or an NULL
		// reference to allow default handling of the request. |browser| and |frame|
		// will be the browser window and frame respectively that originated the
		// request or NULL if the request did not originate from a browser window (for
		// example, if the request came from cef_urlrequest_t). The |request| object
		// passed to this function will not contain cookie data.
		create: function(self: PCefSchemeHandlerFactory; browser: PCefBrowser; frame: PCefFrame; const scheme_name: PCefString; request: PCefRequest): PCefResourceHandler; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
	end;

	//............................................................................cef_stream_capi
	// Structure the client can implement to provide a custom stream reader. The
	// functions of this structure may be called on any thread
	TCefReadHandler = record
		// Base structure.
		base: TCefBase;
		// Read raw binary data.
		read: function(self: PCefReadHandler; ptr: void; size: csize_t; n: csize_t): csize_t; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Seek to the specified offset position. |whence| may be any one of SEEK_CUR,
		// SEEK_END or SEEK_SET. Return zero on success and non-zero on failure.
		seek: function(self: PCefReadHandler; offset: cint64; whence: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Return the current offset position.
		tell: function(self: PCefReadHandler): cint64; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Return non-zero if at end of file.
		eof: function(self: PCefReadHandler): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Return true (1) if this handler performs work like accessing the file
		// system which may block. Used as a hint for determining the thread to access
		// the handler from.
		may_block: function(self: PCefReadHandler): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
	end;

	// Structure used to read data from a stream. The functions of this structure
	// may be called on any thread
	TCefStreamReader = record
		// Base structure.
		base: TCefBase;
		// Read raw binary data.
		read: function(self: PCefStreamReader; ptr: void; size: csize_t; n: csize_t): csize_t; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Seek to the specified offset position. |whence| may be any one of SEEK_CUR,
		// SEEK_END or SEEK_SET. Returns zero on success and non-zero on failure.
		seek: function(self: PCefStreamReader; offset: cint64; whence: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Return the current offset position.
		tell: function(self: PCefStreamReader): cint64; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Return non-zero if at end of file.
		eof: function(self: PCefStreamReader): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns true (1) if this reader performs work like accessing the file
		// system which may block. Used as a hint for determining the thread to access
		// the reader from.
		may_block: function(self: PCefStreamReader): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
	end;

	// Structure the client can implement to provide a custom stream writer. The
	// functions of this structure may be called on any thread
	TCefWriteHandler = record
		// Base structure.
		base: TCefBase;
		// Write raw binary data.
		write: function(self: PCefWriteHandler; const ptr: void; size: csize_t; n: csize_t): csize_t; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Seek to the specified offset position. |whence| may be any one of SEEK_CUR,
		// SEEK_END or SEEK_SET. Return zero on success and non-zero on failure.
		seek: function(self: PCefWriteHandler; offset: cint64; whence: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Return the current offset position.
		tell: function(self: PCefWriteHandler): cint64; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Flush the stream.
		flush: function(self: PCefWriteHandler): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Return true (1) if this handler performs work like accessing the file
		// system which may block. Used as a hint for determining the thread to access
		// the handler from.
		may_block: function(self: PCefWriteHandler): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
	end;

	// Structure used to write data to a stream. The functions of this structure may
	// be called on any thread
	TCefStreamWriter = record
		// Base structure.
		base: TCefBase;
		// Write raw binary data.
		write: function(self: PCefStreamWriter; const ptr: void; size: csize_t; n: csize_t): csize_t; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Seek to the specified offset position. |whence| may be any one of SEEK_CUR,
		// SEEK_END or SEEK_SET. Returns zero on success and non-zero on failure.
		seek: function(self: PCefStreamWriter; offset: cint64; whence: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Return the current offset position.
		tell: function(self: PCefStreamWriter): cint64; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Flush the stream.
		flush: function(self: PCefStreamWriter): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns true (1) if this writer performs work like accessing the file
		// system which may block. Used as a hint for determining the thread to access
		// the writer from.
		may_block: function(self: PCefStreamWriter): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
	end;

	//............................................................................cef_string_visitor_capi
	// Implement this structure to receive string values asynchronously
	TCefStringVisitor = record
		// Base structure.
		base: TCefBase;
		// Method that will be executed.
		visit: procedure(self: PCefStringVisitor; const _string: PCefString); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
	end;

	//............................................................................cef_task_capi
	// Implement this structure for asynchronous task execution. If the task is
	// posted successfully and if the associated message loop is still running then
	// the execute() function will be called on the target thread. If the task fails
	// to post then the task object may be destroyed on the source thread instead of
	// the target thread. For this reason be cautious when performing work in the
	// task object destructor
	TCefTask = record
		// Base structure.
		base: TCefBase;
		// Method that will be executed on the target thread.
		execute: procedure(self: PCefTask); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
	end;

	// Structure that asynchronously executes tasks on the associated thread. It is
	// safe to call the functions of this structure on any thread.
	//
	// CEF maintains multiple internal threads that are used for handling different
	// types of tasks in different processes. The cef_thread_id_t definitions in
	// cef_types.h list the common CEF threads. Task runners are also available for
	// other CEF threads as appropriate (for example, V8 WebWorker threads
	TCefTaskRunner = record
		// Base structure.
		base: TCefBase;
		// Returns true (1) if this object is pointing to the same task runner as
		// |that| object.
		is_same: function(self: PCefTaskRunner; that: PCefTaskRunner): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns true (1) if this task runner belongs to the current thread.
		belongs_to_current_thread: function(self: PCefTaskRunner): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns true (1) if this task runner is for the specified CEF thread.
		belongs_to_thread: function(self: PCefTaskRunner; threadId: TCefThreadId): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Post a task for execution on the thread associated with this task runner.
		// Execution will occur asynchronously.
		post_task: function(self: PCefTaskRunner; task: PCefTask): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Post a task for delayed execution on the thread associated with this task
		// runner. Execution will occur asynchronously. Delayed tasks are not
		// supported on V8 WebWorker threads and will be executed without the
		// specified delay.
		post_delayed_task: function(self: PCefTaskRunner; task: PCefTask; delay_ms: cint64): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
	end;

	//............................................................................cef_trace_capi
	// Implement this structure to receive notification when tracing has completed.
	// The functions of this structure will be called on the browser process UI
	// thread
	TCefEndTracingCallback = record
		// Base structure.
		base: TCefBase;
		// Called after all processes have sent their trace data. |tracing_file| is
		// the path at which tracing data was written. The client is responsible for
		// deleting |tracing_file|.
		on_end_tracing_complete: procedure(self: PCefEndTracingCallback; const tracing_file: PCefString); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
	end;

	//............................................................................cef_urlrequest_capi
	// Structure used to make a URL request. URL requests are not associated with a
	// browser instance so no cef_client_t callbacks will be executed. URL requests
	// can be created on any valid CEF thread in either the browser or render
	// process. Once created the functions of the URL request object must be
	// accessed on the same thread that created it
	TCefUrlrequest = record
		// Base structure.
		base: TCefBase;
		// Returns the request object used to create this URL request. The returned
		// object is read-only and should not be modified.
		get_request: function(self: PCefUrlrequest): PCefRequest; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the client.
		get_client: function(self: PCefUrlrequest): PCefUrlrequestClient; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the request status.
		get_request_status: function(self: PCefUrlrequest): TCefUrlrequestStatus; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the request error if status is UR_CANCELED or UR_FAILED, or 0
		// otherwise.
		get_request_error: function(self: PCefUrlrequest): TCefErrorcode; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the response, or NULL if no response information is available.
		// Response information will only be available after the upload has completed.
		// The returned object is read-only and should not be modified.
		get_response: function(self: PCefUrlrequest): PCefResponse; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Cancel the request.
		cancel: procedure(self: PCefUrlrequest); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
	end;

	// Structure that should be implemented by the cef_urlrequest_t client. The
	// functions of this structure will be called on the same thread that created
	// the request unless otherwise documented
	TCefUrlrequestClient = record
		// Base structure.
		base: TCefBase;
		// Notifies the client that the request has completed. Use the
		// cef_urlrequest_t::GetRequestStatus function to determine if the request was
		// successful or not.
		on_request_complete: procedure(self: PCefUrlrequestClient; request: PCefUrlrequest); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Notifies the client of upload progress. |current| denotes the number of
		// bytes sent so far and |total| is the total size of uploading data (or -1 if
		// chunked upload is enabled). This function will only be called if the
		// UR_FLAG_REPORT_UPLOAD_PROGRESS flag is set on the request.
		on_upload_progress: procedure(self: PCefUrlrequestClient; request: PCefUrlrequest; current: cuint64; total: cuint64); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Notifies the client of download progress. |current| denotes the number of
		// bytes received up to the call and |total| is the expected total size of the
		// response (or -1 if not determined).
		on_download_progress: procedure(self: PCefUrlrequestClient; request: PCefUrlrequest; current: cuint64; total: cuint64); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Called when some part of the response is read. |data| contains the current
		// bytes received since the last call. This function will not be called if the
		// UR_FLAG_NO_DOWNLOAD_DATA flag is set on the request.
		on_download_data: procedure(self: PCefUrlrequestClient; request: PCefUrlrequest; const data: void; data_length: csize_t); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Called on the IO thread when the browser needs credentials from the user.
		// |isProxy| indicates whether the host is a proxy server. |host| contains the
		// hostname and |port| contains the port number. Return true (1) to continue
		// the request and call cef_auth_callback_t::cont() when the authentication
		// information is available. Return false (0) to cancel the request. This
		// function will only be called for requests initiated from the browser
		// process.
		get_auth_credentials: function(self: PCefUrlrequestClient; isProxy: cint; const host: PCefString; port: cint; const realm: PCefString; const scheme: PCefString; callback: PCefAuthCallback): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
	end;

	//............................................................................cef_v8_capi
	// Structure representing a V8 context handle. V8 handles can only be accessed
	// from the thread on which they are created. Valid threads for creating a V8
	// handle include the render process main thread (TID_RENDERER) and WebWorker
	// threads. A task runner for posting tasks on the associated thread can be
	// retrieved via the cef_v8context_t::get_task_runner() function
	TCefV8context=record
		// Base structure.
		base:TCefBase;
		// Returns the task runner associated with this context. V8 handles can only
		// be accessed from the thread on which they are created. This function can be
		// called on any render process thread.
		get_task_runner:function(self:PCefV8context):PCefTaskRunner; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns true (1) if the underlying handle is valid and it can be accessed
		// on the current thread. Do not call any other functions if this function
		// returns false (0).
		is_valid:function(self:PCefV8context): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the browser for this context. This function will return an NULL
		// reference for WebWorker contexts.
		get_browser:function(self:PCefV8context):PCefBrowser; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the frame for this context. This function will return an NULL
		// reference for WebWorker contexts.
		get_frame:function(self:PCefV8context):PCefFrame; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the global object for this context. The context must be entered
		// before calling this function.
		get_global:function(self:PCefV8context):PCefV8value; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Enter this context. A context must be explicitly entered before creating a
		// V8 Object, Array, Function or Date asynchronously. exit() must be called
		// the same number of times as enter() before releasing this context. V8
		// objects belong to the context in which they are created. Returns true (1)
		// if the scope was entered successfully.
		enter:function(self:PCefV8context): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Exit this context. Call this function only after calling enter(). Returns
		// true (1) if the scope was exited successfully.
		exit:function(self:PCefV8context): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns true (1) if this object is pointing to the same handle as |that|
		// object.
		is_same:function(self:PCefV8context; that:PCefV8context): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Evaluates the specified JavaScript code using this context's global object.
		// On success |retval| will be set to the return value, if any, and the
		// function will return true (1). On failure |exception| will be set to the
		// exception, if any, and the function will return false (0).
		eval:function(self:PCefV8context; const code:PCefString; retval:PCefV8value; exception:PCefV8exception): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
	end;

  // Structure that should be implemented to handle V8 function calls. The
	// functions of this structure will be called on the thread associated with the
	// V8 function
	TCefV8handler=record
		// Base structure.
		base:TCefBase;
		// Handle execution of the function identified by |name|. |object| is the
		// receiver ('this' object) of the function. |arguments| is the list of
		// arguments passed to the function. If execution succeeds set |retval| to the
		// function return value. If execution fails set |exception| to the exception
		// that will be thrown. Return true (1) if execution was handled.
		execute: function(self: PCefv8Handler;
        const name: PCefString; obj: PCefv8Value; argumentsCount: csize_t;
        const arguments: PPCefV8Value; var retval: PCefV8Value;
        var exception: TCefString): Integer; {$IFNDEF UNIX}{$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF}{$ELSE}cdecl{$ENDIF};
	end;

  // Structure that should be implemented to handle V8 accessor calls. Accessor
	// identifiers are registered by calling cef_v8value_t::set_value_byaccessor().
	// The functions of this structure will be called on the thread associated with
	// the V8 accessor
	TCefV8accessor=record
		// Base structure.
		base:TCefBase;
		// Handle retrieval the accessor value identified by |name|. |object| is the
		// receiver ('this' object) of the accessor. If retrieval succeeds set
		// |retval| to the return value. If retrieval fails set |exception| to the
		// exception that will be thrown. Return true (1) if accessor retrieval was
		// handled.
		get:function(self:PCefV8accessor; const name:PCefString; _object:PCefV8value; out retval:PCefV8value; exception:PCefString): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Handle assignment of the accessor value identified by |name|. |object| is
		// the receiver ('this' object) of the accessor. |value| is the new value
		// being assigned to the accessor. If assignment fails set |exception| to the
		// exception that will be thrown. Return true (1) if accessor assignment was
		// handled.
		_set:function(self:PCefV8accessor; const name:PCefString; _object:PCefV8value; value:PCefV8value; exception:PCefString): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
	end;

  // Structure representing a V8 exception. The functions of this structure may be
	// called on any render process thread
	TCefV8exception=record
		// Base structure.
		base:TCefBase;
		// Returns the exception message.
		// The resulting string must be freed by calling cef_string_userfree_free().
		get_message:function(self:PCefV8exception):PCefStringUserfree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the line of source code that the exception occurred within.
		// The resulting string must be freed by calling cef_string_userfree_free().
		get_source_line:function(self:PCefV8exception):PCefStringUserfree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the resource name for the script from where the function causing
		// the error originates.
		// The resulting string must be freed by calling cef_string_userfree_free().
		get_script_resource_name:function(self:PCefV8exception):PCefStringUserfree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the 1-based number of the line where the error occurred or 0 if the
		// line number is unknown.
		get_line_number:function(self:PCefV8exception): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the index within the script of the first character where the error
		// occurred.
		get_start_position:function(self:PCefV8exception): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the index within the script of the last character where the error
		// occurred.
		get_end_position:function(self:PCefV8exception): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the index within the line of the first character where the error
		// occurred.
		get_start_column:function(self:PCefV8exception): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the index within the line of the last character where the error
		// occurred.
		get_end_column:function(self:PCefV8exception): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
	end;

  // Structure representing a V8 value handle. V8 handles can only be accessed
	// from the thread on which they are created. Valid threads for creating a V8
	// handle include the render process main thread (TID_RENDERER) and WebWorker
	// threads. A task runner for posting tasks on the associated thread can be
	// retrieved via the cef_v8context_t::get_task_runner() function
	TCefV8value=record
		// Base structure.
		base:TCefBase;
		// Returns true (1) if the underlying handle is valid and it can be accessed
		// on the current thread. Do not call any other functions if this function
		// returns false (0).
		is_valid:function(self:PCefV8value): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// True if the value type is undefined.
		is_undefined:function(self:PCefV8value): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// True if the value type is null.
		is_null:function(self:PCefV8value): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// True if the value type is bool.
		is_bool:function(self:PCefV8value): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// True if the value type is int.
		is_int:function(self:PCefV8value): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// True if the value type is unsigned int.
		is_uint:function(self:PCefV8value): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// True if the value type is double.
		is_double:function(self:PCefV8value): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// True if the value type is Date.
		is_date:function(self:PCefV8value): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// True if the value type is string.
		is_string:function(self:PCefV8value): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// True if the value type is object.
		is_object:function(self:PCefV8value): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// True if the value type is array.
		is_array:function(self:PCefV8value): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// True if the value type is function.
		is_function:function(self:PCefV8value): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns true (1) if this object is pointing to the same handle as |that|
		// object.
		is_same:function(self:PCefV8value; that:PCefV8value): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Return a bool value.  The underlying data will be converted to if
		// necessary.
		get_bool_value:function(self:PCefV8value): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Return an int value.  The underlying data will be converted to if
		// necessary.
		get_int_value:function(self:PCefV8value): cint32; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Return an unisgned int value.  The underlying data will be converted to if
		// necessary.
		get_uint_value:function(self:PCefV8value): cuint32; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Return a double value.  The underlying data will be converted to if
		// necessary.
		get_double_value:function(self:PCefV8value): cdouble; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Return a Date value.  The underlying data will be converted to if
		// necessary.
		get_date_value:function(self:PCefV8value):TCefTime; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Return a string value.  The underlying data will be converted to if
		// necessary.
		// The resulting string must be freed by calling cef_string_userfree_free().
		get_string_value:function(self:PCefV8value):PCefStringUserfree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// OBJECT METHODS - These functions are only available on objects. Arrays and
		// functions are also objects. String- and integer-based keys can be used
		// interchangably with the framework converting between them as necessary.
		// Returns true (1) if this is a user created object.
		is_user_created:function(self:PCefV8value): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns true (1) if the last function call resulted in an exception. This
		// attribute exists only in the scope of the current CEF value object.
		has_exception:function(self:PCefV8value): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the exception resulting from the last function call. This attribute
		// exists only in the scope of the current CEF value object.
		get_exception:function(self:PCefV8value):PCefV8exception; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Clears the last exception and returns true (1) on success.
		clear_exception:function(self:PCefV8value): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns true (1) if this object will re-throw future exceptions. This
		// attribute exists only in the scope of the current CEF value object.
		will_rethrow_exceptions:function(self:PCefV8value): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Set whether this object will re-throw future exceptions. By default
		// exceptions are not re-thrown. If a exception is re-thrown the current
		// context should not be accessed again until after the exception has been
		// caught and not re-thrown. Returns true (1) on success. This attribute
		// exists only in the scope of the current CEF value object.
		set_rethrow_exceptions:function(self:PCefV8value; rethrow: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns true (1) if the object has a value with the specified identifier.
		has_value_bykey:function(self:PCefV8value; const key:PCefString): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns true (1) if the object has a value with the specified identifier.
		has_value_byindex:function(self:PCefV8value; index: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Deletes the value with the specified identifier and returns true (1) on
		// success. Returns false (0) if this function is called incorrectly or an
		// exception is thrown. For read-only and don't-delete values this function
		// will return true (1) even though deletion failed.
		delete_value_bykey:function(self:PCefV8value; const key:PCefString): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Deletes the value with the specified identifier and returns true (1) on
		// success. Returns false (0) if this function is called incorrectly, deletion
		// fails or an exception is thrown. For read-only and don't-delete values this
		// function will return true (1) even though deletion failed.
		delete_value_byindex:function(self:PCefV8value; index: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the value with the specified identifier on success. Returns NULL if
		// this function is called incorrectly or an exception is thrown.
		get_value_bykey:function(self:PCefV8value; const key:PCefString):PCefV8value; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the value with the specified identifier on success. Returns NULL if
		// this function is called incorrectly or an exception is thrown.
		get_value_byindex:function(self:PCefV8value; index: cint):PCefV8value; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Associates a value with the specified identifier and returns true (1) on
		// success. Returns false (0) if this function is called incorrectly or an
		// exception is thrown. For read-only values this function will return true
		// (1) even though assignment failed.
		set_value_bykey:function(self:PCefV8value; const key:PCefString; value:PCefV8value; attribute:TCefV8Propertyattribute): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Associates a value with the specified identifier and returns true (1) on
		// success. Returns false (0) if this function is called incorrectly or an
		// exception is thrown. For read-only values this function will return true
		// (1) even though assignment failed.
		set_value_byindex:function(self:PCefV8value; index: cint; value:PCefV8value): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Registers an identifier and returns true (1) on success. Access to the
		// identifier will be forwarded to the cef_v8accessor_t instance passed to
		// cef_v8value_t::cef_v8value_create_object(). Returns false (0) if this
		// function is called incorrectly or an exception is thrown. For read-only
		// values this function will return true (1) even though assignment failed.
		set_value_byaccessor:function(self:PCefV8value; const key:PCefString; settings:TCefV8Accesscontrol; attribute:TCefV8Propertyattribute): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Read the keys for the object's values into the specified vector. Integer-
		// based keys will also be returned as strings.
		get_keys:function(self:PCefV8value; keys:TCefStringList): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Sets the user data for this object and returns true (1) on success. Returns
		// false (0) if this function is called incorrectly. This function can only be
		// called on user created objects.
		set_user_data:function(self:PCefV8value; user_data:PCefBase): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the user data, if any, assigned to this object.
		get_user_data:function(self:PCefV8value):PCefBase; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the amount of externally allocated memory registered for the
		// object.
		get_externally_allocated_memory:function(self:PCefV8value): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Adjusts the amount of registered external memory for the object. Used to
		// give V8 an indication of the amount of externally allocated memory that is
		// kept alive by JavaScript objects. V8 uses this information to decide when
		// to perform global garbage collection. Each cef_v8value_t tracks the amount
		// of external memory associated with it and automatically decreases the
		// global total by the appropriate amount on its destruction.
		// |change_in_bytes| specifies the number of bytes to adjust by. This function
		// returns the number of bytes associated with the object after the
		// adjustment. This function can only be called on user created objects.
		adjust_externally_allocated_memory:function(self:PCefV8value; change_in_bytes: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// ARRAY METHODS - These functions are only available on arrays.
		// Returns the number of elements in the array.
		get_array_length:function(self:PCefV8value): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// FUNCTION METHODS - These functions are only available on functions.
		// Returns the function name.
		// The resulting string must be freed by calling cef_string_userfree_free().
		get_function_name:function(self:PCefV8value):PCefStringUserfree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the function handler or NULL if not a CEF-created function.
		get_function_handler:function(self:PCefV8value):PCefV8handler; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Execute the function using the current V8 context. This function should
		// only be called from within the scope of a cef_v8handler_t or
		// cef_v8accessor_t callback, or in combination with calling enter() and
		// exit() on a stored cef_v8context_t reference. |object| is the receiver
		// ('this' object) of the function. If |object| is NULL the current context's
		// global object will be used. |arguments| is the list of arguments that will
		// be passed to the function. Returns the function return value on success.
		// Returns NULL if this function is called incorrectly or an exception is
		// thrown.
		execute_function:function(self:PCefV8value; _object:PCefV8value; argumentsCount: csize_t; arguments:PPCefV8value):PCefV8value; {$IFNDEF UNIX}{$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF}{$ELSE}cdecl{$ENDIF};
		// Execute the function using the specified V8 context. |object| is the
		// receiver ('this' object) of the function. If |object| is NULL the specified
		// context's global object will be used. |arguments| is the list of arguments
		// that will be passed to the function. Returns the function return value on
		// success. Returns NULL if this function is called incorrectly or an
		// exception is thrown.
		execute_function_with_context:function(self:PCefV8value; context:PCefV8context; _object:PCefV8value; argumentsCount: csize_t; arguments:PPCefV8value):PCefV8value; {$IFNDEF UNIX}{$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF}{$ELSE}cdecl{$ENDIF};
	end;

  // Structure representing a V8 stack trace handle. V8 handles can only be
	// accessed from the thread on which they are created. Valid threads for
	// creating a V8 handle include the render process main thread (TID_RENDERER)
	// and WebWorker threads. A task runner for posting tasks on the associated
	// thread can be retrieved via the cef_v8context_t::get_task_runner() function
	TCefV8stackTrace=record
		// Base structure.
		base:TCefBase;
		// Returns true (1) if the underlying handle is valid and it can be accessed
		// on the current thread. Do not call any other functions if this function
		// returns false (0).
		is_valid:function(self:PCefV8stackTrace): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the number of stack frames.
		get_frame_count:function(self:PCefV8stackTrace): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the stack frame at the specified 0-based index.
		get_frame:function(self:PCefV8stackTrace; index: cint):PCefV8stackFrame; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
	end;

  // Structure representing a V8 stack frame handle. V8 handles can only be
	// accessed from the thread on which they are created. Valid threads for
	// creating a V8 handle include the render process main thread (TID_RENDERER)
	// and WebWorker threads. A task runner for posting tasks on the associated
	// thread can be retrieved via the cef_v8context_t::get_task_runner() function
	TCefV8stackFrame=record
		// Base structure.
		base:TCefBase;
		// Returns true (1) if the underlying handle is valid and it can be accessed
		// on the current thread. Do not call any other functions if this function
		// returns false (0).
		is_valid:function(self:PCefV8stackFrame): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the name of the resource script that contains the function.
		// The resulting string must be freed by calling cef_string_userfree_free().
		get_script_name:function(self:PCefV8stackFrame):PCefStringUserfree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the name of the resource script that contains the function or the
		// sourceURL value if the script name is undefined and its source ends with a
		// "//@ sourceURL=..." string.
		// The resulting string must be freed by calling cef_string_userfree_free().
		get_script_name_or_source_url:function(self:PCefV8stackFrame):PCefStringUserfree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the name of the function.
		// The resulting string must be freed by calling cef_string_userfree_free().
		get_function_name:function(self:PCefV8stackFrame):PCefStringUserfree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the 1-based line number for the function call or 0 if unknown.
		get_line_number:function(self:PCefV8stackFrame): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the 1-based column offset on the line for the function call or 0 if
		// unknown.
		get_column:function(self:PCefV8stackFrame): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns true (1) if the function was compiled using eval().
		is_eval:function(self:PCefV8stackFrame): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns true (1) if the function was called as a constructor via "new".
		is_constructor:function(self:PCefV8stackFrame): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
	end;

	//............................................................................cef_values_capi
	// Structure representing a binary value. Can be used on any process and thread
	TCefBinaryValue = record
		// Base structure.
		base: TCefBase;
		// Returns true (1) if this object is valid. Do not call any other functions
		// if this function returns false (0).
		is_valid: function(self: PCefBinaryValue): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns true (1) if this object is currently owned by another object.
		is_owned: function(self: PCefBinaryValue): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns a copy of this object. The data in this object will also be copied.
		copy: function(self: PCefBinaryValue): PCefBinaryValue; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the data size.
		get_size: function(self: PCefBinaryValue): csize_t; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Read up to |buffer_size| number of bytes into |buffer|. Reading begins at
		// the specified byte |data_offset|. Returns the number of bytes read.
		get_data: function(self: PCefBinaryValue; buffer: void; buffer_size: csize_t; data_offset: csize_t): csize_t; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
	end;

	// Structure representing a dictionary value. Can be used on any process and
	// thread
	TCefDictionaryValue = record
		// Base structure.
		base: TCefBase;
		// Returns true (1) if this object is valid. Do not call any other functions
		// if this function returns false (0).
		is_valid: function(self: PCefDictionaryValue): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns true (1) if this object is currently owned by another object.
		is_owned: function(self: PCefDictionaryValue): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns true (1) if the values of this object are read-only. Some APIs may
		// expose read-only objects.
		is_read_only: function(self: PCefDictionaryValue): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns a writable copy of this object. If |exclude_NULL_children| is true
		// (1) any NULL dictionaries or lists will be excluded from the copy.
		copy: function(self: PCefDictionaryValue; exclude_empty_children: cint): PCefDictionaryValue; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the number of values.
		get_size: function(self: PCefDictionaryValue): csize_t; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Removes all values. Returns true (1) on success.
		clear: function(self: PCefDictionaryValue): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns true (1) if the current dictionary has a value for the given key.
		has_key: function(self: PCefDictionaryValue; const key: PCefString): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Reads all keys for this dictionary into the specified vector.
		get_keys: function(self: PCefDictionaryValue; keys: TCefStringList): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Removes the value at the specified key. Returns true (1) is the value was
		// removed successfully.
		remove: function(self: PCefDictionaryValue; const key: PCefString): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the value type for the specified key.
		get_type: function(self: PCefDictionaryValue; const key: PCefString): TCefValueType; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the value at the specified key as type bool.
		get_bool: function(self: PCefDictionaryValue; const key: PCefString): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the value at the specified key as type int.
		get_int: function(self: PCefDictionaryValue; const key: PCefString): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the value at the specified key as type double.
		get_double: function(self: PCefDictionaryValue; const key: PCefString): cdouble; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the value at the specified key as type string.
		// The resulting string must be freed by calling cef_string_userfree_free().
		get_string: function(self: PCefDictionaryValue; const key: PCefString): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the value at the specified key as type binary.
		get_binary: function(self: PCefDictionaryValue; const key: PCefString): PCefBinaryValue; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the value at the specified key as type dictionary.
		get_dictionary: function(self: PCefDictionaryValue; const key: PCefString): PCefDictionaryValue; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the value at the specified key as type list.
		get_list: function(self: PCefDictionaryValue; const key: PCefString): PCefListValue; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Sets the value at the specified key as type null. Returns true (1) if the
		// value was set successfully.
		set_null: function(self: PCefDictionaryValue; const key: PCefString): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Sets the value at the specified key as type bool. Returns true (1) if the
		// value was set successfully.
		set_bool: function(self: PCefDictionaryValue; const key: PCefString; value: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Sets the value at the specified key as type int. Returns true (1) if the
		// value was set successfully.
		set_int: function(self: PCefDictionaryValue; const key: PCefString; value: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Sets the value at the specified key as type double. Returns true (1) if the
		// value was set successfully.
		set_double: function(self: PCefDictionaryValue; const key: PCefString; value: cdouble): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Sets the value at the specified key as type string. Returns true (1) if the
		// value was set successfully.
		set_string: function(self: PCefDictionaryValue; const key: PCefString; const value: PCefString): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Sets the value at the specified key as type binary. Returns true (1) if the
		// value was set successfully. If |value| is currently owned by another object
		// then the value will be copied and the |value| reference will not change.
		// Otherwise, ownership will be transferred to this object and the |value|
		// reference will be invalidated.
		set_binary: function(self: PCefDictionaryValue; const key: PCefString; value: PCefBinaryValue): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Sets the value at the specified key as type dict. Returns true (1) if the
		// value was set successfully. After calling this function the |value| object
		// will no longer be valid. If |value| is currently owned by another object
		// then the value will be copied and the |value| reference will not change.
		// Otherwise, ownership will be transferred to this object and the |value|
		// reference will be invalidated.
		set_dictionary: function(self: PCefDictionaryValue; const key: PCefString; value: PCefDictionaryValue): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Sets the value at the specified key as type list. Returns true (1) if the
		// value was set successfully. After calling this function the |value| object
		// will no longer be valid. If |value| is currently owned by another object
		// then the value will be copied and the |value| reference will not change.
		// Otherwise, ownership will be transferred to this object and the |value|
		// reference will be invalidated.
		set_list: function(self: PCefDictionaryValue; const key: PCefString; value: PCefListValue): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
	end;

	// Structure representing a list value. Can be used on any process and thread
	TCefListValue = record
		// Base structure.
		base: TCefBase;
		// Returns true (1) if this object is valid. Do not call any other functions
		// if this function returns false (0).
		is_valid: function(self: PCefListValue): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns true (1) if this object is currently owned by another object.
		is_owned: function(self: PCefListValue): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns true (1) if the values of this object are read-only. Some APIs may
		// expose read-only objects.
		is_read_only: function(self: PCefListValue): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns a writable copy of this object.
		copy: function(self: PCefListValue): PCefListValue; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Sets the number of values. If the number of values is expanded all new
		// value slots will default to type null. Returns true (1) on success.
		set_size: function(self: PCefListValue; size: csize_t): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the number of values.
		get_size: function(self: PCefListValue): csize_t; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Removes all values. Returns true (1) on success.
		clear: function(self: PCefListValue): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Removes the value at the specified index.
		remove: function(self: PCefListValue; index: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the value type at the specified index.
		get_type: function(self: PCefListValue; index: cint): TCefValueType; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the value at the specified index as type bool.
		get_bool: function(self: PCefListValue; index: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the value at the specified index as type int.
		get_int: function(self: PCefListValue; index: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the value at the specified index as type double.
		get_double: function(self: PCefListValue; index: cint): cdouble; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the value at the specified index as type string.
		// The resulting string must be freed by calling cef_string_userfree_free().
		get_string: function(self: PCefListValue; index: cint): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the value at the specified index as type binary.
		get_binary: function(self: PCefListValue; index: cint): PCefBinaryValue; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the value at the specified index as type dictionary.
		get_dictionary: function(self: PCefListValue; index: cint): PCefDictionaryValue; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the value at the specified index as type list.
		get_list: function(self: PCefListValue; index: cint): PCefListValue; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Sets the value at the specified index as type null. Returns true (1) if the
		// value was set successfully.
		set_null: function(self: PCefListValue; index: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Sets the value at the specified index as type bool. Returns true (1) if the
		// value was set successfully.
		set_bool: function(self: PCefListValue; index: cint; value: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Sets the value at the specified index as type int. Returns true (1) if the
		// value was set successfully.
		set_int: function(self: PCefListValue; index: cint; value: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Sets the value at the specified index as type double. Returns true (1) if
		// the value was set successfully.
		set_double: function(self: PCefListValue; index: cint; value: cdouble): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Sets the value at the specified index as type string. Returns true (1) if
		// the value was set successfully.
		set_string: function(self: PCefListValue; index: cint; const value: PCefString): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Sets the value at the specified index as type binary. Returns true (1) if
		// the value was set successfully. After calling this function the |value|
		// object will no longer be valid. If |value| is currently owned by another
		// object then the value will be copied and the |value| reference will not
		// change. Otherwise, ownership will be transferred to this object and the
		// |value| reference will be invalidated.
		set_binary: function(self: PCefListValue; index: cint; value: PCefBinaryValue): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Sets the value at the specified index as type dict. Returns true (1) if the
		// value was set successfully. After calling this function the |value| object
		// will no longer be valid. If |value| is currently owned by another object
		// then the value will be copied and the |value| reference will not change.
		// Otherwise, ownership will be transferred to this object and the |value|
		// reference will be invalidated.
		set_dictionary: function(self: PCefListValue; index: cint; value: PCefDictionaryValue): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Sets the value at the specified index as type list. Returns true (1) if the
		// value was set successfully. After calling this function the |value| object
		// will no longer be valid. If |value| is currently owned by another object
		// then the value will be copied and the |value| reference will not change.
		// Otherwise, ownership will be transferred to this object and the |value|
		// reference will be invalidated.
		set_list: function(self: PCefListValue; index: cint; value: PCefListValue): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
	end;

	//............................................................................cef_web_plugin_capi
	// Information about a specific web plugin
	TCefWebPluginInfo = record
		// Base structure.
		base: TCefBase;
		// Returns the plugin name (i.e. Flash).
		// The resulting string must be freed by calling cef_string_userfree_free().
		get_name: function(self: PCefWebPluginInfo): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the plugin file path (DLL/bundle/library).
		// The resulting string must be freed by calling cef_string_userfree_free().
		get_path: function(self: PCefWebPluginInfo): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the version of the plugin (may be OS-specific).
		// The resulting string must be freed by calling cef_string_userfree_free().
		get_version: function(self: PCefWebPluginInfo): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns a description of the plugin from the version information.
		// The resulting string must be freed by calling cef_string_userfree_free().
		get_description: function(self: PCefWebPluginInfo): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
	end;

	// Structure to implement for visiting web plugin information. The functions of
	// this structure will be called on the browser process UI thread
	TCefWebPluginInfoVisitor = record
		// Base structure.
		base: TCefBase;
		// Method that will be called once for each plugin. |count| is the 0-based
		// index for the current plugin. |total| is the total number of plugins.
		// Return false (0) to stop visiting plugins. This function may never be
		// called if no plugins are found.
		visit: function(self: PCefWebPluginInfoVisitor; info: PCefWebPluginInfo; count: cint; total: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
	end;

	// Structure to implement for receiving unstable plugin information. The
	// functions of this structure will be called on the browser process IO thread
	TCefWebPluginUnstableCallback = record
		// Base structure.
		base: TCefBase;
		// Method that will be called for the requested plugin. |unstable| will be
		// true (1) if the plugin has reached the crash count threshold of 3 times in
		// 120 seconds.
		is_unstable: procedure(self: PCefWebPluginUnstableCallback; const path: PCefString; unstable: cint); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
	end;

	//............................................................................cef_xml_reader_capi
	// Structure that supports the reading of XML data via the libxml streaming API.
	// The functions of this structure should only be called on the thread that
	// creates the object
	TCefXmlReader = record
		// Base structure.
		base: TCefBase;
		// Moves the cursor to the next node in the document. This function must be
		// called at least once to set the current cursor position. Returns true (1)
		// if the cursor position was set successfully.
		move_to_next_node: function(self: PCefXmlReader): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Close the document. This should be called directly to ensure that cleanup
		// occurs on the correct thread.
		close: function(self: PCefXmlReader): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns true (1) if an error has been reported by the XML parser.
		has_error: function(self: PCefXmlReader): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the error string.
		// The resulting string must be freed by calling cef_string_userfree_free().
		get_error: function(self: PCefXmlReader): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// The below functions retrieve data for the node at the current cursor
		// position.
		// Returns the node type.
		get_type: function(self: PCefXmlReader): TCefXmlNodeType; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the node depth. Depth starts at 0 for the root node.
		get_depth: function(self: PCefXmlReader): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the local name. See http://www.w3.org/TR/REC-xml-names/#NT-
		// LocalPart for additional details.
		// The resulting string must be freed by calling cef_string_userfree_free().
		get_local_name: function(self: PCefXmlReader): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the namespace prefix. See http://www.w3.org/TR/REC-xml-names/ for
		// additional details.
		// The resulting string must be freed by calling cef_string_userfree_free().
		get_prefix: function(self: PCefXmlReader): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the qualified name, equal to (Prefix:)LocalName. See
		// http://www.w3.org/TR/REC-xml-names/#ns-qualnames for additional details.
		// The resulting string must be freed by calling cef_string_userfree_free().
		get_qualified_name: function(self: PCefXmlReader): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the URI defining the namespace associated with the node. See
		// http://www.w3.org/TR/REC-xml-names/ for additional details.
		// The resulting string must be freed by calling cef_string_userfree_free().
		get_namespace_uri: function(self: PCefXmlReader): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the base URI of the node. See http://www.w3.org/TR/xmlbase/ for
		// additional details.
		// The resulting string must be freed by calling cef_string_userfree_free().
		get_base_uri: function(self: PCefXmlReader): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the xml:lang scope within which the node resides. See
		// http://www.w3.org/TR/REC-xml/#sec-lang-tag for additional details.
		// The resulting string must be freed by calling cef_string_userfree_free().
		get_xml_lang: function(self: PCefXmlReader): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns true (1) if the node represents an NULL element. <a/> is considered
		// NULL but <a></a> is not.
		is_empty_element: function(self: PCefXmlReader): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns true (1) if the node has a text value.
		has_value: function(self: PCefXmlReader): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the text value.
		// The resulting string must be freed by calling cef_string_userfree_free().
		get_value: function(self: PCefXmlReader): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns true (1) if the node has attributes.
		has_attributes: function(self: PCefXmlReader): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the number of attributes.
		get_attribute_count: function(self: PCefXmlReader): csize_t; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the value of the attribute at the specified 0-based index.
		// The resulting string must be freed by calling cef_string_userfree_free().
		get_attribute_byindex: function(self: PCefXmlReader; index: cint): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the value of the attribute with the specified qualified name.
		// The resulting string must be freed by calling cef_string_userfree_free().
		get_attribute_byqname: function(self: PCefXmlReader; const qualifiedName: PCefString): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the value of the attribute with the specified local name and
		// namespace URI.
		// The resulting string must be freed by calling cef_string_userfree_free().
		get_attribute_bylname: function(self: PCefXmlReader; const localName: PCefString; const namespaceURI: PCefString): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns an XML representation of the current node's children.
		// The resulting string must be freed by calling cef_string_userfree_free().
		get_inner_xml: function(self: PCefXmlReader): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns an XML representation of the current node including its children.
		// The resulting string must be freed by calling cef_string_userfree_free().
		get_outer_xml: function(self: PCefXmlReader): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the line number for the current node.
		get_line_number: function(self: PCefXmlReader): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Attribute nodes are not traversed by default. The below functions can be
		// used to move the cursor to an attribute node. move_to_carrying_element()
		// can be called afterwards to return the cursor to the carrying element. The
		// depth of an attribute node will be 1 + the depth of the carrying element.
		// Moves the cursor to the attribute at the specified 0-based index. Returns
		// true (1) if the cursor position was set successfully.
		move_to_attribute_byindex: function(self: PCefXmlReader; index: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Moves the cursor to the attribute with the specified qualified name.
		// Returns true (1) if the cursor position was set successfully.
		move_to_attribute_byqname: function(self: PCefXmlReader; const qualifiedName: PCefString): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Moves the cursor to the attribute with the specified local name and
		// namespace URI. Returns true (1) if the cursor position was set
		// successfully.
		move_to_attribute_bylname: function(self: PCefXmlReader; const localName: PCefString; const namespaceURI: PCefString): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Moves the cursor to the first attribute in the current element. Returns
		// true (1) if the cursor position was set successfully.
		move_to_first_attribute: function(self: PCefXmlReader): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Moves the cursor to the next attribute in the current element. Returns true
		// (1) if the cursor position was set successfully.
		move_to_next_attribute: function(self: PCefXmlReader): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Moves the cursor back to the carrying element. Returns true (1) if the
		// cursor position was set successfully.
		move_to_carrying_element: function(self: PCefXmlReader): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
	end;

	//............................................................................cef_zip_reader_capi
	// Structure that supports the reading of zip archives via the zlib unzip API.
	// The functions of this structure should only be called on the thread that
	// creates the object
	TCefZipReader = record
		// Base structure.
		base: TCefBase;
		// Moves the cursor to the first file in the archive. Returns true (1) if the
		// cursor position was set successfully.
		move_to_first_file: function(self: PCefZipReader): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Moves the cursor to the next file in the archive. Returns true (1) if the
		// cursor position was set successfully.
		move_to_next_file: function(self: PCefZipReader): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Moves the cursor to the specified file in the archive. If |caseSensitive|
		// is true (1) then the search will be case sensitive. Returns true (1) if the
		// cursor position was set successfully.
		move_to_file: function(self: PCefZipReader; const fileName: PCefString; caseSensitive: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Closes the archive. This should be called directly to ensure that cleanup
		// occurs on the correct thread.
		close: function(self: PCefZipReader): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// The below functions act on the file at the current cursor position.
		// Returns the name of the file.
		// The resulting string must be freed by calling cef_string_userfree_free().
		get_file_name: function(self: PCefZipReader): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the uncompressed size of the file.
		get_file_size: function(self: PCefZipReader): cint64; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the last modified timestamp for the file.
		get_file_last_modified: function(self: PCefZipReader): time_t; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Opens the file for reading of uncompressed data. A read password may
		// optionally be specified.
		open_file: function(self: PCefZipReader; const password: PCefString): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Closes the file.
		close_file: function(self: PCefZipReader): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Read uncompressed file contents into the specified buffer. Returns < 0 if
		// an error occurred, 0 if at the end of file, or the number of bytes read.
		read_file: function(self: PCefZipReader; buffer: void; bufferSize: csize_t): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns the current offset in the uncompressed file contents.
		tell: function(self: PCefZipReader): cint64; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
		// Returns true (1) if at end of the file contents.
		eof: function(self: PCefZipReader): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
	end;

implementation

end.