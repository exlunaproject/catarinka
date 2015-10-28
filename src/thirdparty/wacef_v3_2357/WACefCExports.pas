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

unit WACefCExports;

interface

{$I WACef.inc}

uses
  {$IFDEF FPC}
  ctypes,
  {$ENDIF}
  WACefCApi,
  WACefTypes;

const
  {$IFDEF MSWINDOWS}
  CefLibrary = 'libcef.dll';
  {$ENDIF}
  {$IFDEF MACOS}
  CefLibrary = 'libcef.dylib';
  {$ENDIF}
  {$IFDEF LINUX}
  CefLibrary = 'libcef.so';
  {$ENDIF}

  {$IFNDEF FPC}
  //............................................................................cef_app_capi
	//This function should be called from the application entry point function to
	// execute a secondary process. It can be used to run secondary processes from
	// the browser client executable (default behavior) or from a separate
	// executable specified by the CefSettings.browser_subprocess_path value. If
	// called for the browser process (identified by no "type" command-line value)
	// it will return immediately with a value of -1. If called for a recognized
	// secondary process it will block until the process should exit and then return
	// the process exit code. The |application| parameter may be NULL. The
	// |windows_sandbox_info| parameter is only used on Windows and may be NULL (see
	// cef_sandbox_win.h for details
	function cef_execute_process(const args: PCefMainArgs; application: PCefApp; windows_sandbox_info: void): cint; cdecl; external CefLibrary {$IFNDEF FPC}delayed{$ENDIF} {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
	//This function should be called on the main application thread to initialize
	// the CEF browser process. The |application| parameter may be NULL. A return
	// value of true (1) indicates that it succeeded and false (0) indicates that it
	// failed. The |windows_sandbox_info| parameter is only used on Windows and may
	// be NULL (see cef_sandbox_win.h for details
	function cef_initialize(const args: PCefMainArgs; const settings: PCefSettings; application: PCefApp; windows_sandbox_info: void): cint; cdecl; external CefLibrary {$IFNDEF FPC}delayed{$ENDIF} {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
	//This function should be called on the main application thread to shut down
	// the CEF browser process before the application exits
	procedure cef_shutdown; cdecl; external CefLibrary {$IFNDEF FPC}delayed{$ENDIF} {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
	//Perform a single iteration of CEF message loop processing. This function is
	// used to integrate the CEF message loop into an existing application message
	// loop. Care must be taken to balance performance against excessive CPU usage.
	// This function should only be called on the main application thread and only
	// if cef_initialize() is called with a CefSettings.multi_threaded_message_loop
	// value of false (0). This function will not block
	procedure cef_do_message_loop_work; cdecl; external CefLibrary {$IFNDEF FPC}delayed{$ENDIF} {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
	//Run the CEF message loop. Use this function instead of an application-
	// provided message loop to get the best balance between performance and CPU
	// usage. This function should only be called on the main application thread and
	// only if cef_initialize() is called with a
	// CefSettings.multi_threaded_message_loop value of false (0). This function
	// will block until a quit message is received by the system
	procedure cef_run_message_loop; cdecl; external CefLibrary {$IFNDEF FPC}delayed{$ENDIF} {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
	//Quit the CEF message loop that was started by calling cef_run_message_loop().
	// This function should only be called on the main application thread and only
	// if cef_run_message_loop() was used
	procedure cef_quit_message_loop; cdecl; external CefLibrary {$IFNDEF FPC}delayed{$ENDIF} {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
	//Set to true (1) before calling Windows APIs like TrackPopupMenu that enter a
	// modal message loop. Set to false (0) after exiting the modal message loop
	procedure cef_set_osmodal_loop(osModalLoop: cint); cdecl; external CefLibrary {$IFNDEF FPC}delayed{$ENDIF} {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
	//............................................................................cef_browser_capi
	//Create a new browser window using the window parameters specified by
	// |windowInfo|. All values will be copied internally and the actual window will
	// be created on the UI thread. If |request_context| is NULL the global request
	// context will be used. This function can be called on any browser process
	// thread and will not block
	function cef_browser_host_create_browser(const windowInfo: PCefWindowInfo; client: PCefClient; const url: PCefString; const settings: PCefBrowserSettings; request_context: PCefRequestContext): cint; cdecl; external CefLibrary {$IFNDEF FPC}delayed{$ENDIF} {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
	//Create a new browser window using the window parameters specified by
	// |windowInfo|. If |request_context| is NULL the global request context will be
	// used. This function can only be called on the browser process UI thread
	function cef_browser_host_create_browser_sync(const windowInfo: PCefWindowInfo; client: PCefClient; const url: PCefString; const settings: PCefBrowserSettings; request_context: PCefRequestContext): PCefBrowser; cdecl; external CefLibrary {$IFNDEF FPC}delayed{$ENDIF} {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
	//............................................................................cef_command_line_capi
	//Create a new cef_command_line_t instance
	function cef_command_line_create: PCefCommandLine; cdecl; external CefLibrary {$IFNDEF FPC}delayed{$ENDIF} {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
	//Returns the singleton global cef_command_line_t object. The returned object
	// will be read-only
	function cef_command_line_get_global: PCefCommandLine; cdecl; external CefLibrary {$IFNDEF FPC}delayed{$ENDIF} {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
	//............................................................................cef_cookie_capi
	//Returns the global cookie manager. By default data will be stored at
	// CefSettings.cache_path if specified or in memory otherwise. If |callback| is
	// non-NULL it will be executed asnychronously on the IO thread after the
	// manager's storage has been initialized. Using this function is equivalent to
	// calling cef_request_tContext::cef_request_context_get_global_context()->get_d
	// efault_cookie_manager
	function cef_cookie_manager_get_global_manager(callback: PCefCompletionCallback): PCefCookieManager; cdecl; external CefLibrary {$IFNDEF FPC}delayed{$ENDIF} {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
	//Creates a new cookie manager. If |path| is NULL data will be stored in memory
	// only. Otherwise, data will be stored at the specified |path|. To persist
	// session cookies (cookies without an expiry date or validity interval) set
	// |persist_session_cookies| to true (1). Session cookies are generally intended
	// to be transient and most Web browsers do not persist them. If |callback| is
	// non-NULL it will be executed asnychronously on the IO thread after the
	// manager's storage has been initialized
	function cef_cookie_manager_create_manager(const path: PCefString; persist_session_cookies: cint; callback: PCefCompletionCallback): PCefCookieManager; cdecl; external CefLibrary {$IFNDEF FPC}delayed{$ENDIF} {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
	//............................................................................cef_drag_data_capi
	//Create a new cef_drag_data_t object
	function cef_drag_data_create: PCefDragData; cdecl; external CefLibrary {$IFNDEF FPC}delayed{$ENDIF} {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
	//............................................................................cef_geolocation_capi
	//Request a one-time geolocation update. This function bypasses any user
	// permission checks so should only be used by code that is allowed to access
	// location information
	function cef_get_geolocation(callback: PCefGetGeolocationCallback): cint; cdecl; external CefLibrary {$IFNDEF FPC}delayed{$ENDIF} {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
	//............................................................................cef_origin_whitelist_capi
	//Add an entry to the cross-origin access whitelist.
	//
	// The same-origin policy restricts how scripts hosted from different origins
	// (scheme + domain + port) can communicate. By default, scripts can only access
	// resources with the same origin. Scripts hosted on the HTTP and HTTPS schemes
	// (but no other schemes) can use the "Access-Control-Allow-Origin" header to
	// allow cross-origin requests. For example, https://source.example.com can make
	// XMLHttpRequest requests on http://target.example.com if the
	// http://target.example.com request returns an "Access-Control-Allow-Origin:
	// https://source.example.com" response header.
	//
	// Scripts in separate frames or iframes and hosted from the same protocol and
	// domain suffix can execute cross-origin JavaScript if both pages set the
	// document.domain value to the same domain suffix. For example,
	// scheme://foo.example.com and scheme://bar.example.com can communicate using
	// JavaScript if both domains set document.domain="example.com".
	//
	// This function is used to allow access to origins that would otherwise violate
	// the same-origin policy. Scripts hosted underneath the fully qualified
	// |source_origin| URL (like http://www.example.com) will be allowed access to
	// all resources hosted on the specified |target_protocol| and |target_domain|.
	// If |target_domain| is non-NULL and |allow_target_subdomains| if false (0)
	// only exact domain matches will be allowed. If |target_domain| contains a top-
	// level domain component (like "example.com") and |allow_target_subdomains| is
	// true (1) sub-domain matches will be allowed. If |target_domain| is NULL and
	// |allow_target_subdomains| if true (1) all domains and IP addresses will be
	// allowed.
	//
	// This function cannot be used to bypass the restrictions on local or display
	// isolated schemes. See the comments on CefRegisterCustomScheme for more
	// information.
	//
	// This function may be called on any thread. Returns false (0) if
	// |source_origin| is invalid or the whitelist cannot be accessed
	function cef_add_cross_origin_whitelist_entry(const source_origin: PCefString; const target_protocol: PCefString; const target_domain: PCefString; allow_target_subdomains: cint): cint; cdecl; external CefLibrary {$IFNDEF FPC}delayed{$ENDIF} {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
	//Remove an entry from the cross-origin access whitelist. Returns false (0) if
	// |source_origin| is invalid or the whitelist cannot be accessed
	function cef_remove_cross_origin_whitelist_entry(const source_origin: PCefString; const target_protocol: PCefString; const target_domain: PCefString; allow_target_subdomains: cint): cint; cdecl; external CefLibrary {$IFNDEF FPC}delayed{$ENDIF} {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
	//Remove all entries from the cross-origin access whitelist. Returns false (0)
	// if the whitelist cannot be accessed
	function cef_clear_cross_origin_whitelist: cint; cdecl; external CefLibrary {$IFNDEF FPC}delayed{$ENDIF} {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  //............................................................................cef_parser_capi
	//Parse the specified |url| into its component parts. Returns false (0) if the
	// URL is NULL or invalid
	function cef_parse_url(const url: PCefString; parts: PCefUrlparts): cint; cdecl; external CefLibrary {$IFNDEF FPC}delayed{$ENDIF} {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
	//Creates a URL from the specified |parts|, which must contain a non-NULL spec
	// or a non-NULL host and path (at a minimum), but not both. Returns false (0)
	// if |parts| isn't initialized as described
	function cef_create_url(const parts: PCefUrlparts; url: PCefString): cint; cdecl; external CefLibrary {$IFNDEF FPC}delayed{$ENDIF} {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
	//Returns the mime type for the specified file extension or an NULL string if
	// unknown
	function cef_get_mime_type(const extension: PCefString): PCefStringUserFree; cdecl; external CefLibrary {$IFNDEF FPC}delayed{$ENDIF} {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
	//Get the extensions associated with the given mime type. This should be passed
	// in lower case. There could be multiple extensions for a given mime type, like
	// "html,htm" for "text/html", or "txt,text,html,..." for "text/*". Any existing
	// elements in the provided vector will not be erased
	procedure cef_get_extensions_for_mime_type(const mime_type: PCefString; extensions: TCefStringList); cdecl; external CefLibrary {$IFNDEF FPC}delayed{$ENDIF} {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
	//Encodes |data| as a base64 string
	function cef_base64encode(const data: void; data_size: csize_t): PCefStringUserFree; cdecl; external CefLibrary {$IFNDEF FPC}delayed{$ENDIF} {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
	//Decodes the base64 encoded string |data|. The returned value will be NULL if
	// the decoding fails
	function cef_base64decode(const data: PCefString): PCefBinaryValue; cdecl; external CefLibrary {$IFNDEF FPC}delayed{$ENDIF} {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
	//Escapes characters in |text| which are unsuitable for use as a query
	// parameter value. Everything except alphanumerics and -_.!~*'() will be
	// converted to "%XX". If |use_plus| is true (1) spaces will change to "+". The
	// result is basically the same as encodeURIComponent in Javacript
	function cef_uriencode(const text: PCefString; use_plus: cint): PCefStringUserFree; cdecl; external CefLibrary {$IFNDEF FPC}delayed{$ENDIF} {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
	//Unescapes |text| and returns the result. Unescaping consists of looking for
	// the exact pattern "%XX" where each X is a hex digit and converting to the
	// character with the numerical value of those digits (e.g. "i%20=%203%3b"
	// unescapes to "i = 3;"). If |convert_to_utf8| is true (1) this function will
	// attempt to interpret the initial decoded result as UTF-8. If the result is
	// convertable into UTF-8 it will be returned as converted. Otherwise the
	// initial decoded result will be returned.  The |unescape_rule| parameter
	// supports further customization the decoding process
	function cef_uridecode(const text: PCefString; convert_to_utf8: cint; unescape_rule: TCefUriUnescapeRule): PCefStringUserFree; cdecl; external CefLibrary {$IFNDEF FPC}delayed{$ENDIF} {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
	//Parses |string| which represents a CSS color value. If |strict| is true (1)
	// strict parsing rules will be applied. Returns true (1) on success or false
	// (0) on error. If parsing succeeds |color| will be set to the color value
	// otherwise |color| will remain unchanged
	function cef_parse_csscolor(const _string: PCefString; strict: cint; color: PCefColor): cint; cdecl; external CefLibrary {$IFNDEF FPC}delayed{$ENDIF} {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
	//............................................................................cef_path_util_capi
	//Retrieve the path associated with the specified |key|. Returns true (1) on
	// success. Can be called on any thread in the browser process
	function cef_get_path(key: TCefPathKey; path: PCefString): cint; cdecl; external CefLibrary {$IFNDEF FPC}delayed{$ENDIF} {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
	//............................................................................cef_print_settings_capi
	//Create a new cef_print_settings_t object
	function cef_print_settings_create: PCefPrintSettings; cdecl; external CefLibrary {$IFNDEF FPC}delayed{$ENDIF} {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
	//............................................................................cef_process_message_capi
	//Create a new cef_process_message_t object with the specified name
	function cef_process_message_create(const name: PCefString): PCefProcessMessage; cdecl; external CefLibrary {$IFNDEF FPC}delayed{$ENDIF} {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
	//............................................................................cef_process_util_capi
	//Launches the process specified via |command_line|. Returns true (1) upon
	// success. Must be called on the browser process TID_PROCESS_LAUNCHER thread.
	//
	// Unix-specific notes: - All file descriptors open in the parent process will
	// be closed in the
	//   child process except for stdin, stdout, and stderr.
	// - If the first argument on the command line does not contain a slash,
	//   PATH will be searched. (See man execvp
	function cef_launch_process(command_line: PCefCommandLine): cint; cdecl; external CefLibrary {$IFNDEF FPC}delayed{$ENDIF} {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
	//............................................................................cef_request_capi
	//Create a new cef_request_t object
	function cef_request_create: PCefRequest; cdecl; external CefLibrary {$IFNDEF FPC}delayed{$ENDIF} {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
	//Create a new cef_post_data_t object
	function cef_post_data_create: PCefPostData; cdecl; external CefLibrary {$IFNDEF FPC}delayed{$ENDIF} {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
	//Create a new cef_post_data_element_t object
	function cef_post_data_element_create: PCefPostDataElement; cdecl; external CefLibrary {$IFNDEF FPC}delayed{$ENDIF} {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
	//............................................................................cef_request_context_capi
	//Returns the global context object
	function cef_request_context_get_global_context: PCefRequestContext; cdecl; external CefLibrary {$IFNDEF FPC}delayed{$ENDIF} {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
	//Creates a new context object with the specified |settings| and optional
	// |handler
	function cef_request_context_create_context(const settings: PCefRequestContextSettings; handler: PCefRequestContextHandler): PCefRequestContext; cdecl; external CefLibrary {$IFNDEF FPC}delayed{$ENDIF} {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
	//Creates a new context object that shares storage with |other| and uses an
	// optional |handler
	function create_context_shared(other: PCefRequestContext; handler: PCefRequestContextHandler): PCefRequestContext; cdecl; external CefLibrary {$IFNDEF FPC}delayed{$ENDIF} {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
	//............................................................................cef_response_capi
	//Create a new cef_response_t object
	function cef_response_create: PCefResponse; cdecl; external CefLibrary {$IFNDEF FPC}delayed{$ENDIF} {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
	//............................................................................cef_scheme_capi
	//Register a scheme handler factory with the global request context. An NULL
	// |domain_name| value for a standard scheme will cause the factory to match all
	// domain names. The |domain_name| value will be ignored for non-standard
	// schemes. If |scheme_name| is a built-in scheme and no handler is returned by
	// |factory| then the built-in scheme handler factory will be called. If
	// |scheme_name| is a custom scheme then you must also implement the
	// cef_app_t::on_register_custom_schemes() function in all processes. This
	// function may be called multiple times to change or remove the factory that
	// matches the specified |scheme_name| and optional |domain_name|. Returns false
	// (0) if an error occurs. This function may be called on any thread in the
	// browser process. Using this function is equivalent to calling cef_request_tCo
	// ntext::cef_request_context_get_global_context()->register_scheme_handler_fact
	// ory
	function cef_register_scheme_handler_factory(const scheme_name: PCefString; const domain_name: PCefString; factory: PCefSchemeHandlerFactory): cint; cdecl; external CefLibrary {$IFNDEF FPC}delayed{$ENDIF} {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
	//Clear all scheme handler factories registered with the global request
	// context. Returns false (0) on error. This function may be called on any
	// thread in the browser process. Using this function is equivalent to calling c
	// ef_request_tContext::cef_request_context_get_global_context()->clear_scheme_h
	// andler_factories
	function cef_clear_scheme_handler_factories: cint; cdecl; external CefLibrary {$IFNDEF FPC}delayed{$ENDIF} {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
	//............................................................................cef_stream_capi
	//Create a new cef_stream_reader_t object from a file
	function cef_stream_reader_create_for_file(const fileName: PCefString): PCefStreamReader; cdecl; external CefLibrary {$IFNDEF FPC}delayed{$ENDIF} {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
	//Create a new cef_stream_reader_t object from data
	function cef_stream_reader_create_for_data(data: void; size: csize_t): PCefStreamReader; cdecl; external CefLibrary {$IFNDEF FPC}delayed{$ENDIF} {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
	//Create a new cef_stream_reader_t object from a custom handler
	function cef_stream_reader_create_for_handler(handler: PCefReadHandler): PCefStreamReader; cdecl; external CefLibrary {$IFNDEF FPC}delayed{$ENDIF} {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
	//Create a new cef_stream_writer_t object for a file
	function cef_stream_writer_create_for_file(const fileName: PCefString): PCefStreamWriter; cdecl; external CefLibrary {$IFNDEF FPC}delayed{$ENDIF} {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
	//Create a new cef_stream_writer_t object for a custom handler
	function cef_stream_writer_create_for_handler(handler: PCefWriteHandler): PCefStreamWriter; cdecl; external CefLibrary {$IFNDEF FPC}delayed{$ENDIF} {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
	//............................................................................cef_task_capi
	//Returns the task runner for the current thread. Only CEF threads will have
	// task runners. An NULL reference will be returned if this function is called
	// on an invalid thread
	function cef_task_runner_get_for_current_thread: PCefTaskRunner; cdecl; external CefLibrary {$IFNDEF FPC}delayed{$ENDIF} {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
	//Returns the task runner for the specified CEF thread
	function cef_task_runner_get_for_thread(threadId: TCefThreadId): PCefTaskRunner; cdecl; external CefLibrary {$IFNDEF FPC}delayed{$ENDIF} {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
	//Returns true (1) if called on the specified thread. Equivalent to using
	// cef_task_tRunner::GetForThread(threadId)->belongs_to_current_thread
	function cef_currently_on(threadId: TCefThreadId): cint; cdecl; external CefLibrary {$IFNDEF FPC}delayed{$ENDIF} {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
	//Post a task for execution on the specified thread. Equivalent to using
	// cef_task_tRunner::GetForThread(threadId)->PostTask(task
	function cef_post_task(threadId: TCefThreadId; task: PCefTask): cint; cdecl; external CefLibrary {$IFNDEF FPC}delayed{$ENDIF} {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
	//Post a task for delayed execution on the specified thread. Equivalent to
	// using cef_task_tRunner::GetForThread(threadId)->PostDelayedTask(task,
	// delay_ms
	function cef_post_delayed_task(threadId: TCefThreadId; task: PCefTask; delay_ms: cint64): cint; cdecl; external CefLibrary {$IFNDEF FPC}delayed{$ENDIF} {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
	//............................................................................cef_trace_capi
	//Start tracing events on all processes. Tracing is initialized asynchronously
	// and |callback| will be executed on the UI thread after initialization is
	// complete.
	//
	// If CefBeginTracing was called previously, or if a CefEndTracingAsync call is
	// pending, CefBeginTracing will fail and return false (0).
	//
	// |categories| is a comma-delimited list of category wildcards. A category can
	// have an optional '-' prefix to make it an excluded category. Having both
	// included and excluded categories in the same list is not supported.
	//
	// Example: "test_MyTest*" Example: "test_MyTest*,test_OtherStuff" Example:
	// "-excluded_category1,-excluded_category2"
	//
	// This function must be called on the browser process UI thread
	function cef_begin_tracing(const categories: PCefString; callback: PCefCompletionCallback): cint; cdecl; external CefLibrary {$IFNDEF FPC}delayed{$ENDIF} {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
	//Stop tracing events on all processes.
	//
	// This function will fail and return false (0) if a previous call to
	// CefEndTracingAsync is already pending or if CefBeginTracing was not called.
	//
	// |tracing_file| is the path at which tracing data will be written and
	// |callback| is the callback that will be executed once all processes have sent
	// their trace data. If |tracing_file| is NULL a new temporary file path will be
	// used. If |callback| is NULL no trace data will be written.
	//
	// This function must be called on the browser process UI thread
	function cef_end_tracing(const tracing_file: PCefString; callback: PCefEndTracingCallback): cint; cdecl; external CefLibrary {$IFNDEF FPC}delayed{$ENDIF} {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
	//Returns the current system trace time or, if none is defined, the current
	// high-res time. Can be used by clients to synchronize with the time
	// information in trace events
	function cef_now_from_system_trace_time: cint64; cdecl; external CefLibrary {$IFNDEF FPC}delayed{$ENDIF} {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
	//............................................................................cef_urlrequest_capi
	//Create a new URL request. Only GET, POST, HEAD, DELETE and PUT request
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
	// renderer process' browser will be used
	function cef_urlrequest_create(request: PCefRequest; client: PCefUrlrequestClient; request_context: PCefRequestContext): PCefUrlrequest; cdecl; external CefLibrary {$IFNDEF FPC}delayed{$ENDIF} {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
	//............................................................................cef_v8_capi
	//Returns the current (top) context object in the V8 context stack
	function cef_v8context_get_current_context: PCefV8context; cdecl; external CefLibrary {$IFNDEF FPC}delayed{$ENDIF} {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
	//Returns the entered (bottom) context object in the V8 context stack
	function cef_v8context_get_entered_context: PCefV8context; cdecl; external CefLibrary {$IFNDEF FPC}delayed{$ENDIF} {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
	//Returns true (1) if V8 is currently inside a context
	function cef_v8context_in_context: cint; cdecl; external CefLibrary {$IFNDEF FPC}delayed{$ENDIF} {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
	//Create a new cef_v8value_t object of type undefined
	function cef_v8value_create_undefined: PCefV8value; cdecl; external CefLibrary {$IFNDEF FPC}delayed{$ENDIF} {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
	//Create a new cef_v8value_t object of type null
	function cef_v8value_create_null: PCefV8value; cdecl; external CefLibrary {$IFNDEF FPC}delayed{$ENDIF} {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
	//Create a new cef_v8value_t object of type bool
	function cef_v8value_create_bool(value: cint): PCefV8value; cdecl; external CefLibrary {$IFNDEF FPC}delayed{$ENDIF} {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
	//Create a new cef_v8value_t object of type int
	function cef_v8value_create_int(value: cint32): PCefV8value; cdecl; external CefLibrary {$IFNDEF FPC}delayed{$ENDIF} {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
	//Create a new cef_v8value_t object of type unsigned int
	function cef_v8value_create_uint(value: cuint32): PCefV8value; cdecl; external CefLibrary {$IFNDEF FPC}delayed{$ENDIF} {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
	//Create a new cef_v8value_t object of type double
	function cef_v8value_create_double(value: cdouble): PCefV8value; cdecl; external CefLibrary {$IFNDEF FPC}delayed{$ENDIF} {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
	//Create a new cef_v8value_t object of type Date. This function should only be
	// called from within the scope of a cef_render_process_handler_t,
	// cef_v8handler_t or cef_v8accessor_t callback, or in combination with calling
	// enter() and exit() on a stored cef_v8context_t reference
	function cef_v8value_create_date(const date: PCefTime): PCefV8value; cdecl; external CefLibrary {$IFNDEF FPC}delayed{$ENDIF} {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
	//Create a new cef_v8value_t object of type string
	function cef_v8value_create_string(const value: PCefString): PCefV8value; cdecl; external CefLibrary {$IFNDEF FPC}delayed{$ENDIF} {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
	//Create a new cef_v8value_t object of type object with optional accessor. This
	// function should only be called from within the scope of a
	// cef_render_process_handler_t, cef_v8handler_t or cef_v8accessor_t callback,
	// or in combination with calling enter() and exit() on a stored cef_v8context_t
	// reference
	function cef_v8value_create_object(accessor: PCefV8accessor): PCefV8value; cdecl; external CefLibrary {$IFNDEF FPC}delayed{$ENDIF} {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
	//Create a new cef_v8value_t object of type array with the specified |length|.
	// If |length| is negative the returned array will have length 0. This function
	// should only be called from within the scope of a
	// cef_render_process_handler_t, cef_v8handler_t or cef_v8accessor_t callback,
	// or in combination with calling enter() and exit() on a stored cef_v8context_t
	// reference
	function cef_v8value_create_array(length: cint): PCefV8value; cdecl; external CefLibrary {$IFNDEF FPC}delayed{$ENDIF} {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
	//Create a new cef_v8value_t object of type function. This function should only
	// be called from within the scope of a cef_render_process_handler_t,
	// cef_v8handler_t or cef_v8accessor_t callback, or in combination with calling
	// enter() and exit() on a stored cef_v8context_t reference
	function cef_v8value_create_function(const name: PCefString; handler: PCefV8handler): PCefV8value; cdecl; external CefLibrary {$IFNDEF FPC}delayed{$ENDIF} {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
	//Returns the stack trace for the currently active context. |frame_limit| is
	// the maximum number of frames that will be captured
	function cef_v8stack_trace_get_current(frame_limit: cint): PCefV8stackTrace; cdecl; external CefLibrary {$IFNDEF FPC}delayed{$ENDIF} {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
	//Register a new V8 extension with the specified JavaScript extension code and
	// handler. Functions implemented by the handler are prototyped using the
	// keyword 'native'. The calling of a native function is restricted to the scope
	// in which the prototype of the native function is defined. This function may
	// only be called on the render process main thread.
	//
	// Example JavaScript extension code: <pre>
	//   // create the 'example' global object if it doesn't already exist.
	//   if (!example)
	//     example = {};
	//   // create the 'example.test' global object if it doesn't already exist.
	//   if (!example.test)
	//     example.test = {};
	//   (function() {
	//     // Define the function 'example.test.myfunction'.
	//     example.test.myfunction = function() {
	//       // Call CefV8Handler::Execute() with the function name 'MyFunction'
	//       // and no arguments.
	//       native function MyFunction();
	//       return MyFunction();
	//     };
	//     // Define the getter function for parameter 'example.test.myparam'.
	//     example.test.__defineGetter__('myparam', function() {
	//       // Call CefV8Handler::Execute() with the function name 'GetMyParam'
	//       // and no arguments.
	//       native function GetMyParam();
	//       return GetMyParam();
	//     });
	//     // Define the setter function for parameter 'example.test.myparam'.
	//     example.test.__defineSetter__('myparam', function(b) {
	//       // Call CefV8Handler::Execute() with the function name 'SetMyParam'
	//       // and a single argument.
	//       native function SetMyParam();
	//       if(b) SetMyParam(b);
	//     });
	//
	//     // Extension definitions can also contain normal JavaScript variables
	//     // and functions.
	//     var myint = 0;
	//     example.test.increment = function() {
	//       myint += 1;
	//       return myint;
	//     };
	//   })();
	// </pre> Example usage in the page: <pre>
	//   // Call the function.
	//   example.test.myfunction();
	//   // Set the parameter.
	//   example.test.myparam = value;
	//   // Get the parameter.
	//   value = example.test.myparam;
	//   // Call another function.
	//   example.test.increment();
	// </pre
	function cef_register_extension(const extension_name: PCefString; const javascript_code: PCefString; handler: PCefV8handler): cint; cdecl; external CefLibrary {$IFNDEF FPC}delayed{$ENDIF} {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
	//............................................................................cef_values_capi
	//Creates a new object
	function cef_value_create: PCefValue; cdecl; external CefLibrary {$IFNDEF FPC}delayed{$ENDIF} {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
	//Creates a new object that is not owned by any other object. The specified
	// |data| will be copied
	function cef_binary_value_create(const data: void; data_size: csize_t): PCefBinaryValue; cdecl; external CefLibrary {$IFNDEF FPC}delayed{$ENDIF} {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
	//Creates a new object that is not owned by any other object
	function cef_dictionary_value_create: PCefDictionaryValue; cdecl; external CefLibrary {$IFNDEF FPC}delayed{$ENDIF} {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
	//Creates a new object that is not owned by any other object
	function cef_list_value_create: PCefListValue; cdecl; external CefLibrary {$IFNDEF FPC}delayed{$ENDIF} {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
	//............................................................................cef_web_plugin_capi
	//Visit web plugin information. Can be called on any thread in the browser
	// process
	procedure cef_visit_web_plugin_info(visitor: PCefWebPluginInfoVisitor); cdecl; external CefLibrary {$IFNDEF FPC}delayed{$ENDIF} {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
	//Cause the plugin list to refresh the next time it is accessed regardless of
	// whether it has already been loaded. Can be called on any thread in the
	// browser process
	procedure cef_refresh_web_plugins; cdecl; external CefLibrary {$IFNDEF FPC}delayed{$ENDIF} {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
	//Add a plugin path (directory + file). This change may not take affect until
	// after cef_refresh_web_plugins() is called. Can be called on any thread in the
	// browser process
	procedure cef_add_web_plugin_path(const path: PCefString); cdecl; external CefLibrary {$IFNDEF FPC}delayed{$ENDIF} {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
	//Add a plugin directory. This change may not take affect until after
	// cef_refresh_web_plugins() is called. Can be called on any thread in the
	// browser process
	procedure cef_add_web_plugin_directory(const dir: PCefString); cdecl; external CefLibrary {$IFNDEF FPC}delayed{$ENDIF} {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
	//Remove a plugin path (directory + file). This change may not take affect
	// until after cef_refresh_web_plugins() is called. Can be called on any thread
	// in the browser process
	procedure cef_remove_web_plugin_path(const path: PCefString); cdecl; external CefLibrary {$IFNDEF FPC}delayed{$ENDIF} {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
	//Unregister an internal plugin. This may be undone the next time
	// cef_refresh_web_plugins() is called. Can be called on any thread in the
	// browser process
	procedure cef_unregister_internal_web_plugin(const path: PCefString); cdecl; external CefLibrary {$IFNDEF FPC}delayed{$ENDIF} {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
	//Force a plugin to shutdown. Can be called on any thread in the browser
	// process but will be executed on the IO thread
	procedure cef_force_web_plugin_shutdown(const path: PCefString); cdecl; external CefLibrary {$IFNDEF FPC}delayed{$ENDIF} {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
	//Register a plugin crash. Can be called on any thread in the browser process
	// but will be executed on the IO thread
	procedure cef_register_web_plugin_crash(const path: PCefString); cdecl; external CefLibrary {$IFNDEF FPC}delayed{$ENDIF} {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
	//Query if a plugin is unstable. Can be called on any thread in the browser
	// process
	procedure cef_is_web_plugin_unstable(const path: PCefString; callback: PCefWebPluginUnstableCallback); cdecl; external CefLibrary {$IFNDEF FPC}delayed{$ENDIF} {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
	//............................................................................cef_xml_reader_capi
	//Create a new cef_xml_reader_t object. The returned object's functions can
	// only be called from the thread that created the object
	function cef_xml_reader_create(stream: PCefStreamReader; encodingType: TCefXmlEncodingType; const URI: PCefString): PCefXmlReader; cdecl; external CefLibrary {$IFNDEF FPC}delayed{$ENDIF} {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
	//............................................................................cef_zip_reader_capi
	//Create a new cef_zip_reader_t object. The returned object's functions can
	// only be called from the thread that created the object
	function cef_zip_reader_create(stream: PCefStreamReader): PCefZipReader; cdecl; external CefLibrary {$IFNDEF FPC}delayed{$ENDIF} {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};


  {----------------------------CefStringMap.h----------------------------------}
  // Allocate a new string map.
  function cef_string_map_alloc: TCefStringMap; cdecl; external CefLibrary delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  //function cef_string_map_size(map: TCefStringMap): Integer; cdecl; external CefLibrary delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  function cef_string_map_size(map: TCefStringMap): Integer; cdecl; external CefLibrary delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  // Return the value assigned to the specified key.
  function cef_string_map_find(map: TCefStringMap; const key: PCefString; var value: PCefString): Integer; cdecl; external CefLibrary delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  // Return the key at the specified zero-based string map index.
  function cef_string_map_key(map: TCefStringMap; index: Integer; var key: TCefString): Integer; cdecl; external CefLibrary delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  // Return the value at the specified zero-based string map index.
  function cef_string_map_value(map: TCefStringMap; index: Integer; var value: TCefString): Integer; cdecl; external CefLibrary delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  // Append a new key/value pair at the end of the string map.
  function cef_string_map_append(map: TCefStringMap; const key, value: PCefString): Integer; cdecl; external CefLibrary delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  // Clear the string map.
  procedure cef_string_map_clear(map: TCefStringMap); cdecl; external CefLibrary delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  // Free the string map.
  procedure cef_string_map_free(map: TCefStringMap); cdecl; external CefLibrary delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};

  {----------------------------CefStringList.h---------------------------------}
  // Allocate a new string map.
  function cef_string_list_alloc: TCefStringList; cdecl; external CefLibrary delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  // Return the number of elements in the string list.
  function cef_string_list_size(list: TCefStringList): Integer; cdecl; external CefLibrary delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  // Retrieve the value at the specified zero-based string list index. Returns
  // true (1) if the value was successfully retrieved.
  function cef_string_list_value(list: TCefStringList; index: Integer; value: PCefString): Integer; cdecl; external CefLibrary delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  // Append a new value at the end of the string list.
  procedure cef_string_list_append(list: TCefStringList; const value: PCefString); cdecl; external CefLibrary delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  // Clear the string list.
  procedure cef_string_list_clear(list: TCefStringList); cdecl; external CefLibrary delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  // Free the string list.
  procedure cef_string_list_free(list: TCefStringList); cdecl; external CefLibrary delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  // Creates a copy of an existing string list.
  function cef_string_list_copy(list: TCefStringList): TCefStringList; cdecl; external CefLibrary delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};

  {----------------------------CefStringMultiMap.h-----------------------------}
  // Allocate a new string multimap.
  function cef_string_multimap_alloc: TCefStringMultimap; cdecl; external CefLibrary delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  // Return the number of elements in the string multimap.
  function cef_string_multimap_size(map: TCefStringMultimap): Integer; cdecl; external CefLibrary delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  // Return the number of values with the specified key.
  function cef_string_multimap_find_count(map: TCefStringMultimap; const key: PCefString): Integer; cdecl; external CefLibrary delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  // Return the value_index-th value with the specified key.
  function cef_string_multimap_enumerate(map: TCefStringMultimap;
    const key: PCefString; value_index: Integer; var value: TCefString): Integer; cdecl; external CefLibrary delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  // Return the key at the specified zero-based string multimap index.
  function cef_string_multimap_key(map: TCefStringMultimap; index: Integer; var key: TCefString): Integer; cdecl; external CefLibrary delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  // Return the value at the specified zero-based string multimap index.
  function cef_string_multimap_value(map: TCefStringMultimap; index: Integer; var value: TCefString): Integer; cdecl; external CefLibrary delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  // Append a new key/value pair at the end of the string multimap.
  function cef_string_multimap_append(map: TCefStringMultimap; const key, value: PCefString): Integer; cdecl; external CefLibrary delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  // Clear the string multimap.
  procedure cef_string_multimap_clear(map: TCefStringMultimap); cdecl; external CefLibrary delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  // Free the string multimap.
  procedure cef_string_multimap_free(map: TCefStringMultimap); cdecl; external CefLibrary delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  function cef_build_revision: Integer; cdecl; external CefLibrary delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};

  {----------------------------Ceftime.h---------------------------------------}
  // Converts cef_time_t to/from time_t. Returns true (1) on success and false (0)
  // on failure.
  function cef_time_to_timet(const cef_time:PCefTime; time: pcint64):integer; cdecl; external CefLibrary delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  function cef_time_from_timet(time:Int64; cef_time:TCefTime):integer; cdecl; external CefLibrary delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  // Converts cef_time_t to/from a double which is the number of seconds since
  // epoch (Jan 1, 1970). Webkit uses this format to represent time. A value of 0
  // means "not initialized". Returns true (1) on success and false (0) on
  // failure.
  function cef_time_to_doublet(const cef_time:PCefTime; time:PDouble):integer; cdecl; external CefLibrary delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  function cef_time_from_doublet(time:Double; cef_time:PCefTime):integer; cdecl; external CefLibrary delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  // Retrieve the current system time.
  function cef_time_now(cef_time:PCefTime):integer; cdecl; external CefLibrary delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  // Retrieve the delta in milliseconds between two time values.
  function cef_time_delta(const cef_time1:PCefTime; const cef_time2:PCeftime; delta:UInt64):integer; cdecl; external CefLibrary delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};


  // These functions set string values. If |copy| is true (1) the value will be
  // copied instead of referenced. It is up to the user to properly manage
  // the lifespan of references.
  function cef_string_wide_set(const src: PWideChar; src_len: Cardinal;  output: PCefStringWide; copy: Integer): Integer; cdecl; external CefLibrary delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  function cef_string_utf8_set(const src: PAnsiChar; src_len: Cardinal; output: PCefStringUtf8; copy: Integer): Integer; cdecl; external CefLibrary delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  function cef_string_utf16_set(const src: PChar16; src_len: Cardinal; output: PCefStringUtf16; copy: Integer): Integer; cdecl; external CefLibrary delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};

  {$IFDEF CEF_STRING_TYPE_UTF8}
  function cef_string_set(const src: PCefChar; src_len: Cardinal; output: PCefString; copy: Integer): Integer; cdecl; external CefLibrary name 'cef_string_utf8_set' delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  procedure cef_string_clear(str: PCefString); cdecl; external CefLibrary name 'cef_string_utf8_clear' delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  function cef_string_userfree_alloc(): PCefStringUserFree; cdecl; external CefLibrary name 'cef_string_userfree_utf8_alloc' delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  procedure cef_string_userfree_free(str: PCefStringUserFree); cdecl; external CefLibrary name 'cef_string_userfree_utf8_free' delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  function cef_string_from_ascii(const src: PAnsiChar; src_len: Cardinal; output: PCefString): Integer; cdecl; external CefLibrary name 'cef_string_ascii_to_utf8' delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  function cef_string_to_utf8(const src: PCefChar; src_len: Cardinal; output: PCefStringUtf8): Integer; cdecl; external CefLibrary name 'cef_string_utf8_copy' delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  function cef_string_from_utf8(const src: PAnsiChar; src_len: Cardinal; output: PCefString): Integer; cdecl; external CefLibrary name 'cef_string_utf8_copy' delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  function cef_string_to_utf16(const src: PCefChar; src_len: Cardinal; output: PCefStringUtf16): Integer; cdecl; external CefLibrary name 'cef_string_utf8_to_utf16' delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  function cef_string_from_utf16(const src: PChar16; src_len: Cardinal; output: PCefString): Integer; cdecl; external CefLibrary name 'cef_string_utf16_to_utf8' delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  function cef_string_to_wide(const src: PCefChar; src_len: Cardinal; output: PCefStringWide): Integer; cdecl; external CefLibrary name 'cef_string_utf8_to_wide' delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  function cef_string_from_wide(const src: PWideChar; src_len: Cardinal; output: PCefString): Integer; cdecl; external CefLibrary name 'cef_string_wide_to_utf8' delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  {$ENDIF}
  {$IFDEF CEF_STRING_TYPE_UTF16}
  function cef_string_set(const src: PCefChar; src_len: Cardinal; output: PCefString; copy: Integer): Integer; cdecl; external CefLibrary name 'cef_string_utf16_set' delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  procedure cef_string_clear(str: PCefString); cdecl; external CefLibrary name 'cef_string_utf16_clear' delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  function cef_string_userfree_alloc: PCefStringUserFree; cdecl; external CefLibrary name 'cef_string_userfree_utf16_alloc' delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  procedure cef_string_userfree_free(str: PCefStringUserFree); cdecl; external CefLibrary name 'cef_string_userfree_utf16_free' delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  function cef_string_from_ascii(const src: PAnsiChar; src_len: Cardinal; output: PCefString): Integer; cdecl; external CefLibrary name 'cef_string_ascii_to_utf16' delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  function cef_string_to_utf8(const src: PCefChar; src_len: Cardinal; output: PCefStringUtf8): Integer; cdecl; external CefLibrary name 'cef_string_utf16_to_utf8' delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  function cef_string_from_utf8(const src: PAnsiChar; src_len: Cardinal; output: PCefString): Integer; cdecl; external CefLibrary name 'cef_string_utf8_to_utf16' delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  function cef_string_to_utf16(const src: PCefChar; src_len: Cardinal; output: PCefStringUtf16): Integer; cdecl; external CefLibrary name 'cef_string_utf16_copy' delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  function cef_string_from_utf16(const src: PChar16; src_len: Cardinal; output: PCefString): Integer; cdecl; external CefLibrary name 'cef_string_utf16_copy' delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  function cef_string_to_wide(const src: PCefChar; src_len: Cardinal; output: PCefStringWide): Integer; cdecl; external CefLibrary name 'cef_string_utf16_to_wide' delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  function cef_string_from_wide(const src: PWideChar; src_len: Cardinal; output: PCefString): Integer; cdecl; external CefLibrary name 'cef_string_wide_to_utf16' delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  {$ENDIF}
  {$IFDEF CEF_STRING_TYPE_WIDE}
  function cef_string_set(const src: PCefChar; src_len: Cardinal; output: PCefString; copy: Integer): Integer; cdecl; external CefLibrary name 'cef_string_wide_set' delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  procedure cef_string_clear(str: PCefString); cdecl; external CefLibrary name 'cef_string_wide_clear' delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  function cef_string_userfree_alloc(): PCefStringUserFree; cdecl; external CefLibrary name 'cef_string_userfree_wide_alloc' delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  procedure cef_string_userfree_free(str: PCefStringUserFree); cdecl; external CefLibrary name 'cef_string_userfree_wide_free' delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  function cef_string_from_ascii(const src: PAnsiChar; src_len: Cardinal; output: PCefString): Integer; cdecl; external CefLibrary name 'cef_string_ascii_to_wide' delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  function cef_string_to_utf8(const src: PCefChar; src_len: Cardinal; output: PCefStringUtf8): Integer; cdecl; external CefLibrary name 'cef_string_wide_to_utf8' delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  function cef_string_from_utf8(const src: PAnsiChar; src_len: Cardinal; output: PCefString): Integer; cdecl; external CefLibrary name 'cef_string_utf8_to_wide' delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  function cef_string_to_utf16(const src: PCefChar; src_len: Cardinal; output: PCefStringUtf16): Integer; cdecl; external CefLibrary name 'cef_string_wide_to_utf16' delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  function cef_string_from_utf16(const src: PChar16; src_len: Cardinal; output: PCefString): Integer; cdecl; external CefLibrary name 'cef_string_utf16_to_utf8' delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  function cef_string_to_wide(const src: PCefChar; src_len: Cardinal; output: PCefStringWide): Integer; cdecl; external CefLibrary name 'cef_string_utf8_to_wide' delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  function cef_string_from_wide(const src: PWideChar; src_len: Cardinal; output: PCefString): Integer; cdecl; external CefLibrary name 'cef_string_wide_to_utf8' delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  function cef_string_from_utf16(const src: PChar16; src_len: Cardinal; output: PCefString): Integer; cdecl; external CefLibrary name 'cef_string_utf16_to_wide' delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  function cef_string_to_wide(const src: PCefChar; src_len: Cardinal; output: PCefStringWide): Integer; cdecl; external CefLibrary name 'cef_string_wide_copy' delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  function cef_string_from_wide(const src: PWideChar; src_len: Cardinal; output: PCefString): Integer; cdecl; external CefLibrary name 'cef_string_wide_copy' delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  {$ENDIF}

  // These functions clear string values. The structure itself is not freed.

  procedure cef_string_wide_clear(str: PCefStringWide); cdecl; external CefLibrary delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  procedure cef_string_utf8_clear(str: PCefStringUtf8); cdecl; external CefLibrary delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  procedure cef_string_utf16_clear(str: PCefStringUtf16); cdecl; external CefLibrary delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};

  // These functions compare two string values with the same results as strcmp().

  function cef_string_wide_cmp(const str1, str2: PCefStringWide): Integer; cdecl; external CefLibrary delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  function cef_string_utf8_cmp(const str1, str2: PCefStringUtf8): Integer; cdecl; external CefLibrary delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  function cef_string_utf16_cmp(const str1, str2: PCefStringUtf16): Integer; cdecl; external CefLibrary delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};

  // These functions convert between UTF-8, -16, and -32 strings. They are
  // potentially slow so unnecessary conversions should be avoided. The best
  // possible result will always be written to |output| with the boolean return
  // value indicating whether the conversion is 100% valid.

  function cef_string_wide_to_utf8(const src: PWideChar; src_len: Cardinal; output: PCefStringUtf8): Integer; cdecl; external CefLibrary delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  function cef_string_utf8_to_wide(const src: PAnsiChar; src_len: Cardinal; output: PCefStringWide): Integer; cdecl; external CefLibrary delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};

  function cef_string_wide_to_utf16(const src: PWideChar; src_len: Cardinal; output: PCefStringUtf16): Integer; cdecl; external CefLibrary delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  function cef_string_utf16_to_wide(const src: PChar16; src_len: Cardinal; output: PCefStringWide): Integer; cdecl; external CefLibrary delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};

  function cef_string_utf8_to_utf16(const src: PAnsiChar; src_len: Cardinal; output: PCefStringUtf16): Integer; cdecl; external CefLibrary delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  function cef_string_utf16_to_utf8(const src: PChar16; src_len: Cardinal; output: PCefStringUtf8): Integer; cdecl; external CefLibrary delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};

  // These functions convert an ASCII string, typically a hardcoded constant, to a
  // Wide/UTF16 string. Use instead of the UTF8 conversion routines if you know
  // the string is ASCII.

  function cef_string_ascii_to_wide(const src: PAnsiChar; src_len: Cardinal; output: PCefStringWide): Integer; cdecl; external CefLibrary delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  function cef_string_ascii_to_utf16(const src: PAnsiChar; src_len: Cardinal; output: PCefStringUtf16): Integer; cdecl; external CefLibrary delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};

  // These functions allocate a new string structure. They must be freed by
  // calling the associated free function.

  function cef_string_userfree_wide_alloc: PCefStringUserFreeWide; cdecl; external CefLibrary delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  function cef_string_userfree_utf8_alloc: PCefStringUserFreeUtf8; cdecl; external CefLibrary delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  function cef_string_userfree_utf16_alloc: PCefStringUserFreeUtf16; cdecl; external CefLibrary delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};

  // These functions free the string structure allocated by the associated
  // alloc function. Any string contents will first be cleared.

  procedure cef_string_userfree_wide_free(str: PCefStringUserFreeWide); cdecl; external CefLibrary delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  procedure cef_string_userfree_utf8_free(str: PCefStringUserFreeUtf8); cdecl; external CefLibrary delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  procedure cef_string_userfree_utf16_free(str: PCefStringUserFreeUtf16); cdecl; external CefLibrary delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};

  {$ELSE}
var
    // This function should be called from the application entry point function to
		// execute a secondary process. It can be used to run secondary processes from
		// the browser client executable (default behavior) or from a separate
		// executable specified by the CefSettings.browser_subprocess_path value. If
		// called for the browser process (identified by no "type" command-line value)
		// it will return immediately with a value of -1. If called for a recognized
		// secondary process it will block until the process should exit and then return
		// the process exit code. The |application| parameter may be NULL. The
		// |windows_sandbox_info| parameter is only used on Windows and may be NULL (see
		// cef_sandbox_win.h for details).
		cef_execute_process: function(const args: PCefMainArgs; application: PCefApp; windows_sandbox_info: cvoid): cint;  cdecl;
		// This function should be called on the main application thread to initialize
		// the CEF browser process. The |application| parameter may be NULL. A return
		// value of true (1) indicates that it succeeded and false (0) indicates that it
		// failed. The |windows_sandbox_info| parameter is only used on Windows and may
		// be NULL (see cef_sandbox_win.h for details).
		cef_initialize: function(const args: PCefMainArgs; const settings: PCefSettings; application: PCefApp; windows_sandbox_info: cvoid): cint;  cdecl;
		// This function should be called on the main application thread to shut down
		// the CEF browser process before the application exits.
		cef_shutdown: procedure;  cdecl;
		// Perform a single iteration of CEF message loop processing. This function is
		// used to integrate the CEF message loop into an existing application message
		// loop. Care must be taken to balance performance against excessive CPU usage.
		// This function should only be called on the main application thread and only
		// if cef_initialize() is called with a CefSettings.multi_threaded_message_loop
		// value of false (0). This function will not block.
		cef_do_message_loop_work: procedure;  cdecl;
		// Run the CEF message loop. Use this function instead of an application-
		// provided message loop to get the best balance between performance and CPU
		// usage. This function should only be called on the main application thread and
		// only if cef_initialize() is called with a
		// CefSettings.multi_threaded_message_loop value of false (0). This function
		// will block until a quit message is received by the system.
		cef_run_message_loop: procedure;  cdecl;
		// Quit the CEF message loop that was started by calling cef_run_message_loop().
		// This function should only be called on the main application thread and only
		// if cef_run_message_loop() was used.
		cef_quit_message_loop: procedure;  cdecl;
		// Set to true (1) before calling Windows APIs like TrackPopupMenu that enter a
		// modal message loop. Set to false (0) after exiting the modal message loop.
		cef_set_osmodal_loop: procedure(osModalLoop: cint);  cdecl;
		// Create a new browser window using the window parameters specified by
		// |windowInfo|. All values will be copied internally and the actual window will
		// be created on the UI thread. If |request_context| is NULL the global request
		// context will be used. This function can be called on any browser process
		// thread and will not block.
		cef_browser_host_create_browser: function(const windowInfo: PCefWindowInfo; client: PCefClient; const url: PCefString; settings: PCefBrowserSettings; request_context: PCefRequestContext): cint;  cdecl;
		// Create a new browser window using the window parameters specified by
		// |windowInfo|. If |request_context| is NULL the global request context will be
		// used. This function can only be called on the browser process UI thread.
		cef_browser_host_create_browser_sync: function(const windowInfo: PCefWindowInfo; client: PCefClient; const url: PCefString; settings: PCefBrowserSettings; request_context: PCefRequestContext): PCefBrowser;  cdecl;
		// Create a new cef_command_line_t instance.
		cef_command_line_create: function: PCefCommandLine;  cdecl;
		// Returns the singleton global cef_command_line_t object. The returned object
		// will be read-only.
		cef_command_line_get_global: function: PCefCommandLine;  cdecl;
		// Returns the global cookie manager. By default data will be stored at
		// CefSettings.cache_path if specified or in memory otherwise. If |callback| is
		// non-NULL it will be executed asnychronously on the IO thread after the
		// manager's storage has been initialized. Using this function is equivalent to
		// calling cef_request_tContext::cef_request_context_get_global_context()->get_d
		// efault_cookie_manager().
		cef_cookie_manager_get_global_manager: function(callback: PCefCompletionCallback): PCefCookieManager;  cdecl;
		// Creates a new cookie manager. If |path| is NULL data will be stored in memory
		// only. Otherwise, data will be stored at the specified |path|. To persist
		// session cookies (cookies without an expiry date or validity interval) set
		// |persist_session_cookies| to true (1). Session cookies are generally intended
		// to be transient and most Web browsers do not persist them. If |callback| is
		// non-NULL it will be executed asnychronously on the IO thread after the
		// manager's storage has been initialized.
		cef_cookie_manager_create_manager: function(const path: PCefString; var persist_session_cookies: cint; callback: PCefCompletionCallback): PCefCookieManager;  cdecl;
		// Create a new cef_drag_data_t object.
		cef_drag_data_create: function: PCefDragData;  cdecl;
		// Request a one-time geolocation update. This function bypasses any user
		// permission checks so should only be used by code that is allowed to access
		// location information.
		cef_get_geolocation: function(callback: PCefGetGeolocationCallback): cint;  cdecl;
		// Add an entry to the cross-origin access whitelist.
		// The same-origin policy restricts how scripts hosted from different origins
		// (scheme + domain + port) can communicate. By default, scripts can only access
		// resources with the same origin. Scripts hosted on the HTTP and HTTPS schemes
		// (but no other schemes) can use the "Access-Control-Allow-Origin" header to
		// allow cross-origin requests. For example, https://source.example.com can make
		// XMLHttpRequest requests on http://target.example.com if the
		// http://target.example.com request returns an "Access-Control-Allow-Origin:
		// https://source.example.com" response header.
		// Scripts in separate frames or iframes and hosted from the same protocol and
		// domain suffix can execute cross-origin JavaScript if both pages set the
		// document.domain value to the same domain suffix. For example,
		// scheme://foo.example.com and scheme://bar.example.com can communicate using
		// JavaScript if both domains set document.domain="example.com".
		// This function is used to allow access to origins that would otherwise violate
		// the same-origin policy. Scripts hosted underneath the fully qualified
		// |source_origin| URL (like http://www.example.com) will be allowed access to
		// all resources hosted on the specified |target_protocol| and |target_domain|.
		// If |target_domain| is non-NULL and |allow_target_subdomains| if false (0)
		// only exact domain matches will be allowed. If |target_domain| contains a top-
		// level domain component (like "example.com") and |allow_target_subdomains| is
		// true (1) sub-domain matches will be allowed. If |target_domain| is NULL and
		// |allow_target_subdomains| if true (1) all domains and IP addresses will be
		// allowed.
		// This function cannot be used to bypass the restrictions on local or display
		// isolated schemes. See the comments on CefRegisterCustomScheme for more
		// information.
		// This function may be called on any thread. Returns false (0) if
		// |source_origin| is invalid or the whitelist cannot be accessed.
		cef_add_cross_origin_whitelist_entry: function(const source_origin: PCefString; const target_protocol: PCefString; const target_domain: PCefString; allow_target_subdomains: cint): cint;  cdecl;
		// Remove an entry from the cross-origin access whitelist. Returns false (0) if
		// |source_origin| is invalid or the whitelist cannot be accessed.
		cef_remove_cross_origin_whitelist_entry: function(const source_origin: PCefString; const target_protocol: PCefString; const target_domain: PCefString; allow_target_subdomains: cint): cint;  cdecl;
		// Remove all entries from the cross-origin access whitelist. Returns false (0)
		// if the whitelist cannot be accessed.
		cef_clear_cross_origin_whitelist: function: cint;  cdecl;
		// Parse the specified |url| into its component parts. Returns false (0) if the
		// URL is NULL or invalid.
		cef_parse_url: function(const url: PCefString; parts: PCefUrlparts): cint;  cdecl;
		// Creates a URL from the specified |parts|, which must contain a non-NULL spec
		// or a non-NULL host and path (at a minimum), but not both. Returns false (0)
		// if |parts| isn't initialized as described.
		cef_create_url: function(const parts: PCefUrlparts; url: PCefString): cint;  cdecl;
		// Returns the mime type for the specified file extension or an NULL string if
		// unknown.
		// The resulting string must be freed by calling cef_string_userfree_free().
		cef_get_mime_type: function(const extension: PCefString): PCefStringUserFree;  cdecl;
		// Get the extensions associated with the given mime type. This should be passed
		// in lower case. There could be multiple extensions for a given mime type, like
		// "html,htm" for "text/html", or "txt,text,html,..." for "text/*". Any existing
		// elements in the provided vector will not be erased.
		cef_get_extensions_for_mime_type: procedure(const mime_type: PCefString; extensions: TCefStringList);  cdecl;
		// Encodes |data| as a base64 string.
		// The resulting string must be freed by calling cef_string_userfree_free().
		cef_base64encode: function(data: cvoid; data_size: csize_t): PCefStringUserFree;  cdecl;
		// Decodes the base64 encoded string |data|. The returned value will be NULL if
		// the decoding fails.
		cef_base64decode: function(const data: PCefString): PCefBinaryValue;  cdecl;
		// Escapes characters in |text| which are unsuitable for use as a query
		// parameter value. Everything except alphanumerics and -_.!~*'() will be
		// converted to "%XX". If |use_plus| is true (1) spaces will change to "+". The
		// result is basically the same as encodeURIComponent in Javacript.
		// The resulting string must be freed by calling cef_string_userfree_free().
		cef_uriencode: function(const text: PCefString; use_plus: cint): PCefStringUserFree;  cdecl;
		// Unescapes |text| and returns the result. Unescaping consists of looking for
		// the exact pattern "%XX" where each X is a hex digit and converting to the
		// character with the numerical value of those digits (e.g. "i%20=%203%3b"
		// unescapes to "i = 3;"). If |convert_to_utf8| is true (1) this function will
		// attempt to interpret the initial decoded result as UTF-8. If the result is
		// convertable into UTF-8 it will be returned as converted. Otherwise the
		// initial decoded result will be returned.  The |unescape_rule| parameter
		// supports further customization the decoding process.
		// The resulting string must be freed by calling cef_string_userfree_free().
		cef_uridecode: function(const text: PCefString; convert_to_utf8: cint; unescape_rule: TCefUriUnescapeRule): PCefStringUserFree;  cdecl;
		// Parses |string| which represents a CSS color value. If |strict| is true (1)
		// strict parsing rules will be applied. Returns true (1) on success or false
		// (0) on error. If parsing succeeds |color| will be set to the color value
		// otherwise |color| will remain unchanged.
		cef_parse_csscolor: function(const _string: PCefString; strict: cint; color: PCefColor): cint;  cdecl;
		// Retrieve the path associated with the specified |key|. Returns true (1) on
		// success. Can be called on any thread in the browser process.
		cef_get_path: function(key: TCefPathKey; path: PCefString): cint;  cdecl;
		// Create a new cef_print_settings_t object.
		cef_print_settings_create: function: PCefPrintSettings;  cdecl;
		// Create a new cef_process_message_t object with the specified name.
		cef_process_message_create: function(const name: PCefString): PCefProcessMessage;  cdecl;
		// Launches the process specified via |command_line|. Returns true (1) upon
		// success. Must be called on the browser process TID_PROCESS_LAUNCHER thread.
		// Unix-specific notes: - All file descriptors open in the parent process will
		// be closed in the
		//   child process except for stdin, stdout, and stderr.
		// - If the first argument on the command line does not contain a slash,
		//   PATH will be searched. (See man execvp.)
		cef_launch_process: function(command_line: PCefCommandLine): cint;  cdecl;
		// Create a new cef_request_t object.
		cef_request_create: function: PCefRequest;  cdecl;
		// Create a new cef_post_data_t object.
		cef_post_data_create: function: PCefPostData;  cdecl;
		// Create a new cef_post_data_element_t object.
		cef_post_data_element_create: function: PCefPostDataElement;  cdecl;
		// Returns the global context object.
		cef_request_context_get_global_context: function: PCefRequestContext;  cdecl;
		// Creates a new context object with the specified |settings| and optional
		// |handler|.
		cef_request_context_create_context: function(const settings: PCefRequestContextSettings; handler: PCefRequestContextHandler): PCefRequestContext;  cdecl;
		// Creates a new context object that shares storage with |other| and uses an
		// optional |handler|.
		create_context_shared: function(other: PCefRequestContext; handler: PCefRequestContextHandler): PCefRequestContext;  cdecl;
		// Create a new cef_response_t object.
		cef_response_create: function: PCefResponse;  cdecl;
		// Register a scheme handler factory with the global request context. An NULL
		// |domain_name| value for a standard scheme will cause the factory to match all
		// domain names. The |domain_name| value will be ignored for non-standard
		// schemes. If |scheme_name| is a built-in scheme and no handler is returned by
		// |factory| then the built-in scheme handler factory will be called. If
		// |scheme_name| is a custom scheme then you must also implement the
		// cef_app_t::on_register_custom_schemes() function in all processes. This
		// function may be called multiple times to change or remove the factory that
		// matches the specified |scheme_name| and optional |domain_name|. Returns false
		// (0) if an error occurs. This function may be called on any thread in the
		// browser process. Using this function is equivalent to calling cef_request_tCo
		// ntext::cef_request_context_get_global_context()->register_scheme_handler_fact
		// ory().
		cef_register_scheme_handler_factory: function(const scheme_name: PCefString; const domain_name: PCefString; factory: PCefSchemeHandlerFactory): cint;  cdecl;
		// Clear all scheme handler factories registered with the global request
		// context. Returns false (0) on error. This function may be called on any
		// thread in the browser process. Using this function is equivalent to calling c
		// ef_request_tContext::cef_request_context_get_global_context()->clear_scheme_h
		// andler_factories().
		cef_clear_scheme_handler_factories: function: cint;  cdecl;
		// Create a new cef_stream_reader_t object from a file.
		cef_stream_reader_create_for_file: function(const fileName: PCefString): PCefStreamReader;  cdecl;
		// Create a new cef_stream_reader_t object from data.
		cef_stream_reader_create_for_data: function(data: cvoid; size: csize_t): PCefStreamReader;  cdecl;
		// Create a new cef_stream_reader_t object from a custom handler.
		cef_stream_reader_create_for_handler: function(handler: PCefReadHandler): PCefStreamReader;  cdecl;
		// Create a new cef_stream_writer_t object for a file.
		cef_stream_writer_create_for_file: function(const fileName: PCefString): PCefStreamWriter;  cdecl;
		// Create a new cef_stream_writer_t object for a custom handler.
		cef_stream_writer_create_for_handler: function(handler: PCefWriteHandler): PCefStreamWriter;  cdecl;
		// Returns the task runner for the current thread. Only CEF threads will have
		// task runners. An NULL reference will be returned if this function is called
		// on an invalid thread.
		cef_task_runner_get_for_current_thread: function: PCefTaskRunner;  cdecl;
		// Returns the task runner for the specified CEF thread.
		cef_task_runner_get_for_thread: function(threadId: TCefThreadId): PCefTaskRunner;  cdecl;
		// Returns true (1) if called on the specified thread. Equivalent to using
		// cef_task_tRunner::GetForThread(threadId)->belongs_to_current_thread().
		cef_currently_on: function(threadId: TCefThreadId): cint;  cdecl;
		// Post a task for execution on the specified thread. Equivalent to using
		// cef_task_tRunner::GetForThread(threadId)->PostTask(task).
		cef_post_task: function(threadId: TCefThreadId; task: PCefTask): cint;  cdecl;
		// Post a task for delayed execution on the specified thread. Equivalent to
		// using cef_task_tRunner::GetForThread(threadId)->PostDelayedTask(task,
		// delay_ms).
		cef_post_delayed_task: function(threadId: TCefThreadId; task: PCefTask; delay_ms: cint64): cint;  cdecl;
		// Start tracing events on all processes. Tracing is initialized asynchronously
		// and |callback| will be executed on the UI thread after initialization is
		// complete.
		// If CefBeginTracing was called previously, or if a CefEndTracingAsync call is
		// pending, CefBeginTracing will fail and return false (0).
		// |categories| is a comma-delimited list of category wildcards. A category can
		// have an optional '-' prefix to make it an excluded category. Having both
		// included and excluded categories in the same list is not supported.
		// Example: "test_MyTest*" Example: "test_MyTest*,test_OtherStuff" Example:
		// "-excluded_category1,-excluded_category2"
		// This function must be called on the browser process UI thread.
		cef_begin_tracing: function(const categories: PCefString; callback: PCefCompletionCallback): cint;  cdecl;
		// Stop tracing events on all processes.
		// This function will fail and return false (0) if a previous call to
		// CefEndTracingAsync is already pending or if CefBeginTracing was not called.
		// |tracing_file| is the path at which tracing data will be written and
		// |callback| is the callback that will be executed once all processes have sent
		// their trace data. If |tracing_file| is NULL a new temporary file path will be
		// used. If |callback| is NULL no trace data will be written.
		// This function must be called on the browser process UI thread.
		cef_end_tracing: function(const tracing_file: PCefString; callback: PCefEndTracingCallback): cint;  cdecl;
		// Returns the current system trace time or, if none is defined, the current
		// high-res time. Can be used by clients to synchronize with the time
		// information in trace events.
		cef_now_from_system_trace_time: function: cint64;  cdecl;
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
		cef_urlrequest_create: function(request: PCefRequest; client: PCefUrlrequestClient; request_context: PCefRequestContext): PCefUrlrequest;  cdecl;
		// Returns the current (top) context object in the V8 context stack.
		cef_v8context_get_current_context: function: PCefV8context;  cdecl;
		// Returns the entered (bottom) context object in the V8 context stack.
		cef_v8context_get_entered_context: function: PCefV8context;  cdecl;
		// Returns true (1) if V8 is currently inside a context.
		cef_v8context_in_context: function: cint;  cdecl;
		// Create a new cef_v8value_t object of type undefined.
		cef_v8value_create_undefined: function: PCefV8value;  cdecl;
		// Create a new cef_v8value_t object of type null.
		cef_v8value_create_null: function: PCefV8value;  cdecl;
		// Create a new cef_v8value_t object of type bool.
		cef_v8value_create_bool: function(value: cint): PCefV8value;  cdecl;
		// Create a new cef_v8value_t object of type int.
		cef_v8value_create_int: function(value: cint32): PCefV8value;  cdecl;
		// Create a new cef_v8value_t object of type unsigned int.
		cef_v8value_create_uint: function(value: cuint32): PCefV8value;  cdecl;
		// Create a new cef_v8value_t object of type double.
		cef_v8value_create_double: function(value: cdouble): PCefV8value;  cdecl;
		// Create a new cef_v8value_t object of type Date. This function should only be
		// called from within the scope of a cef_render_process_handler_t,
		// cef_v8handler_t or cef_v8accessor_t callback, or in combination with calling
		// enter() and exit() on a stored cef_v8context_t reference.
		cef_v8value_create_date: function(date: PCefTime): PCefV8value;  cdecl;
		// Create a new cef_v8value_t object of type string.
		cef_v8value_create_string: function(const value: PCefString): PCefV8value;  cdecl;
		// Create a new cef_v8value_t object of type object with optional accessor. This
		// function should only be called from within the scope of a
		// cef_render_process_handler_t, cef_v8handler_t or cef_v8accessor_t callback,
		// or in combination with calling enter() and exit() on a stored cef_v8context_t
		// reference.
		cef_v8value_create_object: function(accessor: PCefV8accessor): PCefV8value;  cdecl;
		// Create a new cef_v8value_t object of type array with the specified |length|.
		// If |length| is negative the returned array will have length 0. This function
		// should only be called from within the scope of a
		// cef_render_process_handler_t, cef_v8handler_t or cef_v8accessor_t callback,
		// or in combination with calling enter() and exit() on a stored cef_v8context_t
		// reference.
		cef_v8value_create_array: function(length: cint): PCefV8value;  cdecl;
		// Create a new cef_v8value_t object of type function. This function should only
		// be called from within the scope of a cef_render_process_handler_t,
		// cef_v8handler_t or cef_v8accessor_t callback, or in combination with calling
		// enter() and exit() on a stored cef_v8context_t reference.
		cef_v8value_create_function: function(const name: PCefString; handler: PCefV8handler): PCefV8value;  cdecl;
		// Returns the stack trace for the currently active context. |frame_limit| is
		// the maximum number of frames that will be captured.
		cef_v8stack_trace_get_current: function(frame_limit: cint): PCefV8stackTrace;  cdecl;
		// Register a new V8 extension with the specified JavaScript extension code and
		// handler. Functions implemented by the handler are prototyped using the
		// keyword 'native'. The calling of a native function is restricted to the scope
		// in which the prototype of the native function is defined. This function may
		// only be called on the render process main thread.
		// Example JavaScript extension code: <pre>
		//   // create the 'example' global object if it doesn't already exist.
		//   if (!example)
		//     example = {};
		//   // create the 'example.test' global object if it doesn't already exist.
		//   if (!example.test)
		//     example.test = {};
		//   (function() {
		//     // Define the function 'example.test.myfunction'.
		//     example.test.myfunction = function() {
		//       // Call CefV8Handler::Execute() with the function name 'MyFunction'
		//       // and no arguments.
		//       native function MyFunction();
		//       return MyFunction();
		//     };
		//     // Define the getter function for parameter 'example.test.myparam'.
		//     example.test.__defineGetter__('myparam', function() {
		//       // Call CefV8Handler::Execute() with the function name 'GetMyParam'
		//       // and no arguments.
		//       native function GetMyParam();
		//       return GetMyParam();
		//     });
		//     // Define the setter function for parameter 'example.test.myparam'.
		//     example.test.__defineSetter__('myparam', function(b) {
		//       // Call CefV8Handler::Execute() with the function name 'SetMyParam'
		//       // and a single argument.
		//       native function SetMyParam();
		//       if(b) SetMyParam(b);
		//     });
		//     // Extension definitions can also contain normal JavaScript variables
		//     // and functions.
		//     var myint = 0;
		//     example.test.increment = function() {
		//       myint += 1;
		//       return myint;
		//     };
		//   })();
		// </pre> Example usage in the page: <pre>
		//   // Call the function.
		//   example.test.myfunction();
		//   // Set the parameter.
		//   example.test.myparam = value;
		//   // Get the parameter.
		//   value = example.test.myparam;
		//   // Call another function.
		//   example.test.increment();
		// </pre>
		cef_register_extension: function(const extension_name: PCefString; const javascript_code: PCefString; handler: PCefV8handler): cint;  cdecl;
		// Creates a new object.
		cef_value_create: function: PCefValue;  cdecl;
		// Creates a new object that is not owned by any other object. The specified
		// |data| will be copied.
		cef_binary_value_create: function(data: cvoid; data_size: csize_t): PCefBinaryValue;  cdecl;
		// Creates a new object that is not owned by any other object.
		cef_dictionary_value_create: function: PCefDictionaryValue;  cdecl;
		// Creates a new object that is not owned by any other object.
		cef_list_value_create: function: PCefListValue;  cdecl;
		// Visit web plugin information. Can be called on any thread in the browser
		// process.
		cef_visit_web_plugin_info: procedure(visitor: PCefWebPluginInfoVisitor);  cdecl;
		// Cause the plugin list to refresh the next time it is accessed regardless of
		// whether it has already been loaded. Can be called on any thread in the
		// browser process.
		cef_refresh_web_plugins: procedure;  cdecl;
		// Add a plugin path (directory + file). This change may not take affect until
		// after cef_refresh_web_plugins() is called. Can be called on any thread in the
		// browser process.
		cef_add_web_plugin_path: procedure(const path: PCefString);  cdecl;
		// Add a plugin directory. This change may not take affect until after
		// cef_refresh_web_plugins() is called. Can be called on any thread in the
		// browser process.
		cef_add_web_plugin_directory: procedure(const dir: PCefString);  cdecl;
		// Remove a plugin path (directory + file). This change may not take affect
		// until after cef_refresh_web_plugins() is called. Can be called on any thread
		// in the browser process.
		cef_remove_web_plugin_path: procedure(const path: PCefString);  cdecl;
		// Unregister an internal plugin. This may be undone the next time
		// cef_refresh_web_plugins() is called. Can be called on any thread in the
		// browser process.
		cef_unregister_internal_web_plugin: procedure(const path: PCefString);  cdecl;
		// Force a plugin to shutdown. Can be called on any thread in the browser
		// process but will be executed on the IO thread.
		cef_force_web_plugin_shutdown: procedure(const path: PCefString);  cdecl;
		// Register a plugin crash. Can be called on any thread in the browser process
		// but will be executed on the IO thread.
		cef_register_web_plugin_crash: procedure(const path: PCefString);  cdecl;
		// Query if a plugin is unstable. Can be called on any thread in the browser
		// process.
		cef_is_web_plugin_unstable: procedure(const path: PCefString; callback: PCefWebPluginUnstableCallback);  cdecl;
		// Create a new cef_xml_reader_t object. The returned object's functions can
		// only be called from the thread that created the object.
		cef_xml_reader_create: function(stream: PCefStreamReader; encodingType: TCefXmlEncodingType; const URI: PCefString): PCefXmlReader;  cdecl;
		// Create a new cef_zip_reader_t object. The returned object's functions can
		// only be called from the thread that created the object.
		cef_zip_reader_create: function(stream: PCefStreamReader): PCefZipReader;  cdecl;

  {----------------------------CefStringMap.h----------------------------------}
  // Allocate a new string map.
  cef_string_map_alloc: function: TCefStringMap; cdecl;
  //cef_string_map_size(map: TCefStringMap): Integer; cdecl;
  cef_string_map_size: function(map: TCefStringMap): Integer; cdecl;
  // Return the value assigned to the specified key.
  cef_string_map_find: function(map: TCefStringMap; const key: PCefString; var value: PCefString): Integer; cdecl;
  // Return the key at the specified zero-based string map index.
  cef_string_map_key: function(map: TCefStringMap; index: Integer; var key: TCefString): Integer; cdecl;
  // Return the value at the specified zero-based string map index.
  cef_string_map_value: function(map: TCefStringMap; index: Integer; var value: TCefString): Integer; cdecl;
  // Append a new key/value pair at the end of the string map.
  cef_string_map_append: function(map: TCefStringMap; const key, value: PCefString): Integer; cdecl;
  // Clear the string map.
  cef_string_map_clear: procedure(map: TCefStringMap); cdecl;
  // Free the string map.
  cef_string_map_free: procedure(map: TCefStringMap); cdecl;

  {----------------------------CefStringList.h---------------------------------}
  // Allocate a new string map.
  cef_string_list_alloc: function: TCefStringList; cdecl;
  // Return the number of elements in the string list.
  cef_string_list_size: function(list: TCefStringList): Integer; cdecl;
  // Retrieve the value at the specified zero-based string list index. Returns
  // true (1) if the value was successfully retrieved.
  cef_string_list_value: function(list: TCefStringList; index: Integer; value: PCefString): Integer; cdecl;
  // Append a new value at the end of the string list.
  cef_string_list_append: procedure(list: TCefStringList; const value: PCefString); cdecl;
  // Clear the string list.
  cef_string_list_clear: procedure(list: TCefStringList); cdecl;
  // Free the string list.
  cef_string_list_free: procedure(list: TCefStringList); cdecl;
  // Creates a copy of an existing string list.
  cef_string_list_copy: function(list: TCefStringList): TCefStringList; cdecl;

  {----------------------------CefStringMultiMap.h-----------------------------}
  // Allocate a new string multimap.
  cef_string_multimap_alloc: function: TCefStringMultimap; cdecl;
  // Return the number of elements in the string multimap.
  cef_string_multimap_size: function(map: TCefStringMultimap): Integer; cdecl;
  // Return the number of values with the specified key.
  cef_string_multimap_find_count: function(map: TCefStringMultimap; const key: PCefString): Integer; cdecl;
  // Return the value_index-th value with the specified key.
  cef_string_multimap_enumerate: function(map: TCefStringMultimap;
    const key: PCefString; value_index: Integer; var value: TCefString): Integer; cdecl;
  // Return the key at the specified zero-based string multimap index.
  cef_string_multimap_key: function(map: TCefStringMultimap; index: Integer; var key: TCefString): Integer; cdecl;
  // Return the value at the specified zero-based string multimap index.
  cef_string_multimap_value: function(map: TCefStringMultimap; index: Integer; var value: TCefString): Integer; cdecl;
  // Append a new key/value pair at the end of the string multimap.
  cef_string_multimap_append: function(map: TCefStringMultimap; const key, value: PCefString): Integer; cdecl;
  // Clear the string multimap.
  cef_string_multimap_clear: procedure(map: TCefStringMultimap); cdecl;
  // Free the string multimap.
  cef_string_multimap_free: procedure(map: TCefStringMultimap); cdecl;
  cef_build_revision: function: cint; cdecl;

  {----------------------------Ceftime.h---------------------------------------}
  // Converts cef_time_t to/from time_t. Returns true (1) on success and false (0)
  // on failure.
  cef_time_to_timet: function(const cef_time:PCefTime; time: pcint64):integer; cdecl;
  cef_time_from_timet: function(time:Int64; cef_time:TCefTime):integer; cdecl;
  // Converts cef_time_t to/from a double which is the number of seconds since
  // epoch (Jan 1, 1970). Webkit uses this format to represent time. A value of 0
  // means "not initialized". Returns true (1) on success and false (0) on
  // failure.
  cef_time_to_doublet: function(const cef_time:PCefTime; time:PDouble):integer; cdecl;
  cef_time_from_doublet: function(time:Double; cef_time:PCefTime):integer; cdecl;
  // Retrieve the current system time.
  cef_time_now: function(cef_time:PCefTime):integer; cdecl;
  // Retrieve the delta in milliseconds between two time values.
  cef_time_delta: function(const cef_time1:PCefTime; const cef_time2:PCeftime; delta:UInt64):integer; cdecl;


  // These functions set string values. If |copy| is true (1) the value will be
  // copied instead of referenced. It is up to the user to properly manage
  // the lifespan of references.
  cef_string_wide_set: function(const src: PWideChar; src_len: Cardinal;  output: PCefStringWide; copy: Integer): Integer; cdecl;
  cef_string_utf8_set: function(const src: PAnsiChar; src_len: Cardinal; output: PCefStringUtf8; copy: Integer): Integer; cdecl;
  cef_string_utf16_set: function(const src: PChar16; src_len: Cardinal; output: PCefStringUtf16; copy: Integer): Integer; cdecl;

  {$IFDEF CEF_STRING_TYPE_UTF8}
  cef_string_set: function(const src: PCefChar; src_len: Cardinal; output: PCefString; copy: Integer): Integer; cdecl; external CefLibrary name 'cef_string_utf8_set' delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  cef_string_clear: procedure(str: PCefString); cdecl; external CefLibrary name 'cef_string_utf8_clear' delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  cef_string_userfree_alloc: function(): PCefStringUserFree; cdecl; external CefLibrary name 'cef_string_userfree_utf8_alloc' delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  cef_string_userfree_free: procedure(str: PCefStringUserFree); cdecl; external CefLibrary name 'cef_string_userfree_utf8_free' delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  cef_string_from_ascii: function(const src: PAnsiChar; src_len: Cardinal; output: PCefString): Integer; cdecl; external CefLibrary name 'cef_string_ascii_to_utf8' delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  cef_string_to_utf8: function(const src: PCefChar; src_len: Cardinal; output: PCefStringUtf8): Integer; cdecl; external CefLibrary name 'cef_string_utf8_copy' delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  cef_string_from_utf8: function(const src: PAnsiChar; src_len: Cardinal; output: PCefString): Integer; cdecl; external CefLibrary name 'cef_string_utf8_copy' delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  cef_string_to_utf16: function(const src: PCefChar; src_len: Cardinal; output: PCefStringUtf16): Integer; cdecl; external CefLibrary name 'cef_string_utf8_to_utf16' delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  cef_string_from_utf16: function(const src: PChar16; src_len: Cardinal; output: PCefString): Integer; cdecl; external CefLibrary name 'cef_string_utf16_to_utf8' delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  cef_string_to_wide: function(const src: PCefChar; src_len: Cardinal; output: PCefStringWide): Integer; cdecl; external CefLibrary name 'cef_string_utf8_to_wide' delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  cef_string_from_wide: function(const src: PWideChar; src_len: Cardinal; output: PCefString): Integer; cdecl; external CefLibrary name 'cef_string_wide_to_utf8' delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  {$ENDIF}
  {$IFDEF CEF_STRING_TYPE_UTF16}
  cef_string_set: function(const src: PCefChar; src_len: Cardinal; output: PCefString; copy: Integer): Integer; cdecl;
  cef_string_clear: procedure(str: PCefString); cdecl;
  cef_string_userfree_alloc: function: PCefStringUserFree; cdecl;
  cef_string_userfree_free: procedure(str: PCefStringUserFree); cdecl;
  cef_string_from_ascii: function(const src: PAnsiChar; src_len: Cardinal; output: PCefString): Integer; cdecl;
  cef_string_to_utf8: function(const src: PCefChar; src_len: Cardinal; output: PCefStringUtf8): Integer; cdecl;
  cef_string_from_utf8: function(const src: PAnsiChar; src_len: Cardinal; output: PCefString): Integer; cdecl;
  cef_string_to_utf16: function(const src: PCefChar; src_len: Cardinal; output: PCefStringUtf16): Integer; cdecl;
  cef_string_from_utf16: function(const src: PChar16; src_len: Cardinal; output: PCefString): Integer; cdecl;
  cef_string_to_wide: function(const src: PCefChar; src_len: Cardinal; output: PCefStringWide): Integer; cdecl;
  cef_string_from_wide: function(const src: PWideChar; src_len: Cardinal; output: PCefString): Integer; cdecl;
  {$ENDIF}
  {$IFDEF CEF_STRING_TYPE_WIDE}
  cef_string_set: function(const src: PCefChar; src_len: Cardinal; output: PCefString; copy: Integer): Integer; cdecl; external CefLibrary name 'cef_string_wide_set' delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  cef_string_clear: procedure(str: PCefString); cdecl; external CefLibrary name 'cef_string_wide_clear' delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  cef_string_userfree_alloc: function(): PCefStringUserFree; cdecl; external CefLibrary name 'cef_string_userfree_wide_alloc' delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  cef_string_userfree_free: procedure(str: PCefStringUserFree); cdecl; external CefLibrary name 'cef_string_userfree_wide_free' delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  cef_string_from_ascii: function(const src: PAnsiChar; src_len: Cardinal; output: PCefString): Integer; cdecl; external CefLibrary name 'cef_string_ascii_to_wide' delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  cef_string_to_utf8: function(const src: PCefChar; src_len: Cardinal; output: PCefStringUtf8): Integer; cdecl; external CefLibrary name 'cef_string_wide_to_utf8' delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  cef_string_from_utf8: function(const src: PAnsiChar; src_len: Cardinal; output: PCefString): Integer; cdecl; external CefLibrary name 'cef_string_utf8_to_wide' delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  cef_string_to_utf16: function(const src: PCefChar; src_len: Cardinal; output: PCefStringUtf16): Integer; cdecl; external CefLibrary name 'cef_string_wide_to_utf16' delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  cef_string_from_utf16: function(const src: PChar16; src_len: Cardinal; output: PCefString): Integer; cdecl; external CefLibrary name 'cef_string_utf16_to_utf8' delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  cef_string_to_wide: function(const src: PCefChar; src_len: Cardinal; output: PCefStringWide): Integer; cdecl; external CefLibrary name 'cef_string_utf8_to_wide' delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  cef_string_from_wide: function(const src: PWideChar; src_len: Cardinal; output: PCefString): Integer; cdecl; external CefLibrary name 'cef_string_wide_to_utf8' delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  cef_string_from_utf16: function(const src: PChar16; src_len: Cardinal; output: PCefString): Integer; cdecl; external CefLibrary name 'cef_string_utf16_to_wide' delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  cef_string_to_wide(const src: PCefChar; src_len: Cardinal; output: PCefStringWide): Integer; cdecl; external CefLibrary name 'cef_string_wide_copy' delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  cef_string_from_wide(const src: PWideChar; src_len: Cardinal; output: PCefString): Integer; cdecl; external CefLibrary name 'cef_string_wide_copy' delayed {$IFDEF SUPPORTS_INLINE}inline{$ENDIF};
  {$ENDIF}

  // These functions clear string values. The structure itself is not freed.

  cef_string_wide_clear: procedure(str: PCefStringWide); cdecl;
  cef_string_utf8_clear: procedure(str: PCefStringUtf8); cdecl;
  cef_string_utf16_clear: procedure(str: PCefStringUtf16); cdecl;

  // These functions compare two string values with the same results as strcmp().

  cef_string_wide_cmp: function(const str1, str2: PCefStringWide): Integer; cdecl;
  cef_string_utf8_cmp: function(const str1, str2: PCefStringUtf8): Integer; cdecl;
  cef_string_utf16_cmp: function(const str1, str2: PCefStringUtf16): Integer; cdecl;

  // These functions convert between UTF-8, -16, and -32 strings. They are
  // potentially slow so unnecessary conversions should be avoided. The best
  // possible result will always be written to |output| with the boolean return
  // value indicating whether the conversion is 100% valid.

  cef_string_wide_to_utf8: function(const src: PWideChar; src_len: Cardinal; output: PCefStringUtf8): Integer; cdecl;
  cef_string_utf8_to_wide: function(const src: PAnsiChar; src_len: Cardinal; output: PCefStringWide): Integer; cdecl;

  cef_string_wide_to_utf16: function(const src: PWideChar; src_len: Cardinal; output: PCefStringUtf16): Integer; cdecl;
  cef_string_utf16_to_wide: function(const src: PChar16; src_len: Cardinal; output: PCefStringWide): Integer; cdecl;

  cef_string_utf8_to_utf16: function(const src: PAnsiChar; src_len: Cardinal; output: PCefStringUtf16): Integer; cdecl;
  cef_string_utf16_to_utf8: function(const src: PChar16; src_len: Cardinal; output: PCefStringUtf8): Integer; cdecl;

  // These functions convert an ASCII string, typically a hardcoded constant, to a
  // Wide/UTF16 string. Use instead of the UTF8 conversion routines if you know
  // the string is ASCII.

  cef_string_ascii_to_wide: function(const src: PAnsiChar; src_len: Cardinal; output: PCefStringWide): Integer; cdecl;
  cef_string_ascii_to_utf16: function(const src: PAnsiChar; src_len: Cardinal; output: PCefStringUtf16): Integer; cdecl;

  // These functions allocate a new string structure. They must be freed by
  // calling the associated free function.

  cef_string_userfree_wide_alloc: function: PCefStringUserFreeWide; cdecl;
  cef_string_userfree_utf8_alloc: function: PCefStringUserFreeUtf8; cdecl;
  cef_string_userfree_utf16_alloc: function: PCefStringUserFreeUtf16; cdecl;

  // These functions free the string structure allocated by the associated
  // alloc function. Any string contents will first be cleared.

  cef_string_userfree_wide_free: procedure(str: PCefStringUserFreeWide); cdecl;
  cef_string_userfree_utf8_free: procedure(str: PCefStringUserFreeUtf8); cdecl;
  cef_string_userfree_utf16_free: procedure(str: PCefStringUserFreeUtf16); cdecl;
  {$ENDIF}

implementation

end.
