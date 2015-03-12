unit WACefLib;

{$IFDEF FPC}
  {$MODE DELPHI}{$H+}
{$ENDIF}

interface

{$I WACef.inc}

uses
  WACefTypes,
  WACefCApi,
  WACefInterfaces,
  WACefOwns,
  WACefRefs,
  WACefCExports,
  {$IFDEF MSWINDOWS}
  Windows,
  {$ELSE}
  dynlibs,
  {$ENDIF}
  SysUtils,
  {$IFDEF FPC}
  ctypes,
  {$ENDIF}
  Classes;

type
  TWACef = class
  private
    class var FLibHandle: THandle;
    class var FIsMainProcess :boolean;
    class var FApp: ICefApp;
    class var FArgs : TCefMainArgs;
    class var FInitialized: boolean;
    class var FAppStarted: boolean;
  public
    class function LoadLib: boolean;
    class procedure StartApp;
    class procedure Initialize;
    class procedure Finalize;
    class function GetObject(ptr: Pointer): TObject; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
    class function GetData(const i: ICefBase): Pointer; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
    class function ColorSetARGB(a, r, g, b: Byte): TCefColor;
    //..............................................................................String methods
    class function StringFreeAndGet(const str: PCefStringUserFree): ustring;
    class function ToCefString(const str: ustring): TCefString; overload;
    class function ToString(const str: PCefString): ustring; overload;
    class function StringClearAndGet(var str: TCefString): ustring;
    class function StringAlloc(const str: ustring): TCefString;
    class procedure StringFree(const str: PCefString);
    class function UserFreeString(const str: ustring): PCefStringUserFree;
    class procedure StringSet(const str: PCefString; const value: ustring);
    //..............................................................................Date and time methods
    {$IFDEF MSWINDOWS}
    class function CefTimeToSystemTime(const dt: TCefTime): TSystemTime;
    class function SystemTimeToCefTime(const dt: TSystemTime): TCefTime;
    class function CefTimeToSQLInt(const ct: TCefTime): Int64;
    class function SQLIntToCefTime(const si: Int64): TCefTime;
    {$ENDIF}
    class function CefTimeToDateTime(const dt: TCefTime): TDateTime;
    class function DateTimeToCefTime(dt: TDateTime): TCefTime;
    {---------------------------------CefAppCApi---------------------------------}
    {$IFNDEF CEF_MULTI_THREADED_MESSAGE_LOOP}
    class procedure DoMessageLoopWork;
    class procedure RunMessageLoop;
    class procedure QuitMessageLoop;
    {$ENDIF}
    {-------------------------------CefBrowserCApi-------------------------------}
    class function BrowserHostCreateSync(windowInfo:PCefWindowInfo; const client:ICefClient;
      const url:ustring; const settings:PCefBrowserSettings; const requestContext:ICefRequestContext): ICefBrowser;
    class function BrowserHostCreateBrowser(windowInfo: PCefWindowInfo; const client: ICefClient;
      const url: ustring; const settings: PCefBrowserSettings; const requestContext:ICefRequestContext): boolean;
    {----------------------------CefGeolocationCApi------------------------------}
    class function GetGeolocation(const callback: ICefGetGeolocationCallback): Boolean;
    {----------------------------CefOriginWhiteListCApi--------------------------}
    class function AddCrossOriginWhitelistEntry(const SourceOrigin, TargetProtocol,
      TargetDomain: ustring; AllowTargetSubdomains: Boolean): Boolean;
    class function RemoveCrossOriginWhitelistEntry(
      const SourceOrigin, TargetProtocol, TargetDomain: ustring;
      AllowTargetSubdomains: Boolean): Boolean;
    class function ClearCrossOriginWhitelist: Boolean;
    {----------------------------CefPathUtilCApi---------------------------------}
    class function GetPath(key: TCefPathKey; out path: ustring): Boolean;
    {----------------------------CefSchemeCApi-----------------------------------}
    class function RegisterSchemeHandlerFactory(const SchemeName, HostName: ustring;
      SyncMainThread: Boolean; const handler: TCefResourceHandlerClass): Boolean;
    class function ClearSchemeHandlerFactories: Boolean;
    {----------------------------CefTraceCApi------------------------------------}
    class function NowFromSystemTraceTime: cint64;
    {----------------------------CefUrlCApi--------------------------------------}
    class function ParseUrl(const url: ustring; var parts: TUrlParts): Boolean;
    class function CreateUrl(var parts: TUrlParts): ustring;
    class procedure GetExtensionsForMimeType(const MimeType: ustring; Extensions: TStrings);
    {----------------------------CefV8CApi---------------------------------------}
    class function RegisterExtension(const name, code: ustring; const Handler: ICefv8Handler): Boolean;
    {----------------------------CefWebPluginCApi--------------------------------}
    class procedure VisitWebPluginInfo(const visitor: ICefWebPluginInfoVisitor);
    class procedure VisitWebPluginInfoProc(const visitor: TCefWebPluginInfoVisitorProc);
    class procedure RefreshWebPlugins;
    class procedure AddWebPluginPath(const path: ustring);
    class procedure AddWebPluginDirectory(const dir: ustring);
    class procedure RemoveWebPluginPath(const path: ustring);
    class procedure UnregisterInternalWebPlugin(const path: ustring);
    class procedure ForceWebPluginShutdown(const path: ustring);
    class procedure RegisterWebPluginCrash(const path: ustring);
    class procedure IsWebPluginUnstable(const path: ustring; const callback: ICefWebPluginUnstableCallback);
    class procedure IsWebPluginUnstableProc(const path: ustring; const callback: TCefWebPluginIsUnstableProc);
  end;

var
  CefSingleProcess: Boolean = false;
  CefBrowserSubprocessPath: ustring = '';
  CefCommandLineArgsDisabled: Boolean = false;
  CefCachePath: ustring = '';
  CefPersistSessionCookies: Boolean = false;
  CefUserAgent: ustring = '';
  CefProductVersion: ustring = '';
  CefLocale: ustring = '';
  CefLogFile: ustring = '';
  CefLogSeverity: TCefLogSeverity = LOGSEVERITY_DISABLE;
  CefJavaScriptFlags: ustring = '';
  CefResourcesDirPath: ustring = '';
  CefLocalesDirPath: ustring = '';
  CefPackLoadingDisabled: Boolean = false;
  CefRemoteDebuggingPort: Integer = 9000;
  CefUncaughtExceptionStackSize: cint = 0;
  CefContextSafetyImplementation: Boolean = false;
  CefIgnoreCertificateErrors: Boolean = false;
  CefBackgroundColor: TCefColor = High(TCefColor);

  CefGetDataResource: TGetDataResource;
  CefGetLocalizedString: TGetLocalizedString;
  CefResourceBundleHandler: ICefResourceBundleHandler;
  CefBrowserProcessHandler: ICefBrowserProcessHandler;
  CefRenderProcessHandler: ICefRenderProcessHandler;
  CefOnBeforeCommandLineProcessing: TOnBeforeCommandLineProcessing;
  CefOnRegisterCustomSchemes: TOnRegisterCustomSchemes;

implementation

uses
  Math;

type
  TInitiator = class(TThread)
    procedure Execute; override;
  end;

procedure TInitiator.Execute;
begin

end;

{$IFDEF MSWINDOWS}
function TzSpecificLocalTimeToSystemTime(
      lpTimeZoneInformation: PTimeZoneInformation;
      lpLocalTime, lpUniversalTime: PSystemTime): BOOL; stdcall; external 'kernel32.dll';
{$ENDIF}

//{$IFDEF FPC}
function cef_string_utf16_copy(const src: PChar16; src_len: csize_t; output: PCefStringUtf16): Integer; cdecl;
begin
  Result := cef_string_utf16_set(src, src_len, output, ord(True))
end;
//{$ENDIF}

//..............................................................................WACEF
{Private section}
{Public section}
class function TWACef.LoadLib: boolean;
begin
  if FLibHandle = 0 then
  begin
    with TInitiator.Create(false) do
    begin
      WaitFor;
      Free;
    end;

    result := false;
    FLibHandle := LoadLibrary(PChar(ExtractFilePath(ParamStr(0)) + CefLibrary));
    If FLibHandle = 0 then
      Exit;
    {$IFDEF FPC}
      cef_execute_process                     := GetProcAddress(FLibHandle, 'cef_execute_process');

      cef_initialize                          := GetProcAddress(FLibHandle, 'cef_initialize');
      cef_shutdown                            := GetProcAddress(FLibHandle, 'cef_shutdown');
      cef_do_message_loop_work                := GetProcAddress(FLibHandle, 'cef_do_message_loop_work');
      cef_run_message_loop                    := GetProcAddress(FLibHandle, 'cef_run_message_loop');
      cef_quit_message_loop                   := GetProcAddress(FLibHandle, 'cef_quit_message_loop');
      cef_set_osmodal_loop                    := GetProcAddress(FLibHandle, 'cef_set_osmodal_loop');

      cef_browser_host_create_browser         := GetProcAddress(FLibHandle, 'cef_browser_host_create_browser');
      cef_browser_host_create_browser_sync    := GetProcAddress(FLibHandle, 'cef_browser_host_create_browser_sync');

      cef_command_line_create                 := GetProcAddress(FLibHandle, 'cef_command_line_create');
      cef_command_line_get_global             := GetProcAddress(FLibHandle, 'cef_command_line_get_global');

      cef_cookie_manager_get_global_manager   := GetProcAddress(FLibHandle, 'cef_cookie_manager_get_global_manager');
      cef_cookie_manager_create_manager       := GetProcAddress(FLibHandle, 'cef_cookie_manager_create_manager');

      cef_drag_data_create                    := GetProcAddress(FLibHandle, 'cef_drag_data_create');

      cef_get_geolocation                     := GetProcAddress(FLibHandle, 'cef_get_geolocation');

      cef_add_cross_origin_whitelist_entry    := GetProcAddress(FLibHandle, 'cef_add_cross_origin_whitelist_entry');
      cef_remove_cross_origin_whitelist_entry := GetProcAddress(FLibHandle, 'cef_remove_cross_origin_whitelist_entry');
      cef_clear_cross_origin_whitelist        := GetProcAddress(FLibHandle, 'cef_clear_cross_origin_whitelist');

      cef_get_path                            := GetProcAddress(FLibHandle, 'cef_get_path');

      cef_print_settings_create               := GetProcAddress(FLibHandle, 'cef_print_settings_create');

      cef_process_message_create              := GetProcAddress(FLibHandle, 'cef_process_message_create');

      cef_launch_process                      := GetProcAddress(FLibHandle, 'cef_launch_process');

      cef_request_create                      := GetProcAddress(FLibHandle, 'cef_request_create');
      cef_post_data_create                    := GetProcAddress(FLibHandle, 'cef_post_data_create');
      cef_post_data_element_create            := GetProcAddress(FLibHandle, 'cef_post_data_element_create');

      cef_request_context_get_global_context  := GetProcAddress(FLibHandle, 'cef_request_context_get_global_context');
      cef_request_context_create_context      := GetProcAddress(FLibHandle, 'cef_request_context_create_context');

      cef_response_create                     := GetProcAddress(FLibHandle, 'cef_response_create');

      cef_register_scheme_handler_factory     := GetProcAddress(FLibHandle, 'cef_register_scheme_handler_factory');
      cef_clear_scheme_handler_factories      := GetProcAddress(FLibHandle, 'cef_clear_scheme_handler_factories');

      cef_stream_reader_create_for_file       := GetProcAddress(FLibHandle, 'cef_stream_reader_create_for_file');
      cef_stream_reader_create_for_data       := GetProcAddress(FLibHandle, 'cef_stream_reader_create_for_data');
      cef_stream_reader_create_for_handler    := GetProcAddress(FLibHandle, 'cef_stream_reader_create_for_handler');
      cef_stream_writer_create_for_file       := GetProcAddress(FLibHandle, 'cef_stream_writer_create_for_file');
      cef_stream_writer_create_for_handler    := GetProcAddress(FLibHandle, 'cef_stream_writer_create_for_handler');

      cef_task_runner_get_for_current_thread  := GetProcAddress(FLibHandle, 'cef_task_runner_get_for_current_thread');
      cef_task_runner_get_for_thread          := GetProcAddress(FLibHandle, 'cef_task_runner_get_for_thread');

      cef_currently_on                        := GetProcAddress(FLibHandle, 'cef_currently_on');
      cef_post_task                           := GetProcAddress(FLibHandle, 'cef_post_task');
      cef_post_delayed_task                   := GetProcAddress(FLibHandle, 'cef_post_delayed_task');

      cef_begin_tracing                       := GetProcAddress(FLibHandle, 'cef_begin_tracing');
      cef_end_tracing                         := GetProcAddress(FLibHandle, 'cef_end_tracing');
      cef_now_from_system_trace_time          := GetProcAddress(FLibHandle, 'cef_now_from_system_trace_time');

      cef_urlrequest_create                   := GetProcAddress(FLibHandle, 'cef_urlrequest_create');

      cef_parse_url                           := GetProcAddress(FLibHandle, 'cef_parse_url');
      cef_create_url                          := GetProcAddress(FLibHandle, 'cef_create_url');
      cef_get_mime_type                       := GetProcAddress(FLibHandle, 'cef_get_mime_type');
      cef_get_extensions_for_mime_type        := GetProcAddress(FLibHandle, 'cef_get_extensions_for_mime_type');

      cef_register_extension                  := GetProcAddress(FLibHandle, 'cef_register_extension');
      cef_v8context_get_current_context       := GetProcAddress(FLibHandle, 'cef_v8context_get_current_context');
      cef_v8context_get_entered_context       := GetProcAddress(FLibHandle, 'cef_v8context_get_entered_context');
      cef_v8context_in_context                := GetProcAddress(FLibHandle, 'cef_v8context_in_context');
      cef_v8value_create_undefined            := GetProcAddress(FLibHandle, 'cef_v8value_create_undefined');
      cef_v8value_create_null                 := GetProcAddress(FLibHandle, 'cef_v8value_create_null');
      cef_v8value_create_bool                 := GetProcAddress(FLibHandle, 'cef_v8value_create_bool');
      cef_v8value_create_int                  := GetProcAddress(FLibHandle, 'cef_v8value_create_int');
      cef_v8value_create_uint                 := GetProcAddress(FLibHandle, 'cef_v8value_create_uint');
      cef_v8value_create_double               := GetProcAddress(FLibHandle, 'cef_v8value_create_double');
      cef_v8value_create_date                 := GetProcAddress(FLibHandle, 'cef_v8value_create_date');
      cef_v8value_create_string               := GetProcAddress(FLibHandle, 'cef_v8value_create_string');
      cef_v8value_create_object               := GetProcAddress(FLibHandle, 'cef_v8value_create_object');
      cef_v8value_create_array                := GetProcAddress(FLibHandle, 'cef_v8value_create_array');
      cef_v8value_create_function             := GetProcAddress(FLibHandle, 'cef_v8value_create_function');
      cef_v8stack_trace_get_current           := GetProcAddress(FLibHandle, 'cef_v8stack_trace_get_current');

      cef_binary_value_create                 := GetProcAddress(FLibHandle, 'cef_binary_value_create');
      cef_dictionary_value_create             := GetProcAddress(FLibHandle, 'cef_dictionary_value_create');
      cef_list_value_create                   := GetProcAddress(FLibHandle, 'cef_list_value_create');

      cef_visit_web_plugin_info               := GetProcAddress(FLibHandle, 'cef_visit_web_plugin_info');
      cef_refresh_web_plugins                 := GetProcAddress(FLibHandle, 'cef_refresh_web_plugins');
      cef_add_web_plugin_path                 := GetProcAddress(FLibHandle, 'cef_add_web_plugin_path');
      cef_add_web_plugin_directory            := GetProcAddress(FLibHandle, 'cef_add_web_plugin_directory');
      cef_remove_web_plugin_path              := GetProcAddress(FLibHandle, 'cef_remove_web_plugin_path');
      cef_unregister_internal_web_plugin      := GetProcAddress(FLibHandle, 'cef_unregister_internal_web_plugin');
      cef_force_web_plugin_shutdown           := GetProcAddress(FLibHandle, 'cef_force_web_plugin_shutdown');
      cef_register_web_plugin_crash           := GetProcAddress(FLibHandle, 'cef_register_web_plugin_crash');
      cef_is_web_plugin_unstable              := GetProcAddress(FLibHandle, 'cef_is_web_plugin_unstable');

      cef_xml_reader_create                   := GetProcAddress(FLibHandle, 'cef_xml_reader_create');

      cef_zip_reader_create                   := GetProcAddress(FLibHandle, 'cef_zip_reader_create');

      cef_string_map_alloc                    := GetProcAddress(FLibHandle, 'cef_string_map_alloc');
      cef_string_map_size                     := GetProcAddress(FLibHandle, 'cef_string_map_size');
      cef_string_map_find                     := GetProcAddress(FLibHandle, 'cef_string_map_find');
      cef_string_map_key                      := GetProcAddress(FLibHandle, 'cef_string_map_key');
      cef_string_map_value                    := GetProcAddress(FLibHandle, 'cef_string_map_value');
      cef_string_map_append                   := GetProcAddress(FLibHandle, 'cef_string_map_append');
      cef_string_map_clear                    := GetProcAddress(FLibHandle, 'cef_string_map_clear');
      cef_string_map_free                     := GetProcAddress(FLibHandle, 'cef_string_map_free');

      cef_string_list_alloc                   := GetProcAddress(FLibHandle, 'cef_string_list_alloc');
      cef_string_list_size                    := GetProcAddress(FLibHandle, 'cef_string_list_size');
      cef_string_list_value                   := GetProcAddress(FLibHandle, 'cef_string_list_value');
      cef_string_list_append                  := GetProcAddress(FLibHandle, 'cef_string_list_append');
      cef_string_list_clear                   := GetProcAddress(FLibHandle, 'cef_string_list_clear');
      cef_string_list_free                    := GetProcAddress(FLibHandle, 'cef_string_list_free');
      cef_string_list_copy                    := GetProcAddress(FLibHandle, 'cef_string_list_copy');

      cef_string_multimap_alloc               := GetProcAddress(FLibHandle, 'cef_string_multimap_alloc');
      cef_string_multimap_size                := GetProcAddress(FLibHandle, 'cef_string_multimap_size');
      cef_string_multimap_find_count          := GetProcAddress(FLibHandle, 'cef_string_multimap_find_count');
      cef_string_multimap_enumerate           := GetProcAddress(FLibHandle, 'cef_string_multimap_enumerate');
      cef_string_multimap_key                 := GetProcAddress(FLibHandle, 'cef_string_multimap_key');
      cef_string_multimap_value               := GetProcAddress(FLibHandle, 'cef_string_multimap_value');
      cef_string_multimap_append              := GetProcAddress(FLibHandle, 'cef_string_multimap_append');
      cef_string_multimap_clear               := GetProcAddress(FLibHandle, 'cef_string_multimap_clear');
      cef_string_multimap_free                := GetProcAddress(FLibHandle, 'cef_string_multimap_free');

      cef_build_revision                      := GetProcAddress(FLibHandle, 'cef_build_revision');

      cef_string_wide_set             := GetProcAddress(FLibHandle, 'cef_string_wide_set');
      cef_string_utf8_set             := GetProcAddress(FLibHandle, 'cef_string_utf8_set');
      cef_string_utf16_set            := GetProcAddress(FLibHandle, 'cef_string_utf16_set');
      cef_string_wide_clear           := GetProcAddress(FLibHandle, 'cef_string_wide_clear');
      cef_string_utf8_clear           := GetProcAddress(FLibHandle, 'cef_string_utf8_clear');
      cef_string_utf16_clear          := GetProcAddress(FLibHandle, 'cef_string_utf16_clear');
      cef_string_wide_cmp             := GetProcAddress(FLibHandle, 'cef_string_wide_cmp');
      cef_string_utf8_cmp             := GetProcAddress(FLibHandle, 'cef_string_utf8_cmp');
      cef_string_utf16_cmp            := GetProcAddress(FLibHandle, 'cef_string_utf16_cmp');
      cef_string_wide_to_utf8         := GetProcAddress(FLibHandle, 'cef_string_wide_to_utf8');
      cef_string_utf8_to_wide         := GetProcAddress(FLibHandle, 'cef_string_utf8_to_wide');
      cef_string_wide_to_utf16        := GetProcAddress(FLibHandle, 'cef_string_wide_to_utf16');
      cef_string_utf16_to_wide        := GetProcAddress(FLibHandle, 'cef_string_utf16_to_wide');
      cef_string_utf8_to_utf16        := GetProcAddress(FLibHandle, 'cef_string_utf8_to_utf16');
      cef_string_utf16_to_utf8        := GetProcAddress(FLibHandle, 'cef_string_utf16_to_utf8');
      cef_string_ascii_to_wide        := GetProcAddress(FLibHandle, 'cef_string_ascii_to_wide');
      cef_string_ascii_to_utf16       := GetProcAddress(FLibHandle, 'cef_string_ascii_to_utf16');
      cef_string_userfree_wide_alloc  := GetProcAddress(FLibHandle, 'cef_string_userfree_wide_alloc');
      cef_string_userfree_utf8_alloc  := GetProcAddress(FLibHandle, 'cef_string_userfree_utf8_alloc');
      cef_string_userfree_utf16_alloc := GetProcAddress(FLibHandle, 'cef_string_userfree_utf16_alloc');
      cef_string_userfree_wide_free   := GetProcAddress(FLibHandle, 'cef_string_userfree_wide_free');
      cef_string_userfree_utf8_free   := GetProcAddress(FLibHandle, 'cef_string_userfree_utf8_free');
      cef_string_userfree_utf16_free  := GetProcAddress(FLibHandle, 'cef_string_userfree_utf16_free');

      {$IFDEF CEF_STRING_TYPE_UTF8}
      cef_string_set            := cef_string_utf8_set;
      cef_string_clear          := cef_string_utf8_clear;
      cef_string_userfree_alloc := cef_string_userfree_utf8_alloc;
      cef_string_userfree_free  := cef_string_userfree_utf8_free;
      cef_string_from_ascii     := cef_string_utf8_copy;
      cef_string_to_utf8        := cef_string_utf8_copy;
      cef_string_from_utf8      := cef_string_utf8_copy;
      cef_string_to_utf16       := cef_string_utf8_to_utf16;
      cef_string_from_utf16     := cef_string_utf16_to_utf8;
      cef_string_to_wide        := cef_string_utf8_to_wide;
      cef_string_from_wide      := cef_string_wide_to_utf8;
      {$ENDIF}

      {$IFDEF CEF_STRING_TYPE_UTF16}
      cef_string_set            := cef_string_utf16_set;
      cef_string_clear          := cef_string_utf16_clear;
      cef_string_userfree_alloc := cef_string_userfree_utf16_alloc;
      cef_string_userfree_free  := cef_string_userfree_utf16_free;
      cef_string_from_ascii     := cef_string_ascii_to_utf16;
      cef_string_to_utf8        := cef_string_utf16_to_utf8;
      cef_string_from_utf8      := cef_string_utf8_to_utf16;
      cef_string_to_utf16       := @cef_string_utf16_copy;
      cef_string_from_utf16     := @cef_string_utf16_copy;
      cef_string_to_wide        := cef_string_utf16_to_wide;
      cef_string_from_wide      := cef_string_wide_to_utf16;
      {$ENDIF}

      {$IFDEF CEF_STRING_TYPE_WIDE}
      cef_string_set            := cef_string_wide_set;
      cef_string_clear          := cef_string_wide_clear;
      cef_string_userfree_alloc := cef_string_userfree_wide_alloc;
      cef_string_userfree_free  := cef_string_userfree_wide_free;
      cef_string_from_ascii     := cef_string_ascii_to_wide;
      cef_string_to_utf8        := cef_string_wide_to_utf8;
      cef_string_from_utf8      := cef_string_utf8_to_wide;
      cef_string_to_utf16       := cef_string_wide_to_utf16;
      cef_string_from_utf16     := cef_string_utf16_to_wide;
      cef_string_to_wide        := cef_string_wide_copy;
      cef_string_from_wide      := cef_string_wide_copy;
      {$ENDIF}
    {$ENDIF}
    Result:= true;
  end
  else
    Result := true;
end;

class procedure TWACef.StartApp;
var
  ErrCode: Integer;
begin
  if not FAppStarted then
  begin
    if not LoadLib then
      Exit;
    FApp := TInternalApp.Create;
    {$IFDEF MSWINDOWS}
    FArgs.instance := HINSTANCE;
    {$ELSE}
    FArgs.argc := argc;
    FArgs.argv := argv;
    {$ENDIF}
    ErrCode := cef_execute_process(@FArgs, GetData(FApp), nil);
    If ErrCode >= 0 then
      Halt(ErrCode);
    FAppStarted := true;
  end;
end;

class procedure TWACef.Initialize;
var
  Settings: TCefSettings;
  ErrCode: Integer;
begin
  If not FInitialized then
  begin
    // deactivate FPU exception
    Set8087CW(Get8087CW or $3F);
    SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
    if not LoadLib then
      Exit;
    FillChar(settings, SizeOf(settings), 0);
    settings.size := SizeOf(settings);
    settings.single_process := Ord(CefSingleProcess);
    settings.browser_subprocess_path := ToCefString(CefBrowserSubprocessPath);
    {$IFDEF CEF_MULTI_THREADED_MESSAGE_LOOP}
    settings.multi_threaded_message_loop := True;
    {$ELSE}
    settings.multi_threaded_message_loop := Ord(False);
    {$ENDIF}
    settings.command_line_args_disabled := Ord(CefCommandLineArgsDisabled);
    settings.cache_path := ToCefString(CefCachePath);
    settings.persist_session_cookies := Ord(CefPersistSessionCookies);
    settings.user_agent := ToCefString(CefUserAgent);
    settings.product_version := ToCefString(CefProductVersion);
    settings.locale := ToCefString(CefLocale);
    settings.log_file := ToCefString(CefLogFile);
    settings.log_severity := CefLogSeverity;
    settings.javascript_flags := ToCefString(CefJavaScriptFlags);
    settings.resources_dir_path := ToCefString(CefResourcesDirPath);
    settings.locales_dir_path := ToCefString(CefLocalesDirPath);
    settings.pack_loading_disabled := Ord(CefPackLoadingDisabled);
    settings.remote_debugging_port := CefRemoteDebuggingPort;
    settings.uncaught_exception_stack_size := CefUncaughtExceptionStackSize;
    settings.context_safety_implementation := Ord(CefContextSafetyImplementation);
    settings.ignore_certificate_errors := Ord(CefIgnoreCertificateErrors);
    settings.background_color := CefBackgroundColor;
    FInitialized := true;
    StartApp;
    ErrCode := cef_initialize(@FArgs, @settings, GetData(FApp), nil);
    If ErrCode <> 1 then
      Exit;
    FIsMainProcess := True;
  end;
end;

class procedure TWACef.Finalize;
begin
  If FInitialized then
  begin
    {$IFNDEF CEF_MULTI_THREADED_MESSAGE_LOOP}
    if FIsMainProcess then
      cef_shutdown;
    {$ENDIF}
    FreeLibrary(FLibHandle);
    FLibHandle := 0;
    FInitialized := false;
  end;
end;

class function TWACef.GetObject(ptr: Pointer): TObject;
begin
  Dec(PByte(ptr), SizeOf(Pointer));
  Result := TObject(ptr^);
end;

class function TWACef.GetData(const i: ICefBase): Pointer;
begin
  if i <> nil then
    Result := i.Wrap else
    Result := nil;
end;

class function TWACef.ColorSetARGB(a, r, g, b : Byte) : TCefColor;
begin
  Result :=
    (a shl 24) or
    (r shl 16) or
    (g shl 8) or
    (b shl 0);
end;

//..............................................................................String methods
class function TWACef.StringFreeAndGet(const str: PCefStringUserFree): ustring;
begin
  If str <> nil then
  begin
    Result := ToString(PCefString(str));
    cef_string_userfree_free(str);
  end
  Else Result := '';
end;

class function TWACef.ToCefString(const str: ustring): TCefString;
begin
  Result.str := PChar16(PWideChar(str));
  Result.length := Length(str);
  Result.dtor := nil;
end;

class function TWACef.ToString(const str: PCefString): ustring;
begin
  If str <> nil then
    SetString(Result, str^.str, str^.length)
  Else
    Result := '';
end;

class function TWACef.StringClearAndGet(var str: TCefString): ustring;
begin
  Result := ToString(@str);
  cef_string_clear(@str);
end;

class function TWACef.StringAlloc(const str: ustring): TCefString;
begin
  FillChar(Result, SizeOf(Result), 0);
  If str <> '' then
    cef_string_from_wide(PWideChar(str), Length(str), @Result);
end;

class procedure TWACef.StringFree(const str: PCefString);
begin
  If str <> nil then
    cef_string_clear(str);
end;

procedure _free_string(str: PChar16);{$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  If str <> nil then
    FreeMem(str);
end;

class function TWACef.UserFreeString(const str: ustring): PCefStringUserFree;
begin
  Result := cef_string_userfree_alloc();
  Result^.length := Length(str);
  Result^.dtor := @_free_string;

  GetMem(Result^.str, Result^.length * SizeOf(TCefChar));
  Move(PCefChar(str)^, Result^.str^, Result^.length * SizeOf(TCefChar));
end;

class procedure TWACef.StringSet(const str: PCefString; const value: ustring);
begin
  If str <> nil then
    cef_string_set(PWideChar(value), Length(value), str, 1);
end;

//..............................................................................Date and time methods
{$IFDEF MSWINDOWS}
class function TWACef.CefTimeToSystemTime(const dt: TCefTime): TSystemTime;
begin
  Result.wYear := dt.year;
  Result.wMonth := dt.month;
  Result.wDayOfWeek := dt.day_of_week;
  Result.wDay := dt.day_of_month;
  Result.wHour := dt.hour;
  Result.wMinute := dt.minute;
  Result.wSecond := dt.second;
  Result.wMilliseconds := dt.millisecond;
end;

class function TWACef.SystemTimeToCefTime(const dt: TSystemTime): TCefTime;
begin
  Result.year := dt.wYear;
  Result.month := dt.wMonth;
  Result.day_of_week := dt.wDayOfWeek;
  Result.day_of_month := dt.wDay;
  Result.hour := dt.wHour;
  Result.minute := dt.wMinute;
  Result.second := dt.wSecond;
  Result.millisecond := dt.wMilliseconds;
end;

class function TWACef.CefTimeToDateTime(const dt: TCefTime): TDateTime;
var
  st: TSystemTime;
begin
  Result:=0;
  try
    st := CefTimeToSystemTime(dt);
    SystemTimeToTzSpecificLocalTime(nil, st, st);
    Result := SystemTimeToDateTime(st);
  finally
  end;
end;

class function TWACef.DateTimeToCefTime(dt: TDateTime): TCefTime;
var
  st: TSystemTime;
begin
  DateTimeToSystemTime(dt, st);
  TzSpecificLocalTimeToSystemTime(nil, @st, @st);
  Result := SystemTimeToCefTime(st);
end;

class function TWACef.CefTimeToSQLInt(const ct: TCefTime): Int64;
var
  st:TSystemTime;
  ft:TFileTime;
  ularge:ULARGE_INTEGER;
begin
  st:=CefTimeToSystemTime(ct);
  SystemTimeToFileTime(st,ft);
  ularge.LowPart:=ft.dwLowDateTime;
  ularge.HighPart:=ft.dwHighDateTime;
  result:=round(ularge.QuadPart/10);
end;

class function TWACef.SQLIntToCefTime(const si: Int64): TCefTime;
var
  st:TSystemTime;
  ft:TFileTime;
  ularge:ULARGE_INTEGER;
begin
  ularge.QuadPart := si * 10;
  ft.dwHighDateTime := ularge.HighPart;
  ft.dwLowDateTime := ularge.LowPart;
  FileTimeToSystemTime(ft, st);
  result := SystemTimeToCefTime(st);
end;

{$ELSE}

class function TWACef.CefTimeToDateTime(const dt: TCefTime): TDateTime;
begin
  Result:=EncodeDate(dt.year, dt.month, dt.day_of_month) + EncodeTime(dt.hour, dt.minute, dt.second, dt.millisecond);
end;

class function TWACef.DateTimeToCefTime(dt: TDateTime): TCefTime;
Var
  Year, Month, Day, Hour, Min, Sec, MSec: Word;
begin
  DecodeDate(dt, Year, Month, Day);
  DecodeTime(dt, Hour, Min, Sec, MSec);

  With Result do
  begin
    year := Year;
    month := Month;
    day_of_week := DayOfWeek(dt);
    day_of_month := Month;
    hour := Hour;
    minute := Min;
    second := Sec;
    millisecond := MSec;
  end;
end;
{$ENDIF}
//..............................................................................Message loop
{$IFNDEF CEF_MULTI_THREADED_MESSAGE_LOOP}
class procedure TWACef.DoMessageLoopWork;
begin
  cef_do_message_loop_work();
end;

class procedure TWACef.RunMessageLoop;
begin
  cef_run_message_loop();
end;

class procedure TWACef.QuitMessageLoop;
begin
  cef_quit_message_loop();
end;
{$ENDIF}

class function TWACef.BrowserHostCreateSync(windowInfo: PCefWindowInfo; const client: ICefClient;
  const url: ustring; const settings: PCefBrowserSettings; const requestContext:ICefRequestContext): ICefBrowser;
var
  u: TCefString;
  data:Pointer;
begin
  Initialize;
  u := ToCefString(url);
  data := cef_browser_host_create_browser_sync(windowInfo, GetData(client), @u, settings, GetData(requestContext));
  Result := TCefBrowserRef.UnWrap(data);
end;

class function TWACef.BrowserHostCreateBrowser(windowInfo: PCefWindowInfo; const client: ICefClient;
  const url: ustring; const settings: PCefBrowserSettings; const requestContext:ICefRequestContext): boolean;
var
  u: TCefString;
begin
  Initialize;
  u := ToCefString(url);
  Result := cef_browser_host_create_browser(windowInfo, GetData(client), @u, settings, GetData(requestContext)) <> 0;
end;

class function TWACef.GetGeolocation(const callback: ICefGetGeolocationCallback): Boolean;
begin
  Result := cef_get_geolocation(GetData(callback)) <> 0;
end;

class function TWACef.AddCrossOriginWhitelistEntry(const SourceOrigin, TargetProtocol,
  TargetDomain: ustring; AllowTargetSubdomains: Boolean): Boolean;
var
  so, tp, td: TCefString;
begin
  Initialize;
  so := ToCefString(SourceOrigin);
  tp := ToCefString(TargetProtocol);
  td := ToCefString(TargetDomain);

  If TargetDomain <> '' then
    Result := cef_add_cross_origin_whitelist_entry(@so, @tp, @td, Ord(AllowTargetSubdomains)) <> 0
  Else
    Result := cef_add_cross_origin_whitelist_entry(@so, @tp, nil, Ord(AllowTargetSubdomains)) <> 0;
end;

class function TWACef.RemoveCrossOriginWhitelistEntry(
  const SourceOrigin, TargetProtocol, TargetDomain: ustring;
  AllowTargetSubdomains: Boolean): Boolean;
var
  so, tp, td: TCefString;
begin
  Initialize;
  so := ToCefString(SourceOrigin);
  tp := ToCefString(TargetProtocol);
  td := ToCefString(TargetDomain);
  Result := cef_remove_cross_origin_whitelist_entry(@so, @tp, @td, Ord(AllowTargetSubdomains)) <> 0;
end;

class function TWACef.ClearCrossOriginWhitelist: Boolean;
begin
  Initialize;
  Result := cef_clear_cross_origin_whitelist() <> 0;
end;

class function TWACef.GetPath(key: TCefPathKey; out path: ustring): Boolean;
var
  p: TCefString;
begin
  p := ToCefString('');
  Result := cef_get_path(key, @p) <> 0;
  path := StringClearAndGet(p);
end;

class function TWACef.RegisterSchemeHandlerFactory(const SchemeName, HostName: ustring;
  SyncMainThread: Boolean; const handler: TCefResourceHandlerClass): Boolean;
var
  s, h: TCefString;
begin
  Initialize;
  s := ToCefString(SchemeName);
  h := ToCefString(HostName);
  Result := cef_register_scheme_handler_factory(
    @s,
    @h,
    GetData(TCefSchemeHandlerFactoryOwn.Create(handler, SyncMainThread) as ICefBase)
  ) <> 0;
end;

class function TWACef.ClearSchemeHandlerFactories: Boolean;
begin
  Initialize;
  Result := cef_clear_scheme_handler_factories() <> 0;
end;

class function TWACef.NowFromSystemTraceTime: cint64;
begin
  Result := cef_now_from_system_trace_time();
end;

class function TWACef.ParseUrl(const url: ustring; var parts: TUrlParts): Boolean;
var
  u: TCefString;
  p: TCefUrlParts;
begin
  FillChar(p, sizeof(p), 0);
  u := ToCefString(url);
  Result := cef_parse_url(@u, @p) <> 0;
  if Result then
  begin
    parts.scheme := ToString(@p.scheme);
    parts.username := ToString(@p.username);
    parts.password := ToString(@p.password);
    parts.host := ToString(@p.host);
    parts.port := ToString(@p.port);
    parts.path := ToString(@p.path);
    parts.query := ToString(@p.query);
  end;
end;

class function TWACef.CreateUrl(var parts: TUrlParts): ustring;
var
  p: TCefUrlParts;
  u: TCefString;
begin
  FillChar(p, sizeof(p), 0);

  p.spec := ToCefString(parts.spec);
  p.scheme := ToCefString(parts.scheme);
  p.username := ToCefString(parts.username);
  p.password := ToCefString(parts.password);
  p.host := ToCefString(parts.host);
  p.port := ToCefString(parts.port);
  p.path := ToCefString(parts.path);
  p.query := ToCefString(parts.query);

  FillChar(u, SizeOf(u), 0);
  If cef_create_url(@p, @u) <> 0 then
    Result := ToString(@u)
  Else
    Result := '';
end;

class procedure TWAcef.GetExtensionsForMimeType(const MimeType: ustring; Extensions: TStrings);
var
  list: TCefStringList;
  m, str: TCefString;
  i: integer;
begin
  m := ToCefString(MimeType);
  list := cef_string_list_alloc();
  try
//    cef_get_extensions_for_mime_type(@m, list);
    FillChar(str, SizeOf(str), 0);
    for i := 0 to cef_string_list_size(list) - 1 do
    begin
      cef_string_list_value(list, i, @str);
      Extensions.Add(StringClearAndGet(str));
    end;
  finally
    cef_string_list_free(list);
  end;
end;

class function TWACef.RegisterExtension(const name: ustring; const code: ustring; const Handler: ICefv8Handler): boolean;
var
  n, c: TCefString;
begin
  Initialize;
  n := ToCefString(name);
  c := ToCefString(code);
  Result := cef_register_extension(@n, @c, GetData(handler)) <> 0;
end;

class procedure TWACef.VisitWebPluginInfo(const visitor: ICefWebPluginInfoVisitor);
begin
  cef_visit_web_plugin_info(GetData(visitor));
end;

class procedure TWACef.VisitWebPluginInfoProc(const visitor: TCefWebPluginInfoVisitorProc);
begin
  VisitWebPluginInfo(TCefFastWebPluginInfoVisitor.Create(visitor));
end;

class procedure TWACef.RefreshWebPlugins;
begin
  cef_refresh_web_plugins();
end;

class procedure TWACef.AddWebPluginPath(const path: ustring);
var
  p: TCefString;
begin
  p := ToCefString(path);
  cef_add_web_plugin_path(@p);
end;

class procedure TWACef.AddWebPluginDirectory(const dir: ustring);
var
  d: TCefString;
begin
  d := ToCefString(dir);
  cef_add_web_plugin_directory(@d);
end;

class procedure TWACef.RemoveWebPluginPath(const path: ustring);
var
  p: TCefString;
begin
  p := ToCefString(path);
  cef_remove_web_plugin_path(@p);
end;

class procedure TWACef.UnregisterInternalWebPlugin(const path: ustring);
var
  p: TCefString;
begin
  p := ToCefString(path);
  cef_unregister_internal_web_plugin(@p);
end;

class procedure TWACef.ForceWebPluginShutdown(const path: ustring);
var
  p: TCefString;
begin
  p := ToCefString(path);
  cef_force_web_plugin_shutdown(@p);
end;

class procedure TWACef.RegisterWebPluginCrash(const path: ustring);
var
  p: TCefString;
begin
  p := ToCefString(path);
  cef_register_web_plugin_crash(@p);
end;

class procedure TWACef.IsWebPluginUnstable(const path: ustring; const callback: ICefWebPluginUnstableCallback);
var
  p: TCefString;
begin
  p := ToCefString(path);
  cef_is_web_plugin_unstable(@p, GetData(callback));
end;

class procedure TWACef.IsWebPluginUnstableProc(const path: ustring; const callback: TCefWebPluginIsUnstableProc);
begin
  IsWebPluginUnstable(path, TCefFastWebPluginUnstableCallback.Create(callback));
end;

initialization

TWACef.FAppStarted := false;
TWACef.FLibHandle := 0;
TWACef.FIsMainProcess := False;
TWACef.FInitialized := False;

finalization

TWACef.Finalize;

end.
