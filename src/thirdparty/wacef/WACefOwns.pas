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
  TCefBaseOwn = class(TInterfacedObject, ICefBase)
  private
    FData: Pointer;
  public
    function Wrap: Pointer;
    constructor CreateData(size: csize_t; owned: Boolean = False); virtual;
    destructor Destroy; override;
  end;

  TCefClientOwn = class(TCefBaseOwn, ICefClient)
  protected
    function GetContextMenuHandler: ICefContextMenuHandler; virtual;
    function GetDialogHandler: ICefDialogHandler; virtual;
    function GetDisplayHandler: ICefDisplayHandler; virtual;
    function GetDownloadHandler: ICefDownloadHandler; virtual;
    function GetDragHandler: ICefDragHandler; virtual;
    function GetFocusHandler: ICefFocusHandler; virtual;
    function GetGeolocationHandler: ICefGeolocationHandler; virtual;
    function GetJsdialogHandler: ICefJsdialogHandler; virtual;
    function GetKeyboardHandler: ICefKeyboardHandler; virtual;
    function GetLifeSpanHandler: ICefLifeSpanHandler; virtual;
    function GetLoadHandler: ICefLoadHandler; virtual;
    function GetRenderHandler: ICefRenderHandler; virtual;
    function GetRequestHandler: ICefRequestHandler; virtual;
    function OnProcessMessageReceived(const browser: ICefBrowser;
      sourceProcess: TCefProcessId; const message: ICefProcessMessage): Boolean; virtual;
  public
    constructor Create; virtual;
  end;

  TCefGeolocationHandlerOwn = class(TCefBaseOwn, ICefGeolocationHandler)
  protected
    procedure OnRequestGeolocationPermission(const browser: ICefBrowser;
      const requestingUrl: ustring; requestId: cint;
      const callback: ICefGeolocationCallback); virtual;
    procedure OnCancelGeolocationPermission(const browser: ICefBrowser;
      const requestingUrl: ustring; requestId: cint); virtual;
  public
    constructor Create; virtual;
  end;

  TCefLifeSpanHandlerOwn = class(TCefBaseOwn, ICefLifeSpanHandler)
  protected
    function OnBeforePopup(const parentBrowser: ICefBrowser; const frame: ICefFrame;
      var TargetURL: ustring; const TargetFrameName: ustring;
      var popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo;
      var client: ICefClient; var settings: TCefBrowserSettings; var NoJavascriptAccess:boolean): Boolean; virtual;
    procedure OnAfterCreated(const browser: ICefBrowser); virtual;
    procedure OnBeforeClose(const browser: ICefBrowser); virtual;
    function RunModal(const browser: ICefBrowser): Boolean; virtual;
    function DoClose(const browser: ICefBrowser): Boolean; virtual;
  public
    constructor Create; virtual;
  end;

  TCefLoadHandlerOwn = class(TCefBaseOwn, ICefLoadHandler)
  protected
    procedure OnLoadingStateChange(const browser: ICefBrowser; IsLoading:boolean; CanGoBack:boolean; CanGoForward:boolean); virtual;
    procedure OnLoadStart(const browser: ICefBrowser; const frame: ICefFrame); virtual;
    procedure OnLoadEnd(const browser: ICefBrowser; const frame: ICefFrame; httpStatusCode: cint); virtual;
    procedure OnLoadError(const browser: ICefBrowser; const frame: ICefFrame; errorCode: TCefErrorCode;
      const errorText, failedUrl: ustring); virtual;
  public
    constructor Create; virtual;
  end;

  TCefRequestHandlerOwn = class(TCefBaseOwn, ICefRequestHandler)
  protected
    function OnBeforeBrowse(const browser: ICefBrowser; const frame: ICefFrame;
      const request: ICefRequest; IsRedirect:boolean): Boolean; virtual;
    function OnBeforeResourceLoad(const browser: ICefBrowser; const frame: ICefFrame;
      const request: ICefRequest): Boolean; virtual;
    function GetResourceHandler(const browser: ICefBrowser; const frame: ICefFrame;
      const request: ICefRequest): ICefResourceHandler; virtual;
    procedure OnResourceRedirect(const browser: ICefBrowser; const frame: ICefFrame;
      const oldUrl: ustring; var newUrl: ustring); virtual;
    function GetAuthCredentials(const browser: ICefBrowser; const frame: ICefFrame;
      isProxy: Boolean; const host: ustring; port: cint; const realm, scheme: ustring;
      const callback: ICefAuthCallback): Boolean; virtual;
    function OnQuotaRequest(const browser: ICefBrowser; const originUrl: ustring;
      newSize: Int64; const callback: ICefQuotaCallback): Boolean; virtual;
    function OnCertificateError(CertError: TCefErrorcode; const RequestUrl: ustring;
      const Callback: ICefAllowCertificateErrorCallback): boolean; virtual;
    procedure OnProtocolExecution(const browser: ICefBrowser; const url: ustring; out allowOsExecution: Boolean); virtual;
    function OnBeforePluginLoad(const browser: ICefBrowser; const url, policyUrl: ustring;
      const info: ICefWebPluginInfo): Boolean; virtual;
    procedure OnPluginCrashed(const browser: ICefBrowser; const pluginPath: ustring); virtual;
    procedure OnRenderProcessTerminated(const browser: ICefBrowser; status: TCefTerminationStatus); virtual;
  public
    constructor Create; virtual;
  end;

  TCefContextMenuHandlerOwn = class(TCefBaseOwn, ICefContextMenuHandler)
  protected
    procedure OnBeforeContextMenu(const browser: ICefBrowser; const frame: ICefFrame;
      const params: ICefContextMenuParams; const model: ICefMenuModel); virtual;
    function OnContextMenuCommand(const browser: ICefBrowser; const frame: ICefFrame;
      const params: ICefContextMenuParams; commandId: cint;
      eventFlags: TCefEventFlags): Boolean; virtual;
    procedure OnContextMenuDismissed(const browser: ICefBrowser; const frame: ICefFrame); virtual;
  public
    constructor Create; virtual;
  end;

  TCefStringVisitorOwn = class(TCefBaseOwn, ICefStringVisitor)
  protected
    procedure Visit(const str: ustring); virtual;
  public
    constructor Create; virtual;
  end;

  TCefFastStringVisitor = class(TCefStringVisitorOwn, ICefStringVisitor)
  private
    FVisit: TCefStringVisitorProc;
  protected
    procedure Visit(const str: ustring); override;
  public
    constructor Create(const callback: TCefStringVisitorProc); reintroduce;
  end;

  TCefV8AccessorOwn = class(TCefBaseOwn, ICefV8Accessor)
  protected
    function Get(const name: ustring; const obj: ICefv8Value;
      out value: ICefv8Value; const exception: ustring): Boolean; virtual;
    function Put(const name: ustring; const obj, value: ICefv8Value;
      const exception: ustring): Boolean; virtual;
  public
    constructor Create; virtual;
  end;

  TCefV8ExceptionOwn = class(TCefBaseOwn, ICefV8Exception)
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
    function GetMessage: ustring; virtual;
    function GetSourceLine: ustring; virtual;
    function GetScriptResourceName: ustring; virtual;
    function GetLineNumber: cint; virtual;
    function GetStartPosition: cint; virtual;
    function GetEndPosition: cint; virtual;
    function GetStartColumn: cint; virtual;
    function GetEndColumn: cint; virtual;
  public
    constructor Create; virtual;
  end;

  TCefDomVisitorOwn = class(TCefBaseOwn, ICefDomVisitor)
  protected
    procedure visit(const document: ICefDomDocument); virtual;
  public
    constructor Create; virtual;
  end;

  TCefFastDomVisitor = class(TCefDomVisitorOwn)
  private
    FProc: TCefDomVisitorProc;
  protected
    procedure visit(const document: ICefDomDocument); override;
  public
    constructor Create(const proc: TCefDomVisitorProc); reintroduce; virtual;
  end;

  TCefDisplayHandlerOwn = class(TCefBaseOwn, ICefDisplayHandler)
  protected
    procedure OnLoadingStateChange(const browser: ICefBrowser; isLoading, canGoBack, canGoForward: Boolean); virtual;
    procedure OnAddressChange(const browser: ICefBrowser; const frame: ICefFrame; const url: ustring); virtual;
    procedure OnTitleChange(const browser: ICefBrowser; const title: ustring); virtual;
    function OnTooltip(const browser: ICefBrowser; var text: ustring): Boolean; virtual;
    procedure OnStatusMessage(const browser: ICefBrowser; const value: ustring); virtual;
    function OnConsoleMessage(const browser: ICefBrowser; const message, source: ustring; line: cint): Boolean; virtual;
  public
    constructor Create; virtual;
  end;

  TCefDownloadHandlerOwn = class(TCefBaseOwn, ICefDownloadHandler)
  protected
    procedure OnBeforeDownload(const browser: ICefBrowser; const downloadItem: ICefDownloadItem;
      const suggestedName: ustring; const callback: ICefBeforeDownloadCallback); virtual;
    procedure OnDownloadUpdated(const browser: ICefBrowser; const downloadItem: ICefDownloadItem;
        const callback: ICefDownloadItemCallback); virtual;
  public
    constructor Create; virtual;
  end;

  TCefDragHandlerOwn = class(TCefBaseOwn, ICefDragHandler)
  protected
    function OnDragEnter(const browser: ICefBrowser; const dragData: ICefDragData; mask: TCefDragOperationsMask):Boolean; virtual;
  public
    constructor Create; virtual;
  end;

  TCefFocusHandlerOwn = class(TCefBaseOwn, ICefFocusHandler)
  protected
    procedure OnTakeFocus(const browser: ICefBrowser; next: Boolean); virtual;
    function OnSetFocus(const browser: ICefBrowser; source: TCefFocusSource): Boolean; virtual;
    procedure OnGotFocus(const browser: ICefBrowser); virtual;
  public
    constructor Create; virtual;
  end;

  TCefKeyboardHandlerOwn = class(TCefBaseOwn, ICefKeyboardHandler)
  protected
    function OnPreKeyEvent(const browser: ICefBrowser; const event: PCefKeyEvent;
      osEvent: TCefEventHandle; out isKeyboardShortcut: Boolean): Boolean; virtual;
    function OnKeyEvent(const browser: ICefBrowser; const event: PCefKeyEvent;
      osEvent: TCefEventHandle): Boolean; virtual;
  public
    constructor Create; virtual;
  end;

  TCefJsDialogHandlerOwn = class(TCefBaseOwn, ICefJsDialogHandler)
  protected
    function OnJsdialog(const browser: ICefBrowser; const originUrl, acceptLang: ustring;
      dialogType: TCefJsDialogType; const messageText, defaultPromptText: ustring;
      callback: ICefJsDialogCallback; out suppressMessage: Boolean): Boolean; virtual;
    function OnBeforeUnloadDialog(const browser: ICefBrowser;
      const messageText: ustring; isReload: Boolean;
      const callback: ICefJsDialogCallback): Boolean; virtual;
    procedure OnResetDialogState(const browser: ICefBrowser); virtual;
    procedure OnDialogClosed(const browser: ICefBrowser); virtual;
  public
    constructor Create; virtual;
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

  TCefPostDataElementOwn = class(TCefBaseOwn, ICefPostDataElement)
  private
    FDataType: TCefPostDataElementType;
    FValueByte: Pointer;
    FValueStr: TCefString;
    FSize: csize_t;
    FReadOnly: Boolean;
    procedure Clear;
  protected
    function IsReadOnly: Boolean; virtual;
    procedure SetToEmpty; virtual;
    procedure SetToFile(const fileName: ustring); virtual;
    procedure SetToBytes(size: csize_t; const bytes: Pointer); virtual;
    function GetType: TCefPostDataElementType; virtual;
    function GetFile: ustring; virtual;
    function GetBytesCount: csize_t; virtual;
    function GetBytes(size: csize_t; bytes: Pointer): csize_t; virtual;
  public
    constructor Create(readonly: Boolean); virtual;
  end;

  TCefCookieVisitorOwn = class(TCefBaseOwn, ICefCookieVisitor)
  protected
    function visit(const cookie: TWACefCookie; count, total: cint; out deleteCookie: Boolean): Boolean; virtual;
  public
    constructor Create; virtual;
  end;

  TCefFastCookieVisitor = class(TCefCookieVisitorOwn)
  private
    FVisitor: TCefCookieVisitorProc;
  protected
    function visit(const cookie: TWACefCookie; count, total: cint; out deleteCookie: Boolean): Boolean; override;
  public
    constructor Create(const visitor: TCefCookieVisitorProc); reintroduce;
  end;

  TCefResourceHandlerOwn = class(TCefBaseOwn, ICefResourceHandler)
  protected
    function ProcessRequest(const request: ICefRequest; const callback: ICefCallback): Boolean; virtual;
    procedure GetResponseHeaders(const response: ICefResponse;
      out responseLength: Int64; out redirectUrl: ustring); virtual;
    function ReadResponse(const dataOut: Pointer; bytesToRead: cint;
      var bytesRead: cint; const callback: ICefCallback): Boolean; virtual;
    function CanGetCookie(const cookie: PCefCookie): Boolean; virtual;
    function CanSetCookie(const cookie: PCefCookie): Boolean; virtual;
    procedure Cancel; virtual;
  public
    constructor Create(const browser: ICefBrowser; const frame: ICefFrame;
      const schemeName: ustring; const request: ICefRequest); virtual;
  end;
  TCefResourceHandlerClass = class of TCefResourceHandlerOwn;

  TCefSchemeHandlerFactoryOwn = class(TCefBaseOwn, ICefSchemeHandlerFactory)
  private
    FClass: TCefResourceHandlerClass;
  protected
    function New(const browser: ICefBrowser; const frame: ICefFrame;
      const schemeName: ustring; const request: ICefRequest): ICefResourceHandler; virtual;
  public
    constructor Create(const AClass: TCefResourceHandlerClass; SyncMainThread: Boolean); virtual;
  end;

  TCefv8HandlerOwn = class(TCefBaseOwn, ICefv8Handler)
  protected
    function Execute(const name: ustring; const obj: ICefv8Value;
      const arguments: TCefv8ValueArray; var retval: ICefv8Value;
      var exception: ustring): Boolean; virtual;
  public
    constructor Create; virtual;
  end;

  TCefTaskOwn = class(TCefBaseOwn, ICefTask)
  protected
    procedure Execute; virtual;
  public
    constructor Create; virtual;
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
    constructor Create; virtual;
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
    constructor Create; virtual;
    destructor Destroy; override;
  end;

  TCefResourceBundleHandlerOwn = class(TCefBaseOwn, ICefResourceBundleHandler)
  protected
    function GetDataResource(resourceId: cint; out data: Pointer;
      out dataSize: csize_t): Boolean; virtual; abstract;
    function GetLocalizedString(messageId: cint;
      out stringVal: ustring): Boolean; virtual; abstract;
  public
    constructor Create; virtual;
  end;

  TCefFastResourceBundle = class(TCefResourceBundleHandlerOwn)
  private
    FGetDataResource: TGetDataResource;
    FGetLocalizedString: TGetLocalizedString;
  protected
    function GetDataResource(resourceId: cint; out data: Pointer;
      out dataSize: csize_t): Boolean; override;
    function GetLocalizedString(messageId: cint;
      out stringVal: ustring): Boolean; override;
  public
    constructor Create(AGetDataResource: TGetDataResource;
      AGetLocalizedString: TGetLocalizedString); reintroduce;
  end;

  TCefAppOwn = class(TCefBaseOwn, ICefApp)
  protected
    procedure OnBeforeCommandLineProcessing(const processType: ustring; const commandLine: ICefCommandLine); virtual; abstract;
    procedure OnRegisterCustomSchemes(const registrar: ICefSchemeRegistrar); virtual; abstract;
    function GetResourceBundleHandler: ICefResourceBundleHandler; virtual; abstract;
    function GetBrowserProcessHandler: ICefBrowserProcessHandler; virtual; abstract;
    function GetRenderProcessHandler: ICefRenderProcessHandler; virtual; abstract;
  public
    constructor Create; virtual;
  end;

   TCefUrlrequestClientOwn = class(TCefBaseOwn, ICefUrlrequestClient)
  protected
    procedure OnRequestComplete(const request: ICefUrlRequest); virtual;
    procedure OnUploadProgress(const request: ICefUrlRequest; current, total: cuint64); virtual;
    procedure OnDownloadProgress(const request: ICefUrlRequest; current, total: cuint64); virtual;
    procedure OnDownloadData(const request: ICefUrlRequest; data: Pointer; dataLength: csize_t); virtual;
    function GetAuthCredentials(IsProxy: Boolean; const Host: ustring; Port: CInt; const Realm: ustring; const Scheme: ustring; Callback: ICefAuthCallback): Boolean; virtual;
  public
    constructor Create; virtual;
  end;

  TCefWebPluginInfoVisitorOwn = class(TCefBaseOwn, ICefWebPluginInfoVisitor)
  protected
    function Visit(const info: ICefWebPluginInfo; count, total: cint): Boolean; virtual;
  public
    constructor Create; virtual;
  end;

  TCefFastWebPluginInfoVisitor = class(TCefWebPluginInfoVisitorOwn)
  private
    FProc: TCefWebPluginInfoVisitorProc;
  protected
    function Visit(const info: ICefWebPluginInfo; count, total: cint): Boolean; override;
  public
    constructor Create(const proc: TCefWebPluginInfoVisitorProc); reintroduce;
  end;

  TCefWebPluginUnstableCallbackOwn = class(TCefBaseOwn, ICefWebPluginUnstableCallback)
  protected
    procedure IsUnstable(const path: ustring; unstable: Boolean); virtual;
  public
    constructor Create; virtual;
  end;

  TCefFastWebPluginUnstableCallback = class(TCefWebPluginUnstableCallbackOwn)
  private
    FCallback: TCefWebPluginIsUnstableProc;
  protected
    procedure IsUnstable(const path: ustring; unstable: Boolean); override;
  public
    constructor Create(const callback: TCefWebPluginIsUnstableProc); reintroduce;
  end;

  ECefException = class(Exception)
  end;

  TInternalApp = class(TCefAppOwn)
  protected
    procedure OnBeforeCommandLineProcessing(const processType: ustring;
      const commandLine: ICefCommandLine); override;
    procedure OnRegisterCustomSchemes(const registrar: ICefSchemeRegistrar); override;
    function GetResourceBundleHandler: ICefResourceBundleHandler; override;
    function GetBrowserProcessHandler: ICefBrowserProcessHandler; override;
    function GetRenderProcessHandler: ICefRenderProcessHandler; override;
  end;

  TCefRenderProcessHandlerOwn = class(TCefBaseOwn, ICefRenderProcessHandler)
  protected
    procedure OnRenderThreadCreated(const ExtraInfo: ICefListValue); virtual;
    procedure OnWebKitInitialized; virtual;
    procedure OnBrowserCreated(const browser: ICefBrowser); virtual;
    procedure OnBrowserDestroyed(const browser: ICefBrowser); virtual;
    function GetLoadHandler: ICefLoadHandler; virtual;
    function OnBeforeNavigation(const Browser: ICefBrowser; const Frame: ICefFrame; const Request: ICefRequest;
      NavigationType: TCefNavigationType; IsRedirect: Boolean): Boolean; virtual;
    procedure OnContextCreated(const browser: ICefBrowser;
      const frame: ICefFrame; const context: ICefv8Context); virtual;
    procedure OnContextReleased(const browser: ICefBrowser;
      const frame: ICefFrame; const context: ICefv8Context); virtual;
    procedure OnUncaughtException(const browser: ICefBrowser;
      const frame: ICefFrame; const context: ICefV8Context;
      const exception: ICefV8Exception; const stackTrace: ICefV8StackTrace); virtual;
    procedure OnFocusedNodeChanged(const browser: ICefBrowser;
      const frame: ICefFrame; const node: ICefDomNode); virtual;
    function OnProcessMessageReceived(const browser: ICefBrowser;
      sourceProcess: TCefProcessId; const message: ICefProcessMessage): Boolean; virtual;
  public
    constructor Create; virtual;
  end;

  TCefBrowserProcessHandlerOwn = class(TCefBaseOwn, ICefBrowserProcessHandler)
  protected
    procedure OnContextInitialized; virtual;
    procedure OnBeforeChildProcessLaunch(const commandLine: ICefCommandLine); virtual;
    procedure OnRenderProcessThreadCreated(const ExtraInfo: ICefListValue); virtual;
    function GetPrintHandler: ICefPrintHandler; virtual;
  public
    constructor Create; virtual;
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
    function Execute(const name: ustring; const obj: ICefv8Value;
      const arguments: TCefv8ValueArray; var retval: ICefv8Value;
      var exception: ustring): Boolean; override;
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

  TCefRunFileDialogCallbackOwn = class(TCefBaseOwn, ICefRunFileDialogCallback)
  protected
    procedure Cont(const browserHost: ICefBrowserHost; filePaths: TStrings); virtual;
  public
    constructor Create;
  end;

  TCefFastRunFileDialogCallback = class(TCefRunFileDialogCallbackOwn)
  private
    FCallback: TCefRunFileDialogCallbackProc;
  protected
    procedure Cont(const browserHost: ICefBrowserHost; filePaths: TStrings); override;
  public
    constructor Create(callback: TCefRunFileDialogCallbackProc); reintroduce; virtual;
  end;

  TCefDialogHandlerOwn = class(TCefBaseOwn, ICefDialogHandler)
  protected
    function OnFileDialog(const browser: ICefBrowser; mode: TCefFileDialogMode;
      const title: ustring; const DefaultFileName: ustring;
      AcceptTypes: TStrings; const callback: ICefFileDialogCallback):Boolean; virtual;
  public
    constructor Create; virtual;
  end;

  TCefRenderHandlerOwn = class(TCefBaseOwn, ICefRenderHandler)
  protected
    function GetRootScreenRect(const browser: ICefBrowser; rect: PCefRect): Boolean; virtual;
    function GetViewRect(const browser: ICefBrowser; rect: PCefRect): Boolean; virtual;
    function GetScreenPoint(const browser: ICefBrowser; viewX, viewY: cint;
      screenX, screenY: pcint): Boolean; virtual;
    function GetScreenInfo(const browser: ICefBrowser; out ScreenInfo: TCefScreenInfo): Boolean; virtual;
    procedure OnPopupShow(const browser: ICefBrowser; show: Boolean); virtual;
    procedure OnPopupSize(const browser: ICefBrowser; const rect: PCefRect); virtual;
    procedure OnPaint(const browser: ICefBrowser; kind: TCefPaintElementType;
      dirtyRectsCount: csize_t; const dirtyRects: PCefRectArray;
      const buffer: Pointer; width, height: cint); virtual;
    function StartDragging(const browser: ICefBrowser; const DragData: ICefDragData;
      AllowedOps: TCefDragOperationsMask; x: cint; y: cint): Boolean; virtual;
		procedure UpdateDragCursor(const browser: ICefBrowser; operation: TCefDragOperationsMask); virtual;
    procedure OnCursorChange(const browser: ICefBrowser; cursor: TCefCursorHandle); virtual;
    procedure OnScrollOffsetChanged(const browser: ICefBrowser);
  public
    constructor Create; virtual;
  end;


implementation

uses
  WACefRefs,
  WACefLib;

//..............................................................................TCefBaseOwn
function cef_base_add_ref(self: PCefBase): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  Result := TCefBaseOwn(TWACef.GetObject(self))._AddRef;
end;

function cef_base_release(self: PCefBase): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  Result := TCefBaseOwn(TWACef.GetObject(self))._Release;
end;

function cef_base_has_one_ref(self: PCefBase): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  Result := Ord(TCefBaseOwn(TWACef.GetObject(self)).FRefCount = 1);
end;

function cef_base_add_ref_owned(self: PCefBase): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  Result := 1;
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
    PCefBase(FData)^.add_ref := @cef_base_add_ref_owned;
    PCefBase(FData)^.release := @cef_base_release_owned;
    PCefBase(FData)^.has_one_ref := @cef_base_has_one_ref_owned;
  end else
  begin
    PCefBase(FData)^.add_ref := @cef_base_add_ref;
    PCefBase(FData)^.release := @cef_base_release;
    PCefBase(FData)^.has_one_ref := @cef_base_has_one_ref;
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

//..............................................................................TCefClientOwn
function cef_client_get_context_menu_handler(self: PCefClient): PCefContextMenuHandler; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  with TCefClientOwn(TWACef.GetObject(self)) do
    Result := TWACef.GetData(GetContextMenuHandler);
end;

function cef_client_get_dialog_handler(self: PCefClient): PCefDialogHandler; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  with TCefClientOwn(TWACef.GetObject(self)) do
    Result := TWACef.GetData(GetDialogHandler);
end;

function cef_client_get_display_handler(self: PCefClient): PCefDisplayHandler; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  with TCefClientOwn(TWACef.GetObject(self)) do
    Result := TWACef.GetData(GetDisplayHandler);
end;

function cef_client_get_download_handler(self: PCefClient): PCefDownloadHandler; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  with TCefClientOwn(TWACef.GetObject(self)) do
    Result := TWACef.GetData(GetDownloadHandler);
end;

function cef_client_get_drag_handler(self: PCefClient): PCefDragHandler; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  with TCefClientOwn(TWACef.GetObject(self)) do
    Result := TWACef.GetData(GetDragHandler);
end;

function cef_client_get_focus_handler(self: PCefClient): PCefFocusHandler; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  with TCefClientOwn(TWACef.GetObject(self)) do
    Result := TWACef.GetData(GetFocusHandler);
end;

function cef_client_get_geolocation_handler(self: PCefClient): PCefGeolocationHandler; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  with TCefClientOwn(TWACef.GetObject(self)) do
    Result := TWACef.GetData(GetGeolocationHandler);
end;

function cef_client_get_jsdialog_handler(self: PCefClient): PCefJsDialogHandler; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  with TCefClientOwn(TWACef.GetObject(self)) do
    Result := TWACef.GetData(GetJsdialogHandler);
end;

function cef_client_get_keyboard_handler(self: PCefClient): PCefKeyboardHandler; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  with TCefClientOwn(TWACef.GetObject(self)) do
    Result := TWACef.GetData(GetKeyboardHandler);
end;

function cef_client_get_life_span_handler(self: PCefClient): PCefLifeSpanHandler; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  with TCefClientOwn(TWACef.GetObject(self)) do
    Result := TWACef.GetData(GetLifeSpanHandler);
end;

function cef_client_get_load_handler(self: PCefClient): PCefLoadHandler; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  with TCefClientOwn(TWACef.GetObject(self)) do
    Result := TWACef.GetData(GetLoadHandler);
end;

function cef_client_get_render_handler(self: PCefClient): PCefRenderHandler; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  with TCefClientOwn(TWACef.GetObject(self)) do
    Result := TWACef.GetData(GetRenderHandler);
end;

function cef_client_get_request_handler(self: PCefClient): PCefRequestHandler; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  with TCefClientOwn(TWACef.GetObject(self)) do
    Result := TWACef.GetData(GetRequestHandler);
end;

function cef_client_on_process_message_received(self: PCefClient; browser: PCefBrowser;
  source_process: TCefProcessId; message: PCefProcessMessage): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  with TCefClientOwn(TWACef.GetObject(self)) do
    Result := Ord(OnProcessMessageReceived(TCefBrowserRef.UnWrap(browser), source_process,
      TCefProcessMessageRef.UnWrap(message)));
end;

constructor TCefClientOwn.Create;
begin
  inherited CreateData(SizeOf(TCefClient));
  with PCefClient(FData)^ do
  begin
    get_context_menu_handler := @cef_client_get_context_menu_handler;
    get_dialog_handler := @cef_client_get_dialog_handler;
    get_display_handler := @cef_client_get_display_handler;
    get_download_handler := @cef_client_get_download_handler;
    get_drag_handler := @cef_client_get_drag_handler;
    get_focus_handler := @cef_client_get_focus_handler;
    get_geolocation_handler := @cef_client_get_geolocation_handler;
    get_jsdialog_handler := @cef_client_get_jsdialog_handler;
    get_keyboard_handler := @cef_client_get_keyboard_handler;
    get_life_span_handler := @cef_client_get_life_span_handler;
    get_load_handler := @cef_client_get_load_handler;
    get_render_handler := @cef_client_get_render_handler;
    get_request_handler := @cef_client_get_request_handler;
    on_process_message_received := @cef_client_on_process_message_received;
  end;
end;

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

function TCefClientOwn.GetFocusHandler: ICefFocusHandler;
begin
  Result := nil;
end;

function TCefClientOwn.GetGeolocationHandler: ICefGeolocationHandler;
begin
  Result := nil;
end;

function TCefClientOwn.GetJsdialogHandler: ICefJsDialogHandler;
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

function TCefClientOwn.OnProcessMessageReceived(const browser: ICefBrowser;
  sourceProcess: TCefProcessId; const message: ICefProcessMessage): Boolean;
begin
  Result := False;
end;

//..............................................................................TCefGeolocationHandlerOwn
procedure cef_geolocation_handler_on_request_geolocation_permission(self: PCefGeolocationHandler;
  browser: PCefBrowser; const requesting_url: PCefString; request_id: cint;
  callback: PCefGeolocationCallback); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  with TCefGeolocationHandlerOwn(TWACef.GetObject(self)) do
    OnRequestGeolocationPermission(TCefBrowserRef.UnWrap(browser), TWACef.ToString(requesting_url),
      request_id, TCefGeolocationCallbackRef.UnWrap(callback));
end;

procedure cef_geolocation_handler_on_cancel_geolocation_permission(self: PCefGeolocationHandler;
  browser: PCefBrowser; const requesting_url: PCefString; request_id: cint); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  with TCefGeolocationHandlerOwn(TWACef.GetObject(self)) do
    OnCancelGeolocationPermission(TCefBrowserRef.UnWrap(browser), TWACef.ToString(requesting_url), request_id);
end;

constructor TCefGeolocationHandlerOwn.Create;
begin
  inherited CreateData(SizeOf(TCefGeolocationHandler));
  with PCefGeolocationHandler(FData)^ do
  begin
    on_request_geolocation_permission := @cef_geolocation_handler_on_request_geolocation_permission;
    on_cancel_geolocation_permission := @cef_geolocation_handler_on_cancel_geolocation_permission;
  end;
end;


procedure TCefGeolocationHandlerOwn.OnRequestGeolocationPermission(
  const browser: ICefBrowser; const requestingUrl: ustring; requestId: cint;
  const callback: ICefGeolocationCallback);
begin

end;

procedure TCefGeolocationHandlerOwn.OnCancelGeolocationPermission(
  const browser: ICefBrowser; const requestingUrl: ustring; requestId: cint);
begin

end;

//..............................................................................TCefLifeSpanHandlerOwn
function cef_life_span_handler_on_before_popup(self:PCefLifeSpanHandler; parentBrowser:PCefBrowser; frame:PCefFrame;
      const target_url:PCefString; const target_frame_name:PCefString;
      const popupFeatures:PCefPopupFeatures; windowInfo:PCefWindowInfo;
      var client:PCefClient; settings:PCefBrowserSettings; no_javascript_access: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
  _url,_framename: ustring;
  _no_js_access:boolean;
  _client: ICefClient;
begin
  _url := TWACef.ToString(target_url);
  _framename := TWACef.ToString(target_frame_name);
  _no_js_access:=no_javascript_access<>0;
  TWACef.GetObject(client);
  _client := TCefClientOwn(TWACef.GetObject(client)) as ICefClient;
  with TCefLifeSpanHandlerOwn(TWACef.GetObject(self)) do
    Result := Ord(OnBeforePopup(
      TCefBrowserRef.UnWrap(parentBrowser),
      TCefFrameRef.UnWrap(frame),
      _url,
      _framename,
      popupFeatures^,
      windowInfo^,
      _client,
      settings^,
      _no_js_access
    ));
  TWACef.StringSet(target_url, _url);
  client := TWACef.GetData(_client);
  _client := nil;
end;


procedure cef_life_span_handler_on_after_created(self: PCefLifeSpanHandler; browser: PCefBrowser); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  with TCefLifeSpanHandlerOwn(TWACef.GetObject(self)) do
    OnAfterCreated(TCefBrowserRef.UnWrap(browser));
end;

procedure cef_life_span_handler_on_before_close(self: PCefLifeSpanHandler; browser: PCefBrowser); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  with TCefLifeSpanHandlerOwn(TWACef.GetObject(self)) do
    OnBeforeClose(TCefBrowserRef.UnWrap(browser));
end;

function cef_life_span_handler_run_modal(self: PCefLifeSpanHandler; browser: PCefBrowser): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  with TCefLifeSpanHandlerOwn(TWACef.GetObject(self)) do
    Result := Ord(RunModal(TCefBrowserRef.UnWrap(browser)));
end;

function cef_life_span_handler_do_close(self: PCefLifeSpanHandler; browser: PCefBrowser): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  with TCefLifeSpanHandlerOwn(TWACef.GetObject(self)) do
    Result := Ord(DoClose(TCefBrowserRef.UnWrap(browser)));
end;

constructor TCefLifeSpanHandlerOwn.Create;
begin
  inherited CreateData(SizeOf(TCefLifeSpanHandler));
  with PCefLifeSpanHandler(FData)^ do
  begin
    on_before_popup := @cef_life_span_handler_on_before_popup;
    on_after_created := @cef_life_span_handler_on_after_created;
    on_before_close := @cef_life_span_handler_on_before_close;
    run_modal := @cef_life_span_handler_run_modal;
    do_close := @cef_life_span_handler_do_close;
  end;
end;

procedure TCefLifeSpanHandlerOwn.OnAfterCreated(const browser: ICefBrowser);
begin

end;

procedure TCefLifeSpanHandlerOwn.OnBeforeClose(const browser: ICefBrowser);
begin

end;

function TCefLifeSpanHandlerOwn.OnBeforePopup(const parentBrowser: ICefBrowser; const frame: ICefFrame;
      var TargetUrl: ustring; const TargetFrameName: ustring;
      var popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo;
      var client: ICefClient; var settings: TCefBrowserSettings; var NoJavascriptAccess:boolean): Boolean;
begin
  Result := False;
end;

function TCefLifeSpanHandlerOwn.DoClose(const browser: ICefBrowser): Boolean;
begin
  Result := False;
end;

function TCefLifeSpanHandlerOwn.RunModal(const browser: ICefBrowser): Boolean;
begin
  Result := False;
end;

//..............................................................................TCefLoadHandlerOwn
procedure cef_load_handler_on_loading_state_change(self:PCefLoadHandler; browser:PCefBrowser; isLoading: cint;
  canGoBack: cint; canGoForward: cint); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  with TCefLoadHandlerOwn(TWACef.GetObject(self)) do
    OnLoadingStateChange(TCefBrowserRef.UnWrap(browser),isLoading<>0,canGoBack<>0,canGoForward<>0);
end;

procedure cef_load_handler_on_load_start(self: PCefLoadHandler;
  browser: PCefBrowser; frame: PCefFrame); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  with TCefLoadHandlerOwn(TWACef.GetObject(self)) do
    OnLoadStart(TCefBrowserRef.UnWrap(browser), TCefFrameRef.UnWrap(frame));
end;

procedure cef_load_handler_on_load_end(self: PCefLoadHandler;
  browser: PCefBrowser; frame: PCefFrame; httpStatusCode: cint); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  with TCefLoadHandlerOwn(TWACef.GetObject(self)) do
    OnLoadEnd(TCefBrowserRef.UnWrap(browser), TCefFrameRef.UnWrap(frame), httpStatusCode);
end;

procedure cef_load_handler_on_load_error(self: PCefLoadHandler; browser: PCefBrowser;
  frame: PCefFrame; errorCode: TCefErrorCode; const errorText, failedUrl: PCefString); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  with TCefLoadHandlerOwn(TWACef.GetObject(self)) do
    OnLoadError(TCefBrowserRef.UnWrap(browser), TCefFrameRef.UnWrap(frame),
      errorCode, TWACef.ToString(errorText), TWACef.ToString(failedUrl));
end;

constructor TCefLoadHandlerOwn.Create;
begin
  inherited CreateData(SizeOf(TCefLoadHandler));
  with PCefLoadHandler(FData)^ do
  begin
    on_loading_state_change := @cef_load_handler_on_loading_state_change;
    on_load_start := @cef_load_handler_on_load_start;
    on_load_end := @cef_load_handler_on_load_end;
    on_load_error := @cef_load_handler_on_load_error;
  end;
end;

procedure TCefLoadHandlerOwn.OnLoadingStateChange(const browser: ICefBrowser; IsLoading:boolean; CanGoBack:boolean; CanGoForward:boolean);
begin

end;

procedure TCefLoadHandlerOwn.OnLoadStart(const browser: ICefBrowser;
  const frame: ICefFrame);
begin

end;

procedure TCefLoadHandlerOwn.OnLoadEnd(const browser: ICefBrowser;
  const frame: ICefFrame; httpStatusCode: cint);
begin

end;

procedure TCefLoadHandlerOwn.OnLoadError(const browser: ICefBrowser;
  const frame: ICefFrame; errorCode: TCefErrorCode; const errorText, failedUrl: ustring);
begin

end;

//..............................................................................TCefRequestHandlerOwn
function cef_request_handler_on_before_browse(self: PCefRequestHandler;
   browser:PCefBrowser; frame:PCefFrame; request:PCefRequest; is_redirect: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  with TCefRequestHandlerOwn(TWACef.GetObject(self)) do
    Result:=Ord(OnBeforeBrowse(TCefBrowserRef.UnWrap(browser),TCefFrameRef.UnWrap(frame),TCefRequestRef.UnWrap(request),boolean(is_redirect)));
end;

function cef_request_handler_on_before_resource_load(self: PCefRequestHandler;
   browser: PCefBrowser; frame: PCefFrame; request: PCefRequest): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  with TCefRequestHandlerOwn(TWACef.GetObject(self)) do
    Result:=Ord(OnBeforeResourceLoad(TCefBrowserRef.UnWrap(browser),TCefFrameRef.UnWrap(frame),TCefRequestRef.UnWrap(request)));
end;

function cef_request_handler_get_resource_handler(self: PCefRequestHandler;
  browser: PCefBrowser; frame: PCefFrame; request: PCefRequest): PCefResourceHandler; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  with TCefRequestHandlerOwn(TWACef.GetObject(self)) do
    Result:=TWACef.GetData(GetResourceHandler(TCefBrowserRef.UnWrap(browser),TCefFrameRef.UnWrap(frame),TCefRequestRef.UnWrap(request)));
end;

procedure cef_request_handler_on_resource_redirect(self: PCefRequestHandler;
  browser: PCefBrowser; frame: PCefFrame; const old_url: PCefString; new_url: PCefString); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
  url: ustring;
begin
  url := TWACef.ToString(new_url);
  with TCefRequestHandlerOwn(TWACef.GetObject(self)) do
    OnResourceRedirect(TCefBrowserRef.UnWrap(browser), TCefFrameRef.UnWrap(frame),
      TWACef.ToString(old_url), url);
  if url <> '' then
    TWACef.StringSet(new_url, url);
end;

function cef_request_handler_get_auth_credentials(self: PCefRequestHandler;
  browser: PCefBrowser; frame: PCefFrame; isProxy: cint; const host: PCefString;
  port: cint; const realm, scheme: PCefString; callback: PCefAuthCallback): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  with TCefRequestHandlerOwn(TWACef.GetObject(self)) do
    Result := Ord(GetAuthCredentials(
      TCefBrowserRef.UnWrap(browser), TCefFrameRef.UnWrap(frame), isProxy <> 0,
      TWACef.ToString(host), port, TWACef.ToString(realm), TWACef.ToString(scheme), TCefAuthCallbackRef.UnWrap(callback)));
end;

function cef_request_handler_on_quota_request(self: PCefRequestHandler; browser: PCefBrowser;
  const origin_url: PCefString; new_size: Int64; callback: PCefQuotaCallback): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  with TCefRequestHandlerOwn(TWACef.GetObject(self)) do
    Result := Ord(OnQuotaRequest(TCefBrowserRef.UnWrap(browser),
      TWACef.ToString(origin_url), new_size, TCefQuotaCallbackRef.UnWrap(callback)));
end;

procedure cef_request_handler_on_protocol_execution(self: PCefRequestHandler;
  browser: PCefBrowser; const url: PCefString; allow_os_execution: pcint); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
  allow: Boolean;
begin
  allow := allow_os_execution^ <> 0;
  with TCefRequestHandlerOwn(TWACef.GetObject(self)) do
    OnProtocolExecution(
      TCefBrowserRef.UnWrap(browser),
      TWACef.ToString(url), allow);
  allow_os_execution^ := Ord(allow);
end;

function cef_request_handler_on_certificate_error(self:PCefRequestHandler; cert_error: TCefErrorcode;
  const request_url:PCefString; callback:PCefAllowCertificateErrorCallback): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  with TCefRequestHandlerOwn(TWACef.GetObject(self)) do
    result:=Ord(OnCertificateError(cert_error,TWACef.ToString(request_url), TCefAllowCertificateErrorCallbackRef.Unwrap(callback)));
end;

function cef_request_handler_on_before_plugin_load(self: PCefRequestHandler; browser: PCefBrowser;
  const url, policy_url: PCefString; info: PCefWebPluginInfo): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  with TCefRequestHandlerOwn(TWACef.GetObject(self)) do
    Result := Ord(OnBeforePluginLoad(TCefBrowserRef.UnWrap(browser),
      TWACef.ToString(url), TWACef.ToString(policy_url), TCefWebPluginInfoRef.UnWrap(info)));
end;

procedure cef_request_handler_on_plugin_crashed(self:PCefRequestHandler; browser:PCefBrowser; const plugin_path:PCefString); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  with TCefRequestHandlerOwn(TWACef.GetObject(self)) do
    OnPluginCrashed(TCefBrowserRef.UnWrap(browser),TWACef.ToString(plugin_path));
end;

procedure cef_request_handler_on_render_process_terminated(self:PCefRequestHandler; browser:PCefBrowser;
  status: TCefTerminationStatus); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  with TCefRequestHandlerOwn(TWACef.GetObject(self)) do
    OnRenderProcessTerminated(TCefBrowserRef.UnWrap(browser),status);
end;

constructor TCefRequestHandlerOwn.Create;
begin
  inherited CreateData(SizeOf(TCefRequestHandler));
  with PCefRequestHandler(FData)^ do
    begin
      on_before_browse:= @cef_request_handler_on_before_browse;
      on_before_resource_load:= @cef_request_handler_on_before_resource_load;
      get_resource_handler:= @cef_request_handler_get_resource_handler;
      on_resource_redirect:= @cef_request_handler_on_resource_redirect;
      get_auth_credentials:= @cef_request_handler_get_auth_credentials;
      on_quota_request:= @cef_request_handler_on_quota_request;
      on_protocol_execution:= @cef_request_handler_on_protocol_execution;
      on_certificate_error:= @cef_request_handler_on_certificate_error;
      on_before_plugin_load:= @cef_request_handler_on_before_plugin_load;
      on_plugin_crashed:= @cef_request_handler_on_plugin_crashed;
      on_render_process_terminated:= @cef_request_handler_on_render_process_terminated;
    end;
end;

function TCefRequestHandlerOwn.OnBeforeBrowse(const browser: ICefBrowser; const frame: ICefFrame;
      const request: ICefRequest; IsRedirect:boolean): Boolean;
begin
  result:=false;
end;

function TCefRequestHandlerOwn.OnBeforeResourceLoad(const browser: ICefBrowser; const frame: ICefFrame;
  const request: ICefRequest): Boolean;
begin
  Result:=False;
end;

function TCefRequestHandlerOwn.GetResourceHandler(const browser: ICefBrowser;
  const frame: ICefFrame; const request: ICefRequest): ICefResourceHandler;
begin
  Result:=nil;
end;

procedure TCefRequestHandlerOwn.OnResourceRedirect(const browser: ICefBrowser;
  const frame: ICefFrame; const oldUrl: ustring; var newUrl: ustring);
begin

end;

function TCefRequestHandlerOwn.GetAuthCredentials(const browser: ICefBrowser; const frame: ICefFrame;
  isProxy: Boolean; const host: ustring; port: cint; const realm, scheme: ustring;
  const callback: ICefAuthCallback): Boolean;
begin
  Result:=False;
end;

function TCefRequestHandlerOwn.OnQuotaRequest(const browser: ICefBrowser;
  const originUrl: ustring; newSize: Int64;
  const callback: ICefQuotaCallback): Boolean;
begin
  Result:=False;
end;

function TCefRequestHandlerOwn.OnCertificateError(CertError: TCefErrorcode; const RequestUrl: ustring;
      const Callback: ICefAllowCertificateErrorCallback): boolean;
begin
  result:=false;
end;

procedure TCefRequestHandlerOwn.OnProtocolExecution(const browser: ICefBrowser;
  const url: ustring; out allowOsExecution: Boolean);
begin

end;

function TCefRequestHandlerOwn.OnBeforePluginLoad(const browser: ICefBrowser;
  const url, policyUrl: ustring; const info: ICefWebPluginInfo): Boolean;
begin
  Result:=False;
end;

procedure TCefRequestHandlerOwn.OnPluginCrashed(const browser: ICefBrowser; const pluginPath: ustring);
begin

end;

procedure TCefRequestHandlerOwn.OnRenderProcessTerminated(const browser: ICefBrowser; status: TCefTerminationStatus);
begin

end;

//..............................................................................TCefDisplayHandlerOwn
procedure cef_display_handler_on_address_change(self: PCefDisplayHandler;
  browser: PCefBrowser; frame: PCefFrame; const url: PCefString); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  with TCefDisplayHandlerOwn(TWACef.GetObject(self)) do
    OnAddressChange(
      TCefBrowserRef.UnWrap(browser),
      TCefFrameRef.UnWrap(frame),
      TWACef.ToString(url))
end;

procedure cef_display_handler_on_title_change(self: PCefDisplayHandler;
  browser: PCefBrowser; const title: PCefString); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  with TCefDisplayHandlerOwn(TWACef.GetObject(self)) do
    OnTitleChange(TCefBrowserRef.UnWrap(browser), TWACef.ToString(title));
end;

function cef_display_handler_on_tooltip(self: PCefDisplayHandler;
  browser: PCefBrowser; text: PCefString): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
  t: ustring;
begin
  t := TWACef.StringClearAndGet(text^);
  with TCefDisplayHandlerOwn(TWACef.GetObject(self)) do
    Result := Ord(OnTooltip(
      TCefBrowserRef.UnWrap(browser), t));
  text^ := TWACef.StringAlloc(t);
end;

procedure cef_display_handler_on_status_message(self: PCefDisplayHandler;
  browser: PCefBrowser; const value: PCefString); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  with TCefDisplayHandlerOwn(TWACef.GetObject(self)) do
    OnStatusMessage(TCefBrowserRef.UnWrap(browser), TWACef.ToString(value));
end;

function cef_display_handler_on_console_message(self: PCefDisplayHandler;
    browser: PCefBrowser; const message: PCefString;
    const source: PCefString; line: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  with TCefDisplayHandlerOwn(TWACef.GetObject(self)) do
    Result := Ord(OnConsoleMessage(TCefBrowserRef.UnWrap(browser),
    TWACef.ToString(message), TWACef.ToString(source), line));
end;

constructor TCefDisplayHandlerOwn.Create;
begin
  inherited CreateData(SizeOf(TCefDisplayHandler));
  with PCefDisplayHandler(FData)^ do
  begin
    on_address_change := @cef_display_handler_on_address_change;
    on_title_change := @cef_display_handler_on_title_change;
    on_tooltip := @cef_display_handler_on_tooltip;
    on_status_message := @cef_display_handler_on_status_message;
    on_console_message := @cef_display_handler_on_console_message;
  end;
end;

procedure TCefDisplayHandlerOwn.OnLoadingStateChange(const browser: ICefBrowser;
  isLoading, canGoBack, canGoForward: Boolean);
begin

end;

procedure TCefDisplayHandlerOwn.OnAddressChange(const browser: ICefBrowser;
  const frame: ICefFrame; const url: ustring);
begin

end;

function TCefDisplayHandlerOwn.OnConsoleMessage(const browser: ICefBrowser;
  const message, source: ustring; line: cint): Boolean;
begin
  Result := False;
end;

procedure TCefDisplayHandlerOwn.OnStatusMessage(const browser: ICefBrowser;
  const value: ustring);
begin

end;

procedure TCefDisplayHandlerOwn.OnTitleChange(const browser: ICefBrowser;
  const title: ustring);
begin

end;

function TCefDisplayHandlerOwn.OnTooltip(const browser: ICefBrowser;
  var text: ustring): Boolean;
begin
  Result := False;
end;

//..............................................................................TCefFocusHandlerOwn
procedure cef_focus_handler_on_take_focus(self: PCefFocusHandler;
  browser: PCefBrowser; next: cint); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  with TCefFocusHandlerOwn(TWACef.GetObject(self)) do
    OnTakeFocus(TCefBrowserRef.UnWrap(browser), next <> 0);
end;

function cef_focus_handler_on_set_focus(self: PCefFocusHandler;
  browser: PCefBrowser; source: TCefFocusSource): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  with TCefFocusHandlerOwn(TWACef.GetObject(self)) do
    Result := Ord(OnSetFocus(TCefBrowserRef.UnWrap(browser), source))
end;

procedure cef_focus_handler_on_got_focus(self: PCefFocusHandler; browser: PCefBrowser); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  with TCefFocusHandlerOwn(TWACef.GetObject(self)) do
    OnGotFocus(TCefBrowserRef.UnWrap(browser));
end;


constructor TCefFocusHandlerOwn.Create;
begin
  inherited CreateData(SizeOf(TCefFocusHandler));
  with PCefFocusHandler(FData)^ do
  begin
    on_take_focus := @cef_focus_handler_on_take_focus;
    on_set_focus := @cef_focus_handler_on_set_focus;
    on_got_focus := @cef_focus_handler_on_got_focus;
  end;
end;

function TCefFocusHandlerOwn.OnSetFocus(const browser: ICefBrowser;
  source: TCefFocusSource): Boolean;
begin
  Result := False;
end;

procedure TCefFocusHandlerOwn.OnGotFocus(const browser: ICefBrowser);
begin

end;

procedure TCefFocusHandlerOwn.OnTakeFocus(const browser: ICefBrowser;
  next: Boolean);
begin

end;

//..............................................................................TCefKeyboardHandlerOwn
function cef_keyboard_handler_on_pre_key_event(self: PCefKeyboardHandler;
  browser: PCefBrowser; const event: PCefKeyEvent;
  os_event: TCefEventHandle; is_keyboard_shortcut: pcint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
  ks: Boolean;
begin
  ks := is_keyboard_shortcut^ <> 0;
  with TCefKeyboardHandlerOwn(TWACef.GetObject(self)) do
    Result := Ord(OnPreKeyEvent(TCefBrowserRef.UnWrap(browser), event, os_event, ks));
  is_keyboard_shortcut^ := Ord(ks);
end;

function cef_keyboard_handler_on_key_event(self: PCefKeyboardHandler;
    browser: PCefBrowser; const event: PCefKeyEvent; os_event: TCefEventHandle): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  with TCefKeyboardHandlerOwn(TWACef.GetObject(self)) do
    Result := Ord(OnKeyEvent(TCefBrowserRef.UnWrap(browser), event, os_event));
end;

constructor TCefKeyboardHandlerOwn.Create;
begin
  inherited CreateData(SizeOf(TCefKeyboardHandler));
  with PCefKeyboardHandler(FData)^ do
  begin
    on_pre_key_event := @cef_keyboard_handler_on_pre_key_event;
    on_key_event := @cef_keyboard_handler_on_key_event;
  end;
end;

function TCefKeyboardHandlerOwn.OnPreKeyEvent(const browser: ICefBrowser;
  const event: PCefKeyEvent; osEvent: TCefEventHandle;
  out isKeyboardShortcut: Boolean): Boolean;
begin
  Result := False;
end;

function TCefKeyboardHandlerOwn.OnKeyEvent(const browser: ICefBrowser;
  const event: PCefKeyEvent; osEvent: TCefEventHandle): Boolean;
begin
  Result := False;
end;

//..............................................................................TCefJsDialogHandlerOwn
function cef_jsdialog_handler_on_jsdialog(self: PCefJsDialogHandler;
  browser: PCefBrowser; const origin_url, accept_lang: PCefString;
  dialog_type: TCefJsDialogType; const message_text, default_prompt_text: PCefString;
  callback: PCefJsDialogCallback; suppress_message: pcint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
  sm: Boolean;
begin
  sm := suppress_message^ <> 0;
  with TCefJsDialogHandlerOwn(TWACef.GetObject(self)) do
    Result := Ord(OnJsdialog(TCefBrowserRef.UnWrap(browser), TWACef.ToString(origin_url),
      TWACef.ToString(accept_lang), dialog_type, TWACef.ToString(message_text),
      TWACef.ToString(default_prompt_text), TCefJsDialogCallbackRef.UnWrap(callback), sm));
  suppress_message^ := Ord(sm);
end;

function cef_jsdialog_handler_on_before_unload_dialog(self: PCefJsDialogHandler;
  browser: PCefBrowser; const message_text: PCefString; is_reload: cint;
  callback: PCefJsDialogCallback): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  with TCefJsDialogHandlerOwn(TWACef.GetObject(self)) do
    Result := Ord(OnBeforeUnloadDialog(TCefBrowserRef.UnWrap(browser), TWACef.ToString(message_text),
      is_reload <> 0, TCefJsDialogCallbackRef.UnWrap(callback)));
end;

procedure cef_jsdialog_handler_on_reset_dialog_state(self: PCefJsDialogHandler;
  browser: PCefBrowser); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  with TCefJsDialogHandlerOwn(TWACef.GetObject(self)) do
    OnResetDialogState(TCefBrowserRef.UnWrap(browser));
end;

procedure cef_jsdialog_handler_on_dialog_closed(self: PCefJsDialogHandler;
  browser: PCefBrowser); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  with TCefJsDialogHandlerOwn(TWACef.GetObject(self)) do
    OnDialogClosed(TCefBrowserRef.UnWrap(browser));
end;

constructor TCefJsDialogHandlerOwn.Create;
begin
  inherited CreateData(SizeOf(TCefJsDialogHandler));
  with PCefJsDialogHandler(FData)^ do
  begin
    on_jsdialog := @cef_jsdialog_handler_on_jsdialog;
    on_before_unload_dialog := @cef_jsdialog_handler_on_before_unload_dialog;
    on_reset_dialog_state := @cef_jsdialog_handler_on_reset_dialog_state;
    on_dialog_closed := @cef_jsdialog_handler_on_dialog_closed;
  end;
end;

function TCefJsDialogHandlerOwn.OnJsdialog(const browser: ICefBrowser;
  const originUrl, acceptLang: ustring; dialogType: TCefJsDialogType;
  const messageText, defaultPromptText: ustring; callback: ICefJsDialogCallback;
  out suppressMessage: Boolean): Boolean;
begin
  Result := False;
end;

function TCefJsDialogHandlerOwn.OnBeforeUnloadDialog(const browser: ICefBrowser;
  const messageText: ustring; isReload: Boolean; const callback: ICefJsDialogCallback): Boolean;
begin
  Result := False;
end;

procedure TCefJsDialogHandlerOwn.OnResetDialogState(const browser: ICefBrowser);
begin

end;

procedure TCefJsDialogHandlerOwn.OnDialogClosed(const browser: ICefBrowser);
begin

end;

//..............................................................................TCefMenuHandlerOwn
procedure cef_context_menu_handler_on_before_context_menu(self: PCefContextMenuHandler;
  browser: PCefBrowser; frame: PCefFrame; params: PCefContextMenuParams;
  model: PCefMenuModel); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  with TCefContextMenuHandlerOwn(TWACef.GetObject(self)) do
    OnBeforeContextMenu(TCefBrowserRef.UnWrap(browser), TCefFrameRef.UnWrap(frame),
      TCefContextMenuParamsRef.UnWrap(params), TCefMenuModelRef.UnWrap(model));
end;

function cef_context_menu_handler_on_context_menu_command(self: PCefContextMenuHandler;
  browser: PCefBrowser; frame: PCefFrame; params: PCefContextMenuParams;
  command_id: cint; event_flags: TCefEventFlags): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  with TCefContextMenuHandlerOwn(TWACef.GetObject(self)) do
    Result := Ord(OnContextMenuCommand(TCefBrowserRef.UnWrap(browser), TCefFrameRef.UnWrap(frame),
      TCefContextMenuParamsRef.UnWrap(params), command_id, TCefEventFlags(Pointer(@event_flags)^)));
end;

procedure cef_context_menu_handler_on_context_menu_dismissed(self: PCefContextMenuHandler;
  browser: PCefBrowser; frame: PCefFrame); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  with TCefContextMenuHandlerOwn(TWACef.GetObject(self)) do
    OnContextMenuDismissed(TCefBrowserRef.UnWrap(browser), TCefFrameRef.UnWrap(frame));
end;

constructor TCefContextMenuHandlerOwn.Create;
begin
  inherited CreateData(SizeOf(TCefContextMenuHandler));
  with PCefContextMenuHandler(FData)^ do
  begin
    on_before_context_menu := @cef_context_menu_handler_on_before_context_menu;
    on_context_menu_command := @cef_context_menu_handler_on_context_menu_command;
    on_context_menu_dismissed := @cef_context_menu_handler_on_context_menu_dismissed;
  end;
end;

procedure TCefContextMenuHandlerOwn.OnBeforeContextMenu(
  const browser: ICefBrowser; const frame: ICefFrame;
  const params: ICefContextMenuParams; const model: ICefMenuModel);
begin

end;

function TCefContextMenuHandlerOwn.OnContextMenuCommand(
  const browser: ICefBrowser; const frame: ICefFrame;
  const params: ICefContextMenuParams; commandId: cint;
  eventFlags: TCefEventFlags): Boolean;
begin
  Result := False;
end;

procedure TCefContextMenuHandlerOwn.OnContextMenuDismissed(
  const browser: ICefBrowser; const frame: ICefFrame);
begin

end;

//..............................................................................TCefStringVisitorOwn
procedure cef_string_visitor_visit(self: PCefStringVisitor; const str: PCefString); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  TCefStringVisitorOwn(TWACef.GetObject(self)).Visit(TWACef.ToString(str));
end;

constructor TCefStringVisitorOwn.Create;
begin
  inherited CreateData(SizeOf(TCefStringVisitor));
  with PCefStringVisitor(FData)^ do
    visit := @cef_string_visitor_visit;
end;

procedure TCefStringVisitorOwn.Visit(const str: ustring);
begin

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

//..............................................................................TCefV8AccessorOwn
function cef_v8_accessor_get(self: PCefV8Accessor; const name: PCefString;
      _object: PCefv8Value; out retval: PCefv8Value; exception: PCefString): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
  ret: ICefv8Value;
begin
  Result := Ord(TCefV8AccessorOwn(TWACef.GetObject(self)).Get(TWACef.ToString(name),
    TCefv8ValueRef.UnWrap(_object), ret, TWACef.ToString(exception)));
  retval := TWACef.GetData(ret);
end;

function cef_v8_accessor_put(self: PCefV8Accessor; const name: PCefString;
      obj: PCefv8Value; value: PCefv8Value; exception: PCefString): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  Result := Ord(TCefV8AccessorOwn(TWACef.GetObject(self)).Put(TWACef.ToString(name),
    TCefv8ValueRef.UnWrap(obj), TCefv8ValueRef.UnWrap(value), TWACef.ToString(exception)));
end;

constructor TCefV8AccessorOwn.Create;
begin
  inherited CreateData(SizeOf(TCefV8Accessor));
  PCefV8Accessor(FData)^.get  := @cef_v8_accessor_get;
  PCefV8Accessor(FData)^._set := @cef_v8_accessor_put;
end;

function TCefV8AccessorOwn.Get(const name: ustring; const obj: ICefv8Value;
  out value: ICefv8Value; const exception: ustring): Boolean;
begin
  Result := False;
end;

function TCefV8AccessorOwn.Put(const name: ustring; const obj,
  value: ICefv8Value; const exception: ustring): Boolean;
begin
  Result := False;
end;

//..............................................................................TCefV8ExceptionOwn
function cef_v8_exception_get_message(self: PCefV8Exception): PCefStringUserFree;
begin
  with TCefV8ExceptionOwn(TWACef.GetObject(self)) do
    Result := TWACef.UserFreeString(GetMessage);
end;

function cef_v8_exception_get_source_line(self: PCefV8Exception): PCefStringUserFree;
begin
  with TCefV8ExceptionOwn(TWACef.GetObject(self)) do
    Result := TWACef.UserFreeString(GetSourceLine);
end;

function cef_v8_exception_get_script_resource_name(self: PCefV8Exception): PCefStringUserFree;
begin
  with TCefV8ExceptionOwn(TWACef.GetObject(self)) do
    Result := TWACef.UserFreeString(GetScriptResourceName);
end;

function cef_v8_exception_get_line_number(self: PCefV8Exception): cint;
begin
  with TCefV8ExceptionOwn(TWACef.GetObject(self)) do
    Result := GetLineNumber;
end;

function cef_v8_exception_get_start_position(self: PCefV8Exception): cint;
begin
  with TCefV8ExceptionOwn(TWACef.GetObject(self)) do
    Result := GetStartPosition;
end;

function cef_v8_exception_get_end_position(self: PCefV8Exception): cint;
begin
  with TCefV8ExceptionOwn(TWACef.GetObject(self)) do
    Result := GetEndPosition;
end;

function cef_v8_exception_get_start_column(self: PCefV8Exception): cint;
begin
  with TCefV8ExceptionOwn(TWACef.GetObject(self)) do
    Result := GetStartColumn;
end;

function cef_v8_exception_get_end_column(self: PCefV8Exception): cint;
begin
  with TCefV8ExceptionOwn(TWACef.GetObject(self)) do
    Result := GetEndColumn;
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
constructor TCefV8ExceptionOwn.Create;
begin
  inherited CreateData(SizeOf(TCefV8Exception));
  FMessage := '';
  FSourceLine := '';
  FScriptResourceName := '';
  FLineNumber := 0;
  FStartPosition := 0;
  FEndPosition := 0;
  FStartColumn := 0;
  FEndColumn := 0;

  PCefV8Exception(FData)^.get_message  := @cef_v8_exception_get_message;
  PCefV8Exception(FData)^.get_source_line  := @cef_v8_exception_get_source_line;
  PCefV8Exception(FData)^.get_script_resource_name  := @cef_v8_exception_get_script_resource_name;
  PCefV8Exception(FData)^.get_line_number  := @cef_v8_exception_get_line_number;
  PCefV8Exception(FData)^.get_start_position  := @cef_v8_exception_get_start_position;
  PCefV8Exception(FData)^.get_end_position  := @cef_v8_exception_get_end_position;
  PCefV8Exception(FData)^.get_start_column  := @cef_v8_exception_get_start_column;
  PCefV8Exception(FData)^.get_end_column  := @cef_v8_exception_get_end_column;
end;

//..............................................................................TCefDomVisitorOwn
procedure cef_dom_visitor_visite(self: PCefDomVisitor; document: PCefDomDocument); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  TCefDomVisitorOwn(TWACef.GetObject(self)).visit(TCefDomDocumentRef.UnWrap(document));
end;

constructor TCefDomVisitorOwn.Create;
begin
  inherited CreateData(SizeOf(TCefDomVisitor));
  PCefDomVisitor(FData)^.visit := @cef_dom_visitor_visite;
end;

procedure TCefDomVisitorOwn.visit(const document: ICefDomDocument);
begin

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

//..............................................................................TCefDownloadHandlerOwn
procedure cef_download_handler_on_before_download(self: PCefDownloadHandler;
  browser: PCefBrowser; download_item: PCefDownloadItem;
  const suggested_name: PCefString; callback: PCefBeforeDownloadCallback); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  TCefDownloadHandlerOwn(TWACef.GetObject(self)).
    OnBeforeDownload(TCefBrowserRef.UnWrap(browser),
    TCefDownLoadItemRef.UnWrap(download_item), TWACef.ToString(suggested_name),
    TCefBeforeDownloadCallbackRef.UnWrap(callback));
end;

procedure cef_download_handler_on_download_updated(self: PCefDownloadHandler;
  browser: PCefBrowser; download_item: PCefDownloadItem; callback: PCefDownloadItemCallback); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  TCefDownloadHandlerOwn(TWACef.GetObject(self)).
    OnDownloadUpdated(TCefBrowserRef.UnWrap(browser),
    TCefDownLoadItemRef.UnWrap(download_item),
    TCefDownloadItemCallbackRef.UnWrap(callback));
end;

constructor TCefDownloadHandlerOwn.Create;
begin
  inherited CreateData(SizeOf(TCefDownloadHandler));
  with PCefDownloadHandler(FData)^ do
  begin
    on_before_download := @cef_download_handler_on_before_download;
    on_download_updated := @cef_download_handler_on_download_updated;
  end;
end;

procedure TCefDownloadHandlerOwn.OnBeforeDownload(const browser: ICefBrowser;
  const downloadItem: ICefDownloadItem; const suggestedName: ustring;
  const callback: ICefBeforeDownloadCallback);
begin

end;

procedure TCefDownloadHandlerOwn.OnDownloadUpdated(const browser: ICefBrowser;
  const downloadItem: ICefDownloadItem;
  const callback: ICefDownloadItemCallback);
begin

end;

//..............................................................................TCefDragHandlerOwn
function cef_drag_handler_on_drag_enter(self:PCefDragHandler; browser:PCefBrowser; dragData:PCefDragData;
  mask: TCefDragOperationsMask): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  result:=Ord(TCefDragHandlerOwn(TWACef.GetObject(self)).OnDragEnter(
    TCefBrowserRef.UnWrap(browser),
    TCefDragDataRef.UnWrap(dragData),
    mask
  ));
end;

//Protected section
function TCefDragHandlerOwn.OnDragEnter(const browser: ICefBrowser; const dragData: ICefDragData;
  mask: TCefDragOperationsMask):boolean;
begin
  result:=false;
end;

//Public section
constructor TCefDragHandlerOwn.Create;
begin
  inherited CreateData(SizeOf(TCefDragHandler));
  with PCefDragHandler(FData)^ do
  begin
    on_drag_enter:= @cef_drag_handler_on_drag_enter;
  end;
end;


//..............................................................................TCefCustomStreamReader
function cef_stream_reader_read(self: PCefReadHandler; ptr: Pointer; size, n: csize_t): csize_t; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  with TCefCustomStreamReader(TWACef.GetObject(self)) do
    Result := Read(ptr, size, n);
end;

function cef_stream_reader_seek(self: PCefReadHandler; offset: Int64; whence: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  with TCefCustomStreamReader(TWACef.GetObject(self)) do
    Result := Seek(offset, whence);
end;

function cef_stream_reader_tell(self: PCefReadHandler): Int64; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  with TCefCustomStreamReader(TWACef.GetObject(self)) do
    Result := Tell;
end;

function cef_stream_reader_eof(self: PCefReadHandler): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
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
    read := @cef_stream_reader_read;
    seek := @cef_stream_reader_seek;
    tell := @cef_stream_reader_tell;
    eof := @cef_stream_reader_eof;
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

//..............................................................................TCefPostDataElementOwn
function cef_post_data_element_is_read_only(self: PCefPostDataElement): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  with TCefPostDataElementOwn(TWACef.GetObject(self)) do
    Result := Ord(IsReadOnly)
end;

procedure cef_post_data_element_set_to_empty(self: PCefPostDataElement); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  with TCefPostDataElementOwn(TWACef.GetObject(self)) do
    SetToEmpty;
end;

procedure cef_post_data_element_set_to_file(self: PCefPostDataElement; const fileName: PCefString); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  with TCefPostDataElementOwn(TWACef.GetObject(self)) do
    SetToFile(TWACef.ToString(fileName));
end;

procedure cef_post_data_element_set_to_bytes(self: PCefPostDataElement; size: csize_t; const bytes: Pointer); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  with TCefPostDataElementOwn(TWACef.GetObject(self)) do
    SetToBytes(size, bytes);
end;

function cef_post_data_element_get_type(self: PCefPostDataElement): TCefPostDataElementType; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  with TCefPostDataElementOwn(TWACef.GetObject(self)) do
    Result := GetType;
end;

function cef_post_data_element_get_file(self: PCefPostDataElement): PCefStringUserFree; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  with TCefPostDataElementOwn(TWACef.GetObject(self)) do
    Result := TWACef.UserFreeString(GetFile);
end;

function cef_post_data_element_get_bytes_count(self: PCefPostDataElement): csize_t; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  with TCefPostDataElementOwn(TWACef.GetObject(self)) do
    Result := GetBytesCount;
end;

function cef_post_data_element_get_bytes(self: PCefPostDataElement; size: csize_t; bytes: Pointer): csize_t; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  with TCefPostDataElementOwn(TWACef.GetObject(self)) do
    Result := GetBytes(size, bytes)
end;

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

constructor TCefPostDataElementOwn.Create(readonly: Boolean);
begin
  inherited CreateData(SizeOf(TCefPostDataElement));
  FReadOnly := readonly;
  FDataType := PDE_TYPE_EMPTY;
  FValueByte := nil;
  FillChar(FValueStr, SizeOf(FValueStr), 0);
  FSize := 0;
  with PCefPostDataElement(FData)^ do
  begin
    is_read_only := @cef_post_data_element_is_read_only;
    set_to_empty := @cef_post_data_element_set_to_empty;
    set_to_file := @cef_post_data_element_set_to_file;
    set_to_bytes := @cef_post_data_element_set_to_bytes;
    get_type := @cef_post_data_element_get_type;
    get_file := @cef_post_data_element_get_file;
    get_bytes_count := @cef_post_data_element_get_bytes_count;
    get_bytes := @cef_post_data_element_get_bytes;
  end;
end;

function TCefPostDataElementOwn.GetBytes(size: csize_t;
  bytes: Pointer): csize_t;
begin
  if (FDataType = PDE_TYPE_BYTES) and (FValueByte <> nil) then
  begin
    if size > FSize then
      Result := FSize else
      Result := size;
    Move(FValueByte^, bytes^, Result);
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

procedure TCefPostDataElementOwn.SetToBytes(size: csize_t; const bytes: Pointer);
begin
  Clear;
  if (size > 0) and (bytes <> nil) then
  begin
    GetMem(FValueByte, size);
    Move(bytes^, FValueByte, size);
    FSize := size;
  end else
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

procedure TCefPostDataElementOwn.SetToFile(const fileName: ustring);
begin
  Clear;
  FSize := 0;
  FValueStr := TWACef.StringAlloc(fileName);
  FDataType := PDE_TYPE_FILE;
end;

//..............................................................................TCefCookieVisitorOwn

function cef_cookie_visitor_visit(self: PCefCookieVisitor; const cookie: PCefCookie;
  count, total: cint; deleteCookie: pcint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
  delete: Boolean;
  exp: TDateTime;
  cook: TWACefCookie;
begin
  delete := False;
  if cookie.has_expires <> 0 then
    exp := TWACef.CefTimeToDateTime(cookie.expires) else
    exp := 0;
  cook.secure := cookie^.secure;
  cook.domain := TWACef.ToString(@(cookie^.domain));
  cook.value := TWACef.ToString(@(cookie^.value));
  cook.path := TWACef.ToString(@(cookie^.path));
  cook.name := TWACef.ToString(@(cookie^.name));
  cook.last_access := cookie^.last_access;
  cook.httponly := cookie^.httponly;
  cook.has_expires := cookie^.has_expires;
  cook.expires := cookie^.expires;
  cook.creation := cookie^.creation;
  Result := Ord(TCefCookieVisitorOwn(TWACef.GetObject(self)).visit(cook, count, total, delete));
  deleteCookie^ := Ord(delete);
end;

constructor TCefCookieVisitorOwn.Create;
begin
  inherited CreateData(SizeOf(TCefCookieVisitor));
  PCefCookieVisitor(FData)^.visit := @cef_cookie_visitor_visit;
end;

function TCefCookieVisitorOwn.visit(const Cookie: TWACefCookie; count, total: cint; out deleteCookie: Boolean): Boolean;
begin
  Result := True;
end;

//..............................................................................TCefFastCookieVisitor

constructor TCefFastCookieVisitor.Create(const visitor: TCefCookieVisitorProc);
begin
  inherited Create;
  FVisitor := visitor;
end;

function TCefFastCookieVisitor.visit(const Cookie: TWACefCookie; count, total: cint; out deleteCookie: Boolean): Boolean;
var
  wacookie: TWACefCookie;
begin
  wacookie.name:=TWACef.ToString(@(Cookie.name));
  wacookie.value:=TWACef.ToString(@(Cookie.value));
  wacookie.domain:=TWACef.ToString(@(Cookie.domain));
  wacookie.path:=TWACef.ToString(@(Cookie.path));
  wacookie.secure:=Cookie.secure;
  wacookie.httponly:=Cookie.httponly;
  wacookie.creation:=Cookie.creation;
  wacookie.last_access:=Cookie.last_access;
  wacookie.has_expires:=Cookie.has_expires;
  wacookie.expires:=Cookie.expires;
  Result := FVisitor(wacookie, count, total, deleteCookie);
end;

//..............................................................................TCefResourceHandlerOwn
function cef_resource_handler_process_request(self: PCefResourceHandler;
  request: PCefRequest; callback: PCefCallback): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  with TCefResourceHandlerOwn(TWACef.GetObject(self)) do
    Result := Ord(ProcessRequest(TCefRequestRef.UnWrap(request), TCefCallbackRef.UnWrap(callback)));
end;

procedure cef_resource_handler_get_response_headers(self: PCefResourceHandler;
  response: PCefResponse; response_length: pcint64; redirectUrl: PCefString); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
  ru: ustring;
begin
  ru := '';
  with TCefResourceHandlerOwn(TWACef.GetObject(self)) do
    GetResponseHeaders(TCefResponseRef.UnWrap(response), response_length^, ru);
  if ru <> '' then
    TWACef.StringSet(redirectUrl, ru);
end;

function cef_resource_handler_read_response(self: PCefResourceHandler;
  data_out: Pointer; bytes_to_read: cint; bytes_read: pcint;
    callback: PCefCallback): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  with TCefResourceHandlerOwn(TWACef.GetObject(self)) do
    Result := Ord(ReadResponse(data_out, bytes_to_read, bytes_read^, TCefCallbackRef.UnWrap(callback)));
end;

function cef_resource_handler_can_get_cookie(self: PCefResourceHandler;
  const cookie: PCefCookie): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  with TCefResourceHandlerOwn(TWACef.GetObject(self)) do
    Result := Ord(CanGetCookie(cookie));
end;

function cef_resource_handler_can_set_cookie(self: PCefResourceHandler;
  const cookie: PCefCookie): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  with TCefResourceHandlerOwn(TWACef.GetObject(self)) do
    Result := Ord(CanSetCookie(cookie));
end;

procedure cef_resource_handler_cancel(self: PCefResourceHandler); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  with TCefResourceHandlerOwn(TWACef.GetObject(self)) do
    Cancel;
end;

procedure TCefResourceHandlerOwn.Cancel;
begin

end;

function TCefResourceHandlerOwn.CanGetCookie(const cookie: PCefCookie): Boolean;
begin
  Result := False;
end;

function TCefResourceHandlerOwn.CanSetCookie(const cookie: PCefCookie): Boolean;
begin
  Result := False;
end;

constructor TCefResourceHandlerOwn.Create;
begin
  inherited CreateData(SizeOf(TCefResourceHandler));
  with PCefResourceHandler(FData)^ do
  begin
    process_request := @cef_resource_handler_process_request;
    get_response_headers := @cef_resource_handler_get_response_headers;
    read_response := @cef_resource_handler_read_response;
    can_get_cookie := @cef_resource_handler_can_get_cookie;
    can_set_cookie := @cef_resource_handler_can_set_cookie;
    cancel:= @cef_resource_handler_cancel;
  end;
end;

procedure TCefResourceHandlerOwn.GetResponseHeaders(
  const response: ICefResponse; out responseLength: Int64;
  out redirectUrl: ustring);
begin

end;

function TCefResourceHandlerOwn.ProcessRequest(const request: ICefRequest;
  const callback: ICefCallback): Boolean;
begin
  Result := False;
end;

function TCefResourceHandlerOwn.ReadResponse(const dataOut: Pointer;
  bytesToRead: cint; var bytesRead: cint;
  const callback: ICefCallback): Boolean;
begin
  Result := False;
end;

//..............................................................................TCefSchemeHandlerFactoryOwn
function cef_scheme_handler_factory_create(self: PCefSchemeHandlerFactory;
  browser: PCefBrowser; frame: PCefFrame; const scheme_name: PCefString;
  request: PCefRequest): PCefResourceHandler; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  with TCefSchemeHandlerFactoryOwn(TWACef.GetObject(self)) do
    Result := TWACef.GetData(New(TCefBrowserRef.UnWrap(browser), TCefFrameRef.UnWrap(frame),
      TWACef.ToString(scheme_name), TCefRequestRef.UnWrap(request)));
end;

constructor TCefSchemeHandlerFactoryOwn.Create(
  const AClass: TCefResourceHandlerClass; SyncMainThread: Boolean);
begin
  inherited CreateData(SizeOf(TCefSchemeHandlerFactory));
  FClass := AClass;
  with PCefSchemeHandlerFactory(FData)^ do
    create := @cef_scheme_handler_factory_create;
end;

function TCefSchemeHandlerFactoryOwn.New(const browser: ICefBrowser;
  const frame: ICefFrame; const schemeName: ustring;
  const request: ICefRequest): ICefResourceHandler;
begin
  Result := FClass.Create(browser, frame, schemeName, request);
end;

//..............................................................................TCefv8HandlerOwn
function cef_v8_handler_execute(self: PCefv8Handler;
  const name: PCefString; _object: PCefv8Value; argumentsCount: csize_t;
  const arguments: PPCefV8Value; var retval: PCefV8Value;
  var exception: TCefString): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
  args: TCefv8ValueArray;
  i: cint;
  ret: ICefv8Value;
  exc: ustring;
begin
  SetLength(args, argumentsCount);
  for i := 0 to argumentsCount - 1 do
    args[i] := TCefv8ValueRef.UnWrap(arguments[i]);

  Result := -Ord(TCefv8HandlerOwn(TWACef.GetObject(self)).Execute(
    TWACef.ToString(name), TCefv8ValueRef.UnWrap(_object), args, ret, exc));
  retval := TWACef.GetData(ret);
  ret := nil;
  exception := TWACef.ToCefString(exc);
end;

constructor TCefv8HandlerOwn.Create;
begin
  inherited CreateData(SizeOf(TCefv8Handler));
  with PCefv8Handler(FData)^ do
    execute := @cef_v8_handler_execute;
end;

function TCefv8HandlerOwn.Execute(const name: ustring; const obj: ICefv8Value;
  const arguments: TCefv8ValueArray; var retval: ICefv8Value;
  var exception: ustring): Boolean;
begin
  Result := False;
end;

//..............................................................................TCefTaskOwn
procedure cef_task_execute(self: PCefTask); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  TCefTaskOwn(TWACef.GetObject(self)).Execute;
end;

constructor TCefTaskOwn.Create;
begin
  inherited CreateData(SizeOf(TCefTask));
  with PCefTask(FData)^ do
    execute := @cef_task_execute;
end;

procedure TCefTaskOwn.Execute;
begin

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

constructor TCefStringMapOwn.Create;
begin
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

constructor TCefStringMultimapOwn.Create;
begin
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

//..............................................................................TCefResourceBundleHandlerOwn
function cef_resource_bundle_handler_get_localized_string(self: PCefResourceBundleHandler;
  message_id: cint; string_val: PCefString): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
  str: ustring;
begin
  Result := Ord(TCefResourceBundleHandlerOwn(TWACef.GetObject(self)).
    GetLocalizedString(message_id, str));
  if Result <> 0 then
    string_val^ := TWACef.ToCefString(str);
end;

function cef_resource_bundle_handler_get_data_resource(self: PCefResourceBundleHandler;
  resource_id: cint; var data: Pointer; var data_size: csize_t): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  Result := Ord(TCefResourceBundleHandlerOwn(TWACef.GetObject(self)).
    GetDataResource(resource_id, data, data_size));
end;

constructor TCefResourceBundleHandlerOwn.Create;
begin
  inherited CreateData(SizeOf(TCefResourceBundleHandler));
  with PCefResourceBundleHandler(FData)^ do
  begin
    get_localized_string := @cef_resource_bundle_handler_get_localized_string;
    get_data_resource := @cef_resource_bundle_handler_get_data_resource;
  end;
end;

//..............................................................................TCefFastResourceBundle

constructor TCefFastResourceBundle.Create(AGetDataResource: TGetDataResource;
  AGetLocalizedString: TGetLocalizedString);
begin
  inherited Create;
  FGetDataResource := AGetDataResource;
  FGetLocalizedString := AGetLocalizedString;
end;

function TCefFastResourceBundle.GetDataResource(resourceId: cint;
  out data: Pointer; out dataSize: csize_t): Boolean;
begin
  if Assigned(FGetDataResource) then
    Result := FGetDataResource(resourceId, data, dataSize) else
    Result := False;
end;

function TCefFastResourceBundle.GetLocalizedString(messageId: cint;
  out stringVal: ustring): Boolean;
begin
  if Assigned(FGetLocalizedString) then
    Result := FGetLocalizedString(messageId, stringVal) else
    Result := False;
end;

//..............................................................................TCefAppOwn
procedure cef_app_on_before_command_line_processing(self : PCefApp;
  const process_type : PCefString; command_line : PCefCommandLine); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  With TCefAppOwn(TWACef.GetObject(self)) do
    OnBeforeCommandLineProcessing(TWACef.ToString(process_type), TCefCommandLineRef.UnWrap(command_line));
end;

procedure cef_app_on_register_custom_schemes(self : PCefApp; registrar : PCefSchemeRegistrar); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  With TCefAppOwn(TWACef.GetObject(self)) do OnRegisterCustomSchemes(TCefSchemeRegistrarRef.UnWrap(registrar));
end;

function cef_app_get_resource_bundle_handler(self : PCefApp) : PCefResourceBundleHandler; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  Result := TWACef.GetData(TCefAppOwn(TWACef.GetObject(self)).GetResourceBundleHandler());
end;

function cef_app_get_browser_process_handler(self : PCefApp) : PCefBrowserProcessHandler; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  Result := TWACef.GetData(TCefAppOwn(TWACef.GetObject(self)).GetBrowserProcessHandler());
end;

function cef_app_get_render_process_handler(self : PCefApp) : PCefRenderProcessHandler; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  Result := TWACef.GetData(TCefAppOwn(TWACef.GetObject(self)).GetRenderProcessHandler());
end;

constructor TCefAppOwn.Create;
begin
  inherited CreateData(SizeOf(TCefApp));
  With PCefApp(FData)^ do
  begin
    on_before_command_line_processing := @cef_app_on_before_command_line_processing;
    on_register_custom_schemes := @cef_app_on_register_custom_schemes;
    get_resource_bundle_handler := @cef_app_get_resource_bundle_handler;
    get_browser_process_handler := @cef_app_get_browser_process_handler;
    get_render_process_handler := @cef_app_get_render_process_handler;
  end;
end;

//..............................................................................TCefUrlrequestClientOwn

procedure cef_url_request_client_on_request_complete(self: PCefUrlRequestClient; request: PCefUrlRequest); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  with TCefUrlrequestClientOwn(TWACef.GetObject(self)) do
    OnRequestComplete(TCefUrlRequestRef.UnWrap(request));
end;

procedure cef_url_request_client_on_upload_progress(self: PCefUrlRequestClient;
  request: PCefUrlRequest; current, total: cuint64); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  with TCefUrlrequestClientOwn(TWACef.GetObject(self)) do
    OnUploadProgress(TCefUrlRequestRef.UnWrap(request), current, total);
end;

procedure cef_url_request_client_on_download_progress(self: PCefUrlRequestClient;
  request: PCefUrlRequest; current, total: cuint64); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  with TCefUrlrequestClientOwn(TWACef.GetObject(self)) do
    OnDownloadProgress(TCefUrlRequestRef.UnWrap(request), current, total);
end;

procedure cef_url_request_client_on_download_data(self: PCefUrlRequestClient;
  request: PCefUrlRequest; const data: Pointer; data_length: csize_t); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  with TCefUrlrequestClientOwn(TWACef.GetObject(self)) do
    OnDownloadData(TCefUrlRequestRef.UnWrap(request), data, data_length);
end;

function cef_url_request_client_get_auth_credentials(self:PCefUrlrequestClient;
  isProxy: cint; const host:PCefString; port: cint; const realm:PCefString;
  const scheme:PCefString; callback:PCefAuthCallback): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  with TCefUrlrequestClientOwn(TWACef.GetObject(self)) do
    Result := Ord(GetAuthCredentials(
      IsProxy <> 0,
      TWACef.StringClearAndGet(Host^),
      Port,
      TWACef.StringClearAndGet(Realm^),
      TWACef.StringClearAndGet(Scheme^),
      TCefAuthCallbackRef.UnWrap(Callback)
     ));
end;

constructor TCefUrlrequestClientOwn.Create;
begin
  inherited CreateData(SizeOf(TCefUrlrequestClient));
  with PCefUrlrequestClient(FData)^ do
  begin
    on_request_complete := @cef_url_request_client_on_request_complete;
    on_upload_progress := @cef_url_request_client_on_upload_progress;
    on_download_progress := @cef_url_request_client_on_download_progress;
    on_download_data := @cef_url_request_client_on_download_data;
    get_auth_credentials := @cef_url_request_client_get_auth_credentials;
  end;
end;

procedure TCefUrlrequestClientOwn.OnRequestComplete(
  const request: ICefUrlRequest);
begin

end;

procedure TCefUrlrequestClientOwn.OnUploadProgress(
  const request: ICefUrlRequest; current, total: cuint64);
begin

end;

procedure TCefUrlrequestClientOwn.OnDownloadProgress(
  const request: ICefUrlRequest; current, total: cuint64);
begin

end;

procedure TCefUrlrequestClientOwn.OnDownloadData(const request: ICefUrlRequest;
  data: Pointer; dataLength: csize_t);
begin

end;

function TCefUrlRequestClientOwn.GetAuthCredentials(IsProxy: Boolean; const Host: ustring;
  Port: cint; const Realm: ustring; const Scheme: ustring; Callback: ICefAuthCallback): Boolean;
begin
  Result := False;
end;

//..............................................................................TCefWebPluginInfoVisitorOwn
function cef_web_plugin_info_visitor_visit(self: PCefWebPluginInfoVisitor;
      info: PCefWebPluginInfo; count, total: cint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  with TCefWebPluginInfoVisitorOwn(TWACef.GetObject(self)) do
    Result := Ord(Visit(TCefWebPluginInfoRef.UnWrap(info), count, total));
end;

constructor TCefWebPluginInfoVisitorOwn.Create;
begin
  inherited CreateData(SizeOf(TCefWebPluginInfoVisitor));
  PCefWebPluginInfoVisitor(FData).visit := @cef_web_plugin_info_visitor_visit;
end;

function TCefWebPluginInfoVisitorOwn.Visit(const info: ICefWebPluginInfo; count,
  total: cint): Boolean;
begin
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

//..............................................................................TCefWebPluginUnstableCallbackOwn
procedure cef_web_plugin_unstable_callback_is_unstable(
  self: PCefWebPluginUnstableCallback; const path: PCefString; unstable: cint); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  with TCefWebPluginUnstableCallbackOwn(TWACef.GetObject(self)) do
    IsUnstable(TWACef.ToString(path), unstable <> 0);
end;

constructor TCefWebPluginUnstableCallbackOwn.Create;
begin
  inherited CreateData(SizeOf(TCefWebPluginUnstableCallback));
  PCefWebPluginUnstableCallback(FData).is_unstable := @cef_web_plugin_unstable_callback_is_unstable;
end;

procedure TCefWebPluginUnstableCallbackOwn.IsUnstable(const path: ustring;
  unstable: Boolean);
begin

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

//..............................................................................TCefRenderProcessHandlerOwn

procedure cef_render_process_handler_on_render_thread_created(self: PCefRenderProcessHandler; ExtraInfo:PCefListValue); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  with TCefRenderProcessHandlerOwn(TWACef.GetObject(Self)) do
    OnRenderThreadCreated(TCefListValueRef.UnWrap(ExtraInfo));
end;

procedure cef_render_process_handler_on_web_kit_initialized(self: PCefRenderProcessHandler); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  with TCefRenderProcessHandlerOwn(TWACef.GetObject(Self)) do
    OnWebKitInitialized;
end;

procedure cef_render_process_handler_on_browser_created(self: PCefRenderProcessHandler;
  browser: PCefBrowser); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  with TCefRenderProcessHandlerOwn(TWACef.GetObject(Self)) do
    OnBrowserCreated(TCefBrowserRef.UnWrap(browser));
end;

procedure cef_render_process_handler_on_browser_destroyed(self: PCefRenderProcessHandler;
  browser: PCefBrowser); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  with TCefRenderProcessHandlerOwn(TWACef.GetObject(Self)) do
    OnBrowserDestroyed(TCefBrowserRef.UnWrap(browser));
end;

function cef_render_process_handler_get_load_handler(self: PCefRenderProcessHandler):PCefLoadHandler; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  Result:=TWACef.GetData(TCefRenderProcessHandlerOwn(TWACef.GetObject(Self)).GetLoadHandler);
end;

procedure cef_render_process_handler_on_context_created(self: PCefRenderProcessHandler;
  browser: PCefBrowser; frame: PCefFrame; context: PCefv8Context); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  with TCefRenderProcessHandlerOwn(TWACef.GetObject(Self)) do
    OnContextCreated(TCefBrowserRef.UnWrap(browser), TCefFrameRef.UnWrap(frame), TCefv8ContextRef.UnWrap(context));
end;

procedure cef_render_process_handler_on_context_released(self: PCefRenderProcessHandler;
  browser: PCefBrowser; frame: PCefFrame; context: PCefv8Context); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  with TCefRenderProcessHandlerOwn(TWACef.GetObject(Self)) do
    OnContextReleased(TCefBrowserRef.UnWrap(browser), TCefFrameRef.UnWrap(frame), TCefv8ContextRef.UnWrap(context));
end;

procedure cef_render_process_handler_on_uncaught_exception(self:PCefRenderProcessHandler;
        browser:PCefBrowser; frame:PCefFrame; context:PCefV8Context;
        exception:PCefV8Exception; stackTrace:PCefV8StackTrace); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  with TCefRenderProcessHandlerOwn(TWACef.GetObject(Self)) do
    OnUncaughtException(TCefBrowserRef.UnWrap(browser),TCefFrameRef.UnWrap(frame),
      TCefV8ContextRef.UnWrap(context),TCefV8ExceptionRef.UnWrap(exception),
      TCefV8StackTraceRef.UnWrap(stackTrace));
end;

procedure cef_render_process_handler_on_focused_node_changed(self: PCefRenderProcessHandler;
  browser: PCefBrowser; frame: PCefFrame; node: PCefDomNode); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  with TCefRenderProcessHandlerOwn(TWACef.GetObject(Self)) do
    OnFocusedNodeChanged(TCefBrowserRef.UnWrap(browser), TCefFrameRef.UnWrap(frame),
      TCefDomNodeRef.UnWrap(node));
end;

function cef_render_process_handler_on_process_message_received(self: PCefRenderProcessHandler;
  browser: PCefBrowser; source_process: TCefProcessId;
  message: PCefProcessMessage): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  with TCefRenderProcessHandlerOwn(TWACef.GetObject(Self)) do
    Result := Ord(OnProcessMessageReceived(TCefBrowserRef.UnWrap(browser), source_process,
      TCefProcessMessageRef.UnWrap(message)));
end;

constructor TCefRenderProcessHandlerOwn.Create;
begin
  inherited CreateData(SizeOf(TCefRenderProcessHandler));
  with PCefRenderProcessHandler(FData)^ do
  begin
    on_render_thread_created := @cef_render_process_handler_on_render_thread_created;
    on_web_kit_initialized := @cef_render_process_handler_on_web_kit_initialized;
    on_browser_created := @cef_render_process_handler_on_browser_created;
    on_browser_destroyed := @cef_render_process_handler_on_browser_destroyed;
    get_load_handler:= @cef_render_process_handler_get_load_handler;
    on_context_created := @cef_render_process_handler_on_context_created;
    on_context_released := @cef_render_process_handler_on_context_released;
    on_uncaught_exception:= @cef_render_process_handler_on_uncaught_exception;
    on_focused_node_changed := @cef_render_process_handler_on_focused_node_changed;
    on_process_message_received := @cef_render_process_handler_on_process_message_received;
  end;
end;

procedure TCefRenderProcessHandlerOwn.OnBrowserCreated(
  const browser: ICefBrowser);
begin

end;

procedure TCefRenderProcessHandlerOwn.OnBrowserDestroyed(
  const browser: ICefBrowser);
begin

end;

function TCefRenderProcessHandlerOwn.GetLoadHandler;
begin
  Result:=nil;
end;

function TCefRenderProcessHandlerOwn.OnBeforeNavigation(const Browser: ICefBrowser; const Frame: ICefFrame;
  const Request: ICefRequest; NavigationType: TCefNavigationType; IsRedirect: Boolean): Boolean;
begin
  Result := false;
end;

procedure TCefRenderProcessHandlerOwn.OnContextCreated(
  const browser: ICefBrowser; const frame: ICefFrame;
  const context: ICefv8Context);
begin

end;

procedure TCefRenderProcessHandlerOwn.OnContextReleased(
  const browser: ICefBrowser; const frame: ICefFrame;
  const context: ICefv8Context);
begin

end;

procedure TCefRenderProcessHandlerOwn.OnUncaughtException(const browser: ICefBrowser;
  const frame: ICefFrame; const context: ICefV8Context;
  const exception: ICefV8Exception; const stackTrace: ICefV8StackTrace);
begin

end;

procedure TCefRenderProcessHandlerOwn.OnFocusedNodeChanged(
  const browser: ICefBrowser; const frame: ICefFrame; const node: ICefDomNode);
begin

end;

function TCefRenderProcessHandlerOwn.OnProcessMessageReceived(
  const browser: ICefBrowser; sourceProcess: TCefProcessId;
  const message: ICefProcessMessage): Boolean;
begin
  Result := False;
end;

procedure TCefRenderProcessHandlerOwn.OnRenderThreadCreated;
begin

end;

procedure TCefRenderProcessHandlerOwn.OnWebKitInitialized;
begin

end;

//..............................................................................TCefBrowserProcessHandlerOwn
procedure cef_browser_process_handler_on_context_initialized(self: PCefBrowserProcessHandler); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  with TCefBrowserProcessHandlerOwn(TWACef.GetObject(self)) do
    OnContextInitialized;
end;

procedure cef_browser_process_handler_on_before_child_process_launch(
  self: PCefBrowserProcessHandler; command_line: PCefCommandLine); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  with TCefBrowserProcessHandlerOwn(TWACef.GetObject(self)) do
    OnBeforeChildProcessLaunch(TCefCommandLineRef.UnWrap(command_line));
end;

procedure cef_browser_render_process_thread_created(self:PCefBrowserProcessHandler; extra_info:PCefListValue); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  with TCefBrowserProcessHandlerOwn(TWACef.GetObject(self)) do
    OnRenderProcessThreadCreated(TCefListValueRef.UnWrap(extra_info));
end;

constructor TCefBrowserProcessHandlerOwn.Create;
begin
  inherited CreateData(SizeOf(TCefBrowserProcessHandler));
  with PCefBrowserProcessHandler(FData)^ do
  begin
    on_context_initialized := @cef_browser_process_handler_on_context_initialized;
    on_before_child_process_launch := @cef_browser_process_handler_on_before_child_process_launch;
    on_render_process_thread_created := @cef_browser_render_process_thread_created;
  end;
end;

procedure TCefBrowserProcessHandlerOwn.OnBeforeChildProcessLaunch(
  const commandLine: ICefCommandLine);
begin

end;

procedure TCefBrowserProcessHandlerOwn.OnContextInitialized;
begin

end;

procedure TCefBrowserProcessHandlerOwn.OnRenderProcessThreadCreated(const ExtraInfo: ICefListValue);
begin

end;

function TCefBrowserProcessHandlerOwn.GetPrintHandler: ICefPrintHandler;
begin
  Result := nil;
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
      sv.fd := v.GetDateValue;
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
      ud := v.GetUserData;
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
      ud := v.GetUserData;
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
    ud := TCefv8ValueRef.NewArray(1);
    rt := FCtx.GetType(v.TypeInfo);
    ud.SetValueByIndex(0, TCefv8ValueRef.NewInt(Integer(rt)));
    ret := TCefv8ValueRef.NewObject(nil);
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

    ud := TCefv8ValueRef.NewArray(2);
    ud.SetValueByIndex(0, TCefv8ValueRef.NewInt(Integer(rt)));
    ud.SetValueByIndex(1, TCefv8ValueRef.NewInt(Integer(v.AsObject)));
    ret := TCefv8ValueRef.NewObject(nil); // todo
    ret.SetUserData(ud);

    for m in rt.GetMethods do
      if m.Visibility > mvProtected then
      begin
        f := TCefv8ValueRef.NewFunction(m.Name, Self);
        ret.SetValueByKey(m.Name, f, V8_PROPERTY_ATTRIBUTE_NONE);
      end;

    for p in rt.GetProperties do
      if (p.Visibility > mvProtected) then
      begin
        if _g = nil then _g := ret.GetValueByKey('__defineGetter__');
        if _s = nil then _s := ret.GetValueByKey('__defineSetter__');
        SetLength(_a, 2);
        _a[0] := TCefv8ValueRef.NewString(p.Name);
        if p.IsReadable then
        begin
          _a[1] := TCefv8ValueRef.NewFunction('$pg' + p.Name, Self);
          _r := _g.ExecuteFunction(ret, _a);
        end;
        if p.IsWritable then
        begin
          _a[1] := TCefv8ValueRef.NewFunction('$ps' + p.Name, Self);
          _r := _s.ExecuteFunction(ret, _a);
        end;
      end;

    for fl in rt.GetFields do
      if (fl.Visibility > mvProtected) then
      begin
        if _g = nil then _g := ret.GetValueByKey('__defineGetter__');
        if _s = nil then _s := ret.GetValueByKey('__defineSetter__');

        SetLength(_a, 2);
        _a[0] := TCefv8ValueRef.NewString(fl.Name);
        _a[1] := TCefv8ValueRef.NewFunction('$vg' + fl.Name, Self);
        _r := _g.ExecuteFunction(ret, _a);
        _a[1] := TCefv8ValueRef.NewFunction('$vs' + fl.Name, Self);
        _r := _s.ExecuteFunction(ret, _a);
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

    ud := TCefv8ValueRef.NewArray(2);
    ud.SetValueByIndex(0, TCefv8ValueRef.NewInt(Integer(rt)));
    ud.SetValueByIndex(1, TCefv8ValueRef.NewInt(Integer(c)));
    ret := TCefv8ValueRef.NewObject(nil); // todo
    ret.SetUserData(ud);

    if c <> nil then
    begin
      //proto := ret.GetValueByKey('__proto__');
      for m in rt.GetMethods do
        if (m.Visibility > mvProtected) and (m.MethodKind in [mkClassProcedure, mkClassFunction]) then
        begin
          f := TCefv8ValueRef.NewFunction(m.Name, Self);
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
        ret := TCefv8ValueRef.NewInt(vr);
      varByte, varWord, varLongWord:
        ret := TCefv8ValueRef.NewUInt(vr);
      varUString, varOleStr, varString:
        ret := TCefv8ValueRef.NewString(vr);
      varSingle, varDouble, varCurrency, varUInt64, varInt64:
        ret := TCefv8ValueRef.NewDouble(vr);
      varBoolean:
        ret := TCefv8ValueRef.NewBool(vr);
      varNull:
        ret := TCefv8ValueRef.NewNull;
      varEmpty:
        ret := TCefv8ValueRef.NewUndefined;
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

    ud := TCefv8ValueRef.NewArray(2);
    ud.SetValueByIndex(0, TCefv8ValueRef.NewInt(Integer(rt)));
    {$IFDEF WIN32}
    ud.SetValueByIndex(1, TCefv8ValueRef.NewInt(Integer(v.AsInterface)));
    {$ELSE}
    ud.SetValueByIndex(1, TCefv8ValueRef.NewInt(Int64(v.AsInterface)));
    {$ENDIF}
    ret := TCefv8ValueRef.NewObject(nil);
    ret.SetUserData(ud);

    for m in rt.GetMethods do
      if m.Visibility > mvProtected then
      begin
        f := TCefv8ValueRef.NewFunction(m.Name, Self);
        ret.SetValueByKey(m.Name, f, V8_PROPERTY_ATTRIBUTE_NONE);
      end;

    Result := True;
  end;

  function ProcessFloat: Boolean;
  begin
    if v.TypeInfo = TypeInfo(TDateTime) then
      ret := TCefv8ValueRef.NewDate(TValueData(v).FAsDouble) else
      ret := TCefv8ValueRef.NewDouble(v.AsExtended);
    Result := True;
  end;

begin
  case v.TypeInfo.Kind of
    tkUString, tkLString, tkWString, tkChar, tkWChar:
      ret := TCefv8ValueRef.NewString(v.AsString);
    tkInteger: ret := TCefv8ValueRef.NewInt(v.AsInteger);
    tkEnumeration:
      if v.TypeInfo = TypeInfo(Boolean) then
        ret := TCefv8ValueRef.NewBool(v.AsBoolean) else
        ret := TCefv8ValueRef.NewInt(TValueData(v).FAsSLong);
    tkFloat: if not ProcessFloat then Exit(False);
    tkInt64: ret := TCefv8ValueRef.NewDouble(v.AsInt64);
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

function TCefRTTIExtension.Execute(const name: ustring; const obj: ICefv8Value;
  const arguments: TCefv8ValueArray; var retval: ICefv8Value;
  var exception: ustring): Boolean;
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
  p := PChar(name);
  m := nil;
  if obj <> nil then
  begin
    ud := obj.GetUserData;
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
                          Exit(SetValue(ret, retval));
                        end else
                        {$ENDIF}
                          Exit(SetValue(pr.GetValue(val), retval));
                      end;
                    's':
                      begin
                        inc(p);
                        pr := rt.GetProperty(p);
                        if GetValue(pr.PropertyType.Handle, arguments[0], ret) then
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
                          Exit(SetValue(ret, retval));
                        end else
                        {$ENDIF}
                          Exit(SetValue(vl.GetValue(val), retval));
                      end;
                    's':
                      begin
                        inc(p);
                        vl := rt.GetField(p);
                        if GetValue(vl.FieldType.Handle, arguments[0], ret) then
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
              m := rt.GetMethod(name);
          end;
        tkClassRef:
          begin
            val := nil;
            cls := TClass(ud.GetValueByIndex(1).GetIntValue);
            m := FCtx.GetType(cls).GetMethod(name);
          end;
      else
        m := nil;
        cls := nil;
        val := nil;
      end;

      prm := m.GetParameters;
      i := Length(prm);
      if i = Length(arguments) then
      begin
        SetLength(args, i);
        for i := 0 to i - 1 do
          if not GetValue(prm[i].ParamType.Handle, arguments[i], args[i]) then
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
          if not SetValue(ret, retval) then
            Exit(False);
      end else
        Exit(False);
    end else
    if p^ = '$' then
    begin
      inc(p);
      case p^ of
        'g': SetValue(FValue, retval);
        's': GetValue(FValue.TypeInfo, arguments[0], FValue);
      else
        Exit(False);
      end;
    end else
      Exit(False);
  end else
    Exit(False);
end;
{$ENDIF}

//..............................................................................TCefRunFileDialogCallbackOwn
procedure cef_run_file_dialog_callback_cont(self: PCefRunFileDialogCallback;
  browser_host: PCefBrowserHost; file_paths: TCefStringList); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
  list: TStringList;
  i: cint;
  str: TCefString;
begin
  list := TStringList.Create;
  try
    for i := 0 to cef_string_list_size(file_paths) - 1 do
    begin
      FillChar(str, SizeOf(str), 0);
      cef_string_list_value(file_paths, i, @str);
      list.Add(TWACef.StringClearAndGet(str));
    end;
    with TCefRunFileDialogCallbackOwn(TWACef.GetObject(self)) do
      cont(TCefBrowserHostRef.UnWrap(browser_host), list);
  finally
    list.Free;
  end;
end;

procedure TCefRunFileDialogCallbackOwn.Cont(const browserHost: ICefBrowserHost; filePaths: TStrings);
begin

end;

constructor TCefRunFileDialogCallbackOwn.Create;
begin
  inherited CreateData(SizeOf(TCefRunFileDialogCallback));
  with PCefRunFileDialogCallback(FData)^ do
    cont := @cef_run_file_dialog_callback_cont;
end;

//..............................................................................TCefFastRunFileDialogCallback
procedure TCefFastRunFileDialogCallback.Cont(const browserHost: ICefBrowserHost;
  filePaths: TStrings);
begin
  FCallback(browserHost, filePaths);
end;

constructor TCefFastRunFileDialogCallback.Create(
  callback: TCefRunFileDialogCallbackProc);
begin
  inherited Create;
  FCallback := callback;
end;

//..............................................................................TCefDialogHandlerOwn
function cef_dialog_handler_on_file_dialog(self: PCefDialogHandler; browser: PCefBrowser;
  mode: TCefFileDialogMode; const title, default_file_name: PCefString;
  accept_types: TCefStringList; callback: PCefFileDialogCallback): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
  list: TStringList;
  i: cint;
  str: TCefString;
begin
  list := TStringList.Create;
  try
    for i := 0 to cef_string_list_size(accept_types) - 1 do
    begin
      FillChar(str, SizeOf(str), 0);
      cef_string_list_value(accept_types, i, @str);
      list.Add(TWACef.StringClearAndGet(str));
    end;

    with TCefDialogHandlerOwn(TWACef.GetObject(self)) do
      Result := Ord(OnFileDialog(TCefBrowserRef.UnWrap(browser), mode, TWACef.ToString(title),
        TWACef.ToString(default_file_name), list, TCefFileDialogCallbackRef.UnWrap(callback)));
  finally
    list.Free;
  end;
end;

constructor TCefDialogHandlerOwn.Create;
begin
  CreateData(SizeOf(TCefDialogHandler));
  with PCefDialogHandler(FData)^ do
    on_file_dialog := @cef_dialog_handler_on_file_dialog;
end;

function TCefDialogHandlerOwn.OnFileDialog(const browser: ICefBrowser; mode: TCefFileDialogMode;
      const title: ustring; const DefaultFileName: ustring;
      AcceptTypes: TStrings; const callback: ICefFileDialogCallback): Boolean;
begin
  Result := False;
end;

//..............................................................................TCefRenderHandlerOwn
function cef_render_handler_get_root_screen_rect(self: PCefRenderHandler;
  browser: PCefBrowser; rect: PCefRect): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  with TCefRenderHandlerOwn(TWACef.GetObject(self)) do
    Result := Ord(GetRootScreenRect(TCefBrowserRef.UnWrap(browser), rect));
end;

function cef_render_handler_get_view_rect(self: PCefRenderHandler;
  browser: PCefBrowser; rect: PCefRect): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  with TCefRenderHandlerOwn(TWACef.GetObject(self)) do
    Result := Ord(GetViewRect(TCefBrowserRef.UnWrap(browser), rect));
end;

function cef_render_handler_get_screen_point(self: PCefRenderHandler;
  browser: PCefBrowser; viewX, viewY: cint; screenX, screenY: pcint): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  with TCefRenderHandlerOwn(TWACef.GetObject(self)) do
    Result := Ord(GetScreenPoint(TCefBrowserRef.UnWrap(browser), viewX, viewY, screenX, screenY));
end;

function cef_render_handler_get_screen_info(self: PCefRenderHandler;
  browser:PCefBrowser; out screen_info:PCefScreenInfo): cint; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  with TCefRenderHandlerOwn(TWACef.GetObject(self)) do
    Result := Ord(GetScreenInfo(TCefBrowserRef.UnWrap(browser), screen_info^));
end;

procedure cef_render_handler_on_popup_show(self: PCefRenderHandler;
  browser: PCefBrowser; show: cint); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  with TCefRenderHandlerOwn(TWACef.GetObject(self)) do
    OnPopupShow(TCefBrowserRef.UnWrap(browser), show <> 0);
end;

procedure cef_render_handler_on_popup_size(self: PCefRenderHandler;
  browser: PCefBrowser; const rect: PCefRect); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  with TCefRenderHandlerOwn(TWACef.GetObject(self)) do
    OnPopupSize(TCefBrowserRef.UnWrap(browser), rect);
end;

procedure cef_render_handler_on_paint(self: PCefRenderHandler;
  browser: PCefBrowser; _type: TCefPaintElementType; dirtyRectsCount: csize_t;
  const dirtyRects: PCefRectArray; const buffer: Pointer; width, height: cint); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  with TCefRenderHandlerOwn(TWACef.GetObject(self)) do
    OnPaint(TCefBrowserRef.UnWrap(browser), _type, dirtyRectsCount, dirtyRects,
      buffer, width, height);
end;

procedure cef_render_handler_on_cursor_change(self: PCefRenderHandler;
  browser: PCefBrowser; cursor: TCefCursorHandle); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  with TCefRenderHandlerOwn(TWACef.GetObject(self)) do
    OnCursorChange(TCefBrowserRef.UnWrap(browser), cursor);
end;

procedure cef_render_handler_on_scroll_offset_changed(self: PCefRenderHandler;
  browser: PCefBrowser); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
begin
  with TCefRenderHandlerOwn(TWACef.GetObject(self)) do
    OnScrollOffsetChanged(TCefBrowserRef.UnWrap(browser));
end;

constructor TCefRenderHandlerOwn.Create;
begin
  CreateData(SizeOf(TCefRenderHandler), False);
  with PCefRenderHandler(FData)^ do
  begin
    get_root_screen_rect := @cef_render_handler_get_root_screen_rect;
    get_view_rect := @cef_render_handler_get_view_rect;
    get_screen_point := @cef_render_handler_get_screen_point;
    get_screen_info := @cef_render_handler_get_screen_info;
    on_popup_show := @cef_render_handler_on_popup_show;
    on_popup_size := @cef_render_handler_on_popup_size;
    on_paint := @cef_render_handler_on_paint;
    on_cursor_change := @cef_render_handler_on_cursor_change;
    on_scroll_offset_changed := @cef_render_handler_on_scroll_offset_changed;
  end;
end;

function TCefRenderHandlerOwn.GetRootScreenRect(const browser: ICefBrowser;
  rect: PCefRect): Boolean;
begin
  Result := False;
end;

function TCefRenderHandlerOwn.GetScreenPoint(const browser: ICefBrowser; viewX,
  viewY: cint; screenX, screenY: pcint): Boolean;
begin
  Result := False;
end;

function TCefRenderHandlerOwn.GetScreenInfo(const browser: ICefBrowser; out ScreenInfo: TCefScreenInfo): Boolean;
begin
  Result := false;
end;

function TCefRenderHandlerOwn.GetViewRect(const browser: ICefBrowser;
  rect: PCefRect): Boolean;
begin
  Result := False;
end;

procedure TCefRenderHandlerOwn.OnCursorChange(const browser: ICefBrowser;
  cursor: TCefCursorHandle);
begin

end;

procedure TCefRenderHandlerOwn.OnScrollOffsetChanged(const browser: ICefBrowser);
begin

end;

procedure TCefRenderHandlerOwn.OnPaint(const browser: ICefBrowser;
  kind: TCefPaintElementType; dirtyRectsCount: csize_t;
  const dirtyRects: PCefRectArray; const buffer: Pointer; width, height: cint);
begin

end;

function TCefRenderHandlerOwn.StartDragging(const browser: ICefBrowser; const DragData: ICefDragData;
  AllowedOps: TCefDragOperationsMask; x: cint; y: cint): Boolean;
begin
  Result := false;
end;

procedure TCefRenderHandlerOwn.UpdateDragCursor(const browser: ICefBrowser; operation: TCefDragOperationsMask);
begin

end;

procedure TCefRenderHandlerOwn.OnPopupShow(const browser: ICefBrowser;
  show: Boolean);
begin

end;

procedure TCefRenderHandlerOwn.OnPopupSize(const browser: ICefBrowser;
  const rect: PCefRect);
begin

end;

end.