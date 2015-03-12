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
  TCefBaseRef=class(TInterfacedObject,ICefBase)
  private
    FData: Pointer;
  public
    constructor Create(data: Pointer); virtual;
    destructor Destroy; override;
    function Wrap: Pointer;
    class function UnWrap(data: Pointer): ICefBase;
  end;

  TCefContextMenuHandlerRef = class(TCefBaseRef, ICefContextMenuHandler)
  protected
    procedure OnBeforeContextMenu(const browser: ICefBrowser; const frame: ICefFrame;
      const params: ICefContextMenuParams; const model: ICefMenuModel);
    function OnContextMenuCommand(const browser: ICefBrowser; const frame: ICefFrame;
      const params: ICefContextMenuParams; commandId: cint;
      eventFlags: TCefEventFlags): Boolean;
    procedure OnContextMenuDismissed(const browser: ICefBrowser; const frame: ICefFrame);
  public
    class function UnWrap(data: Pointer): ICefContextMenuHandler;
  end;

  TCefDialogHandlerRef = class(TCefBaseRef, ICefDialogHandler)
  protected
    function OnFileDialog(const browser: ICefBrowser; mode: TCefFileDialogMode; const title: ustring; const defaultFileName: ustring; acceptTypes: TStrings; const callback: ICefFileDialogCallback): boolean;
  public
    class function UnWrap(data: Pointer): ICefDialogHandler;
  end;

  TCefDisplayHandlerRef = class(TCefBaseRef, ICefDisplayHandler)
  protected
    procedure OnAddressChange(const  browser: ICefBrowser; const  frame: ICefFrame; const  url: ustring);
    procedure OnTitleChange(const  browser: ICefBrowser; const  title: ustring);
    function OnTooltip(const  browser: ICefBrowser; var text: ustring): Boolean;
    procedure OnStatusMessage(const  browser: ICefBrowser; const  value: ustring);
    function OnConsoleMessage(const  browser: ICefBrowser; const  message, source: ustring; line: cint): Boolean;
  public
    class function UnWrap(data: Pointer): ICefDisplayHandler;
  end;

  TCefDownloadHandlerRef = class(TCefBaseRef, ICefDownloadHandler)
  protected
    procedure OnBeforeDownload(const  browser: ICefBrowser; const  downloadItem: ICefDownloadItem;
      const  suggestedName: ustring; const  callback: ICefBeforeDownloadCallback);
    procedure OnDownloadUpdated(const  browser: ICefBrowser; const  downloadItem: ICefDownloadItem;
      const  callback: ICefDownloadItemCallback);
  public
    class function UnWrap(data: Pointer): ICefDownloadHandler;
  end;

  TCefDragHandlerRef = class(TCefBaseRef, ICefDragHandler)
  protected
    function OnDragEnter(const  browser: ICefBrowser; const  dragData: ICefDragData; mask: TCefDragOperationsMask): Boolean;
  public
    class function UnWrap(data: Pointer): ICefDragHandler;
  end;

  TCefFocusHandlerRef = class(TCefBaseRef, ICefFocusHandler)
  protected
    procedure OnTakeFocus(const  browser: ICefBrowser; next: Boolean);
    function OnSetFocus(const  browser: ICefBrowser; source: TCefFocusSource): Boolean;
    procedure OnGotFocus(const  browser: ICefBrowser);
  public
    class function UnWrap(data: Pointer): ICefFocusHandler;
  end;

  TCefGeolocationHandlerRef = class(TCefBaseRef, ICefGeolocationHandler)
  protected
    procedure OnRequestGeolocationPermission(const  browser: ICefBrowser;
      const  requestingUrl: ustring; requestId: cint; const  callback: ICefGeolocationCallback);
    procedure OnCancelGeolocationPermission(const  browser: ICefBrowser;
      const  requestingUrl: ustring; requestId: cint);
  public
    class function UnWrap(data: Pointer): ICefGeolocationHandler;
  end;

  TCefJsDialogHandlerRef = class(TCefBaseRef, ICefJsDialogHandler)
  protected
    function OnJsdialog(const  browser: ICefBrowser; const  originUrl, acceptLang: ustring;
      dialogType: TCefJsDialogType; const  messageText, defaultPromptText: ustring;
      callback: ICefJsDialogCallback; out suppressMessage: Boolean): Boolean;
    function OnBeforeUnloadDialog(const  browser: ICefBrowser;
      const  messageText: ustring; isReload: Boolean;
      const  callback: ICefJsDialogCallback): Boolean;
    procedure OnResetDialogState(const  browser: ICefBrowser);
    procedure OnDialogClosed(const browser: ICefBrowser);
  public
    class function UnWrap(data: Pointer): ICefJsDialogHandler;
  end;

  TCefKeyboardHandlerRef = class(TCefBaseRef, ICefKeyboardHandler)
  protected
    function OnPreKeyEvent(const  browser: ICefBrowser; const  event: PCefKeyEvent;
      osEvent: TCefEventHandle; out isKeyboardShortcut: Boolean): Boolean;
    function OnKeyEvent(const  browser: ICefBrowser; const  event: PCefKeyEvent;
      osEvent: TCefEventHandle): Boolean;
  public
    class function UnWrap(data: Pointer): ICefKeyboardHandler;
  end;

  TCefLifeSpanHandlerRef = class(TCefBaseRef, ICefLifeSpanHandler)
  protected
    function OnBeforePopup(const parentBrowser: ICefBrowser; const frame: ICefFrame;
      var TargetURL: ustring; const TargetFrameName: ustring;
      var popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo;
      var client: ICefClient; var settings: TCefBrowserSettings; var NoJavascriptAccess: Boolean): Boolean;
    procedure OnAfterCreated(const  browser: ICefBrowser);
    procedure OnBeforeClose(const  browser: ICefBrowser);
    function RunModal(const  browser: ICefBrowser): Boolean;
    function DoClose(const  browser: ICefBrowser): Boolean;
  public
    class function UnWrap(data: Pointer): ICefLifeSpanHandler;
  end;

  TCefLoadHandlerRef = class(TCefBaseRef, ICefLoadHandler)
  protected
    procedure OnLoadingStateChange(const  browser: ICefBrowser; IsLoading: Boolean; CanGoBack: Boolean; CanGoForward: Boolean);
    procedure OnLoadStart(const  browser: ICefBrowser; const  frame: ICefFrame);
    procedure OnLoadEnd(const  browser: ICefBrowser; const  frame: ICefFrame; httpStatusCode: cint);
    procedure OnLoadError(const  browser: ICefBrowser; const  frame: ICefFrame; errorCode: TCefErrorCode;
      const  errorText, failedUrl: ustring);
  public
    class function UnWrap(data: Pointer): ICefLoadHandler;
  end;

  TCefRenderHandlerRef = class(TCefBaseRef, ICefRenderHandler)
  protected
    function GetRootScreenRect(const  browser: ICefBrowser; rect:PCefRect): Boolean;
    function GetViewRect(const  browser: ICefBrowser; rect:PCefRect): Boolean;
    function GetScreenPoint(const  browser: ICefBrowser; viewX, viewY: cint;
      screenX, screenY: pcint): Boolean;
    function GetScreenInfo(const browser: ICefBrowser; out ScreenInfo: TCefScreenInfo): Boolean;
    procedure OnPopupShow(const  browser: ICefBrowser; show: Boolean);
    procedure OnPopupSize(const  browser: ICefBrowser; const  rect:PCefRect);
    procedure OnPaint(const  browser: ICefBrowser; aType: TCefPaintElementType;
        dirtyRectsCount: csize_t; const  dirtyRects:PCefRectArray; const  buffer:Pointer;
        width: cint; height: cint);
    procedure OnCursorChange(const  browser: ICefBrowser; cursor: TCefCursorHandle);
    function StartDragging(const browser: ICefBrowser; const DragData: ICefDragData;
      AllowedOps: TCefDragOperationsMask; x: cint; y: cint): Boolean;
    procedure UpdateDragCursor(const browser: ICefBrowser; operation: TCefDragOperationsMask);
		procedure OnScrollOffsetChanged(const browser: ICefBrowser);
  public
    class function UnWrap(data: Pointer): ICefRenderHandler;
  end;

  TCefResourceHandlerRef = class(TCefBaseRef, ICefResourceHandler)
  protected
    function ProcessRequest(const  request: ICefRequest; const  callback: ICefCallback): Boolean;
    procedure GetResponseHeaders(const  response: ICefResponse;
      out responseLength: cint64; out redirectUrl: ustring);
    function ReadResponse(const  dataOut: Pointer; bytesToRead: cint;
      var bytesRead: cint; const  callback: ICefCallback): Boolean;
    function CanGetCookie(const  cookie: PCefCookie): Boolean;
    function CanSetCookie(const  cookie: PCefCookie): Boolean;
    procedure Cancel;
  public
    class function UnWrap(data: Pointer): ICefResourceHandler;
  end;

  TCefRequestHandlerRef = class(TCefBaseRef, ICefRequestHandler)
  protected
    function OnBeforeBrowse(const  browser: ICefBrowser; const  frame: ICefFrame;
      const  request: ICefRequest; IsRedirect: Boolean): Boolean;
    function OnBeforeResourceLoad(const  browser: ICefBrowser; const  frame: ICefFrame;
      const  request: ICefRequest): Boolean;
    function GetResourceHandler(const  browser: ICefBrowser; const  frame: ICefFrame;
      const  request: ICefRequest): ICefResourceHandler;
    procedure OnResourceRedirect(const  browser: ICefBrowser; const  frame: ICefFrame;
      const  oldUrl: ustring; var newUrl: ustring);
    function GetAuthCredentials(const  browser: ICefBrowser; const  frame: ICefFrame;
      isProxy: Boolean; const  host: ustring; port: cint; const  realm, scheme: ustring;
      const  callback: ICefAuthCallback): Boolean;
    function OnQuotaRequest(const  browser: ICefBrowser;
      const  originUrl: ustring; newSize: cint64; const  callback: ICefQuotaCallback): Boolean;
    function OnCertificateError(CertError: TCefErrorcode; const  RequestUrl: ustring;
      const  Callback: ICefAllowCertificateErrorCallback): boolean;
    procedure OnProtocolExecution(const  browser: ICefBrowser; const  url: ustring; out allowOsExecution: Boolean);
    function OnBeforePluginLoad(const  browser: ICefBrowser; const  url, policyUrl: ustring;
      const  info: ICefWebPluginInfo): Boolean;
    procedure OnPluginCrashed(const  browser: ICefBrowser; const  pluginPath: ustring);
    procedure OnRenderProcessTerminated(const  browser: ICefBrowser; status: TCefTerminationStatus);
  public
    class function UnWrap(data: Pointer): ICefRequestHandler;
  end;

  TCefClientRef=class(TCefBaseRef, ICefClient)
  protected
    function GetContextMenuHandler: ICefContextMenuHandler;
    function GetDialogHandler: ICefDialogHandler;
    function GetDisplayHandler: ICefDisplayHandler;
    function GetDownloadHandler: ICefDownloadHandler;
    function GetDragHandler: ICefDragHandler;
    function GetFocusHandler: ICefFocusHandler;
    function GetGeolocationHandler: ICefGeolocationHandler;
    function GetJsdialogHandler: ICefJsdialogHandler;
    function GetKeyboardHandler: ICefKeyboardHandler;
    function GetLifeSpanHandler: ICefLifeSpanHandler;
    function GetLoadHandler: ICefLoadHandler;
    function GetRenderHandler: ICefRenderHandler;
    function GetRequestHandler: ICefRequestHandler;
    function OnProcessMessageReceived(const browser: ICefBrowser;
      sourceProcess: TCefProcessId; const message: ICefProcessMessage): Boolean;
  public
    class function UnWrap(data: Pointer): ICefClient;
  end;

  TCefRequestContextHandlerRef=class(TCefBaseRef, ICefRequestContextHandler)
  protected
    function GetCookieManager: ICefCookieManager;
  public
    class function UnWrap(data: Pointer): ICefRequestContextHandler;
  end;

  TCefRequestContextRef=class(TCefBaseRef, ICefRequestContext)
  protected
    function IsSame(other: ICefRequestContext): Boolean;
    function IsGlobal: Boolean;
    function GetHandler: ICefRequestContextHandler;
  public
    class function UnWrap(data: Pointer): ICefRequestContext;
    class function Global: ICefRequestContext;
  end;

  TCefBrowserHostRef = class(TCefBaseRef, ICefBrowserHost)
  protected
    function GetBrowser: ICefBrowser;
    procedure CloseBrowser(aForceClose: Boolean);
    procedure SetFocus(enable: Boolean);
    procedure SetWindowVisibility(visible: Boolean);
    function GetWindowHandle: TCefWindowHandle;
    function GetOpenerWindowHandle: TCefWindowHandle;
    function GetClient: ICefClient;
    function GetRequestContext: ICefRequestContext;
    function GetZoomLevel: Double;
    procedure SetZoomLevel(zoomLevel: Double);
    procedure RunFileDialog(mode: TCefFileDialogMode; const  title: ustring; const  DefaultFileName: ustring;
      AcceptTypes: TStrings; const  callback: ICefRunFileDialogCallback);
    procedure RunFileDialogProc(mode: TCefFileDialogMode;
      const title, defaultFileName: ustring; acceptTypes: TStrings;
      const callback: TCefRunFileDialogCallbackProc);
    procedure StartDownload(const url: ustring);
    procedure Print;
    procedure Find(identifier: cint; const  searchText: ustring; fwd: Boolean; matchCase: Boolean; findNext: Boolean);
    procedure StopFinding(clearSelection: Boolean);
    procedure ShowDevTools(var windowInfo: TCefWindowInfo; const client: ICefClient; var settings: TCefBrowserSettings; InspectElementAt: TCefPoint); overload;
    procedure ShowDevTools(var windowInfo: TCefWindowInfo; const client: ICefClient; var settings: TCefBrowserSettings); overload;
    procedure CloseDevTools;
    procedure SetMouseCursorChangeDisabled(disabled: Boolean);
    function GetIsMouseCursorChangeDisabled: Boolean;
    procedure ReplaceMisspelling(const word: ustring);
    function GetIsWindowRenderingDisabled: Boolean;
    procedure WasResized;
    procedure WasHidden(hidden: Boolean);
    procedure NotifyScreenInfoChanged;
    procedure Invalidate(const aType: TCefPaintElementType);
    procedure SendKeyEvent(const  event: TCefKeyEvent);
    procedure SendMouseClickEvent(const  event: TCefMouseEvent; aType: TCefMouseButtonType;
      mouseUp: Boolean; clickCount: cint);
    procedure SendMouseMoveEvent(event: TCefMouseEvent; mouseLeave: Boolean);
    procedure SendMouseWheelEvent(const  event: TCefMouseEvent; deltaX: cint; deltaY: cint);
    procedure SendFocusEvent(aSetFocus: cint);
    procedure SendCaptureLostEvent;
    function GetNSTextInputContext: TCefTextInputContext;
    procedure HandleKeyEventBeforeTextInputClient(keyEvent: TCefEventHandle);
    procedure HandleKeyEventAfterTextInputClient(keyEvent: TCefEventHandle);
    procedure DragTargetDragEnter(const DragData: ICefDragData; const event: TCefMouseEvent; AllowedOps: TCefDragOperationsMask);
		procedure DragTargetDragOver(const event: TCefMouseEvent; AllowedOps: TCefDragOperationsMask);
		procedure DragTargetDragLeave;
		procedure DragTargetDrop(const event: TCefMouseEvent);
		procedure DragSourceEndedAt(x: cint; y: cint; op: TCefDragOperationsMask);
		procedure DragSourceSystemDragEnded;
  public
    class function UnWrap(data: Pointer): ICefBrowserHost;
  end;

  TCefFastV8Accessor = class(TCefV8AccessorOwn)
  private
    FGetter: TCefV8AccessorGetterProc;
    FSetter: TCefV8AccessorSetterProc;
  protected
    function Get(const name: ustring; const obj: ICefv8Value;
      out value: ICefv8Value; const exception: ustring): Boolean; override;
    function Put(const name: ustring; const obj, value: ICefv8Value;
      const exception: ustring): Boolean; override;
  public
    constructor Create(const getter: TCefV8AccessorGetterProc;
      const setter: TCefV8AccessorSetterProc); reintroduce;
  end;

  TCefV8ExceptionRef = class(TCefBaseRef, ICefV8Exception)
  protected
    function GetMessage: ustring;
    function GetSourceLine: ustring;
    function GetScriptResourceName: ustring;
    function GetLineNumber: cint;
    function GetStartPosition: cint;
    function GetEndPosition: cint;
    function GetStartColumn: cint;
    function GetEndColumn: cint;
  public
    class function UnWrap(data: Pointer): ICefV8Exception;
  end;

  TCefv8ValueRef = class(TCefBaseRef, ICefv8Value)
  protected
    function IsValid: Boolean;
    function IsUndefined: Boolean;
    function IsNull: Boolean;
    function IsBool: Boolean;
    function IsInt: Boolean;
    function IsUInt: Boolean;
    function IsDouble: Boolean;
    function IsDate: Boolean;
    function IsString: Boolean;
    function IsObject: Boolean;
    function IsArray: Boolean;
    function IsFunction: Boolean;
    function IsSame(const that: ICefv8Value): Boolean;
    function GetBoolValue: Boolean;
    function GetIntValue: cint;
    function GetUIntValue: csize_t;
    function GetDoubleValue: Double;
    function GetDateValue: TDateTime;
    function GetStringValue: ustring;
    function IsUserCreated: Boolean;
    function HasException: Boolean;
    function GetException: ICefV8Exception;
    function ClearException: Boolean;
    function WillRethrowExceptions: Boolean;
    function SetRethrowExceptions(rethrow: Boolean): Boolean;
    function HasValueByKey(const key: ustring): Boolean;
    function HasValueByIndex(index: cint): Boolean;
    function DeleteValueByKey(const key: ustring): Boolean;
    function DeleteValueByIndex(index: cint): Boolean;
    function GetValueByKey(const key: ustring): ICefv8Value;
    function GetValueByIndex(index: cint): ICefv8Value;
    function SetValueByKey(const key: ustring; const value: ICefv8Value;
      attribute: TCefV8PropertyAttribute): Boolean;
    function SetValueByIndex(index: cint; const value: ICefv8Value): Boolean;
    function SetValueByAccessor(const key: ustring; settings: TCefV8AccessControl;
      attribute: TCefV8PropertyAttribute): Boolean;
    function GetKeys(const keys: TStrings): cint;
    function SetUserData(const data: ICefv8Value): Boolean;
    function GetUserData: ICefv8Value;
    function GetExternallyAllocatedMemory: cint;
    function AdjustExternallyAllocatedMemory(changeInBytes: cint): cint;
    function GetArrayLength: cint;
    function GetFunctionName: ustring;
    function GetFunctionHandler: ICefv8Handler;
    function ExecuteFunction(const obj: ICefv8Value;
      const arguments: TCefv8ValueArray): ICefv8Value;
    function ExecuteFunctionWithContext(const context: ICefv8Context;
      const obj: ICefv8Value; const arguments: TCefv8ValueArray): ICefv8Value;
  public
    class function UnWrap(data: Pointer): ICefv8Value;
    class function NewUndefined: ICefv8Value;
    class function NewNull: ICefv8Value;
    class function NewBool(value: Boolean): ICefv8Value;
    class function NewInt(value: cint): ICefv8Value;
    class function NewUInt(value: csize_t): ICefv8Value;
    class function NewDouble(value: Double): ICefv8Value;
    class function NewDate(value: TDateTime): ICefv8Value;
    class function NewString(const str: ustring): ICefv8Value;
    class function NewObject(const Accessor: ICefV8Accessor): ICefv8Value;
    class function NewObjectProc(const getter: TCefV8AccessorGetterProc;
      const setter: TCefV8AccessorSetterProc): ICefv8Value;
    class function NewArray(len: cint): ICefv8Value;
    class function NewFunction(const name: ustring; const handler: ICefv8Handler): ICefv8Value;
  end;

  TCefv8ContextRef = class(TCefBaseRef, ICefv8Context)
  protected
    function GetTaskRunner: ICefTaskRunner;
    function IsValid: Boolean;
    function GetBrowser: ICefBrowser;
    function GetFrame: ICefFrame;
    function GetGlobal: ICefv8Value;
    function Enter: Boolean;
    function Exit: Boolean;
    function IsSame(const that: ICefv8Context): Boolean;
    function Eval(const code: ustring; var retval: ICefv8Value; var exception: ICefV8Exception): Boolean;
  public
    class function UnWrap(data: Pointer): ICefv8Context;
    class function Current: ICefv8Context;
    class function Entered: ICefv8Context;
  end;

  TCefV8StackFrameRef = class(TCefBaseRef, ICefV8StackFrame)
  protected
    function IsValid: Boolean;
    function GetScriptName: ustring;
    function GetScriptNameOrSourceUrl: ustring;
    function GetFunctionName: ustring;
    function GetLineNumber: cint;
    function GetColumn: cint;
    function IsEval: Boolean;
    function IsConstructor: Boolean;
  public
    class function UnWrap(data: Pointer): ICefV8StackFrame;
  end;

  TCefV8StackTraceRef = class(TCefBaseRef, ICefV8StackTrace)
  protected
    function IsValid: Boolean;
    function GetFrameCount: cint;
    function GetFrame(index: cint): ICefV8StackFrame;
  public
    class function UnWrap(data: Pointer): ICefV8StackTrace;
    class function Current(frameLimit: cint): ICefV8StackTrace;
  end;

  TCefv8HandlerRef = class(TCefBaseRef, ICefv8Handler)
  protected
    function Execute(const name: ustring; const obj: ICefv8Value;
      const arguments: TCefv8ValueArray; var retval: ICefv8Value;
      var exception: ustring): Boolean;
  public
    class function UnWrap(data: Pointer): ICefv8Handler;
  end;

  TCefFrameRef = class(TCefBaseRef, ICefFrame)
  protected
    function IsValid: Boolean;
    procedure Undo;
    procedure Redo;
    procedure Cut;
    procedure Copy;
    procedure Paste;
    procedure Del;
    procedure SelectAll;
    procedure ViewSource;
    procedure GetSource(const visitor: ICefStringVisitor);
    procedure GetSourceProc(const proc: TCefStringVisitorProc);
    procedure GetText(const visitor: ICefStringVisitor);
    procedure GetTextProc(const proc: TCefStringVisitorProc);
    procedure LoadRequest(const request: ICefRequest);
    procedure LoadUrl(const url: ustring);
    procedure LoadString(const str, url: ustring);
    procedure ExecuteJavaScript(const code, scriptUrl: ustring; startLine: cint);
    function IsMain: Boolean;
    function IsFocused: Boolean;
    function GetName: ustring;
    function GetIdentifier: cint64;
    function GetParent: ICefFrame;
    function GetUrl: ustring;
    function GetBrowser: ICefBrowser;
    function GetV8Context: ICefv8Context;
    procedure VisitDom(const visitor: ICefDomVisitor);
    procedure VisitDomProc(const proc: TCefDomVisitorProc);
  public
    class function UnWrap(data: Pointer): ICefFrame;
  end;

  TCefBrowserRef = class(TCefBaseRef, ICefBrowser)
  protected
    function GetHost: ICefBrowserHost;
    function CanGoBack: Boolean;
    procedure GoBack;
    function CanGoForward: Boolean;
    procedure GoForward;
    function IsLoading: Boolean;
    procedure Reload;
    procedure ReloadIgnoreCache;
    procedure StopLoad;
    function GetIdentifier: cint;
    function IsSame(const that: ICefBrowser): Boolean;
    function IsPopup: Boolean;
    function HasDocument: Boolean;
    function GetMainFrame: ICefFrame;
    function GetFocusedFrame: ICefFrame;
    function GetFrameByident(identifier: cint64): ICefFrame;
    function GetFrame(const name: ustring): ICefFrame;
    function GetFrameCount: csize_t;
    procedure GetFrameIdentifiers(count: pcsize_t; identifiers: pcint64);
    procedure GetFrameNames(names: TStrings);
    function SendProcessMessage(targetProcess: TCefProcessId;
      message: ICefProcessMessage): Boolean;
  public
    class function UnWrap(data: Pointer): ICefBrowser;
  end;

  TCefContextMenuParamsRef = class(TCefBaseRef, ICefContextMenuParams)
  protected
    function GetXCoord: cint;
    function GetYCoord: cint;
    function GetTypeFlags: TCefContextMenuTypeFlags;
    function GetLinkUrl: ustring;
    function GetUnfilteredLinkUrl: ustring;
    function GetSourceUrl: ustring;
    function HasImageContents: Boolean;
    function GetPageUrl: ustring;
    function GetFrameUrl: ustring;
    function GetFrameCharset: ustring;
    function GetMediaType: TCefContextMenuMediaType;
    function GetMediaStateFlags: TCefContextMenuMediaStateFlags;
    function GetSelectionText: ustring;
    function IsEditable: Boolean;
    function GetEditStateFlags: TCefContextMenuEditStateFlags;
  public
    class function UnWrap(data: Pointer): ICefContextMenuParams;
  end;

  TCefMenuModelRef = class(TCefBaseRef, ICefMenuModel)
  protected
    function Clear: Boolean;
    function GetCount: cint;
    function AddSeparator: Boolean;
    function AddItem(commandId: cint; const text: ustring): Boolean;
    function AddCheckItem(commandId: cint; const text: ustring): Boolean;
    function AddRadioItem(commandId: cint; const text: ustring; groupId: cint): Boolean;
    function AddSubMenu(commandId: cint; const text: ustring): ICefMenuModel;
    function InsertSeparatorAt(index: cint): Boolean;
    function InsertItemAt(index, commandId: cint; const text: ustring): Boolean;
    function InsertCheckItemAt(index, commandId: cint; const text: ustring): Boolean;
    function InsertRadioItemAt(index, commandId: cint; const text: ustring; groupId: cint): Boolean;
    function InsertSubMenuAt(index, commandId: cint; const text: ustring): ICefMenuModel;
    function Remove(commandId: cint): Boolean;
    function RemoveAt(index: cint): Boolean;
    function GetIndexOf(commandId: cint): cint;
    function GetCommandIdAt(index: cint): cint;
    function SetCommandIdAt(index, commandId: cint): Boolean;
    function GetLabel(commandId: cint): ustring;
    function GetLabelAt(index: cint): ustring;
    function SetLabel(commandId: cint; const text: ustring): Boolean;
    function SetLabelAt(index: cint; const text: ustring): Boolean;
    function GetType(commandId: cint): TCefMenuItemType;
    function GetTypeAt(index: cint): TCefMenuItemType;
    function GetGroupId(commandId: cint): cint;
    function GetGroupIdAt(index: cint): cint;
    function SetGroupId(commandId, groupId: cint): Boolean;
    function SetGroupIdAt(index, groupId: cint): Boolean;
    function GetSubMenu(commandId: cint): ICefMenuModel;
    function GetSubMenuAt(index: cint): ICefMenuModel;
    function IsVisible(commandId: cint): Boolean;
    function isVisibleAt(index: cint): Boolean;
    function SetVisible(commandId: cint; visible: Boolean): Boolean;
    function SetVisibleAt(index: cint; visible: Boolean): Boolean;
    function IsEnabled(commandId: cint): Boolean;
    function IsEnabledAt(index: cint): Boolean;
    function SetEnabled(commandId: cint; enabled: Boolean): Boolean;
    function SetEnabledAt(index: cint; enabled: Boolean): Boolean;
    function IsChecked(commandId: cint): Boolean;
    function IsCheckedAt(index: cint): Boolean;
    function setChecked(commandId: cint; checked: Boolean): Boolean;
    function setCheckedAt(index: cint; checked: Boolean): Boolean;
    function HasAccelerator(commandId: cint): Boolean;
    function HasAcceleratorAt(index: cint): Boolean;
    function SetAccelerator(commandId, keyCode: cint; shiftPressed, ctrlPressed, altPressed: Boolean): Boolean;
    function SetAcceleratorAt(index, keyCode: cint; shiftPressed, ctrlPressed, altPressed: Boolean): Boolean;
    function RemoveAccelerator(commandId: cint): Boolean;
    function RemoveAcceleratorAt(index: cint): Boolean;
    function GetAccelerator(commandId: cint; out keyCode: cint; out shiftPressed, ctrlPressed, altPressed: Boolean): Boolean;
    function GetAcceleratorAt(index: cint; out keyCode: cint; out shiftPressed, ctrlPressed, altPressed: Boolean): Boolean;
  public
    class function UnWrap(data: Pointer): ICefMenuModel;
  end;

  TCefDomNodeRef = class(TCefBaseRef, ICefDomNode)
  protected
    function GetType: TCefDomNodeType;
    function IsText: Boolean;
    function IsElement: Boolean;
    function IsEditable: Boolean;
    function IsFormControlElement: Boolean;
    function GetFormControlElementType: ustring;
    function IsSame(const that: ICefDomNode): Boolean;
    function GetName: ustring;
    function GetValue: ustring;
    function SetValue(const value: ustring): Boolean;
    function GetAsMarkup: ustring;
    function GetDocument: ICefDomDocument;
    function GetParent: ICefDomNode;
    function GetPreviousSibling: ICefDomNode;
    function GetNextSibling: ICefDomNode;
    function HasChildren: Boolean;
    function GetFirstChild: ICefDomNode;
    function GetLastChild: ICefDomNode;
    function GetElementTagName: ustring;
    function HasElementAttributes: Boolean;
    function HasElementAttribute(const attrName: ustring): Boolean;
    function GetElementAttribute(const attrName: ustring): ustring;
    procedure GetElementAttributes(const attrMap: ICefStringMap);
    function SetElementAttribute(const attrName, value: ustring): Boolean;
    function GetElementInnerText: ustring;
  public
    class function UnWrap(data: Pointer): ICefDomNode;
  end;

  TCefDomDocumentRef = class(TCefBaseRef, ICefDomDocument)
  protected
    function GetType: TCefDomDocumentType;
    function GetDocument: ICefDomNode;
    function GetBody: ICefDomNode;
    function GetHead: ICefDomNode;
    function GetTitle: ustring;
    function GetElementById(const id: ustring): ICefDomNode;
    function GetFocusedNode: ICefDomNode;
    function HasSelection: Boolean;
    function GetSelectionStartNode: ICefDomNode;
    function GetSelectionStartOffset: cint;
    function GetSelectionEndNode: ICefDomNode;
    function GetSelectionEndOffset: cint;
    function GetSelectionAsMarkup: ustring;
    function GetSelectionAsText: ustring;
    function GetBaseUrl: ustring;
    function GetCompleteUrl(const partialURL: ustring): ustring;
  public
    class function UnWrap(data: Pointer): ICefDomDocument;
  end;

  TCefDownLoadItemRef = class(TCefBaseRef, ICefDownLoadItem)
  protected
    function IsValid: Boolean;
    function IsInProgress: Boolean;
    function IsComplete: Boolean;
    function IsCanceled: Boolean;
    function GetCurrentSpeed: cint64;
    function GetPercentComplete: cint;
    function GetTotalBytes: cint64;
    function GetReceivedBytes: cint64;
    function GetStartTime: TDateTime;
    function GetEndTime: TDateTime;
    function GetFullPath: ustring;
    function GetId: cint;
    function GetUrl: ustring;
    function GetSuggestedFileName: ustring;
    function GetContentDisposition: ustring;
    function GetMimeType: ustring;
  public
    class function UnWrap(data: Pointer): ICefDownLoadItem;
  end;

  TCefBeforeDownloadCallbackRef = class(TCefBaseRef, ICefBeforeDownloadCallback)
  protected
    procedure Cont(const downloadPath: ustring; showDialog: Boolean);
  public
     class function UnWrap(data: Pointer): ICefBeforeDownloadCallback;
  end;

  TCefDownloadItemCallbackRef = class(TCefBaseRef, ICefDownloadItemCallback)
  protected
    procedure cancel;
  public
    class function UnWrap(data: Pointer): ICefDownloadItemCallback;
  end;

  TCefJsDialogCallbackRef = class(TCefBaseRef, ICefJsDialogCallback)
  protected
    procedure Cont(success: Boolean; const userInput: ustring);
  public
    class function UnWrap(data: Pointer): ICefJsDialogCallback;
  end;

  TCefGeolocationCallbackRef = class(TCefBaseRef, ICefGeolocationCallback)
  protected
    procedure Cont(allow: Boolean);
  public
    class function UnWrap(data: Pointer): ICefGeolocationCallback;
  end;

  TCefListValueRef = class(TCefBaseRef, ICefListValue)
  protected
    function IsValid: Boolean;
    function IsOwned: Boolean;
    function IsReadOnly: Boolean;
    function Copy: ICefListValue;
    function SetSize(size: csize_t): Boolean;
    function GetSize: csize_t;
    function Clear: Boolean;
    function Remove(index: cint): Boolean;
    function GetType(index: cint): TCefValueType;
    function GetBool(index: cint): Boolean;
    function GetInt(index: cint): cint;
    function GetDouble(index: cint): Double;
    function GetString(index: cint): ustring;
    function GetBinary(index: cint): ICefBinaryValue;
    function GetDictionary(index: cint): ICefDictionaryValue;
    function GetList(index: cint): ICefListValue;
    function SetNull(index: cint): Boolean;
    function SetBool(index: cint; value: Boolean): Boolean;
    function SetInt(index, value: cint): Boolean;
    function SetDouble(index: cint; value: Double): Boolean;
    function SetString(index: cint; const value: ustring): Boolean;
    function SetBinary(index: cint; const value: ICefBinaryValue): Boolean;
    function SetDictionary(index: cint; const value: ICefDictionaryValue): Boolean;
    function SetList(index: cint; const value: ICefListValue): Boolean;
  public
    class function UnWrap(data: Pointer): ICefListValue;
    class function New: ICefListValue;
  end;

  TCefBinaryValueRef = class(TCefBaseRef, ICefBinaryValue)
  protected
    function IsValid: Boolean;
    function IsOwned: Boolean;
    function Copy: ICefBinaryValue;
    function GetSize: csize_t;
    function GetData(buffer: Pointer; bufferSize, dataOffset: csize_t): csize_t;
  public
    class function UnWrap(data: Pointer): ICefBinaryValue;
    class function New(const data: Pointer; dataSize: csize_t): ICefBinaryValue;
  end;

  TCefDictionaryValueRef = class(TCefBaseRef, ICefDictionaryValue)
  protected
    function IsValid: Boolean;
    function isOwned: Boolean;
    function IsReadOnly: Boolean;
    function Copy(excludeEmptyChildren: Boolean): ICefDictionaryValue;
    function GetSize: csize_t;
    function Clear: Boolean;
    function HasKey(const key: ustring): Boolean;
    function GetKeys(const keys: TStrings): Boolean;
    function Remove(const key: ustring): Boolean;
    function GetType(const key: ustring): TCefValueType;
    function GetBool(const key: ustring): Boolean;
    function GetInt(const key: ustring): cint;
    function GetDouble(const key: ustring): Double;
    function GetString(const key: ustring): ustring;
    function GetBinary(const key: ustring): ICefBinaryValue;
    function GetDictionary(const key: ustring): ICefDictionaryValue;
    function GetList(const key: ustring): ICefListValue;
    function SetNull(const key: ustring): Boolean;
    function SetBool(const key: ustring; value: Boolean): Boolean;
    function SetInt(const key: ustring; value: cint): Boolean;
    function SetDouble(const key: ustring; value: Double): Boolean;
    function SetString(const key, value: ustring): Boolean;
    function SetBinary(const key: ustring; const value: ICefBinaryValue): Boolean;
    function SetDictionary(const key: ustring; const value: ICefDictionaryValue): Boolean;
    function SetList(const key: ustring; const value: ICefListValue): Boolean;
  public
    class function UnWrap(data: Pointer): ICefDictionaryValue;
    class function New: ICefDictionaryValue;
  end;

  TCefPostDataRef = class(TCefBaseRef, ICefPostData)
  protected
    function IsReadOnly: Boolean;
    function GetElementCount: csize_t;
    function GetElements(Count: csize_t): IInterfaceList; // ICefPostDataElement
    function RemoveElement(const element: ICefPostDataElement): cint;
    function AddElement(const element: ICefPostDataElement): cint;
    procedure RemoveElements;
  public
    class function UnWrap(data: Pointer): ICefPostData;
    class function New: ICefPostData;
  end;

  TCefPostDataElementRef = class(TCefBaseRef, ICefPostDataElement)
  protected
    function IsReadOnly: Boolean;
    procedure SetToEmpty;
    procedure SetToFile(const fileName: ustring);
    procedure SetToBytes(size: csize_t; const bytes: Pointer);
    function GetType: TCefPostDataElementType;
    function GetFile: ustring;
    function GetBytesCount: csize_t;
    function GetBytes(size: csize_t; bytes: Pointer): csize_t;
  public
    class function UnWrap(data: Pointer): ICefPostDataElement;
    class function New: ICefPostDataElement;
  end;

  TCefRequestRef = class(TCefBaseRef, ICefRequest)
  protected
    function IsReadOnly: Boolean;
    function GetUrl: ustring;
    function GetMethod: ustring;
    function GetPostData: ICefPostData;
    procedure GetHeaderMap(const HeaderMap: ICefStringMultimap);
    procedure SetUrl(const value: ustring);
    procedure SetMethod(const value: ustring);
    procedure SetPostData(const value: ICefPostData);
    procedure SetHeaderMap(const HeaderMap: ICefStringMultimap);
    function GetFlags: TCefUrlRequestFlags;
    procedure SetFlags(flags: TCefUrlRequestFlags);
    function GetFirstPartyForCookies: ustring;
    procedure SetFirstPartyForCookies(const url: ustring);
    procedure Assign(const url, method: ustring;
      const postData: ICefPostData; const headerMap: ICefStringMultimap);
    function GetResourceType: TCefResourceType;
    function GetTransitionType: TCefTransitionType;
  public
    class function UnWrap(data: Pointer): ICefRequest;
    class function New: ICefRequest;
  end;

  TCefStreamReaderRef = class(TCefBaseRef, ICefStreamReader)
  protected
    function Read(ptr: Pointer; size, n: csize_t): csize_t;
    function Seek(offset: cint64; whence: cint): cint;
    function Tell: cint64;
    function Eof: Boolean;
    function MayBlock: Boolean;
  public
    class function UnWrap(data: Pointer): ICefStreamReader;
    class function CreateForFile(const filename: ustring): ICefStreamReader;
    class function CreateForCustomStream(const stream: ICefCustomStreamReader): ICefStreamReader;
    class function CreateForStream(const stream: TSTream; owned: Boolean): ICefStreamReader;
    class function CreateForData(data: Pointer; size: csize_t): ICefStreamReader;
  end;

  TCefQuotaCallbackRef = class(TCefBaseRef, ICefQuotaCallback)
  protected
    procedure Cont(allow: Boolean);
    procedure Cancel;
  public
     class function UnWrap(data: Pointer): ICefQuotaCallback;
  end;

  TCefCallbackRef = class(TCefBaseRef, ICefCallback)
  protected
    procedure Cont;
    procedure Cancel;
  public
    class function UnWrap(data: Pointer): ICefCallback;
  end;

  TCefXmlReaderRef = class(TCefBaseRef, ICefXmlReader)
  protected
    function MoveToNextNode: Boolean;
    function Close: Boolean;
    function HasError: Boolean;
    function GetError: ustring;
    function GetType: TCefXmlNodeType;
    function GetDepth: cint;
    function GetLocalName: ustring;
    function GetPrefix: ustring;
    function GetQualifiedName: ustring;
    function GetNamespaceUri: ustring;
    function GetBaseUri: ustring;
    function GetXmlLang: ustring;
    function IsEmptyElement: Boolean;
    function HasValue: Boolean;
    function GetValue: ustring;
    function HasAttributes: Boolean;
    function GetAttributeCount: csize_t;
    function GetAttributeByIndex(index: cint): ustring;
    function GetAttributeByQName(const qualifiedName: ustring): ustring;
    function GetAttributeByLName(const localName, namespaceURI: ustring): ustring;
    function GetInnerXml: ustring;
    function GetOuterXml: ustring;
    function GetLineNumber: cint;
    function MoveToAttributeByIndex(index: cint): Boolean;
    function MoveToAttributeByQName(const qualifiedName: ustring): Boolean;
    function MoveToAttributeByLName(const localName, namespaceURI: ustring): Boolean;
    function MoveToFirstAttribute: Boolean;
    function MoveToNextAttribute: Boolean;
    function MoveToCarryingElement: Boolean;
  public
    class function UnWrap(data: Pointer): ICefXmlReader;
    class function New(const stream: ICefStreamReader;
      encodingType: TCefXmlEncodingType; const URI: ustring): ICefXmlReader;
  end;

  TCefZipReaderRef = class(TCefBaseRef, ICefZipReader)
  protected
    function MoveToFirstFile: Boolean;
    function MoveToNextFile: Boolean;
    function MoveToFile(const fileName: ustring; caseSensitive: Boolean): Boolean;
    function Close: Boolean;
    function GetFileName: ustring;
    function GetFileSize: cint64;
    function GetFileLastModified: LongInt;
    function OpenFile(const password: ustring): Boolean;
    function CloseFile: Boolean;
    function ReadFile(buffer: Pointer; bufferSize: csize_t): cint;
    function Tell: cint64;
    function Eof: Boolean;
  public
    class function UnWrap(data: Pointer): ICefZipReader;
    class function New(const stream: ICefStreamReader): ICefZipReader;
  end;

  TCefResponseRef = class(TCefBaseRef, ICefResponse)
  protected
    function IsReadOnly: Boolean;
    function GetStatus: cint;
    procedure SetStatus(status: cint);
    function GetStatusText: ustring;
    procedure SetStatusText(const StatusText: ustring);
    function GetMimeType: ustring;
    procedure SetMimeType(const mimetype: ustring);
    function GetHeader(const name: ustring): ustring;
    procedure GetHeaderMap(const headerMap: ICefStringMultimap);
    procedure SetHeaderMap(const headerMap: ICefStringMultimap);
  public
    class function UnWrap(data: Pointer): ICefResponse;
    class function New: ICefResponse;
  end;

  TCefCookieManagerRef = class(TCefBaseRef, ICefCookieManager)
  protected
    procedure SetSupportedSchemes(schemes: TStrings);
    function VisitAllCookies(const visitor: ICefCookieVisitor): Boolean;
    function VisitAllCookiesProc(const visitor: TCefCookieVisitorProc): Boolean;
    function VisitUrlCookies(const url: ustring;
      includeHttpOnly: Boolean; const visitor: ICefCookieVisitor): Boolean;
    function VisitUrlCookiesProc(const url: ustring;
      includeHttpOnly: Boolean; const visitor: TCefCookieVisitorProc): Boolean;
    function SetCookie(const url: ustring; const name, value, domain, path: ustring; secure, httponly,
      hasExpires: Boolean; const creation, lastAccess, expires: TDateTime): Boolean;
    function DeleteCookies(const url, cookieName: ustring): Boolean;
    function SetStoragePath(const path: ustring; PersistSessionCookies: Boolean): Boolean;
    function FlushStore(callback: ICefCompletionCallback): Boolean;
  public
    class function UnWrap(data: Pointer): ICefCookieManager;
    class function Global: ICefCookieManager;
  end;

  TCefWebPluginInfoRef = class(TCefBaseRef, ICefWebPluginInfo)
  protected
    function GetName: ustring;
    function GetPath: ustring;
    function GetVersion: ustring;
    function GetDescription: ustring;
  public
    class function UnWrap(data: Pointer): ICefWebPluginInfo;
  end;

  TCefProcessMessageRef = class(TCefBaseRef, ICefProcessMessage)
  protected
    function IsValid: Boolean;
    function IsReadOnly: Boolean;
    function Copy: ICefProcessMessage;
    function GetName: ustring;
    function GetArgumentList: ICefListValue;
  public
    class function UnWrap(data: Pointer): ICefProcessMessage;
    class function New(const name: ustring): ICefProcessMessage;
  end;

  TCefPrintSettingsRef = class(TCefBaseRef, ICefPrintSettings)
  protected
    function IsValid: Boolean;
    function IsReadOnly: Boolean;
    function Copy: ICefPrintSettings;
    procedure SetOrientation(Landscape: Boolean);
    function IsLandscape: Boolean;
    procedure SetPrinterPrintableArea(const PhysicalSizeDeviceUnits: TCefSize; const PrintableAreaDeviceUnits: TCefRect; LandscapeNeedsFlip: Boolean);
    procedure SetDeviceName(const Name: ustring);
    function GetDeviceName: ustring;
    procedure SetDpi(Dpi: CInt);
    function GetDpi: CInt;
    procedure SetPageRanges(RangesCount: csize_t; const Ranges: PCefPageRangeArray);
    function GetPageRangesCount: csize_t;
    procedure GetPageRanges(RangesCount: csize_t; Ranges: PCefPageRangeArray);
    procedure SetSelectionOnly(SelectionOnly: Boolean);
    function IsSelectionOnly: Boolean;
    procedure SetCollate(Collate: Boolean);
    function WillCollate: Boolean;
    procedure SetColorModel(Model: TCefColorModel);
    function GetColorModel: TCefColorModel;
    procedure SetCopies(Copies: CInt);
    function GetCopies: CInt;
    procedure SetDuplexMode(Mode: TCefDuplexMode);
    function GetDuplexMode: TCefDuplexMode;
  public
    class function UnWrap(data: Pointer): ICefPrintSettings;
    class function New(const name: ustring): ICefPrintSettings;
  end;

  TCefAuthCallbackRef = class(TCefBaseRef, ICefAuthCallback)
  protected
    procedure Cont(const username, password: ustring);
    procedure Cancel;
  public
     class function UnWrap(data: Pointer): ICefAuthCallback;
  end;

  TCefCommandLineRef = class(TCefBaseRef, ICefCommandLine)
  protected
    function IsValid: Boolean;
    function IsReadOnly: Boolean;
    function Copy: ICefCommandLine;
    procedure InitFromArgv(argc: cint; const argv: PPAnsiChar);
    procedure InitFromString(const commandLine: ustring);
    procedure Reset;
    procedure GetArgv(argv: TStrings);
    function GetCommandLineString: ustring;
    function GetProgram: ustring;
    procedure SetProgram(const _program: ustring);
    function HasSwitches: Boolean;
    function HasSwitch(const name: ustring): Boolean;
    function GetSwitchValue(const name: ustring): ustring;
    procedure GetSwitches(switches: ICefStringMap);
    procedure AppendSwitch(const name: ustring);
    procedure AppendSwitchWithValue(const name: ustring; const value: ustring);
    function HasArguments: Boolean;
    procedure GetArguments(arguments: TStrings);
    procedure AppendArgument(const argument: ustring);
    procedure PrependWrapper(const wrapper: ustring);
  public
    class function UnWrap(data: Pointer): ICefCommandLine;
    class function New: ICefCommandLine;
  end;

  TCefSchemeRegistrarRef = class(TCefBaseRef, ICefSchemeRegistrar)
  protected
    function AddCustomScheme(const schemeName: ustring; IsStandard, IsLocal,
      IsDisplayIsolated: Boolean): Boolean; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
  public
    class function UnWrap(data: Pointer): ICefSchemeRegistrar;
  end;

  TCefUrlRequestClientRef = class(TCefBaseRef, ICefUrlRequestClient)
  protected
    procedure OnRequestComplete(const  request: ICefUrlRequest);
    procedure OnUploadProgress(const  request: ICefUrlRequest; current, total: UInt64);
    procedure OnDownloadProgress(const  request: ICefUrlRequest; current, total: UInt64);
    procedure OnDownloadData(const  request: ICefUrlRequest; data: Pointer; dataLength: csize_t);
    function GetAuthCredentials(IsProxy: Boolean; const Host: ustring; Port: CInt; const Realm: ustring; const Scheme: ustring; Callback: ICefAuthCallback): Boolean;
  public
    class function UnWrap(data: Pointer): ICefUrlRequestClient;
  end;

  TCefUrlRequestRef = class(TCefBaseRef, ICefUrlRequest)
  protected
    function GetRequest: ICefRequest;
    function GetClient: ICefUrlrequestClient;
    function GetRequestStatus: TCefUrlRequestStatus;
    function GetRequestError: TCefErrorcode;
    function GetResponse: ICefResponse;
    procedure Cancel;
  public
    class function UnWrap(data: Pointer): ICefUrlRequest;
    class function New(const request: ICefRequest; const client: ICefUrlRequestClient): ICefUrlRequest;
  end;

  TCefTaskRunnerRef = class(TCefBaseRef, ICefTaskRunner)
  protected
    function IsSame(that: ICefTaskRunner): Boolean;
    function BelongsToCurrentThread: Boolean;
    function BelongsToThread(ThreadID: TCefThreadID): Boolean;
    function PostTask(task: ICefTask): cint;
    function PostDelayedTask(task: ICefTask; DelayMS:Int64): cint;
  public
    class function UnWrap(data: Pointer): ICefTaskRunner;
    class function GetForThread(const ThreadID: TCefThreadID): ICefTaskRunner;
    class function GetForCurrentThread: ICefTaskRunner;
  end;

  TCefTaskRef = class(TCefBaseRef, ICefTask)
  protected
    procedure Execute; virtual;
  public
    class function UnWrap(data: Pointer): ICefTask;
  end;

  TCefFileDialogCallbackRef = class(TCefBaseRef, ICefFileDialogCallback)
  protected
    procedure Cont(filePaths: TStrings);
    procedure Cancel;
  public
    class function UnWrap(data: Pointer): ICefFileDialogCallback;
  end;

  TCefAllowCertificateErrorCallbackRef=class(TCefBaseRef,ICefAllowCertificateErrorCallback)
  protected
    procedure Cont(allow: Boolean);
  public
    class function UnWrap(data: Pointer): ICefAllowCertificateErrorCallback;
  end;

  TCefDragDataRef=class(TCefBaseRef,ICefDragData)
  protected
    function Clone: ICefDragData;
		function IsReadOnly: Boolean;
		function IsLink: boolean;
		function IsFragment: Boolean;
		function IsFile: Boolean;
		function GetLinkUrl: ustring;
		function GetLinkTitle: ustring;
		function GetLinkMetadata: ustring;
		function GetFragmentText: ustring;
		function GetFragmentHTML: ustring;
		function GetFragmentBaseURL: ustring;
		function GetFileName: ustring;
		function GetFileContents(const writer: ICefStreamWriter): csize_t;
		function GetFileNames(names: TStrings): Boolean;
		procedure SetLinkURL(const url: ustring);
		procedure SetLinkTitle(const title: ustring);
		procedure SetLinkMetadata(const data: ustring);
		procedure SetFragmentText(const text: ustring);
		procedure SetFragmentHTML(const HTML: ustring);
		procedure SetFragmentBaseURL(const BaseUrl: ustring);
		procedure ResetFileContents;
		procedure AddFile(const path: ustring; const DisplayName: ustring);
  public
    class function UnWrap(data: Pointer): ICefDragData;
    class function New: ICefDragData;
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
    Result := Create(data) as ICefBase else
    Result := nil;
end;

function TCefBaseRef.Wrap: Pointer;
begin
  Result := FData;
  if Assigned(PCefBase(FData)^.add_ref) then
    PCefBase(FData)^.add_ref(PCefBase(FData));
end;

//..............................................................................TCefContextMenuHandlerRef
//Protected section
procedure TCefContextMenuHandlerRef.OnBeforeContextMenu(const browser: ICefBrowser; const frame: ICefFrame; const params: ICefContextMenuParams; const model: ICefMenuModel);
begin
  PCefContextMenuHandler(FData)^.on_before_context_menu(
    PCefContextMenuHandler(FData),
    TWACef.GetData(browser),
    TWACef.GetData(frame),
    TWACef.GetData(params),
    TWACef.GetData(model)
  );
end;

function TCefContextMenuHandlerRef.OnContextMenuCommand(const browser: ICefBrowser; const frame: ICefFrame; const params: ICefContextMenuParams; commandId: cint; eventFlags: TCefEventFlags): boolean;
begin
  result := PCefContextMenuHandler(FData)^.on_context_menu_command(
    PCefContextMenuHandler(FData),
    TWACef.GetData(browser),
    TWACef.GetData(frame),
    TWACef.GetData(params),
    commandId,
    eventFlags
  ) <> 0;
end;

procedure TCefContextMenuHandlerRef.OnContextMenuDismissed(const browser: ICefBrowser; const frame: ICefFrame);
begin
  PCefContextMenuHandler(FData)^.on_context_menu_dismissed(
    PCefContextMenuHandler(FData),
    TWACef.GetData(browser),
    TWACef.GetData(frame)
  );
end;

//Public section
class function TCefContextMenuHandlerRef.UnWrap(data: Pointer): ICefContextMenuHandler;
begin
  if data <> nil then
    Result := Create(data) as ICefContextMenuHandler else
    Result := nil;
end;

//..............................................................................TCefDialogHandlerRef
//Protected section
function TCefDialogHandlerRef.OnFileDialog(const browser: ICefBrowser; mode: TCefFileDialogMode; const title: ustring; const defaultFileName: ustring; acceptTypes: TStrings; const callback: ICefFileDialogCallback): boolean;
var
  t, f : TCefString;
  list: TCefStringList;
  item: TCefString;
  i: cint;
begin
  t := TWACef.ToCefString(title);
  f := TWACef.ToCefString(defaultFileName);
  list:=cef_string_list_alloc();
  try
    for i := 0 to acceptTypes.Count - 1 do
    begin
      item := TWACef.ToCefString(acceptTypes[i]);
      cef_string_list_append(list, @item);
    end;
    result := PCefDialogHandler(FData)^.on_file_dialog(
      PCefDialoghandler(FData),
      TWACef.GetData(browser),
      mode,
      @t,
      @f,
      list,
      TWACef.GetData(callback)
    ) <> 0;
  finally
    cef_string_list_free(list);
  end;
end;

//Public section
class function TCefDialogHandlerRef.UnWrap(data: Pointer): ICefDialogHandler;
begin
  if data <> nil then
    Result := Create(data) as ICefDialogHandler else
    Result := nil;
end;

//..............................................................................TCefDisplayHandlerRef
//Protected section
procedure TCefDisplayHandlerRef.OnAddressChange(const browser: ICefBrowser; const frame: ICefFrame; const url: ustring);
var
  u: TCefString;
begin
  u := TWACef.ToCefString(url);
  PCefDisplayHandler(FData)^.on_address_change(
    PCefDisplayHandler(FData),
    TWACef.GetData(browser),
    TWACef.GetData(frame),
    @u
  );
end;

procedure TCefDisplayHandlerRef.OnTitleChange(const  browser: ICefBrowser; const  title: ustring);
var
  u: TCefString;
begin
  u := TWACef.ToCefString(title);
  PCefDisplayHandler(FData)^.on_title_change(
    PCefDisplayHandler(FData),
    TWACef.GetData(browser),
    @u
  );
end;

function TCefDisplayHandlerRef.OnTooltip(const  browser: ICefBrowser; var text: ustring): Boolean;
var
  u: TCefString;
begin
  u := TWACef.ToCefString(text);
  result := PCefDisplayHandler(FData)^.on_tooltip(
    PCefDisplayHandler(FData),
    TWACef.GetData(browser),
    @u
  ) <> 0;
end;

procedure TCefDisplayHandlerRef.OnStatusMessage(const  browser: ICefBrowser; const  value: ustring);
var
  u: TCefString;
begin
  u := TWACef.ToCefString(value);
  PCefDisplayHandler(FData)^.on_status_message(
    PCefDisplayHandler(FData),
    TWACef.GetData(browser),
    @u
  );
end;

function TCefDisplayHandlerRef.OnConsoleMessage(const  browser: ICefBrowser; const  message, source: ustring; line: cint): Boolean;
var
  m, u: TCefString;
begin
  m := TWACef.ToCefString(message);
  u := TWACef.ToCefString(source);
  result := PCefDisplayHandler(FData)^.on_console_message(
    PCefDisplayHandler(FData),
    TWACef.GetData(browser),
    @m,
    @u,
    line
  ) <> 0;
end;

//Public section
class function TCefDisplayHandlerRef.UnWrap(data: Pointer): ICefDisplayHandler;
begin
  if data <> nil then
    Result := Create(data) as ICefDisplayHandler else
    Result := nil;
end;

//..............................................................................TCefDownloadHandlerRef
//Protected section
procedure TCefDownloadHandlerRef.OnBeforeDownload(const  browser: ICefBrowser; const  downloadItem: ICefDownloadItem;
  const  suggestedName: ustring; const  callback: ICefBeforeDownloadCallback);
var
  u: TCefString;
begin
  u := TWACef.ToCefString(suggestedName);
  PCefDownloadHandler(FData)^.on_before_download(
    PCefDownloadHandler(FData),
    TWACef.GetData(browser),
    TWACef.GetData(downloadItem),
    @u,
    TWACef.GetData(callback)
  );
end;

procedure TCefDownloadHandlerRef.OnDownloadUpdated(const  browser: ICefBrowser; const  downloadItem: ICefDownloadItem;
  const  callback: ICefDownloadItemCallback);
begin
  PCefDownloadHandler(FData)^.on_download_updated(
    PCefDownloadHandler(FData),
    TWACef.GetData(browser),
    TWACef.GetData(downloadItem),
    TWACef.GetData(callback)
  );
end;

//Public section
class function TCefDownloadHandlerRef.UnWrap(data: Pointer): ICefDownloadHandler;
begin
  if data <> nil then
    Result := Create(data) as ICefDownloadHandler else
    Result := nil;
end;

//..............................................................................TCefDragHandlerRef
//Protected section
function TCefDragHandlerRef.OnDragEnter(const  browser: ICefBrowser; const dragData: ICefDragData; mask: TCefDragOperationsMask): Boolean;
begin
  result := PCefDragHandler(FData)^.on_drag_enter(
    PCefDragHandler(FData),
    TWACef.GetData(browser),
    TWACef.GetData(dragData),
    mask
  ) <> 0;
end;


//Public section
class function TCefDragHandlerRef.UnWrap(data: Pointer): ICefDragHandler;
begin
  if data <> nil then
    Result := Create(data) as ICefDragHandler else
    Result := nil;
end;

//..............................................................................TCefFocusHandlerRef
//Protected section
procedure TCefFocusHandlerRef.OnTakeFocus(const  browser: ICefBrowser; next: Boolean);
begin
  PCefFocusHandler(FData)^.on_take_focus(
    PCefFocusHandler(FData),
    TWACef.GetData(browser),
    ord(next)
  );
end;

function TCefFocusHandlerRef.OnSetFocus(const  browser: ICefBrowser; source: TCefFocusSource): Boolean;
begin
  result := PCefFocusHandler(FData)^.on_set_focus(
    PCefFocusHandler(FData),
    TWACef.GetData(browser),
    source
  ) <> 0;
end;

procedure TCefFocusHandlerRef.OnGotFocus(const  browser: ICefBrowser);
begin
  PCefFocusHandler(FData)^.on_got_focus(
    PCefFocusHandler(FData),
    TWACef.GetData(browser)
  );
end;

//Public section
class function TCefFocusHandlerRef.UnWrap(data: Pointer): ICefFocusHandler;
begin
  if data <> nil then
    Result := Create(data) as ICefFocusHandler else
    Result := nil;
end;

//..............................................................................TCefGeolocationHandlerRef
//Protected section
procedure TCefGeolocationHandlerRef.OnRequestGeolocationPermission(const  browser: ICefBrowser;
  const  requestingUrl: ustring; requestId: cint; const  callback: ICefGeolocationCallback);
var
  u: TCefString;
begin
  u := TWACef.ToCefString(requestingUrl);
  PCefGeolocationHandler(FData)^.on_request_geolocation_permission(
    PCefGeolocationHandler(FData),
    TWACef.GetData(browser),
    @u,
    requestId,
    TWACef.GetData(callback)
  );
end;

procedure TCefGeolocationHandlerRef.OnCancelGeolocationPermission(const  browser: ICefBrowser;
  const  requestingUrl: ustring; requestId: cint);
var
  u: TCefString;
begin
  u := TWACef.ToCefString(requestingUrl);
  PCefGeolocationHandler(FData)^.on_cancel_geolocation_permission(
    PCefGeolocationHandler(FData),
    TWACef.GetData(browser),
    @u,
    requestId
  );
end;

//Public section
class function TCefGeolocationHandlerRef.UnWrap(data: Pointer): ICefGeolocationHandler;
begin
  if data <> nil then
    Result := Create(data) as ICefGeolocationHandler else
    Result := nil;
end;

//..............................................................................TCefJsDialogHandlerRef
//Protected section
function TCefJsDialogHandlerRef.OnJsdialog(const  browser: ICefBrowser; const  originUrl, acceptLang: ustring;
  dialogType: TCefJsDialogType; const  messageText, defaultPromptText: ustring;
  callback: ICefJsDialogCallback; out suppressMessage: Boolean): Boolean;
var
  u, a, m, d: TCefString;
  sm: pcint;
begin
  u := TWACef.ToCefString(originUrl);
  a := TWACef.ToCefString(acceptLang);
  m := TWACef.ToCefString(messageText);
  d := TWACef.ToCefString(defaultPromptText);
  new(sm);
  try
    result := PCefJsDialogHandler(FData)^.on_jsdialog(
      PCefJsDialogHandler(FData),
      TWACef.GetData(browser),
      @u,
      @a,
      dialogType,
      @m,
      @d,
      TWACef.GetData(callback),
      sm
    ) <> 0;
    suppressMessage := sm^ <> 0;
  finally
    Dispose(sm);
  end;
end;

function TCefJsDialogHandlerRef.OnBeforeUnloadDialog(const  browser: ICefBrowser;
  const  messageText: ustring; isReload: Boolean;
  const  callback: ICefJsDialogCallback): Boolean;
var
  m: TCefString;
begin
  m := TWACef.ToCefString(messageText);
  result := PCefJsDialogHandler(FData)^.on_before_unload_dialog(
    PCefJsDialogHandler(FData),
    TWACef.GetData(browser),
    @m,
    ord(isReload),
    TWACef.GetData(callback)
  ) <> 0;
end;

procedure TCefJsDialogHandlerRef.OnResetDialogState(const  browser: ICefBrowser);
begin
  PCefJsDialogHandler(FData)^.on_reset_dialog_state(
    PCefJsDialogHandler(FData),
    TWACef.GetData(browser)
  );
end;

procedure TCefJsDialogHandlerRef.OnDialogClosed(const  browser: ICefBrowser);
begin
  PCefJsDialogHandler(FData)^.on_dialog_closed(
    PCefJsDialogHandler(FData),
    TWACef.GetData(browser)
  );
end;

//Public section
class function TCefJsDialogHandlerRef.UnWrap(data: Pointer): ICefJsDialogHandler;
begin
  if data <> nil then
    Result := Create(data) as ICefJsDialogHandler else
    Result := nil;
end;

//..............................................................................TCefKeyboardHandlerRef
//Protected section
function TCefKeyboardHandlerRef.OnPreKeyEvent(const  browser: ICefBrowser; const  event: PCefKeyEvent;
  osEvent: TCefEventHandle; out isKeyboardShortcut: Boolean): Boolean;
var
  ks: pcint;
begin
  new(ks);
  try
    result := PCefKeyboardHandler(FData)^.on_pre_key_event(
      PCefKeyboardHandler(FData),
      TWACef.GetData(browser),
      event,
      osEvent,
      ks
    ) <> 0;
    isKeyboardShortcut := ks^ <> 0;
  finally
    Dispose(ks);
  end;
end;

function TCefKeyboardHandlerRef.OnKeyEvent(const  browser: ICefBrowser; const  event: PCefKeyEvent;
  osEvent: TCefEventHandle): Boolean;
begin
  result := PCefKeyboardHandler(FData)^.on_key_event(
    PCefKeyboardHandler(FData),
    TWACef.GetData(browser),
    event,
    osEvent
  ) <> 0;
end;

//Public section
class function TCefKeyboardHandlerRef.UnWrap(data: Pointer): ICefKeyboardHandler;
begin
  if data <> nil then
    Result := Create(data) as ICefKeyboardHandler else
    Result := nil;
end;

//..............................................................................TCefLifeSpanHandlerRef
//Protected section
function TCefLifeSpanHandlerRef.OnBeforePopup(const parentBrowser: ICefBrowser; const frame: ICefFrame;
      var TargetURL: ustring; const TargetFrameName: ustring;
      var popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo;
      var client: ICefClient; var settings: TCefBrowserSettings; var NoJavascriptAccess: Boolean): Boolean;
var
  u, f: TCefString;
  jsa: pcint;
begin
  u := TWACef.ToCefString(TargetURL);
  f := TWACef.ToCefString(TargetFrameName);
  new(jsa);
  jsa^ := ord(NoJavascriptAccess);
  try
  result := PCefLifeSpanHandler(FData)^.on_before_popup(
    PCefLifeSpanHandler(FData),
    TWACef.GetData(parentBrowser),
    TWACef.GetData(frame),
    @u,
    @f,
    @popupFeatures,
    @windowInfo,
    TWACef.GetData(client),
    @settings,
    jsa
  ) <> 0;
  TargetURL := TWACef.StringClearAndGet(u);
  NoJavascriptAccess := jsa^ <> 0;
  finally
    Dispose(jsa);
  end;
end;

procedure TCefLifeSpanHandlerRef.OnAfterCreated(const  browser: ICefBrowser);
begin
  PCefLifeSpanHandler(FData)^.on_after_created(
    PCefLifeSpanHandler(FData),
    TWACef.GetData(browser)
  );
end;

procedure TCefLifeSpanHandlerRef.OnBeforeClose(const  browser: ICefBrowser);
begin
  PCefLifeSpanHandler(FData)^.on_before_close(
    PCefLifeSpanHandler(FData),
    TWACef.GetData(browser)
  );
end;

function TCefLifeSpanHandlerRef.RunModal(const  browser: ICefBrowser): Boolean;
begin
  PCefLifeSpanHandler(FData)^.run_modal(
    PCefLifeSpanHandler(FData),
    TWACef.GetData(browser)
  );
end;

function TCefLifeSpanHandlerRef.DoClose(const  browser: ICefBrowser): Boolean;
begin
  PCefLifeSpanHandler(FData)^.do_close(
    PCefLifeSpanHandler(FData),
    TWACef.GetData(browser)
  );
end;

//Public section
class function TCefLifeSpanHandlerRef.UnWrap(data: Pointer): ICefLifeSpanHandler;
begin
  if data <> nil then
    Result := Create(data) as ICefLifeSpanHandler else
    Result := nil;
end;

//..............................................................................TCefLoadHandlerRef
//Protected section
procedure TCefLoadHandlerRef.OnLoadingStateChange(const  browser: ICefBrowser; IsLoading: Boolean; CanGoBack: Boolean; CanGoForward: Boolean);
begin
  PCefLoadHandler(FData)^.on_loading_state_change(
    PCefLoadHandler(FData),
    TWACef.GetData(browser),
    Ord(IsLoading),
    Ord(CanGoBack),
    Ord(CanGoForward)
  );
end;

procedure TCefLoadHandlerRef.OnLoadStart(const  browser: ICefBrowser; const  frame: ICefFrame);
begin
  PCefLoadHandler(FData)^.on_load_start(
    PCefLoadHandler(FData),
    TWACef.GetData(browser),
    TWACef.GetData(frame)
  );
end;

procedure TCefLoadHandlerRef.OnLoadEnd(const  browser: ICefBrowser; const  frame: ICefFrame; httpStatusCode: cint);
begin
  PCefLoadHandler(FData)^.on_load_end(
    PCefLoadHandler(FData),
    TWACef.GetData(browser),
    TWACef.GetData(frame),
    httpStatusCode
  );
end;

procedure TCefLoadHandlerRef.OnLoadError(const  browser: ICefBrowser; const  frame: ICefFrame; errorCode: TCefErrorCode;
  const  errorText, failedUrl: ustring);
var
  e, u: TCefString;
begin
  e := TWACef.ToCefString(errorText);
  u := TWACef.ToCefString(failedUrl);
  PCefLoadHandler(FData)^.on_load_error(
    PCefLoadHandler(FData),
    TWACef.GetData(browser),
    TWACef.GetData(frame),
    errorCode,
    @e,
    @u
  );
end;

//Public section
class function TCefLoadHandlerRef.UnWrap(data: Pointer): ICefLoadHandler;
begin
  if data <> nil then
    Result := Create(data) as ICefLoadHandler else
    Result := nil;
end;

//..............................................................................TCefRenderHandlerRef
//Protected section
function TCefRenderHandlerRef.GetRootScreenRect(const  browser: ICefBrowser; rect:PCefRect): Boolean;
begin
  result := PCefRenderHandler(FData)^.get_root_screen_rect(
    PCefRenderHandler(FData),
    TWACef.GetData(browser),
    rect
  ) <> 0;
end;

function TCefRenderHandlerRef.GetViewRect(const  browser: ICefBrowser; rect:PCefRect): Boolean;
begin
  result := PCefRenderHandler(FData)^.get_view_rect(
    PCefRenderHandler(FData),
    TWACef.GetData(browser),
    rect
  ) <> 0;
end;

function TCefRenderHandlerRef.GetScreenPoint(const  browser: ICefBrowser; viewX, viewY: cint;
  screenX, screenY: pcint): Boolean;
begin
  result := PCefRenderHandler(FData)^.get_screen_point(
    PCefRenderHandler(FData),
    TWACef.GetData(browser),
    viewX,
    viewY,
    screenX,
    screenY
  ) <> 0;
end;

function TCefRenderHandlerRef.GetScreenInfo(const browser: ICefBrowser; out ScreenInfo: TCefScreenInfo): boolean;
var
  si: PCefScreenInfo;
begin
  new(si);
  try
    result := PCefRenderHandler(FData)^.get_screen_info(
      PCefRenderHandler(FData),
      TWACef.GetData(browser),
      si
    ) <> 0;
    ScreenInfo := si^;
  finally
    Dispose(si);
  end;
end;

procedure TCefRenderHandlerRef.OnPopupShow(const  browser: ICefBrowser; show: Boolean);
begin
  PCefRenderHandler(FData)^.on_popup_show(
    PCefRenderHandler(FData),
    TWACef.GetData(browser),
    Ord(show)
  );
end;

procedure TCefRenderHandlerRef.OnPopupSize(const  browser: ICefBrowser; const  rect:PCefRect);
begin
  PCefRenderHandler(FData)^.on_popup_size(
    PCefRenderHandler(FData),
    TWACef.GetData(browser),
    rect
  );
end;

procedure TCefRenderHandlerRef.OnPaint(const  browser: ICefBrowser; aType: TCefPaintElementType;
    dirtyRectsCount: csize_t; const  dirtyRects:PCefRectArray; const  buffer:Pointer;
    width: cint; height: cint);
begin
  PCefRenderHandler(FData)^.on_paint(
    PCefRenderHandler(FData),
    TWACef.GetData(browser),
    aType,
    dirtyRectsCount,
    dirtyRects,
    buffer,
    width,
    height
  );
end;

procedure TCefRenderHandlerRef.OnCursorChange(const  browser: ICefBrowser; cursor: TCefCursorHandle);
begin
  PCefRenderHandler(FData)^.on_cursor_change(
    PCefRenderHandler(FData),
    TWACef.GetData(browser),
    cursor
  );
end;

function TCefRenderHandlerRef.StartDragging(const browser: ICefBrowser; const DragData: ICefDragData;
  AllowedOps: TCefDragOperationsMask; x: cint; y: cint): Boolean;
begin
  PCefRenderHandler(FData)^.start_dragging(
    PCefRenderHandler(FData),
    TWACef.GetData(browser),
    TWACef.GetData(DragData),
    AllowedOps,
    x,
    y
  );
end;

procedure TCefRenderHandlerRef.UpdateDragCursor(const browser: ICefBrowser; operation: TCefDragOperationsMask);
begin
  PCefRenderHandler(FData)^.update_drag_cursor(
    PCefRenderHandler(FData),
    TWACef.GetData(browser),
    operation
  );
end;

procedure TCefRenderHandlerRef.OnScrollOffsetChanged(const browser: ICefBrowser);
begin
  PCefRenderHandler(FData)^.on_scroll_offset_changed(
    PCefRenderHandler(FData),
    TWACef.GetData(browser)
  );
end;

//Public section
class function TCefRenderHandlerRef.UnWrap(data: Pointer): ICefRenderHandler;
begin
  if data <> nil then
    Result := Create(data) as ICefRenderHandler else
    Result := nil;
end;

//..............................................................................TCefResourceHandlerRef
//Protected section
function TCefResourceHandlerRef.ProcessRequest(const  request: ICefRequest; const  callback: ICefCallback): Boolean;
begin
  result := PCefResourceHandler(FData)^.process_request(
    PCefResourceHandler(FData),
    TWACef.GetData(request),
    TWACef.GetData(callback)
  ) <> 0;
end;

procedure TCefResourceHandlerRef.GetResponseHeaders(const  response: ICefResponse;
  out responseLength: cint64; out redirectUrl: ustring);
var
  rl: pcint64;
  ru: TCefString;
begin
  new(rl);
  try
    PCefResourceHandler(FData)^.get_response_headers(
      PCefResourceHandler(FData),
      TWACef.GetData(response),
      rl,
      @ru
    );
    responseLength := rl^;
    redirectUrl := TWACef.StringClearAndGet(ru);
  finally
    Dispose(rl);
  end;
end;

function TCefResourceHandlerRef.ReadResponse(const  dataOut: Pointer; bytesToRead: cint;
  var bytesRead: cint; const  callback: ICefCallback): Boolean;
var
  br: pcint;
begin
  new(br);
  try
    result := PCefResourceHandler(FData)^.read_response(
      PCefResourceHandler(FData),
      dataOut,
      bytesToRead,
      br,
      TWACef.GetData(callback)
    ) <> 0;
    bytesRead := br^;
  finally
    Dispose(br);
  end;
end;

function TCefResourceHandlerRef.CanGetCookie(const  cookie: PCefCookie): Boolean;
begin
  result := PCefResourceHandler(FData)^.can_get_cookie(
    PCefResourceHandler(FData),
    cookie
  ) <> 0;
end;

function TCefResourceHandlerRef.CanSetCookie(const  cookie: PCefCookie): Boolean;
begin
  result := PCefResourceHandler(FData)^.can_set_cookie(
    PCefResourceHandler(FData),
    cookie
  ) <> 0;
end;

procedure TCefResourceHandlerRef.Cancel;
begin
  PCefResourceHandler(FData)^.cancel(
    PCefResourceHandler(FData)
  );
end;

//Public section
class function TCefResourceHandlerRef.UnWrap(data: Pointer): ICefResourceHandler;
begin
  if data <> nil then
    Result := Create(data) as ICefResourceHandler else
    Result := nil;
end;

//..............................................................................TCefRequestHandlerRef
//Protected section
function TCefRequestHandlerRef.OnBeforeBrowse(const  browser: ICefBrowser; const  frame: ICefFrame;
  const  request: ICefRequest; IsRedirect: Boolean): Boolean;
begin
  result := PCefRequestHandler(FData)^.on_before_browse(
    PCefRequestHandler(FData),
    TWACef.GetData(browser),
    TWACef.GetData(frame),
    TWACef.GetData(request),
    Ord(IsRedirect)
  ) <> 0;
end;

function TCefRequestHandlerRef.OnBeforeResourceLoad(const  browser: ICefBrowser; const  frame: ICefFrame;
  const  request: ICefRequest): Boolean;
begin
  result := PCefRequestHandler(FData)^.on_before_resource_load(
    PCefRequestHandler(FData),
    TWACef.GetData(browser),
    TWACef.GetData(frame),
    TWACef.GetData(request)
  ) <> 0;
end;

function TCefRequestHandlerRef.GetResourceHandler(const  browser: ICefBrowser; const  frame: ICefFrame;
  const  request: ICefRequest): ICefResourceHandler;
begin
  result := TCefResourceHandlerRef.UnWrap(PCefRequestHandler(FData)^.get_resource_handler(
    PCefRequestHandler(FData),
    TWACef.GetData(browser),
    TWACef.GetData(frame),
    TWACef.GetData(request)
  ));
end;

procedure TCefRequestHandlerRef.OnResourceRedirect(const  browser: ICefBrowser; const  frame: ICefFrame;
  const  oldUrl: ustring; var newUrl: ustring);
var
  o, n: TCefString;
begin
  o := TWACef.ToCefString(oldUrl);
  n := TWACef.ToCefString(newUrl);
  PCefRequestHandler(FData)^.on_resource_redirect(
    PCefRequestHandler(FData),
    TWACef.GetData(browser),
    TWACef.GetData(frame),
    @o,
    @n
  );
  newUrl := TWACef.StringClearAndGet(n);
end;

function TCefRequestHandlerRef.GetAuthCredentials(const  browser: ICefBrowser; const  frame: ICefFrame;
  isProxy: Boolean; const  host: ustring; port: cint; const  realm, scheme: ustring;
  const  callback: ICefAuthCallback): Boolean;
var
  h,r,s: TCefString;
begin
  h := TWACef.ToCefString(host);
  r := TWACef.ToCefString(realm);
  s := TWACef.ToCefString(scheme);
  result := PCefRequestHandler(FData)^.get_auth_credentials(
    PCefRequestHandler(FData),
    TWACef.GetData(browser),
    TWACef.GetData(frame),
    Ord(isProxy),
    @h,
    port,
    @r,
    @s,
    TWACef.GetData(callback)
  ) <> 0;
end;

function TCefRequestHandlerRef.OnQuotaRequest(const  browser: ICefBrowser;
  const  originUrl: ustring; newSize: cint64; const  callback: ICefQuotaCallback): Boolean;
var
  u: TCefString;
begin
  u := TWACef.ToCefString(originUrl);
  result := PCefRequestHandler(FData)^.on_quota_request(
    PCefRequestHandler(FData),
    TWACef.GetData(browser),
    @u,
    newSize,
    TWACef.GetData(callback)
  ) <> 0;
end;

function TCefRequestHandlerRef.OnCertificateError(CertError: TCefErrorcode; const  RequestUrl: ustring;
  const  Callback: ICefAllowCertificateErrorCallback): boolean;
var
  u: TCefString;
begin
  u := TWACef.ToCefString(RequestUrl);
  result := PCefRequestHandler(FData)^.on_certificate_error(
    PCefRequestHandler(FData),
    CertError,
    @u,
    TWACef.GetData(callback)
  ) <> 0;
end;

procedure TCefRequestHandlerRef.OnProtocolExecution(const  browser: ICefBrowser; const  url: ustring; out allowOsExecution: Boolean);
var
  u: TCefString;
  ae: pcint;
begin
  u := TWACef.ToCefString(url);
  new(ae);
  try
    PCefRequestHandler(FData)^.on_protocol_execution(
      PCefRequestHandler(FData),
      TWACef.GetData(browser),
      @u,
      ae
    );
    allowOSExecution := ae^ <> 0;
  finally
    Dispose(ae);
  end;
end;

function TCefRequestHandlerRef.OnBeforePluginLoad(const  browser: ICefBrowser; const  url, policyUrl: ustring;
  const  info: ICefWebPluginInfo): Boolean;
var
  u, p: TCefString;
begin
  u := TWACef.ToCefString(url);
  p := TWACef.ToCefString(policyUrl);
  result := PCefRequestHandler(FData)^.on_before_plugin_load(
    PCefRequestHandler(FData),
    TWACef.GetData(browser),
    @u,
    @p,
    TWACef.GetData(info)
  ) <> 0;
end;

procedure TCefRequestHandlerRef.OnPluginCrashed(const  browser: ICefBrowser; const  pluginPath: ustring);
var
  p: TCefString;
begin
  p := TWACef.ToCefString(pluginPath);
  PCefRequestHandler(FData)^.on_plugin_crashed(
    PCefRequestHandler(FData),
    TWACef.GetData(browser),
    @p
  );
end;

procedure TCefRequestHandlerRef.OnRenderProcessTerminated(const  browser: ICefBrowser; status: TCefTerminationStatus);
begin
  PCefRequestHandler(FData)^.on_render_process_terminated(
    PCefRequestHandler(FData),
    TWACef.GetData(browser),
    status
  );
end;

//Public section
class function TCefRequestHandlerRef.UnWrap(data: Pointer): ICefRequestHandler;
begin
  if data <> nil then
    Result := Create(data) as ICefRequestHandler else
    Result := nil;
end;

//..............................................................................TCefClientRef
//Protected section
function TCefClientRef.GetContextMenuHandler: ICefContextMenuHandler;
begin
  result := TCefContextMenuHandlerRef.UnWrap(PCefClient(FData)^.get_context_menu_handler(PCefClient(FData)));
end;

function TCefClientRef.GetDialogHandler: ICefDialogHandler;
begin
  result := TCefDialogHandlerRef.UnWrap(PCefClient(FData)^.get_dialog_handler(PCefClient(FData)));
end;

function TCefClientRef.GetDisplayHandler: ICefDisplayHandler;
begin
  result := TCefDisplayHandlerRef.UnWrap(PCefClient(FData)^.get_display_handler(PCefClient(FData)));
end;

function TCefClientRef.GetDownloadHandler: ICefDownloadHandler;
begin
  result := TCefDownloadHandlerRef.UnWrap(PCefClient(FData)^.get_download_handler(PCefClient(FData)));
end;

function TCefClientRef.GetDragHandler: ICefDragHandler;
begin
  result := TCefDragHandlerRef.UnWrap(PCefClient(FData)^.get_drag_handler(PCefClient(FData)));
end;

function TCefClientRef.GetFocusHandler: ICefFocusHandler;
begin
  result := TCefFocusHandlerRef.UnWrap(PCefClient(FData)^.get_focus_handler(PCefClient(FData)));
end;

function TCefClientRef.GetGeolocationHandler: ICefGeolocationHandler;
begin
  result := TCefGeolocationHandlerRef.UnWrap(PCefClient(FData)^.get_geolocation_handler(PCefClient(FData)));
end;

function TCefClientRef.GetJsdialogHandler: ICefJsDialogHandler;
begin
  result := TCefJsDialogHandlerRef.UnWrap(PCefClient(FData)^.get_jsdialog_handler(PCefClient(FData)));
end;

function TCefClientRef.GetKeyboardHandler: ICefKeyboardHandler;
begin
  result := TCefKeyboardHandlerRef.UnWrap(PCefClient(FData)^.get_keyboard_handler(PCefClient(FData)));
end;

function TCefClientRef.GetLifeSpanHandler: ICefLifespanHandler;
begin
  result := TCefLifeSpanHandlerRef.UnWrap(PCefClient(FData)^.get_life_span_handler(PCefClient(FData)));
end;

function TCefClientRef.GetLoadHandler: ICefLoadHandler;
begin
  result := TCefLoadHandlerRef.UnWrap(PCefClient(FData)^.get_load_handler(PCefClient(FData)));
end;

function TCefClientRef.GetRenderHandler: ICefRenderHandler;
begin
  result := TCefRenderHandlerRef.UnWrap(PCefClient(FData)^.get_render_handler(PCefClient(FData)));
end;

function TCefClientRef.GetRequestHandler: ICefRequestHandler;
begin
  result := TCefRequestHandlerRef.UnWrap(PCefClient(FData)^.get_request_handler(PCefClient(FData)));
end;

function TCefClientRef.OnProcessMessageReceived(const browser: ICefBrowser; sourceProcess: TCefProcessId; const message: ICefProcessMessage): boolean;
begin
  result := PCefClient(FData)^.on_process_message_received(
    PCefClient(FData),
    TWACef.GetData(browser),
    sourceProcess,
    TWACef.GetData(message)
  ) <> 0;
end;


//Public section
class function TCefClientRef.UnWrap(data: Pointer): ICefClient;
begin
  if data <> nil then
    Result := Create(data) as ICefClient else
    Result := nil;
end;

//..............................................................................TCefRequestContextHandlerRef
//Protected section
function TCefRequestContextHandlerRef.GetCookieManager: ICefCookieManager;
begin
  result:=TCefCookieManagerRef.UnWrap(PCefRequestContextHandler(FData)^.get_cookie_manager(PCefRequestContextHandler(FData)));
end;

//Public section
class function TCefRequestContextHandlerRef.UnWrap(data: Pointer): ICefRequestContextHandler;
begin
  if data <> nil then
    Result := Create(data) as ICefRequestContextHandler else
    Result := nil;
end;

//..............................................................................TCefRequestContextRef
//Protected section
function TCefRequestContextRef.IsSame(other: ICefRequestContext): Boolean;
begin
  Result:=PCefRequestContext(FData)^.is_same(PCefRequestContext(FData),TWACef.GetData(other))<>0;
end;

function TCefRequestContextRef.IsGlobal: Boolean;
begin
  Result:=PCefRequestContext(FData)^.is_global(PCefRequestContext(FData))<>0;
end;

function TCefRequestContextRef.GetHandler: ICefRequestContextHandler;
begin
  Result:=TCefRequestContextHandlerRef.UnWrap(PCefRequestContext(FData)^.get_handler(PCefRequestContext(FData)));
end;

//Public section
class function TCefRequestContextRef.UnWrap(data: Pointer): ICefRequestContext;
begin
  if data <> nil then
    Result := Create(data) as ICefRequestContext else
    Result := nil;
end;

class function TCefRequestContextRef.Global: ICefRequestContext;
begin
  Result:=UnWrap(cef_request_context_get_global_context());
end;

//..............................................................................TCefBrowserHostRef
function TCefBrowserHostRef.GetBrowser: ICefBrowser;
begin
  Result := TCefBrowserRef.UnWrap(PCefBrowserHost(FData)^.get_browser(PCefBrowserHost(FData)));
end;

procedure TCefBrowserHostRef.CloseBrowser(aForceClose: Boolean);
begin
  PCefBrowserHost(FData)^.close_browser(PCefBrowserHost(FData),ord(aForceClose));
end;

procedure TCefBrowserHostRef.SetFocus(enable: Boolean);
begin
  PCefBrowserHost(FData)^.set_focus(PCefBrowserHost(FData), Ord(enable));
end;

procedure TCefBrowserHostRef.SetWindowVisibility(visible: Boolean);
begin
  PCefBrowserHost(FData)^.set_window_visibility(PCefBrowserHost(FData), Ord(visible));
end;


function TCefBrowserHostRef.GetWindowHandle: TCefWindowHandle;
begin
  Result := PCefBrowserHost(FData)^.get_window_handle(PCefBrowserHost(FData))
end;

function TCefBrowserHostRef.GetOpenerWindowHandle: TCefWindowHandle;
begin
  Result := PCefBrowserHost(FData)^.get_opener_window_handle(PCefBrowserHost(FData));
end;

function TCefBrowserHostRef.GetClient: ICefClient;
var
  client:PCefClient;
  bs: ICefBase;
  cli: TCefClientOwn;
begin
  client:=PCefBrowserHost(FData)^.get_client(PCefBrowserHost(FData));
  if client<>nil then
    Result:=TCefClientRef.UnWrap(client)
  else
    Result:=nil;
end;

function TCefBrowserHostRef.GetRequestContext: ICefRequestContext;
begin
  Result:=TCefRequestContextRef.UnWrap(PCefBrowserHost(FData)^.get_request_context(PCefBrowserHost(FData)));
end;

function TCefBrowserHostRef.GetZoomLevel: Double;
begin
  Result := PCefBrowserHost(FData)^.get_zoom_level(PCefBrowserHost(FData));
end;

procedure TCefBrowserHostRef.SetZoomLevel(zoomLevel: Double);
begin
  PCefBrowserHost(FData)^.set_zoom_level(PCefBrowserHost(FData), zoomLevel);
end;

procedure TCefBrowserHostRef.RunFileDialog(mode: TCefFileDialogMode; const title: ustring; const DefaultFileName: ustring;
      AcceptTypes: TStrings; const callback: ICefRunFileDialogCallback);
var
  t, f: TCefString;
  list: TCefStringList;
  item: TCefString;
  i: cint;
begin
  t:=TWACef.ToCefString(title);
  f:=TWACef.ToCefString(DefaultFileName);
  list:=cef_string_list_alloc();
  try
    for i:=0 to AcceptTypes.Count-1 do
    begin
      item:=TWACef.ToCefString(AcceptTypes[i]);
      cef_string_list_append(list,@item);
    end;
    PCefBrowserHost(FData)^.run_file_dialog(PCefBrowserHost(FData),mode,@t,@f,list,TWACef.GetData(callback));
  finally
    cef_string_list_free(list);
  end;
end;

procedure TCefBrowserHostRef.RunFileDialogProc(mode: TCefFileDialogMode;
  const title, defaultFileName: ustring; acceptTypes: TStrings;
  const callback: TCefRunFileDialogCallbackProc);
begin
  RunFileDialog(mode, title, defaultFileName, acceptTypes,
    TCefFastRunFileDialogCallback.Create(callback));
end;

procedure TCefBrowserHostRef.StartDownload(const url: ustring);
var
  u: TCefString;
begin
  u := TWACef.ToCefString(url);
  PCefBrowserHost(FData)^.start_download(PCefBrowserHost(FData),@u);
end;

procedure TCefBrowserHostRef.Print;
begin
  PCefBrowserHost(FData)^.print(PCefBrowserHost(FData));
end;

procedure TCefBrowserHostRef.Find(identifier: cint; const searchText: ustring;
  fwd: Boolean; matchCase: Boolean; findNext: Boolean);
var
  text: TCefString;
begin
  text:=TWACef.ToCefString(searchText);
  PCefBrowserHost(FData)^.find(
    PCefBrowserHost(FData),
    identifier,
    @text,
    Ord(fwd),
    Ord(matchCase),
    Ord(findNext)
  );
end;

procedure TCefBrowserHostRef.StopFinding(clearSelection: Boolean);
begin
  PCefBrowserHost(FData)^.stop_finding(PCefBrowserHost(FData),Ord(clearSelection));
end;

procedure TCefBrowserHostRef.ShowDevTools(var windowInfo: TCefWindowInfo; const client: ICefClient;
  var settings: TCefBrowserSettings; InspectElementAt: TCefPoint);
begin
  PCefBrowserHost(FData)^.show_dev_tools(PCefBrowserHost(FData), @windowInfo, TWACef.GetData(client), @settings, @InspectElementAt);
end;

procedure TCefBrowserHostRef.ShowDevTools(var windowInfo: TCefWindowInfo; const client: ICefClient;
  var settings: TCefBrowserSettings);
begin
  PCefBrowserHost(FData)^.show_dev_tools(PCefBrowserHost(FData), @windowInfo, TWACef.GetData(client), @settings, nil);
end;

procedure TCefBrowserHostRef.CloseDevTools;
begin
  PCefBrowserHost(FData)^.close_dev_tools(PCefBrowserHost(FData));
end;

procedure TCefBrowserHostRef.SetMouseCursorChangeDisabled(disabled: Boolean);
begin
  PCefBrowserHost(FData)^.set_mouse_cursor_change_disabled(PCefBrowserHost(FData),Ord(disabled));
end;

function TCefBrowserHostRef.GetIsMouseCursorChangeDisabled: Boolean;
begin
  Result:=PCefBrowserHost(FData)^.is_mouse_cursor_change_disabled(PCefBrowserHost(FData))<>0;
end;

procedure TCefBrowserHostRef.ReplaceMisspelling(const word: ustring);
var
  w: TCefString;
begin
  w := TWACef.ToCefString(word);
  PCefBrowserHost(FData)^.replace_misspelling(PCefBrowserHost(FData), @w);
end;

function TCefBrowserHostRef.GetIsWindowRenderingDisabled: Boolean;
begin
  result:=PCefBrowserHost(FData)^.is_window_rendering_disabled(PCefBrowserHost(FData))<>0;
end;

procedure TCefBrowserHostRef.WasResized;
begin
  PCefBrowserHost(FData)^.was_resized(PCefBrowserHost(FData));
end;

procedure TCefBrowserHostRef.WasHidden(hidden: Boolean);
begin
  PCefBrowserHost(FData)^.was_hidden(PCefBrowserHost(FData),Ord(hidden));
end;

procedure TCefBrowserHostRef.NotifyScreenInfoChanged;
begin
  PCefBrowserHost(FData)^.notify_screen_info_changed(PCefBrowserHost(FData));
end;

procedure TCefBrowserHostRef.Invalidate(const aType: TCefPaintElementType);
begin
  PCefBrowserHost(FData)^.invalidate(PCefBrowserHost(FData), aType);
end;

procedure TCefBrowserHostRef.SendKeyEvent(const event: TCefKeyEvent);
begin
  PCefBrowserHost(FData)^.send_key_event(PCefBrowserHost(FData),@event);
end;

procedure TCefBrowserHostRef.SendMouseClickEvent(const event: TCefMouseEvent; aType: TCefMouseButtonType;
  mouseUp: Boolean; clickCount: cint);
begin
  PCefBrowserHost(FData)^.send_mouse_click_event(PCefBrowserHost(FData),@event,aType,ord(mouseUp),clickCount);
end;

procedure TCefBrowserHostRef.SendMouseMoveEvent(event: TCefMouseEvent; mouseLeave: Boolean);
begin
  PCefBrowserHost(FData)^.send_mouse_move_event(PCefBrowserHost(FData),@event,ord(mouseLeave));
end;

procedure TCefBrowserHostRef.SendMouseWheelEvent(const event: TCefMouseEvent; deltaX: cint; deltaY: cint);
begin
  PCefBrowserHost(FData)^.send_mouse_wheel_event(PCefBrowserHost(FData),@event,deltaX,deltaY);
end;

procedure TCefBrowserHostRef.SendFocusEvent(aSetFocus: cint);
begin
  PCefBrowserHost(FData)^.send_focus_event(PCefBrowserHost(FData),aSetFocus);
end;

procedure TCefBrowserHostRef.SendCaptureLostEvent;
begin
  PCefBrowserHost(FData)^.send_capture_lost_event(PCefBrowserHost(FData));
end;

function TCefBrowserHostRef.GetNSTextInputContext: TCefTextInputContext;
begin
  result := PCefBrowserHost(FData)^.get_nstext_input_context(PCefBrowserHost(FData));
end;

procedure TCefBrowserHostRef.HandleKeyEventBeforeTextInputClient(keyEvent: TCefEventHandle);
begin
  PCefBrowserHost(FData)^.handle_key_event_before_text_input_client(PCefBrowserHost(FData), keyEvent);
end;

procedure TCefBrowserHostRef.HandleKeyEventAfterTextInputClient(keyEvent: TCefEventHandle);
begin
  PCefBrowserHost(FData)^.handle_key_event_after_text_input_client(PCefBrowserHost(FData), keyEvent);
end;

procedure TCefBrowserHostRef.DragTargetDragEnter(const DragData: ICefDragData; const event: TCefMouseEvent; AllowedOps: TCefDragOperationsMask);
begin
  PCefBrowserHost(FData)^.drag_target_drag_enter(PCefBrowserHost(FData), TWACef.GetData(DragData), @event, AllowedOps);
end;

procedure TCefBrowserHostRef.DragTargetDragOver(const event: TCefMouseEvent; AllowedOps: TCefDragOperationsMask);
begin
  PCefBrowserHost(FData)^.drag_target_drag_over(PCefBrowserHost(FData), @event, AllowedOps);
end;

procedure TCefBrowserHostRef.DragTargetDragLeave;
begin
  PCefBrowserHost(FData)^.drag_target_drag_leave(PCefBrowserHost(FData));
end;

procedure TCefBrowserHostRef.DragTargetDrop(const event: TCefMouseEvent);
begin
  PCefBrowserHost(FData)^.drag_target_drop(PCefBrowserHost(FData), @event);
end;

procedure TCefBrowserHostRef.DragSourceEndedAt(x: cint; y: cint; op: TCefDragOperationsMask);
begin
  PCefBrowserHost(FData)^.drag_source_ended_at(PCefBrowserHost(FData), x, y, op);
end;

procedure TCefBrowserHostRef.DragSourceSystemDragEnded;
begin
  PCefBrowserHost(FData)^.drag_source_system_drag_ended(PCefBrowserHost(FData));
end;

class function TCefBrowserHostRef.UnWrap(data: Pointer): ICefBrowserHost;
begin
  if data <> nil then
    Result := Create(data) as ICefBrowserHost else
    Result := nil;
end;

//..............................................................................TFastCefV8Accessor
constructor TCefFastV8Accessor.Create(
  const getter: TCefV8AccessorGetterProc;
  const setter: TCefV8AccessorSetterProc);
begin
  FGetter := getter;
  FSetter := setter;
end;

function TCefFastV8Accessor.Get(const name: ustring; const obj: ICefv8Value;
  out value: ICefv8Value; const exception: ustring): Boolean;
begin
  if Assigned(FGetter)  then
    Result := FGetter(name, obj, value, exception) else
    Result := False;
end;

function TCefFastV8Accessor.Put(const name: ustring; const obj,
  value: ICefv8Value; const exception: ustring): Boolean;
begin
  if Assigned(FSetter)  then
    Result := FSetter(name, obj, value, exception) else
    Result := False;
end;

//..............................................................................TCefV8ExceptionRef
function TCefV8ExceptionRef.GetEndColumn: cint;
begin
  Result := PCefV8Exception(FData)^.get_end_column(FData);
end;

function TCefV8ExceptionRef.GetEndPosition: cint;
begin
  Result := PCefV8Exception(FData)^.get_end_position(FData);
end;

function TCefV8ExceptionRef.GetLineNumber: cint;
begin
  Result := PCefV8Exception(FData)^.get_line_number(FData);
end;

function TCefV8ExceptionRef.GetMessage: ustring;
begin
  Result := TWACef.StringFreeAndGet(PCefV8Exception(FData)^.get_message(FData));
end;

function TCefV8ExceptionRef.GetScriptResourceName: ustring;
begin
  Result := TWACef.StringFreeAndGet(PCefV8Exception(FData)^.get_script_resource_name(FData));
end;

function TCefV8ExceptionRef.GetSourceLine: ustring;
begin
  Result := TWACef.StringFreeAndGet(PCefV8Exception(FData)^.get_source_line(FData));
end;

function TCefV8ExceptionRef.GetStartColumn: cint;
begin
  Result := PCefV8Exception(FData)^.get_start_column(FData);
end;

function TCefV8ExceptionRef.GetStartPosition: cint;
begin
  Result := PCefV8Exception(FData)^.get_start_position(FData);
end;

class function TCefV8ExceptionRef.UnWrap(data: Pointer): ICefV8Exception;
begin
  if data <> nil then
    Result := Create(data) as ICefV8Exception else
    Result := nil;
end;

//..............................................................................TCefV8ValueRef
function TCefv8ValueRef.AdjustExternallyAllocatedMemory(
  changeInBytes: cint): cint;
begin
  Result := PCefV8Value(FData)^.adjust_externally_allocated_memory(PCefV8Value(FData), changeInBytes);
end;

class function TCefv8ValueRef.NewArray(len: cint): ICefv8Value;
begin
  Result := UnWrap(cef_v8value_create_array(len));
end;

class function TCefv8ValueRef.NewBool(value: Boolean): ICefv8Value;
begin
  Result := UnWrap(cef_v8value_create_bool(Ord(value)));
end;

class function TCefv8ValueRef.NewDate(value: TDateTime): ICefv8Value;
var
  dt: TCefTime;
begin
  dt := TWACef.DateTimeToCefTime(value);
  Result := UnWrap(cef_v8value_create_date(@dt));
end;

class function TCefv8ValueRef.NewDouble(value: Double): ICefv8Value;
begin
  Result := UnWrap(cef_v8value_create_double(value));
end;

class function TCefv8ValueRef.NewFunction(const name: ustring;
  const handler: ICefv8Handler): ICefv8Value;
var
  n: TCefString;
begin
  n := TWACef.ToCefString(name);
  Result := UnWrap(cef_v8value_create_function(@n, TWACef.GetData(handler)));
end;

class function TCefv8ValueRef.NewInt(value: cint): ICefv8Value;
begin
  Result := UnWrap(cef_v8value_create_int(value));
end;

class function TCefv8ValueRef.NewUInt(value: csize_t): ICefv8Value;
begin
  Result := UnWrap(cef_v8value_create_uint(value));
end;

class function TCefv8ValueRef.NewNull: ICefv8Value;
begin
  Result := UnWrap(cef_v8value_create_null());
end;

class function TCefv8ValueRef.NewObject(const Accessor: ICefV8Accessor): ICefv8Value;
begin
  Result := UnWrap(cef_v8value_create_object(TWACef.GetData(Accessor)));
end;

class function TCefv8ValueRef.NewObjectProc(const getter: TCefV8AccessorGetterProc;
  const setter: TCefV8AccessorSetterProc): ICefv8Value;
begin
  Result := NewObject(TCefFastV8Accessor.Create(getter, setter) as ICefV8Accessor);
end;

class function TCefv8ValueRef.NewString(const str: ustring): ICefv8Value;
var
  s: TCefString;
begin
  s := TWACef.ToCefString(str);
  Result := UnWrap(cef_v8value_create_string(@s));
end;

class function TCefv8ValueRef.NewUndefined: ICefv8Value;
begin
  Result := UnWrap(cef_v8value_create_undefined());
end;

function TCefv8ValueRef.DeleteValueByIndex(index: cint): Boolean;
begin
  Result := PCefV8Value(FData)^.delete_value_byindex(PCefV8Value(FData), index) <> 0;
end;

function TCefv8ValueRef.DeleteValueByKey(const key: ustring): Boolean;
var
  k: TCefString;
begin
  k := TWACef.ToCefString(key);
  Result := PCefV8Value(FData)^.delete_value_bykey(PCefV8Value(FData), @k) <> 0;
end;

function TCefv8ValueRef.ExecuteFunction(const obj: ICefv8Value;
  const arguments: TCefv8ValueArray): ICefv8Value;
var
  args: PPCefV8Value;
  i: cint;
begin
  GetMem(args, SizeOf(PCefV8Value) * Length(arguments));
  try
    for i := 0 to Length(arguments) - 1 do
      args[i] := TWACef.GetData(arguments[i]);
    Result := TCefv8ValueRef.UnWrap(PCefV8Value(FData)^.execute_function(PCefV8Value(FData),
      TWACef.GetData(obj), Length(arguments), args));
  finally
    FreeMem(args);
  end;
end;

function TCefv8ValueRef.ExecuteFunctionWithContext(const context: ICefv8Context;
  const obj: ICefv8Value; const arguments: TCefv8ValueArray): ICefv8Value;
var
  args: PPCefV8Value;
  i: cint;
begin
  GetMem(args, SizeOf(PCefV8Value) * Length(arguments));
  try
    for i := 0 to Length(arguments) - 1 do
      args[i] := TWACef.GetData(arguments[i]);
    Result := TCefv8ValueRef.UnWrap(PCefV8Value(FData)^.execute_function_with_context(PCefV8Value(FData),
      TWACef.GetData(context), TWACef.GetData(obj), Length(arguments), args));
  finally
    FreeMem(args);
  end;
end;

function TCefv8ValueRef.GetArrayLength: cint;
begin
  Result := PCefV8Value(FData)^.get_array_length(PCefV8Value(FData));
end;

function TCefv8ValueRef.GetBoolValue: Boolean;
begin
  Result := PCefV8Value(FData)^.get_bool_value(PCefV8Value(FData)) <> 0;
end;

function TCefv8ValueRef.GetDateValue: TDateTime;
begin
  Result := TWACef.CefTimeToDateTime(PCefV8Value(FData)^.get_date_value(PCefV8Value(FData)));
end;

function TCefv8ValueRef.GetDoubleValue: Double;
begin
  Result := PCefV8Value(FData)^.get_double_value(PCefV8Value(FData));
end;

function TCefv8ValueRef.GetExternallyAllocatedMemory: cint;
begin
  Result := PCefV8Value(FData)^.get_externally_allocated_memory(PCefV8Value(FData));
end;

function TCefv8ValueRef.GetFunctionHandler: ICefv8Handler;
begin
  Result := TCefv8HandlerRef.UnWrap(PCefV8Value(FData)^.get_function_handler(PCefV8Value(FData)));
end;

function TCefv8ValueRef.GetFunctionName: ustring;
begin
  Result := TWACef.StringFreeAndGet(PCefV8Value(FData)^.get_function_name(PCefV8Value(FData)))
end;

function TCefv8ValueRef.GetIntValue: cint;
begin
  Result := PCefV8Value(FData)^.get_int_value(PCefV8Value(FData))
end;

function TCefv8ValueRef.GetUIntValue: csize_t;
begin
  Result := PCefV8Value(FData)^.get_uint_value(PCefV8Value(FData))
end;

function TCefv8ValueRef.GetKeys(const keys: TStrings): cint;
var
  list: TCefStringList;
  i: cint;
  str: TCefString;
begin
  list := cef_string_list_alloc();
  try
    Result := PCefV8Value(FData)^.get_keys(PCefV8Value(FData), list);
    FillChar(str, SizeOf(str), 0);
    for i := 0 to cef_string_list_size(list) - 1 do
    begin
      cef_string_list_value(list, i, @str);
      keys.Add(TWACef.StringClearAndGet(str));
    end;
  finally
    cef_string_list_free(list);
  end;
end;

function TCefv8ValueRef.SetUserData(const data: ICefv8Value): Boolean;
begin
  Result := PCefV8Value(FData)^.set_user_data(PCefV8Value(FData), TWACef.GetData(data)) <> 0;
end;

function TCefv8ValueRef.GetStringValue: ustring;
begin
  Result := TWACef.StringFreeAndGet(PCefV8Value(FData)^.get_string_value(PCefV8Value(FData)));
end;

function TCefv8ValueRef.IsUserCreated: Boolean;
begin
  Result := PCefV8Value(FData)^.is_user_created(PCefV8Value(FData)) <> 0;
end;

function TCefv8ValueRef.HasException: Boolean;
begin
  Result := PCefV8Value(FData)^.has_exception(PCefV8Value(FData)) <> 0;
end;

function TCefv8ValueRef.GetException: ICefV8Exception;
begin
   Result := TCefV8ExceptionRef.UnWrap(PCefV8Value(FData)^.get_exception(PCefV8Value(FData)));
end;

function TCefv8ValueRef.ClearException: Boolean;
begin
  Result := PCefV8Value(FData)^.clear_exception(PCefV8Value(FData)) <> 0;
end;

function TCefv8ValueRef.WillRethrowExceptions: Boolean;
begin
  Result := PCefV8Value(FData)^.will_rethrow_exceptions(PCefV8Value(FData)) <> 0;
end;

function TCefv8ValueRef.SetRethrowExceptions(rethrow: Boolean): Boolean;
begin
  Result := PCefV8Value(FData)^.set_rethrow_exceptions(PCefV8Value(FData), Ord(rethrow)) <> 0;
end;

function TCefv8ValueRef.GetUserData: ICefv8Value;
begin
  Result := TCefv8ValueRef.UnWrap(PCefV8Value(FData)^.get_user_data(PCefV8Value(FData)));
end;

function TCefv8ValueRef.GetValueByIndex(index: cint): ICefv8Value;
begin
  Result := TCefv8ValueRef.UnWrap(PCefV8Value(FData)^.get_value_byindex(PCefV8Value(FData), index))
end;

function TCefv8ValueRef.GetValueByKey(const key: ustring): ICefv8Value;
var
  k: TCefString;
begin
  k := TWACef.ToCefString(key);
  Result := TCefv8ValueRef.UnWrap(PCefV8Value(FData)^.get_value_bykey(PCefV8Value(FData), @k))
end;

function TCefv8ValueRef.HasValueByIndex(index: cint): Boolean;
begin
  Result := PCefV8Value(FData)^.has_value_byindex(PCefV8Value(FData), index) <> 0;
end;

function TCefv8ValueRef.HasValueByKey(const key: ustring): Boolean;
var
  k: TCefString;
begin
  k := TWACef.ToCefString(key);
  Result := PCefV8Value(FData)^.has_value_bykey(PCefV8Value(FData), @k) <> 0;
end;

function TCefv8ValueRef.IsArray: Boolean;
begin
  Result := PCefV8Value(FData)^.is_array(PCefV8Value(FData)) <> 0;
end;

function TCefv8ValueRef.IsBool: Boolean;
begin
  Result := PCefV8Value(FData)^.is_bool(PCefV8Value(FData)) <> 0;
end;

function TCefv8ValueRef.IsDate: Boolean;
begin
  Result := PCefV8Value(FData)^.is_date(PCefV8Value(FData)) <> 0;
end;

function TCefv8ValueRef.IsDouble: Boolean;
begin
  Result := PCefV8Value(FData)^.is_double(PCefV8Value(FData)) <> 0;
end;

function TCefv8ValueRef.IsFunction: Boolean;
begin
  Result := PCefV8Value(FData)^.is_function(PCefV8Value(FData)) <> 0;
end;

function TCefv8ValueRef.IsInt: Boolean;
begin
  Result := PCefV8Value(FData)^.is_int(PCefV8Value(FData)) <> 0;
end;

function TCefv8ValueRef.IsUInt: Boolean;
begin
  Result := PCefV8Value(FData)^.is_uint(PCefV8Value(FData)) <> 0;
end;

function TCefv8ValueRef.IsNull: Boolean;
begin
  Result := PCefV8Value(FData)^.is_null(PCefV8Value(FData)) <> 0;
end;

function TCefv8ValueRef.IsObject: Boolean;
begin
  Result := PCefV8Value(FData)^.is_object(PCefV8Value(FData)) <> 0;
end;

function TCefv8ValueRef.IsSame(const that: ICefv8Value): Boolean;
begin
  Result := PCefV8Value(FData)^.is_same(PCefV8Value(FData), TWACef.GetData(that)) <> 0;
end;

function TCefv8ValueRef.IsString: Boolean;
begin
  Result := PCefV8Value(FData)^.is_string(PCefV8Value(FData)) <> 0;
end;

function TCefv8ValueRef.IsValid: Boolean;
begin
  Result := PCefV8Value(FData)^.is_valid(PCefV8Value(FData)) <> 0;
end;

function TCefv8ValueRef.IsUndefined: Boolean;
begin
  Result := PCefV8Value(FData)^.is_undefined(PCefV8Value(FData)) <> 0;
end;

function TCefv8ValueRef.SetValueByAccessor(const key: ustring;
  settings: TCefV8AccessControl; attribute: TCefV8PropertyAttribute): Boolean;
var
  k: TCefString;
begin
  k := TWACef.ToCefString(key);
  Result:= PCefV8Value(FData)^.set_value_byaccessor(PCefV8Value(FData), @k,
    settings, attribute) <> 0;
end;

function TCefv8ValueRef.SetValueByIndex(index: cint;
  const value: ICefv8Value): Boolean;
begin
  Result:= PCefV8Value(FData)^.set_value_byindex(PCefV8Value(FData), index, TWACef.GetData(value)) <> 0;
end;

function TCefv8ValueRef.SetValueByKey(const key: ustring;
  const value: ICefv8Value; attribute: TCefV8PropertyAttribute): Boolean;
var
  k: TCefString;
begin
  k := TWACef.ToCefString(key);
  Result:= PCefV8Value(FData)^.set_value_bykey(PCefV8Value(FData), @k,
    TWACef.GetData(value), attribute) <> 0;
end;

class function TCefv8ValueRef.UnWrap(data: Pointer): ICefv8Value;
begin
  if data <> nil then
    Result := Create(data) as ICefv8Value else
    Result := nil;
end;

//..............................................................................TCefV8ContextRef
class function TCefv8ContextRef.Current: ICefv8Context;
begin
  Result := UnWrap(cef_v8context_get_current_context())
end;

function TCefv8ContextRef.Enter: Boolean;
begin
  Result := PCefv8Context(FData)^.enter(PCefv8Context(FData)) <> 0;
end;

class function TCefv8ContextRef.Entered: ICefv8Context;
begin
  Result := UnWrap(cef_v8context_get_entered_context())
end;

function TCefv8ContextRef.Exit: Boolean;
begin
  Result := PCefv8Context(FData)^.exit(PCefv8Context(FData)) <> 0;
end;

function TCefv8ContextRef.GetTaskRunner: ICefTaskRunner;
begin
  Result := TCefTaskRunnerRef.UnWrap(PCefv8Context(FData)^.get_task_runner(PCefv8Context(FData)));
end;

function TCefv8ContextRef.IsValid: Boolean;
begin
  Result := PCefv8Context(FData)^.is_valid(PCefv8Context(FData))<>0;
end;

function TCefv8ContextRef.GetBrowser: ICefBrowser;
begin
  Result := TCefBrowserRef.UnWrap(PCefv8Context(FData)^.get_browser(PCefv8Context(FData)));
end;

function TCefv8ContextRef.GetFrame: ICefFrame;
begin
  Result := TCefFrameRef.UnWrap(PCefv8Context(FData)^.get_frame(PCefv8Context(FData)))
end;

function TCefv8ContextRef.GetGlobal: ICefv8Value;
begin
  Result := TCefv8ValueRef.UnWrap(PCefv8Context(FData)^.get_global(PCefv8Context(FData)));
end;

function TCefv8ContextRef.IsSame(const that: ICefv8Context): Boolean;
begin
  Result := PCefv8Context(FData)^.is_same(PCefv8Context(FData), TWACef.GetData(that)) <> 0;
end;

function TCefv8ContextRef.Eval(const code: ustring; var retval: ICefv8Value;
 var exception: ICefV8Exception): Boolean;
var
  c: TCefString;
  r: PCefv8Value;
  e: PCefV8Exception;
begin
  c := TWACef.ToCefString(code);
  r := nil; e := nil;
  Result := PCefv8Context(FData)^.eval(PCefv8Context(FData), @c, r, e) <> 0;
  retval := TCefv8ValueRef.UnWrap(r);
  exception := TCefV8ExceptionRef.UnWrap(e);
end;

class function TCefv8ContextRef.UnWrap(data: Pointer): ICefv8Context;
begin
  if data <> nil then
    Result := Create(data) as ICefv8Context else
    Result := nil;
end;

//..............................................................................TCefV8StackFrameRef
function TCefV8StackFrameRef.GetColumn: cint;
begin
  Result := PCefV8StackFrame(FData)^.get_column(FData);
end;

function TCefV8StackFrameRef.GetFunctionName: ustring;
begin
  Result := TWACef.StringFreeAndGet(PCefV8StackFrame(FData)^.get_function_name(FData));
end;

function TCefV8StackFrameRef.GetLineNumber: cint;
begin
  Result := PCefV8StackFrame(FData)^.get_line_number(FData);
end;

function TCefV8StackFrameRef.IsValid: Boolean;
begin
  Result := PCefV8StackFrame(FData)^.is_valid(FData) <> 0;
end;

function TCefV8StackFrameRef.GetScriptName: ustring;
begin
  Result := TWACef.StringFreeAndGet(PCefV8StackFrame(FData)^.get_script_name(FData));
end;

function TCefV8StackFrameRef.GetScriptNameOrSourceUrl: ustring;
begin
  Result := TWACef.StringFreeAndGet(PCefV8StackFrame(FData)^.get_script_name_or_source_url(FData));
end;

function TCefV8StackFrameRef.IsConstructor: Boolean;
begin
  Result := PCefV8StackFrame(FData)^.is_constructor(FData) <> 0;
end;

function TCefV8StackFrameRef.IsEval: Boolean;
begin
  Result := PCefV8StackFrame(FData)^.is_eval(FData) <> 0;
end;

class function TCefV8StackFrameRef.UnWrap(data: Pointer): ICefV8StackFrame;
begin
  if data <> nil then
    Result := Create(data) as ICefV8StackFrame else
    Result := nil;
end;

//..............................................................................TCefV8StackTraceRef
class function TCefV8StackTraceRef.Current(frameLimit: cint): ICefV8StackTrace;
begin
  Result := UnWrap(cef_v8stack_trace_get_current(frameLimit));
end;

function TCefV8StackTraceRef.GetFrame(index: cint): ICefV8StackFrame;
begin
  Result := TCefV8StackFrameRef.UnWrap(PCefV8StackTrace(FData)^.get_frame(FData, index));
end;

function TCefV8StackTraceRef.IsValid: Boolean;
begin
  Result := PCefV8StackTrace(FData)^.is_valid(FData) <> 0;
end;

function TCefV8StackTraceRef.GetFrameCount: cint;
begin
  Result := PCefV8StackTrace(FData)^.get_frame_count(FData);
end;

class function TCefV8StackTraceRef.UnWrap(data: Pointer): ICefV8StackTrace;
begin
  if data <> nil then
    Result := Create(data) as ICefV8StackTrace else
    Result := nil;
end;

//..............................................................................TCefV8HandlerRef
function TCefv8HandlerRef.Execute(const name: ustring; const obj: ICefv8Value;
  const arguments: TCefv8ValueArray; var retval: ICefv8Value;
  var exception: ustring): Boolean;
var
  args: array of PCefV8Value;
  i: cint;
  ret: PCefV8Value;
  exc: TCefString;
  n: TCefString;
begin
  SetLength(args, Length(arguments));
  for i := 0 to Length(arguments) - 1 do
    args[i] := TWACef.GetData(arguments[i]);
  ret := nil;
  FillChar(exc, SizeOf(exc), 0);
  n := TWACef.ToCefString(name);
  Result := PCefv8Handler(FData)^.execute(PCefv8Handler(FData), @n,
    TWACef.GetData(obj), Length(arguments), @args, ret, exc) <> 0;
  retval := TCefv8ValueRef.UnWrap(ret);
  exception := TWACef.StringClearAndGet(exc);
end;

class function TCefv8HandlerRef.UnWrap(data: Pointer): ICefv8Handler;
begin
  if data <> nil then
    Result := Create(data) as ICefv8Handler else
    Result := nil;
end;

//..............................................................................TCefFrameRef
function TCefFrameRef.IsValid: Boolean;
begin
  Result := PCefFrame(FData)^.is_valid(PCefFrame(FData)) <> 0;
end;

procedure TCefFrameRef.Copy;
begin
  PCefFrame(FData)^.copy(PCefFrame(FData));
end;

procedure TCefFrameRef.Cut;
begin
  PCefFrame(FData)^.cut(PCefFrame(FData));
end;

procedure TCefFrameRef.Del;
begin
  PCefFrame(FData)^.del(PCefFrame(FData));
end;

procedure TCefFrameRef.ExecuteJavaScript(const code, scriptUrl: ustring;
  startLine: cint);
var
  j, s: TCefString;
begin
  j := TWACef.ToCefString(code);
  s := TWACef.ToCefString(scriptUrl);
  PCefFrame(FData)^.execute_java_script(PCefFrame(FData), @j, @s, startline);
end;

function TCefFrameRef.GetBrowser: ICefBrowser;
begin
  Result := TCefBrowserRef.UnWrap(PCefFrame(FData)^.get_browser(PCefFrame(FData)));
end;

function TCefFrameRef.GetIdentifier: cint64;
begin
  Result := PCefFrame(FData)^.get_identifier(PCefFrame(FData));
end;

function TCefFrameRef.GetName: ustring;
begin
  Result := TWACef.StringFreeAndGet(PCefFrame(FData)^.get_name(PCefFrame(FData)));
end;

function TCefFrameRef.GetParent: ICefFrame;
begin
  Result := TCefFrameRef.UnWrap(PCefFrame(FData)^.get_parent(PCefFrame(FData)));
end;

procedure TCefFrameRef.GetSource(const visitor: ICefStringVisitor);
begin
  PCefFrame(FData)^.get_source(PCefFrame(FData), TWACef.GetData(visitor));
end;

procedure TCefFrameRef.GetSourceProc(const proc: TCefStringVisitorProc);
begin
  GetSource(TCefFastStringVisitor.Create(proc));
end;

procedure TCefFrameRef.getText(const visitor: ICefStringVisitor);
begin
  PCefFrame(FData)^.get_text(PCefFrame(FData), TWACef.GetData(visitor));
end;

procedure TCefFrameRef.GetTextProc(const proc: TCefStringVisitorProc);
begin
  GetText(TCefFastStringVisitor.Create(proc));
end;

function TCefFrameRef.GetUrl: ustring;
begin
  Result := TWACef.StringFreeAndGet(PCefFrame(FData)^.get_url(PCefFrame(FData)));
end;

function TCefFrameRef.GetV8Context: ICefv8Context;
begin
  Result := TCefv8ContextRef.UnWrap(PCefFrame(FData)^.get_v8context(PCefFrame(FData)));
end;

function TCefFrameRef.IsFocused: Boolean;
begin
  Result := PCefFrame(FData)^.is_focused(PCefFrame(FData)) <> 0;
end;

function TCefFrameRef.IsMain: Boolean;
begin
  Result := PCefFrame(FData)^.is_main(PCefFrame(FData)) <> 0;
end;

procedure TCefFrameRef.LoadRequest(const request: ICefRequest);
begin
  PCefFrame(FData)^.load_request(PCefFrame(FData), TWACef.GetData(request));
end;

procedure TCefFrameRef.LoadString(const str, url: ustring);
var
  s, u: TCefString;
begin
  s := TWACef.ToCefString(str);
  u := TWACef.ToCefString(url);
  PCefFrame(FData)^.load_string(PCefFrame(FData), @s, @u);
end;

procedure TCefFrameRef.LoadUrl(const url: ustring);
var
  u: TCefString;
begin
  u := TWACef.ToCefString(url);
  PCefFrame(FData)^.load_url(PCefFrame(FData), @u);
end;

procedure TCefFrameRef.Paste;
begin
  PCefFrame(FData)^.paste(PCefFrame(FData));
end;

procedure TCefFrameRef.Redo;
begin
  PCefFrame(FData)^.redo(PCefFrame(FData));
end;

procedure TCefFrameRef.SelectAll;
begin
  PCefFrame(FData)^.select_all(PCefFrame(FData));
end;

procedure TCefFrameRef.Undo;
begin
  PCefFrame(FData)^.undo(PCefFrame(FData));
end;

procedure TCefFrameRef.ViewSource;
begin
  PCefFrame(FData)^.view_source(PCefFrame(FData));
end;

procedure TCefFrameRef.VisitDom(const visitor: ICefDomVisitor);
begin
  PCefFrame(FData)^.visit_dom(PCefFrame(FData), TWACef.GetData(visitor));
end;

procedure TCefFrameRef.VisitDomProc(const proc: TCefDomVisitorProc);
begin
  VisitDom(TCefFastDomVisitor.Create(proc) as ICefDomVisitor);
end;

class function TCefFrameRef.UnWrap(data: Pointer): ICefFrame;
begin
  if data <> nil then
    Result := Create(data) as ICefFrame else
    Result := nil;
end;

//..............................................................................TCefBrowserRef
function TCefBrowserRef.GetHost: ICefBrowserHost;
begin
  Result := TCefBrowserHostRef.UnWrap(PCefBrowser(FData)^.get_host(PCefBrowser(FData)));
end;

function TCefBrowserRef.CanGoBack: Boolean;
begin
  Result := PCefBrowser(FData)^.can_go_back(PCefBrowser(FData)) <> 0;
end;

function TCefBrowserRef.CanGoForward: Boolean;
begin
  Result := PCefBrowser(FData)^.can_go_forward(PCefBrowser(FData)) <> 0;
end;

function TCefBrowserRef.GetFocusedFrame: ICefFrame;
begin
  Result := TCefFrameRef.UnWrap(PCefBrowser(FData)^.get_focused_frame(PCefBrowser(FData)))
end;

function TCefBrowserRef.GetFrameByident(identifier: cint64): ICefFrame;
begin
  Result := TCefFrameRef.UnWrap(PCefBrowser(FData)^.get_frame_byident(PCefBrowser(FData), identifier));
end;

function TCefBrowserRef.GetFrame(const name: ustring): ICefFrame;
var
  n: TCefString;
begin
  n := TWACef.ToCefString(name);
  Result := TCefFrameRef.UnWrap(PCefBrowser(FData)^.get_frame(PCefBrowser(FData), @n));
end;

function TCefBrowserRef.GetFrameCount: csize_t;
begin
  Result := PCefBrowser(FData)^.get_frame_count(PCefBrowser(FData));
end;

procedure TCefBrowserRef.GetFrameIdentifiers(count: pcsize_t; identifiers: pcint64);
begin
  PCefBrowser(FData)^.get_frame_identifiers(PCefBrowser(FData), count, identifiers);
end;

procedure TCefBrowserRef.GetFrameNames(names: TStrings);
var
  list: TCefStringList;
  i: cint;
  str: TCefString;
begin
  list := cef_string_list_alloc();
  try
    PCefBrowser(FData)^.get_frame_names(PCefBrowser(FData), list);
    FillChar(str, SizeOf(str), 0);
    for i := 0 to cef_string_list_size(list) - 1 do
    begin
      cef_string_list_value(list, i, @str);
      names.Add(TWACef.StringClearAndGet(str));
    end;
  finally
    cef_string_list_free(list);
  end;
end;

function TCefBrowserRef.SendProcessMessage(targetProcess: TCefProcessId;
  message: ICefProcessMessage): Boolean;
begin
  Result := PCefBrowser(FData)^.send_process_message(PCefBrowser(FData), targetProcess, TWACef.GetData(message)) <> 0;
end;

function TCefBrowserRef.GetMainFrame: ICefFrame;
begin
  Result := TCefFrameRef.UnWrap(PCefBrowser(FData)^.get_main_frame(PCefBrowser(FData)))
end;

procedure TCefBrowserRef.GoBack;
begin
  PCefBrowser(FData)^.go_back(PCefBrowser(FData));
end;

procedure TCefBrowserRef.GoForward;
begin
  PCefBrowser(FData)^.go_forward(PCefBrowser(FData));
end;

function TCefBrowserRef.IsLoading: Boolean;
begin
  Result := PCefBrowser(FData)^.is_loading(PCefBrowser(FData)) <> 0;
end;

function TCefBrowserRef.HasDocument: Boolean;
begin
  Result := PCefBrowser(FData)^.has_document(PCefBrowser(FData)) <> 0;
end;

function TCefBrowserRef.IsSame(const that: ICefBrowser): Boolean;
begin
  Result := PCefBrowser(FData)^.is_same(PCefBrowser(FData), TWACef.GetData(that)) <> 0;
end;

function TCefBrowserRef.IsPopup: Boolean;
begin
  Result := PCefBrowser(FData)^.is_popup(PCefBrowser(FData)) <> 0;
end;

procedure TCefBrowserRef.Reload;
begin
  PCefBrowser(FData)^.reload(PCefBrowser(FData));
end;

procedure TCefBrowserRef.ReloadIgnoreCache;
begin
  PCefBrowser(FData)^.reload_ignore_cache(PCefBrowser(FData));
end;

procedure TCefBrowserRef.StopLoad;
begin
  PCefBrowser(FData)^.stop_load(PCefBrowser(FData));
end;

function TCefBrowserRef.GetIdentifier: cint;
begin
  Result := PCefBrowser(FData)^.get_identifier(PCefBrowser(FData));
end;

class function TCefBrowserRef.UnWrap(data: Pointer): ICefBrowser;
begin
  if data <> nil then
    Result := Create(data) as ICefBrowser else
    Result := nil;
end;

//..............................................................................TCefContextMenuParamsRef
function TCefContextMenuParamsRef.GetEditStateFlags: TCefContextMenuEditStateFlags;
begin
  Result:= PCefContextMenuParams(FData)^.get_edit_state_flags(PCefContextMenuParams(FData));
end;

function TCefContextMenuParamsRef.GetFrameCharset: ustring;
begin
  Result := TWACef.StringFreeAndGet(PCefContextMenuParams(FData)^.get_frame_charset(PCefContextMenuParams(FData)));
end;

function TCefContextMenuParamsRef.GetFrameUrl: ustring;
begin
  Result := TWACef.StringFreeAndGet(PCefContextMenuParams(FData)^.get_frame_url(PCefContextMenuParams(FData)));
end;

function TCefContextMenuParamsRef.GetLinkUrl: ustring;
begin
  Result := TWACef.StringFreeAndGet(PCefContextMenuParams(FData)^.get_link_url(PCefContextMenuParams(FData)));
end;

function TCefContextMenuParamsRef.GetMediaStateFlags: TCefContextMenuMediaStateFlags;
begin
  Result:= PCefContextMenuParams(FData)^.get_media_state_flags(PCefContextMenuParams(FData));
end;

function TCefContextMenuParamsRef.GetMediaType: TCefContextMenuMediaType;
begin
  Result := PCefContextMenuParams(FData)^.get_media_type(PCefContextMenuParams(FData));
end;

function TCefContextMenuParamsRef.GetPageUrl: ustring;
begin
  Result := TWACef.StringFreeAndGet(PCefContextMenuParams(FData)^.get_page_url(PCefContextMenuParams(FData)));
end;

function TCefContextMenuParamsRef.GetSelectionText: ustring;
begin
  Result := TWACef.StringFreeAndGet(PCefContextMenuParams(FData)^.get_selection_text(PCefContextMenuParams(FData)));
end;

function TCefContextMenuParamsRef.GetSourceUrl: ustring;
begin
  Result := TWACef.StringFreeAndGet(PCefContextMenuParams(FData)^.get_source_url(PCefContextMenuParams(FData)));
end;

function TCefContextMenuParamsRef.GetTypeFlags: TCefContextMenuTypeFlags;
begin
  Result:=PCefContextMenuParams(FData)^.get_type_flags(PCefContextMenuParams(FData));
end;

function TCefContextMenuParamsRef.GetUnfilteredLinkUrl: ustring;
begin
  Result := TWACef.StringFreeAndGet(PCefContextMenuParams(FData)^.get_unfiltered_link_url(PCefContextMenuParams(FData)));
end;

function TCefContextMenuParamsRef.GetXCoord: cint;
begin
  Result := PCefContextMenuParams(FData)^.get_xcoord(PCefContextMenuParams(FData));
end;

function TCefContextMenuParamsRef.GetYCoord: cint;
begin
  Result := PCefContextMenuParams(FData)^.get_ycoord(PCefContextMenuParams(FData));
end;

function TCefContextMenuParamsRef.IsEditable: Boolean;
begin
  Result := PCefContextMenuParams(FData)^.is_editable(PCefContextMenuParams(FData)) <> 0;
end;

function TCefContextMenuParamsRef.HasImageContents: Boolean;
begin
  Result := PCefContextMenuParams(FData)^.has_image_contents(PCefContextMenuParams(FData)) <> 0;
end;

class function TCefContextMenuParamsRef.UnWrap(
  data: Pointer): ICefContextMenuParams;
begin
  if data <> nil then
    Result := Create(data) as ICefContextMenuParams else
    Result := nil;
end;

//..............................................................................TCefMenuModelRef
function TCefMenuModelRef.AddCheckItem(commandId: cint;
  const text: ustring): Boolean;
var
  t: TCefString;
begin
  t := TWACef.ToCefString(text);
  Result := PCefMenuModel(FData)^.add_check_item(PCefMenuModel(FData), commandId, @t) <> 0;
end;

function TCefMenuModelRef.AddItem(commandId: cint;
  const text: ustring): Boolean;
var
  t: TCefString;
begin
  t := TWACef.ToCefString(text);
  Result := PCefMenuModel(FData)^.add_item(PCefMenuModel(FData), commandId, @t) <> 0;
end;

function TCefMenuModelRef.AddRadioItem(commandId: cint; const text: ustring;
  groupId: cint): Boolean;
var
  t: TCefString;
begin
  t := TWACef.ToCefString(text);
  Result := PCefMenuModel(FData)^.add_radio_item(PCefMenuModel(FData), commandId, @t, groupId) <> 0;
end;

function TCefMenuModelRef.AddSeparator: Boolean;
begin
  Result := PCefMenuModel(FData)^.add_separator(PCefMenuModel(FData)) <> 0;
end;

function TCefMenuModelRef.AddSubMenu(commandId: cint;
  const text: ustring): ICefMenuModel;
var
  t: TCefString;
begin
  t := TWACef.ToCefString(text);
  Result := TCefMenuModelRef.UnWrap(PCefMenuModel(FData)^.add_sub_menu(PCefMenuModel(FData), commandId, @t));
end;

function TCefMenuModelRef.Clear: Boolean;
begin
  Result := PCefMenuModel(FData)^.clear(PCefMenuModel(FData)) <> 0;
end;

function TCefMenuModelRef.GetAccelerator(commandId: cint;
  out keyCode: cint; out shiftPressed, ctrlPressed,
  altPressed: Boolean): Boolean;
var
  sp, cp, ap: cint;
begin
  Result := PCefMenuModel(FData)^.get_accelerator(PCefMenuModel(FData),
    commandId, @keyCode, @sp, @cp, @ap) <> 0;
  shiftPressed := sp <> 0;
  ctrlPressed := cp <> 0;
  altPressed := ap <> 0;
end;

function TCefMenuModelRef.GetAcceleratorAt(index: cint; out keyCode: cint;
  out shiftPressed, ctrlPressed, altPressed: Boolean): Boolean;
var
  sp, cp, ap: cint;
begin
  Result := PCefMenuModel(FData)^.get_accelerator_at(PCefMenuModel(FData),
    index, @keyCode, @sp, @cp, @ap) <> 0;
  shiftPressed := sp <> 0;
  ctrlPressed := cp <> 0;
  altPressed := ap <> 0;
end;

function TCefMenuModelRef.GetCommandIdAt(index: cint): cint;
begin
  Result := PCefMenuModel(FData)^.get_command_id_at(PCefMenuModel(FData), index);
end;

function TCefMenuModelRef.GetCount: cint;
begin
  Result := PCefMenuModel(FData)^.get_count(PCefMenuModel(FData));
end;

function TCefMenuModelRef.GetGroupId(commandId: cint): cint;
begin
  Result := PCefMenuModel(FData)^.get_group_id(PCefMenuModel(FData), commandId);
end;

function TCefMenuModelRef.GetGroupIdAt(index: cint): cint;
begin
  Result := PCefMenuModel(FData)^.get_group_id(PCefMenuModel(FData), index);
end;

function TCefMenuModelRef.GetIndexOf(commandId: cint): cint;
begin
  Result := PCefMenuModel(FData)^.get_index_of(PCefMenuModel(FData), commandId);
end;

function TCefMenuModelRef.GetLabel(commandId: cint): ustring;
begin
  Result := TWACef.StringFreeAndGet(PCefMenuModel(FData)^.get_label(PCefMenuModel(FData), commandId));
end;

function TCefMenuModelRef.GetLabelAt(index: cint): ustring;
begin
  Result := TWACef.StringFreeAndGet(PCefMenuModel(FData)^.get_label_at(PCefMenuModel(FData), index));
end;

function TCefMenuModelRef.GetSubMenu(commandId: cint): ICefMenuModel;
begin
  Result := TCefMenuModelRef.UnWrap(PCefMenuModel(FData)^.get_sub_menu(PCefMenuModel(FData), commandId));
end;

function TCefMenuModelRef.GetSubMenuAt(index: cint): ICefMenuModel;
begin
  Result := TCefMenuModelRef.UnWrap(PCefMenuModel(FData)^.get_sub_menu_at(PCefMenuModel(FData), index));
end;

function TCefMenuModelRef.GetType(commandId: cint): TCefMenuItemType;
begin
  Result := PCefMenuModel(FData)^.get_type(PCefMenuModel(FData), commandId);
end;

function TCefMenuModelRef.GetTypeAt(index: cint): TCefMenuItemType;
begin
  Result := PCefMenuModel(FData)^.get_type_at(PCefMenuModel(FData), index);
end;

function TCefMenuModelRef.HasAccelerator(commandId: cint): Boolean;
begin
  Result := PCefMenuModel(FData)^.has_accelerator(PCefMenuModel(FData), commandId) <> 0;
end;

function TCefMenuModelRef.HasAcceleratorAt(index: cint): Boolean;
begin
  Result := PCefMenuModel(FData)^.has_accelerator_at(PCefMenuModel(FData), index) <> 0;
end;

function TCefMenuModelRef.InsertCheckItemAt(index, commandId: cint;
  const text: ustring): Boolean;
var
  t: TCefString;
begin
  t := TWACef.ToCefString(text);
  Result := PCefMenuModel(FData)^.insert_check_item_at(PCefMenuModel(FData), index, commandId, @t) <> 0;
end;

function TCefMenuModelRef.InsertItemAt(index, commandId: cint;
  const text: ustring): Boolean;
var
  t: TCefString;
begin
  t := TWACef.ToCefString(text);
  Result := PCefMenuModel(FData)^.insert_item_at(PCefMenuModel(FData), index, commandId, @t) <> 0;
end;

function TCefMenuModelRef.InsertRadioItemAt(index, commandId: cint;
  const text: ustring; groupId: cint): Boolean;
var
  t: TCefString;
begin
  t := TWACef.ToCefString(text);
  Result := PCefMenuModel(FData)^.insert_radio_item_at(PCefMenuModel(FData),
    index, commandId, @t, groupId) <> 0;
end;

function TCefMenuModelRef.InsertSeparatorAt(index: cint): Boolean;
begin
  Result := PCefMenuModel(FData)^.insert_separator_at(PCefMenuModel(FData), index) <> 0;
end;

function TCefMenuModelRef.InsertSubMenuAt(index, commandId: cint;
  const text: ustring): ICefMenuModel;
var
  t: TCefString;
begin
  t := TWACef.ToCefString(text);
  Result := TCefMenuModelRef.UnWrap(PCefMenuModel(FData)^.insert_sub_menu_at(
    PCefMenuModel(FData), index, commandId, @t));
end;

function TCefMenuModelRef.IsChecked(commandId: cint): Boolean;
begin
  Result := PCefMenuModel(FData)^.is_checked(PCefMenuModel(FData), commandId) <> 0;
end;

function TCefMenuModelRef.IsCheckedAt(index: cint): Boolean;
begin
  Result := PCefMenuModel(FData)^.is_checked_at(PCefMenuModel(FData), index) <> 0;
end;

function TCefMenuModelRef.IsEnabled(commandId: cint): Boolean;
begin
  Result := PCefMenuModel(FData)^.is_enabled(PCefMenuModel(FData), commandId) <> 0;
end;

function TCefMenuModelRef.IsEnabledAt(index: cint): Boolean;
begin
  Result := PCefMenuModel(FData)^.is_enabled_at(PCefMenuModel(FData), index) <> 0;
end;

function TCefMenuModelRef.IsVisible(commandId: cint): Boolean;
begin
  Result := PCefMenuModel(FData)^.is_visible(PCefMenuModel(FData), commandId) <> 0;
end;

function TCefMenuModelRef.isVisibleAt(index: cint): Boolean;
begin
  Result := PCefMenuModel(FData)^.is_visible_at(PCefMenuModel(FData), index) <> 0;
end;

function TCefMenuModelRef.Remove(commandId: cint): Boolean;
begin
  Result := PCefMenuModel(FData)^.remove(PCefMenuModel(FData), commandId) <> 0;
end;

function TCefMenuModelRef.RemoveAccelerator(commandId: cint): Boolean;
begin
  Result := PCefMenuModel(FData)^.remove_accelerator(PCefMenuModel(FData), commandId) <> 0;
end;

function TCefMenuModelRef.RemoveAcceleratorAt(index: cint): Boolean;
begin
  Result := PCefMenuModel(FData)^.remove_accelerator_at(PCefMenuModel(FData), index) <> 0;
end;

function TCefMenuModelRef.RemoveAt(index: cint): Boolean;
begin
  Result := PCefMenuModel(FData)^.remove_at(PCefMenuModel(FData), index) <> 0;
end;

function TCefMenuModelRef.SetAccelerator(commandId, keyCode: cint;
  shiftPressed, ctrlPressed, altPressed: Boolean): Boolean;
begin
  Result := PCefMenuModel(FData)^.set_accelerator(PCefMenuModel(FData),
    commandId, keyCode, Ord(shiftPressed), Ord(ctrlPressed), Ord(altPressed)) <> 0;
end;

function TCefMenuModelRef.SetAcceleratorAt(index, keyCode: cint;
  shiftPressed, ctrlPressed, altPressed: Boolean): Boolean;
begin
  Result := PCefMenuModel(FData)^.set_accelerator_at(PCefMenuModel(FData),
    index, keyCode, Ord(shiftPressed), Ord(ctrlPressed), Ord(altPressed)) <> 0;
end;

function TCefMenuModelRef.setChecked(commandId: cint;
  checked: Boolean): Boolean;
begin
  Result := PCefMenuModel(FData)^.set_checked(PCefMenuModel(FData),
    commandId, Ord(checked)) <> 0;
end;

function TCefMenuModelRef.setCheckedAt(index: cint;
  checked: Boolean): Boolean;
begin
  Result := PCefMenuModel(FData)^.set_checked_at(PCefMenuModel(FData), index, Ord(checked)) <> 0;
end;

function TCefMenuModelRef.SetCommandIdAt(index, commandId: cint): Boolean;
begin
  Result := PCefMenuModel(FData)^.set_command_id_at(PCefMenuModel(FData), index, commandId) <> 0;
end;

function TCefMenuModelRef.SetEnabled(commandId: cint;
  enabled: Boolean): Boolean;
begin
  Result := PCefMenuModel(FData)^.set_enabled(PCefMenuModel(FData), commandId, Ord(enabled)) <> 0;
end;

function TCefMenuModelRef.SetEnabledAt(index: cint;
  enabled: Boolean): Boolean;
begin
  Result := PCefMenuModel(FData)^.set_enabled_at(PCefMenuModel(FData), index, Ord(enabled)) <> 0;
end;

function TCefMenuModelRef.SetGroupId(commandId, groupId: cint): Boolean;
begin
  Result := PCefMenuModel(FData)^.set_group_id(PCefMenuModel(FData), commandId, groupId) <> 0;
end;

function TCefMenuModelRef.SetGroupIdAt(index, groupId: cint): Boolean;
begin
  Result := PCefMenuModel(FData)^.set_group_id_at(PCefMenuModel(FData), index, groupId) <> 0;
end;

function TCefMenuModelRef.SetLabel(commandId: cint;
  const text: ustring): Boolean;
var
  t: TCefString;
begin
  t := TWACef.ToCefString(text);
  Result := PCefMenuModel(FData)^.set_label(PCefMenuModel(FData), commandId, @t) <> 0;
end;

function TCefMenuModelRef.SetLabelAt(index: cint;
  const text: ustring): Boolean;
var
  t: TCefString;
begin
  t := TWACef.ToCefString(text);
  Result := PCefMenuModel(FData)^.set_label_at(PCefMenuModel(FData), index, @t) <> 0;
end;

function TCefMenuModelRef.SetVisible(commandId: cint;
  visible: Boolean): Boolean;
begin
  Result := PCefMenuModel(FData)^.set_visible(PCefMenuModel(FData), commandId, Ord(visible)) <> 0;
end;

function TCefMenuModelRef.SetVisibleAt(index: cint;
  visible: Boolean): Boolean;
begin
  Result := PCefMenuModel(FData)^.set_visible_at(PCefMenuModel(FData), index, Ord(visible)) <> 0;
end;

class function TCefMenuModelRef.UnWrap(data: Pointer): ICefMenuModel;
begin
  if data <> nil then
    Result := Create(data) as ICefMenuModel else
    Result := nil;
end;

function TCefDomNodeRef.GetAsMarkup: ustring;
begin
  Result := TWACef.StringFreeAndGet(PCefDomNode(FData)^.get_as_markup(PCefDomNode(FData)));
end;

function TCefDomNodeRef.GetDocument: ICefDomDocument;
begin
  Result := TCefDomDocumentRef.UnWrap(PCefDomNode(FData)^.get_document(PCefDomNode(FData)));
end;

function TCefDomNodeRef.GetElementAttribute(const attrName: ustring): ustring;
var
  p: TCefString;
begin
  p := TWACef.ToCefString(attrName);
  Result := TWACef.StringFreeAndGet(PCefDomNode(FData)^.get_element_attribute(PCefDomNode(FData), @p));
end;

procedure TCefDomNodeRef.GetElementAttributes(const attrMap: ICefStringMap);
begin
  PCefDomNode(FData)^.get_element_attributes(PCefDomNode(FData), attrMap.Handle);
end;

function TCefDomNodeRef.GetElementInnerText: ustring;
begin
  Result := TWACef.StringFreeAndGet(PCefDomNode(FData)^.get_element_inner_text(PCefDomNode(FData)));
end;

function TCefDomNodeRef.GetElementTagName: ustring;
begin
  Result := TWACef.StringFreeAndGet(PCefDomNode(FData)^.get_element_tag_name(PCefDomNode(FData)));
end;

function TCefDomNodeRef.GetFirstChild: ICefDomNode;
begin
  Result := TCefDomNodeRef.UnWrap(PCefDomNode(FData)^.get_first_child(PCefDomNode(FData)));
end;

function TCefDomNodeRef.GetFormControlElementType: ustring;
begin
  Result := TWACef.StringFreeAndGet(PCefDomNode(FData)^.get_form_control_element_type(PCefDomNode(FData)));
end;

function TCefDomNodeRef.GetLastChild: ICefDomNode;
begin
  Result := TCefDomNodeRef.UnWrap(PCefDomNode(FData)^.get_last_child(PCefDomNode(FData)));
end;

function TCefDomNodeRef.GetName: ustring;
begin
  Result := TWACef.StringFreeAndGet(PCefDomNode(FData)^.get_name(PCefDomNode(FData)));
end;

function TCefDomNodeRef.GetNextSibling: ICefDomNode;
begin
  Result := TCefDomNodeRef.UnWrap(PCefDomNode(FData)^.get_next_sibling(PCefDomNode(FData)));
end;

function TCefDomNodeRef.GetParent: ICefDomNode;
begin
  Result := TCefDomNodeRef.UnWrap(PCefDomNode(FData)^.get_parent(PCefDomNode(FData)));
end;

function TCefDomNodeRef.GetPreviousSibling: ICefDomNode;
begin
  Result := TCefDomNodeRef.UnWrap(PCefDomNode(FData)^.get_previous_sibling(PCefDomNode(FData)));
end;

function TCefDomNodeRef.GetType: TCefDomNodeType;
begin
  Result := PCefDomNode(FData)^.get_type(PCefDomNode(FData));
end;

function TCefDomNodeRef.GetValue: ustring;
begin
  Result := TWACef.StringFreeAndGet(PCefDomNode(FData)^.get_value(PCefDomNode(FData)));
end;

function TCefDomNodeRef.HasChildren: Boolean;
begin
  Result := PCefDomNode(FData)^.has_children(PCefDomNode(FData)) <> 0;
end;

function TCefDomNodeRef.HasElementAttribute(const attrName: ustring): Boolean;
var
  p: TCefString;
begin
  p := TWACef.ToCefString(attrName);
  Result := PCefDomNode(FData)^.has_element_attribute(PCefDomNode(FData), @p) <> 0;
end;

function TCefDomNodeRef.HasElementAttributes: Boolean;
begin
  Result := PCefDomNode(FData)^.has_element_attributes(PCefDomNode(FData)) <> 0;
end;

function TCefDomNodeRef.IsEditable: Boolean;
begin
  Result := PCefDomNode(FData)^.is_editable(PCefDomNode(FData)) <> 0;
end;

function TCefDomNodeRef.IsElement: Boolean;
begin
  Result := PCefDomNode(FData)^.is_element(PCefDomNode(FData)) <> 0;
end;

function TCefDomNodeRef.IsFormControlElement: Boolean;
begin
  Result := PCefDomNode(FData)^.is_form_control_element(PCefDomNode(FData)) <> 0;
end;

function TCefDomNodeRef.IsSame(const that: ICefDomNode): Boolean;
begin
  Result := PCefDomNode(FData)^.is_same(PCefDomNode(FData), TWACef.GetData(that)) <> 0;
end;

function TCefDomNodeRef.IsText: Boolean;
begin
  Result := PCefDomNode(FData)^.is_text(PCefDomNode(FData)) <> 0;
end;

function TCefDomNodeRef.SetElementAttribute(const attrName,
  value: ustring): Boolean;
var
  p1, p2: TCefString;
begin
  p1 := TWACef.ToCefString(attrName);
  p2 := TWACef.ToCefString(value);
  Result := PCefDomNode(FData)^.set_element_attribute(PCefDomNode(FData), @p1, @p2) <> 0;
end;

function TCefDomNodeRef.SetValue(const value: ustring): Boolean;
var
  p: TCefString;
begin
  p := TWACef.ToCefString(value);
  Result := PCefDomNode(FData)^.set_value(PCefDomNode(FData), @p) <> 0;
end;

class function TCefDomNodeRef.UnWrap(data: Pointer): ICefDomNode;
begin
  if data <> nil then
    Result := Create(data) as ICefDomNode else
    Result := nil;
end;

//..............................................................................TCefDomDocumentRef
function TCefDomDocumentRef.GetBaseUrl: ustring;
begin
  Result := TWACef.StringFreeAndGet(PCefDomDocument(FData)^.get_base_url(PCefDomDocument(FData)))
end;

function TCefDomDocumentRef.GetBody: ICefDomNode;
begin
  Result :=  TCefDomNodeRef.UnWrap(PCefDomDocument(FData)^.get_body(PCefDomDocument(FData)));
end;

function TCefDomDocumentRef.GetCompleteUrl(const partialURL: ustring): ustring;
var
  p: TCefString;
begin
  p := TWACef.ToCefString(partialURL);
  Result := TWACef.StringFreeAndGet(PCefDomDocument(FData)^.get_complete_url(PCefDomDocument(FData), @p));
end;

function TCefDomDocumentRef.GetDocument: ICefDomNode;
begin
  Result := TCefDomNodeRef.UnWrap(PCefDomDocument(FData)^.get_document(PCefDomDocument(FData)));
end;

function TCefDomDocumentRef.GetElementById(const id: ustring): ICefDomNode;
var
  i: TCefString;
begin
  i := TWACef.ToCefString(id);
  Result := TCefDomNodeRef.UnWrap(PCefDomDocument(FData)^.get_element_by_id(PCefDomDocument(FData), @i));
end;

function TCefDomDocumentRef.GetFocusedNode: ICefDomNode;
begin
  Result := TCefDomNodeRef.UnWrap(PCefDomDocument(FData)^.get_focused_node(PCefDomDocument(FData)));
end;

function TCefDomDocumentRef.GetHead: ICefDomNode;
begin
  Result := TCefDomNodeRef.UnWrap(PCefDomDocument(FData)^.get_head(PCefDomDocument(FData)));
end;

function TCefDomDocumentRef.GetSelectionAsMarkup: ustring;
begin
  Result := TWACef.StringFreeAndGet(PCefDomDocument(FData)^.get_selection_as_markup(PCefDomDocument(FData)));
end;

function TCefDomDocumentRef.GetSelectionAsText: ustring;
begin
  Result := TWACef.StringFreeAndGet(PCefDomDocument(FData)^.get_selection_as_text(PCefDomDocument(FData)));
end;

function TCefDomDocumentRef.GetSelectionEndNode: ICefDomNode;
begin
  Result := TCefDomNodeRef.UnWrap(PCefDomDocument(FData)^.get_selection_end_node(PCefDomDocument(FData)));
end;

function TCefDomDocumentRef.GetSelectionEndOffset: cint;
begin
  Result := PCefDomDocument(FData)^.get_selection_end_offset(PCefDomDocument(FData));
end;

function TCefDomDocumentRef.GetSelectionStartNode: ICefDomNode;
begin
  Result := TCefDomNodeRef.UnWrap(PCefDomDocument(FData)^.get_selection_start_node(PCefDomDocument(FData)));
end;

function TCefDomDocumentRef.GetSelectionStartOffset: cint;
begin
  Result := PCefDomDocument(FData)^.get_selection_start_offset(PCefDomDocument(FData));
end;

function TCefDomDocumentRef.GetTitle: ustring;
begin
  Result := TWACef.StringFreeAndGet(PCefDomDocument(FData)^.get_title(PCefDomDocument(FData)));
end;

function TCefDomDocumentRef.GetType: TCefDomDocumentType;
begin
  Result := PCefDomDocument(FData)^.get_type(PCefDomDocument(FData));
end;

function TCefDomDocumentRef.HasSelection: Boolean;
begin
  Result := PCefDomDocument(FData)^.has_selection(PCefDomDocument(FData)) <> 0;
end;

class function TCefDomDocumentRef.UnWrap(data: Pointer): ICefDomDocument;
begin
  if data <> nil then
    Result := Create(data) as ICefDomDocument else
    Result := nil;
end;

//..............................................................................TCefDownloadItemRef
function TCefDownLoadItemRef.GetContentDisposition: ustring;
begin
  Result := TWACef.StringFreeAndGet(PCefDownloadItem(FData)^.get_content_disposition(PCefDownloadItem(FData)));
end;

function TCefDownLoadItemRef.GetCurrentSpeed: cint64;
begin
  Result := PCefDownloadItem(FData)^.get_current_speed(PCefDownloadItem(FData));
end;

function TCefDownLoadItemRef.GetEndTime: TDateTime;
begin
  Result := TWACef.CefTimeToDateTime(PCefDownloadItem(FData)^.get_end_time(PCefDownloadItem(FData)));
end;

function TCefDownLoadItemRef.GetFullPath: ustring;
begin
  Result := TWACef.StringFreeAndGet(PCefDownloadItem(FData)^.get_full_path(PCefDownloadItem(FData)));
end;

function TCefDownLoadItemRef.GetId: cint;
begin
  Result := PCefDownloadItem(FData)^.get_id(PCefDownloadItem(FData));
end;

function TCefDownLoadItemRef.GetMimeType: ustring;
begin
  Result := TWACef.StringFreeAndGet(PCefDownloadItem(FData)^.get_mime_type(PCefDownloadItem(FData)));
end;

function TCefDownLoadItemRef.GetPercentComplete: cint;
begin
  Result := PCefDownloadItem(FData)^.get_percent_complete(PCefDownloadItem(FData));
end;

function TCefDownLoadItemRef.GetReceivedBytes: cint64;
begin
  Result := PCefDownloadItem(FData)^.get_received_bytes(PCefDownloadItem(FData));
end;

function TCefDownLoadItemRef.GetStartTime: TDateTime;
begin
  Result := TWACef.CefTimeToDateTime(PCefDownloadItem(FData)^.get_start_time(PCefDownloadItem(FData)));
end;

function TCefDownLoadItemRef.GetSuggestedFileName: ustring;
begin
  Result := TWACef.StringFreeAndGet(PCefDownloadItem(FData)^.get_suggested_file_name(PCefDownloadItem(FData)));
end;

function TCefDownLoadItemRef.GetTotalBytes: cint64;
begin
  Result := PCefDownloadItem(FData)^.get_total_bytes(PCefDownloadItem(FData));
end;

function TCefDownLoadItemRef.GetUrl: ustring;
begin
  Result := TWACef.StringFreeAndGet(PCefDownloadItem(FData)^.get_url(PCefDownloadItem(FData)));
end;

function TCefDownLoadItemRef.IsCanceled: Boolean;
begin
  Result := PCefDownloadItem(FData)^.is_canceled(PCefDownloadItem(FData)) <> 0;
end;

function TCefDownLoadItemRef.IsComplete: Boolean;
begin
  Result := PCefDownloadItem(FData)^.is_complete(PCefDownloadItem(FData)) <> 0;
end;

function TCefDownLoadItemRef.IsInProgress: Boolean;
begin
  Result := PCefDownloadItem(FData)^.is_in_progress(PCefDownloadItem(FData)) <> 0;
end;

function TCefDownLoadItemRef.IsValid: Boolean;
begin
  Result := PCefDownloadItem(FData)^.is_valid(PCefDownloadItem(FData)) <> 0;
end;

class function TCefDownLoadItemRef.UnWrap(data: Pointer): ICefDownLoadItem;
begin
  if data <> nil then
    Result := Create(data) as ICefDownLoadItem else
    Result := nil;
end;

//..............................................................................TCefBeforeDownloadCallbackRef

procedure TCefBeforeDownloadCallbackRef.Cont(const downloadPath: ustring;
  showDialog: Boolean);
var
  dp: TCefString;
begin
  dp := TWACef.ToCefString(downloadPath);
  PCefBeforeDownloadCallback(FData)^.cont(PCefBeforeDownloadCallback(FData), @dp, Ord(showDialog));
end;

class function TCefBeforeDownloadCallbackRef.UnWrap(
  data: Pointer): ICefBeforeDownloadCallback;
begin
  if data <> nil then
    Result := Create(data) as ICefBeforeDownloadCallback else
    Result := nil;
end;

//..............................................................................TCefDownloadItemCallbackRef

procedure TCefDownloadItemCallbackRef.cancel;
begin
  PCefDownloadItemCallback(FData)^.cancel(PCefDownloadItemCallback(FData));
end;

class function TCefDownloadItemCallbackRef.UnWrap(
  data: Pointer): ICefDownloadItemCallback;
begin
  if data <> nil then
    Result := Create(data) as ICefDownloadItemCallback else
    Result := nil;
end;


//..............................................................................TCefJSDialogCallbackRef
procedure TCefJsDialogCallbackRef.Cont(success: Boolean;
  const userInput: ustring);
var
  ui: TCefString;
begin
  ui := TWACef.ToCefString(userInput);
  PCefJsDialogCallback(FData)^.cont(PCefJsDialogCallback(FData), Ord(success), @ui);
end;

class function TCefJsDialogCallbackRef.UnWrap(
  data: Pointer): ICefJsDialogCallback;
begin
  if data <> nil then
    Result := Create(data) as ICefJsDialogCallback else
    Result := nil;
end;

//..............................................................................TCefGeolocationCallbackRef

procedure TCefGeolocationCallbackRef.Cont(allow: Boolean);
begin
  PCefGeolocationCallback(FData)^.cont(PCefGeolocationCallback(FData), Ord(allow));
end;

class function TCefGeolocationCallbackRef.UnWrap(
  data: Pointer): ICefGeolocationCallback;
begin
  if data <> nil then
    Result := Create(data) as ICefGeolocationCallback else
    Result := nil;
end;

//..............................................................................TCefListValueRef

function TCefListValueRef.Clear: Boolean;
begin
  Result := PCefListValue(FData)^.clear(PCefListValue(FData)) <> 0;
end;

function TCefListValueRef.Copy: ICefListValue;
begin
  Result := UnWrap(PCefListValue(FData)^.copy(PCefListValue(FData)));
end;

class function TCefListValueRef.New: ICefListValue;
begin
  result := UnWrap(cef_list_value_create());
end;

function TCefListValueRef.GetBinary(index: cint): ICefBinaryValue;
begin
  Result := TCefBinaryValueRef.UnWrap(PCefListValue(FData)^.get_binary(PCefListValue(FData), index));
end;

function TCefListValueRef.GetBool(index: cint): Boolean;
begin
  Result := PCefListValue(FData)^.get_bool(PCefListValue(FData), index) <> 0;
end;

function TCefListValueRef.GetDictionary(index: cint): ICefDictionaryValue;
begin
  Result := TCefDictionaryValueRef.UnWrap(PCefListValue(FData)^.get_dictionary(PCefListValue(FData), index));
end;

function TCefListValueRef.GetDouble(index: cint): Double;
begin
  Result := PCefListValue(FData)^.get_double(PCefListValue(FData), index);
end;

function TCefListValueRef.GetInt(index: cint): cint;
begin
  Result := PCefListValue(FData)^.get_int(PCefListValue(FData), index);
end;

function TCefListValueRef.GetList(index: cint): ICefListValue;
begin
  Result := UnWrap(PCefListValue(FData)^.get_list(PCefListValue(FData), index));
end;

function TCefListValueRef.GetSize: csize_t;
begin
  Result := PCefListValue(FData)^.get_size(PCefListValue(FData));
end;

function TCefListValueRef.GetString(index: cint): ustring;
begin
  Result := TWACef.StringFreeAndGet(PCefListValue(FData)^.get_string(PCefListValue(FData), index));
end;

function TCefListValueRef.GetType(index: cint): TCefValueType;
begin
  Result := PCefListValue(FData)^.get_type(PCefListValue(FData), index);
end;

function TCefListValueRef.IsOwned: Boolean;
begin
  Result := PCefListValue(FData)^.is_owned(PCefListValue(FData)) <> 0;
end;

function TCefListValueRef.IsReadOnly: Boolean;
begin
  Result := PCefListValue(FData)^.is_read_only(PCefListValue(FData)) <> 0;
end;

function TCefListValueRef.IsValid: Boolean;
begin
  Result := PCefListValue(FData)^.is_valid(PCefListValue(FData)) <> 0;
end;

function TCefListValueRef.Remove(index: cint): Boolean;
begin
  Result := PCefListValue(FData)^.remove(PCefListValue(FData), index) <> 0;
end;

function TCefListValueRef.SetBinary(index: cint;
  const value: ICefBinaryValue): Boolean;
begin
  Result := PCefListValue(FData)^.set_binary(PCefListValue(FData), index, TWACef.GetData(value)) <> 0;
end;

function TCefListValueRef.SetBool(index: cint; value: Boolean): Boolean;
begin
  Result := PCefListValue(FData)^.set_bool(PCefListValue(FData), index, Ord(value)) <> 0;
end;

function TCefListValueRef.SetDictionary(index: cint;
  const value: ICefDictionaryValue): Boolean;
begin
  Result := PCefListValue(FData)^.set_dictionary(PCefListValue(FData), index, TWACef.GetData(value)) <> 0;
end;

function TCefListValueRef.SetDouble(index: cint; value: Double): Boolean;
begin
  Result := PCefListValue(FData)^.set_double(PCefListValue(FData), index, value) <> 0;
end;

function TCefListValueRef.SetInt(index, value: cint): Boolean;
begin
  Result := PCefListValue(FData)^.set_int(PCefListValue(FData), index, value) <> 0;
end;

function TCefListValueRef.SetList(index: cint;
  const value: ICefListValue): Boolean;
begin
  Result := PCefListValue(FData)^.set_list(PCefListValue(FData), index, TWACef.GetData(value)) <> 0;
end;

function TCefListValueRef.SetNull(index: cint): Boolean;
begin
  Result := PCefListValue(FData)^.set_null(PCefListValue(FData), index) <> 0;
end;

function TCefListValueRef.SetSize(size: csize_t): Boolean;
begin
  Result := PCefListValue(FData)^.set_size(PCefListValue(FData), size) <> 0;
end;

function TCefListValueRef.SetString(index: cint;
  const value: ustring): Boolean;
var
  v: TCefString;
begin
  v := TWACef.ToCefString(value);
  Result := PCefListValue(FData)^.set_string(PCefListValue(FData), index, @v) <> 0;
end;

class function TCefListValueRef.UnWrap(data: Pointer): ICefListValue;
begin
  if data <> nil then
    Result := Create(data) as ICefListValue else
    Result := nil;
end;

//..............................................................................TCefBinaryValueRef

function TCefBinaryValueRef.Copy: ICefBinaryValue;
begin
  Result := UnWrap(PCefBinaryValue(FData)^.copy(PCefBinaryValue(FData)));
end;

function TCefBinaryValueRef.GetData(buffer: Pointer; bufferSize,
  dataOffset: csize_t): csize_t;
begin
  Result := PCefBinaryValue(FData)^.get_data(PCefBinaryValue(FData), buffer, bufferSize, dataOffset);
end;

function TCefBinaryValueRef.GetSize: csize_t;
begin
  Result := PCefBinaryValue(FData)^.get_size(PCefBinaryValue(FData));
end;

function TCefBinaryValueRef.IsOwned: Boolean;
begin
  Result := PCefBinaryValue(FData)^.is_owned(PCefBinaryValue(FData)) <> 0;
end;

function TCefBinaryValueRef.IsValid: Boolean;
begin
  Result := PCefBinaryValue(FData)^.is_valid(PCefBinaryValue(FData)) <> 0;
end;

class function TCefBinaryValueRef.New(const data: Pointer; dataSize: csize_t): ICefBinaryValue;
begin
  Result := UnWrap(cef_binary_value_create(data, dataSize));
end;

class function TCefBinaryValueRef.UnWrap(data: Pointer): ICefBinaryValue;
begin
  if data <> nil then
    Result := Create(data) as ICefBinaryValue else
    Result := nil;
end;

//..............................................................................TCefDictionaryValueRef

function TCefDictionaryValueRef.Clear: Boolean;
begin
  Result := PCefDictionaryValue(FData)^.clear(PCefDictionaryValue(FData)) <> 0;
end;

function TCefDictionaryValueRef.Copy(
  excludeEmptyChildren: Boolean): ICefDictionaryValue;
begin
  Result := UnWrap(PCefDictionaryValue(FData)^.copy(PCefDictionaryValue(FData), Ord(excludeEmptyChildren)));
end;

function TCefDictionaryValueRef.GetBinary(const key: ustring): ICefBinaryValue;
var
  k: TCefString;
begin
  k := TWACef.ToCefString(key);
  Result := TCefBinaryValueRef.UnWrap(PCefDictionaryValue(FData)^.get_binary(PCefDictionaryValue(FData), @k));
end;

function TCefDictionaryValueRef.GetBool(const key: ustring): Boolean;
var
  k: TCefString;
begin
  k := TWACef.ToCefString(key);
  Result := PCefDictionaryValue(FData)^.get_bool(PCefDictionaryValue(FData), @k) <> 0;
end;

function TCefDictionaryValueRef.GetDictionary(
  const key: ustring): ICefDictionaryValue;
var
  k: TCefString;
begin
  k := TWACef.ToCefString(key);
  Result := UnWrap(PCefDictionaryValue(FData)^.get_dictionary(PCefDictionaryValue(FData), @k));
end;

function TCefDictionaryValueRef.GetDouble(const key: ustring): Double;
var
  k: TCefString;
begin
  k := TWACef.ToCefString(key);
  Result := PCefDictionaryValue(FData)^.get_double(PCefDictionaryValue(FData), @k);
end;

function TCefDictionaryValueRef.GetInt(const key: ustring): cint;
var
  k: TCefString;
begin
  k := TWACef.ToCefString(key);
  Result := PCefDictionaryValue(FData)^.get_int(PCefDictionaryValue(FData), @k);
end;

function TCefDictionaryValueRef.GetKeys(const keys: TStrings): Boolean;
var
  list: TCefStringList;
  i: cint;
  str: TCefString;
begin
  list := cef_string_list_alloc();
  try
    Result := PCefDictionaryValue(FData)^.get_keys(PCefDictionaryValue(FData), list) <> 0;
    FillChar(str, SizeOf(str), 0);
    for i := 0 to cef_string_list_size(list) - 1 do
    begin
      FillChar(str, SizeOf(str), 0);
      cef_string_list_value(list, i, @str);
      keys.Add(TWACef.StringClearAndGet(str));
    end;
  finally
    cef_string_list_free(list);
  end;
end;

function TCefDictionaryValueRef.GetList(const key: ustring): ICefListValue;
var
  k: TCefString;
begin
  k := TWACef.ToCefString(key);
  Result := TCefListValueRef.UnWrap(PCefDictionaryValue(FData)^.get_list(PCefDictionaryValue(FData), @k));
end;

function TCefDictionaryValueRef.GetSize: csize_t;
begin
  Result := PCefDictionaryValue(FData)^.get_size(PCefDictionaryValue(FData));
end;

function TCefDictionaryValueRef.GetString(const key: ustring): ustring;
var
  k: TCefString;
begin
  k := TWACef.ToCefString(key);
  Result := TWACef.StringFreeAndGet(PCefDictionaryValue(FData)^.get_string(PCefDictionaryValue(FData), @k));
end;

function TCefDictionaryValueRef.GetType(const key: ustring): TCefValueType;
var
  k: TCefString;
begin
  k := TWACef.ToCefString(key);
  Result := PCefDictionaryValue(FData)^.get_type(PCefDictionaryValue(FData), @k);
end;

function TCefDictionaryValueRef.HasKey(const key: ustring): Boolean;
var
  k: TCefString;
begin
  k := TWACef.ToCefString(key);
  Result := PCefDictionaryValue(FData)^.has_key(PCefDictionaryValue(FData), @k) <> 0;
end;

function TCefDictionaryValueRef.isOwned: Boolean;
begin
  Result := PCefDictionaryValue(FData)^.is_owned(PCefDictionaryValue(FData)) <> 0;
end;

function TCefDictionaryValueRef.IsReadOnly: Boolean;
begin
  Result := PCefDictionaryValue(FData)^.is_read_only(PCefDictionaryValue(FData)) <> 0;
end;

function TCefDictionaryValueRef.IsValid: Boolean;
begin
  Result := PCefDictionaryValue(FData)^.is_valid(PCefDictionaryValue(FData)) <> 0;
end;

class function TCefDictionaryValueRef.New: ICefDictionaryValue;
begin
  Result := UnWrap(cef_dictionary_value_create());
end;

function TCefDictionaryValueRef.Remove(const key: ustring): Boolean;
var
  k: TCefString;
begin
  k := TWACef.ToCefString(key);
  Result := PCefDictionaryValue(FData)^.remove(PCefDictionaryValue(FData), @k) <> 0;
end;

function TCefDictionaryValueRef.SetBinary(const key: ustring;
  const value: ICefBinaryValue): Boolean;
var
  k: TCefString;
begin
  k := TWACef.ToCefString(key);
  Result := PCefDictionaryValue(FData)^.set_binary(PCefDictionaryValue(FData), @k, TWACef.GetData(value)) <> 0;
end;

function TCefDictionaryValueRef.SetBool(const key: ustring;
  value: Boolean): Boolean;
var
  k: TCefString;
begin
  k := TWACef.ToCefString(key);
  Result := PCefDictionaryValue(FData)^.set_bool(PCefDictionaryValue(FData), @k, Ord(value)) <> 0;
end;

function TCefDictionaryValueRef.SetDictionary(const key: ustring;
  const value: ICefDictionaryValue): Boolean;
var
  k: TCefString;
begin
  k := TWACef.ToCefString(key);
  Result := PCefDictionaryValue(FData)^.set_dictionary(PCefDictionaryValue(FData), @k, TWACef.GetData(value)) <> 0;
end;

function TCefDictionaryValueRef.SetDouble(const key: ustring;
  value: Double): Boolean;
var
  k: TCefString;
begin
  k := TWACef.ToCefString(key);
  Result := PCefDictionaryValue(FData)^.set_double(PCefDictionaryValue(FData), @k, value) <> 0;
end;

function TCefDictionaryValueRef.SetInt(const key: ustring;
  value: cint): Boolean;
var
  k: TCefString;
begin
  k := TWACef.ToCefString(key);
  Result := PCefDictionaryValue(FData)^.set_int(PCefDictionaryValue(FData), @k, value) <> 0;
end;

function TCefDictionaryValueRef.SetList(const key: ustring;
  const value: ICefListValue): Boolean;
var
  k: TCefString;
begin
  k := TWACef.ToCefString(key);
  Result := PCefDictionaryValue(FData)^.set_list(PCefDictionaryValue(FData), @k, TWACef.GetData(value)) <> 0;
end;

function TCefDictionaryValueRef.SetNull(const key: ustring): Boolean;
var
  k: TCefString;
begin
  k := TWACef.ToCefString(key);
  Result := PCefDictionaryValue(FData)^.set_null(PCefDictionaryValue(FData), @k) <> 0;
end;

function TCefDictionaryValueRef.SetString(const key, value: ustring): Boolean;
var
  k, v: TCefString;
begin
  k := TWACef.ToCefString(key);
  v := TWACef.ToCefString(value);
  Result := PCefDictionaryValue(FData)^.set_string(PCefDictionaryValue(FData), @k, @v) <> 0;
end;

class function TCefDictionaryValueRef.UnWrap(
  data: Pointer): ICefDictionaryValue;
begin
  if data <> nil then
    Result := Create(data) as ICefDictionaryValue else
    Result := nil;
end;

//..............................................................................TCefPostDataRef

function TCefPostDataRef.IsReadOnly: Boolean;
begin
  Result := PCefPostData(FData)^.is_read_only(PCefPostData(FData)) <> 0;
end;

function TCefPostDataRef.AddElement(
  const element: ICefPostDataElement): cint;
begin
  Result := PCefPostData(FData)^.add_element(PCefPostData(FData), TWACef.GetData(element));
end;

function TCefPostDataRef.GetElementCount: csize_t;
begin
  Result := PCefPostData(FData)^.get_element_count(PCefPostData(FData))
end;

function TCefPostDataRef.GetElements(Count: csize_t): IInterfaceList;
var
  items: PCefPostDataElementArray;
  i: cint;
begin
  Result := TInterfaceList.Create;
  GetMem(items, SizeOf(PCefPostDataElement) * Count);
  FillChar(items^, SizeOf(PCefPostDataElement) * Count, 0);
  try
    PCefPostData(FData)^.get_elements(PCefPostData(FData), @Count, items);
    for i := 0 to Count - 1 do
      Result.Add(TCefPostDataElementRef.UnWrap(items[i]));
  finally
    FreeMem(items);
  end;
end;

class function TCefPostDataRef.New: ICefPostData;
begin
  Result := UnWrap(cef_post_data_create());
end;

function TCefPostDataRef.RemoveElement(
  const element: ICefPostDataElement): cint;
begin
  Result := PCefPostData(FData)^.remove_element(PCefPostData(FData), TWACef.GetData(element));
end;

procedure TCefPostDataRef.RemoveElements;
begin
  PCefPostData(FData)^.remove_elements(PCefPostData(FData));
end;

class function TCefPostDataRef.UnWrap(data: Pointer): ICefPostData;
begin
  if data <> nil then
    Result := Create(data) as ICefPostData else
    Result := nil;
end;

//..............................................................................TCefPostDataElementRef

function TCefPostDataElementRef.IsReadOnly: Boolean;
begin
  Result := PCefPostDataElement(FData)^.is_read_only(PCefPostDataElement(FData)) <> 0;
end;

function TCefPostDataElementRef.GetBytes(size: csize_t;
  bytes: Pointer): csize_t;
begin
  Result := PCefPostDataElement(FData)^.get_bytes(PCefPostDataElement(FData), size, bytes);
end;

function TCefPostDataElementRef.GetBytesCount: csize_t;
begin
  Result := PCefPostDataElement(FData)^.get_bytes_count(PCefPostDataElement(FData));
end;

function TCefPostDataElementRef.GetFile: ustring;
begin
  Result := TWACef.StringFreeAndGet(PCefPostDataElement(FData)^.get_file(PCefPostDataElement(FData)));
end;

function TCefPostDataElementRef.GetType: TCefPostDataElementType;
begin
  Result := PCefPostDataElement(FData)^.get_type(PCefPostDataElement(FData));
end;

class function TCefPostDataElementRef.New: ICefPostDataElement;
begin
  Result := UnWrap(cef_post_data_element_create());
end;

procedure TCefPostDataElementRef.SetToBytes(size: csize_t; const bytes: Pointer);
begin
  PCefPostDataElement(FData)^.set_to_bytes(PCefPostDataElement(FData), size, bytes);
end;

procedure TCefPostDataElementRef.SetToEmpty;
begin
  PCefPostDataElement(FData)^.set_to_empty(PCefPostDataElement(FData));
end;

procedure TCefPostDataElementRef.SetToFile(const fileName: ustring);
var
  f: TCefString;
begin
  f := TWACef.ToCefString(fileName);
  PCefPostDataElement(FData)^.set_to_file(PCefPostDataElement(FData), @f);
end;

class function TCefPostDataElementRef.UnWrap(data: Pointer): ICefPostDataElement;
begin
  if data <> nil then
    Result := Create(data) as ICefPostDataElement else
    Result := nil;
end;

//..............................................................................TCefRequestRef

function TCefRequestRef.IsReadOnly: Boolean;
begin
  Result := PCefRequest(FData)^.is_read_only(PCefRequest(FData)) <> 0;
end;

procedure TCefRequestRef.Assign(const url, method: ustring;
  const postData: ICefPostData; const headerMap: ICefStringMultimap);
var
  u, m: TCefString;
begin
  u := TWACef.ToCefString(url);
  m := TWACef.ToCefString(method);
  PCefRequest(FData)^._set(PCefRequest(FData), @u, @m, TWACef.GetData(postData), headerMap.Handle);
end;

function TCefRequestRef.GetFirstPartyForCookies: ustring;
begin
  Result := TWACef.StringFreeAndGet(PCefRequest(FData)^.get_first_party_for_cookies(PCefRequest(FData)));
end;

function TCefRequestRef.GetFlags: TCefUrlRequestFlags;
begin
  PByte(@result)^ := PCefRequest(FData)^.get_flags(PCefRequest(FData));
end;

procedure TCefRequestRef.GetHeaderMap(const HeaderMap: ICefStringMultimap);
begin
  PCefRequest(FData)^.get_header_map(PCefRequest(FData), HeaderMap.Handle);
end;

function TCefRequestRef.GetMethod: ustring;
begin
  Result := TWACef.StringFreeAndGet(PCefRequest(FData)^.get_method(PCefRequest(FData)))
end;

function TCefRequestRef.GetPostData: ICefPostData;
begin
  Result := TCefPostDataRef.UnWrap(PCefRequest(FData)^.get_post_data(PCefRequest(FData)));
end;

function TCefRequestRef.GetUrl: ustring;
begin
  Result := TWACef.StringFreeAndGet(PCefRequest(FData)^.get_url(PCefRequest(FData)))
end;

class function TCefRequestRef.New: ICefRequest;
begin
  Result := UnWrap(cef_request_create());
end;

procedure TCefRequestRef.SetFirstPartyForCookies(const url: ustring);
var
  str: TCefString;
begin
  str := TWACef.ToCefString(url);
  PCefRequest(FData)^.set_first_party_for_cookies(PCefRequest(FData), @str);
end;

procedure TCefRequestRef.SetFlags(flags: TCefUrlRequestFlags);
begin
  PCefRequest(FData)^.set_flags(PCefRequest(FData), PByte(@flags)^);
end;

procedure TCefRequestRef.SetHeaderMap(const HeaderMap: ICefStringMultimap);
begin
  PCefRequest(FData)^.set_header_map(PCefRequest(FData), HeaderMap.Handle);
end;

procedure TCefRequestRef.SetMethod(const value: ustring);
var
  v: TCefString;
begin
  v := TWACef.ToCefString(value);
  PCefRequest(FData)^.set_method(PCefRequest(FData), @v);
end;

procedure TCefRequestRef.SetPostData(const value: ICefPostData);
begin
  if value <> nil then
    PCefRequest(FData)^.set_post_data(PCefRequest(FData), TWACef.GetData(value));
end;

procedure TCefRequestRef.SetUrl(const value: ustring);
var
  v: TCefString;
begin
  v := TWACef.ToCefString(value);
  PCefRequest(FData)^.set_url(PCefRequest(FData), @v);
end;

function TCefRequestRef.GetResourceType: TCefResourceType;
begin
  result := PCefRequest(FData)^.get_resource_type(PCefRequest(FData));
end;

function TCefRequestRef.GetTransitionType: TCefTransitionType;
begin
  result := PCefRequest(FData)^.get_transition_type(PCefRequest(FData));
end;

class function TCefRequestRef.UnWrap(data: Pointer): ICefRequest;
begin
  if data <> nil then
    Result := Create(data) as ICefRequest else
    Result := nil;
end;

//..............................................................................TCefStreamReaderRef

class function TCefStreamReaderRef.CreateForCustomStream(
  const stream: ICefCustomStreamReader): ICefStreamReader;
begin
  Result := UnWrap(cef_stream_reader_create_for_handler(TWACef.GetData(stream)))
end;

class function TCefStreamReaderRef.CreateForData(data: Pointer; size: csize_t): ICefStreamReader;
begin
  Result := UnWrap(cef_stream_reader_create_for_data(data, size))
end;

class function TCefStreamReaderRef.CreateForFile(const filename: ustring): ICefStreamReader;
var
  f: TCefString;
begin
  f := TWACef.ToCefString(filename);
  Result := UnWrap(cef_stream_reader_create_for_file(@f))
end;

class function TCefStreamReaderRef.CreateForStream(const stream: TSTream;
  owned: Boolean): ICefStreamReader;
begin
  Result := CreateForCustomStream(TCefCustomStreamReader.Create(stream, owned) as ICefCustomStreamReader);
end;

function TCefStreamReaderRef.Eof: Boolean;
begin
  Result := PCefStreamReader(FData)^.eof(PCefStreamReader(FData)) <> 0;
end;

function TCefStreamReaderRef.MayBlock: Boolean;
begin
  Result := PCefStreamReader(FData)^.may_block(PCefStreamReader(FData)) <> 0;
end;

function TCefStreamReaderRef.Read(ptr: Pointer; size, n: csize_t): csize_t;
begin
  Result := PCefStreamReader(FData)^.read(PCefStreamReader(FData), ptr, size, n);
end;

function TCefStreamReaderRef.Seek(offset: cint64; whence: cint): cint;
begin
  Result := PCefStreamReader(FData)^.seek(PCefStreamReader(FData), offset, whence);
end;

function TCefStreamReaderRef.Tell: cint64;
begin
  Result := PCefStreamReader(FData)^.tell(PCefStreamReader(FData));
end;

class function TCefStreamReaderRef.UnWrap(data: Pointer): ICefStreamReader;
begin
  if data <> nil then
    Result := Create(data) as ICefStreamReader else
    Result := nil;
end;

//..............................................................................TCefQuotaCallbackRef

procedure TCefQuotaCallbackRef.Cancel;
begin
  PCefQuotaCallback(FData)^.cancel(FData);
end;

procedure TCefQuotaCallbackRef.Cont(allow: Boolean);
begin
  PCefQuotaCallback(FData)^.cont(FData, Ord(allow));
end;

class function TCefQuotaCallbackRef.UnWrap(data: Pointer): ICefQuotaCallback;
begin
  if data <> nil then
    Result := Create(data) as ICefQuotaCallback else
    Result := nil;
end;

//..............................................................................TCefCallbackRef
procedure TCefCallbackRef.Cancel;
begin
  PCefCallback(FData)^.cancel(PCefCallback(FData));
end;

procedure TCefCallbackRef.Cont;
begin
  PCefCallback(FData)^.cont(PCefCallback(FData));
end;

class function TCefCallbackRef.UnWrap(data: Pointer): ICefCallback;
begin
  if data <> nil then
    Result := Create(data) as ICefCallback else
    Result := nil;
end;


//..............................................................................TCefXmlReaderRef

function TCefXmlReaderRef.Close: Boolean;
begin
  Result := PCefXmlReader(FData)^.close(FData) <> 0;
end;

class function TCefXmlReaderRef.New(const stream: ICefStreamReader;
  encodingType: TCefXmlEncodingType; const URI: ustring): ICefXmlReader;
var
  u: TCefString;
begin
  u := TWACef.ToCefString(URI);
  Result := UnWrap(cef_xml_reader_create(TWACef.GetData(stream), encodingType, @u));
end;

function TCefXmlReaderRef.GetAttributeByIndex(index: cint): ustring;
begin
  Result := TWACef.StringFreeAndGet(PCefXmlReader(FData)^.get_attribute_byindex(FData, index));
end;

function TCefXmlReaderRef.GetAttributeByLName(const localName,
  namespaceURI: ustring): ustring;
var
  l, n: TCefString;
begin
  l := TWACef.ToCefString(localName);
  n := TWACef.ToCefString(namespaceURI);
  Result := TWACef.StringFreeAndGet(PCefXmlReader(FData)^.get_attribute_bylname(FData, @l, @n));
end;

function TCefXmlReaderRef.GetAttributeByQName(
  const qualifiedName: ustring): ustring;
var
  q: TCefString;
begin
  q := TWACef.ToCefString(qualifiedName);
  Result := TWACef.StringFreeAndGet(PCefXmlReader(FData)^.get_attribute_byqname(FData, @q));
end;

function TCefXmlReaderRef.GetAttributeCount: csize_t;
begin
  Result := PCefXmlReader(FData)^.get_attribute_count(FData);
end;

function TCefXmlReaderRef.GetBaseUri: ustring;
begin
  Result := TWACef.StringFreeAndGet(PCefXmlReader(FData)^.get_base_uri(FData));
end;

function TCefXmlReaderRef.GetDepth: cint;
begin
  Result := PCefXmlReader(FData)^.get_depth(FData);
end;

function TCefXmlReaderRef.GetError: ustring;
begin
  Result := TWACef.StringFreeAndGet(PCefXmlReader(FData)^.get_error(FData));
end;

function TCefXmlReaderRef.GetInnerXml: ustring;
begin
  Result := TWACef.StringFreeAndGet(PCefXmlReader(FData)^.get_inner_xml(FData));
end;

function TCefXmlReaderRef.GetLineNumber: cint;
begin
  Result := PCefXmlReader(FData)^.get_line_number(FData);
end;

function TCefXmlReaderRef.GetLocalName: ustring;
begin
  Result := TWACef.StringFreeAndGet(PCefXmlReader(FData)^.get_local_name(FData));
end;

function TCefXmlReaderRef.GetNamespaceUri: ustring;
begin
  Result := TWACef.StringFreeAndGet(PCefXmlReader(FData)^.get_namespace_uri(FData));
end;

function TCefXmlReaderRef.GetOuterXml: ustring;
begin
  Result := TWACef.StringFreeAndGet(PCefXmlReader(FData)^.get_outer_xml(FData));
end;

function TCefXmlReaderRef.GetPrefix: ustring;
begin
  Result := TWACef.StringFreeAndGet(PCefXmlReader(FData)^.get_prefix(FData));
end;

function TCefXmlReaderRef.GetQualifiedName: ustring;
begin
  Result := TWACef.StringFreeAndGet(PCefXmlReader(FData)^.get_qualified_name(FData));
end;

function TCefXmlReaderRef.GetType: TCefXmlNodeType;
begin
  Result := PCefXmlReader(FData)^.get_type(FData);
end;

function TCefXmlReaderRef.GetValue: ustring;
begin
  Result := TWACef.StringFreeAndGet(PCefXmlReader(FData)^.get_value(FData));
end;

function TCefXmlReaderRef.GetXmlLang: ustring;
begin
  Result := TWACef.StringFreeAndGet(PCefXmlReader(FData)^.get_xml_lang(FData));
end;

function TCefXmlReaderRef.HasAttributes: Boolean;
begin
  Result := PCefXmlReader(FData)^.has_attributes(FData) <> 0;
end;

function TCefXmlReaderRef.HasError: Boolean;
begin
  Result := PCefXmlReader(FData)^.has_error(FData) <> 0;
end;

function TCefXmlReaderRef.HasValue: Boolean;
begin
  Result := PCefXmlReader(FData)^.has_value(FData) <> 0;
end;

function TCefXmlReaderRef.IsEmptyElement: Boolean;
begin
  Result := PCefXmlReader(FData)^.is_empty_element(FData) <> 0;
end;

function TCefXmlReaderRef.MoveToAttributeByIndex(index: cint): Boolean;
begin
  Result := PCefXmlReader(FData)^.move_to_attribute_byindex(FData, index) <> 0;
end;

function TCefXmlReaderRef.MoveToAttributeByLName(const localName,
  namespaceURI: ustring): Boolean;
var
  l, n: TCefString;
begin
  l := TWACef.ToCefString(localName);
  n := TWACef.ToCefString(namespaceURI);
  Result := PCefXmlReader(FData)^.move_to_attribute_bylname(FData, @l, @n) <> 0;
end;

function TCefXmlReaderRef.MoveToAttributeByQName(
  const qualifiedName: ustring): Boolean;
var
  q: TCefString;
begin
  q := TWACef.ToCefString(qualifiedName);
  Result := PCefXmlReader(FData)^.move_to_attribute_byqname(FData, @q) <> 0;
end;

function TCefXmlReaderRef.MoveToCarryingElement: Boolean;
begin
  Result := PCefXmlReader(FData)^.move_to_carrying_element(FData) <> 0;
end;

function TCefXmlReaderRef.MoveToFirstAttribute: Boolean;
begin
  Result := PCefXmlReader(FData)^.move_to_first_attribute(FData) <> 0;
end;

function TCefXmlReaderRef.MoveToNextAttribute: Boolean;
begin
  Result := PCefXmlReader(FData)^.move_to_next_attribute(FData) <> 0;
end;

function TCefXmlReaderRef.MoveToNextNode: Boolean;
begin
  Result := PCefXmlReader(FData)^.move_to_next_node(FData) <> 0;
end;

class function TCefXmlReaderRef.UnWrap(data: Pointer): ICefXmlReader;
begin
  if data <> nil then
    Result := Create(data) as ICefXmlReader else
    Result := nil;
end;

//..............................................................................TCefZipReaderRef

function TCefZipReaderRef.Close: Boolean;
begin
  Result := PCefZipReader(FData)^.close(FData) <> 0;
end;

function TCefZipReaderRef.CloseFile: Boolean;
begin
  Result := PCefZipReader(FData)^.close_file(FData) <> 0;
end;

class function TCefZipReaderRef.New(const stream: ICefStreamReader): ICefZipReader;
begin
  Result := UnWrap(cef_zip_reader_create(TWACef.GetData(stream)));
end;

function TCefZipReaderRef.Eof: Boolean;
begin
  Result := PCefZipReader(FData)^.eof(FData) <> 0;
end;

function TCefZipReaderRef.GetFileLastModified: LongInt;
begin
  Result := PCefZipReader(FData)^.get_file_last_modified(FData);
end;

function TCefZipReaderRef.GetFileName: ustring;
begin
  Result := TWACef.StringFreeAndGet(PCefZipReader(FData)^.get_file_name(FData));
end;

function TCefZipReaderRef.GetFileSize: cint64;
begin
  Result := PCefZipReader(FData)^.get_file_size(FData);
end;

function TCefZipReaderRef.MoveToFile(const fileName: ustring;
  caseSensitive: Boolean): Boolean;
var
  f: TCefString;
begin
  f := TWACef.ToCefString(fileName);
  Result := PCefZipReader(FData)^.move_to_file(FData, @f, Ord(caseSensitive)) <> 0;
end;

function TCefZipReaderRef.MoveToFirstFile: Boolean;
begin
  Result := PCefZipReader(FData)^.move_to_first_file(FData) <> 0;
end;

function TCefZipReaderRef.MoveToNextFile: Boolean;
begin
  Result := PCefZipReader(FData)^.move_to_next_file(FData) <> 0;
end;

function TCefZipReaderRef.OpenFile(const password: ustring): Boolean;
var
  p: TCefString;
begin
  p := TWACef.ToCefString(password);
  Result := PCefZipReader(FData)^.open_file(FData, @p) <> 0;
end;

function TCefZipReaderRef.ReadFile(buffer: Pointer;
  bufferSize: csize_t): cint;
begin
    Result := PCefZipReader(FData)^.read_file(FData, buffer, buffersize);
end;

function TCefZipReaderRef.Tell: cint64;
begin
  Result := PCefZipReader(FData)^.tell(FData);
end;

class function TCefZipReaderRef.UnWrap(data: Pointer): ICefZipReader;
begin
  if data <> nil then
    Result := Create(data) as ICefZipReader else
    Result := nil;
end;

//..............................................................................TCefResponseRef

class function TCefResponseRef.New: ICefResponse;
begin
  Result := UnWrap(cef_response_create());
end;

function TCefResponseRef.GetHeader(const name: ustring): ustring;
var
  n: TCefString;
begin
  n := TWACef.ToCefString(name);
  Result := TWACef.StringFreeAndGet(PCefResponse(FData)^.get_header(PCefResponse(FData), @n));
end;

procedure TCefResponseRef.GetHeaderMap(const headerMap: ICefStringMultimap);
begin
  PCefResponse(FData)^.get_header_map(PCefResponse(FData), headermap.Handle);
end;

function TCefResponseRef.GetMimeType: ustring;
begin
  Result := TWACef.StringFreeAndGet(PCefResponse(FData)^.get_mime_type(PCefResponse(FData)));
end;

function TCefResponseRef.GetStatus: cint;
begin
  Result := PCefResponse(FData)^.get_status(PCefResponse(FData));
end;

function TCefResponseRef.GetStatusText: ustring;
begin
  Result := TWACef.StringFreeAndGet(PCefResponse(FData)^.get_status_text(PCefResponse(FData)));
end;

function TCefResponseRef.IsReadOnly: Boolean;
begin
  Result := PCefResponse(FData)^.is_read_only(PCefResponse(FData)) <> 0;
end;

procedure TCefResponseRef.SetHeaderMap(const headerMap: ICefStringMultimap);
begin
  PCefResponse(FData)^.set_header_map(PCefResponse(FData), headerMap.Handle);
end;

procedure TCefResponseRef.SetMimeType(const mimetype: ustring);
var
  txt: TCefString;
begin
  txt := TWACef.ToCefString(mimetype);
  PCefResponse(FData)^.set_mime_type(PCefResponse(FData), @txt);
end;

procedure TCefResponseRef.SetStatus(status: cint);
begin
  PCefResponse(FData)^.set_status(PCefResponse(FData), status);
end;

procedure TCefResponseRef.SetStatusText(const StatusText: ustring);
var
  txt: TCefString;
begin
  txt := TWACef.ToCefString(StatusText);
  PCefResponse(FData)^.set_status_text(PCefResponse(FData), @txt);
end;

class function TCefResponseRef.UnWrap(data: Pointer): ICefResponse;
begin
  if data <> nil then
    Result := Create(data) as ICefResponse else
    Result := nil;
end;


//..............................................................................TCefCookieManagerRef

function TCefCookieManagerRef.DeleteCookies(const url,
  cookieName: ustring): Boolean;
var
  u, n: TCefString;
begin
  u := TWACef.ToCefString(url);
  n := TWACef.ToCefString(cookieName);
  Result := PCefCookieManager(FData)^.delete_cookies(
    PCefCookieManager(FData), @u, @n) <> 0;
end;

class function TCefCookieManagerRef.Global: ICefCookieManager;
begin
  Result := UnWrap(cef_cookie_manager_get_global_manager());
end;

function TCefCookieManagerRef.SetCookie(const url, name, value, domain,
  path: ustring; secure, httponly, hasExpires: Boolean; const creation,
  lastAccess, expires: TDateTime): Boolean;
var
  str: TCefString;
  cook: TCefCookie;
begin
  str := TWACef.ToCefString(url);
  cook.name := TWACef.ToCefString(name);
  cook.value := TWACef.ToCefString(value);
  cook.domain := TWACef.ToCefString(domain);
  cook.path := TWACef.ToCefString(path);
  cook.secure := Ord(secure);
  cook.httponly := Ord(httponly);
  cook.creation := TWACef.DateTimeToCefTime(creation);
  cook.last_access := TWACef.DateTimeToCefTime(lastAccess);
  cook.has_expires := Ord(hasExpires);
  if hasExpires then
    cook.expires := TWACef.DateTimeToCefTime(expires) else
    FillChar(cook.expires, SizeOf(TCefTime), 0);
  Result := PCefCookieManager(FData)^.set_cookie(
    PCefCookieManager(FData), @str, @cook) <> 0;
end;

function TCefCookieManagerRef.SetStoragePath(const path: ustring; PersistSessionCookies: Boolean): Boolean;
var
  p: TCefString;
  psc: cint;
begin
  p:=TWACef.ToCefString(path);
  if PersistSessionCookies=true then
    psc:=1 else
    psc:=0;
  if path<>'' then
    Result:=PCefCookieManager(FData)^.set_storage_path(PCefCookieManager(FData),@p,psc)<>0 else
    Result:=PCefCookieManager(FData)^.set_storage_path(PCefCookieManager(FData),nil,psc)<>0;
end;

function TCefCookieManagerRef.FlushStore(callback: ICefCompletionCallback): Boolean;
begin
  Result:=PCefCookieManager(FData)^.flush_store(PCefCookieManager(FData),PCefCompletionCallback(callback))<>0;
end;

procedure TCefCookieManagerRef.SetSupportedSchemes(schemes: TStrings);
var
  list: TCefStringList;
  i: cint;
  item: TCefString;
begin
  list := cef_string_list_alloc();
  try
    if (schemes <> nil) then
      for i := 0 to schemes.Count - 1 do
      begin
        item := TWACef.ToCefString(schemes[i]);
        cef_string_list_append(list, @item);
      end;
    PCefCookieManager(FData)^.set_supported_schemes(
      PCefCookieManager(FData), list);
  finally
    cef_string_list_free(list);
  end;
end;

class function TCefCookieManagerRef.UnWrap(data: Pointer): ICefCookieManager;
begin
  if data <> nil then
    Result := Create(data) as ICefCookieManager else
    Result := nil;
end;

function TCefCookieManagerRef.VisitAllCookies(
  const visitor: ICefCookieVisitor): Boolean;
begin
  Result := PCefCookieManager(FData)^.visit_all_cookies(
    PCefCookieManager(FData), TWACef.GetData(visitor)) <> 0;
end;

function TCefCookieManagerRef.VisitAllCookiesProc(
  const visitor: TCefCookieVisitorProc): Boolean;
var
  visit: ICefCookieVisitor;
begin
  visit:=TCefFastCookieVisitor.Create(visitor);
  Result := VisitAllCookies(visit);
end;

function TCefCookieManagerRef.VisitUrlCookies(const url: ustring;
  includeHttpOnly: Boolean; const visitor: ICefCookieVisitor): Boolean;
var
  str: TCefString;
begin
  str := TWACef.ToCefString(url);
  Result := PCefCookieManager(FData)^.visit_url_cookies(PCefCookieManager(FData), @str, Ord(includeHttpOnly), TWACef.GetData(visitor)) <> 0;
end;

function TCefCookieManagerRef.VisitUrlCookiesProc(const url: ustring;
  includeHttpOnly: Boolean; const visitor: TCefCookieVisitorProc): Boolean;
begin
  Result := VisitUrlCookies(url, includeHttpOnly,
    TCefFastCookieVisitor.Create(visitor) as ICefCookieVisitor);
end;

//..............................................................................TCefWebPluginInfoRef

function TCefWebPluginInfoRef.GetDescription: ustring;
begin
  Result := TWACef.StringFreeAndGet(PCefWebPluginInfo(FData)^.get_description(PCefWebPluginInfo(FData)));
end;

function TCefWebPluginInfoRef.GetName: ustring;
begin
  Result := TWACef.StringFreeAndGet(PCefWebPluginInfo(FData)^.get_name(PCefWebPluginInfo(FData)));
end;

function TCefWebPluginInfoRef.GetPath: ustring;
begin
  Result := TWACef.StringFreeAndGet(PCefWebPluginInfo(FData)^.get_path(PCefWebPluginInfo(FData)));
end;

function TCefWebPluginInfoRef.GetVersion: ustring;
begin
  Result := TWACef.StringFreeAndGet(PCefWebPluginInfo(FData)^.get_version(PCefWebPluginInfo(FData)));
end;

class function TCefWebPluginInfoRef.UnWrap(data: Pointer): ICefWebPluginInfo;
begin
  if data <> nil then
    Result := Create(data) as ICefWebPluginInfo else
    Result := nil;
end;

//..............................................................................TCefProcessMessageRef
function TCefProcessMessageRef.Copy: ICefProcessMessage;
begin
  Result := UnWrap(PCefProcessMessage(FData)^.copy(PCefProcessMessage(FData)));
end;

function TCefProcessMessageRef.GetArgumentList: ICefListValue;
begin
  Result := TCefListValueRef.UnWrap(PCefProcessMessage(FData)^.get_argument_list(PCefProcessMessage(FData)));
end;

function TCefProcessMessageRef.GetName: ustring;
begin
  Result := TWACef.StringFreeAndGet(PCefProcessMessage(FData)^.get_name(PCefProcessMessage(FData)));
end;

function TCefProcessMessageRef.IsReadOnly: Boolean;
begin
  Result := PCefProcessMessage(FData)^.is_read_only(PCefProcessMessage(FData)) <> 0;
end;

function TCefProcessMessageRef.IsValid: Boolean;
begin
  Result := PCefProcessMessage(FData)^.is_valid(PCefProcessMessage(FData)) <> 0;
end;

class function TCefProcessMessageRef.New(const name: ustring): ICefProcessMessage;
var
  n: TCefString;
begin
  n := TWACef.ToCefString(name);
  Result := UnWrap(cef_process_message_create(@n));
end;

class function TCefProcessMessageRef.UnWrap(data: Pointer): ICefProcessMessage;
begin
  if data <> nil then
    Result := Create(data) as ICefProcessMessage else
    Result := nil;
end;

//..............................................................................TCefPrintSettings
{Protected section}
function TCefPrintSettingsRef.IsValid: Boolean;
begin
  Result := PCefPrintSettings(FData)^.is_valid(PCefPrintSettings(FData)) <> 0;
end;

function TCefPrintSettingsRef.IsReadOnly: Boolean;
begin
  Result := PCefPrintSettings(FData)^.is_read_only(PCefPrintSettings(FData)) <> 0;
end;

function TCefPrintSettingsRef.Copy: ICefPrintSettings;
begin
  Result := UnWrap(PCefPrintSettings(FData)^.copy(PCefPrintSettings(FData)));
end;

procedure TCefPrintSettingsRef.SetOrientation(Landscape: Boolean);
begin
  PCefPrintSettings(FData)^.set_orientation(PCefPrintSettings(FData), Ord(Landscape));
end;

function TCefPrintSettingsRef.IsLandscape: Boolean;
begin
  Result := PCefPrintSettings(FData)^.is_landscape(PCefPrintSettings(FData)) <> 0;
end;

procedure TCefPrintSettingsRef.SetPrinterPrintableArea(const PhysicalSizeDeviceUnits: TCefSize; const PrintableAreaDeviceUnits: TCefRect; LandscapeNeedsFlip: Boolean);
begin
  PCefPrintSettings(FData)^.set_printer_printable_area(
    PCefPrintSettings(FData),
    @PhysicalSizeDeviceUnits,
    @PrintableAreaDeviceUnits,
    Ord(LandscapeNeedsFlip)
  );
end;

procedure TCefPrintSettingsRef.SetDeviceName(const Name: ustring);
var
  n: TCefString;
begin
  n := TWACef.ToCefString(Name);
  PCefPrintSettings(FData)^.set_device_name(PCefPrintSettings(FData), @n);
end;

function TCefPrintSettingsRef.GetDeviceName: ustring;
begin
  Result := TWACef.StringFreeAndGet(PCefPrintSettings(FData)^.get_device_name(PCefPrintSettings(FData)));
end;

procedure TCefPrintSettingsRef.SetDpi(Dpi: CInt);
begin
  PCefPrintSettings(FData)^.set_dpi(PCefPrintSettings(FData), Dpi);
end;

function TCefPrintSettingsRef.GetDpi: CInt;
begin
  Result := PCefPrintSettings(FData)^.get_dpi(PCefPrintSettings(FData));
end;

procedure TCefPrintSettingsRef.SetPageRanges(RangesCount: csize_t; const Ranges: PCefPageRangeArray);
begin
  PCefPrintSettings(FData)^.set_page_ranges(PCefPrintSettings(FData), RangesCount, Ranges);
end;

function TCefPrintSettingsRef.GetPageRangesCount: csize_t;
begin
  Result := PCefPrintSettings(FData)^.get_page_ranges_count(PCefPrintSettings(FData));
end;

procedure TCefPrintSettingsRef.GetPageRanges(RangesCount: csize_t; Ranges: PCefPageRangeArray);
begin
  PCefPrintSettings(FData)^.get_page_ranges(PCefPrintSettings(FData), @RangesCount, Ranges);
end;

procedure TCefPrintSettingsRef.SetSelectionOnly(SelectionOnly: Boolean);
begin
  PCefPrintSettings(FData)^.set_selection_only(PCefPrintSettings(FData), Ord(SelectionOnly));
end;

function TCefPrintSettingsRef.IsSelectionOnly: Boolean;
begin
  Result := PCefPrintSettings(FData)^.is_selection_only(PCefPrintSettings(FData)) <> 0;
end;

procedure TCefPrintSettingsRef.SetCollate(Collate: Boolean);
begin
  PCefPrintSettings(FData)^.set_collate(PCefPrintSettings(FData), Ord(Collate));
end;

function TCefPrintSettingsRef.WillCollate: Boolean;
begin
  Result := PCefPrintSettings(FData)^.will_collate(PCefPrintSettings(FData)) <> 0;
end;

procedure TCefPrintSettingsRef.SetColorModel(Model: TCefColorModel);
begin
  PCefPrintSettings(FData)^.set_color_model(PCefPrintSettings(FData), Model);
end;

function TCefPrintSettingsRef.GetColorModel: TCefColorModel;
begin
  Result := PCefPrintSettings(FData)^.get_color_model(PCefPrintSettings(FData));
end;

procedure TCefPrintSettingsRef.SetCopies(Copies: CInt);
begin
  PCefPrintSettings(FData)^.set_copies(PCefPrintSettings(FData), Copies);
end;

function TCefPrintSettingsRef.GetCopies: CInt;
begin
  Result := PCefPrintSettings(FData)^.get_copies(PCefPrintSettings(FData));
end;

procedure TCefPrintSettingsRef.SetDuplexMode(Mode: TCefDuplexMode);
begin
  PCefPrintSettings(FData)^.set_duplex_mode(PCefPrintSettings(FData), Mode);
end;

function TCefPrintSettingsRef.GetDuplexMode: TCefDuplexMode;
begin
  Result := PCefPrintSettings(FData)^.get_duplex_mode(PCefPrintSettings(FData));
end;

{Public section}
class function TCefPrintSettingsRef.UnWrap(data: Pointer): ICefPrintSettings;
begin
  if data <> nil then
    Result := Create(data) as ICefPrintSettings else
    Result := nil;
end;

class function TCefPrintSettingsRef.New(const name: ustring): ICefPrintSettings;
begin
//  Result := UnWrap(cef_print_settings_create());
end;

//..............................................................................TCefAuthCallbackRef

procedure TCefAuthCallbackRef.Cancel;
begin
  PCefAuthCallback(FData)^.cancel(PCefAuthCallback(FData));
end;

procedure TCefAuthCallbackRef.Cont(const username, password: ustring);
var
  u, p: TCefString;
begin
  u := TWACef.ToCefString(username);
  p := TWACef.ToCefString(password);
  PCefAuthCallback(FData)^.cont(PCefAuthCallback(FData), @u, @p);
end;

class function TCefAuthCallbackRef.UnWrap(data: Pointer): ICefAuthCallback;
begin
  if data <> nil then
    Result := Create(data) as ICefAuthCallback else
    Result := nil;
end;

//..............................................................................TCefCommandLineRef
function TCefCommandLineRef.Copy: ICefCommandLine;
begin
  Result := UnWrap(PCefCommandLine(FData)^.copy(PCefCommandLine(FData)));
end;

procedure TCefCommandLineRef.InitFromArgv(argc: cint;
  const argv: PPAnsiChar);
begin
  PCefCommandLine(FData)^.init_from_argv(PCefCommandLine(FData), argc, argv);
end;

procedure TCefCommandLineRef.InitFromString(const commandLine: ustring);
var
  cl: TCefString;
begin
  cl := TWACef.ToCefString(commandLine);
  PCefCommandLine(FData)^.init_from_string(PCefCommandLine(FData), @cl);
end;

procedure TCefCommandLineRef.Reset;
begin
  PCefCommandLine(FData)^.reset(PCefCommandLine(FData));
end;

function TCefCommandLineRef.IsReadOnly: Boolean;
begin
  Result := PCefCommandLine(FData)^.is_read_only(PCefCommandLine(FData)) <> 0;
end;

function TCefCommandLineRef.IsValid: Boolean;
begin
  Result := PCefCommandLine(FData)^.is_valid(PCefCommandLine(FData)) <> 0;
end;

class function TCefCommandLineRef.New: ICefCommandLine;
begin
  Result := UnWrap(cef_command_line_create());
end;

procedure TCefCommandLineRef.GetArgv(argv: TStrings);
var
  list: TCefStringList;
  i: cint;
  str: TCefString;
begin
  list := cef_string_list_alloc();
  try
    PCefCommandLine(FData)^.get_argv(PCefCommandLine(FData), list);
    FillChar(str, SizeOf(str), 0);
    for i := 0 to cef_string_list_size(list) - 1 do
    begin
      cef_string_list_value(list, i, @str);
      argv.Add(TWACef.StringClearAndGet(str));
    end;
  finally
    cef_string_list_free(list);
  end;
end;

function TCefCommandLineRef.GetCommandLineString: ustring;
begin
  Result := TWACef.StringFreeAndGet(PCefCommandLine(FData)^.get_command_line_string(PCefCommandLine(FData)));
end;

function TCefCommandLineRef.GetProgram: ustring;
begin
  Result := TWACef.StringFreeAndGet(PCefCommandLine(FData)^.get_program(PCefCommandLine(FData)));
end;

procedure TCefCommandLineRef.SetProgram(const _program: ustring);
var
  p: TCefString;
begin
  p := TWACef.ToCefString(_program);
  PCefCommandLine(FData)^.set_program(PCefCommandLine(FData), @p)
end;

function TCefCommandLineRef.HasSwitches: boolean;
begin
  result := PCefCommandLine(FData)^.has_switches(PCefCommandLine(FData))<>0;
end;

function TCefCommandLineRef.HasSwitch(const name: ustring): boolean;
var
  n: TCefString;
begin
  n := TWACef.ToCefString(name);
  result := PCefCommandLine(FData)^.has_switch(PCefCommandLine(FData), @n)<>0;
end;

function TCefCommandLineRef.GetSwitchValue(const name: ustring): ustring;
var
  n: TCefString;
begin
  n := TWACef.ToCefString(name);
  result := TWACef.StringFreeAndGet(PCefCommandLine(FData)^.get_switch_value(PCefCommandLine(FData), @n));
end;

procedure TCefCommandLineRef.GetSwitches(switches: ICefStringMap);
begin
  PCefCommandLine(FData)^.get_switches(PCefCommandLine(FData), switches.Handle);
end;

procedure TCefCommandLineRef.AppendSwitch(const name: ustring);
var
  n: TCefString;
begin
  n := TWACef.ToCefString(name);
  PCefCommandLine(FData)^.append_switch(PCefCommandLine(FData), @n);
end;

procedure TCefCommandLineRef.AppendSwitchWithValue(const name: ustring; const value: ustring);
var
  n: TCefString;
  v: TCefString;
begin
  n := TWACef.ToCefString(name);
  v := TWACef.ToCefString(value);
  PCefCommandLine(FData)^.append_switch_with_value(PCefCommandLine(FData), @n, @v);
end;

function TCefCommandLineRef.HasArguments: boolean;
begin
  result := PCefCommandLine(FData)^.has_arguments(PCefCommandLine(FData))<>0;
end;

procedure TCefCommandLineRef.GetArguments(arguments: TStrings);
var
  list: TCefStringList;
  i: cint;
  str: TCefString;
begin
  list := cef_string_list_alloc();
  try
    PCefCommandLine(FData)^.get_arguments(PCefCommandLine(FData), list);
    FillChar(str, SizeOf(str), 0);
    for i := 0 to cef_string_list_size(list) - 1 do
    begin
      cef_string_list_value(list, i, @str);
      arguments.Add(TWACef.StringClearAndGet(str));
    end;
  finally
    cef_string_list_free(list);
  end;
end;

procedure TCefCommandLineRef.AppendArgument(const argument: ustring);
var
  a: TCefString;
begin
  a := TWACef.ToCefString(argument);
  PCefCommandLine(FData)^.append_argument(PCefCommandLine(FData), @a);
end;

procedure TCefCommandLineRef.PrependWrapper(const wrapper: ustring);
var
  w: TCefString;
begin
  w := TWACef.ToCefString(wrapper);
  PCefCommandLine(FData)^.prepend_wrapper(PCefCommandLine(FData), @w);
end;

class function TCefCommandLineRef.UnWrap(data: Pointer): ICefCommandLine;
begin
  if data <> nil then
    Result := Create(data) as ICefCommandLine else
    Result := nil;
end;


//..............................................................................TCefSchemeRegistrarRef

function TCefSchemeRegistrarRef.AddCustomScheme(const schemeName: ustring;
  IsStandard, IsLocal, IsDisplayIsolated: Boolean): Boolean; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
var
  sn: TCefString;
begin
  sn := TWACef.ToCefString(schemeName);
  Result := PCefSchemeRegistrar(FData)^.add_custom_scheme(PCefSchemeRegistrar(FData),
    @sn, Ord(IsStandard), Ord(IsLocal), Ord(IsDisplayIsolated)) <> 0;
end;

class function TCefSchemeRegistrarRef.UnWrap(
  data: Pointer): ICefSchemeRegistrar;
begin
  if data <> nil then
    Result := Create(data) as ICefSchemeRegistrar else
    Result := nil;
end;

//..............................................................................TCefUrlRequestClientRef
//Protected section
procedure TCefUrlRequestClientRef.OnRequestComplete(const  request: ICefUrlRequest);
begin
  PCefUrlRequestClient(FData)^.on_request_complete(
    PCefUrlRequestClient(FData),
    TWACef.GetData(request)
  );
end;

procedure TCefUrlRequestClientRef.OnUploadProgress(const  request: ICefUrlRequest; current, total: UInt64);
begin
  PCefUrlRequestClient(FData)^.on_upload_progress(
    PCefUrlRequestClient(FData),
    TWACef.GetData(request),
    current,
    total
  );
end;

procedure TCefUrlRequestClientRef.OnDownloadProgress(const  request: ICefUrlRequest; current, total: UInt64);
begin
  PCefUrlRequestClient(FData)^.on_download_progress(
    PCefUrlRequestClient(FData),
    TWACef.GetData(request),
    current,
    total
  );
end;

procedure TCefUrlRequestClientRef.OnDownloadData(const  request: ICefUrlRequest; data: Pointer; dataLength: csize_t);
begin
  PCefUrlRequestClient(FData)^.on_download_data(
    PCefUrlRequestClient(FData),
    TWACef.GetData(request),
    data,
    dataLength
  );
end;

function TCefUrlRequestClientRef.GetAuthCredentials(IsProxy: Boolean; const Host: ustring; Port: CInt; const Realm: ustring; const Scheme: ustring; Callback: ICefAuthCallback): Boolean;
var
  h, r, s: TCefString;
begin
  h := TWACef.ToCefString(Host);
  r := TWACef.ToCefString(Realm);
  s := TWACef.ToCefString(Scheme);
  Result := PCefUrlRequestClient(FData)^.get_auth_credentials(
    PCefUrlRequestClient(FData),
    Ord(IsProxy),
    @h,
    Port,
    @r,
    @s,
    TWACef.GetData(Callback)
  ) <> 0;
end;

//Public section
class function TCefUrlRequestClientRef.UnWrap(data: Pointer): ICefUrlRequestClient;
begin
  if data <> nil then
    Result := Create(data) as ICefUrlRequestClient else
    Result := nil;
end;

//..............................................................................TCefUrlRequestRef
procedure TCefUrlRequestRef.Cancel;
begin
  PCefUrlRequest(FData)^.cancel(PCefUrlRequest(FData));
end;

class function TCefUrlRequestRef.New(const request: ICefRequest;
  const client: ICefUrlRequestClient): ICefUrlRequest;
begin
  Result := UnWrap(cef_urlrequest_create(TWACef.GetData(request), TWACef.GetData(client)));
end;

function TCefUrlRequestRef.GetRequest: ICefRequest;
begin
  Result := TCefRequestRef.UnWrap(PCefUrlRequest(FData)^.get_request(PCefUrlRequest(FData)));
end;

function TCefUrlRequestRef.GetClient: ICefUrlRequestClient;
begin
  Result := TCefUrlRequestClientRef.Unwrap(PCefUrlRequest(FData)^.get_client(PCefUrlRequest(FData)));
end;

function TCefUrlRequestRef.GetRequestError: TCefErrorcode;
begin
  Result := PCefUrlRequest(FData)^.get_request_error(PCefUrlRequest(FData));
end;

function TCefUrlRequestRef.GetRequestStatus: TCefUrlRequestStatus;
begin
  Result := PCefUrlRequest(FData)^.get_request_status(PCefUrlRequest(FData));
end;

function TCefUrlRequestRef.GetResponse: ICefResponse;
begin
  Result := TCefResponseRef.UnWrap(PCefUrlRequest(FData)^.get_response(PCefUrlRequest(FData)));
end;

class function TCefUrlRequestRef.UnWrap(data: Pointer): ICefUrlRequest;
begin
  if data <> nil then
    Result := Create(data) as ICefUrlRequest else
    Result := nil;
end;

//..............................................................................TCefTaskRunnerRef
function TCefTaskRunnerRef.IsSame(that: ICefTaskRunner): Boolean;
begin
  Result:=PCefTaskRunner(FData)^.is_same(PCefTaskRunner(FData),TWACef.GetData(that))<>0;
end;

function TCefTaskRunnerRef.BelongsToCurrentThread: Boolean;
begin
  Result:=PCefTaskRunner(FData)^.belongs_to_current_thread(PCefTaskRunner(FData))<>0;
end;

function TCefTaskRunnerRef.BelongsToThread(ThreadID: TCefThreadId): Boolean;
begin
  Result:=PCefTaskRunner(FData)^.belongs_to_thread(PCefTaskRunner(FData),ThreadID)<>0;
end;

function TCefTaskRunnerRef.PostTask(task: ICefTask): cint;
begin
  Result:=PCefTaskRunner(FData)^.post_task(PCefTaskRunner(FData),TWACef.GetData(task));
end;

function TCefTaskRunnerRef.PostDelayedTask(task: ICefTask; DelayMS: cint64): cint;
begin
  Result:=PCefTaskRunner(FData)^.post_delayed_task(PCefTaskRunner(FData),TWACef.GetData(task), DelayMS);
end;

class function TCefTaskRunnerRef.UnWrap(data: Pointer): ICefTaskRunner;
begin
  if data <> nil then
    Result := Create(data) as ICefTaskRunner else
    Result := nil;
end;

class function TCefTaskRunnerRef.GetForThread(const ThreadID: TCefThreadId): ICefTaskRunner;
begin
  Result:=UnWrap(cef_task_runner_get_for_thread(ThreadID));
end;

class function TCefTaskRunnerRef.GetForCurrentThread: ICefTaskRunner;
begin
  Result:=UnWrap(cef_task_runner_get_for_current_thread());
end;

//..............................................................................TCefTaskRef
procedure TCefTaskRef.Execute;
begin
  PCefTask(FData)^.execute(FData);
end;

class function TCefTaskRef.UnWrap(data: Pointer): ICefTask;
begin
  if data <> nil then
    Result := Create(data) as ICefTask else
    Result := nil;
end;

//..............................................................................TCefFileDialogCallbackRef
procedure TCefFileDialogCallbackRef.Cancel;
begin
  PCefFileDialogCallback(FData)^.cancel(FData);
end;

procedure TCefFileDialogCallbackRef.Cont(filePaths: TStrings);
var
  list: TCefStringList;
  i: cint;
  str: TCefString;
begin
  list := cef_string_list_alloc();
  try
    for i := 0 to filePaths.Count - 1 do
    begin
      str := TWACef.ToCefString(filePaths[i]);
      cef_string_list_append(list, @str);
    end;
    PCefFileDialogCallback(FData)^.cont(FData, list);
  finally
    cef_string_list_free(list);
  end;
end;

class function TCefFileDialogCallbackRef.UnWrap(
  data: Pointer): ICefFileDialogCallback;
begin
  if data <> nil then
    Result := Create(data) as ICefFileDialogCallback else
    Result := nil;
end;

//..............................................................................TCefAllowCertificateErrorCallbackRef
procedure TCefAllowCertificateErrorCallbackRef.Cont(allow: Boolean);
begin
  PCefAllowCertificateErrorCallback(FData)^.cont(FData,ord(allow));
end;

class function TCefAllowCertificateErrorCallbackRef.UnWrap(data: Pointer): ICefAllowCertificateErrorCallback;
begin
  if data <> nil then
    Result:=Create(data) as ICefAllowCertificateErrorCallback else
    Result:=nil;
end;

//..............................................................................TCefDragDataRef
//Protected section
function TCefDragDataRef.Clone: ICefDragData;
begin
  Result := TCefDragDataRef.UnWrap(PCefDragData(FData)^.clone(PCefDragData(FData)));
end;

function TCefDragDataRef.IsReadOnly: boolean;
begin
  Result := PCefDragData(FData)^.is_read_only(PCefdragData(FData))<>0;
end;

function TCefDragDataRef.IsLink: boolean;
begin
  Result := PCefDragData(FData)^.is_link(PCefdragData(FData))<>0;
end;

function TCefDragDataRef.IsFragment: boolean;
begin
  Result := PCefDragData(FData)^.is_fragment(PCefDragData(FData))<>0;
end;

function TCefDragDataRef.IsFile: boolean;
begin
  Result := PCefDragData(FData)^.is_file(PCefDragData(FData))<>0;
end;

function TCefDragDataRef.GetLinkUrl: ustring;
begin
  Result := TWACef.StringFreeAndGet(PCefDragData(FData)^.get_link_url(PCefDragData(FData)));
end;

function TCefDragDataRef.GetLinkTitle: ustring;
begin
  Result := TWACef.StringFreeAndGet(PCefDragData(FData)^.get_link_title(PCefDragData(FData)));
end;

function TCefDragDataRef.GetLinkMetadata: ustring;
begin
  Result := TWACef.StringFreeAndGet(PCefDragData(FData)^.get_link_metadata(PCefDragData(FData)));
end;

function TCefDragDataRef.GetFragmentText: ustring;
begin
  Result := TWACef.StringFreeAndGet(PCefDragData(FData)^.get_fragment_text(PCefDragData(FData)));
end;

function TCefDragDataRef.GetFragmentHTML: ustring;
begin
  Result := TWACef.StringFreeAndGet(PCefDragData(FData)^.get_fragment_html(PCefDragData(FData)));
end;

function TCefDragDataRef.GetFragmentBaseURL: ustring;
begin
  Result := TWACef.StringFreeAndGet(PCefDragData(FData)^.get_fragment_base_url(PCefDragData(FData)));
end;

function TCefDragDataRef.GetFileName: ustring;
begin
  Result := TWACef.StringFreeAndGet(PCefDragData(FData)^.get_file_name(PCefDragData(FData)));
end;

function TCefDragDataRef.GetFileContents(const writer: ICefStreamWriter): csize_t;
begin
  Result := PCefDragData(FData)^.get_file_contents(PCefDragData(FData), TWACef.GetData(writer));
end;

function TCefDragDataRef.GetFileNames(names: TStrings): Boolean;
var
  list: TCefStringList;
  i: cint;
  str: TCefString;
begin
  list:=cef_string_list_alloc();
  try
    Result:=PCefDragData(FData)^.get_file_names(PCefDragData(FData),list)<>0;
    FillChar(str,SizeOf(str),0);
    for i:=0 to cef_string_list_size(list)-1 do
    begin
      cef_string_list_value(list,i,@str);
      names.Add(TWACef.StringClearAndGet(str));
    end;
  finally
    cef_string_list_free(list);
  end;
end;

procedure TCefDragDataRef.SetLinkURL(const url: ustring);
var
  u: TCefString;
begin
  u := TWACef.ToCefString(url);
  PCefDragData(FData)^.set_link_url(PCefDragData(FData), @u);
end;

procedure TCefDragDataRef.SetLinkTitle(const title: ustring);
var
  u: TCefString;
begin
  u := TWACef.ToCefString(title);
  PCefDragData(FData)^.set_link_title(PCefDragData(FData), @u);
end;

procedure TCefDragDataRef.SetLinkMetadata(const data: ustring);
var
  u: TCefString;
begin
  u := TWACef.ToCefString(data);
  PCefDragData(FData)^.set_link_metadata(PCefDragData(FData), @u);
end;

procedure TCefDragDataRef.SetFragmentText(const text: ustring);
var
  u: TCefString;
begin
  u := TWACef.ToCefString(text);
  PCefDragData(FData)^.set_fragment_text(PCefDragData(FData), @u);
end;

procedure TCefDragDataRef.SetFragmentHTML(const HTML: ustring);
var
  u: TCefString;
begin
  u := TWACef.ToCefString(HTML);
  PCefDragData(FData)^.set_fragment_html(PCefDragData(FData), @u);
end;

procedure TCefDragDataRef.SetFragmentBaseURL(const BaseURL: ustring);
var
  u: TCefString;
begin
  u := TWACef.ToCefString(BaseURL);
  PCefDragData(FData)^.set_fragment_base_url(PCefDragData(FData), @u);
end;

procedure TCefDragDataRef.ResetFileContents;
begin
  PCefDragData(FData)^.reset_file_contents(PCefDragData(FData));
end;

procedure TCefDragDataRef.AddFile(const path: ustring; const DisplayName: ustring);
var
  u: TCefString;
  p: TCefString;
begin
  p := TWACef.ToCefString(path);
  u := TWACef.ToCefString(DisplayName);
  PCefDragData(FData)^.add_file(PCefDragData(FData), @p, @u);
end;

//Public section
class function TCefDragDataRef.UnWrap(data: Pointer): ICefDragData;
begin
  if data<>nil then
    Result:=Create(data) as ICefDragData else
    Result:=nil;
end;

class function TCefDragDataRef.New: ICefDragData;
begin
//  Result := UnWrap(cef_drag_data_create());
end;

end.