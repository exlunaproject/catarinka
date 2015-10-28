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
  ICefApp = interface;
  ICefAuthCallback = interface;
  ICefBrowser=interface;
  ICefRunFileDialogCallback = interface;
  ICefBrowserHost=interface;
  ICefBrowserProcessHandler = interface;
  ICefCallback = interface;
  ICefCompletionCallback = interface;
  ICefClient = interface;
  ICefCommandLine = interface;
  ICefContextMenuHandler = interface;
  ICefContextMenuParams = interface;
  ICefCookieManager = Interface;
  ICefCookieVisitor = interface;
  ICefFileDialogCallBack = interface;
  ICefDialogHandler = interface;
  ICefDisplayHandler = interface;
  ICefDomVisitor = interface;
  ICefDomDocument = interface;
  ICefDomNode = interface;
  ICefBeforeDownloadCallback = interface;
  ICefDownloadItemCallback = interface;
  ICefDownloadHandler = interface;
  ICefDownloadItem = interface;
  ICefDragData = interface;
  ICefDragHandler = interface;
  ICefFocusHandler = interface;
  ICefFrame=interface;
  ICefGetGeolocationCallback = interface;
  ICefGeolocationCallback = interface;
  ICefGeolocationHandler = interface;
  ICefJsDialogCallback = interface;
  ICefJsDialogHandler = interface;
  ICefKeyboardHandler = interface;
  ICefLifeSpanHandler = interface;
  ICefLoadHandler = interface;
  ICefMenuModel = interface;
  ICefPrintDialogCallback = interface;
  ICefPrintJobCallback = interface;
  ICefPrintHandler = interface;
  ICefProcessMessage=interface;
  ICefRenderHandler = interface;
  ICefRenderProcessHandler = interface;
  ICefRequest=interface;
  ICefPostData=interface;
  ICefPostDataElement = interface;
  ICefRequestContext = interface;
  ICefRequestContextHandler = interface;
  ICefQuotaCallback = interface;
  ICefAllowCertificateErrorCallback = interface;
  ICefRequestHandler = interface;
  ICefResourceBundleHandler = interface;
  ICefResourceHandler = interface;
  ICefResponse = interface;
  ICefSchemeRegistrar = interface;
  ICefSchemeHandlerFactory = interface;
  ICefReadHandler = interface;
  ICefStreamReader = interface;
  ICefWriteHandler = interface;
  ICefStreamWriter = interface;
  ICefStringVisitor=interface;
  ICefTask = interface;
  ICefTaskRunner = interface;
  ICefEndTracingCallback = interface;
  ICefUrlRequest = interface;
  ICefUrlrequestClient = interface;
  ICefv8Context = interface;
  ICefv8Handler = interface;
  ICefV8Accessor = interface;
  ICefV8Exception = interface;
  ICefv8Value = interface;
  TCefv8ValueArray = array of ICefv8Value;
  ICefV8StackTrace = interface;
  ICefV8StackFrame = interface;
  ICefBinaryValue = interface;
  ICefDictionaryValue = interface;
  ICefListValue = interface;
  ICefWebPluginInfo = interface;
  ICefWebPluginInfoVisitor = interface;
  ICefWebPluginUnstableCallback = interface;
  ICefXmlReader = interface;
  ICefZipReader = interface;

  ICefStringMap = interface;
  ICefStringMultiMap = interface;
  ICefCustomStreamReader = interface;

  TCefDomVisitorProc = {$IFNDEF FPC}reference to {$ENDIF} procedure(const  document: ICefDomDocument);
  TCefStringVisitorProc = {$IFNDEF FPC}reference to {$ENDIF} procedure(const  str: ustring);
  TCefCookieVisitorProc = {$IFNDEF FPC}reference to {$ENDIF} function(const  cookie:TWACefCookie; count, total: cint; out deleteCookie: Boolean): Boolean;
  TOnRegisterCustomSchemes = {$IFNDEF FPC}reference to {$ENDIF} procedure(const  registrar: ICefSchemeRegistrar);
  TOnBeforeCommandLineProcessing = {$IFNDEF FPC}reference to {$ENDIF} procedure(const  processType: ustring; const  commandLine: ICefCommandLine);
  TCefWebPluginInfoVisitorProc = {$IFNDEF FPC}reference to {$ENDIF} function(const  info: ICefWebPluginInfo; count, total: cint): Boolean;
  TCefV8AccessorGetterProc = {$IFNDEF FPC}reference to {$ENDIF} function(const  name: ustring; const  obj: ICefv8Value; out value: ICefv8Value; const  exception: ustring): Boolean;
  TCefV8AccessorSetterProc = {$IFNDEF FPC}reference to {$ENDIF} function(const  name: ustring; const  obj, value: ICefv8Value; const  exception: ustring): Boolean;
  TCefRunFileDialogCallbackProc = {$IFNDEF FPC}reference to {$ENDIF} procedure(const  browserHost: ICefBrowserHost; filePaths: TStrings);


  ICefBase = interface
    ['{1F9A7B44-DCDC-4477-9180-3ADD44BDEB7B}']
    function Wrap: Pointer;
  end;

  //............................................................................cef_app_capi
  ICefApp = interface(ICefBase)
    ['{970CA670-9070-4642-B188-7D8A22DAEED4}']
    procedure OnBeforeCommandLineProcessing(const  processType: ustring;
      const  commandLine: ICefCommandLine);
    procedure OnRegisterCustomSchemes(const  registrar: ICefSchemeRegistrar);
    function GetResourceBundleHandler: ICefResourceBundleHandler;
    function GetBrowserProcessHandler: ICefBrowserProcessHandler;
    function GetRenderProcessHandler: ICefRenderProcessHandler;
  end;

  //............................................................................cef_auth_callback_capi
  ICefAuthCallback = interface(ICefBase)
  ['{500C2023-BF4D-4FF7-9C04-165E5C389131}']
    procedure Cont(const  username, password: ustring);
    procedure Cancel;
  end;

  //............................................................................cef_browser_capi
  ICefBrowser = interface(ICefBase)
  ['{BA003C2E-CF15-458F-9D4A-FE3CEFCF3EEF}']
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
    function GetFrameByident(identifier: Int64): ICefFrame;
    function GetFrame(const  name: ustring): ICefFrame;
    function GetFrameCount: csize_t;
    procedure GetFrameIdentifiers(count: pcsize_t; identifiers: pcint64);
    procedure GetFrameNames(names: TStrings);
    function SendProcessMessage(targetProcess: TCefProcessId;
      message: ICefProcessMessage): Boolean;
    property MainFrame: ICefFrame read GetMainFrame;
    property FocusedFrame: ICefFrame read GetFocusedFrame;
    property FrameCount: csize_t read GetFrameCount;
    property Host: ICefBrowserHost read GetHost;
    property Identifier: cint read GetIdentifier;
  end;

  ICefRunFileDialogCallback = interface(ICefBase)
  ['{59FCECC6-E897-45BA-873B-F09586C4BE47}']
    procedure cont(const  browserHost: ICefBrowserHost; filePaths: TStrings);
  end;

  ICefBrowserHost = interface(ICefBase)
    ['{53AE02FF-EF5D-48C3-A43E-069DA9535424}']
    function GetBrowser: ICefBrowser;
    procedure CloseBrowser(aForceClose:boolean);
    procedure SetFocus(enable: Boolean);
    procedure SetWindowVisibility(visible: Boolean);
    function GetWindowHandle: TCefWindowHandle;
    function GetOpenerWindowHandle: TCefWindowHandle;
    function GetClient:ICefClient;
    function GetRequestContext:ICefRequestContext;
    function GetZoomLevel: Double;
    procedure SetZoomLevel(zoomLevel: Double);
    procedure RunFileDialog(mode:TCefFileDialogMode; const  title:ustring; const  DefaultFileName:ustring;
      AcceptTypes:TStrings; const callback:ICefRunFileDialogCallback);
    procedure StartDownload(const url:ustring);
    procedure Print;
    procedure Find(identifier: cint; const  searchText:ustring; fwd:Boolean; matchCase:Boolean; findNext:Boolean);
    procedure StopFinding(clearSelection:Boolean);
    procedure ShowDevTools(var windowInfo:TCefWindowInfo; const client:ICefClient; var settings:TCefBrowserSettings; InspectElementAt: TCefPoint); overload;
    procedure ShowDevTools(var windowInfo:TCefWindowInfo; const client:ICefClient; var settings:TCefBrowserSettings); overload;
    procedure CloseDevTools;
    procedure SetMouseCursorChangeDisabled(disabled:Boolean);
    function GetIsMouseCursorChangeDisabled:Boolean;
    procedure ReplaceMisspelling(const word: ustring);
    function GetIsWindowRenderingDisabled:boolean;
    procedure WasResized;
    procedure WasHidden(hidden:Boolean);
    procedure NotifyScreenInfoChanged;
    procedure Invalidate(const aType: TCefPaintElementType);
    procedure SendKeyEvent(const  event:TCefKeyEvent);
    procedure SendMouseClickEvent(const  event:TCefMouseEvent; aType:TCefMouseButtonType;
      mouseUp:boolean; clickCount:integer);
    procedure SendMouseMoveEvent(event:TCefMouseEvent; mouseLeave:boolean);
    procedure SendMouseWheelEvent(const  event:TCefMouseEvent; deltaX:integer; deltaY:integer);
    procedure SendFocusEvent(aSetFocus:integer);
    procedure SendCaptureLostEvent;
    {$IFDEF MACOS}
    function GetNSTextInputContext: TCefTextInputContext;
    procedure HandleKeyEventBeforeTextInputClient(keyEvent: TCefEventHandle);
    procedure HandleKeyEventAfterTextInputClient(keyEvent: TCefEventHandle);
    {$ENDIF}
    procedure DragTargetDragEnter(const DragData:ICefDragData; const event:TCefMouseEvent; AllowedOps: TCefDragOperationsMask);
		procedure DragTargetDragOver(const event:TCefMouseEvent; AllowedOps:TCefDragOperationsMask);
		procedure DragTargetDragLeave;
		procedure DragTargetDrop(const event:TCefMouseEvent);
		procedure DragSourceEndedAt(x: cint; y: cint; op:TCefDragOperationsMask);
		procedure DragSourceSystemDragEnded;

    property IsWindowRenderingDisabled:boolean read GetIsWindowRenderingDisabled;
    property Browser: ICefBrowser read GetBrowser;
    property WindowHandle: TCefWindowHandle read GetWindowHandle;
    property OpenerWindowHandle: TCefWindowHandle read GetOpenerWindowHandle;
    property ZoomLevel: Double read GetZoomLevel write SetZoomLevel;
    property MouseCursorChangeDisabled:boolean read GetIsMouseCursorChangeDisabled write SetMouseCursorchangeDisabled;
  end;

  //............................................................................cef_browser_process_handler_capi
  ICefBrowserProcessHandler = interface(ICefBase)
  ['{27291B7A-C0AE-4EE0-9115-15C810E22F6C}']
    procedure OnContextInitialized;
    procedure OnBeforeChildProcessLaunch(const  commandLine: ICefCommandLine);
    procedure OnRenderProcessThreadCreated(const ExtraInfo:ICefListValue);
    function GetPrintHandler: ICefPrintHandler;
  end;

  //............................................................................cef_callback_capi
  ICefCallback = interface(ICefBase)
  ['{1B8C449F-E2D6-4B78-9BBA-6F47E8BCDF37}']
    procedure Cont;
    procedure Cancel;
  end;

  ICefCompletionCallback = interface(ICefBase)
  ['{2257E023-AF5E-4251-B3CB-AD355D6A772C}']
    procedure OnComplete;
  end;

  //............................................................................cef_client_capi
  ICefClient = interface(ICefBase)
    ['{1D502075-2FF0-4E13-A112-9E541CD811F4}']
    function GetContextMenuHandler: ICefContextMenuHandler;
    function GetDialogHandler:ICefDialogHandler;
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
    function OnProcessMessageReceived(const  browser: ICefBrowser;
      sourceProcess: TCefProcessId; const  message: ICefProcessMessage): Boolean;
  end;

  //............................................................................cef_command_line_capi
  ICefCommandLine = interface(ICefBase)
  ['{6B43D21B-0F2C-4B94-B4E6-4AF0D7669D8E}']
    function IsValid: Boolean;
    function IsReadOnly: Boolean;
    function Copy: ICefCommandLine;
    procedure InitFromArgv(argc: cint; const  argv: PPAnsiChar);
    procedure InitFromString(const  commandLine: ustring);
    procedure Reset;
    procedure GetArgv(argv: TStrings);
    function GetCommandLineString:ustring;
    function GetProgram:ustring;
    procedure SetProgram(const _program: ustring);
    function HasSwitches:boolean;
    function HasSwitch(const name:ustring):boolean;
    function GetSwitchValue(const name:ustring):ustring;
    procedure GetSwitches(switches:ICefStringMap);
    procedure AppendSwitch(const name:ustring);
    procedure AppendSwitchWithValue(const name:ustring; const value:ustring);
    function HasArguments:boolean;
    procedure GetArguments(arguments:TStrings);
    procedure AppendArgument(const argument:ustring);
    procedure PrependWrapper(const wrapper:ustring);
  end;

  //............................................................................cef_context_menu_handler_capi
  ICefContextMenuHandler = interface(ICefBase)
  ['{C2951895-4087-49D5-BA18-4D9BA4F5EDD7}']
    procedure OnBeforeContextMenu(const  browser: ICefBrowser; const  frame: ICefFrame;
      const  params: ICefContextMenuParams; const  model: ICefMenuModel);
    function OnContextMenuCommand(const  browser: ICefBrowser; const  frame: ICefFrame;
      const  params: ICefContextMenuParams; commandId: cint;
      eventFlags: TCefEventFlags): Boolean;
    procedure OnContextMenuDismissed(const  browser: ICefBrowser; const  frame: ICefFrame);
  end;

  ICefContextMenuParams = interface(ICefBase)
  ['{E31BFA9E-D4E2-49B7-A05D-20018C8794EB}']
    function GetXCoord: cint;
    function GetYCoord: cint;
    function GetTypeFlags: TCefContextMenuTypeFlags;
    function GetLinkUrl: ustring;
    function GetUnfilteredLinkUrl: ustring;
    function GetSourceUrl: ustring;
    function HasImageContents: boolean;
    function GetPageUrl: ustring;
    function GetFrameUrl: ustring;
    function GetFrameCharset: ustring;
    function GetMediaType: TCefContextMenuMediaType;
    function GetMediaStateFlags: TCefContextMenuMediaStateFlags;
    function GetSelectionText: ustring;
    function IsEditable: Boolean;
    function GetEditStateFlags: TCefContextMenuEditStateFlags;

    property XCoord: cint read GetXCoord;
    property YCoord: cint read GetYCoord;
    property TypeFlags: TCefContextMenuTypeFlags read GetTypeFlags;
    property LinkUrl: ustring read GetLinkUrl;
    property UnfilteredLinkUrl: ustring read GetUnfilteredLinkUrl;
    property SourceUrl: ustring read GetSourceUrl;
    property PageUrl: ustring read GetPageUrl;
    property FrameUrl: ustring read GetFrameUrl;
    property FrameCharset: ustring read GetFrameCharset;
    property MediaType: TCefContextMenuMediaType read GetMediaType;
    property MediaStateFlags: TCefContextMenuMediaStateFlags read GetMediaStateFlags;
    property SelectionText: ustring read GetSelectionText;
    property EditStateFlags: TCefContextMenuEditStateFlags read GetEditStateFlags;
  end;

  //............................................................................cef_cookie_capi
  ICefCookieManager = Interface(ICefBase)
    ['{CC1749E6-9AD3-4283-8430-AF6CBF3E8785}']
    procedure SetSupportedSchemes(schemes: TStrings);
    function VisitAllCookies(const  visitor: ICefCookieVisitor): Boolean;
    function VisitAllCookiesProc(const  visitor: TCefCookieVisitorProc): Boolean;
    function VisitUrlCookies(const  url: ustring;
      includeHttpOnly: Boolean; const  visitor: ICefCookieVisitor): Boolean;
    function VisitUrlCookiesProc(const  url: ustring;
      includeHttpOnly: Boolean; const  visitor: TCefCookieVisitorProc): Boolean;
    function SetCookie(const  url: ustring; const  name, value, domain, path: ustring; secure, httponly,
      hasExpires: Boolean; const  creation, lastAccess, expires: TDateTime): Boolean;
    function DeleteCookies(const  url, cookieName: ustring): Boolean;
    function SetStoragePath(const  path: ustring; PersistSessionCookies:boolean): Boolean;
    function FlushStore(handler:ICefCompletionCallback):boolean;
  end;

  ICefCookieVisitor = interface(ICefBase)
  ['{8378CF1B-84AB-4FDB-9B86-34DDABCCC402}']
    function Visit(const  cookie:TWACefCookie; count, total: cint; out deleteCookie: Boolean): Boolean;
  end;

  //............................................................................cef_dialog_handler_capi
  ICefFileDialogCallBack = interface(ICefBase)
  ['{F5F75E88-4BEC-4BE3-B179-DC5C6DFDAA84}']
    procedure cont(FilePaths:TStrings);
    procedure cancel;
  end;

  ICefDialogHandler = interface(ICefBase)
  ['{07386301-A6AB-4599-873E-8D89545CB39F}']
    function OnFileDialog(const browser:ICefBrowser; mode:TCefFileDialogMode;
      const  title:ustring; const  DefaultFileName:ustring;
      AcceptTypes:TStrings; const callback:ICefFileDialogCallback):Boolean;
  end;

  //............................................................................cef_display_handler_capi
  ICefDisplayHandler = interface(ICefBase)
  ['{1EC7C76D-6969-41D1-B26D-079BCFF054C4}']
    procedure OnAddressChange(const  browser: ICefBrowser; const  frame: ICefFrame; const  url: ustring);
    procedure OnTitleChange(const  browser: ICefBrowser; const  title: ustring);
    function OnTooltip(const  browser: ICefBrowser; var text: ustring): Boolean;
    procedure OnStatusMessage(const  browser: ICefBrowser; const  value: ustring);
    function OnConsoleMessage(const  browser: ICefBrowser; const  message, source: ustring; line: cint): Boolean;
  end;

  //............................................................................cef_dom_capi
  ICefDomVisitor = interface(ICefBase)
  ['{30398428-3196-4531-B968-2DDBED36F6B0}']
    procedure visit(const  document: ICefDomDocument);
  end;

  ICefDomDocument = interface(ICefBase)
  ['{08E74052-45AF-4F69-A578-98A5C3959426}']
    function GetType: TCefDomDocumentType;
    function GetDocument: ICefDomNode;
    function GetBody: ICefDomNode;
    function GetHead: ICefDomNode;
    function GetTitle: ustring;
    function GetElementById(const  id: ustring): ICefDomNode;
    function GetFocusedNode: ICefDomNode;
    function HasSelection: Boolean;
    function GetSelectionStartNode: ICefDomNode;
    function GetSelectionStartOffset: cint;
    function GetSelectionEndNode: ICefDomNode;
    function GetSelectionEndOffset: cint;
    function GetSelectionAsMarkup: ustring;
    function GetSelectionAsText: ustring;
    function GetBaseUrl: ustring;
    function GetCompleteUrl(const  partialURL: ustring): ustring;

    property DocType: TCefDomDocumentType read GetType;
    property Document: ICefDomNode read GetDocument;
    property Body: ICefDomNode read GetBody;
    property Head: ICefDomNode read GetHead;
    property Title: ustring read GetTitle;
    property FocusedNode: ICefDomNode read GetFocusedNode;
    property SelectionStartNode: ICefDomNode read GetSelectionStartNode;
    property SelectionStartOffset: cint read GetSelectionStartOffset;
    property SelectionEndNode: ICefDomNode read GetSelectionEndNode;
    property SelectionEndOffset: cint read GetSelectionEndOffset;
    property SelectionAsMarkup: ustring read GetSelectionAsMarkup;
    property SelectionAsText: ustring read GetSelectionAsText;
    property BaseUrl: ustring read GetBaseUrl;
  end;

  ICefDomNode = interface(ICefBase)
  ['{96C03C9E-9C98-491A-8DAD-1947332232D6}']
    function GetType: TCefDomNodeType;
    function IsText: Boolean;
    function IsElement: Boolean;
    function IsEditable: Boolean;
    function IsFormControlElement: Boolean;
    function GetFormControlElementType: ustring;
    function IsSame(const  that: ICefDomNode): Boolean;
    function GetName: ustring;
    function GetValue: ustring;
    function SetValue(const  value: ustring): Boolean;
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
    function HasElementAttribute(const  attrName: ustring): Boolean;
    function GetElementAttribute(const  attrName: ustring): ustring;
    procedure GetElementAttributes(const  attrMap: ICefStringMap);
    function SetElementAttribute(const  attrName, value: ustring): Boolean;
    function GetElementInnerText: ustring;

    property NodeType: TCefDomNodeType read GetType;
    property Name: ustring read GetName;
    property AsMarkup: ustring read GetAsMarkup;
    property Document: ICefDomDocument read GetDocument;
    property Parent: ICefDomNode read GetParent;
    property PreviousSibling: ICefDomNode read GetPreviousSibling;
    property NextSibling: ICefDomNode read GetNextSibling;
    property FirstChild: ICefDomNode read GetFirstChild;
    property LastChild: ICefDomNode read GetLastChild;
    property ElementTagName: ustring read GetElementTagName;
    property ElementInnerText: ustring read GetElementInnerText;
  end;

  //............................................................................cef_download_handler_capi
  ICefBeforeDownloadCallback = interface(ICefBase)
  ['{5A81AF75-CBA2-444D-AD8E-522160F36433}']
    procedure Cont(const  downloadPath: ustring; showDialog: Boolean);
  end;

  ICefDownloadItemCallback = interface(ICefBase)
  ['{498F103F-BE64-4D5F-86B7-B37EC69E1735}']
    procedure cancel;
  end;

  ICefDownloadHandler = interface(ICefBase)
  ['{3137F90A-5DC5-43C1-858D-A269F28EF4F1}']
    procedure OnBeforeDownload(const  browser: ICefBrowser; const  downloadItem: ICefDownloadItem;
      const  suggestedName: ustring; const  callback: ICefBeforeDownloadCallback);
    procedure OnDownloadUpdated(const  browser: ICefBrowser; const  downloadItem: ICefDownloadItem;
      const  callback: ICefDownloadItemCallback);
  end;

  //............................................................................cef_download_item_capi
  ICefDownloadItem = interface(ICefBase)
  ['{B34BD320-A82E-4185-8E84-B98E5EEC803F}']
    function IsValid: Boolean;
    function IsInProgress: Boolean;
    function IsComplete: Boolean;
    function IsCanceled: Boolean;
    function GetCurrentSpeed: Int64;
    function GetPercentComplete: cint;
    function GetTotalBytes: Int64;
    function GetReceivedBytes: Int64;
    function GetStartTime: TDateTime;
    function GetEndTime: TDateTime;
    function GetFullPath: ustring;
    function GetId: cint;
    function GetUrl: ustring;
    function GetSuggestedFileName: ustring;
    function GetContentDisposition: ustring;
    function GetMimeType: ustring;

    property CurrentSpeed: Int64 read GetCurrentSpeed;
    property PercentComplete: cint read GetPercentComplete;
    property TotalBytes: Int64 read GetTotalBytes;
    property ReceivedBytes: Int64 read GetReceivedBytes;
    property StartTime: TDateTime read GetStartTime;
    property EndTime: TDateTime read GetEndTime;
    property FullPath: ustring read GetFullPath;
    property Id: cint read GetId;
    property Url: ustring read GetUrl;
    property SuggestedFileName: ustring read GetSuggestedFileName;
    property ContentDisposition: ustring read GetContentDisposition;
    property MimeType: ustring read GetMimeType;
  end;

  //............................................................................cef_drag_data_capi
  ICefDragData=interface(ICefBase)
  ['{DD211D8A-E42D-48D4-B01F-2082A7DE0FCD}']
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
		procedure SetFragmentBaseURL(const BaseURL: ustring);
		procedure ResetFileContents;
		procedure AddFile(const path: ustring; const DisplayName: ustring);
  end;

  //............................................................................cef_drag_handler_capi
  ICefDragHandler=interface(ICefBase)
  ['{FA58BA67-6D5B-48DE-A9AB-E39E424763B2}']
    function OnDragEnter(const  browser:ICefBrowser; const  dragData:ICefDragData; mask:TCefDragOperationsMask):Boolean;
  end;

  //............................................................................cef_focus_handler_capi
  ICefFocusHandler = interface(ICefBase)
  ['{BB7FA3FA-7B1A-4ADC-8E50-12A24018DD90}']
    procedure OnTakeFocus(const  browser: ICefBrowser; next: Boolean);
    function OnSetFocus(const  browser: ICefBrowser; source: TCefFocusSource): Boolean;
    procedure OnGotFocus(const  browser: ICefBrowser);
  end;

  //............................................................................cef_frame_capi
  ICefFrame = interface(ICefBase)
  ['{8FD3D3A6-EA3A-4A72-8501-0276BD5C3D1D}']
    function IsValid: Boolean;
    procedure Undo;
    procedure Redo;
    procedure Cut;
    procedure Copy;
    procedure Paste;
    procedure Del;
    procedure SelectAll;
    procedure ViewSource;
    procedure GetSource(const  visitor: ICefStringVisitor);
    procedure GetSourceProc(const  proc: TCefStringVisitorProc);
    procedure GetText(const  visitor: ICefStringVisitor);
    procedure GetTextProc(const  proc: TCefStringVisitorProc);
    procedure LoadRequest(const  request: ICefRequest);
    procedure LoadUrl(const  url: ustring);
    procedure LoadString(const  str, url: ustring);
    procedure ExecuteJavaScript(const  code, scriptUrl: ustring; startLine: cint);
    function IsMain: Boolean;
    function IsFocused: Boolean;
    function GetName: ustring;
    function GetIdentifier: Int64;
    function GetParent: ICefFrame;
    function GetUrl: ustring;
    function GetBrowser: ICefBrowser;
    function GetV8Context: ICefv8Context;
    procedure VisitDom(const  visitor: ICefDomVisitor);
    procedure VisitDomProc(const  proc: TCefDomVisitorProc);

    property Name: ustring read GetName;
    property Url: ustring read GetUrl;
    property Browser: ICefBrowser read GetBrowser;
    property Parent: ICefFrame read GetParent;
  end;

  //............................................................................cef_geolocation_capi
  ICefGetGeolocationCallback = interface(ICeFBase)
  ['{CF21C6CB-5707-4A62-AD9B-31A6310F9EEC}']
    procedure OnLocationUpdate(const position:TCefGeoposition);
  end;

  //............................................................................cef_geolocation_handler_capi
  ICefGeolocationCallback = interface(ICefBase)
  ['{272B8E4F-4AE4-4F14-BC4E-5924FA0C149D}']
    procedure Cont(allow: Boolean);
  end;

  ICefGeolocationHandler = interface(ICefBase)
  ['{1178EE62-BAE7-4E44-932B-EAAC7A18191C}']
    procedure OnRequestGeolocationPermission(const  browser: ICefBrowser;
      const  requestingUrl: ustring; requestId: cint; const  callback: ICefGeolocationCallback);
    procedure OnCancelGeolocationPermission(const  browser: ICefBrowser;
      const  requestingUrl: ustring; requestId: cint);
  end;

  //............................................................................cef_jsdialog_handler_capi
  ICefJsDialogCallback = interface(ICefBase)
  ['{187B2156-9947-4108-87AB-32E559E1B026}']
    procedure Cont(success: Boolean; const  userInput: ustring);
  end;

  //............................................................................cef_jsdialog_handler_capi
  ICefJsDialogHandler = interface(ICefBase)
  ['{64E18F86-DAC5-4ED1-8589-44DE45B9DB56}']
    function OnJsdialog(const  browser: ICefBrowser; const  originUrl, acceptLang: ustring;
      dialogType: TCefJsDialogType; const  messageText, defaultPromptText: ustring;
      callback: ICefJsDialogCallback; out suppressMessage: Boolean): Boolean;
    function OnBeforeUnloadDialog(const  browser: ICefBrowser;
      const  messageText: ustring; isReload: Boolean;
      const  callback: ICefJsDialogCallback): Boolean;
    procedure OnResetDialogState(const  browser: ICefBrowser);
    procedure OnDialogClosed(const browser: ICefBrowser);
  end;

  //............................................................................cef_keyboard_handler_capi
  ICefKeyboardHandler = interface(ICefBase)
  ['{0512F4EC-ED88-44C9-90D3-5C6D03D3B146}']
    function OnPreKeyEvent(const  browser: ICefBrowser; const  event: PCefKeyEvent;
      osEvent: TCefEventHandle; out isKeyboardShortcut: Boolean): Boolean;
    function OnKeyEvent(const  browser: ICefBrowser; const  event: PCefKeyEvent;
      osEvent: TCefEventHandle): Boolean;
  end;

  //............................................................................cef_life_span_handler_capi
  ICefLifeSpanHandler = interface(ICefBase)
  ['{0A3EB782-A319-4C35-9B46-09B2834D7169}']
    function OnBeforePopup(const  parentBrowser:ICefBrowser; const frame:ICefFrame;
      var TargetURL:ustring; const  TargetFrameName:ustring;
      var popupFeatures:TCefPopupFeatures; var windowInfo:TCefWindowInfo;
      var client:ICefClient; var settings:TCefBrowserSettings; var NoJavascriptAccess:boolean): Boolean;
    procedure OnAfterCreated(const  browser: ICefBrowser);
    procedure OnBeforeClose(const  browser: ICefBrowser);
    function RunModal(const  browser: ICefBrowser): Boolean;
    function DoClose(const  browser: ICefBrowser): Boolean;
  end;

  //............................................................................cef_load_handler_capi
  ICefLoadHandler = interface(ICefBase)
  ['{2C63FB82-345D-4A5B-9858-5AE7A85C9F49}']
    procedure OnLoadingStateChange(const  browser: ICefBrowser; IsLoading:boolean; CanGoBack:boolean; CanGoForward:boolean);
    procedure OnLoadStart(const  browser: ICefBrowser; const  frame: ICefFrame);
    procedure OnLoadEnd(const  browser: ICefBrowser; const  frame: ICefFrame; httpStatusCode: cint);
    procedure OnLoadError(const  browser: ICefBrowser; const  frame: ICefFrame; errorCode: TCefErrorCode;
      const  errorText, failedUrl: ustring);
  end;

  //............................................................................cef_menu_model_capi
  ICefMenuModel = interface(ICefBase)
  ['{40AF19D3-8B4E-44B8-8F89-DEB5907FC495}']
    function Clear: Boolean;
    function GetCount: cint;
    function AddSeparator: Boolean;
    function AddItem(commandId: cint; const  text: ustring): Boolean;
    function AddCheckItem(commandId: cint; const  text: ustring): Boolean;
    function AddRadioItem(commandId: cint; const  text: ustring; groupId: cint): Boolean;
    function AddSubMenu(commandId: cint; const  text: ustring): ICefMenuModel;
    function InsertSeparatorAt(index: cint): Boolean;
    function InsertItemAt(index, commandId: cint; const  text: ustring): Boolean;
    function InsertCheckItemAt(index, commandId: cint; const  text: ustring): Boolean;
    function InsertRadioItemAt(index, commandId: cint; const  text: ustring; groupId: cint): Boolean;
    function InsertSubMenuAt(index, commandId: cint; const  text: ustring): ICefMenuModel;
    function Remove(commandId: cint): Boolean;
    function RemoveAt(index: cint): Boolean;
    function GetIndexOf(commandId: cint): cint;
    function GetCommandIdAt(index: cint): cint;
    function SetCommandIdAt(index, commandId: cint): Boolean;
    function GetLabel(commandId: cint): ustring;
    function GetLabelAt(index: cint): ustring;
    function SetLabel(commandId: cint; const  text: ustring): Boolean;
    function SetLabelAt(index: cint; const  text: ustring): Boolean;
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
  end;

  //............................................................................cef_print_handler_capi
  ICefPrintDialogCallback = interface(ICefBase)
  ['{0DE87793-C344-491D-AF9B-1B20C01F5B15}']
    procedure Cont(Settings: TCefPrintSettings);
    procedure Cancel;
  end;

  ICefPrintJobCallback = interface(ICefBase)
  ['{1C9225FC-F532-4A50-A526-6A0D017F05ED}']
    procedure Cont;
  end;

  ICefPrintHandler = interface(ICefBase)
  ['{B60166CA-3216-4D5C-92A9-D828BF30A45B}']
    procedure OnPrintSettings(Settings: TCefPrintSettings; GetDefaults: Boolean);
    function OnPrintDialog(HasSelection: Boolean; Callback: ICefPrintDialogCallback): Boolean;
    function OnPrintJob(const DocumentName: ustring; const PdfFilePath: ustring; Callback: ICefPrintJobCallback): Boolean;
    procedure OnPrintReset;
  end;

  //............................................................................cef_print_settings_capi
  ICefPrintSettings = interface(ICefBase)
  ['{F5463A99-680A-459C-A265-42D38645FF0A}']
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

    property DeviceName: ustring read GetDeviceName write SetDeviceName;
    property Dpi: cint read GetDpi write SetDpi;
    property SelectionOnly: Boolean read IsSelectionOnly write SetSelectionOnly;
    property ColorModel: TCefColorModel read GetColorModel write SetColorModel;
    property Copies: cint read GetCopies write SetCopies;
    property DuplexMode: TCefDuplexMode read GetDuplexMode write SetDuplexMode;
  end;

  //............................................................................cef_process_message_capi
  ICefProcessMessage = interface(ICefBase)
    ['{E0B1001A-8777-425A-869B-29D40B8B93B1}']
    function IsValid: Boolean;
    function IsReadOnly: Boolean;
    function Copy: ICefProcessMessage;
    function GetName: ustring;
    function GetArgumentList: ICefListValue;

    property Name: ustring read GetName;
    property ArgumentList: ICefListValue read GetArgumentList;
  end;

  //............................................................................cef_render_handler_capi
  ICefRenderHandler = interface(ICefBase)
  ['{2AB9C201-F638-4AFE-ADDA-3DCDB556B2FD}']
    function GetRootScreenRect(const  browser:ICefBrowser; rect:PCefRect):Boolean;
    function GetViewRect(const  browser:ICefBrowser; rect:PCefRect):Boolean;
    function GetScreenPoint(const  browser: ICefBrowser; viewX, viewY: cint;
      screenX, screenY: pcint):Boolean;
    function GetScreenInfo(const browser:ICefBrowser; out ScreenInfo: TCefScreenInfo): Boolean;
    procedure OnPopupShow(const  browser:ICefBrowser; show:Boolean);
    procedure OnPopupSize(const  browser:ICefBrowser; const  rect:PCefRect);
    procedure OnPaint(const  browser:ICefBrowser; aType:TCefPaintElementType;
        dirtyRectsCount: csize_t; const  dirtyRects:PCefRectArray; const  buffer:Pointer;
        width:integer; height:integer);
    procedure OnCursorChange(const browser:ICefBrowser; cursor: TCefCursorHandle);
		function StartDragging(const browser:ICefBrowser; const DragData:ICefDragData;
      AllowedOps:TCefDragOperationsMask; x: cint; y: cint): Boolean;
		procedure UpdateDragCursor(const browser:ICefBrowser; operation:TCefDragOperationsMask);
    procedure OnScrollOffsetChanged(const browser:ICefBrowser);
  end;

  //............................................................................cef_render_process_handler_capi
  ICefRenderProcessHandler = interface(IcefBase)
  ['{FADEE3BC-BF66-430A-BA5D-1EE3782ECC58}']
    procedure OnRenderThreadCreated(const  ExtraInfo:ICefListValue);
    procedure OnWebKitInitialized;
    procedure OnBrowserCreated(const  browser: ICefBrowser);
    procedure OnBrowserDestroyed(const  browser: ICefBrowser);
    function GetLoadHandler:ICefLoadHandler;
    function OnBeforeNavigation(const Browser: ICefBrowser; const Frame: ICefFrame; const Request: ICefRequest;
      NavigationType: TCefNavigationType; IsRedirect: Boolean): Boolean;
    procedure OnContextCreated(const  browser: ICefBrowser;
      const  frame: ICefFrame; const  context: ICefv8Context);
    procedure OnContextReleased(const  browser: ICefBrowser;
      const  frame: ICefFrame; const  context: ICefv8Context);
    procedure OnUncaughtException(const  browser:ICefBrowser;
      const  frame:ICefFrame; const  context:ICefV8Context;
      const  exception:ICefV8Exception; const  stackTrace:ICefV8StackTrace);
    procedure OnFocusedNodeChanged(const  browser: ICefBrowser;
      const  frame: ICefFrame; const  node: ICefDomNode);
    function OnProcessMessageReceived(const  browser: ICefBrowser;
      sourceProcess: TCefProcessId; const  message: ICefProcessMessage): Boolean;
  end;

  //............................................................................cef_request_capi
  ICefRequest = interface(ICefBase)
    ['{FB4718D3-7D13-4979-9F4C-D7F6C0EC592A}']
    function IsReadOnly: Boolean;
    function GetUrl: ustring;
    procedure SetUrl(const  value: ustring);
    function GetMethod: ustring;
    function GetPostData: ICefPostData;
    procedure SetPostData(const  value: ICefPostData);
    procedure GetHeaderMap(const  HeaderMap: ICefStringMultimap);
    procedure SetHeaderMap(const  HeaderMap: ICefStringMultimap);
    procedure Assign(const  url, method: ustring;
      const  postData: ICefPostData; const  headerMap: ICefStringMultimap);
    procedure SetMethod(const  value: ustring);
    function GetFlags: TCefUrlRequestFlags;
    procedure SetFlags(flags: TCefUrlRequestFlags);
    function GetFirstPartyForCookies: ustring;
    procedure SetFirstPartyForCookies(const  url: ustring);
    function GetResourceType: TCefResourceType;
    function GetTransitionType: TCefTransitionType;

    property Url: ustring read GetUrl write SetUrl;
    property Method: ustring read GetMethod write SetMethod;
    property PostData: ICefPostData read GetPostData write SetPostData;
    property Flags: TCefUrlRequestFlags read GetFlags write SetFlags;
    property FirstPartyForCookies: ustring read GetFirstPartyForCookies write SetFirstPartyForCookies;
  end;

  ICefPostData = interface(ICefBase)
    ['{1E677630-9339-4732-BB99-D6FE4DE4AEC0}']
    function IsReadOnly: Boolean;
    function GetElementCount: csize_t;
    function GetElements(Count: csize_t): IInterfaceList; // ICefPostDataElement
    function RemoveElement(const  element: ICefPostDataElement): cint;
    function AddElement(const  element: ICefPostDataElement): cint;
    procedure RemoveElements;

    property ElementCount: csize_t read GetElementCount;
  end;

  ICefPostDataElement = interface(ICefBase)
    ['{3353D1B8-0300-4ADC-8D74-4FF31C77D13C}']
    function IsReadOnly: Boolean;
    procedure SetToEmpty;
    procedure SetToFile(const  fileName: ustring);
    procedure SetToBytes(size: csize_t; const bytes: Pointer);
    function GetType: TCefPostDataElementType;
    function GetFile: ustring;
    function GetBytesCount: csize_t;
    function GetBytes(size: csize_t; bytes: Pointer): csize_t;
  end;

  //............................................................................cef_request_context_capi
  ICefRequestContext=interface(ICefBase)
  ['{D8ACE4EB-A23D-407A-92A0-FDACDE97FBC0}']
    function IsSame(other:ICefRequestContext):boolean;
    function IsGlobal:boolean;
    function GetHandler:ICefRequestContextHandler;
  end;

  //............................................................................cef_request_context_handler_capi
  ICefRequestContextHandler=interface(ICefBase)
  ['{0FC0165C-E871-4C12-8857-A459B5FD8C3F}']
    function GetCookieManager:ICefCookieManager;
  end;

  //............................................................................cef_request_handler_capi
  ICefQuotaCallback = interface(ICefBase)
  ['{F163D612-CC9C-49CC-ADEA-FB6A32A25485}']
    procedure cont(allow: Boolean);
    procedure cancel;
  end;

  ICefAllowCertificateErrorCallback = interface (ICefBase)
  ['{E7DAB88B-92D8-4907-91F4-CF4EE6F28C7C}']
    procedure cont(Allow:boolean);
  end;

  ICefRequestHandler = interface(ICefBase)
  ['{050877A9-D1F8-4EB3-B58E-50DC3E3D39FD}']
    function OnBeforeBrowse(const  browser: ICefBrowser; const  frame: ICefFrame;
      const  request: ICefRequest; IsRedirect:boolean): Boolean;
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
      const  originUrl: ustring; newSize: Int64; const  callback: ICefQuotaCallback): Boolean;
    function OnCertificateError(CertError:TCefErrorcode; const  RequestUrl:ustring;
      const  Callback:ICefAllowCertificateErrorCallback): boolean;
    procedure OnProtocolExecution(const  browser: ICefBrowser; const  url: ustring; out allowOsExecution: Boolean);
    function OnBeforePluginLoad(const  browser: ICefBrowser; const  url, policyUrl: ustring;
      const  info: ICefWebPluginInfo): Boolean;
    procedure OnPluginCrashed(const  browser: ICefBrowser; const  pluginPath: ustring);
    procedure OnRenderProcessTerminated(const  browser: ICefBrowser; status:TCefTerminationStatus);
  end;

  //............................................................................cef_resource_bundle_handler_capi
  ICefResourceBundleHandler = interface(ICefBase)
    ['{09C264FD-7E03-41E3-87B3-4234E82B5EA2}']
    function GetLocalizedString(messageId: cint; out stringVal: ustring): Boolean;
    function GetDataResource(resourceId: cint; out data: Pointer; out dataSize: csize_t): Boolean;
  end;

  //............................................................................cef_resource_handler_capi
  ICefResourceHandler = interface(ICefBase)
  ['{BD3EA208-AAAD-488C-BFF2-76993022F2B5}']
    function ProcessRequest(const  request: ICefRequest; const  callback: ICefCallback): Boolean;
    procedure GetResponseHeaders(const  response: ICefResponse;
      out responseLength: Int64; out redirectUrl: ustring);
    function ReadResponse(const  dataOut: Pointer; bytesToRead: cint;
      var bytesRead: cint; const  callback: ICefCallback): Boolean;
    function CanGetCookie(const  cookie: PCefCookie): Boolean;
    function CanSetCookie(const  cookie: PCefCookie): Boolean;
    procedure Cancel;
  end;

  //............................................................................cef_response_capi
  ICefResponse = interface(ICefBase)
  ['{E9C896E4-59A8-4B96-AB5E-6EA3A498B7F1}']
    function IsReadOnly: Boolean;
    function GetStatus: cint;
    procedure SetStatus(status: cint);
    function GetStatusText: ustring;
    procedure SetStatusText(const  StatusText: ustring);
    function GetMimeType: ustring;
    procedure SetMimeType(const  mimetype: ustring);
    function GetHeader(const  name: ustring): ustring;
    procedure GetHeaderMap(const  headerMap: ICefStringMultimap);
    procedure SetHeaderMap(const  headerMap: ICefStringMultimap);

    property Status: cint read GetStatus write SetStatus;
    property StatusText: ustring read GetStatusText write SetStatusText;
    property MimeType: ustring read GetMimeType write SetMimeType;
  end;

  //............................................................................cef_scheme_capi
  ICefSchemeRegistrar = interface(ICefBase)
  ['{1832FF6E-100B-4E8B-B996-AD633168BEE7}']
    function AddCustomScheme(const  schemeName: ustring; IsStandard, IsLocal,
      IsDisplayIsolated: Boolean): Boolean; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
  end;

  ICefSchemeHandlerFactory = interface(ICefBase)
  ['{4D9B7960-B73B-4EBD-9ABE-6C1C43C245EB}']
    function New(const  browser: ICefBrowser; const  frame: ICefFrame;
      const  schemeName: ustring; const  request: ICefRequest): ICefResourceHandler;
  end;

  //............................................................................cef_stream_capi
  ICefReadHandler = interface(ICefBase)
  ['{C41E22FE-7ECF-4A0E-9FB4-EE51BC96E4B3}']
    function Read(ptr:Pointer; size: csize_t; n: csize_t): csize_t;
		function Seek(offset: cint64; whence: cint):Boolean;
		function Tell: cint64;
		function Eof:Boolean;
		function MayBlock:Boolean;
  end;

  ICefStreamReader = interface(ICefBase)
    ['{DD5361CB-E558-49C5-A4BD-D1CE84ADB277}']
    function Read(ptr: Pointer; size, n: csize_t): csize_t;
    function Seek(offset: Int64; whence: cint): cint;
    function Tell: Int64;
    function Eof: Boolean;
    function MayBlock: Boolean;
  end;

  ICefWriteHandler = interface(ICefBase)
  ['{2F1E4A60-B3E7-4FF6-AF3F-918691A27469}']
    function Write(const ptr:Pointer; size: csize_t; n: csize_t): csize_t;
		function Seek(offset: cint64; whence: cint):Boolean;
		function Tell: cint64;
		function Flush:Boolean;
		function MayBlock:Boolean;
  end;

  ICefStreamWriter = interface(ICefBase)
  ['{C7217238-A22D-4B37-82AE-01F63B69F00C}']
		function Write(const ptr:Pointer; size: csize_t; n: csize_t): csize_t;
		function Seek(offset: cint64; whence: cint):Boolean;
		function Tell: cint64;
		function Flush:Boolean;
		function MayBlock:Boolean;
  end;

  //............................................................................cef_string_visitor_capi
  ICefStringVisitor = interface(ICefBase)
    ['{63ED4D6C-2FC8-4537-964B-B84C008F6158}']
    procedure Visit(const  str: ustring);
  end;

  //............................................................................cef_task_capi
  ICefTask = interface(ICefBase)
  ['{0D965470-4A86-47CE-BD39-A8770021AD7E}']
    procedure Execute;
  end;

  ICefTaskRunner = interface(ICefBase)
  ['{B933A3B2-75AD-48DA-B820-B573052A7A4A}']
    function IsSame(that:ICefTaskRunner):boolean;
    function BelongsToCurrentThread:boolean;
    function BelongsToThread(ThreadID:TCefThreadID):boolean;
    function PostTask(task:ICefTask):integer;
    function PostDelayedTask(task:ICefTask; DelayMS:Int64):integer;
  end;

  //............................................................................cef_trace_capi
  ICefEndTracingCallback = interface(ICefBase)
  ['{BFA51F8E-93E0-490A-B3B2-61C19D04F766}']
    procedure OnEndTracingComplete(const TracingFile: ustring);
  end;

  //............................................................................cef_urlrequest_capi
  ICefUrlRequest = interface(ICefBase)
    ['{59226AC1-A0FA-4D59-9DF4-A65C42391A67}']
    function GetRequest: ICefRequest;
    function GetClient: ICefUrlrequestClient;
    function GetRequestStatus: TCefUrlRequestStatus;
    function GetRequestError: TCefErrorcode;
    function GetResponse: ICefResponse;
    procedure Cancel;
  end;

  ICefUrlrequestClient = interface(ICefBase)
    ['{114155BD-C248-4651-9A4F-26F3F9A4F737}']
    procedure OnRequestComplete(const  request: ICefUrlRequest);
    procedure OnUploadProgress(const  request: ICefUrlRequest; current, total: UInt64);
    procedure OnDownloadProgress(const  request: ICefUrlRequest; current, total: UInt64);
    procedure OnDownloadData(const  request: ICefUrlRequest; data: Pointer; dataLength: csize_t);
    function GetAuthCredentials(IsProxy: Boolean; const Host: ustring; Port: CInt; const Realm: ustring; const Scheme: ustring; Callback: ICefAuthCallback): Boolean;
  end;

  //............................................................................cef_v8_capi
  ICefv8Context = interface(ICefBase)
    ['{2295A11A-8773-41F2-AD42-308C215062D9}']
    function GetTaskRunner:ICefTaskRunner;
    function IsValid:boolean;
    function GetBrowser: ICefBrowser;
    function GetFrame: ICefFrame;
    function GetGlobal: ICefv8Value;
    function Enter: Boolean;
    function Exit: Boolean;
    function IsSame(const  that: ICefv8Context): Boolean;
    function Eval(const  code: ustring; var retval: ICefv8Value; var exception: ICefV8Exception): Boolean;

    property Browser: ICefBrowser read GetBrowser;
    property Frame: ICefFrame read GetFrame;
    property Global: ICefv8Value read GetGlobal;
  end;

  ICefv8Handler = interface(ICefBase)
    ['{F94CDC60-FDCB-422D-96D5-D2A775BD5D73}']
    function Execute(const  name: ustring; const  obj: ICefv8Value;
      const  arguments: TCefv8ValueArray; var retval: ICefv8Value;
      var exception: ustring): Boolean;
  end;

  ICefV8Accessor = interface(ICefBase)
    ['{DCA6D4A2-726A-4E24-AA64-5E8C731D868A}']
    function Get(const  name: ustring; const  obj: ICefv8Value;
      out value: ICefv8Value; const  exception: ustring): Boolean;
    function Put(const  name: ustring; const  obj: ICefv8Value;
      const  value: ICefv8Value; const  exception: ustring): Boolean;
  end;

  ICefV8Exception = interface(ICefBase)
    ['{7E422CF0-05AC-4A60-A029-F45105DCE6A4}']
    function GetMessage: ustring;
    function GetSourceLine: ustring;
    function GetScriptResourceName: ustring;
    function GetLineNumber: cint;
    function GetStartPosition: cint;
    function GetEndPosition: cint;
    function GetStartColumn: cint;
    function GetEndColumn: cint;

    property Message: ustring read GetMessage;
    property SourceLine: ustring read GetSourceLine;
    property ScriptResourceName: ustring read GetScriptResourceName;
    property LineNumber: cint read GetLineNumber;
    property StartPosition: cint read GetStartPosition;
    property EndPosition: cint read GetEndPosition;
    property StartColumn: cint read GetStartColumn;
    property EndColumn: cint read GetEndColumn;
  end;

  ICefv8Value = interface(ICefBase)
  ['{52319B8D-75A8-422C-BD4B-16FA08CC7F42}']
    function IsValid:boolean;
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
    function IsSame(const  that: ICefv8Value): Boolean;
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
    function HasValueByKey(const  key: ustring): Boolean;
    function HasValueByIndex(index: cint): Boolean;
    function DeleteValueByKey(const  key: ustring): Boolean;
    function DeleteValueByIndex(index: cint): Boolean;
    function GetValueByKey(const  key: ustring): ICefv8Value;
    function GetValueByIndex(index: cint): ICefv8Value;
    function SetValueByKey(const  key: ustring; const  value: ICefv8Value;
      attribute: TCefV8PropertyAttribute): Boolean;
    function SetValueByIndex(index: cint; const  value: ICefv8Value): Boolean;
    function SetValueByAccessor(const  key: ustring; settings: TCefV8AccessControl;
      attribute: TCefV8PropertyAttribute): Boolean;
    function GetKeys(const  keys: TStrings): cint;
    function SetUserData(const  data: ICefv8Value): Boolean;
    function GetUserData: ICefv8Value;
    function GetExternallyAllocatedMemory: cint;
    function AdjustExternallyAllocatedMemory(changeInBytes: cint): cint;
    function GetArrayLength: cint;
    function GetFunctionName: ustring;
    function GetFunctionHandler: ICefv8Handler;
    function ExecuteFunction(const  obj: ICefv8Value;
      const  arguments: TCefv8ValueArray): ICefv8Value;
    function ExecuteFunctionWithContext(const  context: ICefv8Context;
      const  obj: ICefv8Value; const  arguments: TCefv8ValueArray): ICefv8Value;
  end;

  ICefV8StackTrace = interface(ICefBase)
  ['{32111C84-B7F7-4E3A-92B9-7CA1D0ADB613}']
    function IsValid:boolean;
    function GetFrameCount: cint;
    function GetFrame(index: cint): ICefV8StackFrame;

    property FrameCount: cint read GetFrameCount;
    property Frame[index: cint]: ICefV8StackFrame read GetFrame;
  end;

  ICefV8StackFrame = interface(ICefBase)
  ['{BA1FFBF4-E9F2-4842-A827-DC220F324286}']
    function IsValid:boolean;
    function GetScriptName: ustring;
    function GetScriptNameOrSourceUrl: ustring;
    function GetFunctionName: ustring;
    function GetLineNumber: cint;
    function GetColumn: cint;
    function IsEval: Boolean;
    function IsConstructor: Boolean;

    property ScriptName: ustring read GetScriptName;
    property ScriptNameOrSourceUrl: ustring read GetScriptNameOrSourceUrl;
    property FunctionName: ustring read GetFunctionName;
    property LineNumber: cint read GetLineNumber;
    property Column: cint read GetColumn;
  end;

  //............................................................................cef_values_capi
  ICefBinaryValue = interface(ICefBase)
  ['{974AA40A-9C5C-4726-81F0-9F0D46D7C5B3}']
    function IsValid: Boolean;
    function IsOwned: Boolean;
    function Copy: ICefBinaryValue;
    function GetSize: csize_t;
    function GetData(buffer: Pointer; bufferSize, dataOffset: csize_t): csize_t;
  end;

  ICefDictionaryValue = interface(ICefBase)
  ['{B9638559-54DC-498C-8185-233EEF12BC69}']
    function IsValid: Boolean;
    function isOwned: Boolean;
    function IsReadOnly: Boolean;
    function Copy(excludeEmptyChildren: Boolean): ICefDictionaryValue;
    function GetSize: csize_t;
    function Clear: Boolean;
    function HasKey(const  key: ustring): Boolean;
    function GetKeys(const  keys: TStrings): Boolean;
    function Remove(const  key: ustring): Boolean;
    function GetType(const  key: ustring): TCefValueType;
    function GetBool(const  key: ustring): Boolean;
    function GetInt(const  key: ustring): cint;
    function GetDouble(const  key: ustring): Double;
    function GetString(const  key: ustring): ustring;
    function GetBinary(const  key: ustring): ICefBinaryValue;
    function GetDictionary(const  key: ustring): ICefDictionaryValue;
    function GetList(const  key: ustring): ICefListValue;
    function SetNull(const  key: ustring): Boolean;
    function SetBool(const  key: ustring; value: Boolean): Boolean;
    function SetInt(const  key: ustring; value: cint): Boolean;
    function SetDouble(const  key: ustring; value: Double): Boolean;
    function SetString(const  key, value: ustring): Boolean;
    function SetBinary(const  key: ustring; const  value: ICefBinaryValue): Boolean;
    function SetDictionary(const  key: ustring; const  value: ICefDictionaryValue): Boolean;
    function SetList(const  key: ustring; const  value: ICefListValue): Boolean;
  end;

  ICefListValue = interface(ICefBase)
  ['{09174B9D-0CC6-4360-BBB0-3CC0117F70F6}']
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
    function SetString(index: cint; const  value: ustring): Boolean;
    function SetBinary(index: cint; const  value: ICefBinaryValue): Boolean;
    function SetDictionary(index: cint; const  value: ICefDictionaryValue): Boolean;
    function SetList(index: cint; const  value: ICefListValue): Boolean;
  end;

  ICefWebPluginInfo = interface(ICefBase)
    ['{AA879E58-F649-44B1-AF9C-655FF5B79A02}']
    function GetName: ustring;
    function GetPath: ustring;
    function GetVersion: ustring;
    function GetDescription: ustring;

    property Name: ustring read GetName;
    property Path: ustring read GetPath;
    property Version: ustring read GetVersion;
    property Description: ustring read GetDescription;
  end;

  ICefWebPluginInfoVisitor = interface(ICefBase)
  ['{7523D432-4424-4804-ACAD-E67D2313436E}']
    function Visit(const  info: ICefWebPluginInfo; count, total: cint): Boolean;
  end;

  ICefWebPluginUnstableCallback = interface(ICefBase)
  ['{67459829-EB47-4B7E-9D69-2EE77DF0E71E}']
    procedure IsUnstable(const  path: ustring; unstable: Boolean);
  end;

  ICefXmlReader = interface(ICefBase)
  ['{0DE686C3-A8D7-45D2-82FD-92F7F4E62A90}']
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
    function GetAttributeByQName(const  qualifiedName: ustring): ustring;
    function GetAttributeByLName(const  localName, namespaceURI: ustring): ustring;
    function GetInnerXml: ustring;
    function GetOuterXml: ustring;
    function GetLineNumber: cint;
    function MoveToAttributeByIndex(index: cint): Boolean;
    function MoveToAttributeByQName(const  qualifiedName: ustring): Boolean;
    function MoveToAttributeByLName(const  localName, namespaceURI: ustring): Boolean;
    function MoveToFirstAttribute: Boolean;
    function MoveToNextAttribute: Boolean;
    function MoveToCarryingElement: Boolean;
  end;

  ICefZipReader = interface(ICefBase)
  ['{3B6C591F-9877-42B3-8892-AA7B27DA34A8}']
    function MoveToFirstFile: Boolean;
    function MoveToNextFile: Boolean;
    function MoveToFile(const  fileName: ustring; caseSensitive: Boolean): Boolean;
    function Close: Boolean;
    function GetFileName: ustring;
    function GetFileSize: Int64;
    function GetFileLastModified: Time_t;
    function OpenFile(const  password: ustring): Boolean;
    function CloseFile: Boolean;
    function ReadFile(buffer: Pointer; bufferSize: csize_t): cint;
    function Tell: Int64;
    function Eof: Boolean;
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

implementation

end.
