unit CatSciterAx;

{
  AxSciterLib_TLB.pas slightly modified
  If you regenerate the file, remember to re-apply the changes below.
  -Felipe Daragon

  TSciter was not handling tab and arrow keys. This fixed it:

  Added Messages, and Controls to uses, and:

TSciter = class(TOleControl) 
 private 
   procedure CNKeyDown(var Message: TMessage); message CN_KEYDOWN; 
 public 
   .... 
 end; 

procedure TMyActiveForm.CNKeyDown(var Message: TMessage); 
begin 
end;
}

// ************************************************************************ //
// WARNING                                                                    
// -------                                                                    
// The types declared in this file were generated from data read from a       
// Type Library. If this type library is explicitly or indirectly (via        
// another type library referring to this type library) re-imported, or the   
// 'Refresh' command of the Type Library Editor activated while editing the   
// Type Library, the contents of this file will be regenerated and all        
// manual modifications will be lost.                                         
// ************************************************************************ //

// $Rev: 52393 $
// File generated on 7/5/2013 06:33:59 from Type Library described below.

// ************************************************************************  //
// Type Lib: AxSciter.dll (1)
// LIBID: {25D9681B-32F2-44C9-B94F-5E82E7ED0C75}
// LCID: 0
// Helpfile: 
// HelpString: AxSciter 1.0 Type Library
// DepndLst: 
//   (1) v2.0 stdole, (stdole2.tlb)
// SYS_KIND: SYS_WIN32
// Errors:
//   Error creating palette bitmap of (TElement) : Server AxSciter.dll contains no icons
//   Error creating palette bitmap of (TElements) : Server AxSciter.dll contains no icons
// ************************************************************************ //
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers. 
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}
{$VARPROPSETTER ON}
{$ALIGN 4}

interface

uses Winapi.Windows,
 Winapi.Messages, Vcl.controls,
 System.Classes, System.Variants, System.Win.StdVCL, Vcl.Graphics, Vcl.OleCtrls, Vcl.OleServer, Winapi.ActiveX;
  


// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:        
//   Type Libraries     : LIBID_xxxx                                      
//   CoClasses          : CLASS_xxxx                                      
//   DISPInterfaces     : DIID_xxxx                                       
//   Non-DISP interfaces: IID_xxxx                                        
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  AxSciterLibMajorVersion = 1;
  AxSciterLibMinorVersion = 0;

  LIBID_AxSciterLib: TGUID = '{25D9681B-32F2-44C9-B94F-5E82E7ED0C75}';

  IID_IElements: TGUID = '{C7171909-9F92-48D7-8691-EFB3390DEE55}';
  IID_IElement: TGUID = '{645B0717-C0AB-424D-B513-F083AD486BF1}';
  IID_ISciter: TGUID = '{FA63A755-C7B3-4DB6-833F-3D5FE102495E}';
  DIID__ISciterEvents: TGUID = '{ED2316A7-3EB2-4C80-9146-600B408B08D8}';
  CLASS_Sciter: TGUID = '{99829A7E-007E-4F60-AC36-31B646896593}';
  DIID__IElementEvents: TGUID = '{2A8AAFD6-6E87-4967-BF6D-C3F6BB9B3BD1}';
  CLASS_Element: TGUID = '{53FB239D-7857-4F0D-9083-871D8C0EAE3A}';
  CLASS_Elements: TGUID = '{B1C8635C-12B4-40F7-8038-6134FC5D398F}';

// *********************************************************************//
// Declaration of Enumerations defined in Type Library                    
// *********************************************************************//
// Constants for enum __MIDL___MIDL_itf_AxSciter_0000_0001
type
  __MIDL___MIDL_itf_AxSciter_0000_0001 = TOleEnum;
const
  ContentBox = $00000000;
  PaddingBox = $00000010;
  BorderBox = $00000020;
  MarginBox = $00000030;
  BackImageArea = $00000040;
  ForeImageArea = $00000050;
  ScrollableArea = $00000060;

// Constants for enum __MIDL___MIDL_itf_AxSciter_0000_0002
type
  __MIDL___MIDL_itf_AxSciter_0000_0002 = TOleEnum;
const
  RootRelative = $00000001;
  SelfRelative = $00000002;
  ContainerRelative = $00000003;
  ViewRelative = $00000004;

// Constants for enum __MIDL___MIDL_itf_AxSciter_0000_0003
type
  __MIDL___MIDL_itf_AxSciter_0000_0003 = TOleEnum;
const
  DATA_HTML = $00000000;
  DATA_IMAGE = $00000001;
  DATA_STYLE = $00000002;
  DATA_CURSOR = $00000003;
  DATA_SCRIPT = $00000004;

// Constants for enum __MIDL___MIDL_itf_AxSciter_0000_0004
type
  __MIDL___MIDL_itf_AxSciter_0000_0004 = TOleEnum;
const
  MASK_BUBBLING = $00000000;
  MASK_SINKING = $00008000;
  MASK_HANDLED = $00010000;

// Constants for enum __MIDL___MIDL_itf_AxSciter_0000_0005
type
  __MIDL___MIDL_itf_AxSciter_0000_0005 = TOleEnum;
const
  ME_MOUSE_ENTER = $00000000;
  ME_MOUSE_LEAVE = $00000001;
  ME_MOUSE_MOVE = $00000002;
  ME_MOUSE_UP = $00000003;
  ME_MOUSE_DOWN = $00000004;
  ME_MOUSE_DCLICK = $00000005;
  ME_MOUSE_WHEEL = $00000006;
  ME_MOUSE_TICK = $00000007;
  ME_MOUSE_IDLE = $00000008;
  ME_DROP = $00000009;
  ME_DRAG_ENTER = $0000000A;
  ME_DRAG_LEAVE = $0000000B;
  ME_DRAGGING = $00000100;

// Constants for enum __MIDL___MIDL_itf_AxSciter_0000_0006
type
  __MIDL___MIDL_itf_AxSciter_0000_0006 = TOleEnum;
const
  BE_BUTTON_CLICK = $00000000;
  BE_BUTTON_PRESS = $00000001;
  BE_BUTTON_STATE_CHANGED = $00000002;
  BE_EDIT_VALUE_CHANGING = $00000003;
  BE_EDIT_VALUE_CHANGED = $00000004;
  BE_SELECT_SELECTION_CHANGED = $00000005;
  BE_SELECT_STATE_CHANGED = $00000006;
  BE_POPUP_REQUEST = $00000007;
  BE_POPUP_READY = $00000008;
  BE_POPUP_DISMISSED = $00000009;
  BE_MENU_ITEM_ACTIVE = $0000000A;
  BE_MENU_ITEM_CLICK = $0000000B;
  BE_CONTEXT_MENU_SETUP = $0000000F;
  BE_CONTEXT_MENU_REQUEST = $00000010;
  BE_VISIUAL_STATUS_CHANGED = $00000011;
  BE_HYPERLINK_CLICK = $00000080;
  BE_TABLE_HEADER_CLICK = $00000081;
  BE_TABLE_ROW_CLICK = $00000082;
  BE_TABLE_ROW_DBL_CLICK = $00000083;
  BE_ELEMENT_COLLAPSED = $00000090;
  BE_ELEMENT_EXPANDED = $00000091;
  BE_ACTIVATE_CHILD = $00000092;
  BE_DO_SWITCH_TAB = $00000092;
  BE_INIT_DATA_VIEW = $00000093;
  BE_ROWS_DATA_REQUEST = $00000094;
  BE_UI_STATE_CHANGED = $00000095;
  BE_FORM_SUBMIT = $00000096;
  BE_FORM_RESET = $00000097;
  BE_DOCUMENT_COMPLETE = $00000098;
  BE_HISTORY_PUSH = $00000099;
  BE_HISTORY_DROP = $0000009A;
  BE_HISTORY_PRIOR = $0000009B;
  BE_HISTORY_NEXT = $0000009C;
  BE_HISTORY_STATE_CHANGED = $0000009D;
  BE_FIRST_APPLICATION_EVENT_CODE = $00000100;

type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  IElements = interface;
  IElementsDisp = dispinterface;
  IElement = interface;
  IElementDisp = dispinterface;
  ISciter = interface;
  ISciterDisp = dispinterface;
  _ISciterEvents = dispinterface;
  _IElementEvents = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  Sciter = ISciter;
  Element = IElement;
  Elements = IElements;


// *********************************************************************//
// Declaration of structures, unions and aliases.                         
// *********************************************************************//
  PPSafeArray1 = ^PSafeArray; {*}
  PByte1 = ^Byte; {*}

  ElementBoxType = __MIDL___MIDL_itf_AxSciter_0000_0001; 
  RelativeToType = __MIDL___MIDL_itf_AxSciter_0000_0002; 
  ResourceType = __MIDL___MIDL_itf_AxSciter_0000_0003; 
  PhaseMask = __MIDL___MIDL_itf_AxSciter_0000_0004; 
  MouseEvents = __MIDL___MIDL_itf_AxSciter_0000_0005; 
  BehaviorEvents = __MIDL___MIDL_itf_AxSciter_0000_0006; 

// *********************************************************************//
// Interface: IElements
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {C7171909-9F92-48D7-8691-EFB3390DEE55}
// *********************************************************************//
  IElements = interface(IDispatch)
    ['{C7171909-9F92-48D7-8691-EFB3390DEE55}']
    function _NewEnum: IUnknown; safecall;
    function Get_Count: Integer; safecall;
    function Get_Item(index: Integer): IElement; safecall;
    property Count: Integer read Get_Count;
    property Item[index: Integer]: IElement read Get_Item;
  end;

// *********************************************************************//
// DispIntf:  IElementsDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {C7171909-9F92-48D7-8691-EFB3390DEE55}
// *********************************************************************//
  IElementsDisp = dispinterface
    ['{C7171909-9F92-48D7-8691-EFB3390DEE55}']
    function _NewEnum: IUnknown; dispid -4;
    property Count: Integer readonly dispid 1;
    property Item[index: Integer]: IElement readonly dispid 2;
  end;

// *********************************************************************//
// Interface: IElement
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {645B0717-C0AB-424D-B513-F083AD486BF1}
// *********************************************************************//
  IElement = interface(IDispatch)
    ['{645B0717-C0AB-424D-B513-F083AD486BF1}']
    function Get_Tag: WideString; safecall;
    function Get_Value: OleVariant; safecall;
    procedure Set_Value(pVal: OleVariant); safecall;
    function Select(const cssSelector: WideString): IElement; safecall;
    function SelectAll(const cssSelector: WideString): IElements; safecall;
    function Get_Attr(const name: WideString): OleVariant; safecall;
    procedure Set_Attr(const name: WideString; pVal: OleVariant); safecall;
    function Get_StyleAttr(const name: WideString): OleVariant; safecall;
    procedure Set_StyleAttr(const name: WideString; pVal: OleVariant); safecall;
    procedure Position(out x: Integer; out y: Integer; ofWhat: ElementBoxType; relTo: RelativeToType); safecall;
    procedure Dimension(out width: Integer; out height: Integer; ofWhat: ElementBoxType); safecall;
    function Call(const methodName: WideString; var params: PSafeArray): OleVariant; safecall;
    function Get_HELEMENT: Integer; safecall;
    property Tag: WideString read Get_Tag;
    property Value: OleVariant read Get_Value write Set_Value;
    property Attr[const name: WideString]: OleVariant read Get_Attr write Set_Attr;
    property StyleAttr[const name: WideString]: OleVariant read Get_StyleAttr write Set_StyleAttr;
    property HELEMENT: Integer read Get_HELEMENT;
  end;

// *********************************************************************//
// DispIntf:  IElementDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {645B0717-C0AB-424D-B513-F083AD486BF1}
// *********************************************************************//
  IElementDisp = dispinterface
    ['{645B0717-C0AB-424D-B513-F083AD486BF1}']
    property Tag: WideString readonly dispid 1;
    property Value: OleVariant dispid 2;
    function Select(const cssSelector: WideString): IElement; dispid 3;
    function SelectAll(const cssSelector: WideString): IElements; dispid 4;
    property Attr[const name: WideString]: OleVariant dispid 5;
    property StyleAttr[const name: WideString]: OleVariant dispid 6;
    procedure Position(out x: Integer; out y: Integer; ofWhat: ElementBoxType; relTo: RelativeToType); dispid 7;
    procedure Dimension(out width: Integer; out height: Integer; ofWhat: ElementBoxType); dispid 8;
    function Call(const methodName: WideString; var params: {NOT_OLEAUTO(PSafeArray)}OleVariant): OleVariant; dispid 9;
    property HELEMENT: Integer readonly dispid 10;
  end;

// *********************************************************************//
// Interface: ISciter
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {FA63A755-C7B3-4DB6-833F-3D5FE102495E}
// *********************************************************************//
  ISciter = interface(IDispatch)
    ['{FA63A755-C7B3-4DB6-833F-3D5FE102495E}']
    procedure LoadHtml(const html: WideString; const baseUrl: WideString); safecall;
    procedure LoadUrl(const urlToLoad: WideString); safecall;
    function Get_Root: IElement; safecall;
    function Call(const name: WideString; var params: PSafeArray): OleVariant; safecall;
    function Eval(const script: WideString): OleVariant; safecall;
    procedure DataReady(requestId: Integer; var data: Byte; dataLength: Integer); safecall;
    function Get_Methods: IDispatch; safecall;
    procedure _Set_Methods(const pVal: IDispatch); safecall;
    property Root: IElement read Get_Root;
    property Methods: IDispatch read Get_Methods write _Set_Methods;
  end;

// *********************************************************************//
// DispIntf:  ISciterDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {FA63A755-C7B3-4DB6-833F-3D5FE102495E}
// *********************************************************************//
  ISciterDisp = dispinterface
    ['{FA63A755-C7B3-4DB6-833F-3D5FE102495E}']
    procedure LoadHtml(const html: WideString; const baseUrl: WideString); dispid 1;
    procedure LoadUrl(const urlToLoad: WideString); dispid 2;
    property Root: IElement readonly dispid 3;
    function Call(const name: WideString; var params: {NOT_OLEAUTO(PSafeArray)}OleVariant): OleVariant; dispid 4;
    function Eval(const script: WideString): OleVariant; dispid 5;
    procedure DataReady(requestId: Integer; var data: Byte; dataLength: Integer); dispid 6;
    property Methods: IDispatch dispid 7;
  end;

// *********************************************************************//
// DispIntf:  _ISciterEvents
// Flags:     (4096) Dispatchable
// GUID:      {ED2316A7-3EB2-4C80-9146-600B408B08D8}
// *********************************************************************//
  _ISciterEvents = dispinterface
    ['{ED2316A7-3EB2-4C80-9146-600B408B08D8}']
    function onStdOut(const msg: WideString): HResult; dispid 1;
    function onStdErr(const msg: WideString): HResult; dispid 2;
    function OnLoadData(const url: WideString; resType: ResourceType; requestId: Integer; 
                        out discard: WordBool): HResult; dispid 3;
    function OnDataLoaded(const url: WideString; resType: ResourceType; var data: Byte; 
                          dataLength: Integer; requestId: Integer): HResult; dispid 4;
  end;

// *********************************************************************//
// DispIntf:  _IElementEvents
// Flags:     (4096) Dispatchable
// GUID:      {2A8AAFD6-6E87-4967-BF6D-C3F6BB9B3BD1}
// *********************************************************************//
  _IElementEvents = dispinterface
    ['{2A8AAFD6-6E87-4967-BF6D-C3F6BB9B3BD1}']
    function OnMouse(const target: IElement; eventType: Integer; x: Integer; y: Integer; 
                     buttons: Integer; keys: Integer): WordBool; dispid 1;
    function OnKey(const target: IElement; eventType: Integer; code: Integer; keys: Integer): WordBool; dispid 2;
    function OnFocus(const target: IElement; eventType: Integer): WordBool; dispid 3;
    function OnTimer(timerId: Integer): WordBool; dispid 4;
    function OnSize: HResult; dispid 5;
    function OnControlEvent(const target: IElement; eventType: Integer; reason: Integer; 
                            const source: IElement): WordBool; dispid 6;
    function OnScroll(const target: IElement; eventType: Integer; pos: Integer; isVertical: WordBool): WordBool; dispid 7;
  end;


// *********************************************************************//
// OLE Control Proxy class declaration
// Control Name     : TSciter
// Help String      : Sciter Class
// Default Interface: ISciter
// Def. Intf. DISP? : No
// Event   Interface: _ISciterEvents
// TypeFlags        : (34) CanCreate Control
// *********************************************************************//
  TSciteronStdOut = procedure(ASender: TObject; const msg: WideString) of object;
  TSciteronStdErr = procedure(ASender: TObject; const msg: WideString) of object;
  TSciterOnLoadData = procedure(ASender: TObject; const url: WideString; resType: ResourceType; 
                                                  requestId: Integer; out discard: WordBool) of object;
  TSciterOnDataLoaded = procedure(ASender: TObject; const url: WideString; resType: ResourceType; 
                                                    var data: Byte; dataLength: Integer; 
                                                    requestId: Integer) of object;

  TSciter = class(TOleControl)
  private
    FOnonStdOut: TSciteronStdOut;
    FOnonStdErr: TSciteronStdErr;
    FOnLoadData: TSciterOnLoadData;
    FOnDataLoaded: TSciterOnDataLoaded;
    FIntf: ISciter;
    function  GetControlInterface: ISciter;
    procedure CNKeyDown(var Message: TMessage); message CN_KEYDOWN; 
  protected
    procedure CreateControl;
    procedure InitControlData; override;
    function Get_Root: IElement;
    function Get_Methods: IDispatch;
    procedure _Set_Methods(const pVal: IDispatch);
  public
    procedure LoadHtml(const html: WideString; const baseUrl: WideString);
    procedure LoadUrl(const urlToLoad: WideString);
    function Call(const name: WideString; var params: PSafeArray): OleVariant;
    function Eval(const script: WideString): OleVariant;
    procedure DataReady(requestId: Integer; var data: Byte; dataLength: Integer);
    property  ControlInterface: ISciter read GetControlInterface;
    property  DefaultInterface: ISciter read GetControlInterface;
    property Root: IElement read Get_Root;
    property Methods: IDispatch index 7 read GetIDispatchProp (* [[PUTREF-SETTER]] write _SetIDispatchProp*);
  published
    property Anchors;
    property  TabStop;
    property  Align;
    property  DragCursor;
    property  DragMode;
    property  ParentShowHint;
    property  PopupMenu;
    property  ShowHint;
    property  TabOrder;
    property  Visible;
    property  OnDragDrop;
    property  OnDragOver;
    property  OnEndDrag;
    property  OnEnter;
    property  OnExit;
    property  OnStartDrag;
    property OnonStdOut: TSciteronStdOut read FOnonStdOut write FOnonStdOut;
    property OnonStdErr: TSciteronStdErr read FOnonStdErr write FOnonStdErr;
    property OnLoadData: TSciterOnLoadData read FOnLoadData write FOnLoadData;
    property OnDataLoaded: TSciterOnDataLoaded read FOnDataLoaded write FOnDataLoaded;
  end;

// *********************************************************************//
// The Class CoElement provides a Create and CreateRemote method to          
// create instances of the default interface IElement exposed by              
// the CoClass Element. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoElement = class
    class function Create: IElement;
    class function CreateRemote(const MachineName: string): IElement;
  end;

  TElementOnMouse = procedure(ASender: TObject; const target: IElement; eventType: Integer; 
                                                x: Integer; y: Integer; buttons: Integer; 
                                                keys: Integer) of object;
  TElementOnKey = procedure(ASender: TObject; const target: IElement; eventType: Integer; 
                                              code: Integer; keys: Integer) of object;
  TElementOnFocus = procedure(ASender: TObject; const target: IElement; eventType: Integer) of object;
  TElementOnTimer = procedure(ASender: TObject; timerId: Integer) of object;
  TElementOnControlEvent = procedure(ASender: TObject; const target: IElement; eventType: Integer; 
                                                       reason: Integer; const source: IElement) of object;
  TElementOnScroll = procedure(ASender: TObject; const target: IElement; eventType: Integer; 
                                                 pos: Integer; isVertical: WordBool) of object;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TElement
// Help String      : Element Class
// Default Interface: IElement
// Def. Intf. DISP? : No
// Event   Interface: _IElementEvents
// TypeFlags        : (2) CanCreate
// *********************************************************************//
  TElement = class(TOleServer)
  private
    FOnMouse: TElementOnMouse;
    FOnKey: TElementOnKey;
    FOnFocus: TElementOnFocus;
    FOnTimer: TElementOnTimer;
    FOnSize: TNotifyEvent;
    FOnControlEvent: TElementOnControlEvent;
    FOnScroll: TElementOnScroll;
    FIntf: IElement;
    function GetDefaultInterface: IElement;
  protected
    procedure InitServerData; override;
    procedure InvokeEvent(DispID: TDispID; var Params: TVariantArray); override;
    function Get_Tag: WideString;
    function Get_Value: OleVariant;
    procedure Set_Value(pVal: OleVariant);
    function Get_Attr(const name: WideString): OleVariant;
    procedure Set_Attr(const name: WideString; pVal: OleVariant);
    function Get_StyleAttr(const name: WideString): OleVariant;
    procedure Set_StyleAttr(const name: WideString; pVal: OleVariant);
    function Get_HELEMENT: Integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IElement);
    procedure Disconnect; override;
    function Select(const cssSelector: WideString): IElement;
    function SelectAll(const cssSelector: WideString): IElements;
    procedure Position(out x: Integer; out y: Integer; ofWhat: ElementBoxType; relTo: RelativeToType);
    procedure Dimension(out width: Integer; out height: Integer; ofWhat: ElementBoxType);
    function Call(const methodName: WideString; var params: PSafeArray): OleVariant;
    property DefaultInterface: IElement read GetDefaultInterface;
    property Tag: WideString read Get_Tag;
    property Value: OleVariant read Get_Value write Set_Value;
    property Attr[const name: WideString]: OleVariant read Get_Attr write Set_Attr;
    property StyleAttr[const name: WideString]: OleVariant read Get_StyleAttr write Set_StyleAttr;
    property HELEMENT: Integer read Get_HELEMENT;
  published
    property OnMouse: TElementOnMouse read FOnMouse write FOnMouse;
    property OnKey: TElementOnKey read FOnKey write FOnKey;
    property OnFocus: TElementOnFocus read FOnFocus write FOnFocus;
    property OnTimer: TElementOnTimer read FOnTimer write FOnTimer;
    property OnSize: TNotifyEvent read FOnSize write FOnSize;
    property OnControlEvent: TElementOnControlEvent read FOnControlEvent write FOnControlEvent;
    property OnScroll: TElementOnScroll read FOnScroll write FOnScroll;
  end;

// *********************************************************************//
// The Class CoElements provides a Create and CreateRemote method to          
// create instances of the default interface IElements exposed by              
// the CoClass Elements. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoElements = class
    class function Create: IElements;
    class function CreateRemote(const MachineName: string): IElements;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TElements
// Help String      : Elements Class
// Default Interface: IElements
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
  TElements = class(TOleServer)
  private
    FIntf: IElements;
    function GetDefaultInterface: IElements;
  protected
    procedure InitServerData; override;
    function Get_Count: Integer;
    function Get_Item(index: Integer): IElement;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IElements);
    procedure Disconnect; override;
    property DefaultInterface: IElements read GetDefaultInterface;
    property Count: Integer read Get_Count;
    property Item[index: Integer]: IElement read Get_Item;
  published
  end;

procedure Register;

resourcestring
  dtlServerPage = 'ActiveX';

  dtlOcxPage = 'ActiveX';

implementation

uses System.Win.ComObj;

procedure TSciter.CNKeyDown(var Message: TMessage); 
begin 
end;

procedure TSciter.InitControlData;
const
  CEventDispIDs: array [0..3] of DWORD = (
    $00000001, $00000002, $00000003, $00000004);
  CControlData: TControlData2 = (
    ClassID:      '{99829A7E-007E-4F60-AC36-31B646896593}';
    EventIID:     '{ED2316A7-3EB2-4C80-9146-600B408B08D8}';
    EventCount:   4;
    EventDispIDs: @CEventDispIDs;
    LicenseKey:   nil (*HR:$80004002*);
    Flags:        $00000000;
    Version:      500);
begin
  ControlData := @CControlData;
  TControlData2(CControlData).FirstEventOfs := UIntPtr(@@FOnonStdOut) - UIntPtr(Self);
end;

procedure TSciter.CreateControl;

  procedure DoCreate;
  begin
    FIntf := IUnknown(OleObject) as ISciter;
  end;

begin
  if FIntf = nil then DoCreate;
end;

function TSciter.GetControlInterface: ISciter;
begin
  CreateControl;
  Result := FIntf;
end;

function TSciter.Get_Root: IElement;
begin
  Result := DefaultInterface.Root;
end;

function TSciter.Get_Methods: IDispatch;
begin
  Result := DefaultInterface.Methods;
end;

procedure TSciter._Set_Methods(const pVal: IDispatch);
begin
  DefaultInterface.Methods := pVal;
end;

procedure TSciter.LoadHtml(const html: WideString; const baseUrl: WideString);
begin
  DefaultInterface.LoadHtml(html, baseUrl);
end;

procedure TSciter.LoadUrl(const urlToLoad: WideString);
begin
  DefaultInterface.LoadUrl(urlToLoad);
end;

function TSciter.Call(const name: WideString; var params: PSafeArray): OleVariant;
begin
  Result := DefaultInterface.Call(name, params);
end;

function TSciter.Eval(const script: WideString): OleVariant;
begin
  Result := DefaultInterface.Eval(script);
end;

procedure TSciter.DataReady(requestId: Integer; var data: Byte; dataLength: Integer);
begin
  DefaultInterface.DataReady(requestId, data, dataLength);
end;

class function CoElement.Create: IElement;
begin
  Result := CreateComObject(CLASS_Element) as IElement;
end;

class function CoElement.CreateRemote(const MachineName: string): IElement;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Element) as IElement;
end;

procedure TElement.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{53FB239D-7857-4F0D-9083-871D8C0EAE3A}';
    IntfIID:   '{645B0717-C0AB-424D-B513-F083AD486BF1}';
    EventIID:  '{2A8AAFD6-6E87-4967-BF6D-C3F6BB9B3BD1}';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TElement.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    ConnectEvents(punk);
    Fintf:= punk as IElement;
  end;
end;

procedure TElement.ConnectTo(svrIntf: IElement);
begin
  Disconnect;
  FIntf := svrIntf;
  ConnectEvents(FIntf);
end;

procedure TElement.DisConnect;
begin
  if Fintf <> nil then
  begin
    DisconnectEvents(FIntf);
    FIntf := nil;
  end;
end;

function TElement.GetDefaultInterface: IElement;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call "Connect" or "ConnectTo" before this operation');
  Result := FIntf;
end;

constructor TElement.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TElement.Destroy;
begin
  inherited Destroy;
end;

procedure TElement.InvokeEvent(DispID: TDispID; var Params: TVariantArray);
begin
  case DispID of
    -1: Exit;  // DISPID_UNKNOWN
    1: if Assigned(FOnMouse) then
         FOnMouse(Self,
                  IUnknown(TVarData(Params[0]).VPointer) as IElement {const IElement},
                  Params[1] {Integer},
                  Params[2] {Integer},
                  Params[3] {Integer},
                  Params[4] {Integer},
                  Params[5] {Integer});
    2: if Assigned(FOnKey) then
         FOnKey(Self,
                IUnknown(TVarData(Params[0]).VPointer) as IElement {const IElement},
                Params[1] {Integer},
                Params[2] {Integer},
                Params[3] {Integer});
    3: if Assigned(FOnFocus) then
         FOnFocus(Self,
                  IUnknown(TVarData(Params[0]).VPointer) as IElement {const IElement},
                  Params[1] {Integer});
    4: if Assigned(FOnTimer) then
         FOnTimer(Self, Params[0] {Integer});
    5: if Assigned(FOnSize) then
         FOnSize(Self);
    6: if Assigned(FOnControlEvent) then
         FOnControlEvent(Self,
                         IUnknown(TVarData(Params[0]).VPointer) as IElement {const IElement},
                         Params[1] {Integer},
                         Params[2] {Integer},
                         IUnknown(TVarData(Params[3]).VPointer) as IElement {const IElement});
    7: if Assigned(FOnScroll) then
         FOnScroll(Self,
                   IUnknown(TVarData(Params[0]).VPointer) as IElement {const IElement},
                   Params[1] {Integer},
                   Params[2] {Integer},
                   Params[3] {WordBool});
  end; {case DispID}
end;

function TElement.Get_Tag: WideString;
begin
  Result := DefaultInterface.Tag;
end;

function TElement.Get_Value: OleVariant;
begin
  Result := DefaultInterface.Value;
end;

procedure TElement.Set_Value(pVal: OleVariant);
begin
  DefaultInterface.Value := pVal;
end;

function TElement.Get_Attr(const name: WideString): OleVariant;
begin
  Result := DefaultInterface.Attr[name];
end;

procedure TElement.Set_Attr(const name: WideString; pVal: OleVariant);
begin
  DefaultInterface.Attr[name] := pVal;
end;

function TElement.Get_StyleAttr(const name: WideString): OleVariant;
begin
  Result := DefaultInterface.StyleAttr[name];
end;

procedure TElement.Set_StyleAttr(const name: WideString; pVal: OleVariant);
begin
  DefaultInterface.StyleAttr[name] := pVal;
end;

function TElement.Get_HELEMENT: Integer;
begin
  Result := DefaultInterface.HELEMENT;
end;

function TElement.Select(const cssSelector: WideString): IElement;
begin
  Result := DefaultInterface.Select(cssSelector);
end;

function TElement.SelectAll(const cssSelector: WideString): IElements;
begin
  Result := DefaultInterface.SelectAll(cssSelector);
end;

procedure TElement.Position(out x: Integer; out y: Integer; ofWhat: ElementBoxType; 
                            relTo: RelativeToType);
begin
  DefaultInterface.Position(x, y, ofWhat, relTo);
end;

procedure TElement.Dimension(out width: Integer; out height: Integer; ofWhat: ElementBoxType);
begin
  DefaultInterface.Dimension(width, height, ofWhat);
end;

function TElement.Call(const methodName: WideString; var params: PSafeArray): OleVariant;
begin
  Result := DefaultInterface.Call(methodName, params);
end;

class function CoElements.Create: IElements;
begin
  Result := CreateComObject(CLASS_Elements) as IElements;
end;

class function CoElements.CreateRemote(const MachineName: string): IElements;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Elements) as IElements;
end;

procedure TElements.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{B1C8635C-12B4-40F7-8038-6134FC5D398F}';
    IntfIID:   '{C7171909-9F92-48D7-8691-EFB3390DEE55}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TElements.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as IElements;
  end;
end;

procedure TElements.ConnectTo(svrIntf: IElements);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TElements.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TElements.GetDefaultInterface: IElements;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call "Connect" or "ConnectTo" before this operation');
  Result := FIntf;
end;

constructor TElements.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TElements.Destroy;
begin
  inherited Destroy;
end;

function TElements.Get_Count: Integer;
begin
  Result := DefaultInterface.Count;
end;

function TElements.Get_Item(index: Integer): IElement;
begin
  Result := DefaultInterface.Item[index];
end;

procedure Register;
begin
  RegisterComponents(dtlOcxPage, [TSciter]);
  RegisterComponents(dtlServerPage, [TElement, TElements]);
end;

end.