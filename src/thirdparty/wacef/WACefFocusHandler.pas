unit WACefFocusHandler;

interface

uses
  WACefTypes,
  WACefInterfaces,
  WAcefOwns;

type
  TWACefFocusHandler = class(TCefFocusHandlerOwn)
  private
    FEvents: IChromiumEvents;
  protected
    // Called when the browser component is about to loose focus. For instance, if
		// focus was on the last HTML element and the user pressed the TAB key. |next|
		// will be true (1) if the browser is giving focus to the next component and
		// false (0) if the browser is giving focus to the previous component.
		procedure OnTakeFocus(const aBrowser: ICefBrowser; aNext: Boolean); override;
		// Called when the browser component is requesting focus. |source| indicates
		// where the focus request is originating from. Return false (0) to allow the
		// focus to be set or true (1) to cancel setting the focus.
		function OnSetFocus(const aBrowser: ICefBrowser; aSource: TCefFocusSource): Boolean; override;
		// Called when the browser component has received focus.
		procedure OnGotFocus(const aBrowser: ICefBrowser); override;
  public
    constructor Create(const aEvents: IChromiumEvents); reintroduce; virtual;
  end;


implementation

//..............................................................................TWACefFocusHandler
{Private section}
{Protected section}
// Called when the browser component is about to loose focus. For instance, if
// focus was on the last HTML element and the user pressed the TAB key. |next|
// will be true (1) if the browser is giving focus to the next component and
// false (0) if the browser is giving focus to the previous component.
procedure TWACefFocusHandler.OnGotFocus(const aBrowser: ICefBrowser);
begin
  FEvents.doOnGotFocus(aBrowser);
end;

// Called when the browser component is requesting focus. |source| indicates
// where the focus request is originating from. Return false (0) to allow the
// focus to be set or true (1) to cancel setting the focus.
function TWACefFocusHandler.OnSetFocus(const aBrowser: ICefBrowser;
  aSource: TCefFocusSource): Boolean;
begin
  Result := FEvents.doOnSetFocus(aBrowser, aSource);
end;

// Called when the browser component has received focus.
procedure TWACefFocusHandler.OnTakeFocus(const aBrowser: ICefBrowser;
  aNext: Boolean);
begin
  FEvents.doOnTakeFocus(aBrowser, aNext);
end;

{Public section}
constructor TWACefFocusHandler.Create(const aEvents: IChromiumEvents);
begin
  inherited Create;
  FEvents := aEvents;
end;

end.

