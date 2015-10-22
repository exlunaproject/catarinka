unit WACefKeyboardHandler;

interface

uses
  WACefTypes,
  WACefInterfaces,
  WAcefOwns;

type
  TWACefKeyboardHandler = class(TCefKeyboardHandlerOwn)
  private
    FEvents: IChromiumEvents;
  protected
    // Called before a keyboard event is sent to the renderer. |event| contains
		// information about the keyboard event. |os_event| is the operating system
		// event message, if any. Return true (1) if the event was handled or false
		// (0) otherwise. If the event will be handled in on_key_event() as a keyboard
		// shortcut set |is_keyboard_shortcut| to true (1) and return false (0).
		function OnPreKeyEvent(const aBrowser: ICefBrowser; var aEvent: TCefKeyEvent; aOsEvent: TCefEventHandle;
      out aIsKeyboardShortcut: Boolean): Boolean; override;
		// Called after the renderer and JavaScript in the page has had a chance to
		// handle the event. |event| contains information about the keyboard event.
		// |os_event| is the operating system event message, if any. Return true (1)
		// if the keyboard event was handled or false (0) otherwise.
		function OnKeyEvent(const aBrowser: ICefBrowser; var aEvent: TCefKeyEvent; aOsEvent: TCefEventHandle): Boolean; override;
  public
    constructor Create(const aEvents: IChromiumEvents); reintroduce; virtual;
  end;


implementation

//..............................................................................TWACefKeyboardHandler
{Private section}
{Protected section}
// Called before a keyboard event is sent to the renderer. |event| contains
// information about the keyboard event. |os_event| is the operating system
// event message, if any. Return true (1) if the event was handled or false
// (0) otherwise. If the event will be handled in on_key_event() as a keyboard
// shortcut set |is_keyboard_shortcut| to true (1) and return false (0).
function TWACefKeyboardHandler.OnPreKeyEvent(const aBrowser: ICefBrowser; var aEvent: TCefKeyEvent; aOsEvent: TCefEventHandle;
  out aIsKeyboardShortcut: Boolean): Boolean;
begin
  Result := FEvents.doOnPreKeyEvent(
    aBrowser,
    aEvent,
    aOsEvent,
    aIsKeyboardShortcut
  );
end;

// Called after the renderer and JavaScript in the page has had a chance to
// handle the event. |event| contains information about the keyboard event.
// |os_event| is the operating system event message, if any. Return true (1)
// if the keyboard event was handled or false (0) otherwise.
function TWACefKeyboardHandler.OnKeyEvent(const aBrowser: ICefBrowser; var aEvent: TCefKeyEvent; aOsEvent: TCefEventHandle): Boolean;
begin
  Result := FEvents.doOnKeyEvent(
    aBrowser,
    aEvent,
    aOsEvent
  );
end;

{Public section}
constructor TWACefKeyboardHandler.Create(const aEvents: IChromiumEvents);
begin
  inherited Create;
  FEvents := aEvents;
end;

end.

