unit WACefContextMenuHandler;

interface

uses
  WACefTypes,
  WACefInterfaces,
  WAcefOwns;

type
  TWACefContextMenuHandler = class(TCefContextMenuHandlerOwn)
  private
    FEvents: IChromiumEvents;
  protected
    // Called before a context menu is displayed. |params| provides information
		// about the context menu state. |model| initially contains the default
		// context menu. The |model| can be cleared to show no context menu or
		// modified to show a custom menu. Do not keep references to |params| or
		// |model| outside of this callback.
		procedure OnBeforeContextMenu(const aBrowser: ICefBrowser; const aFrame: ICefFrame;
      const aParams: ICefContextMenuParams; const aModel: ICefMenuModel); override;
		// Called to execute a command selected from the context menu. Return true (1)
		// if the command was handled or false (0) for the default implementation. See
		// cef_menu_id_t for the command ids that have default implementations. All
		// user-defined command ids should be between MENU_ID_USER_FIRST and
		// MENU_ID_USER_LAST. |params| will have the same values as what was passed to
		// on_before_context_menu(). Do not keep a reference to |params| outside of
		// this callback.
		function OnContextMenuCommand(const aBrowser: ICefBrowser; const aFrame: ICefFrame;
      const aParams: ICefContextMenuParams; aCommandId: cint; aEventFlags: TCefEventFlags): Boolean; override;
		// Called when the context menu is dismissed irregardless of whether the menu
		// was NULL or a command was selected.
		procedure OnContextMenuDismissed(const aBrowser: ICefBrowser; const aFrame: ICefFrame); override;
  public
    constructor Create(const aEvents: IChromiumEvents); reintroduce; virtual;
  end;

implementation

//..............................................................................TWACefContextMenuHandler
{Private section}
{Protected section}
// Called before a context menu is displayed. |params| provides information
// about the context menu state. |model| initially contains the default
// context menu. The |model| can be cleared to show no context menu or
// modified to show a custom menu. Do not keep references to |params| or
// |model| outside of this callback.
procedure TWACefContextMenuHandler.OnBeforeContextMenu(const aBrowser: ICefBrowser; const aFrame: ICefFrame;
  const aParams: ICefContextMenuParams; const aModel: ICefMenuModel);
begin
  FEvents.doOnBeforeContextMenu(
    aBrowser,
    aFrame,
    aParams,
    aModel
  );
end;

// Called to execute a command selected from the context menu. Return true (1)
// if the command was handled or false (0) for the default implementation. See
// cef_menu_id_t for the command ids that have default implementations. All
// user-defined command ids should be between MENU_ID_USER_FIRST and
// MENU_ID_USER_LAST. |params| will have the same values as what was passed to
// on_before_context_menu(). Do not keep a reference to |params| outside of
// this callback.
function TWACefContextMenuHandler.OnContextMenuCommand(const aBrowser: ICefBrowser; const aFrame: ICefFrame;
  const aParams: ICefContextMenuParams; aCommandId: cint; aEventFlags: TCefEventFlags): Boolean;
begin
  Result := FEvents.doOnContextMenuCommand(
    aBrowser,
    aFrame,
    aParams,
    aCommandId,
    aEventFlags
  );
end;

// Called when the context menu is dismissed irregardless of whether the menu
// was NULL or a command was selected.
procedure TWACefContextMenuHandler.OnContextMenuDismissed(const aBrowser: ICefBrowser; const aFrame: ICefFrame);
begin
  FEvents.doOnContextMenuDismissed(
    aBrowser,
    aFrame
  );
end;

{Public section}
constructor TWACefContextMenuHandler.Create(const aEvents: IChromiumEvents);
begin
  inherited Create;
  FEvents := aEvents;
end;

end.
