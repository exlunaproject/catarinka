unit WACefDragHandler;

interface

uses
  WACefTypes,
  WACefInterfaces,
  WAcefOwns;

type
  TWACefDragHandler = class(TCefDragHandlerOwn)
  private
    FEvents: IChromiumEvents;
  protected
    // Called when an external drag event enters the browser window. |dragData|
		// contains the drag event data and |mask| represents the type of drag
		// operation. Return false (0) for default drag handling behavior or true (1)
		// to cancel the drag event.
    function OnDragEnter(const aBrowser: ICefBrowser; const aDragData: ICefDragData;
      aMask: TCefDragOperationsMask): Boolean; override;
  public
    constructor Create(const aEvents: IChromiumEvents); reintroduce; virtual;
  end;


implementation

//..............................................................................TWACefDragHandler
{Private section}
{Protected section}
// Called when an external drag event enters the browser window. |dragData|
// contains the drag event data and |mask| represents the type of drag
// operation. Return false (0) for default drag handling behavior or true (1)
// to cancel the drag event.
function TWACefDragHandler.OnDragEnter(const aBrowser: ICefBrowser; const aDragData: ICefDragData;
  aMask: TCefDragOperationsMask): Boolean;
begin
  Result := FEvents.doOnDragEnter(
    aBrowser,
    aDragData,
    aMask
  );
end;

{Public section}
constructor TWACefDragHandler.Create(const aEvents: IChromiumEvents);
begin
  inherited Create;
  FEvents := aEvents;
end;

end.
