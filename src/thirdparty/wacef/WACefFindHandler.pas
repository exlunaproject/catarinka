unit WACefFindHandler;

interface

uses
  WACefTypes,
  WACefInterfaces,
  WAcefOwns;

type
  TWACefFindHandler = class(TCefFindHandlerOwn)
  private
    FEvents: IChromiumEvents;
  protected
    // Called to report find results returned by cef_browser_host_t::find().
		// |identifer| is the identifier passed to find(), |count| is the number of
		// matches currently identified, |selectionRect| is the location of where the
		// match was found (in window coordinates), |activeMatchOrdinal| is the
		// current position in the search results, and |finalUpdate| is true (1) if
		// this is the last find notification.
		procedure OnFindResult(const aBrowser: ICefBrowser; aIdentifier: cint; aCount: cint;
      const aSelectionRect: TCefRect; aActiveMatchOrdinal: cint; aFinalUpdate: Boolean);
  public
    constructor Create(const aEvents: IChromiumEvents); reintroduce; virtual;
  end;


implementation

//..............................................................................TWACefFindHandler
{Private section}
{Protected section}
// Called to report find results returned by cef_browser_host_t::find().
// |identifer| is the identifier passed to find(), |count| is the number of
// matches currently identified, |selectionRect| is the location of where the
// match was found (in window coordinates), |activeMatchOrdinal| is the
// current position in the search results, and |finalUpdate| is true (1) if
// this is the last find notification.
procedure TWACefFindHandler.OnFindResult(const aBrowser: ICefBrowser; aIdentifier: cint; aCount: cint;
  const aSelectionRect: TCefRect; aActiveMatchOrdinal: cint; aFinalUpdate: Boolean);
begin
  FEvents.doOnFindResult(
    aBrowser,
    aIdentifier,
    aCount,
    aSelectionRect,
    aActiveMatchOrdinal,
    aFinalUpdate
  );
end;

{Public section}
constructor TWACefFindHandler.Create(const aEvents: IChromiumEvents);
begin
  inherited Create;
  FEvents := aEvents;
end;

end.

