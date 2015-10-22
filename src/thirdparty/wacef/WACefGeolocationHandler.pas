unit WACefGeolocationHandler;

interface

uses
  WACefInterfaces,
  WACefTypes,
  WACefOwns;

type
  TWACefGeolocationHandler = class(TCefGeolocationHandlerOwn)
  private
    FEvents: IChromiumEvents;
  protected
    // Called when a page requests permission to access geolocation information.
		// |requesting_url| is the URL requesting permission and |request_id| is the
		// unique ID for the permission request. Return true (1) and call
		// cef_geolocation_callback_t::cont() either in this function or at a later
		// time to continue or cancel the request. Return false (0) to cancel the
		// request immediately.
		function OnRequestGeolocationPermission(const aBrowser: ICefBrowser; const aRequestingUrl: ustring;
      aRequestId: cint; const aCallback: ICefGeolocationCallback): Boolean; override;
		// Called when a geolocation access request is canceled. |requesting_url| is
		// the URL that originally requested permission and |request_id| is the unique
		// ID for the permission request.
		procedure OnCancelGeolocationPermission(const aBrowser: ICefBrowser; const aRequestingUrl: ustring;
      aRequestId: cint); override;
  public
    constructor Create(const aEvents: IChromiumEvents); reintroduce; virtual;
  end;

implementation

//..............................................................................TWACefGeolocationHandler
{Private section}
{Protected section}
// Called when a page requests permission to access geolocation information.
// |requesting_url| is the URL requesting permission and |request_id| is the
// unique ID for the permission request. Return true (1) and call
// cef_geolocation_callback_t::cont() either in this function or at a later
// time to continue or cancel the request. Return false (0) to cancel the
// request immediately.
function TWACefGeolocationHandler.OnRequestGeolocationPermission(const aBrowser: ICefBrowser; const aRequestingUrl: ustring;
  aRequestId: Integer; const aCallback: ICefGeolocationCallback): Boolean;
begin
  Result := FEvents.doOnRequestGeolocationPermission(
    aBrowser,
    aRequestingUrl,
    aRequestId,
    aCallback
  );
end;

// Called when a geolocation access request is canceled. |requesting_url| is
// the URL that originally requested permission and |request_id| is the unique
// ID for the permission request.
procedure TWACefGeolocationHandler.OnCancelGeolocationPermission(const aBrowser: ICefBrowser; const aRequestingUrl: ustring;
  aRequestId: Integer);
begin
  FEvents.doOnCancelGeolocationPermission(
    aBrowser,
    aRequestingUrl,
    aRequestId
  );
end;

{Public section}
constructor TWACefGeolocationHandler.Create(const aEvents: IChromiumEvents);
begin
  inherited Create;
  FEvents := aEvents;
end;

end.
