unit WACefLoadHandler;

interface

uses
  WACefTypes,
  WACefInterfaces,
  WAcefOwns;

type
  TWACefLoadHandler = class(TCefLoadHandlerOwn)
  private
    FEvents: IChromiumEvents;
  protected
    // Called when the loading state has changed. This callback will be executed
		// twice -- once when loading is initiated either programmatically or by user
		// action, and once when loading is terminated due to completion, cancellation
		// of failure.
		procedure OnLoadingStateChange(const aBrowser: ICefBrowser; aIsLoading: Boolean;
      aCanGoBack: Boolean; aCanGoForward: Boolean); override;
		// Called when the browser begins loading a frame. The |frame| value will
		// never be NULL -- call the is_main() function to check if this frame is the
		// main frame. Multiple frames may be loading at the same time. Sub-frames may
		// start or continue loading after the main frame load has ended. This
		// function may not be called for a particular frame if the load request for
		// that frame fails. For notification of overall browser load status use
		// OnLoadingStateChange instead.
		procedure OnLoadStart(const aBrowser: ICefBrowser; const aFrame: ICefFrame); override;
		// Called when the browser is done loading a frame. The |frame| value will
		// never be NULL -- call the is_main() function to check if this frame is the
		// main frame. Multiple frames may be loading at the same time. Sub-frames may
		// start or continue loading after the main frame load has ended. This
		// function will always be called for all frames irrespective of whether the
		// request completes successfully.
		procedure OnLoadEnd(const aBrowser: ICefBrowser; const aFrame: ICefFrame; aHttpStatusCode: cint); override;
		// Called when the resource load for a navigation fails or is canceled.
		// |errorCode| is the error code number, |errorText| is the error text and
		// |failedUrl| is the URL that failed to load. See net\base\net_error_list.h
		// for complete descriptions of the error codes.
		procedure OnLoadError(const aBrowser: ICefBrowser; const aFrame: ICefFrame; aErrorCode: TCefErrorcode;
      const aErrorText: ustring; const aFailedUrl: ustring); override;
  public
    constructor Create(const aEvents: IChromiumEvents); reintroduce; virtual;
  end;


implementation

//..............................................................................TWACefLoadHandler
{Private section}
{Protected section}
// Called when the loading state has changed. This callback will be executed
// twice -- once when loading is initiated either programmatically or by user
// action, and once when loading is terminated due to completion, cancellation
// of failure.
procedure TWACefLoadHandler.OnLoadingStateChange(const aBrowser: ICefBrowser; aIsLoading: Boolean;
  aCanGoBack: Boolean; aCanGoForward: Boolean);
begin
  FEvents.doOnLoadingStateChange(
    aBrowser,
    aIsLoading,
    aCanGoBack,
    aCanGoForward
  );
end;

// Called when the browser begins loading a frame. The |frame| value will
// never be NULL -- call the is_main() function to check if this frame is the
// main frame. Multiple frames may be loading at the same time. Sub-frames may
// start or continue loading after the main frame load has ended. This
// function may not be called for a particular frame if the load request for
// that frame fails. For notification of overall browser load status use
// OnLoadingStateChange instead.
procedure TWACefLoadHandler.OnLoadStart(const aBrowser: ICefBrowser;
  const aFrame: ICefFrame);
begin
  FEvents.doOnLoadStart(
    aBrowser,
    aFrame
  );
end;

// Called when the browser is done loading a frame. The |frame| value will
// never be NULL -- call the is_main() function to check if this frame is the
// main frame. Multiple frames may be loading at the same time. Sub-frames may
// start or continue loading after the main frame load has ended. This
// function will always be called for all frames irrespective of whether the
// request completes successfully.
procedure TWACefLoadHandler.OnLoadEnd(const aBrowser: ICefBrowser;
  const aFrame: ICefFrame; aHttpStatusCode: cint);
begin
  FEvents.doOnLoadEnd(
    aBrowser,
    aFrame,
    aHttpStatusCode
  );
end;

// Called when the resource load for a navigation fails or is canceled.
// |errorCode| is the error code number, |errorText| is the error text and
// |failedUrl| is the URL that failed to load. See net\base\net_error_list.h
// for complete descriptions of the error codes.
procedure TWACefLoadHandler.OnLoadError(const aBrowser: ICefBrowser; const aFrame: ICefFrame; aErrorCode: TCefErrorcode;
  const aErrorText, aFailedUrl: ustring);
begin
  FEvents.doOnLoadError(
    aBrowser,
    aFrame,
    aErrorCode,
    aErrorText,
    aFailedUrl
  );
end;

{Public section}
constructor TWACefLoadHandler.Create(const aEvents: IChromiumEvents);
begin
  inherited Create;
  FEvents := aEvents;
end;

end.
