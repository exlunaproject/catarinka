unit WACefDownloadHandler;

interface

uses
  WACefTypes,
  WACefInterfaces,
  WAcefOwns;

type
  TWACefDownloadHandler = class(TCefDownloadHandlerOwn)
  private
    FEvents: IChromiumEvents;
  protected
    // Called before a download begins. |suggested_name| is the suggested name for
		// the download file. By default the download will be canceled. Execute
		// |callback| either asynchronously or in this function to continue the
		// download if desired. Do not keep a reference to |download_item| outside of
		// this function.
		procedure OnBeforeDownload(const aBrowser: ICefBrowser; const aDownloadItem: ICefDownloadItem;
      const aSuggestedName: ustring; const aCallback: ICefBeforeDownloadCallback); override;
		// Called when a download's status or progress information has been updated.
		// This may be called multiple times before and after on_before_download().
		// Execute |callback| either asynchronously or in this function to cancel the
		// download if desired. Do not keep a reference to |download_item| outside of
		// this function.
		procedure OnDownloadUpdated(const aBrowser: ICefBrowser; const aDownloadItem: ICefDownloadItem;
      const aCallback: ICefDownloadItemCallback); override;
  public
    constructor Create(const aEvents: IChromiumEvents); reintroduce; virtual;
  end;


implementation

//..............................................................................TWACefDownloadHandler
{Private section}
{Protected section}
// Called before a download begins. |suggested_name| is the suggested name for
// the download file. By default the download will be canceled. Execute
// |callback| either asynchronously or in this function to continue the
// download if desired. Do not keep a reference to |download_item| outside of
// this function.
procedure TWACefDownloadHandler.OnBeforeDownload(const aBrowser: ICefBrowser; const aDownloadItem: ICefDownloadItem;
  const aSuggestedName: ustring; const aCallback: ICefBeforeDownloadCallback);
begin
  FEvents.doOnBeforeDownload(
    aBrowser,
    aDownloadItem,
    aSuggestedName,
    aCallback
  );
end;

// Called when a download's status or progress information has been updated.
// This may be called multiple times before and after on_before_download().
// Execute |callback| either asynchronously or in this function to cancel the
// download if desired. Do not keep a reference to |download_item| outside of
// this function.
procedure TWACefDownloadHandler.OnDownloadUpdated(const aBrowser: ICefBrowser; const aDownloadItem: ICefDownloadItem;
  const aCallback: ICefDownloadItemCallback);
begin
  FEvents.doOnDownloadUpdated(
    aBrowser,
    aDownloadItem,
    aCallback
  );
end;

{Public section}
constructor TWACefDownloadHandler.Create(const aEvents: IChromiumEvents);
begin
  inherited Create;
  FEvents := aEvents;
end;

end.
