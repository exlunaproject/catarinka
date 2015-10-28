unit WACefDisplayHandler;

interface

uses
  Classes,
  WACefTypes,
  WACefInterfaces,
  WAcefOwns;

type
  TWACefDisplayHandler = class(TCefDisplayHandlerOwn)
  private
    FEvents: IChromiumEvents;
  protected
    // Called when a frame's address has changed.
		procedure OnAddressChange(const aBrowser: ICefBrowser; const aFrame: ICefFrame; const aUrl: ustring); override;
		// Called when the page title changes.
		procedure OnTitleChange(const aBrowser: ICefBrowser; const aTitle: ustring); override;
		// Called when the page icon changes.
		procedure OnFaviconUrlchange(const aBrowser: ICefBrowser; aIconUrls: TStrings); override;
		// Called when the browser is about to display a tooltip. |text| contains the
		// text that will be displayed in the tooltip. To handle the display of the
		// tooltip yourself return true (1). Otherwise, you can optionally modify
		// |text| and then return false (0) to allow the browser to display the
		// tooltip. When window rendering is disabled the application is responsible
		// for drawing tooltips and the return value is ignored.
		function OnTooltip(const aBrowser: ICefBrowser; var aText: ustring): Boolean; override;
		// Called when the browser receives a status message. |value| contains the
		// text that will be displayed in the status message.
		procedure OnStatusMessage(const aBrowser: ICefBrowser; const aValue: ustring); override;
		// Called to display a console message. Return true (1) to stop the message
		// from being output to the console.
		function OnConsoleMessage(const aBrowser: ICefBrowser; const aMessage: ustring;
      const aSource: ustring; aLine: cint): Boolean; override;
  public
    constructor Create(const aEvents: IChromiumEvents); reintroduce; virtual;
  end;
  

implementation

//..............................................................................TWACefDisplayHandler
{Private section}
{Protected section}
// Called when a frame's address has changed.
procedure TWACefDisplayHandler.OnAddressChange(const aBrowser: ICefBrowser; const aFrame: ICefFrame; const aUrl: ustring);
begin
  FEvents.doOnAddressChange(
    aBrowser,
    aFrame,
    aUrl
  );
end;

// Called when the page title changes.
procedure TWACefDisplayHandler.OnTitleChange(const aBrowser: ICefBrowser; const aTitle: ustring);
begin
  FEvents.doOnTitleChange(
    aBrowser,
    aTitle
  );
end;

// Called when the page icon changes.
procedure TWACefDisplayHandler.OnFaviconUrlchange(const aBrowser: ICefBrowser; aIconUrls: TStrings);
begin
  FEvents.doOnFaviconUrlchange(
    aBrowser,
    aIconUrls
  );
end;

// Called when the browser is about to display a tooltip. |text| contains the
// text that will be displayed in the tooltip. To handle the display of the
// tooltip yourself return true (1). Otherwise, you can optionally modify
// |text| and then return false (0) to allow the browser to display the
// tooltip. When window rendering is disabled the application is responsible
// for drawing tooltips and the return value is ignored.
function TWACefDisplayHandler.OnTooltip(const aBrowser: ICefBrowser; var aText: ustring): Boolean;
begin
  Result := FEvents.doOnTooltip(
    aBrowser,
    aText
  );
end;

// Called when the browser receives a status message. |value| contains the
// text that will be displayed in the status message.
procedure TWACefDisplayHandler.OnStatusMessage(const aBrowser: ICefBrowser; const aValue: ustring);
begin
  FEvents.doOnStatusMessage(
    aBrowser,
    aValue
  );
end;

// Called to display a console message. Return true (1) to stop the message
// from being output to the console.
function TWACefDisplayHandler.OnConsoleMessage(const aBrowser: ICefBrowser; const aMessage: ustring;
  const aSource: ustring; aLine: cint): Boolean;
begin
  Result := FEvents.doOnConsoleMessage(
    aBrowser,
    aMessage,
    aSource,
    aLine
  );
end;

{Public section}
constructor TWACefDisplayHandler.Create(const aEvents: IChromiumEvents);
begin
  inherited Create;
  FEvents := aEvents;
end;

end.
