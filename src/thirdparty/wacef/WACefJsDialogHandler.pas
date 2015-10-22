unit WACefJsDialogHandler;

interface

uses
  WACefTypes,
  WACefInterfaces,
  WAcefOwns;

type
  TWACefJsDialogHandler = class(TCefJsDialogHandlerOwn)
  private
    FEvents: IChromiumEvents;
  protected
    // Called to run a JavaScript dialog. The |default_prompt_text| value will be
		// specified for prompt dialogs only. Set |suppress_message| to true (1) and
		// return false (0) to suppress the message (suppressing messages is
		// preferable to immediately executing the callback as this is used to detect
		// presumably malicious behavior like spamming alert messages in
		// onbeforeunload). Set |suppress_message| to false (0) and return false (0)
		// to use the default implementation (the default implementation will show one
		// modal dialog at a time and suppress any additional dialog requests until
		// the displayed dialog is dismissed). Return true (1) if the application will
		// use a custom dialog or if the callback has been executed immediately.
		// Custom dialogs may be either modal or modeless. If a custom dialog is used
		// the application must execute |callback| once the custom dialog is
		// dismissed.
    function OnJsdialog(const aBrowser: ICefBrowser; const aOriginUrl: ustring; const aAcceptLang: ustring; aDialogType: TCefJsdialogType; const aMessageText: ustring; const aDefaultPromptText: ustring; const aCallback: ICefJsdialogCallback; out aSuppressMessage: Boolean): Boolean; override;
		// Called to run a dialog asking the user if they want to leave a page. Return
		// false (0) to use the default dialog implementation. Return true (1) if the
		// application will use a custom dialog or if the callback has been executed
		// immediately. Custom dialogs may be either modal or modeless. If a custom
		// dialog is used the application must execute |callback| once the custom
		// dialog is dismissed.
		function OnBeforeUnloadDialog(const aBrowser: ICefBrowser; const aMessageText: ustring; aIsReload: Boolean; const aCallback: ICefJsdialogCallback): Boolean; override;
		// Called to cancel any pending dialogs and reset any saved dialog state. Will
		// be called due to events like page navigation irregardless of whether any
		// dialogs are currently pending.
		procedure OnResetDialogState(const aBrowser: ICefBrowser); override;
		// Called when the default implementation dialog is closed.
		procedure OnDialogClosed(const aBrowser: ICefBrowser); override;
  public
    constructor Create(const aEvents: IChromiumEvents); reintroduce; virtual;
  end;


implementation

//..............................................................................TWACefJsDialogHandler
{Private section}
{Protected section}
// Called to run a JavaScript dialog. The |default_prompt_text| value will be
// specified for prompt dialogs only. Set |suppress_message| to true (1) and
// return false (0) to suppress the message (suppressing messages is
// preferable to immediately executing the callback as this is used to detect
// presumably malicious behavior like spamming alert messages in
// onbeforeunload). Set |suppress_message| to false (0) and return false (0)
// to use the default implementation (the default implementation will show one
// modal dialog at a time and suppress any additional dialog requests until
// the displayed dialog is dismissed). Return true (1) if the application will
// use a custom dialog or if the callback has been executed immediately.
// Custom dialogs may be either modal or modeless. If a custom dialog is used
// the application must execute |callback| once the custom dialog is
// dismissed.
function TWACefJsDialogHandler.OnJsdialog(const aBrowser: ICefBrowser; const aOriginUrl: ustring; const aAcceptLang: ustring; aDialogType: TCefJsdialogType; const aMessageText: ustring; const aDefaultPromptText: ustring; const aCallback: ICefJsdialogCallback; out aSuppressMessage: Boolean): Boolean;
begin
  Result := FEvents.doOnJsdialog(aBrowser, aOriginUrl, aAcceptLang, aDialogType,
    aMessageText, aDefaultPromptText, aCallback, aSuppressMessage);
end;

// Called to run a dialog asking the user if they want to leave a page. Return
// false (0) to use the default dialog implementation. Return true (1) if the
// application will use a custom dialog or if the callback has been executed
// immediately. Custom dialogs may be either modal or modeless. If a custom
// dialog is used the application must execute |callback| once the custom
// dialog is dismissed.
function TWACefJsDialogHandler.OnBeforeUnloadDialog(const aBrowser: ICefBrowser;
  const aMessageText: ustring; aIsReload: Boolean;
  const aCallback: ICefJsDialogCallback): Boolean;
begin
  Result := FEvents.doOnBeforeUnloadDialog(aBrowser, aMessageText, aIsReload, aCallback);
end;

// Called to cancel any pending dialogs and reset any saved dialog state. Will
// be called due to events like page navigation irregardless of whether any
// dialogs are currently pending.
procedure TWACefJsDialogHandler.OnDialogClosed(const aBrowser: ICefBrowser);
begin
  FEvents.doOnDialogClosed(aBrowser);
end;

// Called when the default implementation dialog is closed.
procedure TWACefJsDialogHandler.OnResetDialogState(const aBrowser: ICefBrowser);
begin
  FEvents.doOnResetDialogState(aBrowser);
end;

{Public section}
constructor TWACefJsDialogHandler.Create(const aEvents: IChromiumEvents);
begin
  inherited Create;
  FEvents := aEvents;
end;

end.

