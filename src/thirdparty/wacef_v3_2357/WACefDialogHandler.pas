unit WACefDialogHandler;

interface

uses
  Classes,
  WACefTypes,
  WACefInterfaces,
  WAcefOwns;

type
  TWACefDialogHandler = class(TCefDialogHandlerOwn)
  private
    FEvents: IChromiumEvents;
  protected
    // Called to run a file chooser dialog. |mode| represents the type of dialog
		// to display. |title| to the title to be used for the dialog and may be NULL
		// to show the default title ("Open" or "Save" depending on the mode).
		// |default_file_path| is the path with optional directory and/or file name
		// component that should be initially selected in the dialog. |accept_filters|
		// are used to restrict the selectable file types and may any combination of
		// (a) valid lower-cased MIME types (e.g. "text/*" or "image/*"), (b)
		// individual file extensions (e.g. ".txt" or ".png"), or (c) combined
		// description and file extension delimited using "|" and ";" (e.g. "Image
		// Types|.png;.gif;.jpg"). |selected_accept_filter| is the 0-based index of
		// the filter that should be selected by default. To display a custom dialog
		// return true (1) and execute |callback| either inline or at a later time. To
		// display the default dialog return false (0).
		function OnFileDialog(const aBrowser: ICefBrowser; aMode: TCefFileDialogMode; const aTitle: ustring;
      const aDefaultFilePath: ustring; aAcceptFilters: TStrings; aSelectedAcceptFilter: cint;
      const aCallback: ICefFileDialogCallback): Boolean; override;
  public
    constructor Create(const aEvents: IChromiumEvents); reintroduce; virtual;
  end;

implementation

//..............................................................................TWACefDialogHandler
{Private section}
{Protected section}
// Called to run a file chooser dialog. |mode| represents the type of dialog
// to display. |title| to the title to be used for the dialog and may be NULL
// to show the default title ("Open" or "Save" depending on the mode).
// |default_file_path| is the path with optional directory and/or file name
// component that should be initially selected in the dialog. |accept_filters|
// are used to restrict the selectable file types and may any combination of
// (a) valid lower-cased MIME types (e.g. "text/*" or "image/*"), (b)
// individual file extensions (e.g. ".txt" or ".png"), or (c) combined
// description and file extension delimited using "|" and ";" (e.g. "Image
// Types|.png;.gif;.jpg"). |selected_accept_filter| is the 0-based index of
// the filter that should be selected by default. To display a custom dialog
// return true (1) and execute |callback| either inline or at a later time. To
// display the default dialog return false (0).
function TWACefDialogHandler.OnFileDialog(const aBrowser: ICefBrowser; aMode: TCefFileDialogMode; const aTitle: ustring;
  const aDefaultFilePath: ustring; aAcceptFilters: TStrings; aSelectedAcceptFilter: cint;
  const aCallback: ICefFileDialogCallback): Boolean;
begin
  Result := FEvents.doOnFileDialog(
    aBrowser,
    aMode,
    aTitle,
    aDefaultFilePath,
    aAcceptFilters,
    aSelectedAcceptFilter,
    aCallback
  );
end;

{Public section}
constructor TWACefDialogHandler.Create(const aEvents: IChromiumEvents);
begin
  inherited Create;
  FEvents := aEvents;
end;

end.
