unit WACefRenderHandler;

interface

uses
  WACefTypes,
  WACefInterfaces,
  WAcefOwns;

type
  TWACefRenderHandler = class(TCefRenderHandlerOwn)
  private
    FEvents: IChromiumEvents;
  protected
    // Called to retrieve the root window rectangle in screen coordinates. Return
		// true (1) if the rectangle was provided.
		function GetRootScreenRect(const aBrowser: ICefBrowser; var aRect: TCefRect): Boolean; override;
		// Called to retrieve the view rectangle which is relative to screen
		// coordinates. Return true (1) if the rectangle was provided.
		function GetViewRect(const aBrowser: ICefBrowser; var aRect: TCefRect): Boolean; override;
		// Called to retrieve the translation from view coordinates to actual screen
		// coordinates. Return true (1) if the screen coordinates were provided.
		function GetScreenPoint(const aBrowser: ICefBrowser; aViewX: cint; aViewY: cint;
      var aScreenX: cint; var aScreenY: cint): Boolean; override;
		// Called to allow the client to fill in the CefScreenInfo object with
		// appropriate values. Return true (1) if the |screen_info| structure has been
		// modified.
		//
		// If the screen info rectangle is left NULL the rectangle from GetViewRect
		// will be used. If the rectangle is still NULL or invalid popups may not be
		// drawn correctly.
		function GetScreenInfo(const aBrowser: ICefBrowser; out aScreenInfo: TCefScreenInfo): Boolean; override;
		// Called when the browser wants to show or hide the popup widget. The popup
		// should be shown if |show| is true (1) and hidden if |show| is false (0).
		procedure OnPopupShow(const aBrowser: ICefBrowser; aShow: Boolean); override;
		// Called when the browser wants to move or resize the popup widget. |rect|
		// contains the new location and size in view coordinates.
		procedure OnPopupSize(const aBrowser: ICefBrowser; var aRect: TCefRect); override;
		// Called when an element should be painted. Pixel values passed to this
		// function are scaled relative to view coordinates based on the value of
		// CefScreenInfo.device_scale_factor returned from GetScreenInfo. |type|
		// indicates whether the element is the view or the popup widget. |buffer|
		// contains the pixel data for the whole image. |dirtyRects| contains the set
		// of rectangles in pixel coordinates that need to be repainted. |buffer| will
		// be |width|*|height|*4 bytes in size and represents a BGRA image with an
		// upper-left origin.
		procedure OnPaint(const aBrowser: ICefBrowser; aType: TCefPaintElementType; aDirtyRectsCount: csize_t;
      const aDirtyRects: TCefRectArray; const aBuffer: cvoid; aWidth: cint; aHeight: cint); override;
		// Called when the browser's cursor has changed. If |type| is CT_CUSTOM then
		// |custom_cursor_info| will be populated with the custom cursor information.
		procedure OnCursorChange(const aBrowser: ICefBrowser; aCursor: TCefCursorHandle;
      aType: TCefCursorType; aCustomCursorInfo: TCefCursorInfo); override;
		// Called when the user starts dragging content in the web view. Contextual
		// information about the dragged content is supplied by |drag_data|. (|x|,
		// |y|) is the drag start location in screen coordinates. OS APIs that run a
		// system message loop may be used within the StartDragging call.
		//
		// Return false (0) to abort the drag operation. Don't call any of
		// cef_browser_host_t::DragSource*Ended* functions after returning false (0).
		//
		// Return true (1) to handle the drag operation. Call
		// cef_browser_host_t::DragSourceEndedAt and DragSourceSystemDragEnded either
		// synchronously or asynchronously to inform the web view that the drag
		// operation has ended.
		function StartDragging(const aBrowser: ICefBrowser; const aDragData: ICefDragData; aAllowedOps: TCefDragOperationsMask;
      aX: cint; aY: cint): Boolean; override;
		// Called when the web view wants to update the mouse cursor during a drag &
		// drop operation. |operation| describes the allowed operation (none, move,
		// copy, link).
		procedure UpdateDragCursor(const aBrowser: ICefBrowser; aOperation: TCefDragOperationsMask); override;
		// Called when the scroll offset has changed.
		procedure OnScrollOffsetChanged(const aBrowser: ICefBrowser; aX: cdouble; aY: cdouble); override;
  public
    constructor Create(const aEvents: IChromiumEvents); reintroduce; virtual;
  end;


implementation

//..............................................................................TWACefRenderHandler
{Private section}
{Protected section}
// Called to retrieve the root window rectangle in screen coordinates. Return
// true (1) if the rectangle was provided.
function TWACefRenderHandler.GetRootScreenRect(const aBrowser: ICefBrowser;
  var aRect: TCefRect): Boolean;
begin
  Result := FEvents.doOnGetRootScreenRect(
    aBrowser,
    aRect
  );
end;

// Called to retrieve the view rectangle which is relative to screen
// coordinates. Return true (1) if the rectangle was provided.
function TWACefRenderHandler.GetViewRect(const aBrowser: ICefBrowser;
  var aRect: TCefRect): Boolean;
begin
  Result := FEvents.doOnGetViewRect(
    aBrowser,
    aRect
  );
end;

// Called to retrieve the translation from view coordinates to actual screen
// coordinates. Return true (1) if the screen coordinates were provided.
function TWACefRenderHandler.GetScreenPoint(const aBrowser: ICefBrowser; aViewX: cint; aViewY: cint;
  var aScreenX: cint; var aScreenY: cint): Boolean;
begin
  Result := FEvents.doGetScreenPoint(
    aBrowser,
    aviewX,
    aviewY,
    ascreenX,
    ascreenY
  );
end;

// Called to allow the client to fill in the CefScreenInfo object with
// appropriate values. Return true (1) if the |screen_info| structure has been
// modified.
//
// If the screen info rectangle is left NULL the rectangle from GetViewRect
// will be used. If the rectangle is still NULL or invalid popups may not be
// drawn correctly.
function TWACefRenderHandler.GetScreenInfo(const aBrowser: ICefBrowser;
  out aScreenInfo: TCefScreenInfo): Boolean;
begin
  Result := FEvents.doOnGetScreenInfo(
    aBrowser,
    aScreenInfo
  );
end;

// Called when the browser wants to show or hide the popup widget. The popup
// should be shown if |show| is true (1) and hidden if |show| is false (0).
procedure TWACefRenderHandler.OnPopupShow(const aBrowser: ICefBrowser; aShow: Boolean);
begin
  FEvents.doOnPopupShow(
    aBrowser,
    aShow
  );
end;

// Called when the browser wants to move or resize the popup widget. |rect|
// contains the new location and size in view coordinates.
procedure TWACefRenderHandler.OnPopupSize(const aBrowser: ICefBrowser;
  var aRect: TCefRect);
begin
  FEvents.doOnPopupSize(
    aBrowser,
    aRect
  );
end;

// Called when an element should be painted. Pixel values passed to this
// function are scaled relative to view coordinates based on the value of
// CefScreenInfo.device_scale_factor returned from GetScreenInfo. |type|
// indicates whether the element is the view or the popup widget. |buffer|
// contains the pixel data for the whole image. |dirtyRects| contains the set
// of rectangles in pixel coordinates that need to be repainted. |buffer| will
// be |width|*|height|*4 bytes in size and represents a BGRA image with an
// upper-left origin.
procedure TWACefRenderHandler.OnPaint(const aBrowser: ICefBrowser; aType: TCefPaintElementType;
  aDirtyRectsCount: csize_t; const aDirtyRects: TCefRectArray; const aBuffer: cvoid; aWidth: cint; aHeight: cint);
begin
  FEvents.doOnPaint(
    aBrowser,
    aType,
    aDirtyRectsCount,
    aDirtyRects,
    aBuffer,
    aWidth,
    aHeight
  );
end;

// Called when the browser's cursor has changed. If |type| is CT_CUSTOM then
// |custom_cursor_info| will be populated with the custom cursor information.
procedure TWACefRenderHandler.OnCursorChange(const aBrowser: ICefBrowser; aCursor: TCefCursorHandle;
  aType: TCefCursorType; aCustomCursorInfo: TCefCursorInfo);
begin
  FEvents.doOnCursorChange(
    aBrowser,
    aCursor,
    aType,
    aCustomCursorInfo
  );
end;

// Called when the user starts dragging content in the web view. Contextual
// information about the dragged content is supplied by |drag_data|. (|x|,
// |y|) is the drag start location in screen coordinates. OS APIs that run a
// system message loop may be used within the StartDragging call.
//
// Return false (0) to abort the drag operation. Don't call any of
// cef_browser_host_t::DragSource*Ended* functions after returning false (0).
//
// Return true (1) to handle the drag operation. Call
// cef_browser_host_t::DragSourceEndedAt and DragSourceSystemDragEnded either
// synchronously or asynchronously to inform the web view that the drag
// operation has ended.
function TWACefRenderHandler.StartDragging(const aBrowser: ICefBrowser;
  const aDragData: ICefDragData; aAllowedOps: TCefDragOperationsMask; aX,
  aY: cint): Boolean;
begin
  Result := FEvents.doOnStartDragging(
    aBrowser,
    aDragData,
    aAllowedOps,
    aX,
    aY
  );
end;

// Called when the web view wants to update the mouse cursor during a drag &
// drop operation. |operation| describes the allowed operation (none, move,
// copy, link).
procedure TWACefRenderHandler.UpdateDragCursor(const aBrowser: ICefBrowser;
  aOperation: TCefDragOperationsMask);
begin
  FEvents.doOnUpdateDragCursor(
    aBrowser,
    aOperation
  );
end;

// Called when the scroll offset has changed.
procedure TWACefRenderHandler.OnScrollOffsetChanged(
  const aBrowser: ICefBrowser; aX, aY: cdouble);
begin
  FEvents.doOnScrollOffsetChanged(
    aBrowser,
    aX,
    aY
  );
end;

{Public section}
constructor TWACefRenderHandler.Create(const aEvents: IChromiumEvents);
begin
  inherited Create;
  FEvents := aEvents;
end;

end.
