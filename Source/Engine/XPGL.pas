unit XPGL;

interface

uses
  {$IFDEF Win32} XPEngine32 {$ELSE} XPEngine64 {$ENDIF},
  Windows, Classes, Controls, StdCtrls, Graphics, SysUtils, Types,
  XPRoutine, dglOpenGL;

type
  TGLRGBFloat = packed record
  public
    function AsText:string;
    procedure Clear;
    case Integer of
      0: (R, G, B: GLFloat);
      1: (RGB: array[0..2] of GLFloat);
  end;

  TGLRGBAFloat = packed record
  public
    procedure Clear;
    procedure Assign(AArray: array of GLFloat); overload;
    procedure Assign(AR, AG, AB, AA: GLFloat); overload;
    case Integer of
      0: (R, G, B, A: GLFloat);
      1: (RGB: TGLRGBFloat);
      2: (RGBA: array[0..3] of GLFloat);
  end;

  TGLXYZFloat = packed record
  public
    procedure Clear;
    procedure Assign(AArray: array of GLFloat); overload; 
    procedure Assign(AX, AY, AZ: GLFloat); overload; inline;
    procedure Assign(AValue: GLFloat); overload; inline;
    procedure MoveBy(AX, AY, AZ: GLFloat); overload; inline;
    procedure MoveBy(APosition: TGLXYZFloat); overload; inline;
    procedure ReOrder(AXOrder, AYOrder, AZOrder: Integer);
    class operator Equal(AValue1, AValue2: TGLXYZFloat): Boolean;
    class operator Add(AValue1, AValue2: TGLXYZFloat): TGLXYZFloat;
    class operator Subtract(AValue1, AValue2: TGLXYZFloat): TGLXYZFloat;
    class operator Negative(AValue: TGLXYZFloat): TGLXYZFloat;
    class operator Divide(AValue1: TGLXYZFloat; AValue2: GLFloat): TGLXYZFloat;
    class operator Divide(AValue1: GLFloat; AValue2: TGLXYZFloat): TGLXYZFloat;
    case Integer of
      0: (X, Y, Z: GLFloat);
      1: (Axis: array[0..2] of GLFloat);
  end;

  TGLXYZWFloat = packed record
  public
    procedure Clear;
    procedure Assign(AArray: array of GLFloat); overload;
    procedure Assign(AX, AY, AZ: GLFloat); overload;
    procedure Assign(AX, AY, AZ, AW: GLFloat); overload;
    procedure MoveBy(AX, AY, AZ: GLFloat; AW: GLFloat = 0.0); overload; inline;
    procedure MoveBy(APosition: TGLXYZWFloat); overload; inline;
    procedure MoveBy(APosition: TGLXYZFloat); overload; inline;
    procedure ScaleBy(AScale: TGLXYZFloat); inline;
    procedure RotateBy(ARotate: TGLXYZFloat); inline;
    procedure ReOrder(AXOrder, AYOrder, AZOrder, AWOrder: Integer); overload;
    procedure ReOrder(AXOrder, AYOrder, AZOrder: Integer); overload;
    function Compare(AValue: TGLXYZWFloat): Integer;
    class operator Add(AValue1, AValue2: TGLXYZWFloat): TGLXYZWFloat; overload;
    class operator Add(AValue1: TGLXYZWFloat; AValue2: TGLXYZFloat): TGLXYZWFloat; overload;
    class operator Subtract(AValue1, AValue2: TGLXYZWFloat): TGLXYZWFloat; overload;
    class operator Subtract(AValue1: TGLXYZWFloat; AValue2: TGLXYZFloat): TGLXYZWFloat; overload;
    class operator Negative(AValue: TGLXYZWFloat): TGLXYZWFloat;
    class operator Equal(AValue1, AValue2: TGLXYZWFloat): Boolean;
    case Integer of
      0: (X, Y, Z, W: GLFloat);
      1: (XYZ: TGLXYZFloat);
      2: (Axis: array[0..3] of GLFloat);
  end;
  TArrayOfGLXYZWFloat = array of TGLXYZWFloat;

  TXPGL = class
  private
    FRC: HGLRC;   // Rendering Context
    FWindow: TWinControl;
    FDisplayCanvas: TCanvas;
    FControlCanvas: TControlCanvas;
    FClipRect, FDrawRect: TRect;
    FDepth: Single;
    FLock: TRTLCriticalSection;
    procedure SetDrawRect(ARect: TRect);
    procedure SetDisplayCanvas(ACanvas: TCanvas);
    procedure SetWindow(AWindow: TWinControl);
    function GetHeight: Integer;
    function GetWidth: Integer;
    procedure CanvasChanged;
    function GetDrawBottom: Integer;
    function GetDrawLeft: Integer;
    function GetDrawRight: Integer;
    function GetDrawTop: Integer;
    procedure SetDrawBottom(const Value: Integer);
    procedure SetDrawLeft(const Value: Integer);
    procedure SetDrawRight(const Value: Integer);
    procedure SetDrawTop(const Value: Integer);
  public
    CurrentPos: TPoint;
    BrushColor, MonoColor, LineColor: TColor32;
    LineWidth: Integer;
    LineStipple: Word;
    constructor Create; overload;
    constructor Create(ACanvas: TCanvas); overload;
    constructor Create(AWindow: TWinControl); overload;
    destructor Destroy; override;
    procedure Initialize;
    procedure SetOrtho; overload;
    procedure SetOrtho(AWidth, AHeight: Integer); overload;
    procedure SetPerspective; overload;
    procedure SetPerspective(AWidth, AHeight: Integer); overload;
    procedure SetBounds(ARect: TRect);
    procedure SetVSync(ASync: Boolean);
    procedure SwapBuffer;
    procedure Activate;
    procedure Clear;
    procedure ClearBounds;
    procedure FillRect(drect: TRect); overload;
    procedure FillRect; overload;
    procedure Rectangle(ARect: TRect);
    procedure MoveTo(X, Y: Integer);
    procedure LineTo(X, Y: Integer);
    procedure Polygon(APoints: array of TPoint);
    procedure Lock;
    procedure Unlock;
    property DisplayCanvas: TCanvas read FDisplayCanvas write SetDisplayCanvas;
    property Window: TWinControl read FWindow write SetWindow;
    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
    property ClipRect: TRect read FClipRect;
    property DrawRect: TRect read FDrawRect write SetDrawRect;
    property DrawLeft: Integer read GetDrawLeft write SetDrawLeft;
    property DrawTop: Integer read GetDrawTop write SetDrawTop;
    property DrawRight: Integer read GetDrawRight write SetDrawRight;
    property DrawBottom: Integer read GetDrawBottom write SetDrawBottom;
    property Depth: Single read FDepth write FDepth;
  end;

function GLRGBAFloat(AR, AG, AB, AA: GLFloat): TGLRGBAFLoat; inline; overload;
function GLRGBFloat(AR, AG, AB: GLFloat): TGLRGBFLoat; inline;
function GLRGBAFloat(ARGB: TGLRGBFloat; AA: GLFloat): TGLRGBAFloat; inline; overload;
function GLXYZFloat(AX, AY, AZ: GLFloat): TGLXYZFLoat; inline;

implementation

function GLRGBAFloat(AR, AG, AB, AA: GLFloat): TGLRGBAFLoat;
begin
  Result.R := AR;
  Result.G := AG;
  Result.B := AB;
  Result.A := AA;
end;

function GLRGBFloat(AR, AG, AB: GLFloat): TGLRGBFLoat;
begin
  Result.R := AR;
  Result.G := AG;
  Result.B := AB;
end;

function GLXYZFloat(AX, AY, AZ: GLFloat): TGLXYZFLoat;
begin
  Result.X := AX;
  Result.Y := AY;
  Result.Z := AZ;
end;

function GLRGBAFloat(ARGB: TGLRGBFloat; AA: GLFloat): TGLRGBAFloat;
begin
  Result.RGB := ARGB;
  Result.A := AA;
end;

procedure TXPGL.Activate;
begin
  wglMakeCurrent(FDisplayCanvas.Handle, FRC);
end;

procedure TXPGL.CanvasChanged;
var
  pfd: TPIXELFORMATDESCRIPTOR;
  pf: Integer;
begin
  if FDisplayCanvas = nil then Exit;
  with FDisplayCanvas do
  begin
    pfd.nSize := SizeOf(pfd);
    pfd.nVersion := 1;
    pfd.dwFlags := PFD_DRAW_TO_WINDOW or PFD_SUPPORT_OPENGL or PFD_DOUBLEBUFFER;
    pfd.iPixelType := PFD_TYPE_RGBA;    // PFD_TYPE_RGBA or PFD_TYPEINDEX
    pfd.cColorBits := 32;

    pf := ChoosePixelFormat(Handle, @pfd); // Returns format that most closely matches above pixel format
    SetPixelFormat(Handle, pf, @pfd);

    FRC := wglCreateContext(Handle);       // Rendering Context = window-glCreateContext
    wglMakeCurrent(Handle, FRC);           // Make the DC (Form1) the rendering Context

    Initialize;
    if FWindow <> nil then
      SetBounds(FWindow.ClientRect)
    else SetBounds(FDisplayCanvas.ClipRect);
  end;
end;

procedure TXPGL.Clear;
begin
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);  // Clear The Screen And The Depth Buffer
  glLoadIdentity;                                       // Reset The View
  FDepth := 0.0;
end;

procedure TXPGL.ClearBounds;
begin
  FDrawRect := FClipRect;
end;

constructor TXPGL.Create(ACanvas: TCanvas);
begin
  Create;
  SetDisplayCanvas(ACanvas);
end;

constructor TXPGL.Create(AWindow: TWinControl);
begin
  Create;
  SetWindow(AWindow);
end;

constructor TXPGL.Create;
begin
  FDepth := 0.0;
  FClipRect := Rect(0, 0, 0, 0);
  FDrawRect := FClipRect;
  BrushColor := c32Black;
  MonoColor := c32White;
  LineColor := c32White;
  LineWidth := 1;
  LineStipple := $FFFF;
  CurrentPos := Point(0, 0);
  FDisplayCanvas := nil;
  FControlCanvas := nil;
  FWindow := nil;
  InitializeCriticalSection(FLock);
end;

destructor TXPGL.Destroy;
begin
  DeleteCriticalSection(FLock);
  wglMakeCurrent(0, 0);
  wglDeleteContext(FRC);
  SetDisplayCanvas(nil);
end;

procedure TXPGL.Initialize;
begin
  glClearColor(0.0, 0.0, 0.0, 0.0); // Black Background
  glShadeModel(GL_SMOOTH);          // Enables Smooth Color Shading
  glClearDepth(1.0);                // Depth Buffer Setup
  glEnable(GL_DEPTH_TEST);          // Enable Depth Buffer
  glDepthFunc(GL_LESS);             // The Type Of Depth Test To Do
  glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);  //Realy Nice perspective calculations
  glDisable(GL_TEXTURE_2D);          // Disable Texture Mapping
end;

procedure TXPGL.LineTo(X, Y: Integer);
begin
  SetOrtho;

  glDisable(GL_TEXTURE_2D);
  glDisable(GL_DEPTH_TEST);
  glDisable(GL_BLEND);
  with LineColor do glColor4f(R/255, G/255, B/255, A/255);

  glPushMatrix();
  glTranslatef(0, Height, FDepth); // x, y, zorder
  glLineWidth(LineWidth / Height);

  glBegin(GL_LINE);
  try
    glVertex2f(CurrentPos.X, CurrentPos.Y);
    glVertex2f(X, Y);
  finally
    glEnd;
  end;

  CurrentPos := Point(X, Y);
  glPopMatrix();
//    SetPerspective;
end;

procedure TXPGL.Lock;
begin
  EnterCriticalSection(FLock);
end;

procedure TXPGL.MoveTo(X, Y: Integer);
begin
  CurrentPos := Point(X, Y);
end;

procedure TXPGL.Polygon(APoints: array of TPoint);
var
  n: Integer;
begin
  SetOrtho;

  glDisable(GL_TEXTURE_2D);
  glDisable(GL_DEPTH_TEST);
  glDisable(GL_BLEND);
  with BrushColor do glColor4f(R/255, G/255, B/255, A/255);

  glPushMatrix();
  glTranslatef(0, Height, FDepth); // x, y, zorder
  glLineWidth(LineWidth);

  glBegin(GL_POLYGON);
  try
    for n := 0 to Length(APoints) - 1 do
    begin
      with APoints[n] do glVertex2f(X, Y);
    end;
  finally
    glEnd;
  end;

  glPopMatrix();
//    SetPerspective;
end;

procedure TXPGL.Rectangle(ARect: TRect);
begin
  SwapIntOrder(ARect.Left, ARect.Right);
  SwapIntOrder(ARect.Top, ARect.Bottom);
  if CropRect(ARect, DrawRect) then
  begin
    SetOrtho;

    glDisable(GL_TEXTURE_2D);
    glDisable(GL_DEPTH_TEST);
    glDisable(GL_BLEND);
    with LineColor do glColor4f(R/255, G/255, B/255, A/255);

    glPushMatrix();
    glTranslatef(0, Height, FDepth); // x, y, zorder
    glLineWidth(LineWidth / Height);

    glPushAttrib(GL_ENABLE_BIT);
    if LineStipple <> $FFFF then
    begin
      glEnable(GL_LINE_STIPPLE);
      glLineStipple(1, LineStipple);
    end;
    glBegin(GL_LINE_LOOP);
    glVertex2f(ARect.Left, -ARect.Top);
    glVertex2f(ARect.Right, -ARect.Top);
    glVertex2f(ARect.Right, -ARect.Bottom);
    glVertex2f(ARect.Left, -ARect.Bottom);
    glEnd;
    glPopAttrib;

    glPopMatrix();
//    SetPerspective;
  end;
end;

procedure TXPGL.SetBounds(ARect: TRect);
begin
  glViewport(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
  FClipRect.Right := ARect.Right - ARect.Left;
  FClipRect.Bottom := ARect.Bottom - ARect.Top;
  FDrawRect := FClipRect;
  SetPerspective; // do not remove
end;

procedure TXPGL.SetDisplayCanvas(ACanvas: TCanvas);
begin
  if ACanvas <> FDisplayCanvas then
  begin
    if FControlCanvas <> nil then
    begin
      FreeAndNil(FControlCanvas);
      FWindow := nil;
    end;
    FDisplayCanvas := ACanvas;
    CanvasChanged;
  end;
end;

procedure TXPGL.SetOrtho(AWidth, AHeight: Integer);
begin
  glMatrixMode(GL_PROJECTION);  // Change Matrix Mode to Projection
  glLoadIdentity;               // Reset View
  glOrtho(0, AWidth, 0, AHeight, 0, 100);
  glMatrixMode(GL_MODELVIEW);   // Change Projection to Matrix Mode
  glLoadIdentity;
end;

procedure TXPGL.SetPerspective;
begin
  SetPerspective(Width, Height);
end;

procedure TXPGL.SetOrtho;
begin
  SetOrtho(Width, Height);
end;

procedure TXPGL.SetPerspective(AWidth, AHeight: Integer);
begin
  glMatrixMode(GL_PROJECTION);        // Change Matrix Mode to Projection
  glLoadIdentity;                     // Reset View
  if AHeight > 0 then                  // Do the perspective calculations. Last value = max clipping depth
    gluPerspective(45.0, AWidth / AHeight, 1.0, 100.0)
  else gluPerspective(45.0, 1.0, 1.0, 100.0);
  glMatrixMode(GL_MODELVIEW);         // Return to the modelview matrix
  glLoadIdentity();                   // Reset View
end;

procedure TXPGL.SetVSync(ASync: Boolean);
begin
end;

procedure TXPGL.SetWindow(AWindow: TWinControl);
begin
  if AWindow <> FWindow then
  begin
    SetDisplayCanvas(nil);
    FControlCanvas := TControlCanvas.Create;
    FControlCanvas.Control := AWindow;
    FWindow := AWindow;
    FDisplayCanvas := FControlCanvas;
    CanvasChanged;
  end;
end;

procedure TXPGL.SwapBuffer;
begin
  SwapBuffers(FDisplayCanvas.Handle);
end;

procedure TXPGL.Unlock;
begin
  LeaveCriticalSection(FLock);
end;

procedure TXPGL.SetDrawBottom(const Value: Integer);
begin
  FDrawRect.Bottom := Value;
end;

procedure TXPGL.SetDrawLeft(const Value: Integer);
begin
  FDrawRect.Left := Value;
end;

procedure TXPGL.SetDrawRect(ARect: TRect);
begin
  FDrawRect := ARect;
  CropRect(FDrawRect, FClipRect);
end;

function TXPGL.GetDrawBottom: Integer;
begin
  Result := FDrawRect.Bottom;
end;

function TXPGL.GetDrawLeft: Integer;
begin
  Result := FDrawRect.Bottom;
end;

function TXPGL.GetDrawRight: Integer;
begin
  Result := FDrawRect.Right;
end;

function TXPGL.GetDrawTop: Integer;
begin
  Result := FDrawRect.Top;
end;

procedure TXPGL.SetDrawRight(const Value: Integer);
begin
  FDrawRect.Right := Value;
end;

procedure TXPGL.SetDrawTop(const Value: Integer);
begin
  FDrawRect.Top := Value;
end;

function TXPGL.GetHeight: Integer;
begin
  Result := FClipRect.Bottom - FClipRect.Top;
end;

function TXPGL.GetWidth: Integer;
begin
  Result := FClipRect.Right - FClipRect.Left;
end;

procedure TXPGL.FillRect(drect: TRect);
begin
  SwapIntOrder(drect.Left, drect.Right);
  SwapIntOrder(drect.Top, drect.Bottom);
  if CropRect(drect, DrawRect) then
  begin
    SetOrtho;

    glDisable(GL_TEXTURE_2D);
    glDisable(GL_DEPTH_TEST);
    glDisable(GL_BLEND);
    with BrushColor do glColor4f(R/255, G/255, B/255, A/255);

    glPushMatrix();
    glTranslatef(0, Height, FDepth); // x, y, zorder

    glBegin(GL_QUADS);
    glVertex2f(drect.Left, -drect.Top);
    glVertex2f(drect.Right, -drect.Top);
    glVertex2f(drect.Right, -drect.Bottom);
    glVertex2f(drect.Left, -drect.Bottom);
    glEnd;

    glPopMatrix();
//    SetPerspective;
  end;
end;

procedure TXPGL.FillRect;
begin
  FillRect(FDrawRect);
end;

{ TGLRGBAFloat }

procedure TGLRGBAFloat.Assign(AArray: array of GLFloat);
var
  n: Integer;
begin
  n := Length(AArray);
  while n > 0 do
  begin
    Dec(n);
    RGBA[n] := AArray[n];
  end;
end;

procedure TGLRGBAFloat.Assign(AR, AG, AB, AA: GLFloat);
begin
  R := AR;
  G := AG;
  B := AB;
  A := AA;
end;

procedure TGLRGBAFloat.Clear;
begin
  R := 0.0;
  G := 0.0;
  B := 0.0;
  A := 0.0;
end;

{ TGLRGBFloat }

procedure TGLRGBFloat.Clear;
begin
  R := 0.0;
  G := 0.0;
  B := 0.0;
end;

{ TGLXYZFloat }

procedure TGLXYZFloat.Assign(AArray: array of GLFloat);
var
  n: Integer;
begin
  n := Length(AArray);
  while n > 0 do
  begin
    Dec(n);
    Axis[n] := AArray[n];
  end;
end;

class operator TGLXYZFloat.Add(AValue1, AValue2: TGLXYZFloat): TGLXYZFloat;
begin
  Result.X := AValue1.X + AValue2.X;
  Result.Y := AValue1.Y + AValue2.Y;
  Result.Z := AValue1.Z + AValue2.Z;
end;

procedure TGLXYZFloat.Assign(AX, AY, AZ: GLFloat);
begin
  X := AX;
  Y := AY;
  Z := AZ;
end;

procedure TGLXYZFloat.Clear;
begin
  X := 0.0;
  Y := 0.0;
  Z := 0.0;
end;

class operator TGLXYZFloat.Divide(AValue1: GLFloat;
  AValue2: TGLXYZFloat): TGLXYZFloat;
begin
  Result.X := AValue1 / AValue2.X;
  Result.Y := AValue1 / AValue2.Y;
  Result.Z := AValue1 / AValue2.Z;
end;

class operator TGLXYZFloat.Divide(AValue1: TGLXYZFloat; AValue2: GLFloat): TGLXYZFloat;
begin
  Result.X := AValue1.X / AValue2;
  Result.Y := AValue1.Y / AValue2;
  Result.Z := AValue1.Z / AValue2;
end;

class operator TGLXYZFloat.Equal(AValue1, AValue2: TGLXYZFloat): Boolean;
begin
  Result := (AValue1.X = AValue2.X)
    and (AValue1.Y = AValue2.Y)
    and (AValue1.Z = AValue2.Z);
end;

procedure TGLXYZFloat.MoveBy(APosition: TGLXYZFloat);
begin
  MoveBy(APosition.X, APosition.Y, APosition.Z);
end;

class operator TGLXYZFloat.Negative(AValue: TGLXYZFloat): TGLXYZFloat;
begin
  Result.X := -AValue.X;
  Result.Y := -AValue.Y;
  Result.Z := -AValue.Z;
end;

procedure TGLXYZFloat.ReOrder(AXOrder, AYOrder, AZOrder: Integer);
var
  temp: TGLXYZFLoat;
begin
  temp.X := Axis[AXOrder];
  temp.Y := Axis[AYOrder];
  temp.Z := Axis[AZOrder];
  X := temp.X;
  Y := temp.Y;
  Z := temp.Z;
end;

procedure TGLXYZFloat.MoveBy(AX, AY, AZ: GLFloat);
begin
  X := X + AX;
  Y := Y + AY;
  Z := Z + AZ;
end;

class operator TGLXYZFloat.Subtract(AValue1, AValue2: TGLXYZFloat): TGLXYZFloat;
begin
  Result.X := AValue1.X - AValue2.X;
  Result.Y := AValue1.Y - AValue2.Y;
  Result.Z := AValue1.Z - AValue2.Z;
end;

procedure TGLXYZFloat.Assign(AValue: GLFloat);
begin
  X := AValue;
  Y := AValue;
  Z := AValue;
end;

{ TGLXYZWFloat }

procedure TGLXYZWFloat.Assign(AArray: array of GLFloat);
var
  n: Integer;
begin
  n := Length(AArray);
  while n > 0 do
  begin
    Dec(n);
    Axis[n] := AArray[n];
  end;
end;

procedure TGLXYZWFloat.Assign(AX, AY, AZ: GLFloat);
begin
  X := AX;
  Y := AY;
  Z := AZ;
end;

class operator TGLXYZWFloat.Add(AValue1, AValue2: TGLXYZWFloat): TGLXYZWFloat;
begin
  Result.X := AValue1.X + AValue2.X;
  Result.Y := AValue1.Y + AValue2.Y;
  Result.Z := AValue1.Z + AValue2.Z;
  Result.W := AValue1.W + AValue2.W;
end;

class operator TGLXYZWFloat.Add(AValue1: TGLXYZWFloat;
  AValue2: TGLXYZFloat): TGLXYZWFloat;
begin
  Result.X := AValue1.X + AValue2.X;
  Result.Y := AValue1.Y + AValue2.Y;
  Result.Z := AValue1.Z + AValue2.Z;
  Result.W := AValue1.W;
end;

procedure TGLXYZWFloat.Assign(AX, AY, AZ, AW: GLFloat);
begin
  X := AX;
  Y := AY;
  Z := AZ;
  W := AW;
end;

procedure TGLXYZWFloat.Clear;
begin
  X := 0.0;
  Y := 0.0;
  Z := 0.0;
  W := 0.0;
end;

function TGLXYZWFloat.Compare(AValue: TGLXYZWFloat): Integer;
var
  d: GLFloat;
begin
  d := X - AValue.X;
  if d = 0.0 then
  begin
    d := Y - AValue.Y;
    if d = 0.0 then
    begin
      d := Z - AValue.Z;
      if d = 0.0 then d := W - AValue.W;
    end;
  end;
  if d < 0 then
    Result := -1
  else if d > 0 then
    Result := 1
  else Result := 0;
end;

class operator TGLXYZWFloat.Equal(AValue1, AValue2: TGLXYZWFloat): Boolean;
begin
  Result := (AValue1.X = AValue2.X) and (AValue1.Y = AValue2.Y)
    and (AValue1.Z = AValue2.Z) and (AValue1.W = AValue2.W);
end;

procedure TGLXYZWFloat.MoveBy(APosition: TGLXYZWFloat);
begin
  MoveBy(APosition.X, APosition.Y, APosition.Z, APosition.W);
end;

procedure TGLXYZWFloat.MoveBy(APosition: TGLXYZFloat);
begin
  MoveBy(APosition.X, APosition.Y, APosition.Z);
end;

class operator TGLXYZWFloat.Negative(AValue: TGLXYZWFloat): TGLXYZWFloat;
begin
  Result.X := -AValue.X;
  Result.Y := -AValue.Y;
  Result.Z := -AValue.Z;
  Result.W := -AValue.W;
end;

procedure TGLXYZWFloat.ReOrder(AXOrder, AYOrder, AZOrder: Integer);
var
  temp: TGLXYZFLoat;
begin
  temp.X := Axis[AXOrder];
  temp.Y := Axis[AYOrder];
  temp.Z := Axis[AZOrder];
  X := temp.X;
  Y := temp.Y;
  Z := temp.Z;
end;

procedure TGLXYZWFloat.RotateBy(ARotate: TGLXYZFloat);
begin

end;

procedure TGLXYZWFloat.ReOrder(AXOrder, AYOrder, AZOrder, AWOrder: Integer);
var
  temp: TGLXYZWFLoat;
begin
  temp.X := Axis[AXOrder];
  temp.Y := Axis[AYOrder];
  temp.Z := Axis[AZOrder];
  temp.W := Axis[AWOrder];
  X := temp.X;
  Y := temp.Y;
  Z := temp.Z;
  W := temp.W;
end;

procedure TGLXYZWFloat.ScaleBy(AScale: TGLXYZFloat);
begin
  X := X * AScale.X;
  Y := Y * AScale.Y;
  Z := Z * AScale.Z;
end;

class operator TGLXYZWFloat.Subtract(AValue1: TGLXYZWFloat;
  AValue2: TGLXYZFloat): TGLXYZWFloat;
begin
  Result.X := AValue1.X - AValue2.X;
  Result.Y := AValue1.Y - AValue2.Y;
  Result.Z := AValue1.Z - AValue2.Z;
  Result.W := AValue1.W;
end;

procedure TGLXYZWFloat.MoveBy(AX, AY, AZ, AW: GLFloat);
begin
  X := X + AX;
  Y := Y + AY;
  Z := Z + AZ;
  W := W + AW;
end;

class operator TGLXYZWFloat.Subtract(AValue1,
  AValue2: TGLXYZWFloat): TGLXYZWFloat;
begin
  Result.X := AValue1.X - AValue2.X;
  Result.Y := AValue1.Y - AValue2.Y;
  Result.Z := AValue1.Z - AValue2.Z;
  Result.W := AValue1.W - AValue2.W;
end;

{ TRGBDouble }

function TGLRGBFloat.AsText: string;
begin
  Result := Format('%.6f %.6f %.6f', [R, G, B]);
end;

initialization
  InitOpenGL;
  ReadOpenGLCore;

end.
