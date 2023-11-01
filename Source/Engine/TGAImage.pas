unit TGAImage;

interface

uses
  Windows, Classes, Graphics, SysUtils;

type
  TTGAHeader = packed record
  private
    function GetDescriptorBit(const Index: Integer): Boolean;
    procedure SetDescriptorBit(const Index: Integer; const Value: Boolean);
  public
    IDLength: Byte;        // 00h  Size of Image ID field
    ColorMapType: Byte;    // 01h  Color map type
    ImageType: Byte;       // 02h  Image type code
    CMapStart: Word;       // 03h  Color map origin
    CMapLength: Word;      // 05h  Color map length
    CMapDepth: Byte;       // 07h  Depth of color map entries
    XOffset: Word;         // 08h  X origin of image
    YOffset: Word;         // 0Ah  Y origin of image
    Width: Word;           // 0Ch  Width of image
    Height: Word;          // 0Eh  Height of image
    PixelDepth: Byte;      // 10h  Image pixel size
    ImageDescriptor: Byte; // 11h  Image descriptor byte
    procedure Clear;
    property IsRightFirst: Boolean index 4 read GetDescriptorBit write SetDescriptorBit;
    property IsTopFirst: Boolean index 5 read GetDescriptorBit write SetDescriptorBit;
  end;

  TTGAImage = class(TGraphic)
  private
    FHeader: TTGAHeader;
    FBitmap: TBitmap;
    FYPos, FYProgressPos: Integer;
    procedure Progressing;
  protected
    function GetHeight: Integer; override;
    procedure SetHeight(Value: Integer); override;
    function GetWidth: Integer; override;
    procedure SetWidth(Value: Integer); override;
    procedure AssignTo(Dest: TPersistent); override;
    procedure Draw(ACanvas: TCanvas; const Rect: TRect); override;
    function Equals(Graphic: TGraphic): Boolean; override;
    function GetPalette: HPALETTE; override;
    procedure SetPalette(Value: HPalette); override;
    function GetEmpty: Boolean; override;
    procedure WriteData(Stream: TStream); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Clear;
    procedure SaveToStream(Stream: TStream); override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure Assign(Source: TPersistent); override;
    procedure LoadFromClipboardFormat(AFormat: Word; AData: THandle;
      APalette: HPALETTE); override;
    procedure SaveToClipboardFormat(var AFormat: Word; var AData: THandle;
      var APalette: HPALETTE); override;
    property Bitmap: TBitmap read FBitmap;
  end;

implementation

{ TTGAImage }

procedure TTGAImage.Assign(Source: TPersistent);
begin
  if Source = Self then Exit;
  if Source = nil then
  begin
    Clear;
    Exit;
  end;
  if Source is TTGAImage then
  begin
    Clear;
    FHeader := TTGAImage(Source).FHeader;
    FBitmap.Assign(TTGAImage(Source).FBitmap);
  end
  else if Source is TBitmap then
  begin
    Clear;
    if TBitmap(Source).Empty then Exit;

    Width := TBitmap(Source).Width;
    Height := TBitmap(Source).Height;

    case TBitmap(Source).PixelFormat of
      pf1bit: FHeader.PixelDepth := 1;
      pf4bit: FHeader.PixelDepth := 4;
      pf8bit: FHeader.PixelDepth := 8;
      pf15bit: FHeader.PixelDepth := 15;
      pf16bit: FHeader.PixelDepth := 16;
      pf24bit: FHeader.PixelDepth := 24;
      pf32bit: FHeader.PixelDepth := 32;
    else
      FHeader.PixelDepth := 32;
    end;
    FBitmap.Assign(TBitmap(Source));
  end;
end;

procedure TTGAImage.AssignTo(Dest: TPersistent);
begin
  if (Dest is TBitmap) then
    Dest.Assign(FBitmap)
  else inherited AssignTo(Dest);
end;

procedure TTGAImage.Clear;
begin
  FBitmap.FreeImage;
  FHeader.Clear;
end;

constructor TTGAImage.Create;
begin
  inherited Create;
  FBitmap := TBitmap.Create;
  FHeader.Clear;
  FYPos := 0;
  FYProgressPos := 0;
end;

destructor TTGAImage.Destroy;
begin
  FBitmap.Free;
  inherited Destroy;
end;

procedure TTGAImage.Draw(ACanvas: TCanvas; const Rect: TRect);
var
  blendFn: BLENDFUNCTION;
  sw, sh, dw, dh: Integer;
  srect: TRect;
begin
  srect := FBitmap.Canvas.ClipRect;
  sw := srect.Right - srect.Left;
  sh := srect.Bottom - srect.Top;
  dw := Rect.Right - Rect.Left;
  dh := Rect.Bottom - Rect.Top;

  if FHeader.PixelDepth = 32 then
  begin
    blendFn.BlendOp := AC_SRC_OVER;
    blendFn.BlendFlags := 0;
    blendFn.SourceConstantAlpha := 255;
    blendFn.AlphaFormat := AC_SRC_ALPHA;
    Windows.AlphaBlend(ACanvas.Handle, Rect.Left, Rect.Top, dw, dh,
      FBitmap.Canvas.Handle, srect.Left, srect.Top, sw, sh, blendFn);
  end
  else ACanvas.CopyRect(Rect, FBitmap.Canvas, FBitmap.Canvas.ClipRect);
end;

function TTGAImage.Equals(Graphic: TGraphic): Boolean;
begin
  Result := Graphic = Self;
end;

function TTGAImage.GetEmpty: Boolean;
begin
  Result := FBitmap.Empty;
end;

function TTGAImage.GetHeight: Integer;
begin
  Result := FBitmap.Height;
end;

function TTGAImage.GetPalette: HPALETTE;
begin
  Result := FBitmap.Palette;
end;

function TTGAImage.GetWidth: Integer;
begin
  Result := FBitmap.Width;
end;

procedure TTGAImage.LoadFromClipboardFormat(AFormat: Word; AData: THandle;
  APalette: HPALETTE);
begin
  FBitmap.LoadFromClipboardFormat(AFormat, AData, APalette);
end;

procedure TTGAImage.LoadFromStream(Stream: TStream);
var
  nbyte, nx: Integer;
  p: PByte;
  b: Byte;
  d: array[0..3] of Byte;
begin
  Progress(Self, psStarting, 0, False, Rect(0,0,0,0), 'Loading...');
  Clear;
  Stream.ReadBuffer(FHeader, SizeOf(FHeader));
  if (FHeader.ImageType <> 2) and (FHeader.ImageType <> 10) then
    raise Exception.Create('Not supported TGA image file type.');
  if FHeader.ColorMapType <> 0 then
    raise Exception.Create('Not supported TGA colormap.');
  if FHeader.PixelDepth < 24 then
    raise Exception.Create('Not supported TGA pixel depth.');
  case FHeader.PixelDepth of
    1: FBitmap.PixelFormat := pf1bit;
    4: FBitmap.PixelFormat := pf4bit;
    8: FBitmap.PixelFormat := pf8bit;
    15: FBitmap.PixelFormat := pf15bit;
    16: FBitmap.PixelFormat := pf16bit;
    24: FBitmap.PixelFormat := pf24bit;
    32: FBitmap.PixelFormat := pf32bit;
  else
    FBitmap.PixelFormat := pfCustom;
  end;
  FBitmap.SetSize(FHeader.Width, FHeader.Height);
  nbyte := (FHeader.PixelDepth + 7) div 8;
  case FHeader.ImageType of
    0:; // No image
    1:; // Colormapped
    2:  // Truecolor
    begin
      FYPos := FBitmap.Height;
      FYProgressPos := FBitmap.Height - FYPos div 100;
      while FYPos > 0 do
      begin
        Dec(FYPos);
        if FHeader.IsTopFirst then
           p := FBitmap.ScanLine[FBitmap.Height - FYPos - 1]
        else p := FBitmap.ScanLine[FYPos];
        Stream.Read(p^, nbyte * FBitmap.Width);
        Progressing;
      end;
    end;
    3:; // Monochrome
    9:; // Colormapped RLE
    10: // Truecolor RLE
    begin
      FYPos := FBitmap.Height;
      FYProgressPos := FBitmap.Height - FYPos div 100;
      nx := FBitmap.Width;
      p := nil;
      repeat
        Stream.Read(b, 1);
        if b < 128 then
        begin
          Inc(b);
          while b > 0 do
          begin
            if (nx <= 0) or (p = nil) then
            begin
              Dec(FYPos);
              if FYPos >= 0 then
              begin
                Progressing;
                if FHeader.IsTopFirst then
                   p := FBitmap.ScanLine[FBitmap.Height - FYPos - 1]
                else p := FBitmap.ScanLine[FYPos];
              end;
              nx := FBItmap.Width;
            end;
            if b <= nx then
            begin
              Stream.Read(p^, b * nbyte);
              Inc(p, b * nbyte);
              Dec(nx, b);
              b := 0;
            end
            else
            begin
              Stream.Read(p^, nx * nbyte);
              Inc(p, nx * nbyte);
              Dec(b, nx);
              Inc(FYPos);
              nx := 0;
            end;
          end;
        end
        else
        begin
          b := (b and 127) + 1;
          Stream.Read(d, nbyte);
          while b > 0 do
          begin
            if (nx <= 0) or (p = nil) then
            begin
              Dec(FYPos);
              if FYPos >= 0 then
              begin
                Progressing;
                if FHeader.IsTopFirst then
                   p := FBitmap.ScanLine[FBitmap.Height - FYPos - 1]
                else p := FBitmap.ScanLine[FYPos];
              end;
              nx := FBItmap.Width;
            end;
            if b <= nx then
            begin
              Dec(nx, b);
              while b > 0 do
              begin
                Move(d, p^, nbyte);
                Inc(p, nbyte);
                Dec(b);
              end;
            end
            else
            begin
              Dec(b, nx);
              while nx > 0 do
              begin
                Move(d, p^, nbyte);
                Inc(p, nbyte);
                Dec(nx);
              end;
              Inc(FYPos);
            end;
          end;
        end;
      until (FYPos < 0) or ((FYPos = 0) and (nx <= 0));
    end;
    11:; // Monochrome RLE
  end;
  Progress(Self, psEnding, 100, True, Rect(0,0,0,0), 'Loading...');
end;

procedure TTGAImage.Progressing;
begin
  if FYPos <= FYProgressPos then
  begin
    Progress(Self, psRunning, 100 - (FYPos * 100 div FBitmap.Height), False, Rect(0, 0, 0, 0), 'Loading...');
    FYProgressPos := (((FYPos - 1) div 100) - 1) * 100;
  end;
end;

procedure TTGAImage.SaveToClipboardFormat(var AFormat: Word; var AData: THandle;
  var APalette: HPALETTE);
begin
  inherited;
end;

procedure TTGAImage.SaveToStream(Stream: TStream);
var
  n, nbyte: Integer;
  p: PByte;
  hd: TTGAHeader;
begin
  hd.Clear;
  hd.ImageType := 2;
  hd.ColorMapType := 0;
  hd.ImageDescriptor := 0;
  hd.Width := FBitmap.Width;
  hd.Height := FBitmap.Height;
  case FBitmap.PixelFormat of
    pf1bit: hd.PixelDepth := 1; // not completed
    pf4bit: hd.PixelDepth := 4; // not completed
    pf8bit: hd.PixelDepth := 8;
    pf15bit: hd.PixelDepth := 15;
    pf16bit: hd.PixelDepth := 16;
    pf24bit: hd.PixelDepth := 24;
    pf32bit: hd.PixelDepth := 32;
  else
    raise Exception.Create('Not supported TGA pixel format.');
  end;
  nbyte := (FHeader.PixelDepth + 7) div 8;

  Stream.Write(hd, SizeOf(hd));
  n := hd.Height;
  while n > 0 do
  begin
    Dec(n);
    p := FBitmap.ScanLine[n];
    Stream.Write(p^, FHeader.Width * nbyte);
  end;
end;

procedure TTGAImage.SetHeight(Value: Integer);
begin
  inherited;

end;

procedure TTGAImage.SetPalette(Value: HPalette);
begin
  inherited;

end;

procedure TTGAImage.SetWidth(Value: Integer);
begin
  inherited;

end;

procedure TTGAImage.WriteData(Stream: TStream);
begin
  inherited WriteData(Stream);
end;

{ TTGAHeader }

procedure TTGAHeader.Clear;
begin
  IDLength := 0;
  ColorMapType := 0;
  ImageType := 0;
  CMapStart := 0;
  CMapLength := 0;
  CMapDepth := 0;
  XOffset := 0;
  YOffset := 0;
  Width := 0;
  Height := 0;
  PixelDepth := 0;
  ImageDescriptor := 0;
end;

function TTGAHeader.GetDescriptorBit(const Index: Integer): Boolean;
begin
  Result := ((ImageDescriptor shr Index) and 1) <> 0;
end;

procedure TTGAHeader.SetDescriptorBit(const Index: Integer;
  const Value: Boolean);
begin
  if Value then
    ImageDescriptor := ImageDescriptor or (1 shl Index)
  else ImageDescriptor := ImageDescriptor and (255 - (1 shl Index));
end;

end.
