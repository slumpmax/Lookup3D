unit DDSImage;

interface

uses
  Windows, Classes, Graphics, SysUtils;

const
  DDS_FORMAT        = $20534444;
  DXT1_FORMAT       = $31545844;
  DXT3_FORMAT       = $33545844;
  DXT5_FORMAT       = $35545844;
  DDPF_FOURCC       = $00000004;
  DDPF_RGB          = $00000040;
  DDPF_ALPHAPIXELS  = $00000001;

type
  TR5G6B6 = Word;
  TARGB = packed record
    B, G, R, A: Byte;
  end;

PARGB = ^TARGB;
  TDDSPixelFormat = packed record
    dwSize: Cardinal;
    dwFlags: Cardinal;
    dwFourCC: Cardinal;
    dwRGBBitCount: Cardinal;
    dwRBitMask: Cardinal;
    dwGBitMask: Cardinal;
    dwBBitMask: Cardinal;
    dwABitMask: Cardinal;
    procedure Clear;
  end;

  TDDSHeader = packed record
    Size: Cardinal;
    Flags: Cardinal;
    Height: Cardinal;
    Width: Cardinal;
    PitchOrLinearSize: Cardinal;
    Depth: Cardinal;
    MipMapCount: Cardinal;
    Reserved1: array[0..10] of Cardinal;
    spf: TDDSPixelFormat;
    Caps: Cardinal;
    Caps2: Cardinal;
    Caps3: Cardinal;
    Caps4: Cardinal;
    Reserved2: Cardinal;
    procedure Clear;
  end;

  TDDSImage = class(TGraphic)
  private
    FHeader: TDDSHeader;
    FBitmap: TBitmap;
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
    procedure DecodeDXT1(ABitmap: TBitmap; AStream: TStream; AWidth, AHeight: Cardinal);
    procedure DecodeDXT3(ABitmap: TBitmap; AStream: TStream; AWidth, AHeight: Cardinal);
    procedure DecodeDXT5(ABitmap: TBitmap; AStream: TStream; AWidth, AHeight: Cardinal);
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

function ARGB(AColor: TR5G6B6): TARGB;
var
  R, G, B: Byte;
begin
  R := AColor shr 11;
  G := (AColor shr 5) and 63;
  B := AColor and 31;
  Result.A := 255;
  Result.R := (R shl 3) or (R shr 2);
  Result.G := (G shl 2) or (G shr 4);
  Result.B := (B shl 3) or (B shr 2);
end;

{ TDDSImage }

procedure TDDSImage.Assign(Source: TPersistent);
begin
  if Source = Self then Exit;
  if Source = nil then
  begin
    Clear;
    Exit;
  end;
  if Source is TDDSImage then
  begin
    Clear;
    FHeader := TDDSImage(Source).FHeader;
    FBitmap.Assign(TDDSImage(Source).FBitmap);
  end
  else if Source is TBitmap then
  begin
    Clear;
    if TBitmap(Source).Empty then Exit;

    Width := TBitmap(Source).Width;
    Height := TBitmap(Source).Height;
    FBitmap.Assign(TBitmap(Source));
  end;
end;

procedure TDDSImage.AssignTo(Dest: TPersistent);
begin
  if (Dest is TBitmap) then
    Dest.Assign(FBitmap)
  else inherited AssignTo(Dest);
end;

procedure TDDSImage.Clear;
begin
  FBitmap.FreeImage;
  FHeader.Clear;
end;

constructor TDDSImage.Create;
begin
  inherited Create;
  FBitmap := TBitmap.Create;
  FHeader.Clear;
end;

procedure TDDSImage.DecodeDXT1(ABitmap: TBitmap; AStream: TStream;
  AWidth, AHeight: Cardinal);
var
  Rx, Cx, Rn, Cn: Cardinal;
  Data: PByte;
  PData: PByteArray;
  C0, C1: TR5G6B6;
  C: array [0..3] of TARGB;
  Pc: PARGB;
  Ofs: Cardinal;
  Mask, Selector: Cardinal;
  LinearSize: Cardinal;
begin
  LinearSize := ((Width + 3) shr 2) shl 3;
  GetMem(Data, LinearSize);
  try
    for Rn := 0 to (Height shr 2) - 1 do
    begin
      AStream.Read(Data^, LinearSize);
      PData := PByteArray(Data);
      for Cn := 0 to (Width shr 2) - 1 do
      begin
        C0 := PWord(@PData[0])^;
        C1 := PWord(@PData[2])^;
        C[0] := ARGB(C0); C[1] := ARGB(C1); PData := @PData[4];
        Mask := PCardinal(@PData[0])^; PData := @PData[4];
        if C0 > C1 then
        begin
          C[2].A := 255;
          C[2].R := (2 * C[0].R + C[1].R + 1) div 3;
          C[2].G := (2 * C[0].G + C[1].G + 1) div 3;
          C[2].B := (2 * C[0].B + C[1].B + 1) div 3;
          C[3].A := 255;
          C[3].R := (C[0].R + 2 * C[1].R + 1) div 3;
          C[3].G := (C[0].G + 2 * C[1].G + 1) div 3;
          C[3].B := (C[0].B + 2 * C[1].B + 1) div 3;
        end
        else
        begin
          C[2].A := 255;
          C[2].R := (C[0].R + C[1].R) shr 1;
          C[2].G := (C[0].G + C[1].G) shr 1;
          C[2].B := (C[0].B + C[1].B) shr 1;
          C[3].A := 0;
        end;
        Ofs := Cn shl 2;
        for Rx := 0 to 3 do
        begin
          Pc := PARGB(ABitmap.ScanLine[Rn shl 2 + Rx]);
          Inc(Pc, Ofs);
          for Cx := 0 to 3 do
          begin
            Selector := Mask and 3;
            Pc^ := C[Selector];
            Inc(Pc);
            Mask := Mask shr 2;
          end;
        end;
      end;
    end;
  finally
    FreeMem(Data);
  end;
end;

procedure TDDSImage.DecodeDXT3(ABitmap: TBitmap; AStream: TStream; AWidth,
  AHeight: Cardinal);
var
  Rx, Cx, Rn, Cn: Cardinal;
  Data: PByte;
  PData: PByteArray;
  C0, C1: TR5G6B6;
  C: array [0..3] of TARGB;
  Pc: PARGB;
  Ofs: Cardinal;
  Mask, Selector: Cardinal;
  Alpha: array [0..3] of Word;
  LinearSize: Cardinal;
begin
  LinearSize := ((Width + 3) shr 2) shl 4;
  GetMem(Data, LinearSize);
  try
    for Rn := 0 to (Height shr 2) - 1 do
    begin
      AStream.Read(Data^, LinearSize);
      PData := PByteArray(Data);
      for Cn := 0 to (Width shr 2) - 1 do
      begin
        Alpha[0] := PWord(@PData[0])^;
        Alpha[1] := PWord(@PData[2])^;
        Alpha[2] := PWord(@PData[4])^;
        Alpha[3] := PWord(@PData[6])^;
        C0 := PWord(@PData[8])^;
        C1 := PWord(@PData[10])^;
        Mask := PCardinal(@PData[12])^;
        PData := @PData[16];
        C[0] := ARGB(C0);
        C[1] := ARGB(C1);
        C[2].A := 255;
        C[2].R := (2 * C[0].R + C[1].R + 1) div 3;
        C[2].G := (2 * C[0].G + C[1].G + 1) div 3;
        C[2].B := (2 * C[0].B + C[1].B + 1) div 3;
        C[3].A := 255;
        C[3].R := (C[0].R + 2 * C[1].R + 1) div 3;
        C[3].G := (C[0].G + 2 * C[1].G + 1) div 3;
        C[3].B := (C[0].B + 2 * C[1].B + 1) div 3;
        Ofs := Cn shl 2;
        for Rx := 0 to 3 do
        begin
          Pc := PARGB(ABitmap.ScanLine[Rn shl 2 + Rx]);
          Inc(Pc, Ofs);
          for Cx := 0 to 3 do
          begin
            Selector := Mask and 3;
            C[Selector].A := Alpha[Rx] and 15;
            Alpha[Rx] := Alpha[Rx] shr 4;;
            Pc^ := C[Selector];
            Inc(Pc);
            Mask := Mask shr 2;
          end;
        end;
      end;
    end;
  finally
    FreeMem(Data);
  end;
end;

procedure TDDSImage.DecodeDXT5(ABitmap: TBitmap; AStream: TStream; AWidth,
  AHeight: Cardinal);
var
  Rx, Cx, Rn, Cn: Cardinal;
  Data, PData: PByteArray;
  C0, C1: TR5G6B6;
  C: array [0..3] of TARGB;
  Pc: PARGB;
  Ofs: Cardinal;
  Mask, Selector: Cardinal;
  A: array [0..7] of Byte;
  PAMask: PByte;
  AMask: Cardinal;
  LinearSize: Cardinal;
begin
  LinearSize := ((Width + 3) shr 2) shl 4;
  GetMem(Data, LinearSize);
  try
    for Rn := 0 to (Height shr 2) - 1 do
    begin
      AStream.Read(Data[0], LinearSize);
      PData := Data;
      for Cn := 0 to (Width shr 2) - 1 do
      begin
        A[0] := PData[0];
        A[1] := PData[1];
        PAMask := @PData[2];
        C0 := PWord(@PData[8])^;
        C1 := PWord(@PData[10])^;
        Mask := PCardinal(@PData[12])^;
        PData := @PData[16];
        C[0] := ARGB(C0);
        C[1] := ARGB(C1);
        C[2].A := 255;
        C[2].R := (2 * C[0].R + C[1].R + 1) div 3;
        C[2].G := (2 * C[0].G + C[1].G + 1) div 3;
        C[2].B := (2 * C[0].B + C[1].B + 1) div 3;
        C[3].A := 255;
        C[3].R := (C[0].R + 2 * C[1].R + 1) div 3;
        C[3].G := (C[0].G + 2 * C[1].G + 1) div 3;
        C[3].B := (C[0].B + 2 * C[1].B + 1) div 3;
        if A[0] > A[1] then
        begin
          A[2] := (6 * A[0] + 1 * A[1] + 3) div 7;
          A[3] := (5 * A[0] + 2 * A[1] + 3) div 7;
          A[4] := (4 * A[0] + 3 * A[1] + 3) div 7;
          A[5] := (3 * A[0] + 4 * A[1] + 3) div 7;
          A[6] := (2 * A[0] + 5 * A[1] + 3) div 7;
          A[7] := (1 * A[0] + 6 * A[1] + 3) div 7;
        end
        else
        begin
          A[2] := (4 * A[0] + 1 * A[1] + 2) div 5;
          A[3] := (3 * A[0] + 2 * A[1] + 2) div 5;
          A[4] := (2 * A[0] + 3 * A[1] + 2) div 5;
          A[5] := (1 * A[0] + 4 * A[1] + 2) div 5;
          A[6] := 0;
          A[7] := 255;
        end;
        Ofs := Cn shl 2;
        AMask := PCardinal(PAMask)^;
        Inc(PAMask, 3);
        for Rx := 0 to 3 do
        begin
          Pc := PARGB(ABitmap.ScanLine[Rn shl 2 + Rx]);
          Inc(Pc, Ofs);
          for Cx := 0 to 3 do
          begin
            Selector := Mask and 3;
            if Rx = 2 then AMask := PCardinal(PAMask)^;
            C[Selector].A := A[AMask and 7];
            AMask := AMask shr 3;;
            Pc^ := C[Selector];
            Inc(Pc);
            Mask := Mask shr 2;
          end;
        end;
      end;
    end;
  finally
    FreeMem(Data);
  end;
end;

destructor TDDSImage.Destroy;
begin
  FBitmap.Free;
  inherited Destroy;
end;

procedure TDDSImage.Draw(ACanvas: TCanvas; const Rect: TRect);
begin
  ACanvas.CopyRect(Rect, FBitmap.Canvas, FBitmap.Canvas.ClipRect);
end;

function TDDSImage.Equals(Graphic: TGraphic): Boolean;
begin
  Result := Graphic = Self;
end;

function TDDSImage.GetEmpty: Boolean;
begin
  Result := FBitmap.Empty;
end;

function TDDSImage.GetHeight: Integer;
begin
  Result := FBitmap.Height;
end;

function TDDSImage.GetPalette: HPALETTE;
begin
  Result := FBitmap.Palette;
end;

function TDDSImage.GetWidth: Integer;
begin
  Result := FBitmap.Width;
end;

procedure TDDSImage.LoadFromClipboardFormat(AFormat: Word; AData: THandle;
  APalette: HPALETTE);
begin
  FBitmap.LoadFromClipboardFormat(AFormat, AData, APalette);
end;

procedure TDDSImage.LoadFromStream(Stream: TStream);
var
  MagicNumber: Cardinal;
  Data: PByte;
  LinearSize: Cardinal;
  FourCC: Cardinal;
  W, H: Cardinal;
  Row: Cardinal;
begin
  Progress(Self, psStarting, 0, False, Rect(0,0,0,0), 'Loading...');
  Clear;

  Stream.Read(MagicNumber, 4);
  if MagicNumber = DDS_FORMAT then
  begin
    Stream.Read(FHeader, SizeOf(FHeader));
    W := FHeader.Width;
    H := FHeader.Height;
    FBitmap.Assign(nil);
    FBitmap.PixelFormat := pf32bit;
    FBitmap.SetSize(W, H);
    if (FHeader.spf.dwFlags and DDPF_FOURCC) <> 0 then
    begin
      FourCC := FHeader.spf.dwFourCC;
      if FourCC = DXT1_FORMAT then
        DecodeDXT1(FBitmap, Stream, W, H)
      else if FourCC = DXT3_FORMAT then
        DecodeDXT3(FBitmap, Stream, W, H)
      else if FourCC = DXT5_FORMAT then
        DecodeDXT5(FBitmap, Stream, W, H)
      else
        ; // Unsupported
    end
    else if (FHeader.spf.dwFlags and DDPF_RGB) <> 0 then
    begin
      LinearSize := ((W * FHeader.spf.dwRGBBitCount + 7 ) shr 3);
      GetMem(Data, LinearSize);
      try
        for Row := 0 to H - 1 do
        begin
          Stream.Read(Data^, LinearSize);
          CopyMemory(FBitmap.ScanLine[Row], Data, LinearSize);
        end;
      finally
        FreeMem(Data);
      end;
    end
    else
      ; // Unsupported
  end;
  Progress(Self, psEnding, 100, True, Rect(0,0,0,0), 'Loading...');
end;

procedure TDDSImage.SaveToClipboardFormat(var AFormat: Word; var AData: THandle;
  var APalette: HPALETTE);
begin
  inherited;

end;

procedure TDDSImage.SaveToStream(Stream: TStream);
begin
  inherited;

end;

procedure TDDSImage.SetHeight(Value: Integer);
begin
  inherited;

end;

procedure TDDSImage.SetPalette(Value: HPalette);
begin
  inherited;

end;

procedure TDDSImage.SetWidth(Value: Integer);
begin
  inherited;

end;

procedure TDDSImage.WriteData(Stream: TStream);
begin
  inherited WriteData(Stream);
end;

{ TDDSHeader }

procedure TDDSHeader.Clear;
var
  n: Integer;
begin
  Size := 0;
  Flags := 0;
  Height := 0;
  Width := 0;
  PitchOrLinearSize := 0;
  Depth := 0;
  MipMapCount := 0;
  for n := 0 to 10 do Reserved1[n] := 0;
  spf.Clear;
  Caps := 0;
  Caps2 := 0;
  Caps3 := 0;
  Caps4 := 0;
  Reserved2 := 0;
end;

{ TDDSPixelFormat }

procedure TDDSPixelFormat.Clear;
begin
  dwSize := 0;
  dwFlags := 0;
  dwFourCC := 0;
  dwRGBBitCount := 0;
  dwRBitMask := 0;
  dwGBitMask := 0;
  dwBBitMask := 0;
  dwABitMask := 0;
end;

end.
