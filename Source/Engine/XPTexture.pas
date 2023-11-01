unit XPTexture;

interface

uses
  {$IFDEF Win32} XPEngine32, {$ELSE} XPEngine64, {$ENDIF}
  XPRoutine, XPBitmap, XPGL, XPStream, Vcl.Imaging.PNGImage,
  Windows, Classes, Graphics, Jpeg, dglOpenGL, SysUtils, TGAImage, DDSImage,
  Dialogs, UITypes;

type
  TXPTexture = class
  private
    FGL: TXPGL;
    FHandle: GLuint;
    FWidth, FHeight: Integer;
    FHaveAlpha: Boolean;
    FOnProgress: TProgressEvent;
    procedure CreateTexture(var ABuffer; AWidth, AHeight: Integer);
    procedure Progress(Sender: TObject; Stage: TProgressStage;
      PercentDone: Byte;  RedrawNow: Boolean; const R: TRect; const Msg: string); dynamic;
  public
    FlipX, FlipY, Blended: Boolean;
    Mode, MinFilter, MagFilter, Wrap: Integer;
    Color, BorderColor: TGLRGBAFloat;
    BlendSrc, BlendDest: Integer;
    property Handle: GLuint read FHandle;
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;

    constructor Create(AGL: TXPGL);
    destructor Destroy; override;
    procedure Assign(ATexture: TXPTexture);

    procedure Clear;
    procedure CopyRect(srect: TRect; drect: TRect);
    function Draw(X, Y: Integer): Boolean; overload;
    function Draw(ARect: TRect): Boolean; overload;

    procedure Activate;
    procedure SetTexture;
    procedure SetMode;
    procedure SetBlend;
    procedure SetColor;
    procedure SetFilter;
    procedure SetWrap;
    procedure SetBorder;

    procedure LoadFromBitmap(ABitmap: TBitmap; AAlpha: TBitmap = nil);
    procedure LoadFromXPBitmap(XBitmap: TXPBitmap);
    procedure LoadMonoFromXPBitmap(XBitmap: TXPBitmap);

    procedure LoadFromBMP(fname: string; aname: string = '');
    procedure LoadFromJPG(fname: string; aname: string = '');
    procedure LoadFromTIF(fname: string; aname: string = '');
    procedure LoadFromTGA(fname: string; aname: string = '');
    procedure LoadFromPNG(fname: string; aname: string = '');
    procedure LoadFromDDS(fname: string; aname: string = '');
    procedure LoadFromFile(AFileName: string; AlphaName: string = '');

    property GL: TXPGL read FGL write FGL;
    property HaveAlpha: Boolean read FHaveAlpha;
    property OnProgress: TProgressEvent read FOnProgress write FOnProgress;
  end;

  TXPTextureList = class(TList)
  private
    FGL: TXPGL;
    FOnProgress: TProgressEvent;
    function GetTextures(n: Integer): TXPTexture; inline;
    procedure Progress(Sender: TObject; Stage: TProgressStage;
      PercentDone: Byte;  RedrawNow: Boolean; const R: TRect; const Msg: string); dynamic;
  public
    constructor Create(AGL: TXPGL);
    procedure Clear; override;

    function Add(ATexture: TXPTexture): Integer;
    function AddTexture: TXPTexture; reintroduce;
    procedure AddFromFile(AFileName: string);

    property GL: TXPGL read FGL write FGL;
    property Textures[n: Integer]: TXPTexture read GetTextures; default;
    property OnProgress: TProgressEvent read FOnProgress write FOnProgress;
  end;

implementation

procedure TXPTexture.Activate;
begin
  SetTexture;
  SetMode;
  SetBlend;
  SetFilter;
  SetWrap;
  SetColor;
  SetBorder;
end;

procedure TXPTexture.Assign(ATexture: TXPTexture);
var
  mem: PByte;
  w, h: GLInt;
begin
  if ATexture = Self then Exit;
  Clear;
  if ATexture <> nil then
  begin
    ATexture.FGL.Activate;
    glBindTexture(GL_TEXTURE_2D, ATexture.FHandle);
    glGetTexLevelParameteriv(GL_TEXTURE_2D, 0, GL_TEXTURE_WIDTH, @w);
    glGetTexLevelParameteriv(GL_TEXTURE_2D, 0, GL_TEXTURE_HEIGHT, @h);
    GetMem(mem, w * h * 4);
    try
      glGetTexImage(GL_TEXTURE_2D, 0, GL_RGBA, GL_UNSIGNED_BYTE, mem);
      CreateTexture(mem^, w, h);
    finally
      FreeMem(mem);
    end;
  end;
end;

procedure TXPTexture.Clear;
begin
  if FGL <> nil then
  begin
    FGL.Activate;
    if FHandle <> 0 then glDeleteTextures(1, @FHandle);
    FHandle := 0;
    FWidth := 0;
    FHeight := 0;
  end;
end;

constructor TXPTexture.Create(AGL: TXPGL);
begin
  FGL := AGL;
  FHandle := 0;
  FWidth := 0;
  FHeight := 0;
  FlipX := False;
  FlipY := False;
  Mode := GL_MODULATE;
  MinFilter := GL_LINEAR;
  MagFilter := GL_LINEAR;
  Wrap := GL_REPEAT;
  BlendSrc := GL_SRC_ALPHA;
  BlendDest := GL_ONE_MINUS_SRC_ALPHA;
  Blended := True;
  FHaveAlpha := False;
  FOnProgress := nil;
  Color := GLRGBAFloat(1.0, 1.0, 1.0, 1.0);
  BorderColor := GLRGBAFloat(0.0, 0.0, 0.0, 0.0);
end;

destructor TXPTexture.Destroy;
begin
  Clear;
end;

procedure TXPTexture.CreateTexture(var ABuffer; AWidth, AHeight: Integer);
begin
  Clear;
  FGL.Activate;
  FWidth := AWidth;
  FHeight := AHeight;
  glGenTextures(1, @FHandle);
  glBindTexture(GL_TEXTURE_2D, FHandle);
  SetMode;
  SetWrap;
  SetFilter;
  SetBorder;
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, FWidth, FHeight, 0,
    GL_RGBA, GL_UNSIGNED_BYTE, @ABuffer);
end;

procedure TXPTexture.LoadFromFile(AFileName, AlphaName: string);
var
  sext: string;
begin
  if not FileExists(AFileName) then Exit;
  sext := LowerCase(ExtractFileExt(AFileName));
  if sext = '.png' then
    LoadFromPNG(AFileName, AlphaName)
  else if sext = '.bmp' then
    LoadFromBMP(AFileName, AlphaName)
  else if sext = '.jpg' then
    LoadFromJPG(AFileName, AlphaName)
  else if sext = '.tif' then
    LoadFromTIF(AFileName, AlphaName)
  else if sext = '.tga' then
    LoadFromTGA(AFileName, AlphaName)
  else if sext = '.dds' then
    LoadFromDDS(AFileName, AlphaName)
  else MessageDlg('Format does not support', mtError, [mbOK], 0);
end;

procedure TXPTexture.LoadFromBitmap(ABitmap: TBitmap; AAlpha: TBitmap);
var
  sp, dp, ap: PColor32Array;
  dmem: TArrayOfColor32;
  ny: Integer;
begin
  FHaveAlpha :=  (ABitmap.PixelFormat = pf32bit) or (AAlpha <> nil);
  ABitmap.PixelFormat := pf32bit;
  FWidth := ABitmap.Width;
  FHeight := ABitmap.Height;
  SetLength(dmem, FWidth * FHeight);
  if Assigned(AAlpha) then
  begin
    AAlpha.PixelFormat := pf32bit;
    for ny := 0 to FHeight - 1 do
    begin
      sp := ABitmap.ScanLine[ny];
      ap := AAlpha.ScanLine[ny];
      dp := @dmem[ny * FWidth];
      Copy_BGRA_A32_To_RGBA(sp, dp, FWidth, ap, False);
    end;
  end
  else
  begin
    for ny := 0 to FHeight - 1 do
    begin
      sp := ABitmap.ScanLine[ny];
      dp := @dmem[ny * FWidth];
      Copy_RGBA_To_BGRA(sp, dp, FWidth, False);
    end;
  end;
  CreateTexture(dmem[0], FWidth, FHeight);
  SetLength(dmem, 0);
end;

procedure TXPTexture.LoadFromBMP(fname: string; aname: string = '');
var
  bm, abm: TBitmap;
begin
  bm := TBitmap.Create;
  try
    bm.OnProgress := Progress;
    bm.LoadFromFile(fname);
    abm := nil;
    try
      if aname <> '' then
      begin
        abm := TBitmap.Create;
        abm.OnProgress := Progress;
        abm.LoadFromFile(aname);
      end;
      LoadFromBitmap(bm, abm);
    finally
      if Assigned(abm) then abm.Free;
    end;
  finally
    bm.Free;
  end;
end;

procedure TXPTexture.LoadFromDDS(fname: string; aname: string);
var
  dds, adds: TDDSImage;
begin
  dds := TDDSImage.Create;
  adds := nil;
  try
    dds.LoadFromFile(fname);
    dds.OnProgress := Progress;
    if aname <> '' then
    begin
      adds := TDDSImage.Create;
      adds.LoadFromFile(aname);
      LoadFromBitmap(dds.Bitmap, adds.Bitmap);
    end
    else LoadFromBitmap(dds.Bitmap);
  finally
    dds.Free;
    if adds <> nil then adds.Free;
  end;
end;

procedure TXPTexture.LoadFromJPG(fname: string; aname: string);
var
  bm, abm: TBitmap;
  jpg: TJPEGImage;
begin
  bm := TBitmap.Create;
  jpg := TJPEGImage.Create;
  try
    jpg.OnProgress := Progress;
    jpg.LoadFromFile(fname);
    bm.Assign(jpg);
    abm := nil;
    try
      if aname <> '' then
      begin
        abm := TBitmap.Create;
        jpg.LoadFromFile(aname);
        abm.Assign(jpg);
      end;
      LoadFromBitmap(bm, abm);
    finally
      if Assigned(abm) then abm.Free;
    end;
  finally
    bm.Free;
    jpg.Free;
  end;
end;

procedure TXPTexture.LoadFromPNG(fname, aname: string);
var
  dmem: TArrayOfColor32;
  dp: PColor32Array;
  sptr, aptr: PByteArray;
  png, apng: TPNGImage;
  ny: Integer;
begin
  png := TPNGImage.Create;
  apng := nil;
  try
    png.OnProgress := Progress;
    png.LoadFromFile(fname);
    FWidth := png.Width;
    FHeight := png.Height;
    SetLength(dmem, FWidth * FHeight);
    if aname = '' then
    begin
      for ny := 0 to FHeight - 1 do
      begin
        sptr := PByteArray(png.Scanline[ny]);
        aptr := PByteArray(png.AlphaScanline[ny]);
        dp := @dmem[ny * FWidth];
        if aptr <> nil then
          Copy_BGR_A8_To_RGBA(sptr, dp, FWidth, aptr, False)
        else Copy_BGR_A8_To_RGBA(sptr, dp, FWidth, 255, False);
      end;
    end
    else
    begin
      apng := TPNGImage.Create;
      apng.OnProgress := Progress;
      apng.LoadFromFile(aname);
      for ny := 0 to FHeight - 1 do
      begin
        sptr := PByteArray(png.Scanline[ny]);
        aptr := PByteArray(apng.Scanline[ny]);
        dp := @dmem[ny * FWidth];
        Copy_BGR_A24_To_RGBA(sptr, dp, FWidth, aptr, False)
      end;
    end;
  finally
    png.Free;
    if apng <> nil then apng.Free;
  end;
  CreateTexture(dmem[0], FWidth, FHeight);
  SetLength(dmem, 0);
end;

procedure TXPTexture.LoadFromTGA(fname, aname: string);
var
  tga, atga: TTGAImage;
begin
  tga := TTGAImage.Create;
  atga := nil;
  try
    tga.OnProgress := Progress;
    tga.LoadFromFile(fname);
    if aname <> '' then
    begin
      atga := TTGAImage.Create;
      atga.LoadFromFile(aname);
      LoadFromBitmap(tga.Bitmap, atga.Bitmap);
    end
    else LoadFromBitmap(tga.Bitmap);
  finally
    tga.Free;
    if atga <> nil then atga.Free;
  end;
end;

procedure TXPTexture.LoadFromTIF(fname, aname: string);
var
  bm, abm: TBitmap;
  pic: TWICImage;
begin
  bm := TBitmap.Create;
  pic := TWICImage.Create;
  try
    pic.OnProgress := Progress;
    pic.LoadFromFile(fname);
    bm.Assign(pic);
    abm := nil;
    try
      if aname <> '' then
      begin
        abm := TBitmap.Create;
        pic.LoadFromFile(aname);
        abm.Assign(pic);
      end;
      LoadFromBitmap(bm, abm);
    finally
      if Assigned(abm) then abm.Free;
    end;
  finally
    bm.Free;
    pic.Free;
  end;
end;

procedure TXPTexture.LoadFromXPBitmap(XBitmap: TXPBitmap);
var
  dmem: TArrayOfColor32;
  sp, dp: PColor32Array;
  ny: Integer;
begin
  FHaveAlpha := True;
  SetLength(dmem, XBitmap.Width * XBitmap.Height);
  for ny := 0 to XBitmap.Height - 1 do
  begin
    sp := XBitmap.ScanLine[ny];
    dp := @dmem[ny * XBitmap.Width];
    Copy_BGRA_To_RGBA(sp, dp, XBitmap.Width, False);
  end;
  CreateTexture(dmem[0], XBitmap.Width, XBitmap.Height);
  SetLength(dmem, 0);
end;

procedure TXPTexture.LoadMonoFromXPBitmap(XBitmap: TXPBitmap);
var
  dmem: TArrayOfColor32;
  sp, dp: PColor32Array;
  ny: Integer;
begin
  FHaveAlpha := True;
  SetLength(dmem, XBitmap.Width * XBitmap.Height);
  for ny := 0 to XBitmap.Height - 1 do
  begin
    sp := XBitmap.ScanLine[ny];
    dp := @dmem[ny * XBitmap.Width];
    CopyMono(sp, dp, XBitmap.Width, False, c32White);
  end;
  CreateTexture(dmem[0], XBitmap.Width, XBitmap.Height);
  SetLength(dmem, 0);
end;

procedure TXPTexture.Progress(Sender: TObject; Stage: TProgressStage;
  PercentDone: Byte; RedrawNow: Boolean; const R: TRect; const Msg: string);
begin
  if Assigned(FOnProgress) then
    FOnProgress(Sender, Stage, PercentDone, RedrawNow, R, Msg);
end;

procedure TXPTexture.SetMode;
// Mode = GL_MODULATE   :Texture blends with object background
// Mode = GL_DECAL      :Texture does NOT blend with object background
// Mode = GL_REPLACE    :Texture does NOT blend with object background
begin
  glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, Mode);
end;

procedure TXPTexture.SetBlend;
begin
  if Blended then
  begin
    glBlendFunc(BlendSrc, BlendDest);
    glEnable(GL_BLEND);
  end
  else glDisable(GL_BLEND);
end;

procedure TXPTexture.SetBorder;
begin
  glTexParameterfv(GL_TEXTURE_2D, GL_TEXTURE_BORDER, @BorderColor);
end;

procedure TXPTexture.SetColor;
begin
  glColor4f(Color.R, Color.G, Color.B, Color.A);
end;

procedure TXPTexture.SetFilter;
// Filter = GL_NEAREST                :Basic texture (grainy looking texture)
// Filter = GL_LINEAR                 :BiLinear filtering
// Filter = GL_NEAREST_MIPMAP_NEAREST :Basic textrue and Basic mipmapped texture
// Filter = GL_NEAREST_MIPMAP_LINEAR  :Basic texture and BiLinear Mipmapped texture
// Filter = GL_LINEAR_MIPMAP_NEAREST  :Basic mipmapped texture
// Filter = GL_LINEAR_MIPMAP_LINEAR   :BiLinear Mipmapped texture
begin
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, MagFilter);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, MinFilter);
end;

procedure TXPTexture.SetTexture;
begin
  glBindTexture(GL_TEXTURE_2D, FHandle);
  glEnable(GL_TEXTURE_2D);
end;

procedure TXPTexture.SetWrap;
// Wrap = GL_REPEAT           :Repeat texture
// Wrap = GL_CLAMP            :No repeat, used border pixel
// Wrap = GL_CLAMP_TO_BORDER  :No repeat
begin
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, Wrap); // X Axis
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, Wrap); // Y Axis
end;

procedure TXPTexture.CopyRect(srect: TRect; drect: TRect);
begin
  FGL.Activate;
  FGL.SetOrtho;
  Activate;
  glDisable(GL_DEPTH_TEST);
  glPushMatrix();
  glTranslatef(0, FGL.Height, FGL.Depth); // x, y, zorder
  glBegin(GL_QUADS);
    glTexCoord2f(srect.Left/FWidth, srect.Top/FHeight); glVertex2f(drect.Left, -drect.Top);
    glTexCoord2f(srect.Right/FWidth, srect.Top/FHeight); glVertex2f(drect.Right, -drect.Top);
    glTexCoord2f(srect.Right/FWidth, srect.Bottom/FHeight); glVertex2f(drect.Right, -drect.Bottom);
    glTexCoord2f(srect.Left/FWidth, srect.Bottom/FHeight); glVertex2f(drect.Left, -drect.Bottom);
  glEnd;
  glPopMatrix();
end;

function TXPTexture.Draw(X, Y: Integer): Boolean;
var
  qx, qy: Integer;
  drect: TRect;
begin
  Result := False;
  if (FWidth <= 0) or (FHeight <= 0) then Exit;
  drect.Left := X;
  drect.Top := Y;
  drect.Right := X + FWidth;
  drect.Bottom := Y + FHeight;
  Result := CropRect(drect, FGL.DrawRect, X, Y, FlipX, FlipY);
  if Result then
  begin
    qx := X + drect.Right - drect.Left;
    qy := Y + drect.Bottom - drect.Top;
    CopyRect(Rect(X, Y, qx, qy), drect);
  end;
end;

function TXPTexture.Draw(ARect: TRect): Boolean;
var
  px, py, qx, qy, ndx, ndy: Integer;
  srect: TRect;
begin
  Result := False;
  ndx := ARect.Right - ARect.Left;
  ndy := ARect.Bottom - ARect.Top;
  if (FWidth <= 0) or (FHeight <= 0) or (ndx <= 0) or (ndy <= 0) then Exit;
  Result := CropRect(ARect, FGL.DrawRect, px, py, FlipX, FlipY);
  if Result then
  begin
    qx := px + ARect.Right - ARect.Left;
    qy := py + ARect.Bottom - ARect.Top;
    srect.Left := px * FWidth div ndx;
    srect.Right := qx * FWidth div ndx;
    srect.Top := py * FHeight div ndy;
    srect.Bottom := qy * FHeight div ndy;
    CopyRect(srect, ARect);
  end;
end;

{ TXPTextureList }

function TXPTextureList.Add(ATexture: TXPTexture): Integer;
begin
  ATexture.FOnProgress := Progress;
  Result := inherited Add(ATexture);
end;

procedure TXPTextureList.AddFromFile(AFileName: string);
begin
  AddTexture.LoadFromFile(AFileName);
end;

function TXPTextureList.AddTexture: TXPTexture;
begin
  Result := TXPTexture.Create(FGL);
  Result.OnProgress := Progress;
  Add(Result);
end;

procedure TXPTextureList.Clear;
var
  tex: TXPTexture;
  n: Integer;
begin
  n := Count;
  while n > 0 do
  begin
    Dec(n);
    tex := TXPTexture(Items[n]);
    Items[n] := nil;
    tex.Free;
  end;
  inherited Clear;
end;

constructor TXPTextureList.Create(AGL: TXPGL);
begin
  inherited Create;
  FGL := AGL;
  FOnProgress := nil;
end;

function TXPTextureList.GetTextures(n: Integer): TXPTexture;
begin
  Result := TXPTexture(inherited Items[n]);
end;

procedure TXPTextureList.Progress(Sender: TObject; Stage: TProgressStage;
  PercentDone: Byte; RedrawNow: Boolean; const R: TRect; const Msg: string);
begin
  if Assigned(FOnProgress) then
    FOnProgress(Sender, Stage, PercentDone, RedrawNow, R, Msg);
end;

end.
