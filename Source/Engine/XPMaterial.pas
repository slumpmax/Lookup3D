unit XPMaterial;

interface

uses
  XPRoutine, XPGL, XPList, XPStream, XPObject, XPTexture,
  Windows, dglOpenGL, Classes, SysUtils, Dialogs, Graphics, UITypes;

type
  TMatReflective = record
    Color: TGLRGBFloat;
    Dimension, Scale, Offset: TGLXYZFloat;
    Spectral, Rotate: GLFloat;
    RFLFile, MapFile: string;
    HaveScale, HaveOffset: Boolean;
    procedure Clear;
    procedure AssignFromStr(AText: string);
    procedure AssignMapFromStr(AText: string);
  end;

  TXPMaterial = class
  private
    FGL: TXPGL;
    FTexture, FAlphaTexture, FNormalTexture: TXPTexture;
    FOnProgress: TProgressEvent;
    procedure Progress(Sender: TObject; Stage: TProgressStage;
      PercentDone: Byte;  RedrawNow: Boolean; const R: TRect; const Msg: string); dynamic;
  public
    Path, Name, MapAlpha, MapBump, MapNormal, MapOpac: string;
    Ambient: TMatReflective;            // Ka [r] [g] [b], Ka specular [file.rfl] [factor], Ka xyz [x] [y] [z]
    Diffuse: TMatReflective;            // Kd [r] [g] [b], Kd specular [file.rfl] [factor], Kd xyz [x] [y] [z]
    Specular: TMatReflective;           // Ks [r] [g] [b], Ks specular [file.rfl] [factor], Ks xyz [x] [y] [z]
    Emission: TMatReflective;           // Ke [r] [g] [b], Ke specular [file.rfl] [factor], Ke xyz [x] [y] [z]
    TransmissionFilter: TMatReflective; // Tf [r] [g] [b], Tf specular [file.rfl] [factor], Tf xyz [x] [y] [z]
    Illumination: Integer;              // illum [illum_#] (0..10)
    Alpha: GLFloat;                      // d [factor] (Dissolve)
    AlphaHalo: GLFloat;                  // d -halo [factor] (Dissolve Halo)
    Translucent: GLFloat;                // Tr [factor] (Transparent)
    Shininess: GLFloat;                  // Ns [exponent] (Specular Exponent, Focus of Specular Highlight 0..1000)
    Sharpness: GLFloat;                  // sharpness [value] (0..1000);
    OpticalDensity: GLFloat;             // Ni [optical_desity] (Index of Reflaction 0.001..10)
    BumpStrength, Opac: GLFloat;
    HaveAlpha, HaveNormal, HaveDiffuse: Boolean;
    HaveAmbient, HaveSpecular, HaveBump: Boolean;
    HaveOpac, TwoSided: Boolean;

    constructor Create(AGL: TXPGL);
    destructor Destroy; override;
    procedure Clear;
    procedure Assign(AMaterial: TXPMaterial);
    procedure LoadTextures;
    function FindTexture(AFolder, AFileName: string): string;

    property Texture: TXPTexture read FTexture;
    property NormalTexture: TXPTexture read FNormalTexture;
    property GL: TXPGL read FGL write FGL;
    property OnProgress: TProgressEvent read FOnProgress write FOnProgress;
  end;

  TXPMaterialList = class(TList)
  private
    FGL: TXPGL;
    FOnProgress: TProgressEvent;
    function GetMaterials(n: Integer): TXPMaterial;
    procedure Progress(Sender: TObject; Stage: TProgressStage;
      PercentDone: Byte;  RedrawNow: Boolean; const R: TRect; const Msg: string); dynamic;
    function GetMaterialByMapName(AName: string): TXPMaterial;
  public
    Path: string;
    constructor Create(AGL: TXPGL; fname: string = '');
    constructor CreateFromResName(AGL: TXPGL; resname: string; restype: PChar);
    constructor CreateFromResID(AGL: TXPGL; resid: Integer; restype: PChar);
    procedure Clear; override;

    function AddMaterial: TXPMaterial;
    function IndexOfName(AName: string): Integer;
    function IndexOfMapName(AName: string): Integer;

    procedure ReadFromStream(AStream: TStream);
    procedure WriteToStream(AStream: TStream);

    procedure LoadFromFile(AFileName: string);
    procedure AppendFromFile(AFileName: string);
    procedure AppendFromMTL(AFileName: string);
    procedure SaveToFile(AFileName: string);
    procedure LoadTextures;

    property GL: TXPGL read FGL write FGL;
    property Materials[n: Integer]: TXPMaterial read GetMaterials; default;
    property MaterialByMapName[AName: string]: TXPMaterial read GetMaterialByMapName;
    property OnProgress: TProgressEvent read FOnProgress write FOnProgress;
  end;

implementation

{ TXPMaterial }

procedure TXPMaterial.Assign(AMaterial: TXPMaterial);
begin
  Exception.Create('Can not assign XPMaterial. code not complete.');
end;

procedure TXPMaterial.Clear;
begin
  FTexture.Clear;
  Path := '';
  Name := '';
  MapAlpha := '';
  MapNormal := '';
  MapBump := '';
  MapOpac := '';
  Ambient.Clear;
  Ambient.Color := GLRGBFloat(1.0, 1.0, 1.0);
  Diffuse.Clear;
  Diffuse.Color := GLRGBFloat(1.0, 1.0, 1.0);
  Specular.Clear;
  Specular.Color := GLRGBFloat(1.0, 1.0, 1.0);
  Emission.Clear;
  Emission.Color := GLRGBFloat(1.0, 1.0, 1.0);
  TransmissionFilter.Clear;
  TransmissionFilter.Color := GLRGBFloat(1.0, 1.0, 1.0);
  Illumination := 1;
  Alpha := 1.0;
  AlphaHalo := 1.0;
  Shininess := 0;
  Sharpness := 60;
  OpticalDensity := 1.0;
  Opac := 100;
  Translucent := 1.0;
  BumpStrength := 0.0;
  HaveAlpha := False;
  HaveNormal := False;
  HaveDiffuse := False;
  HaveSpecular := False;
  HaveAmbient := False;
  HaveBump := False;
  HaveOpac := False;
  TwoSided := False;
end;

constructor TXPMaterial.Create(AGL: TXPGL);
begin
  FGL := AGL;
  FTexture := TXPTexture.Create(FGL);
  FAlphaTexture := TXPTexture.Create(FGL);
  FNormalTexture := TXPTexture.Create(FGL);
  FTexture.OnProgress := Progress;
  FAlphaTexture.OnProgress := Progress;
  FNormalTexture.OnProgress := Progress;
  FOnProgress := nil;
  Clear;
end;

destructor TXPMaterial.Destroy;
begin
  FTexture.Free;
  FAlphaTexture.Free;
  FNormalTexture.Free;
  inherited Destroy;
end;

function TXPMaterial.FindTexture(AFolder, AFileName: string): string;
begin
  Result := StringReplace(AFileName, '/', '\', []);
  if Result = '' then Exit;

  AFolder := IncludeTrailingPathDelimiter(AFolder);
  if FileExists(AFolder + Result) then Exit;

  AFileName := ExtractFileName(Result);
  Result := AFileName;
  if FileExists(AFolder + Result) then Exit;

  Result := 'Texture\' + AFileName;
  if FileExists(AFolder + Result) then Exit;

  Result := 'Textures\' + AFileName;
  if FileExists(AFolder + Result) then Exit;

  Result := 'tex\' + AFileName;
  if FileExists(AFolder + Result) then Exit;

  Result := '';
end;

procedure TXPMaterial.LoadTextures;
var
  sname, aname: string;
begin
  if Diffuse.MapFile <> '' then
  begin
    sname := FindTexture(Path, Diffuse.MapFile);
    aname := FindTexture(Path, MapAlpha);
    if sname <> '' then
    begin
      if aname <> '' then
      begin
        FTexture.LoadFromFile(Path + sname, Path + aname);
        HaveAlpha := True;
      end
      else
      begin
        FTexture.LoadFromFile(Path + sname);
        HaveAlpha := FTexture.HaveAlpha;
      end;
    end;
  end;
  if MapNormal <> '' then
  begin
    sname := FindTexture(Path, MapNormal);
    if sname <> '' then
    begin
      FNormalTexture.LoadFromFile(Path + sname);
      HaveNormal := True;
    end;
  end;
end;

procedure TXPMaterial.Progress(Sender: TObject; Stage: TProgressStage;
  PercentDone: Byte; RedrawNow: Boolean; const R: TRect; const Msg: string);
begin
  if Assigned(FOnProgress) then
    FOnProgress(Sender, Stage, PercentDone, RedrawNow, R, Msg);
end;

{ TMatReflective }

procedure TMatReflective.AssignFromStr(AText: string);
var
  s: string;
begin
  s := FetchString(' ', AText);
  if s = 'specular' then
  begin
    RFLFile := FetchString(' ', AText);
    Spectral :=  StrToFloatDef(AText, Spectral);
  end
  else if s = 'xyz' then
  begin
    Dimension.X := StrToFloatDef(FetchString(' ', AText), Dimension.X);
    Dimension.Y := StrToFloatDef(FetchString(' ', AText), Dimension.Y);
    Dimension.Z := StrToFloatDef(AText, Dimension.Z);
  end
  else
  begin
    Color.R := StrToFloatDef(s, Color.R);
    Color.G := StrToFloatDef(FetchString(' ', AText), Color.G);
    Color.B := StrToFloatDef(AText, Color.B);
  end;
end;

procedure TMatReflective.AssignMapFromStr(AText: string);
var
  done: Boolean;
begin
  done := False;
  repeat
    if SameText(Copy(AText, 1, 3), '-s ') then
    begin
      FetchString(' ', AText);
      Scale.X := FetchDouble(' ', AText, 1.0);
      Scale.Y := FetchDouble(' ', AText, 1.0);
      Scale.Z := FetchDouble(' ', AText, 1.0);
      HaveScale := True;
    end
    else if SameText(Copy(AText, 1, 3), '-o ') then
    begin
      FetchString(' ', AText);
      Offset.X := FetchDouble(' ', AText, 0.0);
      Offset.Y := FetchDouble(' ', AText, 0.0);
      Offset.Z := FetchDouble(' ', AText, 0.0);
      HaveOffset := True;
    end
    else if SameText(Copy(AText, 1, 4), '-mm ') then
    begin
      FetchString(' ', AText);
      FetchString(' ', AText);
      FetchString(' ', AText);
    end
    else done := True;
  until done;
  MapFile := AText;
end;

procedure TMatReflective.Clear;
begin
  Color := GLRGBFloat(1.0, 1.0, 1.0);
  Dimension := GLXYZFloat(1.0, 1.0, 1.0);
  Scale := GLXYZFloat(1.0, 1.0, 1.0);
  Offset := GLXYZFloat(0.0, 0.0, 0.0);
  Rotate := 0.0;
  Spectral := 1.0;
  MapFile := '';
  RFLFile := '';
  HaveScale := False;
  HaveOffset := False;
end;

{ TXPMaterialList }

function TXPMaterialList.AddMaterial: TXPMaterial;
begin
  Result := TXPMaterial.Create(FGL);
  Result.Path := Path;
  Result.FOnProgress := Progress;
  Add(Result);
end;

procedure TXPMaterialList.AppendFromFile(AFileName: string);
var
  ext: string;
begin
  ext := UpperCase(ExtractFileExt(AFileName));
  if ext = '.MTL' then
    AppendFromMTL(AFileName)
  else raise Exception.Create('Material file not supported.');
end;

procedure TXPMaterialList.AppendFromMTL(AFileName: string);
var
  mat: TXPMaterial;
  ss: TStringList;
  nl: Integer;
  sline, s: string;
  done: Boolean;
begin
  if not FileExists(AFileName) then Exit;
  Path := IncludeTrailingPathDelimiter(ExtractFilePath(AFileName));
  ss := TStringList.Create;
  try
    ss.LoadFromFile(AFileName);
    nl := 0;
    while nl < ss.Count do
    begin
      sline := Trim(ss[nl]);
      s := FetchString(' ', sline);
      if (s = '') or (Copy(s, 1, 1) = '#') then
        Inc(nl)
      else
      begin
        if s <> 'newmtl' then raise Exception.Create('MTL file error.');
        mat := TXPMaterial.Create(FGL);
        mat.OnProgress := Progress;
        mat.Path := Path;
        mat.Name := Trim(sline);
        Inc(nl);
        done := False;
        while (nl < ss.Count) and not done do
        begin
          sline := Trim(ss[nl]);
          s := FetchString(' ', sline);
          case IndexText(s, [
            'newmtl', 'Ns', 'Ka', 'Kd', 'Ks', 'Tf', 'Ni', 'illum',
            'd', 'sharpness', 'map_Ka', 'map_Kd', 'map_Ks', 'map_Tf',
            'Km', 'map_D', 'map_Bump', 'Bump', 'refl', 'map_opacity',
            'map_Ns', 'Tr', 'Ke', 'disp', 'decal', 'map_Disp', 'map_Ke',
            'map_Tr', 'map_Kn'])
          of
            0: // newmtl
            begin
              done := True;
              Dec(nl);
            end;
            1: mat.Shininess := StrToFloatDef(sline, mat.Shininess); // Ns
            2: mat.Ambient.AssignFromStr(sline); // Ka
            3: mat.Diffuse.AssignFromStr(sline); // Kd
            4: mat.Specular.AssignFromStr(sline); // Ks
            5: mat.TransmissionFilter.AssignFromStr(sline); // Tf
            6: mat.OpticalDensity := StrToFloatDef(sline, mat.OpticalDensity); // Ni
            7: mat.Illumination := StrToIntDef(sline, mat.Illumination); // illum
            8: // d
            begin
              s := FetchString(' ', sline);
              if s = '-halo' then
                mat.AlphaHalo := StrToFloatDef(sline, mat.AlphaHalo)
              else mat.Alpha := StrToFloatDef(s, mat.Alpha)
            end;
            9: mat.Sharpness := StrToFloatDef(sline, mat.Sharpness); // sharpness
            10: mat.Ambient.AssignMapFromStr(sline); // map_Ka
            11: mat.Diffuse.AssignMapFromStr(sline); // map_Kd
            12: mat.Specular.AssignMapFromStr(sline); // map_Ks
            13: mat.TransmissionFilter.AssignMapFromStr(sline); // map_Tf
            14: ; // Km
            15: mat.MapAlpha := sline; // map_D
            16: mat.MapBump := sline; // map_Bump
            17: ; // Bump
            18: ; // refl
            19: ; // map_opacity
            20: ; // map_Ns
            21: mat.Translucent := StrToFloatDef(sline, mat.Translucent); // Tr
            22: ; // Ke
            23: ; // disp
            24: ; // decal
            25: mat.MapNormal := sline; // map_Disp
            26: mat.Emission.MapFile := sline; // map_Ke
            27: mat.TransmissionFilter.MapFile := sline; // map_Tr
            28: mat.MapNormal := sline; // map_Kn;
          else
            if (s <> '') and (s <> '#') then MessageDlg(s + ': MTL file propery error.', mtError, [mbOK], 0);
          end;
          Inc(nl);
        end;
        Add(mat);
      end;
    end;
  finally
    ss.Free;
  end;
  LoadTextures;
end;

constructor TXPMaterialList.CreateFromResName(AGL: TXPGL; resname: string; restype: PChar);
begin
  FGL := AGL;
  Path := '';
  FOnProgress := nil;
end;

constructor TXPMaterialList.CreateFromResID(AGL: TXPGL; resid: Integer; restype: PChar);
begin
  FGL := AGL;
  Path := '';
  FOnProgress := nil;
end;

procedure TXPMaterialList.Clear;
var
  mat: TXPMaterial;
  n: Integer;
begin
  n := Count;
  while n > 0 do
  begin
    Dec(n);
    mat := TXPMaterial(Items[n]);
    Items[n] := nil;
    mat.Free;
  end;
  inherited Clear;
end;

constructor TXPMaterialList.Create(AGL: TXPGL; fname: string);
begin
  FGL := AGL;
  Path := '';
  FOnProgress := nil;
  inherited Create;
end;

function TXPMaterialList.GetMaterialByMapName(AName: string): TXPMaterial;
var
  n: Integer;
begin
  n := IndexOfMapName(AName);
  if n < 0 then
    Result := nil
  else Result := Materials[n];
end;

function TXPMaterialList.GetMaterials(n: Integer): TXPMaterial;
  function ReturnAddr: Pointer;
  asm
      mov   eax,[ebp + 4]
  end;
begin
  try
    Result := TXPMaterial(Items[n]);
  except
    raise Exception.CreateFmt('XPMaterialList index out of bound (%d)', [n]) at ReturnAddr;
  end;
end;

function TXPMaterialList.IndexOfMapName(AName: string): Integer;
var
  n: Integer;
begin
  Result := -1;
  n := Count;
  while n > 0 do
  begin
    Dec(n);
    if SameText(Materials[n].Diffuse.MapFile, AName) then Result := n;
  end;
end;

function TXPMaterialList.IndexOfName(AName: string): Integer;
var
  n: Integer;
begin
  Result := -1;
  n := Count;
  while n > 0 do
  begin
    Dec(n);
    if Materials[n].Name = AName then Result := n;
  end;
end;

procedure TXPMaterialList.LoadFromFile(AFileName: string);
begin
  Clear;
  AppendFromFile(AFileName);
end;

procedure TXPMaterialList.LoadTextures;
var
  n: Integer;
begin
  for n := 0 to Count - 1 do Materials[n].LoadTextures;
end;

procedure TXPMaterialList.Progress(Sender: TObject; Stage: TProgressStage;
  PercentDone: Byte; RedrawNow: Boolean; const R: TRect; const Msg: string);
begin
  if Assigned(FOnProgress) then
    FOnProgress(Sender, Stage, PercentDone, RedrawNow, R, Msg);
end;

procedure TXPMaterialList.ReadFromStream(AStream: TStream);
begin
end;

procedure TXPMaterialList.SaveToFile(AFileName: string);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(AFileName, fmCreate);
  try
    fs.Writeln('# MTL File: ' + ExtractFileName(AFileName));
    WriteToStream(fs);
  finally
    fs.Free;
  end;
end;

procedure TXPMaterialList.WriteToStream(AStream: TStream);
var
  n: Integer;
begin
  AStream.Writeln(Format('# Material Count: %d', [Count]));
  AStream.Writeln('');
  for n := 0 to Count - 1 do
  begin
    with Materials[n], AStream do
    begin
      Writeln(Format('newmtl %s', [Name]));
      Writeln(Format('Ns %.6f', [Shininess]));
      Writeln(Format('Ka %.6f %.6f %.6f', [Ambient.Color.R, Ambient.Color.G, Ambient.Color.B]));
      Writeln(Format('Kd %.6f %.6f %.6f', [Diffuse.Color.R, Diffuse.Color.G, Diffuse.Color.B]));
      Writeln(Format('Ks %.6f %.6f %.6f', [Specular.Color.R, Specular.Color.G, Specular.Color.B]));
      Writeln(Format('Ni %.6f', [OpticalDensity]));
      Writeln(Format('d %.6f', [Alpha]));
      Writeln(Format('illum %d', [Illumination]));
      if Ambient.MapFile <> '' then Writeln(Format('map_Ka %s', [Ambient.MapFile]));
      if Diffuse.MapFile <> '' then Writeln(Format('map_Kd %s', [Diffuse.MapFile]));
      if Specular.MapFile <> '' then Writeln(Format('map_Ks %s', [Specular.MapFile]));
      if Emission.MapFile <> '' then Writeln(Format('map_Ke %s', [Emission.MapFile]));
      if TransmissionFilter.MapFile <> '' then Writeln(Format('map_Tf %s', [TransmissionFilter.MapFile]));
      if MapAlpha <> '' then Writeln(Format('map_d %s', [MapAlpha]));
      if MapBump <> '' then Writeln(Format('map_Bump %s', [MapBump]));
      if MapNormal <> '' then Writeln(Format('map_Disp %s', [MapNormal]));
      Writeln('');
    end;
  end;
end;

end.
