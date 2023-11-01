unit XPModel3DS;

interface

uses
  XPGL, XPObject, XPMaterial, XPRoutine, dglOpenGL,
  Windows, Classes, SysUtils, Graphics;

type
  TXPModel3DS = class
  private
    FVersion, FKFIndex: Integer;
    FKHeader: string;
    FModel: TObject;
    FStream: TStream;
    FMaterial: TXPMaterial;
    FModelObject: TObject;
    FStreamPos, FStreamSize, FNextChunkPos, FVertexPos: Integer;
    FOnProgress: TProgressEvent;
    procedure LoadChunk(AParentID: Integer);
    procedure LoadSubChunks(ANextChuckPos: Int64; AParentID: Integer);
    function ReadBool: Boolean; inline;
    function ReadWord: Word; inline;
    function ReadInteger: Integer; inline;
    function ReadFloat: GLFloat; inline;
    function ReadXYZFloat: TGLXYZFloat; inline;
    function ReadRGBFloat: TGLRGBFloat;
    function ReadRGB24ToRGBFloat: TGLRGBFloat;
    function ReadString: string; inline;
    procedure Progressing; inline;
    procedure Progress(Sender: TObject; Stage: TProgressStage;
      PercentDone: Byte;  RedrawNow: Boolean; const R: TRect; const Msg: string); dynamic;
  protected
    function ReadRGB24: TRGBColorEntry;
    function ReadRGBFloatToRGB24: TRGBColorEntry;
  public
    constructor Create(AModel: TObject);
    procedure LoadFromStream(AStream: TStream);
    property OnProgress: TProgressEvent read FOnProgress write FOnProgress;
  end;

implementation

uses
  XPModel, XPModel3DSChunk;

type
  TXPModel3DSHelper = class helper for TXPModel3DS
  private
    function GetModel: TXPModel; inline;
    procedure SetModel(AModel: TXPModel); inline;
    function GetModelObject: TXPModelObject; inline;
    procedure SetModelObject(AModelObject: TXPModelObject); inline;
    property Model: TXPModel read GetModel write SetModel;
    property ModelObject: TXPModelObject read GetModelObject write SetModelObject;
  end;

// XPModel3DS

constructor TXPModel3DS.Create(AModel: TObject);
begin
  FModel := AModel;
  FStreamSize := 0;
  FStreamPos := 0;
  FNextChunkPos := 0;
  FVertexPos := 0;
  FKFIndex := 0;
  FKHeader := '';
  FMaterial := nil;
  FModelObject := nil;
  FOnProgress := nil;
end;

procedure TXPModel3DS.LoadChunk(AParentID: Integer);
var
  chunk: TChunkRec;
  nextpos: Int64;
  n, nmat, ncount: Integer;
  v: TXPVertex;
  vt: TXPTextureMap;
  mdl: TXPModelObject;
  fmap1, fmap2, fmap3: TXPFaceMap;
begin
  nextpos := FStream.Position;
  FStream.Read(chunk, SizeOf(TChunkRec));
  Inc(nextpos, chunk.ChunkLen);
  try
    case chunk.ChunkID of
      MAIN3DS: LoadSubChunks(nextpos, chunk.ChunkID);
      VERS3DS: FVersion := ReadInteger;
      EDIT3DS: LoadSubChunks(nextpos, chunk.ChunkID);
      MASTER_SCALE: Model.Scale := ReadFloat;
      EDIT_MATERIAL:
      begin
        FMaterial := Model.Materials.AddMaterial;
        LoadSubChunks(nextpos, chunk.ChunkID);
      end;
      INT_PERCENTAGE:
      case AParentID of
        MAT_TEXTURE_MAP: FMaterial.Alpha := ReadWord / 100.0;
        MAT_BUMP_MAP: FMaterial.BumpStrength := ReadWord / 10000.0;
        MAT_OPACITY_MAP: FMaterial.Opac := ReadWord;
        MAT_TRANSPARENCY: FMaterial.Alpha := 1.0 - ReadWord / 100.0;
      end;
      FLOAT_PERCENTAGE:
      case AParentID of
        MAT_TEXTURE_MAP: FMaterial.Alpha := ReadFloat;
        MAT_BUMP_MAP: FMaterial.BumpStrength := ReadFloat;
        MAT_OPACITY_MAP: FMaterial.Opac := Round(ReadFloat * 100.0);
        MAT_TRANSPARENCY: FMaterial.Alpha := 1.0 - ReadFloat;
      end;
      MAT_COLOR_24:
      case AParentID of
        MAT_AMBIENT: FMaterial.Ambient.Color := ReadRGB24ToRGBFloat;
        MAT_DIFFUSE: FMaterial.Diffuse.Color := ReadRGB24ToRGBFloat;
        MAT_SPECULAR: FMaterial.Specular.Color := ReadRGB24ToRGBFloat;
      end;
      MAT_COLOR_F:
      case AParentID of
        MAT_AMBIENT: FMaterial.Ambient.Color := ReadRGBFloat;
        MAT_DIFFUSE: FMaterial.Diffuse.Color := ReadRGBFloat;
        MAT_SPECULAR: FMaterial.Specular.Color := ReadRGBFloat;
      end;
      MAT_MAPFILE:
      case AParentID of
        MAT_TEXTURE_MAP: FMaterial.Diffuse.MapFile := ReadString;
        MAT_BUMP_MAP: FMaterial.MapBump := ReadString;
        MAT_OPACITY_MAP: FMaterial.MapOpac := ReadString;
      end;
      MAT_MATNAME: FMaterial.Name := ReadString;
      MAT_AMBIENT: LoadSubChunks(nextpos, chunk.ChunkID);
      MAT_DIFFUSE: LoadSubChunks(nextpos, chunk.ChunkID);
      MAT_SPECULAR: LoadSubChunks(nextpos, chunk.ChunkID);
      MAT_TEXTURE_MAP: LoadSubChunks(nextpos, chunk.ChunkID);
      MAT_BUMP_MAP: LoadSubChunks(nextpos, chunk.ChunkID);
      MAT_OPACITY_MAP: LoadSubChunks(nextpos, chunk.ChunkID);
      MAT_USCALE: FMaterial.Diffuse.Scale.X := ReadFloat;
      MAT_VSCALE: FMaterial.Diffuse.Scale.Y := ReadFloat;
      MAT_UOFF: FMaterial.Diffuse.Offset.X := ReadFloat;
      MAT_VOFF: FMaterial.Diffuse.Offset.Y := ReadFloat;
      MAT_TEXROT: FMaterial.Diffuse.Rotate := ReadFloat;
      MAT_TRANSPARENCY: LoadSubChunks(nextpos, chunk.ChunkID);
      MAT_TWO_SIDE: FMaterial.TwoSided := True;
      EDIT_OBJECT{, OBJ_HIDDEN}:
      begin
        ModelObject := Model.AddObject(Model);
        ModelObject.Name := ReadString;
        LoadSubChunks(nextpos, chunk.ChunkID);
      end;
      OBJ_TRIMESH:
      begin
        FVertexPos := Model.VertexCount;
        LoadSubChunks(nextpos, chunk.ChunkID);
      end;
      OBJ_LAMP:;
      OBJ_CAMERA:;
      TRI_VERTEXS:
      begin
        ncount := ReadWord;
        v.W := 0.0;
        for n := 0 to ncount - 1 do
        begin
          Progressing;
          v.XYZ := ReadXYZFloat;
          Model.Add(v);
        end;
      end;
      TRI_MAPPINGCOORDS:
      begin
        ncount := ReadWord;
        vt.W := 0.0;
        for n := 0 to ncount - 1 do
        begin
          Progressing;
          vt.U := ReadFloat;
          vt.V := ReadFloat;
          Model.Add(vt);
        end;
      end;
      TRI_MATGROUP:
      begin
        nmat := Model.Materials.IndexOfName(ReadString);
        ncount := ReadWord;
        while ncount > 0 do
        begin
          n := ReadWord;
          ModelObject[n].MatIndex := nmat + 1;
          Dec(ncount);
        end;
      end;
      TRI_VISIBLE: ModelObject.Deleted := ReadBool;
      TRI_FACES:
      begin
        ncount := ReadWord;
        fmap1.Clear;
        fmap2.Clear;
        fmap3.Clear;
        for n := 0 to ncount - 1 do
        begin
          Progressing;

          fmap1.VertexID := FVertexPos + ReadWord + 1;
          fmap1.TextureID := fmap1.VertexID;
          fmap1.NormalID := fmap1.VertexID;

          fmap2.VertexID := FVertexPos + ReadWord + 1;
          fmap2.TextureID := fmap2.VertexID;
          fmap2.NormalID := fmap2.VertexID;

          fmap3.VertexID := FVertexPos + ReadWord + 1;
          fmap3.TextureID := fmap3.VertexID;
          fmap3.NormalID := fmap3.VertexID;

          with ModelObject.AddFace do
          begin
            Add(fmap1);
            Add(fmap2);
            Add(fmap3);
          end;
          ReadWord;
          // temp_diff:=wordbuffer AND $000F;
          // FMesh[acount - 1].Face[i * 3 + 3]:=(temp_diff AND $0004) OR 2;
          // FMesh[acount - 1].Face[i * 3 + 4]:=(temp_diff AND $0002) OR 1;
          // FMesh[acount - 1].Face[i * 3 + 5]:=(temp_diff AND $0001);
        end;
        LoadSubChunks(nextpos, chunk.ChunkID);
      end;
      TRI_MATRIX:
      begin
        for n := 0 to 11 do ModelObject.Matrix[n mod 3, n div 3] := ReadFloat;
        for n := 0 to 3 do ModelObject.Matrix[3, n] := 0.0;
      end;
      EDIT_KEYFRAME: LoadSubChunks(nextpos, chunk.ChunkID);
      KEYF_OBJ_NODE: LoadSubChunks(nextpos, chunk.ChunkID);
      KEYF_NODE_ID: FKFIndex := ReadWord;
      KEYF_NODE_HEADER: FKHeader := ReadString;
      KEYF_PIVOT:
      begin
        mdl := Model.ObjectByName[FKHeader];
        if mdl <> nil then
        begin
          mdl.Pivot := ReadXYZFloat;
          mdl.UsedPivot := True;
          Model.UsedPivot := True;
        end;
//        if FKFIndex < Model.Count then
//        begin
//          Model[FKFIndex].Pivot := ReadXYZFloat;
//          Model[FKFIndex].UsedPivot := True;
//          Model.UsedPivot := True;
//        end;
      end;
      KEYF_POSITION_TRACK:
      begin
        mdl := Model.ObjectByName[FKHeader];
        if mdl <> nil then
        begin
          ReadWord;
          ReadWord;
          ReadWord;
          ReadWord;
          ReadWord;
          n := 0;
          FSTream.Read(n, 2);
          ReadWord;
          if n > 0 then
          begin
            ReadWord;
            ReadInteger;
            mdl.Position := ReadXYZFloat;
          end;
        end;
//        if FKFIndex < Model.Count then
//        begin
//          ReadWord;
//          ReadWord;
//          ReadWord;
//          ReadWord;
//          ReadWord;
//          n := 0;
//          FSTream.Read(n, 2);
//          ReadWord;
//          if n > 0 then
//          begin
//            ReadWord;
//            ReadInteger;
//            Model[FKFIndex].Position := ReadXYZFloat;
//          end;
//        end;
      end;
      KEYF_SCALE_TRACK:
      begin
        mdl := Model.ObjectByName[FKHeader];
        if mdl <> nil then
        begin
          ReadWord;
          ReadWord;
          ReadWord;
          ReadWord;
          ReadWord;
          n := 0;
          FSTream.Read(n, 2);
          ReadWord;
          if n > 0 then
          begin
            ReadWord;
            ReadInteger;
            mdl.Scale := ReadXYZFloat;
          end;
        end;
//        if FKFIndex < Model.Count then
//        begin
//          ReadWord;
//          ReadWord;
//          ReadWord;
//          ReadWord;
//          ReadWord;
//          n := 0;
//          FSTream.Read(n, 2);
//          ReadWord;
//          if n > 0 then
//          begin
//            ReadWord;
//            ReadInteger;
//            Model[FKFIndex].Scale := ReadXYZFloat;
//          end;
//        end;
      end;
    end;
  finally
    FStream.Seek(nextpos, soBeginning);
  end;
end;

procedure TXPModel3DS.LoadFromStream(AStream: TStream);
begin
  Progress(Model, psStarting, 0, False, Rect(0, 0, 0, 0), 'Loading...');
  try
    FStream := AStream;
    FStreamSize := AStream.Size;
    FStreamPos := FStreamSize div 100;
    LoadSubChunks(AStream.Size, 0);
  finally
    Progress(Model, psEnding, 100, True, Rect(0, 0, 0, 0), 'Loading...');
  end;
  Model.SwapOrder(1, 2);
  Model.FlipX := True;
  Model.FlipZ := True;
end;

procedure TXPModel3DS.LoadSubChunks(ANextChuckPos: Int64; AParentID: Integer);
begin
  while FStream.Position < ANextChuckPos do LoadChunk(AParentID);
end;

procedure TXPModel3DS.Progress(Sender: TObject; Stage: TProgressStage;
  PercentDone: Byte; RedrawNow: Boolean; const R: TRect; const Msg: string);
begin
  if Assigned(FOnProgress) then
    FOnProgress(Sender, Stage, PercentDone, RedrawNow, R, Msg);
end;

procedure TXPModel3DS.Progressing;
begin
  if FStream.Position >= FStreamPos then
  begin
    Progress(Self, psRunning, FStream.Position * 100 div FStreamSize, False, Rect(0, 0, 0, 0), 'Loading...');
    FStreamPos := (((FStream.Position + 99) div 100) + 1) * 100;
  end;
end;

function TXPModel3DS.ReadBool: Boolean;
var
  b: Byte;
begin
  FStream.Read(b, 1);
  Result := b <> 0;
end;

function TXPModel3DS.ReadFloat: GLFloat;
begin
  FStream.Read(Result, 4);
end;

function TXPModel3DS.ReadInteger: Integer;
begin
  FStream.Read(Result, 4);
end;

function TXPModel3DS.ReadRGB24: TRGBColorEntry;
begin
  FStream.Read(Result, 3);
end;

function TXPModel3DS.ReadRGB24ToRGBFloat: TGLRGBFloat;
var
  rgb: TRGBColorEntry;
begin
  FStream.Read(rgb, 3);
  Result.R := rgb.R / 255;
  Result.G := rgb.G / 255;
  Result.B := rgb.B / 255;
end;

function TXPModel3DS.ReadRGBFloat: TGLRGBFloat;
begin
  FStream.Read(Result, 12);
end;

function TXPModel3DS.ReadRGBFloatToRGB24: TRGBColorEntry;
var
  frgb: TGLRGBFloat;
begin
  FStream.Read(frgb, 12);
  Result.R := Round(frgb.R * 255);
  Result.G := Round(frgb.G * 255);
  Result.B := Round(frgb.B * 255);
end;

function TXPModel3DS.ReadString: string;
var
  ch: Char;
begin
  Result := '';
  repeat
    ch := #0;
    FStream.Read(ch, 1);
    if ch <> #0 then Result := Result + ch;
  until ch = #0;
end;

function TXPModel3DS.ReadWord: Word;
begin
  FStream.Read(Result, 2);
end;

function TXPModel3DS.ReadXYZFloat: TGLXYZFloat;
begin
  FStream.Read(Result, 12);
end;

{ TXPModel3DSHelper }

function TXPModel3DSHelper.GetModel: TXPModel;
begin
  Result := TXPModel(FModel);
end;

function TXPModel3DSHelper.GetModelObject: TXPModelObject;
begin
  Result := TXPModelObject(FModelObject);
end;

procedure TXPModel3DSHelper.SetModel(AModel: TXPModel);
begin
  FModel := AModel;
end;

procedure TXPModel3DSHelper.SetModelObject(AModelObject: TXPModelObject);
begin
  FModelObject := AModelObject;
end;

end.

