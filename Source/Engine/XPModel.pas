unit XPModel;

interface

uses
  XPTextStream, XPStrings, XPTexture, XPGL, XPStream, XPXML,
  XPMaterial, XPRoutine, dglOpenGL, XPList,
  Windows, Graphics, Classes, SysUtils, Math;

type
  TXPModel = class;
  TXPModelObject = class;
  PXPVertex = ^TXPVertex;
  TXPVertex = TGLXYZWFloat;
  TArrayOfXPVertice = TArrayOfGLXYZWFloat;

  TXPVertexRefList = class(TList)
  private
    function GetVertices(AIndex: Integer): PXPVertex;
    function FindIndex(var AVertex: TXPVertex; AStartPos, AEndPos: Integer): Integer;
  public
    function Add(var AVertex: TXPVertex): Integer;
    function NearIndexOf(AVertex: TXPVertex): Integer;
    property Vertices[AIndex: Integer]: PXPVertex read GetVertices; default;
  end;

  TXPTextureMap = packed record
    constructor Create(AU, AV, AW: GLFloat);
    case Integer of
      0: (U, V, W: GLFloat);
      1: (Axis: array[0..2] of GLFloat);
  end;
  TArrayOfXPTextureMap = array of TXPTextureMap;

  TXPNormalMap = packed record
    constructor Create(AI, AJ, AK: GLFloat);
    class operator Add(AValue1, AValue2: TXPNormalMap): TXPNormalMap;
    class operator Divide(AValue1: TXPNormalMap; AValue2: GLFloat): TXPNormalMap;
    case Integer of
      0: (I, J, K: GLFloat);
      1: (Axis: array[0..2] of GLFloat);
  end;
  TArrayOfXPNormalMap = array of TXPNormalMap;

  PXPFaceMap = ^TXPFaceMap;
  TXPFaceMap = record
  private
    function GetPointers(AIndex: Integer): PXPFacemap; inline;
  public
    VertexID, NormalID, TextureID: Integer;
    procedure Clear;
    property Pointers[AIndex: Integer]: PXPFacemap read GetPointers; default;
  end;
  TXPFaceMapRefList = class(TList)
  private
    FModel: TXPModel;
    function GetFaceMaps(AIndex: Integer): PXPFaceMap;
    function FindIndex(var AFaceMap: TXPFaceMap; AStartPos, AEndPos: Integer): Integer;
  public
    constructor Create(AModel: TXPModel);
    function Add(var AFaceMap: TXPFaceMap): Integer;
    function NearIndexOf(AFaceMap: TXPFaceMap): Integer;
    property FaceMaps[AIndex: Integer]: PXPFaceMap read GetFaceMaps; default;
  end;

  TXPFace = class
  private
    FPointMaps: PXPFaceMap;
    FPointCount: Integer;
    function GetPointMaps(AIndex: Integer): PXPFaceMap;
    procedure SetPointMaps(AIndex: Integer; APMap: PXPFaceMap);
  public
    Smoothed, Deleted: Boolean;
    MatIndex: Integer;
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Add(APointMap: TXPFaceMap): Integer;
    property PointCount: Integer read FPointCount;
    property PointMaps[AIndex: Integer]: PXPFaceMap read GetPointMaps write SetPointMaps; default;
  end;
  TXPFaceRefList = class(TList)
  private
    function GetFaces(AIndex: Integer): TXPFace;
  public
    property Faces[AIndex: Integer]: TXPFace read GetFaces; default;
  end;
  TXPFaceList = class(TXPFaceRefList)
  public
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
    function AddFace: TXPFace;
  end;

  TXPModelObjectList = class(TList)
  private
    function GetObjects(AIndex: Integer): TXPModelObject;
    procedure SetOrder(AIndex, AValue: Integer); virtual;
    function GetObjectByName(AName: string): TXPModelObject;
  public
    constructor Create;
    procedure Clear; override;
    function AddObject(AModel: TXPModel): TXPModelObject; overload;
    function AddObject(AObject: TXPModelObject): TXPModelObject; overload;
    function IndexOfName(AName: string): Integer;
    procedure DeleteFaceByMatID(AMatID: Integer; ADeleted: TXPBoolean = xpTrue);
    procedure ReplaceMatID(AOldIndex, ANewIndex: Integer); overload;
    procedure ReplaceMatID(ANewIndex: Integer); overload;
    procedure ReplaceOrder(AOrderIndex, AValue: Integer);
    procedure SwapOrder(AOrderIndex1, AOrderIndex2: Integer); virtual;
    procedure ReplaceFlip(AOrderIndex: Integer; AValue: TXPBoolean);
    procedure SetUsedPivot(AValue: Boolean);
    procedure PaintMat(AMaterialID: Integer);
    procedure CalcNormals;
    property Objects[AIndex: Integer]: TXPModelObject read GetObjects; default;
    property ObjectByName[AName: string]: TXPModelObject read GetObjectByName;
  end;

  TXPModelObject = class(TXPFaceRefList)
  private
    FModel: TXPModel;
    FSubObjects: TXPModelObjectList;
    FSubLevel: Integer;
    FName: string;
    FDeleted, FUsedPivot: Boolean;
    FFlip: array[0..3] of Boolean;
    FOrder: array[0..3] of Integer;
    function GetOrder(AIndex: Integer): Integer; inline;
    procedure SetOrder(AIndex, AValue: Integer); inline;
    function GetFlip(AIndex: Integer): Boolean; inline;
    procedure SetFlip(AIndex: Integer; AValue: Boolean); inline;
    procedure SetUsedPivot(AValue: Boolean);
  public
    ShowLine, ShowFace: Boolean;
    Pivot, Position, Scale: TGLXYZFloat;
    Matrix: array[0..3, 0..3] of GLFloat;
    constructor Create(AModel: TXPModel);
    constructor CreateSub(AModelObject: TXPModelObject);
    destructor Destroy; override;
    procedure Clear; override;
    function Add(AFace: TXPFace): Integer;
    function AddFace: TXPFace;
    function IndexOfPoint(AFace: TXPFace): Integer;
    procedure DeleteFaceByMatID(AMatID: Integer; ADeleted: TXPBoolean = xpTrue);
    procedure ReplaceMatID(AOldIndex, ANewIndex: Integer); overload;
    procedure ReplaceMatID(ANewIndex: Integer); overload;
    procedure ReplaceOrder(AOrderIndex, AValue: Integer);
    procedure SwapOrder(AOrderIndex1, AOrderIndex2: Integer);
    procedure ReplaceFlip(AOrderIndex: Integer; AValue: TXPBoolean);
    procedure CalcNormals;

    procedure Paint;
    procedure PaintMat(AMaterialID: Integer);
    procedure PaintFace(const AFace: TXPFace);

    procedure WriteToOBJ(AStream: TStream; var AMatIndex: Integer;
      AIsSub: Boolean = False);
    property Name: string read FName write FName;
    property Model: TXPModel read FModel;
    property SubObjects: TXPModelObjectList read FSubObjects;
    property Deleted: Boolean read FDeleted write FDeleted;
    property XOrder: Integer index 0 read GetOrder write SetOrder;
    property YOrder: Integer index 1 read GetOrder write SetOrder;
    property ZOrder: Integer index 2 read GetOrder write SetOrder;
    property WOrder: Integer index 3 read GetOrder write SetOrder;
    property FlipX: Boolean index 0 read GetFlip write SetFlip;
    property FlipY: Boolean index 1 read GetFlip write SetFlip;
    property FlipZ: Boolean index 2 read GetFlip write SetFlip;
    property FlipW: Boolean index 3 read GetFlip write SetFlip;
    property UsedPivot: Boolean read FUsedPivot write SetUsedPivot;
  end;

  TXPModel = class(TXPModelObjectList)
  private
    FPath: string;
    FFlip: array[0..3] of Boolean;
    FOrder: array[0..3] of Integer;
    FUsedPivot, FUsedMatrix, FUsedPosition, FUsedScale, FUsedAlpha: Boolean;
    FGL: TXPGL;
    FVertices: TArrayOfXPVertice;
    FTextureMaps: TArrayOfXPTextureMap;
    FNormalMaps: TArrayOfXPNormalMap;
    FFaces: TXPFaceList;
    FTextures: TXPTextureList;
    FMaterials: TXPMaterialList;
    FFaceMapRefs: TXPFaceMapRefList;
    FScale: GLFloat;
    FRunningPos: Integer;
    FOnProgress: TProgressEvent;
    function GetSizeX: GLFloat; inline;
    function GetSizeY: GLFloat; inline;
    function GetSizeZ: GLFloat; inline;
    function GetTextureMapCount: Integer;
    function GetVertexCount: Integer;
    function GetNormalMapCount: Integer;
    function GetFlip(AIndex: Integer): Boolean; inline;
    procedure SetOrder(AIndex, AValue: Integer); override;
    function GetOrder(AIndex: Integer): Integer; inline;
    procedure ProgressRunning(AStage: TProgressStage);
    procedure Progress(Sender: TObject; Stage: TProgressStage;
      PercentDone: Byte;  RedrawNow: Boolean; const R: TRect; const Msg: string); dynamic;
    procedure XMLProgress(Sender: TObject; Stage: TProgressStage;
      PercentDone: Byte;  RedrawNow: Boolean; const R: TRect; const Msg: string);
    procedure MaterialProgress(Sender: TObject; Stage: TProgressStage;
      PercentDone: Byte;  RedrawNow: Boolean; const R: TRect; const Msg: string);
  public
    Position, Center, MinPos, MaxPos: TGLXYZFloat;
    Color, LineColor: TGLRGBAFloat;
    ShowLine, ShowFace, ReversedOrder, ReversedFace: Boolean;

    constructor Create(AGL: TXPGL; AFileName: string = '');
    destructor Destroy; override;

    procedure Clear; override;
    procedure Paint;
    procedure PaintMat(AMaterialID: Integer);

    function Add(AVertex: TXPVertex): Integer; overload;
    function Add(AMap: TXPNormalMap): Integer; overload;
    function Add(AMap: TXPTextureMap): Integer; overload;
    procedure MoveBy(AX, AY, AZ: GLFloat); overload; inline;
    procedure MoveBy(APosition: TGLXYZFloat); overload;
    procedure MoveToCenter;
    procedure CalcCenter;
    procedure CalcNormals;
    procedure ClearNormals;
    procedure SmoothNormals(AAveraged: Boolean = False);
    procedure SetFlip(AIndex: Integer; AValue: Boolean); inline;
    procedure SwapOrder(AOrderIndex1, AOrderIndex2: Integer); override;
    function IndexOf(AVertex: TXPVertex): Integer; overload;
    function IndexOf(AMap: TXPTextureMap): Integer; overload;

    procedure AppendFrom3DS(AStream: TStream);
    procedure AppendFromRAW(AStream: TStream);
    procedure AppendFromOBJ(AStream: TStream);
    procedure AppendFromDAE(AStream: TStream);

    procedure AppendFromFile(AFileName: string);
    procedure LoadFromFile(AFileName: string);
    procedure SaveToFile(AFileName: string);
    procedure SaveToOBJ(AFileName: string);

    property SizeX: GLFloat read GetSizeX;
    property SizeY: GLFloat read GetSizeY;
    property SizeZ: GLFloat read GetSizeZ;
    property XOrder: Integer index 0 read GetOrder write SetOrder;
    property YOrder: Integer index 1 read GetOrder write SetOrder;
    property ZOrder: Integer index 2 read GetOrder write SetOrder;
    property WOrder: Integer index 3 read GetOrder write SetOrder;
    property FlipX: Boolean index 0 read GetFlip write SetFlip;
    property FlipY: Boolean index 1 read GetFlip write SetFlip;
    property FlipZ: Boolean index 2 read GetFlip write SetFlip;
    property FlipW: Boolean index 3 read GetFlip write SetFlip;
    property UsedAlpha: Boolean read FUsedAlpha write FUsedAlpha;
    property UsedPivot: Boolean read FUsedPivot write FUsedPivot;
    property UsedMatrix: Boolean read FUsedMatrix write FUsedMatrix;
    property UsedPosition: Boolean read FUsedPosition write FUsedPosition;
    property UsedScale: Boolean read FUsedScale write FUsedScale;
    property Faces: TXPFaceList read FFaces;
    property Vertices: TArrayOfXPVertice read FVertices;
    property TextureMaps: TArrayOfXPTextureMap read FTextureMaps;
    property NormalMaps: TArrayOfXPNormalMap read FNormalMaps;
    property VertexCount: Integer read GetVertexCount;
    property NormalMapCount: Integer read GetNormalMapCount;
    property TextureMapCount: Integer read GetTextureMapCount;
    property Materials: TXPMaterialList read FMaterials;
    property Textures: TXPTextureList read FTextures;
    property Scale: GLFloat read FScale write FScale;
    property Path: string read FPath write FPath;
    property OnProgress: TProgressEvent read FOnProgress write FOnProgress;
  end;

implementation

uses
  XPModel3DS;

{ TXPModelObject }

function TXPModelObject.Add(AFace: TXPFace): Integer;
begin
  FModel.FFaces.Add(AFace);
  Result := inherited Add(AFace);
end;

function TXPModelObject.AddFace: TXPFace;
begin
  Result := TXPFace.Create;
  Add(Result);
end;

procedure TXPModelObject.CalcNormals;
var
  n, nm, nprev, nnext, nn: Integer;
  vn: TXPNormalMap;
  u, v: TXPVertex;
  d: GLFloat;
begin
  for n := 0 to Count - 1 do
  begin
    with FModel, Faces[n] do
    begin
      for nm := 0 to PointCount - 1 do
      begin
        if nm = 0 then
          nprev := PointCount - 1
        else nprev := nm - 1;
        if nm = PointCount - 1 then
          nnext := 0
        else nnext := nm + 1;
        u := FVertices[PointMaps[nnext].VertexID - 1] - FVertices[PointMaps[nm].VertexID - 1];
        v := FVertices[PointMaps[nprev].VertexID - 1] - FVertices[PointMaps[nm].VertexID - 1];
        vn.I := u.Axis[YOrder] * v.Axis[ZOrder] - u.Axis[ZOrder] * v.Axis[YOrder];
        vn.J := u.Axis[ZOrder] * v.Axis[XOrder] - u.Axis[XOrder] * v.Axis[ZOrder];
        vn.K := u.Axis[XOrder] * v.Axis[YOrder] - u.Axis[YOrder] * v.Axis[XOrder];
        d := Sqrt(vn.I * vn.I + vn.J * vn.J + vn.K * vn.K);
        if d <> 0.0 then
        begin
          vn.I  := vn.I / d;
          vn.J := vn.J / d;
          vn.K := vn.K / d;
        end;
        nn := FModel.Add(vn);
        PointMaps[nm].NormalID := nn + 1;
      end;
    end;
  end;
  FSubObjects.CalcNormals;
end;

procedure TXPModelObject.Clear;
var
  n: Integer;
begin
  FName := '';
  ShowLine := False;
  ShowFace := True;
  UsedPivot := False;
  FDeleted := False;
  FOrder[0] := 0;
  FOrder[1] := 1;
  FOrder[2] := 2;
  FOrder[3] := 3;
  FFlip[0] := False;
  FFlip[1] := False;
  FFlip[2] := False;
  FFlip[3] := False;
  FSubObjects.Clear;
  Pivot.Clear;
  Position.Clear;
  Scale.Assign(1.0);
  for n := 0 to 15 do Matrix[n mod 4, n div 4] := 0.0;
  inherited Clear;
end;

constructor TXPModelObject.Create(AModel: TXPModel);
begin
  inherited Create;
  FSubObjects := TXPModelObjectList.Create;
  FModel := AModel;
  FSubLevel := 0;
  Clear;
end;

constructor TXPModelObject.CreateSub(AModelObject: TXPModelObject);
begin
  inherited Create;
  FSubObjects := TXPModelObjectList.Create;
  FModel := AModelObject.FModel;
  FSubLevel := AModelObject.FSubLevel + 1;
  Clear;
end;

procedure TXPModelObject.DeleteFaceByMatID(AMatID: Integer; ADeleted: TXPBoolean);
var
  nf, nmat: Integer;
begin
  for nf := 0 to Count - 1 do
  begin
    nmat := Faces[nf].MatIndex;
    if (nmat = AMatID) or
      ((AMatID = 0) and (nmat <= 0) and
      (nmat > FModel.Materials.Count))
    then
    case ADeleted of
      xpFalse: Faces[nf].Deleted := True;
      xpTrue: Faces[nf].Deleted := False;
    else
      Faces[nf].Deleted := not Faces[nf].Deleted;
    end;
  end;
  FSubObjects.DeleteFaceByMatID(AMatID, ADeleted);
end;

destructor TXPModelObject.Destroy;
begin
  Clear;
  FSubObjects.Free;
  inherited Destroy;
end;

function TXPModelObject.GetFlip(AIndex: Integer): Boolean;
begin
  Result := FFlip[AIndex];
end;

function TXPModelObject.GetOrder(AIndex: Integer): Integer;
begin
  Result := FOrder[AIndex];
end;

function TXPModelObject.IndexOfPoint(AFace: TXPFace): Integer;
var
  n, p: Integer;
begin
  Result := -1;
  n := Count;
  while (n > 0) and (Result < 0) do
  begin
    Dec(n);
    p := 4;
    Result := n;
    while (Result > 0) and (p > 0) do
    begin
      dec(p);
      if (AFace.PointMaps[p].VertexID <> Faces[n].PointMaps[p].VertexID)
        or (AFace.PointMaps[p].TextureID <> Faces[n].PointMaps[p].TextureID)
        or (AFace.PointMaps[p].NormalID <> Faces[n].PointMaps[p].NormalID)
      then Result := -1;
    end;
  end;
end;

procedure TXPModelObject.Paint;
var
  n: Integer;
begin
  if FDeleted then Exit;
  if FModel.ReversedOrder then
  begin
    for n := Count - 1 downto 0 do PaintFace(Faces[n]);
    for n := FSubObjects.Count - 1 downto 0 do FSubObjects[n].Paint;
  end
  else
  begin
    for n := 0 to Count - 1 do PaintFace(Faces[n]);
    for n := 0 to FSubObjects.Count - 1 do FSubObjects[n].Paint;
  end;
end;

procedure TXPModelObject.PaintFace(const AFace: TXPFace);
var
  n, np, nv, nt, nn, nmat, vcount, tcount, ncount: Integer;
  scaled, moved: Boolean;
  v, v0: TXPVertex;
  vt: TXPTextureMap;
  vn: TXPNormalMap;
  cl: TGLRGBAFloat;
  mat: TXPMaterial;
begin
//mat.illum
//0. Color on and Ambient off
//1. Color on and Ambient on
//2. Highlight on
//3. Reflection on and Ray trace on
//4. Transparency: Glass on, Reflection: Ray trace on
//5. Reflection: Fresnel on and Ray trace on
//6. Transparency: Refraction on, Reflection: Fresnel off and Ray trace on
//7. Transparency: Refraction on, Reflection: Fresnel on and Ray trace on
//8. Reflection on and Ray trace off
//9. Transparency: Glass on, Reflection: Ray trace off
//10. Casts shadows onto invisible surfaces

  if (FModel = nil) or AFace.Deleted then Exit;

  vcount := FModel.VertexCount;
  tcount := FModel.TextureMapCount;
  ncount := FModel.NormalMapCount;

  with AFace do
  begin
    nmat := AFace.MatIndex;
    if (nmat > 0) and (nmat <= FModel.FMaterials.Count) then
    begin
      mat := FModel.FMaterials[nmat - 1];
      mat.Texture.Activate;

      cl.RGB := mat.Diffuse.Color;
      cl.A := mat.Translucent;
      if FModel.UsedAlpha and mat.HaveAlpha and (mat.Alpha < 1.0) then
      begin
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
        glEnable(GL_BLEND);
      end
      else
      begin
        glDisable(GL_BLEND);
      end;
      scaled := mat.Diffuse.HaveScale;
      moved := mat.Diffuse.HaveOffset;
    end
    else
    begin
      cl := FModel.Color;
      glBindTexture(GL_TEXTURE_2D, 0);
      glDisable(GL_TEXTURE_2D);
      glDisable(GL_BLEND);
      scaled := False;
      moved := False;
      mat := nil;
    end;
    glColor4f(cl.R, cl.G, cl.B, cl.A);

    if ShowFace and FModel.ShowFace then
    begin
      case AFace.PointCount of
        1: glBegin(GL_POINTS);
        2: glBegin(GL_LINES);
        3: glBegin(GL_TRIANGLES);
        4: glBegin(GL_QUADS);
      else
        glBegin(GL_POLYGON);
      end;
      try
        for n := 0 to PointCount - 1 do
        begin
          if FModel.ReversedFace then
            np := PointCount - n - 1
          else np := n;
          nv := PointMaps[np].VertexID;
          nt := PointMaps[np].TextureID;
          nn := PointMaps[np].NormalID;
          if (nv > 0) and (nv <= vcount) then
          begin
            // Texture maps
            if (nt > 0) and (nt <= tcount) then
            begin
              vt := FModel.FTextureMaps[nt - 1];
              if scaled then
              begin
                vt.U := vt.U * mat.Diffuse.Scale.X;
                vt.V := vt.V * mat.Diffuse.Scale.Y;
              end;
              if moved then
              begin
                vt.U := vt.U + mat.Diffuse.Offset.X;
                vt.V := vt.V + mat.Diffuse.Offset.Y;
              end;
              glTexCoord2f(vt.U, 1.0 - vt.V);
            end;

            // Vertex maps
            v0 := FModel.FVertices[nv - 1];
            if FModel.FlipX xor FlipX then v0.X := -v0.X;
            if FModel.FUsedMatrix then
            begin
              v.X := v0.X * Matrix[0, 0] + v0.Y * Matrix[0, 1] + v0.Z * Matrix[0, 2] + Matrix[0, 3];
              v.Y := v0.X * Matrix[1, 0] + v0.Y * Matrix[1, 1] + v0.Z * Matrix[1, 2] + Matrix[1, 3];
              v.Z := v0.X * Matrix[2, 0] + v0.Y * Matrix[2, 1] + v0.Z * Matrix[2, 2] + Matrix[2, 3];
              v0 := v;
            end;
            if FModel.FUsedPivot and FUsedPivot then v0.MoveBy(-Pivot);
            if FModel.FUsedScale then v0.ScaleBy(1 / Scale);
            if FModel.FUsedPosition then v0.MoveBy(-Position);
            v.X := v0.Axis[XOrder];
            v.Y := v0.Axis[YOrder];
            v.Z := v0.Axis[ZOrder];
            v.W := v0.AXis[WOrder];
//            if FModel.FlipX xor FlipX then v.X := -v.X;
            if FModel.FlipY xor FlipY then v.Y := -v.Y;
            if FModel.FlipZ xor FlipZ then v.Z := -v.Z;
            if FModel.FlipW xor FlipW then v.W := -v.W;

            // Normal maps
            v.MoveBy(FModel.Position);
            if (nn > 0) and (nn <= ncount) then
            begin
              vn := FModel.FNormalMaps[nn - 1];
              glNormal3f(vn.Axis[XOrder], vn.Axis[YOrder], vn.Axis[ZOrder]);
            end
            else glNormal3f(v.X, v.Y, v.Z);
            glVertex3f(v.X, v.Y, v.Z);
          end;
        end;
      finally
        glEnd;
      end;
    end;

    if ShowLine and FModel.ShowLine then
    begin
      glColor4f(FModel.LineColor.R, FModel.LineColor.G, FModel.LineColor.B, FModel.LineColor.A);
      case AFace.PointCount of
        1: glBegin(GL_POINTS);
        2: glBegin(GL_LINES);
      else
        glBegin(GL_LINE_LOOP);
      end;
      try
        for np := 0 to PointCount - 1 do
        begin
          nv := PointMaps[np].VertexID;
          if (nv > 0) and (nv <= vcount) then
          begin
            v := FModel.FVertices[nv - 1];
            glVertex3f(v.X + FModel.Position.X, v.Y + FModel.Position.Y, v.Z + FModel.Position.Z);
          end;
        end;
      finally
        glEnd;
      end;
    end;
  end;
end;

procedure TXPModelObject.PaintMat(AMaterialID: Integer);
var
  n, nmat: Integer;
begin
  if FDeleted then Exit;
  for n := 0 to Count - 1 do
  begin
    nmat := Faces[n].MatIndex;
    if (nmat = AMaterialID) or
      ((AMaterialID <= 0) and (nmat <= 0) and
      (nmat > FModel.FMaterials.Count))
    then PaintFace(Faces[n])
  end;
  FSubObjects.PaintMat(AMaterialID);
end;

procedure TXPModelObject.ReplaceFlip(AOrderIndex: Integer; AValue: TXPBoolean);
begin
  case AValue of
    xpFalse: FFlip[AOrderIndex] := False;
    xpTrue: FFlip[AOrderIndex] := True;
  else
    FFlip[AOrderIndex] := not FFlip[AOrderIndex];
  end;
  FSubObjects.ReplaceFlip(AOrderIndex, AValue);
end;

procedure TXPModelObject.ReplaceMatID(ANewIndex: Integer);
var
  n: Integer;
begin
  for n := 0 to Count - 1 do Faces[n].MatIndex := ANewIndex;
  FSubObjects.ReplaceMatID(ANewIndex);
end;

procedure TXPModelObject.ReplaceOrder(AOrderIndex, AValue: Integer);
begin
  FOrder[AOrderIndex] := AValue;
  FSubObjects.ReplaceOrder(AOrderIndex, AValue);
end;

procedure TXPModelObject.SetFlip(AIndex: Integer; AValue: Boolean);
begin
  if AValue then
    ReplaceFlip(AIndex, xpTrue)
  else ReplaceFlip(AIndex, xpFalse);
end;

procedure TXPModelObject.SetOrder(AIndex, AValue: Integer);
begin
  ReplaceOrder(AIndex, AValue);
end;

procedure TXPModelObject.SetUsedPivot(AValue: Boolean);
begin
  FUsedPivot := AValue;
  FSubObjects.SetUsedPivot(AValue);
end;

procedure TXPModelObject.SwapOrder(AOrderIndex1, AOrderIndex2: Integer);
var
  order: Integer;
begin
  order := FOrder[AOrderIndex1];
  FOrder[AOrderIndex1] := FOrder[AOrderIndex2];
  FOrder[AOrderIndex2] := order;
  FSubObjects.SwapOrder(AOrderIndex1, AOrderIndex2);
end;

procedure TXPModelObject.ReplaceMatID(AOldIndex, ANewIndex: Integer);
var
  n, nmat: Integer;
begin
  for n := 0 to Count - 1 do
  begin
    nmat := Faces[n].MatIndex;
    if (nmat = AOldIndex) or
      ((AOldIndex = 0) and (nmat <= 0) and
      (nmat > FModel.Materials.Count))
    then Faces[n].MatIndex := ANewIndex;
  end;
  for n := 0 to FSubObjects.Count - 1 do FSubObjects[n].ReplaceMatID(AOldIndex, ANewIndex);
end;

procedure TXPModelObject.WriteToOBJ(AStream: TStream; var AMatIndex: Integer;
  AIsSub: Boolean);
var
  nv, ni, np: Integer;
  s: string;
  n: Integer;
  fmap: PXPFaceMap;
begin
  if not AIsSub then
  begin
    s := 'o ' + Name;
    AStream.Writeln(s);
  end
  else
  begin
    s := 'g ' + Name;
    AStream.Writeln(s);
  end;
  for nv := 0 to Count - 1 do
  begin
    ni := Faces[nv].MatIndex;
    if ni <> AMatIndex then
    begin
      if (ni > 0) and (ni <= FModel.FMaterials.Count) then
        s := 'usemtl ' + FModel.FMaterials[ni - 1].Name
      else s := 'usemtl';
      AStream.Writeln(s);
      AMatIndex := ni;
    end;
    if not Faces[nv].Deleted then
    begin
      s := 'f';
      for np := 0 to Faces[nv].PointCount - 1 do
      begin
        if Faces[nv].PointCount > 3 then
        begin
          s := s + '#';
        end;

        fmap := Faces[nv].PointMaps[np];
        s := s + ' ';
        if fmap.VertexID > 0 then s := s + IntToStr(fmap.VertexID);
        s := s + '/';
        if fmap.TextureID > 0 then s := s + IntToStr(fmap.TextureID);
        if fmap.NormalID > 0 then s := s + '/' + IntToStr(fmap.NormalID);
      end;
      AStream.Writeln(s);
    end;
  end;
  AStream.Writeln(Format('# %d faces'#13#10, [Count]));
  for n := 0 to FSubObjects.Count - 1 do
  begin
    FSubObjects[n].WriteToOBJ(AStream, AMatIndex, True);
  end;
end;

{ TXPTextureMap }

constructor TXPTextureMap.Create(AU, AV, AW: GLFloat);
begin
  U := AU;
  V := AV;
  W := AW;
end;

{ TXPModel }

function TXPModel.Add(AMap: TXPTextureMap): Integer;
begin
  Result := TextureMapCount;
  SetLength(FTextureMaps, Result + 1);
  FTextureMaps[Result] := AMap;
end;

function TXPModel.Add(AMap: TXPNormalMap): Integer;
begin
  Result := NormalMapCount;
  SetLength(FNormalMaps, Result + 1);
  FNormalMaps[Result] := AMap;
end;

function TXPModel.Add(AVertex: TXPVertex): Integer;
begin
  Result := VertexCount;
  SetLength(FVertices, Result + 1);
  FVertices[Result] := AVertex;
end;

procedure TXPModel.AppendFrom3DS(AStream: TStream);
var
  f3d: TXPModel3DS;
begin
  f3d := TXPModel3DS.Create(Self);
  try
    f3d.OnProgress := FOnProgress;
    f3d.LoadFromStream(AStream);
  finally
    f3d.Free;
  end;
end;

procedure TXPModel.AppendFromDAE(AStream: TStream);
var
  xml: TXPXML;
  mnode, gnode, snode: TXPNode;
  obj: TXPModelObject;
  n, nl, nv, nt, nn: Integer;
  sl: TStringList;
  v: TXPVertex;
  vt: TXPTextureMap;
  vn: TXPNormalMap;
  face: TXPFace;
  fmap: TXPFaceMap;
  s, sp: string;
begin
  ProgressRunning(psStarting);
  try
    xml := TXPXML.Create;
    xml.OnProgress := XMLProgress;
    sl := TStringList.Create;
    try
      xml.LoadFromStream(AStream);
      mnode := xml.Nodes.FindNode('COLLADA/library_geometries');
      if mnode <> nil then
      begin
        for n := 0 to mnode.SubNodes.Count - 1 do
        begin
          ProgressRunning(psRunning);
          gnode := mnode.SubNodes[n];
          if SameText(gnode.Key, 'geometry') then
          begin
            obj := TXPModelObject.Create(Self);
            obj.Name := gnode.Attributes['name'];
            gnode := gnode.SubNodes.NodeByName['mesh'];
            if gnode <> nil then
            begin
              nv := VertexCount;
              nt := TextureMapCount ;
              nn := NormalMapCount;
              snode := gnode.SubNodes[0].SubNodes[0];
              sl.Text := snode.FirstSubKey;
              for nl := 0 to sl.Count - 1 do
              begin
                ProgressRunning(psRunning);
                s := sl[nl];
                v.X := FetchDouble(' ', s);
                v.Y := FetchDouble(' ', s);
                v.Z := FetchDouble(' ', s);
                v.W := 0.0;
                Add(v);
              end;
              snode := gnode.SubNodes[1].SubNodes[0];
              sl.Text := snode.FirstSubKey;
              for nl := 0 to sl.Count - 1 do
              begin
                ProgressRunning(psRunning);
                s := sl[nl];
                vn.I := FetchDouble(' ', s);
                vn.J := FetchDouble(' ', s);
                vn.K := FetchDouble(' ', s);
                Add(vn);
              end;
              snode := gnode.SubNodes[2].SubNodes[0];
              sl.Text := snode.FirstSubKey;
              for nl := 0 to sl.Count - 1 do
              begin
                ProgressRunning(psRunning);
                s := sl[nl];
                vt.U := FetchDouble(' ', s);
                vt.V := FetchDouble(' ', s);
                vt.W := 0.0;
                Add(vt);
              end;
              snode := gnode.SubNodes[4];
              for nl := 3 to snode.SubNodes.Count - 1 do
              begin
                face := TXPFace.Create;
                face.Smoothed := True;
                s := snode.SubNodes[nl].FirstSubKey;
                sp := FetchString(' ', s);
                while sp <> '' do
                begin
                  ProgressRunning(psRunning);
                  fmap.VertexID := FetchInteger(' ', s) + nv;
                  fmap.NormalID := FetchInteger(' ', s) + nn;
                  fmap.TextureID := FetchInteger(' ', s) + nt;
                  face.Add(fmap);
                  sp := FetchString(' ', s);
                end;
                obj.Add(face);
              end;
            end;
            Add(obj);
          end;
        end;
      end;
    finally
      sl.Free;
      xml.Free;
    end;
  finally
    ProgressRunning(psEnding);
  end;
end;

procedure TXPModel.AppendFromFile(AFileName: string);
const
  ExtIndexs: array[0..3] of string = ('.RAW', '.3DS', '.OBJ', '.DAE');
var
  fs: TFileStream;
  ext: string;
  n: Integer;
begin
  FPath := IncludeTrailingPathDelimiter(ExtractFilePath(AFileName));
  FMaterials.Path := FPath;
  ext := UpperCase(ExtractFileExt(AFileName));
  n :=StringIndex(ext, ExtIndexs);
  if n < 0 then raise Exception.Create('Not support ' + ext);
  fs := TFileStream.Create(AFileName, fmOpenRead);
  try
    case n of
      1: AppendFrom3DS(fs);
      2: AppendFromOBJ(fs);
      3: AppendFromDAE(fs);
    else
      AppendFromRAW(fs);
    end;
  finally
    fs.Free;
  end;
  FMaterials.LoadTextures;
end;

constructor TXPModel.Create(AGL: TXPGL; AFileName: string);
begin
  FGL := AGL;
  FOnProgress := nil;
  FFaces := TXPFaceList.Create;
  FTextures := TXPTextureList.Create(FGL);
  FMaterials := TXPMaterialList.Create(FGL);
  FFaceMapRefs := TXPFaceMapRefList.Create(Self);
  FTextures.OnProgress := MaterialProgress;
  FMaterials.OnProgress := MaterialProgress;
  inherited Create;
  if AFileName <> '' then AppendFromFile(AFileName);
end;

procedure TXPModel.CalcCenter;
var
  v: TXPVertex;
  n: Integer;
begin
  n := VertexCount;
  if n = 0 then
  begin
    Center.Clear;
    MinPos.Clear;
    MaxPos.Clear;
    Exit;
  end;
  v := Vertices[0];
  MinPos := v.XYZ;
  MaxPos := v.XYZ;
  while n > 1 do
  begin
    Dec(n);
    v := Vertices[n];
    if v.X < MinPos.X then MinPos.X := v.X;
    if v.Y < MinPos.Y then MinPos.Y := v.Y;
    if v.Z < MinPos.Z then MinPos.Z := v.Z;
    if v.X > MaxPos.X then MaxPos.X := v.X;
    if v.Y > MaxPos.Y then MaxPos.Y := v.Y;
    if v.Z > MaxPos.Z then MaxPos.Z := v.Z;
  end;
  Center := (MinPos + MaxPos) / 2.0;
  Center.ReOrder(XOrder, YOrder, ZOrder);
  MinPos.ReOrder(XOrder, YOrder, ZOrder);
  MaxPos.ReOrder(XOrder, YOrder, ZOrder);
end;

procedure TXPModel.CalcNormals;
var
  n: Integer;
begin
  SetLength(FNormalMaps, 0);
  for n := 0 to Count - 1 do Objects[n].CalcNormals;
end;

procedure TXPModel.Clear;
begin
  inherited Clear;
  Position.Clear;
  Center.Clear;
  MinPos.Clear;
  MaxPos.Clear;
  FScale := 1.0;
  FOrder[0] := 0;
  FOrder[1] := 1;
  FOrder[2] := 2;
  FOrder[3] := 3;
  FFlip[0] := False;
  FFlip[1] := False;
  FFlip[2] := False;
  FFlip[3] := False;
  FUsedAlpha := False;
  FUsedPivot := False;
  FUsedMatrix := False;
  FUsedPosition := False;
  FUsedScale := False;
  FPath := '';
  Color := GLRGBAFloat(1.0, 1.0, 1.0, 1.0);
  LineColor := GLRGBAFloat(0.0, 0.0, 0.0, 1.0);
  ShowLine := False;
  ShowFace := True;
  ReversedOrder := False;
  ReversedFace := False;
  FMaterials.Clear;
  FTextures.Clear;
  FFaces.Clear;
  FFaceMapRefs.Clear;
  SetLength(FVertices, 0);
  SetLength(FNormalMaps, 0);
  SetLength(FTextureMaps, 0);
end;

procedure TXPModel.ClearNormals;
begin
  SetLength(FNormalMaps, 0);
end;

destructor TXPModel.Destroy;
begin
  Clear;
  FFaces.Free;
  FTextures.Free;
  FMaterials.Free;
  FFaceMapRefs.Free;
  inherited;
end;

function TXPModel.GetFlip(AIndex: Integer): Boolean;
begin
  Result := FFlip[AIndex];
end;

function TXPModel.GetOrder(AIndex: Integer): Integer;
begin
  Result := FOrder[AIndex];
end;

function TXPModel.GetNormalMapCount: Integer;
begin
  Result := Length(FNormalMaps);
end;

function TXPModel.GetSizeX: GLFloat;
begin
  Result := MaxPos.X - MinPos.X;
end;

function TXPModel.GetSizeY: GLFloat;
begin
  Result := MaxPos.Y - MinPos.Y;
end;

function TXPModel.GetSizeZ: GLFloat;
begin
  Result := MaxPos.Z - MinPos.Z;
end;

function TXPModel.GetTextureMapCount: Integer;
begin
  Result := Length(FTextureMaps);
end;

function TXPModel.GetVertexCount: Integer;
begin
  Result := Length(FVertices);
end;

function TXPModel.IndexOf(AVertex: TXPVertex): Integer;
var
  n: Integer;
begin
  Result := -1;
  n := VertexCount;
  while (n > 0) and (Result < 0) do
  begin
    Dec(n);
    if (AVertex.X = FVertices[n].X)
      and (AVertex.Y = FVertices[n].Y)
      and (AVertex.Z = FVertices[n].Z)
    then Result := n;
  end;
end;

function TXPModel.IndexOf(AMap: TXPTextureMap): Integer;
var
  n: Integer;
begin
  Result := -1;
  n := TextureMapCount;
  while (n > 0) and (Result < 0) do
  begin
    Dec(n);
    if (AMap.U = FTextureMaps[n].U)
      and (AMap.V = FTextureMaps[n].V)
      and (AMap.W = FTextureMaps[n].W)
    then Result := n;
  end;
end;

procedure TXPModel.LoadFromFile(AFileName: string);
begin
  Clear;
  AppendFromFile(AFileName);
end;

procedure TXPModel.AppendFromRAW(AStream: TStream);
var
  obj: TXPModelObject;
  vertex: TXPVertex;
  face: TXPFace;
  fmap: TXPFaceMap;
  nx, nv, npos, nsize: Integer;
  s: string;
begin
  Progress(Self, psStarting, 0, False, Rect(0, 0, 0, 0), 'Loading...');
  try
    nsize := AStream.Size;
    npos := 0;
    obj := TXPModelObject.Create(Self);
    while AStream.Readln(s) do
    begin
      if AStream.Position >= npos then
      begin
        Progress(Self, psRunning, AStream.Position * 100 div nsize, False, Rect(0, 0, 0, 0), 'Loading...');
        npos := (((AStream.Position + 99) div 100) + 1) * 100;
      end;
      fmap.Clear;
      face := TXPFace.Create;
      for nv := 0 to 3 do
      begin
        vertex.X := FetchDouble(' ', s);
        vertex.Z := -FetchDouble(' ', s);
        vertex.Y := FetchDouble(' ', s);
        nx := IndexOf(vertex);
        if nx < 0 then nx := Add(vertex);
        fmap.VertexID := nx + 1;
        face.Add(fmap);
      end;
      obj.Add(face);
    end;
    Add(obj);
  finally
    Progress(Self, psEnding, 100, True, Rect(0, 0, 0, 0), 'Loading...');
  end;
end;

procedure TXPModel.MoveBy(AX, AY, AZ: GLFloat);
begin
  MoveBy(GLXYZFloat(AX, AY, AZ));
end;

procedure TXPModel.MoveBy(APosition: TGLXYZFloat);
var
  n: Integer;
begin
  n := VertexCount;
  while n > 0 do
  begin
    Dec(n);
    with Vertices[n] do
    begin
      Axis[XOrder] := Axis[XOrder] + APosition.X;
      Axis[YOrder] := Axis[YOrder] + APosition.Y;
      Axis[ZOrder] := Axis[ZOrder] + APosition.Z;
    end;
  end;
  MinPos.MoveBy(APosition);
  MaxPos.MoveBy(APosition);
  Center.MoveBy(APosition);
end;

procedure TXPModel.MoveToCenter;
begin
  CalcCenter;
  MoveBy(-Center);
end;

procedure TXPModel.AppendFromOBJ(AStream: TStream);
type
  TOBJCommand = (cmV = 0, cmVN, cmVT, cmF, cmO, cmMTLLIB, cmUSEMTL, cmS, cmG);
const
  OBJCommandList: array[TOBJCommand] of string = (
   'v', 'vn', 'vt', 'f', 'o', 'mtllib', 'usemtl', 's', 'g');
var
  obj, mobj: TXPModelObject;
  vertex: TXPVertex;
  vnormal: TXPNormalMap;
  vertext: TXPTextureMap;
  face: TXPFace;
  fmap: TXPFaceMap;
  sbuf, sn : string;
  is_smooth: Boolean;
  n, np, nt, nn, npos, nsize, mat_id: Integer;
begin
  Progress(Self, psStarting, 0, False, Rect(0, 0, 0, 0), 'Loading');
  try
    nsize := AStream.Size;
    npos := nsize div 100;
    mat_id := 0;
    is_smooth := False;
    obj := AddObject(Self);
    obj.FName := '';
    mobj := obj;
    while AStream.Readln(sbuf) do
    begin
      if AStream.Position >= npos then
      begin
        Progress(Self, psRunning, AStream.Position * 100 div nsize, False, Rect(0, 0, 0, 0), 'Loading...');
        npos := (((AStream.Position + 99) div 100) + 1) * 100;
      end;
      case TOBJCommand(FetchStringIndex(sbuf, OBJCommandList)) of
        cmV:
        begin
          vertex.X := FetchDouble(' ', sbuf, 0.0);
          vertex.Y := FetchDouble(' ', sbuf, 0.0);
          vertex.Z := FetchDouble(' ', sbuf, 0.0);
          vertex.W := FetchDouble(' ', sbuf, 0.0);
          Add(vertex);
        end;
        cmVN:
        begin
          vnormal.I := FetchDouble(' ', sbuf, 0.0);
          vnormal.J := FetchDouble(' ', sbuf, 0.0);
          vnormal.K := FetchDouble(' ', sbuf, 0.0);
          Add(vnormal);
        end;
        cmVT:
        begin
          vertext.U := FetchDouble(' ', sbuf, 0.0);
          vertext.V := FetchDouble(' ', sbuf, 0.0);
          vertext.W := FetchDouble(' ', sbuf, 0.0);
          Add(vertext);
        end;
        cmF:
        begin
          np := VertexCount + 1;
          nt := TextureMapCount + 1;
          nn := NormalMapCount + 1;
          face := TXPFace.Create;
          sn := FetchString(' ', sbuf);
          while sn <> '' do
          begin
            n := FetchInteger('/', sn, 0);
            if n < 0 then n := np + n;
            fmap.VertexID := n;
            n := FetchInteger('/', sn, 0);
            if n < 0 then n := nt + n;
            fmap.TextureID := n;
            n := FetchInteger('/', sn, 0);
            if n < 0 then n := nn + n;
            fmap.NormalID := n;
            face.Add(fmap);
            sn := FetchString(' ', sbuf);
          end;
          face.MatIndex := mat_id;
          face.Smoothed := is_smooth;
          obj.Add(face);
        end;
        cmO:
        begin
          if obj.Count > 0 then
          begin
            obj := AddObject(Self);
//              is_smooth := False;
//              mat_id := 0;
          end;
          obj.FName := sbuf;
          mobj := obj;
        end;
        cmMTLLIB: FMaterials.LoadFromFile(FPath + sbuf);
        cmUSEMTL: mat_id := FMaterials.IndexOfName(sbuf) + 1;
        cmS: is_smooth := StrToIntDef(sbuf, 0) <> 0;
        cmG:
        begin
          if obj.FSubLevel > 0 then
          begin
            if obj.Count > 0 then
            begin
              obj := mobj.FSubObjects.AddObject(mobj);
//                is_smooth := False;
//                mat_id := 0;
            end;
            obj.FName := sbuf;
          end
          else
          begin
            obj := mobj.FSubObjects.AddObject(mobj);
//              is_smooth := False;
//              mat_id := 0;
            obj.FName := sbuf;
          end;
        end;
      end;
    end;
  finally
    Progress(Self, psEnding, 100, True, Rect(0, 0, 0, 0), 'Loading');
  end;
end;

procedure TXPModel.Paint;
var
  n: Integer;
begin
  if ReversedOrder then
  begin
    for n := Count - 1 downto 0 do Objects[n].Paint;
  end
  else for n := 0 to Count - 1 do Objects[n].Paint
end;

procedure TXPModel.PaintMat(AMaterialID: Integer);
var
  n: Integer;
begin
  for n := 0 to Count - 1 do Objects[n].PaintMat(AMaterialID);
end;

procedure TXPModel.Progress(Sender: TObject; Stage: TProgressStage;
  PercentDone: Byte; RedrawNow: Boolean; const R: TRect; const Msg: string);
begin
  if Assigned(FOnProgress) then
    FOnProgress(Sender, Stage, PercentDone, RedrawNow, R, Msg);
end;

procedure TXPModel.ProgressRunning(AStage: TProgressStage);
begin
  case AStage of
    psStarting:
    begin
      FRunningPos := 0;
      Progress(Self, AStage, 0, False, Rect(0, 0, 0, 0), 'Loading...')
    end;
    psEnding: Progress(Self, AStage, 100, True, Rect(0, 0, 0, 0), 'Loading...')
  else
    Inc(FRunningPos);
    if (FRunningPos mod 100) = 0 then
      Progress(Self, AStage, (FRunningPos div 100) mod 100, False, Rect(0, 0, 0, 0), 'Loading...');
  end;
end;

procedure TXPModel.SaveToFile(AFileName: string);
var
  ext: string;
begin
  ext := LowerCase(ExtractFileExt(AFileName));
  if ext = '.obj' then
    SaveToOBJ(AFileName)
  else Exception.Create('Not support ' + ExtractFileExt(AFileName));
end;

procedure TXPModel.SaveToOBJ(AFileName: string);
var
  fs: TFileStream;
  obj: TXPModelObject;
  nobj, nmat, nv, np: Integer;
  s: string;
begin
  fs := TFileStream.Create(AFileName, fmCreate);
  try
    fs.Writeln('# Lookup 3D Viewer OBJ File: ' + ExtractFileName(AFileName));
    s := FormatDateTime('dd/mm/yyyy, hh:nn:ss', Now);
    fs.Writeln('# ' + s);
    if FMaterials.Count > 0 then
    begin
      s := ChangeFileExt(AFileName, '.mtl');
      FMaterials.SaveToFile(s);
      fs.Writeln('mtllib ' + ExtractFileName(s));
    end;
    if VertexCount > 0 then
    begin
      for nv := 0 to VertexCount - 1 do
      begin
        s := 'v';
        for np := 0 to 2 do s := Format('%s %.6f', [s, FVertices[nv].Axis[np]]);
        fs.Writeln(s);
      end;
      fs.Writeln(Format('# %d vertices'#13#10, [VertexCount]));
    end;
    if TextureMapCount > 0 then
    begin
      for nv := 0 to TextureMapCount - 1 do
      begin
        s := 'vt';
        for np := 0 to 1 do s := Format('%s %.6f', [s, TextureMaps[nv].Axis[np]]);
        fs.Writeln(s);
      end;
      fs.Writeln(Format('# %d texture maps'#13#10, [TextureMapCount]));
    end;
    if NormalMapCount > 0 then
    begin
      for nv := 0 to NormalMapCount - 1 do
      begin
        s := 'vn';
        for np := 0 to 2 do s := Format('%s %.6f', [s, FNormalMaps[nv].Axis[np]]);
        fs.Writeln(s);
      end;
      fs.Writeln(Format('# %d normal maps'#13#10, [NormalMapCount]));
    end;
    nmat := 0;
    for nobj := 0 to Count - 1 do
    begin
      obj := Objects[nobj];
      obj.WriteToOBJ(fs, nmat);
    end;
  finally
    fs.Free;
  end;
end;

procedure TXPModel.SetFlip(AIndex: Integer; AValue: Boolean);
begin
  FFlip[AIndex] := AValue;
end;

procedure TXPModel.SetOrder(AIndex, AValue: Integer);
begin
  FOrder[AIndex] := AValue;
  inherited SetOrder(AIndex, AValue);
end;

procedure TXPModel.SmoothNormals(AAveraged: Boolean);
var
  nv, nf, np, nn, nmap: Integer;
  v1, v2: TXPVertex;
  vn: TXPNormalMap;
begin
  if FFaceMapRefs.Count = 0 then
  begin
    for nf := 0 to FFaces.Count - 1 do
    begin
      for np := 0 to FFaces[nf].PointCount - 1 do
      begin
        FFaceMapRefs.Add(FFaces[nf][np]^);
      end;
    end;
  end;
  if FFaceMapRefs.Count < 2 then Exit;
  np := -1;
  repeat
    Inc(np);
    nv := FFaceMapRefs[np].VertexID;
  until (nv <= VertexCount) or (np >= FFaceMapRefs.Count - 1);
  nf := np + 1;
  nmap := 1;
  v1 := FVertices[nv - 1];
  nn := FFaceMapRefs[np].NormalID;
  vn := FNormalMaps[nn - 1];
  while nf < FFaceMapRefs.Count do
  begin
    nv := FFaceMapRefs[nf].VertexID;
    if nv <= VertexCount then
    begin
      v2 := FVertices[nv - 1];
      if v1.Compare(v2) <> 0 then
      begin
        if AAveraged then vn := vn / nmap;
        FNormalMaps[nn - 1] := vn;
        while np < nf do
        begin
          FFaceMapRefs[np].NormalID := nn;
          Inc(np);
        end;
        v1 := FVertices[FFaceMapRefs[nf].VertexID - 1];
        nn := FFaceMapRefs[nf].NormalID;
        vn := FNormalMaps[nn - 1];
        nmap := 1;
        Inc(nf);
      end
      else
      begin
        vn := vn + FNormalMaps[FFaceMapRefs[nf].NormalID - 1];
        Inc(nf);
        Inc(nmap);
      end;
    end
    else Inc(nf);
  end;
  if nmap > 1 then
  begin
    if AAveraged then vn := vn / nmap;
    FNormalMaps[nn - 1] := vn;
    while np < nf do
    begin
      FFaceMapRefs[np].NormalID := nn;
      Inc(np);
    end;
  end;
end;

procedure TXPModel.SwapOrder(AOrderIndex1, AOrderIndex2: Integer);
var
  n: Integer;
begin
  n := FOrder[AOrderIndex1];
  FOrder[AOrderIndex1] := FOrder[AOrderIndex2];
  FOrder[AOrderIndex2] := n;
  inherited SwapOrder(AOrderIndex1, AOrderIndex2);
end;

procedure TXPModel.MaterialProgress(Sender: TObject; Stage: TProgressStage;
  PercentDone: Byte; RedrawNow: Boolean; const R: TRect; const Msg: string);
begin
  Progress(Sender, psRunning, PercentDone, False, R, 'Loading textures...');
end;

procedure TXPModel.XMLProgress(Sender: TObject; Stage: TProgressStage;
  PercentDone: Byte; RedrawNow: Boolean; const R: TRect; const Msg: string);
begin
  Progress(Sender, psRunning, PercentDone div 2, False, R, 'Reading...');
end;

{ TXPNormalMap }

class operator TXPNormalMap.Add(AValue1, AValue2: TXPNormalMap): TXPNormalMap;
begin
  Result.I := AValue1.I + AValue2.I;
  Result.J := AValue1.J + AValue2.J;
  Result.K := AValue1.K + AValue2.K;
end;

constructor TXPNormalMap.Create(AI, AJ, AK: GLFloat);
begin
  I := AI;
  J := AJ;
  K := AK;
end;

class operator TXPNormalMap.Divide(AValue1: TXPNormalMap;
  AValue2: GLFloat): TXPNormalMap;
begin
  Result.I := AValue1.I / AValue2;
  Result.J := AValue1.J / AValue2;
  Result.K := AValue1.K / AValue2;
end;

{ TXPModelObjectList }

function TXPModelObjectList.AddObject(AModel: TXPModel): TXPModelObject;
begin
  Result := TXPModelObject.Create(AModel);
  Add(Result);
end;

function TXPModelObjectList.AddObject(AObject: TXPModelObject): TXPModelObject;
begin
  Result := TXPModelObject.CreateSub(AObject);
  Add(Result);
end;

procedure TXPModelObjectList.CalcNormals;
var
  n: Integer;
begin
  for n := 0 to Count - 1 do Objects[n].CalcNormals;
end;

procedure TXPModelObjectList.Clear;
var
  obj: TXPModelObject;
  n: Integer;
begin
  n := Count;
  while n > 0 do
  begin
    Dec(n);
    obj := TXPModelObject(Items[n]);
    Items[n] := nil;
    obj.Free;
  end;
  inherited Clear;
end;

constructor TXPModelObjectList.Create;
begin
  inherited Create;
  Clear;
end;

procedure TXPModelObjectList.DeleteFaceByMatID(AMatID: Integer;
  ADeleted: TXPBoolean);
var
  n: Integer;
begin
  for n := 0 to Count - 1 do Objects[n].DeleteFaceByMatID(AMatID, ADeleted);
end;

function TXPModelObjectList.GetObjectByName(AName: string): TXPModelObject;
var
  n: Integer;
begin
  if Trim(AName) = '' then
    Result := nil
  else
  begin
    n := IndexOfName(AName);
    if n < 0 then
      Result := nil
    else Result := GetObjects(n);
  end;
end;

function TXPModelObjectList.GetObjects(AIndex: Integer): TXPModelObject;
begin
  Result := TXPModelObject(Items[AIndex]);
end;

function TXPModelObjectList.IndexOfName(AName: string): Integer;
var
  n: Integer;
begin
  Result := -1;
  n := Count;
  while (n > 0) and (Result = -1) do
  begin
    Dec(n);
    if CompareText(GetObjects(n).FName, AName) = 0 then
    begin
      Result := n;
    end;
  end;
end;

procedure TXPModelObjectList.PaintMat(AMaterialID: Integer);
var
  n: Integer;
begin
  for n := 0 to Count - 1 do Objects[n].PaintMat(AMaterialID);
end;

procedure TXPModelObjectList.ReplaceFlip(AOrderIndex: Integer;
  AValue: TXPBoolean);
var
  n: Integer;
begin
  for n := 0 to Count - 1 do Objects[n].ReplaceFlip(AOrderIndex, AValue);
end;

procedure TXPModelObjectList.ReplaceMatID(ANewIndex: Integer);
var
  n: Integer;
begin
  for n := 0 to Count - 1 do Objects[n].ReplaceMatID(ANewIndex);
end;

procedure TXPModelObjectList.ReplaceMatID(AOldIndex, ANewIndex: Integer);
var
  n: Integer;
begin
  for n := 0 to Count - 1 do Objects[n].ReplaceMatID(AOldIndex, ANewIndex);
end;

procedure TXPModelObjectList.ReplaceOrder(AOrderIndex, AValue: Integer);
var
  n: Integer;
begin
  for n := 0 to Count - 1 do Objects[n].ReplaceOrder(AOrderIndex, AValue);
end;

procedure TXPModelObjectList.SetOrder(AIndex, AValue: Integer);
var
  n: Integer;
begin
  for n := 0 to Count - 1 do Objects[n].SetOrder(AIndex, AValue);
end;

procedure TXPModelObjectList.SetUsedPivot(AValue: Boolean);
var
  n: Integer;
begin
  for n := 0 to Count - 1 do Objects[n].SetUsedPivot(AValue);
end;

procedure TXPModelObjectList.SwapOrder(AOrderIndex1, AOrderIndex2: Integer);
var
  n: Integer;
begin
  for n := 0 to Count - 1 do Objects[n].SwapOrder(AOrderIndex1, AOrderIndex2);
end;

{ TXPFace }

function TXPFace.Add(APointMap: TXPFaceMap): Integer;
var
  p: PXPFaceMap;
begin
  Result := FPointCount;
  Inc(FPointCount);
  ReallocMem(FPointMaps, FPointCount * SizeOf(TXPFaceMap));
  p := FPointMaps;
  Inc(p, Result);
  p^ := APointMap;
end;

procedure TXPFace.Clear;
begin
  FreeMemNil(FPointMaps);
  FPointCount := 0;
  MatIndex := 0;
  Smoothed := False;
  Deleted := False;
end;

constructor TXPFace.Create;
begin
  FPointMaps := nil;
  Clear;
end;

destructor TXPFace.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TXPFace.GetPointMaps(AIndex: Integer): PXPFaceMap;
begin
  Result := FPointMaps^[AIndex];
end;

procedure TXPFace.SetPointMaps(AIndex: Integer; APMap: PXPFaceMap);
begin
  FPointMaps^[AIndex]^ := APMap^;
end;

{ TXPFaceMap }

procedure TXPFaceMap.Clear;
begin
  VertexID := 0;
  NormalID := 0;
  TextureID := 0;
end;

function TXPFaceMap.GetPointers(AIndex: Integer): PXPFacemap;
begin
  Result := @Self;
  Inc(Result, AIndex);
end;

{ TXPFaceRefList }

function TXPFaceRefList.GetFaces(AIndex: Integer): TXPFace;
begin
  Result := TXPFace(Items[AIndex]);
end;

{ TXPFaceList }

function TXPFaceList.AddFace: TXPFace;
begin
  Result := TXPFace.Create;
  Add(Result);
end;

procedure TXPFaceList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if Action = lnDeleted then TXPFace(Ptr).Free;
end;

{ TXPVertexRefList }

function TXPVertexRefList.Add(var AVertex: TXPVertex): Integer;
begin
  Result := NearIndexOf(AVertex);
  Insert(Result, @AVertex);
end;

function TXPVertexRefList.FindIndex(var AVertex: TXPVertex; AStartPos,
  AEndPos: Integer): Integer;
var
  done: Boolean;
begin
  if AStartPos >= AEndPos then
  begin
    Result := AEndPos;
    Exit;
  end;
  Result := AStartPos + ((AEndPos - AStartPos) shr 1);
  case AVertex.Compare(Vertices[Result]^) of
    -1: Result := FindIndex(AVertex, AStartPos, Result);
    1: Result := FindIndex(AVertex, Result + 1, AEndPos);
  end;
  done := False;
  while (Result > AStartPos) and not done do
  begin
    done := AVertex.Compare(Vertices[Result - 1]^) <> 0;
    if not done then Dec(Result);
  end;
end;

function TXPVertexRefList.GetVertices(AIndex: Integer): PXPVertex;
begin
  Result := PXPVertex(Items[AIndex]);
end;

function TXPVertexRefList.NearIndexOf(AVertex: TXPVertex): Integer;
begin
  Result := FindIndex(AVertex, 0, Count);
end;

{ TXPFaceMapRefList }

function TXPFaceMapRefList.Add(var AFaceMap: TXPFaceMap): Integer;
begin
  Result := NearIndexOf(AFaceMap);
  Insert(Result, @AFaceMap);
end;

constructor TXPFaceMapRefList.Create(AModel: TXPModel);
begin
  inherited Create;
  FModel := AModel;
end;

function TXPFaceMapRefList.FindIndex(var AFaceMap: TXPFaceMap; AStartPos,
  AEndPos: Integer): Integer;
var
  done: Boolean;
  pv1, pv2: PXPVertex;
begin
  if AStartPos >= AEndPos then
  begin
    Result := AEndPos;
    Exit;
  end;
  Result := AStartPos + ((AEndPos - AStartPos) shr 1);
  pv1 := @FModel.FVertices[AFaceMap.VertexID - 1];
  pv2 := @FModel.FVertices[FaceMaps[Result].VertexID - 1];
  case pv1.Compare(pv2^) of
    -1: Result := FindIndex(AFaceMap, AStartPos, Result);
    1: Result := FindIndex(AFaceMap, Result + 1, AEndPos);
  end;
  done := False;
  while (Result > AStartPos) and not done do
  begin
    pv2 := @FModel.FVertices[FaceMaps[Result - 1].VertexID - 1];
    done := pv1.Compare(pv2^) <> 0;
    if not done then Dec(Result);
  end;
end;

function TXPFaceMapRefList.GetFaceMaps(AIndex: Integer): PXPFaceMap;
begin
  Result := PXPFaceMap(Items[AIndex]);
end;

function TXPFaceMapRefList.NearIndexOf(AFaceMap: TXPFaceMap): Integer;
begin
  Result := FindIndex(AFaceMap, 0, Count);
end;

end.
