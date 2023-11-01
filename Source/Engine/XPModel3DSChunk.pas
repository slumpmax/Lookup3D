unit XPModel3DSChunk;

interface

uses
  XPRoutine, XPGL,
  Classes, SysUtils;
  
const
  OVersion = 3; //expects 3ds version 3 files

  //Chunk ID's
  MAIN3DS = $4D4D;
  VERS3DS = $0002;
  EDIT3DS = $3D3D;

  MAT_COLOR_F = $0010;
  MAT_COLOR_24 = $0011;
  LIN_COLOR_24 = $0012;
  LIN_COLOR_F = $0013;
  INT_PERCENTAGE = $0030;
  FLOAT_PERCENTAGE = $0031;
  MASTER_SCALE = $0100;

  EDIT_OBJECT = $4000;
  OBJ_HIDDEN = $4010;
  OBJ_VERINFO = $3D3E;
  OBJ_TRIMESH = $4100;
  OBJ_LAMP = $4600;
  OBJ_LAMP_SPOT = $4610; // The light is a spotloght.
  OBJ_LAMP_OFF = $4620;  // The light off.
  OBJ_LAMP_ATTENUATE = $4625;
  OBJ_LAMP_RAYSHADE = $4627;
  OBJ_LAMP_SHADOWED = $4630;
  OBJ_LAMP_LOCAL_SHADOW = $4640;
  OBJ_LAMP_LOCAL_SHADOW2 = $4641;
  OBJ_LAMP_SEE_CONE = $4650;
  OBJ_LAMP_SPOT_RECTANGULAR = $4651;
  OBJ_LAMP_SPOT_OVERSHOOT = $4652;
  OBJ_LAMP_SPOT_PROJECTOR = $4653;
  OBJ_LAMP_EXCLUDE = $4654;
  OBJ_LAMP_RANGE = $4655;
  OBJ_LAMP_ROLL = $4656;
  OBJ_LAMP_SPOT_ASPECT = $4657;
  OBJ_LAMP_RAY_BIAS = $4658;
  OBJ_LAMP_INNER_RANGE = $4659;
  OBJ_LAMP_OUTER_RANGE = $465A;
  OBJ_LAMP_MULTIPLIER = $465B;
  OBJ_LAMP_AMBIENT_LIGHT = $4680;
  OBJ_CAMERA = $4700;
  OBJ_CAM_RANGES = $4720;

  TRI_VERTEXS = $4110;
  TRI_FACES = $4120;
  TRI_MATGROUP = $4130;
  TRI_MAPPINGCOORDS = $4140;
  TRI_SMOOTH_GROUP = $4150;
  TRI_MATRIX = $4160; //TRI_LOCAL; //Gives a matrix for each mesh?
  TRI_VISIBLE = $4165; //Is mesh visible or not

  EDIT_MATERIAL = $AFFF;
  MAT_MATNAME = $A000;
  MAT_AMBIENT = $A010;
  MAT_DIFFUSE = $A020;
  MAT_SPECULAR = $A030;
  MAT_SHININESS = $A040;
  MAT_SHIN2PCT = $A041;
  MAT_TRANSPARENCY = $A050; // Transparency Material
  MAT_XPFALL = $A052;
  MAT_REFBLUR = $A053;
  MAT_SELF_ILLUM = $A080;
  MAT_TWO_SIDE = $A081; //thanks sos
  MAT_SELF_ILPCT = $A084;
  MAT_WIRE = $A085;
  MAT_WIRESIZE = $A087;
  MAT_XPFALLIN = $A08A;
  MAT_SHADING = $A100;
  MAT_TEXTURE_MAP = $A200;  //texmap
  MAT_SPECULAR_MAP = $A204;
  MAT_OPACITY_MAP = $A210;
  MAT_REFLECTION_MAP = $A220;
  MAT_BUMP_MAP = $A230;
  MAT_MAPFILE = $A300;
  MAT_SHINMAP = $A33C;
  MAT_MAP_TILING = $A351;
  MAT_MAP_TEXBLUR = $A353;
  MAT_VSCALE = $A354;
  MAT_USCALE = $A356;
  MAT_UOFF = $A358;
  MAT_VOFF = $A35A;
  MAT_TEXROT = $A35C;

  EDIT_KEYFRAME = $B000;
  KEYF_AMBIENT_NODE = $B001;
  KEYF_OBJ_NODE = $B002;
  KEYF_CAMERA_NODE = $B003;
  KEYF_TARGET_NODE = $B004;
  KEYF_LIGHT_NODE = $B005;
  KEYF_L_TARGET_NODE = $B006;
  KEYF_SPOTLIGHT_NODE = $B007;
  KEYF_SEG = $B008;
  KEYF_CURTIME = $B009;
  KEYF_HEADER = $B00A;
  KEYF_NODE_HEADER = $B010;
  KEYF_INSTANCE_NAME = $B011;
  KEYF_KEYSCALE = $B012;
  KEYF_PIVOT = $B013;
  KEYF_BOUNDBOX = $B014;
  KEYF_MORPH_SMOOTH = $B015;
  KEYF_POSITION_TRACK = $B020;
  KEYF_ROTATION_TRACK = $B021;
  KEYF_SCALE_TRACK = $B022;
  KEYF_CAMERA_FOV_TRACK = $B023;
  KEYF_CAMERA_ROLL_TRACK = $B024;
  KEYF_COLOR_TRACK = $B025;
  KEYF_MORPH_TRACK = $B026;
  KEYF_HOTSPOT_TRACK = $B027;
  KEYF_FALLOF_TRACK = $B028;
  KEYF_HIDE_TRACK = $B029;
  KEYF_NODE_ID = $B030;

  SOLID_BGND = $1200;
  USE_SOLID_BGND = $1201;
  AMBIENT_LIGHT = $2100;

  XDATA_RFU6 = $800A;
  ROOT_OBJECT = $FFFF;

type
  TChunkRec = packed record
    ChunkID: Word;
    ChunkLen: Cardinal;
  end;
  TXP3DSChunkList = class;
  TXP3DSChunk = class
  private
    FSubChunks: TXP3DSChunkList;
    FStream: TStream;
    FNextFilePos: Int64;
    procedure LoadData;
    procedure LoadSubChunks;
    function ReadWord: string;
    function ReadInteger: string;
    function ReadSingle: string;
    function ReadRGB24: string;
    function ReadRGBFloat: string;
    function ReadXYZFloat: string;
    function ReadString: string;
    function ReadDummy: string;
  public
    ChunkID: Word;
    ChunkLength: Cardinal;
    Name, Text: string;
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromStream(AStream: TStream);
    property SubChunks: TXP3DSChunkList read FSubChunks;
  end;
  TXP3DSChunkList = class(TList)
  private
    function GetChunks(AIndex: Integer): TXP3DSChunk; inline;
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    constructor Create;
    function AddChunk: TXP3DSChunk;
    procedure LoadFromStream(AStream: TStream);
    property Chunks[AIndex: Integer]: TXP3DSChunk read GetChunks; default;
  end;

function Get3DSChunkName(AChunkID: Cardinal): string;

implementation

function Get3DSChunkName(AChunkID: Cardinal): string;
begin
  case AChunkID of
    MAIN3DS: Result := 'MAIN3DS';
    VERS3DS: Result := 'VERS3DS';
    EDIT3DS: Result := 'EDIT3DS';
    EDIT_KEYFRAME: Result := 'EDIT_KEYFRAME';

    MAT_COLOR_F: Result := 'MAT_COLOR_F';
    MAT_COLOR_24: Result := 'MAT_COLOR_24';
    LIN_COLOR_24: Result := 'LIN_COLOR_24';
    LIN_COLOR_F: Result := 'LIN_COLOR_F';
    INT_PERCENTAGE: Result := 'INT_PERCENTAGE';
    FLOAT_PERCENTAGE: Result := 'FLOAT_PERCENTAGE';

    EDIT_OBJECT: Result := 'EDIT_OBJECT';
    MASTER_SCALE: Result := 'MASTER_SCALE';

    OBJ_HIDDEN: Result := 'OBJ_HIDDEN';
    OBJ_VERINFO: Result := 'OBJ_VERINFO';
    OBJ_TRIMESH: Result := 'OBJ_TRIMESH';
    OBJ_LAMP: Result := 'OBJ_LAMP';
    OBJ_LAMP_SPOT: Result := 'OBJ_LAMP_SPOT';
    OBJ_LAMP_OFF: Result := 'OBJ_LAMP_OFF';
    OBJ_LAMP_ATTENUATE: Result := 'OBJ_LAMP_ATTENUATE';
    OBJ_LAMP_RAYSHADE: Result := 'OBJ_LAMP_RAYSHADE';
    OBJ_LAMP_SHADOWED: Result := 'OBJ_LAMP_SHADOWED';
    OBJ_LAMP_LOCAL_SHADOW: Result := 'OBJ_LAMP_LOCAL_SHADOW';
    OBJ_LAMP_LOCAL_SHADOW2: Result := 'OBJ_LAMP_LOCAL_SHADOW2';
    OBJ_LAMP_SEE_CONE: Result := 'OBJ_LAMP_SEE_CONE';
    OBJ_LAMP_SPOT_RECTANGULAR: Result := 'OBJ_LAMP_SPOT_RECTANGULAR';
    OBJ_LAMP_SPOT_OVERSHOOT: Result := 'OBJ_LAMP_SPOT_OVERSHOOT';
    OBJ_LAMP_SPOT_PROJECTOR: Result := 'OBJ_LAMP_SPOT_PROJECTOR';
    OBJ_LAMP_EXCLUDE: Result := 'OBJ_LAMP_EXCLUDE';
    OBJ_LAMP_RANGE: Result := 'OBJ_LAMP_RANGE';
    OBJ_LAMP_ROLL: Result := 'OBJ_LAMP_ROLL';
    OBJ_LAMP_SPOT_ASPECT: Result := 'OBJ_LAMP_SPOT_ASPECT';
    OBJ_LAMP_RAY_BIAS: Result := 'OBJ_LAMP_RAY_BIAS';
    OBJ_LAMP_INNER_RANGE: Result := 'OBJ_LAMP_INNER_RANGE';
    OBJ_LAMP_OUTER_RANGE: Result := 'OBJ_LAMP_OUTER_RANGE';
    OBJ_LAMP_MULTIPLIER: Result := 'OBJ_LAMP_MULTIPLIER';
    OBJ_LAMP_AMBIENT_LIGHT: Result := 'OBJ_LAMP_AMBIENT_LIGHT';
    OBJ_CAMERA: Result := 'OBJ_CAMERA';
    OBJ_CAM_RANGES: Result := 'OBJECT_CAM_RANGES';

    TRI_VERTEXS: Result := 'TRI_VERTEXS';
    TRI_FACES: Result := 'TRI_FACES';
    TRI_MATGROUP: Result := 'TRI_MATGROUP';
    TRI_SMOOTH_GROUP: Result := 'TRI_SMOOTH_GROUP';
    TRI_MAPPINGCOORDS: Result := 'TRI_MAPPINGCOORDS';
    TRI_MATRIX: Result := 'TRI_MATRIX';
    TRI_VISIBLE: Result := 'TRI_VISIBLE';

    EDIT_MATERIAL: Result := 'EDIT_MATERIAL';
    MAT_MATNAME: Result := 'MAT_MATNAME';
    MAT_AMBIENT: Result := 'MAT_AMBIENT';
    MAT_DIFFUSE: Result := 'MAT_DIFFUSE';
    MAT_SPECULAR: Result := 'MAT_SPECULAR';
    MAT_SHININESS: Result := 'MAT_SHININESS';
    MAT_SHIN2PCT: Result := 'MAT_SHIN2PCT';
    MAT_TRANSPARENCY: Result := 'MAT_TRANSPARENCY';
    MAT_SELF_ILLUM: Result := 'MAT_SELF_ILLUM';
    MAT_WIRE: Result := 'MAT_WIRE';
    MAT_TEXTURE_MAP: Result := 'MAT_TEXTURE_MAP';
    MAT_SPECULAR_MAP: Result := 'MAT_SPECULAR_MAP';
    MAT_OPACITY_MAP: Result := 'MAT_OPACITY_MAP';
    MAT_REFLECTION_MAP: Result := 'MAT_REFLECTION_MAP';
    MAT_BUMP_MAP: Result := 'MAT_BUMP_MAP';
    MAT_MAPFILE: Result := 'MAT_MAPFILE';
    MAT_VSCALE: Result := 'MAT_VSCALE';
    MAT_USCALE: Result := 'MAT_USCALE';
    MAT_VOFF: Result := 'MAT_VOFF';
    MAT_UOFF: Result := 'MAT_UOFF';
    MAT_TEXROT: Result := 'MAT_TEXROT';
    MAT_TWO_SIDE: Result := 'MAT_TWO_SIDE';
    MAT_SHADING: Result := 'MAT_SHADING';
    MAT_XPFALL: Result := 'MAT_XPFALL';
    MAT_REFBLUR: Result := 'MAT_REFBLUR';
    MAT_SELF_ILPCT: Result := 'MAT_SELF_ILPCT';
    MAT_WIRESIZE: Result := 'MAT_WIRESIZE';
    MAT_XPFALLIN: Result := 'MAT_XPFALLIN';
    MAT_MAP_TILING: Result := 'MAT_MAP_TILING';
    MAT_MAP_TEXBLUR: Result := 'MAT_MAP_TEXBLUR';
    MAT_SHINMAP: Result := 'MAT_SHINMAP';

    KEYF_OBJ_NODE: Result := 'KEYF_OBJ_NODE';
    KEYF_INSTANCE_NAME: Result := 'KEYF_INSTANCE_NAME';
    KEYF_PIVOT: Result := 'KEYF_PIVOT';
    KEYF_NODE_HEADER: Result := 'KEYF_NODE_HEADER';
    KEYF_NODE_ID: Result := 'KEYF_NODE_ID';
    KEYF_COLOR_TRACK: Result := 'KEYF_COLOR_TRACK';
    KEYF_POSITION_TRACK: Result := 'KEYF_POSITION_TRACK';
    KEYF_ROTATION_TRACK: Result := 'KEYF_ROTATION_TRACK';
    KEYF_SCALE_TRACK: Result := 'KEYF_SCALE_TRACK';

    AMBIENT_LIGHT: Result := 'AMBIENT_LIGHT';
    SOLID_BGND: Result := 'SOLID_BGND';
    USE_SOLID_BGND: Result := 'USE_SOLID_BGND';
    KEYF_AMBIENT_NODE: Result := 'KEYF_AMBIENT_NODE';
    KEYF_CAMERA_NODE: Result := 'KEYF_CAMERA_NODE';
    KEYF_TARGET_NODE: Result := 'KEYF_TARGET_NODE';
    KEYF_LIGHT_NODE: Result := 'KEYF_LIGHT_NODE';
    KEYF_L_TARGET_NODE: Result := 'KEYF_L_TARGET_NODE';
    KEYF_SPOTLIGHT_NODE: Result := 'KEYF_SPOTLIGHT_NODE';
    KEYF_HEADER: Result := 'KEYF_HEADER';
    KEYF_SEG: Result := 'KEYF_SEG';
    KEYF_CURTIME: Result := 'KEYF_CURTIME';
    KEYF_BOUNDBOX: Result := 'KEYF_BOUNDBOX';
    KEYF_MORPH_SMOOTH: Result := 'KEYF_MORPH_SMOOTH';

    XDATA_RFU6: Result := 'XDATA_RFU6';
  else
    Result := Format('ID($%4.4X)', [AChunkID]);
  end;
end;

constructor TXP3DSChunk.Create;
begin
  FSubChunks := TXP3DSChunkList.Create;
  ChunkID := 0;
  ChunkLength := 0;
  FNextFilePos := 0;
  FStream := nil;
  Name := '';
  Text := '';
end;

destructor TXP3DSChunk.Destroy;
begin
  FSubChunks.Free;
  inherited Destroy;
end;

procedure TXP3DSChunk.LoadData;
var
  n, ncount: Integer;
  s: string;
begin
  Text := '';
  case ChunkID of
    MAIN3DS: LoadSubChunks;
    VERS3DS: Text := ReadInteger;
    EDIT3DS: LoadSubChunks;
    EDIT_KEYFRAME: LoadSubChunks;
    INT_PERCENTAGE: Text := ReadWord;
    FLOAT_PERCENTAGE: Text := ReadSingle;
    EDIT_OBJECT: //$4000;
    begin
      Text := ReadString;
      LoadSubChunks;
    end;
    MASTER_SCALE: Text := ReadSingle;
    OBJ_HIDDEN: //$4010;
    begin
      Text := ReadString;
      LoadSubChunks;
    end;
    OBJ_VERINFO: Text := ReadSingle;
    OBJ_TRIMESH: LoadSubChunks;
    OBJ_LAMP: Text := ReadDummy;
    OBJ_CAMERA: Text := ReadDummy;
    TRI_VERTEXS: //$4110;
    begin
      ncount := 0;
      FStream.Read(ncount, 2);
      Text := Format('%d vertices', [ncount]);
      for n := 0 to ncount - 1 do
      begin
        s := ReadXYZFloat;
        if (n < 30) or (n > ncount - 30) then
        begin
          Text := Text + #13#10 + s;
          if n = 29 then Text := Text + #13#10'...';
        end;
      end;
    end;
    TRI_FACES: //$4120;
    begin
      ncount := 0;
      FStream.Read(ncount, 2);
      Text := Format('%d faces', [ncount]);
      for n := 0 to ncount - 1 do
      begin
        s := 'ID:  ' + ReadWord;
        s := s + '  ' + ReadWord;
        s := s + '  ' + ReadWord;
        s := s + '  /  Diff: ' + ReadWord;
        if (n < 30) or (n > ncount - 30) then
        begin
          Text := Text + #13#10 + s;
          if n = 29 then Text := Text + #13#10'...';
        end;
      end;
      LoadSubChunks;
    end;
    TRI_MATGROUP: //$4130;
    begin
      Text := ReadString;
      ncount := 0;
      FStream.Read(ncount, 2);
      Text := Text + Format(#13#10'%d face texture maps', [ncount]);
      for n := 0 to ncount - 1 do
      begin
        s := ReadWord;
        if (n < 30) or (n > ncount - 30) then
        begin
          Text := Text + #13#10 + s;
          if n = 29 then Text := Text + #13#10'...';
        end;
      end;
    end;
    TRI_MAPPINGCOORDS: //$4140;
    begin
      ncount := 0;
      FStream.Read(ncount, 2);
      Text := Format('%d texture maps', [ncount]);
      for n := 0 to ncount - 1 do
      begin
        s := 'UV:  ' + ReadSingle;
        s := s + '  ' + ReadSingle;
        if (n < 30) or (n > ncount - 30) then
        begin
          Text := Text + #13#10 + s;
          if n = 29 then Text := Text + #13#10'...';
        end;
      end;
    end;
    TRI_MATRIX: //$4160; //TRI_LOCAL; //Gives a matrix for each mesh?
    begin
      for n := 0 to 11 do
      begin
        if n > 0 then
        begin
          if (n mod 3) = 0 then
            Text := Text + #13#10
          else Text := Text + '  ';
        end;
        Text := Text + ReadSingle;
      end;
    end;
    TRI_VISIBLE: //$4165; //Is mesh visible or not
    begin
      n := 0;
      FStream.Read(n, 1);
      if n <> 0 then
        Text := 'True'
      else Text := 'False';
    end;
    EDIT_MATERIAL: LoadSubChunks;
    MAT_MATNAME: Text := ReadString;
    MAT_AMBIENT: LoadSubChunks;
    MAT_DIFFUSE: LoadSubChunks;
    MAT_SPECULAR: LoadSubChunks;
    MAT_SHININESS: LoadSubChunks;
    MAT_SHIN2PCT: LoadSubChunks;
    MAT_SHADING: Text := ReadWord;
    MAT_TRANSPARENCY: LoadSubChunks;
    MAT_TEXTURE_MAP: LoadSubChunks;
    MAT_SPECULAR_MAP: LoadSubChunks;
    MAT_REFLECTION_MAP: LoadSubChunks;
    MAT_OPACITY_MAP: LoadSubChunks;
    MAT_BUMP_MAP: LoadSubChunks;
    MAT_MAPFILE: Text := ReadString;
    MAT_VSCALE: Text := ReadSingle;
    MAT_USCALE: Text := ReadSingle;
    MAT_VOFF: Text := ReadSingle;
    MAT_UOFF: Text := ReadSingle;
    MAT_TEXROT: Text := ReadSingle;
    MAT_COLOR_F: Text := ReadRGBFloat;
    MAT_COLOR_24: Text := ReadRGB24;
    LIN_COLOR_F: Text := ReadRGBFloat;
    LIN_COLOR_24: Text := ReadRGB24;
    MAT_TWO_SIDE: Text := ReadDummy;
    KEYF_LIGHT_NODE: LoadSubChunks;
    KEYF_OBJ_NODE: LoadSubChunks;
    KEYF_PIVOT: Text := ReadXYZFloat;
    KEYF_HEADER:
    begin
      Text := 'Revision: ' + ReadWord;
      Text := Text + #13#10'Header: ' + ReadString;
      Text := Text + #13#10'Length: ' + ReadWord;
    end;
    KEYF_NODE_ID: Text := ReadWord;
    KEYF_COLOR_TRACK:
    begin
      Text := 'Flags: ' + ReadWord;
      Text := Text + #13#10'Unknown: ' + ReadWord;
      Text := Text + #13#10'Unknown: ' + ReadWord;
      Text := Text + #13#10'Unknown: ' + ReadWord;
      Text := Text + #13#10'Unknown: ' + ReadWord;
      Text := Text + #13#10'Keys: ' + ReadWord;
      Text := Text + #13#10'Unknown: ' + ReadWord;
      Text := Text + #13#10'Color:';
    end;
    KEYF_POSITION_TRACK:
    begin
      Text := 'Flags: ' + ReadWord;
      Text := Text + #13#10'Unknown: ' + ReadWord;
      Text := Text + #13#10'Unknown: ' + ReadWord;
      Text := Text + #13#10'Unknown: ' + ReadWord;
      Text := Text + #13#10'Unknown: ' + ReadWord;
      n := 0;
      FStream.Read(n, 2);
      Text := Text + #13#10'Keys: ' + IntToStr(n);
      Text := Text + #13#10'Unknown: ' + ReadWord;
      while n > 0 do
      begin
        Text := Text + #13#10;
        Text := Text + #13#10'FrameNum: ' + ReadWord;
        Text := Text + #13#10'Unknown: ' + ReadInteger;
        Text := Text + #13#10 + ReadXYZFloat;
        Dec(n);
      end;
    end;
    KEYF_ROTATION_TRACK:
    begin
      Text := 'Flags: ' + ReadWord;
      Text := Text + #13#10'Unknown: ' + ReadWord;
      Text := Text + #13#10'Unknown: ' + ReadWord;
      Text := Text + #13#10'Unknown: ' + ReadWord;
      Text := Text + #13#10'Unknown: ' + ReadWord;
      n := 0;
      FStream.Read(n, 2);
      Text := Text + #13#10'Keys: ' + IntToStr(n);
      Text := Text + #13#10'Unknown: ' + ReadWord;
      while n > 0 do
      begin
        Text := Text + #13#10;
        Text := Text + #13#10'FrameNum: ' + ReadWord;
        Text := Text + #13#10'Unknown: ' + ReadInteger;
        Text := Text + #13#10'Radian: ' + ReadSingle;
        Text := Text + #13#10 + ReadXYZFloat;
        Dec(n);
      end;
    end;
    KEYF_SCALE_TRACK:
    begin
      Text := 'Flags: ' + ReadWord;
      Text := Text + #13#10'Unknown: ' + ReadWord;
      Text := Text + #13#10'Unknown: ' + ReadWord;
      Text := Text + #13#10'Unknown: ' + ReadWord;
      Text := Text + #13#10'Unknown: ' + ReadWord;
      n := 0;
      FStream.Read(n, 2);
      Text := Text + #13#10'Keys: ' + IntToStr(n);
      Text := Text + #13#10'Unknown: ' + ReadWord;
      while n > 0 do
      begin
        Text := Text + #13#10;
        Text := Text + #13#10'FrameNum: ' + ReadWord;
        Text := Text + #13#10'Unknown: ' + ReadInteger;
        Text := Text + #13#10 + ReadXYZFloat;
        Dec(n);
      end;
    end;
    KEYF_SEG:
    begin
      Text := 'Start: ' + ReadWord;
      Text := Text + #13#10'End: ' + ReadWord;
    end;
    KEYF_CURTIME: Text := ReadWord;
    KEYF_MORPH_SMOOTH: Text := ReadSingle + ' degrees';
  end;
  if FStream.Position < FNextFilePos then
  begin
    if Text <> '' then Text := Text + #13#10#13#10;
    Text := Text + ReadDummy;
  end;
  FStream.Seek(FNextFilePos, soBeginning);
end;

procedure TXP3DSChunk.LoadFromStream(AStream: TStream);
begin
  FStream := AStream;
  FNextFilePos := FStream.Position;
  FStream.Read(ChunkID, 2);
  FStream.Read(ChunkLength, 4);
  Inc(FNextFilePos, ChunkLength);
  Name := Get3DSChunkName(ChunkID) + Format(' (6, %d)', [ChunkLength - SizeOf(TChunkRec)]);
  LoadData;
end;

procedure TXP3DSChunk.LoadSubChunks;
begin
  while FStream.Position < FNextFilePos do
  begin
    FSubChunks.AddChunk.LoadFromStream(FStream);
  end;
end;

function TXP3DSChunk.ReadDummy: string;
var
  p, q: PByte;
  n, nd: Integer;
  s: string;
begin
  n := FNextFilePos - FStream.Position;
  Result := Format('dummy data (%d bytes)'#13#10, [n]);
  if n > 1024 then n := 1024;
  s := '';
  GetMem(p, n);
  nd := 0;
  q := p;
  try
    FStream.Read(p^, n);
    while n > 0 do
    begin
      if nd > 0 then
      case nd mod 16 of
        0:
        begin
          Result := Result + ' ' + s + #13#10;
          s := '';
        end;
        8: Result := Result + '-';
      else
        Result := Result + ' ';
      end;
      Result := Result + Format('%2.2X', [p^]);
      if p^ < 32 then
        s := s + '.'
      else s := s + Chr(p^);
      Inc(p);
      Inc(nd);
      Dec(n);
    end;
    Result := Result + ' ' + s;
  finally
    FreeMem(q);
  end;
end;

function TXP3DSChunk.ReadInteger: string;
var
  n: Integer;
begin
  FStream.Read(n, 4);
  Result := Format('%d', [n]);
end;

function TXP3DSChunk.ReadRGB24: string;
var
  v: TColor24Entry;
begin
  FStream.Read(v, 3);
  Result := Format('RGB24:  %d  %d  %d', [v.R, v.G, v.B]);
end;

function TXP3DSChunk.ReadRGBFloat: string;
var
  v: TGLRGBFloat;
begin
  FStream.Read(v, 12);
  Result := Format('RGB:  %.6f  %.6f  %.6f', [v.R, v.G, v.B]);
end;

function TXP3DSChunk.ReadSingle: string;
var
  d: Single;
begin
  FStream.Read(d, 4);
  Result := Format('%.6f', [d]);
end;

function TXP3DSChunk.ReadString: string;
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

function TXP3DSChunk.ReadWord: string;
var
  w: Word;
begin
  FStream.Read(w, 2);
  Result := Format('%d', [w]);
end;

function TXP3DSChunk.ReadXYZFloat: string;
var
  v: TGLXYZFloat;
begin
  FStream.Read(v, 12);
  Result := Format('XYZ:  %.6f  %.6f  %.6f', [v.X, v.Y, v.Z]);
end;

{ TXP3DSChunkList }

function TXP3DSChunkList.AddChunk: TXP3DSChunk;
begin
  Result := TXP3DSChunk.Create;
  Add(Result);
end;

constructor TXP3DSChunkList.Create;
begin
  inherited Create;
end;

function TXP3DSChunkList.GetChunks(AIndex: Integer): TXP3DSChunk;
begin
  Result := TXP3DSChunk(Items[AIndex]);
end;

procedure TXP3DSChunkList.LoadFromStream(AStream: TStream);
begin
  Clear;
  while AStream.Position < AStream.Size do AddChunk.LoadFromStream(AStream);
end;

procedure TXP3DSChunkList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if Action = lnDeleted then TXP3DSChunk(Ptr).Free;
end;

end.
