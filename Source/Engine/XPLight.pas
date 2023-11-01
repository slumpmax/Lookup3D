unit XPLight;

interface

uses
  XPGL, dglOpenGL,
  Classes, SysUtils;

const
  GLLightHandles: array[0..7] of Integer = (
    GL_LIGHT0, GL_LIGHT1, GL_LIGHT2, GL_LIGHT3,
    GL_LIGHT4, GL_LIGHT5, GL_LIGHT6, GL_LIGHT7
  );

type
  TXPLight = class
  private
    FLightIndex, FLightHandle: Integer;
  public
    Ambient, Diffuse, Specular: TGLRGBAFloat;
    Position: TGLXYZWFloat;
    SpotDirection: TGLXYZFloat;
    constructor Create(ALightIndex: Integer);
    procedure SetParam(AParam: Integer; AValue: GLFloat);
    procedure SetParams(AParam: Integer; var AValues: GLFloat);
    procedure SetAmbient;
    procedure SetDiffuse;
    procedure SetSpecular;
    procedure SetPosition;
    procedure SetSpotDirection;
    property LightIndex: Integer read FLightIndex;
    property LightHandle: Integer read FLightHandle;
  end;

  TXPLights = class(TList)
  private
    function GetLights(AIndex: Integer): TXPLight;
  public
    MatShininess: GLFloat;
    MatAmbient, MatDiffuse, MatSpecular: TGLRGBAFloat;
    constructor Create(ALightCount: Integer = 0);
    function AddLight(AIndex: Integer): TXPLight;
    procedure SetParam(AIndex, AParam: Integer; AValue: GLFloat);
    procedure SetParams(AIndex, AParam: Integer; var AValues: GLFloat);
    procedure SetMatShininess;
    procedure SetMatAmbient;
    procedure SetMatDiffuse;
    procedure SetMatSpecular;
    property Lights[AIndex: Integer]: TXPLight read GetLights; default;
  end;

implementation

{ TXPLight }

constructor TXPLight.Create(ALightIndex: Integer);
begin
  FLightIndex := ALightIndex;
  FLightHandle := GLLightHandles[ALightIndex];
  Ambient.Clear;
  Diffuse.Clear;
  Specular.Clear;
  Position.Clear;
  SpotDirection.Clear;
  glEnable(GL_LIGHTING);
  glEnable(FLightHandle);
end;

procedure TXPLight.SetAmbient;
begin
  glLightfv(FLightHandle, GL_AMBIENT, @Ambient.R);
end;

procedure TXPLight.SetDiffuse;
begin
  glLightfv(FLightHandle, GL_DIFFUSE, @Diffuse);
end;

procedure TXPLight.SetParam(AParam: Integer; AValue: GLFloat);
begin
  glLightf(FLightHandle, AParam, AValue);
end;

procedure TXPLight.SetParams(AParam: Integer; var AValues: GLFloat);
begin
  glLightfv(FLightHandle, AParam, @AValues);
end;

procedure TXPLight.SetPosition;
begin
  glLightfv(FLightHandle, GL_POSITION, @Position);
end;

procedure TXPLight.SetSpecular;
begin
  glLightfv(FLightHandle, GL_SPECULAR, @Specular);
end;

procedure TXPLight.SetSpotDirection;
begin
  glLightfv(FLightHandle, GL_SPOT_DIRECTION, @SpotDirection);
end;

{ TXPLights }

function TXPLights.AddLight(AIndex: Integer): TXPLight;
begin
  Result := TXPLight.Create(AIndex);
  Add(Result);
end;

constructor TXPLights.Create(ALightCount: Integer);
var
  n: Integer;
begin
  inherited Create;
  MatShininess := 0.0;
  MatAmbient.Clear;
  MatDiffuse.Clear;
  MatSpecular.Clear;
  if ALightCount > 0 then
  begin
    if ALightCount > 8 then ALightCount := 8;
    for n := 0 to ALightCount - 1 do AddLight(n);
  end;
end;

function TXPLights.GetLights(AIndex: Integer): TXPLight;
  function ReturnAddr: Pointer;
  asm
      mov   eax,[ebp + 4]
  end;
begin
  if (AIndex >= 0) and (AIndex < Count) then
    Result := TXPLight(Items[AIndex])
  else raise Exception.CreateFmt('Light index out of bound. (%d)', [AIndex]) at ReturnAddr;
end;

procedure TXPLights.SetMatAmbient;
begin
  glMaterialfv(GL_FRONT, GL_AMBIENT, @MatAmbient);
end;

procedure TXPLights.SetMatDiffuse;
begin
  glMaterialfv(GL_FRONT, GL_DIFFUSE, @MatDiffuse);
end;

procedure TXPLights.SetMatShininess;
begin
  glMaterialf(GL_FRONT, GL_SHININESS, MatShininess);
end;

procedure TXPLights.SetMatSpecular;
begin
  glMaterialfv(GL_FRONT, GL_SPECULAR, @MatSpecular);
end;

procedure TXPLights.SetParam(AIndex, AParam: Integer; AValue: GLFloat);
begin
  Lights[AIndex].SetParam(AParam, AValue);
end;

procedure TXPLights.SetParams(AIndex, AParam: Integer; var AValues: GLFloat);
begin
  Lights[AIndex].SetParam(AParam, AValues);
end;

end.
