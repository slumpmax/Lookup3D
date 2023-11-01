unit Form_Main;

interface

uses
  XPGL, XPTexture, XPWindow, XPList, XPModel, XPRoutine, XPLight,
  XPMaterial,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, AppEvnts, dglOpenGL, Jpeg, ExtCtrls, StdCtrls, Math, ComCtrls,
  Buttons, Grids, ActnList, ToolWin, System.Actions;

type
  TFormMain = class(TForm)
    Panel1: TPanel;
    StatusBar1: TStatusBar;
    PageControl1: TPageControl;
    ButtonOBJ: TSpeedButton;
    ButtonExport: TSpeedButton;
    GridFile: TStringGrid;
    PanelProgress: TPanel;
    ProgressBar1: TProgressBar;
    TimerProgress: TTimer;
    PaintBoxGL: TPaintBox;
    ApplicationEvents1: TApplicationEvents;
    Panel2: TPanel;
    Label1: TLabel;
    LabelTexture: TLabel;
    ActionList1: TActionList;
    Label2: TLabel;
    LabelMatName: TLabel;
    ActionDeleteFace: TAction;
    ButtonDAE: TSpeedButton;
    ButtonRAW: TSpeedButton;
    ListBoxTexture: TListBox;
    TreeObject: TTreeView;
    TreeMaterial: TTreeView;
    TabSheet1: TTabSheet;
    ListBoxOrder: TListBox;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    Button3DS: TSpeedButton;
    ActionSmoothNormals: TAction;
    ActionCreateSmooths: TAction;
    ActionCreateFlats: TAction;
    ActionCreateAverages: TAction;
    ActionReverseOrder: TAction;
    ActionMoveToCenter: TAction;
    ActionViewFront: TAction;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    ActionViewLeft: TAction;
    ActionViewTop: TAction;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ActionTogglePivot: TAction;
    ButtonDeleteNormals: TToolButton;
    ActionDeleteNormals: TAction;
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
    ActionViewBack: TAction;
    ActionViewRight: TAction;
    ActionViewBottom: TAction;
    ToolButton15: TToolButton;
    ToolButton16: TToolButton;
    ToolButton17: TToolButton;
    ToolButton18: TToolButton;
    ActionToggleAlpha: TAction;
    ToolButton19: TToolButton;
    ActionStructure: TAction;
    ToolButton20: TToolButton;
    ActionMatrix: TAction;
    ToolButton21: TToolButton;
    ActionTogglePosition: TAction;
    ActionReverseFace: TAction;
    ToolButton22: TToolButton;
    EditFolder: TEdit;
    ButtonBrowse: TButton;
    ActionToggleScale: TAction;
    ToolButton23: TToolButton;
    ButtonAll: TSpeedButton;
    ActionFlipX: TAction;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure Panel1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Panel1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Panel1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ButtonOBJClick(Sender: TObject);
    procedure ButtonExportClick(Sender: TObject);
    procedure TimerProgressTimer(Sender: TObject);
    procedure ApplicationEvents1Idle(Sender: TObject; var Done: Boolean);
    procedure GridFileClick(Sender: TObject);
    procedure ActionDeleteFaceExecute(Sender: TObject);
    procedure ButtonDAEClick(Sender: TObject);
    procedure ButtonRAWClick(Sender: TObject);
    procedure ListBoxTextureClick(Sender: TObject);
    procedure TreeObjectClick(Sender: TObject);
    procedure ListBoxOrderClick(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure Button3DSClick(Sender: TObject);
    procedure ActionSmoothNormalsExecute(Sender: TObject);
    procedure ActionCreateSmoothsExecute(Sender: TObject);
    procedure ActionCreateFlatsExecute(Sender: TObject);
    procedure ActionCreateAveragesExecute(Sender: TObject);
    procedure ActionReverseOrderExecute(Sender: TObject);
    procedure ActionMoveToCenterExecute(Sender: TObject);
    procedure ActionViewFrontExecute(Sender: TObject);
    procedure ActionViewLeftExecute(Sender: TObject);
    procedure ActionViewTopExecute(Sender: TObject);
    procedure ActionTogglePivotExecute(Sender: TObject);
    procedure GridFileMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure GridFileMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure ActionDeleteNormalsExecute(Sender: TObject);
    procedure ActionViewBackExecute(Sender: TObject);
    procedure ActionViewRightExecute(Sender: TObject);
    procedure ActionViewBottomExecute(Sender: TObject);
    procedure ActionToggleAlphaExecute(Sender: TObject);
    procedure ActionStructureExecute(Sender: TObject);
    procedure ActionMatrixExecute(Sender: TObject);
    procedure ActionTogglePositionExecute(Sender: TObject);
    procedure ActionReverseFaceExecute(Sender: TObject);
    procedure ButtonBrowseClick(Sender: TObject);
    procedure ActionToggleScaleExecute(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure ButtonAllClick(Sender: TObject);
    procedure ActionFlipXExecute(Sender: TObject);
  private
    FGL: TXPGL;
    FModel: TXPModel;
    FLights: TXPLights;
    FScale, FXOffset, FYOffset, FZOffset, FRotX, FRotY, FRotZ: GLFloat;
    FDragX, FDragY, FDisplayIndex: Integer;
    FDisplayObject: TObject;
    FDragButton: TMouseButton;
    FIsDrag, FLoading, FUpdating: Boolean;
    FDisplay: TControl;
    FFolder: string;
    FTabs: array of TTabSheet;
    procedure Load3D(AFileName: string);
    procedure LoadFileList(AExtension: string = '*');
    procedure UpdateTreeObject;
    procedure UpdateTreeMaterial;
    procedure UpdateTextureList(APath: string);
    procedure UpdateStatus;
    procedure UpdateInfo;
    procedure ResetView;
    procedure SetOrderIndex;
    procedure SetFlipCheckBox;
    procedure Zoom(AFactor: GLFloat);
    procedure SetScrollPos(AScrollBar: TScrollBar;
      ALightIndex, AGroupIndex, AScrollIndex: Integer);
    procedure CreateLightTab(ALightIndex: Integer);
    function VScale(AValue: GLFloat): GLFloat; inline;
    procedure SetColorParam(var AColor: TGLRGBAFloat; AIndex, AValue: Integer;
      AScrollBar: TScrollBar);
    procedure LightBarScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure ModelProgress(Sender: TObject; Stage: TProgressStage; PercentDone: Byte;
      RedrawNow: Boolean; const R: TRect; const Msg: string);
    { Private declarations }
  public
    procedure UpdateScreen;
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

uses
  Form_Structure, Form_XML;

procedure TFormMain.ActionCreateAveragesExecute(Sender: TObject);
begin
  FModel.CalcNormals;
  FModel.SmoothNormals(True);
  UpdateScreen;
end;

procedure TFormMain.ActionCreateFlatsExecute(Sender: TObject);
begin
  FModel.CalcNormals;
  UpdateScreen;
end;

procedure TFormMain.ActionCreateSmoothsExecute(Sender: TObject);
begin
  FModel.CalcNormals;
  FModel.SmoothNormals(False);
  UpdateScreen;
end;

procedure TFormMain.ActionDeleteFaceExecute(Sender: TObject);
begin
  FUpdating := True;
  try
    if FDisplayObject is TXPMaterialList then
      FModel.DeleteFaceByMatID(FDisplayIndex, xpToggle)
    else if FDisplayObject is TXPModelObjectList then
      with TXPModelObjectList(FDisplayObject)[FDisplayIndex] do
        Deleted := not Deleted;
    UpdateScreen;
  finally
    FUpdating := False;
  end;
end;

procedure TFormMain.ActionDeleteNormalsExecute(Sender: TObject);
begin
  FModel.ClearNormals;
end;

procedure TFormMain.ActionFlipXExecute(Sender: TObject);
begin
  FUpdating := True;
  try
    if FDisplayObject is TXPModelObjectList then
    with TXPModelObjectList(FDisplayObject)[FDisplayIndex] do
      FlipX := not FlipX;
    UpdateScreen;
  finally
    FUpdating := False;
  end;
end;

procedure TFormMain.ActionMatrixExecute(Sender: TObject);
begin
  ActionMatrix.Checked := not ActionMatrix.Checked;
  FModel.UsedMatrix := ActionMatrix.Checked;
  UpdateScreen;
end;

procedure TFormMain.ActionMoveToCenterExecute(Sender: TObject);
begin
  FModel.MoveToCenter;
  UpdateInfo;
//  ResetView;
  UpdateScreen;
end;

procedure TFormMain.ActionReverseFaceExecute(Sender: TObject);
begin
  ActionReverseFace.Checked := not ActionReverseFace.Checked;
  FModel.ReversedFace := ActionReverseFace.Checked;
  UpdateScreen; 
end;

procedure TFormMain.ActionReverseOrderExecute(Sender: TObject);
begin
  ActionReverseOrder.Checked := not ActionReverseOrder.Checked;
  FModel.ReversedOrder := ActionReverseOrder.Checked;
  UpdateScreen;
end;

procedure TFormMain.ActionStructureExecute(Sender: TObject);
var
  aform: TForm;
  s, ext: string;
begin
  s := FFolder + GridFile.Cells[1, GridFile.Row];
  ext := UpperCase(ExtractFileExt(s));
  if ext = '.3DS' then
  begin
    aform := TFormStructure.Create(nil);
    try
      TFormStructure(aform).LoadStructure(s);
      aform.ShowModal;
    finally
      aform.Free;
    end;
  end
  else if ext = '.DAE' then
  begin
    aform := TFormXML.Create(nil);
    try
      TFormXML(aform).LoadXML(s);
      aform.ShowModal;
    finally
      aform.Free;
    end;
  end;
end;

procedure TFormMain.ActionSmoothNormalsExecute(Sender: TObject);
begin
  FModel.SmoothNormals;
  UpdateScreen;
end;

procedure TFormMain.ActionToggleAlphaExecute(Sender: TObject);
begin
  ActionToggleAlpha.Checked := not ActionToggleAlpha.Checked;
  FModel.UsedAlpha := ActionToggleAlpha.Checked;
  UpdateScreen;
end;

procedure TFormMain.ActionTogglePivotExecute(Sender: TObject);
begin
  ActionTogglePivot.Checked := not ActionTogglePivot.Checked;
  FModel.UsedPivot := ActionTogglePivot.Checked;
  UpdateScreen;
end;

procedure TFormMain.ActionTogglePositionExecute(Sender: TObject);
begin
  ActionTogglePosition.Checked := not ActionTogglePosition.Checked;
  FModel.UsedPosition := ActionTogglePosition.Checked;
  UpdateScreen;
end;

procedure TFormMain.ActionToggleScaleExecute(Sender: TObject);
begin
  ActionToggleScale.Checked := not ActionToggleScale.Checked;
  FModel.UsedScale := ActionToggleScale.Checked;
  UpdateScreen;
end;

procedure TFormMain.ActionViewBackExecute(Sender: TObject);
begin
  ResetView;
  FXOffset := -FScale * FModel.Center.X;
  FRotY := 180;
  UpdateScreen;
end;

procedure TFormMain.ActionViewBottomExecute(Sender: TObject);
begin
  ResetView;
  FRotX := -90.0;
//  FYOffset := FScale * FModel.CenterY;
  FYOffset := 0;
  UpdateScreen;
end;

procedure TFormMain.ActionViewFrontExecute(Sender: TObject);
begin
  ResetView;
  UpdateScreen;
end;

procedure TFormMain.ActionViewLeftExecute(Sender: TObject);
begin
  ResetView;
  FRotY := -90.0;
//  FXOffset := -FScale * FModel.CenterY;
  FXOffset := 0;
  UpdateScreen;
end;

procedure TFormMain.ActionViewRightExecute(Sender: TObject);
begin
  ResetView;
  FRotY := 90.0;
//  FXOffset := FScale * FModel.CenterY;
  FXOffset := 0;
  UpdateScreen;
end;

procedure TFormMain.ActionViewTopExecute(Sender: TObject);
begin
  ResetView;
  FRotX := 90.0;
//  FYOffset := -FScale * FModel.CenterY;
  FYOffset := 0;
  UpdateScreen;
end;

procedure TFormMain.ApplicationEvents1Idle(Sender: TObject; var Done: Boolean);
begin
  UpdateScreen;
end;

procedure TFormMain.ButtonBrowseClick(Sender: TObject);
var
  s: string;
begin
  s := FFolder;
  if BrowseForFolder(s, 'Select 3D files folder') then
  begin
    FFolder := IncludeTrailingPathDelimiter(s);
    EditFolder.Text := FFolder;
    ButtonAll.Click;
  end;
end;

procedure TFormMain.Button3DSClick(Sender: TObject);
begin
  Button3DS.Down := True;
  LoadFileList('3ds');
end;

procedure TFormMain.ButtonAllClick(Sender: TObject);
begin
  ButtonAll.Down := True;
  LoadFileList('*');
end;

procedure TFormMain.ButtonDAEClick(Sender: TObject);
begin
  ButtonDAE.Down := True;
  LoadFileList('dae');
end;

procedure TFormMain.ButtonExportClick(Sender: TObject);
var
  s, t: string;
  n: Integer;
begin
  t := ChangeFileExt(GridFile.Cells[1, GridFile.Row], '_Export.obj');
  GridFile.InsertRow(GridFile.Row + 1, [UpperCase(ExtractFileExt(t)), t]);
  t := FFolder + t;
  s := t;
  n := 0;
  while FileExists(s) do
  begin
    s := ChangeFileExt(t, IntToStr(n) + '.obj');
    Inc(n);
  end;
  FModel.SaveToFile(s);
end;

procedure TFormMain.ButtonOBJClick(Sender: TObject);
begin
  ButtonOBJ.Down := True;
  LoadFileList('obj');
end;

procedure TFormMain.ButtonRAWClick(Sender: TObject);
begin
  ButtonRAW.Down := True;
  LoadFileList('raw');
end;

procedure TFormMain.CheckBox1Click(Sender: TObject);
begin
  if FUpdating then Exit;
  with TCheckBox(Sender) do
  begin
    FModel.SetFlip(Tag, Checked);
    UpdateScreen;
  end;
end;

procedure TFormMain.CreateLightTab(ALightIndex: Integer);
const
  TabNameG: array[0..3] of string = ('Shininess', 'Ambient', 'Diffuse', 'Specular');
  TabNameL: array[0..2] of string = ('Ambient', 'Diffuse', 'Specular');
  ScrollCountG: array[0..3] of Integer = (1, 4, 4, 4);
  ScrollCountL: array[0..2] of Integer = (4, 4, 4);
var
  ScrollCounts: PIntegerArray;
  TabNames: PStringArray;
  tab: TTabSheet;
  ng, ns, py, qy, maxg: Integer;
  grp: TGroupBox;
  scb: TScrollBar;
  stitle, slid, sgid: string;
begin
  if ALightIndex < 0 then
  begin
    TabNames := @TabNameG;
    ScrollCounts := @ScrollCountG[0];
    maxg := Length(TabNameG);
    slid := 'LightG';
    stitle := 'Material';
  end
  else
  begin
    TabNames := @TabNameL;
    ScrollCounts := @ScrollCountL;
    maxg := Length(TabNameL);
    slid := 'Light' + IntToStr(ALightIndex);
    stitle := slid;
  end;
  tab := TTabSheet(PageControl1.FindComponent('Tab' + slid));
  if tab <> nil then
  begin
    PageControl1.ActivePage := tab;
    Exit;
  end;

  tab := TTabSheet.Create(PageControl1);
  tab.Name := 'Tab' + slid;
  tab.PageControl := PageControl1;
  tab.Caption := stitle;
  py := 0;
  for ng := 0 to maxg - 1 do
  begin
    sgid := 'Group' + IntToStr(ng);
    grp := TGroupBox.Create(tab);
    grp.Name := slid + sgid;
    grp.Parent := tab;
    grp.Width := tab.ClientWidth - 8;
    grp.Left := 4;
    grp.Top := py;
    grp.Caption := TabNames[ng];
    qy := 14;
    for ns := 0 to ScrollCounts[ng] - 1 do
    begin
      scb := TScrollBar.Create(grp);
      scb.Name := slid + sgid + 'Scroll' + IntToStr(ns);
      scb.Parent := grp;
      scb.Anchors := [akLeft, akTop, akRight];
      scb.Width := grp.ClientWidth - 8;
      scb.Height := 12;
      scb.Left := 4;
      scb.Top := qy;
      Inc(qy, scb.Height + 4);
      scb.Max := 100;
      SetScrollPos(scb, ALightIndex, ng, ns);
      scb.OnScroll := LightBarScroll;
    end;
    grp.Height := qy;
    Inc(py, grp.Height);
  end;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  FUpdating := True;
  TimerProgress.Enabled := False;
  PanelProgress.Visible := False;
  ListBoxOrder.ItemIndex := 0;
  FDisplay := Panel1;
  FScale := 1.0;
  FXOffset := 0.0;
  FYOffset := -9;
  FZOffset := -25;
  FRotX := 0.0;
  FRotY := 0.0;
  FRotZ := 0.0;
  EditFolder.Text := ExtractFilePath(Application.ExeName) + 'Models';
  FFolder := IncludeTrailingPathDelimiter(EditFolder.Text);
  FDisplayObject := FModel;
  FDisplayIndex := 0;
  LabelMatName.Caption := '';
  LabelTexture.Caption := '';
  FIsDrag := False;
  FLoading := False;
  FUpdating := False;
  FGL := TXPGL.Create(Panel1);
  FModel := TXPModel.Create(FGL);
  FModel.OnProgress := ModelProgress;
  FModel.Color := GLRGBAFloat(0.0, 0.0, 0.0, 1.0);
  FLights := TXPLights.Create(2);

  glEnable(GL_LINE_SMOOTH);
  glEnable(GL_POLYGON_SMOOTH);
  glEnable(GL_MULTISAMPLE);
  glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_FASTEST);

  glClearColor(0.0, 0.3, 0.1, 1.0);
  glShadeModel(GL_SMOOTH);
  glColorMaterial(GL_FRONT, GL_AMBIENT_AND_DIFFUSE);
  glEnable(GL_COLOR_MATERIAL);
  glEnable(GL_DEPTH_TEST);
  glDisable(GL_BLEND);
  glEnable(GL_NORMALIZE);


  with FLights do
  begin
    MatShininess := 50.0;
    MatAmbient.Assign(1.0, 1.0, 1.0, 1.0);
    MatDiffuse.Assign(1.0, 1.0, 1.0, 1.0);
    MatSpecular.Assign(1.0, 1.0, 1.0, 1.0);
    SetMatShininess;
    SetMatAmbient;
    SetMatDiffuse;
    SetMatSpecular;
  end;

  with FLights[0] do
  begin
    Ambient.Assign(0.0, 0.0, 0.0, 0.0);
    Diffuse.Assign(1.0, 1.0, 1.0, 1.0);
    Specular.Assign(0.0, 0.0, 0.0, 1.0);
    Position.Assign(0.0, 0.0, 1.0);
    SetAmbient;
    SetDiffuse;
    SetSpecular;
    SetPosition;
  end;

  with FLights[1] do
  begin
    Ambient.Assign(0.0, 0.0, 0.0, 1.0);
    Diffuse.Assign(1.0, 1.0, 1.0, 1.0);
    Specular.Assign(0.0, 0.0, 0.0, 1.0);
    Position.Assign(-1.0, 1.0, 1.0);
    SpotDirection.Assign(1.0, -1.0, -1.0);
    SetAmbient;
    SetDiffuse;
    SetSpecular;
    SetPosition;
    SetSpotDirection;
    SetParam(GL_CONSTANT_ATTENUATION, 1.5);
    SetParam(GL_LINEAR_ATTENUATION, 0.5);
    SetParam(GL_QUADRATIC_ATTENUATION, 0.2);
    SetParam(GL_SPOT_CUTOFF, 45.0);
    SetParam(GL_SPOT_EXPONENT, 2.0);
  end;

  CreateLightTab(-1);
  CreateLightTab(0);
  CreateLightTab(1);
  ListBoxTexture.Clear;
  ButtonAll.Click;
  FUpdating := False;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  SetLength(FTabs, 0);
  FModel.Free;
  FGL.Free;
end;

procedure TFormMain.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  d: GLFLoat;
begin
  d := Sqrt(FScale) * Sign(WheelDelta);
  if ssCtrl in Shift then
    if ssShift in Shift then
      Zoom(d * 0.01)
    else Zoom(d * 0.001)
  else Zoom(d * 0.0002);
  Handled := True;
end;

procedure TFormMain.FormResize(Sender: TObject);
begin
  FGL.Activate;
  FGL.SetBounds(FDisplay.ClientRect);
  UpdateScreen;
end;

procedure TFormMain.LightBarScroll(Sender: TObject; ScrollCode: TScrollCode;
  var ScrollPos: Integer);
var
  scb: TScrollBar;
  nl, ng, ns: Integer;
  lgt: TXPLight;
  s: string;
begin
  scb := TScrollBar(Sender);
  if Copy(scb.Name, 1, 5) + Copy(scb.Name, 7, 5) + Copy(scb.Name, 13, 6)
    <> 'LightGroupScroll' then Exit;
  s := Copy(scb.Name, 6, 1);
  if s = 'G' then
  begin
    nl := -1;
    lgt := nil;
  end
  else
  begin
    nl := StrToIntDef(Copy(scb.Name, 6, 1), -1);
    lgt := FLights[nl];
  end;
  ng := StrToIntDef(Copy(scb.Name, 12, 1), -1);
  ns := StrToIntDef(Copy(scb.Name, 19, 1), -1);
  case nl of
    -1:
    case ng of
      0:
      begin
        FLights.MatShininess := ScrollPos;
        FLights.SetMatShininess;
        UpdateScreen;
      end;
      1:
      begin
        SetColorParam(FLights.MatAmbient, ns, ScrollPos, scb);
        FLights.SetMatAmbient;
        UpdateScreen;
      end;
      2:
      begin
        SetColorParam(FLights.MatDiffuse, ns, ScrollPos, scb);
        FLights.SetMatDiffuse;
        UpdateScreen;
      end;
      3:
      begin
        SetColorParam(FLights.MatSpecular, ns, ScrollPos, scb);
        FLights.SetMatSpecular;
        UpdateScreen;
      end;
    end;
    0..7:
    case ng of
      0:
      begin
        SetColorParam(lgt.Ambient, ns, ScrollPos, scb);
        lgt.SetAmbient;
        UpdateScreen;
      end;
      1:
      begin
        SetColorParam(lgt.Diffuse, ns, ScrollPos, scb);
        lgt.SetDiffuse;
        UpdateScreen;
      end;
      2:
      begin
        SetColorParam(lgt.Specular, ns, ScrollPos, scb);
        lgt.SetSpecular;
        UpdateScreen;
      end;
    end;
  end;
end;

procedure TFormMain.ListBoxOrderClick(Sender: TObject);
begin
  if FUpdating then Exit;
  case ListBoxOrder.ItemIndex of
    1:
    begin
      FModel.XOrder := 0;
      FModel.YOrder := 2;
      FModel.ZOrder := 1;
    end;
    2:
    begin
      FModel.XOrder := 1;
      FModel.YOrder := 0;
      FModel.ZOrder := 2;
    end;
    3:
    begin
      FModel.XOrder := 1;
      FModel.YOrder := 2;
      FModel.ZOrder := 0;
    end;
    4:
    begin
      FModel.XOrder := 2;
      FModel.YOrder := 0;
      FModel.ZOrder := 1;
    end;
    5:
    begin
      FModel.XOrder := 2;
      FModel.YOrder := 1;
      FModel.ZOrder := 0;
    end;
  else
    FModel.XOrder := 0;
    FModel.YOrder := 1;
    FModel.ZOrder := 2;
  end;
  UpdateScreen;
end;

procedure TFormMain.ListBoxTextureClick(Sender: TObject);
var
  mat: TXPMaterial;
  n: Integer;
  s, sd: string;
begin
  if FDisplayObject = nil then Exit;

  s := ListBoxTexture.Items[ListBoxTexture.ItemIndex];
  if Copy(s, 1, 1) = '*' then s := '';
  mat := nil;
  if FDisplayObject is TXPMaterialList then
  begin
    if FDisplayIndex < 1 then
    begin
      mat := TXPMaterial.Create(FGL);
      DateTimeToString(sd, 'yyyymmdd_hhnnss_zzz', Now);
      mat.Name := 'AutoMaterial_' + sd;
      FDisplayIndex := FModel.Materials.Add(mat) + 1;
      FModel.ReplaceMatID(0, FDisplayIndex);
      UpdateTreeMaterial;
    end
    else mat := FModel.Materials[FDisplayIndex - 1];
  end
  else if FDisplayObject is TXPModelObjectList then
  begin
    n := FModel.Materials.IndexOfMapName(s);
    if n < 0 then
    begin
      mat := TXPMaterial.Create(FGL);
      DateTimeToString(sd, 'yyyymmdd_hhnnss_zzz', Now);
      mat.Name := 'AutoMaterial_' + sd;
      n := FModel.Materials.Add(mat);
      UpdateTreeMaterial;
    end
    else mat := FModel.Materials[n];
    TXPModelObjectList(FDisplayObject)[FDisplayIndex].ReplaceMatID(n + 1);
  end;
  mat.Diffuse.MapFile := s;
  if s <> '' then
  begin
    s := FFolder + ExtractFilePath(GridFile.Cells[1, GridFile.Row]) + s;
    mat.Texture.LoadFromFile(s);
  end
  else mat.Texture.Assign(nil);
  UpdateStatus;
  UpdateScreen;
end;

procedure TFormMain.Load3D(AFileName: string);
begin
  if not FileExists(AFileName) then Exit;
  FUpdating := True;
  try
    FGL.Activate;
    FModel.LoadFromFile(AFileName);
    FScale := 1.0;
    FXOffset := 0.0;
    FYOffset := -9;
    FZOffset := -25;
    FRotX := 0.0;
    FRotY := 0.0;
    FRotZ := 0.0;
    FDisplayObject := nil;
    FDisplayIndex := 0;
    LabelMatName.Caption := '';
    LabelTexture.Caption := '';
    ListBoxOrder.ItemIndex := 0;
    CheckBox1.Checked := False;
    CheckBox2.Checked := False;
    CheckBox3.Checked := False;
    CheckBox4.Checked := False;
    ActionToggleAlpha.Checked := FModel.UsedAlpha;
    ActionMatrix.Checked := FModel.UsedMatrix;
    ActionTogglePivot.Checked := FModel.UsedPivot;
    ActionTogglePosition.Checked := FModel.UsedPosition;
    ActionToggleScale.Checked := FModel.UsedScale;
    ActionReverseOrder.Checked := FModel.ReversedOrder;
    ActionReverseFace.Checked := FModel.ReversedFace;
    StatusBar1.Panels[1].Text := Format('Vertex(%d) Texture(%d) Normal(%d) Face(%d)',
      [FModel.VertexCount, FModel.TextureMapCount, FModel.NormalMapCount, FModel.Faces.Count]);
    with FModel do
    begin
  //    MoveToCenter;
      CalcCenter;
      if NormalMapCount = 0 then
      begin
  //      CalcNormals;
  //      SmoothNormals;
      end;
      ResetView;
      UpdateInfo;
      SetOrderIndex;
      SetFlipCheckBox;
    end;
    UpdateTreeObject;
    UpdateTreeMaterial;
    UpdateTextureList(ExtractFilePath(AFileName));
  finally
    FUpdating := False;
  end;
  UpdateScreen;
end;

procedure TFormMain.LoadFileList(AExtension: string);
var
  sl: TStringList;
  n: Integer;
  s: string;
begin
  with GridFile do
  begin
    sl := TStringList.Create;
    try
      RowCount := 2;
      Cells[0, 0] := 'Type';
      Cells[1, 0] := 'Filename';
      ColWidths[0] := 32;
      ColWidths[1] := 500;
      Rows[1].Text := '';

      if AExtension = '*' then
      begin
        GetFileList(sl, FFolder + '*.3ds', True);
        GetFileList(sl, FFolder + '*.obj', True);
        GetFileList(sl, FFolder + '*.dae', True);
        GetFileList(sl, FFolder + '*.raw', True);
      end
      else GetFileList(sl, FFolder + '*.' + AExtension, True);
      sl.Sort;
      GridFile.RowCount := Max(2, sl.Count + 1);
      for n := 0 to sl.Count - 1 do
      begin
        s := sl[n];
        Cells[0, n + 1] := UpperCase(Copy(ExtractFileExt(s), 2, Length(s) - 1));
        Cells[1, n + 1] := s;
      end;
    finally
      sl.Free;
    end;
  end;
end;

procedure TFormMain.ModelProgress(Sender: TObject; Stage: TProgressStage;
  PercentDone: Byte; RedrawNow: Boolean; const R: TRect; const Msg: string);
begin
  ProgressBar1.Position := PercentDone;
  PanelProgress.Caption := Msg;
  case Stage of
    psStarting: TimerProgress.Enabled := True;
    psRunning: ;
    psEnding:
    begin
      TimerProgress.Enabled := False;
      PanelProgress.Visible := False;
    end;
  end;
  Application.ProcessMessages;
end;

procedure TFormMain.Panel1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FDragX := X;
  FDragY := Y;
  FDragButton := Button;
  FIsDrag := True;
end;

procedure TFormMain.Panel1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if FIsDrag then
  begin
    case FDragButton of
      mbRight:
      begin
        if ssShift in Shift then
        begin
//          if FDisplayObject is TXPModelObjectList then
//          with TXPModelObjectList(FDisplayObject)[FDisplayIndex] do
//            Pivot.Z := Pivot.Z + (Y - FDragY) / 23.0
          FZOffset := FZOffset + (Y - FDragY) / 23.0;
        end
        else
        begin
//          if FDisplayObject is TXPModelObjectList then
//          with TXPModelObjectList(FDisplayObject)[FDisplayIndex] do
//          begin
//            Pivot.X := Pivot.X + (X - FDragX) / 23.0;
//            Pivot.Y := Pivot.Y - (Y - FDragY) / 23.0;
//          end
//          else
//          begin
          FXOffset := FXOffset + (X - FDragX) / 23.0;
          FYOffset := FYOffset - (Y - FDragY) / 23.0;
//          end;
        end;
        UpdateScreen;
      end;
      mbLeft:
      begin
        FRotX :=  FRotX + Y - FDragY;
        FRotY :=  FRotY + X - FDragX;
        UpdateScreen;
      end;
    end;
    FDragX := X;
    FDragY := Y;
  end;
end;

procedure TFormMain.Panel1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FIsDrag := False;
end;

procedure TFormMain.ResetView;
begin
  FScale := 1.0;
  FXOffset := 0.0;
  FYOffset := -9;
  FZOffset := -25;
  FRotX := 0.0;
  FRotY := 0.0;
  FRotZ := 0.0;
  with FModel do
  begin
    if (SizeX = 0.0) or (SizeY = 0.0) then
      FScale := 1.0
    else
    begin
      if SizeY * FDisplay.Width / FDisplay.Height > SizeX * FDisplay.Height / FDisplay.Width  then
        FScale := 15.0 / SizeY
      else FScale := 18.0 / SizeX;
    end;
    FXOffset := FScale * Center.X * 1.0;
    FYOffset := -FScale * Center.Y;
    FZOffset := -25 - FScale * Center.Z;
  end;
end;

procedure TFormMain.SetColorParam(var AColor: TGLRGBAFloat; AIndex, AValue: Integer;
  AScrollBar: TScrollBar);
var
  n: Integer;
begin
  AColor.RGBA[AIndex] := AValue / 100;
  if (GetAsyncKeyState(VK_CONTROL) <> 0) and (AIndex < 3) then
  begin
    for n := 0 to 2 do
    begin
      if n <> AIndex then
      begin
        AColor.RGBA[n] := AColor.RGBA[AIndex];
        TScrollBar(AScrollBar.Parent.Components[n]).Position := Round(AColor.RGBA[AIndex] * 100);
      end;
    end;
  end;
end;

procedure TFormMain.SetFlipCheckBox;
begin
  with FModel do
  begin
    CheckBox1.Checked := FlipX;
    CheckBox2.Checked := FlipY;
    CheckBox3.Checked := FlipZ;
    CheckBox4.Checked := FlipW;
  end;
end;

procedure TFormMain.SetScrollPos(AScrollBar: TScrollBar;
  ALightIndex, AGroupIndex, AScrollIndex: Integer);
begin
  case ALightIndex of
    -1:
    case AGroupIndex of
      0: AScrollBar.Position := Round(FLights.MatShininess);
      1: AScrollBar.Position := Round(FLights.MatAmbient.RGBA[AScrollIndex] * 100);
      2: AScrollBar.Position := Round(FLights.MatDiffuse.RGBA[AScrollIndex] * 100);
      3: AScrollBar.Position := Round(FLights.MatSpecular.RGBA[AScrollIndex] * 100);
    end;
    0..7:
    case AGroupIndex of
      0: AScrollBar.Position := Round(FLights[ALightIndex].Ambient.RGBA[AScrollIndex] * 100);
      1: AScrollBar.Position := Round(FLights[ALightIndex].Diffuse.RGBA[AScrollIndex] * 100);
      2: AScrollBar.Position := Round(FLights[ALightIndex].Specular.RGBA[AScrollIndex] * 100);
    end;
  end;
end;

procedure TFormMain.SetOrderIndex;
const
  Orders: array[0..2, 0..2, 0..2] of Integer = (
    ((-1, -1, -1), (-1, -1,  0), (-1,  1, -1)),
    ((-1, -1,  2), (-1, -1, -1), ( 3, -1, -1)),
    ((-1,  4, -1), ( 5, -1, -1), (-1, -1, -1))
  );
var
  n: Integer;
begin
  with FModel do
  begin
    n := Orders[XOrder, YOrder, ZOrder];
    if n < 0 then
    begin
      XOrder := 0;
      YOrder := 1;
      ZOrder := 2;
      n := 0;
    end;
    ListBoxOrder.ItemIndex := n;
  end;
end;

procedure TFormMain.GridFileClick(Sender: TObject);
var
  s: string;
begin
  if FLoading then Exit;
  FLoading := True;
  ListBoxTexture.Clear;
  GridFile.Enabled := False;
  try
    s := FFolder + GridFile.Cells[1, GridFile.Row];
    Load3D(s);
  finally
    FLoading := False;
    GridFile.Enabled := True;
    FocusControl(GridFile);
  end;
end;

procedure TFormMain.GridFileMouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
var
  d: GLFLoat;
begin
  d := -Sqrt(FScale);
  if ssCtrl in Shift then
    if ssShift in Shift then
      Zoom(d * 0.01)
    else Zoom(d * 0.001)
  else Zoom(d * 0.0002);
  Handled := True;
end;

procedure TFormMain.GridFileMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
var
  d: GLFloat;
begin
  d := Sqrt(FScale);
  if ssCtrl in Shift then
    if ssShift in Shift then
      Zoom(d * 0.01)
    else Zoom(d * 0.001)
  else Zoom(d * 0.0002);
  Handled := True;
end;

procedure TFormMain.TimerProgressTimer(Sender: TObject);
begin
  PanelProgress.Visible := True;
end;

procedure TFormMain.TreeObjectClick(Sender: TObject);
var
  node: TTreeNode;
begin
  node := TTreeView(Sender).Selected;
  if node <> nil then
  begin
    FDisplayObject := node.Data;
    FDisplayIndex := node.Index;
    UpdateStatus;
  end;
end;

procedure TFormMain.UpdateInfo;
begin
  with FModel do
    StatusBar1.Panels[0].Text := Format('DimX(%.2f, %.2f) DimY(%.2f, %.2f) DimZ(%.2f, %.2f)', [MinPos.X, MaxPos.X, MinPos.Y, MaxPos.Y, MinPos.Z, MaxPos.Z])
      + Format(' CenterX(%.2f) CenterY(%.2f) CenterZ(%.2f)', [Center.X, Center.Y, Center.Z]) ;
end;

procedure TFormMain.UpdateScreen;
const
  cmat_amdif: array[0..3] of GLFloat = (0.0, 0.0, 1.0, 1.0);
var
  p: TGLXYZWFloat;
begin
  if FUpdating then Exit;

  FGL.Activate;
  FGL.Clear;

  glTranslatef(FXOffset, FYOffset, FZOffset);
  glScalef(FScale, FScale, FScale);
  glRotatef(FRotX, 1, 0, 0);
  glRotatef(FRotY, 0, 1, 0);
  glRotatef(FRotZ, 0, 0, 1);

  if FDisplayObject = nil then
    FModel.Paint
  else if FDisplayObject is TXPMaterialList then
    FModel.PaintMat(FDisplayIndex)
  else if FDisplayObject is TXPModelObjectList then
    TXPModelObjectList(FDisplayObject)[FDisplayIndex].Paint;

  glColor4f(1.0, 1.0, 1.0, 1.0);
  glBegin(GL_POLYGON); // Draw Light Position
  try
    with FModel do
    begin
      p := FLights[0].Position;
      glColor4f(1.0, 1.0, 1.0, 1.0);
      glVertex3f(p.X - VScale(0.1), p.Y - VScale(0.1), p.Z);
      glColor4f(1.0, 1.0, 1.0, 1.0);
      glVertex3f(p.X + VScale(0.1), p.Y - VScale(0.1), p.Z);
      glColor4f(1.0, 1.0, 1.0, 1.0);
      glVertex3f(p.X + VScale(0.1), p.Y + VScale(0.1), p.Z);
      glColor4f(1.0, 1.0, 1.0, 1.0);
      glVertex3f(p.X - VScale(0.1), p.Y + VScale(0.1), p.Z);
    end;
  finally
    glEnd;
  end;

  FGL.SwapBuffer;

  Caption := Format('Scale(%.6f) XOffset(%.6f) YOffset(%.6f) ZOffset(%.6f)',
    [FScale, FXOffset, FYOffset, FZOffset]);
end;

procedure TFormMain.UpdateStatus;
var
  nmat: Integer;
begin
  LabelMatName.Caption := '';
  LabelTexture.Caption := '';
  if FDisplayObject is TXPMaterialList then
  begin
    if FDisplayIndex = 0 then
    begin
      LabelMatName.Caption := '{ unassigned material }';
      LabelTexture.Caption := '';
    end
    else
    with TXPMaterialList(FDisplayObject)[FDisplayIndex - 1] do
    begin
      LabelMatName.Caption := Name;
      LabelTexture.Caption := Diffuse.MapFile;
    end;
  end
  else if FDisplayObject is TXPModelObjectList then
  begin
    with TXPModelObjectList(FDisplayObject)[FDisplayIndex] do
    begin
      nmat := 0;
      if Count > 0 then
        nmat := Faces[0].MatIndex
      else if SubObjects.Count > 0 then
        if SubObjects[0].Count > 0 then nmat := SubObjects[0][0].MatIndex;
      if (nmat > 0) and (nmat <= FModel.Materials.Count) then
      begin
        LabelMatName.Caption := FModel.Materials[nmat - 1].Name;
        LabelTexture.Caption := FModel.Materials[nmat - 1].Diffuse.MapFile;
      end;
    end;
  end;
end;

procedure TFormMain.UpdateTextureList(APath: string);
begin
  ListBoxTexture.Clear;
  ListBoxTexture.Items.Add('*** DESELECT TEXTURE ***');
  GetFileList(ListBoxTexture.Items, APath + '*.bmp', True);
  GetFileList(ListBoxTexture.Items, APath + '*.dds', True);
  GetFileList(ListBoxTexture.Items, APath + '*.gif', True);
  GetFileList(ListBoxTexture.Items, APath + '*.jpg', True);
  GetFileList(ListBoxTexture.Items, APath + '*.png', True);
  GetFileList(ListBoxTexture.Items, APath + '*.tga', True);
  GetFileList(ListBoxTexture.Items, APath + '*.tif', True);
end;

procedure TFormMain.UpdateTreeMaterial;
var
  rnode: TTreeNode;
  nm: Integer;
  s: string;
begin
  TreeMaterial.Items.Clear;
  rnode := TreeMaterial.Items.AddObject(nil, 'All materials', nil);
  TreeMaterial.Items.AddChildObject(rnode, '{ unassigned material }', FModel.Materials);
  for nm := 0 to FModel.Materials.Count - 1 do
  begin
    s := Trim(FModel.Materials[nm].Name);
    if s = '' then s := '{ no name }';
    TreeMaterial.Items.AddChildObject(rnode, s, FModel.Materials);
  end;
  rnode.Expand(True);
end;

procedure TFormMain.UpdateTreeObject;
var
  rnode, pnode: TTreeNode;
  nm, ns: Integer;
  s: string;
begin
  TreeObject.Items.Clear;
  rnode := TreeObject.Items.AddObject(nil, 'All objects', nil);
  for nm := 0 to FModel.Count - 1 do
  begin
    s := Trim(FModel[nm].Name);
    if s = '' then s := '{ no name }';
    pnode := TreeObject.Items.AddChildObject(rnode, s, FModel);
    for ns := 0 to FModel[nm].SubObjects.Count - 1 do
    begin
      s := Trim(FModel[nm].SubObjects[ns].Name);
      if s = '' then s := '{ no name }';
      TreeObject.Items.AddChildObject(pnode, s, FModel[nm].SubObjects);
    end;
  end;
  rnode.Expand(True);
end;

function TFormMain.VScale(AValue: GLFloat): GLFloat;
begin
  Result := AValue / FScale;
end;

procedure TFormMain.Zoom(AFactor: GLFloat);
var
  ds: GLFloat;
begin
  ds := FScale;
  FScale := FScale + AFactor * 500;
  if FScale < 0.000005 then FScale := 0.000005;
  FYOffset := (FYOffset / ds) * FScale;
  UpdateScreen;
end;

end.
