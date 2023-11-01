unit Form_XML;

interface

uses
  XPXML,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ToolWin, Menus, ImgList, Grids, ValEdit;

type
  TFormXML = class(TForm)
    TreeXML: TTreeView;
    EditorXML: TValueListEditor;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TreeXMLClick(Sender: TObject);
  private
    FXML: TXPXML;
    FFileName: string;
    procedure MakeTreeNode(XNode: TXPNode; TNode: TTreeNode);
    { Private declarations }
  public
    procedure LoadXML(AFileName: string);
    { Public declarations }
  end;

var
  FormXML: TFormXML;

implementation

{$R *.dfm}

procedure TFormXML.FormCreate(Sender: TObject);
begin
  FXML := TXPXML.Create;
end;

procedure TFormXML.FormDestroy(Sender: TObject);
begin
  FXML.Free;
end;

procedure TFormXML.LoadXML(AFileName: string);
var
  nnode: Integer;
  tnode: TTreeNode;
begin
  FFileName := AFileName;
  FXML.LoadFromFile(FFileName);
  TreeXML.Items.Clear;
  for nnode := 0 to FXML.Nodes.Count - 1 do
  begin
    tnode := TreeXML.Items.AddObject(nil, FXML.Nodes[nnode].Key, FXML.Nodes[nnode]);
    MakeTreeNode(FXML.Nodes[nnode], tnode);
  end;
end;

procedure TFormXML.MakeTreeNode(XNode: TXPNode; TNode: TTreeNode);
var
  tn: TTreeNode;
  n: Integer;
begin
  if not XNode.UsedSubNodes then Exit;
  if (XNode.SubNodes.Count = 1) and not XNode.SubNodes[0].UsedSubNodes then Exit;
  for n := 0 to XNode.SubNodes.Count - 1 do
  begin
    tn := TreeXML.Items.AddChildObject(TNode,
      XNode.SubNodes[n].Key, XNode.SubNodes[n]);
    MakeTreeNode(XNode.SubNodes[n], tn);
  end;
end;

procedure TFormXML.TreeXMLClick(Sender: TObject);
var
  tn: TTreeNode;
  xn: TXPNode;
  n: Integer;
  s: string;
begin
  EditorXML.Strings.Clear;
  tn := TreeXML.Selected;
  if tn = nil then Exit;
  xn := TXPNode(tn.Data);
  if xn.SubNodes.Count = 1 then
  begin
    if not xn.SubNodes[0].UsedSubNodes then
    begin
      s := xn.FirstSubKey;
      if Length(s) > 50 then s := Copy(s, 1, 50) + '...';
      EditorXML.Values['.'] := s;
    end;
  end;
  if xn.UsedAttributes then
  begin
    for n := 0 to xn.Attributes.Count - 1 do
    begin
      s := xn.Attributes.Items[n].Value;
      if Length(s) > 50 then s := Copy(s, 1, 50) + '...';
      EditorXML.Values[xn.Attributes.Items[n].Name] := s;
    end;
  end;
  Caption := xn.Key;
end;

end.
