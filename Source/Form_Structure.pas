unit Form_Structure;

interface

uses
  XPRoutine, XPGL, dglOpenGL, XPModel3DSChunk,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls;

type
  TFormStructure = class(TForm)
    TreeChunks: TTreeView;
    MemoDetail: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TreeChunksClick(Sender: TObject);
  private
    FChunks: TXP3DSChunkList;
    procedure LoadTreeView(ANode: TTreeNode; AChunks: TXP3DSChunkList);
    { Private declarations }
  public
    procedure LoadStructure(AFileName: string);
    { Public declarations }
  end;

var
  FormStructure: TFormStructure;

implementation

{$R *.dfm}

procedure TFormStructure.FormCreate(Sender: TObject);
begin
  FChunks := TXP3DSChunkList.Create;
end;

procedure TFormStructure.FormDestroy(Sender: TObject);
begin
  FChunks.Free;
end;

procedure TFormStructure.LoadStructure(AFileName: string);
var
  fs: TFileStream;
begin
  TreeChunks.Items.Clear;
  fs := TFileStream.Create(AFileName, fmOpenRead);
  try
    FChunks.LoadFromStream(fs);
  finally
    fs.Free;
  end;
  LoadTreeView(nil, FChunks);
  if TreeChunks.Items.Count > 0 then TreeChunks.Items[0].Expand(False);
end;

procedure TFormStructure.LoadTreeView(ANode: TTreeNode; AChunks: TXP3DSChunkList);
var
  node: TTreeNode;
  n: Integer;
begin
  for n := 0 to AChunks.Count - 1 do
  begin
    with AChunks[n] do
    begin
      node := TreeChunks.Items.AddChildObject(ANode, Name, AChunks[n]);
      LoadTreeView(node, SubChunks);
    end;
  end;
end;

procedure TFormStructure.TreeChunksClick(Sender: TObject);
var
  chunk: TXP3DSChunk;
begin
  chunk := TXP3DSChunk(TreeChunks.Selected.Data);
  MemoDetail.Text := chunk.Text;
end;

end.
