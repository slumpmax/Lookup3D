unit XPXML;

interface

uses
  XPRoutine, XPList,
  Windows, Classes, Graphics, SysUtils;

type
  TXPXML = class;
  TXPNodeList = class;

  TXPNode = class
  private
    FAttributes: TXPStringCollection;
    FSubNodes: TXPNodeList;
    function GetSubKey: string;
  protected
    function GetAttributes(AIndex: string): string;
    procedure SetAttributes(AIndex, AValue: string);
  public
    Key: string;
    Commented, UsedAttributes, UsedSubNodes: Boolean;
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Assign(ANode: TXPNode);
    property Attributes: TXPStringCollection read FAttributes;
    function FirstSubKey: string;
    property SubNodes: TXPNodeList read FSubNodes;
    property SubKey: string read GetSubKey;
  end;

  TXPNodeList = class(TList)
  private
    FChar: string;
    FStream: TStream;
    function GetStrFromStream(ADelimiters: string): string;
    function GetQuotedStrFromStream: string;
    function GetCommentFromStream: string;
    function GetNodeFromStream(ParentKey: string; AXML: TXPXML): TXPNode;
    function GetKeyFromStream: string;
    function GetAttributeFromStream: TXPStringCollectionItem;
    function JSONEncode(AText: string): string;
    procedure GetCharFromStream;
    procedure GetCloseTag(AKey: string);
    procedure PutCommentToStream(AComment, AIndent: string);
    procedure PutNodeToStream(ANode: TXPNode; AIndent: string);
    procedure PutNodeToJSONStream(ANode: TXPNode; AIndent: string);
    procedure TrimCharFromStream;
  protected
    function GetNodes(n: Integer): TXPNode;
    function GetNodeByName(AKey: string): TXPNode;
    function GetNodeByAttribute(AName, AValue: string): TXPNode;
  public
    function FindNode(AKey: string): TXPNode;
    function AddNode: TXPNode; reintroduce;
    function InsertNode(n: Integer): TXPNode; reintroduce;
    function IndexOf(AKey: string): Integer;
    function IndexOfAttribute(AName, AValue: string): Integer;
    property Nodes[n: Integer]: TXPNode read GetNodes; default;
    property NodeByName[AKey: string]: TXPNode read GetNodeByName;
    property NodeByAttribute[AName, AValue: string]: TXPNode read GetNodeByAttribute;
  end;

  TXPXML = class
  private
    FNodes: TXPNodeList;
    FOnProgress: TProgressEvent;
    procedure Progress(Sender: TObject; Stage: TProgressStage;
      PercentDone: Byte;  RedrawNow: Boolean; const R: TRect; const Msg: string); dynamic;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromFile(AFileName: string);
    procedure LoadFromStream(AStream: TStream);
    procedure SaveToStream(AStream: TStream);
    procedure SaveToJSONStream(AStream: TStream);
    procedure SaveToFile(AFileName: string);
    procedure SaveToJSONFile(AFileName: string);
    property Nodes: TXPNodeList read FNodes;
    property OnProgress: TProgressEvent read FOnProgress write FOnProgress;
  end;

implementation

{ TXPXML }

constructor TXPXML.Create;
begin
  FNodes := TXPNodeList.Create;
  FOnProgress := nil;
end;

destructor TXPXML.Destroy;
begin
  FNodes.Free;
  inherited;
end;

procedure TXPXML.LoadFromFile(AFileName: string);
var
  fs: TFileStream;
  ms: TMemoryStream;
begin
  Progress(Self, psStarting, 0, False, Rect(0, 0, 0, 0), 'Reading...');
  try
    ms := TMemoryStream.Create;
    try
      fs := TFileStream.Create(AFileName, fmOpenRead);
      try
        ms.LoadFromStream(fs);
        LoadFromStream(ms);
      finally
        fs.Free;
      end;
    finally
      ms.Free;
    end;
  finally
    Progress(Self, psEnding, 100, True, Rect(0, 0, 0, 0), 'Decoding...');
  end;
end;

procedure TXPXML.LoadFromStream(AStream: TStream);
var
  node: TXPNode;
begin
  FNodes.Clear;
  FNodes.FStream := AStream;
  FNodes.GetCharFromStream;
  if FNodes.FChar = #$EF then
  begin
    FNodes.GetCharFromStream;
    FNodes.GetCharFromStream;
    FNodes.GetCharFromStream;
  end;
  node := FNodes.GetNodeFromStream('', Self);
  while Assigned(node) do
  begin
    FNodes.Add(node);
    node := FNodes.GetNodeFromStream('', Self);
  end;
end;

procedure TXPXML.Progress(Sender: TObject; Stage: TProgressStage;
  PercentDone: Byte; RedrawNow: Boolean; const R: TRect; const Msg: string);
begin
  if Assigned(FOnProgress) then
    FOnProgress(Sender, Stage, PercentDone, RedrawNow, R, Msg);
end;

procedure TXPXML.SaveToFile(AFileName: string);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(AFileName, fmCreate);
  try
    SaveToStream(fs);
  finally
    fs.Free;
  end;
end;

procedure TXPXML.SaveToJSONFile(AFileName: string);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(AFileName, fmCreate);
  try
    SaveToJSONStream(fs);
  finally
    fs.Free;
  end;
end;

procedure TXPXML.SaveToJSONStream(AStream: TStream);
var
  n: Integer;
begin
  FNodes.FStream := AStream;
  AStream.WriteString('['#13#10);
  for n := 0 to FNodes.Count - 1 do FNodes.PutNodeToJSONStream(FNodes.Items[n], '  ');
  AStream.WriteString(#13#10']');
end;

procedure TXPXML.SaveToStream(AStream: TStream);
var
  n: Integer;
begin
  FNodes.FStream := AStream;
  for n := 0 to FNodes.Count - 1 do FNodes.PutNodeToStream(FNodes.Items[n], '');
end;

{ TXPXMLNodeList }

function TXPNodeList.AddNode: TXPNode;
begin
  Result := TXPNode.Create;
  Add(Result);
end;

function TXPNodeList.GetNodeByAttribute(AName, AValue: string): TXPNode;
var
  n: Integer;
begin
  n := IndexOfAttribute(AName, AValue);
  if n < 0 then
    Result := nil
  else Result := Items[n];
end;

function TXPNodeList.GetNodeByName(AKey: string): TXPNode;
var
  n: Integer;
begin
  n := IndexOf(AKey);
  if n < 0 then
    Result := nil
  else Result := Items[n];
end;

function TXPNodeList.GetNodes(n: Integer): TXPNode;
begin
  Result := TXPNode(inherited Items[n]);
end;

function TXPNodeList.FindNode(AKey: string): TXPNode;
var
  p: Integer;
  s: string;
begin
  p := Pos('/', AKey);
  if p = 0 then p := Length(AKey) + 1;
  s := Copy(AKey, 1, p - 1);
  System.Delete(AKey, 1, p);
  Result := NodeByName[s];
  if (Result <> nil) and (AKey <> '') then Result := Result.FSubNodes.FindNode(AKey);
end;

function TXPNodeList.GetAttributeFromStream: TXPStringCollectionItem;
var
  k, v: string;
begin
  k := GetStrFromStream('?=/> '#9#13#10);
  if k = '' then
    Result := nil
  else
  begin
    TrimCharFromStream;
    if FChar <> '=' then
      raise Exception.Create('XML: Attribure value error.')
    else
    begin
      GetCharFromStream;
      v := GetQuotedStrFromStream;
      GetCharFromStream;
      Result := TXPStringCollectionItem.Create;
      Result.Name := k;
      Result.Value := v;
    end;
  end;
end;

procedure TXPNodeList.GetCharFromStream;
var
  b1, b2, b3, b4: Byte;
begin
  FChar := '';
  if FStream.ReadData(b1, 1) = 0 then Exit;
  if (b1 and $80) = 0 then
    FChar := Chr(b1)
  else
  begin
    if FStream.ReadData(b2, 1) = 0 then Exit;
    if (b1 and $E0) = $C0 then
      FChar := string(UTF8Encode(AnsiChar(b1) + AnsiChar(b2)))
    else
    begin
      if FStream.ReadData(b3, 1) = 0 then Exit;
      if (b1 and $F0) = $E0 then
        FChar := string(UTF8Encode(AnsiChar(b1) + AnsiChar(b2) + AnsiChar(b3)))
      else
      begin
        if FStream.ReadData(b4, 1) = 0 then Exit;
        if (b1 and $F8) = $F0 then
        begin
          FChar := string(UTF8Encode(AnsiChar(b1) + AnsiChar(b2) + AnsiChar(b3) + AnsiChar(b4)));
        end;
      end;
    end;
  end;
end;

procedure TXPNodeList.GetCloseTag(AKey: string);
begin
  if Copy(Akey, 1, 1) = '?' then
  begin
    if FChar <> '?' then
      raise Exception.Create('XML: Close tag error')
    else GetCharFromStream;
  end;
  if FChar <> '>' then
    raise Exception.Create('XML: Close tag error')
  else GetCharFromStream;
end;

function TXPNodeList.GetCommentFromStream: string;
var
  cm: string;
begin
  cm := FChar;
  Result := '';
  while (FChar <> '') and (cm <> '-->') do
  begin
    Result := Result + FChar;
    GetCharFromStream;
    cm := cm + FChar;
    cm := Copy(cm, Length(cm) - 2, 3);
  end;
  if FChar <> '' then System.Delete(Result, Length(Result) - 1, 2);
  GetCharFromStream;
end;

function TXPNodeList.GetKeyFromStream: string;
begin
  Result := GetStrFromStream('/> '#9#13#10);
end;

function TXPNodeList.GetNodeFromStream(ParentKey: string; AXML: TXPXML): TXPNode;
var
  attr: TXPStringCollectionItem;
  node: TXPNode;
  s: string;
begin
  AXML.Progress(AXML, psRunning, FStream.Position * 100 div FStream.Size, False,
    Rect(0, 0, 0, 0), 'Reading...');
  
  Result := nil;
  TrimCharFromStream;

  if FChar <> '<' then
  begin
    if FChar <> '' then
    begin
      Result := TXPNode.Create;
      with Result do
      begin
        Key := FChar;
        GetCharFromStream;
        while (FChar <> '') and (FChar <> '<') do
        begin
          Key := Key + FChar;
          GetCharFromStream;
        end;
      end;
    end;
    Exit;
  end;

  GetCharFromStream;

  if FChar = '/' then
  begin
    GetCharFromStream;
    if ParentKey <> GetKeyFromStream then raise Exception.Create('XML: End tag not match.');
    TrimCharFromStream;
    if FChar <> '>' then raise Exception.Create('XML: End tag syntax error');
    GetCharFromStream;
    Exit;
  end;

  Result := TXPNode.Create;
  with Result do
  begin
    if FChar = '!' then
    begin
      GetCharFromStream;
      if FChar <> '-' then raise Exception.Create('XML: Comment block error');
      GetCharFromStream;
      if FChar <> '-' then raise Exception.Create('XML: Comment block error');
      GetCharFromStream;
      Key := GetCommentFromStream;
      Commented := True;
      Exit;
    end;

    Key := GetKeyFromStream;
    if Key = '' then
      raise Exception.Create('XML: No key name')
    else
    begin
      UsedAttributes := True;
      TrimCharFromStream;
      attr := GetAttributeFromStream;
      while Assigned(attr) do
      begin
        FAttributes.Add(attr);
        TrimCharFromStream;
        attr := GetAttributeFromStream;
      end;
      TrimCharFromStream;
      if Copy(Key, 1, 1) = '?' then
        s := '?'
      else s := '/';
      if FChar = s then
      begin
        GetCharFromStream;
        if FChar <> '>' then
          raise Exception.Create('XML: End tag error')
        else GetCharFromStream;
      end
      else
      begin
        GetCloseTag(Key);
        UsedSubNodes := True;
        node := GetNodeFromStream(Key, AXML);
        while Assigned(node) do
        begin
          FSubNodes.Add(Node);
          node := GetNodeFromStream(Key, AXML);
        end;
      end;
    end;
  end;
end;

function TXPNodeList.GetQuotedStrFromStream: string;
var
  qchar: string;
begin
  Result := '';
  if (FChar <> '''') and (FChar <> '"') then
    raise Exception.Create('XML: Attribute value error.')
  else
  begin
    qchar := FChar;
    GetCharFromStream;
    while (FChar <> '') and (FChar <> qchar) do
    begin
      Result := Result + FChar;
      GetCharFromStream;
    end;
  end;
end;

function TXPNodeList.GetStrFromStream(ADelimiters: string): string;
var
  n: Integer;
  c: AnsiChar;
begin
  Result := '';
  n := Length(FChar);
  while (n > 0) and (Pos(FChar[1], ADelimiters) = 0) do
  begin
    Result := Result + FChar[1];
    n := FStream.Read(c, 1);
    FChar[1] := Char(c);
  end;
  if n = 0 then FChar := '';
end;

function TXPNodeList.IndexOf(AKey: string): Integer;
var
  n: Integer;
begin
  Result := -1;
  n := Count;
  while (Result < 0) and (n > 0) do
  begin
    Dec(n);
    if SameText(Nodes[n].Key, AKey) then Result := n;
  end;
end;

function TXPNodeList.IndexOfAttribute(AName, AValue: string): Integer;
var
  n: Integer;
  s: string;
begin
  Result := -1;
  n := Count;
  while (Result < 0) and (n > 0) do
  begin
    Dec(n);
    s := Nodes[n].Attributes[AName];
    if SameText(Nodes[n].Attributes[AName], AValue) then Result := n;
  end;
end;

function TXPNodeList.InsertNode(n: Integer): TXPNode;
begin
  Result := TXPNode.Create;
  Insert(n, Result);
end;

function TXPNodeList.JSONEncode(AText: string): string;
var
  n: Integer;
  s: string;
  c: Char;
begin
  Result := '';
  for n := 1 to Length(AText) do
  begin
    c := AText[n];
    case c of
      #9: s := '\t';
      #10: s := '\n';
      #13: s := '\r';
      '"': s := '\"';
      '\': s := '\\';
    else
      s := c;
    end;
    Result := Result + s;
  end;
end;

procedure TXPNodeList.PutCommentToStream(AComment, AIndent: string);
var
  p: Integer;
  s: string;
begin
  FStream.WriteString(AIndent + '<!--');
  while AComment <> '' do
  begin
    p := Pos(#10, AComment);
    if p > 0 then
    begin
      s := Copy(AComment, 1, p - 1);
      System.Delete(AComment, 1, p);
      while Copy(s, Length(s), 1) = #13 do System.Delete(s, Length(s), 1);
      s := Trim(s) + #13#10 + AIndent;
    end
    else
    begin
      s := Trim(AComment);
      AComment := '';
    end;
    FStream.WriteString(s);
  end;
  FStream.Writeln('-->');
end;

procedure TXPNodeList.PutNodeToJSONStream(ANode: TXPNode; AIndent: string);
var
  n: Integer;
  s: string;
begin
  if ANode.UsedAttributes then
  begin
    FStream.WriteString(AIndent + '{'#13#10'  ' + AIndent + '"Object": "' + ANode.Key + '"');
    for n := 0 to ANode.FAttributes.Count - 1 do
    begin
      s := ','#13#10'  ' + AIndent + '"' + ANode.FAttributes.Names[n] + '": "' + JSONEncode(ANode.FAttributes[n]) + '"';
      FStream.WriteString(s);
    end;
    if ANode.UsedSubNodes then
    begin
      s := ','#13#10'  ' + AIndent + '"Childs": ['#13#10;
      FStream.WriteString(s);
      for n := 0 to ANode.FSubNodes.Count - 1 do
      begin
        if n > 0 then FStream.WriteString(','#13#10);
        PutNodeToJSONStream(ANode.FSubNodes[n], AIndent + '    ');
      end;
      FStream.WriteString(#13#10 + AIndent + '  ]');
    end;
    FStream.WriteString(#13#10 + AIndent + '}');
  end
  else
  begin
    if ANode.Commented then
      PutCommentToStream(ANode.Key, AIndent)
    else
    begin
      s := AIndent + '"' + JSONEncode(ANode.Key) + '"';
      FStream.WriteString(s);
    end;
  end;
end;

procedure TXPNodeList.PutNodeToStream(ANode: TXPNode; AIndent: string);
var
  node: TXPNode;
  done: Boolean;
  n: Integer;
  s: string;
begin
  if ANode.UsedAttributes then
  begin
    FStream.WriteString(AIndent);
    s := '<' + ANode.Key;
    FStream.WriteString(s);
    for n := 0 to ANode.FAttributes.Count - 1 do
    begin
      s := ' ' + ANode.FAttributes.Names[n] + '="' + ANode.FAttributes[n] + '"';
      FStream.WriteString(s);
    end;
    if ANode.UsedSubNodes then
    begin
      n := ANode.FSubNodes.Count;
      done := n > 0;
      if done then
        node := ANode.FSubNodes[0]
      else node := nil;
      if done then done := (n > 1) or node.UsedAttributes or node.Commented;
      if done then
      begin
        s := '>'#13#10;
        FStream.WriteString(s);
        s := AIndent + '  ';
        for n := 0 to ANode.FSubNodes.Count - 1 do PutNodeToStream(ANode.FSubNodes[n], s);
      end
      else
      begin
        s := '>';
        if n > 0 then s := s + node.Key;
        FStream.WriteString(s);
      end;
      s := '</' + ANode.Key + '>' + #13#10;
      if done then s := AIndent + s;
      FStream.WriteString(s);
    end
    else
    begin
      if Copy(ANode.Key, 1, 1) = '?' then
        s := '?'
      else s := '/';
      s := s + '>'#13#10;
      FStream.WriteString(s);
    end;
  end
  else
  begin
    if ANode.Commented then
      PutCommentToStream(ANode.Key, AIndent)
    else
    begin
      s := AIndent + ANode.Key + #13#10;
      FStream.WriteString(s);
    end;
  end;
end;

procedure TXPNodeList.TrimCharFromStream;
var
  n: Integer;
begin
  n := Length(FChar);
  while (n > 0) and CharInSet(FChar[1], [' ', #9, #13, #10]) do n := FStream.ReadChar(FChar[1]);
  if n = 0 then FChar := '';
end;

{ TXPNode }

procedure TXPNode.Assign(ANode: TXPNode);
begin
  inherited;
  with ANode do
  begin
    Self.FAttributes.Assign(FAttributes);
    Self.Commented := Commented;
    Self.Key := Key;
    Self.UsedAttributes := UsedAttributes;
  end;
end;

procedure TXPNode.Clear;
begin
  FAttributes.Clear;
  FSubNodes.Clear;
  Key := '';
  Commented := False;
  UsedAttributes := False;
  UsedSubNodes := False;
end;

constructor TXPNode.Create;
begin
  FAttributes := TXPStringCollection.Create;
  FSubNodes := TXPNodeList.Create;
  Key := '';
  Commented := False;
  UsedAttributes := False;
  UsedSubNodes := False;
end;

destructor TXPNode.Destroy;
begin
  FAttributes.Free;
  FSubNodes.Free;
  inherited;
end;

function TXPNode.GetAttributes(AIndex: string): string;
begin
  Result := FAttributes[AIndex];
end;

function TXPNode.GetSubKey: string;
begin
  if FSubNodes.Count > 0 then
    Result := FSubNodes[0].Key
  else Result := '';
end;

procedure TXPNode.SetAttributes(AIndex, AValue: string);
begin
  FAttributes[AIndex] := AValue;
end;

function TXPNode.FirstSubKey: string;
begin
  Result := '';
  if Assigned(FSubNodes) then
  begin
    if FSubNodes.Count > 0 then Result := FSubNodes[0].Key;
  end;
end;

end.
