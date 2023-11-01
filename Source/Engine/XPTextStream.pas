unit XPTextStream;

interface

uses
  Classes, SysUtils;

const
  DefaultTextBufferSize = 32768;

type
  PCharArray = ^TCharArray;
  TCharArray = array[0..0] of Char;

  TBufferStream = class(TStream)
  private
    FStream: TStream;
    FReadBuffer, FWriteBuffer: array of Char;
    FReadBufferPos, FWriteBufferPos: Integer;
    FReadPos, FWritePos: Int64;
    FReadRemain, FBufferSize: Integer;
  public
    constructor Create(AStream: TStream; ABufSize: Integer = DefaultTextBufferSize);
    destructor Destroy; override;
    function Read(var ABuffer; ACount: Longint): Longint; override;
    function Write(const ABuffer; ACount: Longint): Longint; override;
    function Flush: Longint;
  end;

implementation

// TBufferStream
constructor TBufferStream.Create(AStream: TStream; ABufSize: Integer = DefaultTextBufferSize);
begin
  inherited Create;
  FStream := AStream;
  FBufferSize := ABufSize;
  SetLength(FReadBuffer, ABufSize);
  SetLength(FWriteBuffer, ABufSize);
  FReadPos := 0;
  FWritePos := 0;
  FReadBufferPos := 0;
  FWriteBufferPos := 0;
  FReadRemain := 0;
end;

destructor TBufferStream.Destroy;
begin
  Flush;
  SetLength(FReadBuffer, 0);
  SetLength(FWriteBuffer, 0);
  inherited Destroy;
end;

function TBufferStream.Flush: Longint;
begin
  Result := FWriteBufferPos;
  if Result > 0 then
  begin
    FStream.Write(FWriteBuffer[0], FWriteBufferPos);
    FWriteBufferPos := 0;
  end;
end;

function TBufferStream.Read(var ABuffer; ACount: Longint): Longint;
var
  buf: PCharArray;
  n: Integer;
begin
  Result := 0;
  buf := @ABuffer;
  while Result < ACount do
  begin
    if FReadRemain < 1 then
    begin
      FReadRemain := FStream.Read(FReadBuffer[0], FBufferSize);
      FReadBufferPos := 0;
    end;
    if FReadRemain = 0 then
      ACount := Result
    else
    begin
      n := ACount - Result;
      if n > FReadRemain then n := FReadRemain;
      Move(FReadBuffer[FReadBufferPos], buf[Result], n);
      Inc(FReadBufferPos, n);
      Dec(FReadRemain, n);
      Inc(Result, n);
    end;
  end;
end;

function TBufferStream.Write(const ABuffer; ACount: Longint): Longint;
var
  buf: PCharArray;
  n: Integer;
begin
  Result := 0;
  buf := @ABuffer;
  while Result < ACount do
  begin
    n := ACount - Result;
    if n > (FBufferSize - FWriteBufferPos) then n := FBufferSize - FWriteBufferPos;
    Move(buf[Result], FWriteBuffer[FWriteBufferPos], n);
    Inc(FWriteBufferPos, n);
    Inc(Result, n);
    if FWriteBufferPos >= FBufferSize then
    begin
      FStream.Write(FWriteBuffer[0], FBufferSize);
      FWriteBufferPos := 0;
    end;
  end;
end;

end.
