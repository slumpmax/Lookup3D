unit XPStrings;

interface

uses
  SysUtils;

function StringIndex(AItem: string; const ALists: string; AItemLength: Integer): Integer; overload;
function StringIndex(const AItem: string; const ALists: array of string): Integer; overload;
function FetchStringIndex(var ASource: string; const ALists: string; ALength: Integer): Integer; overload;
function FetchStringIndex(var ASource: string; const ALists: array of string): Integer; overload;
function FetchHeader(var ASource: string): string;

implementation

function StringIndex(AItem: string; const ALists: string; AItemLength: Integer): Integer;
var
  p: Integer;
begin
  AItem := Copy(AItem + StringOfChar(' ', AItemLength), 1, AItemLength);
  p := Pos(AItem, ALists);
  if p > 0 then p := ((p - 1) div AItemLength) + 1;
  Result := p;
end;

function StringIndex(const AItem: string; const ALists: array of string): Integer;
var
  n: Integer;
begin
  Result := -1;
  n := Length(ALists);
  while (n > 0) and (Result < 0) do
  begin
    Dec(n);
    if ALists[n] = AItem then Result := n;
  end;
end;

function FetchStringIndex(var ASource: string; const ALists: string; ALength: Integer): Integer;
var
  p: Integer;
begin
  p := Pos(' ', ASource);
  if p = 0 then p := Length(ASource) + 1;
  Result := StringIndex(Copy(ASource, 1, p - 1), ALists, ALength);
  Delete(ASource, 1, p);
  ASource := TrimLeft(ASource);
end;

function FetchStringIndex(var ASource: string; const ALists: array of string): Integer;
var
  p: Integer;
begin
  p := Pos(' ', ASource);
  if p = 0 then p := Length(ASource) + 1;
  Result := StringIndex(Copy(ASource, 1, p - 1), ALists);
  Delete(ASource, 1, p);
  ASource := TrimLeft(ASource);
end;

function FetchHeader(var ASource: string): string;
var
  p: Integer;
begin
  p := Pos(': ', ASource);
  Result := UpperCase(Copy(ASource, 1, p - 1));
  if p > 0 then ASource := Copy(ASource, p + 2, Length(ASource) - p - 1);
end;

end.
