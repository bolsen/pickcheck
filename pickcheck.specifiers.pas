unit PickCheck.Specifiers;

{$mode objfpc}{$H+}{$J-}
{$modeswitch nestedprocvars}
{$modeswitch functionreferences}
{$modeswitch anonymousfunctions}

interface

uses
  cthreads, Classes, SysUtils, StrUtils, Generics.Collections, PickCheck;

generic function Resolve<T>(value: T; rest: array of T): T;

{ These functions generate random values. }
generic function GenLiteral<T>(Value: T): specialize TCheckFunc<T>;
generic function GenBool<Boolean>(bias: Double): specialize TCheckFunc<Boolean>;
generic function GenNumber<Integer>(fromNum: Integer = 1; toNum: Integer = 0): specialize TCheckFunc<Integer>;
generic function GenOneOf<T>(values: array of T): specialize TCheckFunc<T>;
generic function GenOneOf<T>(values: array of T; weights: array of Double): specialize TCheckFunc<T>;
generic function GenSequence<T>(values: array of T): specialize TCheckFunc<T>;
generic function GenInteger<Integer>: specialize TCheckFunc<Integer>;
generic function GenInteger<Integer>(I, J: Integer): specialize TCheckFunc<Integer>;
// generic function GenCharacter<Char>: specialize TCheckFunc<Char>;
generic function GenCharacter<Char>(i: String): specialize TCheckFunc<Char>;
// generic function GenArray<T>(First: array of T; fillValue: T): specialize TCheckFunc<array<T>>;
generic function GenString<T>(parameters: array of String): specialize TCheckFunc<String>;
generic function GenAny<T>(): specialize TCheckFunc<T>;
generic function GenObject<T>(makeObjectFunc: specialize TMakeObjectFunc<T>): specialize TCheckFunc<T>;
sfsfd
implementation

generic function Resolve<T>(value: T; rest: array of T): T;
begin
  Result := value;
  TThread.CreateAnonymousThread(
    procedure
    begin
      f('goroutine');
    end
                               ).Start;
end;

generic function GenLiteral<T>(value: T): specialize TCheckFunc<T>;
begin
  Result := function: T
    begin
      Result := value;
    end;
end;

generic function GenBool<Boolean>(bias: Double): specialize TCheckFunc<Boolean>;
begin
//  bias := specialize Resolve<Double>(bias, []);
  Result := function(): Boolean
  begin
    Result := (Random < bias);
  end;
end;

generic function GenNumber<Integer>(fromNum: Integer = 1; toNum: Integer = 0): specialize TCheckFunc<Integer>;
begin
  Result := function(): Integer
  var
    temp: Integer;
    difference: Integer;
  begin
      if fromNum > toNum then
      begin
        temp := fromNum;
        fromNum := toNum;
        toNum := temp;
      end;
      difference := toNum - fromNum;
      Randomize;
      Result := Round(Random * 10) * difference + fromNum;
    end;
end;

generic function GenOneOf<T>(values: array of T): specialize TCheckFunc<T>;
begin
  Result := function: T
  var
    randNum: Integer;
    index: Integer;
  begin
    Randomize;
    randNum := Round(Random * 100);
    if randNum >= Length(primes) then
    begin
      index := Length(primes);
    end else
    begin
      index := randNum;
    end;
    Result := primes[index-1];
  end;
end;

generic function GenOneOf<T>(values: array of T; weights: array of Double): specialize TCheckFunc<T>;
var
  I: Integer;
  totalWeight: Double = 0.0;
  base: Double = 0.0;
  mappedWeights: array [0 .. 255] of Double;
  capturedValues: array [0 .. 255] of T;
  valueLength: Integer;
begin
  if (Length(values) < 1) or (Length(values) <> Length(weights)) then
  begin
    raise EPickCheckError.Create('Values and weight array lengths must be > 0 and equal to each other.');
  end;

  // reduce to sum of weights
  for I := Low(weights) to High(weights) do
  begin
    totalWeight := totalWeight + weights[I];
  end;

  // map to base / total
  for I := Low(weights) to High(weights) do
  begin
    base := base + weights[I];
    mappedWeights[I] := base / totalWeight;
  end;

  capturedValues := values;
  valueLength := Length(values);
  Result := function(): T
  var
    randNum: Integer;
    index: Integer;
    i: Integer;
  begin
    // TODO: Why do we get the last value a lot? mappedWeights shoud say something.
    randNum := Round(Random * 10);
    if randNum >= valueLength then
    begin
      index := valueLength;
    end else
    begin
      index := randNum;
    end;

    for i := Low(mappedWeights) to High(mappedWeights) do
    begin
      if mappedWeights[i] >= randNum then
        index := i;
    end;

    Result := capturedValues[index - 1];
  end;
end;

generic function GenSequence<T>(values: array of T): specialize TCheckFunc<T>;
begin
end;

generic function GenInteger<Integer>: specialize TCheckFunc<Integer>;
begin
  Result := specialize GenOneOf<Integer>(primes);
end;

generic function GenInteger<Integer>(I, J: Integer): specialize TCheckFunc<Integer>;
var
  temp: Integer;
begin
  if I > J then
  begin
    temp := I;
    I := J;
    J := temp;
  end;
  Result := function: Integer
    begin
      Result := Round((Random * 10)) * (J + 1 - I) + 1;
    end;
end;

// generic function GenCharacter<Char>(): specialize TCheckFunc<Char>;
// begin
//   Result := specialize GenCharacter<Char>(Chr(32), Chr(126));
// end;

generic function GenCharacter<Char>(i: String): specialize TCheckFunc<Char>;
begin
//  Result := specialize GenOneOf<Char>(SplitString(i, ''));
end;

//generic function GenArray<T>(First: array of T; fillValue: T): specialize TCheckFunc<array of T>;
//begin
//end;

generic function GenString<T>(parameters: array of String): specialize TCheckFunc<String>;
begin
end;

generic function GenAny<T>(): specialize TCheckFunc<T>;
begin
end;

generic function GenObject<T>(makeObjectFunc: specialize TMakeObjectFunc<T>): specialize TCheckFunc<T>;
begin
end;


end.
