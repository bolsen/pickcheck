unit PickCheck.Specifiers;

{$mode delphi}{$H+}{$J-}
{$modeswitch nestedprocvars}
{$modeswitch functionreferences}
{$modeswitch anonymousfunctions}

interface

uses
  cthreads, Classes, SysUtils, StrUtils, Generics.Collections;

const
    Primes: array of Integer = [
    2, 3, 5, 7, 11, 13, 17, 19, 23, 29,
    31, 37, 41, 43, 47, 53, 59, 61, 67, 71,
    73, 79, 83, 89, 97, 101, 103, 107, 109, 113,
    127, 131, 137, 139, 149, 151, 157, 163, 167, 173,
    179, 181, 191, 193, 197, 199, 211, 223, 227, 229,
    233, 239, 241, 251, 257, 263, 269, 271, 277, 281,
    283, 293, 307, 311, 313, 317, 331, 337, 347, 349,
    353, 359, 367, 373, 379, 383, 389, 397, 401, 409,
    419, 421, 431, 433, 439, 443, 449, 457, 461, 463,
    467, 479, 487, 491, 499, 503, 509, 521, 523, 541,
    547, 557, 563, 569, 571, 577, 587, 593, 599, 601,
    607, 613, 617, 619, 631, 641, 643, 647, 653, 659,
    661, 673, 677, 683, 691, 701, 709, 719, 727, 733,
    739, 743, 751, 757, 761, 769, 773, 787, 797, 809,
    811, 821, 823, 827, 829, 839, 853, 857, 859, 863,
    877, 881, 883, 887, 907, 911, 919, 929, 937, 941,
    947, 953, 967, 971, 977, 983, 991, 997];

type
  TSpecifierGeneratorFunc<T> = reference to function: T;
  TMakeObjectFunc<T> = reference to function: T;
  EPickCheckSpecifierError = class(Exception) end;

function Resolve<T>(value: T; rest: array of T): T;

{ These functions generate random values. }
function GenLiteral<T>(Value: T): TSpecifierGeneratorFunc<T>;
function GenBool<Boolean>(bias: Double): TSpecifierGeneratorFunc<Boolean>;
function GenNumber<Integer>(fromNum: Integer = 1; toNum: Integer = 0): TSpecifierGeneratorFunc<Integer>;
function GenOneOf<T>(values: array of T): TSpecifierGeneratorFunc<T>; overload;
function GenOneOf<T>(values: array of T; weights: array of Double): TSpecifierGeneratorFunc<T>; overload;
function GenSequence<T>(values: array of T): TSpecifierGeneratorFunc<T>;
function GenInteger<Integer>: TSpecifierGeneratorFunc<Integer>; overload;
function GenInteger<Integer>(I: Integer; J: Integer): TSpecifierGeneratorFunc<Integer>; overload;
// function GenCharacter<Char>: TSpecifierGeneratorFunc<Char>;
function GenCharacter<Char>(i: String): TSpecifierGeneratorFunc<Char>;
// function GenArray<T>(First: array of T; fillValue: T): TSpecifierGeneratorFunc<array<T>>;
function GenString<T>(parameters: array of String): TSpecifierGeneratorFunc<String>;
function GenAny<T>(): TSpecifierGeneratorFunc<T>;
function GenObject<T>(makeObjectFunc: TMakeObjectFunc<T>): TSpecifierGeneratorFunc<T>;

implementation

function Resolve<T>(value: T; rest: array of T): T;
begin
  Result := value;
end;

function GenLiteral<T>(value: T): TSpecifierGeneratorFunc<T>;
begin
  Result := function: T
    begin
      Result := value;
    end;
end;

function GenBool<Boolean>(bias: Double): TSpecifierGeneratorFunc<Boolean>;
begin
//  bias := Resolve<Double>(bias, []);
  Result := function(): Boolean
  begin
    Result := (Random < bias);
  end;
end;

function GenNumber<Integer>(fromNum: Integer = 1; toNum: Integer = 0): TSpecifierGeneratorFunc<Integer>;
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

  Result := function(): Integer
  begin
      Result := Round(Random * toNum) - fromNum;
    end;
end;

function GenOneOf<T>(values: array of T): TSpecifierGeneratorFunc<T>; overload;
begin
  Result := function: T
  var
    randNum: Integer;
    index: Integer;
  begin
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

function GenOneOf<T>(values: array of T; weights: array of Double): TSpecifierGeneratorFunc<T>; overload;
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
    raise EPickCheckSpecifierError.Create('Values and weight array lengths must be > 0 and equal to each other.');
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

function GenSequence<T>(values: array of T): TSpecifierGeneratorFunc<T>;
begin
end;

function GenInteger<Integer>: TSpecifierGeneratorFunc<Integer>; overload;
begin
  Result := GenOneOf<Integer>(primes);
end;

function GenInteger<Integer>(I: Integer; J: Integer): TSpecifierGeneratorFunc<Integer>; overload;
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

// function GenCharacter<Char>(): TSpecifierGeneratorFunc<Char>;
// begin
//   Result := GenCharacter<Char>(Chr(32), Chr(126));
// end;

function GenCharacter<Char>(i: String): TSpecifierGeneratorFunc<Char>;
begin
//  Result := GenOneOf<Char>(SplitString(i, ''));
end;

//function GenArray<T>(First: array of T; fillValue: T): TSpecifierGeneratorFunc<array of T>;
//begin
//end;

function GenString<T>(parameters: array of String): TSpecifierGeneratorFunc<String>;
begin
end;

function GenAny<T>(): TSpecifierGeneratorFunc<T>;
begin
end;

function GenObject<T>(makeObjectFunc: TMakeObjectFunc<T>): TSpecifierGeneratorFunc<T>;
begin
end;


end.
