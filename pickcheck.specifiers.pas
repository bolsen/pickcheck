unit PickCheck.Specifiers;

{$mode objfpc}{$H+}{$J-}
{$modeswitch nestedprocvars}
{$modeswitch functionreferences}
{$modeswitch anonymousfunctions}

interface

uses
  cthreads, Classes, SysUtils, StrUtils, Generics.Collections, PickCheck;

const
    Primes: array of Integer = (
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
    947, 953, 967, 971, 977, 983, 991, 997);


generic function Resolve<T>(value: T; rest: array of T): T;

{ These functions generate random values. }
generic function GenLiteral<T>(Value: T): specialize TSpecifierGeneratorFunc<T>;
generic function GenBool<Boolean>(bias: Double): specialize TSpecifierGeneratorFunc<Boolean>;
generic function GenNumber<Integer>(fromNum: Integer = 1; toNum: Integer = 0): specialize TSpecifierGeneratorFunc<Integer>;
generic function GenOneOf<T>(values: array of T): specialize TSpecifierGeneratorFunc<T>;
generic function GenOneOf<T>(values: array of T; weights: array of Double): specialize TSpecifierGeneratorFunc<T>;
generic function GenSequence<T>(values: array of T): specialize TSpecifierGeneratorFunc<T>;
generic function GenInteger<Integer>: specialize TSpecifierGeneratorFunc<Integer>;
generic function GenInteger<Integer>(I: Integer; J: Integer): specialize TSpecifierGeneratorFunc<Integer>;
// generic function GenCharacter<Char>: specialize TSpecifierGeneratorFunc<Char>;
generic function GenCharacter<Char>(i: String): specialize TSpecifierGeneratorFunc<Char>;
// generic function GenArray<T>(First: array of T; fillValue: T): specialize TSpecifierGeneratorFunc<array<T>>;
generic function GenString<T>(parameters: array of String): specialize TSpecifierGeneratorFunc<String>;
generic function GenAny<T>(): specialize TSpecifierGeneratorFunc<T>;
generic function GenObject<T>(makeObjectFunc: specialize TMakeObjectFunc<T>): specialize TSpecifierGeneratorFunc<T>;

implementation

generic function Resolve<T>(value: T; rest: array of T): T;
begin
  Result := value;
end;

generic function GenLiteral<T>(value: T): specialize TSpecifierGeneratorFunc<T>;
begin
  Result := function: T
    begin
      Result := value;
    end;
end;

generic function GenBool<Boolean>(bias: Double): specialize TSpecifierGeneratorFunc<Boolean>;
begin
//  bias := specialize Resolve<Double>(bias, []);
  Result := function(): Boolean
  begin
    Result := (Random < bias);
  end;
end;

generic function GenNumber<Integer>(fromNum: Integer = 1; toNum: Integer = 0): specialize TSpecifierGeneratorFunc<Integer>;
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

generic function GenOneOf<T>(values: array of T): specialize TSpecifierGeneratorFunc<T>;
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

generic function GenOneOf<T>(values: array of T; weights: array of Double): specialize TSpecifierGeneratorFunc<T>;
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

generic function GenSequence<T>(values: array of T): specialize TSpecifierGeneratorFunc<T>;
begin
end;

generic function GenInteger<Integer>: specialize TSpecifierGeneratorFunc<Integer>;
begin
  Result := specialize GenOneOf<Integer>(primes);
end;

generic function GenInteger<Integer>(I: Integer; J: Integer): specialize TSpecifierGeneratorFunc<Integer>;
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

// generic function GenCharacter<Char>(): specialize TSpecifierGeneratorFunc<Char>;
// begin
//   Result := specialize GenCharacter<Char>(Chr(32), Chr(126));
// end;

generic function GenCharacter<Char>(i: String): specialize TSpecifierGeneratorFunc<Char>;
begin
//  Result := specialize GenOneOf<Char>(SplitString(i, ''));
end;

//generic function GenArray<T>(First: array of T; fillValue: T): specialize TSpecifierGeneratorFunc<array of T>;
//begin
//end;

generic function GenString<T>(parameters: array of String): specialize TSpecifierGeneratorFunc<String>;
begin
end;

generic function GenAny<T>(): specialize TSpecifierGeneratorFunc<T>;
begin
end;

generic function GenObject<T>(makeObjectFunc: specialize TMakeObjectFunc<T>): specialize TSpecifierGeneratorFunc<T>;
begin
end;


end.
