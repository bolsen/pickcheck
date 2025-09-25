program tests;

{$mode objfpc}
{$H+}


uses SysUtils, PickCheck, PickCheck.Example;

begin
  {$if declared(UseHeapTrace)}
  HeapTrc.KeepReleased := True;
  {$endif}
  RandSeed := GetTickCount64;
  // writeln(specialize GenLiteral<String>('A')());
  // writeln('test: ', specialize GenBool<Boolean>(0.5)());
  // writeln('test: ', specialize GenBool<Boolean>(0.9)());
  // writeln('num test: ', specialize GenNumber<Integer>(1, 10)());
  // writeln('num oneof: ', specialize GenOneOf<Integer>([1, 2 ,3, 4, 5], [1, 1, 1, 1, 1])());
  // writeln('num prime: ', specialize GenInteger<Integer>()());
  // writeln('num integer: ', specialize GenInteger<Integer>(1, 12393)());
  // //  writeln('char: ', specialize GenCharacter<Char>('abcdefghijklmnop')());
  // writeln(round(random*2000));
  // writeln(round(random*2000));
  // writeln(round(random*2000));
  // writeln(round(random*2000));
  // writeln(round(random*2000));

  //RunExample;
  RunPropertyExample;
RunAnotherExample;
  RunWithClassifier;
//  RunPropertyExample;
//  RunAnotherExample;
//  RunWithClassifier;

end.
