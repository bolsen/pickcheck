unit pickcheck.example;

{$mode objfpc}{$H+}{$J-}
{$modeswitch nestedprocvars}
{$modeswitch functionreferences}
{$modeswitch anonymousfunctions}

interface

uses pickcheck, pickcheck.specifiers, pickcheck.checker;

type
  TMyCheckPropertyBuilder = class (specialize TCheckPropertyBuilder<Integer>)
  public
   function Predicate(value: array of Integer): Boolean; override;
   function Classify(value: array of Integer): String; override;
  end;

procedure RunExample;
procedure RunPropertyExample;

implementation

function TMyCheckPropertyBuilder.Predicate(value: array of Integer): Boolean;
begin
  Result := value[1] < value[2];
end;

function TMyCheckPropertyBuilder.Classify(value: array of Integer): String;
begin
  if (value[0] > value[1]) then
  begin
    Result := 'gt';
  end else if value[0] < value[1] then
  begin
    Result := 'lt';
  end else if value[0] = value[1] then Exit('eq');
end;

procedure RunExample;
var
  a: specialize TCheckPropertyBuilderSuite<Integer>;
  builder: TMyCheckPropertyBuilder;
  prop: specialize TCheckProperty<Integer>;
  suite: specialize TCheckPropertyBuilderSuite<Integer>;
begin
  a := specialize TCheckPropertyBuilderSuite<Integer>.Create;
  builder := TMyCheckPropertyBuilder.Create;
  builder.Name := 'This tests an array';
  builder.Signatures := [specialize GenNumber<Integer>(1, 2000), specialize GenNumber<Integer>(1, 2000)];
  prop := specialize TCheckProperty<Integer>.Create;
  builder.Build(prop);
  WriteLn('OK');
  WriteLn('Name: ', builder.Name);
  WriteLn('OK');
  with prop do
  begin
    WriteLn('OK');
    WriteLn('Data: ', Values[0], ' ', Values[1]);
    WriteLn('Verdict: ', Verdict);
    WriteLn('Classified as: ', Classification);
  end;
end;

procedure RunPropertyExample;
var
  suite: specialize TCheckPropertyBuilderSuite<Integer>;
  reporter: specialize TCheckPropertyConsoleReporter<Integer>;
begin
  try
    suite := specialize PropertyChecker<Integer>.
      ForAll([specialize GenNumber<Integer>(1,1000), specialize GenNumber<Integer>(1,1000)],
             function(value: array of Integer): Boolean
               begin
                 Result := value[0] > value[1];
               end).
      WithName('Test for greater-than').
      WithNumberOfTrials(100).
      Check;

    reporter := specialize TCheckPropertyConsoleReporter<Integer>.Create(suite.Report[0], suite.Options);
    reporter.DoReport;

  finally
    suite.Free;
    reporter.Free;
  end;

  //  a.AddProperty(builder);
//  a.Check;
end;

end.
