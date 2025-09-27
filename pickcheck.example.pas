unit PickCheck.Example;

{$mode delphi}{$H+}{$J-}
{$modeswitch nestedprocvars}
{$modeswitch functionreferences}
{$modeswitch anonymousfunctions}

interface

uses PickCheck.types, PickCheck.Specifiers, PickCheck.Checker;

type
  TMyCheckPropertyBuilder = class(TCheckPropertyBuilder<Integer>)
  public
   function Predicate(value: array of Integer): Boolean; override;
   function Classify(value: array of Integer): String; override;
  end;

procedure RunExample;
procedure RunPropertyExample;
procedure RunAnotherExample;
procedure RunWithClassifier;

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
  a: TCheckPropertySuite<Integer>;
  builder: TMyCheckPropertyBuilder;
  prop: TCheckProperty<Integer>;
  suite: TCheckPropertySuite<Integer>;
begin
  a := TCheckPropertySuite<Integer>.Create;
  builder := TMyCheckPropertyBuilder.Create;
  builder.Name := 'This tests an array';
  builder.Signatures := [GenNumber<Integer>(1, 2000), GenNumber<Integer>(1, 2000)];
  prop := builder.Build;
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
  suite: TCheckPropertySuite<Integer>;
  reporter: TCheckPropertyConsoleReporter<Integer>;
begin
  try
    suite := PropertyChecker<Integer>.
      ForAll([GenNumber<Integer>(1,1000), GenNumber<Integer>(1,1000)],
             function(value: array of Integer): Boolean
               begin
                 Result := value[0] > value[1];
               end).
      WithName('Test for greater-than').
      WithClassifier(function(values: array of Integer): String
                       begin
                         if values[0] > values[1] then
                           Result := 'gt'
                         else if values[0] <= values[1] then
                           Result := 'lte'
                       end).
      WithNumberOfTrials(100).
      Check;

    reporter := TCheckPropertyConsoleReporter<Integer>.Create(suite.Report[0], suite.Options);
    reporter.DoReport;

  finally
    suite.Free;
    reporter.Free;
  end;

  //  a.AddProperty(builder);
//  a.Check;
end;


procedure RunAnotherExample;
var
  f: PropertyChecker<Integer>;
  suite: TCheckPropertySuite<Integer>;
  reporter: TCheckPropertyConsoleReporter<Integer>;

begin
  try

    suite := PropertyChecker<Integer>.
      ForAll([GenNumber<Integer>(1,2000)],
             function(value: array of Integer): Boolean
               begin
                 Result := value[0] <= 2000;
               end).
      WithName('Check specifier').
      WithNumberOfTrials(100).
      Check;

    reporter := TCheckPropertyConsoleReporter<Integer>.Create(suite.Report[0], suite.Options);
    reporter.DoReport;


  finally
//    f.Free;
    suite.Free;
    reporter.Free;
  end;

end;

procedure RunWithClassifier;
var
  suite: TCheckPropertySuite<Integer>;
  reporter: TCheckPropertyConsoleReporter<Integer>;

begin
  try

    suite := PropertyChecker<Integer>.
      ForAll([GenNumber<Integer>(1,2000)],
             function(value: array of Integer): Boolean
               begin
                 Result := value[0] <= 2000;
               end).
      WithName('Using a classifier for the inputs').
      WithClassifier(function (value: array of Integer): String
                       begin
                         if value[0] > 2000 then
                         begin
                           Result := 'Value greater than 2000';
                         end else begin
                           Result := 'Value less than 2000';
                         end;
                       end).
      WithNumberOfTrials(100).
      Check;

    reporter := TCheckPropertyConsoleReporter<Integer>.Create(suite.Report[0], suite.Options);
    reporter.DoReport;

  finally
    suite.Free;
    reporter.Free;
  end;

end;

end.
