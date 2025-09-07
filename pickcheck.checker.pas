unit pickcheck.checker;

{$mode objfpc}{$H+}{$J-}
{$modeswitch nestedprocvars}
{$modeswitch functionreferences}
{$modeswitch anonymousfunctions}

interface

uses
  pickcheck;

type
  generic TPropertyCheckerBuilder<T> = class (specialize TCheckPropertyBuilder<T>)
    fPredFunc: specialize TPredicateFunc<T>;
  public
    constructor Create(predicateFunc: specialize TPredicateFunc<T>);
    function Predicate(value: array of Integer): Boolean; override;
    function Classify(value: array of Integer): String; override;
  end;


  generic PropertyChecker<T> = class
    fOptions: TCheckPropertyOptions;
    fBuilder: specialize TPropertyCheckerBuilder<T>;
    public
    constructor ForAll(signatures: specialize TSignatures<T>; predicateFunc: specialize TPredicateFunc<T>);
//    procedure ForAllSetup(signatures: specialize TSignatures<T>; predicateFunc: specialize TPredicateFunc<T>);
    function WithConfig(options: TCheckPropertyOptions): PropertyChecker;
    function WithNumberOfTrials(count: Integer): PropertyChecker;
    function WithSeed(seed: longint): PropertyChecker;
    function Check: specialize TCheckPropertyBuilderSuite<T>;
  end;


generic function MakeAPropCheckeBuilder<T>(predicateFunc: specialize TPredicateFunc<T>): specialize TPropertyCheckerBuilder<T>;

implementation

{ TPropertycheckerbuilder }
constructor TPropertyCheckerBuilder.Create(predicateFunc: specialize TPredicateFunc<T>);
begin
  fPredFunc := predicateFunc;
end;

function TPropertyCheckerBuilder.Predicate(value: array of Integer): Boolean;
begin;
  Result := fPredFunc(value);
end;

function TPropertyCheckerBuilder.Classify(value: array of Integer): String;
begin
  Result := '(tbd)';
end;


// The sole reason this exists is that if you define a field with a
// specialize Class<T> and then try to initialize in a procedure
// with a fFoo := specialize Class<T>.Create; it throws a "Duplicate Identifier" error.
// possibly some context: https://forum.lazarus.freepascal.org/index.php?topic=40796.0
generic function MakeAPropCheckeBuilder<T>(predicateFunc: specialize TPredicateFunc<T>): specialize TPropertyCheckerBuilder<T>;
begin
  Result := specialize TPropertyCheckerBuilder<T>.Create(predicateFunc);
end;

constructor PropertyChecker.ForAll(signatures: specialize TSignatures<T>; predicateFunc: specialize TPredicateFunc<T>);
begin
  fBuilder := specialize MakeAPropCheckeBuilder<T>(predicateFunc);
  fBuilder.Signatures := signatures;
  fOptions := TCheckPropertyOptions.Create;
end;

function PropertyChecker.WithConfig(options: TCheckPropertyOptions): PropertyChecker;
begin
  fOptions := options;
  Result := Self;
end;

function PropertyChecker.WithNumberOfTrials(count: Integer): PropertyChecker;
begin
  fOptions.NumberOfTrials := count;
  Result := Self;
end;

function PropertyChecker.WithSeed(seed: longint): PropertyChecker;
begin
  fOptions.Seed := seed;
  Result := Self;
end;

function PropertyChecker.Check: specialize TCheckPropertyBuilderSuite<T>;
begin
  with Result do
  begin
    Create(fOptions);
    AddProperty(fBuilder);
    Check;
  end;
end;

end.
