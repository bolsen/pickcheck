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
    function WithName(name: String): PropertyChecker;
    function WithConfig(options: TCheckPropertyOptions): PropertyChecker;
    function WithNumberOfTrials(count: Integer): PropertyChecker;
    function WithSeed(seed: longint): PropertyChecker; // TODO: This is non-functioning.
    function StopOnFail(value: Boolean): PropertyChecker;
    function Check: specialize TCheckPropertyBuilderSuite<T>;
  end;


generic function MakeAPropCheckeBuilder<T>(predicateFunc: specialize TPredicateFunc<T>): specialize TPropertyCheckerBuilder<T>;
generic function MakeASuite<T>(options: TCheckPropertyOptions): specialize TCheckPropertyBuilderSuite<T>;

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
  fBuilder.Name := 'PropertyChecker';
  fOptions := TCheckPropertyOptions.Create;
end;

function PropertyChecker.WithConfig(options: TCheckPropertyOptions): PropertyChecker;
begin
  fOptions := options;
  Result := Self;
end;

function PropertyChecker.WithName(name: String): PropertyChecker;
begin
  fOptions.Name := name;
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

function PropertyChecker.StopOnFail(value: Boolean): PropertyChecker;
begin
  fOptions.StopOnFirstFail := value;
  Result := Self;
end;

generic function MakeASuite<T>(options: TCheckPropertyOptions): specialize TCheckPropertyBuilderSuite<T>;
begin
  Result := specialize TCheckPropertyBuilderSuite<T>.Create(options);
end;

function PropertyChecker.Check: specialize TCheckPropertyBuilderSuite<T>;
begin
  Result := specialize MakeASuite<T>(fOptions);
  Result.AddProperty(fBuilder);
  Result.Check;
end;

end.
