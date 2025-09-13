unit pickcheck.checker;

{$mode delphi}{$H+}{$J-}
{$modeswitch nestedprocvars}
{$modeswitch functionreferences}
{$modeswitch anonymousfunctions}

interface

uses
  pickcheck;

type
  TPropertyCheckerBuilder<T> = class (TCheckPropertyBuilder<T>)
    fPredFunc: TPredicateFunc<T>;
  public
    constructor Create(predicateFunc: TPredicateFunc<T>);
    function Predicate(value: array of Integer): Boolean; override;
    function Classify(value: array of Integer): String; override;
  end;


  PropertyChecker<T> = class
    fOptions: TCheckPropertyOptions;
    fBuilder: TPropertyCheckerBuilder<T>;
    public
    constructor ForAll(signatures: TSignatures<T>; predicateFunc: TPredicateFunc<T>);
    //    procedure ForAllSetup(signatures: specialize TSignatures<T>; predicateFunc: specialize TPredicateFunc<T>);
    function WithName(name: String): PropertyChecker<T>;
    function WithConfig(options: TCheckPropertyOptions): PropertyChecker<T>;
    function WithNumberOfTrials(count: Integer): PropertyChecker<T>;
    function WithSeed(seed: longint): PropertyChecker<T>; // TODO: This is non-functioning.
    function StopOnFail(value: Boolean): PropertyChecker<T>;
    function Check: TCheckPropertySuite<T>;
  end;


function MakeAPropCheckeBuilder<T>(predicateFunc: TPredicateFunc<T>): TPropertyCheckerBuilder<T>;
function MakeASuite<T>(options: TCheckPropertyOptions): TCheckPropertySuite<T>;

implementation

{ TPropertycheckerbuilder }
constructor TPropertyCheckerBuilder<T>.Create(predicateFunc: TPredicateFunc<T>);
begin
  fPredFunc := predicateFunc;
end;

function TPropertyCheckerBuilder<T>.Predicate(value: array of Integer): Boolean;
begin;
  Result := fPredFunc(value);
end;

function TPropertyCheckerBuilder<T>.Classify(value: array of Integer): String;
begin
  Result := '(tbd)';
end;


// The sole reason this exists is that if you define a field with a
// Class<T> and then try to initialize in a procedure
// with a fFoo := Class<T>.Create; it throws a "Duplicate Identifier" error.
// possibly some context: https://forum.lazarus.freepascal.org/index.php?topic=40796.0
function MakeAPropCheckeBuilder<T>(predicateFunc: TPredicateFunc<T>): TPropertyCheckerBuilder<T>;
begin
  Result := TPropertyCheckerBuilder<T>.Create(predicateFunc);
end;

constructor PropertyChecker<T>.ForAll(signatures: TSignatures<T>; predicateFunc: TPredicateFunc<T>);
begin
  fBuilder := MakeAPropCheckeBuilder<T>(predicateFunc);
  fBuilder.Signatures := signatures;
  fBuilder.Name := 'PropertyChecker';
  fOptions := TCheckPropertyOptions.Create;
end;

function PropertyChecker<T>.WithConfig(options: TCheckPropertyOptions): PropertyChecker<T>;
begin
  fOptions := options;
  Result := Self;
end;

function PropertyChecker<T>.WithName(name: String): PropertyChecker<T>;
begin
  fOptions.Name := name;
  Result := Self;
end;

function PropertyChecker<T>.WithNumberOfTrials(count: Integer): PropertyChecker<T>;
begin
  fOptions.NumberOfTrials := count;
  Result := Self;
end;

function PropertyChecker<T>.WithSeed(seed: longint): PropertyChecker<T>;
begin
  fOptions.Seed := seed;
  Result := Self;
end;

function PropertyChecker<T>.StopOnFail(value: Boolean): PropertyChecker<T>;
begin
  fOptions.StopOnFirstFail := value;
  Result := Self;
end;

function MakeASuite<T>(options: TCheckPropertyOptions): TCheckPropertySuite<T>;
begin
  Result := TCheckPropertySuite<T>.Create(options);
end;

function PropertyChecker<T>.Check: TCheckPropertySuite<T>;
begin
  Result := MakeASuite<T>(fOptions);
  Result.AddProperty(fBuilder);
  Result.Check;
end;

end.
