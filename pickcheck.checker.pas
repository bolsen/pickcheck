unit PickCheck.Checker;

{$mode delphi}{$H+}{$J-}
{$modeswitch nestedprocvars}
{$modeswitch functionreferences}
{$modeswitch anonymousfunctions}

interface

uses
  PickCheck.Types;

type
  TPredicateFunc<T> = function(value: array of T): Boolean; // unlike JSCheck, this is passed into an internal function.
  TClassifierFunc<T> = function(value: array of T): String;

  TPropertyCheckerBuilder<T> = class (TCheckPropertyBuilder<T>)
    fPredFunc: TPredicateFunc<T>;
    fClassifyFunc: TClassifierFunc<T>;
  public
    constructor Create(predicateFunc: TPredicateFunc<T>);
    function Predicate(value: array of T): Boolean; override;
    function Classify(value: array of T): String; override;
    property ClassifierFunction: TClassifierFunc<T> write fClassifyFunc;
  end;


  PropertyChecker<T> = class
    fOptions: TCheckPropertyOptions;
    fBuilder: TPropertyCheckerBuilder<T>;
    public
      constructor ForAll(signatures: TSignatures<T>; predicateFunc: TPredicateFunc<T>);
      destructor Destroy; override;
      function WithClassifier(classifier: TClassifierFunc<T>): PropertyChecker<T>;
      function WithName(name: String): PropertyChecker<T>;
      function WithConfig(options: TCheckPropertyOptions): PropertyChecker<T>;
      function WithNumberOfTrials(count: Integer): PropertyChecker<T>;
      function WithSeed(seed: longint): PropertyChecker<T>; // TODO: This is non-functioning.
      function StopOnFail(value: Boolean): PropertyChecker<T>;
      function Check: TCheckPropertySuite<T>;
  end;

implementation

{ TPropertycheckerbuilder }
constructor TPropertyCheckerBuilder<T>.Create(predicateFunc: TPredicateFunc<T>);
begin
  fPredFunc := predicateFunc;
end;

destructor PropertyChecker<T>.Destroy;
begin
  if fOptions <> Nil then
    fOptions.Free;
  if fBuilder <> Nil then
    fBuilder.Free;
end;

function TPropertyCheckerBuilder<T>.Predicate(value: array of T): Boolean;
begin;
  Result := fPredFunc(value);
end;

function TPropertyCheckerBuilder<T>.Classify(value: array of T): String;
begin
  if @fClassifyFunc <> Nil then
  begin
    Result := fClassifyFunc(value);
  end else begin
    Result := '(not classified)';
  end;
end;

constructor PropertyChecker<T>.ForAll(signatures: TSignatures<T>; predicateFunc: TPredicateFunc<T>);
begin
  fBuilder := TPropertyCheckerBuilder<T>.Create(predicateFunc);
  fBuilder.Signatures := signatures;
  fBuilder.Name := 'PropertyChecker';
  fOptions := TCheckPropertyOptions.Create;
end;

function PropertyChecker<T>.WithClassifier(classifier: TClassifierFunc<T>): PropertyChecker<T>;
begin
  fBuilder.ClassifierFunction := classifier;
  Result := Self;
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

function PropertyChecker<T>.Check: TCheckPropertySuite<T>;
begin
  Result := TCheckPropertySuite<T>.Create(fOptions);
  Result.AddProperty(fBuilder);
  Result.Check;
end;

end.
