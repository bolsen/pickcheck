{
 PickCheck: Simple QuickCheck in Pascal
 Author: Brian Olsen

 This program is free software; you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation, either version 3 of the License, or
 (at your option) any later version.

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program. If not, see <http://www.gnu.org/licenses/>.


 Commentary:

 This is a Pascal implementation, based off of JSCheck by Douglas Crockford:
 https://github.com/douglascrockford/JSCheck/, which implements a method
 of doing QuickCheck-style checks:
 https://dl.acm.org/doi/pdf/10.1145/351240.351266.

 This is intended to be a simple implementation of QuickCheck.
 The purpose is to integrate this into a test suite by generating random test cases
 and ensuring functions respond to random inputs. Thus, the test cases act as
 testable specifications (specifically working well with design-by-contract methodology.)

 The idea is a different than JSCheck.

 You have specifiers similar to JSCheck. To run a test, you would make a builder (with a Predicate and
 a Classifier then add it to a Suite. The suite builds objects for you.)
}

unit PickCheck;

{$mode delphi}{$H+}{$J-}
{$modeswitch nestedprocvars}
{$modeswitch functionreferences}
{$modeswitch anonymousfunctions}

interface

uses
  cthreads, Classes, SysUtils, StrUtils, Generics.Collections;

type
  EPickCheckError = class(Exception) end;

  TSpecifierGeneratorFunc<T> = reference to function: T;
  TMakeObjectFunc<T> = reference to function: T;
  TRandomFunc = function(value: LongInt): LongInt;
  TPredicateFunc<T> = function(value: array of T): Boolean; // unlike JSCheck, this is passed into an internal function.
  TClassifierFunc<T> = function(value: array of T): String;

  TSignatures<T> = array of TSpecifierGeneratorFunc<T>;
  TCheckPropertyValues<T> = array of T;

  TCheckPropertyOptions = class;

  TCheckProperty<T> = class
    fSerial: TGuid;
    fValues: TCheckPropertyValues<T>;
    fClassification: String;
    fVerdict: Boolean;
  public
    property Serial: TGuid read fSerial write fSerial;
    property Values: TCheckPropertyValues<T> read fValues write fValues;
    property Classification: String read fClassification write fClassification;
    property Verdict: Boolean read fVerdict write fVerdict;
  end;

  TCheckPropertyBuilder<T> = class
  private
    fName: String;
    fSignatures: TSignatures<T>;
  public
    function Predicate(values: array of T): Boolean; virtual;
    function Classify(values: array of T): String; virtual;
    procedure Build(var built: TCheckProperty<T>);
    property Signatures: TSignatures<T> read fSignatures write fSignatures;
    property Name: String read fName write fName;
  end;

  TCheckProperties<T> = class(TObjectList<TCheckProperty<T>>);

  TCheckPropertyReport<T> = class
  private
    fName: String;
    fFails: TCheckProperties<T>;
    fPasses: TCheckProperties<T>;
  public
    constructor Create;
    destructor Destroy; override;
    // TODO: start to use TLists, because saying array of specialize ... in a property falls apart
    procedure AddFail(fail: TCheckProperty<T>);
    function AllFails: TCheckProperties<T>;
    procedure AddPass(pass: TCheckProperty<T>);
    function AllPasses: TCheckProperties<T>;
    property Name: String read fName write fName;
  end;

  TCheckPropertyReporter<T> = class
  protected
    fReport: TCheckPropertyReport<T>;
    fOptions: TCheckPropertyOptions;
  public
    constructor Create(report: TCheckPropertyReport<T>; options: TCheckPropertyOptions);
    procedure DoReport; virtual;
    property Report: TCheckPropertyReport<T> read fReport write fReport;
  end;

  TCheckPropertyConsoleReporter<T> = class(TCheckPropertyReporter<T>)
  public
    procedure DoReport; override;
  end;

  TCheckPropertyReports<T> = array of TCheckPropertyReport<T>;

  TCheckPropertySuite<T> = class
  private
    fProperties: array of TCheckPropertyBuilder<T>;
    fPropCount: Integer;
    fConfig: TCheckPropertyOptions;
    fReport: TCheckPropertyReports<T>;
  public
    constructor Create; overload;
    constructor Create(config: TCheckPropertyOptions); overload;
    destructor Destroy; override;
    procedure AddProperty(prop: TCheckPropertyBuilder<T>);
    procedure Check;

    property Report: TCheckPropertyReports<T> read fReport;
    property Options: TCheckPropertyOptions read fConfig write fConfig;
  end;

  TCheckPropertyOptions = class
  private
    fName: String;
    fNumberOfTrials: Integer;
    fRandomFunc: TRandomFunc;
    fSeed: LongInt;
    fStopOnFail: Boolean;
    procedure SetNumTrials(value: Integer);
    procedure SetRandomFunc(func: TRandomFunc);
  public
    constructor Create;
    property Name: String read fName write fName;
    property NumberOfTrials: Integer read fNumberOfTrials write SetNumTrials default 100;
    property RandomFunc: TRandomFunc read fRandomFunc write SetRandomFunc;
    property Seed: LongInt read fSeed write fSeed;
    property StopOnFirstFail: Boolean read fStopOnFail write fStopOnFail;
  end;

implementation

{ TCheckPropertyBuilder }

function TCheckPropertyBuilder<T>.Predicate(values: array of T): Boolean;
begin
  Result := True;
end;

function TCheckPropertyBuilder<T>.Classify(values: array of T): String;
begin
  Result := '(not classified)';
end;

procedure TCheckPropertyBuilder<T>.Build(var built: TCheckProperty<T>);
var
  sigArgs: array of T;
  i: Integer;
begin
  with built do
  begin
    SetLength(sigArgs, Length(fSignatures));
      // Make random values from the signature.
    for i := low(fSignatures) to high(fSignatures) do
    begin
      sigArgs[i] := fSignatures[i]();
    end;

    // Set all the random values to the checkProperty values
    Values := sigArgs;

    // Get the verdict by passing in the values.
    Verdict := Predicate(Values);

    // Classify the signature data and store it in the object.
    Classification := Classify(sigArgs);

  // Make a serial
    Serial := TGuid.NewGuid;
  end;
end;

{ TCheckPropertyBuilderOptions }
constructor TCheckPropertyOptions.Create;
begin
  fStopOnFail := true;
end;

procedure TCheckPropertyOptions.SetNumTrials(value: Integer);
begin
  fNumberOfTrials := value;
end;

procedure TCheckPropertyOptions.SetRandomFunc(func: TRandomFunc);
begin

end;

{ TCheckPropertySuite }

constructor TCheckPropertySuite<T>.Create; overload;
begin
  fConfig := TCheckPropertyOptions.Create;
  SetLength(fProperties, 255);
  fPropCount := 0;
end;

constructor TCheckPropertySuite<T>.Create(config: TCheckPropertyOptions); overload;
begin
  fConfig := config;
  SetLength(fProperties, 255);
  fPropCount := 0;

end;

destructor TCheckPropertySuite<T>.Destroy;
var
  I: Integer;
begin
  fConfig.Free;
  for i := low(fProperties) to high(fProperties) do
  begin
    if fProperties[i] <> Nil then fProperties[i].Free;
  end;

  for i := low(fReport) to high(fReport) do
  begin
    if fReport[i] <> Nil then fReport[i].Free;
  end;
end;

procedure TCheckPropertySuite<T>.AddProperty(prop: TCheckPropertyBuilder<T>);
begin
  fProperties[fPropCount] := prop;
  Inc(fPropCount);
end;

procedure TCheckPropertySuite<T>.Check;
var
  i, j: Integer;
  built: TCheckProperty<T>;
  trials: Integer;
begin
  SetLength(fReport, fPropCount);

  trials := fConfig.NumberOfTrials;
  for i := 0 to fPropCount - 1 do
  begin
    fReport[i] := TCheckPropertyReport<T>.Create;
    for j := 1 to trials do
    begin
      built := TCheckProperty<T>.Create;
      fProperties[i].Build(built);

      if built.Verdict then
      begin
        fReport[i].AddPass(built);
      end else begin
        fReport[i].AddFail(built);
        if fConfig.StopOnFirstFail then
        begin
          Exit;
        end;
      end;
    end;
  end;
end;

{ TCheckPropertyReport }
constructor TCheckPropertyReport<T>.Create;
begin
  fFails := TCheckProperties<T>.Create;
  fPasses := TCheckProperties<T>.Create;
end;

destructor TCheckPropertyReport<T>.Destroy;
begin
  fPasses.Free;
  fFails.Free;
end;

procedure TCheckPropertyReport<T>.AddFail(fail: TCheckProperty<T>);
begin
  fFails.Add(fail);
end;

function TCheckPropertyReport<T>.AllFails: TCheckProperties<T>;
begin
    Result := fFails;
end;

procedure TCheckPropertyReport<T>.AddPass(pass: TCheckProperty<T>);
begin
  fPasses.Add(pass);
end;

function TCheckPropertyReport<T>.AllPasses: TCheckProperties<T>;
begin
  Result := fPasses;
end;

{ TCheckPropertyReporter }
constructor TCheckPropertyReporter<T>.Create(report: TCheckPropertyReport<T>; options: TCheckPropertyOptions);
begin
  fReport := report;
  fOptions := options;
end;

procedure TCheckPropertyReporter<T>.DoReport;
begin
  WriteLn('(not implemented)');
end;


{ TCheckPropertyReporter }
procedure TCheckPropertyConsoleReporter<T>.DoReport;
var
  i : Integer;
  failingReport: TCheckProperty<T>;
begin
  if fReport.AllPasses.Count = fOptions.NumberOfTrials then
  begin
    WriteLn(fOptions.Name, ': All tests passed!');
  end else if fOptions.StopOnFirstFail then
  begin
    WriteLn(Format('%s: Falsification after %d tests.',
                   [
                     fOptions.Name,
                     fReport.AllFails.Count + fReport.AllPasses.Count
                   ]));
    failingReport := fReport.AllFails.Last;
    WriteLn('Failing input was : ');
    for i := low(failingReport.Values) to high(failingReport.Values) do
    begin
      WriteLn(failingReport.Values[i]);
    end;
    WriteLn('');
  end else begin
    WriteLn('For ', fReport.Name);
    WriteLn('Pass: ', fReport.AllPasses.Count);
    WriteLn('Fail: ', fReport.AllFails.Count);
  end;

end;


end.
