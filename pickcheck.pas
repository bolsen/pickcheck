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

 Examples:
 type TLEClaim = class(TClaim<Integer>)
   function Predicate(value): Boolean; override;
 end;

 function LE(a: Integer; b: Integer): Boolean;
 begin
    Result := a <= b;
 end;

 function TLEClaim.Predicate(value): Boolean;
 begin
   Result := Self.Verdict(LE(value)); // this looks wrong
 end;

 runner := TClaimRunner.Create;
 claim := TLEClaim.Create('Less than', [GenInteger(10), GenInteger(20)])
 runner.Add(claim);
 runner.Check();
}

unit PickCheck;

{$mode objfpc}{$H+}{$J-}
{$modeswitch nestedprocvars}
{$modeswitch functionreferences}
{$modeswitch anonymousfunctions}

interface

uses
  cthreads, Classes, SysUtils, StrUtils, Generics.Collections;

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

  // {name}: {class}{cases} cases tested, {pass} pass{fail}{lost}\n
  TestFormatString = '%s: %s %n cases tested, %n pass %n %s\n';

type
  EPickCheckError = class(Exception) end;

  generic TCheckFunc<T> = reference to function: T;
  generic TMakeObjectFunc<T> = reference to function: T;
  TRandomFunc = function(value: LongInt): LongInt;
  generic TPredicateFunc<T> = function(value: T): Boolean; // unlike JSCheck, this is passed into an internal function.
  generic TSignatures<T> = array of specialize TCheckFunc<T>;

  TCheckCase = class
  private
    fNumberOfTrials: Integer;
    fRandomFunc: TRandomFunc;
    procedure SetNumTrials(value: Integer);
    procedure SetRandomFunc(func: TRandomFunc);
  public
    property NumberOfTrials: Integer read fNumberOfTrials write SetNumTrials;
    property RandomFunc: TRandomFunc read fRandomFunc write SetRandomFunc;
  end;

  generic TClaim<T> = class
  private
    fName: String;
    fSignatures: array of specialize TSignatures<T>;
  protected
    procedure Verdict(outcome: Boolean);
  public
    function Predicate(values: array of T): Boolean; virtual;
    property Name: String read fName;
  end;

  generic TCheckRunner<T> = class
  private
    fAllClaims: array of specialize TClaim<T>;
    fConfig: TCheckCase;
  public
    constructor Create(config: TCheckCase);
    destructor Destroy; override;
    procedure AddClaim(claim: specialize TClaim<T>);
    procedure Check;
//    generic procedure Register<T>(Serial: Integer; Value: T);
    procedure Claim(name: String; predicate: specialize TPredicateFunc<T>; signature: specialize TSignatures<T>; classifier: String = '');
  end;


implementation

{ TClaim }
procedure TClaim.Verdict(outcome: Boolean); begin end;
function TClaim.Predicate(values: array of T): Boolean; begin end;b

{ TCheckCase }
procedure TCheckCase.SetNumTrials(value: Integer);
begin end;
procedure TCheckCase.SetRandomFunc(func: TRandomFunc);
begin end;

{ TCheckRunner }
constructor TCheckRunner.Create(config: TCheckCase);
begin
  fConfig := config;
end;

destructor TCheckRunner.Destroy;
begin
  fConfig.Free;
end;

procedure TCheckRunner.AddClaim(claim: specialize TClaim<T>);
begin
end;

procedure TCheckRunner.Check;
begin

end;

procedure TCheckRunner.Claim(name: String; predicate: specialize TPredicateFunc<T>; signature: specialize TSignatures<T>; classifier: String = '');
begin
end;



End.
