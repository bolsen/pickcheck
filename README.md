# PickCheck : QuickCheck for Free Pascal

Note: This is in development, so it doesn't work at all right now.

This is an implementation of QuickCheck for Free Pascal (Object Pascal) loosely based on Douglas Crockford's JSCheck and a fluid interface influenced by jetCheck for Java. It's not complete, so the readme here now has caveats on the API design.

The idea of QuickCheck originated for Haskell in this paper: [https://dl.acm.org/doi/10.1145/351240.351266](https://dl.acm.org/doi/10.1145/351240.351266). It is intended to test programs to see if it follows properties, whether with fixed or random data.

There is a light and verbose interface. The verbose interface is the actual implementation, but it might be quicker to throw around property checks that are a little opinionated (this comes from jetCheck, which is a Java version of QuickCheck.)

The heaviest bit here is whether generics are actually needed. The type you are declaring for the generic classes is the primary input type (but values can be wrapped in composite types.) The predicates center around testing the validity of the inputs.

# Example

Start with some simple examples.

```
try
  runner := specialize PropertyChecker<Integer>.
    ForAll([specialize GenNumber<Integer>(1,1000), specialize GenNumber<Integer>(1,1000)],
           function(value: array of Integer): Boolean
             begin
               Result := value[0] > value[1];
             end).
    Check;
finally
    runner.Free;
end;
```

The `Check` function returns a `TCheckPropertyBuilderSuite` object, which you can then use to inspect the output.

In the example, we want to check two numbers and find any case where the first value is not the same as the second value. It will be quick to see that with random data, it will falsify the statement. This is an output of the run:

```

```

With real software, we want to aim for 100% un-falsifiability. What is nice is that random data can serve towards heuristic testing, where repeated runs will produce diminishing returns in finding bugs.

A more complex example:

```
type
  TPerson = record
    name: String;
    age: String;
   end;

function MakePerson: specialize TCheckFunc<TPerson>;
begin
    Result := function()
    begin
        with Result do
        begin
            name := GenString()();
            age  := GenNumber(1, 120)();
        end;
    end;
end;

try
  runner := specialize PropertyChecker<TPerson>.
    ForAll([MakePerson()],
           function(value: array of TPerson): Boolean
             begin
               Result := value[0].age < 120;
             end).
    Check;
finally
    runner.Free;
end;
```



# Verbose interface

The `PropertyChecker` just spins up all of the things to check one property, outputing
a special sub-class of `TCheckPropertyBuilderSuite`. This class can support more
properties as a suite and more customization, but the `PropertyChecker` should suffice
for most cases and ideally in the future, do everything.

```
type
  TBalanceOrderProperty = class (specialize TCheckPropertyBuilder<Integer>)
  public
   function Predicate(value: array of Integer): Boolean; override;
   function Classify(value: array of Integer): String; override;
  end;

implementation

function TMyCheckPropertyBuilder.Predicate(value: array of TOrder): Boolean;
begin
    if value[0].Side = otBuy then
    begin
      Result :=
      (value[0].FillAmount = value[0].Amount) and (value[1].FillAmount = 0);
  end else if value[0].Side = otSell then
      Result := (value[0].FillAmount = 0) and (value[0].FillAmount = value[0].Amount);

end;

function TMyCheckPropertyBuilder.Classify(value: array of Integer): String;
begin
...
end;

```
