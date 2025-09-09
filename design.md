# Design

1. `TCheckPropertyBuilderSuite` has
 - A list of properties array of `TCheckPropertyBuilder`
 - A configuration:  `TCheckPropertyOptions`
 - And a list of reports for each property ran: array of `TCheckPropertyReport`.
 - It `Check`s all built `TCheckProperty`.

2. `TCheckPropertyBuilder`
 - This stores the bits for a single property. This would be subclassed.
 - This builds `TCheckProperty` values.
 - You supply a `Predicate`
 - You classify the output with `Classify`
 - It can have it's own `Name`
 - It has `Signatures` which are an array of functions that return data.
 - `Signatures` are made available in a `Predicate` and a `Classify`.

 This is subclassable to define different predicates and ways to show the result.

3. `TCheckProperty`
 - It can have a `Serial` value
 - It will have reified values ran from the `Signature` in the builder.
 - It classifies the values.
 - It contains a result, called a `Verdict`.

4. `TCheckPropertyOptions`
 - When creating a `TCheckPropertyBuilderSuite`, you give it or it will create an object
   of this type.
 - It stores a Name, NumberOfTrials for a suite, the Random Seed, if it should StopOnFirstFail.
 - This is passed to a reporter object and also drives the checker.

5. `TCheckPropertyReport`, Each suite `Check` results in a  `TCheckPropertyReport` :
 - It contains `AllFails` and `AllPasses`. It has the `Name` of the run.

6. `TCheckPropertyReporter`:
 - This is subclassable way to report to an output (console, screen).
 - It takes `TCheckPropertyOptions` and a `TCheckPropertyReport`.
 - It takes the information and shows a report to the user. There is a Console version of this.
