# TODO

1. [ ] Cannot use classifiers yet in output
2. [ ] Refactor types into separate units to make it easier to navigate
3. [ ] Write a test suite
4. [ ] Maybe refactor specifiers? Primarily something where they can print themselves?
5. [ ] Check heap usage/memory leaks
6. [ ] Reporters should just use report objects. It doesn't need options
7. [ ] Is the generic interface adding value? Every class that generics touches must be generic too only to serve fields that are. The T refers to the basic input type for signatures. You can use `Variant` as a type
8. [ ] Improve the randomizer
9. [ ] Serialize reports as test cases
10. [ ] Fix the Build(built) function to have building happen in Build;
11. [ ] Use TLists instead of arrays to simplify list code
12. [ ] We don't use Serial values for TCheckProperty
13. [ ] write more specifiers
14. [ ] collect common types across units into one unit (e.g. specifiers can stand on its own, just needs to share type info. it can go into an .inc?)
15. [ ] Make a GUI reporter (like TestInsight, integrate into Lazarus?)
16. [X] Put it in Delphi mode?
17. [ ] handle asynchronous tests?
18. [ ] Priority: Reporter should calculate statistics to make available in test suites
19. [ ] IDEA: Create instrumentation to serialize data for testing. Like, publish out to some endpoint to continually test property cases from a live system
