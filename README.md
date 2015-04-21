# pr-foobar

`pr-foobar` is a collection of print debugging macros combining the
functionality of [AlexBaranosky/print-foo](https://github.com/AlexBaranosky/print-foo) with
[cldwalker/table](https://github.com/cldwalker/table).

The following macros are provided: 

`pr->` `pr->>` `pr-let` `pr-when-let` `pr-if-let` `pr-if` `pr-if-not` `pr-when`
`pr-when-not` `pr-cond` `pr-and` `pr-or`

## Example

```clojure
user> (pr-let [a (map inc (range 5))
               b (filter even? a)
               c (reduce + b)]
        [a b c])
┌────────┬───────────────────────┐
│ let    │ value                 │
├────────┼───────────────────────┤
│ a      ╎ (1 2 3 4 5)           │
│ b      ╎ (2 4)                 │
│ c      ╎ 6                     │
│ RESULT ╎ [(1 2 3 4 5) (2 4) 6] │
└────────┴───────────────────────┘
[(1 2 3 4 5) (2 4) 6]
user>
```

Exceptions are handled gracefully, so using `pr-foobar` is a fast way to get an
idea why your code is failing:

```clojure
user> (pr->> (range 5) rest first dec (/ 1) str)
┌──────────────────────────────────┬───────────────────────────────────────────────────────────────┐
│ ->> clojure.lang.LazySeq@1b554e1 │ step                                                          │
├──────────────────────────────────┼───────────────────────────────────────────────────────────────┤
│ rest                             ╎ (1 2 3 4)                                                     │
│ first                            ╎ 1                                                             │
│ dec                              ╎ 0                                                             │
│ (/ 1)                            ╎ #<ArithmeticException java.lang.ArithmeticException: Divid... │
└──────────────────────────────────┴───────────────────────────────────────────────────────────────┘
ArithmeticException Divide by zero  clojure.lang.Numbers.divide (Numbers.java:156)
```

## Installation and Usage

Add `[pr-foobar "0.1.0"]` as a dependency. You may wish to add it to your
`~/.lein/profiles.clj` or `~/.profile.boot` files to have it available with all
of your projects.

To use it:

```clojure
(use 'pr.foobar)
```

Or you might consider using a tool like [vinyasa](https://github.com/zcaudate/vinyasa) 
to have it required and available in all of your namespaces.

## Limitations

You should be careful about using `pr-foobar` with side-effectful code, as the
results of an expression may be evaluated multiple times in the macro call.

Doesn't play well with infinite lazy sequences.

## License

Copyright © Olli Piepponen 2015

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
