# weaving

*Weaving is to lambdas what threading is to s-expressions.*

This library's purpose is to provide a set of simple function combinators to ease out the day-to-day wrangling of Clojure code.

Just like **threading macros** in Clojure end with an arrow, **weaving functions** end with `|`.

## Usage

```clojure
[net.clojars.unexpectedness/weaving "0.2.5"]
```

```clojure
(require '[weaving.core :refer :all])
```

Supports ClojureScript.

## Showcase

```clojure
(def f
  (->| (when| number? (| * 2))
       (if| (or| string? symbol?)
         (|| str "The string ")
         (->| str
              (tap| (|| println "Warning: not a string:"))))
        vector
        (apply| (| clojure.string/replace "0" "X"))))

(f 100)
;; Warning: not a string: 200
;; => "2XX"

(f "slime")
;; => "The string slime"
```

## [API doc](https://unexpectedness.github.io/weaving/index.html)

## Rationale

- `->|` works like comp, but in reverse order, the same as `->`.
- Conditionals, namely `if|` and `when|`, will return the passed argument rather than nil if the predicate does not hold. `((when| number? inc) "a")` will return `"a"`, not `nil`.
- `<-|` is an alias for constantly.
- Partialization can happen in two places.
  - `||`: at the end of the arglist, just like partial or `->>`.
    - `(|| str "ABC")` produces strings like `"ABC_"`
  - `|` : at the beginning, mirroring `->`.
    - `(| str "ABC")` produces strings like `"_ABC"`.
- Be mindful of the fact the arguments to these combinators will be evaluated before being passed to the combinators. `(when| *my-pred* (| do-something a))` will capture the value of `*my-pred*` and `a`. There is no way to change them after the fact.

## List of combinators

- equality: `=|`. `(=| 2)` is equivalent to `#(= % 2)`.
- boolean operators: `or|`, `and|` & `not|`.
- conditionals: `if|` and `when|`.
- partialization: `|` & `||`.
- `<-|` : works like `constantly`.
- `->|`: works like `comp` but in a reverse, sane order (the same as `->`).
- function arguments weaving: `args|`. `((args| + inc dec)  2)` yields `4` (`(+ (inc 2) (dec 2))`). Only works on single arg functions.
- `each-arg|`. Like `args|`, but applies the same function to all the arguments. `((each-arg| + inc) 1 2) ;; => 5`. Works on multi args functions.
- `doto|`. `(doto| inc debug-print)`
- `*|`: works like `juxt`.
- `juxtm|`: works like `juxt` but accepts kw-args and returns a map.
- `apply|` & `unapply|`.
- `call`. Useful for `(condp call 1 number? :ok) ;; => :ok`.
- `in|`. To apply a function deeply into a datastructure. `((in| [1 1 0] inc) [0 [0 [0]]]) ;; => [0 [0 [1]]]`. Unlike `update-in`, works on lists, not just vectors and map.
- unpartialization: `ø|` to remove the first argument and `ø||` to remove the last one.

## %|

"Improves" upon #(.. %) by allowing naming of arguments and deep access:

```clojure
;; Works like #(... %)
(= ((%| str %1 %2)        'a 'b)       "ab")
(= ((%| str %  %2)        'a 'b)       "ab")
(= ((%| apply str %&)     'a 'b)       "ab")

;; Can name args
(= ((%| str %1a %2b)      'a 'b)       "ab")
(= ((%| apply str %&ab)   'a 'b)       "ab")

;; And refer to these named args later
(= ((%| str %1a %a %1)    'a 'b)       "aaa")

;; Accepts args with no num, only a name. Indices are deduced incrementally.
(= ((%| str %a %b)        'a 'b)       "ab")
(= ((%| str %2b %c %1 %d) 'a 'b 'c 'd) "bcad")
(= ((%| apply str %&abcd) 'a 'b 'c 'd) "abcd")

;; Deep access.
;; Works like get-in. Each access symbol must follow this grammar:
;;   access-sym  => %access-sym+
;;   access-sym+ => key:access-sym+ || key
;;   key         => num || numname || name

;; To stay consistent with the way `#(... %)` indexes arguments, and unlike get-in,
;; indices start at 1, no matter the depth.
(= ((%| str %:1)  '[a])   "a")

;; Works with maps
(= ((%| str %:a)  '{:a a}) "a")

;; And lists
(= ((%| str %:1)  '(a))    "a")

;; Nice !
(= ((%| str %:1 %other-arg:2opts:value)  '[a] '[b {:value c}])
   "ac")
```

## License

Copyright © 2024 unexpectedness

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
