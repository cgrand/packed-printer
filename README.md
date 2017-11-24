# packed-printer

A Clojure library to pretty print data in a packed manner.

It's an adaptation of [minimum raggedness](https://en.wikipedia.org/wiki/Line_wrap_and_word_wrap#Minimum_raggedness) to data.

The cost function is the square of the amount of whitespace (both to the left (indentation) and right (from end of line to desired width) of a line).

In strict mode, the algorithm can't print past the desired width. If no suitable layout is found for the given width an exception is thrown.

In non-strict mode, the algorithm can print beyond the desired width, in such cases the cost function takes into account the numbers of chars written after the limit.

Default width is 70 chars and default mode is non-strict.

## Usage

Relaxed mode:
```clj
=> (pprint (partition 10 (range 50)) :width 15)
;...5....0....5
((0 1 2 3 4
  5 6 7 8 9)
 (10 11 12 13 14
  15 16 17 18 19)
 (20 21 22 23 24
  25 26 27 28 29)
 (30 31 32 33 34
  35 36 37 38 39)
 (40 41 42 43 44
  45 46 47 48 49))
```

Strict mode:
```clj
=> (pprint (partition 10 (range 50)) :width 15 :strict true)
;...5....0....5
((0 1 2 3 4
  5 6 7 8 9)
 (10 11 12
  13 14 15 16
  17 18 19)
 (20 21 22
  23 24 25 26
  27 28 29)
 (30 31 32
  33 34 35 36
  37 38 39)
 (40 41 42
  43 44 45 46
  47 48 49))
```

On code (not intended for that):
```clj
=> (pprint '(defn render [lines]
     (doseq [{:keys [indent] [head & spans] :spans} lines]
       (prsp indent) (prsp (leading-spaces head true)) (print (str head))
       (doseq [span spans]  (prsp (leading-spaces span false)) (print (str span)))
       (newline)))
     :coll-indents {"(" 2})
(defn render [lines]
  (doseq [{:keys [indent],  [head & spans] :spans} lines]
    (prsp indent) (prsp (leading-spaces head true)) (print (str head))
    (doseq [span spans] (prsp (leading-spaces span false))
      (print (str span)))
    (newline)))
```

Values are indented when they can't be set on the same line:

```clj
=> (pprint '{:foo :bar :baz :quux} :width 10)
{:foo :bar,
 :baz :quux}
=> (pprint '{:foo :bar :baz :quux} :width 6)
{:foo
   :bar,
 :baz
   :quux}
```

## Comparison

### clojure.pprint
```clj
=> (binding [clojure.pprint/*print-right-margin* 30]
     (clojure.pprint/pprint {:a :b :c {:e :f :g :h :i :j :k :l} :m :n :o {:p {:q :r :s :t}}}))

;        1    1    2    2    3
;...5....0....5....0....5....0
{:a :b,
 :c
 {:e :f,
  :g :h,
  :i :j,
  :k :l},
 :m :n,
 :o {:p {:q :r, :s :t}}}
```

### zprint
```clj
=> (czprint {:a :b :c {:e :f :g :h :i :j :k :l} :m :n :o {:p {:q :r :s :t}}} 30 {:map {:nl-separator? true}})

;        1    1    2    2    3
;...5....0....5....0....5....0
{:a :b,
 :c {:e :f,
     :g :h,
     :i :j,
     :k :l},
 :m :n,
 :o {:p {:q :r, :s :t}}}
 ```

### packed-printer 
```clj
=> (pprint {:a :b :c {:e :f :g :h :i :j :k :l} :m :n :o {:p {:q :r :s :t}}} :width 30)

;        1    1    2    2    3
;...5....0....5....0....5....0
{:a :b, :c {:e :f, :g :h,
            :i :j, :k :l},
 :m :n, :o {:p {:q :r, :s :t}}}
 ```

## Implementation

### Stages

There are three stages: `spans` (which converts data into spans), `layout` (which finds the optimal layout), and `render` (which turns the layout in side effects).

The main stage in `layout`. TODO: describe its inputs and outputs.

### Complexity

Complexity is `O(n * w^3)` where n is the length (in spans) of the data to layout and w is the desired width.

## License

Copyright © 2017 Christophe Grand

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
