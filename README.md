# packed-printer

A Clojure and Clojurescript library to pretty print data in a packed manner.

*This printer likes rectangular shapes (the wider the better) and despises staircases.*

It's an adaptation of [minimum raggedness](https://en.wikipedia.org/wiki/Line_wrap_and_word_wrap#Minimum_raggedness) (used by TeX for non-justified text) to data.

## Packed printer in one sentence

## Usage

Coordinates `[net.cgrand/packed-printer "0.2.1"]`.

```clj
(require '[net.cgrand.packed-printer :refer [pprint]])
```

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

And in strict mode:

```clj
=> (pprint {:a :b :c {:e :f :g :h :i :j :k :l} :m :n :o {:p {:q :r :s :t}}} :width 30 :strict true)

;        1    1    2    2    3
;...5....0....5....0....5....0
{:a :b, :c {:e :f, :g :h,
            :i :j, :k :l},
 :m :n, :o {:p {:q :r,
                :s :t}}}
```

And in between:

```clj
=> (pprint {:a :b :c {:e :f :g :h :i :j :k :l} :m :n :o {:p {:q :r :s :t}}} :width 30 :strict 1)

;        1    1    2    2    3
;...5....0....5....0....5....0
{:a :b, :c {:e :f, :g :h, :i :j, :k :l},
 :m :n, :o {:p {:q :r, :s :t}}}
```

## Stages

There are three stages: `spans` (which converts data into spans), `layout` (which finds the optimal layout), and `render` (which turns the layout in side effects).

The main stage in `layout` and is fixed. `core/spans` and `core/render` are multimethods and thus can be extended. Dispatch occurs based on the values of options `:to` (the target, defaults is `:text` and `:as` the input format (default `:edn`). `core/spans` dispatches on the pair `[to as]` while `core/render` dispatches only on `to`.

### `core/spans`

It must returns a sequences of spans (see protocol `core/Span`).

### `core/render`

It takes a sequence of lines, a line being a map with keys `:indent` (the amount of whitespace by which to indent the line) and `:spans` a sequence of spans.

When `:to` is `:text`, spans must implement the `text/Text` protocol. 

## Implementation

Complexity is `O(n * w^3)` where n is the length (in spans) of the data to layout and w is the desired width.

The `w^3` may be frightening but it's just an upper bound: dynamic programming is used, giving a cache size of `O(n * w^3)` but in practice this cache is very sparsely populated. Binding `core/*print-stats*` to true causes the actual value for `w^3` to be printed out.

### Minimum raggedness

The usual algorithm found for minimum raggedness is `O(n^2)` and uses dynamic programming.

However if you are laying out things on a grid whose number of columns is low then `O(n*w)` is possible. The key insight is that the cost of a layout depends only of the position in the list of words/spans to layout and the position in the current line -- hence the `n*w`.

### Adaptation for data

To layout data, two parameters are added: current indentation and next line indentation. Both are bounded by `w`. Well, they are bounded by `w` only when `:strict true`.

In relaxed mode, how far in the margin can things be printed is bounded in practice by the cost function. Making the cost function more punitive (increasing strictness) will result in less margin prints.

## License

Copyright © 2017 Christophe Grand

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
