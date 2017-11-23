(ns net.cgrand.packed-printer
  "Compact pretty printing"
  (:require [clojure.string :as str]))

(defrecord Span [s indentation nesting first-indent leading-spaces]
  Object
  (toString [_] s))

(defn indentation [span]
  (if (string? span) 0 (:indentation span)))

(defn nesting [span]
  (if (string? span) 0 (:nesting span)))

(defn first-indent [span]
  (if (string? span) 0 (:first-indent span)))

(defn leading-spaces [span first?]
  (cond first?
    (first-indent span)
    (string? span) 1
    :else (:leading-spaces span)))

(defn nest
  ([span dn] (nest span dn dn 0))
  ([span dn di] (nest span dn di 0))
  ([span dn di df]
    (let [n (+ (nesting span) dn)
          i (+ (indentation span) di)
          f (+ (first-indent span) df)
          sp (leading-spaces span false)]
      (if (and (zero? n) (zero? i) (zero? f) (= 1 sp))
        (str span)
        (Span. (str span) i n f sp)))))

(defn trailer [span]
  (if (string? span)
    (Span. span 0 0 ##Inf 0)
    (assoc span :first-indent ##Inf :leading-spaces 0)))

(defn length
  ([span] (length span false))
  ([span first?]
    (assert (some? span))
    (+ (leading-spaces span first?) (count (str span)))))

(defn- trail-only? [span]
  (= ##Inf (leading-spaces span true)))

(defn- opening [s indent]
  (nest s 1 indent))

(defn- closing [s indent]
  (trailer (nest s -1 (- indent))))

(def ^:private comma (trailer ","))

(defn spans
  "Turns x into a vector of spans for layout. Options supported are:
 * kv-indent the amount of spaces by which to indent a value when it appears
   at the start of a line (default 2),
 * coll-indents a map of collection start delimiters (as strings) to the amount
   by which to indent (default: length of the delimiter)."
  [x {:keys [kv-indent coll-indents] :or {kv-indent 2 coll-indents {}}}]
  (letfn [(coll-spans [x open close]
            (if (seq x)
              (let [indent (coll-indents open (count open))]
                (-> [(opening open indent)] (into (mapcat spans) x) (conj (closing close indent))
                  (update 1 trailer)))
              [(str open close)]))
          (kv-spans [[k v]]
            (let [sps (-> (spans k) (update 0 nest 1 0))
                  n (count sps)
                  sps (into sps (spans v))]
              (-> sps
                (update n nest 0 0 kv-indent)
                (update (dec (count sps)) nest -1 0))))
          (spans [x]
            (cond
              (vector? x) (coll-spans x "[" "]")
              (set? x) (coll-spans x "#{" "}")
              (seq? x) (coll-spans x "(" ")")
              (map? x)
              (if (seq x)
                (let [indent (coll-indents "{" 1)
                      kvs (into [] x)]
                  (-> [(opening "{" indent)] 
                    (into (mapcat (fn [kv] (conj (kv-spans kv) comma))) (pop kvs))
                    (into (kv-spans (peek kvs)))
                    (conj (closing "}" indent))
                    (update 1 trailer)))
                ["{}"])
            :else [(pr-str x)]))]
    (spans x)))

(def ^:private empty-layout {:cost 0 :line () :lines ()})
(def ^:private impossible-layout {:cost ##Inf :line () :lines ()})

(defn- split-block [spans n]
  ; a block can either be layout on one line or more lines (it doesn't mean that it is all vertical)
  ; if a block uses more than one line then it must be followed by a br
  (loop [spans spans block [] n n]
    (if (pos? n)
      (let [[span & spans] (seq spans)] ; invariant: n >= 0 => spans not empty
        (recur spans (conj block span) (+ n (nesting span))))
      [block spans])))

(defn layout
  ([spans full-width] (layout spans full-width false))
  ([spans full-width strict]
    (let [cache (atom {})]
      (letfn [(best-layout [& args]
                (or (@cache args)
                  (doto (apply raw-best-layout args)
                    (->> (swap! cache assoc args)))))
              (raw-best-layout [spans w w' pos {:keys [cost line lines] :as layout}]
                (if (and strict (<= w 0))
                  impossible-layout
                  (min-key #(estimate w pos %)
                    (if (pos? pos) (br w' (best-layout spans w' w' 0 layout)) impossible-layout)
                    (if-some [[span & spans] spans]
                      (let [end-pos (+ pos (length span (zero? pos)))]
                        (cond
                          (if strict
                            (< w end-pos)
                            (and (pos? pos) (<= w pos) (not (trail-only? span)))) impossible-layout
                      
                          (pos? (nesting span)) ; is block head
                          (layout-block span spans w w' pos)
                    
                        :else ; not a block head
                        (cat span (best-layout spans w  w' end-pos layout))))
                      layout))))
              (layout-block [block-head spans w bottom-w pos]
                (let [w' (- w pos (leading-spaces block-head (zero? pos)) (indentation block-head))
                      [block-tail bottom] (split-block spans (nesting block-head))
                      head-end-pos (+ pos (length block-head (zero? pos)))
                      block-layout (cat block-head (best-layout block-tail w w' head-end-pos empty-layout))]
                  (if (seq (:lines block-layout))
                    ; multiline
                    (let [block-lines (into [] (:lines block-layout))
                          {last-spans :spans last-indent :indent} (peek block-lines)
                          last-line-pos (transduce (map length) + (count (str (first last-spans))) (rest last-spans))
                          bottom-layout (best-layout bottom bottom-w bottom-w last-line-pos empty-layout)
                          last-width (- full-width last-indent)
                          layout (br last-width (assoc bottom-layout :line (concat last-spans (:line bottom-layout))))
                          layout (reduce (fn [layout {:keys [spans indent] :as line}]
                                           (-> layout
                                             (update :cost + (line-cost (- full-width indent) 0 spans))
                                             (assoc :lines (cons line (:lines layout)))))
                                   layout (rseq (pop block-lines)))]
                      (assoc layout :line (:line block-layout)))
                    ; single line
                    (let [line-end-pos (transduce (map length) + head-end-pos (rest (:line block-layout)))
                          bottom-layout (best-layout bottom w bottom-w line-end-pos empty-layout)]
                      (assoc bottom-layout
                        :cost (max (:cost block-layout) (:cost bottom-layout))
                        :line (concat (:line block-layout) (:line bottom-layout)))))))
             (estimate [width pos {:keys [line cost]}]
               (+ cost (line-cost width pos line)))
             (line-cost [width pos line]
               (if (or (seq line) (pos? pos))
                 (let [rem (Math/abs
                             (if-some [[span & spans] (seq line)]
                               (transduce (map length) - (- width pos (length span (zero? pos))) spans) ; because of the abs, 1-arity of - is not a pb
                               (- width pos)))
                       left-spaces (+ (- full-width width) (if (pos? 0) (leading-spaces (first line) true) 0))]
                   (Math/pow (+ rem left-spaces) 2))
                 0))
             (cat [span layout]
               (assoc layout :line (cons span (:line layout))))
             (br [width {:keys [line lines] :as layout}]
               (if (= () line)
                 layout
                 (let [indent (- full-width width)]
                   {:cost (estimate width 0 layout)
                    :line ()
                    :lines (cons {:spans line :indent (- full-width width)} lines)})))]
       (let [layout (best-layout spans full-width full-width 0 empty-layout)]
         (when-not (= ##Inf (:cost layout))
           (:lines (br full-width layout))))))))

(defn- prsp [^long n]
  (case n
    0 nil
    1 (print " ")
    2 (print "  ")
    3 (print "   ")
    4 (print "    ")
    (print (apply str (repeat n " ")))))

(defn render [lines]
  (doseq [{:keys [indent] [head & spans] :spans} lines]
    (prsp indent) (prsp (leading-spaces head true)) (print (str head))
    (doseq [span spans]  (prsp (leading-spaces span false)) (print (str span)))
    (newline)))

(defn pprint
  "Packed prints x, supported options are those of #'spans and:
 * :width the desired output width (in chars)
 * :strict, a boolean, true when the desired width is a hard limit; pprint may throw when no suitable ayout is found."
  [x & {:keys [strict width] :or {strict false width 70} :as opts}]
  (if-some [layout (layout (spans x opts) width strict)]
    (render layout)
    (throw (ex-info "Can't find a suitable layout. See ex-data." {:data x :opts opts}))))
