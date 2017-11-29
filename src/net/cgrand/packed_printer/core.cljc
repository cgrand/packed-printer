(ns net.cgrand.packed-printer.core
  "Layout engine.")

(defmulti spans (fn [x to+as opts] to+as))

(defmulti render (fn [x to opts] to))

(defprotocol Span
  (length [span start-of-line] 
    "Returns the length of the span (depending on whether it
appears at the start of a line) or nil if not acceptable.")
  (br-after? [span] "Returns whether a line break authorized after this span.")
  (indent [span]
    "Returns:
* nil for a regular span,
* a natural integer (so zero included) to indicate the start of a group and
  the amount by which to indent next lines of this group,
* any negative number to indicate the end of a group."))

(defn block-head? [span]
  (some-> (indent span) (>= 0)))

(defn trailer?
  "Returns true for span that can't appear at the start of a line."
  [span]
  (nil? (length span true)))

(extend-protocol Span
  #?(:clj String :cljs string)
  (length [s _] (count s))
  (br-after? [s] true)
  (indent [s] nil)
  #?(:clj clojure.lang.APersistentMap :cljs cljs.core/PersistentHashMap)
  (length [s start-of-line] (if start-of-line (:start-length s) (:length s)))
  (br-after? [s] (:br-after? s))
  (indent [s] (:indent s))
  #?@(:cljs
       [cljs.core/PersistentArrayMap
        (length [s start-of-line] (if start-of-line (:start-length s) (:length s)))
        (br-after? [s] (:br-after? s))
        (indent [s] (:indent s))]))

(def ^:private empty-layout {:cost 0 :line () :lines ()})

(defn- split-block
  "Returns a pair [block+trail bottom] where block-trail is the block and its trail
   (spans that must appear right after it) and bottom the remaining spans."
  [spans from to]
  (loop [from (inc from) n 1]
    (let [span (nth spans from nil)]
      (if (pos? n)
        (recur (inc from) (+ n (if-some [i (indent span)] (if (neg? i) -1 1) 0))) ; invariant: n >= 0 => from < to
        (if (< from to)
          (cond
            (length span true) from ; not a trailing span: it can appear at the start of a line
            (block-head? span) (recur (inc from) 1)
            :else (recur (inc from) n))
          from)))))

(defn- line-pos [pos spans]
  (reduce (fn [pos span] (+ pos (length span (zero? pos)))) pos spans))

(def ^:dynamic *print-stats* false)

(defn layout
  ([spans full-width] (layout spans full-width false))
  ([spans full-width strict]
    (let [relax (not (true? strict))
          penalty (if (number? strict) strict 2)
          cache (atom {})
          pending (atom [])
          spans (vec spans)]
      (when-not (pos? penalty)
        (throw (ex-info "Strictness can't be zero or negative." {:strict strict})))
      (letfn [(safe-best-layout [& args] ; kind of a trampoline against SO
                (loop [todo [args]]
                  (when-some [args (peek todo)]
                    (recur (try
                             (apply best-layout args)
                             (pop todo)
                             (catch #?(:clj StackOverflowError :cljs :default) e
                               (let [todo (into todo @pending)]
                                 (reset! pending [])
                                 todo))))))
                (apply best-layout args))
              (best-layout [& args]
                (or (@cache args)
                  (let [_ (swap! pending conj args)
                        r (apply raw-best-layout args)]
                    (swap! cache assoc args r)
                    (swap! pending pop)
                    r)))
              (raw-best-layout [from to i i' pos may-br]
                (when (or relax (<= i full-width))
                  (if (< from to)
                    (let [span (nth spans from)
                          ; the marginal cost is extra cost of adding the span to the current line and past the margin (sic)
                          marginal-cost (when-some [end-pos (some-> (length span (= i pos)) (+ pos))]
                                          (when (< full-width end-pos)
                                            (- (line-cost i end-pos nil) (line-cost i pos nil))))
                          br-layout (when (and may-br (< i pos))
                                      (br i' (raw-best-layout from to i' i' i' false)))
                          inline-layout (when-not (and br-layout marginal-cost (< (:cost br-layout) marginal-cost))
                                          ; avoid computing inline layout when it's bound to be more expensive
                                          (if (block-head? span)
                                            (layout-block from to i i' pos)
                                            (layout-regular from to i i' pos)))]
                      ; inline before br in preference order
                      (min-cost i pos inline-layout br-layout))
                    empty-layout)))
              (layout-regular [from to i i' pos]
                (let [span (nth spans from)]
                  (when-some [n (length span (= i pos))]
                    (let [end-pos (+ pos n)]
                      (when (or relax (<= end-pos full-width))
                        (cat span (best-layout (inc from) to i i' end-pos (br-after? span))))))))
              (layout-block [from to i bottom-i pos]
                (let [block-head (nth spans from)
                      i' (+ pos (indent block-head))
                      block-to (split-block spans from to)]
                  (when-some [{:keys [lines line cost] :as block-layout} (layout-regular from block-to i i' pos)]
                    (if (seq lines)
                      ; multiline
                      (when-some [bottom-layout (br bottom-i (best-layout block-to to bottom-i bottom-i bottom-i false))]
                        {:cost (+ cost (:cost bottom-layout))
                         :line line
                         :lines (concat lines (:lines bottom-layout))})
                      ; single line
                      (let [pos (line-pos pos line)
                            bottom-layout (best-layout block-to to i bottom-i pos true)]
                        (assoc bottom-layout :line (concat line (:line bottom-layout))))))))
              (min-cost [i pos a b]
                (cond
                  (nil? a) b
                  (nil? b) a
                  (<= (estimate i pos a) (estimate i pos b)) a
                  :else b))
             (estimate [indent pos {:keys [line cost]}]
               (+ cost (line-cost indent pos line)))
             (line-cost
               ([{:keys [spans indent]}] (line-cost indent indent spans))
               ([indent pos spans]
                 (if (or (seq spans) (< indent pos))
                   (let [left (min indent full-width)
                         right (- (+ indent (line-pos (- pos indent) spans)) full-width)]
                     (+ (* left left) (* right right (if (pos? right) penalty 1))))
                   0)))
             (cat [span layout]
               (some-> layout (assoc :line (cons span (:line layout)))))
             (br [indent {:keys [line lines] :as layout}]
               (if (-> layout :line seq)
                 {:cost (estimate indent indent layout)
                  :line ()
                  :lines (cons {:spans line :indent indent} lines)}
                 layout))]
        (when *print-stats*
          (safe-best-layout 0 (count spans) 0 0 0 false) ; populate cache
          (println "; in practice" (str full-width "^3") "is" (/ (double (count @cache)) (count spans)) "and not" (Math/pow full-width 3)))
        (some->> (safe-best-layout 0 (count spans) 0 0 0 false) (br 0) :lines)))))
