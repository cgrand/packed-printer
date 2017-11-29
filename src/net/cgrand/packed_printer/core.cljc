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
  [spans]
  (loop [spans spans block+trail [] n 1]
    (if (pos? n)
      (let [[span & spans] (seq spans)] ; invariant: n >= 0 => spans not empty
        (recur spans (conj block+trail span) (+ n (if-some [i (indent span)] (if (neg? i) -1 1) 0))))
      (if-some [[span & spans :as all-spans] (seq spans)]
        (cond
          (length span true) ; not a trailing span: it can appear at the start of a line
          [block+trail all-spans]
          (block-head? span)
          (recur spans (conj block+trail span) 1)
          :else (recur spans (conj block+trail span) n))
        [block+trail spans]))))

(defn- line-pos [pos spans]
  (reduce (fn [pos span] (+ pos (length span (zero? pos)))) pos spans))

(def ^:dynamic *print-stats* false)

(defn layout
  ([spans full-width] (layout spans full-width false))
  ([spans full-width strict]
    (let [relax (not (true? strict))
          penalty (if (number? strict) strict 2)
          cache (atom {})
          pending (atom [])]
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
              (raw-best-layout [spans i i' pos may-br]
                (when (or relax (<= i full-width))
                  (if-some [[span & spans :as all-spans] (seq spans)]
                    (let [; the marginal cost is extra cost of adding the span to the current line and past the margin (sic)
                          marginal-cost (when-some [end-pos (some-> (length span (= i pos)) (+ pos))]
                                          (when (< full-width end-pos)
                                            (- (line-cost i end-pos nil) (line-cost i pos nil))))
                          br-layout (when (and may-br (< i pos))
                                      (br i' (raw-best-layout all-spans i' i' i' false)))
                          inline-layout (when-not (and br-layout marginal-cost (< (:cost br-layout) marginal-cost))
                                          ; avoid computing inline layout when it's bound to be more expensive
                                          (if (block-head? span)
                                            (layout-block span spans i i' pos)
                                            (layout-regular span spans i i' pos)))]
                      ; inline before br in preference order
                      (min-cost i pos inline-layout br-layout))
                    empty-layout)))
              (layout-regular [span spans i i' pos]
                (when-some [n (length span (= i pos))]
                  (let [end-pos (+ pos n)]
                    (when (or relax (<= end-pos full-width))
                      (cat span (best-layout spans i i' end-pos (br-after? span)))))))
              (layout-block [block-head spans i bottom-i pos]
                (let [i' (+ pos (indent block-head))
                      [block-tail bottom] (split-block spans)]
                  (when-some [{:keys [lines line cost] :as block-layout} (layout-regular block-head block-tail i i' pos)]
                    (if (seq lines)
                      ; multiline
                      (when-some [bottom-layout (br bottom-i (best-layout bottom bottom-i bottom-i bottom-i false))]
                        {:cost (+ cost (:cost bottom-layout))
                         :line line
                         :lines (concat lines (:lines bottom-layout))})
                      ; single line
                      (let [pos (line-pos pos line)
                            bottom-layout (best-layout bottom i bottom-i pos true)]
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
          (safe-best-layout spans 0 0 0 false) ; populate cache
          (println "; in practice" (str full-width "^3") "is" (/ (double (count @cache)) (count spans)) "and not" (Math/pow full-width 3)))
        (some->> (safe-best-layout spans 0 0 0 false) (br 0) :lines)))))
