(ns net.cgrand.packed-printer.text.edn
  "Compact pretty printing"
  (:require [net.cgrand.packed-printer.core :as core]))

(def space {:length 1 :text " " :start-length 0 :start-text ""})

(defn opening [s i]
  {:start-length (count s)
   :start-text s
   :length (count s)
   :text s
   :indent i})

(defn closing [s] 
  {:length (count s)
   :text s
   :br-after? true
   :indent -1})

(def delims
  (-> {}
    (into (map (fn [s] [s (opening s (count s))])) ["(" "[" "{" "#{"])
    (into (map (fn [s] [s (closing s)])) [")" "]" "}"])))

(def comma {:length 1
            :text ","
            :br-after? true})

(def kv-close
  {:length 0
   :text ""
   :br-after? true
   :indent -1})

(defn spans
  "Turns x into a collection of spans for layout. Options supported are:
 * kv-indent the amount of spaces by which to indent a value when it appears
   at the start of a line (default 2),
 * coll-indents a map of collection start delimiters (as strings) to the amount
   by which to indent (default: length of the delimiter)."
  [x {:keys [kv-indent coll-indents] :or {kv-indent 2 coll-indents {}}}]
  (let [delims (into delims (map (fn [[s i]] [s (opening s i)])) coll-indents)
        kv-open
        {:length 0
         :text ""
         :start-length 0
         :start-text ""
         :indent kv-indent}]
    (letfn [(coll-spans
              ([x] (coll-spans x [space] spans))
              ([x sp spans]
                (sequence (comp (map spans) (interpose sp) cat) x)))
            (kv-spans [[k v]]
              (-> [kv-open] (into (spans k)) (conj space) (into (spans v)) (conj kv-close)))
            (spans [x]
              (cond
                (tagged-literal? x) (concat [kv-open (str "#" (pr-str (:tag x))) space] (spans (:form x)) [kv-close])
                (vector? x) (concat [(delims "[")] (coll-spans x) [(delims "]")])
                (set? x) (concat [(delims "#{")] (coll-spans x) [(delims "}")])
                (seq? x) (concat [(delims "(")] (coll-spans x) [(delims ")")])
                (map? x) (concat [(delims "{")] (coll-spans x [comma space] kv-spans) [(delims "}")])
                :else [(pr-str x)]))]
    (spans x))))

(defmethod core/spans [:text :edn] [x to-as opts]
  (spans x opts))

