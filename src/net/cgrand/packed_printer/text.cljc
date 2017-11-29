(ns net.cgrand.packed-printer.text
  "Compact pretty printing"
  (:require [net.cgrand.packed-printer.core :as core]))

(defprotocol Text
  (text [span start-of-line]))

(extend-protocol Text
  #?(:clj String :cljs string)
  (text [s _] s)
  #?(:clj clojure.lang.APersistentMap :cljs cljs.core/PersistentHashMap)
  (text [m start-of-line] (if start-of-line (:start-text m) (:text m)))
  #?@(:cljs [cljs.core/PersistentArrayMap
             (text [m start-of-line] (if start-of-line (:start-text m) (:text m)))]))

(def ^:dynamic *ruler-width* nil)

(defn print-ruler
  ([]
    (when (number? *ruler-width*)
      (print-ruler *ruler-width*)))
  ([n]
    (let [n (long n)
          digits (pr-str n)]
      (when (<= 2 n)
        (reduce
          (fn [stride digit]
            (println (apply str (concat (->> (if (< 1 stride)
                                             (->> "1234567890" cycle
                                               (mapcat (fn [d] (concat (repeat (dec stride) " ") [d]))))
                                             (->> "....5....0" cycle))
                                          (drop 1) (cons \;) (take (dec n)))
                                [digit])))
            (/ stride 10))
          (Math/pow 10 (dec (count digits))) digits)))))

(defn render [lines]
  (print-ruler)
  (doseq [{:keys [spans indent]} lines]
    (print (apply str (repeat indent " ")))
    (reduce (fn [n span]
              (print (text span (zero? n)))
              (+ n (core/length span (zero? n))))
      0 spans)
    (newline)))

(defmethod core/render :text [lines to opts]
  (render lines))
