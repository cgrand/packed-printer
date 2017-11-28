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

(defn render [lines]
  (doseq [{:keys [spans indent]} lines]
    (print (apply str (repeat indent " ")))
    (reduce (fn [n span]
              (print (text span (zero? n)))
              (+ n (core/length span (zero? n))))
      0 spans)
    (newline)))

(defmethod core/render :text [lines to opts]
  (render lines))
