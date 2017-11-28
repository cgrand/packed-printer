(ns net.cgrand.packed-printer
  "Compact pretty printing."
  (:require 
    [net.cgrand.packed-printer.core :as core]
    ; below requires have defmethods
    [net.cgrand.packed-printer.text]
    [net.cgrand.packed-printer.text.edn]))

(defn pprint
  "Packed prints x, core options are:
 * :width the desired output width (in chars, defaults to 72),
 * :strict, a boolean (defaults to false), true when the desired width is a hard limit
   (pprint may throw when no suitable layout is found),
 * :as a keyword denoting the nature of the input (defaults to :edn),
 * :to a keyword denoting the expected output (defaults to :text).

More options may be available depending on :as and :to.

For [:text :edn], supported options are:
 * kv-indent the amount of spaces by which to indent a value when it appears
   at the start of a line (default 2),
 * coll-indents a map of collection start delimiters (as strings) to the amount
   by which to indent (default: length of the delimiter)."
  [x & {:as opts :keys [strict width as to] :or {strict false width 72 as :edn to :text}}]
  (if-some [layout (core/layout (core/spans x [to as] opts) width strict)]
    (core/render layout to opts)
    (throw (ex-info "Can't find a suitable layout. See ex-data."
             {:dispatch [to as] :data x :opts opts}))))