
(ns network-six.util
  (:require [clojure.string :as string]
            [clojure.set :as set]
            [org.clojure/tools.trace "0.7.8"]))

(defn mean [coll]
  (let [
        ;; Traverse a collection, count size one by one,
        ;; calculate sum step by step
        accum (fn [[sum size] n]
                   [(+ sum n) (inc size)])
        ;; Calculate mean
        calc-mean (fn [[sum size]] (/ sum size))]
    (calc-mean
      (reduce accum [0 0] coll))))

(defn median [coll]
  (let [n (count coll), i (quot n 2)]
    (if (zero? (mod n 2))
      (nth coll i)
      (/ (+ (nth coll i) (nth coll (inc i))) 2.0))))

(defn log
  [src & args]
  (print (str (string/join " " (concat [\[ (str (java.util.Date.)) \] src]
                                       (map str args)))
              "\n"))
  (flush))

(defn log-every
  [n m src & args]
  (when (zero? (mod n m))
    (apply log src args)))



;; another version of group-seq using loop recur
(defn group-seq-strict
  [key-fn coll]
  (loop [xs coll
         groups []]
    (if (empty? xs)
      groups
      (let [k (key-fn (first xs))
            [g1 g2] (split-with #(= k (key-fn %)) xs)]
        (recur g2 (conj groups [k g1]))))))
(defn group-seq
  [& args]
  (lazy-seq (apply group-seq-strict args)))
;(group-seq #(str "hello" %) (range 100))
;(take 5 (group-seq #(str "hello" %) (range 100)))
;(time (doseq [_ (range 100)] (take 5 (group-seq #(str "hello" %) (range 10000)))))
;(time (doseq [_ (range 100)] (take 5 (group-seq-strict #(str "hello" %) (range 10000)))))


(defn trim-dot
  "This removes trailing dot from a string."
  [s]
  (if (.endsWith s ".")
    (.substring s 0 (- (.length s) 1))
    s))

(defn trim-dots
  "Remoes all trailing __dots__ from a string"
  [s]
  (clojure.string/replace s #"\.+$" ""))
;(trim-dots "you.....")



;; Take a collection and index each of its elements
(defn numbered
  [coll]
  (map vector (range) coll))
; (numbered [9 99 999])

;; Extract value of options, in the format `--opt=val`
(defn arg-value
  ([args opt-key] (arg-value args opt-key nil))
  ([args opt-key default]
   (let [opt-str (str "--" (name opt-key))]
     (if-let [opt (first (filter #(.startsWith % opt-str) args))]
       (second (clojure.string/split opt #"="))
       default))))
;(arg-value ["--python=3.5" "--java=8" "--clojure=1.6.0"] "clojure")
;(arg-value ["--python=3.5" "--java=8" "--clojure=1.6.0"] :clojure)

(defn set-diff [a b]
  [(set/difference a b) (set/difference b a)])

