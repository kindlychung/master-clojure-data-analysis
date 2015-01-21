
;;;; This module defines a pipeline for cleaning up the data as it's being
;;;; read.

(ns network-six.io
  (:require [clojure.java.io :as io])
  (:import [java.io SequenceInputStream StringBufferInputStream]
           [java.util Enumeration]))

(def nl (System/getProperty "line.separator"))

(defn seq->enumeration
  [s]
  (let [a (atom (seq s))]
    (reify Enumeration
      (hasMoreElements [_] (empty? @a))
      (nextElement [_]
        (let [f (first @a)]
          (swap! a rest))))))

;; Need to convert the seq to an Enumeration to pass to SequenceInputStream.
(defn lines->stream
  "This takes a sequence of strings (each a line) adds a \newline, if
  necessary, and returns a Stream that contains those strings. It assumes that
  there are more data than you want to hold in memory at once."
  [lines]
  (SequenceInputStream.
    (seq->enumeration
      (map #(StringBufferInputStream. (str % nl)) lines))))

