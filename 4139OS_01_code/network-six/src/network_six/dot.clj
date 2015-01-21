
(ns network-six.dot
  (:require [network-six.graph :as graph]
            [network-six.util :as u]
            [clojure.java.io :as io])
  (:gen-class))

(def ^:dynamic *colors* ["red"
                         "pink"
                         "magenta"
                         "purple"
                         "blueviolet"
                         "blue"])

