
(ns user
  (:require [clojure.java.io :as io]
            [clojure.core.reducers :as r]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.pprint :refer (pprint)]
            [clojure.repl :refer :all]
            [clojure.set :as set]
            [clojure.tools.namespace.repl :refer (refresh refresh-all)]
            [network-six.ego :as ego]
            [network-six.ga :as ga]
            [network-six.graph :as g]
            [network-six.dot :as dot]
            [network-six.system :as system]
            [network-six.util :as u]
            [me.raynes.fs :as fs]))

(def system nil)

(defn init
  []
  (alter-var-root #'system
                  (constantly (system/system))))

(defn start
  []
  (alter-var-root #'system system/start))

(defn stop
  []
  (alter-var-root #'system (fn [s] (when s (system/stop s)))))

(defn go
  []
  (init)
  (start)
  nil)

(defn reset []
  (stop)
  (refresh :after 'user/go))

(defn update [f]
  (alter-var-root #'system f)
  nil)

