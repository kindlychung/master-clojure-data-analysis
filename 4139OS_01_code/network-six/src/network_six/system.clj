
(ns network-six.system
  (:require [network-six.ga :as ga]
            [network-six.graph :as graph]
            [network-six.ego :as ego]
            [network-six.dot :as dot]
            [network-six.util :as u]
            [clojure.java.io :as io]))

(def ^:dynamic *ego-dir* "./facebook")

(defn system []
  {:db-config [:local false nil]
   :ego-dir *ego-dir*
   :graph-opened false
   :graph nil})

(defn start
  ([system] (start system false))
  ([system load-egos]
   (u/log :system :start)
   (let [edges (ego/read-edge-files (:ego-dir system))]
     (assoc system
            :graph-opened true
            :graph edges))))

(defn stop [system]
  (u/log :system :stop)
  (assoc system :graph-opened false))

(defn read-n [n system]
  (let [read-lines (fn [file-name]
                     (with-open [f (io/reader file-name)]
                       (doall (line-seq f))))
        ego-dir (str (:ego-dir system) "/")]
    (assoc system
           (keyword (str "ego-" n))
           {:circles (read-lines (str ego-dir n ".circles"))
            :graph (read-lines (str ego-dir n ".edges"))
            :ego-feat (read-lines (str ego-dir n ".egofeat"))
            :feat (read-lines (str ego-dir n ".feat"))
            :feat-names (read-lines (str ego-dir n ".featnames"))})))

