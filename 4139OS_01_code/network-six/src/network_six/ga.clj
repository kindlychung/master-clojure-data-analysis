
(ns network-six.ga
  (:require [network-six.ego :as ego]
            [network-six.graph :as g]))

(defrecord GA
  [selection-rate m-rate x-over-rate pass-through population-size populations rand-gene graph width])

(defrecord Chromosome
  [v fitness])

(defn vec->gene [v]
  (Chromosome. v nil))

(defrecord Population
  [n pool])

(defn ->fn [n]
  (if (number? n)
    (constantly n)
    n))

(defn fn-param [p ga]
  ((p ga) ga))

(defn learning-rate
  "This is a time-decaying learning rate, suitable for the m-rate."
  [initial-rate t2]
  (fn [ga]
    (let [t (count (:populations ga))]
      (* initial-rate (Math/pow Math/E (/ (* -1 t) t2))))))

(defn dist [p0 p1]
  (let [[x0 y0] p0, [x1 y1] p1
        sq-diff (fn [a b] (let [d (- a b)] (* d d)))]
    (Math/sqrt (+ (sq-diff y1 y0) (sq-diff x1 x0)))))

(defn cache-positions
  [v width]
  (->> v
    count
    range
    (map (fn [n] [(quot n width) (mod n width)]))
    (map vector v)
    (into {})))

(defn fitness
  "This takes the population and a chromosome and returns its fitness.

  For this, the fitness is the sum distance between each node and all of its
  neighbors on the graph.

  This updates the gene's pos-cache and fitness value."
  [ga gene]
  (let [pos (cache-positions (:v gene) (:width ga))]
    (->>
      ga
      :graph
      g/get-edges
      (map #(dist (pos (first %)) (pos (second %))))
      (reduce + 0))))

(defn update-fitness
  "This re-computes the fitness for the population passed in. Those values are
  updated in the populuation, and the population is added to the list in the
  GA."
  ([ga population] (update-fitness ga population (:pool population)))
  ([ga population pool]
   (->> pool
     (mapv #(assoc % :fitness (fitness ga %)))
     (assoc population :pool)
     (conj (:populations ga))
     (assoc ga :populations))))

(defn select
  "This selects the pool to derive the next generation from."
  [ga pass-through]
  (let [k (- (fn-param :selection-rate ga) pass-through)
        [pop-0 pop-1] (split-at pass-through
                                (:pool (last (:populations ga))))
        group-1 (* (:population-size ga) k)
        group-2 (- (:population-size ga) group-1)
        items (keys (:graph ga))
        survivors (sort-by :fitness pop-1)
        rand-gene (:rand-gene ga)]
    (Population.
      (count (:populations ga))
      (mapv
        #(assoc % :fitness (fitness ga %))
        (take (:population-size ga)
              (concat
                pop-0
                (take group-1 survivors)
                (repeatedly group-2 #(rand-gene items))))))))

(defn rand-swap
  ([coll] (rand-swap (rand-int (count coll))))
  ([coll i]
   (let [j (rand-int (count coll))]
     (assoc coll
            i (nth coll j)
            j (nth coll i)))))

(defn mutate-i
  [m-rate gene-vec i]
  (if (< (rand) m-rate)
    (rand-swap gene-vec i)
    gene-vec))

(defn mutate
  "This takes the population and chromosome and mutates the chromosome.

  This uses *exchange mutation*, in which two items are swapped."
  [ga gene]
  (let [m-rate (fn-param :m-rate ga)]
    (vec->gene
      (reduce #(mutate-i m-rate %1 %2)
              (:v gene)
              (range (count (:v gene)))))))

(defn get-mappings
  "This determines the mapping relationship for the PMX operator."
  [v1 v2]
  [(into {} (map vector v1 v2)), (into {} (map vector v2 v1))])

(defn find-rel
  [rel k]
  (let [v (rel k)]
    (cond (nil? v) k
          (contains? rel v) (find-rel rel v)
          :else v)))

(defn make-child [p1 p2 i j rel]
  (vec
    (concat
      (map #(find-rel rel %) (subvec p1 0 i))
      (subvec p2 i j)
      (map #(find-rel rel %) (subvec p1 j)))))

(defn sample-indexes
  [n k]
  (if (< k n)
    (loop [seen #{}]
      (if (>= (count seen) k)
        seen
        (recur (conj seen (rand-int n)))))
    (range n)))

(defn partition-population
  [population r]
  (let [indexes (sample-indexes (count population) (* (count population) r))
        in-sample (fn [pair] (indexes (first pair)))
        indexed (map-indexed vector population)]
    [(map second (filter in-sample indexed))
     (map second (remove in-sample indexed))]))

(defn x-over
  "This takes the population and two chromosomes and performs the cross-over
  operation.

  This uses *partially-matched crossover* (PMX)."
  [ga parent-1 parent-2]
  (let [p1 (:v parent-1), p2 (:v parent-2)
        [i j] (sort [(rand-int (count p1))
                     (rand-int (count p2))])
        core-1 (subvec p1 i j)
        core-2 (subvec p2 i j)
        [rel-1 rel-2] (get-mappings core-1 core-2)
        child-1 (make-child p1 p2 i j rel-2)
        child-2 (make-child p2 p1 i j rel-1)]
    [(vec->gene child-1), (vec->gene child-2)]))

(defn x-over-pair
  [ga pair]
  (if (= (count pair) 1)
    pair
    (x-over ga (first pair) (second pair))))

(defn x-over-pop
  "This takes a certain percentage of a new population and applies the
  crossover operation on them."
  [ga population]
  (let [[sample leftovers] (partition-population
                             population
                             (fn-param :x-over-rate ga))]
    (->> sample
      shuffle
      (partition-all 2)
      (mapcat #(x-over-pair ga %))
      (concat leftovers)
      vec)))

(defn rand-gene
  "This generates a random gene for the GA."
  [vocab]
  (vec->gene (vec (shuffle vocab))))

(defn init
  ([rand-gene graph] (init 0.75 0.05 0.5 5 50 rand-gene graph))
  ([sel-rate m-rate x-over-rate pass-through population-size rand-gene graph]
   (let [items (keys graph)
         initial (Population. 0 (repeatedly population-size #(rand-gene items)))
         ga (GA. (->fn sel-rate)
                 (->fn m-rate)
                 (->fn x-over-rate)
                 (->fn pass-through)
                 population-size
                 []
                 rand-gene graph
                 66)]
     (update-fitness ga initial))))

(defn partition-call [n f1 f2 coll]
  (let [[c1 c2] (split-at n coll)]
    (concat (f1 c1) (f2 c2))))

(defn generation [ga]
  (let [pass-through (fn-param :pass-through ga)
        p0 (select ga pass-through)]
    (->> p0
      :pool
      (sort-by :fitness)
      (partition-call pass-through identity (fn [coll]
                                              (x-over-pop ga
                                                          (map #(mutate ga %) coll))))
      ; (x-over-pop ga)
      (update-fitness ga p0))))

(defn report [ga]
  (let [fits (->> ga
               :populations
               last
               :pool
               (map :fitness))
        mn (reduce min Integer/MAX_VALUE fits)
        mx (reduce max Integer/MIN_VALUE fits)
        mean (/ (reduce + 0 fits) (float (count fits)))]
    {:max mx, :min mn, :mean mean}))

(defn get-min-fitness [population]
  (->> population
    :pool
    (sort-by :fitness)
    first))

(defn run
  ([iterations ga]
   (let [step (fn [[ga lowest accum] n]
                (let [ga-next (generation ga)
                      last-p (last (:populations ga-next))
                      rpt (report ga-next)]
                  (println n rpt (fn-param :m-rate ga))
                  [ga-next
                   (min-key :fitness lowest (get-min-fitness last-p))
                   (conj accum rpt)]))
         start [ga
                (get-min-fitness (last (:populations ga)))
                [(report ga)]]]
     (reduce step start (range iterations))))
  ([graph iterations sel-rate m-rate x-over-rate pass-through pop-size]
   (run iterations (init sel-rate m-rate x-over-rate pass-through pop-size rand-gene graph))))

(comment

(reset)
(->> system :ga :populations first :pool count)
(->> system :ga :populations first :pool first :v count)
(->> system :ga :populations last :pool first :fitness)
(->> system :ga :populations last :pool (map :fitness) (reduce min Integer/MAX_VALUE))
(->> system :ga :populations last :pool (map :fitness) (reduce max Integer/MIN_VALUE))
(def p0 (ga/select (:ga system) 0.75))
(def m0 (->> p0 :pool (map #(ga/mutate (:ga system) %))))
(def xo (ga/x-over-pop (:ga system) m0))
(def uf (ga/update-fitness (:ga system) p0 xo))
(def g0 (ga/generation (:ga system)))
(ga/report (:ga system))

(reset)
(def graph {0 #{4}, 1 #{2 4 5}, 2 #{1 6}, 3 #{4}, 4 #{0 1 3 5 7 8},
            5 #{1 2 4 6 7 8}, 6 #{2 5 8}, 7 #{4 5 8 9}, 8 #{4 5 6 7}, 9 #{7},
            10 #{}, 11 #{}, 12 #{}, 13 #{14 15}, 14 #{13 15 16 17 18},
            15 #{13 14 16 18 19}, 16 #{14 15 18}, 17 #{14 18}, 18 #{14 15 16 17 19}, 19 #{15 18}})
(def ga (ga/init 0.75 (ga/learning-rate 0.95 10.0) 0.5 5 100 ga/rand-gene graph))
(def output (ga/run 100 ga))

(def output (ga/run graph 100 0.75 (ga/learning-rate 0.95 10.0) 0.5 5 100))
(def output (ga/run (:edges system) 100 0.75 (ga/learning-rate 0.95 10.0) 0.5 2 100))
(first (second output))
(last (second output))

  )

