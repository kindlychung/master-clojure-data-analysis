
(ns network-six.hive)

(def pi (.-PI js/Math))

(defn degrees [radians]
  (- (* 180 (/ radians pi)) 90))

(defn make-angle []
  (.. js/d3
    -scale
    ordinal
    (domain (.range js/d3 4))
    (rangePoints (array 0 (* 2.0 pi)))))

(defn make-color []
  (.. js/d3
    -scale
    category10
    (domain (array 0 1 2 3 4 5 6))))

(defn make-svg [width height]
  (.. js/d3
    (select ".container")
    (append "svg")
    (attr "width" width)
    (attr "height" height)
    (append "g")
    (attr "transform"
          (str "translate(" (/ width 2) \, (/ height 2) \)))))

(defn has-degree? [nodes d]
  (not (nil? (.-data (aget nodes (.-source d))))))

(defn get-radius [nodes]
  (.. js/d3
    -scale
    linear
    (range (array 40 400))
    (domain (array (.min js/d3 nodes #(.-count %))
                   (.max js/d3 nodes #(.-count %))))))

(defn make-axes [svg angle radius]
  (.. svg
    (selectAll ".axis")
    (data (.range js/d3 3))
    enter
    (append "line")
    (attr "class" "axis")
    (attr "transform" #(str "rotate(" (degrees (angle %)) \)))
    (attr "x1" (aget (.range radius) 0))
    (attr "x2" (aget (.range radius) 1))))

(defn get-degreed [nodes data]
  (.. data -links (filter #(has-degree? nodes %))))

(defn get-classes [obj]
  (let [degree (.-data obj)]
    (str "degree degree-" (if (nil? degree) "null" degree))))

(defn make-arcs [svg nodes degreed color angle radius]
  (.. svg
    (selectAll ".link")
    (data degreed)
    (enter)
    (append "path")
    (attr "class" "link")
    (attr "stroke" #(color (.-data (aget nodes (.-source %)))))
    (attr "d" (.. js/d3
                -hive
                link
                (angle #(-> nodes (aget %) .-n (mod 3) angle))
                (radius #(-> nodes (aget %) .-count radius))))
    (attr "class" #(str "link " (get-classes (aget nodes (.-source %)))))))

(defn make-circles [svg nodes color angle radius]
  (.. svg
    (selectAll ".node")
    (data nodes)
    (enter)
    (append "circle")
    (attr "stroke" #(color (.-data %)))
    (attr "transform" #(str "rotate(" (degrees (angle (mod (.-n %) 3))) \)))
    (attr "cx" #(radius (.-count %)))
    (attr "r" 5)
    (attr "class" #(get-classes %))))

(defn ^:export main [json-file]
  (let [width 750, height 750
        angle (make-angle), color (make-color)
        svg (make-svg width height)]
    (.json js/d3 json-file
           (fn [err data]
             (let [nodes (.-nodes data)
                   radius (get-radius nodes)]
               (make-axes svg angle radius)
               (let [df (get-degreed nodes data)]
                 (make-arcs svg nodes df color angle radius)
                 (make-circles svg nodes color angle radius)))))))

