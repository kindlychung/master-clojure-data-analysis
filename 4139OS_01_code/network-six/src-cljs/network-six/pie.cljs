
(ns network-six.pie)

(defrecord Freq [degree amount])

(defn make-color []
  (.. js/d3
    -scale
    category10
    (domain (array 0 1 2 3 4 5 6))))

(defn make-arc [radius]
  (.. js/d3 -svg arc
    (outerRadius (- radius 10))
    (innerRadius 0)))

(defn make-pie []
  (.. js/d3 -layout pie
    (sort nil)
    (value #(.-amount %))))

(defn make-svg [width height]
  (.. js/d3
    (select ".container")
    (append "svg")
    (attr "width" width)
    (attr "height" height)
    (append "g")
    (attr "transform" (str "translate(" (/ width 2) \, (/ height 2) \)))))

(defn get-freqs [data]
  (->> data
    .-nodes
    (map #(.-data %))
    frequencies
    (map #(Freq. (first %) (second %)))
    into-array))

(defn make-g [svg pie freqs]
  (.. svg
    (selectAll ".arc")
    (data (pie freqs))
    enter
    (append "g")
    (attr "class" "arc")))

(defn make-paths [g arc color]
  (.. g
    (append "path")
    (attr "d" arc)
    (style "fill" #(color (.-amount (.-data %)))))
  g)

(defn label [obj]
  (let [d (.-data obj)]
    (str (.-degree d) " (" (.-amount d) \))))

(defn make-text [g arc]
  (.. g
    (append "text")
    (attr "transform" #(str "translate(" (.centroid arc %) \)))
    (attr "dy" ".35em")
    (style "text-anchor" "middle")
    (text label))
  g)

(defn ^:export main [json-file]
  (let [width 700, height 450
        radius (/ (min width height) 2)
        color (make-color), arc (make-arc radius)
        pie (make-pie), svg (make-svg width height)]
    (.json js/d3 json-file
           (fn [err data]
             (let [freqs (get-freqs data)]
               (-> svg
                 (make-g pie freqs)
                 (make-paths arc color)
                 #_(make-text arc)))))))

