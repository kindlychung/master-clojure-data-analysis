
(ns network-six.force)

(defn make-color []
  (.. js/d3
    -scale
    category10
    (domain (array 0 1 2 3 4 5 6))))

(defn make-force-layout [width height]
  (.. js/d3
    -layout
    force
    (charge -120)
    (linkDistance 30)
    (size (array width height))))

(defn make-svg [width height]
  (.. js/d3
    (select ".container")
    (append "svg")
    (attr "width" width)
    (attr "height" height)))

(defn setup-force-layout [force-layout graph]
  (.. force-layout
    (nodes (.-nodes graph))
    (links (.-links graph))
    start))

(defn make-links [svg graph color]
  (.. svg
    (selectAll ".link")
    (data (.-links graph))
    enter
    (append "line")
    (attr "class" "link")
    (attr "stroke" #(-> % .-source .-data color))
    (style "stroke-width" #(js/Math.sqrt (.-value %)))))

(defn make-nodes [svg graph color force-layout]
  (.. svg
    (selectAll ".node")
    (data (.-nodes graph))
    enter
    (append "circle")
    (attr "class" "node")
    (attr "r" 5)
    (attr "data-degree" #(.-data %))
    (attr "data-id" #(.-n %))
    (style "fill" #(color (.-data %)))
    (call (.-drag force-layout))))

(defn on-tick [link node]
  (fn []
    (.. link
      (attr "x1" #(.. % -source -x))
      (attr "y1" #(.. % -source -y))
      (attr "x2" #(.. % -target -x))
      (attr "y2" #(.. % -target -y)))
    (.. node
      (attr "cx" #(.-x %))
      (attr "cy" #(.-y %)))))

(defn ^:export main [json-file]
  (let [width 960, height 600
        color (make-color)
        force-layout (make-force-layout width height)
        svg (make-svg width height)]
    (.json js/d3 json-file
           (fn [err graph]
             (.. graph
               -links
               (forEach #(do (aset %1 "weight" 1.0)
                           (aset %1 "index" %2))))
             (setup-force-layout force-layout graph)
             (let [link (make-links svg graph color)
                   node (make-nodes svg graph color force-layout)]
               (.on force-layout "tick"
                    (on-tick link node)))))))

