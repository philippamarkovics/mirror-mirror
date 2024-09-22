(ns mirror-mirror
  "Some say mirrors are windows into parallel worlds,
   where every glance reveals not what is,
   but what could be."
  (:require ["fabric" :as fabric]
            ["geometric" :as geometric]
            ["js-utils" :as jsu]
            ["points-on-curve" :refer [pointsOnBezierCurves]]
            ["points-on-path" :refer [pointsOnPath]]
            ["react-dom/client" :as react-client]
            [applied-science.js-interop :as j]
            [clojure.string :as str]
            [reagent.core :as r]))

(def !mirrored
  (atom nil))

(defn distance [[ax ay] [bx by]]
  (Math/sqrt (+ (Math/pow (- ax bx) 2)
                (Math/pow (- ay by) 2))))

(defn closest-point [ps p]
  (reduce (fn [candidate current]
            (if (< (distance p current)
                   (distance p candidate))
              current
              candidate))
          (first ps)
          ps))

(defn circ-path-str [[cx cy] r]
  (str "M " (- cx r) "," (+ cy r) " a " r "," r " 0 1,0 " (* r 2) ",0 a " r "," r " 0 1,0 " (* r -2) ",0"))

(defn line-path-str [[ax ay] [bx by]]
  (str "M " ax "," ay " L " bx "," by))

(defn quad-path-str [[ax ay] [qx qy] [bx by]]
  (str "M " ax "," ay " Q " qx "," qy " "  bx "," by))

(defn cubic-path-str [[ax ay] [c1x c1y] [c2x c2y] [bx by]]
  (str "M " ax "," ay " C " c1x "," c1y " " c2x "," c2y " "  bx "," by))

(defn line-points [[ax ay] [bx by]]
  (let [dx (Math/abs (- bx ax))
        dy (Math/abs (- by ay))
        interpolate (geometric/lineInterpolate [[ax ay] [bx by]])
        steps (cond
                (or (= ax bx) (< dx dy)) dy
                (or (= ay by) (< dy dx)) dx)]
    (->> (range steps)
         (map #(+ 0 (* % (/ 1 (dec steps)))))
         (map #(interpolate %)))))

(defn slope [[ax ay] [bx by]]
  (if (= ax bx) :vertical (/ (- by ay) (- bx by))))

(defn straight-line? [points]
  (apply = (map slope points (rest points))))

(defn ->points [path-str]
  (let [points (first (pointsOnPath path-str))]
    (if (straight-line? points)
      (line-points (first points) (last points))
      points)))

(defn ->path-str
  ([points]
   (->path-str points true))
  ([points closed?]
   (jsu/getSvgPathFromStroke points closed?)))

(defn path-segments->path-str [ops]
  (reduce (fn [acc op] (str acc " " (str/join " " op))) "" ops))

(defn transformed-points [^js path-obj debug]
  (->points (path-segments->path-str (fabric/util.transformPath (.-path path-obj)
                                                                (.calcTransformMatrix path-obj)
                                                                (.-pathOffset path-obj)))))

(defn make-mirror-object!
  "The silvered glass hums with an ancient magic, bending light and reality to its will.
   It reflects not only the surface but the essence of what stands before it.
   Some claim that if you stare too long, the mirror will blink back at you."
  [canvas ^js subj ^js mirror]
  (when @!mirrored
    (.remove canvas @!mirrored))
  (let [mirror-points (transformed-points mirror "mirror")
        mirrored-points (clj->js (mapv
                                  (fn [p]
                                    (geometric/pointRotate p 180 (closest-point mirror-points p)))
                                  (transformed-points subj nil)))
        mirrored (fabric/Path. (->path-str mirrored-points)
                               #js{:stroke "rgba(205,253,240,0.5)"
                                   :strokeWidth 1
                                   :shadow (fabric/Shadow. #js {:color "#72DEC2"
                                                                :blur 30
                                                                :offsetX 0
                                                                :offsetY 0})})]
    (.add canvas mirrored)
    (reset! !mirrored mirrored)))

(def control-styles
  #js {:hasBorders false
       :cornerStyle "circle"
       :padding 20
       :cornerSize 10
       :cornerStrokeColor "rgba(114,222,194,0.8)"
       :cornerColor "rgba(114,222,194,0.8)"
       :controlStroke 0})

(def !object-selected? (atom false))
(def !mouse-down? (atom false))

(defn mirror-path [path-str]
  (fabric/Path. path-str
                #js {:stroke "white"
                     :strokeWidth 1
                     :strokeDashArray [0 2 0]}))

(defn line-mirror [a b]
  (mirror-path (line-path-str a b)))

(defn quad-mirror [a q b]
  (let [path (mirror-path (quad-path-str a q b))]
    (.set path "controls" (fabric/controlsUtils.createPathControls path))
    path))

(defn make-fabric! [el]
  (let [canvas (fabric/Canvas. el #js {:width js/innerWidth
                                       :height js/innerHeight})
        subj (fabric/Path. (circ-path-str [300 (- (/ js/innerHeight 2) 100)] 50)
                           #js {:stroke "#72DEC2"
                                :strokeWidth 2})
        mirror (line-mirror [(+ 200 150 100) (- (/ js/innerHeight 2) 200)]
                            [(+ 200 150 100) (+ (/ js/innerHeight 2) 100)])
        #_#_mirror (quad-mirror [(+ 200 150 100) (- (/ js/innerHeight 2) 200)]
                                [(+ 200 150 100) (- (/ js/innerHeight 2) 50)]
                                [(+ 200 150 100) (+ (/ js/innerHeight 2) 100)])]
    (.add canvas subj)
    (.add canvas mirror)
    (.set subj control-styles)
    
    (.on canvas "selection:created" #(reset! !object-selected? true))
    (.on canvas "selection:cleared" #(reset! !object-selected? false))
    (.on canvas "mouse:down" #(reset! !mouse-down? true))
    (.on canvas "mouse:up" #(reset! !mouse-down? false))
    (.on canvas "mouse:move" #(when (and @!object-selected? @!mouse-down?)
                                (make-mirror-object! canvas subj mirror)))

    (.set mirror control-styles)
    (.set canvas #js {:selectionColor "rgba(114,222,194,0.3)"
                      :selectionBorderColor "rgba(114,222,194,0.8)"
                      :selectionLineWidth 1})
    (make-mirror-object! canvas subj mirror)))

(defn world []
  (r/with-let [ref-fn #(when % (make-fabric! %))]
    [:div.bg-black
     [:canvas.w-screen.h-screen.block
      {:ref ref-fn}]]))

(defn app []
  [world])

(defonce ^js react-root
  (when-let [root-el (and (exists? js/document)
                          (js/document.getElementById "app"))]
    (react-client/createRoot root-el)))

(defn ^:dev/after-load main [opts]
  (when react-root
    (.render react-root (r/as-element [app]))))
