(ns mirror-mirror
  "Some say mirrors are windows into parallel worlds,
   where every glance reveals not what is,
   but what could be."
  (:require ["bezier-js" :refer [Bezier]]
            ["fabric" :as fabric]
            ["geometric" :as geometric]
            ["js-utils" :as jsu]
            ["points-on-path" :refer [pointsOnPath]]
            ["react-dom/client" :as react-client]
            [applied-science.js-interop :as j]
            [clojure.string :as str]
            [reagent.core :as r]))

(def !object-selected? (atom false))
(def !mouse-down? (atom false))
(def !canvas (atom nil))
(def !subj (atom nil))
(def !mirrors (atom []))
(def !mirrored (atom []))

(def control-styles
  #js {:hasBorders false
       :cornerStyle "circle"
       :padding 20
       :cornerSize 10
       :cornerStrokeColor "rgba(114,222,194,0.8)"
       :cornerColor "rgba(114,222,194,0.8)"
       :controlStroke 0})

(defn add-to-canvas! [objs]
  (doseq [obj objs]
    (.add @!canvas obj)
    (.set obj control-styles)))

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

(defn cubic-path-str [cs]
  (let [[ax ay] (first cs)]
    (str "M " ax "," ay " C "
         (reduce (fn [s [cx cy]]
                   (str s cx "," cy " "))
                 ""
                 (rest cs)))))

(defn ts [length]
  (mapv #(+ 0 (* % (/ 1 (dec length)))) (range length)))

(defn line-points [[ax ay] [bx by]]
  (let [dx (Math/abs (- bx ax))
        dy (Math/abs (- by ay))
        interpolate (geometric/lineInterpolate [[ax ay] [bx by]])]
    (mapv interpolate
          (ts (cond
                (or (= ax bx) (< dx dy)) dy
                (or (= ay by) (< dy dx)) dx)))))

(defn bezier-points [ps]
  (let [bezier (jsu/makeBezier ps)]
    (mapv (fn [t]
            (let [point (.get bezier t)]
              [(.-x point) (.-y point)]))
          (ts (.length bezier)))))

(defn ->path-str
  ([points]
   (->path-str points true))
  ([points closed?]
   (jsu/getSvgPathFromStroke points closed?)))

(defn transformed-path [^js path-obj]
  (fabric/util.transformPath (.-path path-obj)
                             (.calcTransformMatrix path-obj)
                             (.-pathOffset path-obj)))

(defn ->path-points [^js path-obj]
  (->> path-obj
       transformed-path
       (reduce (fn [acc op] (str acc " " (str/join " " op))) "" )
       pointsOnPath
       first))

(defn ->mirror-points [^js path-obj]
  (let [ps (mapcat (fn [segment] (rest segment)) (transformed-path path-obj))]
    (clj->js
     (if (= (count ps) 4) ;; a straight line made of two points
       (apply line-points (partition 2 ps))
       (bezier-points ps)))))

(defn make-mirrored [subj mirror]
  (let [mirror-points (->mirror-points mirror)]
    (clj->js (mapv
              (fn [p]
                (geometric/pointRotate p 180 (closest-point mirror-points p)))
              (->path-points subj)))))

(defn make-mirror-objects!
  "The silvered glass hums with an ancient magic, bending light and reality to its will.
   It reflects not only the surface but the essence of what stands before it.
   Some claim that if you stare too long, the mirror will blink back at you."
  []
  (doseq [m @!mirrored]
    (.remove @!canvas m))
  (let [mirrored (map (fn [mirror]
                        (fabric/Path. (->path-str (make-mirrored @!subj mirror))
                                      #js{:hasControls false
                                          :stroke "rgba(205,253,240,0.5)"
                                          :strokeWidth 1
                                          :shadow (fabric/Shadow. #js {:color "#72DEC2"
                                                                       :blur 30
                                                                       :offsetX 0
                                                                       :offsetY 0})}))
                      @!mirrors)]
    (add-to-canvas! mirrored)
    (reset! !mirrored mirrored)))

(defn mirror-path [path-str]
  (fabric/Path. path-str
                #js {:stroke "white"
                     :strokeWidth 1
                     :fill false
                     :strokeDashArray [0 2 0]}))

(def center-x (/ js/innerWidth 2))
(def center-y (/ js/innerHeight 2))

(defn add-mirror! [path]
  (add-to-canvas! [path])
  (swap! !mirrors conj path)
  (make-mirror-objects!))

(defn add-line-mirror! []
  (add-mirror! (mirror-path (line-path-str [center-x (- center-y 150)]
                                           [center-x (+ center-y 150)]))))

(defn add-quad-mirror! []
  (let [path (mirror-path (quad-path-str [center-x (- center-y 150)]
                                         [center-x center-y]
                                         [center-x (+ center-y 150)]))]
    (.set path "controls" (fabric/controlsUtils.createPathControls path))
    (add-mirror! path)))

(defn add-cubic-mirror! []
  (let [path (mirror-path (cubic-path-str [[center-x (- center-y 150)]
                                           [center-x (- center-y 75)]
                                           [center-x (+ center-y 75)]
                                           [center-x (+ center-y 150)]]))]
    (.set path "controls" (fabric/controlsUtils.createPathControls path))
    (add-mirror! path)))

(defn make-fabric! [el]
  (let [canvas (fabric/Canvas. el #js {:width js/innerWidth
                                       :height js/innerHeight})
        subj (fabric/Path. (circ-path-str [(- center-x 50) (- center-y 50)] 50)
                           #js {:stroke "#72DEC2"
                                :strokeWidth 2})]
    (reset! !canvas canvas)
    (add-to-canvas! [subj])
    (reset! !subj subj)
    (.on canvas "selection:created" #(reset! !object-selected? true))
    (.on canvas "selection:cleared" #(reset! !object-selected? false))
    (.on canvas "mouse:down" #(reset! !mouse-down? true))
    (.on canvas "mouse:up" #(reset! !mouse-down? false))
    (.on canvas "mouse:move" #(when (and @!object-selected? @!mouse-down? (seq @!mirrors))
                                (make-mirror-objects!)))
    (.set canvas #js {:selectionColor "rgba(114,222,194,0.3)"
                      :selectionBorderColor "rgba(114,222,194,0.8)"
                      :selectionLineWidth 1})))

(defn menu []
  (let [button-class ["w-[30px]" "h-[30px]" "opacity-70" "hover:opacity-100" "text-[#72DEC2]" "transition-all" "hover:scale-110"]]
    [:div.flex.items-center.gap-1.fixed.top-7.left-7
     [:button {:class button-class :on-click add-line-mirror!}
      [:svg {:viewBox "-20 -7.5 60 40" :xmlns "http://www.w3.org/2000/svg"}
       [:path {:fill "currentColor" :d "m9.979 12.433c0 1.342 1.15 2.493 2.493 2.493 1.342 0 2.493-1.151 2.493-2.493s-1.151-2.493-2.493-2.493c-1.343 0-2.493 1.151-2.493 2.493zm17.517 17.517c1.342 0 2.493-1.151 2.493-2.493s-1.151-2.493-2.493-2.493c-1.343 0-2.493 1.151-2.493 2.493s1.15 2.493 2.493 2.493zm3.753-22.949-24.272 24.271c-.24.24-.336.48-.336.767 0 .672.624 1.296 1.248 1.296.288 0 .527-.096.767-.384l24.272-24.271c.24-.24.384-.528.384-.815 0-.624-.576-1.2-1.248-1.2-.288 0-.576.096-.815.336z"}]]]
     [:button {:class button-class :on-click add-quad-mirror!}
      [:svg {:viewBox "-20 -7.5 60 40" :xmlns "http://www.w3.org/2000/svg"}
       [:path {:fill "currentColor" :d "m10.523 33.347c-.912 0-1.344.575-1.344 1.197 0 .623.432 1.245 1.344 1.245 9.552 0 19.194-4.218 19.194-15.309 0-10.418-8.49-14.926-17.946-15.309h-1.248c-.96 0-1.392.622-1.392 1.197s.432 1.197 1.392 1.197c1.248 0 .816.048 1.248.048 8.784.336 15.6 4.417 15.6 12.867 0 9.698-8.736 12.867-16.848 12.867z"}]]]
     [:button {:class button-class :on-click add-cubic-mirror!}
      [:svg {:viewBox "-20 -7.5 60 40" :xmlns "http://www.w3.org/2000/svg"}
       [:path {:fill "currentColor" :d "m12.006 32.045c3.025 0 5.041-1.867 6.866-4.795 2.4-3.841 2.688-7.923 5.329-11.476 1.296-1.729 2.208-2.593 3.936-2.593 6.386 0 7.682 10.564 7.682 15.222v1.679c0 .862.622 1.245 1.197 1.245s1.197-.383 1.197-1.245v-1.535c0-5.859-1.58-17.76-10.076-17.76-3.072 0-4.993 2.155-6.529 4.651-2.352 3.841-2.64 7.971-5.233 11.524-1.248 1.681-2.544 2.689-4.369 2.689-6.769 0-7.825-12.821-7.825-16.997 0-.862-.622-1.292-1.197-1.292s-1.197.43-1.197 1.292c0 5.665 1.389 19.391 10.219 19.391z"}]]]]))

(defn world []
  (r/with-let [ref-fn #(when % (make-fabric! %))]
    [:div.bg-black
     [:canvas.w-screen.h-screen.block
      {:ref ref-fn}]
     [menu]]))

(defn app []
  [world])

(defonce ^js react-root
  (when-let [root-el (and (exists? js/document)
                          (js/document.getElementById "app"))]
    (react-client/createRoot root-el)))

(defn ^:dev/after-load main [opts]
  (when react-root
    (.render react-root (r/as-element [app]))))
