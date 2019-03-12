(ns snazzy.core
  (:require cljsjs.d3))

;; HTML element IDs
(def html-id-app "app")
(def html-id-chart "chart")
(def html-id-buffer "buffer")
(def html-id-fps "fps")

(def hex-white "#ffffff")
(def hex-black "#000000")

;; Config vals
(def cfg-chart-height 500)
(def cfg-chart-width 500)
(def cfg-chart-height-center (/ cfg-chart-height 2))
(def cfg-chart-width-center (/ cfg-chart-width 2))

(def cfg-dots-count 25)
(def cfg-dots-radius-min 10)
(def cfg-dots-radius-max 25)
(def cfg-dots-stroke-width 3)
(def cfg-dots-stroke-color hex-black)

(def cfg-sim-iters 500)
(def cfg-sim-charge 20)

;; Paul Tol bright colors (https://personal.sron.nl/~pault/)
(def ptb-colors ["#4477AA" ; blue
                 "#EE6677" ; pink
                 "#228833" ; green
                 "#CCBB44" ; olive
                 "#66CCEE" ; light blue
                 "#AA3377" ; purple
                 "#BBBBBB" ; gray
                 ])

;; https://github.com/d3/d3-force#simulation_tick
(defn sim-decay
  "Takes the number of iterations for the sim and returns the alpha decay."
  [iters]
  (- 1 (Math/pow 0.001 (/ 1 iters))))

(defn time-now [] (.now js/Date))

(defn rand-int-between
  "Return a random int between `min` (inclusive) and `max` (exclusive)."
  [min max]
  (+ min (rand-int (- max min))))

(defn rand-dot
  [chart-width chart-height dot-min-r dot-max-r]
  (let [r (rand-int-between dot-min-r dot-max-r)]
    ;; Keep the x and y vals so that the whole circle will be in the
    ;; chart
    {:x (rand-int-between r (- chart-width r))
     :y (rand-int-between r (- chart-height r))
     :r r
     :fill (rand-nth ptb-colors)}))

(defn rand-dots
  "Returns a JS object with data points for `n` dots."
  [n]
  (clj->js (map (fn []
                  (rand-dot cfg-chart-width
                            cfg-chart-height
                            cfg-dots-radius-min
                            cfg-dots-radius-max))
                (range n))))


(defn bind-data!
  "Binds data to a container and returns the d3 selection.
  `fill-attr` can be fillStyle for canvas or fill for svg."
  [data container elem class fill-attr]
  (let [binding (-> container
                    (.selectAll (str elem \. class))
                    (.data data))]
    (-> binding
        .enter
        (.append elem)
        (.classed class true)
        (.merge binding)
        ;; The following attributes may change on sim tick
        (.attr "cx" (fn [d] (goog.object/get d "x")))
        (.attr "cy" (fn [d] (goog.object/get d "y")))
        (.attr "r" (fn [d] (goog.object/get d "r")))
        (.attr fill-attr (fn [d] (goog.object/get d "fill"))))
    (-> binding .exit .remove)
    binding))

(defn canvas-clear-context!
  "Clears the canvas associated with the given `context`."
  [context]
  (set! (.-fillStyle context) hex-white)
  (let [x 0 y 0]
    (.rect context x y cfg-chart-width cfg-chart-height))
  (.fill context))

(defn canvas-draw-dot!
  "Draw a dot on the canvas `context` with params from `datum`.
  Datum would be some bound d3 data."
  [context datum]
  (let [cx (goog.object/get datum "x")
        cy (goog.object/get datum "y")
        r (goog.object/get datum "r")
        fill (goog.object/get datum "fill")
        arc-start-theta 0
        arc-stop-theta (* 2 Math/PI)]
    (.beginPath context)
    (set! (.-fillStyle context) fill)
    (set! (.-strokeStyle context) cfg-dots-stroke-color)
    (set! (.-lineWidth context) cfg-dots-stroke-width)
    (.arc context cx cy r arc-start-theta arc-stop-theta)
    (.fill context)
    (.stroke context)))

(defn canvas-copy-to-context!
  "Copy the drawing from `source-canvas` to `target-context`.
  Used to speed up canvas rendering by drawing a complete canvas image
  in memory then copying to an actually visible canvas later."
  [source-canvas target-context]
  (let [x 0 y 0]
    (.drawImage target-context source-canvas x y)))

(defn collide-radius
  "Returns the actual radius for collision.
  The radius of the datum does not include the stroke width, so for
  nice looking collisions, we need to account for that.
  TODO need to fix this....not quite right"
  [datum]
  (+ cfg-dots-stroke-width (goog.object/get datum "r")))

(defn sim!
  "Set up the d3 force simulation.
  Generates the simulation for the given `data` and calling
  `update-fn` on every tick."
  [data update-fn]
  (-> js/d3
      (.forceSimulation data)
      (.alphaDecay (sim-decay cfg-sim-iters))
      ;; Nodes repel each other.
      (.force "charge"
              (-> js/d3
                  .forceManyBody
                  (.strength cfg-sim-charge)))
      ;; Nodes head towards the center.
      (.force "center"
              (-> js/d3
                  (.forceCenter cfg-chart-width-center
                                cfg-chart-height-center)))
      ;; Nodes repel each other
      (.force "collision"
              (-> js/d3
                  .forceCollide
                  (.radius collide-radius)))
      (.on "tick" update-fn)))

(defn canvas-with-context
  "Return a map with a canvas element and its associated context."
  [id]
  (let [canvas (.createElement js/document "canvas")]
    (.setAttribute canvas "id" id)
    (.setAttribute canvas "width" cfg-chart-width)
    (.setAttribute canvas "height" cfg-chart-height)
    (let [context (.getContext canvas "2d")]
      {:canvas canvas :context context})))

(defn canvas-clear! [context]
  (set! (.-fillStyle context) hex-white)
  (let [x 0 y 0]
    (.rect context x y cfg-chart-width cfg-chart-height))
  (.fill context))

;; (defn canvas-draw-dots! [context data]
;;   (doseq [d data]
;;     (canvas-draw-dot! context d)))

(defn main []
  (let [data (rand-dots cfg-dots-count)
        chart (canvas-with-context html-id-chart)
        buffer (canvas-with-context html-id-buffer)]

    (js/console.log (:context chart))

    ;; Make sure any existing canvas has been removed.
    (-> js/d3
        (.select (str "#" html-id-chart))
        .remove)
    ;; Add the canvas element to the app.
    (.appendChild (.getElementById js/document html-id-app)
                  (:canvas chart))
    ;; Run the simulation
    (sim! data
          (fn []
            (canvas-clear! (:context buffer))
            (doseq [d data]
              (canvas-draw-dot! (:context buffer) d))
            (canvas-copy-to-context! (:canvas buffer) (:context chart))))))

(main)
