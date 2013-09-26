(ns clj-playn
  (:import (playn.core Game$Default
                       Image
                       PlayN
                       AbstractFont
                       TextFormat
                       TextFormat$Alignment
                       Font$Style)
           (playn.java JavaPlatform
                       JavaPlatform$Config)))

;; Utils

(defn assets []
  (PlayN/assets))

(defn graphics []
  (PlayN/graphics))

(defn root-layer []
  (.rootLayer (graphics)))

(defn new-image [w h]
  (.createImage (graphics) w h))

(defmacro with-canvas [canvas w h & body]
  `(let [img# (new-image ~w ~h)
         ~canvas (.canvas img#)]
     ~@body
     img#))

;;;; Font

(defn font [name style size]
  (let [style (cond (= style :plain) Font$Style/PLAIN
                    (= style :bold) Font$Style/BOLD
                    (= style :italic) Font$Style/ITALIC
                    (= style :bold-italic) Font$Style/BOLD_ITALIC)]
    (-> (graphics)
        (.createFont name style size))))

(defn text-format [font align]
  (let [align (cond (= align :left) TextFormat$Alignment/LEFT
                    (= align :right) TextFormat$Alignment/RIGHT
                    (= align :center) TextFormat$Alignment/CENTER)]
    (-> (TextFormat.)
        (.withFont font)
        (.withAlignment align))))

(defn layout-text [text format]
  (-> (graphics)
      (.layoutText text format)))

(defn draw-text [canvas text font-name style size x y]
  (-> canvas
      (.fillText
       (layout-text
        text
        (text-format
         (font font-name style size) :left))
       x y)))

;;;; Sprites

(defonce sprites (atom {}))

(defn load-sprite [key img]
  (swap! sprites
           #(assoc %1 key
                   (-> (graphics)
                       (.createImageLayer img)))))

(defn load-sprite-from-file [key file]
  (println (str "Load image : " file " as " key))
  (load-sprite key (-> (assets) (.getImage file))))

(defn add-sprite [key]
  (let [layer (get @sprites key)]
    (-> (root-layer)
        (.add layer))))

(defn get-sprite [key]
  (get @sprites key))

(defn remove-sprite [key]
  (-> (root-layer)
      (.remove (get @sprites key))))

(defn clear-sprites []
  (-> (root-layer)
      (.clear)))

(defn set-translation [key x y]
  (.setTranslation (get-sprite key) x y))


;;;; Animation

(defonce anis (atom []))

(defn move-by [key by duration]
  (swap! anis
         #(conj %1
                {:fn :move
                 :key key
                 :duration duration
                 :elapsed 0

                 :total-x (first by)
                 :total-y (second by)
                 :dx 0
                 :dy 0})))

(defn ratio [v dt duration]
  (/ (* dt v) duration))


;;;; Entry point

(declare -init)

(defn -update [delta]
  ;; Delete finished animations
  (swap!
   anis
   (fn [anis]
     (filter #(not (>= (:elapsed %1)
                       (:duration %1)))
             anis)))

  ;; Update animations
  (let [dt (/ delta 1000)]
   (swap!
    anis
    (fn [anis]
      (map
       (fn [a]
         (let [duration (:duration a)
               dx (ratio (:total-x a) dt duration)
               dy (ratio (:total-y a) dt duration)]
           
           ;; Update values
           (merge a
                  {:elapsed (+ (:elapsed a) dt)
                   :dx dx
                   :dy dy})))
       anis)))))

(defn -paint [alpha]
  (doseq [a @anis]
    (let [sprite (get-sprite (:key a))
          tx (.tx sprite)
          ty (.ty sprite)
          x (+ tx (:dx a))
          y (+ ty (:dy a))]

      (.setTx sprite x)
      (.setTy sprite y))))

(defn config [w h]
  (let [config (JavaPlatform$Config.)]
    (set! (. config width) w)
    (set! (. config height) h)
    config))

(defn -main [& args]
  (let [platform (JavaPlatform/register (config 600 400))]
    (future
      (try 
       (PlayN/run
        (proxy [Game$Default] [25]
          (init []
            (-init))
          (update [delta]
            (-update delta))
          (paint [alpha]
            (-paint alpha))))
       (catch Exception e
         (println (.getMessage e)))))))

;;;; Test codes

(defn dog []
  (load-sprite-from-file :dog "dog.jpg")
  (add-sprite :dog))

(defn background []
  (let [w (.width (graphics))
        h (.height (graphics))]
   (load-sprite :background
                (with-canvas canvas w h
                  (doto canvas
                    (.setFillColor 0xffffffff)
                    (.fillRoundRect 0 0 w h 0))))
   (add-sprite :background)))



(defn button []
  (let [w 300
        h 100
        radius 5]
   (load-sprite :button
                (with-canvas canvas w h
                  (doto canvas
                    (.setFillColor 0xff36a5d7)
                    (.fillRoundRect 0 0 w h radius)
                    (.setFillColor 0xffffffff)
                    (draw-text "Welcome to clj-playn!"
                               "Arial"
                               :plain
                               30
                               0 (- (/ h 2) 15))))))
  (add-sprite :button))

(defn -init []
  (background)
  (dog)
  (button))
