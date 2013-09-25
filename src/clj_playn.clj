(ns clj-playn
  (:import (playn.core Game$Default
                       Image
                       PlayN)
           (playn.java JavaPlatform)))

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

(defn -init []
  )

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


(defn -main [& args]
  (let [platform (JavaPlatform/register)]
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

(defn sample []
  (load-sprite :sample
               (with-canvas canvas 300 300
                 (doto canvas
                   (.setStrokeWidth 10)
                   (.setStrokeColor 0xffff0000)
                   (.strokeRect 100 100 100 100))))
  (add-sprite :sample))
