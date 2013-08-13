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

;;;; Sprites

(defonce sprites (atom {}))

(defn load-sprite [key file]
  (when (nil? (get @sprites key))
    (println (str "Load image : " file " as " key))
    (swap! sprites
           #(assoc %1 key
                   (let [img (-> (assets)
                                 (.getImage file))]
                     (-> (graphics)
                         (.createImageLayer img)))))))


(defn add-sprite [key]
  (let [layer (get @sprites key)]
    (-> (root-layer)
        (.add layer))))

(defn get-sprite [key]
  (get @sprites key))

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
  (load-sprite :dog "dog.jpg")
  (add-sprite :dog))

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

