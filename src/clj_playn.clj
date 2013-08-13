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




;;;; Entry point

(defn -init []
  (load-sprite :dog "dog.jpg")
  (add-sprite :dog))

(defn -update [delta]
  )

(defn -paint [alpha]
  )


(defn -main [& args]
  (let [platform (JavaPlatform/register)]
    (future (PlayN/run
      (proxy [Game$Default] [25]
        (init []
          (-init))
        (update [delta]
          (-update delta))
        (paint [alpha]
          (-paint alpha)))))))

