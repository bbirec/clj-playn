(ns clj-playn
  (:import (playn.core Game$Default
                       Image
                       PlayN)
           (playn.java JavaPlatform)))

(defonce layer (atom nil))

(defn -init []
  (let [image (.getImage (PlayN/assets) "dog.jpg")
        l (.createImageLayer (PlayN/graphics) image)]
    (reset! layer l)
    (.add (.rootLayer (PlayN/graphics)) l)))


(defn -update [delta]
  )

(defn -paint [alpha]
  (.setTranslation @layer 0 0))


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

