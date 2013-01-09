(ns info-theory.viz
  (:use [info-theory.core])
  (:require [incanter.charts :as c]
            [incanter.core :as i]))

(defn perm-hist
  []
  (let [data (permutation-count 4 (dgp 400))
        categories (map str (range (count data)))]
    (prn categories)
    (doto (c/bar-chart categories (map val data))
      (c/set-x-label "Sequence indices")
      (c/set-y-label ""))))

(defn save-graphs []
  (i/save (perm-hist)
          "/mnt/hgfs/Dropbox/Public/permutation-entropy.png"
          :width 800 :height 500))

(defn line-graph []
  (let [x (range 400)
        y1 (dgp 400)
        y2 (demean y1)
        plot (c/xy-plot x y1)]
    (doto plot
      (c/add-lines x y2)
      (i/save "/mnt/hgfs/Dropbox/Public/lines.png"
              :width 800 :height 500))))
