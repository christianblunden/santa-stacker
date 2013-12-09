(ns santa-stacker.scoring
  (:use quil.core))

(def conf {3 [1 1 0 3 1 0 1 3 0 3 3 0 3 3 5 1 1 5 3 1 5 3 3 5]
           2 [4 3 6 9 3 6 4 10 6 9 10 6 4 3 15 9 3 15 4 10 15 9 10 15]
           1 [1 1 0 3 1 0 1 3 0 3 3 0 3 3 5 1 1 5 3 1 5 3 3 5]})

(defn create-rect [[x1 z1] [x2 z2]]
  (flatten
   (for [x [x1 x2]
         y [0 1]
         z [z1 z2]]
     [x y z])))

(def conf2 {1 (create-rect [1 4] [1 7])
            2 (create-rect [2 4] [3 8])
            3 (create-rect [8 1] [10 5])
            4 (create-rect [4 3] [6 5])
            5 (create-rect [4 6] [5 7])
            6 (create-rect [6 6] [7 7])
            7 (create-rect [4 1] [6 2])
            8 (create-rect [1 1] [3 3])
            9 (create-rect [8 6] [10 8])
            10 (create-rect [7 1] [7 3])})

(defn score-z [conf]
  (->> (vals conf)
       (map last)
       (reduce max)))

(defn score-order [conf]
  (->> (keys conf)
       (group-by #(last (conf %)))
       (map (fn [[level ids]]
              [level (sort ids)]))
       (sort-by #(- (first %)))
       (mapcat second)
       (map-indexed (fn [ind id]
                      (Math/abs (- (inc ind) id))))
       (apply +)))

(defn score [conf]
  (+ (* 2 (score-z conf))
     (score-order conf)))

(def sze 20)

(defn setup []
;  (frame-rate 1)
  (background 200))

(def colors (repeatedly #(list (+ 128 (rand-int 127))
                               (+ 128 (rand-int 127))
                               (+ 128 (rand-int 127)))))

(defn draw3d []
  (push-matrix)
  (background 200)
;  (translate 1 1)
  (translate (/ (width) 2) (/ (height) 2))
  (scale 30)
  (rotate-y (* (mouse-x) (/ 2.0 (width)) TWO-PI))
  (rotate-x (* (mouse-y) (/ 2.0 (height)) TWO-PI))
  (println "\n\n\n")
  (doseq [[id cells] conf2]
    (let [sizes (map - (take-last 3 cells) (take 3 cells) [-1 -1 -1])
          [x1 y1 z1] (map + cells [-1 -1 -1]
                          (map #(/ % 2) sizes))]
      (fill (apply color (nth colors id)))
      (with-translation [x1 y1 z1]
        (let [sizes (map - (take-last 3 cells) (take 3 cells) [-1 -1 -1])]
          (println :id id :coords x1 y1 z1 sizes)

          (apply box sizes))
        (fill 0)
        (with-translation [0 5 0]
          (text (str id) )))))
  (pop-matrix))

(defn draw []
  (doseq [[id cells] conf2]
    (let [x1 (first cells)
          z1 (- 10 (nth cells 2) )
          x2 (inc (nth cells 21))
          z2 (- 10 (inc (nth cells 23)))]
      (fill (apply color (nth colors id)))
      (println z1 z2)
      (rect (* sze x1) (* sze z1) (* sze (- x2 x1)) (* sze (- z2 z1)))
      (fill 0)
      (text (str id) (* 1/2 (+ x1 x2) sze) (* 1/2 (+ z1 z2) sze)))))

#_(defsketch example                  ;;Define a new sketch named example
  :title "Oh so many grey circles"  ;;Set the title of the sketch
  :setup setup                      ;;Specify the setup fn
  :draw #'draw3d                        ;;Specify the draw fn
  :renderer :p3d
  :size [1000 600])                  ;


(assert (and (= (score-order conf2) 22)
             (= (score-z conf2) 8)
             (= (score conf2) 38)))
