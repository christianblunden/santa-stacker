(ns santa-stacker.core
  (:require (clojure.data [csv :as csv])
            (clojure.java [io :as io])))

(defn mapify [[id x y z]] {:id (Integer/parseInt id) :x (Integer/parseInt x) :y (Integer/parseInt y) :z (Integer/parseInt z)})


(defn all-corners [{:keys [id x y z]}]
  (for [z [0 z]
        y [0 y]
        x [0 x]]
    {:id id :x x :y y :z z}))

(defn offset [[x' y' z'] corners]
  (map (fn [{:keys [id x y z]}] {:id id  :x (+ x x') :y (+ y y') :z (+ z z')}) corners)
  )

(all-corners {:id "X" :x 10 :y 16 :z 34})

(offset [100 100 100] (all-corners {:id "X" :x 10 :y 16 :z 34}))

(def initial-data
  (map (comp mapify)
       (with-open [in-file (io/reader "/Users/nick/Downloads/presents.csv")]
         (doall (take 4 (rest (csv/read-csv in-file)))))))

initial-data

(defn max-z-of-all-corners [s]
  (if (empty? s)
    0
    (apply max (map :z s))))


(reverse
 (reduce (fn [result item]
           (let [z-offset (max-z-of-all-corners (first result))]
             (cons (offset [0 0 z-offset] item)
                   result)))
         nil
         (map all-corners initial-data)
         ))

(map all-corners initial-data)
