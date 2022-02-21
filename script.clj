(require '[clojure.set :as set])

(defn vertical-line [board line]
  (->> board (map (fn [l] (nth l line)))))

(defn vertical-lines [board]
  (->> (range 9) (map (fn [line] (vertical-line board line)))))

(defn subgrids [board]
  (let  [board (->> board
                    (mapcat (partial partition 3)))]
    (for [r (concat (range 0 3)
                    (range 9 12)
                    (range 18 21))]
      (let [indicies (->> r
                          (iterate (partial + 3))
                          (take 3))]
        (mapcat (fn [idx] (nth board idx)) indicies)))))

(defn subgrid-for-coord [board coord]
  (let [[row column] (map #(quot % 3) coord)
        subgrid-number (+ column (* row 3))]
    (nth (subgrids board) subgrid-number)))

(defn constraints-for-coord
  "Returns a vector containing the horizontal, vertical and subgrid
  constraints at a given [row, col] coordinate"
  [board [row column]]
  [(nth (vertical-lines board) column)
   (nth board row)
   (subgrid-for-coord board [row column])])

(def possible-values (into #{} (range 1 10)))

(defn valid-digit-at [board coord]
  (let [taken-values (->> (constraints-for-coord board coord)
                          flatten
                          (into #{}))]
    (set/difference possible-values taken-values)))

(defn cell->coord [cell]
  [(quot cell 9) (mod cell 9)])

(defn solve
  ([board] (solve board 0))
  ([board cell]
   (if (= cell 81)
     board
     (let [coord (cell->coord cell)
           value (get-in board coord)]

       (if (not= value 0)
         (solve board (inc cell))
         (for [value (valid-digit-at board coord)
               solution (solve
                         (update-in board coord (fn [_] value))
                         (inc cell))]
           solution))))))

(def example-board
  [[5 3 0 0 7 0 0 0 0]
   [6 0 0 1 9 5 0 0 0]
   [0 9 8 0 0 0 0 6 0]
   [8 0 0 0 6 0 0 0 3]
   [4 0 0 8 0 3 0 0 1]
   [7 0 0 0 2 0 0 0 6]
   [0 6 0 0 0 0 2 8 0]
   [0 0 0 4 1 9 0 0 5]
   [0 0 0 0 8 0 0 7 9]])

(solve example-board)


(def new-board
  (->> "005000060000006302040081597012038754000200810087014000120007680000092030954860200"
       (partition 9)
       (map (partial map str))
       (map (partial map (fn [i] (Integer/parseInt i))))
       (map (partial into []))
       (into [])))

(solve new-board)

