(require ['clojure.string :as 'str])

(defn load-input []
  (str/split-lines (slurp "./inputs/2.txt")))

;; Initial position as X, Y coordinates.
(defn initial-position []
  {:x 0 :y 0 :aim 0})

(defn parse-command [command]
  (str/split command #" "))

(defn apply-command-one [command coordinates]
  (let [split-command (parse-command command)
        direction (first split-command)
        magnitude (Integer/parseInt (last split-command))]
    (case direction
      "forward" (assoc coordinates :x (+ (get coordinates :x) magnitude))
      "up" (assoc coordinates :y (- (get coordinates :y) magnitude))
      "down" (assoc coordinates :y (+ (get coordinates :y) magnitude)))))

(defn apply-command-two [command coordinates]
  (let [split-command (parse-command command)
        direction (first split-command)
        magnitude (Integer/parseInt (last split-command))]
    (case direction
      "forward" (assoc (assoc coordinates :x (+ (get coordinates :x) magnitude)) :y (+ (* (get coordinates :aim) magnitude) (get coordinates :y)))
      "up" (assoc coordinates :aim (- (get coordinates :aim) magnitude))
      "down" (assoc coordinates :aim (+ (get coordinates :aim) magnitude)))))

(defn part-one []
  (let [coordinates (reduce
          (fn [coordinates cmd]
            (apply-command-one cmd coordinates))
          (initial-position)
          (load-input))]
    (* (get coordinates :x) (get coordinates :y))))

(defn part-two []
  (let [coordinates (reduce
          (fn [coordinates cmd]
            (apply-command-two cmd coordinates))
          (initial-position)
          (load-input))]
    (* (get coordinates :x) (get coordinates :y))))

;; uncomment to solve part one
;; (part-one)

;; uncomment to solve part two
(part-two)