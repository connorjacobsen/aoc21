(require ['clojure.string :as 'str])

(defn parse-int [v]
  (Integer/parseInt v))

(defn load-input []
  (map parse-int (str/split-lines (slurp "./inputs/1.txt"))))

(defn count-increasing [input-seq]
  (get
    (reduce
      (fn [data input]
        (if (> input (get data :last))
          {:count (+ (get data :count) 1) :last input}
          (assoc data :last input)))
      {:count -1 :last 0}
      input-seq)
    :count))

;; turn the list of data into all possible windows of 3 items
(defn windows [data]
  (if (= 3 (count (take 3 data)))
    (cons (reduce + (take 3 data)) (windows (rest data)))
    []))

(defn part-one []
  (count-increasing (load-input)))
(defn part-two []
  (count-increasing (windows (load-input))))

;; uncomment to solve part one
;; (part-one)

;; uncomment to solve part two
;; (part-two)
