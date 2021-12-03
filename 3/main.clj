(require ['clojure.string :as 'str])

(defn load-input []
  (str/split-lines (slurp "./inputs/3.txt")))

(defn build-counter [size]
  (vec (repeat size 0)))

(defn add-vectors [a b]
  (map + a b))

(defn parse-int [x]
  (Integer/parseInt x))

(defn binary-to-vector [input]
  (map parse-int (str/split input #"")))

(defn process-input [input initial-counter]
  (reduce
    (fn [counter binary]
      (map + counter (binary-to-vector binary)))
    initial-counter
    input))

(defn invert-binary-vec [input]
  (map (fn [x]
    (if (= x "1") "0" "1"))
    input))

(defn gamma-from-vec [counter size]
  (map (fn [x]
    (if (> x (/ size 2)) "1" "0"))
    counter))

(defn vec-to-binary [x]
  (str/join #"" x))

(defn binary-to-decimal [x]
  (Integer/parseInt x 2))

(defn part-one []
  (let [input (load-input)
        binary-width (count (first input))
        input-size (count input)
        output-vec (process-input input (build-counter binary-width))
        gamma (gamma-from-vec output-vec input-size)
        epsilon (invert-binary-vec gamma)]
    (println (* (binary-to-decimal (vec-to-binary gamma)) (binary-to-decimal (vec-to-binary epsilon))))))

(defn filter-nth-bit [data n value]
  (filter
    (fn [x]
      (= (str (nth x n)) value))
    data))

(defn mcb [data n]
  (let [width (count (first data))]
    (if (>= (reduce + (map (fn [x] (nth (binary-to-vector x) n)) data)) (/ (count data) 2))
      "1" "0")))

(defn lcb [data n]
  (let [width (count (first data))]
    (if (>= (reduce + (map (fn [x] (nth (binary-to-vector x) n)) data)) (/ (count data) 2))
      "0" "1")))

(defn find-oxygen-rating [input width]
    (reduce
      (fn [acc n]
        (if (= (count acc) 1)
          acc
          (filter-nth-bit acc n (mcb acc n))))
      input
      (vec (range width))))

(defn find-scrubber-rating [input width]
    (reduce
      (fn [acc n]
        (if (= (count acc) 1)
          acc
          (filter-nth-bit acc n (lcb acc n))))
      input
      (vec (range width))))

(defn part-two []
  (let [input (load-input)
        width (count (first input))
        oxygen-rating (first (find-oxygen-rating input width))
        scrubber-rating (first (find-scrubber-rating input width))]
    (println (* (binary-to-decimal (vec-to-binary oxygen-rating)) (binary-to-decimal (vec-to-binary scrubber-rating))))))

;; (part-one)
(part-two)