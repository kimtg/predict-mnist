;; slower than Racket
(ns racket-to-clojure.core
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

;; Helper function for squaring a number
(defn square [x]
  (* x x))

;; Calculates the sum of squared errors between two vectors from a starting index
(defn sum-sq-err [xs ys start]
  (reduce +
          (for [i (range start (count xs))]
            (square (- (get xs i) (get ys i))))))

;; Calculates the sum of squared errors for specific sample indexes
(defn sum-sq-err-sample [xs ys sample-indexes]
  (reduce +
          (for [i sample-indexes]
            (square (- (get xs i) (get ys i))))))

;; Predicts a label based on training data using an ensemble approach
(defn predict [train-data test nensemble nsample]
  (let [votes (atom {})] ; Use an atom for mutable state (hash map for votes)
    (dotimes [sample-num nensemble] ; Loop nensemble times, sample-num will iterate from 0 to nensemble-1
      (let [predicted (atom 0)
            min-error (atom -1.0) ; Use -1.0 for double comparison
            ;; Generate sample indexes: shuffle a range and take nsample
            sample-indexes (take nsample (shuffle (range 1 (count test))))]
        (doseq [train train-data] ; Iterate over training data
          (let [error (sum-sq-err-sample test train sample-indexes)]
            (when (or (< error @min-error) (< @min-error 0))
              (reset! predicted (get train 0))
              (reset! min-error error))))
        ;; Update votes: increment count for predicted label
        (swap! votes update @predicted (fnil inc 0)))) ; Use update with fnil for safe increment

    ;; Find the key with the maximum vote
    (if (empty? @votes) ; Handle case where votes might be empty
      nil
      (key (apply max-key val @votes)))))

;; Main function to load data, train, and evaluate predictions
(defn -main
  []
  (let [file-name-train "mnist_train.csv"
        file-name-test "mnist_test.csv"]

    ;; Load training data
    (def train-data
      (with-open [reader (io/reader file-name-train)]
        (doall (for [line (line-seq reader)]
                 (mapv (fn [field] (Double/parseDouble field)) (str/split line #",")))))) ; Use mapv for vectors

    (println (format "train data loaded. rows: %d" (count train-data)))

    ;; Evaluate on test data
    (let [n-rows (atom 0)
          n-correct (atom 0)]
      (with-open [reader (io/reader file-name-test)]
        (doseq [line (line-seq reader)]
          (let [test (mapv (fn [field] (Double/parseDouble field)) (str/split line #",")) ; Convert line to vector
                predicted (predict train-data test 20 40)
                answer (get test 0)]
            (swap! n-rows inc)
            (when (= predicted answer)
              (swap! n-correct inc))
            (println (format "row: %d predicted: %s answer: %s accuracy: %.4f"
                             @n-rows predicted answer (double (/ @n-correct @n-rows))))))))))

;; Entry point for the application
(-main)