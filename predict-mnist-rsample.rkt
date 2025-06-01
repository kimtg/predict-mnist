#lang racket

(require racket/file)
(require racket/string)
(require racket/list)

(define (square x)
  (* x x))

(define (sum-sq-err xs ys start)
  (for/sum ([i (in-range start (vector-length xs))])
    (square (- (vector-ref xs i) (vector-ref ys i)))))

(define (sum-sq-err-sample xs ys start sample-indexes)
  (for/sum ([i (in-list sample-indexes)])
    (square (- (vector-ref xs i) (vector-ref ys i)))))

(define (predict train-data test nensemble nsample)
  (define votes (make-hash))
  (for ([sample-num (in-range nensemble)])
    (define predicted 0)
    (define min-error -1)
    (define sample-indexes (take (shuffle (range 1 (vector-length test))) nsample))
    (for ([train (in-list train-data)])
      (define error (sum-sq-err-sample test train 1 sample-indexes))
      (when (or (< error min-error) (< min-error 0))
        (set! predicted (vector-ref train 0))
        (set! min-error error)))
    (hash-set! votes predicted (add1 (hash-ref! votes predicted 0))))
  ;; find max vote
  (define maxk 0)
  (define maxv 0)
  (for ([(k v) (in-hash votes)])
    (when (> v maxv)
      (set! maxk k)
      (set! maxv v)))
  maxk)

(define (main)
  (define file-name-train "mnist_train.csv")
  (define file-name-test "mnist_test.csv")

  ;; train
  (define train-data
    (call-with-input-file file-name-train
      (lambda (in)
        (for/list ([line (in-lines in)])
          (for/vector ([field (in-list (string-split line ","))]) (string->number field))))))
  
  (printf "train data loaded. rows: ~a~n" (length train-data))

  ;; test
  (define n-rows 0)
  (define n-correct 0)
  (call-with-input-file file-name-test
    (lambda (in)
      (for ([line (in-lines in)])
        (let ((test (for/vector ([field (in-list (string-split line ","))]) (string->number field)))) ; convert line to vector
          (set! n-rows (+ n-rows 1))
          (define predicted (predict train-data test 20 40))              
          (define answer (vector-ref test 0))
          (when (= predicted answer)
            (set! n-correct (+ n-correct 1)))
          (printf "row: ~a predicted: ~a answer: ~a accuracy: ~a~n" n-rows predicted answer (/ (exact->inexact n-correct) n-rows)))))))
  
(main)
