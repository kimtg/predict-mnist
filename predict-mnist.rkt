#lang racket

(require racket/file)
(require racket/string)
(require racket/list)

(define (square x)
  (* x x))

(define (sum-sq-err xs ys start)
  (for/sum ([i (in-range start (vector-length xs))])
    (square (- (vector-ref xs i) (vector-ref ys i)))))

(define (main)
  (define file-name-train "mnist_train.csv")
  (define file-name-test "mnist_test.csv")

  ;; train
  (define train-data
    (with-input-from-file file-name-train
      (lambda ()
        (let loop ((lines '()))
          (let ((line (read-line)))
            (if (eof-object? line)
                (reverse lines)
                (let ((split (string-split line ",")))
                  (loop (cons (for/vector ([s (in-list split)]) (string->number s)) lines)))))))))
  (printf "train data loaded. rows: ~a~n" (length train-data))

  ;; test
  (define n-rows 0)
  (define n-correct 0)
  (with-input-from-file file-name-test
    (lambda ()
      (let loop ()
        (let ((line (read-line)))
          (unless (eof-object? line)
            (let ((test (for/vector ([s (in-list (string-split line ","))]) (string->number s))))
              (set! n-rows (+ n-rows 1))
              (define min-error -1)
              (define predicted 0)
              (for ([train (in-list train-data)])
                (define error (sum-sq-err test train 1))
                (when (or (< error min-error) (< min-error 0))
                  (set! predicted (vector-ref train 0))
                  (set! min-error error)))
              (define answer (vector-ref test 0))
              (when (= predicted answer)
                (set! n-correct (+ n-correct 1)))
              (printf "row: ~a predicted: ~a answer: ~a accuracy: ~a~n" n-rows predicted answer (/ (exact->inexact n-correct) n-rows))
              (loop))))))))
  
(main)
