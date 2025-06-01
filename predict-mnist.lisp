(require "asdf")

(defun square (x) (* x x))

(defun sum-square-error (xs ys)
  (when (/= (length xs) (length ys)) (return-from sum-square-error -1))
  (loop for x across xs
        for y across ys
        sum (square (- x y))))

(defparameter *inputs* (make-array 0 :fill-pointer t :adjustable t))
(defparameter *outputs* (make-array 0 :fill-pointer t :adjustable t))

(defun train (input output)
  (vector-push-extend input *inputs*)
  (vector-push-extend output *outputs*))

(defun predict (input)
  (let ((best 0)
        (min-error -1))
    (loop for i from 0
          for d across *inputs*
          for error = (sum-square-error input d)
          when (or (< min-error 0) (< error min-error))
          do (setf min-error error
                   best (aref *outputs* i)))
    best))

(defun main ()
  ;; train
  (with-open-file (stream "mnist_train.csv")
    (loop for line = (read-line stream nil)
          while line
          for num-rows from 1
          do
          (let* ((split (uiop:split-string line :separator ","))
                    (input (coerce (mapcar #'parse-integer (cdr split)) 'vector))
                    (output (parse-integer (car split))))
            (train input output)))
    (format t "train data loaded. rows: ~a~%" (length *inputs*)))
  
  ;; test
  (with-open-file (stream "mnist_test.csv")
    (let ((num-correct 0))
      (loop for line = (read-line stream nil)
            while line
            for num-rows from 1
            do
            (let* ((split (uiop:split-string line :separator ","))
                   (input (coerce (mapcar #'parse-integer (cdr split)) 'vector))
                   (output (parse-integer (car split)))
                   ;; predict
                   (predicted (predict input)))
              (when (= predicted output)
                (incf num-correct))
              (format t "row: ~a predicted: ~a answer: ~a accuracy: ~a~%" num-rows predicted output (/ (coerce num-correct 'float) num-rows)))))))

(main)
