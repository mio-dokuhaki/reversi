(defpackage :aliquot-sequence
  (:use :cl))

(in-package :aliquot-sequence)

(defun divisors (n)
  (let ((divs (list 1))
        (limit (isqrt n)))
    (do ((i 2 (+ i 1))) ((> i limit) divs)
      (when (zerop (mod n i))
        (push i divs)
        (let ((j (/ n i)))
          (unless (= i j)
            (push j divs)))))))

(defun sum-of-proper-divisors (n)
  (reduce #'+ (divisors n)))

(defun aliquot-sequence (start)
  (let ((current start)
        (history (make-hash-table)))
    (format t "~A" current)
    (loop
      (let ((next (or (gethash current history)
                      (sum-of-proper-divisors current))))
        (setf (gethash current history) next)
        (if (= next current)
          (return (format t " -> Ends with perfect number ~A" next))
          (format nil ""))
        (if (= next 1)
          (return (format t " -> 1~%"))
          (format nil ""))
        (format t " -> ~A" next)
        (setf current next)))))

(defun main ()
  (format t "Please enter a number to start the Aliquot sequence: ")
  (finish-output)
  (let ((input (read)))
    (aliquot-sequence input)))

(main)
