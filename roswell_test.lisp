(defpackage :test
  (:use :cl))

(in-package :test)

(defun main ()
  (let* ((a (read))
         (b (read)))
    (format t "~d~%" (* a b))))
