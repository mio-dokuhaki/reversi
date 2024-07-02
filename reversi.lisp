;; load ltk
(ql:quickload '(:ltk :alexandria))

(defpackage :reversi
  (:use :cl :ltk)
  (:import-from #:alexandria
                #:copy-array)
  (:export #:reversi-board))

(in-package :reversi)

;; define variables
(defparameter *board* (make-array '(8 8)))
(defvar *board-stack-back*)
(defvar *board-stack-forward*)

(defparameter cell-size 55)
(defparameter margin-size 10)

(defconstant +black+ 1)
(defconstant +white+ 2)

(defun init-board! (board)
  (setf *board-stack-back* nil)
  (setf *board-stack-forward* nil)
  (loop for i from 0 below (array-dimension board 0) do
    (loop for j from 0 below (array-dimension board 1) do
      (cond ((and (= i (1- (/ (array-dimension board 0) 2)))
		  (= j (1- (/ (array-dimension board 1) 2))))
	     (setf (aref board i j) +black+))
	    ((and (= i (/ (array-dimension board 0) 2))
		  (= j (/ (array-dimension board 1) 2)))
	     (setf (aref board i j) +black+))
	    ((and (= i (1- (/ (array-dimension board 0) 2)))
		  (= j (/ (array-dimension board 1) 2)))
	     (setf (aref board i j) +white+))
	    ((and (= i (/ (array-dimension board 0) 2))
		  (= j (1- (/ (array-dimension board 1) 2))))
	     (setf (aref board i j) +white+))
	    (t (setf (aref board i j) 0))))))

(defun opposite (self-b/w)
  (if (= self-b/w +black+)
      +white+
      +black+))

(defun h-line-plus-check (board i j self-b/w &key (cnt 0))
  (cond ((= j (1- (array-dimension board 1))) nil)
	((= (aref board i (1+ j)) 0) nil)
	((= (aref board i (1+ j)) (opposite self-b/w))
	 (h-line-plus-check board i (1+ j) self-b/w :cnt (1+ cnt)))
	((= (aref board i (1+ j)) self-b/w) cnt)))

(defun h-line-plus-reverse! (board i j self-b/w)
  (when (= (aref board i (1+ j)) (opposite self-b/w))
    (setf (aref board i (1+ j)) self-b/w)
    (h-line-plus-reverse! board i (1+ j) self-b/w)))

(defun h-line-minus-check (board i j self-b/w &key (cnt 0))
  (cond ((= j 0) nil)
	((= (aref board i (1- j)) 0) nil)
	((= (aref board i (1- j)) (opposite self-b/w))
	 (h-line-minus-check board i (1- j) self-b/w :cnt (1+ cnt)))
	((= (aref board i (1- j)) self-b/w) cnt)))

(defun h-line-minus-reverse! (board i j self-b/w)
  (when (= (aref board i (1- j)) (opposite self-b/w))
    (setf (aref board i (1- j)) self-b/w)
    (h-line-minus-reverse! board i (1- j) self-b/w)))

(defun v-line-plus-check (board i j self-b/w &key (cnt 0))
  (cond ((= i (1- (array-dimension board 0))) nil)
	((= (aref board (1+ i) j) 0) nil)
	((= (aref board (1+ i) j) (opposite self-b/w))
	 (v-line-plus-check board (1+ i) j self-b/w :cnt (1+ cnt)))
	((= (aref board (1+ i) j) self-b/w) cnt)))

(defun v-line-plus-reverse! (board i j self-b/w)
  (when (= (aref board (1+ i) j) (opposite self-b/w))
    (setf (aref board (1+ i) j) self-b/w)
    (v-line-plus-reverse! board (1+ i) j self-b/w)))

(defun v-line-minus-check (board i j self-b/w &key (cnt 0))
  (cond ((= i 0) nil)
	((= (aref board (1- i) j) 0) nil)
	((= (aref board (1- i) j) (opposite self-b/w))
	 (v-line-minus-check board (1- i) j self-b/w :cnt (1+ cnt)))
	((= (aref board (1- i) j) self-b/w) cnt)))

(defun v-line-minus-reverse! (board i j self-b/w)
  (when (= (aref board (1- i) j) (opposite self-b/w))
    (setf (aref board (1- i) j) self-b/w)
    (v-line-minus-reverse! board (1- i) j self-b/w)))

(defun right-naname-plus-check (board i j self-b/w &key (cnt 0))
  (cond ((or (= i (1- (array-dimension board 0)))
	     (= j (1- (array-dimension board 1)))) nil)
	((= (aref board (1+ i) (1+ j)) 0) nil)
	((= (aref board (1+ i) (1+ j)) (opposite self-b/w))
	 (right-naname-plus-check board (1+ i) (1+ j) self-b/w :cnt (1+ cnt)))
	((= (aref board (1+ i) (1+ j)) self-b/w) cnt)))

(defun right-naname-plus-reverse! (board i j self-b/w)
  (when (= (aref board (1+ i) (1+ j)) (opposite self-b/w))
    (setf (aref board (1+ i) (1+ j)) self-b/w)
    (right-naname-plus-reverse! board (1+ i) (1+ j) self-b/w)))

(defun right-naname-minus-check (board i j self-b/w &key (cnt 0))
  (cond ((or (= i 0)
	     (= j 0)) nil)
	((= (aref board (1- i) (1- j)) 0) nil)
	((= (aref board (1- i) (1- j)) (opposite self-b/w))
	 (right-naname-minus-check board (1- i) (1- j) self-b/w :cnt (1+ cnt)))
	((= (aref board (1- i) (1- j)) self-b/w) cnt)))

(defun right-naname-minus-reverse! (board i j self-b/w)
  (when (= (aref board (1- i) (1- j)) (opposite self-b/w))
    (setf (aref board (1- i) (1- j)) self-b/w)
    (right-naname-minus-reverse! board (1- i) (1- j) self-b/w)))

(defun left-naname-plus-check (board i j self-b/w &key (cnt 0))
  (cond ((or (= i 0)
	     (= j (1- (array-dimension board 1)))) nil)
	((= (aref board (1- i) (1+ j)) 0) nil)
	((= (aref board (1- i) (1+ j)) (opposite self-b/w))
	 (left-naname-plus-check board (1- i) (1+ j) self-b/w :cnt (1+ cnt)))
	((= (aref board (1- i) (1+ j)) self-b/w) cnt)))

(defun left-naname-plus-reverse! (board i j self-b/w)
  (when (= (aref board (1- i) (1+ j)) (opposite self-b/w))
    (setf (aref board (1- i) (1+ j)) self-b/w)
    (left-naname-plus-reverse! board (1- i) (1+ j) self-b/w)))

(defun left-naname-minus-check (board i j self-b/w &key (cnt 0))
  (cond ((or (= i (1- (array-dimension board 0)))
	     (= j 0)) nil)
	((= (aref board (1+ i) (1- j)) 0) nil)
	((= (aref board (1+ i) (1- j)) (opposite self-b/w))
	 (left-naname-minus-check board (1+ i) (1- j) self-b/w :cnt (1+ cnt)))
	((= (aref board (1+ i) (1- j)) self-b/w) cnt)))

(defun left-naname-minus-reverse! (board i j self-b/w)
  (when (= (aref board (1+ i) (1- j)) (opposite self-b/w))
    (setf (aref board (1+ i) (1- j)) self-b/w)
    (left-naname-minus-reverse! board (1+ i) (1- j) self-b/w)))

(defun put-stone! (board i j b/w)
  (let ((h-line-plus-check?        (and (h-line-plus-check        board i j b/w)
				        (> (h-line-plus-check        board i j b/w) 0)))
	(h-line-minus-check?       (and (h-line-minus-check       board i j b/w)
				        (> (h-line-minus-check       board i j b/w) 0)))
	(v-line-plus-check?        (and (v-line-plus-check        board i j b/w)
				        (> (v-line-plus-check        board i j b/w) 0)))
	(v-line-minus-check?       (and (v-line-minus-check       board i j b/w)
				        (> (v-line-minus-check       board i j b/w) 0)))
	(right-naname-plus-check?  (and (right-naname-plus-check  board i j b/w)
				        (> (right-naname-plus-check  board i j b/w) 0)))
	(right-naname-minus-check? (and (right-naname-minus-check board i j b/w)
				        (> (right-naname-minus-check board i j b/w) 0)))
	(left-naname-plus-check?   (and (left-naname-plus-check   board i j b/w)
				        (> (left-naname-plus-check   board i j b/w) 0)))
	(left-naname-minus-check?  (and (left-naname-minus-check  board i j b/w)
				        (> (left-naname-minus-check  board i j b/w) 0))))

    (if h-line-plus-check?  (h-line-plus-reverse! board i j b/w))
    (if h-line-minus-check? (h-line-minus-reverse! board i j b/w))
    (if v-line-plus-check?  (v-line-plus-reverse! board i j b/w))
    (if v-line-minus-check? (v-line-minus-reverse! board i j b/w))
    (if right-naname-plus-check?  (right-naname-plus-reverse! board i j b/w))
    (if right-naname-minus-check? (right-naname-minus-reverse! board i j b/w))
    (if left-naname-plus-check?   (left-naname-plus-reverse! board i j b/w))
    (if left-naname-minus-check?  (left-naname-minus-reverse! board i j b/w))

    (if (and
	 (zerop (aref board i j))
	 (or h-line-plus-check?       h-line-minus-check?
	     v-line-plus-check?       v-line-minus-check?
	     right-naname-plus-check? right-naname-minus-check?
	     left-naname-plus-check?  left-naname-minus-check?  ))
	(progn (setf (aref board i j) b/w)
	       b/w)
	nil)))

(defun put-able? (board i j b/w)
  (let ((h-line-plus-check?        (and (h-line-plus-check        board i j b/w)
				        (> (h-line-plus-check        board i j b/w) 0)))
	(h-line-minus-check?       (and (h-line-minus-check       board i j b/w)
				        (> (h-line-minus-check       board i j b/w) 0)))
	(v-line-plus-check?        (and (v-line-plus-check        board i j b/w)
				        (> (v-line-plus-check        board i j b/w) 0)))
	(v-line-minus-check?       (and (v-line-minus-check       board i j b/w)
				        (> (v-line-minus-check       board i j b/w) 0)))
	(right-naname-plus-check?  (and (right-naname-plus-check  board i j b/w)
				        (> (right-naname-plus-check  board i j b/w) 0)))
	(right-naname-minus-check? (and (right-naname-minus-check board i j b/w)
				        (> (right-naname-minus-check board i j b/w) 0)))
	(left-naname-plus-check?   (and (left-naname-plus-check   board i j b/w)
				        (> (left-naname-plus-check   board i j b/w) 0)))
	(left-naname-minus-check?  (and (left-naname-minus-check  board i j b/w)
				        (> (left-naname-minus-check  board i j b/w) 0))))

    (and
     (zerop (aref board i j))
     (or h-line-plus-check?       h-line-minus-check?
	 v-line-plus-check?       v-line-minus-check?
	 right-naname-plus-check? right-naname-minus-check?
	 left-naname-plus-check?  left-naname-minus-check?  ))))

(defun print-board (board)
  (format t "   ")
  (loop for j from 0 below (array-dimension board 1) do
    (format t "~A " j))
  (format t "~%~%")

  (loop for i from 0 below (array-dimension board 0) do
    (format t "~A  " i)
    (loop for j from 0 below (array-dimension board 1) do
      (format t "~A " (aref board i j)))
    (format t "~%")))

(defun create-board (canvas board margin-size cell-size stone-color)
  (loop for i from 0 below (array-dimension board 0) do
    (loop for j from 0 below (array-dimension board 1) do
      (if (put-able? board i j stone-color)
	  (itemconfigure
	   canvas
	   (create-rectangle canvas
			     (+ margin-size (* j cell-size))
			     (+ margin-size (* i cell-size))
			     (+ margin-size (* j cell-size) cell-size)
			     (+ margin-size (* i cell-size) cell-size))
	   "fill" "#008000")
	  (itemconfigure
	   canvas
	   (create-rectangle canvas
			     (+ margin-size (* j cell-size))
			     (+ margin-size (* i cell-size))
			     (+ margin-size (* j cell-size) cell-size)
			     (+ margin-size (* i cell-size) cell-size))
	   "fill" "dark green")))))

(defun reflect-board (canvas board margin-size cell-size)
  (loop for i from 0 below (array-dimension board 0) do
    (loop for j from 0 below (array-dimension board 1) do
      (cond ((= (aref board i j) +black+)
	     (itemconfigure
	      canvas
	      (create-oval canvas
			   (+ margin-size (* j cell-size) (/ (- cell-size (* cell-size 0.8)) 2))
			   (+ margin-size (* i cell-size) (/ (- cell-size (* cell-size 0.8)) 2))
			   (+ margin-size (* j cell-size)
			      (- cell-size (/ (- cell-size (* cell-size 0.8)) 2)))
			   (+ margin-size (* i cell-size)
			      (- cell-size (/ (- cell-size (* cell-size 0.8)) 2))))
	      "fill" "black"))
	    ((= (aref board i j) +white+)
	     (itemconfigure
	      canvas
	      (create-oval canvas
			   (+ margin-size (* j cell-size) (/ (- cell-size (* cell-size 0.8)) 2))
			   (+ margin-size (* i cell-size) (/ (- cell-size (* cell-size 0.8)) 2))
			   (+ margin-size (* j cell-size)
			      (- cell-size (/ (- cell-size (* cell-size 0.8)) 2)))
			   (+ margin-size (* i cell-size)
			      (- cell-size (/ (- cell-size (* cell-size 0.8)) 2))))
	      "fill" "white"))))))

(defun yes-or-no-dialog (message)
  (let ((response (make-instance 'tk-message-box
                                 :type :yesno
                                 :message message)))
    (string= response "yes")))

(defun board-full? (board)
  (loop for i from 0 below (array-dimension board 0) always
    (loop for j from 0 below (array-dimension board 1) always
      (not (zerop (aref board i j))))))

(defun check-winner (board tb)
  (let ((black-count 0)
        (white-count 0))
    (loop for i from 0 below (array-dimension board 0) do
      (loop for j from 0 below (array-dimension board 1) do
        (cond ((= (aref board i j) +black+) (incf black-count))
              ((= (aref board i j) +white+) (incf white-count)))))
    (clear-text tb)
    (append-text tb (format nil "BLACK: ~A WHITE: ~A~%" black-count white-count))
    (cond ((> black-count white-count)
           (append-text tb "BLACK wins!"))
          ((< black-count white-count)
           (append-text tb "WHITE wins!"))
          (t
           (append-text tb "It's a tie!")))))

(defun display-board (board)
  (with-ltk ()
    (let* ((board-frame (make-instance 'frame))
           (canvas (make-canvas board-frame
                                :width  (+ (* (array-dimension board 1) cell-size)
                                           (* 2 margin-size))
                                :height (+ (* (array-dimension board 0) cell-size)
                                           (* 2 margin-size))))
           (stone-color +black+)
           (button-frame (make-instance 'frame))
           (tb (make-text button-frame :width nil :height 2))
           (back-b (make-instance 'button :master button-frame :text "Back"
                                  :command (lambda ()
                                             (when *board-stack-back*
                                               (push (copy-array board) *board-stack-forward*)
                                               (setf board (pop *board-stack-back*))
                                               (clear-text tb)
                                               (if (= stone-color +black+)
                                                   (append-text tb (format nil "WHITE's turn~%"))
                                                   (append-text tb (format nil "BLACK's turn~%")))
                                               (if (= stone-color +black+)
                                                   (setf stone-color +white+)
                                                   (setf stone-color +black+))
                                               (clear canvas)
                                               (create-board canvas board margin-size cell-size stone-color)
                                               (reflect-board canvas board margin-size cell-size)))))
           (forward-b (make-instance 'button :master button-frame :text "Forward"
                                     :command (lambda ()
                                                (when *board-stack-forward*
                                                  (push (copy-array board) *board-stack-back*)
                                                  (setf board (pop *board-stack-forward*))
                                                  (clear-text tb)
                                                  (if (= stone-color +black+)
                                                      (append-text tb (format nil "WHITE's turn~%"))
                                                      (append-text tb (format nil "BLACK's turn~%")))
                                                  (if (= stone-color +black+)
                                                      (setf stone-color +white+)
                                                      (setf stone-color +black+))
                                                  (clear canvas)
                                                  (create-board canvas board margin-size cell-size stone-color)
                                                  (reflect-board canvas board margin-size cell-size)))))
           (pass-b (make-instance 'button :master button-frame :text "Pass"
                                  :command (lambda ()
                                             (push (copy-array board) *board-stack-back*)
                                             (setf *board-stack-forward* nil)
                                             (clear-text tb)
                                             (if (= stone-color +black+)
                                                 (append-text tb (format nil "WHITE's turn~%"))
                                                 (append-text tb (format nil "BLACK's turn~%")))
                                             (if (= stone-color +black+)
                                                 (setf stone-color +white+)
                                                 (setf stone-color +black+))
                                             (clear canvas)
                                             (create-board canvas board margin-size cell-size stone-color)
                                             (reflect-board canvas board margin-size cell-size)))))
      (pack board-frame)
      (pack canvas :side :left)
      (pack button-frame)
      (pack tb :side :left)
      (pack back-b :side :left)
      (pack forward-b :side :left)
      (pack pass-b :side :left)

      (create-board canvas board margin-size cell-size stone-color)
      (reflect-board canvas board margin-size cell-size)
      (append-text tb (format nil "BLACK's turn~%"))
      (append-text tb (format nil "BLACK: 2 WHITE: 2"))

      (bind canvas "<ButtonPress-1>"
            (lambda (evt)
              (let ((i (truncate (- (event-y evt) margin-size) cell-size))
                    (j (truncate (- (event-x evt) margin-size) cell-size)))

                (push (copy-array board) *board-stack-back*)
                (setf *board-stack-forward* nil)

                (when (put-stone! board i j stone-color)
                  ;; (print-board board) ;debug
                  (clear canvas)
                  (create-board canvas board margin-size cell-size
                                (if (= stone-color +black+) +white+ +black+))
                  (reflect-board canvas board margin-size cell-size)

                  (let ((b-cnt 0) (w-cnt 0))
                    (loop for i from 0 below (array-dimension board 0) do
                      (loop for j from 0 below (array-dimension board 1) do
                        (cond ((= (aref board i j) +black+) (incf b-cnt))
                              ((= (aref board i j) +white+) (incf w-cnt)))))
                    ;; display text box
                    (clear-text tb)
                    (if (= stone-color +black+)
                        (append-text tb (format nil "WHITE's turn~%"))
                        (append-text tb (format nil "BLACK's turn~%")))
                    (append-text tb (format nil "BLACK: ~A WHITE: ~A" b-cnt w-cnt))

                    ;; change stone-color
                    (if (= stone-color +black+)
                        (setf stone-color +white+)
                        (setf stone-color +black+))

                    ;; Check for game end and winner
                    (when (or (zerop b-cnt) (zerop w-cnt) (board-full? board))
                      (check-winner board tb))))))))))

(defun reversi-board ()
  (init-board! *board*)
  (display-board *board*))

(reversi-board)
