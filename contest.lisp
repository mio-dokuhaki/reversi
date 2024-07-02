#-swank
(unless (member :child-sbcl *features*)
  (quit
   :recklessly-p t
   :unix-status
   (process-exit-code
    (run-program *runtime-pathname*
                 `("--control-stack-size" "1024MB"
                   "--noinform" "--disable-ldb" "--lose-on-corruption" "--end-runtime-options"
                   "--eval" "(push :child-sbcl *features*)"
                   "--script" ,(namestring *load-pathname*))
                 :output t :error t :input t))))

(ql:quickload "mito")
(ql:quickload "cl-mysql")

(defpackage :myapp
  (:use :cl :mito)
  (:export :connect-to-db
           :create-table
           :insert-records
           :fetch-records
           :update-record
           :delete-record
           :drop-table
           :disconnect-from-db))

(in-package :myapp)

(defun connect-to-db ()
  (handler-case
      (mito:connect-toplevel :mysql :database-name "myapp")
    (error (e)
      (format t "Error connecting to database: ~a~%" e))))

(defun create-table ()
  (handler-case
      (mito:define-table (:users)
        ((id :integer :primary-key :auto-increment :not-null)
         (name :string :not-null)
         (age :integer :not-null)))
    (error (e)
      (format t "Error creating table: ~a~%" e))))

(defun insert-records ()
  (handler-case
      (mito:insert-into :users
        (list :name :age)
        (list "Alice" 20)
        (list "Bob" 30))
    (error (e)
      (format t "Error inserting records: ~a~%" e))))

(defun fetch-records ()
  (handler-case
      (mito:select '(:users))
    (error (e)
      (format t "Error fetching records: ~a~%" e))))

(defun update-record ()
  (handler-case
      (mito:update :users
        (set '(:name "Charlie"))
        (where '(:= :id 1)))
    (error (e)
      (format t "Error updating record: ~a~%" e))))

(defun delete-record ()
  (handler-case
      (mito:delete-from :users
        (where '(:= :id 1)))
    (error (e)
      (format t "Error deleting record: ~a~%" e))))

(defun drop-table ()
  (handler-case
      (mito:drop-table :users)
    (error (e)
      (format t "Error dropping table: ~a~%" e))))

(defun disconnect-from
