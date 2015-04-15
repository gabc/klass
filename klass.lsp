(defpackage :myobj (:use :common-lisp))
(in-package :myobj)

(define-condition method-not-implemented-error (error)
  ((method :initarg :method
	   :reader not-implemented-method)
   (args :initarg :args
	 :reader not-implemented-args))
  (:report (lambda (condition stream)
	     (format stream "~A is not implemented"
		     (not-implemented-method condition)))))

(defmacro defklass (name parent attrs &rest methods)
  (let ((dispatch nil) (meh (gensym)) (par (gensym)))
    (if parent
	(push `(t (funcall ,par ,meh)) dispatch)
	(push `(t (error 'method-not-implemented-error :method ,meh))
	      dispatch))
    (loop for m in methods
       do (push `((eq ,meh ',(car m))
		  (lambda ,(car (cdr m)) (funcall #',(car m) ,@(car (cdr m)))))
		dispatch))
    `(defun ,name ,attrs
       (let ((,par (,@parent)))
	 (labels ,methods
	     (lambda (,meh)
	       (cond ,@dispatch)))))))

(defmacro send (obj mess &rest args)
  `(funcall (funcall ,obj ',mess) ,@args))

(defklass superpoint () (height width)
  (compute () (format t "~A ~A~%"
		      (get-height)
		      (get-width)))
  (get-height () height)
  (get-width () width)
  (set-width (w) (setf width w)
	     (print (+ 10 w))))

(defklass mypoint (superpoint 2 2) (x y)
  (get-x () x)
  (get-y () y)
  (set-x (nx) (setf x nx)))

;;; Old one. No calling other methods
;; (defmacro defklass (name parent attrs &rest methods)
;;   (let ((dispatch nil) (meh (gensym)) (par (gensym)))
;;     (if parent
;; 	(push `(t (funcall ,par ,meh)) dispatch)
;; 	(push `(t (error 'method-not-implemented-error :method ,meh))
;; 	      dispatch))
;;     (loop for m in methods
;;        do (push `((eq ,meh ',(car m))
;; 		  (lambda ,(car (cdr m)) ,(car (cdr (cdr m)))))
;; 		dispatch))
;;     `(defun ,name ,attrs
;;        (let ((,par (,@parent)))
;; 	 (lambda (,meh)
;; 	   (cond ,@dispatch))))))
