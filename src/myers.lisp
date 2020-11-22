(in-package :cl-git)

(defparameter *array-size* nil)

(defun slurp-file (filename)
  (let ((file-lines (uiop:read-file-lines filename)))
    (mapcan (lambda (x) (cl-ppcre:split "" x)) file-lines)))

(defclass myers ()
  ((a :initarg :a :accessor :a :initform nil)
   (b :initarg :b :accessor :b :initform nil)))

(defun make-myers (file1 file2)
  (make-instance 'myers
                 :a (slurp-file file1)
                 :b (slurp-file file2)))

(defgeneric diff (algorithm)
  (:documentation "Diff two files using algorithm ALGORITHM."))

(defun normalize (x)
  "Normalize an index X such that if we get a negative index we wrap around."
  (if (minusp x)
      (mod (+ (truncate *array-size* 2)
              (- *array-size* (abs x)))
           *array-size*)
      (mod (+ (truncate *array-size* 2) x)
           *array-size*)))

(defun going-down (k d v)
  (or (= k (- d))
      (and (/= k d)
           (< (aref v (normalize (1- k)))
              (aref v (normalize (1+ k)))))))

(defun shortest-edit (a b)
  (let* ((n (length a))
         (m (length b))
         (max (+ n m))
         (v (make-array (setf *array-size* (+ (* 2 max) 1))))
         x y trace)
    (setf (aref v 1) 0)
    (loop for d from 0 to max do
      (loop for k from (- d) to d by 2 do
        (if (going-down k d v)
            (setf x (aref v (normalize (1+ k))))
            (setf x (1+ (aref v (normalize (1- k))))))
        (setf y (- x k))
        (loop while (and (< x n)
                         (< y m)
                         (string= (nth x a)
                                  (nth y b)))
              do (incf x)
                 (incf y))
        (setf (aref v (normalize k)) x)
        (when (and (>= x n) (>= y m))
          (push (copy-seq v) trace)
          (return-from shortest-edit trace)))
      (push (copy-seq v) trace))))

(defun backtrack (a b)
  (let ((x (length a))
        (y (length b))
        (trace (shortest-edit a b))
        prev-k prev-x prev-y new-trace)
    (loop for d from 0 below (- (length trace) 1)
          for v = (nth d trace)
          for k = (- x y) do
      (if (going-down k d v)
          (setf prev-k (1+ k))
          (setf prev-k (1- k)))
      (setf prev-x (aref v (normalize prev-k))
            prev-y (- prev-x prev-k))
      (loop while (and (> x prev-x)
                       (> y prev-y))
            do
               (push (list (1- x) (1- y) x y) new-trace)
               (decf x)
               (decf y))
      (push (list prev-x prev-y x y) new-trace)
      (setf x prev-x
            y prev-y))
    new-trace))

(defmethod diff ((algorithm myers))
  (with-slots (a b) algorithm
    (let ((trace (backtrack a b))
          diff)
      (dolist (items trace)
        (multiple-value-bind (prev-x prev-y x y) (apply #'values items)
          (let ((a-line (nth prev-x a))
                (b-line (nth prev-y b)))
            (cond
              ((= x prev-x)
               (push `(:ins nil ,b-line) diff))
              ((= y prev-y)
               (push `(:del ,a-line nil) diff))
              (t
               (push `(:eql ,a-line ,b-line) diff))))))
      (nreverse diff))))

(defun myers-diff (file-a file-b)
  (diff (make-myers file-a file-b)))

(defun show-diff (diff &optional (stream t))
  (mapcar (lambda (x)
            (cond
              ((eq (car x) :del)
               (format stream "~a~a~%" '- (cadr x)))
              ((eq (car x) :eql)
               (format stream " ~a~%" (cadr x)))
              ((eq (car x) :ins)
               (format stream "~a~a~%" '+ (caddr x)))))
          diff)
  nil)
