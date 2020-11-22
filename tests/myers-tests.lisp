(in-package :cl-git-tests)

(deftest myers
  (testing "Myers diff"
    (ok (equal (myers-diff "a" "b")
               '((:DEL "A" NIL)
                 (:DEL "B" NIL)
                 (:EQL "C" "C")
                 (:INS NIL "B")
                 (:EQL "A" "A")
                 (:EQL "B" "B")
                 (:DEL "B" NIL)
                 (:EQL "A" "A")
                 (:INS NIL "C"))))))

(deftest show-diff
  (testing "Printed representation"
    (let ((str (make-array '(0) :element-type 'base-char
                                :fill-pointer 0 :adjustable t)))
      (with-output-to-string (stream str)
        (show-diff (myers-diff "a" "b") stream))
      (ok (string= str (format nil "-A~%-B~% C~%+B~% A~% B~%-B~% A~%+C~%"))))))


