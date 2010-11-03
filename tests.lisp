(defpackage :series-ext-test
  (:use :cl :rt :series))

(in-package :series-ext-test)

(do-symbols (s :series)
  (shadowing-import s))

(rem-all-tests)

(deftest Gsequence.0
  (collect
    (subseries (Gsequence nil)
               0 1000))
  #.(make-list 1000))

(deftest Glist.0
  (collect
    (subseries (Glist '(1 2 3))
               0 7))
  (1 2 3 nil nil nil nil))

(deftest Gsublist.0
  (collect
    (subseries (Gsublist '(1 2 3))
               0 7))
  ((1 2 3) (2 3) (3) nil nil nil nil))

(deftest Grange.0
  (collect
    (subseries (Grange 1000 7)
               0 100))
  #.(loop :repeat 100
          :for i :from 1000 :by 7
          :collect i))

(deftest Grange.1
  (collect
    (subseries (Grange)
               0 100))
  #.(loop :repeat 100
          :for i :from 1
          :collect i))

(progn
  (do-symbols (s :series)
    (shadowing-import s))
  (do-tests))


