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

(deftest letS*.0001
  series::*series-implicit-map* 
  T )

(deftest letS*.0
  (letS* ((a (series 'a))
          (b (series 'b))
          (c (series 'c))
          (abc (list a b c)))
    (Rlist (subseries abc 0 10)))
  ((A B C) (A B C) (A B C) (A B C) (A B C) (A B C) (A B C) (A B C) (A B C)
   (A B C)))

(deftest letS*.1
  (letS* ((a (series 'a))
          (b (series 'b))
          (c (series 'c)))
    (letS* ((abc (list a b c)))
      (Rlist (subseries abc 0 10))))
  ((A B C) (A B C) (A B C) (A B C) (A B C) (A B C) (A B C) (A B C) (A B C)
   (A B C)))

(deftest letS*.2
  (letS* ((a (series 'a)))
    (letS* ((a (list a)))
      (letS* ((b (series 'b)))
        (letS* ((b (list b)))
          (letS* ((c (series 'c)))
            (letS* ((c (list c)))
              (letS* ((abc (list a b c)))
                (Rlist (subseries abc 0 10)))))))))
  (((A) (B) (C)) ((A) (B) (C)) ((A) (B) (C)) ((A) (B) (C)) ((A) (B) (C))
   ((A) (B) (C)) ((A) (B) (C)) ((A) (B) (C)) ((A) (B) (C)) ((A) (B) (C))))

(deftest letS*.3
  (letS* (((a b c) (list (series 'a)
                         (series 'b)
                         (series 'c)))
          ((a b c) (list c b a)))
    (Rlist (subseries (list a 0 b 2 c 3) 0 10)))
  ((C 0 B 2 A 3) (C 0 B 2 A 3) (C 0 B 2 A 3) (C 0 B 2 A 3) (C 0 B 2 A 3)
   (C 0 B 2 A 3) (C 0 B 2 A 3) (C 0 B 2 A 3) (C 0 B 2 A 3) (C 0 B 2 A 3)))

(deftest letS*.4
  (letS* ((a '(a b c))
          ((a b c) (reverse a)))
    (Rlist (subseries (list a 0 b 2 c 3) 0 10)))
  ((C 0 B 2 A 3) (C 0 B 2 A 3) (C 0 B 2 A 3) (C 0 B 2 A 3) (C 0 B 2 A 3)
   (C 0 B 2 A 3) (C 0 B 2 A 3) (C 0 B 2 A 3) (C 0 B 2 A 3) (C 0 B 2 A 3)) )

(deftest letS*.5
  (letS* ((a '(a b c))
          (b '(1 2 3))
          (ab (concatenate 'list a b)))
    (Rlist (subseries ab 0 10)))
  ((A B C 1 2 3) (A B C 1 2 3) (A B C 1 2 3) (A B C 1 2 3) (A B C 1 2 3)
   (A B C 1 2 3) (A B C 1 2 3) (A B C 1 2 3) (A B C 1 2 3) (A B C 1 2 3)) )

(deftest Rlist.0
  (Rlist (subseries (series 0) 0 5))
  (0 0 0 0 0) )

#|||
???
 (letS* ((a '(a b c))
          (b '(1 2 3))
          (ab (catenate a b)))
  (Rlist (subseries ab 0 10)))
||||#

(progn
  (do-symbols (s :series)
    (shadowing-import s))
  (do-tests))
