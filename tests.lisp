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

#|(deftest letS*.0001
  series::*series-implicit-map* 
  T )|#

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

#|||
???
 (letS* ((a '(a b c))
          (b '(1 2 3))
          (ab (catenate a b)))
  (Rlist (subseries ab 0 10)))
||||#

(deftest Rlist.0
  (Rlist (subseries (series 0) 0 5))
  (0 0 0 0 0) )

(deftest Eplist.0
  (Rlist (Eplist '(a 1 b 2 c 3)))
  ((A . 1) (B . 2) (C . 3)) )

(deftest Ealist.0
  (Rlist (Ealist '((A . 1) (B . 2) (C . 3))))
  ((A . 1) (B . 2) (C . 3)) )

(deftest Erange.0
  (Rlist (Erange 4 8 2))
  (4 6 8) )

(deftest Rvector.0
  (Rvector (subseries (series 0) 0 5))
  #(0 0 0 0 0) )

(deftest Efile.0
  (let ((file #P"/tmp/series-ext-test-Efile.lisp"))
    (unless (probe-file file)
      (with-open-file (out file :direction :output)
        (loop :for i :from 0 :to 100 :do (print i out))))
    (prog1 (Rlist (Efile file))
           (when (probe-file file)
             (delete-file file))))
  (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28
     29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54
     55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80
     81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 100) )

(deftest Eselect.0
  (Rlist (Fselect (Elist '(1 2 3 4))
                  (Elist '(nil T T nil))))
  (NIL T T NIL) )

(deftest Fpositive.0
  (Rlist (Fpositive (Elist '(-1 0 1))))
  (1) )

(deftest Fgreater.0
  (Rlist (Fgreater (Elist '(1 2 3)) 2))
  (3) )

(deftest Tselect.0
  (Rlist (Tselect (Elist '(1 2 3 4))
                (Elist '(nil T T nil))))
  (1) )

(deftest Rlast.0
  (Rlast (Elist '(1 2 3)))
  3 )

(deftest Rlast.1
  (Rlast (Elist '()))
  nil )

(deftest Rignore.0
  (Rignore (Elist '(1 2 3)))
  nil )

(deftest Elist.0
  (Rlist (Elist '(1 2 3)))
  (1 2 3) )

(deftest Rbag.0
  (sort (Rbag (Elist '(1 2 3))) #'<)
  (1 2 3) )

;; Rlist*

(deftest Rnconc.0
  (Rnconc
   (Elist (list
           (list 1 2)
           nil
           (list 3 4))))
  (1 2 3 4) )

(deftest Rappend.0
  (Rappend
   (Elist (list
           (list 1 2)
           nil
           (list 3 4))))
  (1 2 3 4) )

(deftest Rset.0
  (Rset (Elist '(a a (b) (b))))
  ((B) A) )

(deftest Reqset.0
  (Reqset (Elist '(a a (b) (b))))
  ((B) (B) A) )

(progn
  (do-symbols (s :series)
    (shadowing-import s))
  (do-tests))
