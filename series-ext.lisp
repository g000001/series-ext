;;;; series-ext/series-ext.lisp

(in-package #:series-ext)

(defvar *original-functions* () )

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun keep-original-function (sym)
    (pushnew (cons sym (symbol-function sym))
             *original-functions*
             :key #'car)))

(keep-original-function 'series::scan-file)

(in-package :series)

(defun guess-file-encoding (path &aux (buffer (make-array 8192 :initial-element 0)))
  (declare (dynamic-extent buffer))
  (with-open-file (s path :direction :input :element-type '(unsigned-byte 8))
    #+(or sbcl allegro) (read-sequence buffer s :partial-fill t)
    #-(or sbcl allegro) (read-sequence buffer s)
    (jp:guess buffer :jp)))

;; Over write
(defS scan-file (name &optional (reader #'read) &key (external-format :default))
    "(scan-file file-name &optional (reader #'read)

SCAN-FILE opens the file named by the string FILE-NAME and applies the
function READER to it repeatedly until the end of the file is
reached. READER must accept the standard input function arguments
input-stream, eof-error-p, and eof-value as its arguments. (For
instance, reader can be read, read-preserving-white-space, read-line,
or read-char.) If omitted, READER defaults to READ. SCAN-FILE returns
a series of the values returned by READER, up to but not including the
value returned when the end of the file is reached. The file is
correctly closed, even if an abort occurs. "
  (fragl ((name) (reader)) ((items t))
	 ((items t)
	  (lastcons cons (list nil))
	  (lst list))
	 ()
         ((setq lst lastcons)
	  (with-open-file (f name :direction :input :external-format external-format)
            (cl:let ((done (list nil)))
              (loop              
                (cl:let ((item (cl:funcall reader f nil done)))
                  (when (eq item done)
                    (return nil))
		  (setq lastcons (setf (cdr lastcons) (cons item nil)))))))
	  (setq lst (cdr lst)))
         ((if (null lst) (go end))
          (setq items (car lst))
          (setq lst (cdr lst)))
	 ()
	 ()
	 :context) ; file can change
                   ; Movement should only be allowed if no unknown functions
                   ; and constrained by sync and file operations
 :optimizer
  (apply-literal-frag
    (cl:let ((file (new-var 'file)))
      `((((reader)) ((items t))
	 ((items t) (done t (list nil)))
	 ()
         ()
         ((if (eq (setq items (cl:funcall reader ,file nil done)) done)
              (go end)))
	 ()
         ((#'(lambda (code)
              (list 'with-open-file
                    '(,file ,name :direction :input :external-format ,external-format)
                    code)) :loop))
	 :context)
	,reader))))

;; New
(export 'scan-file-lines)
(defS scan-file-lines (name &key (external-format :default))
    "(scan-file-lines file-name)"
  (fragl ((name)) ((items t))
	 ((items t)
	  (lastcons cons (list nil))
	  (lst list))
	 ()
         ((setq lst lastcons)
	  (with-open-file (f name :direction :input :external-format external-format)
            (cl:let ((done (list nil)))
              (loop              
                (cl:let ((item (read-line f nil done)))
                  (when (eq item done)
                    (return nil))
		  (setq lastcons (setf (cdr lastcons) (cons item nil)))))))
	  (setq lst (cdr lst)))
         ((if (null lst) (go end))
          (setq items (car lst))
          (setq lst (cdr lst)))
	 ()
	 ()
	 :context) ; file can change
                   ; Movement should only be allowed if no unknown functions
                   ; and constrained by sync and file operations
 :optimizer
  (apply-literal-frag
    (cl:let ((file (new-var 'file)))
      `((() 
         ((items t))
	 ((items t) (done t (list nil)))
	 ()
         ()
         ((if (eq (setq items (read-line ,file nil done)) done)
              (go end)))
	 ()
         ((#'(lambda (code)
              (list 'with-open-file
                    '(,file ,name :direction :input :external-format ,external-format)
                    code)) :loop))
	 :context)
	#'read-line))))

#|| 

 (collect (scan-file-lines "/etc/passwd"))

 (apply-literal-frag
 (cl:let ((file (new-var 'file)))
   `((() ((items t))
      ((items t) (done t (list nil)))
      ()
      ()
      ;; loop
      ((if (eq (setq items (read-line ,file nil done)) done)
           (go end)))
      ()
      ;; with-
      ((
        ;; 1
        #'(lambda (code)
            (list 'with-open-file
                  '(,file ,name :direction :input :external-format (guess-file-encoding ,name))
                  code)) 
        ;; 2
          :loop))
      :context)
     read-line)))

 (setq *OPTIMIZE-SERIES-EXPRESSIONS* t)

 (CL:LET* (#:ITEMS-3483
          (#:DONE-3484 (LIST NIL))
          (#:LASTCONS-3480 (LIST NIL))
          (#:LST-3481 #:LASTCONS-3480))
  (DECLARE (TYPE CONS #:LASTCONS-3480)
           (TYPE LIST #:LST-3481))
  ;; with-
  (WITH-OPEN-FILE (#:FILE-3482 "/etc/passwd" :DIRECTION :INPUT :EXTERNAL-FORMAT (GUESS-FILE-ENCODING "/etc/passwd"))
    (TAGBODY
     #:LL-3485
      ;; loop
      (IF (EQ (SETQ #:ITEMS-3483 (READ-LINE #:FILE-3482 NIL #:DONE-3484))
              #:DONE-3484)
          (GO END))
      ;; tconsing
      (SETQ #:LASTCONS-3480
              (SETF (CDR #:LASTCONS-3480) (CONS #:ITEMS-3483 NIL)))
      (GO #:LL-3485)
     END))
  ;; tconsなのでcdrを返す
  (CDR #:LST-3481))

=>
 (cl:let* ((done (list nil)))
  (with-open-file (file "/etc/passwd" :direction :input :external-format (guess-file-encoding "/etc/passwd"))
    (loop for items := (read-line file nil done) :unless (eq items done)
          :collect items)))
||#

;; Add
(defmacro scan-file-lines-dwim (file)
  `(scan-file-lines ,file
                    :external-format (guess-file-encoding ,file)))

;; Add
(defmacro collect-firstn (num series)
  `(collect (subseries ,series 0 ,num)))


;; Add
;; (#Mgensym)でOKらしい…
(defmacro scan-gensyms (&optional (thing "G"))
  `(scan-fn 'symbol
            (lambda () (gensym ,thing))
            #+SBCL (lambda ()
                     (declare (optimize (speed 3) (safety 0)))
                     (gensym ,thing))
            #-SBCL (lambda (x)
                     (declare (ignore x))
                     (gensym ,thing))))

;; Add
(defS *scan-dlist (alist &optional (test #'eql))
  "(scan-dlist pattern list &optional (test #'eql))

Creates two series containing the keys and values in an alist."
  (fragl ((alist) (test))
         ((keys t) (values t))
         ((alistptr list alist)
	  (keys t) (values t) (parent list))
         ((keys (setf (car parent) *alt*) parent)
          (values (setf (cdr parent) *alt*) parent))
         ()
         (L (if (null alistptr) (go end))
            (setq parent (car alistptr))
            (setq alistptr (cdr alistptr))
            (if (or (null parent)
                    (not (eq parent (assoc (car parent) alist :test test))))
                (go L))
            (setq keys (car parent))
            (setq values (cdr parent)))
	 ()
	 ()
	 :mutable))


;; The Basic Sequence Functions
(defun mapS (fn &rest Zs)
  (apply #'map-fn t fn Zs))

;; Generators
(defun Gsequence (object)
  (series object))

;; Enumerators
(defun Elist (list)
  (scan 'list list))

(defun Esublists (list)
  (scan-sublists list))

(defun Elist* (list)
  (do ((L list (cdr L))
       (ans () (cons (car L) ans)))
      ((atom L) (scan
                 (nreverse 
                  (if (null L) 
                      ans
                      (cons L ans)))))))

(defun Eplist (plist)
  (scan-plist plist))

(defun Ealist (alist)
  (scan-alist alist))

(defun  Erange (first last &optional (step-size 1))
  (scan-range :from first :upto last :by step-size))

(defun Rvector (Z)
  (collect 'vector Z))

(defun Efile (file)
  (scan-file file))

;; Filters and Terminators
(defun Fpositive (Z)
  (choose-if #'plusp Z))

;; Reducers
(defun Rlist (Z)
  (collect 'list Z))

#||
 (defun pairwise-max (list1 list2)
  (Rlist (mapS #'max (Elist list1) (Elist list2))))

 ;; とも書けるらしい → macroか
 (defun pairwise-max (list1 list2)
   (Rlist (max (Elist list1) (Elist list2))))
||#

(defun Rfile (file Z)
  (collect-file file Z))

(defun Rsum (Z)
  (collect-sum Z))

(defun Rcount (Z)
  (collect-length Z))

(defun Rlast (Z)
  (collect-last Z))

(export '(collect-firstn ealist efile elist elist* eplist erange esublists fpositive
          gsequence guess-file-encoding maps rcount rfile rlast rlist
          rsum rvector scan-file-lines-dwim scan-gensyms))

