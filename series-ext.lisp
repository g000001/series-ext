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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun guess-file-encoding (path &aux (buffer (make-array 8192 :initial-element 0)))
    (declare (dynamic-extent buffer))
    (with-open-file (s path :direction :input :element-type '(unsigned-byte 8))
      #+(or sbcl allegro) (read-sequence buffer s :partial-fill t)
      #-(or sbcl allegro) (read-sequence buffer s)
      (jp:guess buffer :jp))) )

(defmacro defunS (name (&rest lambda-list) &body body)
  (cl:multiple-value-bind (name valnum)
                       (if (consp name)
                              (values-list name)
                              (values name 1))
    `(series::defun ,name (,@lambda-list)
       (declare (optimizable-series-function ,valnum))
       ,@body)))

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

(defmacro mapS (fn &rest Zs)
  `(map-fn t ,fn ,@Zs))

;; Generators [4/4]
(defunS Gsequence (object)
  (series object))

(defunS Glist (list)
  (catenate (scan list) 
            (series nil)))

(defunS Gsublist (list)
  (catenate (scan-sublists list) 
            (series nil)))

(defunS Grange (&optional (first 1) (step-size 1))
  (scan-range :from first :by step-size))

;; Enumerators [8/8]
(defunS Elist (list)
  (scan 'list list))

(defunS Esublists (list)
  (scan-sublists list))

(defunS Elist* (list)
  (do ((L list (cdr L))
       (ans () (cons (car L) ans)))
      ((atom L) (scan
                 (nreverse 
                  (if (null L) 
                      ans
                      (cons L ans)))))))

(defunS Eplist (plist)
  (mapping (((key val) (scan-plist plist)))
    (cons key val)))

(defunS Ealist (alist)
  (mapping (((key val) (scan-alist alist)))
    (cons key val)))

(defunS Erange (first last &optional (step-size 1))
  (scan-range :from first :upto last :by step-size))

(defunS Evector (vector)
  (scan 'vector vector))

(defunS Efile (file)
  (scan-file file))

;; Filters and Terminators [4/4]
(defunS Fselect (Z boolean-Z)
  (choose Z boolean-Z))

(defunS Fpositive (Z)
  (choose-if #'plusp Z))

(defunS Fgreater (Z limit)
  (choose-if (lambda (x) (> x limit)) Z))

(defunS Tselect (Z boolean-Z)
  (until boolean-Z Z))

;; Reducers [/23]
(defunS Rlast (Z)
  (collect-last Z))

(defunS Rignore (Z)
  (collect-ignore Z))

(defunS Rlist (Z)
  (collect 'list Z))

(defunS Rbag (Z)
  (collect 'bag Z))

#|(defunS Rlist* (Z))|#

(defunS Rnconc (Z)
  (collect-nconc Z))

(defunS Rappend (Z)
  (collect-append Z))

(defunS Rset (Z)
  (delete-duplicates (collect 'bag Z)
                     :test #'equal))

(defunS Reqset (Z)
  (delete-duplicates (collect 'bag Z)
                     :test #'eq))

;; Rplist
;; Palist
;; Reqplist
;; Reqalist

(defunS Rvector (Z)
  (collect 'vector Z))

(defunS Rfile (file Z)
  (collect-file file Z))

(defunS Rsum (Z)
  (collect-sum Z))

;; Rsum$
;; Rmax
;; Rmin

(defunS Rcount (Z)
  (collect-length Z))

;; Rand
;; Rand-fast
;; Ror
;; Ror-fast

;; destructuring-bind
#+SBCL
(in-package :sb-kernel)

#+SBCL
(defun parse-defmacroS
       (lambda-list whole-var body name context
                    &key
                    (anonymousp nil)
                    (doc-string-allowed t)
                    ((:environment env-arg-name))
                    ((:default-default *default-default*))
                    (error-fun 'error)
                    (wrap-block t))
  (series::multiple-value-bind (forms declarations documentation)
                               (parse-body body 
                                           :doc-string-allowed doc-string-allowed)
    (series::let ((*arg-tests* ())
                  (*user-lets* ())
                  (*system-lets* ())
                  (*ignorable-vars* ())
                  (*env-var* nil))
      (series::multiple-value-bind (env-arg-used minimum maximum)
                                   (parse-defmacro-lambda-list lambda-list
                                                               whole-var
                                                               name context
                                                               :error-fun error-fun
                                                               :anonymousp anonymousp)
        (values `(series::let* (,@(nreverse *system-lets*))
                   ,@(when *ignorable-vars*
                       `((declare (ignorable ,@*ignorable-vars*))))
                   ,@*arg-tests*
                   (series::let* (,@(when env-arg-used
                                      `((,*env-var* ,env-arg-name)))
                                  ,@(nreverse *user-lets*))
                     ,@declarations
                     ,@(if wrap-block
                           `((block ,(fun-name-block-name name)
                               ,@forms))
                           forms)))
                `(,@(when (and env-arg-name (not env-arg-used))
                      `((declare (ignore ,env-arg-name)))))
                documentation
                minimum
                maximum)))))
#+SBCL
(in-package :sb-int)

#+SBCL
(defmacro-mundanely series::destructuring-bindS (lambda-list expression &body body)
  (let ((whole-name (gensym "WHOLE")))
    (series::multiple-value-bind (body local-decls)
                                 (sb-kernel::parse-defmacroS lambda-list
                                                             whole-name
                                                             body
                                                             nil
                                                             'destructuring-bindS
                                                             :anonymousp t
                                                             :doc-string-allowed nil
                                                             :wrap-block nil)
      (declare (ignore local-decls))
      `(series::let ((,whole-name ,expression))
         ,body))))
#+SBCL
(in-package :series)

;; FIXME compiler-letが上手く処理できていないので上書き
(cl:defun not-expr-like-special-form-p (sym)
  (and (symbolp sym)
       #-series-ansi(special-form-p sym)
       #+series-ansi(special-operator-p sym)
       (not (member sym /expr-like-special-forms/))))

(defmacro with-series-implicit-map (&body body)
  `(compiler-let ((*series-implicit-map* 'T))
     ,@body))

#+SBCL
(defmacro letS*-1 (binds &body body)
  (if (endp binds)
      `(progn ,@body)
      (cl:let ((bind (car binds)))
        (if (consp (car bind))
            `(series::destructuring-bindS ,(car bind) 
                                          ,(cadr bind)
               (letS*-1 ,(cdr binds)
                 ,@body))
            `(series::let ((,(car bind) ,(cadr bind)))
               (letS*-1 ,(cdr binds)
                 ,@body))))))

#+SBCL
(defmacro letS* (binds &body body)
  `(with-series-implicit-map
     (letS*-1 ,binds
       ,@body)))

(export '(collect-firstn defunS Ealist Efile Elist Elist* Eplist Erange Esublists
          Evector Fgreater Fpositive Fselect Glist Grange Gsequence
          Gsublist Lets* Maps Rappend Rbag Rcount Reqset Rfile Rignore
          Rlast Rlist Rnconc Rset Rsum Rvector Scan-File Scan-File-Lines
          Scan-File-Lines-Dwim Scan-Gensyms Tselect
          With-Series-Implicit-Map))


