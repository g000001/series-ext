;;;; /home/mc/lisp/work/series-ext/series-ext.asd

(asdf:defsystem #:series-ext
  :serial t
  :depends-on (:series :jp)
  :components ((:file "package")
               (:file "series-ext")))

