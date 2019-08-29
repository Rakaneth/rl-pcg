;;;; cl-pcg.asd

(asdf:defsystem #:cl-pcg
  :description "A random number generator of the PCG family for use with roguelikes"
  :author "Quincy West"
  :license  "MIT"
  :version "0.2.2"
  :serial t
  :components ((:file "package")
               (:file "cl-pcg")
               (:file "dice")))

