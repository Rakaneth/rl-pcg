;;;; package.lisp

(defpackage #:cl-pcg
  (:use #:cl)
  (:export 
     :get-int
     :get-bool
     :new-rng
     :test-get-int
     :test-get-bool
     :*global-rng*
     :roll
     :roll-simple)
  (:nicknames :pcg))
