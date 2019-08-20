;;;; package.lisp

(defpackage #:cl-pcg
  (:use #:cl)
  (:export 
     :get-int
     :get-bool
     :get-float
     :new-rng
     :*global-rng*
     :roll
     :roll-simple
     :get-weighted
     :random-element)
  (:nicknames :pcg))
