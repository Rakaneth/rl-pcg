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
     :get-random-element)
  (:nicknames :pcg))
