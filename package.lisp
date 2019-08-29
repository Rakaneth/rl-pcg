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
     :roll-total
     :roll-success
     :roll-hits
     :get-weighted
     :get-random-element
     :create-dice-parser
     :parse)
  (:nicknames :pcg))
