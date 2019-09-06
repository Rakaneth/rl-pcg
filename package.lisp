;;;; package.lisp

(defpackage #:rl-pcg
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
     :parse
     :get-uuid
     :shuffle!)
  (:nicknames :pcg))
