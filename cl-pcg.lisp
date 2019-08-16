;;;; cl-pcg.lisp

(in-package #:cl-pcg)

(defstruct pcg
  (state 0 :type (unsigned-byte 64))
  (counter 0 :type (unsigned-byte 64)))

(defun new-rng (&key (seed nil))
  (let* ((start (or seed (get-universal-time)))
         (rng (make-pcg :state start :counter (mod start 12))))
   rng ))

(defun shuffle-rng (rng)
  (let* ((cur-state (pcg-state rng))
         (shift-1 (ash cur-state -18))
         (shift-2 (logxor shift-1 cur-state))
         (xorshifted (ash shift-2 27))
         (rot (ash cur-state -59)))
    (setf (pcg-state rng) (+ (pcg-counter rng) 
                             (* cur-state 6364136223846783005)))
    (logior (ash xorshifted (- rot)) 
            (ash xorshifted (logand (- rot) 31)))))


