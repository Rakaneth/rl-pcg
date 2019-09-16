;;;; cl-pcg.lisp

(in-package #:rl-pcg)

(defparameter *bit-mask-64* #xFFFFFFFFFFFFFFFF)


(defstruct pcg
  (state 0 :type (unsigned-byte 64))
  (counter 0 :type (unsigned-byte 64)))

(defun new-rng (&key (seed nil))
  (let* ((start (or seed (get-universal-time)))
         (rng (make-pcg))
         (counter (mod start 12)))
    (setf (pcg-counter rng) (logior (shl-64 counter 1) 1))
    (shuffle-rng :rng rng)
    (incf (pcg-state rng) counter)
    (shuffle-rng :rng rng)
    rng))

(defun shl-64 (x bits)
  (logand (ash x bits) *bit-mask-64*))

(defun shr-64 (x bits)
  (logand (ash x (- bits)) *bit-mask-64*))

(defun shuffle-rng (&key (rng nil))
  (let* ((cur-rng (or rng *global-rng*))
         (cur-state (pcg-state cur-rng))
         (shift-1 (shr-64 cur-state 18))
         (shift-2 (logxor shift-1 cur-state))
         (xorshifted (shr-64 shift-2 27))
         (rot (shr-64 cur-state 59)))
    (setf (pcg-state cur-rng) (logand *bit-mask-64* 
                                      (+ (pcg-counter cur-rng) 
                                         (* cur-state 6364136223846783005))))
    (logior (shr-64 xorshifted rot)
            (shl-64 xorshifted (logand (- rot) 31)))))

(defun test-rng (times &key (seed nil))
  (let ((rng (new-rng :seed seed))
        (lst '()))
    (dotimes (x times lst)
      (setf lst (cons (shuffle-rng :rng rng) lst)))))

(defun get-int (&key (min-num 0) max-num (rng nil))
  "Gets a random integer between min-num and max-num. Min-num is 0 by default."
  (let* ((threshold (mod #x10000000000000000 max-num))
         (cur-rng (or rng *global-rng*)))
    (loop :for x = (shuffle-rng :rng cur-rng)
       :do (if (>= x threshold)
               (return (+ (mod x max-num) min-num))))))

(defun get-bool (&key (rng nil))
  "Gets a random boolean."
  (let ((cur-rng (or rng *global-rng*)))
    (oddp (shuffle-rng :rng cur-rng))))

(defun get-float (&key (rng nil))
  "Gets a random float in [0, 1)."
  (let* ((-rng (or rng *global-rng*))
         (roll (shuffle-rng :rng -rng)))
    (float (* roll (expt 2 -64)))))

(defun test-get-int (times &key (seed nil))
  (loop :repeat times
     :with rng = (new-rng :seed seed)
     :for roll = (get-int :max-num 100 :rng rng)
     :collect roll))

(defun test-get-bool (times &key (seed nil))
  (loop :repeat times
     :with rng = (new-rng :seed seed)
     :for roll = (get-bool :rng rng)
     :collect roll))


(defun sum-weighted-list (table)
  (loop :for pair in table
       :summing (cdr pair) into total
       :finally (return total)))

(defun get-weighted (table &key (rng nil))
  "Gets a random item from a list of weighted items in the form of (item . weight)."
  (if table 
      (let* ((-rng (or rng *global-rng*))
             (total (sum-weighted-list table))
             (roll (get-int :max-num total :rng -rng)))
        (loop :for (item . weight) in table
              :summing weight into acc
              :do 
                 (when (< roll acc)
                   (return item))))))

(defun get-random-element (s &key (rng nil))
  "Gets a random element from a sequence."
  (let* ((-rng (or rng *global-rng*))
         (roll (get-int :max-num (length s) :rng -rng)))
    (elt s roll)))

(defun shuffle! (sequence &key rng)
  (loop :with l = (length sequence)
        :with temp = nil
        :for i from (1- l) :downto 1
        :for r = (get-int :max-num (1+ i) :rng rng)
        :do (setf temp (elt sequence i)
                  (elt sequence i) (elt sequence r)
                  (elt sequence r) temp)
        :finally (return sequence)))

(defparameter *global-rng* (new-rng))
