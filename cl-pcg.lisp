;;;; cl-pcg.lisp

(in-package #:cl-pcg)

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

(defun roll (sides &key (dice 1) (bonus 0) (rng nil) (diff 0) (target 0) (keep nil))
  (loop :repeat dice
        :with k = (or keep dice)
     :for y = (get-int :min-num 1 :max-num sides :rng rng)
     :collect y into rolls
     :summing y into total
     :counting (>= y diff) into hits   
     :finally (return (list :total (+ total bonus) 
                            :rolls (subseq (sort rolls #'>) 0 (min k dice))
                            :hits hits
                            :success (>= total target)))))

(defun sum-weighted-list (table)
  (loop :for pair in table
       :summing (cdr pair) into total
       :finally (return total)))

(defun get-weighted (table &key (rng nil))
  "Gets a random item from a list of weighted items in the form of (item . weight)."
  (let* ((-rng (or rng *global-rng*))
         (total (sum-weighted-list table))
         (roll (get-int :max-num total :rng -rng)))
    ;;;; (format t "Roll is ~d~%" roll)
    (loop :for (item . weight) in table
          :summing weight into acc
          :do 
             ;;;; (format t "Item ~a with weight ~a; Acc is ~d~%" item weight acc)
             (when (< roll acc) 
               ;;;; (terpri)
               (return item)))))

(defun get-random-element (s &key (rng nil))
  "Gets a random element from a sequence."
  (let* ((-rng (or rng *global-rng*))
         (roll (get-int :max-num (length s) :rng -rng)))
    (nth roll s)))

(defmacro extract-roll-function (key)
  (let* ((package (symbol-package 'roll))
         (fn-name (intern (format nil "ROLL-~a" key) package)))
    `(defun ,fn-name (sides &key (dice 1) (bonus 0) (diff 0) (target 0) (rng nil) (keep nil))
       (let* ((-roll (roll sides 
                           :dice dice
                           :bonus bonus
                           :diff diff
                           :target target
                           :rng rng
                           :keep keep))
              (result (getf -roll ,key)))
         (values result -roll)))))

(extract-roll-function :total)
(extract-roll-function :hits)
(extract-roll-function :success)

(defparameter *global-rng* (new-rng))
