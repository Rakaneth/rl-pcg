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
  (let* ((real-range (1+ (- max-num min-num)))
         (threshold (mod #x10000000000000000 real-range))
         (cur-rng (or rng *global-rng*)))
    (loop :with x = (shuffle-rng :rng cur-rng)
          :do (if (>= x threshold)
                  (return (+ (mod x real-range)
                             min-num))))))

(defun get-bool (&key (rng nil))
  (let ((cur-rng (or rng *global-rng*)))
    (oddp (shuffle-rng :rng cur-rng))))

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

(defparameter *global-rng* (new-rng))
