(in-package #:rl-pcg)

(defun get-random-bytes (&key rng)
  (loop :for i from 0 below 16
        :for b = (get-int :max-num 256 :rng rng)
        :when (= i 6)
          :do (setf b (logior #x40 (logand b #xF)))
        :end
        :when (= i 8)
          :do (setf b (logior #x80 (logand b #x3F)))
        :end
        :collect b into bytes
        :finally (return (format nil "~{~2,'0x~}" bytes))))

(defun get-uuid (&key rng)
  (let* ((raw (get-random-bytes :rng rng))
         (first8 (subseq raw 0 8))
         (second4 (subseq raw 8 12))
         (third4 (subseq raw 12 16))
         (fourth4 (subseq raw 16 20))
         (final (subseq raw 20)))
    (format nil "~a-~a-~a-~a-~a" first8 second4 third4 fourth4 final)))
