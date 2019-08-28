(in-package #:cl-pcg)

(defun roll (sides &key (dice 1) (bonus 0) (rng nil) (diff 0) (target 0) (keep nil))
  (loop :with k = (or keep dice)
        :repeat dice
        :for y = (get-int :min-num 1 :max-num sides :rng rng)
        :collect y into rolls
        :counting (>= y diff) into hits   
        :finally (return (let* ((keeplist (subseq (sort (copy-seq rolls) #'>) 
                                                  0 
                                                  (min k dice)))
                                (total (+ bonus (reduce #'+ keeplist))))
                           (list :total total
                                 :roll (sort rolls #'>)
                                 :success (>= total target)
                                 :hits hits)))))

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


