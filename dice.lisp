(in-package #:cl-pcg)

(defgeneric roll (rollable &key))

(defmethod roll ((sides integer) &key (dice 1) (bonus 0) (rng nil) (diff 0) (target 0) (keep nil))
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

(defclass dice-parser ()
  ((state :initform #'state-dice :accessor dp/state)
   (dice-string :initarg :dice-string :accessor dp/dice-string)
   (dice :initform 1 :accessor dp/dice)
   (sides :accessor dp/sides)
   (target :initform 0 :accessor dp/target)
   (diff :initform 0 :accessor dp/diff)
   (rng :initform nil :initarg :rng :accessor dp/rng )
   (bonus :initform 0 :accessor dp/bonus)
   (penalty :initform 0 :accessor dp/penalty)
   (keep :initform 0 :accessor dp/keep)
   (working-vector :initform (make-array 10 :fill-pointer 0 :adjustable t)
                   :accessor dp/working-vector)))

(define-condition dice-parse-error (error) 
  ((dice-string :initarg :dice-string)
   (state :initarg :state)))

(defmacro state-case (dp cur-char &body body)
  `(if (digit-char-p ,cur-char)
       (vector-push-extend ,cur-char (dp/working-vector ,dp))
       (case ,cur-char ,@body (t (error 'dice-parse-error
                                        :dice-string (dp/dice-string ,dp)
                                        :state (dp/state ,dp))))))

(defmacro change-state (dp accessor new-state)
  `(setf (,accessor ,dp) (read-from-string (coerce (dp/working-vector ,dp) 'string))
         (dp/state ,dp) ,new-state
         (dp/working-vector ,dp) (make-array 10 :fill-pointer 0 :adjustable t)))

(defmethod state-dice ((dp dice-parser) cur-char) 
  (state-case dp cur-char 
    (#\d (change-state dp dp/dice #'state-sides))))

(defmethod state-sides ((dp dice-parser) cur-char)
  (state-case dp cur-char
    (#\k (change-state dp dp/sides #'state-keep))
    (#\+ (change-state dp dp/sides #'state-bonus))
    (#\- (change-state dp dp/sides #'state-penalty))
    (#\t (change-state dp dp/sides #'state-target))
    (#\f (change-state dp dp/sides #'state-diff))
    (#\Nul (change-state dp dp/sides #'state-done))))

(defmethod state-keep ((dp dice-parser) cur-char)
  (state-case dp cur-char
    (#\+ (change-state dp dp/keep #'state-bonus))
    (#\- (change-state dp dp/keep #'state-penalty))
    (#\t (change-state dp dp/keep #'state-target))
    (#\Nul (change-state dp dp/keep #'state-done))))

(defmethod state-bonus ((dp dice-parser) cur-char)
  (state-case dp cur-char
    (#\t (change-state dp dp/bonus #'state-target))
    (#\Nul (change-state dp dp/bonus #'state-done))))

(defmethod state-penalty ((dp dice-parser) cur-char)
  (state-case dp cur-char
    (#\t (change-state dp dp/penalty #'state-target))
    (#\Nul (change-state dp dp/penalty #'state-done))))

(defmethod state-target ((dp dice-parser) cur-char)
  (state-case dp cur-char
    (#\Nul (change-state dp dp/target #'state-done))))

(defmethod state-diff ((dp dice-parser) cur-char)
  (state-case dp cur-char
    (#\Nul (change-state dp dp/diff #'state-done))))

(defmethod do-parse ((dp dice-parser))
  (loop :initially (setf (dp/state dp) #'state-dice
                         (dp/keep dp) 0)
        :for c across (dp/dice-string dp)
        :do (funcall (dp/state dp) dp c)
        :finally (return (progn 
                           (funcall (dp/state dp) dp #\Nul)
                           (funcall (dp/state dp) dp)
                           dp))))

(defun parse (dice-string &key rng)
  (do-parse (create-dice-parser dice-string :rng rng)))

(defmethod state-done ((dp dice-parser))
  (if (zerop (dp/keep dp))
      (setf (dp/keep dp) (dp/dice dp))))

(defun create-dice-parser (dice-string &key rng)
  (make-instance 'dice-parser 
                 :dice-string dice-string
                 :rng rng))

(defmethod roll ((dp dice-parser) &key)
  (roll (dp/sides dp) 
        :dice (dp/dice dp)
        :bonus (- (dp/bonus dp) (dp/penalty dp))
        :diff (dp/diff dp)
        :target (dp/target dp)
        :keep (dp/keep dp)
        :rng (dp/rng dp)))

(defmethod roll ((dice-string string) &key rng)
  (roll (parse dice-string :rng rng)))







