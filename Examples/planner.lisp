(defstruct action
  name
  preconditions
  effects+
  effects-)

;; convenience constructor to avoid quoting s-expressions
(defmacro action (name &key preconditions effects+ effects-)
  `(make-action :name ',name 
                :preconditions ',preconditions
                :effects+ ',effects+
                :effects- ',effects-))

;; reader/parser for convenient action notation
(defun read-action (stream char arg)
  "reader for actions, so one may write #a<lift-box :effect+ (holds box) :effect- (gripper-free)>"
  (declare (ignore char arg)) ; char and arg are "#a"
  (unless (eql (read-char stream) #\<)
    (error "action #a<...> misformatted, '<' expected"))
  (let ((expr (read-delimited-list #\> stream)))
    `(action ,@expr)))

;; add reader for actions to Lisp parser
(set-dispatch-macro-character
 #\# #\a
 #'read-action)

(defparameter *robot-actions*
  (list 
   #a<grasp-mug :preconditions (gripper-empty) :effects- (gripper-empty) :effects+ (gripper-has-mug)>
   #a<release   :preconditions () :effects- (gripper-has-mug) :effects+ (gripper-empty)>))

(defun action-admissible? (state action)
  (subsetp (action-preconditions action)
           state 
           :test #'equal))

(defun action-transition (state action)
  (set-difference (union state (action-effects+ action) :test #'equal) ; add effects+
                  (action-effects- action) ;  remove effects-
                  :test #'equal))

(defun simple-forward-planner (state goal &optional plan-so-far)
  (if (subsetp goal state :test #'equal) ; reach goal state?
      plan-so-far
      (loop for a in (remove-if-not #'(lambda (x) (action-admissible? state x)) *robot-actions*)
        finally (return nil) ; return nil=no plan when no action leads to goal
        do
        (let* ((plan-candidate  (cons (action-name a) plan-so-far))
               (trial (simple-forward-planner (action-transition state a)
                                              goal
                                              plan-candidate)))
          (format t "trying action ~a~%" (action-name a))
          (when trial
            (format t "reached goal: ~a" state)
            (return-from simple-forward-planner plan-candidate))))))



;; non-determinism using throw-catch
(defparameter +failure+ (gensym))
(defparameter +try-other+ (gensym))

(defmacro fail ()
  `(throw +try-other+ +failure+))

(defmacro choose (variable list-of-values &body code)
  (let ((result (gensym))
        (choices (gensym)))
    `(let ((,choices ,list-of-values))
       (loop for ,variable in ,choices finally (return +failure+) do
         (let ((,result (catch ',+try-other+ ,@code)))
           (unless (eql ,result +failure+)
             (return ,result)))))))

#|
(let ((lst (loop for i from 1 to 100 collect i)))
    (choose a lst
      (choose b lst
        (choose c lst 
          (if (= (+ (* a a) (* b b)) 
                 (* c c))
              (list a b c)
              (fail))))))
|#