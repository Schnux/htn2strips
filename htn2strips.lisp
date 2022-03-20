;(htn2strips "inputs/tea/domain.hddl" "inputs/tea/problem.hddl")

(defstruct strips-action
  name
  preconditions
  postconditions
  parameters)

(defstruct strips-method
  name
  postconditions
  subtasks
  parameters)

(defstruct strips-task
  name
  parameters)

(defstruct strips-init
  name
  parameters)

(defstruct strips-goal
  name
  parameters)

(defun question-reader (stream char)
  (declare (ignore char))
  (read stream))

(defun htn2strips (domain-file problem-file)
  "Main method: takes a domain-file and problem-file in HTN format"
  (let ((htn-task '())
        (htn-method '())
        (htn-action '())
        (htn-init '())
        (htn-goal '())
        (strips-task '())
        (strips-method '())
        (strips-action '())
        (strips-init '())
        (strips-goal '())
        (domain '())
        (problem '()))
    (declare (ignorable strips-task))
    ;; reader makro to remove '?'
    (let ((*readtable* (copy-readtable)))
      (set-macro-character #\? #'question-reader)
      ;read and save domain/problem hddl file
      (setq domain (subst '! 'NOT (read-file domain-file)))
      (setq problem (subst '! 'NOT (read-file problem-file))))

    ;get and save tasks, methods and actions in lists
    (setq htn-task (get-from-htn domain ':task))
    (setq htn-method (get-from-htn domain ':method))
    (setq htn-action (get-from-htn domain ':action))
    (setq htn-init (cdr (first (get-from-htn problem ':init))))
    (setq htn-goal (parse-it :tasks (first (get-from-htn problem ':htn))))

    ;transfrom htn to strips
    (setq strips-action (parse-actions htn-action))
    (setq strips-method (parse-methods htn-method strips-action))
    (setq strips-task (parse-tasks htn-task))
    (setq strips-init (parse-init htn-init))
    (setq strips-goal (parse-goal htn-goal))

    (write-file strips-method strips-init strips-goal strips-action))

  "Complete")

(defun read-file (filename)
  "Function to read an input-file"
  (with-open-file (stream filename)
    (read stream)))

(defun is-of-type (my-type input)
  (eq input my-type))

(defun get-from-htn (list key-word)
  "retuns sublist where the first elem is equal with the key-word"
  (remove-if-not (lambda (elem) (is-of-type key-word elem)) (cdr list) :key #'first))

(defun remove-hyphen (symbol-or-list)
  "turns some list (foo-bar baz foo-foo) into (foobar baz foofoo)"
  (if (listp symbol-or-list)
      (mapcar #'remove-hyphen symbol-or-list)
      (intern (remove #\- (symbol-name symbol-or-list)))))

(defun parse-actions (htn-actions)
  "Takes htn-action and turns it into a strips-action struct"
  (loop for element in htn-actions
        collect
        (make-strips-action
         :parameters (remove-item-and-next '- (parse-it :parameters element))
         :name (remove-hyphen (second element))
         :preconditions (remove-hyphen (parse-it :precondition element))
         :postconditions (empty-postconditions (remove-hyphen (parse-it :effect element)) element))))

(defun parse-methods (htn-methods strips-action)
  "Takes htn-method and turns it into a strips-method struct"
  (loop for element in htn-methods
        collect
        (make-strips-method
         :parameters (remove-item-and-next '- (parse-it :parameters element))
         :name (add-prefix-to-element "M" (list (remove-hyphen (second element))))
         :postconditions (remove-hyphen (parse-it :task element))
         :subtasks (change-name (remove-hyphen (parse-it :subtasks element)) strips-action))))

(defun parse-tasks (htn-tasks)
  "Takes htn-task and turns it into a strips-task struct"
  (loop for element in htn-tasks
        collect
        (make-strips-task
         :parameters (remove-item-and-next '- (parse-it :parameters element))
         :name (add-prefix-to-element "T" (list (remove-hyphen (second element)))))))

(defun parse-init (htn-init)
  "Takes htn-init and turns it into a strips-init struct"
  (loop for element in htn-init
        collect
        (make-strips-init
         :parameters (remove-hyphen (cdr element))
         :name (remove-hyphen (first element)))))

(defun parse-goal (htn-goal)
  "Takes htn-goal and turns it into a strips-goal struct"
  (make-strips-goal
   :parameters (remove-hyphen (cdr htn-goal))
   :name (add-prefix-to-element "T" (list (remove-hyphen (first htn-goal))))))


;; substitutes the Action- or Taskname with its first postive postcondition
(defun change-name (precon-list strips-action)
  (let ((res (list 'AND)))
    (cond
     ((not (eq (first precon-list) 'AND)) (setq precon-list (append res (list precon-list)))))
    (loop for elem in (cdr precon-list) do
            (setq res (append res (list (append (list (find-postcondition-name (first elem) strips-action)) (cdr elem))))))
    res))


;; Finds and returns the corispondent postcondition name of the Action/Task name
(defun find-postcondition-name (name strips-action)
  (let ((res))
    (loop for elem in strips-action do
            (cond
             ((eq name (strips-action-name elem)) (setq res (strips-action-postconditions elem)))))
    (cond ((eq (first res) 'AND) (first (first (find-not-negative-conditions res))))
          ((eq 0 (length res)) (add-prefix-to-element "T" (list name)))
          (T (first res)))))

;; Finds postive conditions in a list of conditions and returns it
(defun find-not-negative-conditions (cond)
  (let ((res))
    (loop for postcond in (cdr cond) do
            (if (not (eq (first postcond) '!))
                (setq res (append res (list postcond)))))
    res))

;; Set postcondition when postcondition from Action is empty
(defun empty-postconditions (postcon-list elem)
  (let ((name (remove-hyphen (parse-it :ACTION elem)))
        (params (remove-item-and-next '- (parse-it :PARAMETERS elem))))
    (cond ((eq nil postcon-list) (append (list (add-prefix-to-element "E" (list name))) params))
          (T postcon-list))))

(defun parse-it (keyword element)
  "grabs keyword definition from (:action foo .... :keyword x y)"
  (second (member keyword element)))

(defun remove-item-and-next (item list)
  (cond ((null list) nil)
        ((equal item (first list))
         (remove-item-and-next item (cdr (rest list))))
        (T (cons (first list)
                 (remove-item-and-next item (rest list))))))

(defun write-file (strips-method strips-init strips-goal strips-action)
  "Formats and writes the resulting data to output.txt"
  (with-open-file (file #P"output.txt" :direction :output
                        :if-exists :supersede
                        :if-does-not-exist :create)
    ;;init
    (format file "Initial state: ")
    (loop for init in (butlast strips-init)
          finally
          (format file "~12,0T~a" (strips-init-name (first (last strips-init))))
          (format file "(~{~a~^,~})" (strips-init-parameters (first (last strips-init))))
          do (format file "~12,0T~a" (strips-init-name init))
             (format file "(~{~a~^,~})" (strips-init-parameters init))
             (format file ","))
    (fresh-line file)
    ;;goal
    (format file "Goal state: ")

    (format file "~12,0T~a" (strips-goal-name strips-goal))
    (format file "(~{~a~^,~})" (strips-goal-parameters strips-goal))

    (fresh-line file)
    (format file "Actions:")
    (fresh-line file)
    ;;methods
    (loop for method in strips-method do
            (format file "~12,0T~a" (strips-method-name method))
            (format file "(~{~a~^,~})~%" (strips-method-parameters method))
            (format file "~12,0TPreconditions: ")
            (format file "~a ~%" (process-conditions (strips-method-subtasks method) (make-array 0 :element-type 'character :fill-pointer 0)))
            (format file "~12,0TPostconditions: ")
            (format file "T~a ~%" (process-conditions (strips-method-postconditions method) (make-array 0 :element-type 'character :fill-pointer 0)))
            (terpri file))
    ;;actions
    (loop for action in strips-action do
            (format file "~12,0T~a" (strips-action-name action))
            (format file "(~{~a~^,~})~%" (strips-action-parameters action))
            (format file "~12,0TPreconditions: ")
            (format file "~a ~%" (process-conditions (strips-action-preconditions action) (make-array 0 :element-type 'character :fill-pointer 0)))
            (format file "~12,0TPostconditions: ")
            (format file "~a ~%" (process-conditions (strips-action-postconditions action) (make-array 0 :element-type 'character :fill-pointer 0)))
            (terpri file))))

(defun process-conditions (conditions stream)
  (cond ;; re-format AND: (AND x y z) -> x, y, z
   ((eq (first conditions) 'AND)
    (loop for elem in (butlast (rest conditions))
          finally (process-conditions (first (last (rest conditions))) stream)
          do (process-conditions elem stream)
             (format stream ",")) stream)

   ((eq (first conditions) '!)

    (format stream "~a~a(~{~a~^,~})"
            (first conditions)
            (first (first (cdr conditions)))
            (cdr (first (cdr conditions)))) stream)
   (T
    (format stream "~a(~{~a~^,~})"
            (first conditions)
            (cdr conditions)) stream)))

(defun add-prefix-to-element (prefix element)
  (first (string-to-list (concatenate 'String prefix (write-to-string (first element))))))


;; https://stackoverflow.com/questions/7459501/how-to-convert-a-string-to-list-using-clisp/13832673
;Turns a string into a stream so it can be read into a list
(defun string-to-list (str)
  (if (not (streamp str))
      (string-to-list (make-string-input-stream str))
      (if (listen str)
          (cons (read str) (string-to-list str))
          nil)))