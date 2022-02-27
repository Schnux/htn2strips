;(htn2strips "input/domain.hddl" "input/problem.hddl")

(defvar *domain* '())
(defvar *problem* '())
(defvar *htn-task* '())
(defvar *strips-task* '())
(defvar *htn-method* '())
(defvar *strips-method* '())
(defvar *htn-action* '())
(defvar *strips-action* '())
(defvar *htn-init* '())
(defvar *strips-init* '())

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

(defun question-reader (stream char)
  (declare (ignore char))
  (read stream))

(defun htn2strips (domain-file problem-file)
  (let ((*readtable* (copy-readtable)))
    (set-macro-character #\? #'question-reader)

    ;read and save domain/problem hddl file
    (setq *domain* (subst '! 'NOT (read-file domain-file)))
    (setq *problem* (subst '! 'NOT (read-file problem-file))))

  ;get and save tasks, methods and actions in lists
  (setq *htn-task* (get-from-htn *domain* ':task))
  (setq *htn-method* (get-from-htn *domain* ':method))
  (setq *htn-action* (get-from-htn *domain* ':action))
  (setq *htn-init* (cdr (first (get-from-htn *problem* ':init))))

  ;transfrom htn to strips
  (setq *strips-action* (parse-actions *htn-action*))
  (setq *strips-method* (parse-methods *htn-method*))
  (setq *strips-task* (parse-tasks *htn-task*))

  (write-file)

  "Complete")

(defun read-file (filename)
  (with-open-file (stream filename)
    (read stream)))

(defun is-of-type (my-type input)
  (eq input my-type))

(defun get-from-htn (list key-word)
  (remove-if-not (lambda (elem) (is-of-type key-word elem)) (cdr list) :key #'first))

;; Fehler?
(defun remove-hyphen (symbol-or-list)
  "turns some list (foo-bar baz foo-foo) into (foobar baz foofoo)"
  ;; (loop for symbol in list-of-symbols collect 
  ;;   (intern (remove #\- (symbol-name symbol)))
  (if (listp symbol-or-list)
      (mapcar #'remove-hyphen symbol-or-list)
      (intern (remove #\- (symbol-name symbol-or-list)))))

(defun remove-hyphen-old (h-list)
  (first (string-to-list (remove #\- (write-to-string h-list)))))

(defun parse-actions (htn-actions)
  (loop for element in htn-actions
        collect
        (make-strips-action
         :parameters (remove-item-and-next '- (parse-it :parameters element))
         :name (remove-hyphen (second element))
         :preconditions (remove-hyphen (parse-it :precondition element))
         :postconditions (remove-hyphen (parse-it :effect element)))))

(defun parse-methods (htn-methods)
  (loop for element in htn-methods
        collect
        (make-strips-method
         :parameters (remove-item-and-next '- (parse-it :parameters element))
         :name (add-prefix-to-element "M" (list (remove-hyphen (second element))))
         :postconditions (remove-hyphen (parse-it :task element))
         :subtasks (change-name (remove-hyphen (parse-it :subtasks element))))))

(defun parse-tasks (htn-tasks)
  (loop for element in htn-tasks
        collect
        (make-strips-task
         :parameters (remove-item-and-next '- (parse-it :parameters element))
         :name (add-prefix-to-element "T" (list (remove-hyphen (second element)))))))


;;TODO
;; - Names with and
;; - find right params
(defun change-name (precon-list)
  (let ((res (list 'AND)))
    (loop for elem in (cdr precon-list) do
            (setq res (append res (list (append (list (find-postcondition (first elem))) (cdr elem))))))
    res))

(defun find-postcondition-name (name)
  (let ((res))
    (loop for elem in *strips-action* do
            (cond
             ((eq name (strips-action-name elem)) (setq res (strips-action-postconditions elem)))))
    (cond ((eq (first res) 'AND) 'WIP)
          ((eq res NIL) (add-prefix-to-element "T" (list name)))
          (T (first res)))))

(defun parse-it (keyword element)
  "grabs keyword definition from (:action foo .... :keyword x y)"
  ;; todo
  (second (member keyword element)))

(defun remove-item-and-next (item list)
  (cond ((null list) nil)
        ((equal item (first list))
         (remove-item-and-next item (cdr (rest list))))
        (T (cons (first list)
                 (remove-item-and-next item (rest list))))))

(defun write-file ()
  (with-open-file (file #P"output.txt" :direction :output
                        :if-exists :supersede
                        :if-does-not-exist :create)
    (format file "Initial state: ")
    ;;init
    (let ((i 0))
      (loop for element in *htn-init* do
              (setq i (1+ i))
              (format file "~{~a(~a)~}" element)
              (if (< i (length *htn-init*))
                  (format file ","))))
    (fresh-line file)
    (format file "Goal state: ")
    (fresh-line file)
    (format file "Actions:")
    (fresh-line file)
    ;;methods
    (loop for method in *strips-method* do
            (format file "~12,0T~a" (strips-method-name method))
            (format file "(~{~a~^,~})~%" (strips-method-parameters method))
            (format file "~12,0TPreconditions: ")
            (format file "~a ~%" (process-conditions (strips-method-subtasks method) (make-array 0 :element-type 'character :fill-pointer 0)))
            (format file "~12,0TPostconditions: ")
            (format file "T~a" (process-conditions (strips-method-postconditions method) (make-array 0 :element-type 'character :fill-pointer 0)))
            (fresh-line file)
            (terpri file))
    ;;actions
    (loop for action in *strips-action* do
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
