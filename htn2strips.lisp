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
  (setq *htn-task* (get-from-htn *domain* 'is-task))
  (setq *htn-method* (get-from-htn *domain* 'is-method))
  (setq *htn-action* (get-from-htn *domain* 'is-action))
  (setq *htn-init* (get-from-htn *problem* 'is-init))

  ;transfrom htn to strips
  (setq *strips-action* (translate-actions))

  (print "Complete"))

(defun read-file (filename)
  (with-open-file (stream filename)
    (read stream)))

(defun is-task (input)
  (eq input ':task))

(defun is-method (input)
  (eq input ':method))

(defun is-action (input)
  (eq input ':action))

(defun is-init (input)
  (eq input ':init))

(defun get-from-htn (list key-word)
  (remove-if-not key-word (cdr list) :key #'first))

;; translating the actions
(defun translate-actions ()
  (let ((action '())
        (params '())
        (preconditions '())
        (postconditions '())
        (keywords '(:action :parameters :precondition :effect))
        (current-keyword '())
        (htn-actions-copy *htn-action*)
        (strips-actions '()))

    (loop for element in htn-actions-copy do
            (loop while (not (eq element NIL))
                  do (if (eq (first element) (first keywords))
                         (progn
                          (setq action (append action (list (second element)))) ;;adds the actionname to the list
                          (setq element (remove (first element) element)) ;; remove the :action keyword from the htn action
                          (setq element (remove (first element) element))))
                     ;; remove the actionname from the htn action

                     (if (eq (first element) (second keywords))
                         (progn
                          (setq current-keyword (second keywords))
                          (setq element (remove (first element) element))))

                     (if (eq (first element) (third keywords))
                         (progn
                          (setq current-keyword (third keywords))
                          (setq element (remove (first element) element))))

                     (if (eq (first element) (fourth keywords))
                         (progn
                          (setq current-keyword (fourth keywords))
                          (setq element (remove (first element) element))))

                     (if (eq current-keyword (second keywords))
                         (progn
                          (setq params (append params (first element)))
                          (setq element (remove (first element) element))))

                     (if (eq current-keyword (third keywords))
                         (progn
                          (setq preconditions (append preconditions (first element)))
                          (setq element (remove (first element) element))))

                     (if (eq current-keyword (fourth keywords))
                         (progn
                          (setq postconditions (append postconditions (first element)))
                          (setq element (remove (first element) element)))))

            (setq action (append action (list params)))
            (setq action (append action (list preconditions)))
            (setq action (append action (list postconditions)))
            (setq params '())
            (setq preconditions '())
            (setq postconditions '())
            (setq strips-actions (append strips-actions (list action)))
            (setq action '()))

    (setq htn-actions-copy '())
    strips-actions))
