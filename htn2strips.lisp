;(htn2strips "input/domain.hddl" "input/problem.hddl")
;(split "(:init" problem '())
;*..* für globle var

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

(defun htn2strips (domain-file problem-file)

  ;read and save domain/problem hddl file
  (setq *domain* (read-file domain-file))
  (setq *problem* (read-file problem-file))

  ;get and save tasks, methods and actions in lists
  (setq *htn-task* (get-from-htn *domain* 'is-task))
  (setq *htn-method* (get-from-htn *domain* 'is-method))
  (setq *htn-action* (get-from-htn *domain* 'is-action))
  (setq *htn-init* (get-from-htn *problem* 'is-init)))

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
        (htn-actions-copy htn-actions)))
  (loop for element in htn-actions-copy do
          ((loop while (not (= element nil))
                 do ((if (eq (first element) (first keywords))
                         ((setq action (append action (second element)) ;;adds the actionname to the list
                                (setq element (remove element (first element)) ;; remove the :action keyword from the htn action
                                      (setq element (remove element (first element))))))) ;; remove the actionname from the htn action

                     (if (eq (first element) (second keywords))
                         ((setq current-keyword (second keywords))
                          (setq element (remove element (first element)))))

                     (if (eq (first element) (third keywords))
                         ((setq current-keyword (third keywords))
                          (setq element (remo+ve element (first element)))))

                     (if (eq current-keyword (second keywords))
                         ((setq params (append params (first element)))
                          (setq element (remove element (first element)))))

                     (if (eq current-keyword (third keywords))
                         ((setq preconditions (append preconditions (first element)))
                          (setq element (remove element (first element)))))

                     (if (eq current-keyword (fourth keywords))
                         ((setq postconditions (append postconditions (first element)))
                          (setq element (remove element (first element)))))))

           (setq action (append action params))
           (setq action (append action preconditions))
           (setq action (append action postconditions))
           (setq strips-actions (append strips-actions action))))

  (setq htn-actions-copy '())
  strips-actions)
