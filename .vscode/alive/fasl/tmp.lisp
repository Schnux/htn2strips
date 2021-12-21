;(htn2strips "git/input/domain.hddl" "git/input/problem.hddl")
;(split "(:init" problem '())
(defvar domain '())
(defvar problem '())
(defvar htn-actions '())
(defvar strips-actions '())

(defun htn2strips (domain-file problem-file)

  (get-domain-file domain-file)
  (get-problem-file problem-file))

(defun get-domain-file (filename)
  (setq domain '())
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect (setq domain (if (string= "" (string-right-trim '(#\Tab #\Return) (string-left-trim '(#\Tab) line)))
                                   domain
                                   (append domain (list (string-right-trim '(#\Tab #\Return) (string-left-trim '(#\Tab) line))))))))
  '())

(defun get-problem-file (filename)
  (setq problem '())
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect (setq problem (if (string= "" (string-right-trim '(#\Tab #\Return) (string-left-trim '(#\Tab) line)))
                                    problem
                                    (append problem (list (string-right-trim '(#\Tab #\Return) (string-left-trim '(#\Tab) line))))))))
  '())

(defun find-substrings (substring string)
  (loop with sub-length = (length substring)
        for i from 0 to (- (length string) sub-length)
        when (string= string substring
                      :start1 i :end1 (+ i sub-length)) return T))

(defun count-substrings (substring string)
  (loop with sub-length = (length substring)
        for i from 0 to (- (length string) sub-length)
        when (string= string substring
                      :start1 i :end1 (+ i sub-length))
        count it))

(defun split (substring inlist retlist)
  (defvar i)
  (setq i 0)
  (defvar brackets)
  (setq brackets 0)
  (defvar templist)
  (setq templist '())
  (setq retlist '())

  (loop for line in inlist do
          (if (find-substrings substring line)
              (progn
               (setq templist (append templist (list line)))
               (setq i (+ i 1))
               (setq brackets (+ brackets (- (count-substrings "(" line) (count-substrings ")" line))))
               ;(print i)
               (loop for j from 0 to (- (length inlist) i)
                     while (> brackets 0)
                     do (setq templist (append templist (list (nth (+ i j) inlist))))
                        (setq brackets (+ brackets (- (count-substrings "(" (nth (+ i j) inlist)) (count-substrings ")" (nth (+ i j) inlist))))))
               (setq retlist (append retlist (list templist)))
               (setq templist '()))
              ;else
              (setq i (+ i 1))))
  retlist)


;; translating the actions
(defun translate-actions ()
  (setq action '())
  (setq keywords '(:action :parameters :precondition :effect))
  (setq current-keyword '())
  (setq htn-actions-copy htn-actions)
  (loop for element in htn-actions-copy do
          (loop while (not (= element nil))
                do ((if (eq (first element) (first keywords))
                        ((setq action (append action (second element)) ;;adds the actionname to the list
                               (setq element (remove element (first element)) ;; remove the :action keyword from the htn action
                                     (setq element (remove element (first element))))))) ;; remove the actionname from the htn action
                   )))
  (setq htn-actions-copy '())
  strips-actions)

(defun string-to-list (str)
  (if (not (streamp str))
      (string-to-list (make-string-input-stream str))
      (if (listen str)
          (cons (read str) (string-to-list str))
          nil)))

(defun concatString (list)
  (if (listp list)
      (let ((result ""))
        (dolist (item list)
          (if (stringp item)
              (setq result (concatenate 'string result item))))
        result)))
