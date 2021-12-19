(defvar domain '())
(defvar problem '())

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
                                   (append domain (list (string-right-trim '(#\Tab #\Return) (string-left-trim '(#\Tab) line)))))))))

(defun get-problem-file (filename)
  (setq problem '())
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect (setq problem (if (string= "" (string-right-trim '(#\Tab #\Return) (string-left-trim '(#\Tab) line)))
                                    problem
                                    (append problem (list (string-right-trim '(#\Tab #\Return) (string-left-trim '(#\Tab) line)))))))))