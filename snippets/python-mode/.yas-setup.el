(require 'yasnippet)
(defvar yas-text)

(defun python-split-args (arg-string)
  "Split a python argument string into ((name, default)..) tuples"
  (mapcar (lambda (x)
             (split-string x "[[:blank:]]*=[[:blank:]]*" t))
          (split-string arg-string "[[:blank:]]*,[[:blank:]]*" t)))

(defun python-args-to-docstring ()
  "return docstring format for the python arguments in yas-text"
  (let* ((indent (concat "\n" (make-string (current-column) 32)))
         (args (python-split-args yas-text))
         (max-len (if args (apply 'max (mapcar (lambda (x) (length (nth 0 x))) args)) 0))
         (formatted-args (mapconcat
                (lambda (x)
                   (concat (nth 0 x) (make-string (- max-len (length (nth 0 x))) ? ) " -- "
                           (if (nth 1 x) (concat "\(default " (nth 1 x) "\)"))))
                args
                indent)))
    (unless (string= formatted-args "")
      (mapconcat 'identity (list "Keyword Arguments:" formatted-args) indent))))

(defun python-args-to-docstring-numpy ()
  "return docstring format for the python arguments in yas-text"
  (let* ((args (python-split-args yas-text))
         (format-arg (lambda(arg)
                       (concat (nth 0 arg) " : " (if (nth 1 arg) ", optional") "\n")))
         (formatted-params (mapconcat format-arg args "\n"))
         (formatted-ret (mapconcat format-arg (list (list "out")) "\n")))
    (unless (string= formatted-params "")
      (mapconcat 'identity
                 (list "\nParameters\n----------" formatted-params
                       "\nReturns\n-------" formatted-ret)
                 "\n"))))

(defun python-args-to-docstring-rest ()
  "return docstring format(reST) for the python arguments in yas-text"
  (let* ((indent (make-string (current-column) 32))
		 (args (python-split-args (replace-regexp-in-string  "[\n\|[:space:]]" "" yas-text)))
         (format-arg (lambda(arg)
                       (concat indent ":param " (nth 0 arg) ":")))
         (formatted-params (mapconcat format-arg args "\n"))
         (formatted-ret (concat indent ":return: ")))
    (unless (string= formatted-params "")
      (mapconcat 'identity
                 (list "" formatted-params formatted-ret) "\n"))))
(defalias 'sgu-python-args-parse-current-kill-insert 'python-args-parse-current-kill-insert)
(defun python-args-parse-current-kill-insert()
  "pretent the current-kill as yas-text, parse and insert to point"
  (interactive)
  (let* ((yas-text (substring-no-properties (current-kill 0)))
		 (s (python-args-to-docstring-rest)))
	(insert s)))


(add-hook 'python-mode-hook #'yasnippet-snippets--fixed-indent)
