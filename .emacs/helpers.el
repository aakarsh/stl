(require 'cl)
(require 'ensime)

(defvar scala-src-dir "~/src/pub/scala/src")
(defvar scala-src-lib-dir "~/src/pub/scala/src/library")
(defvar combinator-dir "~/src/pub/scala-parser-combinators/")
(defvar scala-files-regexp "*.scala")

(defvar an/scala-class-find-regexp
  "\\(\\(trait\\)\\|\\(class\\)\\)\s+\\([a-zA-Z0-9]+\\)")

(defun  an/scala-class-regexp(cls)
   (format  "\\(\\(trait\\)\\|\\(class\\)\\)\s*%s" cls))

(defun an/scala-grep-class-in-dir(dir class)
  (rgrep (an/scala-class-regexp class) scala-files-regexp dir))

(defun an/scala-combin-grep-class()
  (interactive)
  (let ((class  (thing-at-point 'symbol)))
    (an/scala-grep-class-in-dir combinator-dir class)))

(defun an/scala-grep-class-in-dir-src()
  (interactive)
  (an/scala-grep-class-in-dir scala-src-dir (thing-at-point 'symbol)))

(defun an/sbt-last-err()
  (interactive)
  (with-current-buffer (get-buffer "*ensime-sbt*<stl>")
    (save-excursion
      (goto-char (point-max))
      (search-backward-regexp "(\\(\\(.*.scala\\):\\([0-9]+\\)\\))" )
      (let ((file-name (match-string 2))
            (lineno  (string-to-number (match-string 3))))
      (message (format "Goto %s line [%d]" file-name lineno))
      (switch-to-buffer (get-buffer file-name))
      (goto-line lineno)))))


(defun an/line()
  (buffer-substring-no-properties (point-at-bol) (point-at-eol)))

(defstruct trace-line line class method file lineno)

(defun an/sbt-parse-trace-line()
  (interactive)
  (with-current-buffer (get-buffer "*ensime-sbt*<stl>")
    (save-excursion 
      (narrow-to-region (point-at-bol) (point-at-eol))
      (goto-char (point-min))
      (search-forward-regexp "^\\[info\\][ ]+at \\(.*\\)\\.\\([a-zA-Z0-9$]+\\)(\\(.*\\(:[0-9]+\\)?\\))")
      (widen)
      (let* ((full-line (match-string 0))
             (class (match-string 1))
            (method (match-string 2))
            (file-info  (split-string (match-string 3) ":"))
            (file-name (car file-info))
            (line-no  (if (> (length file-info) 1) (string-to-int (cadr file-info )) -1)))
        (message (format "Class [%s] Method [%s] Filename[%s]Line [%s]" class method file-name line-no))
        (make-trace-line :line full-line
                    :class class
                    :method method
                    :file file-name
                    :lineno line-no))
      )))

(defun an/sbt-trace-follow()
  (interactive)
  (let* ((tl (an/sbt-parse-trace-line))
        (n (trace-line-lineno tl))
        (f (trace-line-file tl)))
    (switch-to-buffer (get-buffer f))
    (if (>  n 0)
        (goto-line n)
      (message "No line number"))))      


(defun an/scala-grep-class-in-dir-src-go()
  (interactive)
  (save-window-excursion (an/scala-grep-class-in-dir-src))
  (an/grep-go 1 4))

(defun an/last-msg()
  (save-excursion
    (let ((line ""))
      (with-current-buffer (get-buffer "*Messages*")
        (goto-char (point-max))
        (setf line (an/line)))
      line)))

(defun an/insert-last-msg()
  (interactive)
  (insert (an/last-msg)))


(defun an/grep-go(n timout)
  (with-current-buffer (get-buffer "*grep*") 
    (sleep-for timout)
    (next-error n t)))

(defun an/scala-combin-grep-class-go()
  (interactive)
  (save-window-excursion
    (an/scala-combin-grep-class))
  (an/grep-go 1 2))


(defun an/scala-search-src(regexp)
  (interactive "sSearch For: ")
  (rgrep regexp scala-files-regexp  scala-src-dir ))

(defun an/scala-lib-search-src(regexp)
  (interactive "sSearch For: ")
    (rgrep regexp scala-files-regexp  scala-src-lib-dir ))

(defun an/scala-find-file-src(regexp)
  (interactive "sFind file : ")
  (find-name-dired  scala-src-dir regexp))

(defmacro an/sbt-alias(alias cmd)
  (let* ((name (car (split-string cmd)))
         (func-name (intern (concat "an/ensime-sbt-do-" alias))))
    `(defun ,func-name()
       (interactive)
       (ensime-sbt-switch)
       (ensime-sbt-action ,cmd))))

(defun an/sbt-macros()
  (an/sbt-alias "my-repl" "run -i")
  (an/sbt-alias "i" "run -i")
  (an/sbt-alias "test"  "test"))

(defun an/scala-guess-class()
  (interactive)                  
  (save-excursion
    (goto-char (point-min))
    (re-search-forward an/scala-class-find-regexp)
    (match-string-no-properties 4)))
    

(defun an/ensime-test-only-class() 
  (interactive)
  (ensime-sbt-switch)  
  (ensime-sbt-action
   (format "test-only com.aakarshn.%s" (an/scala-guess-class))))

(defun an/ensime-sbt-compile()
  (interactive)
  (ensime-sbt-switch)
  (ensime-sbt-action "compile")
  (ensime-sbt-action "test:compile"))

(add-hook 'ensime-mode-hook
  (lambda()
    (message "Initialize custom hooks")    
    (an/sbt-macros)
    (define-key ensime-mode-map (kbd "C-c b t") 'ensime-sbt-do-compile)
    (define-key ensime-mode-map (kbd "C-c C-b t") 'an/ensime-sbt-do-test)
    (define-key ensime-mode-map (kbd "C-c C-b i") 'an/ensime-sbt-do-i)
    (define-key ensime-mode-map (kbd "<f11>") 'an/ensime-sbt-compile)
    (define-key ensime-mode-map (kbd "<f12>") 'an/ensime-sbt-do-test)
))
