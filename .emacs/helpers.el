(require 'ensime)

(defvar scala-src-dir "~/src/pub/scala/src")
(defvar scala-src-lib-dir "~/src/pub/scala/src/library")
(defvar combinator-dir "~/src/pub/scala-parser-combinators/")
(defvar scala-files-regexp "*.scala")

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


(defun an/scala-grep-class-in-dir-src-go()
  (interactive)
  (save-window-excursion (an/scala-grep-class-in-dir-src))
  (an/grep-go 1 4))

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



(defun an/ensime-sbt-do-test ()
  (interactive)
  (ensime-sbt-switch)
  (ensime-sbt-action "test"))

(add-hook 'ensime-mode-hook
  (lambda()
    (message "Initialize custom hooks")    
    (define-key ensime-mode-map (kbd "C-c C-b t") 'an/ensime-sbt-do-test)))
