(require 'ensime)

(defvar scala-src-dir "~/src/pub/scala/src")
(defvar scala-src-lib-dir "~/src/pub/scala/src/library")
(defvar scala-files-regexp "*.scala")

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
    
    (define-key ensime-mode-map (kbd "C-b t") 'an/ensime-sbt-do-test)))
