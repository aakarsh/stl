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


