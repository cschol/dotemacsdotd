;; Machine-specific org-file locations
(cond ((and (system-type-windows-p)
            (string= system-name "MARVIN"))
       (setq org-files-dir "D:/Dropbox/org-mode/"))
      ((system-type-linux-p)
       (setq org-files-dir "~/Dropbox/org-mode/"))
      (t
       (setq org-files-dir org-directory)))

;; org-mode files
(setq org-default-notes-file (concat org-files-dir "default-notes.org"))
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline (concat org-files-dir "todo.org") "Tasks")
         "* TODO %?\n  %i\n")
        ("n" "Note" entry (file+headline (concat org-files-dir "notes.org") "Notes")
         "* %?\n%U\n  %i\n")))
