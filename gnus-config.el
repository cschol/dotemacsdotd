;; gnus configuration
(setq user-mail-address "cschol2112@googlemail.com")

;; runtime-data-dir defined in init.el
(defvar gnus-runtime-data-directory (concat runtime-data-dir "gnus")
  "Directory for gnus runtime data.")

(setq message-directory (concat gnus-runtime-data-directory "/mail"))
(setq gnus-home-directory gnus-runtime-data-directory)
(setq gnus-dribble-directory gnus-home-directory)
(setq gnus-startup-file (concat gnus-home-directory "/.newsrc"))
(setq gnus-directory (concat gnus-home-directory "/News/"))
(setq gnus-kill-files-directory (concat gnus-directory "/scores/"))
(setq gnus-article-save-directory (concat gnus-directory "/saved/"))

(setq gnus-nntp-server nil
      gnus-read-active-file nil
      gnus-save-newsrc-file nil
      gnus-read-newsrc-file nil)

(setq mail-user-agent 'gnus-user-agent)

(setq gnus-select-method
      '(nnimap "gmail"
                (nnimap-address "imap.gmail.com")
                (nnimap-stream ssl)
                (nnimap-server-port 993)))

(setq gnus-invalid-group-regexp "[:`'\"]\\|^$")
(setq gnus-ignored-newsgroups "")
(setq gnus-outgoing-message-group "[Google Mail]/Sent Mail")

(setq gnus-thread-sort-functions
      '(gnus-thread-sort-by-most-recent-date))

(setq gnus-thread-hide-subtree t)
(setq message-generate-headers-first t)
(setq message-kill-buffer-on-exit t)
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
