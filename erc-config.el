(autoload 'erc-select "erc" "IRC client" t)

(erc-timestamp-mode t)
(erc-completion-mode t)

(setq erc-server "irc.freenode.net"
      erc-port 6667
      erc-nick "cschol"
      erc-user-full-name user-full-name
      erc-email-userid "userid"
      erc-prompt-for-password nil
      erc-auto-query 'buffer ; show queries in separate buffer
      erc-kill-buffer-on-part t
      erc-kill-server-buffer-on-quit t
      erc-current-nick-highlight-type 'nick
      erc-timestamp-only-if-changed-flag nil
      erc-timestamp-format "[%H:%M] "
      erc-insert-timestamp-function 'erc-insert-timestamp-left
      erc-fill-prefix "        "
      erc-interpret-mirc-color t)

(require 'erc-match)
(setq erc-keywords '("cschol")) ; highlight these words
(erc-match-mode)

(erc-track-mode t)
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                 "324" "329" "332" "333" "353" "477"))
;; don't show any of this
(setq erc-hide-list '("MODE"))

;; (require 'erc-join)
;; (erc-autojoin-mode t)
;; (setq erc-autojoin-channels-alist
;;       '((".*\\.freenode.net" "#emacs")
;;         (".*\\.quakenet.org" "#progrock-dt")))

(defun my-erc-after-connect-hook (server nick)
  (cond
   ((string-match ".*\\.quakenet.org" server)
    (erc-message "PRIVMSG" "Q@CServe.quakenet.org AUTH schroedkroet MFS-ywhz")
    (erc-server-send (format "MODE %s +x" nick))
    )))

(add-hook 'erc-after-connect 'my-erc-after-connect-hook)

(defun erc-quakenet ()
  "Connect to Quakenet."
  (interactive)
  (erc :server "us.quakenet.org" :port 6668
       :nick "USAschroed" :full-name "Schroeder"))

(defun erc-freenode ()
  "Connect to Freenode."
  (interactive)
  (erc :server "irc.freenode.net" :port 6667
       :nick "cschol" :full-name "Christoph"))
