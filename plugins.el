;; Settings for emacs plugins

;; custom themes
(add-to-list 'custom-theme-load-path (concat dotfiles-dir "themes"))

;; org-mode
;; (require 'org-install)
;; (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; (setq org-directory (concat runtime-data-dir "org"))
;; (when (not (file-exists-p org-directory))
;;   (make-directory org-directory))

;; (setq org-default-notes-file (concat org-directory "/default-notes.org")
;;       org-capture-templates
;;       '(("t" "Todo" entry (file+headline (concat org-directory "/todo.org") "Tasks")
;;          "* TODO %?\n  %i\n")
;;         ("n" "Note" entry (file+headline (concat org-directory "/notes.org") "Notes")
;;          "* %?\n%U\n  %i\n")))

;; (defun my-org-mode-hook ()
;;   ;; Fix conflict between org-mode and yasnippet
;;   (lambda ()
;;     (org-set-local 'yas/trigger-key [tab])
;;     (define-key yas/keymap [tab] 'yas/next-field-group)))
;; (add-hook 'org-mode-hook 'my-org-mode-hook)

;; yasnippet
;; (require 'yasnippet)
;; (setq yas/root-directory
;;       (list (concat dotfiles-dir "plugins/snippets")
;;             (concat dotfiles-dir "plugins/yasnippet/snippets")
;;             (concat dotfiles-dir "plugins/snippets-work")))

;; (mapc (lambda (dir)
;;         (yas/load-directory dir))
;;       yas/root-directory)

;; (setq yas/indent-line 'fixed) ;; Fixes indentation problem with comments
;; (setq yas/prompt-functions '(yas/ido-prompt yas/dropdown-prompt))
;; (yas/global-mode 1)

;; (defun my-yasnippet-update-hook ()
;;   (when (string-match "snippets" buffer-file-name)
;;     (yas/load-directory (concat dotfiles-dir "plugins/snippets"))))
;; (add-hook 'after-save-hook 'my-yasnippet-update-hook)

;; ido
(require 'ido)
(setq ido-enable-flex-matching t
      ido-confirm-unique-completion nil
      ido-create-new-buffer 'always
      ido-save-directory-list-file (concat runtime-data-dir ".ido.last")
      ido-auto-merge-work-directories-length -1)
(ido-everywhere t)
(ido-mode 1)

;; grep
(setq grep-files-aliases
      '(("all" . "* .*")
        ("el" . "*.el")
        ("ch" . "*.c *.h")
        ("cc" . "*.cc *.cxx *.cpp *.C *.CC *.c++")
        ("hh" . "*.hh *.hxx *.hpp *.H *.HH *.h++")
        ("c" . "*.c")
        ("h" . "*.h")
        ))

;; cc-mode customizations
(autoload 'cc-mode "cc-mode" nil t)

;; cc-mode style for stuff at work
(defconst my-work-c-style
  '((c-basic-offset . 2)
    (c-offsets-alist . ((substatement . 0)
                        (substatement-open . 0)
                        (brace-list-open . 0)
                        (statement-case-open . 0)
                        (case-label . +))))
  "My C style for work")
(c-add-style "work" my-work-c-style)

(defun my-c-initialization-hook ()
  "Init-time cc-mode customizations"
  ;; ignore #include lines with ff-find-other-file
  (setq ff-ignore-include t))
(add-hook 'c-initialization-hook 'my-c-initialization-hook)

(defun my-c-mode-hook ()
  "cc-mode customizations for work"
  (when (string-match "work" buffer-file-name)
    (c-set-style "work")))
(add-hook 'c-mode-hook 'my-c-mode-hook)

;; uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-ignore-buffers-re "^\\*")

;; ibuffer
(require 'ibuffer)
(setq ibuffer-default-sorting-mode 'recency
      ibuffer-restore-window-config-on-quit t
      ibuffer-always-show-last-buffer t
      ibuffer-never-show-predicates (list "^ \\*"))

;; ediff
(defun my-ediff-load-hook ()
 (setq ediff-window-setup-function 'ediff-setup-windows-plain
       ediff-split-window-function 'split-window-horizontally
       ediff-keep-variants nil))
(add-hook 'ediff-load-hook 'my-ediff-load-hook)

;; desktop-save
(setq desktop-path (list runtime-data-dir))
(desktop-save-mode 1)

;; midnight
(require 'midnight)
(setq midnight-mode t)

;; eshell
(setq eshell-directory-name (concat runtime-data-dir "eshell"))

;; bookmark
(setq bookmark-default-file (concat runtime-data-dir ".emacs.bmk"))

;; aspell
(require 'ispell)
(setq-default ispell-program-name "aspell")
(setq flyspell-issue-message-flag nil)

;; narrowing
(put 'narrow-to-defun 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; find function
(find-function-setup-keys)

;; abbrevs
(setq abbrev-file-name (concat runtime-data-dir "abbrev_defs"))

;; windmove
(windmove-default-keybindings)

;; tramp
(require 'tramp)
(setq tramp-default-method "pscp")
(setq tramp-verbose 3) ;; 3=default
(setq password-cache-expiry nil)
