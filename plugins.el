;; Settings for Emacs plugins

;; custom themes
(add-to-list 'custom-theme-load-path (concat dotfiles-dir "themes"))

;; FIXME Do I need this?
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
(add-to-list 'ido-ignore-buffers ".*TAGS$")

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

(autoload 'find-file "find-file" nil t)
(eval-after-load 'find-file
  '(progn
     (setq cc-search-directories 
           (append cc-search-directories '("../../*"
                                           "../*/*"
                                           "$PROJECT_ROOT/*/*"
                                           "$PROJECT_ROOT/*/*/include/*")))
     (set-default 'ff-ignore-include t)
     (set-default 'ff-quiet-mode t)
     (set-default 'ff-always-try-to-create nil)))

;; cc-mode customizations
(autoload 'cc-mode "cc-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(defconst my-c-style
  '((c-basic-offset . 2)
    (c-offsets-alist . ((substatement . 0)
                        (substatement-open . 0)
                        (brace-list-open . 0)
                        (statement-case-open . 0)
                        (case-label . +))))
  "My C style")
(c-add-style "cschol-style" my-c-style)

(defun my-c-initialization-hook ()
  "Init-time cc-mode customizations"
  ;; Nothing to do here yet
  )
(add-hook 'c-initialization-hook 'my-c-initialization-hook)

(defun my-c-mode-hook ()
  "c-mode customizations."
  ;; Nothing to do here yet
  )
(add-hook 'c-mode-hook 'my-c-mode-hook)

(defun my-c++-mode-hook ()
  "c++-mode customizations."
  (c-set-style "stroustrup")
  (gtags-mode 1))
(add-hook 'c++-mode-hook 'my-c++-mode-hook)

;; python
(progn
  (defun my-insert-self ()
    "Insert `self.' at the beginning of the current expression."
    (interactive)
    (save-excursion
      (search-backward-regexp "[ \n\t,(-]\\|^")
      (if (not (looking-at "^"))
          (forward-char))
      (insert "self.")))

  (defun my-python-mode-hook ()
    (setq python-indent-offset 4)

    ;; Adjust autopair behavior for Python triple-quotes
    (setq autopair-handle-action-fns
          (list 'autopair-default-handle-action
                'autopair-python-triple-quote-action)))
  (add-hook 'python-mode-hook 'my-python-mode-hook))

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

(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("dired" (mode . dired-mode))
               ("org" (or
                           (name . "^\\*Calendar\\*$")
                           (name . "^diary$")
                           (name . "^\\*Org Agenda\\*$")
                           (name . "\\.org$")))
               ("emacs" (or
                         (name . "^\\*scratch\\*$")
                         (name . "^\\*Messages\\*$")
                         (name . "^\\*Completions*$")
                         (name . "^\\*shell*$")))))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))

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

;; gtags
(autoload 'gtags-mode "gtags" "" t)

(defun my-gtags-settings ()
  "Settings for gtags."
  (interactive)

  (define-prefix-command 'gtags-keymap)
  (define-key global-map (kbd "C-c g") 'gtags-keymap)

  (define-key gtags-mode-map (kbd "C->") 'gtags-find-tag-from-here)
  (define-key gtags-mode-map (kbd "C-<") 'gtags-pop-stack)
  (define-key gtags-mode-map (kbd "C-c g s") 'gtags-find-symbol)
  (define-key gtags-mode-map (kbd "C-c g t") 'gtags-find-tag)
  (define-key gtags-mode-map (kbd "C-c g r") 'gtags-find-rtag)
  (define-key gtags-mode-map (kbd "C-c g f") 'gtags-find-file)
  (define-key gtags-mode-map (kbd "C-c g d") 'gtags-visit-rootdir)

  (define-key gtags-select-mode-map (kbd "n") 'next-line)
  (define-key gtags-select-mode-map (kbd "p") 'previous-line)
  (define-key gtags-select-mode-map (kbd "RET") 'gtags-select-tag)
  (define-key gtags-select-mode-map (kbd "C->") 'gtags-select-tag)
  (define-key gtags-select-mode-map (kbd "C-<") 'gtags-pop-stack)

  (define-key gtags-select-mode-map (kbd "q") 'quit-window)

  (add-hook 'gtags-select-mode-hook '(lambda () (hl-line-mode 1)))
  )
(eval-after-load "gtags" '(my-gtags-settings))

;; ack-and-a-half
(autoload 'ack-and-a-half-same "ack-and-a-half" nil t)
(autoload 'ack-and-a-half "ack-and-a-half" nil t)
(autoload 'ack-and-a-half-find-file-same "ack-and-a-half" nil t)
(autoload 'ack-and-a-half-find-file "ack-and-a-half" nil t)

(defalias 'ack 'ack-and-a-half)
(defalias 'ack-same 'ack-and-a-half-same)
(defalias 'ack-find-file 'ack-and-a-half-find-file)
(defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)

(defvar my-ack-options
  "-G .*(test|backup|build|deprecated).* --invert-file-match"
  "Custom ack options.")

(defun acksrc ()
  "Call ack interactively while modifying ACK_OPTIONS to discard certain files."
  (interactive)
  (let* (curr-ack-options (getenv "ACK_OPTIONS"))
    (setenv "ACK_OPTIONS" my-ack-options)
    (call-interactively 'ack)
    (setenv "ACK_OPTIONS" curr-ack-options)
    ))

;; winner
(require 'winner)
(setq winner-dont-bind-my-keys t) ;; default bindings conflict with org-mode
(global-set-key (kbd "<C-s-268632078>") 'winner-undo)
(global-set-key (kbd "<C-s-p>") 'winner-redo)
(winner-mode t)

;; holidays
(setq holiday-hebrew-holidays nil
      holiday-islamic-holidays nil
      holiday-bahai-holidays nil
      holiday-oriental-holidays nil)
