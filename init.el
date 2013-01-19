;; EMACS configuration
(defvar dotfiles-dir (file-name-directory
                      (or (buffer-file-name) load-file-name))
  "Location of .emacs.d directory.")

;; Adjust load path
(add-to-list 'load-path (concat dotfiles-dir "vendor"))
(progn
  (cd (concat dotfiles-dir "vendor"))
  (normal-top-level-add-subdirs-to-load-path))

;; Path to runtime configuration data files
(defconst tmp-data-dir "~/")
(defvar runtime-data-dir (concat tmp-data-dir ".emacs-data/")
  "Location of Emacs data files.")
(when (not (file-exists-p runtime-data-dir))
  (make-directory runtime-data-dir t))

;; Utility functions
(defun system-type-windows-p ()
  "Return true if system type is Windows"
  (string-equal system-type "windows-nt"))

(defun system-type-linux-p ()
  "Return true if system type is Linux"
  (string-equal system-type "gnu/linux"))

;; User configuration
(setq user-full-name "Christoph Scholtes")

;; General Emacs Look'n'Feel
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t
      visible-bell nil
      ring-bell-function 'ignore           ; disable audible bell
      line-number-mode t                   ; show line number in mode line
      column-number-mode t                 ; show column number in mode line
      size-indication-mode t               ; show buffer size in mode line
      blink-matching-paren t
      scroll-preserve-screen-position t
      truncate-partial-width-windows nil
      yank-excluded-properties t
      window-nest t
)
(setq-default truncate-lines t             ; disable line wrap
              indent-tabs-mode nil         ; disable tabs globally
              comment-column 0)
(tool-bar-mode -1)                         ; disable toolbar
(menu-bar-mode -1)                         ; disable menu bar
(scroll-bar-mode -1)                       ; disable scrollbars
(blink-cursor-mode 0)                      ; disable cursor blinking
(setq transient-mark-mode nil)             ; disable t-m-m
(global-auto-revert-mode 1)                ; always reread file if they have changed
(fset 'yes-or-no-p 'y-or-n-p)              ; enable shortcuts for yes or no

(setq split-width-threshold nil)           ; don't allow horizontal split

;; Changelog
(setq add-log-keep-changes-together t)

;; Link certain registers to frequently accessed files
(set-register ?i `(file . ,(concat dotfiles-dir "init.el")))
(set-register ?p `(file . ,(concat dotfiles-dir "plugins.el")))

;; Highlight keywords in source code for certain modes
(defvar my-highlight-keywords-mode-list
  '(c-mode
    c++-mode
    python-mode
    ruby-mode
    org-mode
    emacs-lisp-mode
    js2-mode)
  "List of modes with highlighted keywords.")

(make-face 'font-lock-fixme-face)
(modify-face 'font-lock-fixme-face "Red" nil nil t nil nil nil nil)
(make-face 'font-lock-todo-face)
(modify-face 'font-lock-todo-face "Blue" nil nil t nil nil nil nil)
(mapc (lambda (mode)
        (font-lock-add-keywords mode
                                '(("\\<\\(FIXME\\)" 1 'font-lock-fixme-face t))))
      my-highlight-keywords-mode-list)

(setq font-lock-maximum-size nil)

(defvar my-major-programming-mode-hook-list
  '(emacs-lisp-mode-hook
    c-mode-hook
    c++-mode-hook
    python-mode-hook
    js2-mode-hook
    ruby-mode-hook)
  "List of major programming modes I use frequently.")

(defun my-programming-modes-common-hook ()
  "Common hook for major programming modes"
  ;(setq show-trailing-whitespace t)
  )

;; Add hooks common to all my major programming modes
(mapc (lambda (hook)
        (add-hook hook 'my-programming-modes-common-hook))
      my-major-programming-mode-hook-list)

;; Allow minibuffer to resize as required
(setq resize-mini-windows t
      max-mini-window-height 5)
;; Disable line truncation in minibuffer
(add-hook 'minibuffer-setup-hook
    (lambda () (setq truncate-lines nil)))

;; Show full file path in frames title bar
(setq frame-title-format
      '((buffer-file-name "%f" (dired-directory dired-directory "%b")) " - "
        invocation-name "@" system-name))

;; Backup & version control
(setq backup-directory-alist `(("." . ,(concat runtime-data-dir "backup")))
      backup-by-copying t
      make-backup-files t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; Auto-save settings (file name, location)
(setq auto-save-list-file-name (concat runtime-data-dir ".auto-save-list"))
(defvar auto-save-dir (concat runtime-data-dir "auto-save/"))
(when (not (file-exists-p auto-save-dir))
  (make-directory auto-save-dir t))
(setq auto-save-file-name-transforms
      `(("\\(?:[^/]*/\\)*\\(.*\\)" ,(concat auto-save-dir "\\1") t)))

(defun auto-save-file-name-p (filename)
  (string-match "^#.*#$" (file-name-nondirectory filename)))

(defun make-auto-save-file-name ()
  (concat auto-save-dir
          (if buffer-file-name
              (concat "#" (file-name-nondirectory buffer-file-name) "#")
            (expand-file-name
             (concat "#%" (buffer-name) "#")))))

;; Enable auto-fill in text mode
(setq-default major-mode 'text-mode)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; vc
(setq vc-diff-switches "-u") ; always show unified diff

;; auto-indent yanked code in certain modes
(defvar my-auto-indent-on-yank-mode-list
  '(emacs-lisp-mode
    scheme-mode
    lisp-mode
    c-mode
    c++-mode
    ruby-mode
    objc-mode
    latex-mode
    plain-tex-mode)
  "List of modes which support auto-indent on yank.")

(defadvice yank (after indent-region activate)
  (if (member major-mode my-auto-indent-on-yank-mode-list)
      (let ((mark-even-if-inactive transient-mark-mode))
        (indent-region (region-beginning) (region-end) nil))))

(defadvice yank-pop (after indent-region activate)
  (if (member major-mode my-auto-indent-on-yank-mode-list)
      (let ((mark-even-if-inactive transient-mark-mode))
        (indent-region (region-beginning) (region-end) nil))))

;; Advise quit-window to kill buffer instead of burying it
(defadvice quit-window (before advise-quit-window activate)
  (when (called-interactively-p 'any)
    (ad-set-arg 0 (not (ad-get-arg 0)))))

;; emacs server
(require 'server)
(setq server-auth-dir (concat runtime-data-dir "server/"))

(defun maybe-server-start ()
  "Start emacs server if it is not already running."
  (if (not (eq t (server-running-p server-name)))
      (server-start)
    (message (format "Server %S is already running." server-name))))

(add-hook 'after-init-hook 'maybe-server-start)

;; Load additional info files
(setq Info-default-directory-list
      (append Info-default-directory-list `(,(concat dotfiles-dir "info"))))

;; Load platform specific configuration
(if (system-type-windows-p) (load (concat dotfiles-dir "windows.el")))
(if (system-type-linux-p) (load (concat dotfiles-dir "linux.el")))

;; Load other configuration files
(load (concat dotfiles-dir "el-get-config.el"))
(load (concat dotfiles-dir "package-config.el"))
(load (concat dotfiles-dir "plugins.el"))
(load (concat dotfiles-dir "custom-functions.el"))
(load (concat dotfiles-dir "mail-config.el") 'noerror)
(load (concat dotfiles-dir "gnus-config.el") 'noerror)
(load (concat dotfiles-dir "project-config.el") 'noerror)
(load (concat dotfiles-dir "erc-config.el") 'noerror)
(load (concat dotfiles-dir "minimal-cedet-config.el"))
(load (concat dotfiles-dir "keybindings.el"))
(setq custom-file (concat dotfiles-dir "init-custom.el"))
(load custom-file 'noerror)

;; Load org-mode configuration from Dropbox (if available)
(let* ((dropbox-org-dir (getenv "DROPBOXORG")))
  (if (not (or (eq dropbox-org-dir nil)
               (eq dropbox-org-dir "")))
      (load (concat dropbox-org-dir "/GTD/org-config.el") nil)
    (message "Invalid environment variable DROPBOXORG.")))

;; Globally enable custom key layout...
(my-keys-minor-mode 1)
;; ...except in the mini-buffer
(defun my-minibuffer-setup-hook ()
  (my-keys-minor-mode 0))
(add-hook 'minibuffer-setup-hook 'my-minibuffer-setup-hook)
