;; Original idea from
;; http://stackoverflow.com/questions/683425/globally-override-key-binding-in-emacs
(defvar my-keys-minor-mode-map (make-sparse-keymap) "my-keys-minor-mode keymap.")

(defvar my-keys-minor-mode-mapping-alist
  '(("C-z"   . scroll-up-line)
    ("C-M-z" . scroll-down-line)

    ("C-c l" . org-store-link)
    ("C-c c" . org-capture)
    ("C-c a" . org-agenda)

    ("C-M-<backspace>" . kill-whole-line)
    ("C-x C-b"         . ibuffer-other-window)
    ("C-c C-b"         . bury-buffer)
    ("C-c C-u"         . unbury-buffer)

    ;; from http://sites.google.com/site/steveyegge2/effective-emacs
    ("C-c C-m" . execute-extended-command)
    ("C-w"     . backward-kill-word)
    ("C-x C-k" . kill-region)
    ("C-c C-k" . kill-region)

    ;; Map my custom functions
    ("C-S-l" . my-duplicate-line)
    ("C-M-o" . my-vi-open-line)
    ("C-M-;" . my-comment-dwim-line)
    ("C-c m" . my-maximize-frame)
    ("C-x K" . my-kill-all-buffers))

  "List mapping custom keybindings to functions per mode and mode-keymap.")

;; Add all global key bindings to my custom keymap.
(mapc
 (lambda (item)
   (define-key my-keys-minor-mode-map (read-kbd-macro (car item)) (cdr item)))
 my-keys-minor-mode-mapping-alist)

(define-minor-mode my-keys-minor-mode
  "A minor mode for my own custom key bindings." t
  " my-keys" 'my-keys-minor-mode-map)

;; Map functions specific to certain modes
(eval-after-load 'cc-mode
  '(progn
     (define-key c-mode-base-map (kbd "C-c o") 'ff-find-other-file)))

(eval-after-load 'python
  '(progn
     (define-key python-mode-map (kbd "C-;") 'my-insert-self)))

(eval-after-load 'paredit
  '(progn
     (define-key paredit-mode-map (kbd "M-r") nil)))

;; Function alias definitions
(defalias 'qrr 'query-replace-regexp)
(defalias 'wc 'whitespace-cleanup)

;; Switch default behavior of quit-window from bury to kill
(global-set-key [remap quit-window]
                (lambda () (interactive) (quit-window (not current-prefix-arg))))
