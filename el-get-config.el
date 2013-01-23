(setq el-get-sources
      '((:name smex
               :after (progn
                        (setq smex-save-file (concat runtime-data-dir ".smex-items"))
                        (global-set-key (kbd "M-x") 'smex)
                        (global-set-key (kbd "M-X") 'smex-major-mode-commands)))

        (:name paredit
               :after (progn
                        (defvar my-enable-paredit-mode-hook-list
                          '(emacs-lisp-mode-hook
                            lisp-mode-hook
                            lisp-interaction-mode-hook)
                          "List of hooks of modes that have paredit enabled.")

                        (mapc (lambda (hook)
                                (add-hook hook (lambda () (paredit-mode +1))))
                              my-enable-paredit-mode-hook-list)))

        (:name auto-complete
               :after (progn
                        (require 'auto-complete-config)
                        (add-to-list 'ac-dictionary-directories (concat dotfiles-dir "plugins/autocomplete/dict"))
                        (ac-config-default)
                        (setq ac-comphist-file (concat runtime-data-dir "ac-comphist.dat"))
                        (setq ac-auto-start nil)
                        (define-key ac-mode-map (kbd "M-/") 'auto-complete)
                        (setq ac-use-quick-help nil)
                        (setq ac-use-menu-map t)
                        (define-key ac-menu-map "\C-n" 'ac-next)
                        (define-key ac-menu-map "\C-p" 'ac-previous)

                        ;; (ac-define-source etags
                        ;;   '((candidates . (lambda ()
                        ;;                     (all-completions ac-target (tags-completion-table))))
                        ;;     (requires . 3)
                        ;;     (summary "auto-complete source for Etags")))

                        ;; (add-hook 'c-mode-common-hook
                        ;;           '(lambda ()
                        ;;              (add-to-list 'ac-omni-completion-sources
                        ;;                           (cons "\\." '(ac-source-semantic)))
                        ;;              (add-to-list 'ac-omni-completion-sources
                        ;;                           (cons "->" '(ac-source-semantic)))
                        ;;              (add-to-list 'ac-sources 'ac-source-etags)))
                        (set-cursor-color "white")
                        ))

        (:name browse-kill-ring
               :after (progn
                        (require 'browse-kill-ring)
                        (browse-kill-ring-default-keybindings)
                        ))

        (:name browse-kill-ring+
               :type emacswiki
               :feature browse-kill-ring+
               :after (progn
                        (setq browse-kill-ring-no-duplicates t)
                        ))

        (:name magit
               :after (progn
                        (global-set-key (kbd "C-x C-z") 'magit-status)))

        (:name popwin
               :after (progn
                        (setq display-buffer-function 'popwin:display-buffer)))
        ))

(setq el-get-packages
      '(el-get
        helm
        org-mode
        cedet))

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))

(el-get 'sync
        (append el-get-packages
                (mapcar 'el-get-source-name el-get-sources)))
